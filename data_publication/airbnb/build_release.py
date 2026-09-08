#!/usr/bin/env python3
"""Build the local, unpublished Chicago and Twin Cities Airbnb candidate."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
REQUIRED_SOURCES = {
    "CHICAGO_LISTINGS": "chicago_listings.csv.gz",
    "CHICAGO_CALENDAR": "chicago_calendar.csv.gz",
    "CHICAGO_REVIEWS": "chicago_reviews.csv.gz",
    "TWIN_LISTINGS": "twin_cities_msa_listings.csv.gz",
    "TWIN_CALENDAR": "twin_cities_msa_calendar.csv.gz",
    "TWIN_REVIEWS": "twin_cities_msa_reviews.csv.gz",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source-dir",
        default=".data-build/airbnb/chicago-twin-cities-2026-06-v1/source",
    )
    parser.add_argument(
        "--output",
        default=".data-build/airbnb/chicago-twin-cities-2026-06-v1",
    )
    return parser.parse_args()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def run_sql(connection: duckdb.DuckDBPyConnection, path: Path, **values: str) -> None:
    started = time.perf_counter()
    print(f"Starting {path.name}...", flush=True)
    sql = path.read_text(encoding="utf-8")
    for key, value in values.items():
        sql = sql.replace("{{" + key + "}}", value)
    if "{{" in sql or "}}" in sql:
        raise ValueError(f"Unresolved SQL placeholder in {path}")
    connection.execute(sql)
    print(
        f"Completed {path.name} in {time.perf_counter() - started:,.1f} seconds.",
        flush=True,
    )


def main() -> int:
    args = parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(
            f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}"
        )

    dataset_dir = Path(__file__).resolve().parent
    source_dir = Path(args.source_dir).resolve()
    output_root = Path(args.output).resolve()
    work_dir = output_root / "work"
    release_dir = output_root / "release"
    if work_dir.exists() or release_dir.exists():
        raise FileExistsError(
            "Refusing to overwrite an existing work or release directory: "
            f"{output_root}"
        )

    sources = {
        placeholder: source_dir / filename
        for placeholder, filename in REQUIRED_SOURCES.items()
    }
    required = list(sources.values()) + [source_dir / "source_manifest.json"]
    missing = [str(path) for path in required if not path.is_file()]
    if missing:
        raise FileNotFoundError(f"Required source files are missing: {missing}")

    work_dir.mkdir(parents=True)
    release_dir.mkdir(parents=True)
    database = work_dir / "airbnb.duckdb"
    connection = duckdb.connect(str(database))
    try:
        run_sql(
            connection,
            dataset_dir / "sql" / "10_ingest.sql",
            **{key: sql_path(value) for key, value in sources.items()},
        )
        run_sql(connection, dataset_dir / "sql" / "20_model.sql")
        run_sql(connection, dataset_dir / "sql" / "30_validate.sql")

        artifact_paths = {
            "listings": release_dir / "airbnb_listings.parquet",
            "calendar": release_dir / "airbnb_calendar.parquet",
            "reviews_monthly": release_dir / "airbnb_reviews_monthly.parquet",
            "listing_analysis": release_dir / "airbnb_listing_analysis.parquet",
        }
        run_sql(
            connection,
            dataset_dir / "sql" / "40_publish.sql",
            OUTPUT_LISTINGS=sql_path(artifact_paths["listings"]),
            OUTPUT_CALENDAR=sql_path(artifact_paths["calendar"]),
            OUTPUT_REVIEWS_MONTHLY=sql_path(artifact_paths["reviews_monthly"]),
            OUTPUT_LISTING_ANALYSIS=sql_path(artifact_paths["listing_analysis"]),
        )

        generated = {}
        for name, artifact_path in artifact_paths.items():
            rows = connection.execute(
                f"SELECT COUNT(*) FROM curated.{name}"
            ).fetchone()[0]
            schema_rows = connection.execute(
                f"DESCRIBE SELECT * FROM curated.{name}"
            ).fetchall()
            generated[name] = {
                "filename": artifact_path.name,
                "sha256": sha256(artifact_path),
                "bytes": artifact_path.stat().st_size,
                "rows": rows,
                "columns": len(schema_rows),
                "schema": [
                    {
                        "name": row[0],
                        "duckdb_type": row[1],
                        "nullable": row[2] == "YES",
                    }
                    for row in schema_rows
                ],
            }
    finally:
        connection.close()

    definition = json.loads((dataset_dir / "dataset.json").read_text(encoding="utf-8"))
    if definition["license"].get("public_redistribution_approved") is not False:
        raise ValueError("Airbnb candidate must retain the closed publication gate")
    definition["release_status"] = "local-candidate"
    definition["built_at_utc"] = datetime.now(timezone.utc).isoformat()
    definition["build"] = {"duckdb_version": duckdb.__version__}
    for artifact in definition["artifacts"]:
        artifact.update(generated[artifact["name"]])

    (release_dir / "metadata.json").write_text(
        json.dumps(definition, indent=2) + "\n", encoding="utf-8"
    )
    (release_dir / "schemas.json").write_text(
        json.dumps(
            {name: details["schema"] for name, details in generated.items()},
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )
    shutil.copy2(source_dir / "source_manifest.json", release_dir)
    shutil.copy2(dataset_dir / "LICENSE-DATA.txt", release_dir)
    shutil.copy2(dataset_dir / "data_dictionary.md", release_dir / "DATA-DICTIONARY.md")
    for details in generated.values():
        filename = details["filename"]
        (release_dir / f"{filename}.sha256").write_text(
            f"{details['sha256']}  {filename}\n", encoding="ascii"
        )

    print(f"Built local-only release candidate at {release_dir}")
    for name, details in generated.items():
        print(
            f"{name}: {details['rows']:,} rows, {details['columns']} columns, "
            f"{details['bytes']:,} bytes"
        )
    print("Publication gate: CLOSED - Inside Airbnb redistribution is unresolved")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
