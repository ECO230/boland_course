#!/usr/bin/env python3
"""Build the local, unpublished Chicago Traffic Parquet release candidate."""

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
    "crashes": "chicago_crashes_2018_2025.csv",
    "vehicles": "chicago_vehicles_2018_2025.csv",
    "people": "chicago_people_2018_2025.csv",
    "zip_regions": "chicago_zip_regions.csv",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source-dir",
        default=".data-build/traffic/chicago-2018-2025-v1/source",
        help="Directory produced by download_sources.py",
    )
    parser.add_argument(
        "--output",
        default=".data-build/traffic/chicago-2018-2025-v1",
        help="Unpublished build root",
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
    elapsed = time.perf_counter() - started
    print(f"Completed {path.name} in {elapsed:,.1f} seconds.", flush=True)


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

    sources = {name: source_dir / filename for name, filename in REQUIRED_SOURCES.items()}
    sources["weather_glob"] = source_dir / "noaa_lcd" / "*.csv"
    required = list(sources.values())[:-1] + [source_dir / "source_manifest.json"]
    missing = [str(path) for path in required if not path.is_file()]
    weather_files = sorted((source_dir / "noaa_lcd").glob("*.csv"))
    if not weather_files:
        missing.append(str(sources["weather_glob"]))
    if missing:
        raise FileNotFoundError(f"Required source files are missing: {missing}")

    work_dir.mkdir(parents=True)
    release_dir.mkdir(parents=True)
    database = work_dir / "traffic.duckdb"
    connection = duckdb.connect(str(database))
    try:
        run_sql(
            connection,
            dataset_dir / "sql" / "10_ingest.sql",
            CRASHES_CSV=sql_path(sources["crashes"]),
            VEHICLES_CSV=sql_path(sources["vehicles"]),
            PEOPLE_CSV=sql_path(sources["people"]),
            ZIP_REGIONS_CSV=sql_path(sources["zip_regions"]),
            WEATHER_GLOB=sql_path(sources["weather_glob"]),
        )
        run_sql(connection, dataset_dir / "sql" / "20_model.sql")
        run_sql(connection, dataset_dir / "sql" / "30_validate.sql")

        artifact_paths = {
            "crashes": release_dir / "chicago_traffic_crashes.parquet",
            "vehicles": release_dir / "chicago_traffic_vehicles.parquet",
            "people": release_dir / "chicago_traffic_people.parquet",
            "crash_analysis": release_dir / "chicago_traffic_crash_analysis.parquet",
        }
        run_sql(
            connection,
            dataset_dir / "sql" / "40_publish.sql",
            OUTPUT_CRASHES=sql_path(artifact_paths["crashes"]),
            OUTPUT_VEHICLES=sql_path(artifact_paths["vehicles"]),
            OUTPUT_PEOPLE=sql_path(artifact_paths["people"]),
            OUTPUT_ANALYSIS=sql_path(artifact_paths["crash_analysis"]),
        )

        generated = {}
        for name, artifact_path in artifact_paths.items():
            rows = connection.execute(f"SELECT COUNT(*) FROM curated.{name}").fetchone()[0]
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
    definition["release_status"] = "local-candidate"
    definition["built_at_utc"] = datetime.now(timezone.utc).isoformat()
    definition["build"] = {"duckdb_version": duckdb.__version__}
    for artifact in definition["artifacts"]:
        artifact.update(generated[artifact["name"]])

    (release_dir / "metadata.json").write_text(
        json.dumps(definition, indent=2) + "\n", encoding="utf-8"
    )
    (release_dir / "schemas.json").write_text(
        json.dumps({name: details["schema"] for name, details in generated.items()}, indent=2)
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

    print(f"Built unpublished release candidate at {release_dir}")
    for name, details in generated.items():
        print(
            f"{name}: {details['rows']:,} rows, {details['columns']} columns, "
            f"{details['bytes']:,} bytes"
        )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
