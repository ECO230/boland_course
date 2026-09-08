#!/usr/bin/env python3
"""Build flat Parquet release candidates from the four legacy group projects."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import sys
import zipfile
from datetime import datetime, timezone
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def clean_name(value: str) -> str:
    value = re.sub(r"[^0-9A-Za-z]+", "_", value).strip("_").lower()
    return value or "column"


def quoted(value: str) -> str:
    return '"' + value.replace('"', '""') + '"'


def extract_source(repo: Path, spec: dict, destination: Path) -> None:
    zip_path = repo / spec["zip_path"]
    if not zip_path.is_file():
        raise FileNotFoundError(zip_path)
    with zipfile.ZipFile(zip_path) as archive:
        try:
            member = archive.getinfo(spec["zip_member"])
        except KeyError as error:
            raise FileNotFoundError(f"{spec['zip_member']} in {zip_path}") from error
        with archive.open(member) as source, destination.open("wb") as target:
            shutil.copyfileobj(source, target)
    actual = sha256(destination)
    if actual != spec["expected_source_sha256"]:
        raise ValueError(
            f"Source checksum changed for {spec['slug']}: expected "
            f"{spec['expected_source_sha256']}, found {actual}"
        )


def source_columns(connection: duckdb.DuckDBPyConnection, source: Path) -> list[tuple[str, str]]:
    path = sql_path(source)
    rows = connection.execute(
        f"DESCRIBE SELECT * FROM read_csv_auto('{path}', sample_size=-1, ignore_errors=false)"
    ).fetchall()
    return [(row[0], row[1]) for row in rows]


def model_query(spec: dict, columns: list[tuple[str, str]], source: Path) -> str:
    used: set[str] = set()
    selections: list[str] = []
    excluded = {clean_name(item) for item in spec.get("excluded_columns", [])}
    for original, _ in columns:
        cleaned = clean_name(original)
        if cleaned in excluded:
            continue
        if cleaned == "column00":
            cleaned = "legacy_source_index"
        if cleaned in used:
            suffix = 2
            candidate = f"{cleaned}_{suffix}"
            while candidate in used:
                suffix += 1
                candidate = f"{cleaned}_{suffix}"
            cleaned = candidate
        used.add(cleaned)
        if spec["slug"] == "hospital" and cleaned in {
            "average_covered_charges", "average_total_payments", "average_medicare_payments"
        }:
            selections.append(
                f"TRY_CAST(replace(replace({quoted(original)}, '$', ''), ',', '') AS DECIMAL(18,2)) "
                f"AS {quoted(cleaned)}"
            )
        else:
            selections.append(f"{quoted(original)} AS {quoted(cleaned)}")
    if "legacy_source_index" not in used:
        selections.insert(0, "row_number() OVER () - 1 AS legacy_source_index")
    return (
        "SELECT\n  " + ",\n  ".join(selections) +
        f"\nFROM read_csv_auto('{sql_path(source)}', sample_size=-1, ignore_errors=false)"
    )


def build_one(repo: Path, definition_dir: Path, output_base: Path, release_version: str, spec: dict) -> Path:
    output_root = output_base / f"legacy-{spec['slug']}-2026-fall-v1"
    work_dir = output_root / "work"
    release_dir = output_root / "release"
    if output_root.exists():
        raise FileExistsError(f"Refusing to overwrite existing build: {output_root}")
    work_dir.mkdir(parents=True)
    release_dir.mkdir()
    source = work_dir / spec["source_filename"]
    extract_source(repo, spec, source)
    artifact = release_dir / spec["artifact_filename"]
    connection = duckdb.connect()
    try:
        columns = source_columns(connection, source)
        query = model_query(spec, columns, source)
        connection.execute(
            f"COPY ({query}) TO '{sql_path(artifact)}' "
            "(FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000)"
        )
        artifact_schema_rows = connection.execute(
            f"DESCRIBE SELECT * FROM read_parquet('{sql_path(artifact)}')"
        ).fetchall()
        rows = connection.execute(
            f"SELECT count(*) FROM read_parquet('{sql_path(artifact)}')"
        ).fetchone()[0]
        distinct_keys = connection.execute(
            f"SELECT count(DISTINCT legacy_source_index) FROM read_parquet('{sql_path(artifact)}')"
        ).fetchone()[0]
        if rows < 1 or rows != distinct_keys:
            raise ValueError(f"Invalid row count or key uniqueness for {spec['slug']}")
    finally:
        connection.close()

    schema = [
        {"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"}
        for row in artifact_schema_rows
    ]
    artifact_hash = sha256(artifact)
    relative_dir = str(Path(spec["relative_path"]).parent).replace("\\", "/")
    metadata = {
        "schema_version": 2,
        "dataset_id": spec["dataset_id"],
        "title": spec["title"],
        "description": spec["description"],
        "project": "project1",
        "topic": f"legacy-{spec['slug']}",
        "release_version": release_version,
        "release_status": "local-candidate",
        "built_at_utc": datetime.now(timezone.utc).isoformat(),
        "legacy_status": "backup-only; superseded for new course projects",
        "source": {
            "url": spec["source_url"],
            "license": spec["source_license"],
            "snapshot_sha256": spec["expected_source_sha256"]
        },
        "privacy": {
            "contains_student_data": False,
            "assessment": spec["privacy_assessment"]
        },
        "license": {
            "public_redistribution_approved": spec["public_redistribution_approved"],
            "approval_gate": spec["approval_gate"],
            "notice_file": "LICENSE-DATA.txt"
        },
        "artifacts": [{
            "name": spec["slug"],
            "grain": spec["grain"],
            "primary_key": "legacy_source_index",
            "relative_path": spec["relative_path"],
            "public_url": f"https://data.60land.com/{spec['relative_path']}",
            "filename": artifact.name,
            "sha256": artifact_hash,
            "bytes": artifact.stat().st_size,
            "rows": rows,
            "columns": len(schema),
            "schema": schema
        }],
        "release_directory": relative_dir,
        "build": {"duckdb_version": duckdb.__version__}
    }
    source_manifest = {
        "schema_version": 1,
        "dataset_id": spec["dataset_id"],
        "source_archive": spec["zip_path"],
        "source_archive_sha256": sha256(repo / spec["zip_path"]),
        "source_member": spec["zip_member"],
        "source_filename": spec["source_filename"],
        "source_member_sha256": spec["expected_source_sha256"],
        "source_url": spec["source_url"],
        "source_license": spec["source_license"],
        "provenance_note": "Built from the full legacy Posit Cloud group-project export, not from shared/data classroom subsets."
    }
    (release_dir / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    (release_dir / "schemas.json").write_text(
        json.dumps({spec["slug"]: schema}, indent=2) + "\n", encoding="utf-8"
    )
    (release_dir / "source_manifest.json").write_text(
        json.dumps(source_manifest, indent=2) + "\n", encoding="utf-8"
    )
    shutil.copy2(definition_dir / "licenses" / f"{spec['slug']}.txt", release_dir / "LICENSE-DATA.txt")
    shutil.copy2(definition_dir / "dictionaries" / f"{spec['slug']}.md", release_dir / "DATA-DICTIONARY.md")
    (release_dir / f"{artifact.name}.sha256").write_text(
        f"{artifact_hash}  {artifact.name}\n", encoding="ascii"
    )
    print(
        f"Built {spec['slug']}: {rows:,} rows, {artifact.stat().st_size:,} bytes -> {release_dir}"
    )
    return release_dir


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", default=".data-build/legacy")
    parser.add_argument("--only", action="append", choices=["airbnb", "traffic", "hospital", "nfl"])
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}")
    definition_dir = Path(__file__).resolve().parent
    repo = definition_dir.parents[1]
    config = json.loads((definition_dir / "datasets.json").read_text(encoding="utf-8"))
    selected = set(args.only or [])
    for spec in config["datasets"]:
        if selected and spec["slug"] not in selected:
            continue
        build_one(repo, definition_dir, repo / args.output, config["release_version"], spec)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
