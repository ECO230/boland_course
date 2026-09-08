#!/usr/bin/env python3
"""Independently verify the flat U.S. ZIP reference candidate bundle."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
EXPECTED_ROWS = 42368


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "release_directory",
        nargs="?",
        default=".data-build/zip-reference/us-zip-reference-2026-fall-v1/release",
    )
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(
            f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}"
        )

    release = Path(args.release_directory).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    schemas = json.loads((release / "schemas.json").read_text(encoding="utf-8"))
    if metadata.get("schema_version") != 2 or metadata.get("release_status") != "local-candidate":
        raise ValueError("Invalid schema_version or release_status")
    if metadata.get("privacy", {}).get("contains_student_data") is not False:
        raise ValueError("contains_student_data must be explicitly false")
    if metadata.get("license", {}).get("public_redistribution_approved") is not False:
        raise ValueError("This candidate must retain the unresolved redistribution gate")

    expected = {
        "metadata.json",
        "schemas.json",
        "source_manifest.json",
        "LICENSE-DATA.txt",
        "DATA-DICTIONARY.md",
    }
    relative_dirs, artifacts = set(), {}
    for details in metadata["artifacts"]:
        path = release / details["filename"]
        expected.update({path.name, f"{path.name}.sha256"})
        relative_dirs.add(str(Path(details["relative_path"]).parent).replace("\\", "/"))
        artifacts[details["name"]] = (details, path)
    entries = list(release.iterdir())
    if any(entry.is_dir() or entry.is_symlink() for entry in entries):
        raise ValueError("Release must be flat and contain no symbolic links")
    if {entry.name for entry in entries} != expected:
        raise ValueError("Release does not contain exactly the declared files")
    if len(relative_dirs) != 1:
        raise ValueError("Artifacts do not share one release directory")

    connection = duckdb.connect()
    verified = {}
    try:
        for name, (details, path) in artifacts.items():
            digest = sha256(path)
            if digest != details["sha256"] or path.stat().st_size != details["bytes"]:
                raise ValueError(f"Hash or size mismatch for {name}")
            sidecar = (
                release / f"{path.name}.sha256"
            ).read_text(encoding="ascii").strip().replace("\r", "")
            if sidecar != f"{digest}  {path.name}":
                raise ValueError(f"Sidecar mismatch for {name}")
            parquet = path.as_posix().replace("'", "''")
            connection.execute(
                f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{parquet}')"
            )
            actual = connection.execute(f"DESCRIBE SELECT * FROM {name}").fetchall()
            actual_schema = [
                {"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"}
                for row in actual
            ]
            if actual_schema != details["schema"] or schemas[name] != details["schema"]:
                raise ValueError(f"Schema mismatch for {name}")
            rows = connection.execute(f"SELECT count(*) FROM {name}").fetchone()[0]
            distinct = connection.execute(
                f"SELECT count(DISTINCT zip5) FROM {name}"
            ).fetchone()[0]
            if rows != EXPECTED_ROWS or rows != details["rows"] or rows != distinct:
                raise ValueError(f"Row-count or ZIP-key mismatch for {name}")
            if connection.execute(
                f"SELECT count(*) FROM {name} WHERE NOT regexp_full_match(zip5, '[0-9]{{5}}') OR zip5 = '00000'"
            ).fetchone()[0]:
                raise ValueError(f"Invalid ZIP key in {name}")
            verified[name] = {
                "rows": rows,
                "columns": len(actual),
                "bytes": path.stat().st_size,
                "sha256": digest,
            }

        key_difference = connection.execute(
            "SELECT count(*) FROM ((SELECT zip5 FROM zip_geography EXCEPT SELECT zip5 FROM zip_context) "
            "UNION ALL (SELECT zip5 FROM zip_context EXCEPT SELECT zip5 FROM zip_geography))"
        ).fetchone()[0]
        if key_difference:
            raise ValueError("Geography and context ZIP keys differ")
        coverage = connection.execute(
            "SELECT count(population_2023), count(median_age_group_2023), "
            "count(median_commute_time_category), count(median_housing_vintage_category_2022), "
            "count(ruca_primary_code_2010) FROM zip_context"
        ).fetchone()
    finally:
        connection.close()

    report = {
        "release_directory": str(release),
        "proposed_public_directory": next(iter(relative_dirs)),
        "artifacts": verified,
        "non_null_context_coverage": {
            "population_2023": coverage[0],
            "median_age_group_2023": coverage[1],
            "median_commute_time_category": coverage[2],
            "median_housing_vintage_category_2022": coverage[3],
            "ruca_primary_code_2010": coverage[4],
        },
        "privacy": "verified aggregate ZIP reference fields with no student data",
        "publication_gate": "closed: source provenance and redistribution rights are incomplete",
        "status": "verified-local-candidate-not-approved-for-publication",
    }
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
