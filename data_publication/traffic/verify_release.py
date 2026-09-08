#!/usr/bin/env python3
"""Independently verify a Chicago Traffic release candidate."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


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
        default=".data-build/traffic/chicago-2018-2025-v1/release",
    )
    args = parser.parse_args()
    release = Path(args.release_directory).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))

    required = [
        release / "metadata.json",
        release / "schemas.json",
        release / "source_manifest.json",
        release / "LICENSE-DATA.txt",
        release / "DATA-DICTIONARY.md",
    ]
    artifacts = {}
    for artifact in metadata["artifacts"]:
        path = release / artifact["filename"]
        required.extend([path, release / f"{path.name}.sha256"])
        artifacts[artifact["name"]] = (artifact, path)
    missing = [str(path) for path in required if not path.is_file()]
    if missing:
        raise FileNotFoundError(f"Release files are missing: {missing}")

    for name, (details, path) in artifacts.items():
        if sha256(path) != details["sha256"]:
            raise ValueError(f"SHA-256 mismatch for {name}")
        if path.stat().st_size != details["bytes"]:
            raise ValueError(f"Byte-size mismatch for {name}")

    connection = duckdb.connect()
    try:
        for name, (_, path) in artifacts.items():
            parquet = path.as_posix().replace("'", "''")
            connection.execute(
                f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{parquet}')"
            )
        checks = {
            "crashes": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT crash_record_id), "
                "MIN(crash_datetime), MAX(crash_datetime) FROM crashes"
            ).fetchone(),
            "vehicles": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT crash_unit_id), "
                "COUNT(*) FILTER (WHERE c.crash_record_id IS NULL) "
                "FROM vehicles v LEFT JOIN crashes c USING (crash_record_id)"
            ).fetchone(),
            "people": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT person_id), "
                "COUNT(*) FILTER (WHERE c.crash_record_id IS NULL) "
                "FROM people p LEFT JOIN crashes c USING (crash_record_id)"
            ).fetchone(),
            "crash_analysis": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT crash_record_id), "
                "COUNT(*) FILTER (WHERE weather_station_id IS NOT NULL), "
                "COUNT(*) FILTER (WHERE zip5 IS NOT NULL) FROM crash_analysis"
            ).fetchone(),
        }
    finally:
        connection.close()

    for name, (details, _) in artifacts.items():
        if checks[name][0] != details["rows"]:
            raise ValueError(f"Row-count mismatch for {name}")
        if checks[name][0] != checks[name][1]:
            raise ValueError(f"Primary-key uniqueness failed for {name}")
    if checks["vehicles"][2] or checks["people"][2]:
        raise ValueError("Vehicle or person records reference a missing crash")
    if checks["crash_analysis"][0] != checks["crashes"][0]:
        raise ValueError("Analysis table is not one row per crash")

    report = {
        "release_directory": str(release),
        "coverage_start": str(checks["crashes"][2]),
        "coverage_end": str(checks["crashes"][3]),
        "weather_match_rate": checks["crash_analysis"][2] / checks["crash_analysis"][0],
        "zip_key_rate": checks["crash_analysis"][3] / checks["crash_analysis"][0],
        "artifacts": {
            name: {
                "rows": details["rows"],
                "columns": details["columns"],
                "bytes": details["bytes"],
                "sha256": details["sha256"],
            }
            for name, (details, _) in artifacts.items()
        },
        "status": "verified",
    }
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
