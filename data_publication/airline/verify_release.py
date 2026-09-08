#!/usr/bin/env python3
"""Independently verify a flat 2024 airline local-candidate bundle."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


FORBIDDEN = {"tail_number", "passenger_name", "ticket_number", "customer_id"}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("release_directory", nargs="?", default=".data-build/airline/us-airline-marketplace-2024-v1/release")
    args = parser.parse_args()
    release = Path(args.release_directory).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    schemas = json.loads((release / "schemas.json").read_text(encoding="utf-8"))
    if metadata.get("schema_version") != 2 or metadata.get("release_status") != "local-candidate":
        raise ValueError("Invalid schema_version or release_status")
    if metadata.get("privacy", {}).get("contains_student_data") is not False:
        raise ValueError("contains_student_data must be explicitly false")

    expected = {"metadata.json", "schemas.json", "source_manifest.json", "LICENSE-DATA.txt", "DATA-DICTIONARY.md"}
    relative_dirs, artifacts = set(), {}
    for details in metadata["artifacts"]:
        path = release / details["filename"]
        expected.update({path.name, f"{path.name}.sha256"})
        relative_dirs.add(str(Path(details["relative_path"]).parent).replace("\\", "/"))
        artifacts[details["name"]] = (details, path)
    entries = list(release.iterdir())
    if any(entry.is_dir() or entry.is_symlink() for entry in entries) or {entry.name for entry in entries} != expected:
        raise ValueError("Release must be flat and contain exactly the declared files")
    if len(relative_dirs) != 1:
        raise ValueError("Artifacts do not share one release directory")

    con = duckdb.connect()
    try:
        for name, (details, path) in artifacts.items():
            digest = sha256(path)
            if digest != details["sha256"] or path.stat().st_size != details["bytes"]:
                raise ValueError(f"Hash or size mismatch for {name}")
            sidecar = (release / f"{path.name}.sha256").read_text(encoding="ascii").strip().replace("\r", "")
            if sidecar != f"{digest}  {path.name}":
                raise ValueError(f"Sidecar mismatch for {name}")
            parquet = path.as_posix().replace("'", "''")
            con.execute(f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{parquet}')")
            actual = con.execute(f"DESCRIBE SELECT * FROM {name}").fetchall()
            if [(r[0], r[1]) for r in actual] != [(c["name"], c["duckdb_type"]) for c in details["schema"]] or schemas[name] != details["schema"]:
                raise ValueError(f"Schema mismatch for {name}")
            if FORBIDDEN & {r[0].lower() for r in actual}:
                raise ValueError(f"Forbidden field in {name}")
            if con.execute(f"SELECT count(*) FROM {name}").fetchone()[0] != details["rows"]:
                raise ValueError(f"Row count mismatch for {name}")
        checks = {
            "flights": con.execute("SELECT count(*), count(DISTINCT flight_key), min(flight_date), max(flight_date), count(DISTINCT month_number) FROM flights").fetchone(),
            "airports": con.execute("SELECT count(*), count(DISTINCT airport_id) FROM airports").fetchone(),
            "routes": con.execute("SELECT count(*), count(DISTINCT (year, quarter, origin_airport_code, destination_airport_code, ticketing_carrier_code)) FROM route_quarter").fetchone(),
            "carriers": con.execute("SELECT count(*), count(DISTINCT (month, marketing_carrier_code)) FROM carrier_month").fetchone(),
        }
    finally:
        con.close()
    if checks["flights"][0] != checks["flights"][1] or checks["flights"][2:] != (duckdb.sql("SELECT DATE '2024-01-01'").fetchone()[0], duckdb.sql("SELECT DATE '2024-12-31'").fetchone()[0], 12):
        raise ValueError("Flight coverage or primary-key check failed")
    if any(values[0] != values[1] for values in (checks["airports"], checks["routes"], checks["carriers"])):
        raise ValueError("Aggregate primary-key check failed")
    report = {"release_directory": str(release), "proposed_public_directory": next(iter(relative_dirs)), "artifacts": {name: {k: d[k] for k in ("rows", "columns", "bytes", "sha256")} for name, (d, _) in artifacts.items()}, "privacy": "verified no passenger records or raw aircraft tail numbers", "status": "verified-local-candidate"}
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
