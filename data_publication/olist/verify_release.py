#!/usr/bin/env python3
"""Independently verify a flat Olist local-candidate release bundle."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


FORBIDDEN = {"order_id", "customer_id", "customer_unique_id", "product_id", "seller_id", "review_id", "mql_id", "sdr_id", "sr_id", "review_comment_title", "review_comment_message", "geolocation_lat", "geolocation_lng"}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("release_directory", nargs="?", default=".data-build/olist/brazilian-ecommerce-2016-2018-v1/release")
    args = parser.parse_args()
    release = Path(args.release_directory).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    schemas = json.loads((release / "schemas.json").read_text(encoding="utf-8"))
    if metadata.get("schema_version") != 2 or metadata.get("release_status") != "local-candidate":
        raise ValueError("Invalid schema_version or release_status")
    if metadata.get("privacy", {}).get("contains_student_data") is not False:
        raise ValueError("contains_student_data must be explicitly false")
    if metadata.get("license", {}).get("public_redistribution_approved") is not True:
        raise ValueError("Redistribution approval is not recorded")

    expected = {"metadata.json", "schemas.json", "source_manifest.json", "LICENSE-DATA.txt", "DATA-DICTIONARY.md"}
    relative_dirs, artifacts = set(), {}
    for details in metadata["artifacts"]:
        path = release / details["filename"]
        expected.update({path.name, f"{path.name}.sha256"})
        relative_dirs.add(str(Path(details["relative_path"]).parent).replace("\\", "/"))
        artifacts[details["name"]] = (details, path)
    entries = list(release.iterdir())
    if any(entry.is_dir() or entry.is_symlink() for entry in entries):
        raise ValueError("Release must be flat with no symbolic links")
    if {entry.name for entry in entries} != expected:
        raise ValueError("Release file set does not match metadata")
    if len(relative_dirs) != 1:
        raise ValueError("Artifacts do not share one versioned release directory")

    connection = duckdb.connect()
    try:
        for name, (details, path) in artifacts.items():
            digest = sha256(path)
            if digest != details["sha256"] or path.stat().st_size != details["bytes"]:
                raise ValueError(f"Hash or byte mismatch for {name}")
            sidecar = (release / f"{path.name}.sha256").read_text(encoding="ascii").strip().replace("\r", "")
            if sidecar != f"{digest}  {path.name}":
                raise ValueError(f"Checksum sidecar mismatch for {name}")
            parquet = path.as_posix().replace("'", "''")
            connection.execute(f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{parquet}')")
            actual = connection.execute(f"DESCRIBE SELECT * FROM {name}").fetchall()
            actual_schema = [(row[0], row[1]) for row in actual]
            declared_schema = [(col["name"], col["duckdb_type"]) for col in details["schema"]]
            if actual_schema != declared_schema or schemas[name] != details["schema"]:
                raise ValueError(f"Schema mismatch for {name}")
            prohibited = FORBIDDEN & {row[0].lower() for row in actual}
            if prohibited:
                raise ValueError(f"Forbidden source fields in {name}: {sorted(prohibited)}")
            rows = connection.execute(f"SELECT count(*) FROM {name}").fetchone()[0]
            if rows != details["rows"]:
                raise ValueError(f"Row count mismatch for {name}")

        checks = {
            "orders": connection.execute("SELECT count(*), count(DISTINCT order_key) FROM orders").fetchone(),
            "items": connection.execute("SELECT count(*), count(DISTINCT (order_key, order_item_number)), count(*) FILTER (WHERE o.order_key IS NULL) FROM order_items i LEFT JOIN orders o USING(order_key)").fetchone(),
            "payments": connection.execute("SELECT count(*), count(DISTINCT (order_key, payment_sequence)), count(*) FILTER (WHERE o.order_key IS NULL) FROM payments p LEFT JOIN orders o USING(order_key)").fetchone(),
            "analysis": connection.execute("SELECT count(*), count(DISTINCT order_key) FROM order_analysis").fetchone(),
        }
    finally:
        connection.close()
    if any(values[0] != values[1] for values in checks.values()):
        raise ValueError("Primary-key verification failed")
    if checks["items"][2] or checks["payments"][2]:
        raise ValueError("Referential integrity verification failed")
    if checks["orders"][0] != checks["analysis"][0]:
        raise ValueError("Order analysis row count differs from orders")

    report = {
        "release_directory": str(release), "proposed_public_directory": next(iter(relative_dirs)),
        "artifacts": {name: {k: details[k] for k in ("rows", "columns", "bytes", "sha256")} for name, (details, _) in artifacts.items()},
        "privacy": "verified no source identifiers, exact coordinates, representative IDs, or review text",
        "license": "CC BY-NC-SA 4.0 noncommercial attribution/share-alike",
        "status": "verified-local-candidate",
    }
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
