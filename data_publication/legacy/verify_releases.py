#!/usr/bin/env python3
"""Independently verify one or more flat legacy release bundles."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
REQUIRED_STATIC = {
    "metadata.json", "schemas.json", "source_manifest.json",
    "DATA-DICTIONARY.md", "LICENSE-DATA.txt"
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def verify(release: Path) -> None:
    if not release.is_dir():
        raise FileNotFoundError(release)
    if any(path.is_dir() or path.is_symlink() for path in release.iterdir()):
        raise ValueError(f"Bundle is not flat or contains a symbolic link: {release}")
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    schemas = json.loads((release / "schemas.json").read_text(encoding="utf-8"))
    if metadata.get("schema_version") != 2 or metadata.get("release_status") != "local-candidate":
        raise ValueError(f"Invalid metadata contract: {release}")
    if metadata.get("privacy", {}).get("contains_student_data") is not False:
        raise ValueError(f"Student-data assessment must explicitly be false: {release}")
    artifacts = metadata.get("artifacts", [])
    expected = set(REQUIRED_STATIC)
    for artifact in artifacts:
        expected.add(artifact["filename"])
        expected.add(f"{artifact['filename']}.sha256")
    actual = {path.name for path in release.iterdir()}
    if actual != expected:
        raise ValueError(f"Bundle file mismatch in {release}: missing={expected-actual}, extra={actual-expected}")
    connection = duckdb.connect()
    try:
        for artifact in artifacts:
            path = release / artifact["filename"]
            digest = sha256(path)
            if digest != artifact["sha256"]:
                raise ValueError(f"Metadata checksum mismatch: {path.name}")
            sidecar = (release / f"{path.name}.sha256").read_text(encoding="ascii").strip().split()
            if sidecar != [digest, path.name]:
                raise ValueError(f"Checksum sidecar mismatch: {path.name}")
            if path.stat().st_size != artifact["bytes"]:
                raise ValueError(f"Byte count mismatch: {path.name}")
            query_path = path.resolve().as_posix().replace("'", "''")
            rows = connection.execute(f"SELECT count(*) FROM read_parquet('{query_path}')").fetchone()[0]
            keys = connection.execute(
                f"SELECT count(DISTINCT legacy_source_index) FROM read_parquet('{query_path}')"
            ).fetchone()[0]
            if rows != artifact["rows"] or rows != keys or rows < 1:
                raise ValueError(f"Row count or key mismatch: {path.name}")
            schema_rows = connection.execute(
                f"DESCRIBE SELECT * FROM read_parquet('{query_path}')"
            ).fetchall()
            schema = [
                {"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"}
                for row in schema_rows
            ]
            if schema != artifact["schema"] or schema != schemas[artifact["name"]]:
                raise ValueError(f"Schema mismatch: {path.name}")
            print(f"Verified {path.name}: {rows:,} rows, {path.stat().st_size:,} bytes")
    finally:
        connection.close()
    approval = metadata["license"]["public_redistribution_approved"]
    print(f"Verified bundle: {release} (public redistribution approved: {approval})")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("release", nargs="+")
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}")
    for value in args.release:
        verify(Path(value))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Verification failed: {error}", file=sys.stderr)
        raise SystemExit(1)
