#!/usr/bin/env python3
"""Independently verify the local Airbnb release candidate and closed gate."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


FORBIDDEN_COLUMNS = {
    "source_listing_id", "listing_id", "host_id", "host_name", "host_url",
    "host_about", "host_location", "reviewer_id", "reviewer_name", "comments",
    "name", "description", "neighborhood_overview", "listing_url", "picture_url",
    "license",
}


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
        default=".data-build/airbnb/chicago-twin-cities-2026-06-v1/release",
    )
    args = parser.parse_args()
    release = Path(args.release_directory).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    schemas = json.loads((release / "schemas.json").read_text(encoding="utf-8"))

    if metadata.get("schema_version") != 2:
        raise ValueError("metadata.json must use schema_version 2")
    if metadata.get("release_status") != "local-candidate":
        raise ValueError("release_status must be local-candidate")
    if metadata.get("privacy", {}).get("contains_student_data") is not False:
        raise ValueError("contains_student_data must be explicitly false")
    if metadata.get("license", {}).get("public_redistribution_approved") is not False:
        raise ValueError("The unresolved redistribution gate must remain closed")

    expected_names = {
        "metadata.json", "schemas.json", "source_manifest.json",
        "LICENSE-DATA.txt", "DATA-DICTIONARY.md",
    }
    artifacts = {}
    relative_dirs = set()
    for artifact in metadata["artifacts"]:
        path = release / artifact["filename"]
        sidecar = release / f"{path.name}.sha256"
        expected_names.update({path.name, sidecar.name})
        artifacts[artifact["name"]] = (artifact, path)
        relative_dirs.add(str(Path(artifact["relative_path"]).parent).replace("\\", "/"))
    if len(relative_dirs) != 1:
        raise ValueError("Artifact paths do not share one versioned release directory")

    entries = list(release.iterdir())
    if any(entry.is_dir() or entry.is_symlink() for entry in entries):
        raise ValueError("Release bundle must be flat and contain no symbolic links")
    actual_names = {entry.name for entry in entries}
    if actual_names != expected_names:
        raise ValueError(
            f"Release file set mismatch; missing={sorted(expected_names - actual_names)}, "
            f"extra={sorted(actual_names - expected_names)}"
        )

    connection = duckdb.connect()
    try:
        for name, (details, path) in artifacts.items():
            actual_hash = sha256(path)
            if actual_hash != details["sha256"]:
                raise ValueError(f"SHA-256 mismatch for {name}")
            if path.stat().st_size != details["bytes"]:
                raise ValueError(f"Byte-size mismatch for {name}")
            sidecar_text = (release / f"{path.name}.sha256").read_text(
                encoding="ascii"
            ).strip().replace("\r", "")
            if sidecar_text != f"{actual_hash}  {path.name}":
                raise ValueError(f"Checksum sidecar mismatch for {name}")
            if schemas.get(name) != details["schema"]:
                raise ValueError(f"schemas.json differs from metadata for {name}")

            parquet = path.as_posix().replace("'", "''")
            connection.execute(
                f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{parquet}')"
            )
            actual_schema = connection.execute(
                f"DESCRIBE SELECT * FROM {name}"
            ).fetchall()
            actual_names_types = [(row[0], row[1]) for row in actual_schema]
            declared_names_types = [
                (column["name"], column["duckdb_type"]) for column in details["schema"]
            ]
            if actual_names_types != declared_names_types:
                raise ValueError(f"Parquet schema differs from metadata for {name}")
            forbidden = FORBIDDEN_COLUMNS & {column[0].lower() for column in actual_names_types}
            if forbidden:
                raise ValueError(f"Forbidden columns in {name}: {sorted(forbidden)}")

        checks = {
            "listings": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT listing_key), "
                "COUNT(*) FILTER (WHERE market_id='chicago'), "
                "COUNT(*) FILTER (WHERE market_id='twin_cities_msa'), "
                "COUNT(*) FILTER (WHERE county_fips IS NULL OR grid_0_01deg IS NULL) "
                "FROM listings"
            ).fetchone(),
            "calendar": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT (listing_key, calendar_date)), "
                "COUNT(*) FILTER (WHERE l.listing_key IS NULL) "
                "FROM calendar c LEFT JOIN listings l USING (listing_key)"
            ).fetchone(),
            "reviews_monthly": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT (listing_key, review_month)), "
                "COUNT(*) FILTER (WHERE l.listing_key IS NULL) "
                "FROM reviews_monthly r LEFT JOIN listings l USING (listing_key)"
            ).fetchone(),
            "listing_analysis": connection.execute(
                "SELECT COUNT(*), COUNT(DISTINCT listing_key), "
                "COUNT(*) FILTER (WHERE future_calendar_days IS NOT NULL) "
                "FROM listing_analysis"
            ).fetchone(),
        }
    finally:
        connection.close()

    for name, (details, _) in artifacts.items():
        if checks[name][0] != details["rows"]:
            raise ValueError(f"Row-count mismatch for {name}")
        if checks[name][0] != checks[name][1]:
            raise ValueError(f"Primary-key uniqueness failed for {name}")
    if checks["listings"][2] < 8000 or checks["listings"][3] < 5000:
        raise ValueError("Pinned market listing counts are unexpectedly low")
    if checks["listings"][4] != 0:
        raise ValueError("Geographic key coverage failed")
    if checks["calendar"][2] or checks["reviews_monthly"][2]:
        raise ValueError("Child table references a missing listing")
    if checks["listing_analysis"][0] != checks["listings"][0]:
        raise ValueError("Analysis table is not one row per listing")

    source_manifest = json.loads(
        (release / "source_manifest.json").read_text(encoding="utf-8")
    )
    if source_manifest.get("redistribution_status") != "unresolved-do-not-publish":
        raise ValueError("Source manifest does not preserve the publication blocker")

    report = {
        "release_directory": str(release),
        "proposed_public_directory": next(iter(relative_dirs)),
        "listing_counts": {
            "total": checks["listings"][0],
            "chicago": checks["listings"][2],
            "twin_cities_msa": checks["listings"][3],
        },
        "artifacts": {
            name: {
                "rows": details["rows"],
                "columns": details["columns"],
                "bytes": details["bytes"],
                "sha256": details["sha256"],
            }
            for name, (details, _) in artifacts.items()
        },
        "privacy": "verified curated schemas contain no forbidden identity/free-text fields",
        "publication_gate": "CLOSED - Inside Airbnb redistribution unresolved",
        "status": "verified-local-candidate-not-approved-for-publication",
    }
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
