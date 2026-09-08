#!/usr/bin/env python3
"""Verify downloaded Airbnb file identity, headers, and basic CSV parsing."""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import json
from pathlib import Path

import duckdb

from build_release import EXPECTED_DUCKDB_VERSION, REQUIRED_SOURCES


LISTING_COLUMNS = {
    "id", "last_scraped", "neighbourhood_cleansed", "latitude", "longitude",
    "property_type", "room_type", "accommodates", "bathrooms",
    "bathrooms_text", "bedrooms", "beds", "amenities", "price",
    "minimum_nights", "maximum_nights", "minimum_minimum_nights",
    "maximum_minimum_nights", "minimum_maximum_nights",
    "maximum_maximum_nights", "minimum_nights_avg_ntm",
    "maximum_nights_avg_ntm", "has_availability", "availability_30",
    "availability_60", "availability_90", "availability_365",
    "number_of_reviews", "number_of_reviews_ltm", "number_of_reviews_l30d",
    "first_review", "last_review", "review_scores_rating",
    "review_scores_accuracy", "review_scores_cleanliness",
    "review_scores_checkin", "review_scores_communication",
    "review_scores_location", "review_scores_value", "license",
    "instant_bookable", "calculated_host_listings_count",
    "calculated_host_listings_count_entire_homes",
    "calculated_host_listings_count_private_rooms",
    "calculated_host_listings_count_shared_rooms",
}
EXPECTED = {
    "LISTINGS": LISTING_COLUMNS,
    "CALENDAR": {
        "listing_id", "date", "available", "minimum_nights", "maximum_nights",
    },
    "REVIEWS": {"listing_id", "date"},
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def header(path: Path) -> list[str]:
    with gzip.open(path, "rt", encoding="utf-8-sig", newline="") as handle:
        return next(csv.reader(handle))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source-dir",
        default=".data-build/airbnb/chicago-twin-cities-2026-06-v1/source",
    )
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(
            f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}"
        )

    source = Path(args.source_dir).resolve()
    manifest = json.loads((source / "source_manifest.json").read_text(encoding="utf-8"))
    manifest_files = {item["filename"]: item for item in manifest["files"]}
    results = {}
    connection = duckdb.connect()
    try:
        for placeholder, filename in REQUIRED_SOURCES.items():
            path = source / filename
            details = manifest_files.get(filename)
            if details is None:
                raise ValueError(f"{filename} is not declared in source_manifest.json")
            if path.stat().st_size != details["bytes"] or sha256(path) != details["sha256"]:
                raise ValueError(f"Source identity mismatch for {filename}")
            columns = header(path)
            kind = next(key for key in EXPECTED if placeholder.endswith(key))
            missing = sorted(EXPECTED[kind] - set(columns))
            if missing:
                raise ValueError(f"{filename} is missing columns: {missing}")
            escaped = path.as_posix().replace("'", "''")
            parsed = connection.execute(
                "SELECT COUNT(*) FROM (SELECT * FROM read_csv(" 
                f"'{escaped}', header=true, all_varchar=true, sample_size=10000, "
                "strict_mode=true, ignore_errors=false, parallel=false) LIMIT 100)"
            ).fetchone()[0]
            if parsed == 0:
                raise ValueError(f"DuckDB parsed no rows from {filename}")
            results[filename] = {
                "bytes": details["bytes"],
                "sha256": details["sha256"],
                "columns": len(columns),
                "sample_rows_parsed": parsed,
            }
    finally:
        connection.close()

    print(json.dumps({"files": results, "status": "smoke-test-passed"}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
