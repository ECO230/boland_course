#!/usr/bin/env python3
"""Run ingestion and modeling SQL against a small downloader smoke snapshot."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import duckdb

from build_release import REQUIRED_SOURCES, run_sql, sql_path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source-dir",
        default=".data-build/traffic/chicago-smoke/source",
    )
    args = parser.parse_args()
    dataset_dir = Path(__file__).resolve().parent
    source_dir = Path(args.source_dir).resolve()
    sources = {name: source_dir / filename for name, filename in REQUIRED_SOURCES.items()}

    connection = duckdb.connect()
    try:
        run_sql(
            connection,
            dataset_dir / "sql" / "10_ingest.sql",
            CRASHES_CSV=sql_path(sources["crashes"]),
            VEHICLES_CSV=sql_path(sources["vehicles"]),
            PEOPLE_CSV=sql_path(sources["people"]),
            ZIP_REGIONS_CSV=sql_path(sources["zip_regions"]),
            WEATHER_GLOB=sql_path(source_dir / "noaa_lcd" / "*.csv"),
        )
        run_sql(connection, dataset_dir / "sql" / "20_model.sql")
        result = connection.execute(
            """
            SELECT
              (SELECT COUNT(*) FROM curated.crashes) AS crashes,
              (SELECT COUNT(*) FROM curated.vehicles) AS vehicles,
              (SELECT COUNT(*) FROM curated.people) AS people,
              (SELECT COUNT(*) FROM curated.crash_analysis) AS analysis_rows,
              (SELECT COUNT(*) FILTER (WHERE zip5 IS NOT NULL)
                 FROM curated.crash_analysis) AS zip_matches,
              (SELECT COUNT(*) FILTER (WHERE weather_station_id IS NOT NULL)
                 FROM curated.crash_analysis) AS weather_matches
            """
        ).fetchone()
    finally:
        connection.close()

    if result[0] != result[3]:
        raise ValueError("Smoke test did not produce one analysis row per crash")
    print(
        json.dumps(
            {
                "crashes": result[0],
                "vehicles": result[1],
                "people": result[2],
                "analysis_rows": result[3],
                "zip_matches": result[4],
                "weather_matches": result[5],
                "status": "smoke-test-passed",
            },
            indent=2,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
