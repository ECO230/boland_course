#!/usr/bin/env python3
"""Build deterministic CSV extracts for ECO 230 Homework 1-4.

The script reads the approved local Parquet release artifacts. It does not
download or publish data. Each SQL query limits scope first, then selects a
stable 15,000-row sample by ordering on an MD5 hash of the primary key and a
versioned salt.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
DEFAULT_ROW_LIMIT = 15_000
SAMPLE_SALT = "eco230-early-homework-v1"


@dataclass(frozen=True)
class Sample:
    key: str
    filename: str
    sql_file: str
    sources: dict[str, str]
    grain: str
    scope: str
    primary_key: str


SAMPLES = {
    sample.key: sample
    for sample in (
        Sample(
            key="traffic",
            filename="traffic_chicago_2024.csv",
            sql_file="traffic_chicago_2024.sql",
            sources={
                "SOURCE": (
                    ".data-build/traffic/chicago-2018-2025-v1/release/"
                    "chicago_traffic_crash_analysis.parquet"
                )
            },
            grain="One police-reported crash",
            scope="Chicago crashes during calendar year 2024",
            primary_key="crash_id",
        ),
        Sample(
            key="airline",
            filename="airline_midwest_2024.csv",
            sql_file="airline_midwest_2024.sql",
            sources={
                "SOURCE": (
                    ".data-build/airline/us-airline-marketplace-2024-v1/"
                    "release/airline_flights.parquet"
                )
            },
            grain="One scheduled domestic flight",
            scope="2024 departures from ORD, MDW, MKE, or MSP",
            primary_key="flight_id",
        ),
        Sample(
            key="olist",
            filename="olist_southeast_2017.csv",
            sql_file="olist_southeast_2017.sql",
            sources={
                "SOURCE": (
                    ".data-build/olist/brazilian-ecommerce-2016-2018-v1/"
                    "release/olist_order_analysis.parquet"
                )
            },
            grain="One delivered marketplace order",
            scope="2017 delivered orders for customers in SP, RJ, or MG",
            primary_key="order_id",
        ),
        Sample(
            key="nfl",
            filename="nfl_plays_2024.csv",
            sql_file="nfl_plays_2024.sql",
            sources={
                "SOURCE": (
                    ".data-build/nfl/nfl-complete-2021-2025-v1/release/"
                    "nfl_plays.parquet"
                )
            },
            grain="One recorded play",
            scope="2024 NFL regular season plays",
            primary_key="play_id",
        ),
        Sample(
            key="health",
            filename="health_wisconsin_appointments_2023_2025.csv",
            sql_file="health_wisconsin_appointments_2023_2025.sql",
            sources={
                "APPOINTMENTS_SOURCE": (
                    ".data-build/health/great-lakes-synthetic-ehr-2026-public-v1/"
                    "release/ehr_appointments.parquet"
                ),
                "PATIENTS_SOURCE": (
                    ".data-build/health/great-lakes-synthetic-ehr-2026-public-v1/"
                    "release/ehr_patients.parquet"
                ),
                "SATISFACTION_SOURCE": (
                    ".data-build/health/great-lakes-synthetic-ehr-2026-public-v1/"
                    "release/ehr_patient_satisfaction.parquet"
                ),
            },
            grain="One scheduled synthetic appointment",
            scope="Wisconsin appointments scheduled from 2023 through 2025",
            primary_key="appointment_id",
        ),
    )
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--sample",
        action="append",
        choices=sorted(SAMPLES),
        help="Build only this sample; repeat to select more than one",
    )
    parser.add_argument(
        "--row-limit",
        type=int,
        default=DEFAULT_ROW_LIMIT,
        help=f"Maximum rows per CSV (default: {DEFAULT_ROW_LIMIT})",
    )
    parser.add_argument(
        "--output",
        default="shared/data/early-homework-v1",
        help="Output directory for CSV files and manifest.json",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Atomically replace an existing output directory",
    )
    return parser.parse_args()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def render_sql(template: str, values: dict[str, str]) -> str:
    for key, value in values.items():
        template = template.replace("{{" + key + "}}", value)
    if "{{" in template or "}}" in template:
        raise ValueError("Unresolved SQL placeholder remains")
    return template


def main() -> int:
    args = parse_args()
    if args.row_limit < 1:
        raise ValueError("--row-limit must be positive")
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(
            f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}"
        )

    script_dir = Path(__file__).resolve().parent
    repo_root = script_dir.parents[1]
    output_dir = (repo_root / args.output).resolve()
    temp_dir = output_dir.with_name(output_dir.name + ".building")

    if temp_dir.exists():
        shutil.rmtree(temp_dir)
    if output_dir.exists() and not args.force:
        raise FileExistsError(
            f"Output already exists: {output_dir}. Pass --force to replace it."
        )
    temp_dir.mkdir(parents=True)

    selected_keys = args.sample or list(SAMPLES)
    connection = duckdb.connect()
    artifacts: list[dict[str, object]] = []
    try:
        for key in selected_keys:
            sample = SAMPLES[key]
            source_paths = {
                token: (repo_root / relative_path).resolve()
                for token, relative_path in sample.sources.items()
            }
            missing = [str(path) for path in source_paths.values() if not path.is_file()]
            if missing:
                raise FileNotFoundError(
                    f"Missing approved source artifact(s) for {key}: {missing}"
                )

            output_path = temp_dir / sample.filename
            sql = (script_dir / "sql" / sample.sql_file).read_text(encoding="utf-8")
            values = {
                token: sql_path(path) for token, path in source_paths.items()
            }
            values.update(
                {
                    "OUTPUT": sql_path(output_path),
                    "ROW_LIMIT": str(args.row_limit),
                    "SAMPLE_SALT": SAMPLE_SALT,
                }
            )
            connection.execute(render_sql(sql, values))

            row_count = connection.execute(
                "SELECT count(*) FROM read_csv_auto(?, header = true)",
                [str(output_path)],
            ).fetchone()[0]
            columns = connection.execute(
                "DESCRIBE SELECT * FROM read_csv_auto(?, header = true)",
                [str(output_path)],
            ).fetchall()
            if row_count != args.row_limit:
                raise RuntimeError(
                    f"{key} produced {row_count:,} rows; expected {args.row_limit:,}"
                )
            if connection.execute(
                f"SELECT count(*) - count(DISTINCT {sample.primary_key}) "
                "FROM read_csv_auto(?, header = true)",
                [str(output_path)],
            ).fetchone()[0] != 0:
                raise RuntimeError(f"{key} primary key is not unique")

            details = {
                "key": key,
                "filename": sample.filename,
                "rows": row_count,
                "columns": len(columns),
                "bytes": output_path.stat().st_size,
                "sha256": sha256(output_path),
                "grain": sample.grain,
                "scope": sample.scope,
                "primary_key": sample.primary_key,
                "sampling": (
                    f"First {args.row_limit:,} eligible rows ordered by "
                    f"MD5(primary key + '{SAMPLE_SALT}')"
                ),
                "source_artifacts": list(sample.sources.values()),
            }
            artifacts.append(details)
            print(
                f"{sample.filename}: {row_count:,} rows, {len(columns)} columns, "
                f"{details['bytes']:,} bytes"
            )
    finally:
        connection.close()

    manifest = {
        "schema_version": 1,
        "dataset_id": "eco230_early_homework_samples",
        "version": "early-homework-v1",
        "duckdb_version": duckdb.__version__,
        "deterministic_salt": SAMPLE_SALT,
        "artifacts": artifacts,
    }
    (temp_dir / "manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )

    if output_dir.exists():
        shutil.rmtree(output_dir)
    temp_dir.replace(output_dir)
    print(f"Built deterministic teaching extracts at {output_dir}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
