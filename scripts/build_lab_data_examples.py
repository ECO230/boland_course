#!/usr/bin/env python3
"""Build small course-data examples for lab preparation and in-class labs."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile, ZipInfo

import duckdb


ROOT = Path(__file__).resolve().parent.parent
RELEASE_VERSION = "2026-fall-v1"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def write_json(path: Path, value: dict) -> None:
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def build_lab_1_prep(connection: duckdb.DuckDBPyConnection) -> None:
    source = (
        ROOT
        / ".data-build"
        / "olist"
        / "brazilian-ecommerce-2016-2018-v1"
        / "release"
        / "olist_order_analysis.parquet"
    )
    output_dir = ROOT / "week01" / "lab-prep" / "data"
    output_dir.mkdir(parents=True, exist_ok=True)
    output = output_dir / "olist_orders_sample.csv"
    connection.execute(
        f"""
        COPY (
          WITH ranked AS (
            SELECT
              order_key AS order_id,
              CAST(purchased_at AS DATE) AS purchase_date,
              customer_state,
              order_status,
              item_count,
              distinct_category_count,
              CAST(item_value_brl AS DECIMAL(12, 2)) AS item_value_brl,
              CAST(freight_value_brl AS DECIMAL(12, 2)) AS freight_value_brl,
              payment_type_count,
              maximum_installments,
              mean_review_score,
              delivery_days,
              delivered_late,
              row_number() OVER (
                PARTITION BY customer_state
                ORDER BY hash(order_key)
              ) AS sample_rank
            FROM read_parquet('{sql_path(source)}')
            WHERE customer_state IN ('SP', 'RJ', 'MG', 'RS', 'PR', 'SC')
              AND order_status = 'delivered'
              AND purchased_at IS NOT NULL
              AND item_value_brl IS NOT NULL
          )
          SELECT * EXCLUDE sample_rank
          FROM ranked
          WHERE sample_rank <= 100
          ORDER BY customer_state, purchase_date, order_id
        ) TO '{sql_path(output)}' (HEADER, DELIMITER ',')
        """
    )
    rows = connection.execute(
        f"SELECT count(*) FROM read_csv_auto('{sql_path(output)}', header=true)"
    ).fetchone()[0]
    if rows != 600:
        raise ValueError(f"Expected 600 Lab 1 preparation rows, found {rows}")
    write_json(
        output_dir / "source.json",
        {
            "schema_version": 1,
            "purpose": "Lab 1 preparation: Excel import, table, sort, and filter practice",
            "dataset_id": "olist_marketplace_2016_2018",
            "release_version": RELEASE_VERSION,
            "source_url": "https://data.60land.com/project1/2026-fall/v1/olist/olist_order_analysis.parquet",
            "sampling": "100 delivered orders from each of SP, RJ, MG, RS, PR, and SC, selected deterministically by order key hash",
            "artifact": {
                "filename": output.name,
                "rows": rows,
                "sha256": sha256(output),
            },
        },
    )


def build_lab_3_prep(connection: duckdb.DuckDBPyConnection) -> None:
    source = (
        ROOT
        / ".data-build"
        / "airline"
        / "us-airline-marketplace-2024-v1"
        / "release"
        / "airline_carrier_month.parquet"
    )
    output_dir = ROOT / "week03" / "lab-prep" / "data"
    output_dir.mkdir(parents=True, exist_ok=True)
    output = output_dir / "airline_carrier_month.csv"
    connection.execute(
        f"""
        COPY (
          SELECT
            month,
            marketing_carrier_code,
            scheduled_flights,
            completed_nondiverted_flights,
            cancelled_flights,
            diverted_flights,
            arrival_delayed_15_flights,
            mean_arrival_delay_minutes,
            arrival_delay_15_rate,
            cancellation_rate,
            carrier_delay_minutes,
            weather_delay_minutes,
            national_aviation_system_delay_minutes,
            security_delay_minutes,
            late_aircraft_delay_minutes
          FROM read_parquet('{sql_path(source)}')
          ORDER BY marketing_carrier_code, month
        ) TO '{sql_path(output)}' (HEADER, DELIMITER ',')
        """
    )
    rows = connection.execute(
        f"SELECT count(*) FROM read_csv_auto('{sql_path(output)}', header=true)"
    ).fetchone()[0]
    if rows != 120:
        raise ValueError(f"Expected 120 Lab 3 preparation rows, found {rows}")
    write_json(
        output_dir / "source.json",
        {
            "schema_version": 1,
            "purpose": "Lab 3 preparation: introductory Excel and Tableau chart practice",
            "dataset_id": "us_airline_marketplace_2024",
            "release_version": RELEASE_VERSION,
            "source_url": "https://data.60land.com/project1/2026-fall/v1/airline/airline_carrier_month.parquet",
            "sampling": "Complete published carrier-month summary table; no sampling",
            "artifact": {
                "filename": output.name,
                "rows": rows,
                "sha256": sha256(output),
            },
        },
    )


def build_lab_3_in_class(connection: duckdb.DuckDBPyConnection) -> None:
    source = (
        ROOT
        / ".data-build"
        / "traffic"
        / "chicago-2018-2025-v1"
        / "release"
        / "chicago_traffic_crash_analysis.parquet"
    )
    output_dir = ROOT / "week03" / "labs" / "data"
    output_dir.mkdir(parents=True, exist_ok=True)
    output = output_dir / "chicago_traffic_crashes_sample.csv"
    connection.execute(
        f"""
        COPY (
          SELECT
            concat('crash_', lpad(CAST(sample_number AS VARCHAR), 5, '0')) AS crash_id,
            crash_datetime,
            crash_year,
            crash_month,
            crash_month_name,
            crash_weekday_name,
            crash_hour,
            time_period,
            posted_speed_limit_mph,
            traffic_control_device,
            reported_weather_condition,
            reported_lighting_condition,
            first_crash_type,
            trafficway_type,
            reported_road_surface_condition,
            crash_type,
            intersection_related,
            hit_and_run,
            estimated_damage,
            primary_contributory_cause,
            unit_count,
            most_severe_injury,
            injuries_total,
            injuries_fatal,
            latitude,
            longitude,
            zip5,
            weather_station_name,
            temperature_f,
            dew_point_f,
            relative_humidity_percent,
            precipitation_inches,
            visibility_miles,
            wind_speed_mph
          FROM (
            SELECT
              *,
              row_number() OVER (ORDER BY hash(crash_record_id)) AS sample_number
            FROM read_parquet('{sql_path(source)}')
          )
          WHERE sample_number <= 12000
          ORDER BY sample_number
        ) TO '{sql_path(output)}' (HEADER, DELIMITER ',')
        """
    )
    rows = connection.execute(
        f"SELECT count(*) FROM read_csv_auto('{sql_path(output)}', header=true)"
    ).fetchone()[0]
    years = connection.execute(
        f"SELECT min(crash_year), max(crash_year), count(distinct crash_year) "
        f"FROM read_csv_auto('{sql_path(output)}', header=true)"
    ).fetchone()
    if rows != 12000 or years != (2018, 2025, 8):
        raise ValueError(f"Unexpected Lab 3 sample coverage: rows={rows}, years={years}")
    write_json(
        output_dir / "source.json",
        {
            "schema_version": 1,
            "purpose": "In-class Lab 3 visualization dataset",
            "dataset_id": "chicago_traffic_crashes_2018_2025",
            "release_version": RELEASE_VERSION,
            "source_url": "https://data.60land.com/project1/2026-fall/v1/traffic/chicago_traffic_crash_analysis.parquet",
            "sampling": "Deterministic 12,000-row sample ordered by crash-record-key hash; source identifiers replaced with sample-local crash IDs",
            "limitations": "Sample counts are suitable for learning visualization techniques but are not official Chicago crash totals.",
            "artifact": {
                "filename": output.name,
                "rows": rows,
                "sha256": sha256(output),
            },
        },
    )


def build_lab_2_download() -> None:
    data_dir = ROOT / "week02" / "labs" / "data"
    members = ["nfl_games.csv", "nfl_plays.csv", "source.json"]
    output = data_dir / "Lab_02_NFL_Files.zip"
    with ZipFile(output, "w", compression=ZIP_DEFLATED, compresslevel=9) as archive:
        for name in members:
            source = data_dir / name
            if not source.is_file():
                raise FileNotFoundError(source)
            info = ZipInfo(name, date_time=(2026, 9, 6, 0, 0, 0))
            info.compress_type = ZIP_DEFLATED
            info.external_attr = 0o644 << 16
            archive.writestr(info, source.read_bytes())


def main() -> int:
    connection = duckdb.connect()
    build_lab_1_prep(connection)
    build_lab_3_prep(connection)
    build_lab_3_in_class(connection)
    build_lab_2_download()
    print("Built Lab 1 prep, Lab 2 in-class, Lab 3 prep, and Lab 3 in-class examples")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
