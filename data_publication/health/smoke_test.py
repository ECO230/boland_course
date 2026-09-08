#!/usr/bin/env python3
"""Print compact analytical sanity checks for a built health release."""

from __future__ import annotations

import argparse
from pathlib import Path

import duckdb


def parquet_path(release: Path, filename: str) -> str:
    return (release / filename).resolve().as_posix().replace("'", "''")


def print_query(connection: duckdb.DuckDBPyConnection, title: str, query: str) -> None:
    print(f"\n{title}")
    columns = [item[0] for item in connection.execute(query).description]
    print(" | ".join(columns))
    for row in connection.fetchall():
        print(" | ".join(str(value) for value in row))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("release", nargs="?", default=".data-build/health/great-lakes-synthetic-ehr-2026-v1/release")
    args = parser.parse_args()
    release = Path(args.release)
    connection = duckdb.connect()
    try:
        patients = parquet_path(release, "ehr_patients.parquet")
        encounters = parquet_path(release, "ehr_encounters.parquet")
        appointments = parquet_path(release, "ehr_appointments.parquet")
        satisfaction = parquet_path(release, "ehr_patient_satisfaction.parquet")
        diagnoses = parquet_path(release, "ehr_diagnoses.parquet")
        observations = parquet_path(release, "ehr_observations.parquet")
        analysis = parquet_path(release, "ehr_encounter_analysis.parquet")
        print_query(
            connection,
            "Synthetic patients by state and death status",
            f"""SELECT patient_state, count(*) AS patients,
                       count(*) FILTER (WHERE death_date IS NOT NULL) AS deceased_histories
                FROM read_parquet('{patients}') GROUP BY patient_state ORDER BY patient_state""",
        )
        print_query(
            connection,
            "Encounters by class",
            f"""SELECT encounter_class, count(*) AS encounters
                FROM read_parquet('{encounters}') GROUP BY encounter_class ORDER BY encounters DESC""",
        )
        print_query(
            connection,
            "Appointments by outcome",
            f"""SELECT appointment_status, count(*) AS appointments,
                       round(100.0 * count(*) / sum(count(*)) OVER (), 1) AS percent
                FROM read_parquet('{appointments}') GROUP BY appointment_status
                ORDER BY appointments DESC""",
        )
        print_query(
            connection,
            "No-show rates by reminder status",
            f"""SELECT reminder_sent, count(*) AS appointments,
                       round(100.0 * avg(CAST(appointment_status = 'no_show' AS INTEGER)), 1) AS no_show_percent
                FROM read_parquet('{appointments}') GROUP BY reminder_sent ORDER BY reminder_sent""",
        )
        print_query(
            connection,
            "Satisfaction response and rating summary",
            f"""SELECT response_status, count(*) AS invitations,
                       round(avg(overall_rating), 2) AS mean_overall_rating,
                       round(avg(would_recommend_rating), 2) AS mean_recommend_rating
                FROM read_parquet('{satisfaction}') GROUP BY response_status ORDER BY response_status""",
        )
        print_query(
            connection,
            "Respondent satisfaction by encounter class",
            f"""SELECT encounter_class, count(*) AS responses,
                       round(avg(overall_rating), 2) AS mean_overall_rating,
                       round(avg(perceived_wait_minutes), 1) AS mean_perceived_wait
                FROM read_parquet('{satisfaction}') WHERE response_status = 'responded'
                GROUP BY encounter_class ORDER BY responses DESC""",
        )
        print_query(
            connection,
            "Ten most frequent documented diagnosis groups",
            f"""SELECT diagnosis_category, diagnosis_group, count(*) AS diagnosis_rows
                FROM read_parquet('{diagnoses}') GROUP BY diagnosis_category, diagnosis_group
                ORDER BY diagnosis_rows DESC, diagnosis_category, diagnosis_group LIMIT 10""",
        )
        print_query(
            connection,
            "Observation value types",
            f"""SELECT value_type, count(*) AS observations,
                       count(*) FILTER (WHERE encounter_key IS NULL) AS without_encounter
                FROM read_parquet('{observations}') GROUP BY value_type ORDER BY value_type""",
        )
        print_query(
            connection,
            "Thirty-day revisit summary by encounter class",
            f"""SELECT encounter_class, count(*) AS encounters,
                       round(100.0 * avg(CAST(revisit_within_30_days AS INTEGER)), 1) AS revisit_percent
                FROM read_parquet('{analysis}') GROUP BY encounter_class
                ORDER BY encounters DESC""",
        )
    finally:
        connection.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
