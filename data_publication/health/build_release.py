#!/usr/bin/env python3
"""Build the immutable local Great Lakes synthetic EHR release candidate."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import shutil
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
PRIVATE_CROSSWALK_COLUMNS = {
    "diagnosis_crosswalk.csv": [
        "source_code_system", "source_code", "standard_code_system", "standard_code",
        "standard_description", "diagnosis_category", "diagnosis_group",
        "mapping_authority", "mapping_version", "reviewed_by", "reviewed_date",
    ],
    "procedure_crosswalk.csv": [
        "source_code_system", "source_code", "standard_code_system", "standard_code",
        "standard_description", "procedure_category", "procedure_group",
        "mapping_authority", "mapping_version", "reviewed_by", "reviewed_date",
    ],
}
ARTIFACTS = {
    "patients": "ehr_patients.parquet",
    "encounters": "ehr_encounters.parquet",
    "appointments": "ehr_appointments.parquet",
    "satisfaction_surveys": "ehr_patient_satisfaction.parquet",
    "diagnoses": "ehr_diagnoses.parquet",
    "procedures": "ehr_procedures.parquet",
    "medications": "ehr_medications.parquet",
    "observations": "ehr_observations.parquet",
    "allergies": "ehr_allergies.parquet",
    "immunizations": "ehr_immunizations.parquet",
    "careplans": "ehr_careplans.parquet",
    "claims": "ehr_claims.parquet",
    "claim_transactions": "ehr_claim_transactions.parquet",
    "coverage_periods": "ehr_coverage_periods.parquet",
    "organizations": "ehr_organizations.parquet",
    "providers": "ehr_providers.parquet",
    "payers": "ehr_payers.parquet",
    "encounter_analysis": "ehr_encounter_analysis.parquet",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def run_sql(connection: duckdb.DuckDBPyConnection, path: Path, **values: str) -> None:
    started = time.perf_counter()
    sql = path.read_text(encoding="utf-8")
    for key, value in values.items():
        sql = sql.replace("{{" + key + "}}", value)
    if "{{" in sql or "}}" in sql:
        raise ValueError(f"Unresolved SQL placeholder in {path}")
    print(f"Starting {path.name}...", flush=True)
    connection.execute(sql)
    print(f"Completed {path.name} in {time.perf_counter() - started:,.1f}s", flush=True)


def ensure_private_crosswalks(directory: Path) -> dict[str, Path]:
    """Create header-only private crosswalks when reviewed mappings do not exist."""
    directory.mkdir(parents=True, exist_ok=True)
    paths = {}
    for filename, columns in PRIVATE_CROSSWALK_COLUMNS.items():
        path = directory / filename
        if not path.exists():
            with path.open("w", encoding="utf-8", newline="") as handle:
                csv.writer(handle).writerow(columns)
        paths[filename] = path
    return paths


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", default=".data-build/health/great-lakes-synthetic-ehr-2026-v1/source")
    parser.add_argument("--output", default=".data-build/health/great-lakes-synthetic-ehr-2026-v1")
    parser.add_argument(
        "--private-crosswalk-dir",
        default=".data-build/health/private-crosswalks",
        help="Ignored directory containing reviewed terminology mappings; never copied to the release bundle",
    )
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}")

    dataset_dir = Path(__file__).resolve().parent
    source_dir = Path(args.source_dir).resolve()
    output_root = Path(args.output).resolve()
    private_crosswalks = ensure_private_crosswalks(Path(args.private_crosswalk_dir).resolve())
    work_dir, release_dir = output_root / "work", output_root / "release"
    if work_dir.exists() or release_dir.exists():
        raise FileExistsError(f"Refusing to overwrite existing build: {output_root}")
    required = [
        source_dir / state / "csv" / f"{name}.csv"
        for state in ("wisconsin", "minnesota")
        for name in (
            "patients", "encounters", "conditions", "procedures", "medications",
            "observations", "allergies", "immunizations", "careplans", "claims",
            "claims_transactions", "payer_transitions", "organizations", "providers", "payers",
        )
    ] + [source_dir / "source_manifest.json"]
    missing = [str(path) for path in required if not path.is_file()]
    if missing:
        raise FileNotFoundError(f"Missing required sources: {missing}")
    work_dir.mkdir(parents=True)
    release_dir.mkdir(parents=True)

    connection = duckdb.connect(str(work_dir / "health.duckdb"))
    connection.execute("SET threads = 1")
    generated = {}
    try:
        run_sql(
            connection,
            dataset_dir / "sql" / "10_ingest.sql",
            WI_ROOT=sql_path(source_dir / "wisconsin" / "csv"),
            MN_ROOT=sql_path(source_dir / "minnesota" / "csv"),
        )
        run_sql(
            connection,
            dataset_dir / "sql" / "15_internal_code_maps.sql",
            DIAGNOSIS_CROSSWALK=sql_path(private_crosswalks["diagnosis_crosswalk.csv"]),
            PROCEDURE_CROSSWALK=sql_path(private_crosswalks["procedure_crosswalk.csv"]),
        )
        run_sql(connection, dataset_dir / "sql" / "20_model.sql")
        run_sql(connection, dataset_dir / "sql" / "30_validate.sql")
        output_paths = {name: release_dir / filename for name, filename in ARTIFACTS.items()}
        run_sql(
            connection,
            dataset_dir / "sql" / "40_publish.sql",
            **{f"OUTPUT_{name.upper()}": sql_path(path) for name, path in output_paths.items()},
        )
        for name, path in output_paths.items():
            schema_rows = connection.execute(f"DESCRIBE SELECT * FROM curated.{name}").fetchall()
            generated[name] = {
                "filename": path.name,
                "sha256": sha256(path),
                "bytes": path.stat().st_size,
                "rows": connection.execute(f"SELECT count(*) FROM curated.{name}").fetchone()[0],
                "columns": len(schema_rows),
                "schema": [
                    {"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"}
                    for row in schema_rows
                ],
            }
    finally:
        connection.close()

    metadata = json.loads((dataset_dir / "dataset.json").read_text(encoding="utf-8"))
    metadata["release_status"] = "local-candidate"
    metadata["built_at_utc"] = datetime.now(timezone.utc).isoformat()
    metadata["build"] = {"duckdb_version": duckdb.__version__, "threads": 1}
    for artifact in metadata["artifacts"]:
        artifact.update(generated[artifact["name"]])
    (release_dir / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n", encoding="utf-8")
    (release_dir / "schemas.json").write_text(
        json.dumps({name: details["schema"] for name, details in generated.items()}, indent=2) + "\n",
        encoding="utf-8",
    )
    shutil.copy2(source_dir / "source_manifest.json", release_dir)
    shutil.copy2(dataset_dir / "LICENSE-DATA.txt", release_dir)
    shutil.copy2(dataset_dir / "data_dictionary.md", release_dir / "DATA-DICTIONARY.md")
    for details in generated.values():
        (release_dir / f"{details['filename']}.sha256").write_text(
            f"{details['sha256']}  {details['filename']}\n", encoding="ascii"
        )
    print(f"Built Great Lakes synthetic EHR local candidate at {release_dir}")
    for name, details in generated.items():
        print(f"{name}: {details['rows']:,} rows, {details['bytes']:,} bytes")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
