#!/usr/bin/env python3
"""Independently verify a Great Lakes synthetic EHR release bundle."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
PRIMARY_KEYS = {
    "patients": "patient_key",
    "encounters": "encounter_key",
    "appointments": "appointment_key",
    "satisfaction_surveys": "survey_key",
    "diagnoses": "diagnosis_key",
    "procedures": "procedure_key",
    "medications": "medication_key",
    "observations": "observation_key",
    "allergies": "allergy_key",
    "immunizations": "immunization_key",
    "careplans": "careplan_key",
    "claims": "claim_key",
    "claim_transactions": "claim_transaction_key",
    "coverage_periods": "coverage_period_key",
    "organizations": "organization_key",
    "providers": "provider_key",
    "payers": "payer_key",
    "encounter_analysis": "encounter_key",
}
BANNED_COLUMNS = {
    "ssn", "drivers", "passport", "prefix", "first", "middle", "last", "suffix", "maiden",
    "birthplace", "address", "city", "zip", "lat", "lon", "phone", "memberid", "member_id",
    "owner_name", "udi", "source_uuid", "patient_name", "provider_name",
}
CLINICAL_CODE_FIELD_BANS = {
    "encounters": {"encounter_code", "encounter_description", "reason_code", "reason_description"},
    "diagnoses": {"code_system", "diagnosis_code", "diagnosis_description"},
    "procedures": {"code_system", "procedure_code", "procedure_description", "reason_code", "reason_description"},
    "medications": {"reason_code", "reason_description"},
    "allergies": {"code_system", "allergy_code", "allergy_description", "reaction_1_description", "reaction_2_description"},
    "careplans": {"careplan_code", "careplan_description", "reason_code", "reason_description"},
    "claims": {"diagnosis_code_1", "diagnosis_code_2", "diagnosis_code_3", "diagnosis_code_4"},
    "claim_transactions": {"procedure_code"},
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("release", nargs="?", default=".data-build/health/great-lakes-synthetic-ehr-2026-v1/release")
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}")

    release = Path(args.release).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    if metadata.get("schema_version") != 2 or metadata.get("release_status") != "local-candidate":
        raise ValueError("metadata.json is not a schema-version 2 local candidate")
    privacy = metadata.get("privacy", {})
    if (
        privacy.get("contains_student_data") is not False
        or privacy.get("contains_real_patient_data") is not False
        or privacy.get("contains_phi") is not False
    ):
        raise ValueError("Privacy declarations are missing or untruthful")

    source_manifest = json.loads((release / "source_manifest.json").read_text(encoding="utf-8"))
    if source_manifest.get("source_type") != "fully synthetic longitudinal health records":
        raise ValueError("Source manifest does not explicitly identify the data as fully synthetic")
    if source_manifest.get("synthea", {}).get("version") != "4.0.0" or len(source_manifest.get("states", [])) != 2:
        raise ValueError("Source manifest does not identify the pinned Synthea version and two state runs")

    artifacts = metadata["artifacts"]
    if {artifact["name"] for artifact in artifacts} != set(PRIMARY_KEYS):
        raise ValueError("Artifact inventory does not match the expected health tables")
    required = {"metadata.json", "schemas.json", "source_manifest.json", "DATA-DICTIONARY.md", "LICENSE-DATA.txt"}
    for artifact in artifacts:
        required.add(artifact["filename"])
        required.add(f"{artifact['filename']}.sha256")
    actual = {path.name for path in release.iterdir() if path.is_file()}
    if actual != required:
        raise ValueError(f"Release file inventory differs: missing={required-actual}, extra={actual-required}")
    if any(path.is_dir() or path.is_symlink() for path in release.iterdir()):
        raise ValueError("Release must be flat and contain no directories or symbolic links")

    target_directories = {str(Path(artifact["relative_path"]).parent).replace("\\", "/") for artifact in artifacts}
    if len(target_directories) != 1:
        raise ValueError("Artifacts do not share one immutable public directory")

    connection = duckdb.connect()
    try:
        for artifact in artifacts:
            path = release / artifact["filename"]
            actual_hash = sha256(path)
            if actual_hash != artifact["sha256"]:
                raise ValueError(f"Checksum mismatch: {path.name}")
            sidecar = (release / f"{path.name}.sha256").read_text(encoding="ascii").strip().replace("\r", "")
            if sidecar != f"{actual_hash}  {path.name}":
                raise ValueError(f"Checksum sidecar mismatch: {path.name}")
            if path.stat().st_size != artifact["bytes"]:
                raise ValueError(f"Byte count mismatch: {path.name}")
            query_path = path.as_posix().replace("'", "''")
            rows = connection.execute(f"SELECT count(*) FROM read_parquet('{query_path}')").fetchone()[0]
            distinct_rows = connection.execute(
                f"SELECT count(DISTINCT {PRIMARY_KEYS[artifact['name']]}) FROM read_parquet('{query_path}')"
            ).fetchone()[0]
            if rows != artifact["rows"] or rows != distinct_rows or rows < 1:
                raise ValueError(f"Row or primary-key mismatch: {path.name}")
            schema_rows = connection.execute(f"DESCRIBE SELECT * FROM read_parquet('{query_path}')").fetchall()
            schema = [
                {"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"}
                for row in schema_rows
            ]
            if schema != artifact["schema"]:
                raise ValueError(f"Schema mismatch: {path.name}")
            forbidden = BANNED_COLUMNS.intersection(column["name"].lower() for column in schema)
            if forbidden:
                raise ValueError(f"Privacy-reduced schema contains banned columns in {path.name}: {sorted(forbidden)}")
            code_leaks = CLINICAL_CODE_FIELD_BANS.get(artifact["name"], set()).intersection(
                column["name"].lower() for column in schema
            )
            if code_leaks:
                raise ValueError(f"Public clinical schema contains private terminology fields in {path.name}: {sorted(code_leaks)}")
            print(f"Verified {path.name}: {rows:,} rows, {path.stat().st_size:,} bytes")
    finally:
        connection.close()
    print(f"Verified complete local-candidate bundle at {release}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Verification failed: {error}", file=sys.stderr)
        raise SystemExit(1)
