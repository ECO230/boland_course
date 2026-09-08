#!/usr/bin/env python3
"""Generate deterministic Wisconsin and Minnesota Synthea CSV source snapshots."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path


SYNTHEA_VERSION = "4.0.0"
SYNTHEA_URL = (
    "https://github.com/synthetichealth/synthea/releases/download/"
    "v4.0.0/synthea-with-dependencies.jar"
)
SYNTHEA_SHA256 = "ed43c20ad40ba5c3bc724503a5af032715fe3c491620b766148e7c2361e6ecc1"

JAVA_VERSION = "25.0.4.1+1"
JAVA_URL = (
    "https://github.com/adoptium/temurin25-binaries/releases/download/"
    "jdk-25.0.4.1%2B1/OpenJDK25U-jre_x64_windows_hotspot_25.0.4.1_1.zip"
)
JAVA_SHA256 = "4c95451cea98556def2c54f7782933f52a26d4a36bd85e1d59f0364464828b07"

SIMULATION_END_DATE = "20251231"
STATES = (
    {"name": "Wisconsin", "slug": "wisconsin", "seed": 230202601, "clinician_seed": 230202611},
    {"name": "Minnesota", "slug": "minnesota", "seed": 230202602, "clinician_seed": 230202612},
)

EXPECTED_CSVS = (
    "allergies.csv",
    "careplans.csv",
    "claims.csv",
    "claims_transactions.csv",
    "conditions.csv",
    "encounters.csv",
    "immunizations.csv",
    "medications.csv",
    "observations.csv",
    "organizations.csv",
    "patients.csv",
    "payer_transitions.csv",
    "payers.csv",
    "procedures.csv",
    "providers.csv",
)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def download(url: str, destination: Path, expected_sha256: str) -> None:
    if destination.is_file() and sha256(destination) == expected_sha256:
        print(f"Using verified cached {destination.name}")
        return
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_suffix(destination.suffix + ".partial")
    if temporary.exists():
        temporary.unlink()
    print(f"Downloading {url}", flush=True)
    with urllib.request.urlopen(url) as response, temporary.open("wb") as output:
        shutil.copyfileobj(response, output)
    actual = sha256(temporary)
    if actual != expected_sha256:
        temporary.unlink()
        raise RuntimeError(
            f"Checksum mismatch for {destination.name}: expected {expected_sha256}, found {actual}"
        )
    temporary.replace(destination)


def find_java(java_root: Path) -> Path:
    candidates = sorted(java_root.glob("*/bin/java.exe"))
    if len(candidates) != 1:
        raise RuntimeError(f"Expected one portable Java executable under {java_root}; found {candidates}")
    return candidates[0]


def count_csv_rows(path: Path) -> int:
    with path.open("rb") as handle:
        return max(sum(1 for _ in handle) - 1, 0)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--patients-per-state",
        type=int,
        default=2500,
        help="Target living synthetic population for each state (default: 2500); deceased histories may add rows.",
    )
    parser.add_argument(
        "--output",
        default=".data-build/health/great-lakes-synthetic-ehr-2026-v1",
        help="Ignored build root.",
    )
    args = parser.parse_args()
    if args.patients_per_state < 1:
        parser.error("--patients-per-state must be positive")

    output_root = Path(args.output).resolve()
    source_root = output_root / "source"
    if source_root.exists():
        raise FileExistsError(f"Refusing to overwrite existing source snapshot: {source_root}")

    tools_root = output_root.parent / "_tools"
    jar_path = tools_root / f"synthea-{SYNTHEA_VERSION}-with-dependencies.jar"
    java_zip = tools_root / "OpenJDK25U-jre_x64_windows_hotspot_25.0.4.1_1.zip"
    java_root = tools_root / "temurin-25.0.4.1_1-jre"
    download(SYNTHEA_URL, jar_path, SYNTHEA_SHA256)
    download(JAVA_URL, java_zip, JAVA_SHA256)
    if not java_root.exists():
        java_root.mkdir(parents=True)
        with zipfile.ZipFile(java_zip) as archive:
            archive.extractall(java_root)
    java_path = find_java(java_root)

    generated_states = []
    for state in STATES:
        state_root = source_root / state["slug"]
        state_root.mkdir(parents=True)
        command = [
            str(java_path),
            "-jar",
            str(jar_path),
            "-p",
            str(args.patients_per_state),
            "-s",
            str(state["seed"]),
            "-cs",
            str(state["clinician_seed"]),
            "-e",
            SIMULATION_END_DATE,
            state["name"],
            "--exporter.csv.export=true",
            "--exporter.fhir.export=false",
            "--exporter.fhir_stu3.export=false",
            "--exporter.fhir_dstu2.export=false",
            "--exporter.ccda.export=false",
            f"--exporter.baseDirectory={state_root.as_posix()}",
        ]
        log_path = state_root / "synthea.log"
        print(f"Generating {args.patients_per_state:,} requested patients for {state['name']}...", flush=True)
        with log_path.open("w", encoding="utf-8") as log:
            completed = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, text=True)
        if completed.returncode != 0:
            raise RuntimeError(f"Synthea failed for {state['name']}; see {log_path}")
        csv_root = state_root / "csv"
        missing = [name for name in EXPECTED_CSVS if not (csv_root / name).is_file()]
        if missing:
            raise FileNotFoundError(f"Missing Synthea CSVs for {state['name']}: {missing}")
        generated_states.append(
            {
                "state": state["name"],
                "source_subdirectory": f"{state['slug']}/csv",
                "requested_patients": args.patients_per_state,
                "generated_patients": count_csv_rows(csv_root / "patients.csv"),
                "simulation_seed": state["seed"],
                "clinician_seed": state["clinician_seed"],
                "files": [
                    {
                        "name": name,
                        "bytes": (csv_root / name).stat().st_size,
                        "rows": count_csv_rows(csv_root / name),
                        "sha256": sha256(csv_root / name),
                    }
                    for name in EXPECTED_CSVS
                ],
            }
        )

    manifest = {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "source_type": "fully synthetic longitudinal health records",
        "simulation_end_date": "2025-12-31",
        "synthea": {
            "version": SYNTHEA_VERSION,
            "release_url": f"https://github.com/synthetichealth/synthea/releases/tag/v{SYNTHEA_VERSION}",
            "artifact_url": SYNTHEA_URL,
            "artifact_sha256": SYNTHEA_SHA256,
            "license": "Apache License 2.0",
        },
        "runtime": {
            "name": "Eclipse Temurin JRE",
            "version": JAVA_VERSION,
            "artifact_url": JAVA_URL,
            "artifact_sha256": JAVA_SHA256,
        },
        "states": generated_states,
        "privacy_note": (
            "Every record is generated by Synthea and describes no real person. "
            "The curated release still removes names, synthetic government identifiers, "
            "addresses, phone numbers, and source UUIDs."
        ),
    }
    (source_root / "source_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"Generated source snapshot at {source_root}")
    for state in generated_states:
        print(f"{state['state']}: {state['generated_patients']:,} patients")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Source generation failed: {error}", file=sys.stderr)
        raise SystemExit(1)
