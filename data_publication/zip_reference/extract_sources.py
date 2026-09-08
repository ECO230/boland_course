#!/usr/bin/env python3
"""Extract only the approved ZIP reference inputs from Group_Utilities.zip."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import shutil
import sys
import zipfile
from datetime import datetime, timezone
from pathlib import Path


SOURCE_FILES = [
    "zip_cbsa_meta_2025.csv",
    "zip_population_by_zip_2023.csv",
    "zip_median_age_2023.csv",
    "zip_med_commute.csv",
    "zip_pop_density.csv",
    "median_housing_age_by_zip_2022.csv",
    "zip_rucca_2010.csv",
]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def csv_rows(path: Path) -> int:
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.reader(handle)
        next(reader)
        return sum(1 for _ in reader)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--archive",
        default="posit_group/Group_Utilities.zip",
        help="Local instructor utility archive; it is read but never copied.",
    )
    parser.add_argument(
        "--output",
        default=".data-build/zip-reference/us-zip-reference-2026-fall-v1/source",
    )
    args = parser.parse_args()

    archive = Path(args.archive).resolve()
    output = Path(args.output).resolve()
    if not archive.is_file():
        raise FileNotFoundError(f"Missing utility archive: {archive}")
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite source directory: {output}")
    output.mkdir(parents=True)

    manifest_files: list[dict[str, object]] = []
    with zipfile.ZipFile(archive) as bundle:
        by_basename: dict[str, list[str]] = {}
        for member in bundle.namelist():
            if not member.endswith("/"):
                by_basename.setdefault(Path(member).name, []).append(member)

        for filename in SOURCE_FILES:
            matches = by_basename.get(filename, [])
            if len(matches) != 1:
                raise ValueError(
                    f"Expected exactly one {filename} in {archive.name}; found {len(matches)}"
                )
            member = matches[0]
            destination = output / filename
            with bundle.open(member) as source, destination.open("wb") as target:
                shutil.copyfileobj(source, target)
            manifest_files.append(
                {
                    "filename": filename,
                    "archive_member": member,
                    "bytes": destination.stat().st_size,
                    "rows": csv_rows(destination),
                    "sha256": sha256(destination),
                }
            )

    manifest = {
        "schema_version": 1,
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "source_type": "instructor-supplied derived ZIP reference extracts",
        "source_archive": {
            "filename": archive.name,
            "bytes": archive.stat().st_size,
            "sha256": sha256(archive),
        },
        "selection": {
            "included_files": SOURCE_FILES,
            "excluded_content": (
                "All other archive content, including RStudio state, credentials, "
                "scripts, project data, and unrelated extracts"
            ),
        },
        "privacy": {
            "contains_student_data": False,
            "assessment": "Selected files contain aggregate ZIP reference attributes only.",
        },
        "redistribution_status": "not-approved-provenance-and-license-incomplete",
        "files": manifest_files,
    }
    (output / "source_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"Extracted {len(SOURCE_FILES)} approved source files to {output}")
    for details in manifest_files:
        print(f"{details['filename']}: {details['rows']:,} rows")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Source extraction failed: {error}", file=sys.stderr)
        raise SystemExit(1)
