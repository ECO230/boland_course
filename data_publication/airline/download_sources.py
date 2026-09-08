#!/usr/bin/env python3
"""Download and pin the official BTS 2024 airline source archives."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import sys
import time
import urllib.error
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path


ROOT = "https://transtats.bts.gov/PREZIP"
ONTIME_PATTERN = "On_Time_Marketing_Carrier_On_Time_Performance_Beginning_January_2018_2024_{month}.zip"
DB1B_PATTERN = "Origin_and_Destination_Survey_DB1BMarket_2024_{quarter}.zip"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def download(url: str, destination: Path) -> None:
    request = urllib.request.Request(url, headers={"User-Agent": "ECO230-course-data-builder/1.0"})
    partial = destination.with_suffix(destination.suffix + ".partial")
    partial.unlink(missing_ok=True)
    for attempt in range(1, 6):
        try:
            print(f"Downloading {destination.name}...", flush=True)
            with urllib.request.urlopen(request, timeout=600) as response, partial.open("wb") as output:
                shutil.copyfileobj(response, output, length=1024 * 1024)
            os.replace(partial, destination)
            break
        except (urllib.error.URLError, TimeoutError):
            partial.unlink(missing_ok=True)
            if attempt == 5:
                raise
            time.sleep(min(2 ** attempt, 30))
    with zipfile.ZipFile(destination) as bundle:
        csv_members = [name for name in bundle.namelist() if name.lower().endswith(".csv")]
        if len(csv_members) != 1:
            raise ValueError(f"Expected one CSV in {destination.name}; found {csv_members}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", default=".data-build/airline/us-airline-marketplace-2024-v1/source")
    args = parser.parse_args()
    output = Path(args.output).resolve()
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite source directory: {output}")
    output.mkdir(parents=True)
    sources = []
    for kind, values, pattern in (("on_time", range(1, 13), ONTIME_PATTERN), ("db1b_market", range(1, 5), DB1B_PATTERN)):
        for value in values:
            filename = pattern.format(month=value, quarter=value)
            url = f"{ROOT}/{filename}"
            destination = output / filename
            download(url, destination)
            with zipfile.ZipFile(destination) as bundle:
                csv_member = next(name for name in bundle.namelist() if name.lower().endswith(".csv"))
            sources.append({"kind": kind, "period": value, "source_url": url, "filename": filename, "csv_member": csv_member, "bytes": destination.stat().st_size, "sha256": sha256(destination)})
    manifest = {
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "publisher": "Bureau of Transportation Statistics, U.S. Department of Transportation",
        "redistribution_status": "approved-public-federal-data",
        "coverage_year": 2024,
        "files": sources,
    }
    (output / "source_manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(f"Pinned {len(sources)} BTS archives at {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Download failed: {error}", file=sys.stderr)
        raise SystemExit(1)
