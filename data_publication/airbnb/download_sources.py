#!/usr/bin/env python3
"""Download the pinned June 2026 Inside Airbnb source snapshots."""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import os
import shutil
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path


USER_AGENT = "ECO230-course-data-builder/1.0 (academic use)"
MARKETS = {
    "chicago": {
        "market_name": "Chicago",
        "snapshot_date": "2026-06-24",
        "root": "https://data.insideairbnb.com/united-states/il/chicago/2026-06-24/data",
    },
    "twin_cities_msa": {
        "market_name": "Twin Cities MSA",
        "snapshot_date": "2026-06-27",
        "root": "https://data.insideairbnb.com/united-states/mn/twin-cities-msa/2026-06-27/data",
    },
}
KINDS = {
    "listings": {"required": {"id", "last_scraped", "latitude", "longitude"}},
    "calendar": {
        "required": {
            "listing_id", "date", "available", "minimum_nights", "maximum_nights"
        }
    },
    "reviews": {"required": {"listing_id", "date"}},
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        default=".data-build/airbnb/chicago-twin-cities-2026-06-v1/source",
    )
    return parser.parse_args()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def open_url(url: str, timeout: int = 240):
    request = urllib.request.Request(
        url,
        headers={
            "User-Agent": USER_AGENT,
            "Referer": "https://insideairbnb.com/get-the-data/",
        },
    )
    for attempt in range(1, 6):
        try:
            return urllib.request.urlopen(request, timeout=timeout)
        except urllib.error.HTTPError as error:
            if 400 <= error.code < 500 and error.code != 429:
                raise RuntimeError(f"HTTP {error.code} for {url}") from error
            if attempt == 5:
                raise
            wait = min(2**attempt, 30)
            print(f"Request failed ({error}); retrying in {wait}s", flush=True)
            time.sleep(wait)
        except (urllib.error.URLError, TimeoutError) as error:
            if attempt == 5:
                raise
            wait = min(2**attempt, 30)
            print(f"Request failed ({error}); retrying in {wait}s", flush=True)
            time.sleep(wait)


def validate_gzip_header(path: Path, required: set[str]) -> list[str]:
    with gzip.open(path, "rt", encoding="utf-8-sig", newline="") as handle:
        header = handle.readline().rstrip("\r\n")
    columns = header.split(",")
    missing = sorted(required - set(columns))
    if missing:
        raise ValueError(f"{path.name} is missing required columns: {missing}")
    return columns


def download(url: str, destination: Path) -> None:
    if destination.exists():
        raise FileExistsError(f"Refusing to overwrite source snapshot: {destination}")
    partial = destination.with_suffix(destination.suffix + ".partial")
    partial.unlink(missing_ok=True)
    print(f"Downloading {destination.name}", flush=True)
    try:
        with open_url(url) as response, partial.open("wb") as output:
            shutil.copyfileobj(response, output, length=1024 * 1024)
        os.replace(partial, destination)
    finally:
        partial.unlink(missing_ok=True)


def main() -> int:
    args = parse_args()
    output = Path(args.output).resolve()
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite source directory: {output}")
    output.mkdir(parents=True)

    manifest = {
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "publisher": "Inside Airbnb",
        "publisher_page": "https://insideairbnb.com/get-the-data/",
        "data_policy_url": "https://insideairbnb.com/data-policies/",
        "license_statement": "CC BY 4.0",
        "redistribution_status": "unresolved-do-not-publish",
        "files": [],
    }
    try:
        for market_id, market in MARKETS.items():
            for kind, kind_spec in KINDS.items():
                filename = f"{market_id}_{kind}.csv.gz"
                url = f"{market['root']}/{kind}.csv.gz"
                destination = output / filename
                download(url, destination)
                columns = validate_gzip_header(destination, kind_spec["required"])
                manifest["files"].append(
                    {
                        "market_id": market_id,
                        "market_name": market["market_name"],
                        "snapshot_date": market["snapshot_date"],
                        "kind": kind,
                        "source_url": url,
                        "filename": filename,
                        "bytes": destination.stat().st_size,
                        "sha256": sha256(destination),
                        "source_column_count": len(columns),
                    }
                )
        (output / "source_manifest.json").write_text(
            json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
        )
    except Exception:
        # A failed source directory is intentionally left for diagnosis and must
        # be removed explicitly before retrying.
        raise

    print(f"Pinned source snapshot completed at {output}")
    for details in manifest["files"]:
        print(
            f"{details['filename']}: {details['bytes']:,} bytes, "
            f"{details['source_column_count']} columns"
        )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Download failed: {error}", file=sys.stderr)
        raise SystemExit(1)
