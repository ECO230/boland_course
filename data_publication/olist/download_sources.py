#!/usr/bin/env python3
"""Download and pin the two Olist Kaggle source archives."""

from __future__ import annotations

import argparse
import base64
import hashlib
import json
import os
import shutil
import sys
import urllib.error
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path


DATASETS = {
    "commerce": {
        "id": "olistbr/brazilian-ecommerce",
        "archive": "brazilian-ecommerce.zip",
        "files": [
            "olist_customers_dataset.csv", "olist_geolocation_dataset.csv",
            "olist_order_items_dataset.csv", "olist_order_payments_dataset.csv",
            "olist_order_reviews_dataset.csv", "olist_orders_dataset.csv",
            "olist_products_dataset.csv", "olist_sellers_dataset.csv",
            "product_category_name_translation.csv",
        ],
    },
    "marketing": {
        "id": "olistbr/marketing-funnel-olist",
        "archive": "marketing-funnel-olist.zip",
        "files": ["olist_marketing_qualified_leads_dataset.csv", "olist_closed_deals_dataset.csv"],
    },
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def credentials_header() -> dict[str, str]:
    credentials = Path.home() / ".kaggle" / "kaggle.json"
    if not credentials.is_file():
        return {}
    values = json.loads(credentials.read_text(encoding="utf-8"))
    if values.get("username") and values.get("key"):
        token = base64.b64encode(
            f"{values['username']}:{values['key']}".encode("utf-8")
        ).decode("ascii")
        return {"Authorization": f"Basic {token}"}
    return {}


def download(dataset_id: str, destination: Path) -> None:
    url = f"https://www.kaggle.com/api/v1/datasets/download/{dataset_id}"
    headers = {"User-Agent": "ECO230-course-data-builder/1.0", **credentials_header()}
    request = urllib.request.Request(url, headers=headers)
    partial = destination.with_suffix(destination.suffix + ".partial")
    partial.unlink(missing_ok=True)
    try:
        with urllib.request.urlopen(request, timeout=300) as response, partial.open("wb") as output:
            shutil.copyfileobj(response, output, length=1024 * 1024)
        os.replace(partial, destination)
    except urllib.error.HTTPError as error:
        if error.code in {401, 403}:
            raise RuntimeError(
                "Kaggle denied the download. Add free Kaggle API credentials at "
                "%USERPROFILE%\\.kaggle\\kaggle.json or pass the downloaded ZIP "
                "with --commerce-archive/--marketing-archive."
            ) from error
        raise
    finally:
        partial.unlink(missing_ok=True)


def safe_extract(archive: Path, output: Path, required: list[str]) -> None:
    with zipfile.ZipFile(archive) as bundle:
        members = {Path(name).name: name for name in bundle.namelist() if not name.endswith("/")}
        missing = sorted(set(required) - set(members))
        if missing:
            raise ValueError(f"{archive.name} is missing expected files: {missing}")
        for filename in required:
            destination = output / filename
            if destination.exists():
                raise FileExistsError(f"Refusing to overwrite {destination}")
            with bundle.open(members[filename]) as source, destination.open("wb") as target:
                shutil.copyfileobj(source, target)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", default=".data-build/olist/brazilian-ecommerce-2016-2018-v1/source")
    parser.add_argument("--commerce-archive")
    parser.add_argument("--marketing-archive")
    args = parser.parse_args()
    output = Path(args.output).resolve()
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite source directory: {output}")
    output.mkdir(parents=True)

    supplied = {"commerce": args.commerce_archive, "marketing": args.marketing_archive}
    manifest = {
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "publisher": "Olist via Kaggle",
        "license": "CC BY-NC-SA 4.0",
        "redistribution_status": "approved-noncommercial-sharealike",
        "files": [],
    }
    for kind, spec in DATASETS.items():
        archive = output / spec["archive"]
        if supplied[kind]:
            shutil.copy2(Path(supplied[kind]).resolve(), archive)
        else:
            print(f"Downloading {spec['id']}...", flush=True)
            download(spec["id"], archive)
        safe_extract(archive, output, spec["files"])
        manifest["files"].append({
            "dataset_id": spec["id"], "source_url": f"https://www.kaggle.com/datasets/{spec['id']}",
            "archive": archive.name, "bytes": archive.stat().st_size,
            "sha256": sha256(archive), "extracted_files": spec["files"],
        })

    (output / "source_manifest.json").write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(f"Pinned Olist sources at {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Download failed: {error}", file=sys.stderr)
        raise SystemExit(1)
