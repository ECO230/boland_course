#!/usr/bin/env python3
"""Download the immutable inputs used for the Chicago Traffic course snapshot."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import shutil
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timezone
from pathlib import Path


START = "2018-01-01T00:00:00"
END = "2026-01-01T00:00:00"
PAGE_SIZE = 50000
USER_AGENT = "ECO230-course-data-builder/1.0 (academic use)"

SOCRATA = {
    "crashes": {
        "id": "85ca-t3if",
        "format": "json",
        "filename": "chicago_crashes_2018_2025.csv",
        "order": "crash_record_id",
        "columns": [
            "crash_record_id", "crash_date_est_i", "crash_date",
            "posted_speed_limit", "traffic_control_device", "device_condition",
            "weather_condition", "lighting_condition", "first_crash_type",
            "trafficway_type", "lane_cnt", "alignment", "roadway_surface_cond",
            "road_defect", "report_type", "crash_type", "intersection_related_i",
            "private_property_i", "hit_and_run_i", "damage", "date_police_notified",
            "prim_contributory_cause", "sec_contributory_cause", "street_no",
            "street_direction", "street_name", "beat_of_occurrence", "dooring_i",
            "work_zone_i", "work_zone_type", "workers_present_i", "num_units",
            "most_severe_injury", "injuries_total", "injuries_fatal",
            "injuries_incapacitating", "injuries_non_incapacitating",
            "injuries_reported_not_evident", "injuries_no_indication",
            "injuries_unknown", "idot_control_no", "latitude", "longitude",
            "`:@computed_region_rpca_8um6`",
        ],
    },
    "vehicles": {
        "id": "68nd-jvt3",
        "format": "csv",
        "filename": "chicago_vehicles_2018_2025.csv",
        "order": "crash_unit_id",
        "columns": [
            "crash_unit_id", "crash_record_id", "crash_date", "unit_no",
            "unit_type", "num_passengers", "vehicle_id", "cmrc_veh_i", "make",
            "model", "lic_plate_state", "vehicle_year", "vehicle_defect",
            "vehicle_type", "vehicle_use", "travel_direction", "maneuver",
            "towed_i", "fire_i", "occupant_cnt", "exceed_speed_limit_i",
            "first_contact_point", "gvwr", "vehicle_config", "cargo_body_type",
            "load_type", "hazmat_present_i",
        ],
    },
    "people": {
        "id": "u6pd-qa9d",
        "format": "csv",
        "filename": "chicago_people_2018_2025.csv",
        "order": "person_id",
        "columns": [
            "person_id", "person_type", "crash_record_id", "vehicle_id",
            "crash_date", "seat_no", "sex", "age", "safety_equipment",
            "airbag_deployed", "ejection", "injury_classification",
            "driver_action", "driver_vision", "physical_condition",
            "pedpedal_action", "pedpedal_visibility", "pedpedal_location",
            "bac_result", "bac_result_value", "cell_phone_use",
        ],
    },
}

NOAA_STATIONS = {
    "72530094846": "Chicago O'Hare International Airport",
    "72534014819": "Chicago Midway Airport",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        default=".data-build/traffic/chicago-2018-2025-v1/source",
    )
    parser.add_argument("--page-size", type=int, default=PAGE_SIZE)
    parser.add_argument(
        "--smoke-test",
        action="store_true",
        help="Download two days from 2025 plus 2025 weather for SQL testing",
    )
    return parser.parse_args()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def open_url(url: str, timeout: int = 180):
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    for attempt in range(1, 6):
        try:
            return urllib.request.urlopen(request, timeout=timeout)
        except urllib.error.HTTPError as error:
            body = error.read().decode("utf-8", errors="replace")
            if 400 <= error.code < 500 and error.code != 429:
                raise RuntimeError(
                    f"HTTP {error.code} for {url}\n{body[:2000]}"
                ) from error
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


def resource_url(dataset_id: str, extension: str, params: dict[str, str | int]) -> str:
    query = urllib.parse.urlencode(params, safe="(),:*' >=<")
    return f"https://data.cityofchicago.org/resource/{dataset_id}.{extension}?{query}"


def get_count(dataset_id: str, start: str, end: str) -> int:
    url = resource_url(
        dataset_id,
        "json",
        {
            "$select": "count(*)",
            "$where": f"crash_date >= '{start}' AND crash_date < '{end}'",
        },
    )
    with open_url(url) as response:
        return int(json.load(response)[0]["count"])


def download_socrata_table(
    spec: dict, destination: Path, page_size: int, start: str, end: str
) -> dict:
    expected = get_count(spec["id"], start, end)
    temporary = destination.with_suffix(destination.suffix + ".partial")
    if destination.exists():
        raise FileExistsError(f"Refusing to overwrite source snapshot: {destination}")
    temporary.unlink(missing_ok=True)
    print(f"{destination.name}: expecting {expected:,} rows", flush=True)

    rows_written = 0
    with temporary.open("w", encoding="utf-8", newline="") as output:
        writer = csv.writer(output, lineterminator="\n")
        while rows_written < expected:
            source_format = spec["format"]
            url = resource_url(
                spec["id"],
                source_format,
                {
                    "$select": ",".join(spec["columns"]),
                    "$where": f"crash_date >= '{start}' AND crash_date < '{end}'",
                    "$order": spec["order"],
                    "$limit": page_size,
                    "$offset": rows_written,
                },
            )
            with open_url(url) as response:
                if source_format == "json":
                    records = json.load(response)
                    fields = [column.strip("`") for column in spec["columns"]]
                    if rows_written == 0:
                        writer.writerow(fields)
                    for record in records:
                        writer.writerow([record.get(field, "") for field in fields])
                    page_rows = len(records)
                else:
                    reader = csv.reader(
                        (line.decode("utf-8-sig") for line in response), strict=True
                    )
                    header = next(reader)
                    if rows_written == 0:
                        writer.writerow(header)
                    page_rows = 0
                    for row in reader:
                        writer.writerow(row)
                        page_rows += 1
            if page_rows == 0:
                raise RuntimeError(
                    f"Socrata returned no rows after offset {rows_written:,}"
                )
            rows_written += page_rows
            print(f"  {destination.name}: {rows_written:,}/{expected:,}", flush=True)

    if rows_written != expected:
        raise RuntimeError(
            f"Expected {expected:,} rows from {spec['id']}; downloaded {rows_written:,}"
        )
    os.replace(temporary, destination)
    return {
        "dataset_id": spec["id"],
        "source_url": f"https://data.cityofchicago.org/d/{spec['id']}",
        "filename": destination.name,
        "rows": rows_written,
        "bytes": destination.stat().st_size,
        "sha256": sha256(destination),
    }


def download_zip_regions(destination: Path) -> dict:
    if destination.exists():
        raise FileExistsError(f"Refusing to overwrite source snapshot: {destination}")
    url = resource_url(
        "rpca-8um6",
        "csv",
        {"$select": "_feature_id,zip", "$order": "_feature_id", "$limit": 5000},
    )
    with open_url(url) as response, destination.open("wb") as output:
        shutil.copyfileobj(response, output)
    return {
        "dataset_id": "rpca-8um6",
        "source_url": "https://data.cityofchicago.org/d/rpca-8um6",
        "filename": destination.name,
        "bytes": destination.stat().st_size,
        "sha256": sha256(destination),
    }


def download_noaa(output_dir: Path, years: range) -> list[dict]:
    output_dir.mkdir()
    files = []
    for station_id, station_name in NOAA_STATIONS.items():
        for year in years:
            destination = output_dir / f"{station_id}_{year}.csv"
            if destination.exists():
                raise FileExistsError(f"Refusing to overwrite source snapshot: {destination}")
            url = (
                "https://www.ncei.noaa.gov/data/local-climatological-data/"
                f"access/{year}/{station_id}.csv"
            )
            print(f"Downloading NOAA {station_name}, {year}", flush=True)
            with open_url(url) as response, destination.open("wb") as output:
                shutil.copyfileobj(response, output)
            files.append(
                {
                    "station_id": station_id,
                    "station_name": station_name,
                    "year": year,
                    "source_url": url,
                    "filename": f"noaa_lcd/{destination.name}",
                    "bytes": destination.stat().st_size,
                    "sha256": sha256(destination),
                }
            )
    return files


def main() -> int:
    args = parse_args()
    output = Path(args.output).resolve()
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite source directory: {output}")
    output.mkdir(parents=True)

    start = "2025-01-01T00:00:00" if args.smoke_test else START
    end = "2025-01-03T00:00:00" if args.smoke_test else END
    weather_years = range(2025, 2026) if args.smoke_test else range(2018, 2026)
    manifest = {
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "coverage": {"start_inclusive": start, "end_exclusive": end},
        "smoke_test": args.smoke_test,
        "city_of_chicago": [],
        "noaa_lcd": [],
    }
    for name, spec in SOCRATA.items():
        details = download_socrata_table(
            spec, output / spec["filename"], args.page_size, start, end
        )
        details["name"] = name
        manifest["city_of_chicago"].append(details)
    manifest["city_of_chicago"].append(
        {"name": "zip_regions", **download_zip_regions(output / "chicago_zip_regions.csv")}
    )
    manifest["noaa_lcd"] = download_noaa(output / "noaa_lcd", weather_years)
    (output / "source_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n", encoding="utf-8"
    )
    print(f"Source snapshot completed at {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Download failed: {error}", file=sys.stderr)
        raise SystemExit(1)
