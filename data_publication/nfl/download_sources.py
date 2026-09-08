#!/usr/bin/env python3
"""Download and pin the official nflverse 2021-2025 Parquet sources."""

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
from datetime import datetime, timezone
from pathlib import Path


BASE_URL = "https://github.com/nflverse/nflverse-data/releases/download"
SEASONS = tuple(range(2021, 2026))


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def source_specs() -> list[dict[str, object]]:
    specs: list[dict[str, object]] = []
    for season in SEASONS:
        specs.extend(
            [
                {
                    "kind": "play_by_play",
                    "season": season,
                    "release_tag": "pbp",
                    "asset": f"play_by_play_{season}.parquet",
                    "filename": f"pbp_{season}.parquet",
                },
                {
                    "kind": "player_stats_week",
                    "season": season,
                    "release_tag": "stats_player",
                    "asset": f"stats_player_week_{season}.parquet",
                    "filename": f"player_stats_week_{season}.parquet",
                },
                {
                    "kind": "team_stats_week",
                    "season": season,
                    "release_tag": "stats_team",
                    "asset": f"stats_team_week_{season}.parquet",
                    "filename": f"team_stats_week_{season}.parquet",
                },
                {
                    "kind": "season_roster",
                    "season": season,
                    "release_tag": "rosters",
                    "asset": f"roster_{season}.parquet",
                    "filename": f"roster_{season}.parquet",
                },
            ]
        )
    specs.extend(
        [
            {
                "kind": "schedules",
                "season": None,
                "release_tag": "schedules",
                "asset": "games.parquet",
                "filename": "games.parquet",
            },
            {
                "kind": "players",
                "season": None,
                "release_tag": "players",
                "asset": "players.parquet",
                "filename": "players.parquet",
            },
            {
                "kind": "teams",
                "season": None,
                "release_tag": "teams",
                "asset": "teams_colors_logos.parquet",
                "filename": "teams.parquet",
            },
        ]
    )
    return specs


def download(url: str, destination: Path) -> None:
    request = urllib.request.Request(
        url,
        headers={"User-Agent": "ECO230-course-data-builder/1.0"},
    )
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
            time.sleep(min(2**attempt, 30))
    with destination.open("rb") as stream:
        if stream.read(4) != b"PAR1":
            raise ValueError(f"Downloaded file is not Parquet: {destination.name}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output",
        default=".data-build/nfl/nfl-complete-2021-2025-v1/source",
    )
    args = parser.parse_args()
    output = Path(args.output).resolve()
    if output.exists():
        raise FileExistsError(f"Refusing to overwrite source directory: {output}")
    output.mkdir(parents=True)

    files = []
    for spec in source_specs():
        url = f"{BASE_URL}/{spec['release_tag']}/{spec['asset']}"
        destination = output / str(spec["filename"])
        download(url, destination)
        files.append(
            {
                **spec,
                "source_url": url,
                "bytes": destination.stat().st_size,
                "sha256": sha256(destination),
            }
        )

    manifest = {
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "publisher": "nflverse",
        "repository": "https://github.com/nflverse/nflverse-data",
        "retrieval_interface": "Canonical Parquet release URLs used by nflreadpy",
        "coverage_seasons": list(SEASONS),
        "redistribution_status": "CC-BY-4.0 sources; attribution and upstream-rights notice retained",
        "files": files,
    }
    (output / "source_manifest.json").write_text(
        json.dumps(manifest, indent=2) + "\n",
        encoding="utf-8",
    )
    print(f"Pinned {len(files)} nflverse artifacts at {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Download failed: {error}", file=sys.stderr)
        raise SystemExit(1)
