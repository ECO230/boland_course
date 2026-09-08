#!/usr/bin/env python3
"""Build the small 2025 Minnesota NFL extract used by Lab 2."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


DATASET_ID = "nfl_complete_2021_2025"
RELEASE_VERSION = "2026-fall-v1"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def main() -> int:
    root = Path(__file__).resolve().parent.parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--release",
        type=Path,
        default=root
        / ".data-build"
        / "nfl"
        / "nfl-complete-2021-2025-v1"
        / "release",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=root / "week02" / "labs" / "data",
    )
    args = parser.parse_args()
    release = args.release.resolve()
    output = args.output.resolve()
    games_source = release / "nfl_games.parquet"
    plays_source = release / "nfl_plays.parquet"
    for source in (games_source, plays_source):
        if not source.is_file():
            raise FileNotFoundError(f"Missing NFL release artifact: {source}")
    output.mkdir(parents=True, exist_ok=True)

    games_output = output / "nfl_games.csv"
    plays_output = output / "nfl_plays.csv"
    connection = duckdb.connect()
    connection.execute(
        f"""
        COPY (
          SELECT
            game_id, season, season_type, week, game_date, weekday,
            away_team, away_score, home_team, home_score,
            home_score_margin, total_points, overtime,
            roof, surface, temperature_fahrenheit, wind_mph, stadium
          FROM read_parquet('{sql_path(games_source)}')
          WHERE season = 2025
            AND season_type = 'REG'
            AND (home_team = 'MIN' OR away_team = 'MIN')
          ORDER BY week, game_id
        ) TO '{sql_path(games_output)}' (HEADER, DELIMITER ',')
        """
    )
    connection.execute(
        f"""
        COPY (
          SELECT
            p.game_id, p.play_id, p.quarter, p.game_clock,
            p.down, p.yards_to_go, p.possession_team, p.defensive_team,
            p.side_of_field, p.yardline_100, p.play_description, p.play_type,
            p.yards_gained, p.shotgun, p.no_huddle,
            p.pass_length, p.pass_location, p.air_yards, p.yards_after_catch,
            p.run_location, p.run_gap, p.complete_pass, p.first_down,
            p.sack, p.interception, p.fumble_lost,
            p.penalty, p.penalty_yards, p.touchdown,
            p.expected_points_added, p.successful_play,
            p.win_probability_added
          FROM read_parquet('{sql_path(plays_source)}') AS p
          SEMI JOIN (
            SELECT game_id
            FROM read_parquet('{sql_path(games_source)}')
            WHERE season = 2025
              AND season_type = 'REG'
              AND (home_team = 'MIN' OR away_team = 'MIN')
          ) AS g
            ON p.game_id = g.game_id
          ORDER BY p.game_id, p.play_id
        ) TO '{sql_path(plays_output)}' (HEADER, DELIMITER ',')
        """
    )
    games_rows = connection.execute(
        f"SELECT count(*) FROM read_csv_auto('{sql_path(games_output)}', header=true)"
    ).fetchone()[0]
    plays_rows = connection.execute(
        f"SELECT count(*) FROM read_csv_auto('{sql_path(plays_output)}', header=true)"
    ).fetchone()[0]
    if games_rows != 17 or plays_rows < 1000:
        raise ValueError(
            f"Unexpected Lab 2 extract size: {games_rows} games, {plays_rows} plays"
        )
    metadata = {
        "schema_version": 1,
        "dataset_id": DATASET_ID,
        "release_version": RELEASE_VERSION,
        "filter": "2025 regular-season games involving MIN",
        "join_key": "game_id",
        "source_urls": {
            "games": "https://data.60land.com/project1/2026-fall/v1/nfl/nfl_games.parquet",
            "plays": "https://data.60land.com/project1/2026-fall/v1/nfl/nfl_plays.parquet",
        },
        "artifacts": [
            {
                "filename": games_output.name,
                "rows": games_rows,
                "sha256": sha256(games_output),
            },
            {
                "filename": plays_output.name,
                "rows": plays_rows,
                "sha256": sha256(plays_output),
            },
        ],
    }
    (output / "source.json").write_text(
        json.dumps(metadata, indent=2) + "\n", encoding="utf-8"
    )
    print(f"Built Lab 2 extract: {games_rows} games, {plays_rows} plays")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
