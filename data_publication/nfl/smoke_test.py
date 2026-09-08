#!/usr/bin/env python3
"""Print compact analytical sanity checks for a built NFL release."""

from __future__ import annotations

import argparse
from pathlib import Path

import duckdb


def parquet_path(release: Path, filename: str) -> str:
    return (release / filename).resolve().as_posix().replace("'", "''")


def show(connection: duckdb.DuckDBPyConnection, title: str, query: str) -> None:
    print(f"\n{title}")
    result = connection.execute(query)
    print(" | ".join(column[0] for column in result.description))
    for row in result.fetchall():
        print(" | ".join(str(value) for value in row))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "release",
        nargs="?",
        default=".data-build/nfl/nfl-complete-2021-2025-v1/release",
    )
    release = Path(parser.parse_args().release)
    paths = {
        "games": parquet_path(release, "nfl_games.parquet"),
        "plays": parquet_path(release, "nfl_plays.parquet"),
        "players": parquet_path(release, "nfl_players.parquet"),
        "player_stats": parquet_path(release, "nfl_player_game_stats.parquet"),
        "team_stats": parquet_path(release, "nfl_team_game_stats.parquet"),
    }
    connection = duckdb.connect()
    try:
        for name, path in paths.items():
            connection.execute(f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{path}')")
        show(
            connection,
            "Games and scoring by season",
            """SELECT season, count(*) AS games, round(avg(total_points), 1) AS mean_points
               FROM games GROUP BY season ORDER BY season""",
        )
        show(
            connection,
            "Plays by broad type",
            """SELECT play_type, count(*) AS plays, round(avg(yards_gained), 2) AS mean_yards
               FROM plays GROUP BY play_type ORDER BY plays DESC LIMIT 15""",
        )
        show(
            connection,
            "Team offensive output",
            """SELECT team, count(*) AS team_games,
                      round(avg(passing_yards + rushing_yards), 1) AS mean_offensive_yards
               FROM team_stats GROUP BY team ORDER BY mean_offensive_yards DESC LIMIT 10""",
        )
        show(
            connection,
            "Most-used passers",
            """SELECT player_display_name, sum(attempts) AS attempts,
                      sum(passing_yards) AS passing_yards
               FROM player_stats WHERE position = 'QB'
               GROUP BY player_id, player_display_name ORDER BY attempts DESC LIMIT 10""",
        )
        show(
            connection,
            "Player age example",
            """SELECT s.player_display_name, g.game_date, p.birth_date,
                      date_diff('year', p.birth_date, g.game_date)
                        - CASE WHEN (month(g.game_date), day(g.game_date))
                                   < (month(p.birth_date), day(p.birth_date))
                               THEN 1 ELSE 0 END AS age_on_game_date
               FROM player_stats s
               JOIN players p USING (player_id)
               JOIN games g USING (game_id)
               WHERE p.birth_date IS NOT NULL
               ORDER BY g.game_date DESC, s.player_display_name LIMIT 10""",
        )
    finally:
        connection.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
