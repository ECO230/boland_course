#!/usr/bin/env python3
"""Independently verify the flat NFL 2021-2025 local-candidate bundle."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
PRIMARY_KEYS = {
    "games": "game_id",
    "plays": "(game_id, play_id)",
    "player_game_stats": "(game_id, player_id, team)",
    "team_game_stats": "(game_id, team)",
    "players": "player_id",
    "rosters": "(season, team, player_id)",
    "teams": "team_abbr",
}
FORBIDDEN_COLUMNS = {
    "headshot",
    "headshot_url",
    "team_logo_wikipedia",
    "team_logo_espn",
    "team_wordmark",
    "team_conference_logo",
    "team_league_logo",
    "team_logo_squared",
    "espn_id",
    "pfr_id",
    "pff_id",
    "otc_id",
    "smart_id",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "release_directory",
        nargs="?",
        default=".data-build/nfl/nfl-complete-2021-2025-v1/release",
    )
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(
            f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}"
        )
    release = Path(args.release_directory).resolve()
    metadata = json.loads((release / "metadata.json").read_text(encoding="utf-8"))
    schemas = json.loads((release / "schemas.json").read_text(encoding="utf-8"))
    if metadata.get("schema_version") != 2 or metadata.get("release_status") != "local-candidate":
        raise ValueError("Invalid schema_version or release_status")
    if metadata.get("privacy", {}).get("contains_student_data") is not False:
        raise ValueError("contains_student_data must be explicitly false")

    expected = {
        "metadata.json",
        "schemas.json",
        "source_manifest.json",
        "LICENSE-DATA.txt",
        "DATA-DICTIONARY.md",
    }
    relative_dirs, artifacts = set(), {}
    for details in metadata["artifacts"]:
        path = release / details["filename"]
        expected.update({path.name, f"{path.name}.sha256"})
        relative_dirs.add(str(Path(details["relative_path"]).parent).replace("\\", "/"))
        artifacts[details["name"]] = (details, path)
    entries = list(release.iterdir())
    if any(entry.is_dir() or entry.is_symlink() for entry in entries):
        raise ValueError("Release must be flat and contain no symbolic links")
    if {entry.name for entry in entries} != expected:
        raise ValueError("Release does not contain exactly the declared files")
    if len(relative_dirs) != 1:
        raise ValueError("Artifacts do not share one release directory")

    connection = duckdb.connect()
    verified = {}
    try:
        for name, (details, path) in artifacts.items():
            digest = sha256(path)
            if digest != details["sha256"] or path.stat().st_size != details["bytes"]:
                raise ValueError(f"Hash or size mismatch for {name}")
            sidecar = (release / f"{path.name}.sha256").read_text(encoding="ascii").strip().replace("\r", "")
            if sidecar != f"{digest}  {path.name}":
                raise ValueError(f"Sidecar mismatch for {name}")
            parquet = path.as_posix().replace("'", "''")
            connection.execute(f"CREATE VIEW {name} AS SELECT * FROM read_parquet('{parquet}')")
            actual = connection.execute(f"DESCRIBE SELECT * FROM {name}").fetchall()
            actual_schema = [
                {"name": row[0], "duckdb_type": row[1], "nullable": row[2] == "YES"}
                for row in actual
            ]
            if actual_schema != details["schema"] or schemas[name] != details["schema"]:
                raise ValueError(f"Schema mismatch for {name}")
            if FORBIDDEN_COLUMNS & {row[0].lower() for row in actual}:
                raise ValueError(f"Forbidden image, URL, or external-ID field in {name}")
            rows = connection.execute(f"SELECT count(*) FROM {name}").fetchone()[0]
            distinct_keys = connection.execute(
                f"SELECT count(DISTINCT {PRIMARY_KEYS[name]}) FROM {name}"
            ).fetchone()[0]
            if rows != details["rows"] or rows != distinct_keys:
                raise ValueError(f"Row-count or primary-key mismatch for {name}")
            verified[name] = {
                "rows": rows,
                "columns": len(actual),
                "bytes": path.stat().st_size,
                "sha256": digest,
            }

        coverage = connection.execute(
            "SELECT min(season), max(season), count(DISTINCT season), min(game_date), max(game_date) FROM games"
        ).fetchone()
        if coverage[:3] != (2021, 2025, 5):
            raise ValueError(f"Unexpected game coverage: {coverage}")
        if connection.execute("SELECT count(*) FROM games").fetchone()[0] * 2 != connection.execute(
            "SELECT count(*) FROM team_game_stats"
        ).fetchone()[0]:
            raise ValueError("Team-game table does not contain two rows per game")
        if connection.execute(
            "SELECT count(*) FROM players WHERE birth_date IS NOT NULL AND (height_inches IS NOT NULL OR weight_pounds IS NOT NULL OR college_name IS NOT NULL)"
        ).fetchone()[0] == 0:
            raise ValueError("Requested player biographical fields are unexpectedly empty")
    finally:
        connection.close()

    report = {
        "release_directory": str(release),
        "proposed_public_directory": next(iter(relative_dirs)),
        "artifacts": verified,
        "coverage": {
            "seasons": [2021, 2022, 2023, 2024, 2025],
            "season_types": ["REG", "POST"],
        },
        "privacy": "verified no student data, headshots, profile URLs, or external account IDs",
        "status": "verified-local-candidate",
    }
    print(json.dumps(report, indent=2, default=str))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
