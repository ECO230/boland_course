#!/usr/bin/env python3
"""Build the immutable local NFL 2021-2025 candidate release."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import duckdb


EXPECTED_DUCKDB_VERSION = "1.5.2"
ARTIFACTS = {
    "games": "nfl_games.parquet",
    "plays": "nfl_plays.parquet",
    "player_game_stats": "nfl_player_game_stats.parquet",
    "team_game_stats": "nfl_team_game_stats.parquet",
    "players": "nfl_players.parquet",
    "rosters": "nfl_rosters.parquet",
    "teams": "nfl_teams.parquet",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sql_path(path: Path) -> str:
    return path.resolve().as_posix().replace("'", "''")


def run_sql(connection: duckdb.DuckDBPyConnection, path: Path, **values: str) -> None:
    sql = path.read_text(encoding="utf-8")
    for key, value in values.items():
        sql = sql.replace("{{" + key + "}}", value)
    if "{{" in sql or "}}" in sql:
        raise ValueError(f"Unresolved SQL placeholder in {path}")
    started = time.perf_counter()
    print(f"Starting {path.name}...", flush=True)
    connection.execute(sql)
    print(f"Completed {path.name} in {time.perf_counter() - started:,.1f}s", flush=True)


def verify_sources(source_dir: Path, manifest: dict) -> None:
    files = manifest.get("files", [])
    if len(files) != 23:
        raise ValueError(f"Expected 23 source artifacts; found {len(files)}")
    for details in files:
        path = source_dir / details["filename"]
        if not path.is_file():
            raise FileNotFoundError(f"Missing source artifact: {path}")
        if path.stat().st_size != details["bytes"] or sha256(path) != details["sha256"]:
            raise ValueError(f"Source identity mismatch: {path.name}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--source-dir",
        default=".data-build/nfl/nfl-complete-2021-2025-v1/source",
    )
    parser.add_argument(
        "--output",
        default=".data-build/nfl/nfl-complete-2021-2025-v1",
    )
    args = parser.parse_args()
    if duckdb.__version__ != EXPECTED_DUCKDB_VERSION:
        raise RuntimeError(
            f"Expected DuckDB {EXPECTED_DUCKDB_VERSION}; found {duckdb.__version__}"
        )

    dataset_dir = Path(__file__).resolve().parent
    source_dir = Path(args.source_dir).resolve()
    output_root = Path(args.output).resolve()
    work_dir, release_dir = output_root / "work", output_root / "release"
    if work_dir.exists() or release_dir.exists():
        raise FileExistsError(f"Refusing to overwrite existing build: {output_root}")

    manifest_path = source_dir / "source_manifest.json"
    if not manifest_path.is_file():
        raise FileNotFoundError(f"Missing source manifest: {manifest_path}")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    verify_sources(source_dir, manifest)
    work_dir.mkdir(parents=True)
    release_dir.mkdir(parents=True)

    connection = duckdb.connect(str(work_dir / "nfl.duckdb"))
    generated: dict[str, dict[str, object]] = {}
    try:
        run_sql(
            connection,
            dataset_dir / "sql" / "10_ingest.sql",
            PBP_GLOB=sql_path(source_dir / "pbp_*.parquet"),
            PLAYER_STATS_GLOB=sql_path(source_dir / "player_stats_week_*.parquet"),
            TEAM_STATS_GLOB=sql_path(source_dir / "team_stats_week_*.parquet"),
            ROSTER_GLOB=sql_path(source_dir / "roster_*.parquet"),
            GAMES_PATH=sql_path(source_dir / "games.parquet"),
            PLAYERS_PATH=sql_path(source_dir / "players.parquet"),
            TEAMS_PATH=sql_path(source_dir / "teams.parquet"),
        )
        run_sql(connection, dataset_dir / "sql" / "20_model.sql")
        run_sql(connection, dataset_dir / "sql" / "30_validate.sql")
        paths = {name: release_dir / filename for name, filename in ARTIFACTS.items()}
        run_sql(
            connection,
            dataset_dir / "sql" / "40_publish.sql",
            **{f"OUTPUT_{name.upper()}": sql_path(path) for name, path in paths.items()},
        )
        for name, path in paths.items():
            schema_rows = connection.execute(
                f"DESCRIBE SELECT * FROM curated.{name}"
            ).fetchall()
            generated[name] = {
                "filename": path.name,
                "sha256": sha256(path),
                "bytes": path.stat().st_size,
                "rows": connection.execute(
                    f"SELECT count(*) FROM curated.{name}"
                ).fetchone()[0],
                "columns": len(schema_rows),
                "schema": [
                    {
                        "name": row[0],
                        "duckdb_type": row[1],
                        "nullable": row[2] == "YES",
                    }
                    for row in schema_rows
                ],
            }
    finally:
        connection.close()

    metadata = json.loads((dataset_dir / "dataset.json").read_text(encoding="utf-8"))
    metadata["release_status"] = "local-candidate"
    metadata["built_at_utc"] = datetime.now(timezone.utc).isoformat()
    metadata["build"] = {"duckdb_version": duckdb.__version__, "threads": 1}
    for artifact in metadata["artifacts"]:
        artifact.update(generated[artifact["name"]])
    (release_dir / "metadata.json").write_text(
        json.dumps(metadata, indent=2) + "\n",
        encoding="utf-8",
    )
    (release_dir / "schemas.json").write_text(
        json.dumps({name: details["schema"] for name, details in generated.items()}, indent=2)
        + "\n",
        encoding="utf-8",
    )
    shutil.copy2(manifest_path, release_dir)
    shutil.copy2(dataset_dir / "LICENSE-DATA.txt", release_dir)
    shutil.copy2(dataset_dir / "data_dictionary.md", release_dir / "DATA-DICTIONARY.md")
    for details in generated.values():
        (release_dir / f"{details['filename']}.sha256").write_text(
            f"{details['sha256']}  {details['filename']}\n",
            encoding="ascii",
        )

    print(f"Built NFL local candidate at {release_dir}")
    for name, details in generated.items():
        print(f"{name}: {details['rows']:,} rows, {details['bytes']:,} bytes")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as error:
        print(f"Build failed: {error}", file=sys.stderr)
        raise SystemExit(1)
