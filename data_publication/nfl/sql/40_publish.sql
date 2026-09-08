COPY (SELECT * FROM curated.games ORDER BY game_date, game_id)
TO '{{OUTPUT_GAMES}}' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (SELECT * FROM curated.plays ORDER BY season, week, game_id, play_id)
TO '{{OUTPUT_PLAYS}}' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (SELECT * FROM curated.player_game_stats ORDER BY season, week, game_id, team, player_id)
TO '{{OUTPUT_PLAYER_GAME_STATS}}' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (SELECT * FROM curated.team_game_stats ORDER BY season, week, game_id, team)
TO '{{OUTPUT_TEAM_GAME_STATS}}' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (SELECT * FROM curated.players ORDER BY player_id)
TO '{{OUTPUT_PLAYERS}}' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (SELECT * FROM curated.rosters ORDER BY season, team, player_id)
TO '{{OUTPUT_ROSTERS}}' (FORMAT PARQUET, COMPRESSION ZSTD);

COPY (SELECT * FROM curated.teams ORDER BY team_abbr)
TO '{{OUTPUT_TEAMS}}' (FORMAT PARQUET, COMPRESSION ZSTD);
