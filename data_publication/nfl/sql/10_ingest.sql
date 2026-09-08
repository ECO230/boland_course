CREATE SCHEMA raw;

CREATE VIEW raw.plays AS
SELECT * FROM read_parquet('{{PBP_GLOB}}', union_by_name = true);

CREATE VIEW raw.player_game_stats AS
SELECT * FROM read_parquet('{{PLAYER_STATS_GLOB}}', union_by_name = true);

CREATE VIEW raw.team_game_stats AS
SELECT * FROM read_parquet('{{TEAM_STATS_GLOB}}', union_by_name = true);

CREATE VIEW raw.rosters AS
SELECT * FROM read_parquet('{{ROSTER_GLOB}}', union_by_name = true);

CREATE VIEW raw.games AS
SELECT * FROM read_parquet('{{GAMES_PATH}}');

CREATE VIEW raw.players AS
SELECT * FROM read_parquet('{{PLAYERS_PATH}}');

CREATE VIEW raw.teams AS
SELECT * FROM read_parquet('{{TEAMS_PATH}}');
