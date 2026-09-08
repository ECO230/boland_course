-- Coverage and primary-key checks.
SELECT CASE WHEN min(season) = 2021 AND max(season) = 2025 AND count(DISTINCT season) = 5
  THEN true ELSE error('NFL game coverage does not contain exactly 2021-2025') END
FROM curated.games;

WITH key_checks AS (
  SELECT 'games' AS table_name, count(*) AS rows,
         count(game_id) AS nonnull, count(DISTINCT game_id) AS distinct_keys
  FROM curated.games
  UNION ALL
  SELECT 'plays', count(*), count(game_id) FILTER (WHERE play_id IS NOT NULL),
         count(DISTINCT (game_id, play_id)) FROM curated.plays
  UNION ALL
  SELECT 'player_game_stats', count(*), count(game_id) FILTER (WHERE player_id IS NOT NULL AND team IS NOT NULL),
         count(DISTINCT (game_id, player_id, team)) FROM curated.player_game_stats
  UNION ALL
  SELECT 'team_game_stats', count(*), count(game_id) FILTER (WHERE team IS NOT NULL),
         count(DISTINCT (game_id, team)) FROM curated.team_game_stats
  UNION ALL
  SELECT 'players', count(*), count(player_id), count(DISTINCT player_id) FROM curated.players
  UNION ALL
  SELECT 'rosters', count(*), count(player_id) FILTER (WHERE season IS NOT NULL AND team IS NOT NULL),
         count(DISTINCT (season, team, player_id)) FROM curated.rosters
  UNION ALL
  SELECT 'teams', count(*), count(team_abbr), count(DISTINCT team_abbr) FROM curated.teams
)
SELECT CASE WHEN bool_and(rows > 0 AND rows = nonnull AND rows = distinct_keys)
  THEN true ELSE error('A curated NFL primary key is null, duplicated, or empty') END
FROM key_checks;

-- A complete game has two team-game rows and referenced plays.
SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A game does not have exactly two team-game rows') END
FROM (
  SELECT g.game_id, count(t.game_id) AS team_rows
  FROM curated.games g
  LEFT JOIN curated.team_game_stats t USING (game_id)
  GROUP BY g.game_id
  HAVING count(t.game_id) <> 2
);

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A play references a game outside the curated game table') END
FROM curated.plays p
LEFT JOIN curated.games g USING (game_id)
WHERE g.game_id IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A player-game stat references an unknown game or player') END
FROM curated.player_game_stats s
LEFT JOIN curated.games g USING (game_id)
LEFT JOIN curated.players p USING (player_id)
WHERE g.game_id IS NULL OR p.player_id IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A roster row references an unknown player or team') END
FROM curated.rosters r
LEFT JOIN curated.players p USING (player_id)
LEFT JOIN curated.teams t ON t.team_abbr = r.team
WHERE p.player_id IS NULL OR t.team_abbr IS NULL;

-- Range and content checks useful for detecting source/schema drift.
SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Game dates, scores, or season types are invalid') END
FROM curated.games
WHERE game_date NOT BETWEEN DATE '2021-09-01' AND DATE '2026-02-28'
   OR home_score < 0 OR away_score < 0
   OR season_type NOT IN ('REG', 'POST');

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Play situation or probability values are outside expected bounds') END
FROM curated.plays
WHERE quarter NOT BETWEEN 1 AND 5
   OR down NOT BETWEEN 1 AND 4
   OR yards_to_go < 0
   OR possession_team_win_probability NOT BETWEEN 0 AND 1
   OR completion_probability NOT BETWEEN 0 AND 1
   OR expected_pass_probability NOT BETWEEN 0 AND 1;

SELECT CASE WHEN count(*) > 0 THEN true ELSE error('Release contains no non-special-teams plays') END
FROM curated.plays
WHERE coalesce(special_teams_play, false) = false
  AND play_type IN ('pass', 'run', 'qb_kneel', 'qb_spike');

SELECT CASE WHEN count(*) > 0 THEN true ELSE error('Release unexpectedly contains no special-teams plays') END
FROM curated.plays
WHERE special_teams_play;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Player biographical values are implausible') END
FROM curated.players
WHERE birth_date > DATE '2010-01-01'
   OR height_inches NOT BETWEEN 55 AND 90
   OR weight_pounds NOT BETWEEN 120 AND 450;
