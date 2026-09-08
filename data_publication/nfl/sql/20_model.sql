CREATE SCHEMA curated;

CREATE TABLE curated.games AS
SELECT
  game_id,
  season::INTEGER AS season,
  CASE WHEN game_type = 'REG' THEN 'REG' ELSE 'POST' END AS season_type,
  game_type AS playoff_round,
  week::INTEGER AS week,
  try_cast(gameday AS DATE) AS game_date,
  weekday,
  gametime AS game_time,
  away_team,
  away_score::INTEGER AS away_score,
  home_team,
  home_score::INTEGER AS home_score,
  location,
  result::INTEGER AS home_score_margin,
  total::INTEGER AS total_points,
  coalesce(overtime, 0) = 1 AS overtime,
  away_rest::INTEGER AS away_rest_days,
  home_rest::INTEGER AS home_rest_days,
  away_moneyline,
  home_moneyline,
  spread_line,
  away_spread_odds,
  home_spread_odds,
  total_line,
  under_odds,
  over_odds,
  coalesce(div_game, 0) = 1 AS division_game,
  roof,
  surface,
  temp AS temperature_fahrenheit,
  wind AS wind_mph,
  away_qb_id,
  away_qb_name,
  home_qb_id,
  home_qb_name,
  away_coach,
  home_coach,
  referee,
  stadium
FROM raw.games
WHERE season BETWEEN 2021 AND 2025
  AND game_type IN ('REG', 'WC', 'DIV', 'CON', 'SB');

CREATE TABLE curated.plays AS
SELECT
  game_id,
  play_id::BIGINT AS play_id,
  season::INTEGER AS season,
  season_type,
  week::INTEGER AS week,
  try_cast(game_date AS DATE) AS game_date,
  home_team,
  away_team,
  posteam AS possession_team,
  defteam AS defensive_team,
  posteam_type AS possession_team_type,
  side_of_field,
  yardline_100,
  qtr::INTEGER AS quarter,
  game_half,
  down::INTEGER AS down,
  ydstogo::INTEGER AS yards_to_go,
  coalesce(goal_to_go, 0) = 1 AS goal_to_go,
  time AS game_clock,
  game_seconds_remaining,
  drive::INTEGER AS drive_number,
  "desc" AS play_description,
  play_type,
  play_type_nfl,
  coalesce(play, 0) = 1 AS is_play,
  yards_gained,
  shotgun = 1 AS shotgun,
  no_huddle = 1 AS no_huddle,
  qb_dropback = 1 AS qb_dropback,
  qb_kneel = 1 AS qb_kneel,
  qb_spike = 1 AS qb_spike,
  qb_scramble = 1 AS qb_scramble,
  pass_length,
  pass_location,
  air_yards,
  yards_after_catch,
  run_location,
  run_gap,
  complete_pass = 1 AS complete_pass,
  incomplete_pass = 1 AS incomplete_pass,
  first_down = 1 AS first_down,
  third_down_converted = 1 AS third_down_converted,
  third_down_failed = 1 AS third_down_failed,
  fourth_down_converted = 1 AS fourth_down_converted,
  fourth_down_failed = 1 AS fourth_down_failed,
  sack = 1 AS sack,
  qb_hit = 1 AS qb_hit,
  interception = 1 AS interception,
  fumble = 1 AS fumble,
  fumble_lost = 1 AS fumble_lost,
  penalty = 1 AS penalty,
  penalty_team,
  penalty_type,
  penalty_yards,
  touchdown = 1 AS touchdown,
  pass_touchdown = 1 AS pass_touchdown,
  rush_touchdown = 1 AS rush_touchdown,
  return_touchdown = 1 AS return_touchdown,
  two_point_attempt = 1 AS two_point_attempt,
  two_point_conv_result,
  field_goal_attempt = 1 AS field_goal_attempt,
  field_goal_result,
  extra_point_attempt = 1 AS extra_point_attempt,
  extra_point_result,
  kick_distance,
  punt_attempt = 1 AS punt_attempt,
  kickoff_attempt = 1 AS kickoff_attempt,
  return_yards,
  special_teams_play = 1 AS special_teams_play,
  st_play_type AS special_teams_play_type,
  posteam_score AS possession_team_score,
  defteam_score AS defensive_team_score,
  score_differential,
  posteam_score_post AS possession_team_score_after,
  defteam_score_post AS defensive_team_score_after,
  score_differential_post,
  ep AS expected_points_before,
  epa AS expected_points_added,
  wp AS possession_team_win_probability,
  wpa AS win_probability_added,
  success = 1 AS successful_play,
  cp AS completion_probability,
  cpoe AS completion_percentage_over_expected,
  xpass AS expected_pass_probability,
  pass_oe AS pass_rate_over_expected,
  passer_player_id,
  passer_player_name,
  receiver_player_id,
  receiver_player_name,
  rusher_player_id,
  rusher_player_name,
  sack_player_id,
  sack_player_name,
  interception_player_id,
  interception_player_name,
  kicker_player_id,
  kicker_player_name,
  punter_player_id,
  punter_player_name,
  punt_returner_player_id,
  punt_returner_player_name,
  kickoff_returner_player_id,
  kickoff_returner_player_name
FROM raw.plays
WHERE season BETWEEN 2021 AND 2025
  AND season_type IN ('REG', 'POST')
  AND play_id IS NOT NULL
  AND coalesce(play_deleted, 0) = 0;

CREATE TABLE curated.player_game_stats AS
SELECT * EXCLUDE (
  headshot_url,
  fg_made_list,
  fg_missed_list,
  fg_blocked_list,
  fg_made_distance,
  fg_missed_distance,
  fg_blocked_distance,
  gwfg_distance
)
FROM raw.player_game_stats
WHERE season BETWEEN 2021 AND 2025
  AND season_type IN ('REG', 'POST')
  AND player_id IS NOT NULL;

CREATE TABLE curated.team_game_stats AS
SELECT * EXCLUDE (
  fg_made_list,
  fg_missed_list,
  fg_blocked_list,
  fg_made_distance,
  fg_missed_distance,
  fg_blocked_distance,
  gwfg_distance
)
FROM raw.team_game_stats
WHERE season BETWEEN 2021 AND 2025
  AND season_type IN ('REG', 'POST');

CREATE TABLE curated.rosters AS
SELECT
  season::INTEGER AS season,
  team,
  gsis_id AS player_id,
  full_name AS player_name,
  football_name,
  position,
  depth_chart_position,
  jersey_number::INTEGER AS jersey_number,
  status,
  status_description_abbr AS status_description,
  years_exp::INTEGER AS years_experience,
  entry_year::INTEGER AS entry_year,
  rookie_year::INTEGER AS rookie_year,
  draft_club AS draft_team,
  draft_number::INTEGER AS draft_pick
FROM raw.rosters
WHERE season BETWEEN 2021 AND 2025
  AND gsis_id IS NOT NULL;

CREATE TEMP TABLE represented_players AS
SELECT DISTINCT player_id
FROM curated.player_game_stats
UNION
SELECT DISTINCT player_id
FROM curated.rosters;

CREATE TEMP TABLE latest_roster_player AS
SELECT * EXCLUDE (roster_rank)
FROM (
  SELECT
    gsis_id AS player_id,
    full_name AS display_name,
    first_name,
    last_name,
    football_name,
    try_cast(birth_date AS DATE) AS birth_date,
    position,
    CASE WHEN try_cast(height AS INTEGER) BETWEEN 55 AND 90
         THEN try_cast(height AS INTEGER) END AS height_inches,
    CASE WHEN try_cast(weight AS INTEGER) BETWEEN 120 AND 450
         THEN try_cast(weight AS INTEGER) END AS weight_pounds,
    college AS college_name,
    rookie_year::INTEGER AS rookie_season,
    season::INTEGER AS last_season,
    team AS latest_team,
    status,
    years_exp::INTEGER AS years_of_experience,
    rookie_year::INTEGER AS draft_year,
    NULL::INTEGER AS draft_round,
    draft_number::INTEGER AS draft_pick,
    draft_club AS draft_team,
    row_number() OVER (
      PARTITION BY gsis_id
      ORDER BY season DESC, team, coalesce(week, 0) DESC
    ) AS roster_rank
  FROM raw.rosters
  WHERE gsis_id IS NOT NULL
)
WHERE roster_rank = 1;

CREATE TABLE curated.players AS
SELECT
  p.gsis_id AS player_id,
  p.display_name,
  p.first_name,
  p.last_name,
  p.football_name,
  p.suffix,
  try_cast(p.birth_date AS DATE) AS birth_date,
  p.position_group,
  p.position,
  p.height::INTEGER AS height_inches,
  p.weight::INTEGER AS weight_pounds,
  p.college_name,
  p.college_conference,
  p.rookie_season::INTEGER AS rookie_season,
  p.last_season::INTEGER AS last_season,
  p.latest_team,
  p.status,
  p.years_of_experience::INTEGER AS years_of_experience,
  p.draft_year::INTEGER AS draft_year,
  p.draft_round::INTEGER AS draft_round,
  p.draft_pick::INTEGER AS draft_pick,
  p.draft_team,
  'nflverse_players' AS record_source
FROM raw.players p
JOIN represented_players r ON r.player_id = p.gsis_id
UNION ALL
SELECT
  r.player_id,
  r.display_name,
  r.first_name,
  r.last_name,
  r.football_name,
  NULL::VARCHAR AS suffix,
  r.birth_date,
  NULL::VARCHAR AS position_group,
  r.position,
  r.height_inches,
  r.weight_pounds,
  r.college_name,
  NULL::VARCHAR AS college_conference,
  r.rookie_season,
  r.last_season,
  r.latest_team,
  r.status,
  r.years_of_experience,
  r.draft_year,
  r.draft_round,
  r.draft_pick,
  r.draft_team,
  'season_roster_fallback' AS record_source
FROM latest_roster_player r
LEFT JOIN raw.players p ON p.gsis_id = r.player_id
WHERE p.gsis_id IS NULL;

CREATE TABLE curated.teams AS
SELECT
  t.team_abbr,
  t.team_name,
  t.team_nick AS team_nickname,
  t.team_conf AS conference,
  t.team_division AS division,
  t.team_color AS primary_color,
  t.team_color2 AS secondary_color
FROM raw.teams t
WHERE t.team_abbr IN (
  SELECT home_team FROM curated.games
  UNION
  SELECT away_team FROM curated.games
);
