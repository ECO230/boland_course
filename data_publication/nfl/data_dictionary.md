# NFL Games, Plays, Players, and Team Performance, 2021-2025

## Scope

This release contains complete 2021 through 2025 NFL regular seasons and
postseasons. The five-season window begins with the league's 17-game regular-
season era and ends with the completed 2025 postseason in February 2026.
Incomplete 2026 results are intentionally excluded.

The source is nflverse-data. This is conventional play-by-play, schedule,
roster, player, and game-statistical data. It does not contain frame-by-frame
player coordinates or other tracking records.

## Join keys and grain

- `game_id` identifies a game across games, plays, player-game statistics, and
  team-game statistics.
- `(game_id, play_id)` uniquely identifies a play.
- `player_id` is the nflverse/GSIS player identifier used across players,
  rosters, player-game statistics, and selected play participants.
- `team_abbr` is the primary key of the team lookup. Other tables use contextual
  names such as `team`, `home_team`, `away_team`, `possession_team`, and
  `defensive_team`.
- `(season, team, player_id)` identifies a season roster record.

Never join a one-row-per-game table directly to plays and then sum game-level
measures: every game value would repeat once per play. Aggregate the many-side
table to the intended analytical grain before joining.

## `nfl_games.parquet`

One row per game. It includes season and playoff round, date/time, home and away
teams and scores, rest days, betting lines, venue conditions, starting
quarterbacks, coaches, referee, and stadium. `season_type` is `REG` or `POST`;
`playoff_round` preserves nflverse values `WC`, `DIV`, `CON`, and `SB`.

## `nfl_plays.parquet`

One row per nondeleted nflverse play record. Major field groups are:

- identity and situation: game/play keys, season, week, date, teams, field
  position, quarter, down, distance, clock, drive, description, and play type;
- design: shotgun, no-huddle, dropback, scramble, pass depth/location, air
  yards, yards after catch, and run location/gap;
- results: yards, first downs, conversions, sacks, hits, turnovers, penalties,
  touchdowns, kicks, punts, kickoffs, and returns;
- score and models: score before/after, expected points, EPA, win probability,
  WPA, success, completion probability/CPOE, and expected pass/pass rate over
  expected;
- participants: passer, receiver, rusher, sacker, interceptor, kicker, punter,
  punt returner, and kickoff returner identifiers and names.

Boolean play-result fields can be aggregated with `sum(CAST(field AS INTEGER))`
or summarized with proportions. Probability fields range from zero to one.
EPA and WPA are model-derived nflverse measures, not observed physical values.

## `nfl_player_game_stats.parquet`

One row per player/team/game after removing empty source placeholder rows. It
retains nflverse weekly box-score fields across passing, rushing, receiving,
defense, fumbles, penalties, returns, kicking, punting, and fantasy scoring.
List-like strings that repeat individual kick distances are omitted; numeric
attempt, make, miss, distance, and percentage fields remain.

The player field definitions retain their nflverse names. Upstream dictionary:

https://nflreadr.nflverse.com/articles/dictionary_player_stats.html

## `nfl_team_game_stats.parquet`

One row per team/game, exactly two rows per game. It retains the team-level
counterparts of the offensive, defensive, return, kicking, and punting measures
from nflverse. Individual kick-distance list strings are omitted.

Upstream dictionary:

https://nflreadr.nflverse.com/articles/dictionary_team_stats.html

## `nfl_players.parquet`

One row per represented player. Fields include name, suffix, birth date,
position and position group, height in inches, weight in pounds, college and
conference, rookie and last seasons, latest team and status, experience, and
draft year/round/pick/team.

`record_source` is `nflverse_players` when the main player lookup supplied the
record and `season_roster_fallback` when a roster supplied a player missing from
that lookup. Headshots, profile URLs, and unrelated external-platform IDs are
excluded.

Age should be calculated for the event being analyzed rather than stored as a
current value. In DuckDB, after joining players to games:

```sql
date_diff('year', birth_date, game_date)
  - CASE WHEN (month(game_date), day(game_date))
             < (month(birth_date), day(birth_date))
         THEN 1 ELSE 0 END
```

## `nfl_rosters.parquet`

One row per season/team/player. It contains jersey number, position, depth-chart
position, status, experience, entry/rookie years, and draft club/pick. Join to
`nfl_players.parquet` for birth date, height, weight, and college fields.

## `nfl_teams.parquet`

One row per team represented in the five-season games. It contains abbreviation,
name, nickname, conference, division, and primary/secondary color hex codes.
Team logo and wordmark URLs are excluded.

## Source modifications

- Limited all event and statistic tables to 2021-2025 regular and postseason
  games.
- Harmonized postseason schedule codes to `season_type = 'POST'` while retaining
  the original playoff round.
- Renamed selected play and game columns for clarity and converted indicator
  fields to booleans.
- Removed empty player-stat placeholders lacking player identifiers.
- Added roster fallback records so every curated roster/player-stat identifier
  has a player lookup row.
- Removed tracking, images/profile links, external account identifiers, and
  individual kick-distance list strings.

No student, customer, betting-account, or private personal records are present.
