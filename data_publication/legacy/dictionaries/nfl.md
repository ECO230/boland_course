# Legacy NFL special-teams data dictionary

Artifact: `legacy_nfl_special_teams.parquet`

Grain: one player-play record for a special-teams play in the 2018-2020 NFL
seasons. `legacy_source_index` is the unique release row key; `game_id`,
`play_id`, and `nfl_id` support game/play/player grouping. Play situation,
scores, special-teams result, kick and return measures, player biography,
game context, and PFF scouting fields retain their Big Data Bowl meanings.

The table repeats play fields once per associated player and therefore must be
deduplicated or aggregated before play-level analysis. It is not frame-level
tracking data. Public redistribution is prohibited by the competition rules.
