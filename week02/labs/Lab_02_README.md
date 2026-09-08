# Lab 2: Descriptive Statistics, Cross-Tabulations, and Joins

This project contains a classroom-sized extract from the ECO 230 NFL
2021-2025 release:

- `data/nfl_games.csv`: one row per 2025 Minnesota regular-season game;
- `data/nfl_plays.csv`: one row per play in those games;
- `data/source.json`: release, filter, join-key, row-count, and checksum details.

Run `source("project_setup.R")` once, open `Lab_02_IP.qmd`, and render it before
making changes. Keep `game_id` as the join key. Do not sum game-level values
after joining them onto plays unless you first return to the intended grain.

Source: nflverse data, distributed under CC BY 4.0 and curated for ECO 230.
