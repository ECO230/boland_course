# Lab 2: Descriptive Statistics, Cross-Tabulations, and Joins

This project contains a classroom-sized extract from the ECO 230 NFL
2021-2025 release:

- `data/nfl_games.csv`: one row per 2025 Minnesota regular-season game;
- `data/nfl_plays.csv`: one row per play in those games;
- `data/Lab_02_Excel_Starter.xlsx`: a plain two-sheet workbook with `Plays`
  and `Games`; after the original play columns, the `Plays` sheet includes all
  17 non-key `Games` fields through completed `XLOOKUP` formulas. Together with
  the existing `game_id` key, all 18 `Games` fields are represented;
- `data/source.json`: release, filter, join-key, row-count, and checksum details.

Use the workbook for the Excel portion and the two individual CSV files for
Tableau and R. During the Excel portion, create the Tables and PivotTable
yourself.

The lab sequence is counts and a frequency table, grouped means, a
play-type-by-quarter cross-tabulation with percentages, and an Excel Data Model
measure for the median. The project document reproduces the join and these
summaries with direct `dplyr` verbs so the analysis can be rerun without
repeating the Excel or Tableau point-and-click steps.

The unlisted [Lab 2 analysis walkthrough](https://eco230.github.io/boland_course/week02/labs/lab-2-analysis-walkthrough.html)
provides the live-demo order, interpretation prompts, and troubleshooting
reminders used in class.

Run `source("project_setup.R")` once, open `Lab_02_IP.qmd`, and render it before
making changes. Keep `game_id` as the join key. Do not sum game-level values
after joining them onto plays unless you first return to the intended grain.

There is nothing to submit for Lab 2. Keep the workbook, notes, and rendered
Quarto document for reference when completing similar analysis in Homework 2.

Source: nflverse data, distributed under CC BY 4.0 and curated for ECO 230.

## Rebuild from the repository

To refresh the two CSV files from the configured NFL release and then rebuild
the workbook, run this from the course repository in PowerShell:

```powershell
powershell.exe -ExecutionPolicy Bypass -File scripts\build_lab_02_workbook.ps1 -RefreshData
```

Without `-RefreshData`, the same command rebuilds only the workbook from the
current CSV files. The build does not create a ZIP file.
