---
schema_version: 1
key: lab-2-descriptive-statistics-and-cross-tabs
canvas_type: page
visibility: course_only
required_values:
  - lab_02_games_url
  - lab_02_plays_url
  - lab_02_excel_url
  - lab_02_posit_cloud_url
---

## In-Class Lab 2: Descriptive Statistics, Cross-Tabulations, and Joins

### Purpose

Use the same relational NFL data to create and verify summaries in Excel,
Tableau, and R. The lab uses classroom-sized extracts from the current NFL
games and plays tables; it does not use player-tracking data.

### Files and project

- [Download the games CSV]({{ lab_02_games_url }})
- [Download the plays CSV]({{ lab_02_plays_url }})
- [Download the Excel starter workbook]({{ lab_02_excel_url }})
- [Open the Lab 2 project in Posit Cloud]({{ lab_02_posit_cloud_url }})

The workbook contains plain `Plays` and `Games` sheets. After the original play
columns, the `Plays` sheet includes every non-key game-level field added with
`XLOOKUP`; its existing `game_id` column supplies the shared key. Use the two
individual CSV files for Tableau and R. Confirm that
`game_id` is unique in the games table and repeated in the plays table before
joining them.

This is the in-class lab dataset, not a lab-preparation example.

### Tasks

1. Open the starter workbook and inspect the `Plays` and `Games` sheets.
2. State the grain of each table and predict the relationship between them.
3. Inspect the existing `XLOOKUP` formulas that bring all 17 non-key
   game-level fields into `Plays` using `game_id`. Verify several results and
   investigate any unmatched keys.
4. Format each data range as an Excel Table.
5. Create a PivotTable that reports at least one count, one percentage, and one
   appropriate quantitative summary by a categorical variable.
6. Load the two CSV files in Tableau. Relate or join them using `game_id`, verify
   the resulting row behavior, and reproduce one Excel summary.
7. Open the Posit Cloud project. Use `left_join()` to add game fields to plays,
   check the row count before and after the join, and reproduce the same summary
   in R.
8. Compare the results across the three tools. Resolve any difference before
   deciding which result is correct.

### Completion check

Keep the Excel workbook and rendered Quarto document available for class. Only
upload them if the accompanying Canvas assignment requests a submission.
