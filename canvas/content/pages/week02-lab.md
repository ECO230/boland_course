---
schema_version: 1
key: lab-2-descriptive-statistics-and-cross-tabs
canvas_type: page
visibility: course_only
required_values:
  - lab_02_download_url
  - lab_02_posit_cloud_url
---

## In-Class Lab 2: Descriptive Statistics, Cross-Tabulations, and Joins

### Purpose

Use the same relational NFL data to create and verify summaries in Excel,
Tableau, and R. The lab uses classroom-sized extracts from the current NFL
games and plays tables; it does not use player-tracking data.

### Files and project

- [Download the Lab 2 NFL files]({{ lab_02_download_url }})
- [Open the Lab 2 project in Posit Cloud]({{ lab_02_posit_cloud_url }})

The download contains `nfl_games.csv`, `nfl_plays.csv`, and `source.json`.
Confirm that
`game_id` is unique in the games table and repeated in the plays table before
joining them.

This is the in-class lab dataset, not a lab-preparation example.

### Tasks

1. Import both files into Excel and format each as an Excel table.
2. State the grain of each table and predict the relationship between them.
3. Use `XLOOKUP` to bring selected game-level fields into the plays table using
   `game_id`. Verify the result for several games and investigate unmatched
   keys.
4. Create a PivotTable that reports at least one count, one percentage, and one
   appropriate quantitative summary by a categorical variable.
5. Load the same files in Tableau. Relate or join them using `game_id`, verify
   the resulting row behavior, and reproduce one Excel summary.
6. Open the Posit Cloud project. Use `left_join()` to add game fields to plays,
   check the row count before and after the join, and reproduce the same summary
   in R.
7. Compare the results across the three tools. Resolve any difference before
   deciding which result is correct.

### Completion check

Keep the Excel workbook and rendered Quarto document available for class. Only
upload them if the accompanying Canvas assignment requests a submission.
