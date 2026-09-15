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

Use related NFL games and plays tables to create and verify the same summaries
in Excel, Tableau, and R. The extract contains 17 Minnesota regular-season
games from 2025 and 2,791 plays from those games.

### Files and project

- [Download the games CSV]({{ lab_02_games_url }})
- [Download the plays CSV]({{ lab_02_plays_url }})
- [Download the Excel starter workbook]({{ lab_02_excel_url }})
- [Open the Lab 2 project in Posit Cloud]({{ lab_02_posit_cloud_url }})
- [Open the Lab 2 analysis walkthrough](https://eco230.github.io/boland_course/week02/labs/lab-2-analysis-walkthrough.html)

The workbook contains `Plays` and `Games` worksheets. The `Plays` worksheet
has the original play fields in columns A-AF. All 17 non-key game fields appear
after them through completed `XLOOKUP` formulas; the existing `game_id` field
is the shared key. Use the two individual CSV files for Tableau and R.

This is the in-class lab dataset, not a lab-preparation example.

### What you will do

- Identify the grain and key of each table and explain their many-to-one
  relationship.
- Inspect how `XLOOKUP` joins game information to each play, and verify several
  results against the `Games` worksheet.
- Convert the `Plays` and `Games` ranges to Excel Tables.
- Create a `Counts` PivotTable with a count and percent-of-total frequency
  table.
- Compare counts of populated fields with summaries of a binary indicator to
  see how missing values and `FALSE` values affect the result.
- Create a `Means` PivotTable and compare average yards gained by play type and
  quarter.
- Use Excel's Data Model and a measure to calculate a median.
- Create a `Cross Tab` PivotTable and compare percentages of the grand total,
  column total, and row total.
- Reproduce the join and selected summaries in Tableau when available and in
  R with `left_join()`, `filter()`, `count()`, `group_by()`, and `summarise()`.
- Reconcile any differences across the tools before deciding which result is
  correct.

### Analysis reminders

- Keep each PivotTable on its own worksheet. A refresh or layout change can
  expand a PivotTable and overwrite nearby work.
- Count a populated field such as `game_id` when you need all play rows.
  Counting a field with blanks counts only its populated rows.
- Rows and Columns treat numeric fields as discrete categories. A field with
  many distinct values, such as `play_id` or `yardline_100`, usually produces
  an unwieldy table.
- State the denominator before interpreting a percentage.
- Joining game fields to plays repeats each game-level value once per play. Do
  not sum scores, total points, or similar game-level values from the joined
  play table.

### Completion check

Keep the Excel workbook and rendered Quarto document available for class. Only
upload them if the accompanying Canvas assignment requests a submission.
