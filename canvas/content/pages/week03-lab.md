---
schema_version: 1
key: lab-3-creating-basic-visualizations-in-excel-and-tableau
canvas_type: page
visibility: course_only
required_values:
  - lab_03_data_url
---

## In-Class Lab 3: Creating Basic Visualizations in Excel and Tableau

### Purpose

Use a 12,000-row classroom extract of the Chicago traffic crash analysis table to
create and verify common business visualizations. The extract contains one row
per crash and includes reported crash conditions, derived calendar fields,
injury counts, coordinates, and selected numerical weather measurements.

### Resources

- [Download the Lab 3 crash extract]({{ lab_03_data_url }})
- Open the Lab 3 Tableau packaged workbook (`.twbx`) supplied in this Canvas
  module.
- [Lab 3 Hints](https://eco230.github.io/boland_course/week03/labs/lab-3-hints.html)

The extract is a deterministic sample for learning visualization techniques.
Its row counts are not official totals for Chicago crashes.

### Tasks

Create the following views in Tableau. Reproduce at least 3 in Excel.

1. **Ranking:** Rank the 10 most common `primary_contributory_cause` values by
   crash count. Keep missing and not-applicable values out of the ranking.
2. **Nominal comparison:** Compare average `injuries_total` across the selected
   causes. Explain why this average should be interpreted cautiously.
3. **Time series:** Plot monthly crash counts in chronological order. Decide
   whether month, year-month, or a filtered date range best supports the
   comparison.
4. **Part-to-whole:** Show the percentage of crashes in each
   `most_severe_injury` category within each `time_period`. Verify that the
   percentages total 100% within every time period.
5. **Relationship:** Create a scatterplot of `temperature_f` and `dew_point_f`.
   Use `weather_station_name` as a grouping or filter and describe what one mark
   represents.
6. **Distribution:** Compare the distribution of `visibility_miles` across
   selected `reported_weather_condition` categories using box plots or
   histograms.
7. **Map:** Plot crashes using `longitude` and `latitude`. Filter the view to a
   manageable subset and explain what the filter removes.

For every view, use a question-based title, display units where needed, verify
the aggregation, remove unnecessary decoration, and use color only when it
communicates information.

### Completion check

Keep the Excel workbook and Tableau Packaged Workbook available for class.
Only upload them if the accompanying Canvas assignment requests a submission.
