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
- [Lab 3 Video Guide: Tableau and Excel](/courses/{{ canvas_course_id }}/pages/lab-3-video-guide-tableau-and-excel)
- [Optional Lab 3 R companion in Posit Cloud](https://posit.cloud/spaces/3173/content/12970507)
- [R video walkthrough and transcript](/courses/{{ canvas_course_id }}/pages/lab-3-video-guide-tableau-and-excel#r-companion)

The optional R companion shows how to create similar charts with ggplot2.
Open `Lab_03_R_Companion_IP.qmd` and choose **Render** to see the code and
charts together as an HTML guide. The video explains how the summary tables
feed the plots and how to run individual code chunks. You are not expected
to write this code from scratch; use it to explore how the same analysis
works in R. The Tableau and Excel lab requirements below still apply.

Follow the video guide as we build the workbook together in class. Start by
connecting Tableau to the crash CSV and verifying the unfiltered count of
12,000 records. Create a separate worksheet for each view below.

The extract is a deterministic sample for learning visualization techniques.
Its row counts are not official totals for Chicago crashes.

### Tasks

Create the following views in Tableau. Reproduce at least 3 in Excel.

1. **Ranking:** Rank the 10 most common `primary_contributory_cause` values by
   crash count. Exclude missing causes, UNABLE TO DETERMINE, and NOT APPLICABLE,
   sort descending, and keep the ten largest remaining causes. Keep this
   selection local to the ranking sheet.
2. **Nominal comparison:** Duplicate the ranking sheet, remove its cause
   selection, and compare average `injuries_total` across causes. Put crash
   count in the tooltip, filter to at least 30 crashes per cause, and exclude
   NOT APPLICABLE. Explain why averages based on small groups need caution.
3. **Time series:** Plot crash count by continuous month from `crash_datetime`,
   keeping month-year combinations in chronological order. Color by
   `reported_weather_condition`; examine the view without CLEAR and optionally
   UNKNOWN. Document which conditions you exclude.
4. **Part-to-whole:** Show the percentage of crashes in each
   `most_severe_injury` category within each discrete quarter of
   `crash_datetime` (Q1-Q4 across years). Exclude null injury categories and
   verify that the complete categories total 100% within every quarter.
   Convert the text table to stacked bars, then practice hiding NO INDICATION
   OF INJURY and REPORTED, NOT EVIDENT while retaining their contribution to
   the denominator. Explain why the visible portions no longer total 100%
   and how using Exclude instead would change the calculation.
5. **Relationship:** Create a scatterplot of `temperature_f` and `dew_point_f`.
   Turn off Aggregate Measures so each mark represents a crash with weather
   values. Make separate panels by `weather_station_name`, exclude null station
   names, and explain that crashes can share a weather observation.
6. **Distribution:** Explore `temperature_f` with a histogram and experiment
   with bin widths. Then compare temperature across
   `reported_weather_condition` categories using box plots of individual
   observations. Compare medians, spread, and unusual values.
7. **Map:** Plot `longitude` on Columns and `latitude` on Rows, with `crash_id`
   on Detail and `trafficway_type` on Color. Check that each mark represents a
   crash. Optionally explore a geographic subset or a ZIP-code filled map;
   explain any filtering or change in what one mark represents.

Then open the same CSV in Excel and save it as `.xlsx`. Follow the guide's
PivotChart examples to reproduce at least three views. Match the Tableau
filters and aggregations. Use a regular XY Scatter chart from the raw paired
measurements if you choose the relationship view.

For every view, use a question-based title, display units where needed, verify
the aggregation, remove unnecessary decoration, and use color only when it
communicates information.

### Completion check

Save and reopen the Tableau Packaged Workbook (`.twbx`) and Excel workbook
(`.xlsx`). Check your seven Tableau views and at least three Excel views
against the video guide's final checklist, including filters, aggregations,
and percentage denominators. Keep both files available for class.
Only upload them if the accompanying Canvas assignment requests a submission.
