# Descriptives Shiny App -- Context Document

## Overview

This Shiny app supports Week 2 instruction on descriptive measures using the Wisconsin accidents dataset.

The app lets students filter a shared accident dataset and explore:

- measures of size
- central tendency
- spread
- percentiles and IQR
- box-and-whisker plots
- row attrition from filtering, missingness, and trimming

The main teaching goal is to make "N vs usable n" visible before students interpret summary statistics.

## Learning Objectives

- Distinguish total rows, filtered rows, missing or invalid values, trimmed values, and usable observations.
- Compare mean, median, and rounded mode.
- See how trimming extremes changes center and spread.
- Interpret standard deviation, percentiles, IQR, and boxplots as complementary descriptions of shape.
- Connect numeric summaries to the filtered population being described.

## Data Source

The app loads:

```r
/data/junior/boland_course/shared/data/accident_wi.csv
```

It derives:

- `start_time`
- `end_time`
- `season`
- `time_of_day`
- `duration_mins`

The app keeps a limited teaching-friendly set of numeric and categorical fields.

## UI Structure

The interface uses one parameter panel and four instructional tabs:

1. Measures of Size
2. Central Tendency
3. Spread
4. Box & Whisker (Tie Together)

The parameter panel is collapsed by default using an HTML `details` element.

Important controls include:

- numeric variable selector
- categorical filter field and included levels
- date range filter
- drop missing/invalid toggle
- mean/median line toggle
- standard deviation whisker toggle
- percentile line input
- IQR shading toggle
- tail-trimming slider
- rounded mode precision
- boxplot dot toggle

## Core Reactive Flow

The app follows this chain:

1. Load and pre-process accident data once at startup.
2. Build dynamic categorical filter UI from the selected field.
3. Filter rows by date and optional category levels.
4. Convert the selected numeric variable to finite numeric values.
5. Track missing/invalid values.
6. Optionally trim extremes from each tail.
7. Render text summaries and plots from the same filtered and trimmed data.

Key reactive objects:

- `filtered()`
- `x_info()`
- `trim_flags()`
- `x_trimmed()`
- `quartiles()`
- `pct_values()`
- `dot_long()`

## Visualization Strategy

### Measures of Size

Uses a faceted dot grid to show attrition:

- all rows
- after filters
- missing/invalid
- trimmed
- usable n

This is meant to slow students down before they interpret statistics.

### Central Tendency

Uses a point histogram with stacked and jittered dots. Mean and median can be overlaid.

### Spread

Uses a histogram with optional:

- mean and median markers
- custom percentile lines
- IQR shading
- standard deviation whisker

### Boxplot

Draws a manual boxplot so trimming, outliers, mean markers, and optional dots behave consistently.

## Teaching Design

This app is designed to support the Week 2 descriptive measures sequence:

size -> center -> spread -> shape

It emphasizes that every summary statistic is conditional on:

- the dataset
- filters
- valid values
- trimming choices
- the variable's measurement scale

## Modification Guidance

When modifying:

- Keep the startup `renv::load()` block at the top for deployment.
- Keep all tabs driven by the same filtered data.
- Preserve row attrition as a first-class teaching object.
- Avoid adding charts that obscure the difference between total rows and usable n.
- If adding variables, update `num_vars` or the categorical filter choices deliberately.

## Known Constraints

- The app is tied to the deployment path `/data/junior/boland_course`.
- It is built around the WI accidents dataset.
- It uses base Shiny and ggplot2 rather than a broader component framework.
- Missing/invalid values are handled for teaching clarity rather than advanced data validation.
