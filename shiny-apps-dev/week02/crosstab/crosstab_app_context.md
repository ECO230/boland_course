# Cross-Tab Shiny App -- Context Document

## Overview

This Shiny app supports Week 2 instruction on frequency tables, cross-tabs, and percentage interpretation using the Wisconsin accidents dataset.

The app helps students compare:

- one-way frequency tables
- two-way contingency tables
- marginal totals
- table percentages
- row percentages
- column percentages

The main teaching goal is denominator clarity.

## Learning Objectives

- Build and read frequency tables for categorical variables.
- Interpret contingency tables as counts.
- Distinguish row percent, column percent, and table percent.
- Explain which direction sums to approximately 100%.
- Avoid confusing similar-sounding percentage statements.

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

The teaching-friendly categorical variables are:

- `Severity`
- `Wind_Direction`
- `Weather_Condition`
- `Sunrise_Sunset`
- `season`
- `time_of_day`

## UI Structure

The interface uses one parameter panel and four instructional tabs:

1. Frequency Tables (Row + Column)
2. Cross-tab (Counts)
3. Cross-tab (Percentages)
4. What Can Go Wrong?

Important controls include:

- date range filter
- row variable selector
- column variable selector
- frequency table sort order
- missing-as-category toggle
- top-N level collapsing
- margins/totals toggle
- percentage type selector
- percent rounding selector

## Core Reactive Flow

The app follows this chain:

1. Load and pre-process accident data once at startup.
2. Filter rows by date range.
3. Convert selected row and column variables into categorical strings.
4. Optionally treat missing values as a category.
5. Collapse high-cardinality variables to top-N plus `Other`.
6. Build one-way frequency tables for both selected variables.
7. Build two-way count tables.
8. Convert counts to percentage tables with meaningful margins.

Key functions:

- `as_cat()`
- `handle_missing_cat()`
- `collapse_top_n()`
- `make_freq_df()`
- `tab_to_df_counts()`
- `percent_df_with_margins()`

## Percentage Logic

The app intentionally computes margin cells from counts rather than summing displayed percentages.

### Table Percent

Each cell is a share of the grand total.

### Row Percent

Each row is a conditional distribution across columns. The total row is based on overall column shares.

### Column Percent

Each column is a conditional distribution across rows. The total column is based on overall row shares.

This design keeps the totals meaningful and supports classroom discussion about denominators.

## Teaching Design

The "What Can Go Wrong?" tab names the central misconception:

- row percent answers "given this row category..."
- column percent answers "given this column category..."
- table percent answers "out of all records..."

Students should leave the app able to state the denominator before interpreting any percentage.

## Modification Guidance

When modifying:

- Keep counts and percentages paired conceptually.
- Preserve the explanatory "What Can Go Wrong?" tab.
- Keep missing-value handling visible as a control.
- Be careful when changing margin logic; the current implementation avoids misleading summed percentages.
- If adding categorical variables, consider whether top-N collapsing is needed.

## Known Constraints

- The app is tied to the deployment path `/data/junior/boland_course`.
- It is built around the WI accidents dataset.
- Large categorical variables are simplified with top-N collapsing.
- The app uses HTML table rendering rather than `gt` or reactable.
