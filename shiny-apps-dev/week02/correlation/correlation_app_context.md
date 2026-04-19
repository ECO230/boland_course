# Correlation Shiny App -- Context Document

## Overview

This Shiny app supports Week 2 instruction on continuous-to-continuous relationships using the Wisconsin accidents dataset and synthetic examples.

The app teaches that correlation measures linear association, not slope, steepness, or every kind of relationship.

It combines:

- real accident data scatterplots
- Pearson and Spearman correlation
- linear model slope/intercept for teaching
- optional log transforms
- canned nonlinear examples
- linear and loess overlays

## Learning Objectives

- Interpret correlation as strength and direction of linear association.
- Distinguish correlation from slope.
- Recognize nonlinear relationships where Pearson correlation may be near zero.
- Compare Pearson correlation and Spearman rank correlation.
- Use scatterplots before trusting a correlation coefficient.

## Data Source

The app loads:

```r
/data/junior/boland_course/shared/data/accident_wi.csv
```

It parses:

- `Start_Time`
- `End_Time`
- `Duration_min`

Candidate numeric variables include:

- `Distance(mi)`
- `Temperature(F)`
- `Humidity(%)`
- `Wind_Speed(mph)`
- `Visibility(mi)`
- `Pressure(in)`
- `Precipitation(in)`
- `Duration_min`

## UI Structure

The interface uses one parameter panel and two output tabs:

1. Scatter + Correlation
2. Examples Gallery

The parameter panel has two sub-tabs:

- Your Data
- Canned Examples

Important controls for real data:

- x variable
- y variable
- severity filter
- date range
- drop missing/invalid toggle
- log10 transform X
- log10 transform Y
- linear fit toggle
- loess smoother toggle

Important controls for examples:

- example dataset type
- number of points
- target Pearson r for linear examples
- y scaling
- sign flip
- noise
- random seed
- linear fit toggle
- loess smoother toggle

## Canned Examples

The app includes synthetic examples designed to teach limitations of correlation:

- Linear (set r)
- U-shape (r near 0, nonlinear)
- Sine wave (r near 0, nonlinear)
- Circle (r near 0, nonlinear)
- X-shape mixture (r near 0)

These examples make it clear that a small Pearson correlation does not always mean "no relationship."

## Core Reactive Flow

The app follows this chain:

1. Load accident data once at startup.
2. Filter real data by severity and date range.
3. Select two numeric variables.
4. Keep finite paired observations.
5. Optionally apply log10 transforms.
6. Compute Pearson and Spearman summaries.
7. Render scatterplots with optional linear and loess fits.
8. Generate synthetic datasets for the examples gallery.

Key functions:

- `safe_num()`
- `fmt_num()`
- `cor_summary()`
- `make_example()`
- `filtered()`
- `pair_df()`
- `example_obj()`

## Teaching Design

The app makes a repeated distinction:

- Correlation measures linear association.
- Slope depends on units and scale.
- Nonlinear structure can exist even when r is close to zero.
- A scatterplot should be inspected before interpreting r.

The examples gallery is especially useful for challenging the assumption that a single number can summarize every relationship.

## Modification Guidance

When modifying:

- Keep the real-data and canned-example modes separate.
- Preserve the linear-vs-nonlinear teaching examples.
- Keep Pearson and Spearman visible together when possible.
- Keep the slope/intercept note clearly labeled as teaching context.
- Be cautious with log transforms; only positive values survive the transform.

## Known Constraints

- The app is tied to the deployment path `/data/junior/boland_course`.
- It is built around the WI accidents dataset.
- The canned examples are synthetic and intentionally stylized.
- The app does not attempt multivariable regression or causal interpretation.
