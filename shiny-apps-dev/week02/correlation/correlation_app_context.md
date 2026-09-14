# Correlation Shiny App -- Context Document

## Overview

This Shiny app supports Week 2 instruction on continuous-to-continuous relationships using a reproducible sample of Chicago traffic crashes and synthetic examples.

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

The app loads the repository source:

```r
/data/junior/boland_course/shared/data/early-homework-v1/traffic_chicago_2024.csv
```

Shared preparation is defined in:

```r
/data/junior/boland_course/week02/scripts/accident_clean.R
```

The loader uses seed `230` to sample exactly 7,900 rows, retains missing values, parses `crash_datetime`, and derives the shared teaching fields.

Candidate numeric variables include:

- `temperature_f`
- `relative_humidity_percent`
- `wind_speed_mph`
- `visibility_miles`
- `precipitation_inches`
- `posted_speed_limit_mph`
- `unit_count`
- `injuries_total`

The default scatterplot compares `temperature_f` with `relative_humidity_percent`. In the current seeded sample, their paired Pearson correlation is approximately -0.35, providing a visible but imperfect negative association.

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

- The deployed app defaults to `/data/junior/boland_course`; set `ECO230_COURSE_ROOT` for local validation.
- It is built around the tracked Chicago 2024 traffic-crash extract and the shared seeded loader.
- The canned examples are synthetic and intentionally stylized.
- The app does not attempt multivariable regression or causal interpretation.
