# Sampling Distribution Simulator --- Design & Implementation Summary

## Purpose

This Shiny application demonstrates:

-   What a **sampling distribution** is
-   How repeated sampling builds that distribution
-   How **confidence intervals** behave across repeated samples
-   Why approximately (1 − α)% of confidence intervals contain the true population mean

The app is designed for instructional use in statistics and data analysis courses.

------------------------------------------------------------------------

# Conceptual Model

We assume:# Sampling Distribution Simulator -- Context Summary

## Overview

This Shiny app is a teaching-focused sampling distribution simulator
designed to:

-   Demonstrate how sample means build a sampling distribution
-   Visualize confidence intervals across repeated samples
-   Show how standard error and t-based confidence intervals are
    calculated
-   Separate symbolic formulas from numeric plug-in steps and final
    results

The app is optimized for clarity and classroom pacing.

------------------------------------------------------------------------

## Current Behavioral Design

### 1. Add 1 Sample

-   Draws one bootstrap sample (with replacement) from the selected
    population
-   Updates:
    -   Current sample values
    -   Point estimate plot
    -   Confidence interval whisker
    -   Sampling distribution histogram
    -   CI plot
    -   Formula panel

This is fully reactive and updates immediately.

------------------------------------------------------------------------

### 2. Run 100 Samples

-   Clears existing samples
-   Runs 100 samples quickly in the background
-   Uses `withProgress()` for a progress bar
-   Does **not animate**
-   Redraws once after all 100 samples are complete

This avoids UI lag and rendering freezes.

------------------------------------------------------------------------

## Default Dataset

The default population is:

    ChickWeight (weight)

This is set explicitly in the UI using:

    selected = "chickweight"

------------------------------------------------------------------------

## Formula Rendering (No MathJax)

MathJax has been completely removed.

All formulas are rendered using **base R plotmath** inside
`renderPlot()`.

### Why plotmath?

-   Avoids LaTeX parsing issues in Shiny
-   Prevents dynamic typesetting failures
-   Works reliably in server environments
-   No external JavaScript dependency

------------------------------------------------------------------------

## Formula Panel Layout (3 Columns)

The formula panel is now structured into three visual columns to prevent
crowding:

### Left Column -- Generic Formulas

-   SE = s / sqrt(n)
-   x̄ ± t\* · SE

### Middle Column -- Plug In Numbers

-   SE = sd / sqrt(n)
-   t\* = t\_{prob, df}
-   CI = x̄ ± t\* · SE (numeric substitution)

### Right Column -- Final Results

-   SE = computed numeric value
-   t\* = numeric critical value
-   CI = \[lower, upper\]

This separation improves readability and instructional clarity.

------------------------------------------------------------------------

## Key Reactive Objects

The app uses a central `reactiveValues()` object:

    rv$samples
    rv$current_sample
    rv$current_ci
    rv$sample_id

Each sample stores: - mean - sd - se - CI bounds - whether CI contains
population mean

------------------------------------------------------------------------

## Sampling Distribution Logic

-   Histogram bins are fixed per dataset via `POP_CONFIG`

-   Theoretical curve uses:

    sd_mean = sigma / sqrt(n)

-   Histogram auto-scales vertically

-   Theoretical curve scales to 90% of the plot height

------------------------------------------------------------------------

## CI Plot Behavior

-   Displays up to the most recent 100 intervals
-   Blue = interval contains true mean
-   Red = interval misses true mean
-   Vertical line = population mean

------------------------------------------------------------------------

## Architectural Principles

1.  Separate computation from rendering
2.  Avoid chained `==` expressions in plotmath
3.  Avoid dynamic MathJax
4.  Prevent animation-induced UI blocking
5.  Keep numeric results visually distinct from symbolic formulas

------------------------------------------------------------------------

## If Modifying This App

When updating:

-   Do not reintroduce MathJax
-   Keep Run 100 non-animated unless rearchitected with async tools
-   Maintain 3-column formula separation
-   Avoid chained equals inside `bquote()`
-   Preserve ChickWeight as default unless pedagogically necessary to
    change

------------------------------------------------------------------------

This file is intended to provide architectural context for future
LLM-assisted modifications.


-   A **known finite population dataset**
-   The true population mean (μ) and population standard deviation (σ) are known
-   We repeatedly draw samples of size *n* with replacement
-   For each sample we compute:
    -   Sample mean (x̄)
    -   Sample standard deviation (s)
    -   Standard error (SE = s / √n)
    -   Confidence interval using a t critical value

As samples accumulate:

-   Sample means populate a histogram
-   The histogram approximates the theoretical sampling distribution
-   Confidence intervals are drawn and evaluated against μ
-   CI "errors" (intervals not containing μ) are counted

------------------------------------------------------------------------

# Architecture Overview

The app is a single-file `app.R` Shiny application structured as:

-   **UI (navbarPage)**
    -   Simulator tab
    -   Population Data tab
-   **Server**
    -   Reactive population selection
    -   Sampling engine
    -   Sampling distribution renderer
    -   CI renderer
    -   MathJax formula renderer
    -   Animation controller

------------------------------------------------------------------------

# Populations

Three full datasets are embedded:

-   Iris (Sepal Length)
-   Airquality (Temperature)
-   ChickWeight (Weight)

Each population:

-   Contains \>50 observations
-   Is fully known (not simulated)
-   Allows computing true μ and σ

------------------------------------------------------------------------

# Core Sampling Engine

Each sample:

1.  Draw n values with replacement from full population
2.  Compute statistics
3.  Store results in `reactiveValues$samples`
4.  Append:
    -   mean
    -   sd
    -   se
    -   CI bounds
    -   contains_mu flag

Sampling results accumulate in memory and drive all plots.

------------------------------------------------------------------------

# Sampling Distribution Plot

The histogram:

-   Uses fixed bin width (configurable per dataset)
-   X-axis locked per population
-   Y-axis auto-scales as counts increase
-   Theoretical normal curve is scaled to fixed relative height

The curve represents:

```         
Normal( μ , σ / √n )
```

It visually demonstrates convergence as sample count increases.

------------------------------------------------------------------------

# Confidence Interval Plot

For each sample:

-   A horizontal line is drawn from CI low to CI high
-   The sample mean is plotted as a point
-   Color logic:
    -   Blue = interval contains μ
    -   Muted red = interval misses μ

The vertical red line represents μ.

The number of CI failures is tracked and displayed.

------------------------------------------------------------------------

# Current Sample Visualization

The simulator also displays:

-   The current sample values (comma-separated)
-   The sample mean plotted alone
-   The confidence interval for the current sample
-   Summary stats table
-   Mathematical formulas

------------------------------------------------------------------------

# Formula Rendering

MathJax is used for LaTeX-style rendering:

Standard Error:

```         
SE = s / √n
```

Confidence Interval:

```         
x̄ ± t* · SE
```

Right-hand side dynamically substitutes actual computed values.

MathJax re-typesetting is triggered after reactive updates.

------------------------------------------------------------------------

# Animation Model

Two modes:

-   Add 1 Sample (manual step)
-   Run 100 Samples (animated)

The animated version uses asynchronous scheduling (via later package) to avoid UI blocking and allow incremental rendering.

Each sample is drawn with a short delay so the sampling distribution visibly builds over time.

------------------------------------------------------------------------

# Configuration Hooks

Per-dataset configuration supports:

-   Fixed bin width
-   Locked x-axis range

These allow consistent visuals across repeated runs.

------------------------------------------------------------------------

# Key Reactive Objects

`rv$samples`\
Stores all historical sampling results.

`rv$current_sample`\
Stores most recent drawn sample.

`rv$current_ci`\
Stores most recent CI computation.

------------------------------------------------------------------------

# Extension Ideas

-   Allow toggling between t and z intervals
-   Show empirical CI coverage percentage
-   Animate theoretical curve convergence
-   Add skewed population option
-   Show effect of increasing n on CI width dynamically

------------------------------------------------------------------------

# Summary

This application is a teaching tool that:

-   Bridges theory and visualization
-   Demonstrates sampling variability
-   Illustrates confidence interval coverage
-   Makes abstract statistical concepts concrete through animation

Designed as a clean, modular, single-file Shiny app for portability and modification.
