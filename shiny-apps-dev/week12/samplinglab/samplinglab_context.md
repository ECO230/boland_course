# Sampling Lab App

## Overview

This app supports the Week 12 sampling and survey-design lesson.

Its first job is narrow and deliberate:

- compare sampling methods against a known response pool
- make random vs representative visible
- show how subgroup balance can shift even when the estimator looks close

The app is designed to work before full live LimeSurvey database integration is complete.

## Learning Objectives

- Distinguish the response pool from the true target population.
- Compare simple random, convenience, systematic, and stratified sampling.
- See that a sample estimate can look close while subgroup composition is still skewed.
- Connect sampling design to representativeness rather than treating it as luck.

## Data Source

The app currently supports:

1. built-in demo data generated inside `app.R`
2. live LimeSurvey/MariaDB loading through environment variables
3. uploaded CSV files

The live database path uses:

- `SURVEY_DB_HOST`
- `SURVEY_DB_PORT`
- `SURVEY_DB_NAME`
- `SURVEY_DB_USER`
- `SURVEY_DB_PASSWORD`

Version 1 targets survey response table:

- `survey_931648`

## UI Structure

The app uses one left control panel and four tabs:

1. Response pool
2. Sampling lab
3. Repeat sampling
4. Image heatmap

The control panel is collapsed into a `details` element and includes:

- data source mode
- outcome selector
- category selector for categorical outcomes
- sample size
- method selector
- order/strata controls when relevant
- subgroup comparison variable
- one-sample and many-sample actions

## Core Reactive Flow

1. Load demo data or uploaded CSV.
2. Clean column names and coerce mostly numeric character columns.
3. Detect numeric and categorical variables.
4. Define an estimator:
   - mean for numeric variables
   - proportion for a chosen category level
5. Draw one sample using the chosen method.
6. Compare the current sample estimate to the response-pool estimate.
7. Compare subgroup composition between the sample and the pool.
8. Optionally repeat the sampling many times to show estimator variation.
9. Visualize image-click coordinates from the survey on top of the Escher image.

## Sampling Methods In Version 1

- Simple random sample
- Convenience sample
- Systematic sample
- Stratified sample

These four methods are enough to anchor the lesson without making the interface feel crowded.

## Teaching Design

This app should be used with the explicit reminder that:

- the class response pool is observable
- the true class population may still differ

That lets the instructor separate:

- method comparison inside the observed data
- frame/nonresponse problems that happen before the app ever sees the data

## Future Extensions

- explicit frame-error simulator tab
- nonresponse mechanism simulator
- saved presets tied to the Week 12 survey schema
- survey-design examples tied to specific question formats
- a truer image-density heatmap or click-cluster summary if the current overlay becomes a recurring classroom feature
