# Week 12/13 Sampling Survey

This folder contains the working materials for the Week 12/13 class survey used in the sampling and survey-design lab.

## Recommended Path

The old TSV imports were useful for exploration, but they produced messy LimeSurvey output. The recommended path now is:

1. Create a fresh survey in LimeSurvey.
2. Build it manually from `sampling_survey_live_build.md`.
3. Use `sampling_survey_v2_spec.md` as the fuller design reference.
4. Export the clean survey once it looks right.
5. Treat that export as the new baseline artifact.

## Files

- `sampling_survey_live_build.md` - the fastest clean manual build guide for LimeSurvey.
- `sampling_survey_v2_spec.md` - fuller survey blueprint with rationale and optional items.
- `limesurvey_cleanup_notes.md` - triage notes if you keep cleaning the current imported survey.
- `sampling_survey_lab_limesurvey_import.txt` - legacy draft import.
- `sampling_survey_lab_questions_only_import.txt` - legacy safer import attempt.
- `sampling_survey_lab_with_language_import.txt` - legacy import attempt with minimal language rows.

## Current Recommendation On Imports

Do not rely on the TSV imports as the canonical survey.

They can still be useful for reference, but the live student survey should be rebuilt cleanly in the LimeSurvey UI so that:

- question titles are human-readable
- internal variable names stay hidden
- answer lists look correct
- `Other` handling is intentional
- required questions do not show distracting `No answer` options

## Design Purpose

The survey is designed to feed a Shiny app that demonstrates:

- random vs representative samples
- sampling methods such as simple random, convenience, systematic, and stratified sampling
- sampling frame error
- nonresponse error
- survey question formats
- the value of open-text responses for niche populations

The private design source of truth is:

`.codex/private/week12_sampling_survey_design.md`
