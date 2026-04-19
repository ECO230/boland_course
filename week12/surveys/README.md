# Week 12/13 Sampling Survey

This folder contains a draft LimeSurvey import for the sampling and survey-design lab.

## Files

- `sampling_survey_lab_limesurvey_import.txt` - tab-separated LimeSurvey survey-structure draft.
- `sampling_survey_lab_questions_only_import.txt` - safer tab-separated import without survey metadata rows.
- `sampling_survey_lab_with_language_import.txt` - recommended import with minimal English language metadata.

## Import

In LimeSurvey:

1. Go to **Surveys**.
2. Choose **Import a survey**.
3. Upload `sampling_survey_lab_with_language_import.txt`.
4. Import it as a survey structure.
5. Preview the survey and check question logic before activating.

If the recommended file fails, try `sampling_survey_lab_questions_only_import.txt`, then set the survey language/title manually in the LimeSurvey admin UI.

LimeSurvey supports tab-separated `.txt` survey-structure imports. This format is easier to edit than raw `.lss` XML while drafting.

## Post-Import Checks

After import, review these items in the LimeSurvey admin UI:

- Constant sum: verify the multiple-numeric question requires the six values to sum to 100. If the TSV import does not preserve that validation, add it manually.
- Drilldown: the draft uses three dropdown questions instead of a true cascading drilldown. Convert to cascading logic if desired.
- Graphic rating: the draft uses a 5-point choice as the portable core version. Replace with a visual/slider question type if your LimeSurvey setup supports it.
- Heat map: the draft uses a long-text placeholder because heat map/image-click questions may require a plugin or a specific question module.
- Bad-question diagnosis: these are included as list/radio diagnosis items. They can be moved to an in-class activity if the live survey feels too long.

## Design Purpose

The survey is designed to feed a Shiny app that demonstrates:

- random vs representative samples
- simple random, systematic, convenience, stratified, quota, cluster, judgment, and snowball sampling
- sampling frame error
- nonresponse error
- survey question formats
- leading, loaded, double-barreled, and assumption-based questions

The private design source of truth is:

`.codex/private/week12_sampling_survey_design.md`
