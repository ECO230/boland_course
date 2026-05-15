# LimeSurvey Cleanup Notes

Use these notes while cleaning or rebuilding the Week 12 survey in the LimeSurvey admin UI.

## Immediate Problems In The Imported Survey

- Internal codes are visible to students.
- Group titles are not polished.
- Some answer lists imported incorrectly.
- `Other` handling is messy.
- `No answer` is visible on questions that should probably be required.

## Quick UI Fixes

### 1. Group Titles

Rename groups to:

- `About You`
- `Learning and Tools`
- `Open Response`

### 2. Required Questions

Make these required:

- class standing
- academic area
- attendance
- commute time
- work hours
- sleep last night
- primary transport
- confidence items
- numeric anchored scale

### 3. Remove Broken `Other` Fields

If LimeSurvey created both:

- an `Other / not listed` answer
- and a separate `Other:` text field

keep only one intentional `Other` mechanism.

### 4. Rebuild Broken Answer Lists Manually

If only the last answer imported correctly, manually rebuild the answer list in the UI rather than trying to patch the imported question.

### 5. Hide Demo Artifacts

Move these out of the live survey unless they are working cleanly:

- bad question diagnosis items
- heat map placeholders
- graphic rating placeholders

## Best Rebuild Strategy

Use the file:

- `sampling_survey_v2_spec.md`

as the blueprint, and manually rebuild the survey from scratch in LimeSurvey.

Once the manual version looks right:

1. export the survey
2. save the export
3. treat that export as the new baseline
