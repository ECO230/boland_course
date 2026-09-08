# ECO 230 Canvas manifests

This directory is the tracked source for future Canvas publication. It is a
validated draft and is intentionally blocked from publishing.

- `course.yml` defines stable course, assignment, rubric, and module keys.
- `files.yml` defines repository-owned files uploaded into each Canvas course.
- `assessment-workflows.yml` records upload, manual-grade, calculated-grade,
  external-service, and derived-artifact behavior.
- `schedule.yml` maps assignments to the section-aware `C1`-`C14` schedule and
  declares intentionally undated gradebook shells.
- `rubrics/` contains rubric content without Canvas-specific identifiers.
- `RUBRIC-AUDIT.md` records the reference and live assignment-association
  matrix so rubric definitions are not confused with attached grading rubrics.
- `migration/` stores identifiers and dates from the reference course export.
- `review/` contains explicit decisions that must be resolved before publishing.
- `review/legacy-attachment-source-audit.md` documents source, licensing, and
  replacement rationale for the legacy Canvas files.
- `../content/` contains repository-owned Markdown for Canvas-only pages,
  assignments, and discussions. Private semester values are injected at
  publication and are never stored in Git.
- `validation.json` records structural checks and publication blockers.
- `posit-products.yml` maps each Posit Cloud product to its source manifest,
  generated repository, and private semester URL key.

Weekly reading and preparation links are consolidated into one Canvas-native
page per week. Repository-owned readings may be synchronized as native Canvas
Pages while using `placement_action: omit_duplicate`, so they can be linked from
the consolidated weekly page without becoming separate module items. Legacy
licensed readings remain explicit `omit_duplicate` records so the source audit
and selective copy remain reproducible.

Static QMD sources that should appear inside Canvas use
`source.kind: canvas_quarto`. Before planning, render the QMD so its matching
`_site/...html` file exists and is newer than the source. The publisher extracts
the Quarto document body, removes the duplicate title block and executable
scripts/styles, and rewrites relative resources against the unlisted website
copy. Missing or stale rendered HTML blocks the plan. Do not use this mode for
Reveal slides or interactive applications.

The `How should I study for the Practicum?` page is the verified Section 4
pilot for this workflow. Its one-page selective apply completed without
deletions or course publication and passed instructor visual review on
September 7, 2026.

Guided notes are distributed as editable Word documents stored under
`shared/guided-notes/`. Guided Notes 1 remains a 5-point pass/fail file-upload
assignment. The other guided notes retain their non-graded Canvas wrappers with
no submission and must not create gradebook entries.

Fall 2026 Section 4 has completed the guided-notes repair: all eight Word files
and wrapper links were verified, the wrapper assignments were republished, and
the course remained unpublished. Future sections should use the same file and
selective-assignment workflow rather than copying section-specific documents.

The `decision` column in `review/source-mapping.csv` records approved behavior.
Deferred items are resolved decisions but remain publication blockers until their
source material is ready.

Items with `management: canvas_assessment` and `publish_action: preserve` are
intentional Canvas-native assessments. The publisher must match and retain the
existing assessment without replacing its questions. A missing assessment is a
preflight error; preservation does not authorize reconstructing protected exam
or quiz content.

Items with `management: deferred` are not publishable until their authoritative
source has been exported and reconciled.

The practice-practicum discussion and final-practicum link are now backed by
repository manifests. The instructor-solutions page remains the only deferred
practicum-native item; it should not be reconstructed from protected grading
state.

Assignment dates are compiled from `syllabus/data/section_class_dates.csv`.
Assignments appearing in one weekly module inherit that module's course code;
multiweek and module-free exceptions are explicit in `schedule.yml`. Every dry
run records both the regular and alternate timestamp, while the section config's
`due_date_set` selects which timestamp will eventually become Canvas `due_at`.

`assessment-workflows.yml` is the authoritative behavioral overlay when the
baseline `course.yml` still says `pending_review`. A future publisher must apply
the workflow and review decisions before constructing Canvas API requests.

Static slide PDFs are generated from Reveal HTML with
`../scripts/export-slide-pdfs.ps1`. Generated PDFs and their checksum manifest
are written under the ignored `canvas/work/slide-pdfs/` directory. A missing
derived slide PDF is a preflight warning and may be omitted from a publication
run; it does not block the repository-managed Reveal deck. Week 13 intentionally
preserves the existing PowerPoint until its QMD source becomes authoritative.

Do not place Canvas tokens, student submissions, grades, comments, API receipts,
or `.imscc` exports in this directory.
