# Canvas-native content sources

This directory contains repository-owned Markdown used to create Canvas pages,
assignment bodies, and discussions that do not belong on the public course
website.

Files may contain `{{ value_name }}` placeholders. The Canvas publisher must
resolve every placeholder before creating or updating content. Values such as
Zoom meeting URLs and Posit Cloud space invitation URLs must come from the
publisher's private semester configuration or environment; they must never be
committed to this repository.

Public instructional content remains in the weekly or syllabus QMD files.
Static QMD documents are rendered into native Canvas page bodies; the rendered
website copies remain available as unlisted fallbacks. Reveal slides and
interactive applications continue to use their website URLs.

Each week with assigned readings uses one repository-owned Canvas page named
`weekNN-reading-prep.md`. The page groups required, optional, and video
resources so the module does not scatter preparation across many items.
Licensed excerpts remain private Canvas course files. Their links use derived
placeholders such as `{{ canvas_file_example_pdf_url }}`; the publisher builds
those values from the selected destination course's file inventory, so they do
not belong in the semester configuration.

Week 1 keeps the before-class `LabPrep for Lab 1` page separate from the
in-class assignment. The former `Technology Set-Up` assignment and standalone
Posit Cloud certificate link are represented by one assignment named `Lab 1:
Technology Setup`. That assignment links the Posit Cloud project, accepts the
certificate PDF, and asks students to report checklist results or setup
problems in Canvas.

Shared lab downloads are also published once with the public course website,
not copied into each Canvas course. Their source paths, stable public URLs, and
template keys are declared in `canvas/manifests/shared-resources.yml`. Quarto's
`project.resources` list in `_quarto.yml` ensures that each file is copied into
the rendered site. The four shared URL values belong in the publisher's common
`template_values` block so every section resolves the same resource.

## Required template values

- `instructor_email`
- `office_hours`
- `student_hours_url`
- `posit_cloud_join_url`
- `lab_02_download_url`
- `lab_02_posit_cloud_url`
- `lab_03_excel_training_url`
- `lab_03_data_url`
- `practice_r_coding_url`
- `inference_examples_url`
- `sample_survey_url`
- `final_exam_schedule`
- `final_practicum_url`
- `lab_01_data_url`
- `lab_01_posit_cloud_url`
- `practicum_practice_url`

The publisher should derive `final_exam_schedule` from
`syllabus/data/sections.csv` for the selected section rather than maintaining a
second set of semester dates.

The website build runs `scripts/render_section_syllabi.R` after the normal
Quarto render. It creates `syllabus_section_4.html`,
`syllabus_section_11.html`, and `syllabus_section_12.html` at the site root.
The public Syllabus menu links all three section copies. Each Canvas course
uses its `class_section` value to extract the matching rendered syllabus into a
native Canvas Page.

The targeted Week 1 refresh prepares and publishes the underlying Course Info
and Week 1 content objects, but leaves all modules and the course unpublished.
The instructor reviews Canvas and publishes the selected modules and course
manually. The refresh refuses to modify an already published course.

Canvas-only video pages may declare `kaltura_partner_id` and a `videos` list in
their YAML front matter. The publisher renders each `entry_id` using the current
Canvas-compatible Kaltura embed component; generated iframe URLs are not stored
as canonical content.

Publication must fail if a required value is missing or still contains a
literal `{{ ... }}` placeholder.
