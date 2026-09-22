# Homework 3 and Week 3 lab order

The authoritative prompts are `week03/Homework_03.qmd` (website/Canvas) and
`week03/Homework_03_assignment.qmd` (standalone Typst template). The Posit
release uses `week03/homework_03.manifest.json` and
`scripts/build_assignment_repo.py`, then a reviewed update to the existing
`ECO230/homework-03-ip` main branch. Do not substitute direct Posit project
file edits for this repository workflow. Existing student copies are independent.

The September 2026 update keeps the three analytical-frame groups and adds a
required 4-6 sentence AI reflection, including an equivalent non-use path.
All three visuals must be embedded in the submitted Word/PDF with their
explanations, summary, and reflection. Posit Cloud is optional and appears
last in Resources. The 100-point rubric allocates 5 points to AI reflection
and 5 to complete files; the other criterion weights remain unchanged.

Render the website source, then run `../eco230-canvas-ops/canvas/scripts/refresh-homework3.ps1`
for a read-only preflight; append `-Execute` to apply. The wrapper validates
native callouts, updates the existing unpublished assignment and its rubric,
and orders the six existing lab items as LabPrep, Lab 3, Hints, Video Guide,
Completed Workbook, and Opening a Tableau File. It checks all three sections,
existing criterion identities, rubric association, assignment-group membership,
due dates, publication states, and unrelated module item order.

Canvas requires explicit criterion/rating `id` fields in the rubric payload;
using IDs only as the parameter hash keys can regenerate them. The wrapper
and regression test now include those explicit fields. A saved baseline in
`canvas/work/homework3-refresh/section-N-before.json` can restore the six
original criterion IDs after an interrupted run.

Verification: four wrapper tests pass; the standalone template renders to a
four-page PDF; Canvas native callouts were visually checked in section 11;
all three sections passed live order, rubric, and publication checks.
Receipts are under ignored `canvas/work/homework3-refresh/`.
The assignment repository release is `c53e2d8`; website deployment is `020be18`.
