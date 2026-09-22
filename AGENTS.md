# AGENTS.md

Project guidance for Codex working in this repository.

## Project Overview

This repository is a Quarto website for ECO 230 course materials. It contains:

- Weekly Quarto/Reveal.js slide decks in `week01/`, `week02/`, etc.
- Homework, interpretation, and assignment prompt documents as `.qmd` files.
- A section-parameterized syllabus in `syllabus/syllabus.qmd`.
- Shared styles, images, scripts, and datasets under `shared/`.
- Shiny teaching apps under `shiny-apps/` and development app work under `shiny-apps-dev/`.
- Admin/final practicum materials under `admin/final/`.

The site config is `_quarto.yml`. Rendered output goes to `_site/`.

## Important Structure

- `_quarto.yml`: website navigation, global HTML format, and execute defaults.
- `syllabus/syllabus.qmd`: main syllabus. Uses `params$class_section` and section-specific CSVs.
- `syllabus/data/sections.csv`: section metadata such as semester, section, meeting time, office hours, and final exam details.
- `syllabus/data/section_*.csv`: section-filtered versions of schedule, dates, class dates, and assignment percentages. These include a `section` column that should be filtered out of rendered tables.
- `weekXX/week_XX.qmd`: weekly Reveal.js decks.
- `weekXX/media/images/`: slide images local to each week.
- `shared/styles/boland-reveal.scss`: shared Reveal.js slide styling.
- `shared/images/xkcd_*.png`: reusable xkcd-style character assets.
- `admin/final/*.qmd`: practicum prompts/guidelines.
- `canvas/`: repository-managed Canvas content, manifests, guarded PowerShell
  workflows, and validation documentation.
- `renv.lock` and `renv/`: R dependency management.

## Commands

Quarto is not necessarily on PATH in this Windows setup. Use the RStudio-bundled binary:

```powershell
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render syllabus\syllabus.qmd -P class_section:12 --output syllabus_section_12.html --cache-refresh
```

Render a specific document rather than the whole website when local paths or unrelated pages are failing:

```powershell
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render path\to\document.qmd
```

Render Week 13 slides:

```powershell
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render week13\week_13.qmd
```

Render the syllabus for a different section:

```powershell
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render syllabus\syllabus.qmd -P class_section:11 --output syllabus_section_11.html --cache-refresh
```

Check git status with safe-directory because this checkout may trigger dubious ownership warnings:

```powershell
git -c safe.directory=C:/Users/bolan/Documents/boland_course status --short
```

Use `rg` for repository search:

```powershell
rg "search text"
rg --files
```

## Editing Conventions

- Before drafting assignment grading comments, rubric feedback, or revision
  notes, read `canvas/ASSIGNMENT-FEEDBACK-STYLE.md`. Use the instructor's
  historical comment patterns as voice seeds, grounded in the current
  submission and rubric.
- For first-pass grading, read `canvas/GRADING-WORKFLOW.md` and the assignment's
  private context in the separate `eco230-grading` directory. Keep submissions,
  assessments, review workbooks, and upload receipts outside both repositories.
  Instructor review precedes upload.
- Before treating assignment work as missing, inspect student-authored submission
  comments, comment attachments, and all earlier attempts (text and files),
  including Canvas-hosted links in those sources. Use
  `../eco230-canvas-ops/canvas/scripts/grading-probe-supplements.py` for the read-only fallback probe.
  Keep source attempt, comment author, and timestamps with recovered evidence;
  do not mistake instructor feedback for student work or overwrite a reviewed
  workbook when new evidence is found.
- Prefer small, focused edits that match existing Quarto style.
- Before creating or updating a weekly slide deck, read
  `.codex/guides/SLIDE_STYLE_GUIDE.md` and follow its global/shared/local CSS
  decision rules.
- Before creating or updating homework, practice work, interpretation prompts,
  or practicum materials, read `.codex/guides/ASSIGNMENT_STYLE_GUIDE.md`.
- Before building or publishing a standalone homework/lab repository or changing
  its Posit Cloud/renv setup, read
  `.codex/guides/POSIT_CLOUD_ASSIGNMENT_WORKFLOW.md`.
- Before creating or updating project prompts, timelines, presentation
  requirements, or rubrics, read `.codex/guides/PROJECT_STYLE_GUIDE.md`.
- Before provisioning, repairing, or publishing Canvas course content, read
  `.codex/skills/canvas-course-operations/SKILL.md` and use the existing
  wrapper scripts instead of reconstructing the workflow from individual CLI
  commands.
- Do not reorganize navigation unless asked.
- For Reveal.js decks, follow the Week 12 format:
  - `format: revealjs`
  - theme includes `default` and `../shared/styles/boland-reveal.scss`
  - `slide-number: true`
  - `hash: true`
  - `transition: fade`
  - `pdf-export: true`
  - `execute.echo/warning/message: false`
- For syllabus/admin assignment pages, keep plain HTML Quarto output with callouts and simple tables.
- Keep generated text ASCII unless the existing file clearly requires non-ASCII.
- Be careful with Quarto div fences (`:::` and `::::`) in slides. Unclosed fences can cause Reveal decks to nest or loop oddly.
- Use existing images in `weekXX/media/images` and `shared/images` rather than copying new assets unless requested.

## Syllabus Section Workflow

The syllabus should be section-aware. Do not hardcode dates, meeting times, final exam times, or section-specific rows in `syllabus/syllabus.qmd`.

Use `params$class_section` to select a section from `syllabus/data/sections.csv`.

Section-specific CSVs with `section_` prefixes should:

- include a `section` column,
- be filtered to the selected section before rendering,
- remove the `section` column before display.

References to week numbers are acceptable because they are date agnostic.

## Current Risks and Gotchas

- The worktree may already contain user or prior-agent changes. Do not revert unrelated changes.
- `syllabus/data/section_*.csv` and `syllabus/data/sections.csv` may be untracked in git. Confirm status before assuming they are committed.
- `_site/`, `_freeze/`, and `syllabus/syllabus_cache/` can contain stale rendered output. Use `--cache-refresh` when checking syllabus changes.
- Rendering from an editor command palette may render the whole website. Prefer explicit document render commands when debugging.
- Quarto/R renders may write to AppData caches and require elevated approval in the Codex sandbox.
- R may warn that the loaded `renv` version differs from the version configured in the project. This warning has not necessarily blocked renders.
- The global `_quarto.yml` CSS path should be checked before whole-site renders; the shared styles live under `shared/styles/`.
- Some older files may contain misspellings or legacy paths. Preserve them unless they affect the requested task.
- Watch for character-encoding problems such as stray A-circumflex characters or replacement characters in rendered HTML after editing copied syllabus text.
- A Canvas token set in the user's open PowerShell is not necessarily visible
  to a separate Codex terminal. The guarded wrappers fall back to the Windows
  user-scoped `CANVAS_TOKEN`. Persist it only when the user explicitly requests
  that convenience, using `../eco230-canvas-ops/canvas/scripts/set-canvas-token.ps1` in a terminal
  the user can see. Never print it or store it in repository files or logs.
- A copied New Quiz can appear in Assignments while its quiz-service API route
  returns 404. Do not retry alternate assignment or quiz endpoints repeatedly.
  Stop and repair or extend the repository API tooling, then resume the same
  provisioning run so completed checkpoints are reused.
- Never mutate Canvas through browser automation. The repository is the source
  of truth and all Canvas writes must use the reviewed API tooling or wrapper
  scripts. Browser access is read-only validation unless the user explicitly
  grants a one-time exception.
- Canvas Markdown video pages store Kaltura IDs in YAML front matter. The
  publisher must render every declared video as both an embedded player and a
  fallback link; a prose-only page is a renderer failure, not complete content.

## Verification Checklist

Before handing off changes:

- Run `git -c safe.directory=C:/Users/bolan/Documents/boland_course status --short`.
- Render the specific changed `.qmd` file when practical.
- If changing syllabus data or parameters, render at least one section with `-P class_section:<section> --cache-refresh`.
- Check rendered HTML for duplicated section rows and mojibake characters.
- For slides, open or inspect the rendered deck enough to confirm image paths and slide flow.
- For Canvas provisioning, verify assignment-group membership, module and
  module-item publication separately, and the course workflow state. Unless
  explicitly requested otherwise, the course itself must remain unpublished.
- For Canvas video pages, count the rendered Kaltura iframes and fallback links,
  confirm their entry IDs match the source metadata, and visually confirm at
  least one player loads before handing off.

## Repository ownership

Public teaching sources and Canvas content belong here. Canvas operational
code/tests belong in adjacent `eco230-canvas-ops/canvas/scripts`. Quizzes, tests,
keys, and assessment datasets belong in adjacent private `eco230-assessments`.
Student records stay in `eco230-grading` outside Git. Read
`canvas/REPOSITORY-BOUNDARIES.md` before adding new sources.
