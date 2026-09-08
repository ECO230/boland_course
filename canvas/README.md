# Canvas course configuration

The tracked configuration for ECO 230 Canvas publishing is under `manifests/`.
It currently represents a migration draft based on the Spring 2026 Section 12
course export. Final course publication is disabled while the Canvas delivery
model is repaired and reviewed one content type at a time. Draft content/file
synchronization does not authorize publishing the course or its modules.

Local `.imscc` exports are reference inputs only and are ignored by Git. Do not
store Canvas tokens, student submissions, grades, comments, or API responses in
this directory.

See `manifests/README.md` for the role of each generated file. Development and
operational commands live in `C:\Users\bolan\Documents\eco230-canvas-ops`.

Editable guided-note documents recovered from the reference export live under
`../shared/guided-notes/`. Only Guided Notes 1 is graded and submitted; the
remaining guided-note items are non-graded Canvas assignment wrappers with no
submission. See that
directory's README for provenance and reproduction instructions.

The guided-notes workflow was verified in Fall 2026 Section 4 on September 7,
2026: eight editable Word files were uploaded without overwrites, all eight
existing assignment wrappers were updated to use the Canvas-hosted files, and
the wrappers were republished while the course itself remained unpublished.
Guided Notes 1 is a 5-point pass/fail file upload; the other seven wrappers do
not accept submissions or create graded work.

The rubric layer was audited and repaired on September 7, 2026. Homeworks 1-8,
the three Project 1 deliverable assignments, and both Project 2 proposal shells
now have verified Canvas grading-rubric associations. Homework 1 uses the new
repository-owned 10-point rubric; the remaining associations were retained from
the reference-course rubric set. See `manifests/RUBRIC-AUDIT.md` for the exact
mapping and intentional exclusions.

Repository-owned Quarto documents may be delivered as native Canvas content
using `source.kind: canvas_quarto`. The QMD remains authoritative; Quarto first
renders the normal website HTML, and the Canvas publisher extracts only the
document body for a Canvas Page or assignment description. The website copy
may remain available as an unlisted fallback, but ordinary course documents
are not linked from the public site navigation. Reveal slides and interactive
applications remain external because their JavaScript behavior is not portable
to the Canvas rich-content editor.

The first native-Quarto pilot, `How should I study for the Practicum?`, was
synchronized to Fall 2026 Section 4 and visually reviewed on September 7, 2026.
The native Canvas body retained its headings, callouts, lists, and tables with
no external-page stub or executable scripts. The course remained unpublished.

Generate static PDFs for the current Week 1-7 Reveal decks with:

```powershell
& .\canvas\scripts\export-slide-pdfs.ps1
```

The script renders each QMD with Quarto, waits for Reveal through Playwright,
and prints a tagged Letter-landscape PDF with backgrounds. Outputs and a SHA-256
manifest are written under the ignored `canvas/work/slide-pdfs/` directory.
It uses the Codex-bundled `playwright-core` when available. On another machine,
run `npm install` in `canvas/scripts/` once before exporting.

Set a staged weekly module release without deleting Canvas content with:

```powershell
& .\canvas\scripts\set-module-visibility.ps1 -CourseId 870634 -ExpectedModuleCount 16
& .\canvas\scripts\set-module-visibility.ps1 -CourseId 870634 -ExpectedModuleCount 16 -Execute
```

The first command is read-only and writes a proposed before-state audit. The
second command publishes only `Course Info and Resources` and `Week 1: Intro to
Data Analysis`, unpublishes every other module, and verifies the live result.
Pass a different `-PublishedModuleNames` list as additional weeks are released.
The script changes module visibility only; underlying assignments, pages, and
module items retain their own Canvas publication states.
