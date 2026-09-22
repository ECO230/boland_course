# Lab 3 hints and completed workbook

`week03/labs/lab-3-hints.qmd` is the shared source for the website and native
Canvas hints page. Render that document before running
`../eco230-canvas-ops/canvas/scripts/sync-lab3-hints.ps1`; use `-Execute` to apply the reviewed
preflight and `-Execute -Resume` to resume its saved snapshot after interruption.

The wrapper converts the rendered document using the Canvas-safe Quarto
renderer, replaces only the external hints module reference, and replaces the
legacy completed-workbook module reference with `Lab3_Completed.twbx`.
The original Canvas solution files are retained. File release is controlled
through the file lock; Canvas does not permit changing publication through
`module_item[published]` for these File items.

The September 2026 demo workbook is stored at
`canvas/content/files/Lab3_Completed.twbx`, SHA-256
`8b3232add0971515af045edfef82acd9088fd0be1a5af92ee96dbabc93cc6ad8`.
It contains the seven demonstrated worksheets and packaged data. Its upload
was authorized by the instructor. It is course-hosted, not a public website
resource. Source references and transcripts for the hints are documented in
`lab3-video-guide-sources.md`.

Verified in sections 4, 11, and 12: seven native tables, two converted callouts,
matching workbook download hashes, original module positions, unchanged
unrelated item order, and unchanged course/module/page/item release states.
The hints and completed workbook remain unpublished in all three sections.
Receipts and returned native HTML are in ignored `canvas/work/lab3-hints/`.

Validation: `../eco230-canvas-ops/canvas/scripts/tests/test_lab3_hints.py` verifies the native
rendering and rejects missing transformations or executable content.
