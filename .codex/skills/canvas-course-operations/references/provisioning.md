# Provisioning and repair boundaries

Read the relevant workflow in `canvas/README.md` and its existing wrapper.
For a new Fall 2026 section use `provision-section.ps1`; after interruption pass
`-ResumeRunRoot` so successful plans, applies, and uploads are reused while
remaining live checks still run. Full inventory is appropriate for provisioning
or reconciliation; do not substitute a targeted description refresh.

Validate requested assessments are in exactly one intended assignment group,
`Imported Assignments` is empty when migration requires it, and expected module
counts/names match. Check object, module-item, module, and course publication
separately. Publish only requested modules; leave the course unpublished unless
course publication is explicitly authorized.

The older `content plan --only-page <manifest-key>` available-course path uses
`--allow-available-course-page-refresh`. Its existing guard restricts it to
existing unpublished pages and rejects creation, published pages, other object
types, and unscoped plans. Do not weaken that guard to accommodate a new task;
use the reviewed workflow whose explicit scope supports the operation.

## Copied New Quiz 404

An assignment can exist while its copied New Quiz service record returns 404.
After that specific failure, stop the mutation. Do not cycle through generic
Assignments endpoints, guess IDs, or edit in the browser. Repair or extend and
test API tooling, then resume the same provisioning run. Retain completed
checkpoints and perform remaining live checks, including exact assignment-group
membership and absence from `Imported Assignments`.

## Windows execution

The operator uses Windows PowerShell 5. Call tracked Python helpers rather than
multiline `python -c`, whose quotes can be stripped by native argument handling.
Validate new bridges with
`C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe` before live use.
Pass multiple sections as a PowerShell array such as `@(4,11,12)`, not a
comma-containing `-File` argument.
