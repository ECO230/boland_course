# Scratch storage

Use **S:/codex-scratch/boland_course** for generated work on this Windows host.
The C: checkout remains the source of truth. S: is a mapped drive and may
require an escalated shell. Check that it is visible before starting a large
download, render, or data build. Do not fall back to filling C: silently.

| Scratch use | Location |
|---|---|
| Local data candidates, extracts, and frozen bundles | S:/codex-scratch/boland_course/data-build/ |
| Generated assignment and project bundles | S:/codex-scratch/boland_course/assignment-build/ |
| Isolated Quarto and tutorial renders | S:/codex-scratch/boland_course/canvas/work/<task>/ |
| Website deployment worktrees and receipts | S:/codex-scratch/boland_course/canvas/work/<task>/ |
| Per-command downloads and temporary files | S:/codex-scratch/boland_course/tmp/ |

The website navigation publisher accepts an absolute RunRoot under the S:
scratch canvas/work directory. Its deployment worktree uses --no-checkout
and a Git index, avoiding a complete copy of the published site. Git metadata
and objects remain in the source repository on C:.

Use explicit S: paths for tools that accept them. For isolated Quarto renders,
stage a small project on S: and render from there. Set TEMP and TMP for the
individual command when the tool supports a mapped drive; leave system-wide
settings alone. Keep installed runtimes and existing virtual environments in
place; many embed absolute paths.

The existing assignment and group-data builders currently validate their
outputs under the checkout's .assignment-build or .data-build directories.
Do not bypass those guards or pass an unsupported S: path. For a large build,
add and validate explicit scratch-root support before running it. Legacy
receipts may still refer to their original C: paths; use the migration log to
locate the corresponding artifact on S:.

## Active Week 7 preview

The local preview at http://127.0.0.1:8777/week7-chi-square.html serves from
S:/codex-scratch/boland_course/canvas/work/week7-eco230r-live-20261107/build/_site.
Its URL and browser-saved progress are unchanged. Its relocation is recorded
in preview-migration-20261107.jsonl beside the main move log.

## Migration records

The move log is S:/codex-scratch/boland_course/migration-20261107.jsonl.
It records the old path, new path, and byte count for each verified move, or
why an entry was retained. Copies were checked by SHA-256 before removing
their C: originals. The C: roots still exist for retained files and tools
that require them. No directory links were created: creating a local-to-S:
symbolic link requires administrator privileges on this host.

Leave registered worktrees, standalone Git checkouts, active previews, and
private Canvas or grading records in their established locations unless
specifically migrating them with the appropriate workflow. Never publish
scratch bundles, logs, or credentials.


## Navigation deployment runs relocated November 19, 2026

Navigation checkouts created on C: during the November 17-19 publishing work
were copied to S:/codex-scratch/boland_course/canvas/work/migrated-from-c-20261119.
Every file was hash-verified before the original was removed. Git worktree
registrations were repaired to use the new locations. The migration.json in
that directory maps original run directories to their S: locations.

The current rubric-navigation deployment and its receipt use
S:/codex-scratch/boland_course/canvas/work/nav-p2-rubric-20261119.
The related Canvas refresh receipts use
S:/codex-scratch/boland_course/canvas/work/project2-canvas-rubric-20261119/canvas-refresh.
