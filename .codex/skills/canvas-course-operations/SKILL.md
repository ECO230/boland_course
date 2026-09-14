---
name: canvas-course-operations
description: Provision, repair, publish, and validate ECO 230 Canvas sections with the repository's guarded workflows. Use for Canvas section population, assignment-group placement, module visibility, or unpublished-course safety checks.
---

# Canvas Course Operations

Use the existing Canvas automation and its receipts; do not reconstruct a long
sequence of individual `canvasctl` commands when a repository wrapper covers
the request.

## Start Here

Read `canvas/README.md`, then inspect the relevant script before acting. For a
new Fall 2026 section, use `canvas/scripts/provision-section.ps1`. Pass
`-ResumeRunRoot` after an interrupted run so completed plans, applies, uploads,
and verifications are reused.

The guarded wrappers resolve `CANVAS_TOKEN` from the current PowerShell process
and then the Windows user environment. If the user explicitly requests
persistence, have them run `canvas/scripts/set-canvas-token.ps1` in a terminal
they can see. It accepts the token through a masked prompt and saves a
user-scoped environment variable. Explain that Windows stores this value as
plaintext in the user's registry. Never print the token or store it in the
repository, receipts, command history, or logs. Do not rely on an interactive
prompt or tab the agent cannot observe.

For a targeted Week 2 video-page repair in sections 4, 11, and 12, use
`canvas/scripts/refresh-week2-video-pages.ps1`. It renders the three tracked
Markdown sources, updates Canvas through the Pages API, and verifies video IDs,
player and fallback-link counts, and unchanged publication states.

The operator uses Windows PowerShell 5. Do not pass multiline Python source to
`python -c`; its native argument handling can strip the Python string quotes.
Call a tracked Python helper file and validate the bridge with
`C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe` before a live run.
When passing multiple sections from a parent process, use a PowerShell array
such as `@(4,11,12)` rather than a comma-containing `-File` argument.

## Mutation Boundary

The repository is the source of truth for Canvas. Never mutate Canvas through
browser automation or direct UI editing. Every Canvas write must originate in
tracked repository content and use the reviewed API tooling or a repository
PowerShell wrapper. Browser access is limited to read-only visual validation
unless the user explicitly grants a one-time exception.

If the API tooling cannot perform a required operation, stop that mutation,
repair or extend the tooling, add proportional tests and validation, and then
resume the guarded workflow. Do not bypass an API limitation with a browser
edit.

For a repository-page repair after a course is already available, use a
reviewed `content plan --only-page <manifest-key>`. Apply it only with the
explicit `--allow-available-course-page-refresh` guard. That path is restricted
to existing unpublished pages and preserves the page, module, and course
publication states; it must reject creation, published pages, other object
types, and unscoped plans. Follow with selective `content verify` and a
read-only body check for the expected links or embeds.

## Copied New Quiz 404

A copied New Quiz may be listed in Canvas Assignments even though the
quiz-service API returns 404 for its assignment ID. After that specific failure:

1. Do not cycle through the generic Assignments endpoint or guessed quiz IDs.
2. Stop before making any browser or UI mutation.
3. Repair or extend the repository API tooling and test the new behavior.
4. Resume the same provisioning run. The wrapper will skip successful
   checkpoints and perform the remaining live API validations, including exact
   assignment-group membership and absence from `Imported Assignments`.

## Required Validation

Validate live Canvas state, not just a successful command exit:

- every requested assessment is in exactly one intended assignment group;
- `Imported Assignments` is empty when the migration calls for it;
- content objects, module items, modules, and the course are checked as separate
  publication layers;
- only the user-requested modules are published;
- the course remains unpublished unless the user explicitly authorizes course
  publication; and
- the expected module count and names match before and after mutation.

For Canvas Markdown pages with `kaltura_partner_id` and `videos` front matter,
the rendered body must contain one playable Kaltura iframe and one matching
open-in-new-tab fallback link for every declared `entry_id`. Count both, compare
their IDs to the source metadata, and visually confirm at least one player
loads. A page that contains only its prose is a renderer failure even if the
content apply exited successfully.

Keep status updates concise and checkpoint-oriented. Do not ask the user to
rerun a completed phase merely to generate a cleaner receipt when the live
state has already been independently verified.
