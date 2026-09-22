---
name: canvas-course-operations
description: Publish and verify repository-owned ECO 230 Canvas pages and assignment descriptions efficiently; provision or repair sections through guarded Canvas operations workflows.
---

# Canvas Course Operations

Use tracked sources and existing wrappers in `../eco230-canvas-ops/canvas/scripts/`.
Keep one publisher. Never mutate Canvas through browser automation; browser
access is read-only validation unless the user grants a one-time exception.
If tooling cannot perform the operation, repair it and test before resuming.

## Choose the smallest workflow

| Request | Route |
| --- | --- |
| Refresh existing Markdown/rendered Quarto pages or assignment descriptions | `refresh-content.ps1`; read [targeted refresh](references/targeted-refresh.md) |
| Independently verify a completed refresh | Same wrapper with `-VerifyOnly`; use [verification protocol](references/verification.md) |
| Create objects, provision sections, change groups or publication | Existing operation-specific wrapper; read [provisioning and repair](references/provisioning.md) and the relevant part of `canvas/README.md` |
| Grade work or update quiz questions | Follow the grading/private assessment workflows; a description refresh cannot replace them |

Do not load the full inventory, historical receipts, or every reference for a
small refresh. Start with the manifest, affected sources, and compact receipt.
Read detailed artifacts only for a failure or an unresolved requirement.

## Credentials and scope

Wrappers resolve `CANVAS_TOKEN` from the process then Windows user environment.
Never print it or store it in sources, logs, receipts, or command history.
Persist it only on explicit request, using `set-canvas-token.ps1` in a visible
terminal with its masked prompt; explain that Windows stores the user variable
as plaintext in the registry.

Preserve object, module-item, module, and course publication states on refresh.
Provisioning keeps the course unpublished unless explicitly authorized.
Track public teaching sources here, operational code/tests in Canvas ops,
quiz/test material in private assessments, and student records outside Git.

## Verification and reporting

A successful write is not verification. Require fresh live checks of affected
content, links, metadata-derived video embeds/fallbacks, placement and applicable
assignment groups, plus preserved publication states. Use receipts with source,
configuration, and renderer identities; cached checkpoints never authorize
skipping fresh concurrency or live verification checks.

For new or changed video embeds, or a changed embed renderer, visually confirm
at least one player loads. Reuse documented playback evidence for unchanged
embeds during a prose-only refresh; still check every declared ID and iframe/
fallback count deterministically. Honor any broader task-specific verification
requirement. A prose-only render of a declared video page is a failure.

Report changed/unchanged/failed counts, receipt path, verification status, and
API request count. Do not dump bodies or full receipts into conversation. Do not
rerun a completed phase merely to produce a prettier report.
