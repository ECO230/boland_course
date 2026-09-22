# Targeted page and assignment refresh

Use this for existing Markdown or rendered Quarto page bodies and assignment descriptions. Object
creation, files, quizzes, grading, schedules, grading settings, and publication
changes use their dedicated workflows. Known IDs avoid full-course discovery.

From the course root:

```powershell
& ..\eco230-canvas-ops\canvas\scripts\refresh-content.ps1 -Manifest <manifest.json> -RunRoot <ignored-run-directory>
& ..\eco230-canvas-ops\canvas\scripts\refresh-content.ps1 -Manifest <manifest.json> -RunRoot <same-directory> -Execute
& ..\eco230-canvas-ops\canvas\scripts\refresh-content.ps1 -Manifest <manifest.json> -RunRoot <same-directory> -VerifyOnly
```

Default mode plans without Canvas writes. Execute only within the user's
authorized scope. Reuse the run directory for interruption recovery. Fresh
verification remains necessary even when every object is unchanged.

## Manifest

The JSON contains `sections` and `objects` arrays:

- `sections`: each entry has `section`, `course_id`, and optional `course_code`
  (defaults to `ECO 230-XX`). The course ID must match configured section data.
- `objects`: each entry has `kind` (`page` or `assignment`), `id` (page slug or
  numeric assignment ID), repository-relative `source_path`, and known
  `module_id`. Assignments require `assignment_group_id`; quiz assignments are
  rejected and belong in the quiz workflow.
- `validation.required_text` and `validation.forbidden_text`: optional arrays
  of required/forbidden phrases. Keep these in the manifest, not Python assertions.
- `targets`: optional section overrides keyed by section string, such as
  `"11": {"id": 12345, "module_id": 67890, "assignment_group_id": 45678}`.
  Supply actual discovered IDs; they do not transfer between courses.
- `input_paths`: optional repository-relative original source/dependency paths
  to pin alongside rendered output. For HTML, include the original QMD and
  relevant render inputs. `published_url` provides the HTML source's website URL
  for the Quarto conversion when needed.

Markdown sources use the shared `render_canvas_markdown` renderer, including course ID
substitution and video metadata. Derive video entry IDs and expected player/
fallback counts from source front matter; never maintain duplicate counts in
an operation-specific helper. A `.html` source uses `render_canvas_quarto` to extract the Canvas-safe body; render the QMD before planning and include its path in `input_paths`. The refresh wrapper does not render Quarto itself.

## Plan, apply, verify

The plan pins source/configuration and renderer identities, retrieves only
affected objects and placements, and records the existing publication state.
Review the compact plan summary and inspect a diff only where needed.

Apply freshly checks concurrency, skips semantic-equivalent bodies, writes
only changed objects, and saves checkpoints. HTML equivalence is owned by the
shared tested normalizer, which permits known Canvas transformations without
ignoring meaningful links, text, or embeds. Do not add a local normalizer to
work around a failed run.

Verification rereads live state, checks rendered content and declarative
requirements, affected module placement, applicable assignment groups, and
publication layers. Receipts are evidence and recovery checkpoints, not a
replacement for those reads. If source/configuration/renderer changes invalidate
a plan, regenerate deliberately; do not edit its hashes or bypass the guard.

Keep detailed snapshots and receipts in ignored run directories. On mismatch,
inspect the saved failure once, repair the cause, then resume the same workflow;
avoid repeated writes while experimenting with comparison rules.
