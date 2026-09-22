# Course sources, operations, and private assessments

Clone these repositories side by side:

| Location | Owns |
| --- | --- |
| `boland_course` (public) | Teaching sources, slides, homework/lab prompts, Canvas page Markdown, public datasets, uploaded teaching files, course/rubric manifests, and provenance |
| `eco230-canvas-ops` (private) | Canvas publishing, page/assignment/quiz creation, verification, scheduling, grading tooling, and operational tests |
| `eco230-assessments` (private) | Quiz/test questions, answer keys, assessment datasets, source builders, setup instructions, and hash-pinned assessment payloads |
| `eco230-grading` (local, no Git) | Student submissions, assessments, instructor review workbooks, calibration, and upload records |

Assessment repository: https://github.com/ECO230/eco230-assessments

Operational scripts now live in `../eco230-canvas-ops/canvas/scripts/`, relative
to the course root. Canvas content is still sourced from this repository;
scripts are not duplicated here. Operational documentation lives in the
operations repository's `canvas/` directory.

The private assessment checkout defaults to `../eco230-assessments`; set
`ECO230_ASSESSMENTS_ROOT` for another location. GitHub access requires private
repository credentials. Canvas access separately uses `CANVAS_TOKEN` through
the existing process/user environment fallback. Never commit credential values.

The public quiz manifest is a pointer only. Questions, answer keys, datasets,
and the reviewed payload digest live in the private repository. Neither the
private repository nor its rendered previews may be deployed to the public site.

Existing ignored `canvas/work/` receipts remain in place so interrupted jobs
can resume. The old Quiz 1 draft is a retained local backup, not the active
source; Canvas ops reads the private repository. Do not edit the old draft.

Before adding files to this public repository, classify them: teaching sources
and required course downloads belong here; operational code belongs in Canvas
ops; assessment-only content belongs in the private assessment repository;
student records belong outside Git. Generated caches and review outputs do not
belong in version control.
