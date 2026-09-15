# Canvas course configuration

The tracked configuration for ECO 230 Canvas publishing is under `manifests/`.
It currently represents a migration draft based on the Spring 2026 Section 12
course export. Final course publication is disabled while the Canvas delivery
model is repaired and reviewed one content type at a time. Draft content/file
synchronization does not authorize publishing the course or its modules.

Local `.imscc` exports are reference inputs only and are ignored by Git. Do not
store Canvas tokens, student submissions, grades, comments, or API responses in
this directory.

To persist `CANVAS_TOKEN` for the current Windows user without placing it in
PowerShell history or the repository, run:

```powershell
& .\canvas\scripts\set-canvas-token.ps1
```

Enter the token at the masked prompt. Windows stores user environment variables
as plaintext in the current user's registry, so use this only on a trusted
single-user computer. The guarded wrappers check both the current process and
the persistent user environment.

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

Quarto callouts require an additional Canvas conversion step. The publisher
replaces Bootstrap-dependent callout markup with inline-styled Canvas-safe
blocks and removes Quarto's hidden screen-reader label before upload. Verify
that each callout title appears once; flattened text such as `TipImportant`
means the renderer failed and must be repaired in the API tooling rather than
edited in Canvas.

Provision a new Fall 2026 section through the complete guarded workflow with:

```powershell
& .\canvas\scripts\provision-section.ps1 -Section 12 -CourseId 870121 -Execute
```

The script selectively seeds protected Canvas assessments and private files,
creates and verifies the repository-managed course structure and content,
applies the reviewed Week 1 repairs, moves the graded quizzes, Lab 6, practice
activity, and traditional final into their required assignment groups, and
publishes only `Course Info and Resources` and `Week 1: Intro to Data Analysis`.
It refuses to run without the exact section/course mapping and leaves the Canvas
course itself unpublished.

Resume an interrupted run by passing its existing work directory so successful
plans, applies, uploads, and verifications are not repeated:

```powershell
& .\canvas\scripts\provision-section.ps1 -Section 12 -CourseId 870121 -ResumeRunRoot "C:\Users\bolan\Documents\eco230-canvas-ops\work\section-provisioning\fall-2026-12-<timestamp>" -Execute
```

Copied New Quizzes have a Canvas-specific edge case: the assignment can be
visible while its quiz-service API record returns 404. Do not burn retries on
the generic Assignments endpoint or alternate quiz IDs, and do not edit Canvas
through the browser. Stop, add and test the required API support in the
repository tooling, then resume the same run. The wrapper will reuse its
completed checkpoints and continue the remaining validation.

Refresh the repository-owned Week 2 video pages in every Fall 2026 section with
one guarded API workflow:

```powershell
& .\canvas\scripts\refresh-week2-video-pages.ps1 -Sections 4,11,12 -Execute
```

The wrapper renders all three pages from their tracked Markdown metadata,
updates them through the Canvas Pages API, verifies every player and fallback
link, and confirms that course, module, and page publication states did not
change.

After refreshing the repository-owned Lab 2 page, publish only that page and
its existing Week 2 module item with:

```powershell
& .\canvas\scripts\publish-lab2-page.ps1 -Sections @(4,11,12)
& .\canvas\scripts\publish-lab2-page.ps1 -Sections @(4,11,12) -Execute
```

The first run is read-only. The execute run requires the exact section/course
mapping, all 16 expected modules, one published `Week 2: Descriptive
Statistics` module, and exactly one matching Lab 2 page and module item. It
publishes neither the course nor a module and verifies that every course and
module publication state is preserved.

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
& .\canvas\scripts\set-module-visibility.ps1 -CourseId 870634 -ExpectedModuleCount 16 -PublishSelectedContent -KeepModulesUnpublished -Execute
```

The first command is read-only and writes a proposed before-state audit. The
second command publishes the content and module items in `Course Info and
Resources` and `Week 1: Intro to Data Analysis`, keeps every module
unpublished, and verifies the live result. The instructor can then publish the
selected modules and course manually. Omit `-KeepModulesUnpublished` only when
the script should publish the named modules after verification. Pass a
different `-PublishedModuleNames` list as additional weeks are released.

## Fall 2026 operational status

Section 12 (`870121`) completed provisioning on September 9, 2026. Canvas was
validated with `Course Info and Resources` and `Week 1: Intro to Data Analysis`
as the only published modules; modules 3-16 and the course remained
unpublished. Graded Quiz 1-5 were verified in `Quizzes`, Lab 6 and the practice
activity in `Participation Score`, and the traditional final in
`Final Traditional Portion: Raw Grade`. `Imported Assignments` was empty.
The Week 2 video-page renderer was repaired and its three live pages were
refreshed from repository source through the Canvas API on September 10, 2026,
in sections 4, 11, and 12. Each section was verified with 11 Kaltura players
and 11 matching fallback links. Page, module, and course publication states
were preserved; Section 12 remained unpublished.

Section 11 (`869206`) completed the full provisioning workflow on September 8,
2026. The selective Week 1 refresh then updated the section-aware syllabus and
student-hours pages, combined Technology Set-Up and the Posit Cloud certificate
activity as `Lab 1: Technology Setup`, enabled text entry and file upload for
Homework 1, and linked its fallback CSV files and dataset descriptions. The
instructor reviewed and published Section 11 manually.

Section 4 (`870634`) completed the same targeted Week 1 refresh on September 8,
2026. Because that older build contained only `Syllabus - Link`, the refresh
created and placed the native `Syllabus` page. It also removed the redundant
syllabus, Lab 1, and Homework 1 external module links. The updated content
objects are published, while all Section 4 modules and the course remain
unpublished for manual review and release.

Use `scripts/refresh-week1-section.ps1` for this repair on an unpublished
section. The script is section-parameterized, validates the course ID and all
16 modules, writes its audit under the ignored Canvas operations work area,
and leaves module and course publication to the instructor. It refuses to
modify a published course.

## Operational lessons

- Canvas content-object publication, module-item publication, module
  publication, and course publication are separate states. Verify each layer
  explicitly instead of treating a published module as proof that its contents
  are available.
- The Canvas content planner validates every repository-owned Quarto source,
  even during a selective repair. Canvas-native HTML dependencies therefore
  remain in `_quarto.yml` as unlisted render targets; they are generated for
  extraction without appearing in the public navigation.
- A partial apply is recoverable. Re-plan from the live Canvas inventory and
  repeat only the unfinished phase; successful assignment and page updates do
  not need to be rolled back.
- Established courses may contain intentionally published calculated or manual
  gradebook shells. Those objects can block a full placement plan even when the
  requested module repair is unrelated. The Week 1 refresh consequently makes
  only the reviewed native-syllabus placement and obsolete-link removals.
- Canvas list responses should always be flattened explicitly in PowerShell.
  Otherwise a one-item response can be mistaken for the entire collection and
  cause misleading module-count or missing-property errors.
- Course-file name conflicts require deliberate comparison or overwrite
  approval. Do not assume that a same-named Canvas file matches the repository
  source.
- The guarded wrappers check `CANVAS_TOKEN` first in the current process and
  then in the persistent Windows user environment. Use
  `scripts/set-canvas-token.ps1` only in a terminal the user can see; avoid
  hidden prompts that cannot be observed.
- When the New Quizzes API returns 404 for a copied quiz that is visible in the
  Canvas UI, stop endpoint retries and extend the API tooling. Do not use the UI
  to bypass the repository publishing workflow.
- Canvas video metadata is not student-facing by itself. For every `videos`
  entry in Canvas Markdown front matter, verify that the published body contains
  one Kaltura iframe and one matching fallback link. A successful API update
  with prose but no players is not a successful video-page migration.
- Canvas browser access is for read-only visual verification. All mutations
  must originate in repository content and run through the API wrappers.
- An available course remains protected from general content applies. A repair
  to an existing unpublished repository page requires a reviewed
  `content plan --only-page <manifest-key>` and the explicit
  `--allow-available-course-page-refresh` apply guard. Verify the selected page
  and confirm that page, module, and course publication states did not change.
- A repair to one existing unpublished assignment in an available course uses
  `content plan --only-assignment <manifest-key>` and the explicit
  `--allow-available-course-assignment-refresh` apply guard. Add
  `--refresh-existing-rubric` to the plan only when its existing associated
  rubric must be synchronized too. The reviewed plan must not contain creates,
  publication changes, group changes, or unrelated content.
- To remove a duplicate module link already marked `omit_duplicate`, use a
  reviewed `placement plan --only-omitted <manifest-item-key>` and apply with
  both `--remove-omitted` and
  `--allow-available-course-omitted-removal`. Confirm afterward that the target
  placement is absent and every course/module publication state is unchanged.
- Windows PowerShell 5 can corrupt multiline Python supplied through
  `python -c`. Cross-language wrappers must call a tracked Python helper file
  and be tested with the Windows PowerShell 5 executable used by the operator.
