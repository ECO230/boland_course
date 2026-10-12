# Course Website Publishing Workflow

Read this before publishing course website changes or updating its navigation.
Rendering, committing source, and deploying the website are separate operations.

## Source and deployment

- Source repository: `ECO230/boland_course`, branch `dev`.
- Site configuration and navigation: `_quarto.yml`.
- GitHub Pages serves branch `gh-pages`, directory `/`.
- Live site: https://eco230.github.io/boland_course/
- There is no course-site deployment workflow in `.github/workflows/`.
  `publish-assignment.yml` builds student repositories; it does not deploy this site.
- Pushing `dev` does not publish HTML or update live menus.
- Each generated HTML page carries its own navbar. Updating only `index.html`
  leaves stale menus on other pages. Reveal decks normally have no site navbar.

Do not depend on a prior chat's temporary scripts, variables, or checkout names.
The tracked commands below are the entry point for a fresh session.

## Navigation-only requests

1. Check source and deployment status. Read `_quarto.yml` and preserve unrelated
   working changes. Add the requested document to `project.render` when needed
   and add its entry to the appropriate `website.navbar.left` menu. Commit and
   push only the authorized source changes.
2. Use a new ignored run directory and plan the exact requested entries:

```powershell
& scripts/publish-site-navigation.ps1 `
  -RunRoot canvas/work/site-nav-week06-homework05 `
  -Links @('Slides=Week 6', 'Homework=Homework 5')
```

The labels must match `_quarto.yml` exactly. The command fetches the latest
`origin/gh-pages` and creates a fresh `codex/site-nav-*` branch/worktree under
that run directory. It verifies that destination HTML files already exist,
checks all published navbar pages, and saves `plan.json`. It does not publish.
A zero-change plan means the selected menus already match the source.

3. Review the compact plan. For a user-authorized publishing request, continue
   directly to execution; no additional confirmation is required:

```powershell
& scripts/publish-site-navigation.ps1 `
  -RunRoot canvas/work/site-nav-week06-homework05 -Execute
```

The command rejects changed source configuration, an advanced deployment branch,
or unrelated staged files. It stages only the planned HTML Git objects and
pushes to `gh-pages` without force. It changes only the navbar header and keeps
page bodies, data, scripts, styles, and other links intact. Existing valid
relative URLs are retained. An incorrect target is corrected using the canonical
course-site URL.

4. Wait for the expected GitHub Pages build, then verify:

```powershell
& scripts/publish-site-navigation.ps1 `
  -RunRoot canvas/work/site-nav-week06-homework05 -Verify
```

Verification requires the expected commit to have status `built`, checks the
live home page, every changed page, and representative nested pages, and opens
the requested destinations. It writes `receipt.json`. A queued/building build
or a stale live response is not success: wait briefly and retry verification.
Keep waits under 60 seconds when working interactively, and report progress.

Do not rerun plan mode against an existing plan. Reuse its execute/verify phases;
if source or the remote has changed, choose a fresh run directory. If a push
fails after a local commit, preserve the checkout and its saved `local_commit`;
inspect the remote state before retrying a non-force push. Do not reset work.

### Runtime

The PowerShell wrapper uses an existing Python environment with PyYAML. It
prefers `../eco230-canvas-ops/.venv/Scripts/python.exe`, then checks the Codex
bundled Python runtime. It does not require a Canvas token. Git and the GitHub
CLI (`gh`) must be available; use their existing authentication.

Pass `-PythonPath <python.exe>` when another environment provides PyYAML.
If none exists, use the available workspace Python to create an ignored local
virtual environment and install PyYAML there. Do not assume every bundled
runtime includes PyYAML, and do not change the global Python installation.

Run focused checks with the same Python environment:

```powershell
& ../eco230-canvas-ops/.venv/Scripts/python.exe scripts/test_publish_site_navigation.py
```

## Slide, homework, or other page content changes

The navigation command does not render or deploy new page content. For those
requests:

1. Read the applicable slide/assignment/project style guide. Render the specific
   changed QMD using the RStudio-bundled Quarto executable.
2. Confirm the output path reported by Quarto. Website targets usually write to
   `_site/`; documents outside the project's render allowlist can render beside
   their source with a local `<stem>_files` dependency directory. Never assume
   the output location or publish an older `_site` copy.
3. If a root render fails on unrelated sources, nested generated repositories,
   Windows permissions, or project-wide post-render hooks, render in an isolated
   project under `canvas/work/`. Copy only the requested public QMDs and their
   required styles, includes, images, and data. Remove unrelated render targets
   and post-render hooks from the preview configuration. Provide a `.here`
   marker when the source uses `here()` and point `.Rprofile` at the installed
   course R library when needed. Do not change the shared lockfile just to render.
4. Inspect the rendered page/deck and confirm every referenced local asset exists.
   Native plots are not standalone unless their figure files are also deployed.
5. Fetch the latest deployment branch. Use a fresh `codex/*` release worktree or
   a verified current suitable checkout. Stage an explicit allowlist: changed
   public HTML, its actual referenced dependencies, and relevant public images
   or downloads. Preserve existing navigation; deploy any requested menu update
   using the tracked navigation command. Update only the affected search entries
   if content titles/searchable text change.
6. Commit only the approved deployment files, push `HEAD:gh-pages` without force,
   wait for the matching Pages build, and verify live HTML and assets.

Never copy the entire `_site/` blindly or stage everything in a deployment
checkout. It can contain stale outputs or unintended nested working material.
Canvas work products, private assessments/keys, student records, caches, Posit
exports, and generated project/release source directories are not website assets.

## Windows case collisions and older checkouts

The deployment branch contains legacy case-colliding names, including
`week04/Homework_04.html` and `week04/homework_04.html`, and historical Week 7
variants. A Windows checkout can show local changes even immediately after
checkout, and reading two differently cased paths can return the same file.

For navigation, use the tracked publisher: it reads each exact path with
`git show HEAD:<path>`, writes a Git object for the modified navbar, and stages
that object with `git update-index --cacheinfo`. It commits the index, not
`git commit --only` against filesystem copies. Tests cover distinct page bodies,
legacy menus, wrong links, and unsafe target paths.

For content publishing involving a collision, review the exact Git path and
stage the intended content by Git object rather than trusting the filesystem
alias. Do not delete or rename the legacy route as an incidental cleanup.

Many historical worktrees remain under `canvas/work/`. Some are stale or dirty;
the directory name is not evidence that it is the current deployment. Use
`git worktree list --porcelain`, compare HEAD with the fetched `origin/gh-pages`,
and inspect status before reusing one. Do not check out `gh-pages` in the primary
source checkout: that branch may already be attached to another worktree.

## Handoff

Report what changed, the source and deployment commits, whether the matching
Pages build completed, and the verified live URL. For navigation, link the saved
receipt when useful. Do not claim publication from a source commit or a successful
local render alone.
