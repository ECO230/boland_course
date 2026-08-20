# ECO 230 Posit Cloud Assignment Workflow

This guide records the tested workflow for distributing ECO 230 homework and
labs through Posit Cloud without requiring students to use Git.

## Architecture

Use the course repository as the source-of-truth monorepo. Each distributable
assignment has a manifest that selects only the files students need. The build
script copies that allowlist into a small generated repository, and Posit Cloud
creates a source project from that repository.

```text
course monorepo
  -> assignment manifest
  -> scripts/build_assignment_repo.py
  -> generated assignment repository
  -> Posit Cloud source project
  -> Posit Cloud assignment
  -> one independent project per student
```

Git is instructor-side distribution infrastructure. Students should not need to
clone, pull, commit, switch branches, or resolve conflicts. Once a student
starts an assignment, their Posit Cloud project does not need to stay
synchronized with the source repository.

## Tested Reference Assignment

Homework 6 is the reference implementation:

- source document: `week07/Homework_06_assignment.qmd`;
- student README: `week07/Homework_06_README.md`;
- manifest: `week07/homework_06.manifest.json`;
- generated repository: `.assignment-build/homework_06_ip`;
- GitHub reference repository: `ECO230/homework-06-ip`;
- shared environment files: `shared/posit-cloud/`.

The generated repository is intentionally not a mirror of the week folder. Its
contents are defined only by the manifest.

Use a delivery-mode suffix when an assignment has separate versions:

- `IP` means in person;
- `OA` means online asynchronous.

Use lowercase kebab-case for repository names, such as `homework-06-ip`, and
the course filename pattern for student files, such as `Homework_06_IP.qmd`,
`Homework_06_IP.Rproj`, and `Homework_06_IP.pdf`. Write the delivery mode out in
the visible document subtitle (`In-Person Sections` or `Online Asynchronous`)
so students do not need to interpret the abbreviation.

## Manifest Rules

Every assignment manifest must:

- use `schema_version: 1`;
- have a unique assignment `id`;
- write beneath `.assignment-build/`;
- map every source file to an explicit student-facing destination;
- list essential outputs under `required_files`;
- name the primary `.qmd` file under `render`;
- include only data, media, instructions, and environment files needed by that
  assignment.

Do not copy an entire week directory or rely on a broad glob. The allowlist is a
privacy and reproducibility boundary: instructor notes, solutions, unrelated
data, and monorepo configuration must not enter a student repository.

Typical shared entries are:

```text
shared/posit-cloud/.Rprofile
shared/posit-cloud/.gitignore
shared/posit-cloud/project.Rproj
shared/posit-cloud/project_setup.R
shared/posit-cloud/renv.lock
shared/posit-cloud/renv/activate.R
shared/posit-cloud/renv/settings.json
```

Build an assignment locally with:

```powershell
& "C:\Users\bolan\.cache\codex-runtimes\codex-primary-runtime\dependencies\python\python.exe" `
  scripts\build_assignment_repo.py `
  week07\homework_06.manifest.json
```

The builder deletes and recreates the selected output directory. Never point an
assignment manifest outside `.assignment-build/`.

## R and renv Baseline

The tested Posit Cloud baseline is:

- R 4.6.1;
- Ubuntu 24.04 (Noble), x86_64;
- `renv` 1.1.8;
- Posit Public Package Manager;
- a lockfile created under R 4.6.1, not copied from the broader course website.

The assignment lockfile records only the assignment dependency tree. For the
Homework 6 reference environment, the explicit snapshot roots are:

```r
renv::snapshot(
  packages = c(
    "renv",
    "tidyverse",
    "eco230r",
    "knitr",
    "rmarkdown"
  ),
  prompt = FALSE
)
```

Include `renv` itself because `.Rprofile` activates it. Confirm the result with
`renv::status()` before publishing the lockfile.

### Binary package configuration

Use the plain Package Manager source URL and allow `renv` to transform it once:

```r
posit_package_repo <- "https://packagemanager.posit.co/cran/latest"

options(
  repos = c(CRAN = posit_package_repo),
  renv.config.ppm.enabled = TRUE
)
```

Do not use `https://cloud.r-project.org` for these projects. That repository
serves Linux source packages and caused memory failures while compiling
`RcppEigen`.

Do not supply an already platform-specific `/bin/linux/` URL while PPM rewriting
is enabled. `renv` may rewrite it a second time.

Do not set `renv.config.repos.override` to a string such as
`CRAN=https://...`. In the tested `renv` version, that value was treated as the
literal URL and caused requests beginning with `CRAN=`.

The correct configuration lives in both `.Rprofile` and `project_setup.R`.
`.Rprofile` must set it before sourcing `renv/activate.R`, while
`project_setup.R` passes `repos = getOption("repos")` to `renv::restore()` so the
old repository recorded in a lockfile cannot take precedence.

### Fresh-project student setup

On first opening a project, `renv` bootstraps itself. The student or instructor
then runs once:

```r
source("project_setup.R")
```

This restores the locked environment. Binary installation reduced the tested
Homework 6 install to under one minute at the normal Posit Cloud memory level.

If RStudio displays an automatic snapshot/install prompt before restoration,
cancel it and run `source("project_setup.R")`. Do not snapshot an empty or
partially restored library.

The setup script should remain explicit rather than automatically restoring on
every R startup. This keeps startup understandable and prevents repeated network
work when a restore fails.

## Quarto PDF Standard

Student homework should render directly to PDF with Typst. Typst avoids a TeX
installation and rendered successfully in Posit Cloud.

Use this compact profile when an assignment should resemble the earlier R
Markdown `pdf_document` format:

```yaml
format:
  typst:
    toc: false
    papersize: us-letter
    fontsize: 10pt
    linestretch: 1
    margin:
      x: 1in
      y: 1in
    number-sections: false
    page-numbering: "1"
```

The Homework 6 test produced a legible five-page PDF with this profile. Retain
native Quarto callouts unless a later shared Typst theme deliberately changes
them. The current font is Typst's default Libertinus Serif; matching Latin
Modern exactly is optional and would require bundling font files or a custom
Typst template.

RStudio may display a PDF through an HTML-backed preview URL. That does not mean
the assignment rendered to HTML. The deliverable is the `.pdf` file created in
the project. Students can download it from the browser PDF viewer or export it
from the Files pane.

## Publishing

The workflow `.github/workflows/publish-assignment.yml` accepts a manifest path,
target repository, and visibility. It:

1. checks out the monorepo;
2. builds the manifest allowlist;
3. uploads the generated folder as an artifact;
4. creates a new GitHub repository;
5. commits and pushes the generated files.

It requires the repository secret `ASSIGNMENT_REPO_TOKEN`. The workflow refuses
to overwrite an existing target repository. That behavior is intentional for
semester releases.

For iterative testing of an already-created repository, rebuild locally, review
the exact generated diff, commit inside the generated repository, and push its
`main` branch. Students should never perform this update process.

After the repository is final:

1. create a Posit Cloud project from its GitHub URL;
2. verify R 4.6.1 is selected;
3. run `source("project_setup.R")`;
4. require `renv::status()` to report a consistent project;
5. render the `.qmd` and inspect the PDF;
6. mark the verified project as the Posit Cloud assignment source;
7. have students create their own copies from the assignment.

Private repositories require Posit Cloud's GitHub connection to have private
repository access. If the repository belongs to the `ECO230` organization, the
organization's third-party application policy must also permit Posit Cloud.

## Verification Checklist

Before releasing any assignment:

- build from the manifest into a clean temporary output directory;
- inspect the output file list for accidental instructor-only content;
- verify every `required_files` entry exists;
- parse `renv.lock` and confirm its R version matches Posit Cloud;
- confirm `renv`, direct dependencies, and any GitHub package are recorded;
- create a brand-new Posit Cloud project from the generated repository;
- restore at the standard memory allocation and confirm compiled dependencies
  install as binaries;
- run `renv::status()`;
- render the primary `.qmd`;
- visually inspect every PDF page for clipping, overflow, broken paths, and
  illegible code or tables;
- verify the instructions name the correct PDF and Canvas submission process.

## Known Failure Signatures

### `RcppEigen` or another C++ package compiles until the project stops

Cause: installation from a source-only CRAN mirror exhausted memory.

Resolution: restore the plain Posit Package Manager URL with PPM rewriting
enabled. The console should report `installing *binary* package`.

### Repository URL contains two Linux platform segments

Cause: an explicit `/bin/linux/` URL was combined with `renv` PPM rewriting.

Resolution: use only `https://packagemanager.posit.co/cran/latest`.

### Repository request begins with `CRAN=https://...`

Cause: malformed `renv.config.repos.override`.

Resolution: remove the override and use the normal named `repos` option.

### `rlang` fails to compile under R 4.6

Cause: the older course lockfile pinned an `rlang` release that used R APIs no
longer available in R 4.6.

Resolution: build the assignment environment under R 4.6.1 and record a current
compatible release. Homework 6 currently records `rlang` 1.3.0.

### `renv::status()` says `renv` is installed and used but not recorded

Cause: `renv` was omitted from an explicit snapshot package list.

Resolution: include `"renv"` in `renv::snapshot(packages = ...)`.

### Posit Cloud cannot access a valid private GitHub repository

Cause: GitHub OAuth access or the organization third-party application policy
does not authorize Posit Cloud.

Resolution: verify both the Posit Cloud GitHub connection and the GitHub
organization application approval.

## Current Design Decisions

- One generated repository per assignment or lab.
- One Posit Cloud source project per generated repository.
- One independent student copy per assignment.
- No student Git workflow.
- No synchronization requirement after a student begins work.
- Shared source assets and dependency policy live in the monorepo.
- Manifest allowlists decide what is distributed.
- Typst is the default PDF engine for assignment deliverables.
- The generated repository is disposable build output; the monorepo remains the
  authoritative source.
