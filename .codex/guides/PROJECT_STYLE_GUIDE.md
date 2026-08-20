# ECO 230 Project Style Guide

This is the source of truth for creating and updating ECO 230 project prompts,
timelines, presentation requirements, deliverable instructions, and rubrics. It
standardizes the project document family so students encounter one vocabulary,
one set of deliverable names, and one authoritative version of each requirement.

## Communication job

By the end of a project document, an ECO 230 team should understand the decision
problem, the work products required at each stage, who is responsible, when and
how to submit, and how the work will be evaluated.

Project documents should support sustained work across multiple weeks. They must
be precise enough to coordinate a team and stable enough that ancillary pages do
not create conflicting requirements.

## Scope

This guide applies to:

- project prompts in `syllabus/project_N_prompt.qmd`;
- project timelines;
- presentation and coaching-session requirements;
- project rubrics and grading explanations;
- project-related homework and preparation tasks;
- project references in the syllabus and weekly slides.

Weekly preparation assignments must also follow `ASSIGNMENT_STYLE_GUIDE.md`.
Student presentation advice should agree with `SLIDE_STYLE_GUIDE.md`.

## Project document family

Each project should have a small set of documents with nonoverlapping authority.

| Document | Owns | Must not own |
|---|---|---|
| Prompt | Purpose, scenario, audience, scope, deliverables, requirements, policies | Section-specific dates duplicated from the timeline; full rubric descriptors duplicated from the rubric |
| Timeline | Milestones, due dates, meeting dates, submission sequence | New deliverables or grading rules |
| Presentation guide | Oral delivery, visual aid, audience, timing, rehearsal, presentation submission | Written-report requirements or contradictory grading language |
| Rubric | Criteria, point values, performance descriptors, individual/team scoring | New deliverables not named in the prompt |
| Syllabus summary | Short description, overall value, canonical links | A second complete copy of the project instructions |

When information changes, update the document that owns it and link to that
document elsewhere. Do not maintain several prose copies of the same deadline,
filename, or grading rule.

## File and title conventions

Use lowercase snake_case filenames:

```text
syllabus/project_1_prompt.qmd
syllabus/project_1_timeline.qmd
syllabus/project_1_presentation_in_person.qmd
syllabus/project_1_rubric.qmd
syllabus/project_2_prompt.qmd
syllabus/project_2_rubric.qmd
```

Use title case in visible titles:

```text
Project 1: Data Analysis Project
Project 1: Timeline
Project 1: In-Person Presentation
Project 1: Rubric
Project 2: Can We Trust It?
Project 2: Rubric
```

Do not use all caps, an en dash as a title separator, or a generic `Group
Project` title when the project number is known. Do not repeat the YAML title as
an all-caps body heading.

## Default document contract

Use this front matter for project prompts, presentation guides, and rubrics:

```yaml
---
title: "Project N: Document Title"
format:
  html:
    toc: true
    toc-depth: 2
    toc-location: right
    number-sections: false
    anchor-sections: true
    smooth-scroll: true
execute:
  echo: false
  warning: false
  message: false
  freeze: auto
---
```

A short timeline page may disable the TOC. If it uses section-specific dates, it
must retain a `class_section` parameter and derive dates from the syllabus data
rather than hardcoding them.

Use a subtitle only when it adds information not already present in the title.
`Project Prompt`, `Timeline`, and `Presentation Guidelines` are usually clearer
as part of the title.

## Standard project prompt sequence

Use the following order for a main project prompt.

### 1. Overview

In two or three paragraphs, state:

- the authentic problem or opportunity;
- the decision maker or audience;
- the project's central analytical job;
- the form of the final recommendation, presentation, or proposal.

Do not begin with software requirements. Lead with the reason the analysis
matters.

### 2. Learning objectives

Use observable course-level outcomes. Project objectives should integrate
several skills and should not simply repeat weekly homework objectives.

### 3. Scenario and audience

Name who needs the work and what decision it should inform. When teams may choose
their own topic, require them to identify a plausible stakeholder and decision.

### 4. Deliverables at a glance

Use a summary table near the beginning:

| Deliverable | Team or individual | Required format | Due-date source | Points |
|---|---|---|---|---:|
| Analysis plan | Team | Quarto/R Markdown or provided template | Project timeline | 0 |
| Presentation | Team | PowerPoint or Google Slides | Project timeline | 00 |
| Technical documentation | Team | `.qmd`, `.Rmd`, or `.R` plus rendered output | Project timeline | 00 |

Use the project's real values; the example values above are placeholders. The
same deliverable names must appear in the prompt, timeline, submission section,
rubric, syllabus, and Canvas.

### 5. Team expectations

State:

- expected team size or formation process;
- whether students may divide analytical roles;
- what every teammate remains responsible for understanding;
- how attendance, coaching, peer evaluation, and individual adjustments affect
  grades;
- how teams should handle missing or unresponsive members.

Use `team` for the people working together and `teammate` for an individual. Use
`group number` only for the identifier assigned in Canvas or class.

### 6. Required work

Organize requirements by deliverable or stage, not as one long mixed list. Begin
each requirement with a verb. Separate analytical requirements, communication
requirements, technical documentation, and submission logistics.

### 7. Resources and constraints

List provided datasets, templates, software, required functions, optional tools,
and prohibited methods. Distinguish `required`, `permitted`, and `optional`.

### 8. Submission and filenames

Identify where every deliverable is submitted and whether one teammate or every
teammate submits. Use the filename convention below when filenames matter.

### 9. Evaluation

Summarize the points and link to the canonical rubric. Explain team and
individual scoring once. Do not reproduce the full rubric in the prompt unless
the prompt itself is the canonical rubric document.

### 10. Final checklist

End with a concise checklist organized by deliverable. The checklist must not
introduce new requirements.

## Timeline standard

The timeline is the authoritative source for project dates.

- Use the exact deliverable names from the prompt.
- Derive section-specific dates through `params$class_section` and syllabus CSVs.
- Do not hardcode semester dates or final presentation times in prose.
- Week numbers may be used because they are date agnostic.
- Distinguish `work begins`, `coaching/checkpoint`, and `due`.
- State whether each milestone is submitted, brought to class, or discussed only.
- Keep dates in one consistent display format, preferably `Month D, YYYY`.
- When a deadline is in Canvas, the rendered timeline and Canvas must agree.

Use a table with these columns when practical:

| Stage | What the team should have ready | Due or meeting date | Submission |
|---|---|---|---|

Do not add grading penalties in the timeline unless the prompt and rubric already
define them.

## Deliverable naming

Choose one exact name for every deliverable and reuse it verbatim. Preferred
forms include:

- Analysis Plan
- Clean Dataset
- Pre-Coaching Presentation
- In-Person Presentation
- Presentation Materials
- Technical Documentation
- Written Research Proposal
- Red Team Critique
- Peer Evaluation

Use `clean dataset`, not `clean data set`. Use `technical documentation` rather
than alternating among `formal documentation`, `R documentation`, and `work
files` unless those are distinct deliverables.

If two files serve one deliverable, name the deliverable once and list its
required files beneath it.

## Submission filename convention

Use filenames that sort predictably and avoid spaces:

```text
ECO230_Project01_GroupX_Deliverable.ext
```

Examples:

```text
ECO230_Project01_Group4_Presentation.pptx
ECO230_Project01_Group4_TechnicalDocumentation.pdf
ECO230_Project02_Group4_ResearchProposal.pdf
```

The project number in the filename must match the document. Do not reuse a
filename from a different project. If Canvas renames files and a filename is not
needed, omit the requirement rather than enforcing a decorative convention.

## Requirement language

Use requirement words consistently:

- `must` or `required`: evaluated requirement;
- `should`: strong quality recommendation;
- `may`: permitted choice;
- `optional`: not required and not penalized;
- `do not`: prohibited action.

Avoid `encouraged` when the rubric rewards the behavior. If it affects scoring,
state how. Avoid vague phrases such as `make good use of your time`, `pretty good
understanding`, or `as required by your instructor`; replace them with an
observable checkpoint or a canonical link.

Do not use a late penalty, deduction, grade multiplier, or individual adjustment
without stating the calculation clearly and consistently in the prompt, rubric,
and syllabus policy.

## Tone and sentence style

- Address students as `you` and teams as `your team`.
- Use direct verbs and short paragraphs.
- Prefer `Your team will submit...` over `Submission of ... will occur...`.
- Use a professional, supportive tone without jokes in grading or integrity
  language.
- Explain the reason for a constraint when that reason helps teams make better
  decisions.
- Avoid repeating the same requirement in bold, a callout, prose, and a list.

Use `business audience`, `decision maker`, or a named stakeholder consistently.
Do not alternate among `reader`, `client`, `audience`, and `organization` unless
they refer to different people.

## Course terminology

Use the assignment-guide conventions plus these project-specific forms:

| Use | Avoid |
|---|---|
| Project 1 | Project One, group project when the number matters |
| team, teammate | group, group member except for the assigned group number |
| dataset | data set |
| clean dataset | clean data set |
| analysis plan | analysis-plan document unless used as a modifier |
| technical documentation | formal documentation of work, R documentation when referring to the same deliverable |
| R script | RScript |
| R Markdown | RMarkdown |
| Posit Cloud | posit.cloud, Posit.cloud |
| in-person presentation | in person presentation |
| decision maker | decision-maker when used as a noun |

Use `PowerPoint`, `Google Slides`, `Excel`, `Tableau`, `Quarto`, and `R` with
their standard capitalization. Put literal filenames, extensions, functions,
and object names in code font.

## Analysis and evidence language

Project prompts should consistently distinguish:

- the business or research question;
- the statistical story or hypothesis;
- descriptive evidence;
- inferential evidence and uncertainty;
- practical importance;
- limitations and generalizability;
- recommendation or decision implication.

Do not require students to paste raw statistical output into a presentation.
Require polished evidence and a plain-language interpretation. Technical output,
code, assumptions, and reproducibility details belong in technical documentation
or an appendix when appropriate.

For a research proposal, distinguish what the team will actually do from what it
is proposing. Use future or conditional language accurately: `The proposed study
would sample...`, not `The study sampled...`.

## Presentation requirements

The presentation guide should state:

- audience and decision context;
- total time and whether questions count toward it;
- whether every teammate must speak;
- submission location, deadline, format, and filename;
- required narrative elements;
- visual aid expectations;
- technical appendix expectations;
- rehearsal and accessibility checks;
- the linked rubric.

Use the slide style guide's principles when describing quality: takeaway-first
titles, one message per slide, readable visuals, restrained text, consistent
color, and a closing synthesis or action. Do not require decorative templates or
software-specific effects.

All timing language must be exact. Use `8 minutes plus 2 minutes for questions`,
not `about 8-10 minutes`, if timing affects evaluation.

## Technical documentation requirements

State the minimum reproducibility standard:

- source data or a stable reference to it;
- import and cleaning steps;
- derived variables and filters;
- descriptive and inferential analyses;
- code annotations that explain nonobvious decisions;
- session/package information when version differences matter;
- rendered output when required;
- a clear file map if the analysis is split across several source files.

Do not prescribe stylistic code rules as grading criteria unless they support
readability or reproducibility and appear in the rubric. Use `.R`, `.Rmd`, and
`.qmd` accurately; do not refer to all three as R scripts.

## Rubric standard

### Alignment

Every rubric criterion must map to a named deliverable requirement. Every point
in the rubric must be traceable to a criterion, and the points must sum exactly
to the project total.

### Performance levels

Use one set of performance-level names throughout a project. The default is:

- Professional Quality
- Meets Expectations
- Needs Revision
- Missing or Unacceptable

Five levels are acceptable when the distinctions are meaningful, but do not mix
four- and five-level systems within one project.

### Point ranges

Ranges must be mutually exclusive and collectively cover every possible score.
Do not use overlapping boundaries such as `10-8` followed by `8-5`. Use either:

```text
10-9, 8-6, 5-1, 0
```

or exact points for each level. Match the range to the criterion's maximum.

### Descriptors

Write parallel, observable descriptors. Each level should address the same
features in the same order. Describe the submission, not the student.

Good descriptor features include:

- completeness;
- analytical fit;
- accuracy;
- evidence and reasoning;
- audience adaptation;
- limitations;
- reproducibility;
- professional communication.

Avoid vague labels such as `high quality with minor issues` unless the descriptor
defines which issues matter. Avoid grading `ingenuity`, `independence`, or
`effort` without observable evidence and a stated scoring rule.

### Team and individual grading

State the calculation explicitly. If a team score is adjusted by peer
evaluation, provide or link to the formula, allowable range, and treatment of a
missing peer evaluation. Do not say grades may move by `up to two letter grades`
without the decision rule.

### Rubric location

Prefer one rubric file per project when the rubric is substantial. The prompt
should summarize point categories and link to the rubric. This prevents the
prompt, presentation guide, and Canvas rubric from drifting apart.

## Callouts and formatting

Use native Quarto callouts consistently:

- note: project purpose or neutral context;
- important: deliverable or submission requirement;
- tip: planning or collaboration advice;
- warning: prohibited action, integrity rule, or consequential deadline;
- caution: risk, dependency, or condition.

Prefer minimal callouts and use no more than one callout per short section. Do
not repeat a heading in bold uppercase immediately below the actual heading.

Use tables for deliverable summaries, timelines, point allocations, and repeated
rubric structures. Use prose for rationale and nuanced guidance. Keep generated
tables readable in both HTML and PDF when PDF output is supported.

## Dates and section-specific information

Do not hardcode section-specific dates, presentation times, coaching times, or
final exam details in project prompts. Retrieve them through the same section
workflow used by the syllabus:

- select the section with `params$class_section`;
- read section metadata from `syllabus/data/sections.csv`;
- filter `section_*.csv` data to the selected section;
- remove the `section` column before display.

Week-number references are acceptable. If a project document cannot resolve a
date, link to the canonical timeline rather than inventing a fallback date.

## Consistency matrix

Before publishing a project, compare these fields across every related page:

| Field | Prompt | Timeline | Presentation guide | Rubric | Syllabus/Canvas |
|---|---|---|---|---|---|
| Project number and title | Same | Same | Same | Same | Same |
| Deliverable names | Canonical | Exact match | Relevant match | Exact match | Exact match |
| Team vs individual | Defined | Shown per stage | Defined for presenters | Scoring matches | Same |
| Due dates | Link only | Canonical | Link or exact derived date | No duplicate unless needed | Same |
| Submission format | Defined | Short reminder | Presentation files only | Evaluates named file | Same |
| Points | Summary | Optional | No conflicting total | Canonical | Same |
| Filename | Canonical | Optional reminder | Exact match | Not repeated | Same |
| AI/integrity policy | Canonical | No duplicate | Relevant reminder | Evaluates only if stated | Same |

## Current-material audit

| Material | Strength to preserve | Standardization need |
|---|---|---|
| Project 1 prompt | Comprehensive deliverables, audience emphasis, technical documentation, and detailed rubrics | Replace all-caps title/bold duplicates, normalize `dataset`/R terminology, reduce repeated requirements, clarify team vs individual calculation, and move substantial rubrics to a canonical rubric file. |
| Project 1 timeline | Section-aware dates and concise table generation | Keep parameterized; synchronize deliverable names with the prompt and distinguish preparation dates from due dates. |
| Project 1 presentation guide | Strong audience, narrative, visual-aid, rehearsal, and checklist guidance | Correct the filename example that says `Project 3`; normalize the title, remove repeated bold-uppercase labels, and link one canonical presentation rubric. |
| Project 2 prompt | Clear problem-design-trustworthiness arc, deliverable summary, and 100-point allocation | Enable a useful TOC, remove the duplicate all-caps body title, add explicit submission formats/filenames, and separate canonical dates from week-level pacing. |
| Project 2 rubric | Explicit criteria and a visible 100-point total | Fix every shared endpoint in the overlapping score bands; use parallel performance-level names and descriptors. |

## Standardization priorities

1. Correct the Project 1 presentation filename that names the wrong project.
2. Fix Project 2 rubric ranges so no score belongs to two levels.
3. Establish exact deliverable names and synchronize them across prompts,
   timelines, rubrics, the syllabus, and Canvas.
4. Normalize project front matter and visible titles.
5. Separate Project 1's rubric into a canonical rubric file or clearly designate
   the prompt as the only rubric source.
6. Standardize team, dataset, technical documentation, and software terminology.
7. Remove duplicated headings, requirements, and grading statements.
8. Make team/individual grading adjustments explicit and reproducible.

## Authoring checklist

Before editing:

- Read this guide plus the assignment and slide guides when relevant.
- Identify the document that owns the requirement being changed.
- Inspect all related project files and the syllabus reference.
- Check for section-specific dates and existing user changes.

While editing:

- Use canonical titles, deliverable names, and terminology.
- State requirements once and link to the authoritative page elsewhere.
- Use direct team-facing language and observable expectations.
- Keep dates parameterized and point totals explicit.
- Make rubric ranges nonoverlapping and descriptors parallel.
- Preserve render-safe Quarto fences, chunks, and table helpers.

Before delivery:

- Render every changed project document for at least one section when parameters
  are involved.
- Verify all links, paths, tables, and generated dates.
- Run the consistency matrix across the prompt, timeline, presentation guide,
  rubric, syllabus, and Canvas-facing language in the repository.
- Confirm that point totals sum, ranges do not overlap, and every criterion maps
  to a requirement.
- Check for wrong project numbers, stale filenames, duplicated headings,
  hardcoded dates, and inconsistent deliverable names.
- Run `git status --short` and report only the intentional changes.
