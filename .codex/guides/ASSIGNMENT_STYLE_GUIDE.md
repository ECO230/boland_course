# ECO 230 Assignment Style Guide

This is the source of truth for creating and updating ECO 230 homework,
practice assignments, interpretation prompts, and practicum materials. Use the
slide style guide for Reveal.js presentation mechanics; use this guide for
assignment structure, instructions, terminology, and student-facing language.

## Communication job

By the end of an assignment prompt, an ECO 230 student should know why the work
matters, exactly what to do, what to submit, and how success will be judged.

Assignment prompts should reduce avoidable uncertainty without removing the
productive decisions students are expected to make. Be precise about logistics
and flexible only where analytical judgment is part of the learning goal.

## Scope

This guide applies to:

- weekly homework in `weekXX/Homework_NN.qmd`;
- practice activities that students complete or submit;
- practicum prompts and guidelines in `admin/final/`;
- the wording of statistical interpretation exercises;
- companion interpretation decks when they repeat assignment prompts or
  statistical terminology.

Interpretation decks remain Reveal.js slides and must also follow
`SLIDE_STYLE_GUIDE.md`.

## Default document contract

Use this front matter for a new homework document unless there is a documented
reason to deviate:

```yaml
---
title: "Homework N: Topic"
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
---
```

Number task headings manually and keep `number-sections: false`; automatic
numbering combined with headings such as `## 1) ...` produces duplicate numbers.
Do not add `fontsize` to HTML front matter. Show code with chunk-level
`echo: true` only when students are expected to read, copy, or modify that code.

For short reference pages with fewer than four sections, the table of contents
may be disabled. Practicum pages should use the same HTML structure but may use
`toc-depth: 3` when the exam has multiple nested requirements.

Standalone assignments distributed through Posit Cloud use Typst PDF output
instead of the website HTML contract. Follow
`POSIT_CLOUD_ASSIGNMENT_WORKFLOW.md`, including its compact 10 pt, US-letter,
one-inch-margin Typst profile and manifest-based distribution rules.

## Standard assignment sequence

Use these sections in this order. Omit a section only when it truly does not
apply.

### 1. Purpose

Open with a minimal note callout that connects the assignment to the week's
central idea and describes the finished work:

```markdown
::: {.callout-note appearance="minimal" icon="false" title="Purpose"}
This assignment helps you practice ...

You will produce ...
:::
```

The purpose is not a list of instructions. In two short paragraphs, answer:

- Why are students doing this now?
- What will they create, analyze, or explain?

### 2. Objectives

Use three to six observable verbs. Prefer `identify`, `calculate`, `compare`,
`select`, `interpret`, `justify`, `create`, and `revise`. Avoid vague verbs such
as `understand`, `learn about`, or `be familiar with` unless the objective also
states observable evidence.

### 3. Resources

List only resources students are expected to use. Use descriptive Markdown links
instead of exposed URLs. Name course locations consistently:

- Canvas;
- Posit Cloud;
- the relevant weekly slides or interpretation guide;
- provided `.csv` files or the student's project dataset;
- named functions from `eco230r`.

### 4. Expectations or grading basis

Use a minimal tip callout for the assignment's overall standard. State whether
the work is graded for completion, accuracy, analytical judgment, or
professional quality. Do not use reassuring language that contradicts the
rubric or submission requirements.

### 5. Numbered tasks

Use level-two headings in this form:

```markdown
## 1) Choose Your Dataset

## 2) Create the Summary Table
```

Each task should contain, in order:

1. the action;
2. any constraints or required method;
3. the required response or artifact;
4. a short example or hint only when it prevents a common misunderstanding.

Separate top-level tasks with one thematic break (`---`). Do not use thematic
breaks inside code blocks or between every short paragraph.

### 6. Submission

Every graded assignment needs an explicit `## Submission` section, even when
submission instructions also appear in Canvas. State:

- where to submit;
- every required file or response;
- accepted file formats;
- whether source files are required;
- whether students need the file in class;
- any required filename convention.

Do not use `and/or` when the actual requirement can be stated with `required` and
`optional` lists.

### 7. Checklist

Use a short checkbox list for multi-part assignments. Each item should correspond
to a required deliverable or a high-value quality check. A checklist cannot add
requirements that are absent from the task instructions.

### 8. Grading focus or course alignment

End with either a concise grading-focus section or course-outcome alignment.
Describe evidence that will be evaluated, not personality traits or effort that
cannot be observed in the submission.

## Instruction-writing rules

### Use direct student-facing language

Address the student as `you`. Begin steps with verbs. Prefer:

- `Create a grouped summary table.`
- `Explain why the denominator is appropriate.`
- `Upload the PDF and the source file to Canvas.`

Avoid passive or indirect versions such as `A grouped summary table should be
created` or `Students will likely need to`.

### Distinguish requirements from advice

Use modal verbs consistently:

- `must` or `required`: graded requirement;
- `should`: strong quality recommendation;
- `may`: permitted option;
- `do not`: prohibition;
- `optional`: not required and not penalized.

Do not use bold alone to turn a suggestion into a requirement. If an item affects
the grade, state that directly.

### Specify the response

When asking for writing, state the expected form and scope:

- number of sentences or maximum words;
- bullets, paragraph, table, figure, or code;
- required evidence or concepts;
- intended audience when interpretation is required.

Use ranges only when they help students calibrate scope. Avoid arbitrary word
counts for analytical work that is better constrained by required components.

### Examples and models

Label examples clearly as `Example`, `Strong example`, or `Weak example`. Explain
the feature students should notice. Do not provide a model answer so close to the
assigned question that the task becomes transcription.

### Hints

Place a hint immediately after the task it supports. A hint should identify a
decision point or common error, not introduce a new requirement.

## Formatting conventions

### Titles and headings

- YAML title: `Homework N: Topic` in title case.
- Do not repeat the YAML title as an all-caps body heading.
- Level two: main task or document section.
- Level three: part of a task or one of several hypotheses.
- Level four: use sparingly for a repeated response field.
- Use sentence case for explanatory headings and title case only for short named
  document sections.

Avoid all-caps headings such as `HOMEWORK 8`, `SUBMISSION REQUIREMENTS`, and
`NOTES`. Use `Homework 8: ANOVA and Simple Linear Regression`, `Submission`, and
`Guidance` instead.

### Callouts

Use Quarto callout syntax with an explicit semantic class and title:

- `callout-note`: purpose, resources, neutral context;
- `callout-important`: submission requirement or non-negotiable instruction;
- `callout-tip`: strategy, quality advice, or troubleshooting;
- `callout-warning`: invalid method, prohibited action, or academic-integrity
  restriction;
- `callout-caution`: condition that may change the correct procedure.

Prefer `appearance="minimal"` and `icon="false"` for assignment pages. Do not mix
Quarto callouts with Bootstrap classes such as `.alert` and `.alert-info`.

### Lists

Use numbered lists for procedures and bullets for nonsequential choices or
criteria. Do not end every list item with a forced Markdown line break (`\`);
normal list spacing is more robust and easier to edit.

### Tables

Use a table when students must compare repeated fields, submission components,
point values, or permitted options. Keep prose outside the table when it applies
to the entire assignment.

### Code and output

- Put file extensions, object names, function names, and variables in code font.
- Use executable chunks only when the rendered result belongs in the prompt.
- Use `eval: false` for pseudocode or intentionally incomplete student code.
- Use `echo: true` at the chunk level for code students need to see.
- Hide setup and package-loading chunks.
- Never place a second YAML document block inside the body of a `.qmd` file.
- Ensure incomplete code cannot prevent the assignment page from rendering.

### Links and paths

Use descriptive links such as `[Open the Homework 5 project in Posit Cloud](...)`.
Do not expose a raw URL as a numbered instruction. Use repository-relative paths
for local resources and verify them during rendering.

## Naming and terminology

### Files and visible titles

Use the existing repository pattern:

- file: `weekXX/Homework_NN.qmd`;
- title: `Homework N: Topic`;
- interpretation resource: `Interpreting [Method]`;
- practicum files may retain their existing filenames, but their visible YAML
  titles should distinguish `Practice Practicum`, `Practicum Guidelines`, and
  `Final Practicum`.

If a submitted filename is required, use one course-wide pattern:

```text
ECO230_HWNN_LastName.ext
```

Do not invent a filename requirement when Canvas already renames files and no
instructional need exists.

When separate standalone versions exist for different delivery modes, use
`IP` for in person and `OA` for online asynchronous. Use lowercase kebab-case
for generated repository names (`homework-06-ip`) and append the uppercase
suffix to student project files (`Homework_06_IP.qmd`, `Homework_06_IP.Rproj`,
and `Homework_06_IP.pdf`). Keep the visible title in the standard
`Homework N: Topic` form and write the delivery mode out in a subtitle such as
`In-Person Sections` or `Online Asynchronous`.

### Course tools and file types

Use these forms consistently:

| Use | Avoid |
|---|---|
| Canvas | canvas |
| Posit Cloud | posit.cloud, Posit.cloud |
| R | r |
| R script | RScript, R Script |
| R Markdown | RMarkdown |
| Quarto document | QMD document |
| Excel workbook | Excel Workbook |
| Tableau packaged workbook | Tableau Packaged Workbook |
| PDF | .pdf in prose |
| dataset | data set |
| filename | file name |

Use code font when referring to a literal extension: `.csv`, `.xlsx`, `.twbx`,
`.R`, `.Rmd`, `.qmd`, `.html`, or `.pdf`.

### Statistical terminology

Use these standard forms:

- chi-square test, chi-square analysis, chi-square goodness-of-fit test, and
  chi-square test of independence;
- t-test, independent-samples t-test, paired-samples t-test, and one-sample
  t-test;
- Wilcoxon rank-sum test and Wilcoxon signed-rank test;
- one-way ANOVA and simple linear regression;
- null hypothesis and alternative hypothesis, not alternate hypothesis;
- H0 and HA in prose, or `$H_0$` and `$H_A$` in math;
- p-value, alpha level, confidence interval, effect size, and Bayes factor;
- statistical interpretation and practical interpretation.

Do not use `significant` without specifying whether it means statistical
evidence or practical importance. A practical interpretation should translate
the estimate, uncertainty, effect size, and evidence into consequences for the
stated audience.

### Numbers and punctuation

- Spell out one through nine in prose unless the number is a measurement, point
  value, step number, or required quantity.
- Use numerals in requirements: `Create 3 visuals` is easier to scan than
  `Create three visuals` when quantity is being checked.
- Use a hyphen for compound modifiers: `five-step process`, `in-class activity`,
  `decision-focused interpretation`.
- Use an en dash only when the existing file consistently uses Unicode; new
  generated text should remain ASCII, so use a hyphen for numeric ranges.

## Standard five-step hypothesis template

Homeworks 5-8 and the interpretation resources should use the same wording and
order every time:

1. **Statistical story:** What relationship, difference, or pattern are you
   investigating, and why might it matter?
2. **Estimate or plot:** What does the descriptive evidence suggest? If there
   were no uncertainty, what action would the business take?
3. **Test and hypotheses:** Which test is appropriate? State `$H_0$`, `$H_A$`,
   and the alpha level when required.
4. **Statistical interpretation:** What does the result say about the evidence
   against `$H_0$`? Report the relevant estimate and uncertainty in context.
5. **Practical interpretation:** How large and consequential is the result for a
   general business audience? Use the effect size and Bayes factor when they help
   support the conclusion.

For repeated hypothesis exercises, use these labels consistently:

- `Hypothesis 1 - Worked Example`
- `Hypothesis 2 - Complete Steps 4-5`
- `Hypothesis 3 - Complete the Full Analysis`

Use `Worked Example`, not `My Interpretation`; use `Complete`, not `You Attempt`.
Place `*Your response:*` below an open response field rather than repeating
labels such as `Statistical Interpretation:` twice.

## Responsible AI language

Every assignment that addresses AI must identify one policy explicitly. Do not
leave students to infer whether AI is permitted.

### Permitted with disclosure

Use this default for weekly homework when appropriate:

> Generative AI may help with clarification, rewriting, or troubleshooting. It
> may not make analytical decisions or interpret results for you. You are
> responsible for verifying every claim and submitting work you understand. If
> the assignment requests an AI reflection, disclose how you used the tool and
> where its response was incomplete or incorrect.

### Limited use

State the exact permitted tasks, such as syntax troubleshooting, and the exact
prohibited tasks, such as generating interpretations.

### Not permitted

Use direct language for exams and individual assessments:

> Generative AI is not permitted for this assessment. Do not use an AI tool to
> generate, revise, interpret, or troubleshoot any part of your submission.

Academic-integrity restrictions belong in a warning callout and should match the
syllabus and Canvas instructions exactly.

## Practicum and exam prompts

Practice and final practicum documents should use parallel structures so the
practice document teaches students how to read the final prompt:

1. purpose and assessment conditions;
2. scenario and decision maker;
3. dataset and unit of analysis;
4. available tools and allowed resources;
5. required analysis and deliverables;
6. submission files;
7. grading criteria;
8. academic-integrity and AI policy.

State `required`, `optional`, and `not permitted` tools in separate lists. Avoid
phrases such as `Excel and/or Tableau` when one tool is required and the other is
optional. Use the same product capitalization and file-format language as the
weekly assignments.

## Current-material audit

| Material | Strength to preserve | Standardization need |
|---|---|---|
| Homeworks 1-3 | Clear purpose, objectives, resources, numbered tasks, submission, and course alignment | Normalize TOC settings, remove forced line breaks, and standardize Posit Cloud and filename language. |
| Homework 4 | Strong examples, planning scaffold, and checklist | Add an explicit submission section and align its project terminology with the project guide. |
| Homework 5 | Clear focus on test selection and pseudocode | Remove the duplicate all-caps title, use the standard document structure, make the Posit Cloud URL descriptive, and add a final checklist/grading focus. |
| Homeworks 6-7 | Useful worked-example / partial-completion / full-analysis progression | Use `chi-square`, correct `your group's`, adopt the canonical five-step wording, and add explicit submission sections. |
| Homework 8 | Useful ANOVA/regression practice and practical-interpretation emphasis | Highest-priority cleanup: remove the second YAML block, duplicate titles/setup chunks, Bootstrap alerts, and mixed callout syntax; then apply the canonical five-step template. |
| Interpretation decks | Consistent five-step teaching sequence and reusable method explanations | Follow the slide guide visually and use the same statistical terminology and prompt wording as homework. |
| Practicum materials | Parallel scenario/data/task/submission/rubric content | Add YAML titles, remove duplicate all-caps headings, clarify required vs optional tools, standardize AI policy, and modernize R Markdown/PDF terminology. |

## Standardization priorities

1. Repair Homework 8's duplicate document scaffold and mixed callout systems.
2. Apply the canonical five-step hypothesis template to Homeworks 5-8 and the
   interpretation resources.
3. Normalize homework front matter and remove duplicate body titles.
4. Add explicit submission sections and checklists where missing.
5. Standardize statistical terminology, product names, and file types.
6. Align the three practicum documents so requirements and allowed tools cannot
   contradict one another.
7. Replace repeated forced line breaks and legacy Bootstrap markup with native
   Quarto structure.

## Authoring checklist

Before editing:

- Read this guide and the nearest assignment with the same learning goal.
- Confirm the current syllabus, Canvas policy, and permitted AI policy.
- Check whether dates or section-specific requirements must come from data.

While editing:

- Use the standard front matter and document sequence.
- State every requirement once in the task and repeat it only in the final
  checklist.
- Use direct verbs, observable objectives, and explicit response formats.
- Keep examples clearly separate from student response fields.
- Use canonical statistical and product terminology.
- Keep code chunks render-safe and hide setup code.

Before delivery:

- Render the assignment explicitly.
- Test every link, local path, code chunk, image, and table.
- Verify that the title, task numbers, TOC, and checklist agree.
- Verify that submission files, filename rules, and grading criteria agree.
- Check for duplicate YAML, duplicate headings, raw URLs, forced line breaks,
  mojibake, and unclosed Quarto div fences.
- Run `git status --short` and report only the intentional changes.
