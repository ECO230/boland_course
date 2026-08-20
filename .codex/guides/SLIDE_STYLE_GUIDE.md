# ECO 230 Slide Style Guide

This is the source of truth for creating and updating ECO 230 Quarto/Reveal.js
slides. It reflects the recurring visual language in the weekly decks currently
in the repository and distinguishes stable course-wide conventions from
week-specific teaching visuals.

## Communication job

By the end of a class deck, ECO 230 students should be able to explain the
week's central statistical idea and apply it to a business question because the
slides connect a concrete example, a visual explanation, and an actionable
interpretation.

The slides teach; they are not a written textbook. Put supporting detail,
facilitation prompts, and reminders in speaker notes rather than on the canvas.

## Default deck contract

Use the following front matter for new weekly decks unless the content requires
a documented exception:

```yaml
---
title: "Week XX - Topic"
author: "Mike Boland, MBA"
format:
  revealjs:
    theme:
      - default
      - ../shared/styles/boland-reveal.scss
    slide-number: true
    hash: true
    center: false
    transition: fade
    controls: true
    pdf-export: true
    toc: false
execute:
  echo: false
  warning: false
  message: false
---
```

Add a local CSS file only when the deck contains bespoke diagrams or a repeated
visual component. Do not use `margin: 0`, `self-contained`, or other deck-wide
overrides by habit; add them only when the deck needs them.

Use `#` for section dividers and `##` for content slides. A deck may open with a
single `#` question or claim before the first section divider.

## Visual identity

### Character

The established ECO 230 style is clean, direct, lightly playful, and built for
classroom projection:

- White is the default canvas.
- Black or near-black headings carry the hierarchy.
- Images, charts, and teaching diagrams do most of the explanatory work.
- Callouts emphasize one definition, question, or decision rather than decorate
  every slide.
- Humor and surprising images are welcome when they clarify or reset attention.
- Layouts are flat and editorial. Avoid dashboard-like grids of decorative
  cards, pills, tabs, and controls.

### Color

Use the Office-derived course palette consistently:

| Role | Color | Use |
|---|---:|---|
| Body text | `#222222` | Paragraphs, lists, table text |
| Heading | `#000000` | Slide and section titles |
| Navy | `#1f497d` | Accessible accent text, key labels |
| Blue | `#4f81bd` | Links, borders, lines, neutral emphasis |
| Red | `#c0504d` | Warnings, contrast, incorrect/negative state |
| Green | `#9bbb59` | Positive state, selection, visual encoding |
| Purple | `#8064a2` | Secondary category or model |
| Light gray | `#f7f7f7` | Table headers, code and subtle panels |

Navy may be used as text on white. The brighter green should not be used for
small text on white; reserve it for fills, borders, marks, or text on a dark
background. Color must not be the only carrier of meaning: pair it with a label,
shape, position, line style, or symbol.

### Typography

The shared theme's system sans-serif stack is the standard. Do not introduce a
week-specific font unless an imported visual requires it.

- Body copy starts at the theme root size of `24px`.
- Section titles use the largest hierarchy; content-slide titles are smaller but
  always visibly dominant.
- Use bold for the one phrase a student should remember, not for whole blocks.
- Use italics for questions, hypotheses, or a brief spoken aside.
- Essential labels should normally be at least `18px`; captions may be `16px`.
  Existing `13px` labels are legacy exceptions, not a model for new work.
- Shorten copy or change the composition before shrinking text.

The current `.muted` treatment is too faint for essential information. Until it
is corrected in the theme, use `.muted` only for deliberately de-emphasized
content that is also stated elsewhere. Do not put required instructions or key
contrasts in `.muted`.

### Spacing and alignment

- Keep content within the shared theme's `1000px` reading width by default.
- Left-align titles and most explanatory text.
- Center only a short question, quotation, single statistic, or intentional
  visual focal point.
- Use consistent left and right margins. Full-bleed compositions must be a
  deliberate slide-level exception.
- Prefer natural document flow, Quarto columns, CSS Grid, or Flexbox over
  absolute positioning.
- Absolute positioning is acceptable for a bespoke teaching diagram when the
  spatial relationship is part of the explanation.

## Slide grammar

### Opening slide

Open with a question, tension, surprising conclusion, or concrete scenario. The
first slide should establish why the topic matters, not reproduce the YAML title
and author metadata.

Good patterns already in the course include "Can you trust this conclusion?"
and "Can a sample tell the truth?"

### Section divider

Use a level-one heading for a major conceptual turn. Keep it short. A section
divider may use a restrained background visual, but it should not become a
second content slide.

### Explanation slide

Give each slide one job. Prefer a title that states the question or takeaway
instead of a generic topic label. Pair the title with one of:

- a short explanation and one callout;
- a chart or table with an interpretation;
- a two-column comparison;
- a teaching diagram;
- an example followed by a decision or implication.

### Image slide

For photographs, use `background-size="cover"` when cropping is acceptable. For
charts, screenshots, maps, or diagrams, use `background-size="contain"` on a
white background.

Do not place text directly over a busy image without a readable overlay. Use a
short caption panel or reserve quiet space in the image. Image-only slides still
need a meaningful hidden heading and an accessible text description; a filename
or empty heading is not sufficient.

### Comparison slide

Use two columns for an actual contrast, not merely to fill space. Keep equivalent
content aligned and use the same order of information on both sides. Add a
visible divider only if alignment alone does not make the comparison clear.

### Activity slide

State the action first, then the time or constraints, then the expected output.
Use speaker notes for facilitation details. Leave the activity prompt visible
long enough to function without narration.

### Closing slide

Resolve the opening question with a synthesis, application, or exit question.
Do not end on an implementation detail or generic "Thank you."

## Content components

### Callouts

Use Quarto callouts for definitions, the day's question, warnings, and a single
big idea. Choose the semantic type deliberately:

- `callout-note`: framing question, definition, or neutral key idea;
- `callout-important`: decision rule, prime directive, required action;
- `callout-tip`: method, practical advice, or reusable heuristic;
- `callout-warning` / `callout-caution`: common error or invalid conclusion.

Limit most slides to one callout. Titles should be short and informative.

### Lists

Use normal bullets for distinct points and numbered lists for sequences. Use
`.tight-list` only when the items are short and the grouping is more important
than each bullet. Do not use repeated `<br>` tags as the primary layout system;
prefer CSS spacing on a wrapper or a more suitable composition.

### Tables

Plain Markdown tables use the shared theme. `gt` tables are intentionally exempt
from the global table selector and should use `shared/scripts/gt_boland.R`.

- Keep only columns that support the slide's claim.
- Use readable labels rather than raw variable names.
- Align comparable values and round consistently.
- Highlight the one row, column, or comparison being discussed.
- If the full table is the activity, reveal or annotate it; otherwise simplify.

Week 9's room schedules are a legitimate compact-table use. Dense analytical
tables should normally be split or moved to an assignment/document.

### Code

Show code only when reading or modifying the code is part of the learning goal.
Prefer a short, runnable excerpt. If the result is the lesson, show the result
and keep setup code hidden.

### Fragments and auto-animation

Use fragments to reveal a reasoning sequence, an answer, or a comparison - not to
animate every bullet. The layout should remain stable as fragments appear.

Use `auto-animate` only when object continuity teaches the idea, as in the Week
3 visual-encoding sequences or Week 5 science progression. Reuse stable
`data-id` values and verify every intermediate state.

### Speaker notes

Put facilitation cues, transitions, prior-week connections, timing, and likely
student misconceptions in `::: notes`. Week 4 is the best current model for
systematic notes; Weeks 12 and 13 also use them effectively at transitions.

## Accessibility requirements

- Every meaningful image needs useful alternative text. Describe the teaching
  point, not merely the objects pictured.
- Image-only and background-image slides need a meaningful `.sr-only` heading
  plus an accessible description in the slide content.
- Maintain a visible keyboard focus indicator for links and controls.
- Never rely on color alone.
- Avoid text smaller than the typography limits above.
- Do not use low-opacity text for required content.
- Check contrast on image backgrounds and dark slides.
- Check that fragments do not leave required context invisible to exported PDF.
- Preserve reading order when using CSS Grid, Flexbox, or absolute positioning.

## CSS architecture

### Layer 1: course theme

`shared/styles/boland-reveal.scss` is the sole global Reveal.js theme. It should
own:

- palette and typography tokens;
- base slide spacing and readable content width;
- headings, body copy, links, images, tables, code, and callouts;
- accessibility primitives such as `.sr-only` and `:focus-visible`;
- stable, course-wide utilities such as alignment, image fit, captions, compact
  lists, and standard two-/three-column layouts.

Do not add week-specific coordinates, image filenames, or diagram selectors to
the global theme.

### Layer 2: reusable course components

A pattern belongs in shared CSS when it appears in at least two decks with the
same meaning and structure. High-value candidates from the audit are:

- hypothesis label and hypothesis statement typography used in Weeks 6 and 7;
- the shared confusion-matrix / two-outcome teaching composition in Weeks 6 and
  7;
- an accessible dark-slide treatment;
- a readable image-caption overlay;
- a standard relative diagram stage;
- two-, three-, and four-column grids;
- `.fit-img`, `.slide-question`, and an accessible muted treatment;
- a compact schedule table for slides such as Week 9.

Shared custom classes should use the `eco-` prefix, for example
`.eco-hypothesis__statement`, `.eco-grid--3`, and `.eco-image-caption`.

### Layer 3: week-local CSS

Use `weekXX/week_XX.css` for diagrams or compositions unique to a week. Week 12's
population map and Week 13's experimental-design visuals correctly belong at
this layer. Move repeated inline declarations from Weeks 3, 5, 6, and 7 into
week-local CSS even when they are not reusable course-wide.

Prefix local classes with the week, such as `.w03-bandwidth-stage` or
`.w12-systematic-map`. Avoid generic names such as `.col`, `.left`, `.right`,
`.header`, `.axis`, `.method-panel`, and `.method-title`; these already overlap
or have conflicting meanings across the repository.

### Inline-style threshold

Inline CSS is acceptable for a one-off value such as an individual marker's
`left` and `top` coordinate. Move styling into CSS when any of these is true:

- the same declaration appears twice;
- the style contains more than four declarations;
- the element is part of an animated sequence;
- the style expresses a semantic role rather than a one-off coordinate;
- the style is reused in another week.

Keep URLs and per-slide background settings in the QMD heading attributes.

## CSS inventory and disposition

| File | Status | Direction |
|---|---|---|
| `shared/styles/boland-reveal.scss` | Active global theme | Keep as source of truth; add shared tokens, accessibility, and reusable components here or in one imported component partial. |
| `shared/styles/theme-eco.css` | Apparently unused; overlaps the active theme | Retire after confirming no external deck imports it. It conflicts on root font size, heading scale, columns, and minimum slide height. |
| `shared/styles/accesibility.css` | Misspelled accessibility-only file | Do not reference under this name. Merge unique rules into the global theme, then retire it. |
| `shared/styles/accessibility_DEP.css` | Legacy mix of accessibility and layout overrides | Do not reactivate wholesale. Review focus/callout rules individually; its layout overrides are too broad. |
| `shared/styles/boland-revealDEP.scss` | Legacy theme | Keep only as history until the active theme is verified, then archive outside the active styles directory. |
| `week12/population_pipeline.css` | Active local component CSS | Keep local; namespace broad selectors before reuse or consolidation. |
| `week13/week_13.css` | Active local component CSS | Keep local; namespace generic selectors and avoid merging directly with Week 12 because similarly named classes have different meanings. |

Several decks reference `../shared/styles/accessibility.css`, but that file does
not exist. The website configuration also references `styles/accessibility.css`,
which is a different missing path. The preferred fix is to merge the necessary
accessibility rules into `boland-reveal.scss` and remove the extra CSS references,
leaving one source of truth.

## Week-by-week audit

The repository contains main weekly decks for Weeks 1-7, 9, 12, and 13. Week 8
contains assignment/interpretation material but no `week_08.qmd`; there are no
Week 10 or 11 directories.

| Week | Slides/sections | Visual pattern | CSS direction |
|---|---:|---|---|
| 1 | 40 | Data tables, image-led sequences, hidden headings, instructional callouts | Strong baseline with no inline styles. Fix the missing accessibility reference; retain GT theming and image patterns. |
| 2 | 27 | Notation backgrounds, plots, measurement-scale explanations | Strong baseline with no inline styles. Standardize repeated background placement only if it recurs in later decks. |
| 3 | 35 | Auto-animated visual encodings and hand-built diagrams | `292` inline style attributes are the largest maintenance risk. Move repeated bars, labels, stages, and panels into `week_03.css`; keep diagram coordinates local. |
| 4 | 17 | Minimal principle slides, examples, activities, extensive notes | Good narrative and notes reference. Replace repeated centered/code-chip inline styles with semantic classes. |
| 5 | 15 | Quote opener, image slides, repeated auto-animated science model | Move the repeated `560px` stage and four repeated label positions into `week_05.css`; create a reusable quote-slide treatment only if used again. |
| 6 | 36 | Hypothesis, uncertainty, decision, and confusion-matrix diagrams | Extract hypothesis typography and the repeated comparison/confusion composition shared with Week 7. Move remaining diagram declarations into `week_06.css`. |
| 7 | 31 | Casino framing, Bayesian/frequentist contrasts, reused Week 6 diagrams | Share the genuine Week 6 components; keep casino-specific visuals local. Replace repeated white/yellow text declarations with named local classes. |
| 9 | 4 | Plain peer-review instructions and room schedule tables | Apply the standard YAML contract and add a compact schedule-table component only if this pattern will recur. |
| 12 | 56 | Survey pipeline, sampling maps, methods, labs, question examples | Good use of local CSS. Keep individual marker coordinates inline; namespace generic component names and fold remaining semantic styles into the local file. |
| 13 | 36 | Analytics ladder, model flows, experiment designs, image captions | Best current example of class-based local composition. Namespace generic classes; avoid direct consolidation with Week 12's conflicting `.method-*` classes. |

## Standardization priorities

1. **Repair the style entry points.** Resolve the missing accessibility CSS
   references and make the active theme the single global source of truth.
2. **Normalize deck front matter.** Bring all main decks to the default contract,
   with local CSS listed only where needed.
3. **Make accessibility global.** Add visible focus, accessible muted text, dark
   background text handling, captions, and image-fit behavior to the active
   theme.
4. **Extract the Week 6/7 shared components.** These are the clearest true
   cross-week duplicates.
5. **Move repeated inline styles into local CSS.** Start with Weeks 3 and 5, then
   Weeks 6 and 7.
6. **Namespace local CSS.** Start with the overlapping `.method-*` names in Weeks
   12 and 13, then generic selectors such as `.axis`, `.col`, `.left`, and
   `.right`.
7. **Retire inactive alternatives.** Remove or archive `theme-eco.css` and `_DEP`
   styles after representative decks render successfully with the active theme.

## Authoring checklist

Before editing:

- Read this guide and inspect the nearest content-equivalent deck.
- Confirm whether the pattern belongs to the global theme, a shared component,
  or a week-local stylesheet.
- Check the worktree before modifying files.

While editing:

- Give every slide one narrative job.
- Use audience-facing titles and concise visible copy.
- Reuse the course palette, type scale, callouts, and table treatment.
- Add meaningful alternative text and speaker notes.
- Avoid generic custom class names and repeated inline declarations.
- Check Quarto div-fence balance whenever a composition uses nested fences.

Before delivery:

- Render the changed deck explicitly.
- Inspect the opening, a typical content slide, each custom diagram type, every
  dark/image slide, and the closing slide.
- Verify title wrapping, text size, contrast, image crops, fragment states,
  overflow, and slide flow.
- Check the PDF/export state when fragments or background images carry meaning.
- Run `git status --short` and report only the files intentionally changed.
