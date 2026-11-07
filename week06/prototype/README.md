# Week 6 learnr2 prototype

Unlisted preview: https://eco230.github.io/boland_course/week06/prototype/week6-inference.html

## Teaching scope

A 25-35 minute practice sequence based on the public Week 6 slides and guided
notes. It includes a sampling-variation experiment, p-value feedback, confidence
interval interpretation, and a six-step lab that audits an AI-generated analysis.
The R cells use base R with explicitly simulated teaching data. They do not
change the existing slides, Canvas lab, assignments, deadlines, or grades.

This is an actual learnr2/Quarto live-html tutorial. WebR executes R in the
browser. AI prompts are optional and copied into the learner's own tool; no
model API, key, or automatic AI assessment is configured. Model responses are
fixed teaching feedback. Open responses are not graded for correctness.

## Reproduce

Tested with Quarto 1.8.25, R 4.5.2, and learnr2 0.1.3.9006, from official
PPBDS/learnr2 commit `805c9520a97286509ef497362ce31122f2503f8d` (MIT). The bundle includes quarto-live.
Install learnr2 and its dependencies in an isolated R library, then run
`scripts/build-week6-prototype.R` from the course repository root, passing an
unused output directory. The script stages sources and the extension, runs the
learnr2 static checker, and renders only this tutorial. It never publishes.
Set R_LIBS_USER to the isolated library and QUARTO_PATH when needed.
Do not add this prototype to `_quarto.yml` navigation or search.

## Verification

The prototype passed learnr2::check_tutorial() and a Quarto render. Browser
checks covered both WebR exercises (including plots), quiz feedback, reflection
model answers, saved answers after reload, topic navigation, and a downloaded
JSON containing submitted answers and persisted R code. The lab run gave n=40,
mean change=7.7025, t=2.4161, df=39, p=0.02047, and 95% CI [1.254053,14.150947].
The browser download-event observer stalled, but the actual downloaded JSON was
located and its contents verified independently.

## Delivery and data

Publish only the rendered HTML, CSS, and referenced dependencies under
`week06/prototype/`. Keep the existing navbar, search index, and course pages
unchanged. The HTML requests noindex/nofollow; the URL is unlisted, not private.
External runtime/CDN resources require internet access on first load.
Student information and answers are stored in localStorage until explicitly
exported as JSON. The export also includes learnr2's browser/device metadata.
Use demo identities while reviewing. Nothing is submitted to Canvas.

`navigation.html` normalizes same-page TOC links before learnr2 handles clicks;
Quarto can expand them into full URLs, which otherwise prevents skip-ahead.
This is a prototype-local compatibility fix, not a change to package sources.
