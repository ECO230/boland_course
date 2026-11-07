# Week 7 chi-square tutorial prototype

Unlisted preview: https://eco230.github.io/boland_course/week07/prototype/week7-chi-square.html

The tutorial follows the lecture's Chicago crash example, the lab's distinction
between goodness of fit and independence, and Homework 6's Step 4 and Step 5
prompts. Its two-category crash table is aggregated from the public course
sample at week03/labs/data/chicago_traffic_crashes_sample.csv. It does not
contain graded lab items, student submissions, or individual crash records.

Build with the same pinned learnr2/Quarto setup described in the Week 6 prototype
README. Copy this folder's QMD, CSS, and navigation.html to an isolated build
folder. Run learnr2::add_live_extension() on the folder and
learnr2::check_tutorial() before rendering week7-chi-square.qmd. Use a website
project with search disabled and only this QMD in the render list.

The feedback prompt builder combines the draft with evidence calculated by R
and a short course rubric. It does not send requests, grade drafts, or save
the feedback draft. The worked coaching response is written in advance.
Reflections and R code use learnr2's local saving and JSON export. A live model
would require a separately approved server, API account, and course usage cap.

Deploy only the HTML, CSS, and referenced dependencies under week07/prototype/.
Do not add it to site menus or search, or change any Canvas assignment.
