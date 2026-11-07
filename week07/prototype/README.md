# Week 7 chi-square tutorial prototype

Unlisted preview: https://eco230.github.io/boland_course/week07/prototype/week7-chi-square.html

The tutorial follows the lecture's Chicago crash example, the lab's distinction
between goodness of fit and independence, and Homework 6's Step 4 and Step 5
prompts. Both editable WebR cells use the original eco230r functions and show
the package's $results directly. Cool_Time uses the built-in tea_tab dataset.
Airbnb retains every homework row and the two required variables, with the
same filters and formula. Interpretation prompts refer to the starter results.

Build with the pinned learnr2/Quarto setup in the Week 6 prototype README and
eco230r commit a60d66f139cd37874b4b6c2e50be89288b745e6c from the course lockfile.
Copy the QMD, CSS, and navigation.html into an isolated build folder. Run
prepare-browser-assets.py with the installed course eco230r directory and
the build's data directory. Copy load-eco230r.R into that data directory.
The archive includes the pure-R package, its MIT license, and built-in data.
WebR downloads dplyr, BayesFactor, effectsize and their dependencies from the
official WASM repository, so the first load takes longer.

Run learnr2::add_live_extension() and learnr2::check_tutorial() before rendering
week7-chi-square.qmd. Use a website project with search disabled, only this QMD
in the render list, and data/* under project resources. Build-time R also needs
eco230r and dplyr to compute reference evidence for the feedback prompt.

The feedback prompt builder combines the draft with evidence calculated by R
and a short course rubric. It does not send requests, grade drafts, or save
the feedback draft. The worked coaching response is written in advance.
Reflections and edited code use learnr2's local saving and JSON export. A live model
would require a separately approved server, API account, and course usage cap.

Deploy only the HTML, CSS, browser data bundle, and referenced dependencies under week07/prototype/.
Do not add it to site menus or search, or change any Canvas assignment.
