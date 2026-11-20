# Sampling Lab App

Development source: `shiny-apps-dev/week12/samplinglab`.
The comparison redesign is approved for classroom publication. Deployment and
live-connection verification are recorded in the S: release receipt.

## Classroom flow

The default **Compare samples** tab shows the full response pool, a simple
random sample, and a nonrandom sample together. Both samples have the same
number of rows. Start with an outcome and sample size, then compare group
proportions such as class standing or attendance.

- The outcome table shows rows selected, answered rows, the estimate, and its
  difference from the pool. Numeric outcomes use the mean; categorical
  outcomes use the percentage with a selected response. Missing outcome
  answers are excluded from the estimate and disclosed in the answered count.
- The shared horizontal chart uses the same percentage scale and colors for
  all three groups: pool gray, random blue, and nonrandom orange. Missing
  subgroup responses have their own category. An expandable table includes
  counts, percentages, and differences in percentage points.
- **Draw another random sample** changes only the random rows. This makes
  chance variation visible against a fixed nonrandom sample and pool.
- Changing the outcome or comparison variable keeps both samples fixed.
- Changing sample size or the nonrandom selection rule draws a new pair.
- Sample size can range from one row to the entire pool.

### Nonrandom selection

**Convenience sample:** Select the first n rows after ordering by response
order, commute time, work hours, sleep, or confidence. Values are ordered from
smallest to largest; missing values come last. Live response order follows
ascending response ID. CSV row order is retained unless a response_order
column is supplied. Demo response order is simulated.

Try selecting the fewest work hours, then compare sleep and the composition
of the sample. Changing the random draw leaves this nonrandom selection fixed.

**Quota sample:** Allocate whole-person quotas proportionally to a selected
variable, then take the first rows within each group. There is no random
selection. Keep the quota variable fixed and change the comparison variable
to see whether matching one characteristic also matches another.

Neither a close mean nor balanced groups establish overall representativeness.
The app reports differences on the selected variables without a pass/fail
verdict. The response pool itself may omit people in the target population.

## Other tabs

Response pool, the original single-method Sampling lab, Repeat sampling, and
Image heatmap remain available. The original method controls and repeated-draw
controls are grouped under **Other sampling methods and repeated draws**.
The original methods are simple random, convenience, systematic, and
stratified sampling. Systematic sampling uses a fractional N/n interval with
a random start; stratified sampling randomly selects within proportional
whole-person allocations, including missing-group responses.

## Data sources

- Demo data generated in app.R, explicitly labeled simulated.
- Live LimeSurvey/MariaDB using SURVEY_DB_HOST, SURVEY_DB_PORT, SURVEY_DB_NAME,
  SURVEY_DB_USER, and SURVEY_DB_PASSWORD. Credentials are never stored here.
- Uploaded CSV with the app's descriptive column names.

The live query uses survey_931648, completed responses only (non-null
submitdate), ordered by ascending ID. All selected majors are retained as a
combined category rather than silently reducing a multiple-major answer to
its first choice. No live student data was loaded for local verification.

## Implementation and checks

comparison.R contains the shared quota/stratified allocation and group
comparison helpers. All random draws sample row indices, including singleton
groups. The new comparison does not change the pool while students inspect it.
The Linux server still loads its existing renv project. A local preview skips
that server-only path and uses the explicitly configured local R libraries.

Run `Rscript --vanilla test-comparison.R` from this app folder. Base-R checks
cover quotas, exact sample sizes, unique rows, singleton groups, absent
categories, and missing groups. With Shiny installed, integration checks also
cover fixed rows across outcome/group changes, random-only redraws, quota
inspection on another variable, full-pool samples, systematic intervals,
repeated sampling, and missing outcomes.

Local preview runtime, downloaded packages, plots, and disposable evidence:
S:/codex-scratch/boland_course/canvas/work/sampling-lab-20261120.
Run with `shiny::runApp(..., host="127.0.0.1", port=8813)` using the scratch
R library and the existing course library. Tracked source stays in this repo.
Server publication follows shiny-apps-dev/README.md and remains a separate step.

## Many random samples (review update)

The **Many random samples** tab draws 20-1,000 simple random samples, default
200, at the common sample size. Sampling is without replacement within each
draw, and independent draws can contain the same response again. Saved row
indices link the histogram and draw-by-draw plot to an inspectable sample.
The pool and the fixed nonrandom estimate are reference lines, not pass/fail
thresholds. Red highlights the inspected draw, not a rejected sample.

Select a draw by clicking its dot or using the draw-number slider. Buttons
inspect a randomly chosen draw or the farthest usable estimate from the pool
in this batch. The selected draw's outcome and demographics can be compared
with the pool. Switching the demographic keeps its rows fixed. Changing the
pool, outcome, outcome category, sample size, or nonrandom rule clears the run;
redrawing the single random comparison does not clear or alter saved draws.

The empirical middle 90% and full range describe this batch only. They are
not a confidence interval or a guarantee that most draws will be close for
every pool, outcome, and sample size. Draws without outcome answers are counted
and excluded from the distribution summary, not silently treated as zero.

The teaching note explains why we can identify a far-off estimate in the
known-pool exercise but usually cannot in a real survey. Demographic checks
against credible population benchmarks can expose problems; agreement on a
few variables does not establish outcome accuracy. The farthest-draw button
is a demonstration aid, not a real-world sample rejection rule.

Integration checks verify saved rows/estimates, farthest-draw selection,
demographic switching without redrawing, invalidation, categorical outcomes,
and identical estimates when sampling the entire pool.

## Production live-data behavior

The Linux course-server deployment defaults to Live survey DB. Local Windows
previews default to Demo data. A failed production connection never silently
substitutes demo responses. Demo mode remains an explicitly labeled option.
The connection requires the existing server environment variables; port defaults
to 3306 if SURVEY_DB_PORT is absent. No credentials or response exports belong
in this repository.

Refresh survey responses reloads completed responses during class. It updates
the pool and resets sample comparisons and repeated batches. The display shows
the completed-response count and last-refresh time in America/Chicago.
Individual live response records and free-text answers are not displayed in the
pool/sample preview tables or exposed as grouping/stratification choices.
Connection errors are generic in the public UI.

For a newly selected data source, the initial sample size is at most half the
pool (capped at 20), so a small class pool does not initially produce three
identical full-pool comparisons. Refreshing the same source preserves the
chosen outcome, demographic, and sample size where valid. Changing sample size
does not reset the outcome or demographic controls.
