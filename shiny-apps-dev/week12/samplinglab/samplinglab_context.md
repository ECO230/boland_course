# Sampling Lab App

Development source: `shiny-apps-dev/week12/samplinglab`.
The November 24 release below uses actual survey responses by default for the
class exercise and a separate live feed for every recorded G03Q17 answer.
Deployment and live-connection receipts remain on S:.

## November 24 release: automatic comparisons and parallel recruitment

Probability methods immediately draw our sample and 200 additional samples.
The view always shows our sample, the largest demographic mismatch in that
batch, and the distribution, in that order. There is no reveal or inspection
button. The mismatch uses the selected demographic, not the outcome. Changing
the demographic or outcome preserves the saved rows. Draw another sample
replaces our sample and batch; Draw another set of 200 preserves our sample.

Convenience and quota show every available recruitment method simultaneously,
with one independent selector per row. A summary compares outcome estimates,
selected and answered counts, and the largest demographic difference. Each
chart uses the same full pool and 0-100% scale. All rules are deterministic.

Defaults and flips:

- Response order: first responses, or last responses.
- Summer availability: in La Crosse, or away for summer. Missing responses do
  not count as either group.
- Commute: shortest first, or longest first, among answered travel times.
- Attendance: most frequent first, or least frequent first, using the survey's
  ordered attendance categories. This is reported frequency, not a presence log.
- Major: one recorded major, changeable. Multiple-major respondents qualify for
  each major they selected; missing majors do not qualify.
- Class standing: earlier or later class years first. These are class years,
  not an inferred chronological age.
- Age band: younger or older recorded bands first. The original overlapping
  response options are retained; no exact age is inferred.

Changing one row keeps the other recruitment examples fixed. Missing priority
values are excluded from that frame. Quotas allocate from the full pool and
fill only eligible respondents. Unfilled places remain visible in each row's
quota table. If quota groups and recruitment priority are the same variable,
reversing that priority can correctly leave the sample unchanged. The class
standing quota default is retained, and the demographic check remains separate.
The old single recruitment selector and Recruit only control are removed.

Open-ended responses retains newest and oldest lists and adds Word cloud.
The cloud now uses the locally vendored MIT-licensed wordcloud2.js canvas
layout, pinned in www/vendor/wordcloud2. Words are packed spatially with some
rotation rather than arranged as paragraph text. Size uses word occurrence
frequency; the count table also retains response-level mentions. Case and
punctuation are normalized. All words, including one-letter and numeric tokens,
are included by default with no top-60 cap. Common filler/prompt words can be
omitted with an explicit checkbox. The canvas grows if words fail to fit and
reports how many distinct words were actually placed. Newest and oldest retain
the complete, unmodified nonblank answer list, including duplicates. No response
text is sent to an external service; this summarizes literal shared words, not
inferred semantic groups. Answer text and serialized cloud data are HTML-escaped.
Sampling comparison controls are hidden while Open-ended responses is selected;
they never filter the complete answer list or word cloud.

The response view now has its own live-only feed, independent of the sampling
data mode and sample size. It verifies question code G03Q17 against survey 931648
metadata (qid 260, gid 28, column 931648X28X260), then reads every recorded
nonblank answer to that column, including partial responses. The list preserves
raw answer text and genuine duplicates. It sorts by completion/update time with
response order breaking ties. It never substitutes the simulated hobby pool.
If live access is unavailable, it says so instead of showing demo answers.

Open-ended responses loads when first opened and updates only through Refresh
responses. There is no automatic timer. Refreshing it does not redraw the
sampling examples. Unchanged answers do not redraw the cloud. The sampling
pool itself still uses completed survey responses only.

Image heatmap also uses its own live-only feed from the same survey 931648,
question G03Q14 (column 931648X28X257). It loads on first opening and updates
only through Refresh heat map. Sampling modes, sample sizes and the selected
demographic never filter it. Every nonblank recorded image answer is read,
including partial responses; only valid 0-100 coordinate pairs can be plotted.
The source badge shows the total answers and the number without plottable
coordinates. The November 24 check found 47 answers and 27 coordinate pairs;
the other answers contained text rather than clickable locations. Density is
drawn only when enough distinct points are available. No simulated clicks are
substituted if the live connection fails. Both supplementary views hide the
sampling controls and show a last-updated timestamp beside their manual refresh.
The heat map preserves the source image's 318:314 aspect ratio. Its responsive
plot fills the available width up to 1,000 pixels and is centered horizontally;
height follows the aspect ratio. Fixed plot coordinates keep the image and
click/density overlays aligned, and resizing rerenders the plot.

Development evidence and logs:
S:/codex-scratch/boland_course/canvas/work/sampling-lab-comparison-grid-20261124.
Publication URL: https://shiny.60land.com/week12/samplinglab/.
The authenticated server review verified 47 raw answers and all 124 distinct
words, including the instructor's four example terms. The newest and oldest
lists were exact reversals. Live Shiny integration checks confirmed that demo
mode and sample size cannot filter the G03Q17 feed. Counts can grow with new
responses; these numbers record the November 24 review, not fixed expectations.

## Explore: Compare samples

The **Compare samples** tab under Explore shows the full response pool, a simple
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

Explore retains Compare samples, Many random samples, Response pool, and Image
heatmap. Open-ended responses lists the hobby/interest question separately.
The old Sampling lab and Repeat sampling tabs and their controls were removed
from the interface because the class demonstration supersedes them.

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

The class exercise loads all completed responses once into a session's in-memory
reactive cache. All single draws and batches of 200 sample this same data frame;
the draw loop never connects to the survey database. Refresh survey responses,
or switching back to Live survey DB, reloads the pool. With 47 responses in the
November 24 check, the cleaned pool occupied about 21 KB; no response cap was
needed. The open-text and image feeds retain their separate manual refreshes.

Server profiling measured one database fetch at 0.014 seconds, 200 in-memory
draws at 0.089 seconds, and their demographic mismatch checks at 0.426 seconds.
The main cold-start delay was the renv repository-wide synchronization check:
sourcing the app in fresh R processes took 21.842 seconds with the check and
3.510 seconds without it. App startup still activates the existing renv library,
but temporarily disables this diagnostic during renv::load and restores the
previous option afterward. Repository maintenance settings and package versions
are unchanged. The integration check counts survey fetches across the initial
200 draws, another batch, and another sample; only Refresh causes another fetch.
Timing receipts contain counts and durations, not survey records.

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

The app defaults to Live survey DB on every host. A local runtime without the
survey connection reports that live data is unavailable; it never silently
substitutes demo responses. Demo mode remains an explicitly labeled option.
The connection requires the existing server environment variables; port defaults
to 3306 if SURVEY_DB_PORT is absent. No credentials or response exports belong
in this repository.

Refresh survey responses appears in the main Class demonstration controls,
above Data source. It reloads completed responses during class, updates the
pool, and resets sample comparisons and repeated batches. The connection
details show the completed-response count.
The Class demonstration source badge also shows the last successful survey-pool
refresh time, including seconds and the America/Chicago time-zone abbreviation.
Drawing another sample does not change this timestamp; refreshing the survey
pool does. Simulated and uploaded sources do not display a live refresh time.
Individual live response records and free-text answers are not displayed in the
pool/sample preview tables or exposed as grouping/stratification choices. The
requested open-ended text is available only as a separate, unlinked answer list.
Connection errors are generic in the public UI.

For a newly selected data source, the initial sample size is at most half the
pool (capped at 20), so a small class pool does not initially produce three
identical full-pool comparisons. Refreshing the same source preserves the
chosen outcome, demographic, and sample size where valid. Changing sample size
does not reset the outcome or demographic controls.

## Instructor-led revision (2026-11-23)

The new default Class demonstration follows the lecture: simple random,
stratified, systematic, convenience, then quota. Previous/Next or the numbered
method list controls the sequence. The older comparison tabs are under Explore.
The main flow is draw, inspect the outcome, then Check who we got. Demographic
checks do not redraw the sample. The data source is a collapsed control; source
labels always distinguish actual completed responses from simulated data.

Probability methods can draw 200 genuine samples using the current selection
rule. The inspect button selects the largest demographic mismatch on the chosen
characteristic among those saved draws. It does not fabricate a sample or
select on its outcome. Distribution and composition views alternate to keep
the classroom screen focused. Benchmarks in this exercise are the response
pool, not a claim about the true population.

Stratification randomly selects within proportional groups fixed before the
draw. Switching the demographic check reveals whether balancing one variable
also balanced others. The systematic example uses a random start and N/n
interval. In simulated mode, an explicitly labeled alternating summer-residence
list demonstrates periodicity. The simulated pool now includes balanced
summer-residence groups; real responses are never rearranged by this option.

Convenience recruitment can be restricted to respondents who report being in
La Crosse over summer. This models local availability, not on-campus employment.
Missing summer-residence answers are not assumed to mean present. An unavailable
field falls back to a first-response convenience list. Quotas use full-pool
benchmarks, then fill only from the available recruiting frame. Unreachable
slots remain unfilled and are displayed; excluded people are never silently
used to complete the quota. Summer residence is the initial check for this
convenience example; the instructor can examine other recorded characteristics.

The live source and aggregate-only safeguards remain in place. The instructor
approved this revision for publication on November 23. Deployment and live
verification are recorded in the S: release receipt.
Checks: test-demonstration.R covers saved uneven draws, fixed rows when checking
another variable, stratified allocations, periodicity, restricted frames, and
unfilled quotas. test-comparison.R preserves the existing comparison/live-mode
checks. Runtime, logs, and screenshots remain on S:.

## Earlier November 24 review draft (superseded above)

The default remains Class demonstration, with class standing as the initial
demographic check. Draw 200 samples now always shows its distribution, including
while demographic checks are open. The old conditional explicitly hid that
chart when the demographic check was shown.

Inspecting an uneven draw preserves our original sample. Two stacked charts
compare each sample with the same full-pool proportions on a fixed 0-100% axis.
The inspected comparison stays hidden until requested. The outcome table then
shows all three groups. Switching the demographic rechecks the same saved 200
draws; changing the design, refreshing data, or drawing a new sample clears the
batch. Starting another batch leaves our sample fixed and clears inspection.

Convenience recruitment offers only available survey fields: first responses,
summer availability, commute time, attendance, major, class standing, and age
band. Summer availability defaults to Yes; commute uses a maximum travel time.
Other options select a recorded category. Major eligibility includes respondents
who selected that major alongside another major. Missing answers do not satisfy
a restriction. A short commute is not labeled on-campus housing, and summer
residence is not labeled employment. Gender and housing are not collected.

Quota sampling uses the same recruitment choices and full-pool class-standing
targets by default. The target group and the recruitment criterion are separate
controls. Unreachable groups remain unfilled. Convenience and quota rules are
deterministic: the demonstration asks the instructor to change the recruitment
rule rather than drawing 200 identical nonrandom samples.

The current survey has one genuine open-text prompt: hobby/interest/community/
skill. Hometown is a map-location question and is not included in the text list.
Open-ended responses shows each nonblank answer, retaining duplicates, newest
or oldest first by completion time (response order breaks ties). CSV/demo data
without timestamps uses response order. The list does not link answers to any
other response fields, IDs, timestamps, or individual records. HTML is escaped.
Refresh responses reloads completed responses and resets sample comparisons;
there is no automatic timer that would interrupt a demonstration. Uploaded
unknown columns are not automatically published as free text.

Development checks use simulated/synthetic data. Review runtime and logs:
S:/codex-scratch/boland_course/canvas/work/sampling-lab-review-20261124.
