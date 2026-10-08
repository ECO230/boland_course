# Sampling Distribution Simulator

## Teaching purpose

Draw repeated samples with replacement from a known finite population to show
how sample means accumulate into a sampling distribution and how t confidence
intervals vary across samples. Blue intervals contain the population mean;
red intervals miss it. The gray normal curve uses population SD / sqrt(n)
and is a theoretical reference, not a fitted distribution or an assertion of
exact normality for every population and sample size.

## Controls and behavior

- ChickWeight remains the default; Iris and Airquality are also available.
- Add 1 sample updates the current values, mean, SE, interval, histogram, and
  CI chart together. It is disabled while an automatic run is active.
- Run 100 samples starts a fresh run and displays each sample individually.
- Clear cancels the run and removes the samples. Changing population, n, or
  confidence level also cancels and clears, avoiding mixed distributions.
- Clicking Run 100 again cancels the old run and starts a new one.
- The CI chart shows the most recent 100 intervals; the histogram retains all
  accumulated sample means when manual sampling continues beyond 100.

## Rendering and animation

The four chart canvases remain mounted in the browser. R calculates the
statistics and sends a compact JSON frame; plain JavaScript draws the charts
without producing or transferring plot images. A ResizeObserver redraws the
latest frame when the window or visible panel changes size.

The browser acknowledges a displayed frame. R then uses `later::later()` to
schedule the next sample after 80 milliseconds. There is no blocking loop or
sleep in the app. Slow clients and background tabs naturally slow the run
rather than skipping samples. Generation and sample-number checks discard
stale acknowledgments and callbacks after Clear, a settings change, or a
restart. Closed sessions do not draw further samples.

The formula panel preserves three parts: symbolic formulas, numeric
substitution, and final results. It uses ordinary HTML rather than MathJax
or another server-generated image. Narrow panels stack these parts vertically.

Population values wrap into responsive columns within a bounded scrollable
panel. Observation numbers preserve the original order. The population
summary lists one metric per row so all eight metrics fit the sidebar.

## Statistical calculations

Sampling is with replacement. SE = sample SD / sqrt(n), with t critical value
at (1 + confidence) / 2 and n - 1 degrees of freedom. Population SD uses the
finite-population denominator N. Histogram bin widths are 0.1 for Iris and
3 for Airquality/ChickWeight. The common chart range starts at the configured
range and expands to include outlying interval endpoints; no sample mean is
dropped. The normal reference curve is scaled to 90% of histogram height,
matching the existing teaching display rather than showing a density axis.

## Development and deployment

`shiny-apps-dev/week06/samplingdist` is authoritative. Keep the compatibility
copy in `shiny-apps/week06/samplingdist` synchronized, including `www/sampling.js`.
Dependencies are Shiny and later (already used by Shiny). The server renv path
is loaded when it exists, allowing the same sources to be tested locally.

Commit and push repository changes; the instructor pulls them on the Shiny
server. Include the `www` directory when deploying. Existing repo symlinks
follow the pulled source; refresh the app session after updating. These changes
do not require R package recompilation.
