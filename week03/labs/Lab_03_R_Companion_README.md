# Lab 3: Visualizing Data with ggplot2

An optional worked R companion to the Tableau and Excel lab. Use the same
12,000-crash sample to explore ranking, averages, monthly trends, quarterly
shares, relationships, distributions, and a point map.

## Open in Posit Cloud

1. Create a project from the private `ECO230/lab-03-r-companion-ip` GitHub
   repository. Your Posit Cloud GitHub connection needs access to that repository.
2. Use R 4.6.1, matching the shared course lockfile.
3. Run `source("project_setup.R")` once in the Console. After restoration,
   `renv::status()` should report a consistent project.
4. Open `Lab_03_R_Companion_IP.qmd`. Run chunks from the top down, or click
   **Render** for an HTML guide with code and plots.

The companion is a worked reference rather than a submission template, so HTML
is intentional: code stays easy to copy and the longer charts stay readable.
Nothing needs to be submitted beyond the main Lab 3 requirements in Canvas.

## Files and style

- `Lab_03_R_Companion_IP.qmd`: complete, editable examples and optional changes.
- `Lab_03_R_Companion_IP.Rproj`: RStudio project.
- `data/chicago_traffic_crashes_sample.csv`: the existing Lab 3 sample.
- `data/source.json`: provenance, sampling limitations, and source hash.
- `project_setup.R`, `.Rprofile`, and `renv/`: shared course setup.

The examples use ggplot2's built-in minimal theme and a Tableau-like color
palette. No additional theme, mapping package, map service, or API key is needed.
After package setup, all chart data is local. The course environment includes
more packages than this companion uses so it matches the other homework/lab
projects; it is not a newly generated lockfile from a different R version.

The chart data are sampled crashes, not official totals or rates. Several
crashes can share one weather observation. In the reduced quarterly share
example, categories are removed after computing percentages to preserve the
original denominator.

Students work in their own Posit Cloud copies; no Git commands are needed.
