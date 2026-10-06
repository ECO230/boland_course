# Lab 5: Practice R Coding

Choose the learning path that works for you. Both Quarto documents contain the
same 9 starter examples and prompts.

- `Practice_R_Coding_IP.qmd`: working starter examples with empty response chunks.
- `Practice_R_Coding_Solutions.qmd`: the same examples and prompts with complete responses.

You may start with either version, switch between them, copy an example, and
change it. Run chunks from top to bottom and explain the results in your own words.

## Start in Posit Cloud

1. [Open the Lab 5 project](https://posit.cloud/spaces/3173/content/12850281).
2. Use R 4.6.1 and run `source("project_setup.R")` once in the Console.
3. Check `renv::status()`, then open either document and select **Render**.
4. Try changing at least 3 examples. Submit the reflection described in Canvas.

The source is version controlled in the private `ECO230/lab-05-ip` repository,
built from the course repository's `week05/lab_05.manifest.json`. Students
work in independent copies and do not need to use Git.

## Videos

- [Orientation: Working Through the R Recipes](https://cdnapisec.kaltura.com/p/2370711/embedPlaykitJs/uiconf_id/54949472?iframeembed=true&entry_id=1_0t9ct8lt)
- [Full Walkthrough: Solving All Examples](https://cdnapisec.kaltura.com/p/2370711/embedPlaykitJs/uiconf_id/54949472?iframeembed=true&entry_id=1_6cn5y3xa)

The `.Rmd` (R Markdown) files in the recordings have been replaced with `.qmd`
(Quarto) documents. Click **Render** instead of **Knit**. Everything else
follows the same example sequence and prompts.
This release uses `data/airbnb_chicago.csv`, `listed_price`, and `mdy()` for
month/day/year strings, with explicit handling of missing values and zero
denominators. Review-score interval boundaries are stated without overlaps.

HTML is available for a readable code reference; the default Render produces
a PDF with Typst. The shared lockfile matches the course environment.

## Code Help

[Open Reco - eco230r Code Helper](https://chatgpt.com/g/g-699b870e90c48191b249a4835310216e-reco-eco230r-code-helper). Ask it to explain code or help troubleshoot an error. Include what you tried, then run and check its suggestions.
