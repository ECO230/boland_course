---
schema_version: 1
key: rstudio-recipes
canvas_type: assignment
visibility: course_only
kaltura_partner_id: 2370711
videos:
  - title: "Orientation: Working Through the R Recipes"
    entry_id: "1_0t9ct8lt"
  - title: "Worked Examples (Spoilers)"
    entry_id: "1_6cn5y3xa"
---

## Note About the Videos

The `.Rmd` (R Markdown) files shown in the videos have been replaced with
`.qmd` (Quarto) documents. Click **Render** instead of **Knit**. Everything
else follows the same example sequence and prompts; the small data and code
adjustments for the current project are noted below.

## Purpose

Practice adapting working R examples. Some recipes go beyond the statistical
tests used later in the course; the goal is exposure and productive practice,
not completing every example without help.

Watch the orientation video above before beginning. The Week 2 [Video Tutorials: R / Posit Cloud](https://uwlac.instructure.com/courses/{{ canvas_course_id }}/pages/video-tutorials-r-slash-posit-dot-cloud) page contains additional demonstrations if you need a refresher.

## Review the Posit Cloud recipes

- [Read a CSV file](https://posit.cloud/learn/recipes/basics/ImportA1)
- [Read an Excel file](https://posit.cloud/learn/recipes/basics/ImportF)
- [Select columns from a table](https://posit.cloud/learn/recipes/transform/TransformA)
- [Add and calculate a column](https://posit.cloud/learn/recipes/transform/TransformF)
- [Compute grouped summaries](https://posit.cloud/learn/recipes/transform/TransformI)
- [Filter rows](https://posit.cloud/learn/recipes/transform/TransformL)
- [Group a continuous variable into categories](https://posit.cloud/learn/recipes/transform/TransformM)
- [Parse dates and datetimes](https://posit.cloud/learn/recipes/datetime/DatetimeA)
- [Extract date and datetime components](https://posit.cloud/learn/recipes/datetime/DatetimeB)

## Lab 5: Choose Your Learning Path

[Open the Lab 5 Practice R Coding project in Posit Cloud]({{ practice_r_coding_url }}).
Run `source("project_setup.R")` once in the Console, then choose either document:

- **Try it yourself:** `Practice_R_Coding_IP.qmd` has nine working starter examples
  and prompts with empty response chunks for you to complete.
- **Follow the worked examples:** `Practice_R_Coding_Solutions.qmd` has the same
  starter examples and prompts, with complete responses. You may start here,
  copy and run code, then change it before trying the practice version.
- **Watch and follow:** the **Worked Examples (Spoilers)** video above walks
  through all the original examples. Use whichever combination helps you learn.

Both versions are Quarto documents. Select **Render** to create the report,
then run chunks from top to bottom and try changing at least 3 examples.
The recordings show the older R Markdown files: **Render** replaces **Knit**;
our current data use `data/airbnb_chicago.csv`, `listed_price`, and `mdy()`
for month/day/year date strings. The examples now handle missing values and
zero denominators explicitly.

You may adapt examples to your group project. If an example fails, read the
error, compare your code with the working example, and document what you tried.
You may use the solved document or video from the beginning; make sure you can
explain the code and check its results.

## Help While You Work

[Open Reco - eco230r Code Helper](https://chatgpt.com/g/g-699b870e90c48191b249a4835310216e-reco-eco230r-code-helper). Reco can help explain code or troubleshoot an R error. Include the code, the error message, and what you want to do, then run and check the suggested changes yourself.

## Submission

Submit a short text response in Canvas that answers both questions:

1. Which 3 examples did you alter successfully? If you did not complete 3,
   describe the examples you attempted and what happened.
2. Which 3 concepts were most difficult or still need clarification?

An answer of "none completed" can still earn credit when it describes a real
attempt. "I did not try" does not meet the assignment requirements.

