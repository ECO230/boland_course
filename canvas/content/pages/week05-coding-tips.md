---
schema_version: 1
key: coding-tips-and-other-reminders
canvas_type: page
visibility: course_only
---

## Coding Tips and Other Reminders

1. **Practice, start early, and ask for help.** Errors are part of learning R.
   Work through the **RStudio Recipes** assignment, but take a break and ask
   for help if you have been stuck on the same problem for an hour. When you
   ask, identify the Posit Cloud project, document, and code chunk involved.

2. **Put operations in a logical order.** If later calculations should use
   only some observations, filter before grouping and summarizing. A common
   sequence is:

   ```r
   data |>
     filter(...) |>
     group_by(...) |>
     summarise(...)
   ```

   Not every analysis needs every step. Also confirm that the data object is
   visible in RStudio's Environment pane before trying to use it.

3. **Coordinate work in shared Posit Cloud projects.** Posit Cloud is not a
   Google Docs-style editor. Avoid having multiple people edit the same file
   simultaneously. Give each member a separate working document, designate one
   person at a time to update the shared deliverable, close stale browser tabs,
   and render after incorporating changes.

4. **Keep the project organized.** Use short comments and clearly named code
   chunks. Separate major tasks into multiple chunks or scripts instead of one
   large block of code.

5. **Format pipelines consistently.** Put each operation on its own line and
   indent it consistently. Readable code is easier for your group to review and
   easier to troubleshoot.

6. **Start from examples that work.** Copy examples from course demonstrations,
   recipes, assignments, or trustworthy documentation, then change one element
   at a time. Render a new `.qmd` document immediately and again after meaningful
   changes so problems are found early and a readable output is available.

