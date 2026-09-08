# Legacy attachment source audit

This file records the rationale behind the operational decisions in
`legacy-attachments.csv`. The CSV remains the machine-readable source used by
the Canvas migration workflow.

## Decision meanings

- `remove`: omit the attachment without replacement.
- `replace_with_external_link`: publish a Canvas module link to the identified
  source instead of copying the legacy attachment.
- `recreate_qmd`: create a repository-owned Quarto replacement at the recorded
  path before publication.
- `retain_private_course_copy`: the item is a licensed book excerpt. Keep it
  out of public Git and retain it only in the restricted Canvas course when the
  course's licensed use has been confirmed.
- `retain_solution_file`: retain the course solution artifact in restricted
  instructor/course storage; do not publish it on the public website.
- `remove_duplicate`: omit the duplicate and use the named canonical item.

## Instructor decisions resolved in this pass

| Legacy item | Decision | Source or replacement notes |
| --- | --- | --- |
| `About Me.pdf` | Remove | Obsolete and not needed. |
| `BasicDataTypes.pdf` | External link | *Data + Design*, Chapter 1, "Basic Data Types." |
| `Understanding Data.pdf` | Recreate QMD | Course-authored handout using the old CEO Salary example; rebuild with current ECO 230 datasets. |
| `DataAggregation.pdf` | External link | *Data + Design*, Chapter 2, "About Data Aggregation." |
| `Measures Of Variability.pdf` | Recreate QMD | The University of Leicester guide has no stable first-party URL. The new course page preserves the core concepts, corrects the normal-distribution rule, replaces obsolete spreadsheet functions with R, and uses current ECO 230 data. |
| `Working with percentages.pdf` | Recreate QMD | The University of Leicester guide has no stable first-party URL. The new course page teaches denominators, row/column/table percentages, percentage change, and percentage points with current ECO 230 data. |
| `Considerations in DataViz.pdf` | External link | Excerpts from *Data + Design*, Chapters 14, 17, and 18. Link to the open book rather than distributing a custom excerpt. |
| `The Right Graph.pdf` | External link | Stephen Few, "Eenie, Meenie, Minie, Moe: Selecting the Right Graph for Your Message," hosted by Perceptual Edge. |
| `Graph_Choice_Chart.pdf` | External link | The Maine Data Literacy Project's Graph Choice Chart, hosted by Participatory Science and licensed CC BY-SA 4.0. The Tuva copy is a mirror, not the primary source. |
| `AdvancedInterp.pdf` | Recreate QMD | Course-created slide export; rebuild with current ECO 230 datasets and current visualization examples. |
| `BusinessWriting.pdf` | External link | Course-created excerpt/summary of Mary Cullen's "87 Advanced Business Writing Tips"; link to the original article. |
| `More Slide Design Tips` | Remove duplicate | The archived Visage article is an eight-page print capture with a dead first-party URL. Its durable principles - one message, reduced cognitive load, contrast, restrained color, meaningful visuals, and audience focus - are taught in `week04/week_04.qmd`. |
| `Problem Definition Examples.pdf` | Recreate QMD | Course-created worksheet, partly adapted from Chapter 2 of *Keeping Up with the Quants*. Credit Davenport and Kim in the replacement. |
| `Problem Definition Examples-1.pdf` | Remove duplicate | Byte-for-byte duplicate of `Problem Definition Examples.pdf`. |
| `Analysis Plan Outline.pdf` | Recreate QMD | Course-created outline. It is related to the problem-framing framework but contains no source attribution claiming it is a direct book excerpt. |
| `StatsJargon.pdf` | Retain private course copy | Page 56 from Chapter 4 of Wiley's *Data Fluency*. It is a licensed textbook excerpt, not a free-standing open handout. |
| `Lab3_Hints.pptx` | Recreate QMD | Rebuild the Tableau screenshot hints as an accessible Quarto page. |
| `Lab3_Completed_EXT.twbx` | Retain solution file | Keep as a restricted solved Tableau workbook. |
| `Week 12.1.pptx` | Remove duplicate | The old decision-making, hypothesis, independent/dependent variable, and measurement-scale material is now taught in the current Week 5 deck, with measurement-scale foundations in Weeks 1-2. |
| `Bias-Error in Survey Research.pdf` | Recreate QMD | Replace the four-page course handout with an accessible Week 12 page that distinguishes sampling error, coverage, self-selection, nonresponse, response bias, and measurement error. |
| `Week 12.2.pptx` | Remove duplicate | The old sampling and survey-research presentation has been replaced by `week12/week_12.qmd`. |
| `D14_Slides.pptx` | Recreate QMD | Replace the eight-slide ethics mini-deck with `week14/week_14.qmd`. The new deck retains the four decision scenarios and expands the teaching structure around consent, privacy, fairness, transparency, accountability, safeguards, and appeals. |
| `ECO 230 Syllabus S26_S012.pdf` | Remove duplicate | Use the section-aware canonical source at `syllabus/syllabus.qmd`; do not maintain a separate legacy PDF attachment. |
| `project_1_timeline.pdf` | Remove duplicate | Use the existing canonical page at `syllabus/project_1_timeline.qmd`. |
| `project_1_prompt.pdf` | Remove duplicate | Use the existing canonical page at `syllabus/project_1_prompt.qmd`. |
| `project_2_prompt.pdf` | Remove duplicate | Use the existing canonical page at `syllabus/project_2_prompt.qmd`. |
| `rmarkdown_cheat_sheet.pdf` | External link | Link to Posit's maintained, accessible R Markdown cheatsheet page. It also provides the official PDF download when needed. |
| `Tidyverse_cheat_sheet.pdf` | External link | Link to Posit's maintained cheatsheet index. There is no single current whole-tidyverse sheet; the index provides the current dplyr, tidyr, data-import, ggplot2, and related references. |

## Additional external readings identified

| Legacy item | Identified source | Recommended treatment |
| --- | --- | --- |
| `KeepingUpWithTheQuants_Ch1.pdf` | Davenport and Kim, *Keeping Up with the Quants*, Chapter 1 | Licensed excerpt; retain privately or link to library/publisher access. |
| `tidy-data.pdf` | Hadley Wickham, "Tidy Data," *Journal of Statistical Software* 59(10) | Link to the open journal article. |
| `Data Organization in Spreadsheets.pdf` | Karl Broman and Kara Woo, "Data Organization in Spreadsheets" | Link to the authors' CC BY article site. |
| `PieCharts_EXCERPT.pdf` | Stephen Few, "Save the Pies for Dessert" | Link to the author-hosted Perceptual Edge PDF. |
| `DataVisGuide_EXCERPT.pdf` | HubSpot and Visage, *Data Visualization 101: How to Design Charts and Graphs* | Link to the publisher-hosted PDF. |
| `Chapter 1 Clear and to the Point.pdf` | Stephen M. Kosslyn, *Clear and to the Point*, Chapter 1 | Licensed Oxford University Press excerpt; retain privately or link to library/publisher access. |
| `Telling Compelling Stories with Numbers.pdf` | Stephen Few, "Statistical Narrative: Telling Compelling Stories with Numbers" | Link to the author-hosted Perceptual Edge PDF. |
| `Making Data Meaningful UN guide_EXCERPTS.pdf` | UNECE, *Making Data Meaningful Part 1* | Link to the official UNECE publication page. |
| `Keeping Up with the Quants Chapter 2.1.pdf` | Davenport and Kim, *Keeping Up with the Quants*, Chapter 2 | Licensed excerpt; retain privately or link to library/publisher access. |
| `nonresponse bias.pdf` | Pew Research Center, "What Low Response Rates Mean for Telephone Surveys" | Link to the current first-party article. |
| `DataAndDesign - SurveyContent.pdf` | *Data + Design*, Chapters 3 and 4 | Link to the open textbook rather than distributing a custom excerpt. |
| `DataFluency_Chapter4 (2).pdf` | Gemignani et al., *Data Fluency*, Chapter 4 | Licensed Wiley excerpt; retain privately or link to library/publisher access. |
| `EthicalConsiderations.pdf` | Zikmund et al., *Business Research Methods*, 9th ed. | Licensed Cengage excerpt; retain privately or link to library/publisher access. |

## Licensing notes

*Data + Design* is licensed Creative Commons BY-NC-SA. The Graph Choice Chart
states a Creative Commons BY-SA 4.0 license. The academic articles and public
agency resources above have stable first-party pages. Publisher book excerpts
are not public-source replacements simply because a scanned or library-exported
PDF exists in the old Canvas cartridge.
