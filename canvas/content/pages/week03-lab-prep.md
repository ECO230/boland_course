---
schema_version: 1
key: labprep-for-lab-3
canvas_type: page
visibility: course_only
required_values:
  - lab_03_excel_training_url
---

## Lab Prep for Lab 3

This optional practice introduces the chart-building steps used during Lab 3.
There is nothing to submit. Complete as much as you can before class and keep
your working file available during the lab.

[Download the Lab 3 preparation workbook]({{ lab_03_excel_training_url }}).
It contains a small airline carrier-month table. The in-class lab uses a
separate, larger Chicago traffic crash sample.

**Start in Excel:** Open the downloaded file in Excel and read its
`Instructions` worksheet before connecting to it in Tableau. The directions
inside the workbook explain the preparation activity.

### 1) Excel practice

1. Open the downloaded preparation workbook in Excel.
2. Select the `Instructions` worksheet and read it before starting the charts.
3. Follow those directions to create a monthly line chart and one carrier
   comparison using the `Airline Data` worksheet.
4. Save your working copy as `.xlsx` so that charts, formulas, and worksheets
   are retained. Use this same workbook for the Tableau practice below.

Microsoft provides current instructions for
[creating charts](https://support.microsoft.com/en-us/excel/creating-charts-from-start-to-finish)
and
[creating PivotCharts](https://support.microsoft.com/en-US/Excel/get-started/create-a-pivotchart).

### 2) Tableau practice

1. Open Tableau and connect to the preparation workbook you opened in Excel.
2. Select the `Airline Data` worksheet.
3. Confirm that `month` is a date, `marketing_carrier_code` is a dimension,
   and the rate fields are measures.
4. Recreate one Excel chart and build a scatterplot of
   `arrival_delay_15_rate` and `cancellation_rate`.
5. Save your work as a Tableau Packaged Workbook (`.twbx`).

Tableau's current examples are collected under
[Build Common Chart Types](https://help.tableau.com/current/pro/desktop/en-us/dataview_examples.htm).

<div style="border-left: 4px solid #198754; background-color: #f0f8f3; padding: 12px 16px; margin: 16px 0;">
<p><strong>Check the analytical question</strong></p>
<p>
The software may suggest a chart, but you are responsible for deciding whether
its visual encoding matches the comparison you want the audience to make.
</p>
</div>

<div style="border-left: 4px solid #0d6efd; background-color: #eef5ff; padding: 12px 16px; margin: 16px 0;">
<p><strong>Preparation is not the in-class lab</strong></p>
<p>
This workbook is for practice before class. Do not begin the Chicago traffic
activity until the instructor starts the in-class lab.
</p>
</div>
