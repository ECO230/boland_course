---
schema_version: 1
key: lab-3-video-guide-tableau-and-excel
canvas_type: page
visibility: course_only
required_values:
  - lab_03_data_url
kaltura_partner_id: 2370711
video_placement: inline
videos:
  - title: "1. Connect Tableau to the Lab 3 data"
    entry_id: "1_m54bimsj"
  - title: "2. Ranking: count crashes by cause"
    entry_id: "1_9nbvv17j"
  - title: "3. Nominal comparison: average injuries"
    entry_id: "1_6boale46"
  - title: "4. Time series: monthly crash counts"
    entry_id: "1_5rlmqfrg"
  - title: "5. Part-to-whole: percentages and their denominators"
    entry_id: "1_ktwl7eh0"
  - title: "6. Relationship: temperature and dew point"
    entry_id: "1_iv34qksm"
  - title: "7. Distribution: histograms and box plots"
    entry_id: "1_inchbtz6"
  - title: "8. Maps: individual crashes and ZIP codes"
    entry_id: "1_6n4dxamo"
  - title: "9. Excel: PivotCharts and scatterplots"
    entry_id: "1_2otopmwa"
  - title: "10. Optional R companion: charts with ggplot2 in Posit Cloud"
    entry_id: "1_lqzvbpkk"
---

## Build your Lab 3 workbook

Use these recordings and steps to build the seven Tableau views from Lab 3,
then reproduce at least three views in Excel. Pause after each short sequence,
make the changes in your own workbook, and check the result before continuing.

- [Download the Lab 3 crash extract]({{ lab_03_data_url }})
- [Open the Lab 3 task instructions](/courses/{{ canvas_course_id }}/pages/lab-3-creating-basic-visualizations-in-excel-and-tableau)
- [Help opening a Tableau packaged workbook](/courses/{{ canvas_course_id }}/pages/video-tutorial-opening-a-tableau-file)
- [Open the optional Lab 3 R project in Posit Cloud](https://posit.cloud/spaces/3173/content/12970507)

The [R companion walkthrough and transcript](#r-companion) show the same
visualization ideas in ggplot2. This is an optional worked example; you are
not expected to write the R code from scratch.

Use `chicago_traffic_crashes_sample.csv`, the 12,000-row classroom sample with
one row per crash. The airline workbook from Lab Prep 3 is a separate practice
file. These counts describe the sample, not official Chicago crash totals or
crash rates per driver, trip, or mile.

<div style="border-left: 4px solid #0d6efd; background-color: #eef5ff; padding: 12px 16px; margin: 16px 0;">
<p><strong>Our in-class walkthrough</strong></p>
<p>Build these views with the instructor: use quarters for part-to-whole,
temperature for distribution, and causes with at least 30 crashes for average
injuries. The Lab 3 prompt follows this sequence. Optional extensions are
identified below; pause the recordings whenever you need to repeat a step.</p>
</div>

### Jump to a step

1. [Connect to the data](#connect)
2. [Ranking](#ranking)
3. [Nominal comparison](#nominal)
4. [Time series](#time-series)
5. [Part-to-whole](#part-to-whole)
6. [Relationship](#relationship)
7. [Distribution](#distribution)
8. [Map](#map)
9. [Excel](#excel)
10. [Optional R companion and transcript](#r-companion)
11. [Save and check your work](#finish)

---

<h2 id="connect">1) Connect to the data</h2>

<!-- video:1_m54bimsj -->

1. Download the crash extract to a location you can find again. Open Tableau
   and choose **Connect > Text file**, then select the CSV. If you are starting
   with an instructor-supplied `.twbx`, open that workbook instead.
2. On the **Data Source** tab, inspect the preview. Keep `crash_id` as text,
   `crash_datetime` as date/time, and numerical measurements as numbers.
   Preserve decimal precision for coordinates and weather measurements.
3. Check that `latitude` and `longitude` have the appropriate geographic roles.
   If needed, use the field menu to assign them.
4. Open **Sheet 1**. Find the automatically generated table count measure,
   usually named after `chicago_traffic_crashes_sample.csv` with **Count**.
   This guide calls it **crash count**. Do not sum the crash identifier.
5. Drag crash count to **Text** on the Marks card. Before adding filters, the
   result should be **12,000**. Clear the sheet to begin the first chart.

**Check:** If a field turns into nulls after changing its data type, undo the
change and inspect the original values. Blue pills mean discrete fields;
green pills mean continuous fields. That distinction is separate from whether
a field is a dimension or a measure.

**File reminder:** A `.twb` stores workbook instructions and data connections.
A `.twbx` packages the workbook with supporting local data or extracts. Save
the packaged version when you need a portable copy.

---

<h2 id="ranking">2) Rank the most common causes</h2>

<!-- video:1_9nbvv17j -->

**Question:** Which reported causes occur most often in this sample?

1. Rename the worksheet **Ranking**. Drag `primary_contributory_cause` to
   **Rows** and crash count to **Columns**.
2. Set Marks to **Bar**, or select the two fields and use **Show Me > horizontal
   bars**. Sort descending by crash count.
3. Exclude missing causes, **UNABLE TO DETERMINE**, and **NOT APPLICABLE**.
   Select those bars and choose **Exclude**, or use the cause field on Filters.
   Keep a note that these records have been removed.
4. For this fixed sample, select the ten largest remaining cause bars and
   choose **Keep Only**, as shown in the recording. Confirm that ten causes
   remain. This saves a selected list; it does not automatically find a new
   top ten if the underlying data changes.
5. Give the chart a question-based title and label the numerical axis
   **Number of crashes in the sample**.

**Check:** Bar lengths show integer counts, not sums of an identifier or injury
counts. Excluding unknown causes changes which crashes the ranking describes.

**Filter scope:** Keep the top-ten selection local to **Ranking**. Remove it
from the duplicated nominal sheet before comparing average injuries. A new
worksheet should not silently inherit a top-ten-cause filter.

---

<h2 id="nominal">3) Compare average injuries across causes</h2>

<!-- video:1_6boale46 -->

**Question:** How does average injury count differ across the selected causes?

1. Duplicate **Ranking** and rename the copy **Nominal Comparison**. Remove
   the ranking's cause filter so you begin with all causes.
2. Replace crash count on Columns with `injuries_total`.
3. Open that pill's menu and change **Measure > Sum** to **Average**. Sort the
   bars by the average.
4. Drag crash count to **Tooltip** or **Label** so you can inspect the number
   of crashes behind each average. Use a simple, consistent bar color.
5. Drag crash count to **Filters** and choose **At least 30**. Exclude
   **NOT APPLICABLE** from the causes and document both restrictions.
6. Label the numerical axis **Average injuries per crash**. Inspect a category's
   average and its count together before interpreting it.

**Why check the count?** A 30-crash cutoff is an exploratory choice, not a guarantee that
an average is reliable or representative.

**Check:** The pill reads `AVG(injuries_total)`, not `SUM`. Explain why a rare
cause with a high average needs caution. Injury counts can be skewed, and
reported causes and the classroom sample do not establish causation or risk
per trip.

---

<h2 id="time-series">4) Plot crash counts over time</h2>

<!-- video:1_5rlmqfrg -->

**Question:** How do monthly crash counts change over the sample period?

1. Create **Time Series**. Place crash count on **Rows** and `crash_datetime`
   on **Columns**.
2. Open the date pill's menu and choose the **continuous Month** option
   (green). Use **Line** marks. Each month-year should occupy its own position
   on a continuous timeline.
3. Check the first and last dates. A discrete month name by itself can combine
   January across years; use that only if your question is about seasonality.
4. To follow the video, place `reported_weather_condition` on **Color**.
   Start with all conditions, then optionally filter out **CLEAR** and
   **UNKNOWN** to examine the remaining lines more closely.
5. Identify any exclusions in the title or caption. The saved demonstration
   excludes **CLEAR**; the recording also discusses excluding **UNKNOWN**.

**Check:** The axis runs chronologically across years. A smaller number of
crashes in a month is not, by itself, evidence that driving was safer: exposure
and sampling also matter. Weather categories describe reported conditions;
the chart does not prove that weather caused an individual crash.

---

<h2 id="part-to-whole">5) Check percentages before making stacked bars</h2>

<!-- video:1_ktwl7eh0 -->

**Question:** Within each quarter, what share of crashes falls
in each `most_severe_injury` category?

1. Create **Part to Whole**. Build a text table first: put
   `most_severe_injury` on **Rows**, `crash_datetime` on **Columns**, and crash
   count on **Text**. Choose the discrete **Quarter** date part (blue) to
   compare Q1-Q4 across years, as in the completed example.
2. Exclude null injury categories as demonstrated and state that the
   denominator is crashes with a recorded injury category in each quarter.
3. On crash count, select **Quick Table Calculation > Percent of Total**.
   Edit the calculation to compute **Table (Down)** for this text-table layout.
   Each column should total approximately 100%, allowing for rounding.
4. Convert to stacked bars: keep discrete Quarter on Columns, put the calculated
   count on Rows, and move `most_severe_injury` to **Color**. Choose **Bar** and
   ensure marks are stacked.
5. Recheck the calculation after rearranging the view. With **Specific
   Dimensions**, address `most_severe_injury` and partition by Quarter:
   calculate across injury categories separately within each quarter.
   Verify that every complete stack reaches 100%.
6. Practice **Hide** on **NO INDICATION OF INJURY** and **REPORTED, NOT EVIDENT**,
   as in the recording. Keep their contribution to the denominator and label
   the reduced display. Show the hidden categories again to verify the total.

<div style="border-left: 4px solid #b36b00; background-color: #fff7e6; padding: 12px 16px; margin: 16px 0;">
<p><strong>Hide and Exclude answer different questions</strong></p>
<p>The completed demonstration hides <strong>NO INDICATION OF INJURY</strong>
and <strong>REPORTED, NOT EVIDENT</strong> after calculating percentages.
Hidden categories still contribute to that calculation, so the visible bars
no longer total 100%. Excluding those categories instead recalculates shares
among the remaining records. First show all included
categories and verify the full 100% total before trying the hidden-category
variation. Label any reduced display and its denominator explicitly.</p>
</div>

**Check:** The share is within a quarter, not a percentage of the whole
dataset or a percentage across quarters for one injury category. This view
combines the same quarter across years. A year-quarter timeline would answer
a different question.
For another explanation of calculation direction, see Tableau's
[table calculation guide](https://help.tableau.com/current/pro/desktop/en-us/calculations_tablecalculations.htm).

---

<h2 id="relationship">6) Make a scatterplot with one mark per crash</h2>

<!-- video:1_iv34qksm -->

**Question:** How are temperature and dew point related in the weather
observations linked to these crashes?

1. Create **Relationship**. Put `temperature_f` on **Columns** and `dew_point_f`
   on **Rows**.
2. If you see one point with huge coordinates, Tableau is summing the
   measurements. Choose **Analysis > Aggregate Measures** to turn aggregation
   off, as in the video. Alternatively, add the unique `crash_id` to **Detail**.
3. Use circle marks and reduce their opacity to make overlapping points easier
   to see. Label both axes in degrees F.
4. Put `weather_station_name` before temperature on Columns to make separate
   panels, matching the saved example. Color or a station filter is another
   option. Exclude null station names if using the panel comparison and note
   that exclusion.
5. Optionally show a trend line to inspect the pattern. A fitted line is an
   exploratory summary; a full regression analysis is not required here.

**Check:** Each mark represents a crash with the needed weather values, not an
overall sum. Multiple crashes can share the same station-hour observation, so
overlapping marks are not independent weather measurements. Explain that unit
of analysis when describing the chart.

---

<h2 id="distribution">7) Compare distributions</h2>

<!-- video:1_inchbtz6 -->

**Video example:** Explore `temperature_f` with a histogram, then compare its
distribution across `reported_weather_condition` categories with box plots.

1. Create **Distribution**. Select `temperature_f` and use **Show Me > Histogram**.
   Tableau creates a bin field. Edit the bin size to see how it changes the
   display; the recording tries 5-degree bins as one useful option.
2. Keep this exploration on a duplicate sheet if you want to retain it. To
   build the box plots, start a fresh view with `reported_weather_condition`
   on **Rows** and `temperature_f` on **Columns**.
3. Turn **Aggregate Measures** off, or put `crash_id` on Detail. Add a box plot
   with **Show Me** or drag **Box Plot** from the **Analytics** pane to the cells.
   A box based only on one aggregate per category is not the intended result.
4. Compare the median, the middle 50% of observations (the box from the 25th
   to the 75th percentile), and the spread. Inspect unusually distant points.

Label the numerical axis **Temperature (degrees F)**. If you retain separate
histograms for weather categories, use the same bin widths and boundaries
across groups.

**Check:** Each group contains individual crash-linked observations. Outliers
deserve investigation, not automatic removal. A histogram describes the data's
shape; it does not force the data to be normal. Airport weather measurements
and officer-reported conditions need not match exactly at every crash location.

---

<h2 id="map">8) Plot individual crashes on a map</h2>

<!-- video:1_6n4dxamo -->

**Question:** Where are crashes in your selected subset located?

1. Create **Map**. Place `longitude` on **Columns** and `latitude` on **Rows**,
   or select both fields and choose **Show Me > Symbol Map**.
2. Drag `crash_id` to **Detail**. The single average location should separate
   into individual crash marks. The saved example keeps aggregation on and
   uses the ID to provide the detail.
3. To match the example, place `trafficway_type` on **Color**. Adjust mark size
   and opacity, and choose a readable background map.
4. Optionally filter to a manageable date, cause, injury, or geographic subset.
   Explain which records the filter removes. Inspect any null or unknown
   coordinate indicator rather than silently treating those crashes as mapped.

**Optional video extension:** On a duplicate sheet, use the ZIP code geographic
field to create a filled map and color areas by crash count. This changes a
mark from an individual crash to a ZIP-code area. The final supplied workbook
retains the point map.

**Check:** The points fall in the Chicago area; latitude and longitude are not
reversed. Areas with more sampled crashes are not necessarily riskier per
traveler. Counts can also reflect traffic volume, area size, and the sample.

---

<h2 id="excel">9) Reproduce at least three views in Excel</h2>

<!-- video:1_2otopmwa -->

1. Open the same crash CSV and immediately **Save As > Excel Workbook (`.xlsx`)**.
   Format the data as a table with headers. Keep the original data sheet.
2. Create a **PivotTable/PivotChart** on a new sheet. For ranking, put
   `primary_contributory_cause` in Rows/Axis and `crash_id` in Values.
   Confirm **Count of crash_id**. Filter the unwanted causes, sort the pivot
   values largest to smallest, and keep the same ten causes as in Tableau.
   Use a horizontal bar chart.
3. For average injuries, create another PivotTable with causes
   in Rows and `injuries_total` in Values. In **Value Field Settings**, choose
   **Average**. Add Count of `crash_id` to the table, filter that count to at
   least 30, and exclude NOT APPLICABLE. Remove any inherited top-ten selection
   so this matches the Tableau nominal comparison.
4. For a time series, put `crash_datetime` in Rows/Axis and Count of `crash_id`
   in Values. Group dates by **Years and Months** to preserve year-month order,
   then use a line chart. If dates do not group, check for text or invalid
   dates. Weather condition can be a legend field or a filter.
5. For a scatterplot instead, select the raw `temperature_f` and `dew_point_f`
   columns and insert a regular **XY Scatter** chart. A PivotChart cannot be
   converted to the required XY scatterplot. Confirm temperature is X and dew
   point is Y, and keep paired observations together when handling blanks.

**Check:** Use matching filters and aggregations when comparing Excel and
Tableau. If results disagree, inspect count versus sum, average versus total,
date grouping, and excluded categories first. Keep charts two-dimensional and
remove decoration that makes comparisons harder. A map in Excel is optional.
Microsoft's [PivotTable and PivotChart overview](https://support.microsoft.com/en-us/excel/overview-of-pivottables-and-pivotcharts)
explains the restrictions on PivotChart types.

---

<h2 id="r-companion">10) Optional R companion: ggplot2 in Posit Cloud</h2>

<!-- video:1_lqzvbpkk -->

[Open the Lab 3 R companion project](https://posit.cloud/spaces/3173/content/12970507)
and follow along with the roughly 13-minute walkthrough. The project uses the same
12,000-row crash sample as the Tableau and Excel lab. It illustrates what
the analysis looks like in code; writing it from scratch is not a requirement.

1. **Open and render the guide.** Open `Lab_03_R_Companion_IP.qmd` and click
   **Render**. The output is an HTML page with the code beside its results.
   The instructor's source project is already set up. If your copy reports
   missing packages, run `source("project_setup.R")` once in the Console.
2. **Identify the packages and style.** `ggplot2` draws the charts; packages
   such as `dplyr` and `tidyr` prepare the data. A minimal theme and a
   Tableau-like palette provide a starting style that you can change.
3. **Check the data before the chart.** Read the crash CSV, create month and
   quarter fields, and confirm 12,000 rows and 12,000 distinct crash IDs.
   R makes the date-grouping steps explicit rather than adding a date hierarchy
   automatically.
4. **Trace the ranking example in two parts.** First, the data pipeline
   filters causes, counts crashes, and keeps the ten largest groups. `|>`
   passes each result to the next step. Second, `ggplot()` selects that summary
   table, `aes()` maps count and cause to the axes, and `geom_col()` draws bars.
   The `+` signs add layers, labels, and scales. Printing `ranking_plot` displays
   the completed chart.
5. **Compare the other views.** The nominal example includes a table because
   the static chart has no Tableau-style hover details. The time series uses
   small multiples, with separate vertical scales; compare magnitudes only
   after checking those scales. The quarterly example calculates shares
   before hiding categories so the original denominator is retained.
6. **Explore distributions and the map.** Histogram bins and box plots are
   built-in ggplot2 chart types. The point map plots the supplied longitude
   and latitude without a street basemap. It needs no mapping service or key.
7. **Run earlier chunks before experimenting.** Use **Run All Chunks Above**
   before running a later example. An object-not-found error can mean that the
   packages or data have not yet been loaded. Try a small change, such as a
   bar color or bin width, and rerun the affected chunk. You may use coding
   assistance to explain syntax or troubleshoot; check what the code does
   and keep analytical decisions and interpretations your own.

**Check:** Identify the summary table, the axis mappings, and the geometry in
one example. The R companion adds no new submission requirement to Lab 3.

<h3 id="r-transcript">R walkthrough transcript</h3>

<!-- R_TRANSCRIPT_START -->
<p>English automatic captions from the instructor recording (September 22, 2026). Caption text is preserved with line breaks joined into timed paragraphs; automatic transcription can misrecognize package names and other words. Use the walkthrough above and the companion code to check technical spelling.</p>
<p><strong>00:00</strong> Alright, so let&#x27;s take a look at what this looks like in R. So if we open up our lab 3 R project, we can see an example of what this would potentially look like if it was coded in R. And just like the lab 2, this is just shown as an example to show, okay, that same sort of analysis, what does that code look like? There&#x27;s no expectation here yet that</p>
<p><strong>00:30</strong> you&#x27;d be writing this manually, but if you wanted to try it in R, there&#x27;s some examples here to get you started. Also, any coding agent, ChatGPT, Copilot, Claude, is going to do a pretty good job at getting you started on that code as well. I want a chart, a bar chart that shows these categories, this metric. Help me put that together. is uh is a good way to get a feel for</p>
<p><strong>01:03</strong> what this looks like so we&#x27;ll work in lab 3 our companion here uh the q md the q quarto markdown file uh we&#x27;ll open that up um and then if we hit render uh right away we should be able to see what that output looks like and this one is a little bit different it renders to an HTML web page instead of a PDF you could render this as a PDF if you wanted but these are I think these show up a little bit</p>
<p><strong>01:33</strong> better in in a browser so it&#x27;ll take a couple seconds here and you can kind of run through and see what&#x27;s happening here so you know this stuff was already done you know obviously if you&#x27;re opening up the file it&#x27;s in there I&#x27;ve already gone through it and loaded that setup information, so you should be all be ready to go there. And then this walks through what code it ran. So it loaded the libraries that it</p>
<p><strong>02:03</strong> needed for this. So ggplot2 is the library that most examples that you&#x27;ll see, it&#x27;s kind of a go-to for visualizing data in R. And then some of the other libraries that we&#x27;ve loaded before, dplyr and tidy are, you those things that make those data pipelines work are in there as well. It does a little bit of setup work here to set a theme. You know I asked it to do a theme similar to Tableau, but there&#x27;s all sorts of</p>
<p><strong>02:35</strong> different examples out there of different kind of looks and feels that are that are pretty easy to implement here. And then this is also loading a color palette similar to what tableau uses by default and again all these options can be can be changed if needed so first one here is going to do some data setup so it&#x27;s going to read in our our sample file that we used in in tableau and that we used in an excel</p>
<p><strong>03:06</strong> and it&#x27;s going to create a couple of data points that it needs for the chart so you know here you can see it is creating a month variable that doesn&#x27;t exist in here a little bit more work to get this going in in R than it was in tableau tableau sees that date automatically builds in you know your year your month all that sort of stuff in R you have to usually specifically</p>
<p><strong>03:36</strong> call those out but as you can see here there&#x27;s some built -in functions that are able to do that. We don&#x27;t have to go in and manually try to figure out, okay, what was September, what was October, whatever. There&#x27;s a month function that can get at that same with our quarter function, just to duplicate the views and things that we&#x27;re seeing in Tableau. And we can see that we have 12,000 rows here, 12,000 crash IDs, you know, same sort of thing that we saw in Tableau, because it&#x27;s the exact same</p>
<p><strong>04:07</strong> file that&#x27;s feeding this. So first one here, skip past the code and go to the output, you know, so you can see, okay, this is the same sort of chart that we built in Tableau, same sort of thing that we built in Excel. And here&#x27;s the code that&#x27;s doing that. So kind of two parts here. First part is going to be building the actual data. It&#x27;s going to feed the chart. And when you see examples or when you ask an agent to do this, It&#x27;s usually going to follow a similar practice, which</p>
<p><strong>04:40</strong> makes sense if you think about it. The only stuff that&#x27;s showing up in the chart is like the summary that we&#x27;re trying to show. So it makes sense to kind of split that out to say, okay, this is how we&#x27;re creating the summary. You know, same sort of thing that we would see in a pivot table or a tabular view of that data. you know, this is basically building the data that would feed that table and then on top of that we&#x27;re layering to say, okay, instead of showing that as a table, I want you to show this as a plot, as a bar plot</p>
<p><strong>05:11</strong> you know, bar chart, you know, here&#x27;s how to set that up so if we run through here we can see what&#x27;s happening, so we see our data pipes here you know, saying, okay, remember, okay, that pipe is saying, okay, whatever happens in this line, take the result and do you know, further action. It keeps flowing down that pipe from one segment to the other. So take our crashes data, filter that to say okay, you know, if there&#x27;s any missing primary cause, get rid of that. So this is NA</p>
<p><strong>05:43</strong> function is checking to see, okay, was there null data there, missing data there? And then this exclamation point negates that to say, okay, the opposite of that. So basically saying, okay, anywhere where there is data. Primary cause in, you know, selecting from this list, unable to determine, not applicable. It is giving us a count of primary cause and giving it the name crash count. And then this slice max function is saying,</p>
<p><strong>06:13</strong> okay, give me the top 10 and don&#x27;t bring in any ties. So basically the first 10 that it sees. Next one here then is where that ggplot2 library kicks in is we are making this plot. So these are always going to go in the same order. So it starts with our data. So take that ranking data that we just built, you know, all this, built this, and then we assigned it to that variable ranking, which is basically the data that would fill that table. So saying take that.</p>
<p><strong>06:46</strong> This AES stands for like aesthetics, you know, just say okay, this is the, this is what we&#x27;re going to show, basically. What&#x27;s your X variable? What&#x27;s your Y, um, or not variable, but what&#x27;s your X dimension? What&#x27;s your Y dimension? So in X, we&#x27;re going to have crash count, you know, our, our measure here, um, you know, that we created up above. And then here we&#x27;re going to have, um, our primary contributory cause, and then</p>
<p><strong>07:18</strong> it&#x27;s going to reorder it by crash count to say okay you know sort it in in descending order is going to make us our basic plot but if you just ran this it would run without an error but it&#x27;s not going to show anything nothing&#x27;s going to show up in your plot it&#x27;s basically going to set that canvas when these plus signs here are then adding different layers onto that so how do we want to visualize that we&#x27;re going to add geometry and</p>
<p><strong>07:50</strong> this is a column you know there&#x27;s geom line geom point there&#x27;s all sorts of different options there but it&#x27;s saying okay how do you want to show this with a column what&#x27;s and then we can set our options there to say okay fill it with tableau colors one which is blue we change this to two it would change to a different color on to that so that&#x27;s going to give us our bar chart onto that add a scale for y you know with our labels</p>
<p><strong>08:20</strong> add a scale for x with our um you know our um our count and then add a title onto that you know with our you know whatever we want to say there and then last one here is and that then it gets assigned to this ranking plot and then that&#x27;s what it gets printed out so a little bit more complex than what we&#x27;ve been dealing with before but if you kind of follow line by line and think through okay how would I how would I build</p>
<p><strong>08:51</strong> this in in tableau or build this in in excel you can kind of see where some of those different elements are coming in you know if any of these were confusing or you know okay how to scale why discrete work you know you can look them up any agent is going to be able to kind of parse that out and give you a description okay here&#x27;s how here&#x27;s how that comes into into play here uh nominal comparison you know shown here you know it&#x27;s going to be a very similar process what&#x27;s our data you know what&#x27;s that plot look like um you</p>
<p><strong>09:22</strong> know and then this is showing okay it doesn&#x27;t have that hover over you know functionality that the tableau does where it&#x27;s going to show the actual value so it&#x27;s building you know this table here to show okay here&#x27;s what here&#x27;s what&#x27;s included with uh with those different things uh time series you know does a different approach here instead of doing each line differently it does a concept called small multiples where we&#x27;re taking okay all of our different um our different um categories of weather here it</p>
<p><strong>09:52</strong> puts it on you know the same size graph and just does a lot of them together so it&#x27;s easy to kind of scan through and see okay which one which one is different which one is trending up which one is trending down uh part to whole you know it shows some calculations there to you know figure out what does that um what does that calculation look like to get those categories um and then you know from there shows okay how to how to specifically call out okay here&#x27;s here&#x27;s why I want these different things colored</p>
<p><strong>10:22</strong> so it&#x27;s it&#x27;s showing you know a couple different options of how to you know how to do this how to you know hide that data like we did in tableau um you know how to do bins how to do box plots you know some of these are a little bit easier than others because some of these are just built-in chart types in ggplot too um so you can kind of run through those you know for mapping you know you can&#x27;t do anything too wild here unless you pull in like an external mapping service like google maps or something like</p>
<p><strong>10:52</strong> that to get or open maps to get street view or, um, you know, streets and points of interest, things like that. Um, but it is using, you know, because we have that, um, latitude, longitude, you know, it&#x27;s kind of plotting out what that looks like. And we can kind of see what the, you know, that pattern makes sense that, okay, these are, these are your roads feeding into, uh, into downtown sort of thing. You can plot, you know, right on those, where, where are those accidents, um, occurring. Uh, so like I said, that&#x27;s, you know just</p>
<p><strong>11:22</strong> meant as an example to show you know kind of what&#x27;s possible in tableau and you get much more complex you know charts in this so you know look through examples if you want to play around with this you could um you know any coding agent is going to be able to hop in there and help you out with that as well um you can kind of play with these as well you know as with anything you can in that markdown file itself you can run that code uh kind of line by line here just remember that if you try to run something here and these lines</p>
<p><strong>11:55</strong> haven&#x27;t been loaded we don&#x27;t have data loaded yet it&#x27;s going to fail you can run these you have to go and run you know each of these kind of individually to get that data loaded or you can click that you know this button here run all chunks above we&#x27;ll load that in and then when you run that it should uh should come out here and then you can start to start to play around with that so this is provided as example you know this isn&#x27;t going to be you know something that is going to</p>
<p><strong>12:26</strong> be a main focus of the class but I did want to you know kind of give you a feel for okay what are the what are the libraries that come into play what&#x27;s kind of the process that comes into play if we&#x27;re running this in um an arm.</p>
<!-- R_TRANSCRIPT_END -->

---

<h2 id="finish">11) Save and check your work</h2>

Save your Tableau work as a **Tableau Packaged Workbook (`.twbx`)** and your
Excel work as **`.xlsx`**. Close and reopen the packaged workbook to check that
its data travels with it. Do not save the Excel file back to CSV; CSV cannot
retain charts and multiple worksheets.

- You have seven Tableau views and at least three reproduced in Excel.
- Each view has a question-based title, meaningful units, and a checked aggregation.
- The nominal comparison uses average injuries, at least 30 crashes per cause,
  and excludes NOT APPLICABLE; the ranking's top-ten filter has been removed.
- The complete part-to-whole chart totals 100% within every quarter; you can
  explain why hiding categories leaves a visible total below 100%.
- Your distribution comparison uses `temperature_f` across weather categories.
- Scatterplot and map marks have the intended level of detail.
- Filters and hidden categories are documented and apply to the intended sheets.
- You can explain the sample, each chart's denominator, and what one mark represents.

Keep both files available for class. Upload them only if the accompanying
Canvas assignment requests a submission. This guide adds no separate submission.
