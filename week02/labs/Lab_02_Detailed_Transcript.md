# Lab 2 Detailed Reference Transcript

This is an edited, detailed record of the dictated Lab 2 demonstration. It is
kept as a private course-development reference and is not included in the
student assignment repository or public website render list.

## Introduce the two tidy datasets

Begin with the two tidy datasets in the Excel workbook. The same tables are
also available as individual CSV files. The workbook already joins the games
data to the plays data with `XLOOKUP`; the demonstration will first explain the
tables and then show how that join works.

The `Plays` worksheet contains the play-by-play table. Its original fields run
from columns A-AF. One row represents one play. `game_id` and `play_id`
together form the unique key. Either populated field can be used to count rows,
but a purely unique identifier for this extract requires the two fields
together.

The timing fields include `quarter` and `game_clock`. Be careful with time in
Excel and other programs: a football clock represents minutes and seconds,
while software may initially interpret a time-looking value as hours and
minutes or leave it as General text. Further analysis of time remaining or time
elapsed may require parsing the clock and combining it with the quarter.

The situation fields include down, yards to go, possession team, defensive
team, side of field, and `yardline_100`. Special-teams plays such as kickoffs do
not have a down, which is appropriate rather than an error. `yardline_100`
places the ball on a single 0-100 scale. This avoids the ambiguity of having two
physical 20-yard or 30-yard lines on the field. The play description records
what happened in plain text.

Beginning with `play_type`, the data describes whether the play was a run,
pass, punt, field goal, kickoff, and so forth; yards gained; shotgun or huddle
status; pass length and location; air yards and yards after catch; run location
and gap; completion, first down, sack, interception, fumble, penalty, penalty
yards, touchdown, and success indicators. Many values are legitimately blank
because a variable applies only to a certain type of play. Pass location does
not apply to a run, and penalty yards do not apply when no penalty occurred.

Expected points added and win probability added are situation-dependent
metrics. They move up or down as plays change the game state. A routine
two-yard run early in a game generally produces a smaller change than a score
or a high-impact play late in a close game.

Next, open the `Games` worksheet. One row represents one game, and `game_id` is
the unique key. The table contains 17 Minnesota regular-season games from the
2025 season and 18 fields: game ID, season, season type, week, date, weekday,
away team and score, home team and score, home-score margin, total points,
overtime, roof, surface, temperature, wind, and stadium. Temperature and wind
are generally meaningful only for outdoor games. The small team-season extract
is intended to make the relationships easy to see; the complete NFL release
contains many more games and plays.

## Explain the XLOOKUP fields

Return to `Plays` and select one of the joined game fields after column AF. The
formula is already complete. Click the formula to show its parameters:

- the lookup value is the current play row's `game_id`, such as A2;
- the lookup array is the `game_id` column on `Games`;
- the return array is the game field to bring back; and
- the optional not-found argument controls what happens when a key has no
  match.

Connect this to the fourth tidy-data principle: when table 1 and table 2 need to
be combined, they must share a key. The play's `game_id` finds one row on
`Games`, and `XLOOKUP` returns season, week, teams, scores, stadium conditions,
or another game-level value from that row.

The benefit is that each play can now be compared by grass versus turf, indoor
versus outdoor, temperature, stadium, or any other game-level attribute. Other
tables could be joined in the same way. Stadium data might add attendance or
crowd noise. Player data might add information about the passer, receiver, or
runner involved in a play.

The join changes what can be summarized safely. Each play repeats its game's
temperature, home score, total points, and other game-level values. Adding
total points from the joined play table would add the final score once for
every play and produce a wildly inflated season total. Sums of repeated
game-level values are inappropriate unless the analysis first returns to one
row per game. Some averages and medians may still be useful, but the analyst
must explain the weighting created by the play-level grain.

## Build the Counts PivotTable

Click inside each source range, choose Insert > Table, confirm that the range
has headers, and name the tables `Plays` and `Games`. Then insert a PivotTable
from `Plays` with the default option of placing it on a new worksheet. Keep each
PivotTable on its own worksheet. PivotTables can expand or contract when fields,
filters, or source data change. Multiple PivotTables on one sheet can overlap,
overwrite one another, and create unnecessary repair work. Rename the new
worksheet `Counts`.

Cells outside the PivotTable remain ordinary worksheet cells. When a cell in
the PivotTable is selected, the PivotTable Fields pane appears. Excel also
shows the Design and PivotTable Analyze tabs. If the fields pane is missing,
the PivotTable is probably not broken: click inside it, open PivotTable Analyze,
and turn on Field List.

Start with a count of `game_id`. Because `game_id` is populated for every play,
dragging it to Values and summarizing by Count returns the number of plays. A
categorical-looking field generally defaults to Count; a numeric field may
default to Sum. Change the Value Field Setting whenever the default does not
answer the intended question.

The field used for a count must be populated on every row that should be
included. A count of a field with blanks ignores those rows. Demonstrate this
by adding `penalty_yards` to Values. The extract contains 2,791 plays but only
221 populated penalty-yard values. Counting penalty yards therefore counts
plays with a recorded penalty-yard value, not all plays.

Add `play_type` to Rows. The PivotTable now shows extra points, field goals,
passes, punts, runs, and other play types. It also reveals categories for which
penalty data is or is not present. A quarterback kneel or spike, for example,
may have no assigned penalty yards in this sample.

Compare `penalty_yards` with the binary `penalty` field. Counting the penalty
field counts both populated `TRUE` and populated `FALSE` values, so it can be
close to a count of all plays. A numeric 0/1 version can be summed: zero
contributes nothing and one contributes one, making the sum a count of flagged
penalties. If the field is imported as logical `TRUE`/`FALSE`, verify how Excel
has stored it before relying on Sum. Penalty yards can also be summarized by
Sum for total penalized yards or Average for the average recorded penalty.

Investigate `no_play`. Return to the data, filter `play_type` to `no_play`, and
inspect the descriptions. Search or filter for penalty text and use a fill or
temporary visual mark if helpful. Many `no_play` records with penalties are
pre-snap infractions such as false starts; many without a penalty are timeouts.
This illustrates a quick way to investigate an unexpected PivotTable category
before making a stronger claim.

Finish the `Counts` table in this order:

1. Count `game_id` to establish the number of rows.
2. Add `game_id` a second time and use Show Values As to display the percent of
   column total, producing a frequency table.
3. Compare Count of `penalty_yards` with Count of `penalty` to show how missing
   values affect counts.
4. Compare a count with a sum of a valid numeric binary indicator to show why
   a count includes both zeros and ones while a sum counts only the ones.

## Build the Means PivotTable

Insert a second PivotTable on a new worksheet and rename it `Means`. Begin with
`play_type` in Rows and `penalty_yards` in Values. Right-click the value and
change Summarize Values By to Average. Then use Number Format to display two
decimal places for the entire value field.

Use a deliberately poor grouping to make a design point. Drag `yardline_100`
to Columns. Although it is numeric, anything in Rows or Columns is treated as a
set of discrete categories. With many distinct yard lines and few penalties in
each cell, the table becomes wide, sparse, and hard to interpret. `play_id`
would be worse: it would mostly recreate the raw data under arbitrary play
numbers. Remove the high-cardinality field. A correlation or another
continuous-variable analysis is usually more suitable.

`yards_to_go` can be a more manageable numeric grouping because it has fewer
common values. It could support an exploratory question about whether penalty
size changes near a first down, but it still needs a clear reason for treating
the number as discrete.

Place `quarter` in Columns to compare average penalty yards across play types
and quarters. An apparent increase, such as a larger average for `no_play` in
the fourth quarter, is a prompt for investigation. The table establishes that
the sample average changed; it does not establish why. Possible explanations
such as pressure, riskier play calling, or more pre-snap mistakes remain
uncertain until the underlying records and broader data are examined.

A cleaner primary example is average yards gained. Replace the Values field
with Average of `yards_gained` and filter `play_type` to `pass` and `run`, where
moving the ball is central to the play. Passing plays average roughly more
yards than running plays, with variation by quarter.

Add `shotgun` beneath `play_type` in Rows. A binary variable is flexible: it can
be treated as a category for group comparisons or, when properly coded 0/1, as
an indicator that can be averaged or summed. Compare pass and run averages for
shotgun and non-shotgun plays. Then replace `shotgun` with `pass_location` to
compare left, middle, right, and blank values. Blank locations deserve
investigation; they could represent plays for which location was not recorded
or did not apply.

Use the results to ask analytical questions without overclaiming. If left-side
and right-side passing averages move in opposite directions across quarters,
ask whether that is random variation, quarterback preference, defensive
adjustment, or another game-situation effect. The sample describes what
happened in these games; it does not establish whether the pattern holds across
teams or seasons.

Expected points added or win probability added can provide additional examples,
but they are not needed for the main means exercise. Their correct aggregation
depends heavily on the game situation. Keep the lab focused on average yards
gained.

## Build the Cross Tab PivotTable

Insert a third PivotTable on a new worksheet and rename it `Cross Tab`. Put
`play_type` in Rows, `quarter` in Columns, and Count of `game_id` in Values.

First use Show Values As > Percent of Grand Total. Each cell now describes the
share of every play in the dataset represented by that play-type-and-quarter
combination. The largest cell identifies the most common combination overall.

Next use Percent of Column Total. Each column adds to 100%, so the denominator
is all plays in that quarter. This version asks which play types were more or
less common within each quarter. Passing may be relatively more common in the
second quarter, while field-goal frequency may increase near the ends of
halves.

Then use Percent of Row Total. Each play-type row adds to 100%, so the
denominator is every play of that type. This version asks when a particular
play type occurred. It can show, for example, whether punts were relatively
less common in the fourth quarter, when teams may take greater risks on fourth
down.

Show Values As includes many other options, including differences, rankings,
and percentages. It can be applied to metrics other than counts, but that does
not guarantee a useful result. A percent of an average may not add to 100% and
may be difficult for an audience to interpret. Use percentages primarily on
counts unless the alternative metric and denominator have a clear meaning.
Prefer Show Values As to a hard-coded formula because the PivotTable
calculation refreshes with filters and new data.

## Demonstrate the median workaround

Median is not available in the standard PivotTable Value Field Settings. When
inserting the PivotTable, select Add this data to the Data Model. Create a new
measure, name it `Med Yards`, and use the DAX formula:

```text
Med Yards := MEDIAN(Plays[yards_gained])
```

The measure becomes available to use with `quarter`, `play_type`, and other
grouping fields. This is an awkward Excel workaround, especially across Excel
versions. The linked video demonstrates the interface in more detail. Tableau
and R provide median more directly.

## Reproduce the logic in Tableau

Use the individual CSV files. Connect to both tables and relate or join them on
`game_id`. Verify that the result remains at the play grain rather than
multiplying records.

Reproduce the main views:

- count `game_id` by `play_type` and apply a Percent of Total table
  calculation for the frequency table;
- use `play_type` and `quarter` with average or median yards gained;
- create a play-type-by-quarter text table and change the table-calculation
  direction to distinguish grand-total, within-quarter, and within-play-type
  percentages.

Check the computation direction and denominator rather than accepting the
default percentage. Tableau can produce the same kinds of confusing
percentages as Excel when a table calculation is applied without a clear
question.

## Reproduce the logic in R and Quarto

Use the individual CSV files in the Posit Cloud project. Keep the code direct:

- `left_join()` joins games to plays by `game_id`;
- `filter()` limits the analysis to relevant records or play types;
- `count()` creates frequency counts;
- `group_by()` defines comparison groups; and
- `summarise()` calculates means, medians, and other statistics.

The Quarto document shows the same sequence as the PivotTables. It verifies the
keys and row count, creates a frequency table, contrasts counts with missing
values, calculates mean and median yards for pass and run plays, and builds a
cross-tabulation with within-quarter percentages.

The point is not that code is inherently better than a PivotTable. Pointing,
clicking, and dragging are helpful while exploring. Once the intended order and
analysis are known, code and Quarto provide a repeatable record that can be
rerun when the data changes. That record also helps a reviewer understand the
steps behind a report or assess whether an agent-generated analysis follows the
intended logic.
