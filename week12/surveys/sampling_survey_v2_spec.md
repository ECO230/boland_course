# Sampling Survey v2

This is the recommended clean version of the Week 12/13 class survey for LimeSurvey. It is intentionally shorter and cleaner than the experimental import draft.

## Design Goals

- 5-8 minute completion time
- enough variables for the sampling app
- enough question variety to demonstrate survey formats
- no obviously broken import artifacts
- human-readable group and question titles
- no internal variable names shown to students
- a survey that still feels like a real survey, not a software demo

## Important Change From v1

The live student survey should focus on:

1. useful class data for sampling demonstrations
2. a small number of well-designed question formats

The following should move out of the live survey and into the lecture/slides or a separate demo survey:

- bad question diagnosis items
- heat map placeholder
- graphic scale placeholder
- anything that feels like a software demo instead of a real survey

That keeps the student-facing survey cleaner and faster.

## Recommended Live Version

For the actual in-class survey, treat the following as the core live instrument:

- Q1-Q6
- Q8
- Q9
- Q11
- Q14

These items give you enough variation to support the sampling lab without making students feel like they are taking a long methods exam.

## LimeSurvey Build Principles

- Use readable group titles, not variable codes.
- Use short variable names internally, but never show them.
- Only enable "Other" when it is genuinely needed.
- For key variables, make the question required to avoid `No answer` clutter.
- For numeric questions, include instructions in the help text.
- Use one question per concept.
- Keep open-text items near the end.

## Recommended Survey Structure

### Group 1: About You

Group title:

`About You`

Group description:

`These questions help us understand who is represented in today's class response pool.`

#### Q1. Class Standing

Type: List (radio)

Code: `class_stand`

Required: Yes

Question:

`What is your current class standing?`

Answers:

- First-year
- Sophomore
- Junior
- Senior
- Graduate student
- Other

Notes:

- Turn on `Other` only if you want free text for the last option.
- If you use `Other`, remove a separate typed answer option.

#### Q2. Academic Area

Type: List (dropdown)

Code: `acad_area`

Required: Yes

Question:

`Which academic area best describes your primary program?`

Answers:

- Business
- Science or health
- Social sciences
- Arts or humanities
- Education
- Engineering or technology
- Undecided or exploring
- Other

Notes:

- This can later become a true drilldown if needed.
- For now, simple is better than fancy.

#### Q3. In-Person Attendance

Type: List (radio)

Code: `attend`

Required: Yes

Question:

`How often do you usually attend this class in person?`

Answers:

- Almost always
- Often
- Sometimes
- Rarely

#### Q4. Commute Time

Type: Short free text or numerical input

Code: `commute_min`

Required: Yes

Question:

`About how many minutes does it usually take you to get to campus or class?`

Help:

`Enter a whole number of minutes. If you live on campus or are already nearby, enter your usual walking or travel time.`

Validation:

- integer only
- minimum 0

#### Q5. Work Hours

Type: Short free text or numerical input

Code: `work_hrs`

Required: Yes

Question:

`About how many hours do you work for pay in a typical week during the semester?`

Help:

`Enter 0 if you do not currently work for pay.`

Validation:

- integer only
- minimum 0

#### Q6. Sleep Last Night

Type: Short free text or numerical input

Code: `sleep_hrs`

Required: Yes

Question:

`About how many hours did you sleep last night?`

Help:

`Use decimals if needed. For example, 6.5 means six and a half hours.`

Validation:

- numeric
- minimum 0

### Group 2: Learning and Tools

Group title:

`Learning and Tools`

Group description:

`These questions help us compare different question formats and different kinds of variables.`

#### Q7. Primary Way of Getting to Campus

Type: List (radio)

Code: `transport`

Required: Yes

Question:

`What is your primary way of getting to campus or class?`

Answers:

- Walk
- Bike or scooter
- Drive alone
- Carpool
- Bus or public transportation
- I do not usually come to campus
- Other

Notes:

- Keep this only if you want an extra demographic/behavior variable.
- If the survey starts to feel crowded, this can be removed before Q8, Q9, or Q11.

#### Q8. Coursework Tools

Type: Multiple choice

Code: `tools`

Required: No

Question:

`Which tools or supports have you used for coursework this semester? Select all that apply.`

Subquestions:

- Excel
- Tableau
- R or Posit
- ChatGPT or another AI assistant
- YouTube or tutorial videos
- Tutoring center
- Study group
- Office hours
- Canvas course materials

Notes:

- This is your select-all-that-apply example.
- Do not add a `None of these` option unless you enforce exclusivity.
- If the live survey starts to feel too long, keep this question and remove a fancier format instead.

#### Q9. Confidence Interpreting Statistics

Type: List (radio)

Code: `conf_interp`

Required: Yes

Question:

`How confident do you feel interpreting statistical results?`

Answers:

- Not at all confident
- Slightly confident
- Moderately confident
- Very confident
- Extremely confident

Notes:

- This is simpler than a whole Likert battery and still creates an ordered variable.
- This is one of the best questions to keep in the live survey because it is fast, interpretable, and useful for the app.

#### Q10. Statistics Feels...

Type: Array / semantic differential

Code: `stats_feel`

Required: No

Question:

`Right now, statistics feels...`

Rows:

- Easy ... Difficult
- Useful ... Useless
- Concrete ... Abstract

Columns:

- 1
- 2
- 3
- 4
- 5
- 6
- 7

Notes:

- This is the semantic differential example.
- Keep only three rows to reduce survey fatigue.
- This is optional in the live build if time is tight.

#### Q11. Explain Sampling Bias

Type: numerical input

Code: `bias_conf`

Required: Yes

Question:

`On a scale from 0 to 100, how confident are you that you could explain sampling bias to someone else?`

Help:

`0 = not confident at all, 50 = somewhat confident, 100 = completely confident`

Validation:

- integer only
- minimum 0
- maximum 100

#### Q12. What Matters Most?

Type: multiple numerical input

Code: `success`

Required: No

Question:

`Allocate 100 points across the things that most affect your success in this class.`

Subquestions:

- Time available
- Interest in the topic
- Prior math or statistics background
- Quality of examples
- Group or project support
- Confidence with software

Notes:

- This is the constant-sum example.
- After building, enforce total = 100 in LimeSurvey if possible.
- If this feels too clunky in practice, remove it from the live survey and keep it as an in-class screenshot/demo.
- This is the first candidate to remove from the live student-facing survey.

### Group 3: Open Response

Group title:

`Open Response`

Group description:

`These questions help us talk about text responses and niche populations.`

#### Q13. Unique Trait

Type: Long free text

Code: `unique_trait`

Required: No

Question:

`What is something unique about you that a normal survey would probably not know to ask?`

#### Q14. Obscure Hobby

Type: Long free text

Code: `hobby_text`

Required: No

Question:

`What is your most obscure hobby, interest, community, or skill?`

Notes:

- These questions help connect text boxes to judgment and snowball sampling.
- If you keep only one open-text item, keep `Q14`.

## Suggested Final Survey Length

If you want the shortest useful version, keep:

- Q1-Q6
- Q8
- Q9
- Q11
- Q14

That gives you:

- subgroup variables
- a multi-response item
- an ordered confidence measure
- a numeric scale with anchors
- an open text item for niche populations

If you want the best classroom balance between clean data and survey-format variety, add:

- Q10

but still leave out Q12 unless you specifically want a constant-sum example live.

## Recommended Variables For The Sampling App

Best app variables from this survey:

- `class_stand`
- `acad_area`
- `attend`
- `commute_min`
- `work_hrs`
- `sleep_hrs`
- `transport`
- `conf_interp`
- `bias_conf`
- `hobby_text`

## Things To Fix In The Current Imported Survey

If you keep editing the current survey instead of rebuilding:

1. Rename question groups to human-readable titles.
2. Hide or replace any visible internal codes.
3. Remove duplicate `Other` answer structures.
4. Remove `No answer` on questions that should be required.
5. Rebuild broken answer lists manually instead of trusting the import.
6. Remove placeholder/demo items that do not belong in the live student survey.

## Recommendation

Do not spend much more time trying to repair the imported version.

The cleaner path is:

1. Create a fresh survey in LimeSurvey.
2. Build the shortest useful live version manually first.
3. Add optional items only if the survey still feels clean in preview.
4. Export the finished version from LimeSurvey.
5. Treat that export as the new canonical survey artifact.
