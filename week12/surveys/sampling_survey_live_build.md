# Sampling Survey Live Build

This is the recommended clean student-facing build for LimeSurvey.

Use this file when creating a fresh survey manually in the admin UI.

## Survey-Level Settings

Title:

`Sampling and Survey Lab`

Description:

`This short survey gives us class data for today's sampling and survey-design activities.`

Admin note:

- Keep the survey to about 5 to 7 minutes.
- Do not show internal question codes to students.
- Use required questions for the core sampling variables.
- Only turn on `Other` when it adds real value.

## Group 1: About You

Group title:

`About You`

Group description:

`These questions help us understand who is represented in today's response pool.`

### 1. Class standing

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

Build note:

- If you keep `Other`, use LimeSurvey's built-in `Other` option only.
- Do not create a second manual `Other` answer row.

### 2. Academic area

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

### 3. In-person attendance

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

### 4. Commute time

Type: Numerical input

Code: `commute_min`

Required: Yes

Question:

`About how many minutes does it usually take you to get to campus or class?`

Help:

`Enter a whole number of minutes. If you live on campus or are already nearby, enter your usual walking or travel time.`

Validation:

- minimum `0`
- integer only if available

### 5. Work hours

Type: Numerical input

Code: `work_hrs`

Required: Yes

Question:

`About how many hours do you work for pay in a typical week during the semester?`

Help:

`Enter 0 if you do not currently work for pay.`

Validation:

- minimum `0`
- integer only if available

### 6. Sleep last night

Type: Numerical input

Code: `sleep_hrs`

Required: Yes

Question:

`About how many hours did you sleep last night?`

Help:

`Use decimals if needed. For example, 6.5 means six and a half hours.`

Validation:

- minimum `0`

## Group 2: Learning and Tools

Group title:

`Learning and Tools`

Group description:

`These questions let us compare different survey formats and different kinds of variables.`

### 7. Coursework tools

Type: Multiple choice

Code: `tools`

Required: No

Question:

`Which tools or supports have you used for coursework this semester? Select all that apply.`

Choices:

- Excel
- Tableau
- R or Posit
- ChatGPT or another AI assistant
- YouTube or tutorial videos
- Tutoring center
- Study group
- Office hours
- Canvas course materials

Build note:

- Do not add `None of these` unless you set it up as exclusive.

### 8. Confidence interpreting statistics

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

### 9. Explain sampling bias

Type: Numerical input

Code: `bias_conf`

Required: Yes

Question:

`On a scale from 0 to 100, how confident are you that you could explain sampling bias to someone else?`

Help:

`0 = not confident at all, 50 = somewhat confident, 100 = completely confident`

Validation:

- minimum `0`
- maximum `100`
- integer only if available

### 10. Statistics feels...

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

Build note:

- This item is optional in the live survey.
- If the page starts to feel too long, remove this before removing the core sampling questions.

## Group 3: Open Response

Group title:

`Open Response`

Group description:

`These questions help us talk about text responses and niche populations.`

### 11. Obscure hobby or interest

Type: Long free text

Code: `hobby_text`

Required: No

Question:

`What is your most obscure hobby, interest, community, or skill?`

## Minimum Viable Live Survey

If you need the fastest clean version, build only these 9 items:

- class standing
- academic area
- in-person attendance
- commute time
- work hours
- sleep last night
- coursework tools
- confidence interpreting statistics
- explain sampling bias

Then add `hobby_text` if you want the open-response discussion.

## Things To Avoid

- visible variable codes like `sampling_vars`
- duplicate `Other` fields
- demo-only placeholder questions
- bad-question diagnosis items in the live student survey
- long batteries that make the survey feel like homework

## After Build

Before activation:

1. Preview every page as a student.
2. Check that required questions do not show stray `No answer` options.
3. Confirm that answer lists render correctly.
4. Export the finished survey and save it in this folder.
