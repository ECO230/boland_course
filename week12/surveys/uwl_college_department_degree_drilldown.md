# UWL College-Department-Degree Drilldown

This is a small, classroom-friendly drilldown structure based on the current UW-La Crosse academics site.

Source:

- [UWL Academic programs](https://www.uwlax.edu/academics/)

This is intentionally not a full catalog export. It is a compact demo version that is easier to build in LimeSurvey and easier for students to understand.

## Recommended Colleges

Use these top-level options:

- Arts, Social Sciences, and Humanities
- Business
- Science and Health
- Education

## Suggested Department -> Degree Structure

### Arts, Social Sciences, and Humanities

Departments:

- English
- History
- Psychology
- Art

Degrees:

- English
  - English major
  - English minor
  - Creative Writing minor
  - Professional and Technical Writing minor
- History
  - History major
  - History minor
  - Public History certificate
- Psychology
  - Psychology major
  - Psychology minor
  - Child and Youth Care minor
- Art
  - Art major
  - Art minor
  - Art Education major
  - Art History minor

### Business

Departments:

- Accountancy
- Finance
- Information Systems
- Management
- Marketing

Degrees:

- Accountancy
  - Accountancy major
  - Accountancy minor
- Finance
  - Finance major
  - Personal Financial Planning major emphasis
  - Risk, Insurance, and Financial Planning major emphasis
- Information Systems
  - Information Systems major
  - Information Systems minor
  - Business Analytics major
  - Business Analytics minor
- Management
  - Management major
  - Sustainable Business minor
- Marketing
  - Marketing major
  - Digital Marketing graduate certificate

### Science and Health

Departments:

- Biology
- Computer Science and Computer Engineering
- Mathematics and Statistics
- Microbiology
- Physics

Degrees:

- Biology
  - Biology major
  - Biology minor
  - Biomedical Science major emphasis
  - Environmental Science major emphasis
- Computer Science and Computer Engineering
  - Computer Science major
  - Computer Science minor
  - Computer Engineering major
  - Software Engineering graduate degree
- Mathematics and Statistics
  - Mathematics major
  - Mathematics minor
  - Statistics major
  - Data Science major
- Microbiology
  - Microbiology major
  - Microbiology minor
  - Clinical Laboratory Science major
- Physics
  - Physics major
  - Physics minor
  - Engineering Physics major

### Education

Departments:

- Educational Studies
- Teacher Education Programs

Degrees:

- Educational Studies
  - Teaching English to Speakers of Other Languages minor
  - Professional Development-Learning Community graduate degree
  - Reading Teacher and Reading Specialist graduate degree/certificate
- Teacher Education Programs
  - Early Childhood Education and Special Education major
  - Elementary/Middle Education major
  - Middle/High School Education major
  - Grades K-12 Education major

## Recommended LimeSurvey Pattern

Do not try to make one giant fully dynamic dropdown.

Instead:

1. `college`
   - one dropdown
2. one department dropdown per college
   - shown with relevance based on `college`
3. one degree dropdown per department
   - shown with relevance based on the department question

This is the most realistic no-code drilldown pattern in LimeSurvey.

## Suggested Question Codes

- `college`
- `dept_ash`
- `dept_bus`
- `dept_sci`
- `dept_edu`
- `deg_english`
- `deg_history`
- `deg_psych`
- `deg_art`
- `deg_account`
- `deg_finance`
- `deg_is`
- `deg_mgmt`
- `deg_marketing`
- `deg_bio`
- `deg_csce`
- `deg_mathstat`
- `deg_micro`
- `deg_physics`
- `deg_edstud`
- `deg_teachered`

## Example Relevance Equations

### Department question shown after college choice

For `dept_bus`:

```text
college == "BUS"
```

For `dept_sci`:

```text
college == "SCI"
```

### Degree question shown after department choice

For `deg_mathstat`:

```text
dept_sci == "MATHSTAT"
```

For `deg_marketing`:

```text
dept_bus == "MARKETING"
```

## Suggested Answer Codes

Keep codes short and stable.

Example college codes:

- `ASH`
- `BUS`
- `SCI`
- `EDU`

Example department codes:

- `ENGLISH`
- `HISTORY`
- `PSYCH`
- `ART`
- `ACCOUNT`
- `FINANCE`
- `IS`
- `MGMT`
- `MARKETING`
- `BIO`
- `CSCE`
- `MATHSTAT`
- `MICRO`
- `PHYSICS`
- `EDSTUD`
- `TEACHERED`

## Recommendation

For the live class survey, I would only use this if you really want to demonstrate drilldown specifically.

Otherwise, a single `academic area` or `primary program area` dropdown is still the cleaner live-survey choice.

This drilldown is best used as:

- an in-class demo of electronic survey branching, or
- an optional extra section, not a required core survey block
