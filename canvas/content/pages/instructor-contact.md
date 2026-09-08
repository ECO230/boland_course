---
schema_version: 1
key: instructor-contact-info
canvas_type: page
visibility: course_only
required_values:
  - instructor_email
  - office_hours
  - student_hours_url
---

The best way to reach me is by email. I normally respond within one business
day.

- **Email:** [{{ instructor_email }}](mailto:{{ instructor_email }})
- **Student hours:** {{ office_hours }}
- **Online meeting:** [Join student hours on Zoom]({{ student_hours_url }})
