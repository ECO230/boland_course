library(arrow)
library(dplyr)

release_root <- paste0(
  "https://data.60land.com/",
  "project1/2026-fall/v1/health/"
)

appointments <- read_parquet(
  paste0(release_root, "ehr_appointments.parquet")
)

satisfaction <- read_parquet(
  paste0(release_root, "ehr_patient_satisfaction.parquet")
)

appointments %>%
  count(appointment_status) %>%
  mutate(percent = 100 * n / sum(n))

satisfaction %>%
  filter(response_status == "responded") %>%
  group_by(encounter_class) %>%
  summarise(
    responses = n(),
    mean_rating = mean(overall_rating, na.rm = TRUE),
    .groups = "drop"
  )
