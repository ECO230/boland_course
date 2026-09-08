# Download the immutable Great Lakes synthetic EHR encounter table.
# Every record is synthetic and describes no real patient.

library(arrow)

release_base <- "https://data.60land.com/project1/2026-fall/v1/health"
cache_dir <- file.path("data", "great_lakes_synthetic_ehr_2026_fall_v1")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

encounter_file <- file.path(cache_dir, "ehr_encounters.parquet")
if (!file.exists(encounter_file)) {
  download.file(
    paste0(release_base, "/ehr_encounters.parquet"),
    encounter_file,
    mode = "wb",
    quiet = FALSE
  )
}

encounters <- arrow::read_parquet(encounter_file)
dplyr::glimpse(encounters)
