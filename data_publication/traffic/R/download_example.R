base_url <- paste0(
  "https://data.60land.com/",
  "project1/2026-fall/v1/traffic/"
)

files <- c(
  crashes = "chicago_traffic_crashes.parquet",
  vehicles = "chicago_traffic_vehicles.parquet",
  people = "chicago_traffic_people.parquet",
  crash_analysis = "chicago_traffic_crash_analysis.parquet"
)

dir.create("data", showWarnings = FALSE)

download_course_table <- function(table_name) {
  stopifnot(table_name %in% names(files))
  destination <- file.path("data", files[[table_name]])
  if (!file.exists(destination)) {
    download.file(
      paste0(base_url, files[[table_name]]),
      destination,
      mode = "wb",
      quiet = TRUE
    )
  }
  arrow::read_parquet(destination)
}

# Start with the one-row-per-crash table. Download the normalized tables only
# when the research question requires a vehicle- or person-level join.
crash_analysis <- download_course_table("crash_analysis")

stopifnot(
  anyDuplicated(crash_analysis$crash_record_id) == 0,
  min(crash_analysis$crash_datetime) >= as.POSIXct("2018-01-01", tz = "America/Chicago"),
  max(crash_analysis$crash_datetime) < as.POSIXct("2026-01-01", tz = "America/Chicago")
)

crash_analysis
