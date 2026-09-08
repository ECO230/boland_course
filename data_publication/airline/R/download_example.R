library(arrow)
library(dplyr)

base_url <- "https://data.60land.com/project1/2026-fall/v1/airline"
cache_dir <- file.path("data", "airline")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

download_parquet <- function(filename) {
  destination <- file.path(cache_dir, filename)
  if (!file.exists(destination)) {
    download.file(file.path(base_url, filename), destination, mode = "wb")
  }
  destination
}

# Arrow can filter the flight file without loading every row into R memory.
flight_path <- download_parquet("airline_flights.parquet")
flights <- open_dataset(flight_path)

chicago_summary <- flights |>
  filter(origin_airport_code %in% c("ORD", "MDW")) |>
  group_by(origin_airport_code, marketing_carrier_code) |>
  summarize(
    flights = n(),
    cancellation_rate = mean(cancelled),
    delayed_15_rate = mean(arrival_delayed_15, na.rm = TRUE)
  ) |>
  collect()

print(chicago_summary)
