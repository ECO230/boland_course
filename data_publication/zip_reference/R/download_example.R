library(arrow)
library(dplyr)
library(stringr)

base_url <- "https://data.60land.com/common/2026-fall/v1/zip-reference"
data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

download_parquet <- function(filename) {
  destination <- file.path(data_dir, filename)
  if (!file.exists(destination)) {
    download.file(
      paste0(base_url, "/", filename),
      destination,
      mode = "wb",
      quiet = FALSE
    )
  }
  destination
}

normalize_zip5 <- function(x) {
  value <- str_trim(as.character(x))
  first_digits <- str_extract(value, "^\\d{1,5}")
  if_else(
    is.na(first_digits),
    NA_character_,
    str_pad(first_digits, width = 5, side = "left", pad = "0")
  )
}

zip_geography <- read_parquet(download_parquet("us_zip_geography.parquet"))
zip_context <- read_parquet(download_parquet("us_zip_context.parquet"))

# Replace this small example and zipcode with the project's data frame and ZIP field.
example_data <- tibble::tibble(
  location = c("La Crosse", "Chicago", "Saint Paul", "Adjuntas"),
  zipcode = c("54601", "60601-1000", 55101, 601)
)

analysis_data <- example_data |>
  mutate(zip5 = normalize_zip5(zipcode)) |>
  left_join(zip_geography, by = "zip5") |>
  left_join(zip_context, by = "zip5")

analysis_data |>
  summarise(
    rows = n(),
    matched_geography = sum(!is.na(state_abbreviation)),
    matched_population = sum(!is.na(population_2023))
  )
