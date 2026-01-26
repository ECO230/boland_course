
# install.packages(c("readr","dplyr","lubridate","stringr","gt"))  # if needed
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(gt)

# ---- 1) Load with explicit column types ----
# Adjust the path as needed (from your project root):
csv_path <- "week01/data/sleep_tracking_Jan-Jun_2025_with_zip.csv"

sleep <- read_csv(
  file = csv_path,
  col_types = cols(
    Date          = col_character(),   # force character so we control parsing
    DayOfWeek     = col_character(),
    Month         = col_character(),
    PersonName    = col_character(),
    Gender        = col_character(),
    Location      = col_character(),
    SleepEventType= col_character(),
    StartTime     = col_character(),   # force character
    EndTime       = col_character(),   # force character
    TotalMinutes  = col_integer(),
    REMMinutes    = col_integer(),
    DeepMinutes   = col_integer(),
    LightMinutes  = col_integer(),
    SleepQuality  = col_double(),
    ZipCode       = col_character()
  ),
  show_col_types = FALSE
)

# ---- 2) Fix Date if it came in as an Excel serial ----
# If Date looks like a 5-digit integer (e.g., "45295"), convert from Excel origin.
is_excel_serial <- function(x_chr) {
  all(str_detect(x_chr, "^\\d{4,5}$"), na.rm = TRUE)
}

if (is_excel_serial(sleep$Date)) {
  # Excel's typical origin is 1899-12-30 for most exports
  sleep$Date <- as.Date(as.numeric(sleep$Date), origin = "1899-12-30")
} else {
  # Otherwise, parse as ISO or m/d/Y
  # Try YYYY-MM-DD first, then m/d/Y fallback
  parsed1 <- suppressWarnings(ymd(sleep$Date, quiet = TRUE))
  parsed2 <- ifelse(is.na(parsed1),
                    suppressWarnings(mdy(sleep$Date, quiet = TRUE)),
                    parsed1)
  sleep$Date <- as.Date(parsed2)
}

# ---- 3) Robust parsers for StartTime/EndTime ----
# These can be "2025-01-03 22:10", or "9/7/2023 10:53 PM", etc.
parse_start_end <- function(x_chr) {
  x_chr <- str_trim(x_chr)
  # Try multiple common orders:
  # - "YYYY-MM-DD HH:MM"
  # - "m/d/Y H:M p"
  # - "Y-m-d H:M:S" (if seconds exist)
  # - "m/d/Y I:M:S p" etc.
  parse_date_time(
    x_chr,
    orders = c(
      "Y-m-d H:M",
      "Y-m-d H:M:S",
      "m/d/Y I:M p",
      "m/d/Y I:M:S p",
      "m/d/y I:M p",
      "m/d/y I:M:S p"
    ),
    tz = "UTC"   # keep neutral; display only time-of-day
  )
}

sleep$StartTime_dt <- parse_start_end(sleep$StartTime)
sleep$EndTime_dt   <- parse_start_end(sleep$EndTime)

# If you KNOW StartTime/EndTime are already ISO strings (e.g., "2025-01-03 22:10"),
# you could also use:
# sleep$StartTime_dt <- ymd_hm(sleep$StartTime, quiet = TRUE)
# sleep$EndTime_dt   <- ymd_hm(sleep$EndTime, quiet = TRUE)

# ---- 4) Formatters ----
fmt_time_12h <- function(dt) {
  # Handle NA gracefully:
  ifelse(
    is.na(dt), 
    NA_character_,
    str_to_upper(format(dt, "%I:%M %p")) |>
      str_replace("^0", "") |>
      str_replace_all("AM", "A.M.") |>
      str_replace_all("PM", "P.M.")
  )
}

fmt_pct <- function(x) paste0(round(100 * x), "%")

# ---- 5) Function to build the two tables for a chosen person & date ----
make_sleep_tables <- function(data, person = "Mike", date_input = "2025-01-03") {
  # Accept either character date or Date class
  date_val <- if (inherits(date_input, "Date")) date_input else as.Date(date_input)
  
  row <- data %>%
    filter(
      PersonName == person,
      Date == date_val,
      SleepEventType == "Main"
    ) %>%
    arrange(StartTime_dt) %>%
    slice(1)
  
  if (nrow(row) == 0) {
    stop("No 'Main' sleep row found for that person/date.")
  }
  
  categories_df <- tibble::tibble(
    Label = c("Name", "Date", "Gender", "Location", "ZipCode"),
    Value = c(
      row$PersonName,
      format(row$Date, "%m/%d/%Y"),
      row$Gender,
      row$Location,
      row$ZipCode
    )
  )
  
  numbers_df <- tibble::tibble(
    Label = c(
      "Start Time",
      "End Time",
      "R.E.M.",
      "Deep Sleep",
      "Light Sleep",
      "Total Sleep",
      "Quality"
    ),
    Value = c(
      fmt_time_12h(row$StartTime_dt),
      fmt_time_12h(row$EndTime_dt),
      paste0(row$REMMinutes, " Minutes"),
      paste0(row$DeepMinutes, " Minutes"),
      paste0(row$LightMinutes, " Minutes"),
      as.character(row$TotalMinutes),
      fmt_pct(row$SleepQuality)
    )
  )
  
  list(categories = categories_df, numbers = numbers_df)
}

# ---- 6) Example: reproduce your case (adjust date/person to a known row) ----
tables <- make_sleep_tables(sleep, person = "Mike", date_input = "2025-01-03")
categories_df <- tables$categories
numbers_df    <- tables$numbers

print(categories_df)
print(numbers_df)

# Optional pretty display
categories_gt <- categories_df |>
  gt() |>
  tab_header(title = md("**Categories**")) |>
  cols_label(Label = "", Value = "") |>
  tab_options(table.width = pct(60))

numbers_gt <- numbers_df |>
  gt() |>
  tab_header(title = md("**Numbers**")) |>
  cols_label(Label = "", Value = "") |>
  tab_options(table.width = pct(60))

# View in RStudio/Quarto:
# categories_gt
# numbers_gt
