
# install.packages(c("readr","dplyr","gt","scales"))  # if needed
library(readr)
library(dplyr)
library(gt)
library(scales)

# --- Load data (uncomment if needed) ---
# sleep <- read_csv("week01/data/sleep_tracking_Jan-Jun_2025_with_zip.csv",
#                   show_col_types = FALSE)

# --- Define columns ---
# Dimensions (categorical / discrete identifiers)
dims <- c(
  "Date", "DayOfWeek", "Month",
  "PersonName", "Gender",
  "Location", "SleepEventType",
  "StartTime", "EndTime",
  "ZipCode"
)

# Measures (numeric)
meas <- c(
  "TotalMinutes", "REMMinutes", "DeepMinutes",
  "LightMinutes", "SleepQuality"
)

# --- Take head(20) and select columns in desired order ---
tbl20 <- sleep %>%
  select(all_of(dims), all_of(meas)) %>%
  slice_head(n = 20)

# --- Format SleepQuality as a percentage for display ---
# (We’ll use gt::fmt_percent to keep the underlying numeric values numeric.)
# If SleepQuality is not numeric, coerce it first:
# tbl20 <- tbl20 %>% mutate(SleepQuality = as.numeric(SleepQuality))

# --- Tableau-like colors (approx) ---
tableau_blue  <- "#4E79A7"  # spanner/header for dimensions (Tableau-like blue)
tableau_green <- "#59A14F"  # spanner/header for measures  (Tableau-like green)
blue_fill     <- "#EAF2FB"  # light blue fill for dimension columns
green_fill    <- "#EAF6EA"  # light green fill for measure columns
header_text   <- "white"

# --- Build gt table with styling ---
gt_tbl <- tbl20 |>
  gt() |>
  # Spanners over the dimension and measure blocks
  tab_spanner(
    label = "Dimensions",
    columns = all_of(dims)
  ) |>
  tab_spanner(
    label = "Measures",
    columns = all_of(meas)
  ) |>
  # Format SleepQuality as percent with 2 decimals (e.g., 82.35%)
  fmt_percent(
    columns = "SleepQuality", decimals = 2
  ) |>
  # Shade dimension columns (light blue)
  tab_style(
    style = cell_fill(color = blue_fill),
    locations = cells_body(columns = all_of(dims))
  ) |>
  # Shade measure columns (light green)
  tab_style(
    style = cell_fill(color = green_fill),
    locations = cells_body(columns = all_of(meas))
  ) |>
  # Color the spanner headers to match Tableau-like pills
  tab_style(
    style = list(cell_fill(color = tableau_blue), cell_text(color = header_text)),
    locations = cells_column_spanners(spanners = "Dimensions")
  ) |>
  tab_style(
    style = list(cell_fill(color = tableau_green), cell_text(color = header_text)),
    locations = cells_column_spanners(spanners = "Measures")
  ) |>
  # Optional: make it a bit tighter and readable
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(6),
    column_labels.background.color = "white",
    column_labels.font.weight = "bold"
  ) |>
  cols_label(
    SleepEventType = "EventType",
    REMMinutes = "REMMinutes",
    DeepMinutes = "DeepMinutes",
    LightMinutes = "LightMinutes",
    TotalMinutes = "TotalMinutes",
    SleepQuality = "SleepQuality"
  )

gt_tbl
