csv_path <- here("week01", "data", "sleep_tracking_Jan-Jun_2025_with_zip.csv")

# 1) Load data --------------------------------------------------------------


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




# packages
library(dplyr)
library(gt)
library(scales)
library(htmltools)  # for html() labels

# --- Define columns ---
dims <- c(
  "Date", "DayOfWeek", "Month",
  "PersonName", "Gender",
  "Location", "SleepEventType",
  "StartTime", "EndTime",
  "ZipCode"
)

meas <- c(
  "TotalMinutes", "REMMinutes", "DeepMinutes",
  "LightMinutes", "SleepQuality"
)

# --- Build the 20-row sample in the order you want ---
tbl20 <- sleep %>%
  select(all_of(dims), all_of(meas)) %>%
  slice_head(n = 20)

# --- Add a decorative left column (empty body) to carry vertical label + spine ---
# Give it a simple single-space name to keep it non-stub and easy to reference
tbl20_with_tag <- tbl20 %>%
  mutate(` ` = "") %>%
  relocate(` `, .before = everything())

last_row <- nrow(tbl20_with_tag)   # <-- use this instead of n() in gt

# --- Render the gt table ---
gt_tbl <-
  tbl20_with_tag |>
  gt() |>
  # Spanners over the DIMENSION/MEASURE blocks (skip the decorative left column " ")
  tab_spanner(label = "Dimensions", columns = all_of(dims)) |>
  tab_spanner(label = "Measures →", columns = all_of(meas)) |>
  # Format % nicely
  fmt_percent(columns = "SleepQuality", decimals = 2) |>
  # Light fills (optional)
  tab_style(
    style = cell_fill(color = "#EAF2FB"),
    locations = cells_body(columns = all_of(dims))
  ) |>
  tab_style(
    style = cell_fill(color = "#EAF6EA"),
    locations = cells_body(columns = all_of(meas))
  ) |>
  # Narrow width for the decorative left column
  cols_width(` ` ~ px(30)) |>
  # Put a vertical "Dimensions" label in that left header cell
  cols_label(
    ` ` = html(
      '<div style="
           writing-mode: vertical-rl;
           transform: rotate(180deg);
           font-weight: 700;
           color: #1F497D;
           letter-spacing: 0.5px;
         ">
         Dimensions
       </div>'
    )
  ) |>
  # Draw a thick blue spine down the entire left edge
  tab_style(
    style = cell_borders(sides = "left", color = "#1F497D", weight = px(6)),
    locations = list(
      cells_title(groups = "title"),
      cells_column_spanners(spanners = everything()),
      cells_column_labels(columns = everything()),
      cells_body(columns = everything())
    )
  ) |>
  # Add a small down arrow at the bottom of the left decorative column
  text_transform(
    locations = cells_body(columns = ` `, rows = last_row),   # <-- explicit integer
    fn = function(x) {
      html('<div style="color:#1F497D; font-size:18px; line-height:1;">&#8595;</div>')
    }
  ) |>
  # Spanner styling (Tableau-ish)
  tab_style(
    style = list(cell_fill(color = "#4E79A7"), cell_text(color = "white", weight = "700")),
    locations = cells_column_spanners(spanners = "Dimensions")
  ) |>
  tab_style(
    style = list(cell_fill(color = "#59A14F"), cell_text(color = "white", weight = "700")),
    locations = cells_column_spanners(spanners = "Measures →")
  ) |>
  # Optional: a green top line to visually extend "Measures →"
  tab_style(
    style = cell_borders(sides = "top", color = "#59A14F", weight = px(3)),
    locations = cells_column_spanners(spanners = "Measures →")
  ) |>
  # Minimal slide chrome
  tab_options(
    table.width = pct(100),
    table.font.size = px(12),
    data_row.padding = px(6),
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(0),
    table.border.bottom.width = px(0),
    heading.border.bottom.width = px(0),
    column_labels.border.top.width = px(0),
    column_labels.border.bottom.width = px(0),
    table_body.hlines.width = px(0)
  )

gt_tbl
