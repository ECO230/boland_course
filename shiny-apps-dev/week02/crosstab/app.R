# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Load data ONCE at startup ----
COURSE_ROOT <- "/data/junior/boland_course"
acc_path <- file.path(COURSE_ROOT, "shared", "data", "accident_wi.csv")

acc <- read_csv(acc_path, show_col_types = FALSE) %>%
  mutate(
    # Clean raw time strings (handle blanks + extra whitespace)
    Start_Time_clean = na_if(str_squish(Start_Time), ""),
    End_Time_clean   = na_if(str_squish(End_Time), ""),
    
    # Robust parse (handles "m/d/Y H:M" and "m/d/Y H:M:S")
    start_time = parse_date_time(
      Start_Time_clean,
      orders = c("mdy HM", "mdy HMS"),
      tz = "America/Chicago"
    ),
    end_time = parse_date_time(
      End_Time_clean,
      orders = c("mdy HM", "mdy HMS"),
      tz = "America/Chicago"
    ),
    
    # ---- Season from start_time ----
    season = case_when(
      month(start_time) %in% 3:5  ~ "spring",
      month(start_time) %in% 6:8  ~ "summer",
      month(start_time) %in% 9:11 ~ "fall",
      TRUE                        ~ "winter"
    ),
    
    # ---- Time-of-day bins from start_time ----
    time_of_day = case_when(
      hour(start_time) >= 6  & hour(start_time) < 12 ~ "morning",
      hour(start_time) >= 12 & hour(start_time) < 18 ~ "daytime",
      hour(start_time) >= 18 & hour(start_time) < 24 ~ "evening",
      TRUE                                           ~ "night"
    ),
    
    # ---- Duration (minutes) ----
    duration_mins = as.numeric(difftime(end_time, start_time, units = "mins")),
    
    # Optional: ordered factors for nicer tables
    season = factor(season, levels = c("spring", "summer", "fall", "winter")),
    time_of_day = factor(time_of_day, levels = c("night", "morning", "daytime", "evening"))
  ) %>%
  # Keep ONLY the limited set specified (PLUS start_time for date filtering)
  select(
    ID, Severity,
    start_time,
    `Distance(mi)`,
    `Temperature(F)`,
    `Wind_Chill(F)`,
    `Humidity(%)`,
    `Pressure(in)`,
    `Visibility(mi)`,
    Wind_Direction,
    `Wind_Speed(mph)`,
    `Precipitation(in)`,
    Weather_Condition,
    Sunrise_Sunset,
    season,
    time_of_day,
    duration_mins
  )

# ---- Helpers ----
as_cat <- function(x) {
  if (is.factor(x)) return(as.character(x))
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE")))
  if (inherits(x, "Date")) return(as.character(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.character(as.Date(x)))
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  as.character(x)
}

handle_missing_cat <- function(x, missing_as_level = TRUE) {
  x <- as_cat(x)
  if (missing_as_level) {
    x[!nzchar(x) | is.na(x)] <- "(Missing)"
    return(x)
  } else {
    x[!nzchar(x)] <- NA_character_
    return(x)
  }
}

collapse_top_n <- function(x, n = 12, other_label = "Other") {
  x <- as.character(x)
  if (length(x) == 0) return(x)
  
  tab <- sort(table(x), decreasing = TRUE)
  levs <- names(tab)
  
  if (length(levs) <= n) return(x)
  
  keep <- levs[seq_len(n)]
  x2 <- ifelse(x %in% keep, x, other_label)
  
  # Re-order so most frequent appear first, "Other" last
  tab2 <- sort(table(x2), decreasing = TRUE)
  levs2 <- names(tab2)
  if (other_label %in% levs2) {
    levs2 <- c(setdiff(levs2, other_label), other_label)
  }
  factor(x2, levels = levs2)
}

# Frequency table with cumulative columns + sorting + optional Total row
make_freq_df <- function(x, sort_mode = c("cat_asc", "freq_desc", "freq_asc"), add_total = FALSE) {
  sort_mode <- match.arg(sort_mode)
  x <- as.character(x)
  n <- length(x)
  
  if (n == 0) {
    return(data.frame(
      Level = character(0),
      Frequency = integer(0),
      Percent = character(0),
      CumFrequency = integer(0),
      CumPercent = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  tab <- table(x)
  df <- data.frame(
    Level = names(tab),
    Frequency = as.integer(tab),
    stringsAsFactors = FALSE
  )
  
  # Sorting
  if (sort_mode == "cat_asc") {
    df <- df[order(df$Level), ]
  } else if (sort_mode == "freq_desc") {
    df <- df[order(-df$Frequency, df$Level), ]
  } else {
    df <- df[order(df$Frequency, df$Level), ]
  }
  
  total_n <- sum(df$Frequency)
  pct_num <- 100 * df$Frequency / total_n
  cum_freq <- cumsum(df$Frequency)
  cum_pct  <- cumsum(pct_num)
  
  df$Percent <- sprintf("%.1f%%", pct_num)
  df$CumFrequency <- cum_freq
  df$CumPercent <- sprintf("%.1f%%", cum_pct)
  
  if (isTRUE(add_total)) {
    df <- rbind(
      df,
      data.frame(
        Level = "Total",
        Frequency = total_n,
        Percent = "100.0%",
        CumFrequency = total_n,
        CumPercent = "100.0%",
        stringsAsFactors = FALSE
      )
    )
  }
  
  df
}

# Convert count table to display df (counts) with optional margins
tab_to_df_counts <- function(tab, add_margins = FALSE) {
  if (add_margins) tab <- addmargins(tab)
  df <- as.data.frame.matrix(tab, stringsAsFactors = FALSE)
  df <- cbind(Row = rownames(df), df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  df
}

# Build percent table WITH meaningful total row/col based on counts (not sums of percents)
percent_df_with_margins <- function(tab, type = c("table", "row", "col"), digits = 1, add_margins = FALSE) {
  type <- match.arg(type)
  if (is.null(tab) || sum(tab) == 0) return(NULL)
  
  rnames <- rownames(tab)
  cnames <- colnames(tab)
  r <- nrow(tab)
  c <- ncol(tab)
  
  grand_total <- sum(tab)
  row_sums <- rowSums(tab)
  col_sums <- colSums(tab)
  
  # Base percent matrix (no margins)
  p <- matrix(NA_real_, nrow = r, ncol = c, dimnames = list(rnames, cnames))
  
  if (type == "table") {
    p[,] <- 100 * tab / grand_total
  } else if (type == "row") {
    # row-conditional
    for (i in seq_len(r)) {
      denom <- row_sums[i]
      p[i, ] <- if (denom > 0) 100 * tab[i, ] / denom else rep(NA_real_, c)
    }
  } else {
    # col-conditional
    for (j in seq_len(c)) {
      denom <- col_sums[j]
      p[, j] <- if (denom > 0) 100 * tab[, j] / denom else rep(NA_real_, r)
    }
  }
  
  # Add margins meaningfully
  if (isTRUE(add_margins)) {
    total_row_name <- "Total"
    total_col_name <- "Total"
    
    p2 <- matrix(NA_real_, nrow = r + 1, ncol = c + 1,
                 dimnames = list(c(rnames, total_row_name), c(cnames, total_col_name)))
    
    # Fill interior
    p2[seq_len(r), seq_len(c)] <- p
    
    if (type == "table") {
      # Row totals and col totals are % of grand total
      p2[seq_len(r), total_col_name] <- 100 * row_sums / grand_total
      p2[total_row_name, seq_len(c)] <- 100 * col_sums / grand_total
      p2[total_row_name, total_col_name] <- 100
    }
    
    if (type == "row") {
      # For each row, total col is 100 (conditional distribution sums to 100)
      p2[seq_len(r), total_col_name] <- ifelse(row_sums > 0, 100, NA_real_)
      
      # Total row: overall (based on grand total counts) so it sums to 100 across columns
      p2[total_row_name, seq_len(c)] <- 100 * col_sums / grand_total
      p2[total_row_name, total_col_name] <- 100
    }
    
    if (type == "col") {
      # For each column, total row is 100 (conditional distribution sums to 100)
      p2[total_row_name, seq_len(c)] <- ifelse(col_sums > 0, 100, NA_real_)
      
      # Total col: overall (based on grand total counts) so it sums to 100 down rows
      p2[seq_len(r), total_col_name] <- 100 * row_sums / grand_total
      p2[total_row_name, total_col_name] <- 100
    }
    
    p <- p2
  }
  
  # Convert to df + format
  df <- as.data.frame.matrix(p, stringsAsFactors = FALSE)
  df <- cbind(Row = rownames(df), df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  
  for (j in seq_along(df)) {
    if (names(df)[j] == "Row") next
    df[[j]] <- ifelse(
      is.na(df[[j]]),
      "",
      sprintf(paste0("%.", digits, "f%%"), as.numeric(df[[j]]))
    )
  }
  df
}

# ---- Variables (teaching-friendly categorical set) ----
cat_vars <- c(
  "Severity",
  "Wind_Direction",
  "Weather_Condition",
  "Sunrise_Sunset",
  "season",
  "time_of_day"
)
cat_vars <- intersect(cat_vars, names(acc))
cat_vars <- unique(cat_vars)

# ---- Safe date defaults ----
start_min <- suppressWarnings(as.Date(min(acc$start_time, na.rm = TRUE)))
start_max <- suppressWarnings(as.Date(max(acc$start_time, na.rm = TRUE)))
if (!is.finite(as.numeric(start_min)) || !is.finite(as.numeric(start_max))) {
  start_min <- Sys.Date() - 30
  start_max <- Sys.Date()
}

# ---- UI ----
ui <- fluidPage(
  tags$style("
    h2{margin-top:0.2rem;}
    .small{opacity:0.80;font-size:1.05rem;}
    .box{background:#f7f7f7;border-radius:12px;padding:12px;margin-bottom:10px;}
    .tbl-wrap{overflow-x:auto;}

    /* BIGGER tables */
    table.simple {
      border-collapse: collapse;
      width: 100%;
      font-size: 1.22rem;
      background: white;
      border-radius: 12px;
      overflow: hidden;
    }
    table.simple th, table.simple td {
      border: 1px solid #e6e6e6;
      padding: 10px 12px;
      text-align: right;
      white-space: nowrap;
    }
    table.simple th:first-child, table.simple td:first-child { text-align: left; }
    table.simple thead th {
      background: #fafafa;
      font-weight: 800;
    }

    .callout-left {
      border-left: 6px solid #2B6CB0;
      background: rgba(43,108,176,.06);
      border-radius: 12px;
      padding: 12px;
      margin-bottom: 10px;
    }
    details.param { background:#f7f7f7; border-radius:12px; margin-bottom:10px; }
    details.param > summary {
      cursor: pointer; font-weight: 800;
      padding: 12px; border-radius:12px;
      list-style: none;
    }
    details.param > summary::-webkit-details-marker { display:none; }
    details.param .content { padding: 12px; padding-top: 0; }
  "),
  
  h2("WI Accidents — Frequency Tables & Cross Tabs (Limited Dataset)"),
  div(class="small",
      "Demonstrate: frequency tables, contingency tables, marginal counts, and row/column/table percentages — all from the same filtered data."),
  hr(),
  
  fluidRow(
    column(
      width = 4,
      
      tags$details(
        class = "param",
        open = FALSE,
        tags$summary("Parameters (click to expand)"),
        div(
          class = "content",
          
          dateRangeInput(
            "dates", "Start date range",
            start = start_min,
            end   = start_max,
            min   = start_min,
            max   = start_max
          ),
          
          hr(),
          
          selectInput("row_var", "Rows", choices = cat_vars, selected = if ("time_of_day" %in% cat_vars) "time_of_day" else cat_vars[1]),
          selectInput("col_var", "Columns", choices = cat_vars, selected = if ("season" %in% cat_vars) "season" else cat_vars[min(2, length(cat_vars))]),
          
          hr(),
          
          selectInput(
            "freq_sort", "Frequency table sorting",
            choices = c(
              "Category (A→Z)" = "cat_asc",
              "Frequency (high→low)" = "freq_desc",
              "Frequency (low→high)" = "freq_asc"
            ),
            selected = "cat_asc"
          ),
          
          hr(),
          
          checkboxInput("missing_as_level", "Treat missing as a category (\"(Missing)\")", value = TRUE),
          sliderInput("top_n_levels", "Keep top-N levels per variable (rest = \"Other\")",
                      min = 4, max = 25, value = 12, step = 1),
          
          hr(),
          
          checkboxInput("show_margins", "Show totals (margins / total rows)", value = TRUE),
          
          radioButtons(
            "pct_type",
            "Percent table type",
            choices = c(
              "Table % (overall)" = "table",
              "Row % (conditional on row)" = "row",
              "Column % (conditional on column)" = "col"
            ),
            selected = "table"
          ),
          
          sliderInput("pct_digits", "Percent rounding (digits)", min = 0, max = 2, value = 1, step = 1)
        )
      ),
      
    ),
    
    column(
      width = 8,
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Frequency Tables (Row + Column)",
          div(class="box",
              strong("Frequency tables for BOTH selected variables"),
              div(class="small",
                  "Computed from the same filtered dataset as the cross-tabs. ",
                  "Totals row appears when “Show totals” is on."),
              fluidRow(
                column(width = 6, uiOutput("freq_row_ui")),
                column(width = 6, uiOutput("freq_col_ui"))
              ),
              div(class="small", style="margin-top:8px;",
                  "Cumulative % is computed in the currently selected sort order.")
          )
        ),
        
        tabPanel(
          "Cross-tab (Counts)",
          div(class="box",
              strong("Contingency table (counts)"),
              div(class="small", "Rows × Columns. Totals are bold when enabled."),
              uiOutput("ct_counts_ui")
          )
        ),
        
        tabPanel(
          "Cross-tab (Percentages)",
          div(class="box",
              strong("Contingency table (percentages)"),
              div(class="small",
                  "Totals are computed from counts so the grand total row/column is meaningful (and bold)."),
              uiOutput("ct_pct_ui")
          )
        ),
        
        tabPanel(
          "What Can Go Wrong?",
          div(class="box",
              strong("Common confusion: conditional vs overall"),
              div(class="small",
                  HTML(
                    "<b>Row %</b> answers: “Given this row category, what’s the distribution across columns?”<br>
                     <b>Column %</b> answers: “Given this column category, what’s the distribution across rows?”<br>
                     <b>Table %</b> answers: “Out of everyone, what percent are in this cell?”"
                  )
              ),
              hr(),
              strong("Check yourself (fast rule)"),
              div(class="small",
                  HTML(
                    "If you say “<i>given</i> ...”, you’re doing a <b>conditional</b> percent (row or column).<br>
                     If you say “<i>out of all accidents</i> ...”, you’re doing <b>table %</b>."
                  )
              ),
              hr(),
              strong("Tip"),
              div(class="small",
                  "Before interpreting, confirm which direction sums to ~100%: rows (Row %), columns (Column %), or entire table (Table %).")
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # auto-fix: if row_var == col_var, pick a different column
  observeEvent(list(input$row_var, input$col_var), {
    if (!is.null(input$row_var) && input$row_var == input$col_var) {
      alt <- setdiff(cat_vars, input$row_var)
      if (length(alt) > 0) updateSelectInput(session, "col_var", selected = alt[1])
    }
  }, ignoreInit = TRUE)
  
  filtered <- reactive({
    dat <- acc
    
    # date range filter (based on parsed start_time)
    if (!is.null(input$dates)) {
      dat <- dat %>%
        filter(as.Date(start_time) >= input$dates[1],
               as.Date(start_time) <= input$dates[2])
    }
    
    dat
  })
  
  # helper: render HTML table with bold margin row/col if totals enabled
  render_table_bold_margins <- function(df, bold_margins = FALSE) {
    nR <- nrow(df)
    nC <- ncol(df)
    
    tags$table(
      class = "simple",
      tags$thead(tags$tr(lapply(names(df), tags$th))),
      tags$tbody(
        lapply(seq_len(nR), function(i) {
          tags$tr(
            lapply(seq_len(nC), function(j) {
              val <- df[i, j]
              # Bold last row OR last column when margins are enabled
              is_margin_cell <- bold_margins && (
                (i == nR) || (j == nC && names(df)[j] != "Row")
              )
              if (is_margin_cell) tags$td(tags$strong(val)) else tags$td(val)
            })
          )
        })
      )
    )
  }
  
  # ---- Frequency tables (for BOTH selected vars) ----
  freq_df_for <- function(dat, varname) {
    if (is.null(varname) || !varname %in% names(dat)) {
      return(data.frame(
        Level = character(0),
        Frequency = integer(0),
        Percent = character(0),
        CumFrequency = integer(0),
        CumPercent = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    x <- handle_missing_cat(dat[[varname]], missing_as_level = isTRUE(input$missing_as_level))
    if (!isTRUE(input$missing_as_level)) x <- x[!is.na(x)]
    x <- collapse_top_n(x, n = input$top_n_levels %||% 12)
    
    make_freq_df(
      x,
      sort_mode = input$freq_sort %||% "cat_asc",
      add_total = isTRUE(input$show_margins)
    )
  }
  
  freq_row_df <- reactive(freq_df_for(filtered(), input$row_var))
  freq_col_df <- reactive(freq_df_for(filtered(), input$col_var))
  
  render_freq_ui <- function(df, label, nrows_filtered) {
    if (nrows_filtered == 0) {
      return(div(class="small", "No rows after filters. Try widening the date range."))
    }
    if (nrow(df) == 0) {
      return(div(class="small", "No usable categories for this selection (possibly all missing)."))
    }
    
    # Bold the Total row if present
    is_total_row <- df$Level == "Total"
    
    tagList(
      div(style="font-weight:800; margin-bottom:6px;", label),
      div(class="tbl-wrap",
          tags$table(
            class = "simple",
            tags$thead(
              tags$tr(
                tags$th("Level"),
                tags$th("Frequency"),
                tags$th("Percent"),
                tags$th("Cum. Freq"),
                tags$th("Cum. %")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                row_cells <- list(
                  tags$td(df$Level[i]),
                  tags$td(df$Frequency[i]),
                  tags$td(df$Percent[i]),
                  tags$td(df$CumFrequency[i]),
                  tags$td(df$CumPercent[i])
                )
                if (is_total_row[i]) {
                  tags$tr(lapply(row_cells, function(cell) tags$td(tags$strong(cell$children))))
                } else {
                  tags$tr(row_cells)
                }
              })
            )
          )
      )
    )
  }
  
  output$freq_row_ui <- renderUI({
    dat <- filtered()
    render_freq_ui(freq_row_df(), paste0("Row variable: ", input$row_var), nrow(dat))
  })
  
  output$freq_col_ui <- renderUI({
    dat <- filtered()
    render_freq_ui(freq_col_df(), paste0("Column variable: ", input$col_var), nrow(dat))
  })
  
  # ---- Cross-tab counts ----
  ctab_counts <- reactive({
    dat <- filtered()
    rv <- input$row_var
    cv <- input$col_var
    
    if (nrow(dat) == 0 || is.null(rv) || is.null(cv) || !rv %in% names(dat) || !cv %in% names(dat)) {
      return(NULL)
    }
    
    rowx <- handle_missing_cat(dat[[rv]], missing_as_level = isTRUE(input$missing_as_level))
    colx <- handle_missing_cat(dat[[cv]], missing_as_level = isTRUE(input$missing_as_level))
    
    if (!isTRUE(input$missing_as_level)) {
      keep <- !is.na(rowx) & !is.na(colx)
      rowx <- rowx[keep]
      colx <- colx[keep]
    }
    
    rowx <- collapse_top_n(rowx, n = input$top_n_levels %||% 12)
    colx <- collapse_top_n(colx, n = input$top_n_levels %||% 12)
    
    table(rowx, colx)
  })
  
  output$ct_counts_ui <- renderUI({
    tab <- ctab_counts()
    dat <- filtered()
    
    if (nrow(dat) == 0) {
      return(div(class="small", "No rows after filters. Try widening the date range."))
    }
    if (is.null(tab) || length(tab) == 0 || sum(tab) == 0) {
      return(div(class="small", "No usable data for this cross-tab (possibly all missing after settings)."))
    }
    
    df <- tab_to_df_counts(tab, add_margins = isTRUE(input$show_margins))
    
    tagList(
      div(class="tbl-wrap",
          render_table_bold_margins(df, bold_margins = isTRUE(input$show_margins))
      ),
      div(class="small", style="margin-top:8px;",
          paste0("Total (usable) in base table: ", sum(tab)))
    )
  })
  
  # ---- Cross-tab percentages (meaningful totals based on counts) ----
  output$ct_pct_ui <- renderUI({
    tab <- ctab_counts()
    dat <- filtered()
    
    if (nrow(dat) == 0) {
      return(div(class="small", "No rows after filters. Try widening the date range."))
    }
    if (is.null(tab) || sum(tab) == 0) {
      return(div(class="small", "No usable data for percent table (possibly all missing after settings)."))
    }
    
    ptype <- input$pct_type %||% "table"
    digits <- input$pct_digits %||% 1
    
    df <- percent_df_with_margins(
      tab,
      type = ptype,
      digits = digits,
      add_margins = isTRUE(input$show_margins)
    )
    
    if (is.null(df) || nrow(df) == 0) {
      return(div(class="small", "No usable percent table."))
    }
    
    expl <- if (ptype == "row") {
      "Row %: each row sums to ~100%. The Total row is based on overall counts (sums to ~100% across columns)."
    } else if (ptype == "col") {
      "Column %: each column sums to ~100%. The Total column is based on overall counts (sums to ~100% down rows)."
    } else {
      "Table %: each cell is a share of the grand total; totals reflect overall shares."
    }
    
    tagList(
      div(class="small", style="margin-bottom:8px;", expl),
      div(class="tbl-wrap",
          render_table_bold_margins(df, bold_margins = isTRUE(input$show_margins))
      ),
      div(class="small", style="margin-top:8px;",
          paste0("Total (usable) in base count table: ", sum(tab)))
    )
  })
}

shinyApp(ui, server)
