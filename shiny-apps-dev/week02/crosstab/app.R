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
      hour(start_time) >= 6  & hour(start_time) < 12 ~ "morning",  # 06:00–11:59
      hour(start_time) >= 12 & hour(start_time) < 18 ~ "daytime",  # 12:00–17:59
      hour(start_time) >= 18 & hour(start_time) < 24 ~ "evening",  # 18:00–23:59
      TRUE                                           ~ "night"     # 00:00–05:59
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
  
  tab2 <- sort(table(x2), decreasing = TRUE)
  levs2 <- names(tab2)
  if (other_label %in% levs2) {
    levs2 <- c(setdiff(levs2, other_label), other_label)
  }
  factor(x2, levels = levs2)
}

# Frequency table with cumulative columns + sorting
make_freq_df <- function(x, sort_mode = c("cat_asc", "freq_desc", "freq_asc")) {
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
  
  if (sort_mode == "cat_asc") {
    df <- df[order(df$Level), ]
  } else if (sort_mode == "freq_desc") {
    df <- df[order(-df$Frequency, df$Level), ]
  } else {
    df <- df[order(df$Frequency, df$Level), ]
  }
  
  df$PercentNum <- 100 * df$Frequency / sum(df$Frequency)
  df$CumFrequency <- cumsum(df$Frequency)
  df$CumPercentNum <- cumsum(df$PercentNum)
  
  df$Percent <- sprintf("%.1f%%", df$PercentNum)
  df$CumPercent <- sprintf("%.1f%%", df$CumPercentNum)
  
  df$PercentNum <- NULL
  df$CumPercentNum <- NULL
  df
}

tab_to_df <- function(tab, add_margins = FALSE) {
  if (add_margins) tab <- addmargins(tab)
  as.data.frame.matrix(tab, stringsAsFactors = FALSE)
}

percent_table <- function(tab, type = c("table", "row", "col")) {
  type <- match.arg(type)
  if (sum(tab) == 0) return(tab * 0)
  
  if (type == "table") {
    prop.table(tab) * 100
  } else if (type == "row") {
    prop.table(tab, margin = 1) * 100
  } else {
    prop.table(tab, margin = 2) * 100
  }
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

# ---- UI ----
ui <- fluidPage(
  tags$style("
    h2{margin-top:0.2rem;}
    .small{opacity:0.75;font-size:1.0rem;}
    .box{background:#f7f7f7;border-radius:12px;padding:12px;margin-bottom:10px;}
    .tbl-wrap{overflow-x:auto;}

    /* BIGGER tables */
    table.simple {
      border-collapse: collapse;
      width: 100%;
      font-size: 1.20rem;
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
          
          selectInput(
            "sev_filter", "Severity filter (optional)",
            choices = c("All", sort(unique(acc$Severity))),
            selected = "All"
          ),
          
          dateRangeInput(
            "dates", "Start date range",
            start = as.Date(min(acc$start_time, na.rm = TRUE)),
            end   = as.Date(max(acc$start_time, na.rm = TRUE)),
            min   = as.Date(min(acc$start_time, na.rm = TRUE)),
            max   = as.Date(max(acc$start_time, na.rm = TRUE))
          ),
          
          hr(),
          
          selectInput("row_var", "Cross-tab: Rows", choices = cat_vars, selected = "Severity"),
          selectInput("col_var", "Cross-tab: Columns", choices = cat_vars, selected = {
            pick <- setdiff(cat_vars, "Severity")
            if (length(pick) > 0) pick[1] else cat_vars[1]
          }),
          
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
          
          checkboxInput("show_margins", "Show marginal counts (totals)", value = TRUE),
          
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
      
      div(
        class = "callout-left",
        div(style="font-weight:800; margin-bottom:.35rem;", "Teaching cues (quick)"),
        div(
          class="small",
          HTML(
            "<b>Frequency table</b> answers: “How common is each category?”<br>
             <b>Cross-tab counts</b> answer: “How many in each combination?”<br>
             <b>Row %</b>: within each row, how are columns distributed?<br>
             <b>Column %</b>: within each column, how are rows distributed?<br>
             <b>Table %</b>: share of the whole table."
          )
        )
      )
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
                  "These are computed from the same filtered dataset as the cross-tabs. ",
                  "Notice how counts/percents carry through, but interpretation changes once you compare."),
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
              div(class="small", "Rows × Columns. Toggle marginal totals to show row/column totals (shown in bold)."),
              uiOutput("ct_counts_ui")
          )
        ),
        
        tabPanel(
          "Cross-tab (Percentages)",
          div(class="box",
              strong("Contingency table (percentages)"),
              div(class="small", "Choose Table %, Row %, or Column % (conditional distributions). Margins optional (shown in bold)."),
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
    
    # optional severity filter
    if (!is.null(input$sev_filter) && input$sev_filter != "All") {
      dat <- dat %>% filter(Severity == as.integer(input$sev_filter))
    }
    
    # date range filter (based on parsed start_time)
    if (!is.null(input$dates)) {
      dat <- dat %>%
        filter(as.Date(start_time) >= input$dates[1],
               as.Date(start_time) <= input$dates[2])
    }
    
    dat
  })
  
  # Frequency df builder for any chosen variable
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
    
    make_freq_df(x, sort_mode = input$freq_sort %||% "cat_asc")
  }
  
  freq_row_df <- reactive({
    freq_df_for(filtered(), input$row_var)
  })
  
  freq_col_df <- reactive({
    freq_df_for(filtered(), input$col_var)
  })
  
  render_freq_ui <- function(df, label, nrows_filtered) {
    if (nrows_filtered == 0) {
      return(div(class="small", "No rows after filters. Try widening date range or removing the severity filter."))
    }
    if (nrow(df) == 0) {
      return(div(class="small", "No usable categories for this selection (possibly all missing)."))
    }
    
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
                tags$tr(
                  tags$td(df$Level[i]),
                  tags$td(df$Frequency[i]),
                  tags$td(df$Percent[i]),
                  tags$td(df$CumFrequency[i]),
                  tags$td(df$CumPercent[i])
                )
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
  
  # ---------- Cross-tab base table ----------
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
  
  # helper: render cross-tab with bold margins if shown
  render_ctab_html <- function(df, bold_margins = FALSE) {
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
  
  output$ct_counts_ui <- renderUI({
    tab <- ctab_counts()
    dat <- filtered()
    
    if (nrow(dat) == 0) {
      return(div(class="small", "No rows after filters. Try widening date range or removing the severity filter."))
    }
    if (is.null(tab) || length(tab) == 0 || sum(tab) == 0) {
      return(div(class="small", "No usable data for this cross-tab (possibly all missing after settings)."))
    }
    
    df <- tab_to_df(tab, add_margins = isTRUE(input$show_margins))
    df <- cbind(Row = rownames(df), df, stringsAsFactors = FALSE)
    rownames(df) <- NULL
    
    tagList(
      div(class="tbl-wrap",
          render_ctab_html(df, bold_margins = isTRUE(input$show_margins))
      ),
      div(class="small", style="margin-top:8px;",
          paste0("Total (usable) in table: ", sum(tab)))
    )
  })
  
  # ---------- Percent cross-tab ----------
  ctab_pct_df <- reactive({
    tab <- ctab_counts()
    if (is.null(tab) || sum(tab) == 0) return(NULL)
    
    ptype <- input$pct_type %||% "table"
    ptab <- percent_table(tab, type = ptype)
    
    if (isTRUE(input$show_margins)) {
      ptab <- addmargins(ptab)
    }
    
    df <- as.data.frame.matrix(ptab, stringsAsFactors = FALSE)
    df <- cbind(Row = rownames(df), df, stringsAsFactors = FALSE)
    rownames(df) <- NULL
    
    digits <- input$pct_digits %||% 1
    out <- df
    for (j in seq_along(out)) {
      if (names(out)[j] == "Row") next
      out[[j]] <- ifelse(
        is.na(out[[j]]),
        "",
        sprintf(paste0("%.", digits, "f%%"), as.numeric(out[[j]]))
      )
    }
    out
  })
  
  output$ct_pct_ui <- renderUI({
    df <- ctab_pct_df()
    dat <- filtered()
    tab <- ctab_counts()
    
    if (nrow(dat) == 0) {
      return(div(class="small", "No rows after filters. Try widening date range or removing the severity filter."))
    }
    if (is.null(tab) || sum(tab) == 0 || is.null(df) || nrow(df) == 0) {
      return(div(class="small", "No usable data for percent table (possibly all missing after settings)."))
    }
    
    ptype <- input$pct_type %||% "table"
    expl <- if (ptype == "row") {
      "Row %: each row sums to ~100% (conditional on row)."
    } else if (ptype == "col") {
      "Column %: each column sums to ~100% (conditional on column)."
    } else {
      "Table %: the whole table sums to ~100% (overall share)."
    }
    
    tagList(
      div(class="small", style="margin-bottom:8px;", expl),
      div(class="tbl-wrap",
          render_ctab_html(df, bold_margins = isTRUE(input$show_margins))
      ),
      div(class="small", style="margin-top:8px;",
          paste0("Total (usable) in base count table: ", sum(tab)))
    )
  })
}

shinyApp(ui, server)