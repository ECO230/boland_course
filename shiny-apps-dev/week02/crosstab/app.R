# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)
library(dplyr)
library(readr)
library(lubridate)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Load data ONCE at startup ----
COURSE_ROOT <- "/data/junior/boland_course"
acc_path <- file.path(COURSE_ROOT, "shared", "data", "accident_wi.csv")

acc <- read_csv(acc_path, show_col_types = FALSE) %>%
  mutate(
    Start_Time = mdy_hm(Start_Time),
    End_Time   = mdy_hm(End_Time),
    Duration_min = as.numeric(difftime(End_Time, Start_Time, units = "mins"))
  )

# ---- Helpers ----
as_cat <- function(x) {
  # Convert common types to character/factor-friendly categories for tables
  if (is.factor(x)) return(as.character(x))
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE")))
  if (inherits(x, "Date")) return(as.character(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.character(as.Date(x)))
  if (is.numeric(x) || is.integer(x)) return(as.character(x))
  as.character(x)
}

# Treat missing as "(Missing)" or drop missing
handle_missing_cat <- function(x, missing_as_level = TRUE) {
  x <- as_cat(x)
  if (missing_as_level) {
    x[!nzchar(x) | is.na(x)] <- "(Missing)"
    return(x)
  } else {
    # keep NA; caller may drop
    x[!nzchar(x)] <- NA_character_
    return(x)
  }
}

# Collapse to top-N levels (by frequency), remainder -> "Other"
collapse_top_n <- function(x, n = 12, other_label = "Other") {
  x <- as.character(x)
  if (length(x) == 0) return(x)
  
  tab <- sort(table(x), decreasing = TRUE)
  levs <- names(tab)
  
  # Keep all if already small
  if (length(levs) <= n) return(x)
  
  keep <- levs[seq_len(n)]
  x2 <- ifelse(x %in% keep, x, other_label)
  
  # Re-order so the most frequent appear first, "Other" last
  tab2 <- sort(table(x2), decreasing = TRUE)
  levs2 <- names(tab2)
  if (other_label %in% levs2) {
    levs2 <- c(setdiff(levs2, other_label), other_label)
  }
  factor(x2, levels = levs2)
}

# Frequency table for one variable
make_freq_df <- function(x) {
  x <- as.character(x)
  n <- length(x)
  if (n == 0) return(data.frame(Level = character(0), Frequency = integer(0), Percent = character(0)))
  
  tab <- sort(table(x), decreasing = TRUE)
  df <- data.frame(
    Level = names(tab),
    Frequency = as.integer(tab),
    stringsAsFactors = FALSE
  )
  df$Percent <- sprintf("%.1f%%", 100 * df$Frequency / sum(df$Frequency))
  df
}

# Convert table object to data.frame matrix-like with optional margins
tab_to_df <- function(tab, add_margins = FALSE) {
  if (add_margins) tab <- addmargins(tab)
  as.data.frame.matrix(tab, stringsAsFactors = FALSE)
}

# Percent tables: type = "table" | "row" | "col"
percent_table <- function(tab, type = c("table", "row", "col")) {
  type <- match.arg(type)
  if (sum(tab) == 0) return(tab * 0)
  
  if (type == "table") {
    p <- prop.table(tab) * 100
  } else if (type == "row") {
    p <- prop.table(tab, margin = 1) * 100
  } else {
    p <- prop.table(tab, margin = 2) * 100
  }
  p
}

fmt_percent_df <- function(df, digits = 1) {
  # Format numeric matrix-like data.frame to percent strings (keep rownames already converted)
  out <- df
  for (j in seq_along(out)) {
    out[[j]] <- ifelse(is.na(out[[j]]), "", sprintf(paste0("%.", digits, "f%%"), out[[j]]))
  }
  out
}

# Choose categorical vars: limit to reasonable unique levels to keep tables readable
# Include Severity even though it's numeric in the source.
cat_candidates <- names(acc)

# Exclude obvious high-cardinality / unhelpful columns if present
exclude_cols <- c("ID", "Description", "Street", "City", "County", "Zipcode", "Airport_Code", "Weather_Timestamp")
cat_candidates <- setdiff(cat_candidates, exclude_cols)

# Heuristic: candidate if unique values (non-missing) <= 30 OR is Severity
n_unique <- sapply(acc[cat_candidates], function(col) length(unique(col[!is.na(col)])))
cat_vars <- c("Severity", cat_candidates[n_unique <= 30])
cat_vars <- intersect(cat_vars, names(acc))
cat_vars <- unique(cat_vars)

# Fall back if heuristics too strict
if (length(cat_vars) < 2) {
  cat_vars <- names(acc)
}

# ---- UI ----
ui <- fluidPage(
  tags$style("
    h2{margin-top:0.2rem;}
    .small{opacity:0.75;font-size:0.95rem;}
    .box{background:#f7f7f7;border-radius:12px;padding:12px;margin-bottom:10px;}
    .tbl-wrap{overflow-x:auto;}
    table.simple {
      border-collapse: collapse;
      width: 100%;
      font-size: 0.98rem;
      background: white;
      border-radius: 12px;
      overflow: hidden;
    }
    table.simple th, table.simple td {
      border: 1px solid #e6e6e6;
      padding: 8px 10px;
      text-align: right;
      white-space: nowrap;
    }
    table.simple th:first-child, table.simple td:first-child { text-align: left; }
    table.simple thead th {
      background: #fafafa;
      font-weight: 700;
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
      cursor: pointer; font-weight: 700;
      padding: 12px; border-radius:12px;
      list-style: none;
    }
    details.param > summary::-webkit-details-marker { display:none; }
    details.param .content { padding: 12px; padding-top: 0; }
  "),
  
  h2("WI Accidents — Frequency Tables & Cross Tabs"),
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
          
          # Shared filters (keep simple + teaching-friendly)
          selectInput(
            "sev_filter", "Severity filter (optional)",
            choices = c("All", sort(unique(acc$Severity))),
            selected = "All"
          ),
          
          dateRangeInput(
            "dates", "Start date range",
            start = as.Date(min(acc$Start_Time, na.rm = TRUE)),
            end   = as.Date(max(acc$Start_Time, na.rm = TRUE)),
            min   = as.Date(min(acc$Start_Time, na.rm = TRUE)),
            max   = as.Date(max(acc$Start_Time, na.rm = TRUE))
          ),
          
          hr(),
          
          # Variables for tables
          selectInput("freq_var", "Frequency table variable", choices = cat_vars, selected = "Severity"),
          
          hr(),
          
          selectInput("row_var", "Cross-tab: Rows", choices = cat_vars, selected = "Severity"),
          selectInput("col_var", "Cross-tab: Columns", choices = cat_vars, selected = {
            # pick a second variable if possible
            pick <- setdiff(cat_vars, "Severity")
            if (length(pick) > 0) pick[1] else cat_vars[1]
          }),
          
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
        div(style="font-weight:700; margin-bottom:.35rem;", "Teaching cues (quick)"),
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
          "Frequency Table",
          div(class="box",
              strong("Frequency table (counts + percent)"),
              div(class="small", "Counts and % based on the filtered data; missing may be included as “(Missing)”."),
              uiOutput("freq_tbl_ui")
          )
        ),
        
        tabPanel(
          "Cross-tab (Counts)",
          div(class="box",
              strong("Contingency table (counts)"),
              div(class="small", "Rows × Columns. Toggle marginal totals to show row/column totals."),
              uiOutput("ct_counts_ui")
          )
        ),
        
        tabPanel(
          "Cross-tab (Percentages)",
          div(class="box",
              strong("Contingency table (percentages)"),
              div(class="small", "Choose Table %, Row %, or Column % (conditional distributions). Margins optional."),
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
  
  filtered <- reactive({
    dat <- acc
    
    # optional severity filter
    if (!is.null(input$sev_filter) && input$sev_filter != "All") {
      dat <- dat %>% filter(Severity == as.integer(input$sev_filter))
    }
    
    # date range filter
    if (!is.null(input$dates)) {
      dat <- dat %>%
        filter(as.Date(Start_Time) >= input$dates[1],
               as.Date(Start_Time) <= input$dates[2])
    }
    
    dat
  })
  
  # Build a clean categorical vector for any chosen variable (with missing handling + top-N collapsing)
  cat_vec <- function(dat, varname) {
    x <- dat[[varname]]
    x <- handle_missing_cat(x, missing_as_level = isTRUE(input$missing_as_level))
    if (!isTRUE(input$missing_as_level)) {
      # drop missing if missing is not a level
      x <- x[!is.na(x)]
    }
    # collapse for readability
    x <- collapse_top_n(x, n = input$top_n_levels %||% 12)
    x
  }
  
  # ---------- Frequency table ----------
  freq_df <- reactive({
    dat <- filtered()
    v <- input$freq_var
    
    if (is.null(v) || !v %in% names(dat)) {
      return(data.frame(Level = character(0), Frequency = integer(0), Percent = character(0)))
    }
    
    x <- handle_missing_cat(dat[[v]], missing_as_level = isTRUE(input$missing_as_level))
    if (!isTRUE(input$missing_as_level)) x <- x[!is.na(x)]
    x <- collapse_top_n(x, n = input$top_n_levels %||% 12)
    
    make_freq_df(x)
  })
  
  output$freq_tbl_ui <- renderUI({
    df <- freq_df()
    dat <- filtered()
    
    if (nrow(dat) == 0) {
      return(div(class="small", "No rows after filters. Try widening date range or removing the severity filter."))
    }
    
    if (nrow(df) == 0) {
      return(div(class="small", "No usable categories for this selection (possibly all missing)."))
    }
    
    # Simple HTML table (no extra dependencies)
    tagList(
      div(class="tbl-wrap",
          tags$table(
            class = "simple",
            tags$thead(
              tags$tr(
                tags$th("Level"),
                tags$th("Frequency"),
                tags$th("Percent")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                tags$tr(
                  tags$td(df$Level[i]),
                  tags$td(df$Frequency[i]),
                  tags$td(df$Percent[i])
                )
              })
            )
          )
      ),
      div(class="small", style="margin-top:8px;",
          paste0("Rows after filters: ", nrow(dat)))
    )
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
    
    tab <- table(rowx, colx)
    tab
  })
  
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
    
    # Render as simple HTML table
    tagList(
      div(class="tbl-wrap",
          tags$table(
            class = "simple",
            tags$thead(
              tags$tr(lapply(names(df), tags$th))
            ),
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                tags$tr(lapply(df[i, ], function(val) tags$td(val)))
              })
            )
          )
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
    
    # If margins are ON: add margins after percent conversion for teaching
    # Note: for row/col % tables, the margin total will be ~100% for each row/col; for table %, grand total ~100%.
    if (isTRUE(input$show_margins)) {
      if (ptype == "row") {
        # Add row totals (~100%) and column totals (not meaningful for row % but still shown as a teaching device)
        ptab <- addmargins(ptab)
      } else if (ptype == "col") {
        ptab <- addmargins(ptab)
      } else {
        ptab <- addmargins(ptab)
      }
    }
    
    df <- as.data.frame.matrix(ptab, stringsAsFactors = FALSE)
    df <- cbind(Row = rownames(df), df, stringsAsFactors = FALSE)
    rownames(df) <- NULL
    
    # format numeric cells as percent strings; keep Row as-is
    digits <- input$pct_digits %||% 1
    out <- df
    for (j in seq_along(out)) {
      if (names(out)[j] == "Row") next
      out[[j]] <- ifelse(is.na(out[[j]]), "", sprintf(paste0("%.", digits, "f%%"), as.numeric(out[[j]])))
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
    
    # explanatory line: what sums to 100?
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
          tags$table(
            class = "simple",
            tags$thead(tags$tr(lapply(names(df), tags$th))),
            tags$tbody(
              lapply(seq_len(nrow(df)), function(i) {
                tags$tr(lapply(df[i, ], function(val) tags$td(val)))
              })
            )
          )
      ),
      div(class="small", style="margin-top:8px;",
          paste0("Total (usable) in base count table: ", sum(tab)))
    )
  })
}

shinyApp(ui, server)
