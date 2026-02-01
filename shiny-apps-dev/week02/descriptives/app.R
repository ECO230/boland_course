# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(stringr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Load data ONCE at startup (LIMITED + DERIVED FIELDS) ----
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
    duration = end_time - start_time,
    duration_mins = as.numeric(duration, units = "mins"),
    
    # Optional: ordered factors for nicer plots
    season = factor(season, levels = c("spring", "summer", "fall", "winter")),
    time_of_day = factor(time_of_day, levels = c("night", "morning", "daytime", "evening"))
  ) %>%
  # Keep ONLY the limited set you specified (PLUS start_time for date filtering)
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


# ---- Unique ID column ----
ID_COL <- "ID"
if (!ID_COL %in% names(acc)) {
  stop(paste0("Expected a unique ID column named '", ID_COL, "' but it was not found in the dataset."))
}

# Candidate numeric variables (UPDATED for limited dataset)
num_vars <- c(
  "Distance(mi)",
  "Temperature(F)",
  "Wind_Chill(F)",
  "Humidity(%)",
  "Wind_Speed(mph)",
  "Visibility(mi)",
  "Pressure(in)",
  "Precipitation(in)",
  "duration_mins"
)
num_vars <- intersect(num_vars, names(acc))

# Rounded mode helper
mode_rounded <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  xr <- round(as.numeric(x), digits)
  xr <- xr[is.finite(xr)]
  if (length(xr) == 0) return(NA_real_)
  tab <- table(xr)
  as.numeric(names(tab)[which.max(tab)])
}

# Dot-grid helper for attrition view
make_dot_grid <- function(n) {
  cols <- ceiling(sqrt(n))
  rows <- ceiling(n / cols)
  data.frame(
    idx = seq_len(n),
    x = (seq_len(n) - 1) %% cols + 1,
    y = rows - ((seq_len(n) - 1) %/% cols)
  )
}

# Trim helper: trims p from each tail (p in [0, 0.20])
trim_tails <- function(x, p = 0) {
  x <- x[is.finite(x)]
  if (length(x) == 0 || p <= 0) return(x)
  if (p >= 0.5) return(numeric(0))
  qs <- stats::quantile(x, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE, type = 7)
  x[x >= qs[1] & x <= qs[2]]
}

# Parse comma-separated percentiles like "10,25,50,75,90"
parse_percentiles <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(numeric(0))
  parts <- unlist(strsplit(txt, ","))
  vals <- suppressWarnings(as.numeric(trimws(parts)))
  vals <- vals[is.finite(vals)]
  vals <- vals[vals >= 0 & vals <= 100]
  sort(unique(vals))
}

# Safe min/max dates for UI bounds
safe_min_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) Sys.Date() else as.Date(min(x))
}
safe_max_date <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) Sys.Date() else as.Date(max(x))
}

min_date <- safe_min_date(acc$start_time)
max_date <- safe_max_date(acc$start_time)

# ---- UI ----
ui <- fluidPage(
  tags$style("
    h2{margin-top:0.2rem;}
    .small{opacity:0.75;font-size:0.95rem;}
    .box{background:#f7f7f7;border-radius:12px;padding:12px;margin-bottom:10px;}
    pre{margin:0;}

    details.param { background:#f7f7f7; border-radius:12px; margin-bottom:10px; }
    details.param > summary {
      cursor: pointer; font-weight: 700;
      padding: 12px; border-radius:12px;
      list-style: none;
    }
    details.param > summary::-webkit-details-marker { display:none; }
    details.param .content { padding: 12px; padding-top: 0; }

    .callout-left {
      border-left: 6px solid #22A06B;
      background: rgba(34,160,107,.06);
      border-radius: 12px;
      padding: 12px;
      margin-bottom: 10px;
    }
  "),
  
  h2("WI Accidents — Descriptive Stats"),
  div(class="small", "Size (N vs n), central tendency, spread, and boxplots — all from the same filtered data."),
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
          
          selectInput("var", "Numeric variable", choices = num_vars),
          
          # NEW: Pick which categorical field to filter
          selectInput(
            "cat_var",
            "Categorical filter field",
            choices = c(
              "None",
              "Severity",
              "Wind_Direction",
              "Weather_Condition",
              "Sunrise_Sunset",
              "season",
              "time_of_day"
            ),
            selected = "None"
          ),
          
          # NEW: levels picker appears dynamically
          uiOutput("cat_level_ui"),
          
          # Date filter (uses derived start_time)
          dateRangeInput(
            "dates", "Start date range",
            start = min_date,
            end   = max_date,
            min   = min_date,
            max   = max_date
          ),
          
          checkboxInput("drop_na", "Drop missing/invalid values for stats", TRUE),
          
          hr(),
          
          checkboxInput("show_mean_median", "Show mean/median lines (where applicable)", TRUE),
          textInput("pct_list", "Percentile lines (comma-separated)", value = "10,25,50,75,90"),
          checkboxInput("show_percentiles", "Show percentile lines", value = FALSE),
          checkboxInput("shade_iqr", "Shade IQR region (25th–75th)", value = FALSE),
          
          hr(),
          
          sliderInput(
            "trim_p",
            "Trim extremes (each tail) — applies to Center/Spread/Boxplot",
            min = 0, max = 0.20, value = 0, step = 0.01
          ),
          
          sliderInput("mode_digits", "Mode rounding (digits)", min = 0, max = 2, value = 1, step = 1),
          
          hr(),
          
          checkboxInput("show_box_dots", "Show dots on boxplot (outliers always shown)", value = FALSE)
        )
      ),
      
      div(
        class = "callout-left",
        div(style="font-weight:700; margin-bottom:.35rem;", "Mental model (always)"),
        div(
          class="small",
          HTML(
            "<b>N</b> = total rows loaded<br>
             <b>After filters</b> = eligible rows<br>
             <b>Missing/invalid</b> = unusable for selected variable<br>
             <b>Trimmed</b> = removed by trimming rule (purple)<br>
             <b>Usable n</b> = values used by center/spread/boxplot"
          )
        )
      )
    ),
    
    column(
      width = 8,
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Measures of Size",
          div(class="box", verbatimTextOutput("size_text")),
          div(class="box",
              strong("Row attrition (same dots, stages)"),
              div(class="small", "All rows → after filters → missing/invalid → trimmed → usable n."),
              plotOutput("dot_attrition", height = "200px")
          )
        ),
        
        tabPanel(
          "Central Tendency",
          fluidRow(
            column(6, div(class="box", verbatimTextOutput("center_text"))),
            column(6, div(class="box", verbatimTextOutput("center_notes")))
          ),
          div(class="box",
              strong("Point histogram (stacked dots; real values + dodge)"),
              div(class="small", "Each dot is an observation. Stacking shows counts; dodging makes 'weight' visible."),
              plotOutput("center_point_hist", height = "290px")
          )
        ),
        
        tabPanel(
          "Spread",
          fluidRow(
            column(4, div(class="box", verbatimTextOutput("spread_metrics_text"))),
            column(
              4,
              conditionalPanel(
                condition = "input.show_percentiles == true",
                div(class="box", verbatimTextOutput("spread_percentiles_text"))
              )
            ),
            column(4, div(class="box", verbatimTextOutput("spread_notes_text")))
          ),
          div(class="box",
              strong("Histogram + markers"),
              div(class="small", "Mean/median + custom percentiles + IQR shading (uses trimmed data if trim > 0)."),
              plotOutput("spread_hist", height = "320px")
          )
        ),
        
        tabPanel(
          "Box & Whisker (Tie Together)",
          div(class="box",
              strong("Boxplot + points (bulk vs tails)"),
              div(class="small", "Manual box drawing (robust under trimming). Outliers always visible if present in displayed (trimmed) data."),
              plotOutput("boxplot_tie", height = "270px")
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # NEW: dynamic UI for level selection based on chosen categorical field
  output$cat_level_ui <- renderUI({
    req(input$cat_var)
    
    if (identical(input$cat_var, "None")) return(NULL)
    
    # Use date-filtered data for available levels (feels consistent)
    dat0 <- acc
    if (!is.null(input$dates)) {
      dat0 <- dat0 %>%
        filter(as.Date(start_time) >= input$dates[1],
               as.Date(start_time) <= input$dates[2])
    }
    
    vals <- dat0[[input$cat_var]]
    vals <- vals[!is.na(vals)]
    vals <- sort(unique(as.character(vals)))
    
    selectizeInput(
      "cat_levels",
      "Include levels (leave 'All' to disable filtering)",
      choices = c("All", vals),
      selected = "All",
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        placeholder = "Choose levels (or keep 'All')",
        maxOptions = 2000
      )
    )
  })
  
  # Optional nicety: if user selects specific levels, drop "All"
  observeEvent(input$cat_levels, {
    lvls <- input$cat_levels
    if (is.null(lvls)) return()
    
    if ("All" %in% lvls && length(lvls) > 1) {
      updateSelectizeInput(session, "cat_levels", selected = setdiff(lvls, "All"))
    }
  }, ignoreInit = TRUE)
  
  filtered <- reactive({
    dat <- acc
    
    # Date filter
    if (!is.null(input$dates)) {
      dat <- dat %>%
        filter(as.Date(start_time) >= input$dates[1],
               as.Date(start_time) <= input$dates[2])
    }
    
    # Categorical filter (optional)
    if (!is.null(input$cat_var) && input$cat_var != "None") {
      lvls <- input$cat_levels
      if (!is.null(lvls) && !("All" %in% lvls)) {
        dat <- dat %>%
          filter(as.character(.data[[input$cat_var]]) %in% lvls)
      }
    }
    
    dat
  })
  
  x_info <- reactive({
    dat <- filtered()
    v <- input$var
    
    x_num <- suppressWarnings(as.numeric(dat[[v]]))
    valid <- is.finite(x_num)
    
    list(
      dat = dat,
      v = v,
      x_num = x_num,
      valid = valid,
      x_valid = x_num[valid],
      n_after_filters = nrow(dat),
      n_valid_numeric = sum(valid),
      n_missing_or_invalid = nrow(dat) - sum(valid)
    )
  })
  
  trim_flags <- reactive({
    xi <- x_info()
    x_all_valid <- xi$x_valid
    tp <- input$trim_p %||% 0
    
    if (length(x_all_valid) == 0 || tp <= 0) {
      return(list(trim_on = FALSE, cut_lo = NA_real_, cut_hi = NA_real_))
    }
    
    cuts <- stats::quantile(x_all_valid, probs = c(tp, 1 - tp), na.rm = TRUE, type = 7, names = FALSE)
    list(trim_on = TRUE, cut_lo = cuts[1], cut_hi = cuts[2])
  })
  
  x_trimmed <- reactive({
    xi <- x_info()
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) return(numeric(0))
    x <- xi$x_valid
    tp <- input$trim_p %||% 0
    if (tp > 0) x <- trim_tails(x, p = tp)
    x
  })
  
  quartiles <- reactive({
    x <- x_trimmed()
    if (length(x) < 5) return(NULL)
    q <- stats::quantile(x, probs = c(.25, .50, .75), na.rm = TRUE, type = 7, names = FALSE)
    list(Q1 = q[1], Q2 = q[2], Q3 = q[3])
  })
  
  pct_values <- reactive({
    x <- x_trimmed()
    pts <- parse_percentiles(input$pct_list)
    if (length(x) < 5 || length(pts) == 0) return(NULL)
    probs <- pts / 100
    q <- stats::quantile(x, probs = probs, na.rm = TRUE, type = 7, names = FALSE)
    data.frame(pct = pts, value = as.numeric(q))
  })
  
  output$size_text <- renderText({
    xi <- x_info()
    tf <- trim_flags()
    x_valid <- xi$x_valid
    tp <- input$trim_p %||% 0
    
    trimmed_count <- 0L
    usable_after_trim <- length(x_valid)
    
    if (isTRUE(tf$trim_on) && tp > 0 && length(x_valid) > 0) {
      trimmed_count <- sum(x_valid < tf$cut_lo | x_valid > tf$cut_hi)
      usable_after_trim <- length(x_valid) - trimmed_count
    }
    
    paste0(
      "Measures of Size\n",
      "N (total rows):     ", nrow(acc), "\n",
      "After filters:      ", xi$n_after_filters, "\n",
      "Missing/invalid:    ", xi$n_missing_or_invalid, "\n",
      "Trimmed (extremes): ", trimmed_count, "\n",
      "Usable n:           ", usable_after_trim
    )
  })
  
  output$center_text <- renderText({
    xi <- x_info()
    x <- x_trimmed()
    
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return(paste0(
        "Central Tendency\n",
        "Mean:   NA\n",
        "Median: NA\n",
        "Mode*:  NA\n",
        "*Turn on 'Drop missing/invalid' to compute"
      ))
    }
    
    paste0(
      "Central Tendency\n",
      "usable n: ", length(x), "\n",
      "Mean:   ", round(if (length(x)) mean(x) else NA_real_, 3), "\n",
      "Median: ", round(if (length(x)) median(x) else NA_real_, 3), "\n",
      "Mode*:  ", round(if (length(x)) mode_rounded(x, digits = input$mode_digits) else NA_real_, 3), "\n",
      "*Mode computed on rounded values"
    )
  })
  
  output$center_notes <- renderText({
    xi <- x_info()
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return("Notes\nMissing/invalid values exist.\nTurn on 'Drop missing/invalid' to compute.")
    }
    tp <- input$trim_p %||% 0
    paste0(
      "Notes\n",
      "Trim: ", if (tp > 0) paste0(round(tp * 100), "% each tail") else "off", "\n",
      "Mean/median lines: ", if (isTRUE(input$show_mean_median)) "on" else "off"
    )
  })
  
  output$spread_metrics_text <- renderText({
    xi <- x_info()
    x <- x_trimmed()
    
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return(paste0(
        "Spread\n",
        "usable n: NA\nSD: NA\nIQR: NA\nRange: NA\n",
        "*Turn on 'Drop missing/invalid'"
      ))
    }
    if (length(x) == 0) return("Spread\n(No usable data after filters/trim.)")
    
    sd_v <- if (length(x) > 1) sd(x) else NA_real_
    iqr_v <- stats::IQR(x, na.rm = TRUE)
    range_v <- diff(range(x, na.rm = TRUE))
    
    paste0(
      "Spread\n",
      "usable n: ", length(x), "\n",
      "SD:       ", round(sd_v, 3), "\n",
      "IQR:      ", round(iqr_v, 3), "\n",
      "Range:    ", round(range_v, 3)
    )
  })
  
  output$spread_percentiles_text <- renderText({
    xi <- x_info()
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return("Percentiles\nNA (missing/invalid present)")
    }
    pv <- pct_values()
    if (is.null(pv) || nrow(pv) == 0) return("Percentiles\n(none / not enough data)")
    lines <- paste0("P", pv$pct, ": ", round(pv$value, 3))
    paste0("Percentiles\n", paste(lines, collapse = "\n"))
  })
  
  output$spread_notes_text <- renderText({
    tp <- input$trim_p %||% 0
    paste0(
      "Notes\n",
      "Trim: ", if (tp > 0) paste0(round(tp * 100), "% each tail") else "off", "\n",
      "Mean/median lines: ", if (isTRUE(input$show_mean_median)) "on" else "off", "\n",
      "IQR shading: ", if (isTRUE(input$shade_iqr)) "on" else "off", "\n",
      "Percentile lines: ", if (isTRUE(input$show_percentiles)) "on" else "off"
    )
  })
  
  # ---- Measures of size dot attrition plot (includes Trim stage) ----
  dot_long <- reactive({
    req(input$var)
    v <- input$var
    
    set.seed(230)
    n_show <- min(900, nrow(acc))
    ids <- sample(acc[[ID_COL]], n_show)
    
    dat_f <- filtered()
    in_filters <- ids %in% dat_f[[ID_COL]]
    
    idx <- match(ids, acc[[ID_COL]])
    vals <- acc[[v]][idx]
    x_num <- suppressWarnings(as.numeric(vals))
    valid <- is.finite(x_num)
    
    tf <- trim_flags()
    trim_on <- isTRUE(tf$trim_on)
    is_trimmed <- rep(FALSE, length(ids))
    if (trim_on && is.finite(tf$cut_lo) && is.finite(tf$cut_hi)) {
      is_trimmed <- (in_filters & valid) & (x_num < tf$cut_lo | x_num > tf$cut_hi)
    }
    
    grid <- make_dot_grid(n_show)
    grid$in_filters <- in_filters
    grid$valid <- valid
    grid$is_trimmed <- is_trimmed
    
    BLUE   <- "#2B6CB0"
    ORANGE <- "#D97706"
    GREEN  <- "#22A06B"
    PURPLE <- "#7C3AED"
    GREY   <- "#D9D9D9"
    
    stage_all <- transform(grid, stage = "All rows (N)", col = "#BFBFBF", alpha = 0.65)
    stage_filters <- transform(grid, stage = "After filters",
                               col = ifelse(in_filters, BLUE, GREY),
                               alpha = ifelse(in_filters, 0.85, 0.20))
    stage_missing <- transform(grid, stage = "Missing/invalid",
                               col = ifelse(!in_filters, GREY, ifelse(valid, BLUE, ORANGE)),
                               alpha = ifelse(!in_filters, 0.18, 0.85))
    stage_trim <- transform(grid, stage = "Trimmed",
                            col = ifelse(!in_filters, GREY,
                                         ifelse(!valid, GREY, ifelse(is_trimmed, PURPLE, GREY))),
                            alpha = ifelse(is_trimmed, 0.90, 0.12))
    stage_usable <- transform(grid, stage = "Usable n",
                              col = ifelse(in_filters & valid & !is_trimmed, GREEN, GREY),
                              alpha = ifelse(in_filters & valid & !is_trimmed, 0.90, 0.15))
    
    out <- rbind(stage_all, stage_filters, stage_missing, stage_trim, stage_usable)
    out$stage <- factor(out$stage,
                        levels = c("All rows (N)", "After filters", "Missing/invalid", "Trimmed", "Usable n"))
    out
  })
  
  output$dot_attrition <- renderPlot({
    dd <- dot_long()
    ggplot(dd, aes(x, y)) +
      geom_point(aes(color = col, alpha = alpha), size = 2.0) +
      scale_color_identity() +
      scale_alpha_identity() +
      coord_equal() +
      facet_wrap(~stage, ncol = 5) +
      theme_void(base_size = 13) +
      theme(strip.text = element_text(size = 12, face = "bold"),
            plot.margin = margin(0, 0, 0, 0))
  })
  
  # ---- Central tendency: point histogram ----
  output$center_point_hist <- renderPlot({
    xi <- x_info()
    v <- xi$v
    x <- x_trimmed()
    
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "Turn on 'Drop missing/invalid' to compute center."))
    }
    if (length(x) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "No usable data after filters/trim."))
    }
    
    rng <- range(x, na.rm = TRUE)
    nbins <- 30
    bw <- if (diff(rng) > 0) diff(rng) / nbins else 1
    
    breaks <- if (diff(rng) == 0) c(rng[1] - 0.5, rng[2] + 0.5) else seq(rng[1], rng[2], by = bw)
    if (tail(breaks, 1) < rng[2]) breaks <- c(breaks, rng[2] + bw)
    
    bin_id <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE)
    
    df <- data.frame(x = x, bin = bin_id) %>%
      group_by(bin) %>%
      mutate(y = row_number()) %>%
      ungroup()
    
    set.seed(230)
    df$xj <- df$x + runif(nrow(df), -0.35 * bw, 0.35 * bw)
    
    m <- mean(x)
    md <- median(x)
    
    p <- ggplot(df, aes(x = xj, y = y)) +
      geom_point(color = "#2b6cb0", alpha = 0.55, size = 1.05) +
      labs(x = v, y = "Count (stacked dots)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
    
    if (isTRUE(input$show_mean_median)) {
      p <- p +
        geom_vline(xintercept = md, color = "#111111", linewidth = 1.1) +
        geom_vline(xintercept = m,  color = "#22A06B", linewidth = 1.1, linetype = "dashed")
    }
    p
  })
  
  # ---- Spread: histogram ----
  output$spread_hist <- renderPlot({
    xi <- x_info()
    v <- xi$v
    x <- x_trimmed()
    qs <- quartiles()
    pv <- pct_values()
    
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "Turn on 'Drop missing/invalid' to compute spread."))
    }
    if (length(x) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "No usable data after filters/trim."))
    }
    
    df <- data.frame(x = x)
    m <- mean(x)
    md <- median(x)
    
    g <- ggplot(df, aes(x)) +
      geom_histogram(bins = 35, fill = "#2b6cb0", color = "white") +
      labs(x = v, y = "Count") +
      theme_minimal(base_size = 13)
    
    if (isTRUE(input$shade_iqr) && !is.null(qs)) {
      g <- g +
        annotate("rect",
                 xmin = qs$Q1, xmax = qs$Q3,
                 ymin = -Inf, ymax = Inf,
                 fill = "#E57373", alpha = 0.20)
    }
    
    if (isTRUE(input$show_mean_median)) {
      g <- g +
        geom_vline(xintercept = md, color = "#111111", linewidth = 1.1) +
        geom_vline(xintercept = m,  color = "#22A06B", linewidth = 1.1, linetype = "dashed")
    }
    
    if (isTRUE(input$show_percentiles) && !is.null(pv) && nrow(pv) > 0) {
      g <- g + geom_vline(xintercept = pv$value, color = "#C62828", linewidth = 1)
    }
    g
  })
  
  # ---- Boxplot tie-together: manual drawing ----
  output$boxplot_tie <- renderPlot({
    xi <- x_info()
    v  <- xi$v
    x  <- x_trimmed()
    
    if (!isTRUE(input$drop_na) && xi$n_missing_or_invalid > 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "Turn on 'Drop missing/invalid' to compute boxplot."))
    }
    if (length(x) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "No usable data after filters/trim."))
    }
    if (length(x) < 2) {
      return(
        ggplot(data.frame(x = x, y = 1), aes(x, y)) +
          geom_point(color = "#2b6cb0", size = 1.5) +
          scale_y_continuous(NULL, breaks = NULL) +
          labs(x = v, y = NULL, title = "Not enough data for a boxplot after trimming") +
          theme_minimal(base_size = 13)
      )
    }
    
    bp <- boxplot.stats(x)
    s5 <- bp$stats
    lowW <- s5[1]; q1 <- s5[2]; med <- s5[3]; q3 <- s5[4]; hiW <- s5[5]
    
    is_out <- (x < lowW) | (x > hiW)
    x_out <- x[is_out]
    x_in  <- x[!is_out]
    
    set.seed(230)
    if (length(x_in) > 3500) x_in <- sample(x_in, 3500)
    
    df_in  <- data.frame(x = x_in,  y = rep(1, length(x_in)))
    df_out <- data.frame(x = x_out, y = rep(1, length(x_out)))
    
    y0 <- 1
    box_w <- 0.10
    cap_w <- box_w * 0.80
    
    n_in <- nrow(df_in)
    jitter_h <- min(0.22, 0.05 + 0.04 * log10(n_in + 1))
    
    m <- mean(x)
    
    p <- ggplot() +
      geom_rect(aes(xmin = q1, xmax = q3, ymin = y0 - box_w/2, ymax = y0 + box_w/2),
                fill = "grey88", color = "grey35") +
      geom_segment(aes(x = med, xend = med, y = y0 - box_w/2, yend = y0 + box_w/2),
                   color = "grey35", linewidth = 1.1) +
      geom_segment(aes(x = lowW, xend = q1, y = y0, yend = y0), color = "grey35") +
      geom_segment(aes(x = q3, xend = hiW, y = y0, yend = y0), color = "grey35") +
      geom_segment(aes(x = lowW, xend = lowW, y = y0 - cap_w/2, yend = y0 + cap_w/2), color = "grey35") +
      geom_segment(aes(x = hiW,  xend = hiW,  y = y0 - cap_w/2, yend = y0 + cap_w/2), color = "grey35")
    
    if (isTRUE(input$show_box_dots) && nrow(df_in) > 0) {
      p <- p +
        geom_jitter(data = df_in, aes(x = x, y = y),
                    height = jitter_h, width = 0,
                    alpha = 0.25, size = 1.05, color = "#2b6cb0")
    }
    
    if (nrow(df_out) > 0) {
      p <- p +
        geom_point(data = df_out, aes(x = x, y = y),
                   alpha = 0.90, size = 1.35, color = "#2b6cb0")
    }
    
    if (isTRUE(input$show_mean_median)) {
      p <- p + geom_vline(xintercept = m, color = "#22A06B", linewidth = 1.1, linetype = "dashed")
    }
    
    p +
      scale_y_continuous(NULL, breaks = NULL, limits = c(1 - 0.28, 1 + 0.28)) +
      labs(x = v, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
  })
}

shinyApp(ui, server)