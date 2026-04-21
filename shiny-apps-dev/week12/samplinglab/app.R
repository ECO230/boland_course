# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("\u2014")
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("\u2014")
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "%")
}

generate_demo_data <- function(n = 120) {
  set.seed(23012)

  class_stand <- sample(
    c("First-year", "Sophomore", "Junior", "Senior", "Graduate student"),
    size = n,
    replace = TRUE,
    prob = c(0.17, 0.22, 0.27, 0.29, 0.05)
  )

  acad_area <- sample(
    c("Business", "Science or health", "Social sciences", "Arts or humanities", "Education", "Engineering or technology"),
    size = n,
    replace = TRUE,
    prob = c(0.36, 0.19, 0.16, 0.1, 0.08, 0.11)
  )

  attend <- sample(
    c("Almost always", "Often", "Sometimes", "Rarely"),
    size = n,
    replace = TRUE,
    prob = c(0.48, 0.28, 0.18, 0.06)
  )

  transport <- sample(
    c("Walk", "Bike or scooter", "Drive alone", "Carpool", "Bus or public transportation", "I do not usually come to campus"),
    size = n,
    replace = TRUE,
    prob = c(0.2, 0.05, 0.42, 0.08, 0.18, 0.07)
  )

  work_hrs <- pmax(0, round(rnorm(
    n,
    mean = ifelse(class_stand %in% c("Junior", "Senior"), 15, 10),
    sd = 8
  )))

  commute_min <- pmax(0, round(rnorm(
    n,
    mean = ifelse(transport %in% c("Drive alone", "Carpool"), 19, 11),
    sd = 7
  )))

  sleep_hrs <- pmin(24, pmax(2.5, round(rnorm(
    n,
    mean = 6.9 - pmin(work_hrs, 30) / 22,
    sd = 1.05
  ), 2)))

  bias_conf <- pmin(100, pmax(0, round(rnorm(
    n,
    mean = 57 +
      ifelse(attend == "Almost always", 8, 0) +
      ifelse(attend == "Rarely", -10, 0),
    sd = 16
  ))))

  conf_interp <- cut(
    bias_conf,
    breaks = c(-Inf, 20, 40, 60, 80, Inf),
    labels = c(
      "Not at all confident",
      "Slightly confident",
      "Moderately confident",
      "Very confident",
      "Extremely confident"
    ),
    ordered_result = TRUE
  )

  uses_ai <- sample(
    c("Yes", "No"),
    size = n,
    replace = TRUE,
    prob = c(0.62, 0.38)
  )

  hobby_pool <- c(
    "Speedcubing",
    "Birding",
    "Mini painting",
    "Powerlifting",
    "Cosplay sewing",
    "Climbing",
    "Chess tactics",
    "Vintage cameras",
    "Roller derby",
    "Soap making",
    "No especially obscure hobby"
  )

  hobby_text <- sample(hobby_pool, size = n, replace = TRUE)

  tibble(
    response_order = seq_len(n),
    class_stand = class_stand,
    acad_area = acad_area,
    attend = attend,
    transport = transport,
    commute_min = commute_min,
    work_hrs = work_hrs,
    sleep_hrs = sleep_hrs,
    conf_interp = as.character(conf_interp),
    bias_conf = bias_conf,
    uses_ai = uses_ai,
    hobby_text = hobby_text
  )
}

make_clean_data <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- make.names(names(df), unique = TRUE)

  if (!"response_order" %in% names(df)) {
    df$response_order <- seq_len(nrow(df))
  }

  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      suppressWarnings({
        numeric_version <- as.numeric(df[[nm]])
      })
      share_numeric <- mean(!is.na(numeric_version))
      if (is.finite(share_numeric) && share_numeric > 0.85) {
        df[[nm]] <- numeric_version
      }
    }
  }

  df
}

numeric_vars <- function(df) {
  keep <- vapply(df, function(x) is.numeric(x) && dplyr::n_distinct(x, na.rm = TRUE) >= 4, logical(1))
  names(df)[keep]
}

categorical_vars <- function(df) {
  keep <- vapply(df, function(x) {
    (!is.numeric(x) || dplyr::n_distinct(x, na.rm = TRUE) < 10) &&
      !all(is.na(x))
  }, logical(1))
  names(df)[keep]
}

estimate_value <- function(df, var, level = NULL) {
  x <- df[[var]]
  if (is.numeric(x)) {
    mean(x, na.rm = TRUE)
  } else {
    if (is.null(level) || !nzchar(level)) return(NA_real_)
    mean(x == level, na.rm = TRUE)
  }
}

estimate_label <- function(df, var, level = NULL) {
  x <- df[[var]]
  if (is.numeric(x)) {
    paste("Mean of", var)
  } else {
    paste("Proportion with", var, "=", level)
  }
}

sample_srs <- function(df, n) {
  df[sample(seq_len(nrow(df)), size = n, replace = FALSE), , drop = FALSE]
}

sample_convenience <- function(df, n, order_var) {
  ord <- order(df[[order_var]], na.last = TRUE)
  df[head(ord, n), , drop = FALSE]
}

sample_systematic <- function(df, n, order_var) {
  ord <- order(df[[order_var]], na.last = TRUE)
  df2 <- df[ord, , drop = FALSE]
  N <- nrow(df2)
  k <- max(1, floor(N / n))
  start <- sample.int(k, 1)
  idx <- seq(from = start, to = N, by = k)
  idx <- idx[seq_len(min(length(idx), n))]
  if (length(idx) < n) {
    leftovers <- setdiff(seq_len(N), idx)
    idx <- c(idx, sample(leftovers, n - length(idx), replace = FALSE))
  }
  df2[sort(idx), , drop = FALSE]
}

sample_stratified <- function(df, n, strata_var) {
  g <- df[[strata_var]]
  strata <- split(seq_len(nrow(df)), g)
  strata <- strata[lengths(strata) > 0]
  props <- lengths(strata) / nrow(df)
  target <- floor(props * n)
  remainder <- n - sum(target)
  if (remainder > 0) {
    order_idx <- order(props * n - target, decreasing = TRUE)
    target[order_idx[seq_len(remainder)]] <- target[order_idx[seq_len(remainder)]] + 1
  }
  sampled <- integer(0)
  for (i in seq_along(strata)) {
    ids <- strata[[i]]
    take <- min(length(ids), target[i])
    if (take > 0) sampled <- c(sampled, sample(ids, take, replace = FALSE))
  }
  if (length(sampled) < n) {
    leftovers <- setdiff(seq_len(nrow(df)), sampled)
    sampled <- c(sampled, sample(leftovers, n - length(sampled), replace = FALSE))
  }
  df[sampled, , drop = FALSE]
}

draw_sample <- function(df, method, n, order_var = "response_order", strata_var = NULL) {
  n <- min(n, nrow(df))
  switch(
    method,
    "Simple random sample" = sample_srs(df, n),
    "Convenience sample" = sample_convenience(df, n, order_var),
    "Systematic sample" = sample_systematic(df, n, order_var),
    "Stratified sample" = sample_stratified(df, n, strata_var),
    sample_srs(df, n)
  )
}

ui <- fluidPage(
  tags$style("
    h2 { margin-top: 0.25rem; }
    .small { opacity: 0.78; font-size: 0.96rem; }
    .box { background:#f7f7f7; border-radius:12px; padding:12px; margin-bottom:12px; }
    details.param { background:#f7f7f7; border-radius:12px; margin-bottom:12px; }
    details.param > summary { cursor:pointer; font-weight:700; padding:12px; list-style:none; }
    details.param > summary::-webkit-details-marker { display:none; }
    details.param .content { padding:12px; padding-top:0; }
    .metric { background:white; border:1px solid #e7e7e7; border-radius:12px; padding:12px; margin-bottom:12px; }
    .metricTitle { font-weight:800; font-size:0.95rem; color:#555; }
    .metricValue { font-weight:900; font-size:1.8rem; line-height:1.1; }
    .callout { border-left:6px solid #1b6ca8; background:rgba(27,108,168,.06); border-radius:12px; padding:12px; margin-bottom:12px; }
  "),

  h2("Sampling and Survey Error Lab"),
  div(class = "small", "Compare sampling methods against a known response pool. This app teaches random vs representative using either demo data or a survey export."),
  hr(),

  fluidRow(
    column(
      3,
      tags$details(
        class = "param",
        open = TRUE,
        tags$summary("Controls"),
        div(
          class = "content",
          radioButtons("data_mode", "Data source", choices = c("Demo data", "Upload CSV"), selected = "Demo data"),
          conditionalPanel(
            "input.data_mode == 'Upload CSV'",
            fileInput("csv_file", "CSV file", accept = c(".csv"))
          ),
          hr(),
          selectInput("outcome_var", "Outcome variable", choices = NULL),
          uiOutput("outcome_level_ui"),
          sliderInput("sample_n", "Sample size", min = 5, max = 60, value = 20, step = 1),
          selectInput(
            "method",
            "Sampling method",
            choices = c("Simple random sample", "Convenience sample", "Systematic sample", "Stratified sample")
          ),
          uiOutput("order_var_ui"),
          uiOutput("strata_var_ui"),
          selectInput("compare_var", "Compare representativeness by", choices = NULL),
          actionButton("draw_sample", "Draw sample"),
          hr(),
          sliderInput("n_reps", "Repeat samples", min = 50, max = 500, value = 200, step = 50),
          actionButton("simulate", "Simulate many samples")
        )
      ),
      div(
        class = "callout",
        strong("Teaching note"),
        p("The response pool is not the true population. It is the class data we can observe today. That makes it good for comparing methods, but not for claiming perfect truth."),
        uiOutput("method_note")
      )
    ),
    column(
      9,
      tabsetPanel(
        tabPanel(
          "Response pool",
          br(),
          fluidRow(
            column(4, div(class = "metric", div(class = "metricTitle", "Rows in response pool"), div(class = "metricValue", textOutput("n_pool", inline = TRUE)))),
            column(4, div(class = "metric", div(class = "metricTitle", "Outcome definition"), div(class = "small", textOutput("outcome_desc", inline = TRUE)))),
            column(4, div(class = "metric", div(class = "metricTitle", "Response-pool estimate"), div(class = "metricValue", textOutput("pool_estimate", inline = TRUE))))
          ),
          fluidRow(
            column(6, div(class = "box", h4("Variable snapshot"), tableOutput("var_snapshot"))),
            column(6, div(class = "box", h4("Response pool preview"), tableOutput("data_preview")))
          )
        ),
        tabPanel(
          "Sampling lab",
          br(),
          fluidRow(
            column(4, div(class = "metric", div(class = "metricTitle", "Current sample size"), div(class = "metricValue", textOutput("n_sample", inline = TRUE)))),
            column(4, div(class = "metric", div(class = "metricTitle", "Current sample estimate"), div(class = "metricValue", textOutput("sample_estimate", inline = TRUE)))),
            column(4, div(class = "metric", div(class = "metricTitle", "Difference from pool"), div(class = "metricValue", textOutput("estimate_gap", inline = TRUE))))
          ),
          fluidRow(
            column(6, div(class = "box", h4("Sample vs response pool"), plotOutput("compare_plot", height = 320))),
            column(6, div(class = "box", h4("Representativeness by subgroup"), plotOutput("composition_plot", height = 320)))
          ),
          fluidRow(
            column(12, div(class = "box", h4("Current sample rows"), tableOutput("sample_preview")))
          )
        ),
        tabPanel(
          "Repeat sampling",
          br(),
          fluidRow(
            column(12, div(class = "box", h4("Sampling distribution of the estimator"), plotOutput("sampling_dist_plot", height = 360), uiOutput("repeat_note")))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(current_sample = NULL, sim_results = NULL)

  uploaded_data <- reactive({
    req(input$csv_file)
    ext <- tools::file_ext(input$csv_file$name)
    validate(need(tolower(ext) == "csv", "Please upload a CSV file."))
    read_csv(input$csv_file$datapath, show_col_types = FALSE)
  })

  base_data <- reactive({
    if (identical(input$data_mode, "Upload CSV")) {
      make_clean_data(uploaded_data())
    } else {
      make_clean_data(generate_demo_data())
    }
  })

  observe({
    df <- base_data()
    num_vars <- setdiff(numeric_vars(df), "response_order")
    cat_vars <- setdiff(categorical_vars(df), "response_order")
    outcome_choices <- c(num_vars, cat_vars)
    updateSelectInput(session, "outcome_var", choices = outcome_choices, selected = outcome_choices[1] %||% "")
    updateSelectInput(session, "compare_var", choices = cat_vars, selected = cat_vars[1] %||% "")
    max_n <- max(5, min(100, nrow(df)))
    current_n <- min(input$sample_n %||% 20, max_n)
    updateSliderInput(session, "sample_n", max = max_n, value = current_n)
  })

  output$outcome_level_ui <- renderUI({
    req(input$outcome_var)
    df <- base_data()
    x <- df[[input$outcome_var]]
    if (is.numeric(x)) return(NULL)
    levs <- sort(unique(stats::na.omit(as.character(x))))
    selectInput("outcome_level", "Category to estimate", choices = levs, selected = levs[1] %||% "")
  })

  output$order_var_ui <- renderUI({
    if (!input$method %in% c("Convenience sample", "Systematic sample")) return(NULL)
    df <- base_data()
    vars <- names(df)
    selectInput("order_var", "Order by", choices = vars, selected = "response_order")
  })

  output$strata_var_ui <- renderUI({
    if (!identical(input$method, "Stratified sample")) return(NULL)
    df <- base_data()
    cat_vars <- setdiff(categorical_vars(df), "response_order")
    selectInput("strata_var", "Stratify by", choices = cat_vars, selected = cat_vars[1] %||% "")
  })

  current_estimate <- reactive({
    req(rv$current_sample, input$outcome_var)
    estimate_value(rv$current_sample, input$outcome_var, input$outcome_level %||% NULL)
  })

  pool_estimate <- reactive({
    req(input$outcome_var)
    estimate_value(base_data(), input$outcome_var, input$outcome_level %||% NULL)
  })

  observeEvent(input$draw_sample, {
    df <- base_data()
    rv$current_sample <- draw_sample(
      df = df,
      method = input$method,
      n = input$sample_n,
      order_var = input$order_var %||% "response_order",
      strata_var = input$strata_var %||% NULL
    )
  })

  observeEvent(base_data(), {
    df <- base_data()
    rv$current_sample <- draw_sample(df, "Simple random sample", min(20, nrow(df)))
  }, ignoreInit = FALSE)

  observeEvent(list(base_data(), input$outcome_var, input$outcome_level), {
    rv$sim_results <- NULL
  })

  observeEvent(input$simulate, {
    df <- base_data()
    req(nrow(df) >= 5)

    if (identical(input$method, "Convenience sample")) {
      rv$sim_results <- data.frame(
        rep = 1,
        estimate = estimate_value(
          draw_sample(df, input$method, input$sample_n, input$order_var %||% "response_order", input$strata_var %||% NULL),
          input$outcome_var,
          input$outcome_level %||% NULL
        )
      )
    } else {
      estimates <- numeric(input$n_reps)
      withProgress(message = "Drawing repeated samples", value = 0, {
        for (i in seq_len(input$n_reps)) {
          samp <- draw_sample(
            df = df,
            method = input$method,
            n = input$sample_n,
            order_var = input$order_var %||% "response_order",
            strata_var = input$strata_var %||% NULL
          )
          estimates[i] <- estimate_value(samp, input$outcome_var, input$outcome_level %||% NULL)
          if (i %% 20 == 0 || i == input$n_reps) setProgress(i / input$n_reps)
        }
      })
      rv$sim_results <- data.frame(rep = seq_len(input$n_reps), estimate = estimates)
    }
  })

  output$n_pool <- renderText(nrow(base_data()))
  output$outcome_desc <- renderText(estimate_label(base_data(), input$outcome_var, input$outcome_level %||% NULL))
  output$pool_estimate <- renderText({
    x <- base_data()[[input$outcome_var]]
    est <- pool_estimate()
    if (is.numeric(x)) fmt_num(est) else fmt_pct(est)
  })
  output$n_sample <- renderText({
    if (is.null(rv$current_sample)) return("0")
    nrow(rv$current_sample)
  })
  output$sample_estimate <- renderText({
    req(rv$current_sample)
    x <- base_data()[[input$outcome_var]]
    est <- current_estimate()
    if (is.numeric(x)) fmt_num(est) else fmt_pct(est)
  })
  output$estimate_gap <- renderText({
    req(rv$current_sample)
    x <- base_data()[[input$outcome_var]]
    gap <- current_estimate() - pool_estimate()
    if (is.numeric(x)) {
      ifelse(is.na(gap), "\u2014", sprintf("%+.2f", gap))
    } else {
      ifelse(is.na(gap), "\u2014", sprintf("%+.1f pts", 100 * gap))
    }
  })

  output$data_preview <- renderTable({
    head(base_data(), 8)
  }, striped = TRUE)

  output$sample_preview <- renderTable({
    req(rv$current_sample)
    head(rv$current_sample, 10)
  }, striped = TRUE)

  output$var_snapshot <- renderTable({
    df <- base_data()
    num <- setdiff(numeric_vars(df), "response_order")
    cat <- setdiff(categorical_vars(df), "response_order")
    data.frame(
      Numeric = c(num, rep("", max(0, length(cat) - length(num)))),
      Categorical = c(cat, rep("", max(0, length(num) - length(cat)))),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE)

  output$compare_plot <- renderPlot({
    req(rv$current_sample, input$outcome_var)
    x <- base_data()[[input$outcome_var]]
    df_plot <- data.frame(
      source = c("Response pool", "Current sample"),
      estimate = c(pool_estimate(), current_estimate())
    )

    ggplot(df_plot, aes(x = source, y = estimate, fill = source)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = if (is.numeric(x)) "Mean" else "Proportion") +
      theme_minimal(base_size = 14)
  })

  output$composition_plot <- renderPlot({
    req(rv$current_sample, input$compare_var)
    var <- input$compare_var
    pool <- base_data() %>%
      count(level = .data[[var]]) %>%
      mutate(source = "Response pool", pct = n / sum(n))
    samp <- rv$current_sample %>%
      count(level = .data[[var]]) %>%
      mutate(source = "Current sample", pct = n / sum(n))
    plot_df <- bind_rows(pool, samp)

    ggplot(plot_df, aes(x = level, y = pct, fill = source)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
      labs(x = NULL, y = "Percent of rows") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })

  output$sampling_dist_plot <- renderPlot({
    req(rv$sim_results)
    ggplot(rv$sim_results, aes(x = estimate)) +
      geom_histogram(bins = 20, fill = "#6FA8DC", color = "white") +
      geom_vline(xintercept = pool_estimate(), color = "#D34E4E", linewidth = 1.2) +
      labs(x = "Estimate across repeated samples", y = "Count") +
      theme_minimal(base_size = 14)
  })

  output$repeat_note <- renderUI({
    if (is.null(rv$sim_results)) {
      return(tags$p("Run repeated samples to see how the estimator moves around the response-pool value."))
    }
    if (identical(input$method, "Convenience sample")) {
      tags$p("Convenience sampling is deterministic here once the ordering rule is fixed, so repeated draws do not create a meaningful sampling distribution.")
    } else {
      tags$p(paste("The red line marks the response-pool estimate. The histogram shows how much your chosen method and sample size bounce around that value over repeated samples."))
    }
  })

  output$method_note <- renderUI({
    txt <- switch(
      input$method,
      "Simple random sample" = "Every row in the response pool has the same chance of selection. This is the cleanest benchmark method in the app.",
      "Convenience sample" = "Rows are taken from the top of an ordering variable. This is useful for showing how easy it is to get a quick sample that is not representative.",
      "Systematic sample" = "The app orders the rows, picks a random start, and then takes every k-th row. This works well until the ordering itself hides a pattern.",
      "Stratified sample" = "The app samples within groups to preserve subgroup balance. This is a good way to talk about representation as a design choice rather than a lucky accident."
    )
    tags$p(txt)
  })
}

shinyApp(ui, server)
