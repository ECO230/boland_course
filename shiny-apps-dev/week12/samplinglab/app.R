# MUST be at the very top of app.R, before any library(...)
if (dir.exists("/data/junior/boland_course") && requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(DBI)

source("comparison.R", local = TRUE)
source("repeated_comparison.R", local = TRUE)
source("recruitment.R", local = TRUE)
source("demonstration.R", local = TRUE)
source("open_responses.R", local = TRUE)

# Production must not silently substitute simulated responses for the survey.
default_data_mode <- "Live survey DB"
private_response_fields <- c("response_id", "response_order", "submitdate", "hometown_text", "hobby_text", "image_click_coords")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

fmt_num <- function(x, digits = 2) {
  if (is.na(x)) return("\u2014")
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("\u2014")
  paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "%")
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

var_labels <- c(
  submitdate = "Submission time",
  response_order = "Response order",
  age_band = "Age band",
  class_stand = "Class standing",
  major = "Major(s)",
  attend = "In-person attendance",
  commute_min = "Commute time (minutes)",
  work_hrs = "Work hours per week",
  sleep_hrs = "Sleep last night (hours)",
  conf_interp = "Confidence interpreting statistics",
  bias_conf = "Confidence explaining sampling bias (0-100)",
  hometown_text = "Hometown text",
  image_click_coords = "Image click coordinates",
  summer_lacrosse = "Lives in La Crosse over summer",
  hobby_text = "Obscure hobby or interest",
  transport = "Primary transport",
  acad_area = "Academic area",
  uses_ai = "Uses AI for coursework"
)

pretty_var_name <- function(x) {
  out <- unname(var_labels[x])
  ifelse(is.na(out), x, out)
}

teaching_outcome_vars <- c(
  "sleep_hrs",
  "work_hrs",
  "commute_min",
  "bias_conf",
  "class_stand",
  "major",
  "attend",
  "conf_interp",
  "summer_lacrosse",
  "age_band"
)

teaching_compare_vars <- c(
  "class_stand",
  "major",
  "attend",
  "conf_interp",
  "summer_lacrosse",
  "age_band"
)

parse_click_coords <- function(x) {
  if (length(x) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  pieces <- strsplit(trimws(x), ",")
  good <- vapply(pieces, function(p) length(p) == 2, logical(1))
  pieces <- pieces[good]
  if (!length(pieces)) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  xs <- suppressWarnings(as.numeric(vapply(pieces, `[`, character(1), 1)))
  ys <- suppressWarnings(as.numeric(vapply(pieces, `[`, character(1), 2)))
  out <- data.frame(x = xs, y = ys)
  out <- out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
  out[out$x >= 0 & out$x <= 100 & out$y >= 0 & out$y <= 100, , drop = FALSE]
}

interpret_sample <- function(pool_df, sample_df, outcome_var, outcome_level, compare_var) {
  pool_est <- estimate_value(pool_df, outcome_var, outcome_level)
  samp_est <- estimate_value(sample_df, outcome_var, outcome_level)
  gap <- samp_est - pool_est
  outcome_data <- pool_df[[outcome_var]]
  if (!is.finite(gap)) return("There are no usable outcome answers in the pool or this sample. Compare the answered counts before interpreting an estimate.")

  estimate_line <- if (is.numeric(outcome_data)) {
    if (abs(gap) < 0.15) {
      paste("The sample mean is very close to the response-pool mean for", pretty_var_name(outcome_var), ".")
    } else if (gap > 0) {
      paste("The sample mean is higher than the response-pool mean for", pretty_var_name(outcome_var), ".")
    } else {
      paste("The sample mean is lower than the response-pool mean for", pretty_var_name(outcome_var), ".")
    }
  } else {
    if (abs(gap) < 0.03) {
      paste("The sample proportion is close to the response-pool proportion for", outcome_level, ".")
    } else if (gap > 0) {
      paste("The sample overstates the proportion for", outcome_level, ".")
    } else {
      paste("The sample understates the proportion for", outcome_level, ".")
    }
  }

  comp_pool <- pool_df %>%
    count(level = .data[[compare_var]]) %>%
    mutate(pool_pct = n / sum(n))
  comp_sample <- sample_df %>%
    count(level = .data[[compare_var]]) %>%
    mutate(sample_pct = n / sum(n))

  comp <- full_join(comp_pool, comp_sample, by = "level") %>%
    mutate(
      pool_pct = dplyr::coalesce(pool_pct, 0),
      sample_pct = dplyr::coalesce(sample_pct, 0),
      diff = sample_pct - pool_pct
    )

  if (!nrow(comp)) {
    return(estimate_line)
  }

  top_over <- comp %>% arrange(desc(diff)) %>% slice(1)
  top_under <- comp %>% arrange(diff) %>% slice(1)

  balance_line <- if (abs(top_over$diff) < 0.05 && abs(top_under$diff) < 0.05) {
    paste("The sample looks fairly balanced across", pretty_var_name(compare_var), ".")
  } else {
    paste(
      "Compared with the response pool, this sample overrepresents",
      top_over$level,
      "and underrepresents",
      top_under$level,
      "on",
      pretty_var_name(compare_var),
      "."
    )
  }

  paste(estimate_line, balance_line)
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

  summer_lacrosse <- sample(rep(c("Yes", "No"), length.out = n))

  tibble(
    summer_lacrosse = summer_lacrosse,
    age_band = ifelse(class_stand %in% c("First-year", "Sophomore"), "18 - 20", "20 - 22"),
    major = sample(c("Accountancy", "Business Administration", "Economics", "Finance",
      "Information Systems", "International Business", "Management", "Marketing"), n, replace = TRUE),
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
      df[[nm]][!nzchar(trimws(df[[nm]]))] <- NA_character_
      if (nm %in% c(teaching_compare_vars, "transport", "acad_area", "uses_ai")) next
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

connect_limesurvey <- function() {
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("RMariaDB is not available in this Shiny runtime.")
  }

  required <- c("SURVEY_DB_HOST", "SURVEY_DB_NAME", "SURVEY_DB_USER", "SURVEY_DB_PASSWORD")
  if (any(!nzchar(Sys.getenv(required)))) stop("The live survey connection is not configured on this server.")

  DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("SURVEY_DB_HOST"),
    user = Sys.getenv("SURVEY_DB_USER"),
    password = Sys.getenv("SURVEY_DB_PASSWORD"),
    dbname = Sys.getenv("SURVEY_DB_NAME"),
    port = as.integer(Sys.getenv("SURVEY_DB_PORT", "3306"))
  )

}

load_limesurvey_responses <- function() {
  con <- connect_limesurvey()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT * FROM survey_931648 WHERE submitdate IS NOT NULL ORDER BY id ASC")
}

load_limesurvey_image_responses <- function() {
  con <- connect_limesurvey()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  # G03Q14: include every recorded answer, independent of completion or sampling.
  DBI::dbGetQuery(con,paste0("SELECT `931648X28X257` AS image_click_coords FROM survey_931648 ",
    "WHERE `931648X28X257` IS NOT NULL AND TRIM(`931648X28X257`) <> '' ORDER BY id ASC"))
}

clean_limesurvey_responses <- function(df) {
  if (!nrow(df)) return(df)

  class_map <- c(
    AO01 = "First-year",
    AO02 = "Sophomore",
    AO03 = "Junior",
    AO04 = "Senior (+)"
  )

  attend_map <- c(
    AO01 = "Almost always",
    AO02 = "Often",
    AO03 = "Sometimes",
    AO04 = "Rarely"
  )

  conf_interp_map <- c(
    AO01 = "Not at all confident",
    AO02 = "Slightly confident",
    AO03 = "Moderately confident",
    AO04 = "Extremely confident",
    AO05 = "Very confident"
  )

  age_map <- c(
    AO01 = "18 - 20",
    AO02 = "20 - 22",
    AO03 = "22 - 24"
  )

  yes_no_map <- c(
    Y = "Yes",
    N = "No"
  )

  major_labels <- c(
    "931648X26X216SQ001" = "Accountancy",
    "931648X26X216SQ002" = "Business Administration",
    "931648X26X216SQ003" = "Finance",
    "931648X26X216SQ004" = "Economics",
    "931648X26X216SQ005" = "Information Systems",
    "931648X26X216SQ006" = "International Business",
    "931648X26X216SQ007" = "Management",
    "931648X26X216SQ008" = "Marketing"
  )

  available_major <- intersect(names(major_labels), names(df))
  major <- rep(NA_character_, nrow(df))
  if (length(available_major)) {
    selected <- as.matrix(df[, available_major, drop = FALSE]) == "Y"
    selected[is.na(selected)] <- FALSE
    major <- apply(selected, 1, function(row) {
      labels <- unname(major_labels[available_major[row]])
      if (length(labels)) paste(labels, collapse = " + ") else NA_character_
    })
  }

  out <- tibble::tibble(
    response_id = df$id,
    submitdate = df$submitdate,
    response_order = seq_len(nrow(df)),
    age_band = unname(age_map[df$`931648X26X258`]),
    class_stand = unname(class_map[df$`931648X26X211`]),
    major = major,
    attend = unname(attend_map[df$`931648X26X225`]),
    commute_min = safe_numeric(df$`931648X26X226`),
    work_hrs = safe_numeric(df$`931648X26X227`),
    sleep_hrs = safe_numeric(df$`931648X26X228`),
    conf_interp = unname(conf_interp_map[df$`931648X27X229`]),
    bias_conf = safe_numeric(df$`931648X27X248SQ001`),
    hometown_text = df$`931648X28X256`,
    image_click_coords = df$`931648X28X257`,
    summer_lacrosse = unname(yes_no_map[df$`931648X28X259`]),
    hobby_text = df$`931648X28X260`
  )

  out
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
  if (!length(x) || all(is.na(x))) return(NA_real_)
  if (is.numeric(x)) {
    if (!any(is.finite(x))) return(NA_real_)
    mean(x[is.finite(x)])
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
  # Fractional intervals preserve equal inclusion chances when N/n is not
  # an integer; truncating every floor(N/n)-th row excludes the tail.
  k <- N / n
  start <- runif(1, min = 0, max = k)
  idx <- floor(start + (seq_len(n) - 1) * k) + 1L
  df2[sort(idx), , drop = FALSE]
}

sample_stratified <- function(df, n, strata_var) {
  sample_by_group(df, n, strata_var, random = TRUE)
}

draw_sample <- function(df, method, n, order_var = "response_order", strata_var = NULL) {
  n <- max(0L, min(as.integer(n), nrow(df)))
  if (n == 0L) return(df[integer(0), , drop = FALSE])
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
  tags$head(tags$script(src="vendor/wordcloud2/wordcloud2.js"),tags$script(src="response-cloud.js")),
  tags$style(htmltools::HTML("
    h2 { margin-top: 0.25rem; }
    .small { opacity: 0.85; font-size: 14px; }
    body { font-size: 16px; }
    .table { font-size: 15px; }
    .box { background:#f7f7f7; border-radius:4px; padding:12px; margin-bottom:12px; }
    details.param { background:#f7f7f7; border-radius:4px; margin-bottom:12px; }
    details.param > summary { cursor:pointer; font-weight:700; padding:12px; list-style:none; }
    details.param > summary::-webkit-details-marker { display:none; }
    details.param .content { padding:12px; padding-top:0; }
    .metric { background:white; border:1px solid #e7e7e7; border-radius:4px; padding:12px; margin-bottom:12px; }
    .metricTitle { font-weight:800; font-size:14px; color:#555; }
    .metricValue { font-weight:900; font-size:1.8rem; line-height:1.1; }
    .callout { border-left:6px solid #1b6ca8; background:rgba(27,108,168,.06); border-radius:4px; padding:12px; margin-bottom:12px; }
    .open-response-list li { padding: 10px 4px; border-bottom: 1px solid #ddd; white-space: pre-wrap; overflow-wrap: anywhere; }
    .response-cloud { width:100%; }
    .response-cloud canvas { display:block; width:100%; }
    .response-cloud-status { min-height:20px; }
    #heatmap_plot { max-width:1000px; margin:0 auto; aspect-ratio:318 / 314; }
    #heatmap_plot img { object-fit:contain; }
    .tab-pane[data-value='Explore']:has(#lab_tabs li.active a:is([data-value='Open-ended responses'],[data-value='Image heatmap'])) > .row > .col-sm-3 { display:none; }
    .tab-pane[data-value='Explore']:has(#lab_tabs li.active a:is([data-value='Open-ended responses'],[data-value='Image heatmap'])) > .row > .col-sm-9 { width:100%; }
  ")),

  h2("Sampling Lab"),
  div(class = "small", "Draw a sample, look at its result, then check who was included."),
  hr(),

  tabsetPanel(id = "app_mode",
    class_demo_ui(),
    tabPanel("Explore",
  fluidRow(
    column(
      3,
      tags$details(
          class = "param",
          open = TRUE,
          tags$summary("Controls"),
          div(
            class = "content",
          hr(),
          selectInput("outcome_var", "Outcome variable", choices = NULL),
          uiOutput("outcome_level_ui"),
          sliderInput("sample_n", "Sample size", min = 5, max = 60, value = 20, step = 1),
          selectInput("compare_var", "Compare group proportions", choices = NULL),
          selectInput("nonrandom_method", "Nonrandom selection", choices = c("Convenience sample", "Quota sample")),
          uiOutput("nonrandom_controls"),
          actionButton("redraw_random", "Draw another random sample", class = "btn-primary"),
          p(class = "small", "Both samples use the same size. Changing a selection rule or sample size starts a new comparison.")
        )
      ),
      div(
        class = "callout",
        strong("What are we comparing?"),
        p("The full response pool is everyone whose response is included here. Matching this pool does not establish that a sample represents the whole class or university. The survey may already exclude some people."),
        uiOutput("comparison_rule")
      )
    ),
    column(
      9,
      tabsetPanel(
        id = "lab_tabs",
        tabPanel(
          "Compare samples",
          br(),
          uiOutput("comparison_source"),
          h4("Same outcome, three groups"),
          textOutput("comparison_outcome_label"),
          tableOutput("comparison_estimates"),
          h4("Who is included?"),
          plotOutput("paired_composition_plot", height = 380),
          textOutput("paired_summary"),
          tags$details(
            tags$summary("Counts, percentages, and differences"),
            tableOutput("paired_composition_table")
          ),
          p("Does a sample that is close on the outcome also have similar group proportions? Try another comparison variable."),
          p(class = "small", "These comparisons describe the displayed variables. They do not give an overall pass/fail rating for representativeness.")
        ),
        many_samples_ui(),
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
        open_responses_ui(),
        tabPanel(
          "Image heatmap",
          br(),
          p("All recorded image responses from the class survey. Sampling controls do not filter this heat map."),
          actionButton("refresh_heatmap","Refresh heat map"),
          uiOutput("heatmap_source"),
          fluidRow(
            column(4, div(class = "metric", div(class = "metricTitle", "Recorded clicks"), div(class = "metricValue", textOutput("n_clicks", inline = TRUE)))),
            column(8, div(class = "callout", strong("What this shows"), p("Each point marks where a respondent clicked on the Escher image. Brighter regions show where class attention clustered.")))
          ),
          fluidRow(
            column(12, div(class = "box", h4("Class click map"), plotOutput("heatmap_plot", height = "auto")))
          )
        )
      )
    )
  )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(current_sample = NULL, sim_results = NULL, live_db_error = NULL, paired_random = NULL, paired_nonrandom = NULL, pair_rule = NULL, random_run = NULL, run_selected = 1L, live_loaded_at = NULL, controls_mode = NULL)

  uploaded_data <- reactive({
    req(input$csv_file)
    ext <- tools::file_ext(input$csv_file$name)
    validate(need(tolower(ext) == "csv", "Please upload a CSV file."))
    read_csv(input$csv_file$datapath, show_col_types = FALSE)
  })

  live_data <- reactive({
    req(identical(input$data_mode, "Live survey DB"))
    input$refresh_live
    tryCatch({
      rv$live_db_error <- NULL
      df <- clean_limesurvey_responses(load_limesurvey_responses())
      rv$live_loaded_at <- Sys.time()
      df
    }, error = function(e) {
      rv$live_db_error <- conditionMessage(e)
      NULL
    })
  })

  base_data <- reactive({
    if (identical(input$data_mode, "Upload CSV")) {
      make_clean_data(uploaded_data())
    } else if (identical(input$data_mode, "Live survey DB")) {
      df <- live_data()
      validate(need(!is.null(df), "Live survey responses are unavailable. Check the server connection or use the explicitly labeled demo option."))
      make_clean_data(df)
    } else {
      make_clean_data(generate_demo_data())
    }
  })

  output$live_db_status <- renderUI({
    if (identical(input$data_mode, "Live survey DB")) {
      df <- live_data()
      if (is.null(df)) {
        div(class = "small", style = "color:#8b1e1e;", "Unable to load completed survey responses. Check the server connection.")
      } else {
        div(class = "small", style = "color:#1b6c3f;", paste("Connected. Completed responses:", nrow(df)))
      }
    }
  })

  observeEvent(base_data(), {
    df <- base_data()
    validate(need(nrow(df) > 0, "No completed responses are available yet. Choose demo data or upload a CSV."))
    num_vars <- setdiff(numeric_vars(df), c("response_order", "response_id"))
    cat_vars <- setdiff(categorical_vars(df), c("response_order", "response_id", "submitdate", "hometown_text", "hobby_text", "image_click_coords"))
    outcome_choices <- intersect(c(teaching_outcome_vars, num_vars, cat_vars), c(num_vars, cat_vars))
    compare_vars <- intersect(c(teaching_compare_vars, cat_vars), cat_vars)
    outcome_named <- stats::setNames(outcome_choices, pretty_var_name(outcome_choices))
    compare_named <- stats::setNames(compare_vars, pretty_var_name(compare_vars))

    default_outcome <- if ("sleep_hrs" %in% outcome_choices) "sleep_hrs" else outcome_choices[1] %||% ""
    default_compare <- if ("class_stand" %in% compare_vars) "class_stand" else compare_vars[1] %||% ""

    current_outcome <- isolate(input$outcome_var)
    current_compare <- isolate(input$compare_var)
    if (length(current_outcome) && current_outcome %in% outcome_choices) default_outcome <- current_outcome
    if (length(current_compare) && current_compare %in% compare_vars) default_compare <- current_compare
    updateSelectInput(session, "outcome_var", choices = outcome_named, selected = default_outcome)
    updateSelectInput(session, "compare_var", choices = compare_named, selected = default_compare)
    max_n <- max(1, nrow(df))
    current_n <- min(isolate(input$sample_n) %||% 20, max_n)
    if (is.null(rv$controls_mode) || !identical(rv$controls_mode, input$data_mode)) {
      current_n <- min(20L, max(1L, floor(nrow(df) / 2)))
    }
    rv$controls_mode <- input$data_mode
    updateSliderInput(session, "sample_n", min = 1, max = max_n, value = max(1, current_n))
  })

  output$outcome_level_ui <- renderUI({
    req(input$outcome_var)
    df <- base_data()
    x <- df[[input$outcome_var]]
    if (is.numeric(x)) return(NULL)
    levs <- sort(unique(stats::na.omit(as.character(x))))
    selectInput("outcome_level", paste("Level for", pretty_var_name(input$outcome_var)), choices = levs, selected = levs[1] %||% "")
  })

  output$order_var_ui <- renderUI({
    if (!input$method %in% c("Convenience sample", "Systematic sample")) return(NULL)
    df <- base_data()
    vars <- names(df)
    vars <- setdiff(vars, c("response_id", "hobby_text", "hometown_text", "image_click_coords"))
    named_vars <- stats::setNames(vars, pretty_var_name(vars))
    default_order <- if ("submitdate" %in% vars) "submitdate" else if ("response_order" %in% vars) "response_order" else vars[1]
    selectInput("order_var", "Order by", choices = named_vars, selected = default_order)
  })

  output$strata_var_ui <- renderUI({
    if (!identical(input$method, "Stratified sample")) return(NULL)
    df <- base_data()
    safe_vars <- setdiff(categorical_vars(df), private_response_fields)
    cat_vars <- intersect(c(teaching_compare_vars, safe_vars), safe_vars)
    named_vars <- stats::setNames(cat_vars, pretty_var_name(cat_vars))
    default_strata <- if ("class_stand" %in% cat_vars) "class_stand" else cat_vars[1] %||% ""
    selectInput("strata_var", "Stratify by", choices = named_vars, selected = default_strata)
  })


  output$nonrandom_controls <- renderUI({
    df <- base_data()
    if (identical(input$nonrandom_method, "Quota sample")) {
      vars <- setdiff(categorical_vars(df), c("response_id", "response_order", "submitdate", "hobby_text", "hometown_text", "image_click_coords"))
      req(length(vars))
      selectInput("quota_var", "Match quotas for", choices = stats::setNames(vars, pretty_var_name(vars)),
                  selected = if ("class_stand" %in% vars) "class_stand" else vars[1])
    } else {
      vars <- intersect(c("response_order", "commute_min", "work_hrs", "sleep_hrs", "bias_conf"), names(df))
      if (!length(vars)) vars <- "response_order"
      tagList(
        selectInput("convenience_order", "Take the first rows after ordering by", choices = stats::setNames(vars, pretty_var_name(vars)), selected = vars[1]),
        p(class = "small", "Smallest values first; response order means the earliest rows in the pool. The selection rule stays fixed when you redraw the random sample.")
      )
    }
  })

  # Draw both samples atomically from the same pool and size. Outcome and
  # comparison-variable changes reuse the selected rows, rather than redrawing.
  observeEvent(list(base_data(), input$sample_n, input$nonrandom_method,
                    input$convenience_order, input$quota_var), {
    df <- base_data()
    req(nrow(df) > 0, input$sample_n, input$nonrandom_method)
    n <- min(as.integer(input$sample_n), nrow(df))
    if (identical(input$nonrandom_method, "Quota sample")) {
      req(input$quota_var, input$quota_var %in% names(df))
      nonrandom <- sample_by_group(df, n, input$quota_var, random = FALSE)
      rule <- paste("Quota sample: match the pool's", pretty_var_name(input$quota_var),
                    "proportions as closely as whole people allow, then take the first rows within each group. No random selection.")
    } else {
      req(input$convenience_order, input$convenience_order %in% names(df))
      nonrandom <- sample_convenience(df, n, input$convenience_order)
      rule <- paste("Convenience sample: take the first", n, "rows ordered by",
                    pretty_var_name(input$convenience_order), "(smallest first). No random selection.")
    }
    rv$paired_nonrandom <- nonrandom
    rv$paired_random <- sample_srs(df, n)
    rv$pair_rule <- rule
  }, ignoreNULL = FALSE)

  observeEvent(input$redraw_random, {
    req(rv$paired_nonrandom)
    rv$paired_random <- sample_srs(base_data(), nrow(rv$paired_nonrandom))
  })

  pair_data <- reactive({
    req(rv$paired_random, rv$paired_nonrandom)
    list("Full response pool" = base_data(), "Random sample" = rv$paired_random,
         "Nonrandom sample" = rv$paired_nonrandom)
  })

  pair_stats <- reactive({
    req(input$outcome_var)
    groups <- pair_data()
    is_numeric <- is.numeric(base_data()[[input$outcome_var]])
    if (!is_numeric) req(input$outcome_level)
    estimates <- vapply(groups, estimate_value, numeric(1), var = input$outcome_var,
                        level = input$outcome_level %||% NULL)
    answered <- vapply(groups, function(df) {
      x <- df[[input$outcome_var]]
      sum(if (is.numeric(x)) is.finite(x) else !is.na(x) & nzchar(trimws(as.character(x))))
    }, integer(1))
    data.frame(source = names(groups), rows = vapply(groups, nrow, integer(1)),
               answered = answered, estimate = estimates, numeric = is_numeric, row.names = NULL)
  })

  paired_composition <- reactive({
    req(input$compare_var)
    composition_comparison(base_data(), pair_data()[[2]], pair_data()[[3]], input$compare_var)
  })

  output$comparison_source <- renderUI({
    label <- switch(input$data_mode, "Demo data" = "Demo data (simulated responses)",
                    "Live survey DB" = "Completed class survey responses", "Uploaded CSV")
    tagList(
      p(strong(label), paste("| Full response pool:", nrow(base_data()), "rows")),
      if (identical(input$data_mode, "Live survey DB") && !is.null(rv$live_loaded_at))
        p(class = "small", paste("Last refreshed:", format(rv$live_loaded_at, "%I:%M %p %Z", tz = "America/Chicago"), ". Use Refresh survey responses after the class completes the survey."))
    )
  })
  output$comparison_rule <- renderUI({
    req(rv$pair_rule)
    tagList(p("Random sample: every row in this pool has an equal chance of selection."), p(rv$pair_rule))
  })
  output$comparison_outcome_label <- renderText({
    req(input$outcome_var)
    if (is.numeric(base_data()[[input$outcome_var]])) {
      paste("Mean:", pretty_var_name(input$outcome_var))
    } else paste("Percent:", pretty_var_name(input$outcome_var), "=", input$outcome_level %||% "")
  })
  output$comparison_estimates <- renderTable({
    stats <- pair_stats()
    gap <- stats$estimate - stats$estimate[1]
    data.frame(Group = stats$source, Rows = stats$rows, Answered = stats$answered,
      Estimate = if (stats$numeric[1]) vapply(stats$estimate, fmt_num, character(1)) else vapply(stats$estimate, fmt_pct, character(1)),
      `Difference from pool` = if (stats$numeric[1]) ifelse(is.finite(gap), sprintf("%+.2f", gap), "No answers") else ifelse(is.finite(gap), sprintf("%+.1f percentage points", 100 * gap), "No answers"), check.names = FALSE)
  }, striped = TRUE, rownames = FALSE)

  output$paired_composition_plot <- renderPlot({
    comp <- paired_composition()
    plot <- rbind(
      data.frame(level = comp$level, source = "Full response pool", pct = comp$pool_pct),
      data.frame(level = comp$level, source = "Random sample", pct = comp$random_pct),
      data.frame(level = comp$level, source = "Nonrandom sample", pct = comp$nonrandom_pct))
    plot$source <- factor(plot$source, levels = c("Full response pool", "Random sample", "Nonrandom sample"))
    ggplot(plot, aes(x = pct, y = level, fill = source)) +
      geom_col(position = position_dodge(width = .8), width = .7) +
      scale_fill_manual(values = c("Full response pool" = "#555555", "Random sample" = "#286ca5", "Nonrandom sample" = "#b86622"), name = NULL) +
      scale_x_continuous(limits = c(0,1), labels = function(x) paste0(round(x*100), "%")) +
      labs(x = "Percent of all rows (including no response)", y = pretty_var_name(input$compare_var)) +
      theme_minimal(base_size = 14) + theme(legend.position = "top")
  })

  output$paired_composition_table <- renderTable({
    comp <- paired_composition()
    cell <- function(n, pct) paste0(n, " (", vapply(pct, fmt_pct, character(1)), ")")
    data.frame(Group = comp$level,
      Pool = cell(comp$pool_n, comp$pool_pct), Random = cell(comp$random_n, comp$random_pct),
      `Random difference (pp)` = sprintf("%+.1f", 100*(comp$random_pct-comp$pool_pct)),
      Nonrandom = cell(comp$nonrandom_n, comp$nonrandom_pct),
      `Nonrandom difference (pp)` = sprintf("%+.1f", 100*(comp$nonrandom_pct-comp$pool_pct)), check.names = FALSE)
  }, striped = TRUE, rownames = FALSE)

  output$paired_summary <- renderText({
    comp <- paired_composition()
    paste0("Largest group difference from the pool on ", pretty_var_name(input$compare_var),
           ": random sample ", fmt_num(100*max(abs(comp$random_pct-comp$pool_pct)), 1),
           " percentage points; nonrandom sample ", fmt_num(100*max(abs(comp$nonrandom_pct-comp$pool_pct)), 1),
           " percentage points. Outcome estimates use answered rows; group percentages include missing responses.")
  })

  class_demonstration <- bind_class_demo(input, output, session, base_data, function() rv$live_loaded_at)
  raw_open_responses <- reactiveVal(NULL)
  open_refreshed_at <- reactiveVal(NULL)
  open_attempted <- reactiveVal(FALSE)
  refresh_open_data <- function() {
    actual <- tryCatch(load_limesurvey_open_responses(),error=function(e) NULL)
    raw_open_responses(actual)
    open_refreshed_at(if (is.null(actual)) NULL else Sys.time())
    open_attempted(TRUE)
  }
  observe({
    req(identical(input$app_mode,"Explore"),identical(input$lab_tabs,"Open-ended responses"))
    if (!isolate(open_attempted())) refresh_open_data()
  })
  observeEvent(input$refresh_open_responses,refresh_open_data())
  open_responses <- bind_open_responses(input,output,session,raw_open_responses,open_refreshed_at)

  live_image_responses <- reactiveVal(NULL)
  image_refreshed_at <- reactiveVal(NULL)
  image_attempted <- reactiveVal(FALSE)
  refresh_image_data <- function() {
    actual <- tryCatch(load_limesurvey_image_responses(),error=function(e) NULL)
    live_image_responses(actual)
    image_refreshed_at(if (is.null(actual)) NULL else Sys.time())
    image_attempted(TRUE)
  }
  observe({
    req(identical(input$app_mode,"Explore"),identical(input$lab_tabs,"Image heatmap"))
    if (!isolate(image_attempted())) refresh_image_data()
  })
  observeEvent(input$refresh_heatmap,refresh_image_data())
  repeated_comparison <- bind_many_samples(input, output, session, base_data, rv)

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

  output$interpretation_text <- renderText({
    req(rv$current_sample, input$outcome_var, input$compare_var)
    interpret_sample(
      pool_df = base_data(),
      sample_df = rv$current_sample,
      outcome_var = input$outcome_var,
      outcome_level = input$outcome_level %||% NULL,
      compare_var = input$compare_var
    )
  })

  output$data_preview <- renderTable({
    if (identical(input$data_mode, "Live survey DB"))
      return(data.frame(Note = "Individual response rows are not shown. Open-ended answers are listed separately in the Open-ended responses tab."))
    head(base_data(), 8)
  }, striped = TRUE)

  output$sample_preview <- renderTable({
    req(rv$current_sample)
    if (identical(input$data_mode, "Live survey DB"))
      return(data.frame(Note = "Individual response rows are not shown. Open-ended answers are listed separately in the Open-ended responses tab."))
    head(rv$current_sample, 10)
  }, striped = TRUE)

  output$var_snapshot <- renderTable({
    df <- base_data()
    num <- setdiff(numeric_vars(df), "response_order")
    cat <- setdiff(categorical_vars(df), "response_order")
    data.frame(
      Numeric = c(pretty_var_name(num), rep("", max(0, length(cat) - length(num)))),
      Categorical = c(pretty_var_name(cat), rep("", max(0, length(num) - length(cat)))),
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
      labs(
        x = NULL,
        y = if (is.numeric(x)) paste("Mean of", pretty_var_name(input$outcome_var)) else paste("Proportion:", input$outcome_level %||% "")
      ) +
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
      labs(x = pretty_var_name(var), y = "Percent of rows") +
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

  click_df <- reactive({
    df <- live_image_responses(); req(!is.null(df))
    parse_click_coords(stats::na.omit(df$image_click_coords))
  })

  output$heatmap_source <- renderUI({
    df <- live_image_responses()
    if (is.null(df)) return(p("The live survey connection is unavailable. No simulated clicks are shown."))
    tagList(p(strong("Live survey: G03Q14")),
      p(class="small",paste("Updated",format(image_refreshed_at(),"%I:%M:%S %p %Z",tz="America/Chicago"))),
      p(class="small",paste(nrow(df),"recorded answers;",nrow(df)-nrow(click_df()),"without plottable coordinates.")))
  })

  output$n_clicks <- renderText(nrow(click_df()))

  output$heatmap_plot <- renderPlot({
    pts <- click_df()

    img_paths <- c("/data/junior/boland_course/week12/media/images/Print_Gallery_by_M._C._Escher.jpg",
      "../../../week12/media/images/Print_Gallery_by_M._C._Escher.jpg")
    img_path <- img_paths[file.exists(img_paths)][1]
    has_jpeg <- requireNamespace("jpeg", quietly = TRUE)
    has_image <- !is.na(img_path) && file.exists(img_path)

    if (has_jpeg && has_image) {
      img <- jpeg::readJPEG(img_path)
      g <- grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
      p <- ggplot() +
        annotation_custom(g, xmin = 0, xmax = 100, ymin = 100, ymax = 0)
    } else {
      p <- ggplot() +
        annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "grey95", color = "grey70")
    }

    if (nrow(pts)>=3 && length(unique(pts$x))>1 && length(unique(pts$y))>1) {
      p <- p + stat_density_2d(
        data = pts,
        aes(x = x, y = y, fill = after_stat(level), alpha = after_stat(level)),
        geom = "polygon",
        contour = TRUE,
        na.rm = TRUE
      )
    }
    p + geom_point(
        data = pts,
        aes(x = x, y = y),
        color = "#D34E4E",
        alpha = 0.45,
        size = 3
      ) +
      scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
      scale_y_reverse(limits = c(100, 0), expand = c(0, 0)) +
      scale_fill_gradient(low = "#FDBFB6", high = "#A61E4D", guide = "none") +
      scale_alpha(range = c(0.08, 0.35), guide = "none") +
      coord_fixed(ratio = if (has_jpeg && has_image) dim(img)[1]/dim(img)[2] else 314/318, expand = FALSE) +
      labs(x = NULL, y = NULL) +
      theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
  }, execOnResize = TRUE)

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
      "Systematic sample" = "The app orders the rows, picks a random start, and takes evenly spaced rows. The interval is pool size divided by sample size; patterns in the ordering can affect the result.",
      "Stratified sample" = "The app samples within groups to preserve subgroup balance. This is a good way to talk about representation as a design choice rather than a lucky accident."
    )
    tags$p(txt)
  })
}

shinyApp(ui, server)
