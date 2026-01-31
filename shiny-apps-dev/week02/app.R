# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

# app.R
library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# ---- Load data ONCE at startup (important for hosting) ----
COURSE_ROOT <- "/data/junior/boland_course"
acc_path <- file.path(COURSE_ROOT, "shared", "data", "accident_wi.csv")

acc <- read_csv(acc_path, show_col_types = FALSE) %>%
  mutate(
    Start_Time = mdy_hm(Start_Time),
    End_Time   = mdy_hm(End_Time),
    Duration_min = as.numeric(difftime(End_Time, Start_Time, units = "mins"))
  )

# Candidate numeric variables for Week 2 topics (N, center, spread)
num_vars <- c(
  "Distance(mi)",
  "Temperature(F)",
  "Humidity(%)",
  "Wind_Speed(mph)",
  "Visibility(mi)",
  "Pressure(in)",
  "Precipitation(in)",
  "Duration_min"
)
num_vars <- intersect(num_vars, names(acc))

# Rounded mode helper: makes "mode" teachable for continuous variables
mode_rounded <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  xr <- round(as.numeric(x), digits)
  xr <- xr[is.finite(xr)]
  if (length(xr) == 0) return(NA_real_)
  tab <- table(xr)
  as.numeric(names(tab)[which.max(tab)])
}

# ---- UI ----
ui <- fluidPage(
  tags$style("
    h2{margin-top:0.2rem;}
    .small{opacity:0.75;font-size:0.95rem;}
    .box{background:#f7f7f7;border-radius:12px;padding:12px;margin-bottom:10px;}
    pre{margin:0;}
  "),
  h2("WI Accidents — Descriptive Stats"),
  div(class = "small", "Explore N, mean/median/mode, and SD with filters."),
  hr(),
  
  fluidRow(
    column(
      width = 4,
      div(class = "box",
          selectInput("var", "Numeric variable", choices = num_vars),
          selectInput(
            "sev", "Severity filter",
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
          checkboxInput("drop_na", "Drop missing values for selected variable", TRUE),
          sliderInput("mode_digits", "Mode rounding (digits)", min = 0, max = 2, value = 1, step = 1)
      )
    ),
    
    column(
      width = 8,
      fluidRow(
        column(4, div(class="box", verbatimTextOutput("n_text"))),
        column(4, div(class="box", verbatimTextOutput("center_text"))),
        column(4, div(class="box", verbatimTextOutput("spread_text")))
      ),
      div(class="box", plotOutput("hist", height = "340px"))
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered <- reactive({
    dat <- acc
    
    # severity
    if (!is.null(input$sev) && input$sev != "All") {
      dat <- dat %>% filter(Severity == as.integer(input$sev))
    }
    
    # date range (based on Start_Time)
    if (!is.null(input$dates)) {
      dat <- dat %>%
        filter(as.Date(Start_Time) >= input$dates[1],
               as.Date(Start_Time) <= input$dates[2])
    }
    
    dat
  })
  
  stats <- reactive({
    dat <- filtered()
    v <- input$var
    
    # N measures (the core teaching moment)
    n_total <- nrow(acc)
    n_after_filters <- nrow(dat)
    n_nonmissing <- sum(!is.na(dat[[v]]))
    
    x <- dat[[v]]
    if (isTRUE(input$drop_na)) x <- x[!is.na(x)]
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    
    list(
      n_total = n_total,
      n_after_filters = n_after_filters,
      n_nonmissing = n_nonmissing,
      n_usable = length(x),
      mean   = if (length(x)) mean(x) else NA_real_,
      median = if (length(x)) median(x) else NA_real_,
      mode   = if (length(x)) mode_rounded(x, digits = input$mode_digits) else NA_real_,
      sd     = if (length(x) > 1) sd(x) else NA_real_
    )
  })
  
  output$n_text <- renderText({
    s <- stats()
    paste0(
      "N (counts)\n",
      "Total rows:         ", s$n_total, "\n",
      "After filters:      ", s$n_after_filters, "\n",
      "Non-missing (var):  ", s$n_nonmissing, "\n",
      "Usable (for stats): ", s$n_usable
    )
  })
  
  output$center_text <- renderText({
    s <- stats()
    paste0(
      "Center\n",
      "Mean:   ", round(s$mean, 3), "\n",
      "Median: ", round(s$median, 3), "\n",
      "Mode*:  ", round(s$mode, 3), "\n",
      "*Mode computed on rounded values"
    )
  })
  
  output$spread_text <- renderText({
    s <- stats()
    paste0(
      "Spread\n",
      "SD: ", round(s$sd, 3)
    )
  })
  
  output$hist <- renderPlot({
    dat <- filtered()
    v <- input$var
    
    x <- dat[[v]]
    if (isTRUE(input$drop_na)) x <- x[!is.na(x)]
    x <- as.numeric(x)
    x <- x[is.finite(x)]
    
    ggplot(data.frame(x = x), aes(x)) +
      geom_histogram(bins = 35, fill = "#2b6cb0", color = "white") +
      labs(
        title = paste("Distribution of", v),
        x = v,
        y = "Count"
      ) +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
