# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)

# ----------------------------
# Helper functions
# ----------------------------
pop_sd <- function(x) {
  x <- x[is.finite(x)]
  mu <- mean(x)
  sqrt(mean((x - mu)^2))
}

make_summary <- function(x) {
  x <- x[is.finite(x)]
  data.frame(
    N = length(x),
    Mean = mean(x),
    SD = pop_sd(x),
    Min = min(x),
    Q1 = as.numeric(quantile(x, 0.25)),
    Median = median(x),
    Q3 = as.numeric(quantile(x, 0.75)),
    Max = max(x),
    stringsAsFactors = FALSE
  )
}

ci_for_sample <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  se <- s / sqrt(n)
  alpha <- 1 - conf
  tcrit <- qt(1 - alpha / 2, df = n - 1)
  low <- xbar - tcrit * se
  high <- xbar + tcrit * se
  list(mean = xbar, sd = s, se = se, low = low, high = high, n = n, tcrit = tcrit, conf = conf)
}

default_sampling_xlim <- function(mu, sigma, n) {
  sd_mean <- sigma / sqrt(n)
  lo <- mu - 4 * sd_mean
  hi <- mu + 4 * sd_mean
  c(floor(lo), ceiling(hi))
}

# ----------------------------
# Populations (3 real datasets, >50 points each)
# ----------------------------
POPULATIONS <- list(
  list(id = "iris_sepal",      name = "Iris (Sepal Length, n=150)",            data = iris,        value = iris$Sepal.Length, label = "Sepal.Length"),
  list(id = "airquality_temp", name = "Airquality (Daily Temperature, n=153)", data = airquality,  value = airquality$Temp,      label = "Temp"),
  list(id = "chickweight",     name = "ChickWeight (Weight, n=578)",           data = ChickWeight, value = ChickWeight$weight,   label = "weight")
)

pop_choices <- setNames(
  vapply(POPULATIONS, `[[`, character(1), "id"),
  vapply(POPULATIONS, `[[`, character(1), "name")
)

get_pop <- function(id) {
  for (p in POPULATIONS) {
    if (p$id == id) return(p)
  }
  POPULATIONS[[1]]
}

# ----------------------------
# Per-dataset display knobs (bin width + xlim lock)
# ----------------------------
POP_CONFIG <- list(
  iris_sepal      = list(binwidth = 3, xlim = NULL),
  airquality_temp = list(binwidth = 3, xlim = NULL),
  chickweight     = list(binwidth = 3, xlim = NULL)
)

get_cfg <- function(id, mu, sigma, n) {
  cfg <- POP_CONFIG[[id]]
  if (is.null(cfg)) cfg <- list()
  if (is.null(cfg$binwidth)) cfg$binwidth <- 3
  if (is.null(cfg$xlim)) cfg$xlim <- default_sampling_xlim(mu, sigma, n)
  cfg
}

# ----------------------------
# Colors
# ----------------------------
COL_BLUE  <- "#6FA8DC"
COL_RED   <- "#B35A5A"
COL_POP   <- "#D34E4E"
COL_CURVE <- "#666666"

# ----------------------------
# UI
# ----------------------------
ui <- navbarPage(
  title = "Sampling Distribution Simulator",
  tabPanel(
    "Simulator",
    fluidPage(
      fluidRow(
        column(
          3,
          div(
            style = "padding:14px; border-radius:14px; background: rgba(255,255,255,0.88); border:1px solid rgba(0,0,0,0.06);",
            selectInput("pop_id", "Population (known full dataset)", choices = pop_choices, selected = "chickweight"),
            hr(),
            selectInput("n", "Sample size (n)", choices = c(5, 10, 30, 50), selected = 30),
            sliderInput("conf", "Confidence level", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
            hr(),
            actionButton("add1", "Add 1 sample"),
            actionButton("run100", "Run 100 samples"),
            actionButton("clear", "Clear"),
            hr(),
            uiOutput("status_box"),
            hr(),
            strong("Current sample"),
            uiOutput("current_sample_text")
          )
        ),
        column(
          9,
          fluidRow(
            column(
              6,
              div(
                style = "padding:14px; border-radius:14px; background: rgba(255,255,255,0.88); border:1px solid rgba(0,0,0,0.06);",
                h4("Sampling distribution"),
                plotOutput("hist_plot", height = 300),
                hr(),
                h4("Current sample -> point estimate -> sampling distribution"),
                plotOutput("current_mean_dot_plot", height = 150),
                tableOutput("current_stats_tbl"),
                plotOutput("formula_plot", height = 150),
                plotOutput("current_ci_whisker_plot", height = 170)
              )
            ),
            column(
              6,
              div(
                style = "padding:14px; border-radius:14px; background: rgba(255,255,255,0.88); border:1px solid rgba(0,0,0,0.06);",
                h4("Confidence intervals"),
                plotOutput("ci_plot", height = 880),
                uiOutput("ci_error_text")
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Population data",
    fluidPage(
      fluidRow(
        column(
          3,
          div(
            style = "padding:14px; border-radius:14px; background: rgba(255,255,255,0.88); border:1px solid rgba(0,0,0,0.06);",
            selectInput("pop_id2", "Population", choices = pop_choices, selected = "chickweight"),
            hr(),
            h4("Population summary"),
            tableOutput("pop_summary_tbl")
          )
        ),
        column(
          9,
          div(
            style = "padding:14px; border-radius:14px; background: rgba(255,255,255,0.88); border:1px solid rgba(0,0,0,0.06);",
            h4("Full population values"),
            tableOutput("pop_data_tbl")
          )
        )
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    samples = data.frame(
      sample_id = integer(0),
      mean = numeric(0),
      sd = numeric(0),
      se = numeric(0),
      low = numeric(0),
      high = numeric(0),
      contains_mu = logical(0),
      stringsAsFactors = FALSE
    ),
    current_sample = numeric(0),
    current_ci = NULL,
    sample_id = 0L,
    target_total = 100L
  )
  
  # keep population selectors synced + clear on pop change
  observeEvent(input$pop_id, {
    updateSelectInput(session, "pop_id2", selected = input$pop_id)
    rv$samples <- rv$samples[0, ]
    rv$current_sample <- numeric(0)
    rv$current_ci <- NULL
    rv$sample_id <- 0L
  }, ignoreInit = TRUE)
  
  observeEvent(input$pop_id2, {
    updateSelectInput(session, "pop_id", selected = input$pop_id2)
  }, ignoreInit = TRUE)
  
  pop_vals <- reactive({
    p <- get_pop(input$pop_id)
    x <- p$value
    x <- x[is.finite(x)]
    x
  })
  
  pop_mu <- reactive(mean(pop_vals()))
  pop_sigma <- reactive(pop_sd(pop_vals()))
  
  clear_all <- function() {
    rv$samples <- rv$samples[0, ]
    rv$current_sample <- numeric(0)
    rv$current_ci <- NULL
    rv$sample_id <- 0L
  }
  
  take_one_sample <- function() {
    x <- pop_vals()
    n <- as.integer(input$n)
    conf <- input$conf
    
    samp <- sample(x, size = n, replace = TRUE)
    ci <- ci_for_sample(samp, conf = conf)
    
    rv$current_sample <- samp
    rv$current_ci <- ci
    
    rv$sample_id <- rv$sample_id + 1L
    sid <- rv$sample_id
    
    mu <- pop_mu()
    contains <- (ci$low <= mu && mu <= ci$high)
    
    rv$samples <- rbind(
      rv$samples,
      data.frame(
        sample_id = sid,
        mean = ci$mean,
        sd = ci$sd,
        se = ci$se,
        low = ci$low,
        high = ci$high,
        contains_mu = contains,
        stringsAsFactors = FALSE
      )
    )
  }
  
  observeEvent(input$add1, {
    take_one_sample()
  })
  
  observeEvent(input$clear, { clear_all() })
  
  # Run 100 samples quickly and redraw once at the end (no animation).
  observeEvent(input$run100, {
    clear_all()
    withProgress(message = "Running 100 samples...", value = 0, {
      for (i in seq_len(rv$target_total)) {
        take_one_sample()
        if (i %% 10 == 0) incProgress(0.10, detail = paste0("Sample ", i, " / ", rv$target_total))
      }
    })
  })
  
  output$status_box <- renderUI({
    total <- nrow(rv$samples)
    tags$div(
      tags$div(tags$strong("Step: "), paste0(total, " / ", rv$target_total)),
      tags$div(tags$strong("n: "), as.integer(input$n)),
      tags$div(tags$strong("CI: "), paste0(round(input$conf * 100), "%"))
    )
  })
  
  output$current_sample_text <- renderUI({
    s <- rv$current_sample
    if (length(s) == 0) {
      return(tags$div(style="color: rgba(0,0,0,0.6); font-size: 12px;", "Add a sample to display the values."))
    }
    txt <- paste0(round(s, 0), collapse = ", ")
    tags$div(
      style = "font-size: 12px; line-height: 1.35; padding: 8px; border-radius: 10px; background: rgba(0,0,0,0.03); border: 1px solid rgba(0,0,0,0.06); word-break: break-word;",
      txt
    )
  })
  
  output$current_stats_tbl <- renderTable({
    ci <- rv$current_ci
    if (is.null(ci)) return(data.frame())
    data.frame(
      n = ci$n,
      Mean = round(ci$mean, 2),
      SD = round(ci$sd, 2),
      SE = round(ci$se, 2),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, spacing = "s")
  
  # Plotmath formulas (no MathJax)
  # Plotmath formulas (no MathJax) - avoid chained equals for compatibility
  # Plotmath formulas (no MathJax) - 3-column layout to prevent crowding
  output$formula_plot <- renderPlot({
    ci <- rv$current_ci
    if (is.null(ci)) {
      plot.new()
      text(0.5, 0.5, "Add a sample to show formulas.", cex = 1)
      return()
    }
    
    alpha <- 1 - ci$conf
    df <- ci$n - 1
    tprob <- 1 - alpha/2
    
    op <- par(mar = c(0.4, 0.4, 0.4, 0.4))
    on.exit(par(op), add = TRUE)
    
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    
    # panel background
    rect(0.01, 0.05, 0.99, 0.95, border = NA, col = grDevices::adjustcolor("black", 0.03))
    rect(0.01, 0.05, 0.99, 0.95, border = grDevices::adjustcolor("black", 0.08), lwd = 1)
    
    # column anchors
    xL <- 0.05   # left: generic
    xM <- 0.40   # middle: plug-in calculation
    xR <- 0.74   # right: final numeric results
    
    # headings
    text(xL, 0.88, "Formulas", adj = 0, font = 2, cex = 0.95)
    text(xM, 0.88, "Plug in numbers", adj = 0, font = 2, cex = 0.95)
    text(xR, 0.88, "Results", adj = 0, font = 2, cex = 0.95)
    
    # --- Standard error row ---
    text(xL, 0.72, expression(SE == frac(s, sqrt(n))), adj = 0, cex = 1.00)
    text(xM, 0.74, bquote(SE == frac(.(round(ci$sd, 3)), sqrt(.(ci$n)))), adj = 0, cex = 0.92)
    text(xR, 0.74, bquote(SE == .(round(ci$se, 3))), adj = 0, cex = 0.95)
    
    # --- CI bounds row ---
    text(xL, 0.52, expression(bar(x) %+-% t^"*" %.% SE), adj = 0, cex = 1.00)
    
    # show t* calculation (middle) and the final t* value (right)
    text(xM, 0.56, bquote(t^"*" == t[.(round(tprob, 3)), df == .(df)]), adj = 0, cex = 0.86)
    text(xR, 0.56, bquote(t^"*" == .(round(ci$tcrit, 3))), adj = 0, cex = 0.90)
    
    # show CI plug-in (middle) and final interval (right)
    text(xM, 0.42, bquote(CI == .(round(ci$mean, 3)) %+-% .(round(ci$tcrit, 3)) %.% .(round(ci$se, 3))), adj = 0, cex = 0.86)
    text(xR, 0.42, bquote(CI == "[" * .(round(ci$low, 3)) * "," * .(round(ci$high, 3)) * "]"), adj = 0, cex = 0.90)
  })
  
  output$hist_plot <- renderPlot({
    means <- rv$samples$mean
    mu <- pop_mu()
    sigma <- pop_sigma()
    n <- as.integer(input$n)
    
    cfg <- get_cfg(input$pop_id, mu, sigma, n)
    binw <- cfg$binwidth
    xlim <- cfg$xlim
    
    sd_mean <- sigma / sqrt(n)
    brks <- seq(xlim[1], xlim[2] + binw, by = binw)
    
    if (length(means) > 0) {
      h <- hist(means, breaks = brks, plot = FALSE)
      ymax <- max(10, max(h$counts, 0)) + 2
    } else {
      h <- NULL
      ymax <- 10
    }
    
    plot.new()
    plot.window(xlim = xlim, ylim = c(0, ymax))
    axis(1); axis(2)
    title(main = NULL, xlab = "Sample mean", ylab = "Count")
    box()
    
    if (!is.null(h)) {
      for (i in seq_along(h$counts)) {
        if (h$counts[i] <= 0) next
        rect(h$breaks[i], 0, h$breaks[i+1], h$counts[i], col = COL_BLUE, border = NA)
      }
    }
    
    abline(v = mu, col = COL_POP, lwd = 2)
    
    xs <- seq(xlim[1], xlim[2], length.out = 400)
    dens <- dnorm(xs, mean = mu, sd = sd_mean)
    if (max(dens) > 0) {
      ys <- dens / max(dens) * (0.90 * ymax)
      lines(xs, ys, col = COL_CURVE, lwd = 2)
    }
    
    mtext("Gray curve: theoretical sampling distribution", side = 3, line = 0.2, cex = 0.8)
  })
  
  output$current_mean_dot_plot <- renderPlot({
    ci <- rv$current_ci
    mu <- pop_mu()
    n <- as.integer(input$n)
    cfg <- get_cfg(input$pop_id, mu, pop_sigma(), n)
    xlim <- cfg$xlim
    
    plot.new()
    plot.window(xlim = xlim, ylim = c(0, 1))
    axis(1)
    box()
    mtext("Current sample mean (point estimate)", side = 3, line = 0.2, cex = 0.9)
    
    segments(mu, 0, mu, 1, col = COL_POP, lwd = 2)
    if (!is.null(ci)) points(ci$mean, 0.5, pch = 16, col = COL_BLUE, cex = 1.2)
  })
  
  output$current_ci_whisker_plot <- renderPlot({
    ci <- rv$current_ci
    mu <- pop_mu()
    n <- as.integer(input$n)
    cfg <- get_cfg(input$pop_id, mu, pop_sigma(), n)
    xlim <- cfg$xlim
    
    plot.new()
    plot.window(xlim = xlim, ylim = c(0, 1))
    axis(1)
    box()
    mtext("Confidence interval for the current sample (same scale)", side = 3, line = 0.2, cex = 0.9)
    
    segments(mu, 0, mu, 1, col = COL_POP, lwd = 2)
    
    if (!is.null(ci)) {
      contains <- (ci$low <= mu && mu <= ci$high)
      col <- if (contains) COL_BLUE else COL_RED
      segments(ci$low, 0.5, ci$high, 0.5, col = col, lwd = 3)
      points(ci$mean, 0.5, pch = 16, col = col, cex = 1.1)
    }
  })
  
  output$ci_plot <- renderPlot({
    df <- rv$samples
    mu <- pop_mu()
    n <- as.integer(input$n)
    cfg <- get_cfg(input$pop_id, mu, pop_sigma(), n)
    xlim <- cfg$xlim
    
    if (nrow(df) == 0) {
      plot.new()
      title("Add samples to build confidence intervals")
      text(0.5, 0.5, "No confidence intervals yet", cex = 1.2)
      return()
    }
    
    if (nrow(df) > 100) df <- df[(nrow(df) - 99):nrow(df), ]
    
    y <- df$sample_id
    cols <- ifelse(df$contains_mu, COL_BLUE, COL_RED)
    
    ymin <- max(1, max(y) - 99)
    ymax <- max(100, max(y))
    
    plot(
      NA,
      xlim = xlim,
      ylim = c(ymin - 2, ymax + 2),
      xlab = "Value",
      ylab = "Sample #",
      main = NULL
    )
    
    abline(v = mu, lwd = 2, col = COL_POP)
    segments(df$low, y, df$high, y, col = cols, lwd = 2)
    points(df$mean, y, pch = 16, col = cols, cex = 0.85)
    
    mtext(paste0("Confidence Interval: ", round(input$conf * 100), "%"), side = 3, line = 0.2, cex = 0.9)
  })
  
  output$ci_error_text <- renderUI({
    df <- rv$samples
    if (nrow(df) == 0) return(NULL)
    errs <- sum(!df$contains_mu)
    tags$div(tags$strong("CI errors: "), paste0(errs))
  })
  
  output$pop_summary_tbl <- renderTable({
    p <- get_pop(input$pop_id2)
    x <- p$value
    x <- x[is.finite(x)]
    make_summary(x)
  }, striped = TRUE, spacing = "s")
  
  output$pop_data_tbl <- renderTable({
    p <- get_pop(input$pop_id2)
    x <- p$value
    x <- x[is.finite(x)]
    data.frame(Value = round(x, 3))
  }, striped = TRUE, spacing = "s")
}

shinyApp(ui, server)
