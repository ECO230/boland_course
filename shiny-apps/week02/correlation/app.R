# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

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

# Candidate numeric variables (match your descriptives app)
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
if (length(num_vars) < 2) stop("Need at least 2 numeric variables in the dataset to run this app.")

# ---- Helpers ----
safe_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || !is.finite(x)) return("NA")
  format(round(x, digits), nsmall = digits, trim = TRUE)
}

cor_summary <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 3) {
    return(list(n = n, r = NA_real_, p = NA_real_, slope = NA_real_, intercept = NA_real_))
  }
  ct <- tryCatch(stats::cor.test(x, y, method = method), error = function(e) NULL)
  r <- if (!is.null(ct)) unname(ct$estimate) else NA_real_
  p <- if (!is.null(ct)) ct$p.value else NA_real_
  
  # slope/intercept for teaching (only meaningful for lm)
  fit <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
  if (is.null(fit)) {
    slope <- NA_real_; intercept <- NA_real_
  } else {
    coefv <- stats::coef(fit)
    intercept <- unname(coefv[1])
    slope <- unname(coefv[2])
  }
  list(n = n, r = r, p = p, slope = slope, intercept = intercept)
}

# Create canned example datasets
make_example <- function(kind = "Linear (set r)", n = 600, rho = 0.7, seed = 230, noise = 0.6,
                         y_scale = 1.0, y_flip = FALSE) {
  set.seed(seed)
  
  if (kind == "Linear (set r)") {
    # Generate x ~ N(0,1), y = rho*x + sqrt(1-rho^2)*e  (Pearson r ~ rho)
    x <- rnorm(n)
    e <- rnorm(n)
    y <- rho * x + sqrt(pmax(0, 1 - rho^2)) * e
    
    # Scale changes slope, not correlation (unless sign flips)
    y <- y * y_scale
    if (isTRUE(y_flip)) y <- -y
    
    desc <- paste0("Bivariate normal with target r ≈ ", rho,
                   ". Scaling/flip changes slope, not linearity.")
    return(list(df = data.frame(x = x, y = y), xlab = "x", ylab = "y", desc = desc))
  }
  
  if (kind == "U-shape (r ~ 0, nonlinear)") {
    x <- runif(n, -3, 3)
    y <- x^2 + rnorm(n, sd = noise)
    # Center y to stabilize r near 0
    y <- y - mean(y)
    desc <- "Clear nonlinear relationship (U-shape). Pearson r can be ~0 even when pattern is strong."
    return(list(df = data.frame(x = x, y = y), xlab = "x", ylab = "y", desc = desc))
  }
  
  if (kind == "Sine wave (r ~ 0, nonlinear)") {
    x <- runif(n, 0, 2*pi)
    y <- sin(x) + rnorm(n, sd = noise/2)
    desc <- "Nonlinear periodic relationship. Pearson r often ~0 across full cycle(s)."
    return(list(df = data.frame(x = x, y = y), xlab = "x", ylab = "sin(x)", desc = desc))
  }
  
  if (kind == "Circle (r ~ 0, nonlinear)") {
    t <- runif(n, 0, 2*pi)
    x <- cos(t) + rnorm(n, sd = noise/6)
    y <- sin(t) + rnorm(n, sd = noise/6)
    desc <- "Circle: strong structure, but no linear association → r ~ 0."
    return(list(df = data.frame(x = x, y = y), xlab = "x", ylab = "y", desc = desc))
  }
  
  if (kind == "X-shape mixture (r ~ 0)") {
    x <- rnorm(n)
    flip <- rbinom(n, 1, 0.5)
    y <- ifelse(flip == 1, x, -x) + rnorm(n, sd = noise/3)
    desc <- "Mixture of positive and negative lines. Cancels linear trend → r ~ 0."
    return(list(df = data.frame(x = x, y = y), xlab = "x", ylab = "y", desc = desc))
  }
  
  # Fallback
  x <- rnorm(n); y <- rnorm(n)
  list(df = data.frame(x = x, y = y), xlab = "x", ylab = "y", desc = "Random noise.")
}

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
      border-left: 6px solid #7C3AED;
      background: rgba(124,58,237,.06);
      border-radius: 12px;
      padding: 12px;
      margin-bottom: 10px;
    }
    .kpi { display:flex; gap:10px; flex-wrap:wrap; }
    .kpi .pill {
      background:white; border:1px solid #e6e6e6; border-radius:999px;
      padding:8px 12px; font-weight:700;
    }
  "),
  
  h2("WI Accidents — Correlation (Linearity ≠ Slope)"),
  div(class="small",
      "Compare two numeric measures from the same filtered dataset, and use examples to see why r measures linearity—not 'steepness'."),
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
          
          tabsetPanel(
            type = "tabs",
            
            tabPanel(
              "Your Data",
              selectInput("x_var", "X variable", choices = num_vars, selected = num_vars[1]),
              selectInput("y_var", "Y variable", choices = num_vars, selected = num_vars[min(2, length(num_vars))]),
              
              selectInput(
                "sev_filter", "Severity filter",
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
              
              checkboxInput("drop_na", "Drop missing/invalid values (recommended)", TRUE),
              
              hr(),
              
              checkboxInput("use_log_x", "Log10 transform X (for heavy tails)", FALSE),
              checkboxInput("use_log_y", "Log10 transform Y (for heavy tails)", FALSE),
              
              hr(),
              
              checkboxInput("show_lm", "Show linear fit (lm)", TRUE),
              checkboxInput("show_loess", "Show loess (nonlinear smoother)", FALSE)
            ),
            
            tabPanel(
              "Canned Examples",
              selectInput(
                "ex_kind",
                "Example dataset",
                choices = c(
                  "Linear (set r)",
                  "U-shape (r ~ 0, nonlinear)",
                  "Sine wave (r ~ 0, nonlinear)",
                  "Circle (r ~ 0, nonlinear)",
                  "X-shape mixture (r ~ 0)"
                ),
                selected = "Linear (set r)"
              ),
              
              sliderInput("ex_n", "Number of points", min = 100, max = 3000, value = 700, step = 50),
              
              # only relevant for linear example
              conditionalPanel(
                condition = "input.ex_kind == 'Linear (set r)'",
                sliderInput("ex_rho", "Target Pearson r", min = -0.95, max = 0.95, value = 0.70, step = 0.05),
                sliderInput("ex_y_scale", "Scale Y (changes slope, not r)", min = 0.2, max = 5, value = 1, step = 0.1),
                checkboxInput("ex_y_flip", "Flip sign of Y (changes direction)", FALSE)
              ),
              
              conditionalPanel(
                condition = "input.ex_kind != 'Linear (set r)'",
                sliderInput("ex_noise", "Noise (jitter)", min = 0.0, max = 2.0, value = 0.6, step = 0.1)
              ),
              
              numericInput("ex_seed", "Seed", value = 230, min = 1, max = 999999),
              
              hr(),
              
              checkboxInput("show_lm_ex", "Show linear fit (lm)", TRUE),
              checkboxInput("show_loess_ex", "Show loess (nonlinear smoother)", TRUE)
            )
          ),
          
          hr(),
          
          selectInput(
            "cor_method",
            "Correlation method (computed on plotted data)",
            choices = c("Pearson (linear)" = "pearson", "Spearman (rank)" = "spearman"),
            selected = "pearson"
          )
        )
      ),
      
    ),
    
    column(
      width = 8,
      
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Scatter + Correlation",
          div(class="box",
              strong("Scatterplot (same data used for r)"),
              div(class="small", "Use lm for linear trend; turn on loess to reveal curved/nonlinear structure."),
              plotOutput("scatter_plot", height = "360px")
          ),
          fluidRow(
            column(6, div(class="box", uiOutput("kpi_ui"))),
            column(6, div(class="box", verbatimTextOutput("notes_text")))
          )
        ),
        
        tabPanel(
          "Examples Gallery",
          div(class="box",
              strong("Canned examples (quick demonstration)"),
              div(class="small", "These are synthetic datasets designed to teach what r does (and does not) capture."),
              plotOutput("example_plot", height = "420px")
          ),
          div(class="box", uiOutput("example_kpi_ui"))
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # ---- Your data: filtered + numeric pair ----
  filtered <- reactive({
    dat <- acc
    if (!is.null(input$sev_filter) && input$sev_filter != "All") {
      dat <- dat %>% filter(Severity == as.integer(input$sev_filter))
    }
    if (!is.null(input$dates)) {
      dat <- dat %>%
        filter(as.Date(Start_Time) >= input$dates[1],
               as.Date(Start_Time) <= input$dates[2])
    }
    dat
  })
  
  pair_df <- reactive({
    dat <- filtered()
    xv <- input$x_var
    yv <- input$y_var
    if (is.null(xv) || is.null(yv) || !xv %in% names(dat) || !yv %in% names(dat)) {
      return(data.frame(x = numeric(0), y = numeric(0)))
    }
    
    x <- suppressWarnings(as.numeric(dat[[xv]]))
    y <- suppressWarnings(as.numeric(dat[[yv]]))
    
    ok <- is.finite(x) & is.finite(y)
    
    if (!isTRUE(input$drop_na)) {
      # If they refuse dropping NA/invalid, we still must avoid crashing.
      # We'll keep only finite pairs for plots/cor, and explain in notes.
      ok <- ok
    }
    
    df <- data.frame(x = x[ok], y = y[ok])
    
    # optional log transforms for visualization
    if (isTRUE(input$use_log_x)) {
      df <- df %>% filter(x > 0) %>% mutate(x = log10(x))
    }
    if (isTRUE(input$use_log_y)) {
      df <- df %>% filter(y > 0) %>% mutate(y = log10(y))
    }
    
    df
  })
  
  # ---- Canned example dataset ----
  example_obj <- reactive({
    kind <- input$ex_kind %||% "Linear (set r)"
    n <- input$ex_n %||% 700
    seed <- input$ex_seed %||% 230
    
    rho <- input$ex_rho %||% 0.7
    y_scale <- input$ex_y_scale %||% 1
    y_flip <- isTRUE(input$ex_y_flip)
    
    noise <- input$ex_noise %||% 0.6
    
    make_example(kind = kind, n = n, rho = rho, seed = seed, noise = noise, y_scale = y_scale, y_flip = y_flip)
  })
  
  # ---- KPIs for YOUR DATA ----
  output$kpi_ui <- renderUI({
    df <- pair_df()
    method <- input$cor_method %||% "pearson"
    
    summ <- cor_summary(df$x, df$y, method = method)
    summ_p <- cor_summary(df$x, df$y, method = "pearson")
    summ_s <- cor_summary(df$x, df$y, method = "spearman")
    
    # p-value formatting
    ptxt <- if (!is.finite(summ$p)) "NA" else if (summ$p < 0.001) "< 0.001" else fmt_num(summ$p, 3)
    
    tagList(
      div(class="kpi",
          div(class="pill", paste0("n = ", summ$n)),
          div(class="pill", paste0(if (method == "pearson") "Pearson r = " else "Spearman ρ = ", fmt_num(summ$r, 3))),
          div(class="pill", paste0("p = ", ptxt))
      ),
      div(class="small", style="margin-top:10px;",
          paste0("Also: Pearson r = ", fmt_num(summ_p$r, 3),
                 " | Spearman ρ = ", fmt_num(summ_s$r, 3)))
    )
  })
  
  output$notes_text <- renderText({
    df <- pair_df()
    if (nrow(df) == 0) {
      return("Notes\nNo usable paired observations after filtering/transforms.\nTry different variables, widen dates, or turn off log transforms.")
    }
    method <- input$cor_method %||% "pearson"
    summ <- cor_summary(df$x, df$y, method = method)
    
    slope_txt <- if (is.finite(summ$slope)) fmt_num(summ$slope, 4) else "NA"
    int_txt   <- if (is.finite(summ$intercept)) fmt_num(summ$intercept, 4) else "NA"
    
    paste0(
      "Notes\n",
      "Correlation measures linear association (strength + direction).\n",
      "Slope depends on units/scales; r does not.\n\n",
      "lm(y ~ x) (teaching):\n",
      "  intercept = ", int_txt, "\n",
      "  slope     = ", slope_txt, "\n",
      "Tip: Turn ON loess if you suspect a curved relationship.\n"
    )
  })
  
  # ---- Scatter plot for YOUR DATA ----
  output$scatter_plot <- renderPlot({
    df <- pair_df()
    xv <- input$x_var %||% "X"
    yv <- input$y_var %||% "Y"
    
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "No usable paired data.\nTry different variables or filters."))
    }
    
    xlab <- xv
    ylab <- yv
    if (isTRUE(input$use_log_x)) xlab <- paste0("log10(", xv, ")")
    if (isTRUE(input$use_log_y)) ylab <- paste0("log10(", yv, ")")
    
    p <- ggplot(df, aes(x, y)) +
      geom_point(color = "#2b6cb0", alpha = 0.35, size = 1.1) +
      labs(x = xlab, y = ylab) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
    
    if (isTRUE(input$show_lm) && nrow(df) >= 3) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "#111111", linewidth = 1.1)
    }
    if (isTRUE(input$show_loess) && nrow(df) >= 25) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "#7C3AED", linewidth = 1.2)
    }
    p
  })
  
  # ---- Example plot + KPIs ----
  output$example_plot <- renderPlot({
    ex <- example_obj()
    df <- ex$df
    
    method <- input$cor_method %||% "pearson"
    
    if (nrow(df) == 0) {
      return(ggplot() + theme_void() +
               annotate("text", x = 0, y = 0, size = 5,
                        label = "Example generation failed (unexpected)."))
    }
    
    p <- ggplot(df, aes(x, y)) +
      geom_point(color = "#4C51BF", alpha = 0.35, size = 1.1) +
      labs(x = ex$xlab, y = ex$ylab, title = input$ex_kind %||% "Example") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold"))
    
    if (isTRUE(input$show_lm_ex) && nrow(df) >= 3) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "#111111", linewidth = 1.1)
    }
    if (isTRUE(input$show_loess_ex) && nrow(df) >= 25) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "#7C3AED", linewidth = 1.2)
    }
    
    # Subtitle-like annotation (keeps the layout clean)
    summ <- cor_summary(df$x, df$y, method = method)
    rtxt <- if (method == "pearson") "Pearson r" else "Spearman ρ"
    ptxt <- if (!is.finite(summ$p)) "NA" else if (summ$p < 0.001) "< 0.001" else fmt_num(summ$p, 3)
    
    p + annotate("text",
                 x = Inf, y = -Inf,
                 label = paste0("n=", summ$n, " | ", rtxt, "=", fmt_num(summ$r, 3), " | p=", ptxt),
                 hjust = 1.02, vjust = -0.6, size = 4.0, color = "grey20")
  })
  
  output$example_kpi_ui <- renderUI({
    ex <- example_obj()
    df <- ex$df
    
    method <- input$cor_method %||% "pearson"
    summ_m <- cor_summary(df$x, df$y, method = method)
    summ_p <- cor_summary(df$x, df$y, method = "pearson")
    summ_s <- cor_summary(df$x, df$y, method = "spearman")
    
    ptxt <- if (!is.finite(summ_m$p)) "NA" else if (summ_m$p < 0.001) "< 0.001" else fmt_num(summ_m$p, 3)
    
    tagList(
      div(class="small", style="margin-bottom:8px;", ex$desc),
      div(class="kpi",
          div(class="pill", paste0("n = ", summ_m$n)),
          div(class="pill", paste0(if (method == "pearson") "Pearson r = " else "Spearman ρ = ", fmt_num(summ_m$r, 3))),
          div(class="pill", paste0("p = ", ptxt))
      ),
      div(class="small", style="margin-top:10px;",
          paste0("Also: Pearson r = ", fmt_num(summ_p$r, 3),
                 " | Spearman ρ = ", fmt_num(summ_s$r, 3))),
      div(class="small", style="margin-top:8px;",
          HTML("<b>Interpretation cue:</b> if loess shows shape but lm is flat, r may be near 0 even though a nonlinear relationship exists."))
    )
  })
}

shinyApp(ui, server)