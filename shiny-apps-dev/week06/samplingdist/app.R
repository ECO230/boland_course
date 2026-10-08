# MUST be at the very top of app.R, before any library(...)
if (dir.exists("/data/junior/boland_course") && requireNamespace("renv", quietly = TRUE)) {
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
  iris_sepal      = list(binwidth = 0.1, xlim = NULL),
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

# Charts stay mounted in the browser; R sends only the sample statistics.
chart <- function(id, height, label) {
  tags$canvas(id = id, class = "sample-chart", role = "img", `aria-label` = label,
              style = paste0("height:", height, "px; width:100%;"))
}
panel <- function(...) div(class = "sample-panel", ...)
ui <- navbarPage(
  title = "Sampling Distribution Simulator",
  header = tags$head(tags$script(src = "sampling.js"), tags$style(HTML("
    .sample-panel {padding:14px;border-radius:14px;background:#fff;border:1px solid #ddd;margin-bottom:14px;min-width:0;}
    .sample-chart {display:block;max-width:100%;}
    .population-values {display:grid;grid-template-columns:repeat(auto-fit,minmax(70px,1fr));gap:6px;max-height:calc(100vh - 260px);min-height:200px;overflow:auto;padding:8px;}
    .population-value {padding:6px;background:#f3f6f9;border-radius:5px;text-align:center;}
    .population-value small {display:block;color:#666;}
    .formula-columns {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;background:#f7f7f7;padding:10px;margin:10px 0;overflow-wrap:anywhere;}
    .formula-columns p {margin:8px 0;font-size:12px;}
    .sample-values {font-size:12px;line-height:1.4;overflow-wrap:anywhere;margin-top:8px;}
    @media(max-width:1100px) {.formula-columns {grid-template-columns:1fr;}}
  "))),
  tabPanel("Simulator", fluidPage(fluidRow(
    column(3, panel(
      selectInput("pop_id", "Population (known full dataset)", choices = pop_choices, selected = "chickweight"),
      selectInput("n", "Sample size (n)", choices = c(5,10,30,50), selected = 30),
      sliderInput("conf", "Confidence level", min = .80, max = .99, value = .95, step = .01),
      actionButton("add1", "Add 1 sample"), actionButton("run100", "Run 100 samples"), actionButton("clear", "Clear"),
      hr(), div(id = "status_box", `aria-live` = "polite"), hr(), strong("Current sample"),
      div(id = "current_sample_text", class = "sample-values")
    )),
    column(9, fluidRow(
      column(6, panel(h4("Sampling distribution"), chart("hist_plot",300,"Sampling distribution of sample means"),
        hr(), h4("Current sample -> point estimate -> sampling distribution"),
        chart("current_mean_dot_plot",150,"Current sample mean"), div(id="current_stats_tbl"),
        div(id="formula_panel"), chart("current_ci_whisker_plot",170,"Current confidence interval"))),
      column(6, panel(h4("Confidence intervals"), chart("ci_plot",880,"Repeated confidence intervals"),
        div(id="ci_error_text")))
    ))
  ))),
  tabPanel("Population data", fluidPage(fluidRow(
    column(3, panel(selectInput("pop_id2","Population",choices=pop_choices,selected="chickweight"),
      h4("Population summary"), tableOutput("pop_summary_tbl"))),
    column(9, panel(h4("Full population values"), uiOutput("pop_data_grid")))
  )))
)

server <- function(input, output, session) {
  empty_samples <- data.frame(sample_id=integer(),mean=numeric(),sd=numeric(),se=numeric(),
    low=numeric(),high=numeric(),contains_mu=logical())
  rv <- reactiveValues(samples=empty_samples,current_sample=numeric(),current_ci=NULL,
    sample_id=0L,running=FALSE,generation=0L)
  pop_vals <- reactive(get_pop(input$pop_id)$value)
  clear_all <- function() {
    rv$generation <- rv$generation + 1L
    rv$running <- FALSE
    rv$samples <- empty_samples
    rv$current_sample <- numeric()
    rv$current_ci <- NULL
    rv$sample_id <- 0L
  }
  observeEvent(list(input$pop_id,input$n,input$conf), {
    clear_all()
    updateSelectInput(session,"pop_id2",selected=input$pop_id)
  }, priority=100)
  observeEvent(input$pop_id2, {
    if (!identical(input$pop_id2,input$pop_id)) updateSelectInput(session,"pop_id",selected=input$pop_id2)
  }, ignoreInit=TRUE)
  take_one_sample <- function() {
    samp <- sample(pop_vals(),size=as.integer(input$n),replace=TRUE)
    ci <- ci_for_sample(samp,input$conf)
    rv$current_sample <- samp
    rv$current_ci <- ci
    rv$sample_id <- rv$sample_id + 1L
    mu <- mean(pop_vals())
    rv$samples <- rbind(rv$samples,data.frame(sample_id=rv$sample_id,mean=ci$mean,sd=ci$sd,
      se=ci$se,low=ci$low,high=ci$high,contains_mu=ci$low<=mu && mu<=ci$high))
    if (rv$sample_id >= 100L) rv$running <- FALSE
  }
  observeEvent(input$add1, { if (!rv$running) take_one_sample() })
  observeEvent(input$clear, clear_all())
  observeEvent(input$run100, { clear_all(); rv$running <- TRUE; take_one_sample() })
  # The browser acknowledges each frame before another sample is scheduled.
  # This avoids an R loop blocking Shiny or outrunning a slower client.
  last_scheduled <- NULL
  observeEvent(input$sample_drawn, {
    ack <- input$sample_drawn
    key <- paste(ack$generation,ack$index)
    if (!rv$running || ack$generation != rv$generation || ack$index != rv$sample_id || identical(key,last_scheduled)) return()
    last_scheduled <<- key
    generation <- rv$generation
    index <- rv$sample_id
    later::later(function() {
      if (session$isClosed()) return()
      isolate({
        if (rv$running && rv$generation == generation && rv$sample_id == index) take_one_sample()
      })
    }, delay=.08)
  })
  observe({
    req(input$pop_id,input$n,input$conf)
    x <- pop_vals();mu <- mean(x);sigma <- pop_sd(x);n <- as.integer(input$n)
    cfg <- get_cfg(input$pop_id,mu,sigma,n)
    session$sendCustomMessage("sampling-frame",list(generation=rv$generation,index=rv$sample_id,
      running=rv$running,n=n,conf=input$conf,mu=mu,sigma=sigma,binwidth=cfg$binwidth,xlim=cfg$xlim,
      samples=rv$samples,current=rv$current_ci,values=rv$current_sample))
  })
  output$pop_summary_tbl <- renderTable({
    summary <- make_summary(get_pop(input$pop_id2)$value)
    data.frame(Metric=names(summary),Value=round(as.numeric(summary[1,]),3))
  },striped=TRUE,spacing="s",rownames=FALSE)
  output$pop_data_grid <- renderUI({
    x <- get_pop(input$pop_id2)$value
    div(class="population-values",lapply(seq_along(x),function(i) {
      div(class="population-value",tags$small(paste0("#",i)),format(round(x[i],3),trim=TRUE))
    }))
  })
  session$onSessionEnded(function() { rv$running <- FALSE })
}
shinyApp(ui,server)
