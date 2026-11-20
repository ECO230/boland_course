# Repeated simple-random draws linked to a subgroup check of one saved sample.
many_samples_ui <- function() {
  tabPanel("Many random samples",
    br(),
    p("Keep the pool and sample size fixed, then draw many simple random samples. Look at the center and the tails, and inspect individual draws."),
    fluidRow(
      column(3, numericInput("random_run_count", "Number of samples", value = 200, min = 20, max = 1000, step = 20)),
      column(9, br(), actionButton("run_random_samples", "Draw many random samples", class = "btn-primary"))
    ),
    textOutput("random_run_summary"),
    fluidRow(
      column(6, plotOutput("random_run_histogram", height = 270)),
      column(6, plotOutput("random_run_sequence", height = 270, click = "random_run_click"))
    ),
    p(class = "small", "Gray dashed line: full-pool estimate. Orange line: fixed nonrandom sample. Red: the draw being inspected. Click a dot to inspect that draw."),
    uiOutput("inspect_draw_ui"),
    actionButton("inspect_random_draw", "Inspect a random draw"),
    actionButton("inspect_farthest_draw", "Inspect the farthest draw from the pool"),
    h4("Check this draw"),
    tableOutput("inspected_draw_estimates"),
    plotOutput("inspected_draw_composition", height = 320),
    textOutput("inspected_draw_note"),
    div(class = "callout", strong("The demographic sniff test"),
      p("Here we know the full response pool, so we can see which estimates are far off. In a real survey we usually do not know the population outcome. We cannot tell how far off a result is from that result alone."),
      p("Compare demographics with credible population benchmarks when available. A mismatch is a reason to investigate. A match on a few characteristics does not prove the outcome is accurate or rule out nonresponse and measurement problems.")),
    p(class = "small", "The farthest-draw button uses information we have in this demonstration; it is not a rule for rejecting samples in a real study. These are repeated samples without replacement within each draw; rows can appear again in later draws.")
  )
}

bind_many_samples <- function(input, output, session, base_data, rv) {
  # Changing the design/outcome invalidates the run. Inspecting another group
  # or redrawing the single-sample comparison does not change saved rows.
  observeEvent(list(base_data(), input$sample_n, input$outcome_var, input$outcome_level,
                    input$nonrandom_method, input$convenience_order, input$quota_var), {
    rv$random_run <- NULL
    rv$run_selected <- 1L
  }, ignoreNULL = FALSE)

  observeEvent(input$run_random_samples, {
    req(input$outcome_var, rv$paired_nonrandom)
    df <- base_data()
    req(nrow(df) > 0)
    if (!is.numeric(df[[input$outcome_var]])) req(input$outcome_level)
    count <- input$random_run_count %||% 200
    req(is.finite(count), count >= 20, count <= 1000)
    n <- min(as.integer(input$sample_n), nrow(df))
    draws <- lapply(seq_len(as.integer(count)), function(i) sample.int(nrow(df), n))
    values <- vapply(draws, function(ids) estimate_value(df[ids, , drop = FALSE], input$outcome_var, input$outcome_level %||% NULL), numeric(1))
    rv$random_run <- list(draws = draws, estimates = values, pool = estimate_value(df, input$outcome_var, input$outcome_level %||% NULL),
      nonrandom = rv$paired_nonrandom, n = n, numeric = is.numeric(df[[input$outcome_var]]))
    rv$run_selected <- 1L
  })

  selected_draw <- reactive({
    req(rv$random_run)
    max(1L, min(as.integer(rv$run_selected %||% 1L), length(rv$random_run$draws)))
  })
  inspect <- function(i) {
    req(rv$random_run)
    rv$run_selected <- max(1L, min(as.integer(i), length(rv$random_run$draws)))
    updateSliderInput(session, "inspect_draw", value = rv$run_selected)
  }
  observeEvent(input$inspect_draw, inspect(input$inspect_draw))
  observeEvent(input$random_run_click, inspect(round(input$random_run_click$x)))
  observeEvent(input$inspect_random_draw, { req(rv$random_run); inspect(sample.int(length(rv$random_run$draws), 1)) })
  observeEvent(input$inspect_farthest_draw, {
    req(rv$random_run)
    gaps <- abs(rv$random_run$estimates - rv$random_run$pool)
    req(any(is.finite(gaps)))
    gaps[!is.finite(gaps)] <- -Inf
    inspect(which.max(gaps))
  })

  output$inspect_draw_ui <- renderUI({
    req(rv$random_run)
    sliderInput("inspect_draw", "Inspect draw number", min = 1, max = length(rv$random_run$draws), value = 1, step = 1)
  })
  run_data <- reactive({
    req(rv$random_run)
    r <- rv$random_run
    validate(need(any(is.finite(r$estimates)), "No usable outcome answers in these draws. Choose another outcome or a larger sample."))
    data.frame(draw = seq_along(r$estimates), estimate = r$estimates)
  })
  axis_label <- reactive({
    req(rv$random_run)
    if (rv$random_run$numeric) paste("Sample mean:", pretty_var_name(input$outcome_var)) else paste("Sample percent:", input$outcome_level)
  })
  nonrandom_estimate <- reactive({
    req(rv$random_run)
    estimate_value(rv$random_run$nonrandom, input$outcome_var, input$outcome_level %||% NULL)
  })
  output$random_run_summary <- renderText({
    if (is.null(rv$random_run)) return("Draw a batch to see how the results vary. The batch clears when you change the pool, sample size, outcome, or selection rule.")
    r <- rv$random_run
    good <- r$estimates[is.finite(r$estimates)]
    if (!length(good)) return("No usable estimates in this batch.")
    format <- if (r$numeric) fmt_num else fmt_pct
    middle <- quantile(good, c(.05, .95), names = FALSE)
    paste0(length(r$draws), " samples of ", r$n, " rows. Full pool: ", format(r$pool),
      ". Middle 90% of usable estimates in this batch: ", format(middle[1]), " to ", format(middle[2]),
      "; full range: ", format(min(good)), " to ", format(max(good)), ". ",
      sum(!is.finite(r$estimates)), " draws had no usable outcome answers. This describes this run, not a confidence interval or a guarantee about the next sample.")
  })
  output$random_run_histogram <- renderPlot({
    df <- run_data(); r <- rv$random_run
    p <- ggplot(df, aes(x = estimate)) +
      geom_histogram(bins = 20, fill = "#286ca5", color = "white", na.rm = TRUE) +
      geom_vline(xintercept = r$pool, color = "#555555", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = nonrandom_estimate(), color = "#b86622", linewidth = 1, na.rm = TRUE) +
      geom_vline(xintercept = r$estimates[selected_draw()], color = "#b42b35", linewidth = 1, na.rm = TRUE) +
      labs(x = axis_label(), y = "Number of draws") + theme_minimal(base_size = 13)
    if (!r$numeric) p <- p + scale_x_continuous(labels = function(x) paste0(round(x*100), "%"))
    p
  })
  output$random_run_sequence <- renderPlot({
    df <- run_data(); r <- rv$random_run
    p <- ggplot(df, aes(x = draw, y = estimate)) +
      geom_hline(yintercept = r$pool, color = "#555555", linetype = "dashed", linewidth = 1) +
      geom_hline(yintercept = nonrandom_estimate(), color = "#b86622", linewidth = 1, na.rm = TRUE) +
      geom_point(color = "#286ca5", alpha = .45, na.rm = TRUE) +
      geom_point(data = df[df$draw == selected_draw(), , drop = FALSE], color = "#b42b35", size = 3, na.rm = TRUE) +
      labs(x = "Draw number (click a dot)", y = axis_label()) + theme_minimal(base_size = 13)
    if (!r$numeric) p <- p + scale_y_continuous(labels = function(x) paste0(round(x*100), "%"))
    p
  })
  inspected_rows <- reactive({
    req(rv$random_run)
    base_data()[rv$random_run$draws[[selected_draw()]], , drop = FALSE]
  })
  output$inspected_draw_estimates <- renderTable({
    r <- rv$random_run; req(r)
    samp <- inspected_rows()
    values <- c(r$pool, r$estimates[selected_draw()], nonrandom_estimate())
    format <- if (r$numeric) fmt_num else fmt_pct
    data.frame(Group = c("Full response pool", paste("Random draw", selected_draw()), "Fixed nonrandom sample"),
      Rows = c(nrow(base_data()), nrow(samp), nrow(r$nonrandom)),
      Answered = vapply(list(base_data(), samp, r$nonrandom), function(d) {
        x <- d[[input$outcome_var]]
        sum(if (is.numeric(x)) is.finite(x) else !is.na(x))
      }, integer(1)),
      Estimate = vapply(values, format, character(1)), check.names = FALSE)
  }, striped = TRUE, rownames = FALSE)
  inspected_composition <- reactive({
    req(rv$random_run, input$compare_var)
    composition_comparison(base_data(), inspected_rows(), rv$random_run$nonrandom, input$compare_var)
  })
  output$inspected_draw_composition <- renderPlot({
    comp <- inspected_composition()
    df <- rbind(data.frame(level = comp$level, source = "Full response pool", pct = comp$pool_pct),
      data.frame(level = comp$level, source = "Inspected random draw", pct = comp$random_pct))
    ggplot(df, aes(x = pct, y = level, fill = source)) + geom_col(position = "dodge", width = .7) +
      scale_fill_manual(values = c("Full response pool" = "#555555", "Inspected random draw" = "#286ca5"), name = NULL) +
      scale_x_continuous(limits = c(0,1), labels = function(x) paste0(round(x*100), "%")) +
      labs(x = "Percent of all rows (including no response)", y = pretty_var_name(input$compare_var)) +
      theme_minimal(base_size = 14) + theme(legend.position = "top")
  })
  output$inspected_draw_note <- renderText({
    comp <- inspected_composition()
    paste0("For draw ", selected_draw(), ", the largest group difference on ", pretty_var_name(input$compare_var),
      " is ", fmt_num(100*max(abs(comp$random_pct-comp$pool_pct)), 1),
      " percentage points. Try another demographic without changing this draw. Does the outcome difference come with an obvious demographic mismatch?")
  })
  invisible(list(selected = selected_draw, rows = inspected_rows, composition = inspected_composition))
}
