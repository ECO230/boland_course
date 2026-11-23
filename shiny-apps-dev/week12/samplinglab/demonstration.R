# Instructor-led sampling sequence. All rows stay on the server.
demo_steps <- c("1. Simple random" = "srs", "2. Stratified" = "stratified",
                "3. Systematic" = "systematic", "4. Convenience" = "convenience", "5. Quota" = "quota")

quota_from_frame <- function(pool, frame, n, group) {
  levels <- sort(unique(comparison_levels(pool[[group]])))
  counts <- as.integer(table(factor(comparison_levels(pool[[group]]), levels = levels)))
  exact <- counts / nrow(pool) * min(n, nrow(pool))
  target <- floor(exact)
  left <- min(n, nrow(pool)) - sum(target)
  if (left > 0) {
    priority <- order(exact - target, decreasing = TRUE)
    target[priority[seq_len(left)]] <- target[priority[seq_len(left)]] + 1L
  }
  selected <- unlist(lapply(seq_along(levels), function(i) {
    head(which(comparison_levels(frame[[group]]) == levels[i]), target[i])
  }), use.names = FALSE)
  sample <- frame[selected, , drop = FALSE]
  filled <- as.integer(table(factor(comparison_levels(sample[[group]]), levels = levels)))
  list(sample = sample, quotas = data.frame(Group = levels, Target = target, Filled = filled))
}

summer_frame <- function(pool, local_only) {
  if (!local_only || !"summer_lacrosse" %in% names(pool)) return(pool)
  keep <- tolower(trimws(as.character(pool$summer_lacrosse))) %in% c("yes", "y")
  pool[keep, , drop = FALSE]
}

patterned_pool <- function(pool) {
  # A deliberately periodic list for the labeled demo only. No responses change.
  groups <- split(seq_len(nrow(pool)), comparison_levels(pool$summer_lacrosse))
  longest <- max(lengths(groups))
  ids <- unlist(lapply(seq_len(longest), function(i) {
    vapply(groups, function(g) if (i <= length(g)) g[i] else NA_integer_, integer(1))
  }), use.names = FALSE)
  pool[ids[!is.na(ids)], , drop = FALSE]
}

class_demo_ui <- function() {
  tabPanel("Class demonstration",
    br(),
    fluidRow(
      column(3,
        radioButtons("demo_step", "Sampling method", demo_steps),
        actionButton("demo_previous", "Previous"), actionButton("demo_next", "Next"), hr(),
        sliderInput("demo_n", "Sample size", min = 1, max = 120, value = 20, step = 1),
        selectInput("demo_outcome", "What are we measuring?", choices = NULL),
        uiOutput("demo_level_ui"),
        selectInput("demo_compare", "Check the sample by", choices = NULL),
        uiOutput("demo_design_ui"),
        actionButton("demo_draw", "Draw another sample", class = "btn-primary"),
        tags$details(tags$summary("Data source"),
          radioButtons("data_mode", NULL, choices = c("Demo data", "Live survey DB", "Upload CSV"), selected = default_data_mode),
          conditionalPanel("input.data_mode == 'Live survey DB'", uiOutput("live_db_status"), actionButton("refresh_live", "Refresh survey responses")),
          conditionalPanel("input.data_mode == 'Upload CSV'", fileInput("csv_file", "CSV file", accept = ".csv"))
        )
      ),
      column(9,
        uiOutput("demo_source"), h3(textOutput("demo_heading", inline = TRUE)),
        textOutput("demo_rule"),
        tableOutput("demo_estimate"),
        div(class = "callout", textOutput("demo_question")),
        actionButton("demo_reveal", "Check who we got"),
        conditionalPanel("output.demo_show_groups", plotOutput("demo_composition", height = 300),
          textOutput("demo_balance"), tableOutput("demo_quota_table")),
        conditionalPanel("input.demo_step == 'srs' || input.demo_step == 'stratified' || input.demo_step == 'systematic'",
          hr(), actionButton("demo_batch", "Draw 200 samples"),
          actionButton("demo_inspect", "Inspect the most uneven draw in this batch"),
          textOutput("demo_batch_note"), conditionalPanel("output.demo_has_batch && !output.demo_show_groups", plotOutput("demo_batch_plot", height = 220))),
        hr(), p(class = "small", "The response pool is our known benchmark for this demonstration. In a real survey, use credible population information to check who is represented. Matching a few demographics is useful evidence, not proof that every outcome is accurate.")
      )
    )
  )
}

bind_class_demo <- function(input, output, session, base_data) {
  state <- reactiveValues(sample = NULL, batch = NULL, reveal = FALSE, quotas = NULL, mode = NULL, selection = NULL)
  groups <- reactive({
    df <- base_data()
    vars <- setdiff(categorical_vars(df), private_response_fields)
    # Keep demographic checks short; do not include arbitrary free text.
    intersect(unique(c(teaching_compare_vars, "acad_area", "transport", "uses_ai")), vars)
  })
  observeEvent(base_data(), {
    df <- base_data(); req(nrow(df) > 0)
    outcomes <- intersect(teaching_outcome_vars, names(df))
    req(length(outcomes), length(groups()))
    current <- isolate(input$demo_outcome)
    chosen <- if (length(current) && current %in% outcomes) current else if ("sleep_hrs" %in% outcomes) "sleep_hrs" else outcomes[1]
    updateSelectInput(session, "demo_outcome", choices = setNames(outcomes, pretty_var_name(outcomes)), selected = chosen)
    current <- isolate(input$demo_compare)
    chosen <- if (length(current) && current %in% groups()) current else if ("class_stand" %in% groups()) "class_stand" else groups()[1]
    updateSelectInput(session, "demo_compare", choices = setNames(groups(), pretty_var_name(groups())), selected = chosen)
    n <- min(isolate(input$demo_n) %||% 20, nrow(df))
    if (is.null(state$mode) || !identical(state$mode, input$data_mode)) n <- min(20, max(1, floor(nrow(df)/2)))
    state$mode <- input$data_mode
    updateSliderInput(session, "demo_n", max = nrow(df), value = n)
  })
  output$demo_level_ui <- renderUI({
    req(input$demo_outcome); x <- base_data()[[input$demo_outcome]]
    if (is.numeric(x)) return(NULL)
    choices <- sort(unique(na.omit(as.character(x))))
    selectInput("demo_level", "Which response?", choices = choices)
  })
  output$demo_design_ui <- renderUI({
    req(input$demo_step)
    if (input$demo_step %in% c("stratified", "quota")) {
      vars <- groups(); req(length(vars))
      group_control <- selectInput("demo_strata", "Set groups before selecting", setNames(vars, pretty_var_name(vars)), selected = if ("class_stand" %in% vars) "class_stand" else vars[1])
    } else group_control <- NULL
    tagList(group_control,
      if (input$demo_step == "systematic") tagList(
        p(class = "small", "Order: response list. Select a random start, then evenly spaced rows."),
        if (identical(input$data_mode, "Demo data")) checkboxInput("demo_pattern", "Use an alternating summer-residence list (simulated)", FALSE)),
      if (input$demo_step %in% c("convenience", "quota")) {
        if ("summer_lacrosse" %in% names(base_data()))
          checkboxInput("demo_local_only", "Recruit only people in La Crosse over summer", TRUE)
        else p(class = "small", "No summer-residence field in this pool: use the first responses as the convenient list.")
      }
    )
  })
  settings <- reactive({
    req(input$demo_step, input$demo_n)
    df <- base_data(); req(nrow(df) > 0)
    step <- input$demo_step
    group <- if (step %in% c("stratified", "quota")) { req(input$demo_strata); input$demo_strata } else NULL
    local <- step %in% c("convenience", "quota") && isTRUE(input$demo_local_only)
    pattern <- step == "systematic" && identical(input$data_mode, "Demo data") && isTRUE(input$demo_pattern)
    list(pool = df, step = step, n = min(as.integer(input$demo_n), nrow(df)), group = group, local = local, pattern = pattern)
  })
  select_sample <- function(cfg) {
    df <- cfg$pool
    if (cfg$step == "srs") return(list(sample = sample_srs(df, cfg$n), quotas = NULL))
    if (cfg$step == "stratified") return(list(sample = sample_by_group(df, cfg$n, cfg$group, TRUE), quotas = NULL))
    if (cfg$step == "systematic") {
      ordered <- if (cfg$pattern) patterned_pool(df) else df[order(df$response_order), , drop = FALSE]
      # Preserve the demonstrated order rather than sorting it a second time.
      k <- nrow(ordered)/cfg$n
      ids <- floor(runif(1, 0, k) + (seq_len(cfg$n)-1)*k) + 1L
      return(list(sample = ordered[ids, , drop = FALSE], quotas = NULL))
    }
    frame <- summer_frame(df, cfg$local)
    if (cfg$step == "quota") return(quota_from_frame(df, frame, cfg$n, cfg$group))
    list(sample = head(frame, cfg$n), quotas = NULL)
  }
  draw <- function() {
    cfg <- settings(); result <- select_sample(cfg)
    state$sample <- result$sample; state$quotas <- result$quotas
    state$selection <- cfg; state$batch <- NULL; state$reveal <- FALSE
  }
  observeEvent(settings(), draw(), ignoreNULL = FALSE)
  observeEvent(input$demo_draw, draw())
  observeEvent(input$demo_reveal, { state$reveal <- !state$reveal })
  observe({ updateActionButton(session, "demo_reveal", label = if (state$reveal) "Back to sample results" else "Check who we got") })
  observeEvent(input$demo_next, {
    i <- match(input$demo_step, unname(demo_steps)); updateRadioButtons(session, "demo_step", selected = unname(demo_steps)[min(5, i+1)])
  })
  observeEvent(input$demo_previous, {
    i <- match(input$demo_step, unname(demo_steps)); updateRadioButtons(session, "demo_step", selected = unname(demo_steps)[max(1, i-1)])
  })
  observeEvent(input$demo_step, {
    vars <- groups()
    if (input$demo_step == "convenience" && "summer_lacrosse" %in% vars)
      updateSelectInput(session, "demo_compare", selected="summer_lacrosse")
    if (input$demo_step %in% c("stratified", "quota") && "class_stand" %in% vars)
      updateSelectInput(session, "demo_compare", selected="class_stand")
  })
  observeEvent(input$demo_pattern, {
    if (isTRUE(input$demo_pattern) && identical(input$data_mode, "Demo data"))
      updateSelectInput(session, "demo_compare", selected="summer_lacrosse")
  })
  output$demo_source <- renderUI({
    p(strong(if (input$data_mode == "Live survey DB") "Completed class survey responses" else if (input$data_mode == "Demo data") "Simulated response pool" else "Uploaded response pool"), paste("|", nrow(base_data()), "responses"))
  })
  output$demo_heading <- renderText({ req(input$demo_step); names(demo_steps)[match(input$demo_step, demo_steps)] })
  output$demo_rule <- renderText({
    cfg <- settings()
    paste(if (cfg$step %in% c("srs", "stratified", "systematic")) "Probability sampling." else "Nonprobability sampling.", switch(cfg$step,
      srs = "Every response has the same chance of selection. No group totals are set in advance.",
      stratified = paste("Set proportional groups using", pretty_var_name(cfg$group), "before randomly selecting within each group. Small samples and whole-person rounding can still leave very small groups out."),
      systematic = paste0("Choose a random start, then select at intervals of ", fmt_num(nrow(cfg$pool)/cfg$n), " rows.", if (cfg$pattern) " This simulated list alternates summer-residence groups; a matching interval can keep selecting the same group." else " An ordering pattern can matter even with a random start."),
      convenience = if (cfg$local) "Recruit from people who report being in La Crosse over summer. Take the first available responses; people away for summer cannot enter this sample." else "Take the first available responses. There is no random selection.",
      quota = paste("Set quotas from the full pool's", pretty_var_name(cfg$group), "proportions, then take the first available people in each group.", if (cfg$local) "Recruitment is still limited to people in La Crosse over summer." else "Selection within a group is not random.")))
  })
  output$demo_question <- renderText({ req(input$demo_step)
    switch(input$demo_step,
      srs = "Would you trust this result? Draw again, or look at 200 draws. Can chance alone give us an uneven cross-section?",
      stratified = "We controlled one group total. What happens when we check the same sample on a different characteristic?",
      systematic = "Does a random start solve a pattern in the list? Compare the normal and patterned lists using simulated responses.",
      convenience = "Who had a chance to be included? Is a locally available group a good cross-section of the full pool?",
      quota = "Did filling group quotas solve the availability problem? Which people could still never be included?")
  })
  output$demo_estimate <- renderTable({
    req(state$sample, input$demo_outcome)
    pool <- base_data(); sample <- state$sample
    values <- vapply(list(pool, sample), estimate_value, numeric(1), var = input$demo_outcome, level = input$demo_level %||% NULL)
    format <- if (is.numeric(pool[[input$demo_outcome]])) fmt_num else fmt_pct
    data.frame(Group = c("Full response pool", "This sample"), Rows = c(nrow(pool), nrow(sample)),
      Answered = vapply(list(pool,sample), function(d) sum(!is.na(d[[input$demo_outcome]])), integer(1)),
      Estimate = vapply(values, format, character(1)))
  }, rownames = FALSE, striped = TRUE)
  composition <- reactive({
    req(state$sample, input$demo_compare)
    validate(need(nrow(state$sample) > 0, "No one in this pool meets the recruitment rule. No estimate can be made."))
    composition_comparison(base_data(), state$sample, state$sample, input$demo_compare)
  })
  output$demo_has_batch <- reactive(!is.null(state$batch)); outputOptions(output, "demo_has_batch", suspendWhenHidden = FALSE)
  output$demo_show_groups <- reactive(isTRUE(state$reveal)); outputOptions(output, "demo_show_groups", suspendWhenHidden = FALSE)
  output$demo_composition <- renderPlot({
    comp <- composition()
    df <- rbind(data.frame(group=comp$level, source="Full pool", pct=comp$pool_pct), data.frame(group=comp$level, source="This sample", pct=comp$random_pct))
    ggplot(df, aes(x=pct, y=group, fill=source)) + geom_col(position="dodge", width=.7) +
      scale_fill_manual(values=c("Full pool"="#555555", "This sample"="#286ca5"), name=NULL) +
      scale_x_continuous(limits=c(0,1), labels=function(x) paste0(round(100*x), "%")) +
      labs(x="Percent of all rows, including missing responses", y=pretty_var_name(input$demo_compare)) +
      theme_minimal(base_size=15) + theme(legend.position="top")
  })
  output$demo_balance <- renderText({
    comp <- composition(); cfg <- settings(); frame <- summer_frame(cfg$pool, cfg$local)
    paste0("Largest group difference on ", pretty_var_name(input$demo_compare), ": ", fmt_num(100*max(abs(comp$random_pct-comp$pool_pct)),1),
      " percentage points.", if (cfg$step %in% c("convenience", "quota")) paste0(" Available for recruitment: ", nrow(frame), " of ", nrow(cfg$pool), "; selected: ", nrow(state$sample), " of the requested ", cfg$n, ".") else " Check another characteristic without changing the selected rows.")
  })
  output$demo_quota_table <- renderTable({ req(input$demo_step == "quota", state$quotas); state$quotas }, rownames=FALSE)
  observeEvent(input$demo_batch, {
    cfg <- settings(); req(cfg$step %in% c("srs", "stratified", "systematic"), input$demo_outcome)
    state$batch <- lapply(seq_len(200), function(i) select_sample(cfg)$sample)
  })
  batch_values <- reactive({ req(state$batch, input$demo_outcome)
    vapply(state$batch, estimate_value, numeric(1), var=input$demo_outcome, level=input$demo_level %||% NULL)
  })
  observeEvent(input$demo_inspect, {
    req(state$batch, input$demo_compare)
    gaps <- vapply(state$batch, function(sample) {
      comp <- composition_comparison(base_data(), sample, sample, input$demo_compare)
      max(abs(comp$random_pct-comp$pool_pct))
    }, numeric(1))
    i <- which.max(gaps); state$sample <- state$batch[[i]]; state$reveal <- TRUE
  })
  output$demo_batch_note <- renderText({
    if (is.null(state$batch)) return("Each draw is genuine. Inspecting the most uneven draw is a teaching comparison against our known pool, not a rule for rejecting real survey results.")
    paste("200 saved draws using this method. The button inspects the largest demographic mismatch on", pretty_var_name(input$demo_compare), "in this batch. Changing the characteristic rechecks these same draws.")
  })
  output$demo_batch_plot <- renderPlot({
    values <- batch_values(); validate(need(any(is.finite(values)), "No usable outcome estimates in this batch."))
    pool <- estimate_value(base_data(), input$demo_outcome, input$demo_level %||% NULL)
    current <- estimate_value(state$sample, input$demo_outcome, input$demo_level %||% NULL)
    p <- ggplot(data.frame(estimate=values), aes(x=estimate)) + geom_histogram(bins=20, fill="#286ca5", color="white", na.rm=TRUE) +
      geom_vline(xintercept=pool, linetype="dashed", color="#555555") + geom_vline(xintercept=current, color="#b86622", linewidth=1, na.rm=TRUE) +
      labs(x=paste(pretty_var_name(input$demo_outcome), "| gray: full pool; orange: inspected sample"), y="Draws") + theme_minimal(base_size=13)
    if (!is.numeric(base_data()[[input$demo_outcome]])) p <- p + scale_x_continuous(labels=function(x) paste0(round(100*x), "%"))
    p
  })
  invisible(list(state=state, settings=settings, composition=composition, select_sample=select_sample))
}
