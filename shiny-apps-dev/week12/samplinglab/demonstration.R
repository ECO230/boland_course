# Probability examples always show the sample, uneven draw, then distribution.
demo_steps <- c("1. Simple random"="srs","2. Stratified"="stratified",
  "3. Systematic"="systematic","4. Convenience"="convenience","5. Quota"="quota")

patterned_pool <- function(pool) {
  groups <- split(seq_len(nrow(pool)),comparison_levels(pool$summer_lacrosse))
  ids <- unlist(lapply(seq_len(max(lengths(groups))),function(i) {
    vapply(groups,function(g) if (i<=length(g)) g[i] else NA_integer_,integer(1))
  }),use.names=FALSE)
  pool[ids[!is.na(ids)], , drop=FALSE]
}

class_demo_ui <- function() {
  tabPanel("Class demonstration",br(),fluidRow(
    column(3,
      radioButtons("demo_step","Sampling method",demo_steps),
      actionButton("demo_previous","Previous"),actionButton("demo_next","Next"),hr(),
      sliderInput("demo_n","Sample size",min=1,max=120,value=20,step=1),
      uiOutput("demo_design_ui"),
      selectInput("demo_outcome","What are we measuring?",choices=NULL),uiOutput("demo_level_ui"),
      selectInput("demo_compare","Check representation by",choices=NULL),
      conditionalPanel("input.demo_step != 'convenience' && input.demo_step != 'quota'",
        actionButton("demo_draw","Draw another sample",class="btn-primary")),
      conditionalPanel("input.data_mode == 'Live survey DB'",
        div(style="margin:8px 0;",actionButton("refresh_live","Refresh survey responses"))),
      tags$details(tags$summary("Data source"),
        radioButtons("data_mode",NULL,c("Demo data","Live survey DB","Upload CSV"),selected=default_data_mode),
        conditionalPanel("input.data_mode == 'Live survey DB'",uiOutput("live_db_status")),
        conditionalPanel("input.data_mode == 'Upload CSV'",fileInput("csv_file","CSV file",accept=".csv")))),
    column(9,
      uiOutput("demo_source"),h3(textOutput("demo_heading",inline=TRUE)),textOutput("demo_rule"),
      conditionalPanel("input.demo_step != 'convenience' && input.demo_step != 'quota'",
        tableOutput("demo_estimate"),
        h4("Our sample"),plotOutput("demo_composition",height=280),textOutput("demo_balance"),
        h4("Most uneven of 200 draws"),textOutput("demo_inspected_note"),
        plotOutput("demo_inspected_composition",height=280),textOutput("demo_inspected_balance"),
        h4("Results from 200 samples"),plotOutput("demo_batch_plot",height=250),
        actionButton("demo_batch","Draw another set of 200")),
      conditionalPanel("input.demo_step == 'convenience' || input.demo_step == 'quota'",
        p("Compare the recruitment methods below. Change any row's selection rule to see who gets included."),
        tableOutput("recruitment_estimates"),uiOutput("recruitment_examples")),
      hr(),p(class="small","The response pool is our known benchmark for this demonstration. In a real survey, check representation against credible population information. Matching a few demographics does not establish that every outcome is accurate."))))
}

bind_class_demo <- function(input,output,session,base_data) {
  state <- reactiveValues(sample=NULL,batch=NULL,mode=NULL)
  groups <- reactive({
    vars <- setdiff(categorical_vars(base_data()),private_response_fields)
    intersect(unique(c(teaching_compare_vars,"acad_area","transport","uses_ai")),vars)
  })
  observeEvent(base_data(),{
    df <- base_data(); req(nrow(df)>0)
    outcomes <- intersect(teaching_outcome_vars,names(df)); req(length(outcomes),length(groups()))
    current <- isolate(input$demo_outcome)
    chosen <- if (length(current) && current %in% outcomes) current else if ("sleep_hrs" %in% outcomes) "sleep_hrs" else outcomes[1]
    updateSelectInput(session,"demo_outcome",choices=setNames(outcomes,pretty_var_name(outcomes)),selected=chosen)
    current <- isolate(input$demo_compare)
    chosen <- if (length(current) && current %in% groups()) current else if ("class_stand" %in% groups()) "class_stand" else groups()[1]
    updateSelectInput(session,"demo_compare",choices=setNames(groups(),pretty_var_name(groups())),selected=chosen)
    n <- min(isolate(input$demo_n) %||% 20,nrow(df))
    if (is.null(state$mode) || !identical(state$mode,input$data_mode)) n <- min(20,max(1,floor(nrow(df)/2)))
    state$mode <- input$data_mode; updateSliderInput(session,"demo_n",max=nrow(df),value=n)
  })
  output$demo_level_ui <- renderUI({
    req(input$demo_outcome); x <- base_data()[[input$demo_outcome]]
    if (is.numeric(x)) return(NULL)
    selectInput("demo_level","Which response?",sort(unique(na.omit(as.character(x)))))
  })
  output$demo_design_ui <- renderUI({
    req(input$demo_step)
    tagList(
      if (input$demo_step %in% c("stratified","quota")) {
        vars <- groups(); req(length(vars))
        current <- isolate(input$demo_strata)
        chosen <- if (length(current) && current %in% vars) current else if ("class_stand" %in% vars) "class_stand" else vars[1]
        selectInput("demo_strata","Set groups before selecting",setNames(vars,pretty_var_name(vars)),selected=chosen)
      },
      if (input$demo_step=="systematic") tagList(
        p(class="small","Order: response list. Select a random start, then evenly spaced rows."),
        if (identical(input$data_mode,"Demo data")) checkboxInput("demo_pattern","Use an alternating summer-residence list (simulated)",FALSE)))
  })
  settings <- reactive({
    req(input$demo_step,input$demo_n); df <- base_data(); req(nrow(df)>0)
    group <- if (input$demo_step %in% c("stratified","quota")) {
      req(input$demo_strata,input$demo_strata %in% groups()); input$demo_strata
    } else NULL
    list(pool=df,step=input$demo_step,n=max(1L,min(as.integer(input$demo_n),nrow(df))),group=group,
      pattern=input$demo_step=="systematic" && identical(input$data_mode,"Demo data") && isTRUE(input$demo_pattern))
  })
  select_sample <- function(cfg) {
    if (cfg$step=="srs") return(sample_srs(cfg$pool,cfg$n))
    if (cfg$step=="stratified") return(sample_by_group(cfg$pool,cfg$n,cfg$group,TRUE))
    req(cfg$step=="systematic")
    ordered <- if (cfg$pattern) patterned_pool(cfg$pool) else cfg$pool[order(cfg$pool$response_order), , drop=FALSE]
    k <- nrow(ordered)/cfg$n; ids <- floor(runif(1,0,k)+(seq_len(cfg$n)-1)*k)+1L
    ordered[ids, , drop=FALSE]
  }
  draw <- function() {
    cfg <- settings()
    if (cfg$step %in% c("convenience","quota")) {state$sample <- NULL; state$batch <- NULL; return()}
    state$sample <- select_sample(cfg)
    state$batch <- lapply(seq_len(200),function(i) select_sample(cfg))
  }
  observe({settings(); draw()})
  observeEvent(input$demo_draw,draw())
  observeEvent(input$demo_batch,{
    cfg <- settings(); req(cfg$step %in% c("srs","stratified","systematic"))
    state$batch <- lapply(seq_len(200),function(i) select_sample(cfg))
  })
  observeEvent(input$demo_next,{
    i <- match(input$demo_step,unname(demo_steps)); updateRadioButtons(session,"demo_step",selected=unname(demo_steps)[min(5,i+1)])
  })
  observeEvent(input$demo_previous,{
    i <- match(input$demo_step,unname(demo_steps)); updateRadioButtons(session,"demo_step",selected=unname(demo_steps)[max(1,i-1)])
  })
  observeEvent(input$demo_step,{if ("class_stand" %in% groups()) updateSelectInput(session,"demo_compare",selected="class_stand")})
  observeEvent(input$demo_pattern,{
    if (isTRUE(input$demo_pattern) && identical(input$data_mode,"Demo data")) updateSelectInput(session,"demo_compare",selected="summer_lacrosse")
  })
  inspected <- reactive({
    req(state$batch,input$demo_compare)
    gaps <- vapply(state$batch,function(sample) {
      comp <- composition_comparison(base_data(),sample,sample,input$demo_compare)
      max(abs(comp$random_pct-comp$pool_pct))
    },numeric(1))
    i <- which.max(gaps); list(index=i,sample=state$batch[[i]],gap=gaps[i])
  })
  output$demo_source <- renderUI({req(input$data_mode)
    p(strong(if (input$data_mode=="Live survey DB") "Completed class survey responses" else if (input$data_mode=="Demo data") "Simulated response pool" else "Uploaded response pool"),paste("|",nrow(base_data()),"responses"))
  })
  output$demo_heading <- renderText({req(input$demo_step); names(demo_steps)[match(input$demo_step,demo_steps)]})
  output$demo_rule <- renderText({
    cfg <- settings()
    switch(cfg$step,
      srs="Every response has the same chance of selection. No group totals are set in advance.",
      stratified=paste("Randomly select within proportional",pretty_var_name(cfg$group),"groups. Small groups can still be affected by whole-person rounding."),
      systematic=paste0("Choose a random start, then select at intervals of ",fmt_num(nrow(cfg$pool)/cfg$n)," rows.",if (cfg$pattern) " This simulated list alternates summer-residence groups; a matching interval can repeatedly select the same group." else " Patterns in the list can affect the result."),
      convenience="These are separate, fixed recruitment rules. The full pool, requested sample size, outcome, and demographic check stay the same across rows.",
      quota=paste("Each recruitment rule fills quotas based on the full pool's",pretty_var_name(cfg$group),"proportions. Selection within groups is nonrandom; unreachable places remain unfilled."))
  })
  output$demo_estimate <- renderTable({
    req(state$sample,input$demo_outcome)
    all <- list("Full response pool"=base_data(),"Our sample"=state$sample,"Most uneven draw"=inspected()$sample)
    values <- vapply(all,estimate_value,numeric(1),var=input$demo_outcome,level=input$demo_level %||% NULL)
    format <- if (is.numeric(base_data()[[input$demo_outcome]])) fmt_num else fmt_pct
    data.frame(Group=names(all),Rows=vapply(all,nrow,integer(1)),
      Answered=vapply(all,function(d) sum(!is.na(d[[input$demo_outcome]])),integer(1)),
      Estimate=vapply(values,format,character(1)),row.names=NULL)
  },rownames=FALSE,striped=TRUE)
  composition <- reactive({req(state$sample,input$demo_compare); composition_comparison(base_data(),state$sample,state$sample,input$demo_compare)})
  output$demo_composition <- renderPlot({req(state$sample); sample_composition_plot(base_data(),state$sample,input$demo_compare,"Our sample")})
  output$demo_inspected_composition <- renderPlot({sample_composition_plot(base_data(),inspected()$sample,input$demo_compare,"Uneven draw","#b86622")})
  output$demo_balance <- renderText({comp <- composition()
    paste0("Largest group difference on ",pretty_var_name(input$demo_compare),": ",fmt_num(100*max(abs(comp$random_pct-comp$pool_pct)),1)," percentage points.")
  })
  output$demo_inspected_note <- renderText({paste0("Draw ",inspected()$index," of 200 has the largest mismatch on ",pretty_var_name(input$demo_compare),". Our sample remains above.")})
  output$demo_inspected_balance <- renderText({paste0("Largest group difference: ",fmt_num(100*inspected()$gap,1)," percentage points. Selected by demographic mismatch, not by the outcome.")})
  output$demo_batch_plot <- renderPlot({
    req(state$batch,input$demo_outcome)
    values <- vapply(state$batch,estimate_value,numeric(1),var=input$demo_outcome,level=input$demo_level %||% NULL)
    validate(need(any(is.finite(values)),"No usable outcome estimates in this batch."))
    value <- function(d) estimate_value(d,input$demo_outcome,input$demo_level %||% NULL)
    p <- ggplot(data.frame(estimate=values),aes(x=estimate))+geom_histogram(bins=20,fill="#c4d9e9",color="white",na.rm=TRUE)+
      geom_vline(xintercept=value(base_data()),linetype="dashed",color="#555555")+
      geom_vline(xintercept=value(state$sample),color="#286ca5",linewidth=1,na.rm=TRUE)+
      geom_vline(xintercept=value(inspected()$sample),color="#b86622",linewidth=1,na.rm=TRUE)+
      labs(x=pretty_var_name(input$demo_outcome),y="Draws",caption="Gray dashed: full pool; blue: our sample\nOrange: most uneven draw")+theme_minimal(base_size=13)
    if (!is.numeric(base_data()[[input$demo_outcome]])) p <- p+scale_x_continuous(labels=function(x) paste0(round(100*x),"%"))
    p
  })
  recruitments <- bind_recruitment_examples(input,output,session,settings)
  invisible(list(state=state,settings=settings,composition=composition,select_sample=select_sample,inspected=inspected,recruitments=recruitments))
}
