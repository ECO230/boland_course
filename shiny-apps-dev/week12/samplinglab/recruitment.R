# Fixed, parallel recruitment examples. No random resampling within a rule.
recruitment_fields <- c("Response order"="response_order", "Summer availability"="summer_lacrosse",
  "Commute time"="commute_min", "In-person attendance"="attend", "Major"="major",
  "Class standing"="class_stand", "Age band"="age_band")

recruitment_choices <- function(pool) {
  recruitment_fields[vapply(recruitment_fields,function(v) v %in% names(pool) && any(!is.na(pool[[v]])),logical(1))]
}

recruitment_categories <- function(pool,field) {
  if (!field %in% names(pool)) return(character())
  values <- as.character(pool[[field]])
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (field=="major") values <- unlist(strsplit(values," + ",fixed=TRUE),use.names=FALSE)
  sort(unique(trimws(values)))
}

recruitment_options <- function(pool,field) {
  switch(field,
    response_order=c("First responses received"="first","Last responses received"="last"),
    summer_lacrosse=setNames(recruitment_categories(pool,field),
      ifelse(recruitment_categories(pool,field)=="Yes","In La Crosse over summer",
        ifelse(recruitment_categories(pool,field)=="No","Away over summer",recruitment_categories(pool,field)))),
    commute_min=c("Shortest commutes first"="short","Longest commutes first"="long"),
    attend=c("Most often in person first"="more","Least often in person first"="less"),
    class_stand=c("Earlier class years first"="earlier","Later class years first"="later"),
    age_band=c("Younger age bands first"="younger","Older age bands first"="older"),
    major=setNames(recruitment_categories(pool,field),recruitment_categories(pool,field)))
}

recruitment_default <- function(pool,field) {
  choices <- recruitment_options(pool,field)
  if (field=="summer_lacrosse" && "Yes" %in% choices) return("Yes")
  unname(choices[1])
}

recruitment_frame <- function(pool,field,choice) {
  if (!field %in% names(pool)) return(pool[FALSE, , drop=FALSE])
  df <- pool[order(pool$response_order,na.last=TRUE), , drop=FALSE]
  if (field=="response_order") {
    df <- df[!is.na(df$response_order), , drop=FALSE]
    return(if (choice=="last") df[rev(seq_len(nrow(df))), , drop=FALSE] else df)
  }
  x <- as.character(df[[field]])
  if (field=="summer_lacrosse") return(df[which(!is.na(x) & x==choice), , drop=FALSE])
  if (field=="major") {
    keep <- vapply(x,function(s) !is.na(s) && choice %in% trimws(strsplit(s," + ",fixed=TRUE)[[1]]),logical(1))
    return(df[which(keep), , drop=FALSE])
  }
  score <- switch(field,
    commute_min=suppressWarnings(as.numeric(x)),
    attend=unname(c("almost always"=1,"always"=1,"often"=2,"sometimes"=3,"rarely"=4,"never"=5)[tolower(x)]),
    class_stand=unname(c("first-year"=1,"freshman"=1,"sophomore"=2,"junior"=3,"senior"=4,"senior (+)"=4,"graduate student"=5)[tolower(x)]),
    age_band=suppressWarnings(as.numeric(sub("^[^0-9]*([0-9]+).*$","\\1",x))))
  keep <- which(is.finite(score)); score <- score[keep]; df <- df[keep, , drop=FALSE]
  descending <- choice %in% c("long","less","later","older")
  # Reverse the priority only. Equal-priority people retain response order.
  df[order(if (descending) -score else score,df$response_order,na.last=TRUE), , drop=FALSE]
}

quota_from_frame <- function(pool,frame,n,group) {
  levels <- sort(unique(comparison_levels(pool[[group]])))
  counts <- as.integer(table(factor(comparison_levels(pool[[group]]),levels=levels)))
  exact <- counts/nrow(pool)*min(n,nrow(pool)); target <- floor(exact)
  left <- min(n,nrow(pool))-sum(target)
  if (left>0) {
    priority <- order(exact-target,decreasing=TRUE)
    target[priority[seq_len(left)]] <- target[priority[seq_len(left)]]+1L
  }
  ids <- unlist(lapply(seq_along(levels),function(i) head(which(comparison_levels(frame[[group]])==levels[i]),target[i])),use.names=FALSE)
  sample <- frame[ids, , drop=FALSE]
  filled <- as.integer(table(factor(comparison_levels(sample[[group]]),levels=levels)))
  list(sample=sample,quotas=data.frame(Group=levels,Target=target,Filled=filled))
}

sample_composition_plot <- function(pool,sample,var,label="Sample",color="#286ca5") {
  req(var,var %in% names(pool))
  validate(need(nrow(sample)>0,"No respondents meet this recruitment rule."))
  comp <- composition_comparison(pool,sample,sample,var)
  df <- rbind(data.frame(group=comp$level,source="Full pool",pct=comp$pool_pct),
    data.frame(group=comp$level,source=label,pct=comp$random_pct))
  df$source <- factor(df$source,levels=c("Full pool",label))
  ggplot(df,aes(x=pct,y=group,fill=source))+geom_col(position="dodge",width=.7)+
    scale_fill_manual(values=setNames(c("#555555",color),c("Full pool",label)),name=NULL)+
    scale_x_continuous(limits=c(0,1),labels=function(x) paste0(round(100*x),"%"))+
    labs(x="Percent of rows, including missing responses",y=pretty_var_name(var))+
    theme_minimal(base_size=14)+theme(legend.position="top")
}

bind_recruitment_examples <- function(input,output,session,settings) {
  choices <- reactive({cfg <- settings(); recruitment_choices(cfg$pool)})
  selected <- function(field,pool) {
    value <- input[[paste0("recruit_",field)]]
    if (length(value) && value %in% recruitment_options(pool,field)) value else recruitment_default(pool,field)
  }
  samples <- reactive({
    cfg <- settings(); req(cfg$step %in% c("convenience","quota"))
    setNames(lapply(unname(choices()),function(field) {
      choice <- selected(field,cfg$pool); frame <- recruitment_frame(cfg$pool,field,choice)
      result <- if (cfg$step=="quota") quota_from_frame(cfg$pool,frame,cfg$n,cfg$group) else list(sample=head(frame,cfg$n),quotas=NULL)
      c(result,list(field=field,choice=choice,available=nrow(frame)))
    }),unname(choices()))
  })
  output$recruitment_examples <- renderUI({
    cfg <- settings(); req(cfg$step %in% c("convenience","quota"))
    tagList(lapply(unname(choices()),function(field) {
      label <- names(recruitment_fields)[match(field,recruitment_fields)]
      tagList(hr(),h4(label),fluidRow(
        column(4,selectInput(paste0("recruit_",field),"Selection rule",recruitment_options(cfg$pool,field),selected=isolate(selected(field,cfg$pool))),
          textOutput(paste0("recruit_note_",field)),
          if (cfg$step=="quota") tags$details(tags$summary("Quota targets and filled places"),tableOutput(paste0("recruit_quota_",field)))),
        column(8,plotOutput(paste0("recruit_plot_",field),height=230))))
    }))
  })
  output$recruitment_estimates <- renderTable({
    cfg <- settings(); req(input$demo_outcome,input$demo_compare); runs <- samples()
    all <- c(list("Full response pool"=cfg$pool),setNames(lapply(runs,`[[`,"sample"),names(choices())))
    is_numeric <- is.numeric(cfg$pool[[input$demo_outcome]])
    values <- vapply(all,estimate_value,numeric(1),var=input$demo_outcome,level=input$demo_level %||% NULL)
    gaps <- vapply(runs,function(r) {
      if (!nrow(r$sample)) return(NA_real_)
      comp <- composition_comparison(cfg$pool,r$sample,r$sample,input$demo_compare)
      100*max(abs(comp$random_pct-comp$pool_pct))
    },numeric(1))
    data.frame(Recruitment=names(all),Selected=vapply(all,nrow,integer(1)),
      Answered=vapply(all,function(d) sum(!is.na(d[[input$demo_outcome]])),integer(1)),
      Estimate=vapply(values,if (is_numeric) fmt_num else fmt_pct,character(1)),
      `Largest group difference (pp)`=c("--",vapply(gaps,function(x) if (is.finite(x)) fmt_num(x,1) else "No sample",character(1))),check.names=FALSE,row.names=NULL)
  },rownames=FALSE,striped=TRUE)
  for (field in unname(recruitment_fields)) local({
    rule <- field
    output[[paste0("recruit_plot_",rule)]] <- renderPlot({
      cfg <- settings(); run <- samples()[[rule]]; req(run,input$demo_compare)
      sample_composition_plot(cfg$pool,run$sample,input$demo_compare,"Recruited sample","#b86622")
    })
    output[[paste0("recruit_note_",rule)]] <- renderText({
      cfg <- settings(); run <- samples()[[rule]]; req(run)
      paste0(run$available," available; ",nrow(run$sample)," of ",cfg$n," requested selected.",
        if (rule=="commute_min") " Commute measures proximity, not on-campus housing." else "",
        if (cfg$step=="quota" && rule==cfg$group) " Quotas already control this variable, so reversing its priority may leave the sample unchanged." else "")
    })
    output[[paste0("recruit_quota_",rule)]] <- renderTable({req(settings()$step=="quota"); samples()[[rule]]$quotas},rownames=FALSE)
  })
  invisible(list(samples=samples,choices=choices))
}
