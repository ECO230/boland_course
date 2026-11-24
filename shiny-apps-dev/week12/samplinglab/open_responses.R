# Show the requested answer text without linked individual response records.
load_limesurvey_open_responses <- function() {
  con <- connect_limesurvey()
  on.exit(DBI::dbDisconnect(con),add=TRUE)
  tables <- grep("(^|_)questions$",DBI::dbListTables(con),value=TRUE)
  if (length(tables)!=1) stop("The survey question metadata is ambiguous.")
  metadata <- DBI::dbGetQuery(con,paste0("SELECT qid, gid, title FROM ",DBI::dbQuoteIdentifier(con,tables),
    " WHERE sid=931648 AND title=",DBI::dbQuoteString(con,"G03Q17")))
  if (nrow(metadata)!=1 || metadata$qid!=260 || metadata$gid!=28) stop("G03Q17 no longer matches the verified survey field.")
  field <- DBI::dbQuoteIdentifier(con,"931648X28X260")
  DBI::dbGetQuery(con,paste0("SELECT id AS response_order, COALESCE(submitdate, datestamp, startdate) AS answer_time, ",
    field," AS hobby_text FROM survey_931648 WHERE ",field," IS NOT NULL AND TRIM(",field,") <> ",
    DBI::dbQuoteString(con,"")," ORDER BY id ASC"))
}

open_text_answers <- function(pool, field="hobby_text", newest_first=TRUE) {
  if (!field %in% names(pool)) return(character())
  answer <- as.character(pool[[field]])
  rows <- which(!is.na(answer) & nzchar(trimws(answer)))
  if (!length(rows)) return(character())
  if ("answer_time" %in% names(pool)) {
    ordering <- order(pool$answer_time[rows],pool$response_order[rows],decreasing=newest_first,na.last=TRUE)
  } else if ("submitdate" %in% names(pool)) {
    ordering <- order(pool$submitdate[rows],pool$response_order[rows],decreasing=newest_first,na.last=TRUE)
  } else ordering <- order(pool$response_order[rows],decreasing=newest_first,na.last=TRUE)
  unname(answer[rows[ordering]])
}

response_word_counts <- function(answers,omit_common=FALSE) {
  stopwords <- strsplit(paste(
    "a an and are as at be been being but by can could did do does dont for from had has have",
    "he her here hers him his how i if im in into is it its just like me my no not of on or our",
    "ours really she so some than that the their theirs them then there these they this those",
    "to too up us very was we were what when where which who why will with would you your",
    "hobby hobbies interest interests community skill skills obscure especially")," +")[[1]]
  tokens <- lapply(as.character(answers),function(answer) {
    if (is.na(answer)) return(character())
    text <- gsub("'","",tolower(enc2utf8(answer)),fixed=TRUE)
    words <- strsplit(gsub("[^[:alnum:]]"," ",text)," +")[[1]]
    words <- words[nzchar(words)]
    if (omit_common) words <- words[!words %in% stopwords]
    words
  })
  counts <- table(unlist(tokens,use.names=FALSE))
  mentions <- table(unlist(lapply(tokens,unique),use.names=FALSE))
  result <- data.frame(Word=as.character(names(counts)),Occurrences=as.integer(counts),
    Responses=as.integer(mentions[names(counts)]),stringsAsFactors=FALSE)
  result[order(-result$Occurrences,result$Word), , drop=FALSE]
}

open_responses_ui <- function() {
  tabPanel("Open-ended responses",br(),
    h4("What is your most obscure (safe for work) hobby, interest, community, or skill?"),
    p("All recorded answers to G03Q17. Sampling controls do not filter these responses."),
    radioButtons("open_response_order","Show answers",c("Newest first"="newest","Oldest first"="oldest","Word cloud"="cloud"),inline=TRUE),
    actionButton("refresh_open_responses","Refresh responses"),
    uiOutput("open_responses_source"),textOutput("open_responses_count"),
    conditionalPanel("input.open_response_order != 'cloud'",uiOutput("open_responses_list")),
    conditionalPanel("input.open_response_order == 'cloud'",
      p("Larger words occur more often. All words are included by default."),
      checkboxInput("cloud_omit_common","Omit common filler and prompt words",FALSE),
      uiOutput("open_responses_cloud"),
      tags$details(tags$summary("Word counts"),tableOutput("open_responses_word_counts"))))
}

bind_open_responses <- function(input,output,session,response_data,last_refreshed=function() NULL) {
  data <- reactive({value <- response_data(); req(!is.null(value)); value})
  answers <- reactive(open_text_answers(data(),newest_first=!identical(input$open_response_order,"oldest")))
  output$open_responses_source <- renderUI({
    if (is.null(response_data())) return(p("The live survey connection is unavailable. No simulated answers are shown."))
    tagList(p(strong("Live survey: G03Q17")),
      p(class="small",paste("Refreshes every 15 seconds while this view is open.",
        if (!is.null(last_refreshed())) paste("Updated",format(last_refreshed(),"%I:%M:%S %p %Z",tz="America/Chicago")) else "")))
  })
  output$open_responses_count <- renderText(paste(length(answers()),"raw answers"))
  output$open_responses_list <- renderUI({
    values <- answers()
    if (!length(values)) return(p("No answers to this open-ended question are available in the current response pool."))
    # htmltools escapes response-authored markup instead of interpreting it.
    tags$ol(class="open-response-list",lapply(values,function(value) tags$li(value)))
  })
  words <- reactive(response_word_counts(answers(),omit_common=isTRUE(input$cloud_omit_common)))
  output$open_responses_cloud <- renderUI({
    terms <- words()
    if (!nrow(terms)) return(p("No words are available for the cloud."))
    list <- unname(Map(function(word,count) list(word,count),terms$Word,terms$Occurrences))
    div(class="response-cloud",`data-words`=jsonlite::toJSON(list,auto_unbox=TRUE),
      tags$canvas(role="img",`aria-label`="Word cloud: word size shows frequency. Complete counts are available below."),
      p(class="small response-cloud-status",`aria-live`="polite","Laying out the word cloud..."))
  })
  output$open_responses_word_counts <- renderTable(words(),rownames=FALSE)
  invisible(list(answers=answers,words=words))
}
