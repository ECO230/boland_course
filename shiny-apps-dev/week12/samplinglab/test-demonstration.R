source("app.R",local=TRUE)
set.seed(241124)
pool <- generate_demo_data(120)
frame <- recruitment_frame(pool,"summer_lacrosse","Yes")
stopifnot(nrow(frame)==60,all(frame$summer_lacrosse=="Yes"))
quota <- quota_from_frame(pool,frame,20,"summer_lacrosse")
stopifnot(nrow(quota$sample)==10,sum(quota$quotas$Target)==20,
  quota$quotas$Filled[quota$quotas$Group=="No"]==0)
empty <- quota_from_frame(pool,frame[FALSE,],20,"summer_lacrosse")
stopifnot(nrow(empty$sample)==0,sum(empty$quotas$Filled)==0)

shiny::testServer(server,{
  session$setInputs(data_mode="Demo data",outcome_var="sleep_hrs",compare_var="class_stand",sample_n=20,
    nonrandom_method="Convenience sample",convenience_order="response_order",
    demo_step="srs",demo_n=20,demo_outcome="sleep_hrs",demo_compare="class_stand")
  session$flushReact(); demo <- class_demonstration
  # The initial view already has a real sample and an independent batch of 200.
  stopifnot(nrow(demo$state$sample)==20,length(demo$state$batch)==200,
    grepl("Most uneven draw",output$demo_estimate))
  ids <- demo$state$sample$response_order; saved <- demo$state$batch
  session$setInputs(demo_compare="summer_lacrosse",demo_outcome="work_hrs")
  gaps <- vapply(saved,function(s) {
    comp <- composition_comparison(base_data(),s,s,"summer_lacrosse")
    max(abs(comp$random_pct-comp$pool_pct))
  },numeric(1))
  stopifnot(identical(demo$state$sample$response_order,ids),identical(saved,demo$state$batch),
    identical(demo$inspected()$sample,saved[[which.max(gaps)]]))
  session$setInputs(demo_batch=1)
  stopifnot(identical(ids,demo$state$sample$response_order),length(demo$state$batch)==200)
  session$setInputs(demo_step="stratified",demo_strata="class_stand")
  comp <- composition_comparison(base_data(),demo$state$sample,demo$state$sample,"class_stand")
  stopifnot(all(abs(comp$random_n-comp$pool_pct*20)<=1),length(demo$state$batch)==200)
  session$setInputs(demo_step="systematic",demo_pattern=TRUE)
  stopifnot(nrow(demo$state$sample)==20,length(unique(demo$state$sample$summer_lacrosse))==1)
  session$setInputs(demo_step="convenience")
  runs <- demo$recruitments$samples()
  stopifnot(length(runs)==7,is.null(demo$state$batch),
    identical(runs$response_order$sample$response_order,1:20),
    all(runs$summer_lacrosse$sample$summer_lacrosse=="Yes"))
  fixed_summer <- runs$summer_lacrosse$sample$response_order
  session$setInputs(recruit_response_order="last")
  runs <- demo$recruitments$samples()
  stopifnot(identical(runs$response_order$sample$response_order,120:101),
    identical(runs$summer_lacrosse$sample$response_order,fixed_summer))
  session$setInputs(recruit_summer_lacrosse="No")
  stopifnot(all(demo$recruitments$samples()$summer_lacrosse$sample$summer_lacrosse=="No"))
  short <- demo$recruitments$samples()$commute_min$sample$commute_min
  session$setInputs(recruit_commute_min="long",recruit_attend="less",recruit_major="Finance",
    recruit_class_stand="later",recruit_age_band="older")
  runs <- demo$recruitments$samples()
  stopifnot(mean(runs$commute_min$sample$commute_min)>=mean(short),
    all(runs$major$sample$major=="Finance"))
  fixed <- lapply(runs,function(r) r$sample$response_order)
  session$setInputs(demo_compare="major",demo_outcome="sleep_hrs")
  stopifnot(identical(fixed,lapply(demo$recruitments$samples(),function(r) r$sample$response_order)))
  session$setInputs(demo_step="quota",demo_strata="class_stand")
  runs <- demo$recruitments$samples()
  for (run in runs) stopifnot(sum(run$quotas$Target)==20,all(run$quotas$Filled<=run$quotas$Target),
    sum(run$quotas$Filled)==nrow(run$sample))
  comp <- composition_comparison(base_data(),runs$response_order$sample,runs$response_order$sample,"class_stand")
  stopifnot(all(abs(comp$random_n-comp$pool_pct*20)<=1),
    all(runs$summer_lacrosse$sample$summer_lacrosse=="No"))
  session$setInputs(demo_step="srs",demo_n=120)
  stopifnot(all(vapply(demo$state$batch,function(s) abs(estimate_value(s,"sleep_hrs")-estimate_value(base_data(),"sleep_hrs"))<1e-12,logical(1))))
})

# Known priorities, ties, missing values, and combined major membership.
known <- data.frame(response_order=1:6,major=c("Finance + Economics","Finance",NA,"Marketing","Economics","Financial Studies"),
  commute_min=c(0,5,NA,15,10,20),attend=c("Almost always","Often",NA,"Rarely","Sometimes","Often"),
  class_stand=c("First-year","Sophomore",NA,"Senior (+)","Junior","Graduate student"),
  age_band=c("18 - 20","20 - 22",NA,"22 - 24","20 - 22","22 - 24"))
stopifnot(identical(recruitment_frame(known,"major","Finance")$response_order,1:2),
  identical(recruitment_frame(known,"commute_min","long")$response_order,c(6L,4L,5L,2L,1L)),
  identical(recruitment_frame(known,"attend","more")$response_order,c(1L,2L,6L,5L,4L)),
  identical(recruitment_frame(known,"class_stand","later")$response_order,c(6L,4L,5L,2L,1L)),
  identical(recruitment_frame(known,"age_band","older")$response_order,c(4L,6L,2L,5L,1L)),
  nrow(recruitment_frame(known,"absent","X"))==0)

# Separate answer records, safe HTML, literal words and one mention per response.
text_pool <- data.frame(response_order=1:5,submitdate=c("2026-11-24 12:00","2026-11-24 12:04","2026-11-24 12:02",NA,"2026-11-24 12:01"),
  hobby_text=c("Chess","<script>alert(1)</script>","Chess"," ",NA),response_id=1001:1005)
stopifnot(identical(open_text_answers(text_pool),c("<script>alert(1)</script>","Chess","Chess")),
  identical(open_text_answers(text_pool,newest_first=FALSE),c("Chess","Chess","<script>alert(1)</script>")))
raw_spaces <- data.frame(response_order=1,hobby_text="  raw text\nunchanged  ")
stopifnot(identical(open_text_answers(raw_spaces),raw_spaces$hobby_text))
terms <- response_word_counts(c("Chess chess and painting","chess, PAINTING!","no especially obscure hobby"),omit_common=TRUE)
stopifnot(identical(terms$Word,c("chess","painting")),all(terms$Responses==2),
  identical(terms$Occurrences,c(3L,2L)),nrow(response_word_counts(c(NA,"")))==0,
  all(c("the","and","i","3d") %in% response_word_counts(c("the and I 3D"))$Word),
  nrow(response_word_counts(paste0("term",seq_len(100))))==100)
shiny::testServer(function(input,output,session) {
  text_view <- bind_open_responses(input,output,session,reactive(text_pool))
},{
  session$setInputs(data_mode="Demo data",open_response_order="newest")
  body <- output$open_responses_list$html
  stopifnot(grepl("&lt;script&gt;",body,fixed=TRUE),!grepl("<script>",body,fixed=TRUE),!grepl("1002",body,fixed=TRUE))
  words <- text_view$words()
  session$setInputs(open_response_order="cloud")
  stopifnot(identical(words,text_view$words()),grepl("response-cloud",output$open_responses_cloud$html,fixed=TRUE))
  session$setInputs(open_response_order="oldest")
  stopifnot(length(text_view$answers())==3,identical(text_view$answers(),open_text_answers(text_pool,newest_first=FALSE)))
  session$setInputs(open_response_order="newest")
  stopifnot(length(text_view$answers())==3,identical(text_view$answers(),open_text_answers(text_pool)))
})
cat("Automatic sample/batch comparisons, seven independent recruitment rules, quota eligibility, ordered text, and word cloud checks passed.\n")
