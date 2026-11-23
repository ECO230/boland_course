source("app.R", local = TRUE)
set.seed(231123)
pool <- generate_demo_data(120)
# Availability restrictions are not filled with people who could not be reached.
frame <- summer_frame(pool, TRUE)
stopifnot(nrow(frame) == 60, all(frame$summer_lacrosse == "Yes"))
quota <- quota_from_frame(pool, frame, 20, "summer_lacrosse")
stopifnot(nrow(quota$sample) == 10, sum(quota$quotas$Target) == 20,
          quota$quotas$Filled[quota$quotas$Group == "No"] == 0)
empty <- quota_from_frame(pool, frame[FALSE,], 20, "summer_lacrosse")
stopifnot(nrow(empty$sample) == 0, sum(empty$quotas$Filled) == 0)
shiny::testServer(server, {
  session$setInputs(data_mode="Demo data", outcome_var="sleep_hrs", compare_var="class_stand", sample_n=20,
    method="Simple random sample", nonrandom_method="Convenience sample", convenience_order="response_order",
    demo_step="srs", demo_n=20, demo_outcome="sleep_hrs", demo_compare="class_stand")
  session$flushReact()
  demo <- class_demonstration
  stopifnot(nrow(demo$state$sample) == 20, !demo$state$reveal)
  ids <- demo$state$sample$response_order
  session$setInputs(demo_compare="summer_lacrosse")
  stopifnot(identical(ids, demo$state$sample$response_order))
  session$setInputs(demo_batch=1)
  stopifnot(length(demo$state$batch) == 200)
  saved <- demo$state$batch
  session$setInputs(demo_inspect=1)
  gaps <- vapply(saved, function(s) max(abs(composition_comparison(base_data(),s,s,"summer_lacrosse")$random_pct - .5)), numeric(1))
  stopifnot(identical(demo$state$sample, saved[[which.max(gaps)]]), demo$state$reveal)
  session$setInputs(demo_step="stratified", demo_strata="class_stand")
  comp <- composition_comparison(base_data(),demo$state$sample,demo$state$sample,"class_stand")
  stopifnot(all(abs(comp$random_n - comp$pool_pct*20) <= 1), is.null(demo$state$batch))
  ids <- demo$state$sample$response_order
  session$setInputs(demo_compare="summer_lacrosse")
  stopifnot(identical(ids, demo$state$sample$response_order))
  session$setInputs(demo_step="systematic", demo_pattern=TRUE)
  stopifnot(nrow(demo$state$sample) == 20, length(unique(demo$state$sample$summer_lacrosse)) == 1)
  session$setInputs(demo_step="convenience", demo_local_only=TRUE)
  stopifnot(all(demo$state$sample$summer_lacrosse == "Yes"))
  session$setInputs(demo_step="quota", demo_strata="summer_lacrosse")
  stopifnot(nrow(demo$state$sample) == 10, sum(demo$state$quotas$Target)==20)
})
cat("Instructor sequence: saved uneven draws, stable inspections, stratification, periodic list, restricted recruitment, and unfilled quotas passed.\n")
