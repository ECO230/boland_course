# Run with Rscript --vanilla test-comparison.R from the app directory.
source("comparison.R")
set.seed(230)
d <- data.frame(id = seq_len(23), group = c(rep("A", 10), rep("B", 8), rep("C", 4), NA), value = seq_len(23))
for (n in seq_len(nrow(d))) {
  for (random in c(FALSE, TRUE)) {
    s <- sample_by_group(d, n, "group", random)
    stopifnot(nrow(s) == n, !anyDuplicated(s$id), all(s$id %in% d$id))
    comp <- composition_comparison(d, s, s, "group")
    stopifnot(sum(comp$pool_n) == nrow(d), sum(comp$random_n) == n,
              abs(sum(comp$random_pct) - 1) < 1e-12,
              all(abs(comp$random_n - comp$pool_pct * n) <= 1))
  }
}
# A single numeric row index must never be sampled as 1:index.
rare <- data.frame(id = 1:3, group = c("A", "A", "B"))
for (i in 1:20) stopifnot(setequal(sample_by_group(rare, 3, "group")$id, rare$id))
# Quotas are nonrandom and include missing responses in their allocation.
stopifnot(identical(sample_by_group(d, 12, "group", FALSE), sample_by_group(d, 12, "group", FALSE)))
comp <- composition_comparison(d, d[1:5, ], d[23, , drop=FALSE], "group")
stopifnot(comp$nonrandom_n[comp$level == "(No response)"] == 1,
          comp$random_n[comp$level == "C"] == 0)
cat("Sampling allocation, singleton groups, missing responses, and composition checks passed.\n")

# Optional integration checks require the app's existing Shiny dependencies.
if (requireNamespace("shiny", quietly = TRUE)) {
  source("app.R", local = TRUE)
  shiny::testServer(server, {
    session$setInputs(data_mode = "Demo data", outcome_var = "sleep_hrs",
      compare_var = "class_stand", sample_n = 20, method = "Simple random sample",
      nonrandom_method = "Convenience sample", convenience_order = "response_order")
    session$flushReact()
    stopifnot(nrow(rv$paired_random) == 20, nrow(rv$paired_nonrandom) == 20)
    fixed <- rv$paired_nonrandom$response_order
    before <- rv$paired_random$response_order
    session$setInputs(redraw_random = 1)
    stopifnot(identical(fixed, rv$paired_nonrandom$response_order),
              !identical(before, rv$paired_random$response_order))
    before <- rv$paired_random$response_order
    session$setInputs(compare_var = "attend", outcome_var = "work_hrs")
    stopifnot(identical(before, rv$paired_random$response_order),
              identical(fixed, rv$paired_nonrandom$response_order))
    session$setInputs(nonrandom_method = "Quota sample", quota_var = "class_stand")
    comp <- composition_comparison(base_data(), rv$paired_random, rv$paired_nonrandom, "class_stand")
    stopifnot(nrow(rv$paired_nonrandom) == 20,
              all(abs(comp$nonrandom_n - comp$pool_pct * 20) <= 1))
    quota_rows <- rv$paired_nonrandom$response_order
    session$setInputs(compare_var = "conf_interp")
    stopifnot(identical(quota_rows, rv$paired_nonrandom$response_order))
    session$setInputs(sample_n = 120)
    stopifnot(nrow(rv$paired_random) == 120, nrow(rv$paired_nonrandom) == 120)
    stopifnot(all(abs(paired_composition()$random_pct - paired_composition()$pool_pct) < 1e-12))
    # A saved batch links each estimate to exactly the rows used for it.
    session$setInputs(sample_n = 20, outcome_var = "sleep_hrs", random_run_count = 100)
    session$setInputs(run_random_samples = 1)
    stopifnot(length(rv$random_run$draws) == 100,
              all(lengths(rv$random_run$draws) == 20),
              all(vapply(rv$random_run$draws, function(ids) !anyDuplicated(ids), logical(1))))
    session$setInputs(inspect_farthest_draw = 1)
    expected <- which.max(abs(rv$random_run$estimates - rv$random_run$pool))
    stopifnot(repeated_comparison$selected() == expected,
      identical(repeated_comparison$rows()$response_order,
                base_data()$response_order[rv$random_run$draws[[expected]]]))
    saved <- rv$random_run$draws
    session$setInputs(compare_var = "attend")
    stopifnot(identical(saved, rv$random_run$draws), repeated_comparison$selected() == expected)
    session$setInputs(inspect_draw = 7)
    stopifnot(repeated_comparison$selected() == 7,
      abs(estimate_value(repeated_comparison$rows(), "sleep_hrs") - rv$random_run$estimates[7]) < 1e-12)
    session$setInputs(redraw_random = 2)
    stopifnot(identical(saved, rv$random_run$draws))
    session$setInputs(outcome_var = "work_hrs")
    stopifnot(is.null(rv$random_run))
    # Every draw of the full pool has the same outcome estimate.
    session$setInputs(sample_n = 120)
    session$setInputs(run_random_samples = 2)
    stopifnot(all(abs(rv$random_run$estimates - rv$random_run$pool) < 1e-12))
    session$setInputs(sample_n = 10)
    stopifnot(is.null(rv$random_run))
    # Percent outcomes use the same saved-row inspection path.
    session$setInputs(outcome_var = "attend", outcome_level = "Almost always")
    session$setInputs(run_random_samples = 3)
    stopifnot(all(rv$random_run$estimates >= 0 & rv$random_run$estimates <= 1))
    # Repeated-draw controls must still work after adding the comparison tab.
    session$setInputs(n_reps = 50, simulate = 1)
    stopifnot(nrow(rv$sim_results) == 50)
  })
  # Non-divisible systematic intervals still select exactly n unique rows.
  for (n in seq_len(nrow(d))) {
    sample <- sample_systematic(d, n, "id")
    stopifnot(nrow(sample) == n, !anyDuplicated(sample$id), all(sample$id %in% d$id))
  }
  # A missing-only numeric outcome is unavailable rather than NaN or a verdict.
  stopifnot(is.na(estimate_value(data.frame(x = c(NA_real_, NA_real_)), "x")))
  clean <- make_clean_data(data.frame(class_stand = c("1", "2", ""), x = c("2", "3", "")))
  stopifnot(is.character(clean$class_stand), is.na(clean$class_stand[3]))
  # Synthetic survey rows preserve multiple majors and missing selections.
  raw <- data.frame(id=1:2, submitdate=c("2026-11-20", "2026-11-20"))
  fields <- c("931648X26X258", "931648X26X211", "931648X26X225", "931648X26X226",
              "931648X26X227", "931648X26X228", "931648X27X229", "931648X27X248SQ001",
              "931648X28X256", "931648X28X257", "931648X28X259", "931648X28X260")
  for (field in fields) raw[[field]] <- rep(NA_character_, 2)
  for (i in 1:8) raw[[paste0("931648X26X216SQ00", i)]] <- rep(NA_character_, 2)
  raw$`931648X26X216SQ003`[1] <- "Y"
  raw$`931648X26X216SQ004`[1] <- "Y"
  mapped <- clean_limesurvey_responses(raw)
  stopifnot(mapped$major[1] == "Finance + Economics", is.na(mapped$major[2]))
  # Exercise the live branch with synthetic rows; never query student data.
  raw$`931648X26X211` <- c("AO01", "AO03")
  raw$`931648X26X225` <- c("AO01", "AO02")
  raw$`931648X26X227` <- c("12", "20")
  raw$`931648X26X228` <- c("6", "8")
  raw$`931648X28X260` <- c("PRIVATE_HOBBY", "PRIVATE_HOBBY")
  original_loader <- load_limesurvey_responses
  original_open_loader <- load_limesurvey_open_responses
  original_image_loader <- load_limesurvey_image_responses
  live_loads <- 0L
  open_loads <- 0L
  image_loads <- 0L
  load_limesurvey_responses <- function() { live_loads <<- live_loads + 1L; raw }
  load_limesurvey_open_responses <- function() {
    open_loads <<- open_loads+1L
    data.frame(response_order=raw$id,answer_time=raw$submitdate,hobby_text=raw$`931648X28X260`)
  }
  load_limesurvey_image_responses <- function() {
    image_loads <<- image_loads+1L
    if (image_loads>=3L) return(data.frame(image_click_coords=c("the bottom","the middle")))
    data.frame(image_click_coords=c("10,20","30,80","60,40","the bottom",
      if (image_loads>=2L) "80,10" else character()))
  }
  shiny::testServer(server, {
    session$setInputs(data_mode = "Live survey DB", outcome_var = "sleep_hrs",
      compare_var = "class_stand", sample_n = 1, method = "Stratified sample",
      nonrandom_method = "Convenience sample", convenience_order = "response_order",
      app_mode="Explore",lab_tabs="Open-ended responses")
    session$flushReact()
    stopifnot(nrow(base_data()) == 2, live_loads == 1,
      !is.null(rv$live_loaded_at), !grepl("PRIVATE_HOBBY", output$data_preview),
      !grepl("PRIVATE_HOBBY", output$sample_preview),
      !grepl("hobby_text", paste(output$strata_var_ui, collapse="")))
    # Hundreds of draws reuse the loaded pool; only an explicit refresh queries again.
    session$setInputs(demo_step="srs",demo_n=1,demo_outcome="sleep_hrs",demo_compare="class_stand")
    stopifnot(length(class_demonstration$state$batch)==200,live_loads==1L)
    loaded_at <- rv$live_loaded_at
    session$setInputs(demo_batch=1)
    session$setInputs(demo_draw=1)
    stopifnot(length(class_demonstration$state$batch)==200,live_loads==1L,
      identical(rv$live_loaded_at,loaded_at))
    rv$live_loaded_at <- as.POSIXct("2026-11-24 14:23:45",tz="America/Chicago")
    session$flushReact()
    stopifnot(grepl("Last refreshed: 02:23:45 PM CST",output$demo_source$html,fixed=TRUE))
    session$setInputs(refresh_live = 1)
    stopifnot(live_loads == 2, nrow(base_data()) == 2,
      grepl(format(rv$live_loaded_at,"%I:%M:%S %p %Z",tz="America/Chicago"),output$demo_source$html,fixed=TRUE))
    stopifnot(grepl("PRIVATE_HOBBY", output$open_responses_list$html, fixed=TRUE))
    session$setInputs(refresh_open_responses = 1)
    stopifnot(live_loads == 2,open_loads == 2,nrow(base_data()) == 2)
    session$setInputs(data_mode="Demo data",sample_n=1)
    stopifnot(nrow(base_data())==120,length(open_responses$answers())==2,
      all(open_responses$answers()=="PRIVATE_HOBBY"),
      !grepl("Last refreshed:",output$demo_source$html,fixed=TRUE))
    # The map has its own actual-survey feed even with simulated sampling selected.
    session$setInputs(lab_tabs="Image heatmap")
    stopifnot(image_loads==1L,nrow(click_df())==3L,output$n_clicks=="3",
      all(click_df()$x==c(10,30,60)),live_loads==2L,open_loads==2L)
    session$setInputs(lab_tabs="Open-ended responses",sample_n=2)
    session$setInputs(lab_tabs="Image heatmap")
    stopifnot(open_loads==2L,image_loads==1L)
    session$setInputs(refresh_heatmap=1)
    stopifnot(image_loads==2L,nrow(click_df())==4L,live_loads==2L,open_loads==2L)
    session$setInputs(refresh_heatmap=2)
    stopifnot(image_loads==3L,nrow(click_df())==0L,!is.null(output$heatmap_plot),
      !grepl("15 seconds",output$open_responses_source$html,fixed=TRUE))
  })
  load_limesurvey_responses <- original_loader
  load_limesurvey_open_responses <- original_open_loader
  load_limesurvey_image_responses <- original_image_loader
  cat("Reactive comparison, stable rows, full-pool size, repeated sampling, and missing-outcome checks passed.\n")
}
