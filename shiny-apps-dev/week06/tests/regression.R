# Run from the course repository root with the course R environment.
coin <- new.env(); sys.source("shiny-apps-dev/week06/coinflipper/app.R", coin)
sampling <- new.env(); sys.source("shiny-apps-dev/week06/samplingdist/app.R", sampling)
set.seed(230)
for (i in seq_len(100)) {
  sequence <- coin$flip_human(100)
  runs <- rle(sequence)$lengths
  stopifnot(max(runs) <= 4, sum(runs == 4) <= 1)
}
shiny::testServer(sampling$server, {
  session$setInputs(pop_id="chickweight",pop_id2="chickweight",n="30",conf=.95)
  session$setInputs(add1=1)
  stopifnot(rv$sample_id==1, length(rv$current_sample)==30)
  expected <- sampling$ci_for_sample(rv$current_sample,.95)
  stopifnot(isTRUE(all.equal(rv$current_ci,expected)))
  session$setInputs(run100=1)
  stopifnot(rv$sample_id==1,rv$running)
  old_generation <- rv$generation
  session$setInputs(sample_drawn=list(generation=old_generation,index=1))
  session$setInputs(clear=1)
  Sys.sleep(.1);later::run_now();session$flushReact()
  stopifnot(rv$sample_id==0,!rv$running)
  session$setInputs(run100=2)
  for (i in 1:99) {
    session$setInputs(sample_drawn=list(generation=rv$generation,index=rv$sample_id))
    Sys.sleep(.085);later::run_now();session$flushReact()
  }
  stopifnot(rv$sample_id==100,nrow(rv$samples)==100,!rv$running)
  session$setInputs(run100=3)
  session$setInputs(sample_drawn=list(generation=rv$generation,index=1))
  session$setInputs(n="5")
  Sys.sleep(.1);later::run_now();session$flushReact()
  stopifnot(rv$sample_id==0,!rv$running)
})
cat("PASS: human-run rules, sample statistics, incremental 100-sample completion, and pending-callback cancellation.\n")
