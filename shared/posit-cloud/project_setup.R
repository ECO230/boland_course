# renv converts this source URL to the appropriate Posit Package Manager
# binary URL for the Linux distribution and R version in Posit Cloud.
posit_package_repo <- "https://packagemanager.posit.co/cran/latest"

options(
  repos = c(CRAN = posit_package_repo),
  renv.config.ppm.enabled = TRUE
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore(repos = getOption("repos"), prompt = FALSE)

message("The ECO 230 assignment environment is ready.")
