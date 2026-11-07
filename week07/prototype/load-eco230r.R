# WebR setup for the pinned, pure-R eco230r course package.
library_dir <- .libPaths()[1]
if (!file.exists(file.path(library_dir, "eco230r", "DESCRIPTION"))) {
  utils::untar("data/eco230r-course.tar.gz", exdir = library_dir, tar = "internal")
}
suppressPackageStartupMessages(library(eco230r))
suppressPackageStartupMessages(library(dplyr))
