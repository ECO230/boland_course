# Render the section-specific syllabus URLs referenced by the Canvas manifest.
# The environment guard prevents the nested single-document renders from
# recursively starting another round of section renders.

if (identical(Sys.getenv("ECO230_RENDERING_SECTION_SYLLABI"), "1")) {
  quit(status = 0)
}

# Keep incremental slide/page renders fast. Quarto sets this flag only for a
# complete project render, including the render performed before publication.
if (!identical(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"), "1")) {
  quit(status = 0)
}

Sys.setenv(ECO230_RENDERING_SECTION_SYLLABI = "1")

quarto_bin <- Sys.getenv("QUARTO_BIN_PATH")
if (nzchar(quarto_bin) && dir.exists(quarto_bin)) {
  executable <- if (.Platform$OS.type == "windows") "quarto.exe" else "quarto"
  quarto_bin <- file.path(quarto_bin, executable)
}
if (!nzchar(quarto_bin)) {
  quarto_bin <- Sys.which("quarto")
}

if (!nzchar(quarto_bin) && .Platform$OS.type == "windows") {
  bundled_quarto <- file.path(
    Sys.getenv("ProgramFiles"),
    "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe"
  )
  if (file.exists(bundled_quarto)) {
    quarto_bin <- bundled_quarto
  }
}

if (!nzchar(quarto_bin)) {
  stop("Could not locate the Quarto executable for section syllabus renders.")
}

# These renders use a source file under syllabus/ but deliberately place the
# finished pages at the website root. Quarto therefore writes asset and navbar
# links with one unnecessary "../" prefix. Normalize only href/src attributes;
# leave prose and any other generated content untouched.
normalize_root_links <- function(path) {
  html <- readLines(path, warn = FALSE, encoding = "UTF-8")
  html <- gsub(
    "((?:href|src)=[\"'])\\.\\./",
    "\\1",
    html,
    perl = TRUE
  )
  writeLines(html, path, useBytes = TRUE)
}

for (section in c(4, 11, 12)) {
  output_name <- sprintf("syllabus_section_%s.html", section)
  status <- system2(
    quarto_bin,
    c(
      "render",
      "syllabus/syllabus.qmd",
      "-P", sprintf("class_section:%s", section),
      "--output", output_name
    )
  )
  if (!identical(status, 0L)) {
    stop("Section ", section, " syllabus render failed with status ", status, ".")
  }

  output_path <- file.path("_site", output_name)
  if (!file.exists(output_path)) {
    stop("Section ", section, " syllabus output was not created at ", output_path, ".")
  }
  normalize_root_links(output_path)
}
