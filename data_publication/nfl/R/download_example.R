library(arrow)

base_url <- "https://data.60land.com/project1/2026-fall/v1/nfl"
data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

download_parquet <- function(filename) {
  destination <- file.path(data_dir, filename)
  if (!file.exists(destination)) {
    download.file(
      paste0(base_url, "/", filename),
      destination,
      mode = "wb",
      quiet = FALSE
    )
  }
  destination
}

plays_file <- download_parquet("nfl_plays.parquet")
players_file <- download_parquet("nfl_players.parquet")

plays <- arrow::read_parquet(plays_file)
players <- arrow::read_parquet(players_file)

plays |>
  dplyr::count(season, play_type, sort = TRUE) |>
  print(n = 20)

players |>
  dplyr::select(
    player_id,
    display_name,
    birth_date,
    position,
    height_inches,
    weight_pounds,
    college_name
  ) |>
  print(n = 10)
