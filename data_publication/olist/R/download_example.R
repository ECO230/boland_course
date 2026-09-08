library(arrow)
library(dplyr)

base_url <- "https://data.60land.com/project1/2026-fall/v1/olist"
cache_dir <- file.path("data", "olist")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

download_parquet <- function(filename) {
  destination <- file.path(cache_dir, filename)
  if (!file.exists(destination)) {
    download.file(file.path(base_url, filename), destination, mode = "wb")
  }
  read_parquet(destination)
}

orders <- download_parquet("olist_orders.parquet")
items <- download_parquet("olist_order_items.parquet")

monthly_demand <- orders |>
  filter(order_status == "delivered") |>
  mutate(month = format(purchased_at, "%Y-%m")) |>
  count(month, name = "orders")

print(monthly_demand)
