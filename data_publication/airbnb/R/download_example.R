# Intended student example. The URLs will not exist until redistribution is
# approved and the immutable release is published.

library(arrow)

base_url <- paste0(
  "https://data.60land.com/",
  "project1/2026-fall/v1/airbnb/"
)

listings <- read_parquet(paste0(base_url, "airbnb_listings.parquet"))
calendar <- read_parquet(paste0(base_url, "airbnb_calendar.parquet"))
reviews_monthly <- read_parquet(
  paste0(base_url, "airbnb_reviews_monthly.parquet")
)
listing_analysis <- read_parquet(
  paste0(base_url, "airbnb_listing_analysis.parquet")
)

# Example: compare listing-level snapshot prices by market and room type.
listing_analysis |>
  dplyr::group_by(market_name, room_type) |>
  dplyr::summarise(
    listings = dplyr::n(),
    median_snapshot_price = median(snapshot_price_usd, na.rm = TRUE),
    .groups = "drop"
  )
