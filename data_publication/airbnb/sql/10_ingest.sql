CREATE SCHEMA IF NOT EXISTS raw;

CREATE OR REPLACE TABLE raw.listings AS
SELECT 'chicago'::VARCHAR AS market_id, DATE '2026-06-24' AS snapshot_date,
  "id" AS source_listing_id, "last_scraped", "neighbourhood_cleansed",
  "latitude", "longitude", "property_type", "room_type", "accommodates",
  "bathrooms", "bathrooms_text", "bedrooms", "beds", "amenities", "price",
  "minimum_nights", "maximum_nights", "minimum_minimum_nights",
  "maximum_minimum_nights", "minimum_maximum_nights",
  "maximum_maximum_nights", "minimum_nights_avg_ntm",
  "maximum_nights_avg_ntm", "has_availability", "availability_30",
  "availability_60", "availability_90", "availability_365",
  "number_of_reviews", "number_of_reviews_ltm", "number_of_reviews_l30d",
  "first_review", "last_review", "review_scores_rating",
  "review_scores_accuracy", "review_scores_cleanliness",
  "review_scores_checkin", "review_scores_communication",
  "review_scores_location", "review_scores_value", "license",
  "instant_bookable", "calculated_host_listings_count",
  "calculated_host_listings_count_entire_homes",
  "calculated_host_listings_count_private_rooms",
  "calculated_host_listings_count_shared_rooms"
FROM read_csv(
  '{{CHICAGO_LISTINGS}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false, parallel = false
)
UNION ALL BY NAME
SELECT 'twin_cities_msa'::VARCHAR AS market_id, DATE '2026-06-27' AS snapshot_date,
  "id" AS source_listing_id, "last_scraped", "neighbourhood_cleansed",
  "latitude", "longitude", "property_type", "room_type", "accommodates",
  "bathrooms", "bathrooms_text", "bedrooms", "beds", "amenities", "price",
  "minimum_nights", "maximum_nights", "minimum_minimum_nights",
  "maximum_minimum_nights", "minimum_maximum_nights",
  "maximum_maximum_nights", "minimum_nights_avg_ntm",
  "maximum_nights_avg_ntm", "has_availability", "availability_30",
  "availability_60", "availability_90", "availability_365",
  "number_of_reviews", "number_of_reviews_ltm", "number_of_reviews_l30d",
  "first_review", "last_review", "review_scores_rating",
  "review_scores_accuracy", "review_scores_cleanliness",
  "review_scores_checkin", "review_scores_communication",
  "review_scores_location", "review_scores_value", "license",
  "instant_bookable", "calculated_host_listings_count",
  "calculated_host_listings_count_entire_homes",
  "calculated_host_listings_count_private_rooms",
  "calculated_host_listings_count_shared_rooms"
FROM read_csv(
  '{{TWIN_LISTINGS}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false, parallel = false
);

CREATE OR REPLACE TABLE raw.calendar AS
SELECT 'chicago'::VARCHAR AS market_id, DATE '2026-06-24' AS snapshot_date,
  "listing_id" AS source_listing_id, "date", "available",
  "minimum_nights", "maximum_nights"
FROM read_csv(
  '{{CHICAGO_CALENDAR}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false, parallel = false
)
UNION ALL BY NAME
SELECT 'twin_cities_msa'::VARCHAR AS market_id, DATE '2026-06-27' AS snapshot_date,
  "listing_id" AS source_listing_id, "date", "available",
  "minimum_nights", "maximum_nights"
FROM read_csv(
  '{{TWIN_CALENDAR}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false, parallel = false
);

CREATE OR REPLACE TABLE raw.reviews AS
SELECT 'chicago'::VARCHAR AS market_id, DATE '2026-06-24' AS snapshot_date,
  "listing_id" AS source_listing_id, "date" AS review_date
FROM read_csv(
  '{{CHICAGO_REVIEWS}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false, parallel = false
)
UNION ALL BY NAME
SELECT 'twin_cities_msa'::VARCHAR AS market_id, DATE '2026-06-27' AS snapshot_date,
  "listing_id" AS source_listing_id, "date" AS review_date
FROM read_csv(
  '{{TWIN_REVIEWS}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false, parallel = false
);
