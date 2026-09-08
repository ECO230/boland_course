CREATE SCHEMA IF NOT EXISTS curated;

CREATE OR REPLACE MACRO clean_text(value) AS NULLIF(TRIM(value), '');

CREATE OR REPLACE MACRO tf_boolean(value) AS
  CASE
    WHEN LOWER(TRIM(value)) IN ('t', 'true', '1', 'yes', 'y') THEN TRUE
    WHEN LOWER(TRIM(value)) IN ('f', 'false', '0', 'no', 'n') THEN FALSE
    ELSE NULL
  END;

CREATE OR REPLACE MACRO money_usd(value) AS
  TRY_CAST(REPLACE(REPLACE(TRIM(value), '$', ''), ',', '') AS DECIMAL(12, 2));

CREATE OR REPLACE TABLE curated.listing_key_map AS
WITH unique_listings AS (
  SELECT DISTINCT market_id, snapshot_date, clean_text(source_listing_id) AS source_listing_id
  FROM raw.listings
  WHERE clean_text(source_listing_id) IS NOT NULL
), numbered AS (
  SELECT *, ROW_NUMBER() OVER (
    ORDER BY market_id, TRY_CAST(source_listing_id AS UBIGINT), source_listing_id
  ) AS release_listing_number
  FROM unique_listings
)
SELECT
  printf('airbnb_%07d', release_listing_number) AS listing_key,
  market_id,
  snapshot_date,
  source_listing_id
FROM numbered;

CREATE OR REPLACE TABLE curated.county_lookup AS
SELECT * FROM (VALUES
  ('chicago', 'Cook', '17', '17031'),
  ('twin_cities_msa', 'Anoka', '27', '27003'),
  ('twin_cities_msa', 'Carver', '27', '27019'),
  ('twin_cities_msa', 'Chisago', '27', '27025'),
  ('twin_cities_msa', 'Dakota', '27', '27037'),
  ('twin_cities_msa', 'Hennepin', '27', '27053'),
  ('twin_cities_msa', 'Isanti', '27', '27059'),
  ('twin_cities_msa', 'Le Sueur', '27', '27079'),
  ('twin_cities_msa', 'Mille Lacs', '27', '27095'),
  ('twin_cities_msa', 'Ramsey', '27', '27123'),
  ('twin_cities_msa', 'Scott', '27', '27139'),
  ('twin_cities_msa', 'Sherburne', '27', '27141'),
  ('twin_cities_msa', 'Sibley', '27', '27143'),
  ('twin_cities_msa', 'Washington', '27', '27163'),
  ('twin_cities_msa', 'Wright', '27', '27171'),
  ('twin_cities_msa', 'Pierce', '55', '55093'),
  ('twin_cities_msa', 'St. Croix', '55', '55109')
) AS counties(market_id, county_name, state_fips, county_fips);

CREATE OR REPLACE TABLE curated.listings AS
WITH typed AS (
  SELECT
    k.listing_key,
    r.market_id,
    CASE r.market_id
      WHEN 'chicago' THEN 'Chicago'
      WHEN 'twin_cities_msa' THEN 'Twin Cities MSA'
    END AS market_name,
    r.snapshot_date,
    TRY_CAST(r.last_scraped AS DATE) AS listing_scrape_date,
    clean_text(r.neighbourhood_cleansed) AS local_area_name,
    CASE r.market_id
      WHEN 'chicago' THEN 'Chicago community area'
      WHEN 'twin_cities_msa' THEN 'County'
    END AS local_area_type,
    TRY_CAST(r.latitude AS DOUBLE) AS latitude,
    TRY_CAST(r.longitude AS DOUBLE) AS longitude,
    clean_text(r.property_type) AS property_type,
    clean_text(r.room_type) AS room_type,
    TRY_CAST(r.accommodates AS SMALLINT) AS accommodates,
    COALESCE(
      TRY_CAST(r.bathrooms AS DOUBLE),
      TRY_CAST(regexp_extract(COALESCE(r.bathrooms_text, ''), '[0-9]+(?:\\.[0-9]+)?', 0) AS DOUBLE)
    ) AS bathrooms,
    TRY_CAST(r.bedrooms AS DOUBLE) AS bedrooms,
    TRY_CAST(r.beds AS DOUBLE) AS beds,
    TRY_CAST(json_array_length(TRY_CAST(r.amenities AS JSON)) AS SMALLINT) AS amenity_count,
    money_usd(r.price) AS snapshot_price_usd,
    TRY_CAST(r.minimum_nights AS INTEGER) AS minimum_nights,
    TRY_CAST(r.maximum_nights AS INTEGER) AS maximum_nights,
    TRY_CAST(r.minimum_minimum_nights AS INTEGER) AS observed_minimum_nights_low,
    TRY_CAST(r.maximum_minimum_nights AS INTEGER) AS observed_minimum_nights_high,
    TRY_CAST(r.minimum_maximum_nights AS INTEGER) AS observed_maximum_nights_low,
    TRY_CAST(r.maximum_maximum_nights AS INTEGER) AS observed_maximum_nights_high,
    TRY_CAST(r.minimum_nights_avg_ntm AS DOUBLE) AS average_minimum_nights,
    TRY_CAST(r.maximum_nights_avg_ntm AS DOUBLE) AS average_maximum_nights,
    tf_boolean(r.has_availability) AS has_calendar_availability,
    TRY_CAST(r.availability_30 AS SMALLINT) AS availability_30,
    TRY_CAST(r.availability_60 AS SMALLINT) AS availability_60,
    TRY_CAST(r.availability_90 AS SMALLINT) AS availability_90,
    TRY_CAST(r.availability_365 AS SMALLINT) AS availability_365,
    TRY_CAST(r.number_of_reviews AS INTEGER) AS number_of_reviews,
    TRY_CAST(r.number_of_reviews_ltm AS INTEGER) AS number_of_reviews_ltm,
    TRY_CAST(r.number_of_reviews_l30d AS INTEGER) AS number_of_reviews_l30d,
    TRY_CAST(r.first_review AS DATE) AS first_review_date,
    TRY_CAST(r.last_review AS DATE) AS last_review_date,
    TRY_CAST(r.review_scores_rating AS DOUBLE) AS review_score_rating,
    TRY_CAST(r.review_scores_accuracy AS DOUBLE) AS review_score_accuracy,
    TRY_CAST(r.review_scores_cleanliness AS DOUBLE) AS review_score_cleanliness,
    TRY_CAST(r.review_scores_checkin AS DOUBLE) AS review_score_checkin,
    TRY_CAST(r.review_scores_communication AS DOUBLE) AS review_score_communication,
    TRY_CAST(r.review_scores_location AS DOUBLE) AS review_score_location,
    TRY_CAST(r.review_scores_value AS DOUBLE) AS review_score_value,
    tf_boolean(r.instant_bookable) AS instant_bookable,
    TRY_CAST(r.calculated_host_listings_count AS INTEGER) AS operator_listing_count,
    TRY_CAST(r.calculated_host_listings_count_entire_homes AS INTEGER)
      AS operator_entire_home_count,
    TRY_CAST(r.calculated_host_listings_count_private_rooms AS INTEGER)
      AS operator_private_room_count,
    TRY_CAST(r.calculated_host_listings_count_shared_rooms AS INTEGER)
      AS operator_shared_room_count,
    clean_text(r.license) IS NOT NULL AS license_value_reported
  FROM raw.listings r
  JOIN curated.listing_key_map k
    ON r.market_id = k.market_id
    AND r.snapshot_date = k.snapshot_date
    AND clean_text(r.source_listing_id) = k.source_listing_id
), geographic AS (
  SELECT
    t.*,
    CASE WHEN t.market_id = 'chicago' THEN 'Cook' ELSE t.local_area_name END
      AS county_name,
    CONCAT(
      'wgs84_001_',
      CAST(FLOOR((t.latitude + 90.0) * 100.0) AS INTEGER), '_',
      CAST(FLOOR((t.longitude + 180.0) * 100.0) AS INTEGER)
    ) AS grid_0_01deg
  FROM typed t
)
SELECT
  g.listing_key, g.market_id, g.market_name, g.snapshot_date,
  g.listing_scrape_date, c.state_fips, c.county_fips, g.county_name,
  g.local_area_type, g.local_area_name, g.grid_0_01deg,
  g.latitude, g.longitude,
  g.* EXCLUDE (
    listing_key, market_id, market_name, snapshot_date, listing_scrape_date,
    local_area_name, local_area_type, latitude, longitude, county_name,
    grid_0_01deg
  )
FROM geographic g
LEFT JOIN curated.county_lookup c
  ON g.market_id = c.market_id AND g.county_name = c.county_name;

CREATE OR REPLACE TABLE curated.calendar AS
SELECT
  k.listing_key,
  r.market_id,
  CASE r.market_id
    WHEN 'chicago' THEN 'Chicago'
    WHEN 'twin_cities_msa' THEN 'Twin Cities MSA'
  END AS market_name,
  r.snapshot_date,
  TRY_CAST(r.date AS DATE) AS calendar_date,
  TRY_CAST(strftime(TRY_CAST(r.date AS DATE), '%u') AS UTINYINT) AS weekday_number,
  strftime(TRY_CAST(r.date AS DATE), '%A') AS weekday_name,
  TRY_CAST(strftime(TRY_CAST(r.date AS DATE), '%u') AS INTEGER) IN (6, 7)
    AS is_weekend,
  tf_boolean(r.available) AS calendar_available,
  TRY_CAST(r.minimum_nights AS INTEGER) AS minimum_nights,
  TRY_CAST(r.maximum_nights AS INTEGER) AS maximum_nights
FROM raw.calendar r
JOIN curated.listing_key_map k
  ON r.market_id = k.market_id
  AND r.snapshot_date = k.snapshot_date
  AND clean_text(r.source_listing_id) = k.source_listing_id;

CREATE OR REPLACE TABLE curated.reviews_monthly AS
SELECT
  k.listing_key,
  r.market_id,
  CASE r.market_id
    WHEN 'chicago' THEN 'Chicago'
    WHEN 'twin_cities_msa' THEN 'Twin Cities MSA'
  END AS market_name,
  r.snapshot_date,
  TRY_CAST(date_trunc('month', TRY_CAST(r.review_date AS DATE)) AS DATE) AS review_month,
  COUNT(*)::INTEGER AS review_count
FROM raw.reviews r
JOIN curated.listing_key_map k
  ON r.market_id = k.market_id
  AND r.snapshot_date = k.snapshot_date
  AND clean_text(r.source_listing_id) = k.source_listing_id
WHERE TRY_CAST(r.review_date AS DATE) IS NOT NULL
GROUP BY ALL;

CREATE OR REPLACE TABLE curated.listing_analysis AS
WITH calendar_summary AS (
  SELECT
    listing_key,
    MIN(calendar_date) AS future_calendar_start_date,
    MAX(calendar_date) AS future_calendar_end_date,
    COUNT(*)::INTEGER AS future_calendar_days,
    COUNT(*) FILTER (WHERE calendar_available)::INTEGER AS future_days_listed_available,
    AVG(calendar_available::INTEGER) AS future_listed_availability_rate,
    AVG(calendar_available::INTEGER) FILTER (WHERE NOT is_weekend)
      AS weekday_listed_availability_rate,
    AVG(calendar_available::INTEGER) FILTER (WHERE is_weekend)
      AS weekend_listed_availability_rate,
    AVG(calendar_available::INTEGER) FILTER (
      WHERE date_diff('day', snapshot_date, calendar_date) BETWEEN 0 AND 29
    ) AS listed_availability_rate_30d,
    AVG(calendar_available::INTEGER) FILTER (
      WHERE date_diff('day', snapshot_date, calendar_date) BETWEEN 0 AND 89
    ) AS listed_availability_rate_90d,
    AVG(calendar_available::INTEGER) FILTER (
      WHERE date_diff('day', snapshot_date, calendar_date) BETWEEN 0 AND 364
    ) AS listed_availability_rate_365d,
    MIN(minimum_nights) AS minimum_calendar_minimum_nights,
    MEDIAN(minimum_nights) AS median_calendar_minimum_nights,
    MAX(minimum_nights) AS maximum_calendar_minimum_nights,
    COUNT(DISTINCT minimum_nights)::INTEGER AS distinct_minimum_night_settings
  FROM curated.calendar
  GROUP BY listing_key
), price_benchmarks AS (
  SELECT
    market_id,
    room_type,
    MEDIAN(snapshot_price_usd) AS market_room_type_median_price_usd,
    COUNT(snapshot_price_usd)::INTEGER AS market_room_type_priced_listings
  FROM curated.listings
  GROUP BY market_id, room_type
), review_summary AS (
  SELECT
    k.listing_key,
    COUNT(*)::INTEGER AS source_review_count,
    COUNT(*) FILTER (
      WHERE TRY_CAST(r.review_date AS DATE) > l.listing_scrape_date - INTERVAL 30 DAY
        AND TRY_CAST(r.review_date AS DATE) <= l.listing_scrape_date
    )::INTEGER AS review_count_30d,
    COUNT(*) FILTER (
      WHERE TRY_CAST(r.review_date AS DATE) > l.listing_scrape_date - INTERVAL 90 DAY
        AND TRY_CAST(r.review_date AS DATE) <= l.listing_scrape_date
    )::INTEGER AS review_count_90d,
    COUNT(*) FILTER (
      WHERE TRY_CAST(r.review_date AS DATE) > l.listing_scrape_date - INTERVAL 365 DAY
        AND TRY_CAST(r.review_date AS DATE) <= l.listing_scrape_date
    )::INTEGER AS review_count_365d,
    MIN(TRY_CAST(r.review_date AS DATE)) AS source_first_review_date,
    MAX(TRY_CAST(r.review_date AS DATE)) AS source_last_review_date
  FROM raw.reviews r
  JOIN curated.listing_key_map k
    ON r.market_id = k.market_id
    AND r.snapshot_date = k.snapshot_date
    AND clean_text(r.source_listing_id) = k.source_listing_id
  JOIN curated.listings l USING (listing_key)
  GROUP BY k.listing_key
)
SELECT
  l.*,
  CASE WHEN l.accommodates > 0
    THEN l.snapshot_price_usd / l.accommodates
  END AS snapshot_price_per_guest_usd,
  CASE WHEN l.bedrooms > 0
    THEN l.snapshot_price_usd / l.bedrooms
  END AS snapshot_price_per_bedroom_usd,
  p.market_room_type_median_price_usd,
  p.market_room_type_priced_listings,
  l.snapshot_price_usd - p.market_room_type_median_price_usd
    AS snapshot_price_vs_market_room_type_median_usd,
  CASE WHEN p.market_room_type_median_price_usd > 0
    THEN l.snapshot_price_usd / p.market_room_type_median_price_usd - 1
  END AS snapshot_price_vs_market_room_type_median_rate,
  c.future_calendar_start_date,
  c.future_calendar_end_date,
  c.future_calendar_days,
  c.future_days_listed_available,
  c.future_listed_availability_rate,
  c.weekday_listed_availability_rate,
  c.weekend_listed_availability_rate,
  c.listed_availability_rate_30d,
  c.listed_availability_rate_90d,
  c.listed_availability_rate_365d,
  c.minimum_calendar_minimum_nights,
  c.median_calendar_minimum_nights,
  c.maximum_calendar_minimum_nights,
  c.distinct_minimum_night_settings,
  COALESCE(r.source_review_count, 0) AS source_review_count,
  COALESCE(r.review_count_30d, 0) AS review_count_30d,
  COALESCE(r.review_count_90d, 0) AS review_count_90d,
  COALESCE(r.review_count_365d, 0) AS review_count_365d,
  r.source_first_review_date,
  r.source_last_review_date
FROM curated.listings l
LEFT JOIN price_benchmarks p USING (market_id, room_type)
LEFT JOIN calendar_summary c USING (listing_key)
LEFT JOIN review_summary r USING (listing_key);
