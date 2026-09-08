-- Every statement returns one row on success and raises an error on failure.

SELECT CASE WHEN
  COUNT(*) = 14009
  AND COUNT(*) FILTER (WHERE market_id = 'chicago') = 8660
  AND COUNT(*) FILTER (WHERE market_id = 'twin_cities_msa') = 5349
  THEN 'ok: pinned listing row counts'
  ELSE error('Airbnb validation failed: listing counts differ from pinned snapshots')
END
FROM curated.listings;

SELECT CASE WHEN COUNT(*) = COUNT(listing_key)
  AND COUNT(*) = COUNT(DISTINCT listing_key)
  THEN 'ok: listing key'
  ELSE error('Airbnb validation failed: listing key is null or duplicated') END
FROM curated.listings;

SELECT CASE WHEN COUNT(*) = COUNT(DISTINCT (listing_key, calendar_date))
  AND COUNT(*) >= 5000000
  THEN 'ok: calendar grain and row count'
  ELSE error('Airbnb validation failed: calendar key duplicates or unexpectedly few rows') END
FROM curated.calendar;

SELECT CASE WHEN COUNT(*) = COUNT(DISTINCT (listing_key, review_month))
  AND COUNT(*) > 0
  THEN 'ok: monthly review grain'
  ELSE error('Airbnb validation failed: monthly review key duplicates or no rows') END
FROM curated.reviews_monthly;

SELECT CASE WHEN COUNT(*) = (SELECT COUNT(*) FROM curated.listings)
  AND COUNT(*) = COUNT(DISTINCT listing_key)
  THEN 'ok: one analysis row per listing'
  ELSE error('Airbnb validation failed: analysis grain changed') END
FROM curated.listing_analysis;

SELECT CASE WHEN
  COUNT(*) FILTER (WHERE k.listing_key IS NULL) = 17520
  AND COUNT(DISTINCT (r.market_id, r.source_listing_id)) FILTER (
    WHERE k.listing_key IS NULL
  ) = 48
  AND COUNT(*) FILTER (
    WHERE k.listing_key IS NULL AND r.market_id = 'chicago'
  ) = 16060
  AND COUNT(*) FILTER (
    WHERE k.listing_key IS NULL AND r.market_id = 'twin_cities_msa'
  ) = 1460
  THEN 'ok: documented source calendar exclusions'
  ELSE error('Airbnb validation failed: source calendar exclusions changed') END
FROM raw.calendar r
LEFT JOIN curated.listing_key_map k
  ON r.market_id = k.market_id
  AND r.snapshot_date = k.snapshot_date
  AND clean_text(r.source_listing_id) = k.source_listing_id;

SELECT CASE WHEN
  COUNT(*) FILTER (WHERE k.listing_key IS NULL) = 6031
  AND COUNT(DISTINCT (r.market_id, r.source_listing_id)) FILTER (
    WHERE k.listing_key IS NULL
  ) = 46
  AND COUNT(*) FILTER (
    WHERE k.listing_key IS NULL AND r.market_id = 'chicago'
  ) = 5950
  AND COUNT(*) FILTER (
    WHERE k.listing_key IS NULL AND r.market_id = 'twin_cities_msa'
  ) = 81
  THEN 'ok: documented source review exclusions'
  ELSE error('Airbnb validation failed: source review exclusions changed') END
FROM raw.reviews r
LEFT JOIN curated.listing_key_map k
  ON r.market_id = k.market_id
  AND r.snapshot_date = k.snapshot_date
  AND clean_text(r.source_listing_id) = k.source_listing_id;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE state_fips IS NULL OR county_fips IS NULL OR grid_0_01deg IS NULL
      OR local_area_name IS NULL
  ) = 0
  THEN 'ok: geographic key coverage'
  ELSE error('Airbnb validation failed: geographic key is missing') END
FROM curated.listings;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE latitude IS NULL OR longitude IS NULL
      OR (market_id = 'chicago' AND (
        latitude NOT BETWEEN 41.5 AND 42.2 OR longitude NOT BETWEEN -88.2 AND -87.3
      ))
      OR (market_id = 'twin_cities_msa' AND (
        latitude NOT BETWEEN 44.0 AND 46.5 OR longitude NOT BETWEEN -94.8 AND -92.0
      ))
  ) = 0
  THEN 'ok: source coordinate bounds'
  ELSE error('Airbnb validation failed: missing or out-of-market coordinate') END
FROM curated.listings;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE availability_30 NOT BETWEEN 0 AND 30
      OR availability_60 NOT BETWEEN 0 AND 60
      OR availability_90 NOT BETWEEN 0 AND 90
      OR availability_365 NOT BETWEEN 0 AND 365
      OR accommodates < 0 OR bathrooms < 0 OR bedrooms < 0 OR beds < 0
      OR snapshot_price_usd < 0
      OR review_score_rating NOT BETWEEN 0 AND 5
      OR review_score_accuracy NOT BETWEEN 0 AND 5
      OR review_score_cleanliness NOT BETWEEN 0 AND 5
      OR review_score_checkin NOT BETWEEN 0 AND 5
      OR review_score_communication NOT BETWEEN 0 AND 5
      OR review_score_location NOT BETWEEN 0 AND 5
      OR review_score_value NOT BETWEEN 0 AND 5
  ) = 0
  AND COUNT(*) FILTER (WHERE snapshot_price_usd IS NOT NULL) >= COUNT(*) * 0.85
  THEN 'ok: listing value ranges and snapshot-price coverage'
  ELSE error('Airbnb validation failed: invalid listing value or low snapshot-price coverage') END
FROM curated.listings;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE calendar_date IS NULL
      OR calendar_date NOT BETWEEN DATE '2026-06-01' AND DATE '2027-08-01'
      OR minimum_nights < 0 OR maximum_nights < 0
  ) = 0
  THEN 'ok: calendar value ranges'
  ELSE error('Airbnb validation failed: invalid calendar value') END
FROM curated.calendar;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE review_month < DATE '2008-01-01' OR review_month > DATE '2026-08-01'
      OR review_count <= 0
  ) = 0
  THEN 'ok: monthly review value ranges'
  ELSE error('Airbnb validation failed: invalid monthly review value') END
FROM curated.reviews_monthly;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE future_listed_availability_rate NOT BETWEEN 0 AND 1
      OR weekday_listed_availability_rate NOT BETWEEN 0 AND 1
      OR weekend_listed_availability_rate NOT BETWEEN 0 AND 1
      OR listed_availability_rate_30d NOT BETWEEN 0 AND 1
      OR listed_availability_rate_90d NOT BETWEEN 0 AND 1
      OR listed_availability_rate_365d NOT BETWEEN 0 AND 1
      OR snapshot_price_per_guest_usd < 0
      OR snapshot_price_per_bedroom_usd < 0
      OR market_room_type_median_price_usd < 0
      OR minimum_calendar_minimum_nights < 0
      OR maximum_calendar_minimum_nights < 0
      OR review_count_30d < 0 OR review_count_90d < 0 OR review_count_365d < 0
  ) = 0
  THEN 'ok: analysis value ranges'
  ELSE error('Airbnb validation failed: invalid analysis value') END
FROM curated.listing_analysis;

SELECT CASE WHEN COUNT(*) FILTER (WHERE future_calendar_days IS NOT NULL)
    >= COUNT(*) * 0.99
  THEN 'ok: analysis calendar coverage'
  ELSE error('Airbnb validation failed: fewer than 99 percent of listings have calendars') END
FROM curated.listing_analysis;

SELECT CASE WHEN COUNT(*) = 0
  THEN 'ok: unavailable calendar-price claims excluded'
  ELSE error('Airbnb validation failed: calendar or future-price field was curated') END
FROM information_schema.columns
WHERE table_schema = 'curated'
  AND (
    (table_name = 'calendar' AND LOWER(column_name) IN ('price_usd', 'adjusted_price_usd'))
    OR (table_name = 'listing_analysis' AND LOWER(column_name) LIKE '%future%price%')
    OR (table_name = 'listing_analysis' AND LOWER(column_name) LIKE '%weekend%price%')
    OR (table_name = 'listing_analysis' AND LOWER(column_name) LIKE '%weekday%price%')
  );

SELECT CASE WHEN COUNT(*) = 0
  THEN 'ok: raw identity and free-text fields excluded'
  ELSE error('Airbnb validation failed: forbidden identity or free-text column was curated') END
FROM information_schema.columns
WHERE table_schema = 'curated'
  AND table_name IN ('listings', 'calendar', 'reviews_monthly', 'listing_analysis')
  AND LOWER(column_name) IN (
    'source_listing_id', 'listing_id', 'host_id', 'host_name', 'host_url',
    'host_about', 'host_location', 'reviewer_id', 'reviewer_name', 'comments',
    'name', 'description', 'neighborhood_overview', 'listing_url', 'picture_url',
    'license'
  );
