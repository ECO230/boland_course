COPY (
  SELECT * FROM curated.listings ORDER BY market_id, listing_key
) TO '{{OUTPUT_LISTINGS}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

COPY (
  SELECT * FROM curated.calendar ORDER BY market_id, listing_key, calendar_date
) TO '{{OUTPUT_CALENDAR}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

COPY (
  SELECT * FROM curated.reviews_monthly ORDER BY market_id, listing_key, review_month
) TO '{{OUTPUT_REVIEWS_MONTHLY}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

COPY (
  SELECT * FROM curated.listing_analysis ORDER BY market_id, listing_key
) TO '{{OUTPUT_LISTING_ANALYSIS}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

