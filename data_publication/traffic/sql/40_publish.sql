COPY (
  SELECT * FROM curated.crashes ORDER BY crash_datetime, crash_record_id
) TO '{{OUTPUT_CRASHES}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

COPY (
  SELECT * FROM curated.vehicles ORDER BY crash_datetime, crash_unit_id
) TO '{{OUTPUT_VEHICLES}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

COPY (
  SELECT * FROM curated.people ORDER BY crash_datetime, person_id
) TO '{{OUTPUT_PEOPLE}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);

COPY (
  SELECT * FROM curated.crash_analysis ORDER BY crash_datetime, crash_record_id
) TO '{{OUTPUT_ANALYSIS}}' (
  FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000
);
