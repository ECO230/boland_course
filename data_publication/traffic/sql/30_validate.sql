-- Every statement returns one row on success and raises an error on failure.

SELECT CASE WHEN COUNT(*) >= 800000
  THEN 'ok: crash row count'
  ELSE error('Traffic validation failed: unexpectedly few crashes') END
FROM curated.crashes;

SELECT CASE WHEN COUNT(*) = COUNT(crash_record_id)
  AND COUNT(*) = COUNT(DISTINCT crash_record_id)
  THEN 'ok: crash key'
  ELSE error('Traffic validation failed: crash key is null or duplicated') END
FROM curated.crashes;

SELECT CASE WHEN COUNT(*) = COUNT(crash_unit_id)
  AND COUNT(*) = COUNT(DISTINCT crash_unit_id)
  THEN 'ok: vehicle key'
  ELSE error('Traffic validation failed: vehicle key is null or duplicated') END
FROM curated.vehicles;

SELECT CASE WHEN COUNT(*) = COUNT(person_id)
  AND COUNT(*) = COUNT(DISTINCT person_id)
  THEN 'ok: person key'
  ELSE error('Traffic validation failed: person key is null or duplicated') END
FROM curated.people;

SELECT CASE WHEN COUNT(*) FILTER (WHERE c.crash_record_id IS NULL) = 0
  THEN 'ok: vehicle crash foreign key'
  ELSE error('Traffic validation failed: vehicle references missing crash') END
FROM curated.vehicles v LEFT JOIN curated.crashes c USING (crash_record_id);

SELECT CASE WHEN COUNT(*) FILTER (WHERE c.crash_record_id IS NULL) = 0
  THEN 'ok: person crash foreign key'
  ELSE error('Traffic validation failed: person references missing crash') END
FROM curated.people p LEFT JOIN curated.crashes c USING (crash_record_id);

SELECT CASE WHEN MIN(crash_datetime) >= TIMESTAMP '2018-01-01'
  AND MIN(crash_datetime) < TIMESTAMP '2018-01-02'
  AND MAX(crash_datetime) >= TIMESTAMP '2025-12-31'
  AND MAX(crash_datetime) < TIMESTAMP '2026-01-01'
  THEN 'ok: crash coverage'
  ELSE error('Traffic validation failed: coverage is not complete-year 2018-2025') END
FROM curated.crashes;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE latitude IS NOT NULL AND (
      latitude NOT BETWEEN 41.60 AND 42.05
      OR longitude NOT BETWEEN -87.95 AND -87.50
    )
  ) = 0
  THEN 'ok: coordinate bounds'
  ELSE error('Traffic validation failed: coordinate outside Chicago bounds') END
FROM curated.crashes;

SELECT CASE WHEN COUNT(*) FILTER (WHERE zip5 IS NOT NULL) >= COUNT(*) * 0.90
  THEN 'ok: ZIP key coverage'
  ELSE error('Traffic validation failed: ZIP key coverage below 90 percent') END
FROM curated.crashes;

SELECT CASE WHEN COUNT(*) = (SELECT COUNT(*) FROM curated.crashes)
  AND COUNT(*) = COUNT(DISTINCT crash_record_id)
  THEN 'ok: one analysis row per crash'
  ELSE error('Traffic validation failed: analysis grain changed') END
FROM curated.crash_analysis;

SELECT CASE WHEN COUNT(*) FILTER (WHERE weather_station_id IS NOT NULL) >= COUNT(*) * 0.90
  THEN 'ok: numerical weather match rate'
  ELSE error('Traffic validation failed: numerical weather match below 90 percent') END
FROM curated.crash_analysis;

SELECT CASE WHEN COUNT(*) FILTER (
    WHERE weather_time_difference_minutes > 90
      OR temperature_f NOT BETWEEN -40 AND 130
      OR relative_humidity_percent NOT BETWEEN 0 AND 100
      OR wind_speed_mph < 0
      OR visibility_miles < 0
  ) = 0
  THEN 'ok: numerical weather ranges'
  ELSE error('Traffic validation failed: invalid numerical weather value') END
FROM curated.crash_analysis;
