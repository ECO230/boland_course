SELECT CASE WHEN (SELECT count(*) FROM curated.zip_geography) = 42368
  THEN true ELSE error('Unexpected ZIP geography row count') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT zip5) FROM curated.zip_geography)
  THEN true ELSE error('Duplicate ZIP geography key') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.zip_geography
  WHERE NOT regexp_full_match(zip5, '[0-9]{5}') OR zip5 = '00000'
) THEN true ELSE error('Invalid ZIP geography key') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.zip_geography
  WHERE centroid_latitude IS NOT NULL AND NOT centroid_latitude BETWEEN -90 AND 90
) THEN true ELSE error('Invalid latitude') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.zip_geography
  WHERE centroid_longitude IS NOT NULL AND NOT centroid_longitude BETWEEN -180 AND 180
) THEN true ELSE error('Invalid longitude') END;

SELECT CASE WHEN (SELECT count(*) FROM curated.zip_context) = (SELECT count(*) FROM curated.zip_geography)
  THEN true ELSE error('Context is not one row per geography ZIP') END;
SELECT CASE WHEN (SELECT count(*) = count(DISTINCT zip5) FROM curated.zip_context)
  THEN true ELSE error('Duplicate ZIP context key') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.zip_context WHERE population_2023 IS NOT NULL) >= 30000
  THEN true ELSE error('Unexpectedly low 2023 population coverage') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.zip_context WHERE median_age_group_2023 IS NOT NULL) >= 30000
  THEN true ELSE error('Unexpectedly low median-age coverage') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.zip_context WHERE median_commute_time_category IS NOT NULL) >= 30000
  THEN true ELSE error('Unexpectedly low commute coverage') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.zip_context WHERE median_housing_vintage_category_2022 IS NOT NULL) >= 30000
  THEN true ELSE error('Unexpectedly low housing-vintage coverage') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.zip_context
  WHERE population_2023 < 0 OR population_2010 < 0 OR land_square_miles < 0
    OR population_density_per_square_mile_2010 < 0
) THEN true ELSE error('Negative population, area, or density') END;
SELECT CASE WHEN NOT EXISTS (
  SELECT 1 FROM curated.zip_context c
  LEFT JOIN curated.zip_geography g USING (zip5)
  WHERE g.zip5 IS NULL
) THEN true ELSE error('Context references a missing geography ZIP') END;
