CREATE SCHEMA IF NOT EXISTS curated;

CREATE OR REPLACE TABLE curated.zip_geography AS
WITH cleaned AS (
  SELECT
    trim(ZIPCODE) AS zip5,
    nullif(trim(ZIP_CITY_NAME), '') AS city_name,
    upper(nullif(trim(ZIP_STATE_CD), '')) AS state_abbreviation,
    nullif(trim(ZIP_STATE_DESC), '') AS state_name,
    CASE
      WHEN regexp_full_match(trim(ZIP_STATE_FIPS), '[0-9]{2}')
        THEN trim(ZIP_STATE_FIPS)
    END AS state_fips,
    CASE
      WHEN regexp_full_match(trim(ZIP_STATE_COUNTY_FIPS), '[0-9]{5}')
        AND trim(ZIP_STATE_COUNTY_FIPS) <> '00000'
        THEN trim(ZIP_STATE_COUNTY_FIPS)
      WHEN regexp_full_match(trim(ZIP_STATE_FIPS), '[0-9]{2}')
        AND regexp_full_match(trim(ZIP_COUNTY_FIPS), '[0-9]{3}')
        AND trim(ZIP_COUNTY_FIPS) <> '000'
        THEN trim(ZIP_STATE_FIPS) || trim(ZIP_COUNTY_FIPS)
    END AS county_fips,
    CASE WHEN trim(ZIP_COUNTY_DESC) NOT IN ('', 'UNK ZIP')
      THEN trim(ZIP_COUNTY_DESC) END AS county_name,
    CASE WHEN trim(ZIP_CENSUS_REGION) NOT IN ('', 'UNK CENSUS REG')
      THEN trim(ZIP_CENSUS_REGION) END AS census_region,
    CASE WHEN trim(ZIP_CENSUS_DIVISION) NOT IN ('', 'UNK CENSUS DIV')
      THEN trim(ZIP_CENSUS_DIVISION) END AS census_division,
    CASE
      WHEN try_cast(ZIP_CENTROID_LAT AS DOUBLE) BETWEEN -90 AND 90
        AND try_cast(ZIP_CENTROID_LAT AS DOUBLE) <> 0
        THEN try_cast(ZIP_CENTROID_LAT AS DOUBLE)
    END AS centroid_latitude,
    CASE
      WHEN try_cast(ZIP_CENTROID_LONG AS DOUBLE) BETWEEN -180 AND 180
        AND try_cast(ZIP_CENTROID_LONG AS DOUBLE) <> 0
        THEN try_cast(ZIP_CENTROID_LONG AS DOUBLE)
    END AS centroid_longitude,
    CASE WHEN regexp_full_match(trim(CBSA_CBSA_FIPS), '[0-9]{5}')
      AND trim(CBSA_CBSA_FIPS) <> '00000' THEN trim(CBSA_CBSA_FIPS) END AS cbsa_fips,
    CASE WHEN regexp_full_match(trim(CBSA_METRO_CD), '[0-9]{5}')
      AND trim(CBSA_METRO_CD) <> '00000' THEN trim(CBSA_METRO_CD) END AS metro_division_fips,
    CASE WHEN regexp_full_match(trim(CBSA_CSA_FIPS), '[0-9]{3}')
      AND trim(CBSA_CSA_FIPS) <> '000' THEN trim(CBSA_CSA_FIPS) END AS csa_fips,
    nullif(trim(CBSA_CBSA_DESC_CHAR), '') AS cbsa_name,
    nullif(trim(CBSA_CBSA_METRO_FLAG), '') AS cbsa_type,
    nullif(trim(CBSA_METRO_DIV_DESC), '') AS metro_division_name,
    nullif(trim(CBSA_CSA_DESC), '') AS csa_name,
    nullif(trim(CBSA_OUT_FLAG), '') AS central_outlying_status
  FROM raw.geography
  WHERE regexp_full_match(trim(ZIPCODE), '[0-9]{5}')
    AND trim(ZIPCODE) <> '00000'
)
SELECT * FROM cleaned;

CREATE OR REPLACE TABLE curated.zip_context AS
WITH population AS (
  SELECT
    lpad(trim(zipcode), 5, '0') AS zip5,
    try_cast(population_2023 AS BIGINT) AS population_2023
  FROM raw.population
), median_age AS (
  SELECT
    lpad(trim(Z5), 5, '0') AS zip5,
    nullif(trim(MEDIAN_AGE_GROUP), '') AS median_age_group_2023
  FROM raw.median_age
), commute AS (
  SELECT
    lpad(trim(ZIPCODE), 5, '0') AS zip5,
    nullif(trim(MED_HH_COMMUTE_CAT), '') AS median_commute_time_category
  FROM raw.commute
), density AS (
  SELECT
    lpad(trim("Zip/ZCTA"), 5, '0') AS zip5,
    try_cast("2010 Population" AS BIGINT) AS population_2010,
    try_cast("Land-Sq-Mi" AS DOUBLE) AS land_square_miles,
    try_cast("Density Per Sq Mile" AS DOUBLE) AS population_density_per_square_mile_2010
  FROM raw.density
), housing AS (
  SELECT
    lpad(trim(Z5), 5, '0') AS zip5,
    nullif(trim(MEDIAN_HOUSING_AGE), '') AS median_housing_vintage_category_2022
  FROM raw.housing_age
), ruca AS (
  SELECT
    lpad(trim(ZIP_CODE), 5, '0') AS zip5,
    upper(nullif(trim(STATE), '')) AS ruca_state_abbreviation_2010,
    nullif(trim(ZIP_TYPE), '') AS ruca_zip_type_2010,
    try_cast(RUCA1 AS DOUBLE) AS ruca_primary_code_2010,
    try_cast(RUCA2 AS DOUBLE) AS ruca_secondary_code_2010
  FROM raw.ruca
)
SELECT
  g.zip5,
  p.population_2023,
  a.median_age_group_2023,
  c.median_commute_time_category,
  d.population_2010,
  d.land_square_miles,
  d.population_density_per_square_mile_2010,
  h.median_housing_vintage_category_2022,
  r.ruca_state_abbreviation_2010,
  r.ruca_zip_type_2010,
  r.ruca_primary_code_2010,
  r.ruca_secondary_code_2010
FROM curated.zip_geography g
LEFT JOIN population p USING (zip5)
LEFT JOIN median_age a USING (zip5)
LEFT JOIN commute c USING (zip5)
LEFT JOIN density d USING (zip5)
LEFT JOIN housing h USING (zip5)
LEFT JOIN ruca r USING (zip5);
