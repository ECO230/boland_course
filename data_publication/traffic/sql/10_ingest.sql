CREATE SCHEMA IF NOT EXISTS raw;

CREATE OR REPLACE TABLE raw.crashes AS
SELECT * FROM read_csv(
  '{{CRASHES_CSV}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false
);

CREATE OR REPLACE TABLE raw.vehicles AS
SELECT * FROM read_csv(
  '{{VEHICLES_CSV}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false
);

CREATE OR REPLACE TABLE raw.people AS
SELECT * FROM read_csv(
  '{{PEOPLE_CSV}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false
);

CREATE OR REPLACE TABLE raw.zip_regions AS
SELECT * FROM read_csv(
  '{{ZIP_REGIONS_CSV}}', header = true, all_varchar = true,
  sample_size = -1, strict_mode = true, ignore_errors = false
);

CREATE OR REPLACE TABLE raw.noaa_lcd AS
SELECT * FROM read_csv(
  '{{WEATHER_GLOB}}', header = true, all_varchar = true,
  sample_size = -1, union_by_name = true, strict_mode = false,
  ignore_errors = false, parallel = false
);
