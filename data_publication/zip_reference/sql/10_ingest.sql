CREATE SCHEMA IF NOT EXISTS raw;

CREATE OR REPLACE TABLE raw.geography AS
SELECT * FROM read_csv('{{GEOGRAPHY}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);

CREATE OR REPLACE TABLE raw.population AS
SELECT * FROM read_csv('{{POPULATION}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);

CREATE OR REPLACE TABLE raw.median_age AS
SELECT * FROM read_csv('{{MEDIAN_AGE}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);

CREATE OR REPLACE TABLE raw.commute AS
SELECT * FROM read_csv('{{COMMUTE}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);

CREATE OR REPLACE TABLE raw.density AS
SELECT * FROM read_csv('{{DENSITY}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);

CREATE OR REPLACE TABLE raw.housing_age AS
SELECT * FROM read_csv('{{HOUSING_AGE}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);

CREATE OR REPLACE TABLE raw.ruca AS
SELECT * FROM read_csv('{{RUCA}}', header=true, all_varchar=true, sample_size=-1, strict_mode=true);
