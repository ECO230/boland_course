CREATE SCHEMA IF NOT EXISTS curated;

CREATE OR REPLACE MACRO clean_text(value) AS
  NULLIF(TRIM(value), '');

CREATE OR REPLACE MACRO yes_no(value) AS
  CASE
    WHEN UPPER(TRIM(value)) IN ('Y', 'YES') THEN TRUE
    WHEN UPPER(TRIM(value)) IN ('N', 'NO') THEN FALSE
    ELSE NULL
  END;

CREATE OR REPLACE MACRO clean_number(value) AS
  TRY_CAST(
    regexp_extract(COALESCE(value, ''), '[-+]?[0-9]+(?:\.[0-9]+)?', 0)
    AS DOUBLE
  );

CREATE OR REPLACE TABLE curated.crashes AS
WITH zip_lookup AS (
  SELECT clean_text("_feature_id") AS zip_region_id, clean_text("zip") AS zip5
  FROM raw.zip_regions
), typed AS (
  SELECT
    clean_text(c."crash_record_id") AS crash_record_id,
    yes_no(c."crash_date_est_i") AS crash_date_estimated,
    TRY_CAST(c."crash_date" AS TIMESTAMP) AS crash_datetime,
    TRY_CAST(c."posted_speed_limit" AS SMALLINT) AS posted_speed_limit_mph,
    clean_text(c."traffic_control_device") AS traffic_control_device,
    clean_text(c."device_condition") AS traffic_control_condition,
    clean_text(c."weather_condition") AS reported_weather_condition,
    clean_text(c."lighting_condition") AS reported_lighting_condition,
    clean_text(c."first_crash_type") AS first_crash_type,
    clean_text(c."trafficway_type") AS trafficway_type,
    TRY_CAST(c."lane_cnt" AS SMALLINT) AS lane_count,
    clean_text(c."alignment") AS roadway_alignment,
    clean_text(c."roadway_surface_cond") AS reported_road_surface_condition,
    clean_text(c."road_defect") AS reported_road_defect,
    clean_text(c."report_type") AS report_type,
    clean_text(c."crash_type") AS crash_type,
    yes_no(c."intersection_related_i") AS intersection_related,
    yes_no(c."private_property_i") AS outside_public_right_of_way,
    yes_no(c."hit_and_run_i") AS hit_and_run,
    clean_text(c."damage") AS estimated_damage,
    TRY_CAST(c."date_police_notified" AS TIMESTAMP) AS police_notified_datetime,
    clean_text(c."prim_contributory_cause") AS primary_contributory_cause,
    clean_text(c."sec_contributory_cause") AS secondary_contributory_cause,
    TRY_CAST(c."street_no" AS INTEGER) AS street_number,
    clean_text(c."street_direction") AS street_direction,
    clean_text(c."street_name") AS street_name,
    LPAD(clean_text(c."beat_of_occurrence"), 4, '0') AS police_beat,
    yes_no(c."dooring_i") AS dooring,
    yes_no(c."work_zone_i") AS work_zone,
    clean_text(c."work_zone_type") AS work_zone_type,
    yes_no(c."workers_present_i") AS workers_present,
    TRY_CAST(c."num_units" AS SMALLINT) AS unit_count,
    clean_text(c."most_severe_injury") AS most_severe_injury,
    TRY_CAST(c."injuries_total" AS SMALLINT) AS injuries_total,
    TRY_CAST(c."injuries_fatal" AS SMALLINT) AS injuries_fatal,
    TRY_CAST(c."injuries_incapacitating" AS SMALLINT) AS injuries_incapacitating,
    TRY_CAST(c."injuries_non_incapacitating" AS SMALLINT) AS injuries_non_incapacitating,
    TRY_CAST(c."injuries_reported_not_evident" AS SMALLINT) AS injuries_reported_not_evident,
    TRY_CAST(c."injuries_no_indication" AS SMALLINT) AS injuries_no_indication,
    TRY_CAST(c."injuries_unknown" AS SMALLINT) AS injuries_unknown,
    clean_text(c."idot_control_no") AS idot_control_number,
    NULLIF(TRY_CAST(c."latitude" AS DOUBLE), 0.0) AS latitude,
    NULLIF(TRY_CAST(c."longitude" AS DOUBLE), 0.0) AS longitude,
    clean_text(c.":@computed_region_rpca_8um6") AS zip_region_id
  FROM raw.crashes c
)
SELECT
  t.* EXCLUDE (zip_region_id),
  '17'::VARCHAR AS state_fips,
  '1714000'::VARCHAR AS city_geoid,
  z.zip5,
  t.zip_region_id
FROM typed t
LEFT JOIN zip_lookup z USING (zip_region_id);

CREATE OR REPLACE TABLE curated.vehicles AS
SELECT
  TRY_CAST("crash_unit_id" AS BIGINT) AS crash_unit_id,
  clean_text("crash_record_id") AS crash_record_id,
  TRY_CAST("crash_date" AS TIMESTAMP) AS crash_datetime,
  TRY_CAST("unit_no" AS SMALLINT) AS unit_number,
  clean_text("unit_type") AS unit_type,
  TRY_CAST("num_passengers" AS SMALLINT) AS passenger_count,
  clean_text("vehicle_id") AS vehicle_id,
  yes_no("cmrc_veh_i") AS commercial_vehicle,
  clean_text("make") AS vehicle_make,
  clean_text("model") AS vehicle_model,
  clean_text("lic_plate_state") AS license_plate_state,
  TRY_CAST("vehicle_year" AS SMALLINT) AS vehicle_year,
  clean_text("vehicle_defect") AS vehicle_defect,
  clean_text("vehicle_type") AS vehicle_type,
  clean_text("vehicle_use") AS vehicle_use,
  clean_text("travel_direction") AS travel_direction,
  clean_text("maneuver") AS maneuver,
  yes_no("towed_i") AS towed,
  yes_no("fire_i") AS fire,
  TRY_CAST("occupant_cnt" AS SMALLINT) AS occupant_count,
  yes_no("exceed_speed_limit_i") AS exceeded_speed_limit,
  clean_text("first_contact_point") AS first_contact_point,
  clean_text("gvwr") AS gross_vehicle_weight_rating,
  clean_text("vehicle_config") AS vehicle_configuration,
  clean_text("cargo_body_type") AS cargo_body_type,
  clean_text("load_type") AS load_type,
  yes_no("hazmat_present_i") AS hazardous_material_present
FROM raw.vehicles;

CREATE OR REPLACE TABLE curated.people AS
SELECT
  clean_text("person_id") AS person_id,
  clean_text("person_type") AS person_type,
  clean_text("crash_record_id") AS crash_record_id,
  clean_text("vehicle_id") AS vehicle_id,
  TRY_CAST("crash_date" AS TIMESTAMP) AS crash_datetime,
  clean_text("seat_no") AS seat_number,
  clean_text("sex") AS sex,
  TRY_CAST("age" AS SMALLINT) AS age,
  clean_text("safety_equipment") AS safety_equipment,
  clean_text("airbag_deployed") AS airbag_deployed,
  clean_text("ejection") AS ejection,
  clean_text("injury_classification") AS injury_classification,
  clean_text("driver_action") AS driver_action,
  clean_text("driver_vision") AS driver_vision,
  clean_text("physical_condition") AS physical_condition,
  clean_text("pedpedal_action") AS pedestrian_or_pedal_action,
  clean_text("pedpedal_visibility") AS pedestrian_or_pedal_visibility,
  clean_text("pedpedal_location") AS pedestrian_or_pedal_location,
  clean_text("bac_result") AS bac_result,
  TRY_CAST("bac_result_value" AS DOUBLE) AS bac_result_value,
  clean_text("cell_phone_use") AS cell_phone_use
FROM raw.people;

CREATE OR REPLACE TABLE curated.weather_hourly AS
WITH typed AS (
  SELECT
    clean_text("STATION") AS weather_station_id,
    clean_text("NAME") AS weather_station_name,
    TRY_CAST("DATE" AS TIMESTAMP) AS weather_observed_lstd,
    TRY_CAST("LATITUDE" AS DOUBLE) AS weather_station_latitude,
    TRY_CAST("LONGITUDE" AS DOUBLE) AS weather_station_longitude,
    clean_text("REPORT_TYPE") AS weather_report_type,
    clean_number("HourlyDryBulbTemperature") AS temperature_f,
    clean_number("HourlyDewPointTemperature") AS dew_point_f,
    clean_number("HourlyRelativeHumidity") AS relative_humidity_percent,
    CASE
      WHEN UPPER(TRIM(COALESCE("HourlyPrecipitation", ''))) = 'T' THEN 0.0
      ELSE clean_number("HourlyPrecipitation")
    END AS precipitation_inches,
    UPPER(TRIM(COALESCE("HourlyPrecipitation", ''))) = 'T' AS precipitation_trace,
    clean_number("HourlyVisibility") AS visibility_miles,
    TRY_CAST(clean_number("HourlyWindDirection") AS SMALLINT) AS wind_direction_degrees,
    clean_number("HourlyWindSpeed") AS wind_speed_mph,
    clean_number("HourlyWindGustSpeed") AS wind_gust_mph,
    clean_number("HourlyStationPressure") AS station_pressure_inches,
    clean_number("HourlySeaLevelPressure") AS sea_level_pressure_inches,
    clean_text("HourlyPresentWeatherType") AS observed_weather_type,
    clean_text("HourlySkyConditions") AS observed_sky_conditions
  FROM raw.noaa_lcd
  WHERE clean_text("REPORT_TYPE") IN ('FM-15', 'FM-16')
), ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (
      PARTITION BY weather_station_id, weather_observed_lstd
      ORDER BY
        (temperature_f IS NOT NULL)::INTEGER
        + (relative_humidity_percent IS NOT NULL)::INTEGER
        + (wind_speed_mph IS NOT NULL)::INTEGER DESC,
        weather_report_type
    ) AS observation_rank
  FROM typed
  WHERE weather_station_id IS NOT NULL AND weather_observed_lstd IS NOT NULL
)
SELECT * EXCLUDE (observation_rank)
FROM ranked
WHERE observation_rank = 1;

CREATE OR REPLACE TABLE curated.weather_stations AS
SELECT
  weather_station_id,
  ANY_VALUE(weather_station_name) AS weather_station_name,
  MEDIAN(weather_station_latitude) AS weather_station_latitude,
  MEDIAN(weather_station_longitude) AS weather_station_longitude
FROM curated.weather_hourly
GROUP BY weather_station_id;

CREATE OR REPLACE TEMP TABLE crash_station_choice AS
WITH station_candidates AS (
  SELECT
    c.crash_record_id,
    s.*,
    69.0 * SQRT(
      POW(c.latitude - s.weather_station_latitude, 2)
      + POW(
          (c.longitude - s.weather_station_longitude)
          * COS(RADIANS((c.latitude + s.weather_station_latitude) / 2.0)),
          2
        )
    ) AS weather_station_distance_miles,
    ROW_NUMBER() OVER (
      PARTITION BY c.crash_record_id
      ORDER BY
        POW(c.latitude - s.weather_station_latitude, 2)
        + POW(
            (c.longitude - s.weather_station_longitude)
            * COS(RADIANS((c.latitude + s.weather_station_latitude) / 2.0)),
            2
          )
    ) AS station_rank
  FROM curated.crashes c
  JOIN curated.weather_stations s
    ON c.latitude IS NOT NULL AND c.longitude IS NOT NULL
)
SELECT
  c.*,
  timezone(
    'Etc/GMT+6',
    timezone('America/Chicago', c.crash_datetime)
  ) AS crash_datetime_lstd,
  s.weather_station_id AS nearest_station_id,
  s.weather_station_distance_miles
FROM curated.crashes c
LEFT JOIN station_candidates s
  ON c.crash_record_id = s.crash_record_id AND s.station_rank = 1;

CREATE OR REPLACE TABLE curated.crash_analysis AS
WITH weather_candidates AS (
  SELECT
    c.crash_record_id,
    w.weather_station_id AS matched_weather_station_id,
    w.weather_observed_lstd,
    ROW_NUMBER() OVER (
      PARTITION BY c.crash_record_id
      ORDER BY
        (w.weather_observed_lstd IS NULL)::INTEGER,
        ABS(date_diff('second', c.crash_datetime_lstd, w.weather_observed_lstd)),
        (w.temperature_f IS NULL)::INTEGER,
        (w.relative_humidity_percent IS NULL)::INTEGER
    ) AS weather_rank
  FROM crash_station_choice c
  CROSS JOIN (VALUES (-1), (0), (1)) AS offsets(offset_hours)
  LEFT JOIN curated.weather_hourly w
    ON w.weather_station_id = c.nearest_station_id
    AND date_trunc('hour', w.weather_observed_lstd)
      = date_trunc('hour', c.crash_datetime_lstd)
        + offsets.offset_hours * INTERVAL 1 HOUR
    AND ABS(date_diff('second', c.crash_datetime_lstd, w.weather_observed_lstd)) <= 5400
), matched_weather AS (
  SELECT * EXCLUDE (weather_rank)
  FROM weather_candidates
  WHERE weather_rank = 1
)
SELECT
  c.* EXCLUDE (
    crash_datetime_lstd,
    nearest_station_id
  ),
  w.weather_station_id,
  w.weather_station_name,
  w.weather_observed_lstd,
  ABS(date_diff('second', c.crash_datetime_lstd, w.weather_observed_lstd)) / 60.0
    AS weather_time_difference_minutes,
  w.temperature_f,
  w.dew_point_f,
  w.relative_humidity_percent,
  w.precipitation_inches,
  w.precipitation_trace,
  w.visibility_miles,
  w.wind_direction_degrees,
  w.wind_speed_mph,
  w.wind_gust_mph,
  w.station_pressure_inches,
  w.sea_level_pressure_inches,
  w.observed_weather_type,
  w.observed_sky_conditions,
  TRY_CAST(strftime(crash_datetime, '%Y') AS SMALLINT) AS crash_year,
  TRY_CAST(strftime(crash_datetime, '%m') AS UTINYINT) AS crash_month,
  strftime(crash_datetime, '%b') AS crash_month_name,
  TRY_CAST(strftime(crash_datetime, '%u') AS UTINYINT) AS crash_weekday,
  strftime(crash_datetime, '%a') AS crash_weekday_name,
  TRY_CAST(strftime(crash_datetime, '%H') AS UTINYINT) AS crash_hour,
  CASE
    WHEN TRY_CAST(strftime(crash_datetime, '%H') AS INTEGER) BETWEEN 6 AND 9
      THEN 'Morning commute'
    WHEN TRY_CAST(strftime(crash_datetime, '%H') AS INTEGER) BETWEEN 15 AND 18
      THEN 'Evening commute'
    WHEN TRY_CAST(strftime(crash_datetime, '%H') AS INTEGER) BETWEEN 7 AND 17
      THEN 'Daytime'
    ELSE 'Night or early morning'
  END AS time_period
FROM crash_station_choice c
LEFT JOIN matched_weather m USING (crash_record_id)
LEFT JOIN curated.weather_hourly w
  ON w.weather_station_id = m.matched_weather_station_id
  AND w.weather_observed_lstd = m.weather_observed_lstd;
