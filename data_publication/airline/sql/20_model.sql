CREATE SCHEMA IF NOT EXISTS curated;

CREATE OR REPLACE TABLE curated.flights AS
WITH typed AS (
  SELECT
    try_cast("FlightDate" AS DATE) AS flight_date,
    try_cast("Quarter" AS UTINYINT) AS quarter,
    try_cast("Month" AS UTINYINT) AS month_number,
    try_cast("DayofMonth" AS UTINYINT) AS day_of_month,
    try_cast("DayOfWeek" AS UTINYINT) AS day_of_week,
    upper(nullif(trim("IATA_CODE_Marketing_Airline"), '')) AS marketing_carrier_code,
    upper(nullif(trim("IATA_CODE_Operating_Airline"), '')) AS operating_carrier_code,
    try_cast("Flight_Number_Marketing_Airline" AS INTEGER) AS marketing_flight_number,
    CASE WHEN nullif(trim("Tail_Number"), '') IS NULL THEN NULL
      ELSE 'air_' || substr(sha256('bts-tail:' || trim("Tail_Number")), 1, 20) END AS aircraft_key,
    try_cast("OriginAirportID" AS INTEGER) AS origin_airport_id,
    upper(nullif(trim("Origin"), '')) AS origin_airport_code,
    nullif(trim("OriginCityName"), '') AS origin_city_name,
    upper(nullif(trim("OriginState"), '')) AS origin_state,
    try_cast("DestAirportID" AS INTEGER) AS destination_airport_id,
    upper(nullif(trim("Dest"), '')) AS destination_airport_code,
    nullif(trim("DestCityName"), '') AS destination_city_name,
    upper(nullif(trim("DestState"), '')) AS destination_state,
    try_cast("CRSDepTime" AS SMALLINT) AS scheduled_departure_hhmm,
    try_cast("DepTime" AS SMALLINT) AS actual_departure_hhmm,
    try_cast("DepDelay" AS DOUBLE) AS departure_delay_minutes,
    try_cast("DepDelayMinutes" AS DOUBLE) AS departure_delay_positive_minutes,
    try_cast("DepDel15" AS DOUBLE) = 1 AS departure_delayed_15,
    try_cast("CRSArrTime" AS SMALLINT) AS scheduled_arrival_hhmm,
    try_cast("ArrTime" AS SMALLINT) AS actual_arrival_hhmm,
    try_cast("ArrDelay" AS DOUBLE) AS arrival_delay_minutes,
    try_cast("ArrDelayMinutes" AS DOUBLE) AS arrival_delay_positive_minutes,
    try_cast("ArrDel15" AS DOUBLE) = 1 AS arrival_delayed_15,
    try_cast("Cancelled" AS DOUBLE) = 1 AS cancelled,
    upper(nullif(trim("CancellationCode"), '')) AS cancellation_code,
    try_cast("Diverted" AS DOUBLE) = 1 AS diverted,
    try_cast("CRSElapsedTime" AS DOUBLE) AS scheduled_elapsed_minutes,
    try_cast("ActualElapsedTime" AS DOUBLE) AS actual_elapsed_minutes,
    try_cast("AirTime" AS DOUBLE) AS air_time_minutes,
    try_cast("Distance" AS DOUBLE) AS distance_miles,
    try_cast("CarrierDelay" AS DOUBLE) AS carrier_delay_minutes,
    try_cast("WeatherDelay" AS DOUBLE) AS weather_delay_minutes,
    try_cast("NASDelay" AS DOUBLE) AS national_aviation_system_delay_minutes,
    try_cast("SecurityDelay" AS DOUBLE) AS security_delay_minutes,
    try_cast("LateAircraftDelay" AS DOUBLE) AS late_aircraft_delay_minutes
  FROM raw.flights
), numbered AS (
  SELECT *, row_number() OVER (
    PARTITION BY flight_date, marketing_carrier_code, marketing_flight_number,
      origin_airport_code, destination_airport_code, scheduled_departure_hhmm
    ORDER BY operating_carrier_code, aircraft_key NULLS LAST
  )::USMALLINT AS source_duplicate_sequence
  FROM typed
)
SELECT
  'flt_' || substr(sha256(concat_ws(':', flight_date::VARCHAR,
    marketing_carrier_code, marketing_flight_number::VARCHAR,
    origin_airport_code, destination_airport_code,
    scheduled_departure_hhmm::VARCHAR, source_duplicate_sequence::VARCHAR)), 1, 28) AS flight_key,
  *
FROM numbered;

CREATE OR REPLACE TABLE curated.airports AS
WITH represented AS (
  SELECT origin_airport_id AS airport_id, origin_airport_code AS airport_code,
    origin_city_name AS city_name, origin_state AS state_code FROM curated.flights
  UNION ALL
  SELECT destination_airport_id, destination_airport_code,
    destination_city_name, destination_state FROM curated.flights
)
SELECT airport_id, any_value(airport_code) AS airport_code,
  any_value(city_name) AS city_name, any_value(state_code) AS state_code,
  count(*)::BIGINT AS represented_flight_endpoints
FROM represented
GROUP BY airport_id;

CREATE OR REPLACE TABLE curated.route_quarter AS
WITH typed AS (
  SELECT try_cast("Year" AS SMALLINT) AS year,
    try_cast("Quarter" AS UTINYINT) AS quarter,
    upper(nullif(trim("Origin"), '')) AS origin_airport_code,
    try_cast("OriginAirportID" AS INTEGER) AS origin_airport_id,
    try_cast("OriginCityMarketID" AS INTEGER) AS origin_city_market_id,
    upper(nullif(trim("OriginState"), '')) AS origin_state,
    upper(nullif(trim("Dest"), '')) AS destination_airport_code,
    try_cast("DestAirportID" AS INTEGER) AS destination_airport_id,
    try_cast("DestCityMarketID" AS INTEGER) AS destination_city_market_id,
    upper(nullif(trim("DestState"), '')) AS destination_state,
    upper(nullif(trim("TkCarrier"), '')) AS ticketing_carrier_code,
    try_cast("Passengers" AS DOUBLE) AS sampled_passengers,
    try_cast("MktFare" AS DOUBLE) AS market_fare_usd,
    try_cast("MktDistance" AS DOUBLE) AS market_distance_miles,
    try_cast("MktCoupons" AS DOUBLE) AS market_coupons,
    coalesce(try_cast("BulkFare" AS DOUBLE), 0) = 1 AS bulk_fare
  FROM raw.db1b_market
  WHERE upper(trim("OriginCountry")) = 'US' AND upper(trim("DestCountry")) = 'US'
), valid AS (
  SELECT * FROM typed
  WHERE year = 2024 AND sampled_passengers > 0 AND market_fare_usd >= 0
)
SELECT year, quarter, origin_airport_code, origin_airport_id,
  origin_city_market_id, origin_state, destination_airport_code,
  destination_airport_id, destination_city_market_id, destination_state,
  ticketing_carrier_code, count(*)::BIGINT AS source_market_record_count,
  sum(sampled_passengers)::BIGINT AS sampled_passengers,
  (sum(sampled_passengers) * 10)::BIGINT AS estimated_passengers,
  round(sum(market_fare_usd * sampled_passengers) / sum(sampled_passengers), 2) AS weighted_mean_market_fare_usd,
  round(sum(CASE WHEN NOT bulk_fare THEN market_fare_usd * sampled_passengers END) /
    nullif(sum(CASE WHEN NOT bulk_fare THEN sampled_passengers END), 0), 2) AS weighted_mean_nonbulk_fare_usd,
  round(sum(CASE WHEN bulk_fare THEN sampled_passengers ELSE 0 END) / sum(sampled_passengers), 5) AS bulk_fare_passenger_share,
  round(sum(market_distance_miles * sampled_passengers) / sum(sampled_passengers), 1) AS weighted_mean_market_distance_miles,
  round(sum(market_coupons * sampled_passengers) / sum(sampled_passengers), 3) AS weighted_mean_market_coupons,
  round(sum(CASE WHEN market_coupons = 1 THEN sampled_passengers ELSE 0 END) / sum(sampled_passengers), 5) AS nonstop_passenger_share
FROM valid
GROUP BY ALL;

CREATE OR REPLACE TABLE curated.carrier_month AS
SELECT date_trunc('month', flight_date)::DATE AS month,
  marketing_carrier_code,
  count(*)::BIGINT AS scheduled_flights,
  count(*) FILTER (WHERE NOT cancelled AND NOT diverted)::BIGINT AS completed_nondiverted_flights,
  count(*) FILTER (WHERE cancelled)::BIGINT AS cancelled_flights,
  count(*) FILTER (WHERE diverted)::BIGINT AS diverted_flights,
  count(*) FILTER (WHERE arrival_delayed_15)::BIGINT AS arrival_delayed_15_flights,
  round(avg(arrival_delay_minutes) FILTER (WHERE NOT cancelled AND NOT diverted), 3) AS mean_arrival_delay_minutes,
  round(count(*) FILTER (WHERE arrival_delayed_15) / count(*)::DOUBLE, 5) AS arrival_delay_15_rate,
  round(count(*) FILTER (WHERE cancelled) / count(*)::DOUBLE, 5) AS cancellation_rate,
  sum(coalesce(carrier_delay_minutes, 0))::BIGINT AS carrier_delay_minutes,
  sum(coalesce(weather_delay_minutes, 0))::BIGINT AS weather_delay_minutes,
  sum(coalesce(national_aviation_system_delay_minutes, 0))::BIGINT AS national_aviation_system_delay_minutes,
  sum(coalesce(security_delay_minutes, 0))::BIGINT AS security_delay_minutes,
  sum(coalesce(late_aircraft_delay_minutes, 0))::BIGINT AS late_aircraft_delay_minutes
FROM curated.flights
GROUP BY ALL;
