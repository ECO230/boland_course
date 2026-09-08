CREATE SCHEMA IF NOT EXISTS raw;

CREATE OR REPLACE TABLE raw.flights AS
SELECT
  "Year", "Quarter", "Month", "DayofMonth", "DayOfWeek", "FlightDate",
  "IATA_CODE_Marketing_Airline", "IATA_CODE_Operating_Airline",
  "Flight_Number_Marketing_Airline", "Tail_Number",
  "OriginAirportID", "Origin", "OriginCityName", "OriginState",
  "DestAirportID", "Dest", "DestCityName", "DestState",
  "CRSDepTime", "DepTime", "DepDelay", "DepDelayMinutes", "DepDel15",
  "CRSArrTime", "ArrTime", "ArrDelay", "ArrDelayMinutes", "ArrDel15",
  "Cancelled", "CancellationCode", "Diverted", "CRSElapsedTime",
  "ActualElapsedTime", "AirTime", "Distance", "CarrierDelay",
  "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay"
FROM read_csv(
  '{{ONTIME_GLOB}}', header=true, all_varchar=true, sample_size=-1,
  strict_mode=true, union_by_name=true, parallel=true
);

CREATE OR REPLACE TABLE raw.db1b_market AS
SELECT
  "Year", "Quarter", "Origin", "OriginAirportID", "OriginCityMarketID",
  "OriginState", "OriginCountry", "Dest", "DestAirportID",
  "DestCityMarketID", "DestState", "DestCountry", "TkCarrier",
  "Passengers", "MktFare", "MktDistance", "MktCoupons", "BulkFare"
FROM read_csv(
  '{{DB1B_GLOB}}', header=true, all_varchar=true, sample_size=-1,
  strict_mode=true, union_by_name=true, parallel=true
);
