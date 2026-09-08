COPY (SELECT * FROM curated.flights ORDER BY flight_date, marketing_carrier_code, flight_key) TO '{{OUTPUT_FLIGHTS}}' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 250000);
COPY (SELECT * FROM curated.airports ORDER BY airport_id) TO '{{OUTPUT_AIRPORTS}}' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000);
COPY (SELECT * FROM curated.route_quarter ORDER BY year, quarter, origin_airport_code, destination_airport_code, ticketing_carrier_code) TO '{{OUTPUT_ROUTE_QUARTER}}' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000);
COPY (SELECT * FROM curated.carrier_month ORDER BY month, marketing_carrier_code) TO '{{OUTPUT_CARRIER_MONTH}}' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 100000);
