COPY (
    WITH eligible AS (
        SELECT
            flight_key AS flight_id,
            flight_date,
            quarter,
            month_number,
            day_of_week,
            marketing_carrier_code,
            operating_carrier_code,
            origin_airport_code,
            origin_city_name,
            origin_state,
            destination_airport_code,
            destination_city_name,
            destination_state,
            scheduled_departure_hhmm,
            actual_departure_hhmm,
            departure_delay_minutes,
            departure_delayed_15,
            scheduled_arrival_hhmm,
            actual_arrival_hhmm,
            arrival_delay_minutes,
            arrival_delayed_15,
            cancelled,
            cancellation_code,
            diverted,
            scheduled_elapsed_minutes,
            actual_elapsed_minutes,
            air_time_minutes,
            distance_miles,
            carrier_delay_minutes,
            weather_delay_minutes,
            national_aviation_system_delay_minutes,
            security_delay_minutes,
            late_aircraft_delay_minutes
        FROM read_parquet('{{SOURCE}}')
        WHERE origin_airport_code IN ('ORD', 'MDW', 'MKE', 'MSP')
    )
    SELECT *
    FROM eligible
    ORDER BY md5(flight_id || '{{SAMPLE_SALT}}')
    LIMIT {{ROW_LIMIT}}
) TO '{{OUTPUT}}' (FORMAT CSV, HEADER true);
