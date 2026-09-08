COPY (
    WITH eligible AS (
        SELECT
            crash_record_id AS crash_id,
            crash_datetime,
            crash_month,
            crash_month_name,
            crash_weekday_name,
            crash_hour,
            time_period,
            posted_speed_limit_mph,
            traffic_control_device,
            traffic_control_condition,
            reported_weather_condition,
            reported_lighting_condition,
            first_crash_type,
            trafficway_type,
            lane_count,
            roadway_alignment,
            reported_road_surface_condition,
            reported_road_defect,
            crash_type,
            intersection_related,
            hit_and_run,
            estimated_damage,
            primary_contributory_cause,
            zip5,
            police_beat,
            unit_count,
            most_severe_injury,
            injuries_total,
            injuries_fatal,
            injuries_incapacitating,
            injuries_non_incapacitating,
            temperature_f,
            relative_humidity_percent,
            precipitation_inches,
            visibility_miles,
            wind_speed_mph,
            latitude,
            longitude
        FROM read_parquet('{{SOURCE}}')
        WHERE crash_year = 2024
    )
    SELECT *
    FROM eligible
    ORDER BY md5(crash_id || '{{SAMPLE_SALT}}')
    LIMIT {{ROW_LIMIT}}
) TO '{{OUTPUT}}' (FORMAT CSV, HEADER true);
