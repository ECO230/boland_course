COPY (
    WITH eligible AS (
        SELECT
            a.appointment_key AS appointment_id,
            a.patient_key AS patient_id,
            a.encounter_key AS encounter_id,
            a.organization_key AS organization_id,
            a.provider_key AS provider_id,
            CAST(a.scheduled_start AS DATE) AS appointment_date,
            month(a.scheduled_start) AS appointment_month,
            dayofweek(a.scheduled_start) AS appointment_day_of_week,
            hour(a.scheduled_start) AS appointment_hour,
            a.appointment_type,
            a.scheduling_channel,
            a.reminder_sent,
            a.lead_time_days,
            a.start_delay_minutes,
            a.appointment_status,
            p.administrative_gender,
            p.race,
            p.ethnicity,
            p.marital_status_code,
            p.patient_county,
            p.county_fips,
            p.annual_income,
            date_diff('year', p.birth_date, CAST(a.scheduled_start AS DATE))
              - CASE
                  WHEN month(a.scheduled_start) * 100 + day(a.scheduled_start)
                       < month(p.birth_date) * 100 + day(p.birth_date)
                  THEN 1 ELSE 0
                END AS patient_age_years,
            s.invitation_mode,
            s.response_status,
            s.perceived_wait_minutes,
            s.overall_rating,
            s.would_recommend_rating,
            s.communication_rating,
            s.staff_courtesy_rating,
            s.wait_time_rating,
            s.care_coordination_rating
        FROM read_parquet('{{APPOINTMENTS_SOURCE}}') AS a
        INNER JOIN read_parquet('{{PATIENTS_SOURCE}}') AS p
            ON a.patient_key = p.patient_key
        LEFT JOIN read_parquet('{{SATISFACTION_SOURCE}}') AS s
            ON a.encounter_key = s.encounter_key
        WHERE p.patient_state = 'WI'
          AND a.scheduled_start >= TIMESTAMP '2023-01-01'
          AND a.scheduled_start < TIMESTAMP '2026-01-01'
    )
    SELECT *
    FROM eligible
    ORDER BY md5(appointment_id || '{{SAMPLE_SALT}}')
    LIMIT {{ROW_LIMIT}}
) TO '{{OUTPUT}}' (FORMAT CSV, HEADER true);
