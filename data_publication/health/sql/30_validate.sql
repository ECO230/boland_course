-- Every curated table must preserve its intended source rows.
SELECT CASE WHEN (SELECT count(*) FROM curated.patients) = (SELECT count(*) FROM raw.patients)
  THEN true ELSE error('Patient row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.encounters) = (SELECT count(*) FROM raw.encounters)
  THEN true ELSE error('Encounter row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.diagnoses) = (SELECT count(*) FROM raw.conditions)
  THEN true ELSE error('Diagnosis row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.procedures) = (SELECT count(*) FROM raw.procedures)
  THEN true ELSE error('Procedure row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.medications) = (SELECT count(*) FROM raw.medications)
  THEN true ELSE error('Medication row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.observations) = (SELECT count(*) FROM raw.observations)
  THEN true ELSE error('Observation row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.allergies) = (SELECT count(*) FROM raw.allergies)
  THEN true ELSE error('Allergy row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.immunizations) = (SELECT count(*) FROM raw.immunizations)
  THEN true ELSE error('Immunization row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.careplans) = (SELECT count(*) FROM raw.careplans)
  THEN true ELSE error('Care-plan row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.claims) = (SELECT count(*) FROM raw.claims)
  THEN true ELSE error('Claim row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.claim_transactions) = (SELECT count(*) FROM raw.claim_transactions)
  THEN true ELSE error('Claim-transaction row loss during modeling') END;
SELECT CASE WHEN (SELECT count(*) FROM curated.coverage_periods) = (SELECT count(*) FROM raw.coverage_periods)
  THEN true ELSE error('Coverage-period row loss during modeling') END;

-- Primary keys must be non-null and unique.
WITH key_checks AS (
  SELECT 'patients' AS table_name, count(*) AS rows, count(patient_key) AS nonnull, count(DISTINCT patient_key) AS distinct_keys FROM curated.patients
  UNION ALL SELECT 'encounters', count(*), count(encounter_key), count(DISTINCT encounter_key) FROM curated.encounters
  UNION ALL SELECT 'appointments', count(*), count(appointment_key), count(DISTINCT appointment_key) FROM curated.appointments
  UNION ALL SELECT 'satisfaction_surveys', count(*), count(survey_key), count(DISTINCT survey_key) FROM curated.satisfaction_surveys
  UNION ALL SELECT 'diagnoses', count(*), count(diagnosis_key), count(DISTINCT diagnosis_key) FROM curated.diagnoses
  UNION ALL SELECT 'procedures', count(*), count(procedure_key), count(DISTINCT procedure_key) FROM curated.procedures
  UNION ALL SELECT 'medications', count(*), count(medication_key), count(DISTINCT medication_key) FROM curated.medications
  UNION ALL SELECT 'observations', count(*), count(observation_key), count(DISTINCT observation_key) FROM curated.observations
  UNION ALL SELECT 'allergies', count(*), count(allergy_key), count(DISTINCT allergy_key) FROM curated.allergies
  UNION ALL SELECT 'immunizations', count(*), count(immunization_key), count(DISTINCT immunization_key) FROM curated.immunizations
  UNION ALL SELECT 'careplans', count(*), count(careplan_key), count(DISTINCT careplan_key) FROM curated.careplans
  UNION ALL SELECT 'claims', count(*), count(claim_key), count(DISTINCT claim_key) FROM curated.claims
  UNION ALL SELECT 'claim_transactions', count(*), count(claim_transaction_key), count(DISTINCT claim_transaction_key) FROM curated.claim_transactions
  UNION ALL SELECT 'coverage_periods', count(*), count(coverage_period_key), count(DISTINCT coverage_period_key) FROM curated.coverage_periods
  UNION ALL SELECT 'organizations', count(*), count(organization_key), count(DISTINCT organization_key) FROM curated.organizations
  UNION ALL SELECT 'providers', count(*), count(provider_key), count(DISTINCT provider_key) FROM curated.providers
  UNION ALL SELECT 'payers', count(*), count(payer_key), count(DISTINCT payer_key) FROM curated.payers
  UNION ALL SELECT 'encounter_analysis', count(*), count(encounter_key), count(DISTINCT encounter_key) FROM curated.encounter_analysis
)
SELECT CASE WHEN bool_and(rows > 0 AND rows = nonnull AND rows = distinct_keys)
  THEN true ELSE error('A curated primary key is null, duplicated, or belongs to an empty table') END
FROM key_checks;

-- All clinical facts must reference an included patient and encounter.
WITH orphan_counts AS (
  SELECT count(*) FILTER (WHERE p.patient_key IS NULL) AS patient_orphans,
         count(*) FILTER (WHERE e.encounter_key IS NULL) AS encounter_orphans
  FROM curated.diagnoses f
  LEFT JOIN curated.patients p USING (patient_key)
  LEFT JOIN curated.encounters e USING (encounter_key)
  UNION ALL
  SELECT count(*) FILTER (WHERE p.patient_key IS NULL), count(*) FILTER (WHERE e.encounter_key IS NULL)
  FROM curated.procedures f LEFT JOIN curated.patients p USING (patient_key) LEFT JOIN curated.encounters e USING (encounter_key)
  UNION ALL
  SELECT count(*) FILTER (WHERE p.patient_key IS NULL), count(*) FILTER (WHERE e.encounter_key IS NULL)
  FROM curated.medications f LEFT JOIN curated.patients p USING (patient_key) LEFT JOIN curated.encounters e USING (encounter_key)
  UNION ALL
  SELECT count(*) FILTER (WHERE p.patient_key IS NULL),
         count(*) FILTER (WHERE f.encounter_key IS NOT NULL AND e.encounter_key IS NULL)
  FROM curated.observations f LEFT JOIN curated.patients p USING (patient_key) LEFT JOIN curated.encounters e USING (encounter_key)
)
SELECT CASE WHEN sum(patient_orphans) = 0 AND sum(encounter_orphans) = 0
  THEN true ELSE error('Clinical fact contains an orphaned patient or encounter key') END
FROM orphan_counts;

-- Dates, geography, numeric observations, and the analysis grain must be plausible.
SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Encounter dates are invalid or extend beyond the simulation end date') END
FROM curated.encounters
WHERE encounter_start IS NULL OR encounter_end IS NULL OR encounter_end < encounter_start
   OR CAST(encounter_start AS DATE) > DATE '2025-12-31';

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Patient geography contains an unexpected state') END
FROM curated.patients WHERE patient_state NOT IN ('WI', 'MN');

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A numeric Synthea observation could not be parsed') END
FROM curated.observations WHERE value_type = 'numeric' AND value_numeric IS NULL;

SELECT CASE WHEN (SELECT count(*) FROM curated.encounter_analysis) = (SELECT count(*) FROM curated.encounters)
  THEN true ELSE error('Encounter analysis does not contain exactly one row per encounter') END;

-- Private terminology maps must be one-to-one at the source-concept grain, and
-- every released diagnosis/procedure concept must resolve to two public levels.
SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Private diagnosis crosswalk contains duplicate source concepts') END
FROM (
  SELECT source_code_system, source_code
  FROM internal.diagnosis_crosswalk_override
  GROUP BY source_code_system, source_code HAVING count(*) > 1
);

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Private procedure crosswalk contains duplicate source concepts') END
FROM (
  SELECT source_code_system, source_code
  FROM internal.procedure_crosswalk_override
  GROUP BY source_code_system, source_code HAVING count(*) > 1
);

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Diagnosis grouping is missing a category or group') END
FROM internal.diagnosis_concept_map
WHERE nullif(diagnosis_category, '') IS NULL OR nullif(diagnosis_group, '') IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Procedure grouping is missing a category or group') END
FROM internal.procedure_concept_map
WHERE nullif(procedure_category, '') IS NULL OR nullif(procedure_group, '') IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Public allergy grouping is incomplete') END
FROM curated.allergies
WHERE nullif(allergy_category, '') IS NULL OR nullif(allergen_group, '') IS NULL
   OR (reaction_1_severity IS NOT NULL AND reaction_1_group IS NULL)
   OR (reaction_2_severity IS NOT NULL AND reaction_2_group IS NULL);

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Public care-plan grouping is incomplete') END
FROM curated.careplans
WHERE nullif(care_focus_category, '') IS NULL OR nullif(care_focus_group, '') IS NULL
   OR nullif(reason_diagnosis_category, '') IS NULL OR nullif(reason_diagnosis_group, '') IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A claim diagnosis code does not resolve through the private map') END
FROM raw.claims c
LEFT JOIN internal.diagnosis_code_lookup d1 ON d1.source_code = nullif(c.diagnosis1, '')
LEFT JOIN internal.diagnosis_code_lookup d2 ON d2.source_code = nullif(c.diagnosis2, '')
LEFT JOIN internal.diagnosis_code_lookup d3 ON d3.source_code = nullif(c.diagnosis3, '')
LEFT JOIN internal.diagnosis_code_lookup d4 ON d4.source_code = nullif(c.diagnosis4, '')
WHERE (nullif(c.diagnosis1, '') IS NOT NULL AND d1.source_code IS NULL)
   OR (nullif(c.diagnosis2, '') IS NOT NULL AND d2.source_code IS NULL)
   OR (nullif(c.diagnosis3, '') IS NOT NULL AND d3.source_code IS NULL)
   OR (nullif(c.diagnosis4, '') IS NOT NULL AND d4.source_code IS NULL);

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('A claim-line procedure code does not resolve through the private map') END
FROM raw.claim_transactions t
LEFT JOIN internal.claim_line_procedure_lookup p ON p.source_code = nullif(t.procedurecode, '')
WHERE nullif(t.procedurecode, '') IS NOT NULL AND p.source_code IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Licensed diagnosis/procedure fields leaked into a public table') END
FROM information_schema.columns
WHERE table_schema = 'curated'
  AND table_name IN ('encounters', 'diagnoses', 'procedures', 'medications', 'allergies', 'careplans', 'claims', 'claim_transactions')
  AND column_name IN (
    'encounter_code', 'encounter_description', 'reason_code', 'reason_description',
    'code_system', 'diagnosis_code', 'diagnosis_description',
    'procedure_code', 'procedure_description',
    'diagnosis_code_1', 'diagnosis_code_2', 'diagnosis_code_3', 'diagnosis_code_4',
    'allergy_code', 'allergy_description', 'reaction_1_description', 'reaction_2_description',
    'careplan_code', 'careplan_description'
  );

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Encounter analysis contains a negative duration or patient responsibility') END
FROM curated.encounter_analysis
WHERE encounter_duration_hours < 0 OR patient_responsibility < 0;

-- Operational extension: appointment links, timestamps, statuses, and both
-- completed and noncompleted outcomes must be internally coherent.
SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Appointment contains an orphaned patient, organization, provider, or encounter') END
FROM curated.appointments a
LEFT JOIN curated.patients p USING (patient_key)
LEFT JOIN curated.organizations o USING (organization_key)
LEFT JOIN curated.providers r USING (provider_key)
LEFT JOIN curated.encounters e USING (encounter_key)
WHERE p.patient_key IS NULL OR o.organization_key IS NULL OR r.provider_key IS NULL
   OR (a.encounter_key IS NOT NULL AND e.encounter_key IS NULL);

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Appointment timestamps or status/link rules are invalid') END
FROM curated.appointments
WHERE scheduled_start IS NULL OR scheduled_end <= scheduled_start
   OR appointment_created_time > scheduled_start OR lead_time_days < 1
   OR appointment_status NOT IN ('completed', 'no_show', 'cancelled_patient', 'cancelled_clinic', 'rescheduled')
   OR (appointment_status = 'completed' AND (encounter_key IS NULL OR start_delay_minutes IS NULL))
   OR (appointment_status <> 'completed' AND (encounter_key IS NOT NULL OR start_delay_minutes IS NOT NULL))
   OR (appointment_status = 'no_show' AND cancelled_time IS NOT NULL)
   OR (appointment_status IN ('cancelled_patient', 'cancelled_clinic', 'rescheduled') AND cancelled_time IS NULL);

SELECT CASE WHEN
  (SELECT count(*) FROM curated.appointments WHERE appointment_status = 'completed')
  = (SELECT count(*) FROM curated.encounters e JOIN curated.patients p USING (patient_key)
     WHERE encounter_class IN ('ambulatory', 'wellness', 'outpatient', 'virtual')
       AND (p.death_date IS NULL OR CAST(e.encounter_start AS DATE) <= p.death_date))
  THEN true ELSE error('Eligible encounters do not map one-to-one to completed appointments') END;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Course-generated appointment occurs after the patient death date') END
FROM curated.appointments a
JOIN curated.patients p USING (patient_key)
WHERE p.death_date IS NOT NULL AND CAST(a.scheduled_start AS DATE) > p.death_date;

SELECT CASE WHEN count(*) FILTER (WHERE appointment_status = 'no_show') > 0
                  AND count(*) FILTER (WHERE appointment_status LIKE 'cancelled_%') > 0
                  AND count(*) FILTER (WHERE appointment_status = 'rescheduled') > 0
  THEN true ELSE error('Appointment extension lacks required noncompleted outcome variation') END
FROM curated.appointments;

-- Patient-experience extension: invitations retain nonresponse, while only
-- respondents have complete, bounded numeric ratings.
SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Satisfaction survey contains an orphaned relational key') END
FROM curated.satisfaction_surveys s
LEFT JOIN curated.patients p USING (patient_key)
LEFT JOIN curated.encounters e USING (encounter_key)
LEFT JOIN curated.organizations o ON o.organization_key = s.organization_key
LEFT JOIN curated.providers r ON r.provider_key = s.provider_key
WHERE p.patient_key IS NULL OR e.encounter_key IS NULL OR o.organization_key IS NULL OR r.provider_key IS NULL;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Satisfaction response status, timing, or rating values are invalid') END
FROM curated.satisfaction_surveys
WHERE response_status NOT IN ('responded', 'nonresponse')
   OR perceived_wait_minutes < 0
   OR (response_status = 'nonresponse' AND (
        response_time IS NOT NULL OR overall_rating IS NOT NULL OR would_recommend_rating IS NOT NULL
        OR communication_rating IS NOT NULL OR staff_courtesy_rating IS NOT NULL
        OR wait_time_rating IS NOT NULL OR care_coordination_rating IS NOT NULL))
   OR (response_status = 'responded' AND (
        response_time IS NULL OR response_time < invitation_time
        OR overall_rating NOT BETWEEN 0 AND 10 OR would_recommend_rating NOT BETWEEN 0 AND 10
        OR communication_rating NOT BETWEEN 1 AND 5 OR staff_courtesy_rating NOT BETWEEN 1 AND 5
        OR wait_time_rating NOT BETWEEN 1 AND 5 OR care_coordination_rating NOT BETWEEN 1 AND 5));

SELECT CASE WHEN avg(CAST(response_status = 'responded' AS INTEGER)) BETWEEN 0.20 AND 0.65
  THEN true ELSE error('Satisfaction survey response rate is outside the intended teaching range') END
FROM curated.satisfaction_surveys;

SELECT CASE WHEN count(*) = 0 THEN true ELSE error('Course-generated satisfaction invitation or response occurs after patient death') END
FROM curated.satisfaction_surveys s
JOIN curated.patients p USING (patient_key)
WHERE p.death_date IS NOT NULL
  AND (CAST(s.invitation_time AS DATE) >= p.death_date OR CAST(s.response_time AS DATE) > p.death_date);
