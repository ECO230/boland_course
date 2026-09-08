CREATE SCHEMA curated;

CREATE TEMP TABLE map_patient AS
SELECT source_state, id,
       'P' || lpad(CAST(row_number() OVER (ORDER BY source_state, id) AS VARCHAR), 8, '0') AS patient_key
FROM raw.patients;

CREATE TEMP TABLE map_encounter AS
SELECT source_state, id,
       'E' || lpad(CAST(row_number() OVER (ORDER BY source_state, id) AS VARCHAR), 9, '0') AS encounter_key
FROM raw.encounters;

CREATE TEMP TABLE map_organization AS
SELECT source_state, id,
       'O' || lpad(CAST(row_number() OVER (ORDER BY source_state, id) AS VARCHAR), 6, '0') AS organization_key
FROM raw.organizations;

CREATE TEMP TABLE map_provider AS
SELECT source_state, id,
       'R' || lpad(CAST(row_number() OVER (ORDER BY source_state, id) AS VARCHAR), 7, '0') AS provider_key
FROM raw.providers;

CREATE TEMP TABLE map_payer AS
SELECT id,
       'Y' || lpad(CAST(row_number() OVER (ORDER BY id) AS VARCHAR), 4, '0') AS payer_key
FROM (SELECT DISTINCT id FROM raw.payers);

CREATE TEMP TABLE map_claim AS
SELECT source_state, id,
       'C' || lpad(CAST(row_number() OVER (ORDER BY source_state, id) AS VARCHAR), 9, '0') AS claim_key
FROM raw.claims;

CREATE TABLE curated.patients AS
SELECT
  mp.patient_key,
  try_cast(p.birthdate AS DATE) AS birth_date,
  try_cast(p.deathdate AS DATE) AS death_date,
  nullif(p.marital, '') AS marital_status_code,
  lower(p.race) AS race,
  lower(p.ethnicity) AS ethnicity,
  p.gender AS administrative_gender,
  CASE p.state WHEN 'Wisconsin' THEN 'WI' WHEN 'Minnesota' THEN 'MN' ELSE p.state END AS patient_state,
  nullif(p.county, '') AS patient_county,
  nullif(p.fips, '') AS county_fips,
  try_cast(p.healthcare_expenses AS DECIMAL(18,2)) AS lifetime_patient_expense,
  try_cast(p.healthcare_coverage AS DECIMAL(18,2)) AS lifetime_payer_coverage,
  try_cast(p.income AS INTEGER) AS annual_income
FROM raw.patients p
JOIN map_patient mp USING (source_state, id);

CREATE TABLE curated.organizations AS
SELECT
  mo.organization_key,
  'Great Lakes Facility ' || substring(mo.organization_key, 2) AS organization_label,
  CASE o.state WHEN 'Wisconsin' THEN 'WI' WHEN 'Minnesota' THEN 'MN' ELSE o.state END AS organization_state,
  try_cast(o.revenue AS DECIMAL(18,2)) AS simulated_revenue,
  try_cast(o.utilization AS INTEGER) AS simulated_encounter_count
FROM raw.organizations o
JOIN map_organization mo USING (source_state, id);

CREATE TABLE curated.providers AS
SELECT
  mr.provider_key,
  mo.organization_key,
  p.gender AS administrative_gender,
  nullif(p.speciality, '') AS specialty,
  try_cast(p.encounters AS INTEGER) AS simulated_encounter_count,
  try_cast(p.procedures AS INTEGER) AS simulated_procedure_count
FROM raw.providers p
JOIN map_provider mr USING (source_state, id)
LEFT JOIN map_organization mo ON mo.source_state = p.source_state AND mo.id = p.organization;

CREATE TABLE curated.payers AS
SELECT
  mp.payer_key,
  any_value(p.name) AS payer_name,
  any_value(p.ownership) AS ownership_type,
  sum(try_cast(p.amount_covered AS DECIMAL(18,2))) AS simulated_amount_covered,
  sum(try_cast(p.amount_uncovered AS DECIMAL(18,2))) AS simulated_amount_uncovered,
  sum(try_cast(p.revenue AS DECIMAL(18,2))) AS simulated_revenue,
  CAST(sum(try_cast(p.covered_encounters AS BIGINT)) AS BIGINT) AS covered_encounter_count,
  CAST(sum(try_cast(p.uncovered_encounters AS BIGINT)) AS BIGINT) AS uncovered_encounter_count,
  CAST(sum(try_cast(p.unique_customers AS BIGINT)) AS BIGINT) AS unique_customer_count,
  CAST(sum(try_cast(p.member_months AS BIGINT)) AS BIGINT) AS member_month_count
FROM raw.payers p
JOIN map_payer mp USING (id)
GROUP BY mp.payer_key;

CREATE TABLE curated.encounters AS
SELECT
  me.encounter_key,
  mp.patient_key,
  mo.organization_key,
  mr.provider_key,
  my.payer_key,
  try_cast(e.start AS TIMESTAMP) AS encounter_start,
  try_cast(e.stop AS TIMESTAMP) AS encounter_end,
  lower(e.encounterclass) AS encounter_class,
  try_cast(e.base_encounter_cost AS DECIMAL(18,2)) AS base_encounter_cost,
  try_cast(e.total_claim_cost AS DECIMAL(18,2)) AS total_claim_cost,
  try_cast(e.payer_coverage AS DECIMAL(18,2)) AS payer_coverage,
  CASE WHEN nullif(e.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_category, 'Medicine and Surgery') END AS reason_diagnosis_category,
  CASE WHEN nullif(e.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_group, 'General Medicine') END AS reason_diagnosis_group
FROM raw.encounters e
JOIN map_encounter me USING (source_state, id)
JOIN map_patient mp ON mp.source_state = e.source_state AND mp.id = e.patient
LEFT JOIN map_organization mo ON mo.source_state = e.source_state AND mo.id = e.organization
LEFT JOIN map_provider mr ON mr.source_state = e.source_state AND mr.id = e.provider
LEFT JOIN map_payer my ON my.id = e.payer
LEFT JOIN internal.diagnosis_code_lookup dm ON dm.source_code = nullif(e.reasoncode, '');

CREATE TABLE curated.diagnoses AS
SELECT
  'D' || lpad(CAST(row_number() OVER (ORDER BY c.source_state, c.source_row) AS VARCHAR), 9, '0') AS diagnosis_key,
  mp.patient_key,
  me.encounter_key,
  try_cast(c.start AS DATE) AS diagnosis_start_date,
  try_cast(c.stop AS DATE) AS diagnosis_end_date,
  dm.diagnosis_category,
  dm.diagnosis_group
FROM raw.conditions c
JOIN map_patient mp ON mp.source_state = c.source_state AND mp.id = c.patient
JOIN map_encounter me ON me.source_state = c.source_state AND me.id = c.encounter
JOIN internal.diagnosis_concept_map dm
  ON dm.source_code_system = c.system AND dm.source_code = c.code;

CREATE TABLE curated.procedures AS
SELECT
  'PR' || lpad(CAST(row_number() OVER (ORDER BY p.source_state, p.source_row) AS VARCHAR), 9, '0') AS procedure_key,
  mp.patient_key,
  me.encounter_key,
  try_cast(p.start AS TIMESTAMP) AS procedure_start,
  try_cast(p.stop AS TIMESTAMP) AS procedure_end,
  pm.procedure_category,
  pm.procedure_group,
  try_cast(p.base_cost AS DECIMAL(18,2)) AS base_cost,
  CASE WHEN nullif(p.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_category, 'Medicine and Surgery') END AS reason_diagnosis_category,
  CASE WHEN nullif(p.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_group, 'General Medicine') END AS reason_diagnosis_group
FROM raw.procedures p
JOIN map_patient mp ON mp.source_state = p.source_state AND mp.id = p.patient
JOIN map_encounter me ON me.source_state = p.source_state AND me.id = p.encounter
JOIN internal.procedure_concept_map pm
  ON pm.source_code_system = p.system AND pm.source_code = p.code
LEFT JOIN internal.diagnosis_code_lookup dm ON dm.source_code = nullif(p.reasoncode, '');

CREATE TABLE curated.medications AS
SELECT
  'M' || lpad(CAST(row_number() OVER (ORDER BY m.source_state, m.source_row) AS VARCHAR), 9, '0') AS medication_key,
  mp.patient_key,
  me.encounter_key,
  my.payer_key,
  try_cast(m.start AS TIMESTAMP) AS medication_start,
  try_cast(m.stop AS TIMESTAMP) AS medication_end,
  m.code AS medication_code,
  m.description AS medication_description,
  try_cast(m.base_cost AS DECIMAL(18,2)) AS base_cost,
  try_cast(m.payer_coverage AS DECIMAL(18,2)) AS payer_coverage,
  try_cast(m.dispenses AS INTEGER) AS dispense_count,
  try_cast(m.totalcost AS DECIMAL(18,2)) AS total_cost,
  CASE WHEN nullif(m.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_category, 'Medicine and Surgery') END AS reason_diagnosis_category,
  CASE WHEN nullif(m.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_group, 'General Medicine') END AS reason_diagnosis_group
FROM raw.medications m
JOIN map_patient mp ON mp.source_state = m.source_state AND mp.id = m.patient
JOIN map_encounter me ON me.source_state = m.source_state AND me.id = m.encounter
LEFT JOIN map_payer my ON my.id = m.payer
LEFT JOIN internal.diagnosis_code_lookup dm ON dm.source_code = nullif(m.reasoncode, '');

CREATE TABLE curated.observations AS
SELECT
  'B' || lpad(CAST(row_number() OVER (ORDER BY o.source_state, o.source_row) AS VARCHAR), 10, '0') AS observation_key,
  mp.patient_key,
  me.encounter_key,
  try_cast(o.date AS TIMESTAMP) AS observation_time,
  nullif(lower(o.category), '') AS observation_category,
  o.code AS observation_code,
  o.description AS observation_description,
  CASE WHEN lower(o.type) = 'numeric' THEN try_cast(o.value AS DOUBLE) END AS value_numeric,
  CASE WHEN lower(o.type) <> 'numeric' THEN nullif(o.value, '') END AS value_text,
  nullif(o.units, '') AS units,
  lower(o.type) AS value_type
FROM raw.observations o
JOIN map_patient mp ON mp.source_state = o.source_state AND mp.id = o.patient
LEFT JOIN map_encounter me ON me.source_state = o.source_state AND me.id = nullif(o.encounter, '');

CREATE TABLE curated.allergies AS
SELECT
  'A' || lpad(CAST(row_number() OVER (ORDER BY a.source_state, a.source_row) AS VARCHAR), 9, '0') AS allergy_key,
  mp.patient_key,
  me.encounter_key,
  try_cast(a.start AS DATE) AS allergy_start_date,
  try_cast(a.stop AS DATE) AS allergy_end_date,
  nullif(lower(a.type), '') AS allergy_type,
  nullif(lower(a.category), '') AS allergy_category,
  internal.allergen_group_rule(a.category) AS allergen_group,
  internal.allergy_reaction_group_rule(a.description1) AS reaction_1_group,
  nullif(lower(a.severity1), '') AS reaction_1_severity,
  internal.allergy_reaction_group_rule(a.description2) AS reaction_2_group,
  nullif(lower(a.severity2), '') AS reaction_2_severity
FROM raw.allergies a
JOIN map_patient mp ON mp.source_state = a.source_state AND mp.id = a.patient
JOIN map_encounter me ON me.source_state = a.source_state AND me.id = a.encounter;

CREATE TABLE curated.immunizations AS
SELECT
  'I' || lpad(CAST(row_number() OVER (ORDER BY i.source_state, i.source_row) AS VARCHAR), 9, '0') AS immunization_key,
  mp.patient_key,
  me.encounter_key,
  try_cast(i.date AS TIMESTAMP) AS immunization_time,
  i.code AS immunization_code,
  i.description AS immunization_description,
  try_cast(i.base_cost AS DECIMAL(18,2)) AS base_cost
FROM raw.immunizations i
JOIN map_patient mp ON mp.source_state = i.source_state AND mp.id = i.patient
JOIN map_encounter me ON me.source_state = i.source_state AND me.id = i.encounter;

CREATE TABLE curated.careplans AS
SELECT
  'CP' || lpad(CAST(row_number() OVER (ORDER BY c.source_state, c.id) AS VARCHAR), 8, '0') AS careplan_key,
  mp.patient_key,
  me.encounter_key,
  try_cast(c.start AS DATE) AS careplan_start_date,
  try_cast(c.stop AS DATE) AS careplan_end_date,
  internal.diagnosis_category_rule(
    internal.diagnosis_group_rule(c.description, 'CAREPLAN', c.code)
  ) AS care_focus_category,
  internal.diagnosis_group_rule(c.description, 'CAREPLAN', c.code) AS care_focus_group,
  CASE WHEN nullif(c.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_category, 'Medicine and Surgery') END AS reason_diagnosis_category,
  CASE WHEN nullif(c.reasoncode, '') IS NULL THEN 'No Diagnosis'
       ELSE coalesce(dm.diagnosis_group, 'General Medicine') END AS reason_diagnosis_group
FROM raw.careplans c
JOIN map_patient mp ON mp.source_state = c.source_state AND mp.id = c.patient
JOIN map_encounter me ON me.source_state = c.source_state AND me.id = c.encounter
LEFT JOIN internal.diagnosis_code_lookup dm ON dm.source_code = nullif(c.reasoncode, '');

CREATE TABLE curated.claims AS
SELECT
  mc.claim_key,
  mp.patient_key,
  me.encounter_key,
  mr.provider_key,
  my.payer_key AS primary_payer_key,
  my2.payer_key AS secondary_payer_key,
  try_cast(c.currentillnessdate AS TIMESTAMP) AS current_illness_time,
  try_cast(c.servicedate AS TIMESTAMP) AS service_time,
  nullif(c.status1, '') AS primary_payer_status,
  nullif(c.status2, '') AS secondary_payer_status,
  nullif(c.statusp, '') AS patient_status,
  try_cast(c.outstanding1 AS DECIMAL(18,2)) AS primary_payer_outstanding,
  try_cast(c.outstanding2 AS DECIMAL(18,2)) AS secondary_payer_outstanding,
  try_cast(c.outstandingp AS DECIMAL(18,2)) AS patient_outstanding,
  d1.diagnosis_category AS diagnosis_category_1,
  d1.diagnosis_group AS diagnosis_group_1,
  d2.diagnosis_category AS diagnosis_category_2,
  d2.diagnosis_group AS diagnosis_group_2,
  d3.diagnosis_category AS diagnosis_category_3,
  d3.diagnosis_group AS diagnosis_group_3,
  d4.diagnosis_category AS diagnosis_category_4,
  d4.diagnosis_group AS diagnosis_group_4,
  try_cast(c.healthcareclaimtypeid1 AS INTEGER) AS primary_claim_type_code
FROM raw.claims c
JOIN map_claim mc USING (source_state, id)
JOIN map_patient mp ON mp.source_state = c.source_state AND mp.id = c.patientid
LEFT JOIN map_encounter me ON me.source_state = c.source_state AND me.id = c.appointmentid
LEFT JOIN map_provider mr ON mr.source_state = c.source_state AND mr.id = c.providerid
LEFT JOIN map_payer my ON my.id = c.primarypatientinsuranceid
LEFT JOIN map_payer my2 ON my2.id = c.secondarypatientinsuranceid
LEFT JOIN internal.diagnosis_code_lookup d1 ON d1.source_code = nullif(c.diagnosis1, '')
LEFT JOIN internal.diagnosis_code_lookup d2 ON d2.source_code = nullif(c.diagnosis2, '')
LEFT JOIN internal.diagnosis_code_lookup d3 ON d3.source_code = nullif(c.diagnosis3, '')
LEFT JOIN internal.diagnosis_code_lookup d4 ON d4.source_code = nullif(c.diagnosis4, '');

CREATE TABLE curated.claim_transactions AS
SELECT
  'T' || lpad(CAST(row_number() OVER (ORDER BY t.source_state, t.source_row) AS VARCHAR), 10, '0') AS claim_transaction_key,
  mc.claim_key,
  mp.patient_key,
  me.encounter_key,
  mr.provider_key,
  upper(t.type) AS transaction_type,
  try_cast(t.fromdate AS TIMESTAMP) AS transaction_start,
  try_cast(t.todate AS TIMESTAMP) AS transaction_end,
  nullif(upper(t.method), '') AS payment_method,
  pm.procedure_category AS service_category,
  pm.procedure_group AS service_group,
  try_cast(t.units AS DOUBLE) AS units,
  try_cast(t.unitamount AS DECIMAL(18,2)) AS unit_amount,
  try_cast(t.amount AS DECIMAL(18,2)) AS amount,
  try_cast(t.payments AS DECIMAL(18,2)) AS payment_amount,
  try_cast(t.adjustments AS DECIMAL(18,2)) AS adjustment_amount,
  try_cast(t.transfers AS DECIMAL(18,2)) AS transfer_amount,
  try_cast(t.outstanding AS DECIMAL(18,2)) AS outstanding_amount
FROM raw.claim_transactions t
JOIN map_claim mc ON mc.source_state = t.source_state AND mc.id = t.claimid
JOIN map_patient mp ON mp.source_state = t.source_state AND mp.id = t.patientid
LEFT JOIN map_encounter me ON me.source_state = t.source_state AND me.id = t.appointmentid
LEFT JOIN map_provider mr ON mr.source_state = t.source_state AND mr.id = t.providerid
LEFT JOIN internal.claim_line_procedure_lookup pm ON pm.source_code = nullif(t.procedurecode, '');

CREATE TABLE curated.coverage_periods AS
SELECT
  'V' || lpad(CAST(row_number() OVER (ORDER BY c.source_state, c.source_row) AS VARCHAR), 9, '0') AS coverage_period_key,
  mp.patient_key,
  my.payer_key,
  my2.payer_key AS secondary_payer_key,
  try_cast(c.start_date AS DATE) AS coverage_start_date,
  try_cast(c.end_date AS DATE) AS coverage_end_date,
  nullif(c.plan_ownership, '') AS plan_ownership
FROM raw.coverage_periods c
JOIN map_patient mp ON mp.source_state = c.source_state AND mp.id = c.patient
JOIN map_payer my ON my.id = c.payer
LEFT JOIN map_payer my2 ON my2.id = c.secondary_payer;

CREATE TABLE curated.encounter_analysis AS
WITH diagnosis_counts AS (
  SELECT encounter_key, count(*) AS diagnosis_count FROM curated.diagnoses GROUP BY encounter_key
), procedure_counts AS (
  SELECT encounter_key, count(*) AS procedure_count FROM curated.procedures GROUP BY encounter_key
), medication_counts AS (
  SELECT encounter_key, count(*) AS medication_count FROM curated.medications GROUP BY encounter_key
), observation_counts AS (
  SELECT encounter_key, count(*) AS observation_count,
         count(*) FILTER (WHERE value_numeric IS NOT NULL) AS numeric_observation_count
  FROM curated.observations GROUP BY encounter_key
), sequenced AS (
  SELECT e.*,
         lead(encounter_start) OVER (PARTITION BY patient_key ORDER BY encounter_start, encounter_key) AS next_encounter_start
  FROM curated.encounters e
)
SELECT
  s.encounter_key,
  s.patient_key,
  s.organization_key,
  s.provider_key,
  s.payer_key,
  s.encounter_start,
  s.encounter_end,
  s.encounter_class,
  s.total_claim_cost,
  s.payer_coverage,
  s.total_claim_cost - s.payer_coverage AS patient_responsibility,
  round(date_diff('minute', s.encounter_start, s.encounter_end) / 60.0, 2) AS encounter_duration_hours,
  coalesce(d.diagnosis_count, 0) AS diagnosis_count,
  coalesce(pr.procedure_count, 0) AS procedure_count,
  coalesce(m.medication_count, 0) AS medication_count,
  coalesce(o.observation_count, 0) AS observation_count,
  coalesce(o.numeric_observation_count, 0) AS numeric_observation_count,
  s.next_encounter_start,
  date_diff('day', CAST(s.encounter_end AS DATE), CAST(s.next_encounter_start AS DATE)) AS days_to_next_encounter,
  CASE
    WHEN s.next_encounter_start IS NULL THEN false
    WHEN s.next_encounter_start <= s.encounter_end + INTERVAL 30 DAY THEN true
    ELSE false
  END AS revisit_within_30_days
FROM sequenced s
LEFT JOIN diagnosis_counts d USING (encounter_key)
LEFT JOIN procedure_counts pr USING (encounter_key)
LEFT JOIN medication_counts m USING (encounter_key)
LEFT JOIN observation_counts o USING (encounter_key);

-- Course-generated operational extension. Every eligible source encounter has
-- one completed appointment. A deterministic subset also anchors a fictional
-- appointment that did not result in an encounter.
CREATE TABLE curated.appointments AS
WITH eligible AS (
  SELECT
    e.*,
    p.birth_date,
    p.annual_income,
    1 + CAST(hash('lead|' || e.encounter_key) % 75 AS INTEGER) AS lead_time_days,
    CASE
      WHEN hash('channel|' || e.encounter_key) % 100 < 38 THEN 'patient_portal'
      WHEN hash('channel|' || e.encounter_key) % 100 < 68 THEN 'phone'
      WHEN hash('channel|' || e.encounter_key) % 100 < 85 THEN 'in_person'
      ELSE 'referral'
    END AS scheduling_channel,
    hash('reminder|' || e.encounter_key) % 100 < 88 AS reminder_sent,
    CASE e.encounter_class
      WHEN 'virtual' THEN CAST(hash('delay|' || e.encounter_key) % 16 AS INTEGER)
      WHEN 'wellness' THEN 5 + CAST(hash('delay|' || e.encounter_key) % 31 AS INTEGER)
      ELSE 3 + CAST(hash('delay|' || e.encounter_key) % 38 AS INTEGER)
    END AS start_delay_minutes,
    CASE e.encounter_class
      WHEN 'virtual' THEN 20
      WHEN 'wellness' THEN 45
      WHEN 'outpatient' THEN 40
      ELSE 30
    END AS scheduled_duration_minutes
  FROM curated.encounters e
  JOIN curated.patients p USING (patient_key)
  WHERE e.encounter_class IN ('ambulatory', 'wellness', 'outpatient', 'virtual')
    AND (p.death_date IS NULL OR CAST(e.encounter_start AS DATE) <= p.death_date)
), completed AS (
  SELECT
    'AP' || substring(encounter_key, 2) AS appointment_key,
    patient_key,
    encounter_key,
    organization_key,
    provider_key,
    encounter_class AS appointment_type,
    scheduling_channel,
    reminder_sent,
    encounter_start - start_delay_minutes * INTERVAL '1 minute' AS scheduled_start,
    encounter_start - start_delay_minutes * INTERVAL '1 minute'
      + scheduled_duration_minutes * INTERVAL '1 minute' AS scheduled_end,
    encounter_start - start_delay_minutes * INTERVAL '1 minute'
      - lead_time_days * INTERVAL '1 day' AS appointment_created_time,
    encounter_start - start_delay_minutes * INTERVAL '1 minute'
      - CAST(hash('checkin|' || encounter_key) % 16 AS INTEGER) * INTERVAL '1 minute' AS checkin_time,
    start_delay_minutes,
    lead_time_days,
    birth_date,
    annual_income
  FROM eligible
), extra_parameters AS (
  SELECT
    c.*,
    1 + CAST(hash('extra-lead|' || appointment_key) % 60 AS INTEGER) AS extra_lead_days,
    hash('extra-reminder|' || appointment_key) % 100 < 76 AS extra_reminder_sent,
    CASE
      WHEN hash('extra-channel|' || appointment_key) % 100 < 44 THEN 'phone'
      WHEN hash('extra-channel|' || appointment_key) % 100 < 72 THEN 'patient_portal'
      WHEN hash('extra-channel|' || appointment_key) % 100 < 88 THEN 'in_person'
      ELSE 'referral'
    END AS extra_channel,
    scheduled_start - (14 + CAST(hash('extra-date|' || appointment_key) % 120 AS INTEGER)) * INTERVAL '1 day'
      AS extra_scheduled_start,
    hash('extra-status|' || appointment_key) % 100 AS status_roll
  FROM completed c
), extra_scored AS (
  SELECT
    *,
    5
      + CASE WHEN annual_income < 40000 THEN 6 ELSE 0 END
      + CASE WHEN extra_lead_days > 30 THEN 5 ELSE 0 END
      + CASE WHEN NOT extra_reminder_sent THEN 10 ELSE 0 END
      + CASE WHEN extra_channel = 'phone' THEN 4 ELSE 0 END
      + CASE WHEN appointment_type = 'wellness' THEN 3 ELSE 0 END AS creation_threshold
  FROM extra_parameters
), extra_appointments AS (
  SELECT
    'AX' || substring(appointment_key, 3) AS appointment_key,
    patient_key,
    CAST(NULL AS VARCHAR) AS encounter_key,
    organization_key,
    provider_key,
    appointment_type,
    extra_channel AS scheduling_channel,
    extra_reminder_sent AS reminder_sent,
    extra_scheduled_start AS scheduled_start,
    extra_scheduled_start
      + CASE appointment_type WHEN 'virtual' THEN 20 WHEN 'wellness' THEN 45 WHEN 'outpatient' THEN 40 ELSE 30 END
        * INTERVAL '1 minute' AS scheduled_end,
    extra_scheduled_start - extra_lead_days * INTERVAL '1 day' AS appointment_created_time,
    CAST(NULL AS TIMESTAMP) AS checkin_time,
    CAST(NULL AS INTEGER) AS start_delay_minutes,
    extra_lead_days AS lead_time_days,
    CASE
      WHEN status_roll < 60 THEN 'no_show'
      WHEN status_roll < 80 THEN 'cancelled_patient'
      WHEN status_roll < 90 THEN 'rescheduled'
      ELSE 'cancelled_clinic'
    END AS appointment_status,
    CASE
      WHEN status_roll < 60 THEN CAST(NULL AS TIMESTAMP)
      ELSE extra_scheduled_start
        - (1 + CAST(hash('cancel-time|' || appointment_key) % 72 AS INTEGER)) * INTERVAL '1 hour'
    END AS cancelled_time,
    'course_generated_extension' AS record_source
  FROM extra_scored
  WHERE hash('extra-create|' || appointment_key) % 100 < creation_threshold
    AND extra_scheduled_start - extra_lead_days * INTERVAL '1 day' >= CAST(birth_date AS TIMESTAMP)
)
SELECT
  appointment_key,
  patient_key,
  encounter_key,
  organization_key,
  provider_key,
  appointment_type,
  scheduling_channel,
  reminder_sent,
  scheduled_start,
  scheduled_end,
  appointment_created_time,
  checkin_time,
  start_delay_minutes,
  lead_time_days,
  'completed' AS appointment_status,
  CAST(NULL AS TIMESTAMP) AS cancelled_time,
  'synthea_encounter' AS record_source
FROM completed
UNION ALL BY NAME
SELECT * FROM extra_appointments;

-- Course-generated patient-experience extension. The table retains all survey
-- invitations so response bias is visible; ratings exist only for respondents.
CREATE TABLE curated.satisfaction_surveys AS
WITH invited AS (
  SELECT
    e.encounter_key,
    e.patient_key,
    e.organization_key,
    e.provider_key,
    e.encounter_class,
    e.encounter_end,
    p.birth_date,
    CASE
      WHEN hash('survey-mode|' || e.encounter_key) % 100 < 35 THEN 'email'
      WHEN hash('survey-mode|' || e.encounter_key) % 100 < 70 THEN 'sms'
      WHEN hash('survey-mode|' || e.encounter_key) % 100 < 90 THEN 'patient_portal'
      ELSE 'phone'
    END AS invitation_mode,
    coalesce(
      a.start_delay_minutes,
      CASE e.encounter_class
        WHEN 'emergency' THEN 45 + CAST(hash('survey-wait|' || e.encounter_key) % 181 AS INTEGER)
        WHEN 'urgentcare' THEN 30 + CAST(hash('survey-wait|' || e.encounter_key) % 121 AS INTEGER)
        WHEN 'inpatient' THEN 20 + CAST(hash('survey-wait|' || e.encounter_key) % 91 AS INTEGER)
        ELSE 5 + CAST(hash('survey-wait|' || e.encounter_key) % 46 AS INTEGER)
      END
    ) AS perceived_wait_minutes
  FROM curated.encounters e
  JOIN curated.patients p USING (patient_key)
  LEFT JOIN curated.appointments a
    ON a.encounter_key = e.encounter_key AND a.appointment_status = 'completed'
  WHERE hash('survey-invite|' || e.encounter_key) % 100 < 45
    AND (p.death_date IS NULL OR p.death_date > CAST(e.encounter_end AS DATE) + INTERVAL '15 days')
), latent_scores AS (
  SELECT
    *,
    e.encounter_end + INTERVAL '1 day'
      + CAST(hash('survey-send|' || encounter_key) % 12 AS INTEGER) * INTERVAL '1 hour' AS invitation_time,
    greatest(0.0, least(10.0,
      9.0
      - least(perceived_wait_minutes, 240) / 80.0
      + (CAST(hash('survey-score|' || encounter_key) % 41 AS INTEGER) - 20) / 10.0
      + CASE encounter_class WHEN 'wellness' THEN 0.5 WHEN 'emergency' THEN -0.8 WHEN 'urgentcare' THEN -0.4 ELSE 0.0 END
    )) AS latent_overall_rating
  FROM invited e
), response_flags AS (
  SELECT
    *,
    30
      + CASE invitation_mode WHEN 'sms' THEN 8 WHEN 'patient_portal' THEN 5 ELSE 0 END
      + CASE WHEN latent_overall_rating <= 5 THEN 14 WHEN latent_overall_rating >= 9 THEN 9 ELSE 0 END
      + CASE WHEN date_diff('year', birth_date, CAST(encounter_end AS DATE)) >= 65 THEN 5 ELSE 0 END
      AS response_threshold
  FROM latent_scores
), finalized AS (
  SELECT
    *,
    hash('survey-response|' || encounter_key) % 100 < response_threshold AS did_respond
  FROM response_flags
)
SELECT
  'S' || substring(encounter_key, 2) AS survey_key,
  encounter_key,
  patient_key,
  organization_key,
  provider_key,
  encounter_class,
  invitation_mode,
  invitation_time,
  CASE WHEN did_respond THEN 'responded' ELSE 'nonresponse' END AS response_status,
  CASE WHEN did_respond THEN invitation_time
    + (1 + CAST(hash('survey-response-time|' || encounter_key) % 14 AS INTEGER)) * INTERVAL '1 day'
    + CAST(hash('survey-response-hour|' || encounter_key) % 18 AS INTEGER) * INTERVAL '1 hour'
  END AS response_time,
  perceived_wait_minutes,
  CASE WHEN did_respond THEN CAST(round(latent_overall_rating) AS SMALLINT) END AS overall_rating,
  CASE WHEN did_respond THEN CAST(round(greatest(0.0, least(10.0,
    latent_overall_rating + (CAST(hash('recommend|' || encounter_key) % 3 AS INTEGER) - 1)
  ))) AS SMALLINT) END AS would_recommend_rating,
  CASE WHEN did_respond THEN CAST(round(greatest(1.0, least(5.0,
    (latent_overall_rating + 1.0) / 2.0 + (CAST(hash('communication|' || encounter_key) % 3 AS INTEGER) - 1) * 0.5
  ))) AS SMALLINT) END AS communication_rating,
  CASE WHEN did_respond THEN CAST(round(greatest(1.0, least(5.0,
    (latent_overall_rating + 1.5) / 2.0 + (CAST(hash('courtesy|' || encounter_key) % 3 AS INTEGER) - 1) * 0.5
  ))) AS SMALLINT) END AS staff_courtesy_rating,
  CASE WHEN did_respond THEN CAST(round(greatest(1.0, least(5.0,
    5.2 - least(perceived_wait_minutes, 240) / 60.0
      + (CAST(hash('wait-rating|' || encounter_key) % 3 AS INTEGER) - 1) * 0.5
  ))) AS SMALLINT) END AS wait_time_rating,
  CASE WHEN did_respond THEN CAST(round(greatest(1.0, least(5.0,
    (latent_overall_rating + 0.5) / 2.0 + (CAST(hash('coordination|' || encounter_key) % 3 AS INTEGER) - 1) * 0.5
  ))) AS SMALLINT) END AS care_coordination_rating,
  'course_generated_extension' AS record_source
FROM finalized;
