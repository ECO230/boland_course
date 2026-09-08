# Great Lakes Synthetic EHR Data Dictionary

## Read this first

Every record in this release is fully synthetic and describes no real person.
The data were generated with Synthea 4.0.0 for simulated Wisconsin and Minnesota
populations ending December 31, 2025. Findings apply only to this simulated
health system. Do not describe results as real prevalence, treatment effects,
clinical quality, safety, or causal evidence.

The release resembles a modern relational electronic health-record warehouse,
but it is vendor neutral. It is not a copy of Epic Clarity or Caboodle. Names,
identifier-like synthetic fields, street addresses, phones, exact coordinates,
source UUIDs, device identifiers, and free-text notes have been removed.

## Join and grain guidance

- `patient_key` connects a fictional patient across time.
- `encounter_key` connects clinical events to the visit at which they were
  documented, ordered, performed, or administered.
- `organization_key`, `provider_key`, and `payer_key` connect encounters to
  simulated facilities, clinicians, and insurers.
- `claim_key` connects claims with their transaction history.

Do not join fact tables directly and then total their numeric columns. One
encounter can have many diagnoses, procedures, medications, observations, and
claim transactions. Aggregate each fact table to the grain required by your
question before joining it to another table.

## Tables

### `ehr_patients.parquet`

One row per synthetic patient. Includes birth and death dates, administrative
gender, race, ethnicity, marital-status code, state, county, county FIPS,
simulated annual income, and simulated lifetime healthcare expense and payer
coverage. County geography supports joins to external population or geographic
resources. `lifetime_*` measures cover the full simulated history and must not
be treated as annual amounts.

### `ehr_encounters.parquet`

One row per healthcare encounter. Includes patient, organization, provider, and
payer keys; start and end timestamps; encounter class; base cost; total claim
cost; payer coverage; and the two-level ECO 230 diagnosis grouping for the
documented reason for the encounter. Source terminology codes and descriptions
are not released.
Common encounter classes include ambulatory, emergency, inpatient, urgent care,
and wellness. Encounter costs are simulated.

### `ehr_appointments.parquet`

One scheduled appointment. Eligible ambulatory, wellness, outpatient, and
virtual Synthea encounters receive a completed appointment with an
`encounter_key`. Additional course-generated appointments model no-shows,
patient cancellations, clinic cancellations, and rescheduling; these did not
produce encounters and therefore have a blank `encounter_key`.

Operational fields include appointment creation and scheduled times, type,
scheduling channel, reminder status, check-in time, scheduling lead time, start
delay, outcome status, and cancellation time. `record_source` distinguishes the
appointment reconstructed from a Synthea encounter from a fictional additional
appointment. The probability of an additional noncompleted appointment varies
deterministically with lead time, reminders, scheduling channel, appointment
type, and simulated patient income. These relationships are teaching features,
not estimates of real no-show behavior.

Do not count every blank `encounter_key` as a no-show: cancelled and rescheduled
appointments also lack encounters. Use `appointment_status`.

### `ehr_patient_satisfaction.parquet`

One survey invitation associated with an encounter. Both respondents and
nonrespondents remain in the table so teams can measure response rate and
investigate nonresponse bias. Rating fields are populated only when
`response_status` is `responded`; no comments or other free text are included.

Fields include invitation and response timing, invitation mode, encounter
class, perceived wait time, 0-10 overall and recommendation ratings, and 1-5
communication, courtesy, wait-time, and care-coordination ratings. Invitation,
response, and rating values are course-generated deterministic extensions.
Wait time and encounter class influence the simulated latent satisfaction
score, while response probability varies with invitation mode, age, and very
high or low latent satisfaction. Consequently, observed respondent averages do
not necessarily equal the experience of everyone invited.

### `ehr_diagnoses.parquet`

One condition episode documented at an encounter. `diagnosis_start_date` is
when the condition began or was recognized; `diagnosis_end_date` is blank for
an unresolved or ongoing condition. Multiple diagnosis rows can share the same
patient and encounter. `diagnosis_category` and `diagnosis_group` provide a
broad, independently authored ECO 230 teaching hierarchy. Individual source or
billing codes and descriptions are deliberately excluded. The private build
database retains the auditable source concept and any reviewed ICD-10-CM
crosswalk; that mapping is not part of the release.

### `ehr_procedures.parquet`

One procedure event, with timestamps, the broad `procedure_category` and
`procedure_group`, simulated base cost, and the grouped reason diagnosis.
Several procedures may occur during one encounter. Individual source, CDT,
CPT, or HCPCS codes and descriptions are deliberately excluded. The private
build database retains source concepts and accepts reviewed internal billing-
code crosswalks; that mapping is not part of the release.

### `ehr_medications.parquet`

One medication episode associated with an encounter. Includes start and stop
timestamps, payer, medication code and description, simulated base and total
costs, payer coverage, dispense count, and reason diagnosis. A row represents a
medication episode, not necessarily one pill, administration, or prescription
fill.

### `ehr_observations.parquet`

One laboratory, vital-sign, or assessment result. `value_numeric` is populated
for numeric results; `value_text` is populated for categorical or textual
results. Always interpret a result together with `observation_code`,
`observation_description`, and `units`. Observations are typically coded with
LOINC. Most have an `encounter_key`; a small number of patient-level quality-of-
life or disability measures do not. This is expected to be the largest
clinical table.

### `ehr_allergies.parquet`

One allergy or intolerance episode, with optional end date, category, coded
allergen group, and up to two broad reaction groups and severities. Individual
substance and reaction terminology codes and descriptions are excluded.

### `ehr_immunizations.parquet`

One administered immunization with timestamp, CVX-like code, description, and
simulated base cost.

### `ehr_careplans.parquet`

One care-plan episode with start and optional stop date, a broad care-focus
category/group, and the broad category/group of the optional reason diagnosis.
Individual care-plan and diagnosis codes and descriptions are excluded.

### `ehr_claims.parquet`

One simulated healthcare claim. It links to a patient and usually to an
encounter, provider, and payer. It includes service timing, claim status,
outstanding balances, up to four broad diagnosis category/group pairs, and a
claim-type code. Individual diagnosis codes are excluded. A claim is not a
payment and should not be counted as a unique patient or encounter.

### `ehr_claim_transactions.parquet`

One charge, payment, adjustment, or balance-transfer transaction on a claim.
Amount fields have different meanings by `transaction_type`; do not sum every
amount column together. `service_category` and `service_group` replace the
source procedure identifier. Source line notes, clinical descriptions, and
terminology identifiers are excluded.

## ECO 230 clinical groups

The diagnosis and procedure hierarchies are broad analytical teaching labels,
not clinical billing or coding advice. They intentionally stop at two levels so
students can compare service mix and relationships without working with
individual codes. They are independently authored for this course and are not
Sg2 group assignments.

The local build database contains private `internal.diagnosis_concept_map` and
`internal.procedure_concept_map` tables. These preserve the source concept,
grouping decision, mapping status, review provenance, and any locally supplied
ICD-10-CM, CPT, HCPCS, or other reviewed standard-code mapping. Neither table nor
the private crosswalk CSV files is copied into the release bundle.

### `ehr_coverage_periods.parquet`

One continuous payer-coverage period for a patient. Includes primary and
optional secondary payer, start and end dates, and plan ownership. Original
synthetic member identifiers and owner names are excluded.

### `ehr_organizations.parquet`

One organization used in the simulation. Original organization names,
addresses, coordinates, and phone numbers are excluded. `organization_label`
is a release-local fictional label. Revenue and utilization are simulated
whole-history measures.

### `ehr_providers.parquet`

One synthetic clinician, with release-local organization, administrative
gender, specialty, and simulated whole-history encounter and procedure counts.
Names and location details are excluded.

### `ehr_payers.parquet`

One payer organization, with ownership type and aggregated simulated coverage,
revenue, encounter, customer, and member-month measures. Payer summaries are
simulation outputs, not real insurer performance.

### `ehr_encounter_analysis.parquet`

One row per encounter. This convenience table supplies cost, duration, counts
of related clinical facts, next-encounter timing, and a 30-day revisit flag.
`revisit_within_30_days` means any subsequent recorded encounter within 30 days
of encounter end; it is not the same as an inpatient readmission, preventable
readmission, or adverse outcome. Patient demographics and geography are not
included so students must make and validate that join when relevant.

## Important analytical limitations

- Synthea is a simulation. Relationships, prevalence, care patterns, costs, and
  outcomes are not estimates of real Wisconsin or Minnesota populations.
- Appointment outcomes and satisfaction surveys are additional ECO 230
  simulation layers, not native Synthea records or estimates from Epic.
- Absence of a diagnosis or observation does not prove absence of a condition.
- Clinical facts are documented at different times and grains.
- Results recorded after an encounter begins can create data leakage if used to
  predict an outcome as though they were available at admission.
- The simulation ends on December 31, 2025, so later follow-up is censored.
- A small number of native Synthea encounters occur after the recorded patient
  death date, primarily wellness encounters shortly afterward. They are
  preserved as a discoverable source-data quality issue but are excluded from
  the course-generated appointment and satisfaction extensions.
- Cost and coverage fields are generated values and are not audited financial
  records.
