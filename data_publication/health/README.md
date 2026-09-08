# Great Lakes Synthetic EHR Dataset

This directory defines a fully synthetic, longitudinal health-record dataset
for ECO 230. Synthea is the clinical simulation engine; the public course
release is a privacy-reduced, vendor-neutral relational warehouse rather than a
copy of Epic Clarity or Caboodle.

The first pilot requests a target living population of 2,500 patients from
Wisconsin and 2,500 from Minnesota and fixes the simulation end date at December
31, 2025. Synthea also retains additional deceased patient histories, so the
requested target is recorded separately from the total generated population.
Fixed simulation and clinician seeds make a release reproducible.

## Privacy boundary

Every patient is fictional. The release is therefore **synthetic**, not
"deidentified real data." Even so, the curated tables exclude patient and
provider names, synthetic SSNs, driver's-license and passport identifiers,
street addresses, phone numbers, exact coordinates, source UUIDs, and clinical
notes. Release-local keys are used for joins.

## Local source generation

Synthea 4.0.0 requires Java 17 or newer. The source script downloads pinned,
checksum-verified copies of the Synthea JAR and a portable Eclipse Temurin 25
JRE under the ignored `.data-build/` directory. It does not install Java or
change machine-wide configuration.

```powershell
python data_publication/health/download_sources.py
```

For a fast schema smoke test:

```powershell
python data_publication/health/download_sources.py `
  --patients-per-state 25 `
  --output .data-build/health/schema-smoke
```

Source CSVs, the generator, Java runtime, logs, DuckDB work database, and
generated release bundle remain outside Git. No data is published by this
script.

## Public redistribution boundary

The 2026-fall-v1 public schema passed its terminology audit. Diagnosis and
procedure source terminology is retained only in the ignored local work
database; the corresponding public facts contain broad ECO 230 category/group
labels instead of SNOMED CT, ICD-10-CM, CDT, CPT, or HCPCS identifiers and
descriptions. Encounter reasons, medication reasons, allergies, care plans,
claim diagnoses, and claim-line services are similarly grouped so those fields
cannot reintroduce the private terminology. Public observation, medication,
and immunization fields retain LOINC, NLM-created RxNorm, and CDC CVX content
with the required notices in `LICENSE-DATA.txt`.

## Private terminology crosswalks

The build creates header-only files, when needed, under the ignored directory
`.data-build/health/private-crosswalks/`:

- `diagnosis_crosswalk.csv`
- `procedure_crosswalk.csv`

These files may contain reviewed ICD-10-CM, CPT, HCPCS, CDT, or other internal
standard-code mappings and optional overrides to the two-level ECO 230 groups.
Each reviewed row records its mapping authority, terminology version, reviewer,
and review date. They are read into `internal.diagnosis_concept_map` and
`internal.procedure_concept_map` in the local DuckDB work database. They are not
copied into the flat release bundle, metadata, schemas, checksums, or public
Parquet files. Do not commit them.

The default automated grouping is deterministic and description-based. Treat
`mapping_status = 'source_concept_grouped_crosswalk_pending'` as a review queue,
not as an asserted ICD-10-CM/CPT/HCPCS equivalence.

## Build and verify the local candidate

Use DuckDB 1.5.2, matching the infrastructure publisher:

```powershell
python data_publication/health/build_release.py
python data_publication/health/verify_release.py
python data_publication/health/smoke_test.py
```

The build creates a flat local-candidate bundle under
`.data-build/health/great-lakes-synthetic-ehr-2026-v1/release/`. These commands
do not transfer or publish it. The private `internal` schema is available in
`work/health.duckdb` for staff review and is never published. After
publication, `student_example.R` shows the minimal Arrow workflow for the
appointment/no-show and patient-satisfaction tables.

## Planned curated grains

The first release preserves patients, encounters, appointments, patient-
satisfaction survey invitations, diagnoses, procedures, medications,
observations, allergies, immunizations, care plans, claims, coverage periods,
organizations, providers, and payers as separate grains. A carefully scoped
one-row-per-encounter analysis table is supplied, but students still need to
identify useful joins and protect measures from row multiplication.

Appointments and satisfaction surveys are deterministic course-generated
extensions. They are labeled in the data and documented separately from native
Synthea output. The satisfaction table retains nonresponses and contains only
numeric ratings; it has no patient comments or other free text.

Unit/bed movement, claim denial, and other operational tables remain outside the
Synthea core and this candidate. Any later additions must be labeled as course-
generated synthetic extensions with their own models and validation.
