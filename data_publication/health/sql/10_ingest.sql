CREATE SCHEMA raw;

CREATE VIEW raw.patients AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/patients.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/patients.csv', header = true, all_varchar = true);

CREATE VIEW raw.encounters AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/encounters.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/encounters.csv', header = true, all_varchar = true);

CREATE VIEW raw.conditions AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/conditions.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/conditions.csv', header = true, all_varchar = true);

CREATE VIEW raw.procedures AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/procedures.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/procedures.csv', header = true, all_varchar = true);

CREATE VIEW raw.medications AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/medications.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/medications.csv', header = true, all_varchar = true);

CREATE VIEW raw.observations AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/observations.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/observations.csv', header = true, all_varchar = true);

CREATE VIEW raw.allergies AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/allergies.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/allergies.csv', header = true, all_varchar = true);

CREATE VIEW raw.immunizations AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/immunizations.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/immunizations.csv', header = true, all_varchar = true);

CREATE VIEW raw.careplans AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/careplans.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/careplans.csv', header = true, all_varchar = true);

CREATE VIEW raw.claims AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/claims.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/claims.csv', header = true, all_varchar = true);

CREATE VIEW raw.claim_transactions AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/claims_transactions.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/claims_transactions.csv', header = true, all_varchar = true);

CREATE VIEW raw.coverage_periods AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/payer_transitions.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/payer_transitions.csv', header = true, all_varchar = true);

CREATE VIEW raw.organizations AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/organizations.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/organizations.csv', header = true, all_varchar = true);

CREATE VIEW raw.providers AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/providers.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/providers.csv', header = true, all_varchar = true);

CREATE VIEW raw.payers AS
SELECT 'WI' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{WI_ROOT}}/payers.csv', header = true, all_varchar = true)
UNION ALL BY NAME
SELECT 'MN' AS source_state, row_number() OVER () AS source_row, *
FROM read_csv('{{MN_ROOT}}/payers.csv', header = true, all_varchar = true);
