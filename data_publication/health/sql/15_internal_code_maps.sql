-- Private terminology and grouping layer.
--
-- Tables in the internal schema remain in the local DuckDB work database and
-- are never copied to the public release directory. They preserve the source
-- concepts, accept optional reviewed ICD-10-CM/CPT/HCPCS crosswalks, and assign
-- independent ECO 230 teaching groups. Public tables receive group labels only.

CREATE SCHEMA internal;

CREATE TABLE internal.diagnosis_crosswalk_override AS
SELECT *
FROM read_csv(
  '{{DIAGNOSIS_CROSSWALK}}',
  header = true,
  all_varchar = true,
  columns = {
    'source_code_system': 'VARCHAR',
    'source_code': 'VARCHAR',
    'standard_code_system': 'VARCHAR',
    'standard_code': 'VARCHAR',
    'standard_description': 'VARCHAR',
    'diagnosis_category': 'VARCHAR',
    'diagnosis_group': 'VARCHAR',
    'mapping_authority': 'VARCHAR',
    'mapping_version': 'VARCHAR',
    'reviewed_by': 'VARCHAR',
    'reviewed_date': 'VARCHAR'
  }
);

CREATE TABLE internal.procedure_crosswalk_override AS
SELECT *
FROM read_csv(
  '{{PROCEDURE_CROSSWALK}}',
  header = true,
  all_varchar = true,
  columns = {
    'source_code_system': 'VARCHAR',
    'source_code': 'VARCHAR',
    'standard_code_system': 'VARCHAR',
    'standard_code': 'VARCHAR',
    'standard_description': 'VARCHAR',
    'procedure_category': 'VARCHAR',
    'procedure_group': 'VARCHAR',
    'mapping_authority': 'VARCHAR',
    'mapping_version': 'VARCHAR',
    'reviewed_by': 'VARCHAR',
    'reviewed_date': 'VARCHAR'
  }
);

CREATE MACRO internal.diagnosis_group_rule(description, code_system, code) AS
CASE
  WHEN nullif(trim(description), '') IS NULL AND nullif(trim(code), '') IS NULL THEN 'No Diagnosis'
  WHEN regexp_matches(lower(description), 'medication review due|encounter for (general|screening|examination)|risk activity involvement|prophylactic|preprocedural examination')
    THEN 'Preventive and Administrative Findings'
  WHEN regexp_matches(lower(description), 'dental|tooth|teeth|molar|gingiv|periodont|oral health|oral lesion|caries|edentul')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^K0[0-9]'))
    THEN 'Oral and Dental Health'
  WHEN regexp_matches(lower(description), 'pregnan|gestation|labor|labour|delivery|postpartum|maternal|fetal|foetal|miscarriage|pre.?eclamp|placent|blighted ovum')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^O'))
    THEN 'Obstetrics'
  WHEN regexp_matches(lower(description), 'breast|mammary') THEN 'Breast Health'
  WHEN regexp_matches(lower(description), 'gynec|gynaec|cervix|cervical dysplasia|uter|ovarian|endometri|menstru|vaginal|vulv')
    THEN 'Gynecology'
  WHEN regexp_matches(lower(description), 'newborn|neonat|prematur|birth weight')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^P'))
    THEN 'Neonatal Care'
  WHEN regexp_matches(lower(description), 'cancer|carcinoma|malignan|leukemia|lymphoma|myeloma|neoplasm|tumou?r|metasta')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^(C|D0[0-9]|D1[0-9]|D2[0-9]|D3[0-9]|D4[0-9])'))
    THEN 'Cancer'
  WHEN regexp_matches(lower(description), 'depress|anxiety|panic|bipolar|schizo|psych|mental|stress|post-traumatic|suicid|self-harm|autis')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^F'))
    THEN 'Mental and Behavioral Health'
  WHEN regexp_matches(lower(description), 'alcohol|opioid|substance|drug abuse|drug dependence|misuses drugs|tobacco|smok|overdose')
    THEN 'Substance Use'
  WHEN regexp_matches(lower(description), 'deep vein|thrombo|embol|aneurysm|peripheral vascular|venous|varicose|vascular disease')
    THEN 'Vascular'
  WHEN regexp_matches(lower(description), 'heart|cardiac|coronary|myocard|arrhythm|atrial|angina|hypertension|cardiomy|aortic valve|mitral valve')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^I'))
    THEN 'Cardiology'
  WHEN regexp_matches(lower(description), 'diabet|obes|thyroid|endocr|metabolic|hyperlip|dyslip|triglycer|hypergly|adrenal|pituitary')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^E'))
    THEN 'Endocrine and Metabolic'
  WHEN regexp_matches(lower(description), 'infection|infectious|viral|bacterial|covid|influenza|sepsis|tuberc|streptococ|hepatitis|hiv|pharyngitis')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^[AB]'))
    THEN 'Infectious Disease'
  WHEN regexp_matches(lower(description), 'kidney|renal|nephro|glomerul') THEN 'Nephrology'
  WHEN regexp_matches(lower(description), 'urinary|urolog|bladder|prostate|ureter|urethra|erectile|testicular|cystitis|pyeloneph') THEN 'Urology'
  WHEN regexp_matches(lower(description), 'liver|hepatic|cirrhos|portal hypertension') THEN 'Hepatology'
  WHEN regexp_matches(lower(description), 'gastro|esoph|stomach|bowel|colon|rectal|digest|appendi|crohn|ulcerative colitis|hemorrhoid')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^K'))
    THEN 'Gastroenterology'
  WHEN regexp_matches(lower(description), 'anemia|haem|hemat|sickle|coagul|platelet|neutrop') THEN 'Hematology'
  WHEN regexp_matches(lower(description), 'rheumat|lupus|gout|inflammatory arthritis|fibromyalgia') THEN 'Rheumatology'
  WHEN regexp_matches(lower(description), 'otitis|ear |hearing|sinus|tonsil|nasal|laryn|pharyn|vertigo')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^H(6|7|8|9)'))
    THEN 'Ear, Nose, and Throat'
  WHEN regexp_matches(lower(description), 'burn|wound|lacerat|pressure ulcer') THEN 'Burns and Wounds'
  WHEN regexp_matches(lower(description), 'asthma|bronch|pulmon|respirat|copd|emphysema|sleep apnea|pneumonia|wheez|dyspnea|hypoxemia|cough|sputum')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^J'))
    THEN 'Pulmonology'
  WHEN regexp_matches(lower(description), 'eye|vision|visual|retina|cataract|glaucoma|ophthalm')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^H[0-5]'))
    THEN 'Ophthalmology'
  WHEN regexp_matches(lower(description), 'allerg|anaphyla|immune deficiency|immunologic') THEN 'Allergy and Immunology'
  WHEN regexp_matches(lower(description), 'skin|dermat|eczema|psoriasis|acne|rash')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^L'))
    THEN 'Dermatology'
  WHEN regexp_matches(lower(description), 'stroke|seizure|epilep|migraine|dement|alzheimer|parkinson|neurolog|cerebr|multiple sclerosis|sleep disorder|hydrocephal|chiari|meningomyelo')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^G'))
    THEN 'Neurosciences'
  WHEN regexp_matches(lower(description), 'spine|spinal|vertebr|low back|back pain|sciatica') THEN 'Spine'
  WHEN regexp_matches(lower(description), 'fracture|sprain|bone|joint|knee|hip|shoulder|orthop|osteoarthritis|musculoskeletal')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^M'))
    THEN 'Orthopedics'
  WHEN regexp_matches(lower(description), 'genetic|chromosom|down syndrome|hereditary') THEN 'Genetics'
  WHEN regexp_matches(lower(description), 'employment|unemploy|educat|social isolation|social contact|food insecurity|housing|homeless|violence|abuse|criminal record|financial insecurity|transport|military service|refugee|migrant')
    THEN 'Social and Economic Factors'
  WHEN regexp_matches(lower(description), 'injury|poison|accident|trauma')
       OR (upper(code_system) = 'ICD10' AND regexp_matches(upper(code), '^[ST]'))
    THEN 'Injury and Trauma'
  WHEN regexp_matches(lower(description), 'chronic pain|neck pain|muscle pain|\bpain\b') THEN 'Pain Management'
  ELSE 'General Medicine'
END;

CREATE MACRO internal.diagnosis_category_rule(diagnosis_group) AS
CASE
  WHEN diagnosis_group IN ('Mental and Behavioral Health', 'Substance Use') THEN 'Behavioral Health'
  WHEN diagnosis_group = 'Cancer' THEN 'Cancer Care'
  WHEN diagnosis_group IN ('Cardiology', 'Vascular') THEN 'Cardiovascular'
  WHEN diagnosis_group IN ('Obstetrics', 'Gynecology', 'Breast Health') THEN 'Women''s Health'
  WHEN diagnosis_group = 'Neonatal Care' THEN 'Neonatology and Newborn'
  WHEN diagnosis_group = 'Neurosciences' THEN 'Neurosciences'
  WHEN diagnosis_group = 'Orthopedics' THEN 'Orthopedics'
  WHEN diagnosis_group = 'Spine' THEN 'Spine'
  WHEN diagnosis_group = 'Oral and Dental Health' THEN 'Oral Health'
  WHEN diagnosis_group IN ('Social and Economic Factors', 'Preventive and Administrative Findings') THEN 'Social and Preventive Health'
  WHEN diagnosis_group = 'Injury and Trauma' THEN 'Injury and Trauma'
  WHEN diagnosis_group = 'No Diagnosis' THEN 'No Diagnosis'
  ELSE 'Medicine and Surgery'
END;

CREATE MACRO internal.procedure_group_rule(description, code_system, code) AS
CASE
  WHEN nullif(trim(description), '') IS NULL AND nullif(trim(code), '') IS NULL THEN 'No Procedure'
  WHEN upper(code_system) = 'CDT' OR regexp_matches(lower(description), 'dental|tooth|teeth|gingiv|periodont|oral evaluation|fluoride|prophylaxis')
    THEN 'Dental Services'
  WHEN regexp_matches(lower(description), 'positron emission|\bpet\b') THEN 'PET Imaging'
  WHEN regexp_matches(lower(description), 'magnetic resonance|\bmri\b') THEN 'MRI Imaging'
  WHEN regexp_matches(lower(description), 'computed tomograph|\bct\b') THEN 'CT Imaging'
  WHEN regexp_matches(lower(description), 'ultrasound|ultrason|sonograph|echocardi') THEN 'Ultrasound'
  WHEN regexp_matches(lower(description), 'x-ray|xray|radiograph|mammograph') THEN 'X-Ray and Mammography'
  WHEN regexp_matches(lower(description), 'nuclear medicine|spect|scintigraph') THEN 'Nuclear Medicine and SPECT'
  WHEN regexp_matches(lower(description), 'chemotherapy') THEN 'Chemotherapy'
  WHEN regexp_matches(lower(description), 'radiation therap|radiotherapy') THEN 'Radiation Therapy'
  WHEN regexp_matches(lower(description), 'interventional oncology|tumou?r ablation|tumor ablation') THEN 'Interventional Oncology'
  WHEN regexp_matches(lower(description), 'dialysis') THEN 'Dialysis'
  WHEN regexp_matches(lower(description), 'hospice|skilled nursing|post-acute|home health') THEN 'Post-Acute Services'
  WHEN regexp_matches(lower(description), 'rehabilitation|physical therap|occupational therap|speech therap|speech and language therap|movement therapy') THEN 'Rehabilitation'
  WHEN regexp_matches(lower(description), 'psychotherap|behavioral therap|counseling') THEN 'Psychological and Behavioral Therapy'
  WHEN regexp_matches(lower(description), 'depression screening|anxiety assessment|assessment of anxiety|substance use|drug abuse|alcohol use|domestic abuse|fall scale|screening test')
    THEN 'Behavioral and Risk Screening'
  WHEN regexp_matches(lower(description), 'vaccine|vaccination|immunization') THEN 'Vaccine Administration'
  WHEN regexp_matches(lower(description), 'infusion') THEN 'Infusion Services'
  WHEN regexp_matches(lower(description), 'medication reconciliation|medication review|drug administration|injection|immunotherapy|antibiotic therapy') THEN 'Medication Services'
  WHEN regexp_matches(lower(description), 'microbiol|patholog|culture|biopsy specimen|cytolog|antigen|antibody|assay|titer') THEN 'Microbiology and Pathology Lab'
  WHEN regexp_matches(lower(description), 'hemogram|blood count|blood group typing|chemistry|metabolic panel|lipid|glucose|hemoglobin|laboratory|lab test|urinalysis|blood smear|specimen collection') THEN 'Chemistry and Hematology Lab'
  WHEN regexp_matches(lower(description), 'colonoscopy|endoscopy|bronchoscopy|cystoscopy|arthroscopy|sigmoidoscopy|esophagogastro') THEN 'Endoscopy'
  WHEN regexp_matches(lower(description), 'screening|assessment|examination|evaluation|diagnostic|monitoring|test|auscultation|spirometry|fundal height|fundoscopy|gonioscopy|electrocardio|respiratory function') THEN 'Diagnostics and Assessment'
  WHEN regexp_matches(lower(description), 'transplant|bypass|replacement|amputation|resection|major surgery|cesarean|appendectomy|cholecystectomy') THEN 'Major Procedures'
  WHEN regexp_matches(lower(description), 'surgery|surgical|excision|repair|removal|incision|drainage|sutur|catheter|implant|polypectomy|episiotomy|termination of pregnancy|coronary intervention|angiography') THEN 'Minor Procedures'
  WHEN regexp_matches(lower(description), 'emergency|urgent care') THEN 'Urgent and Emergent Visits'
  WHEN regexp_matches(lower(description), 'observation care') THEN 'Observation Visits'
  WHEN regexp_matches(lower(description), 'encounter|office visit|check up|check-up|wellness visit|consultation') THEN 'Evaluation and Management Visits'
  WHEN regexp_matches(lower(description), 'equipment|device|supply|prosthe|orthotic|wheelchair') THEN 'Equipment and Supplies'
  WHEN regexp_matches(lower(description), 'nursing care|surveillance|plan of care|anticipatory guidance|oxygen administration|discharge|history taking|health risks education|postoperative care|anesthesia care') THEN 'Supportive Clinical Care'
  WHEN nullif(trim(description), '') IS NULL THEN 'Unknown'
  ELSE 'Other Clinical Services'
END;

CREATE MACRO internal.procedure_category_rule(procedure_group) AS
CASE
  WHEN procedure_group IN ('CT Imaging', 'MRI Imaging', 'PET Imaging') THEN 'Advanced Imaging'
  WHEN procedure_group IN ('Nuclear Medicine and SPECT', 'Ultrasound', 'X-Ray and Mammography') THEN 'Standard Imaging'
  WHEN procedure_group IN ('Chemistry and Hematology Lab', 'Microbiology and Pathology Lab') THEN 'Laboratory'
  WHEN procedure_group IN ('Chemotherapy', 'Radiation Therapy', 'Interventional Oncology') THEN 'Oncology Services'
  WHEN procedure_group IN ('Endoscopy', 'Major Procedures', 'Minor Procedures') THEN 'Procedures'
  WHEN procedure_group IN ('Urgent and Emergent Visits', 'Observation Visits', 'Evaluation and Management Visits') THEN 'Visits'
  WHEN procedure_group IN ('Behavioral and Risk Screening', 'Psychological and Behavioral Therapy') THEN 'Behavioral Services'
  WHEN procedure_group IN ('Rehabilitation', 'Post-Acute Services') THEN 'Post-Acute and Rehabilitation'
  WHEN procedure_group = 'Dialysis' THEN 'Renal Services'
  WHEN procedure_group = 'Dental Services' THEN 'Dental'
  WHEN procedure_group IN ('Medication Services', 'Infusion Services', 'Vaccine Administration') THEN 'Medication and Infusion Services'
  WHEN procedure_group = 'Equipment and Supplies' THEN 'Equipment and Supplies'
  WHEN procedure_group = 'Diagnostics and Assessment' THEN 'Diagnostics'
  WHEN procedure_group = 'Supportive Clinical Care' THEN 'Other'
  WHEN procedure_group = 'No Procedure' THEN 'No Procedure'
  WHEN procedure_group = 'Unknown' THEN 'Unknown'
  ELSE 'Other'
END;

CREATE MACRO internal.allergen_group_rule(category) AS
CASE lower(nullif(category, ''))
  WHEN 'food' THEN 'Food Allergens'
  WHEN 'medication' THEN 'Medication Allergens'
  WHEN 'environment' THEN 'Environmental Allergens'
  ELSE 'Other or Unspecified Allergens'
END;

CREATE MACRO internal.allergy_reaction_group_rule(description) AS
CASE
  WHEN nullif(description, '') IS NULL THEN NULL
  WHEN regexp_matches(lower(description), 'anaphyla') THEN 'Severe Systemic Reaction'
  WHEN regexp_matches(lower(description), 'wheal|skin|cutaneous|angioedema|itch|face goes red|eruption') THEN 'Skin and Soft-Tissue Reaction'
  WHEN regexp_matches(lower(description), 'rhinoconjunct|dyspnea|nasal|cough|sneez|wheez') THEN 'Respiratory or ENT Reaction'
  WHEN regexp_matches(lower(description), 'vomit|abdominal|diarrhea|nausea') THEN 'Gastrointestinal Reaction'
  WHEN regexp_matches(lower(description), 'kidney|renal|hyperkalemia') THEN 'Renal or Metabolic Reaction'
  ELSE 'Other Reaction'
END;

CREATE TABLE internal.diagnosis_concept_map AS
WITH claim_codes AS (
  SELECT DISTINCT code
  FROM (
    SELECT unnest([diagnosis1, diagnosis2, diagnosis3, diagnosis4]) AS code
    FROM raw.claims
  )
  WHERE nullif(code, '') IS NOT NULL
), description_candidates AS (
  SELECT code, description, 1 AS source_rank FROM raw.conditions
  UNION ALL SELECT code, description, 2 FROM raw.procedures
  UNION ALL SELECT code, description, 3 FROM raw.encounters
  UNION ALL SELECT reasoncode, reasondescription, 4 FROM raw.encounters WHERE nullif(reasoncode, '') IS NOT NULL
  UNION ALL SELECT reasoncode, reasondescription, 4 FROM raw.procedures WHERE nullif(reasoncode, '') IS NOT NULL
  UNION ALL SELECT reasoncode, reasondescription, 4 FROM raw.medications WHERE nullif(reasoncode, '') IS NOT NULL
  UNION ALL SELECT reasoncode, reasondescription, 4 FROM raw.careplans WHERE nullif(reasoncode, '') IS NOT NULL
  UNION ALL SELECT procedurecode, notes, 5 FROM raw.claim_transactions WHERE nullif(procedurecode, '') IS NOT NULL
), description_registry AS (
  SELECT code, arg_min(description, source_rank) AS description
  FROM description_candidates
  WHERE nullif(code, '') IS NOT NULL AND nullif(description, '') IS NOT NULL
  GROUP BY code
), source_rows AS (
  SELECT system AS source_code_system, code AS source_code, description AS source_description
  FROM raw.conditions
  UNION ALL
  SELECT 'CLAIM-DIAGNOSIS', c.code, d.description
  FROM claim_codes c
  LEFT JOIN description_registry d USING (code)
  WHERE NOT EXISTS (SELECT 1 FROM raw.conditions x WHERE x.code = c.code)
), concepts AS (
  SELECT source_code_system, source_code, any_value(source_description) AS source_description
  FROM source_rows
  GROUP BY source_code_system, source_code
), classified AS (
  SELECT c.*,
         internal.diagnosis_group_rule(c.source_description, c.source_code_system, c.source_code) AS rule_group
  FROM concepts c
)
SELECT
  c.source_code_system,
  c.source_code,
  c.source_description,
  coalesce(nullif(o.standard_code_system, ''),
           CASE WHEN upper(c.source_code_system) = 'ICD10' THEN 'ICD-10-CM' END) AS standard_code_system,
  coalesce(nullif(o.standard_code, ''),
           CASE WHEN upper(c.source_code_system) = 'ICD10' THEN c.source_code END) AS standard_code,
  coalesce(nullif(o.standard_description, ''),
           CASE WHEN upper(c.source_code_system) = 'ICD10' THEN c.source_description END) AS standard_description,
  coalesce(nullif(o.diagnosis_category, ''),
           internal.diagnosis_category_rule(coalesce(nullif(o.diagnosis_group, ''), c.rule_group))) AS diagnosis_category,
  coalesce(nullif(o.diagnosis_group, ''), c.rule_group) AS diagnosis_group,
  CASE WHEN o.source_code IS NOT NULL THEN 'reviewed_private_crosswalk'
       WHEN upper(c.source_code_system) = 'ICD10' THEN 'source_is_icd10_cm'
       ELSE 'source_concept_grouped_crosswalk_pending' END AS mapping_status,
  nullif(o.mapping_authority, '') AS mapping_authority,
  nullif(o.mapping_version, '') AS mapping_version,
  nullif(o.reviewed_by, '') AS reviewed_by,
  try_cast(o.reviewed_date AS DATE) AS reviewed_date
FROM classified c
LEFT JOIN internal.diagnosis_crosswalk_override o USING (source_code_system, source_code);

CREATE TABLE internal.procedure_concept_map AS
WITH source_rows AS (
  SELECT system AS source_code_system, code AS source_code, description AS source_description
  FROM raw.procedures
  UNION ALL
  SELECT 'CLAIM-LINE' AS source_code_system, procedurecode AS source_code, notes AS source_description
  FROM raw.claim_transactions
  WHERE nullif(procedurecode, '') IS NOT NULL
), concepts AS (
  SELECT source_code_system, source_code, any_value(source_description) AS source_description
  FROM source_rows
  GROUP BY source_code_system, source_code
), classified AS (
  SELECT c.*,
         internal.procedure_group_rule(c.source_description, c.source_code_system, c.source_code) AS rule_group
  FROM concepts c
)
SELECT
  c.source_code_system,
  c.source_code,
  c.source_description,
  coalesce(nullif(o.standard_code_system, ''),
           CASE WHEN upper(c.source_code_system) = 'CDT' THEN 'CDT' END) AS standard_code_system,
  coalesce(nullif(o.standard_code, ''),
           CASE WHEN upper(c.source_code_system) = 'CDT' THEN c.source_code END) AS standard_code,
  coalesce(nullif(o.standard_description, ''),
           CASE WHEN upper(c.source_code_system) = 'CDT' THEN c.source_description END) AS standard_description,
  coalesce(nullif(o.procedure_category, ''),
           internal.procedure_category_rule(coalesce(nullif(o.procedure_group, ''), c.rule_group))) AS procedure_category,
  coalesce(nullif(o.procedure_group, ''), c.rule_group) AS procedure_group,
  CASE WHEN o.source_code IS NOT NULL THEN 'reviewed_private_crosswalk'
       WHEN upper(c.source_code_system) = 'CDT' THEN 'source_is_cdt'
       ELSE 'source_concept_grouped_crosswalk_pending' END AS mapping_status,
  nullif(o.mapping_authority, '') AS mapping_authority,
  nullif(o.mapping_version, '') AS mapping_version,
  nullif(o.reviewed_by, '') AS reviewed_by,
  try_cast(o.reviewed_date AS DATE) AS reviewed_date
FROM classified c
LEFT JOIN internal.procedure_crosswalk_override o USING (source_code_system, source_code);

-- Code-only lookups support encounter reasons and claim diagnosis slots. When
-- the same literal code appears in more than one system, prefer a reviewed map,
-- then ICD-10-CM, then the most common source concept.
CREATE TABLE internal.diagnosis_code_lookup AS
WITH scored AS (
  SELECT m.*,
         CASE WHEN mapping_status = 'reviewed_private_crosswalk' THEN 1
              WHEN standard_code_system = 'ICD-10-CM' THEN 2 ELSE 3 END AS preference_rank
  FROM internal.diagnosis_concept_map m
), selected AS (
  SELECT scored.*,
         row_number() OVER (
           PARTITION BY source_code
           ORDER BY preference_rank, source_code_system
         ) AS selected_row
  FROM scored
)
SELECT * EXCLUDE (preference_rank, selected_row)
FROM selected
WHERE selected_row = 1;

CREATE TABLE internal.claim_line_procedure_lookup AS
SELECT *
FROM internal.procedure_concept_map
WHERE source_code_system = 'CLAIM-LINE';
