# Legacy hospital data dictionary

Artifact: `legacy_cms_inpatient_charges_fy2011.parquet`

Grain: one hospital and MS-DRG combination with more than 10 discharges in the
FY2011 CMS public-use file. `legacy_source_index` is a generated row key.
Provider fields identify the billing hospital and location. `drg_definition`
contains the MS-DRG code and description. `total_discharges` is the released
count. Average covered charges, total payments, and Medicare payments are
stored as `DECIMAL(18,2)` rather than currency-formatted text.

The source covers only FY2011's top 100 most frequently billed MS-DRGs and does
not describe all hospital services or non-Medicare patients.
