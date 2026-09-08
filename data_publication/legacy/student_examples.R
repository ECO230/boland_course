# Legacy compatibility datasets. Prefer the current project datasets for new work.
library(arrow)

legacy_traffic <- read_parquet(
  "https://data.60land.com/project1/2026-fall/v1/legacy-traffic/legacy_us_accidents_100k.parquet"
)

legacy_hospital <- read_parquet(
  "https://data.60land.com/project1/2026-fall/v1/legacy-hospital/legacy_cms_inpatient_charges_fy2011.parquet"
)

# The legacy Airbnb and NFL bundles are private backups and intentionally have
# no public download examples because their source terms do not authorize
# redistribution.
