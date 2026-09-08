# U.S. ZIP Geography and Community Context

These are small reference tables intended for `dplyr::left_join()` in R. ZIP
keys are character strings so leading zeroes are preserved. The files use the
ZIP assignments supplied in the existing ECO 230 utilities rather than
substituting Census ZCTAs.

## `us_zip_geography.parquet`

One row per `zip5` after removing the archive's repeated `00000` placeholder.

| Field | Meaning |
|---|---|
| `zip5` | Five-character ZIP join key |
| `city_name` | Supplied ZIP city name |
| `state_abbreviation` | Two-character state or postal-area abbreviation |
| `state_name` | State or postal-area name |
| `state_fips` | Two-character state FIPS where a valid numeric code is available |
| `county_fips` | Five-character state-plus-county FIPS where available |
| `county_name` | Supplied county name |
| `census_region` | Census region label |
| `census_division` | Census division label |
| `centroid_latitude` | Supplied representative latitude; zero placeholders become missing |
| `centroid_longitude` | Supplied representative longitude; zero placeholders become missing |
| `cbsa_fips` | Core-Based Statistical Area code where available |
| `metro_division_fips` | Metropolitan division code where available |
| `csa_fips` | Combined Statistical Area code where available |
| `cbsa_name` | CBSA name |
| `cbsa_type` | Metropolitan or micropolitan classification |
| `metro_division_name` | Metropolitan division name |
| `csa_name` | Combined Statistical Area name |
| `central_outlying_status` | Supplied central/outlying classification |

The location fields are representative ZIP attributes, not household or
property coordinates.

## `us_zip_context.parquet`

One row for every `zip5` in the geography table. Missing values indicate that
the corresponding source extract did not cover that ZIP.

| Field | Meaning |
|---|---|
| `zip5` | Five-character ZIP join key |
| `population_2023` | Supplied 2023 population estimate |
| `median_age_group_2023` | Supplied 2023 median-age category |
| `median_commute_time_category` | Supplied median commute-time category; source vintage was not documented |
| `population_2010` | Population used by the supplied density extract |
| `land_square_miles` | Land area used by the supplied density extract |
| `population_density_per_square_mile_2010` | Supplied population per land square mile |
| `median_housing_vintage_category_2022` | Category containing the supplied 2022 median housing vintage |
| `ruca_state_abbreviation_2010` | State abbreviation in the supplied RUCA file |
| `ruca_zip_type_2010` | ZIP-area type in the supplied RUCA file |
| `ruca_primary_code_2010` | Primary 2010 Rural-Urban Commuting Area code |
| `ruca_secondary_code_2010` | Secondary 2010 Rural-Urban Commuting Area code |

## Joining in R

Normalize the project ZIP to a padded character field and inspect join coverage:

```r
analysis_data <- project_data |>
  dplyr::mutate(
    zip5 = stringr::str_pad(as.character(zipcode), 5, pad = "0")
  ) |>
  dplyr::left_join(zip_context, by = "zip5")

analysis_data |>
  dplyr::summarise(
    rows = dplyr::n(),
    matched_population = sum(!is.na(population_2023)),
    unmatched_population = sum(is.na(population_2023))
  )
```

Do not convert `zip5`, `county_fips`, or other geographic identifiers to
numbers; doing so removes meaningful leading zeroes.
