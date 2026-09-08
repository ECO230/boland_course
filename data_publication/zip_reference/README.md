# U.S. ZIP Reference Tables

This pipeline turns the useful ZIP-level files already present in
`posit_group/Group_Utilities.zip` into two small, clean Parquet reference
tables for R. It does not replace the supplied ZIP assignments with Census
ZCTAs.

## Student-facing tables

- `us_zip_geography.parquet`: one row per ZIP with city, state, county, Census
  region/division, CBSA/CSA, and representative coordinates;
- `us_zip_context.parquet`: one row per ZIP with population, median-age,
  commute, density, housing-vintage, and RUCA fields.

Students use `arrow::read_parquet()` and `dplyr::left_join()`. They do not need
DuckDB, SQL, a Census API key, or access to the source archive.

## Build locally

The source preparation step extracts exactly seven approved CSV files. It does
not extract the archive's RStudio state, scripts, credentials, or unrelated
project data.

```powershell
$buildPython = ".data-build/publication-venv/Scripts/python.exe"

& $buildPython data_publication/zip_reference/extract_sources.py
& $buildPython data_publication/zip_reference/build_release.py
& $buildPython data_publication/zip_reference/verify_release.py
```

Generated files remain under `.data-build/zip-reference/` and outside Git.
Every published artifact is sorted by `zip5`; both tables have exactly the same
unique ZIP keys.

## Publication gate

The selected fields are aggregate reference data with no student or personal
records. However, the source archive does not include redistribution terms,
and related query files identify internal `dim.Geography` and `PlanMktIntel`
sources. The resulting bundle is therefore a local candidate only. Do not
transfer it to the public publisher until source ownership, attribution, and
redistribution rights are documented.

See `LICENSE-DATA.txt` for the full release notice.
