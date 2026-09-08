# Legacy Project Data Backups

This pipeline converts the four full legacy Posit Cloud group-project files to
flat, versioned Parquet release bundles. It does not use the smaller classroom
subsets in `shared/data/`.

The output is retained only as a compatibility backup. New group projects use
the current Traffic, Airbnb, Health, and NFL publication pipelines instead.

Build and independently verify all four candidates with DuckDB 1.5.2:

```powershell
& .data-build/publication-venv/Scripts/python.exe data_publication/legacy/build_releases.py

& .data-build/publication-venv/Scripts/python.exe data_publication/legacy/verify_releases.py `
  .data-build/legacy/legacy-airbnb-2026-fall-v1/release `
  .data-build/legacy/legacy-traffic-2026-fall-v1/release `
  .data-build/legacy/legacy-hospital-2026-fall-v1/release `
  .data-build/legacy/legacy-nfl-2026-fall-v1/release
```

Each output directory contains exactly one Parquet artifact, its checksum
sidecar, metadata, schema, source manifest, data dictionary, and license notice.

Licensing is fail-closed:

- Traffic is approved for this noncommercial educational service under
  CC BY-NC-SA 4.0 with attribution and share-alike terms.
- Hospital is the CMS FY2011 inpatient public-use file and is approved.
- Airbnb is not approved because its Kaggle metadata says `Unknown` license.
- NFL is not approved because the competition rules prohibit republication of
  the competition data to people who have not accepted those rules.

The blocked bundles may be transferred to private operator staging for backup,
but must not be passed to `import-release --approve-public-release`.

After public import, `student_examples.R` provides minimal Arrow examples for
the approved traffic and hospital artifacts.
