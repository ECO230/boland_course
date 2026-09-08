# Chicago Traffic Dataset Publication

This directory defines the ECO 230 Chicago traffic release for complete
calendar years 2018 through 2025. The verified release is publicly available
at `https://data.60land.com/project1/2026-fall/v1/traffic/`.

## Student-facing tables

- `chicago_traffic_crashes.parquet`: one row per crash;
- `chicago_traffic_vehicles.parquet`: one row per crash unit;
- `chicago_traffic_people.parquet`: one row per person;
- `chicago_traffic_crash_analysis.parquet`: one row per crash with time fields,
  geographic join keys, reported conditions, and numerical NOAA weather.

The release intentionally does not include Census, ACS, neighborhood,
roadway, transit, business, or other substantive contextual joins. Students
select those enrichments and explain their join grain.

## Build the local source snapshot

Use Python and install the tested DuckDB version in an isolated environment:

```powershell
python -m pip install duckdb==1.5.2
python data_publication/traffic/download_sources.py `
  --smoke-test `
  --output .data-build/traffic/chicago-smoke/source
python data_publication/traffic/smoke_test.py
python data_publication/traffic/download_sources.py
```

The downloader obtains 4,597,939 rows across the three official City of Chicago
crash, vehicle, and person tables. It separately obtains the small ZIP-region
lookup and 16 NOAA station-year files. It writes only under the Git-ignored
`.data-build/` tree.

Build and independently verify the Parquet candidate:

```powershell
python data_publication/traffic/build_release.py
python data_publication/traffic/verify_release.py
```

Both the source snapshot and published candidate carry SHA-256 manifests.
The build refuses to overwrite an existing source, work, or release directory.

## Weather interpretation

`reported_weather_condition` is the police officer's categorical assessment.
The numerical fields are airport observations. Each geocoded crash is assigned
to the nearer of O'Hare and Midway and matched to the closest standard hourly
observation within 90 minutes. NOAA LCD times are Local Standard Time; the
pipeline converts Chicago crash wall time before matching.

## Publication boundary

Nothing here uploads to `data.60land.com`. The reviewed release directory is
handed to the infrastructure project as a flat, prebuilt release bundle. It is
not loaded into the DuckDB warehouse. The infrastructure publisher validates
the metadata, checksums, byte counts, row counts, and schemas with DuckDB, then
atomically promotes the complete directory into the immutable public release
tree.

The canonical infrastructure contract is:

```text
C:\Users\bolan\Documents\infrastructure\hosts\lucretia\eco230-data\README.md
```

Transfer uses WSL `rsync` to the private
`dadmin@192.168.1.187:~/eco230-staging/` area. A root operator then copies the
bundle into `/srv/eco230-data/releases/.work/incoming/` and separately approves
publication with `./eco230-data import-release ... --approve-public-release`.
Do not write directly to the public release tree or use `docker cp`.
