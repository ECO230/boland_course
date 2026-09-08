# NFL 2021-2025 course-data pipeline

This pipeline replaces the narrow 2022 Big Data Bowl special-teams extract with
a broad, recent nflverse release suitable for ECO 230. It covers complete 2021-
2025 regular seasons and postseasons and intentionally excludes frame-by-frame
tracking data.

The current `shared/data/nfl_special_MasonCrosby.csv` is only 444 rows and is
still referenced by ten course files across Weeks 2, 3, 6, 7, 8, and 9. Do not
overwrite it in place. Publish and test this new schema first, then update those
lessons and assignment manifests deliberately.

## Source interface

`nflreadpy` is the maintained Python successor to the archived `nfl_data_py`
package. This pipeline downloads the same canonical nflverse-data Parquet
assets directly so the raw bytes, URLs, sizes, and SHA-256 values can be frozen
in `source_manifest.json`.

## Build

Use DuckDB 1.5.2, matching the course release pipeline:

```powershell
python data_publication/nfl/download_sources.py
python data_publication/nfl/build_release.py
python data_publication/nfl/verify_release.py
python data_publication/nfl/smoke_test.py
```

The flat local candidate is created at:

```text
.data-build/nfl/nfl-complete-2021-2025-v1/release/
```

The verified release was published on September 6, 2026, at:

```text
https://data.60land.com/project1/2026-fall/v1/nfl/
```

## Course migration

Update the ten legacy references rather than silently
changing the old CSV schema. Assignment projects should download only the
Parquet tables needed by the selected question; they do not need all seven
artifacts. The play table remains the natural replacement for the Crosby file,
while the other tables enable player, team, roster, age, school, body-size,
scoring, efficiency, and game-context questions.
