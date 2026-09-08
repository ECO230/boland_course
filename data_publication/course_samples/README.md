# Early-homework CSV extracts

This pipeline creates the smaller, flat teaching files intended for Homework
1-4. Students receive the finished `.csv` files through an assignment project;
they do not run this builder or connect to the Parquet publication service.

## Extracts

| File | Grain | Limited scope | Rows |
|---|---|---|---:|
| `traffic_chicago_2024.csv` | One crash | Chicago, calendar year 2024 | 15,000 |
| `airline_midwest_2024.csv` | One flight | 2024 departures from ORD, MDW, MKE, or MSP | 15,000 |
| `olist_southeast_2017.csv` | One delivered order | SP, RJ, and MG during 2017 | 15,000 |
| `nfl_plays_2024.csv` | One play | 2024 regular season | 15,000 |
| `health_wisconsin_appointments_2023_2025.csv` | One scheduled appointment | Synthetic Wisconsin patients, 2023-2025 | 15,000 |

Each file includes useful categorical, quantitative, date/time, logical, and
geographic fields for data-anatomy, descriptive-statistics, visualization, and
story-planning work. The health file is intentionally flattened to appointment
grain and includes compatible patient and satisfaction fields.

## Determinism

Every query filters to its documented scope, orders eligible records by
`MD5(primary key + 'eco230-early-homework-v1')`, and takes the first 15,000
rows. The same approved release artifacts therefore produce the same records
and checksums on every build. Changing the scope or sampling salt creates a new
extract version rather than silently changing this one.

## Build

Run from the repository root with the publishing Python environment:

```powershell
& ".data-build\publication-venv\Scripts\python.exe" `
  "data_publication\course_samples\build_samples.py"
```

Outputs are written to `shared/data/early-homework-v1/`. The generated
`manifest.json` records scope, grain, row and column counts, byte sizes,
checksums, source artifacts, and the sampling rule. Pass `--force` to
atomically replace an existing build, or repeat `--sample` to build selected
files only.

The CSV files and manifest are tracked in Git so every Homework 1-4 project is
built from the same reviewed version. The approved source Parquet files remain
under `.data-build/` and outside Git.
