# ECO 230 Data Publication Workflow

This guide defines how approved course datasets are transformed and handed to
the public data service at `https://data.60land.com`.

## Architecture

The course repository owns version-controlled metadata, SQL, validation,
licensing notices, data dictionaries, and student examples. It does not own the
large source datasets, generated DuckDB databases, generated Parquet files, or
server credentials.

```text
local approved source
  -> course-repo metadata and SQL
  -> local unpublished Parquet candidate
  -> private SSH staging on Lucretia
  -> hidden publisher staging under releases/.work/
  -> DuckDB-verified atomic import
  -> immutable data.60land.com release
  -> Posit Cloud project download/cache
```

The public infrastructure repository is `/opt/infrastructure` on the server.
The server-side course checkout is `/data/junior/boland_course`. Infrastructure
deployment details remain in the infrastructure repository.

## Repository locations

- tracked publication definitions: `data_publication/`;
- generated local candidates: `.data-build/`;
- legacy local project exports and source archives: `posit_group/`;
- student-facing project integration: generated from project manifests after a
  public artifact has been verified.

`.data-build/`, `posit_group/`, source files, DuckDB databases, and credentials
must remain ignored by Git.

## Versioning

Public artifacts are immutable. Use paths such as:

```text
project1/2026-fall/v1/traffic/chicago_traffic_crash_analysis.parquet
```

Never replace an existing public artifact in place. Corrections or transformation
changes require a new version. Student projects pin an exact version and must not
depend on an unversioned `latest` alias.

## Dataset definition contract

Each dataset directory should include:

- `dataset.json`: source, grain, primary key, join keys, license, privacy review,
  release version, and intended artifact path;
- `LICENSE-DATA.txt`: redistribution terms and attribution;
- `data_dictionary.md`: student-facing fields and derived-variable definitions;
- `sql/10_ingest.sql`: explicit source ingestion;
- `sql/20_model.sql`: stable types, names, and instructor-provided derivations;
- `sql/30_validate.sql`: fail-closed release assertions;
- `sql/40_publish.sql`: deterministic Parquet export;
- local build and independent verification scripts;
- a minimal student-facing R download/read example.

## Reference implementation

Traffic is the first reference dataset:

- dataset ID: `chicago_traffic_crashes_2018_2025`;
- release: `2026-fall-v1`;
- complete-year coverage: 2018 through 2025;
- tested publisher: DuckDB 1.5.2;
- official sources: City of Chicago crash, vehicle, person, and ZIP-boundary
  tables plus NOAA Local Climatological Data;
- output: three normalized source tables and one crash-grain analysis table;
- tracked definition: `data_publication/traffic/`.

The source snapshot is created directly from official APIs. The generated
`source_manifest.json` records URLs, retrieval time, row counts, byte sizes,
and SHA-256 values. Later upstream corrections do not change an existing
course release; they require a new version.

Airbnb is a second local reference pipeline under `data_publication/airbnb/`.
It uses the June 2026 Chicago and Twin Cities MSA Inside Airbnb snapshots and
produces listings, availability-only calendar, monthly-review, and listing-
analysis tables. The June calendars contain no daily prices, so future-price,
weekday/weekend-price, occupancy, booking, and revenue measures are explicitly
excluded. The local candidate can be built and verified, but it must not enter
the infrastructure handoff while Inside Airbnb's no-republication policy is
unresolved.

Olist is a verified relational marketplace pipeline under
`data_publication/olist/`. It combines Olist's anonymized commerce and sampled
marketing-funnel datasets under CC BY-NC-SA 4.0. The release excludes review
text and source identifiers, preserves normalized grains for student joins,
and adds a carefully aggregated one-row-per-order analysis table.

Airline is a verified federal-data pipeline under `data_publication/airline/`.
It combines all 2024 BTS Marketing Carrier On-Time Performance months with the
four quarterly DB1B Market files. The release retains flight-level operations
but aggregates the 10 percent ticket sample to directional
route-carrier-quarter pricing and demand summaries. It also supplies airport
and carrier-month tables. Flight operations and DB1B prices are intentionally
not row-level joinable.

Health is a verified and redistribution-approved fully synthetic pipeline under
`data_publication/health/`.
It uses pinned Synthea 4.0.0 and portable Java artifacts to generate a fixed-
seed Wisconsin-Minnesota population through December 31, 2025. The curated
release preserves longitudinal clinical, payer-coverage, and claim-transaction
grains while excluding names, identifier-like fields, exact locations, source
UUIDs, and free text. Deterministic appointment/no-show and patient-satisfaction
tables are labeled course-generated extensions rather than native Synthea
output. Its findings describe a simulation and must not be
presented as evidence about real patients or clinical effectiveness. SNOMED CT,
CDT, CPT, HCPCS, ICD-10-CM, and private crosswalk content remain confined to
the ignored source/work layers. Public tables use independent ECO 230 groups
for those concepts and retain only audited LOINC, NLM-created RxNorm, and CDC
CVX fields with their required notices.

The four full legacy Posit Cloud project files have a compatibility-backup
pipeline under `data_publication/legacy/`. The traffic and CMS hospital bundles
are approved for public handoff. The Airbnb bundle remains private because its
Kaggle metadata reports an unknown license, and the NFL bundle remains private
because the Big Data Bowl rules prohibit republication to people who have not
accepted the competition rules.

ZIP Reference is a common-resource pipeline under
`data_publication/zip_reference/`. It extracts seven approved ZIP-level files
from the ignored instructor utility archive and produces geography and
community-context Parquet tables with a shared, unique `zip5` key. Students use
the tables through `arrow::read_parquet()` and `dplyr::left_join()`; DuckDB and
SQL are build-time implementation details only. The local candidate must not be
handed to the public publisher until the internal extracts' provenance and
redistribution rights are documented.

## Local candidate build

Use an isolated Python environment with the same pinned DuckDB release used by
the infrastructure publisher. Build into the ignored directory:

```powershell
python data_publication/traffic/download_sources.py
python data_publication/traffic/build_release.py
```

Then verify the finished artifact independently:

```powershell
python data_publication/traffic/verify_release.py
```

Equivalent dataset-specific commands are documented in each dataset's
`README.md`. Olist and Airline each produce a flat, independently verified
local-candidate bundle under `.data-build/`.

The Traffic release must report:

- a unique primary key in every table;
- complete-year crash coverage from January 1, 2018 through December 31, 2025;
- no vehicle or person record that references a missing crash;
- exactly one analysis row for every crash;
- at least 90 percent ZIP-key and NOAA-weather match coverage;
- valid coordinate and numerical-weather ranges;
- matching Parquet SHA-256 values in generated metadata and checksum files.

## Infrastructure handoff

The canonical infrastructure contract is:

```text
C:\Users\bolan\Documents\infrastructure\hosts\lucretia\eco230-data\README.md
```

The generated release is a prebuilt bundle. It is not loaded into the DuckDB
warehouse. The infrastructure publisher validates every Parquet file with
DuckDB and atomically promotes the complete directory into the public release
tree.

The bundle must be flat, with no nested directories or symbolic links, and
contain exactly:

- `metadata.json` with `schema_version: 2`,
  `release_status: local-candidate`, and a truthful
  `privacy.contains_student_data: false` declaration;
- `schemas.json` and `source_manifest.json`;
- `DATA-DICTIONARY.md` and `LICENSE-DATA.txt`;
- every declared Parquet artifact;
- one matching `<artifact>.parquet.sha256` sidecar for every artifact.

Each artifact entry in `metadata.json` must include its common versioned public
directory, SHA-256 value, byte count, row count, and schema. Windows CRLF line
endings in checksum sidecars are accepted; verify them without modifying the
bundle by removing carriage returns in the validation stream.

Transfer only the completed bundle. Do not transfer the source CSV files,
DuckDB work database, credentials, or restricted raw data. Do not write
directly into `/srv/eco230-data/releases/`, use `docker cp`, or interact with
the static container.

From WSL on the course workstation, use a unique release name:

```bash
set -euo pipefail

release="/mnt/c/Users/bolan/Documents/boland_course/.data-build/traffic/chicago-2018-2025-v1/release"
release_name="chicago-traffic-2026-fall-v1"

test -d "$release"
cd "$release"
for checksum in *.parquet.sha256; do
  tr -d '\r' < "$checksum" | sha256sum --check -
done

ssh dadmin@192.168.1.187 "mkdir -p ~/eco230-staging/$release_name"
rsync -a --partial --append-verify --info=progress2 \
  "$release/" \
  "dadmin@192.168.1.187:~/eco230-staging/$release_name/"
```

On Lucretia, a root operator validates the uploaded copy and places it in the
hidden importer staging directory on the release filesystem:

```bash
set -euo pipefail

release_name="chicago-traffic-2026-fall-v1"
source_dir="/home/dadmin/eco230-staging/$release_name"
stage_dir="/srv/eco230-data/releases/.work/incoming/$release_name"

test -d "$source_dir"
test ! -e "$stage_dir"
cd "$source_dir"
for checksum in *.parquet.sha256; do
  tr -d '\r' < "$checksum" | sha256sum --check -
done

install -d -m 0755 -o 10001 -g 10001 \
  /srv/eco230-data/releases/.work/incoming
cp -a "$source_dir" "$stage_dir"
chown -R 10001:10001 "$stage_dir"
find "$stage_dir" -type d -exec chmod 0755 {} +
find "$stage_dir" -type f -exec chmod 0644 {} +
```

Public promotion is a separate, explicit approval action run from
`/opt/infrastructure/hosts/lucretia/eco230-data`:

```bash
./eco230-data import-release \
  /data/releases/.work/incoming/chicago-traffic-2026-fall-v1 \
  --approve-public-release
```

`import-release` verifies the metadata and sidecar checksums, byte counts,
DuckDB row counts, and schemas. It rejects extra files, symbolic links, path
traversal, inconsistent target directories, missing public approval, and an
existing immutable release. It preserves the candidate metadata, writes
`publication.json`, and updates `catalog.json`. No Docker, Nginx, or proxy
reload is required.

The infrastructure workflow must independently validate and atomically publish
the release. After publication:

1. download the public artifact by its versioned URL;
2. verify its SHA-256 against the published metadata;
3. query it and confirm row count, key uniqueness, schema, and coverage;
4. test the R example from a fresh environment;
5. only then update group-project manifests or setup scripts to use it.

## License and privacy gate

Do not publish a dataset without documented redistribution terms. Attribution,
noncommercial restrictions, and share-alike requirements travel with adapted
artifacts. Source and adapted-dataset citations must be included with the
release.

Do not publish student records, survey exports, credentials, LimeSurvey data,
MariaDB content, or other restricted records without explicit approval and a
separate privacy review.

## Release checklist

- source file identity matches the approved SHA-256;
- source, vintage, license, and citations are documented;
- grain, primary key, and join keys are explicit;
- data types and derived fields are defined in SQL;
- validation fails on row loss, duplicate keys, invalid ranges, or changed
  coverage;
- Parquet is compressed and deterministic;
- generated artifact, source data, database, and credentials are ignored;
- metadata, checksum, schema, dictionary, and license are present;
- local independent verification passes;
- infrastructure-side verification passes;
- public download and Posit Cloud test pass before student release.
