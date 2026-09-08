# ECO 230 Data Publication Sources

This directory contains version-controlled metadata, SQL, validation, and
examples for datasets published through `https://data.60land.com`.

It does not contain source datasets, DuckDB database files, generated Parquet
files, or credentials. Local build products belong under `.data-build/`, and
the original source archives belong outside Git.

Each dataset directory should contain:

- stable metadata and licensing information;
- explicit ingestion and modeling SQL;
- validation SQL that fails closed;
- deterministic Parquet publishing SQL;
- a build or handoff script;
- a minimal student-facing R example.

Published artifacts are immutable. Corrections require a new version rather
than replacement of an existing version.

Current tracked definitions include:

- `traffic/`: published Chicago traffic release pipeline;
- `airbnb/`: verified local Chicago and Twin Cities June 2026 candidate with
  listings, availability-only calendars, monthly reviews, and listing analysis.
  Its public redistribution gate remains closed pending resolution of Inside
  Airbnb's no-republication policy;
- `olist/`: published Olist marketplace release with normalized orders,
  items, payments, products, sellers, review-score summaries, marketing leads,
  and a one-row-per-order analysis table;
- `airline/`: published 2024 U.S. airline marketplace release with 7.5 million
  flight records, airport keys, DB1B route-quarter fare/demand summaries, and
  carrier-month operational-quality measures;
- `nfl/`: published 2021-2025 NFL release with games, plays, player and team
  game statistics, player attributes, rosters, and team reference data;
- `health/`: verified and redistribution-approved fully synthetic
  Wisconsin-Minnesota longitudinal EHR candidate generated with Synthea 4.0.0
  plus deterministic course-generated
  appointment/no-show and patient-satisfaction extensions. It preserves
  clinical, operational, coverage, and claim transaction grains while removing
  all identifier-like patient fields, source UUIDs, exact locations, and free
  text;
- `legacy/`: compatibility-backup conversion for the four full legacy Posit
  Cloud project files. Traffic and CMS hospital are approved for public
  handoff; Airbnb and NFL build and verify locally but remain private because
  their source terms do not authorize public redistribution.
- `zip_reference/`: clean ZIP-keyed geography and community-context reference
  tables built from existing instructor utility extracts for simple R joins.
  The local candidate is verified separately, but public redistribution remains
  gated until the extracts' source provenance and license are documented.
- `course_samples/`: deterministic, instructor-run SQL and Python pipeline for
  the 15,000-row `.csv` extracts used in Homework 1-4. These files are built
  from approved local release artifacts and distributed through the assignment
  projects; students do not connect to the publication service directly.

As of September 6, 2026, Traffic, Airline, NFL, and Olist are publicly
available under `https://data.60land.com/project1/2026-fall/v1/`. The server
preserves each imported bundle's `local-candidate` metadata and records the
public promotion separately in `publication.json` and the service catalog.

See `docs/DATA_PUBLICATION_WORKFLOW.md` for the release contract and
infrastructure handoff checklist.
