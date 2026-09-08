# Chicago and Twin Cities Airbnb Dataset Candidate

This directory defines a local-only ECO 230 release candidate derived from the
June 2026 Inside Airbnb snapshots for Chicago and the Twin Cities MSA.

## Curated tables

- `airbnb_listings.parquet`: one row per listing with structured property,
  capacity, snapshot-price, review, and availability fields;
- `airbnb_calendar.parquet`: one row per listing/date with listed availability
  and stay restrictions;
- `airbnb_reviews_monthly.parquet`: monthly review counts by listing;
- `airbnb_listing_analysis.parquet`: one row per listing with snapshot-price
  benchmarks, future listed-availability summaries, stay restrictions, and
  recent-review measures.

The June 2026 calendar source does not contain daily prices. Accordingly, the
release does not claim future price, weekday/weekend price, price variability,
occupancy, bookings, or revenue. `calendar_available = false` can mean either a
booking or a host-blocked/unavailable date.

The tables share a deterministic release-local `listing_key`. Raw Airbnb
listing and host IDs, names, URLs, descriptions, photos, profile fields, raw
license values, reviewer fields, and review comments are excluded.

The source calendars include 48 listing IDs, and the reviews include 46 listing
IDs, that have no corresponding detailed-listing row in the same market
snapshot. The release excludes their 17,520 calendar rows and 6,031 review rows
because it cannot create the shared key or attach listing attributes. Exact
exclusion counts are validated so later source changes fail closed.

## Geographic design

The release includes market, state and county FIPS codes, local-area names, a
0.01-degree WGS84 grid key, and the approximate latitude/longitude supplied by
Inside Airbnb. Inside Airbnb says coordinates are displaced by up to 150 metres
from the actual address. The grid is an approximate join handle, not a Census
geography. No Census, ACS, tourism, transit, or other substantive attributes are
prejoined.

## Build locally

Use Python with DuckDB 1.5.2:

```powershell
python -m pip install duckdb==1.5.2
python data_publication/airbnb/download_sources.py
python data_publication/airbnb/smoke_test.py
python data_publication/airbnb/build_release.py
python data_publication/airbnb/verify_release.py
```

Generated source archives, the work database, and the flat release candidate
are written below `.data-build/airbnb/` and remain outside Git.

## Publication gate

This candidate must not be transferred or published. Inside Airbnb describes
the data as CC BY 4.0 but its Data Policies page also says not to republish it.
Written redistribution approval, or another documented resolution of that
conflict, is required before infrastructure handoff.
