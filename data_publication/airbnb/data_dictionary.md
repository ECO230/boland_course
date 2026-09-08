# Chicago and Twin Cities Airbnb Data Dictionary

## Table grains and joins

| Table | Grain | Primary key |
|---|---|---|
| `listings` | One listing in one market snapshot | `listing_key` |
| `calendar` | One listing and future date | `listing_key`, `calendar_date` |
| `reviews_monthly` | One listing and review month | `listing_key`, `review_month` |
| `listing_analysis` | One listing with derived summaries | `listing_key` |

`listing_key` is a deterministic identifier created for this release. It joins
the four course tables but is not the source Airbnb listing ID. Raw listing and
host IDs are excluded.

Inside Airbnb's source tables are not perfectly synchronized. Forty-eight
calendar-only listing IDs and 46 review-only listing IDs have no detailed
listing row in the same market snapshot. Their 17,520 calendar rows and 6,031
review rows are excluded because no curated listing key or attributes can be
created. Raw excluded IDs are not published.

Joining `listings` directly to daily calendar or monthly review rows changes
the grain and repeats listing values. Students should choose a target grain and
aggregate before joining when appropriate.

## Listings

The listings table contains:

- market, snapshot, and geographic fields;
- property and room type, capacity, bathroom, bedroom, bed, and amenity counts;
- `snapshot_price_usd`, the listing price observed at scrape time;
- minimum and maximum stay settings;
- source-reported future availability counts for 30, 60, 90, and 365 days;
- review counts, dates, and component scores;
- instant-booking status and source-calculated counts of listings associated
  with the same operator;
- `license_value_reported`, which records only whether the source field was
  populated. The raw value is excluded.

Names, descriptions, URLs, photos, raw amenities, raw license values, and host
profile fields are excluded. A missing value is unknown or unavailable, not
zero. `snapshot_price_usd` is an advertised point-in-time listing price, not a
transaction price and not a complete future price schedule.

## Calendar

Each row describes one listing/date in the forward calendar visible at the
snapshot. `minimum_nights` and `maximum_nights` are stay restrictions reported
for that date. Weekday fields are derived from `calendar_date`, with Saturday
and Sunday classified as weekends.

The June 2026 Inside Airbnb calendar files do not contain price fields. The
course table therefore contains no daily price, adjusted price, future price,
weekday/weekend price, or price-variability measure.

### Critical availability limitation

`calendar_available = false` means that the source calendar did not show the
date as available. It does **not** establish that the night was booked. A host
can block a date, pause availability, use another booking channel, or otherwise
make a date unavailable. Do not label unavailable nights as bookings, occupancy,
or revenue without an explicit model and clearly stated assumptions.

## Monthly reviews

`reviews_monthly` counts review records by listing and calendar month. It does
not include review IDs, reviewer IDs or names, or comments. Reviews are an
imperfect activity measure: not every stay produces a review, and the table only
contains history for listings still represented in the June 2026 snapshot.

## Listing analysis

`listing_analysis` contains every curated listing field plus:

| Field | Meaning |
|---|---|
| `snapshot_price_per_guest_usd` | Snapshot listing price divided by `accommodates`, when positive. |
| `snapshot_price_per_bedroom_usd` | Snapshot listing price divided by bedrooms, when positive. |
| `market_room_type_median_price_usd` | Median snapshot price for the listing's market and room type. |
| `market_room_type_priced_listings` | Nonmissing prices contributing to that benchmark. |
| `snapshot_price_vs_market_room_type_median_usd` | Listing price minus its market/room-type median. |
| `snapshot_price_vs_market_room_type_median_rate` | Relative difference from the market/room-type median. |
| `future_calendar_start_date`, `future_calendar_end_date` | First and last future dates represented. |
| `future_calendar_days` | Number of calendar rows available for the listing. |
| `future_days_listed_available` | Dates the source calendar showed as available. |
| `future_listed_availability_rate` | Share of future calendar rows shown as available; not occupancy. |
| `weekday_listed_availability_rate`, `weekend_listed_availability_rate` | Listed-availability shares by day type; not booking rates. |
| `listed_availability_rate_30d`, `listed_availability_rate_90d`, `listed_availability_rate_365d` | Listed-availability shares in forward windows from the market snapshot. |
| `minimum_calendar_minimum_nights`, `median_calendar_minimum_nights`, `maximum_calendar_minimum_nights` | Summary of date-specific minimum-stay rules. |
| `distinct_minimum_night_settings` | Number of distinct minimum-stay values in the calendar. |
| `source_review_count` | Detailed review rows present for this listing. |
| `review_count_30d`, `review_count_90d`, `review_count_365d` | Review rows in trailing windows ending on the listing's scrape date. |
| `source_first_review_date`, `source_last_review_date` | Earliest and latest detailed review dates found for the listing. |

Price benchmarks are cross-sectional advertised prices. Taxes, fees, discounts,
cleaning charges, length-of-stay effects, and actual payments are not observed.

## Geographic keys and location limits

| Field | Join use |
|---|---|
| `market_id` | Stable course key: `chicago` or `twin_cities_msa`. |
| `state_fips` | Two-character state FIPS code. Twin Cities rows can be Minnesota (`27`) or Wisconsin (`55`). |
| `county_fips` | Five-character county GEOID. Twin Cities source areas are county names; Chicago rows are assigned Cook County (`17031`). |
| `county_name` | County label corresponding to `county_fips`. |
| `local_area_type`, `local_area_name` | Chicago community area or Twin Cities county from the source. |
| `grid_0_01deg` | WGS84 0.01-degree cell generated from the approximate coordinates. It is not a Census grid or tract. |
| `latitude`, `longitude` | Approximate source coordinates for optional spatial joins. |

Inside Airbnb documents that listing coordinates are displaced by up to 150
metres from the actual address. Near-boundary listings can therefore join to
the wrong polygon, tract, county, or grid cell. Buffer or sensitivity checks are
appropriate for small-area analysis.

The release deliberately uses a grid key instead of asserting Census tracts.
Students may perform a spatial tract join when relevant, but should select a
boundary vintage, document the displacement limitation, and check unmatched and
boundary-sensitive records.

## Snapshot and survivorship limits

The two market snapshots have different dates: Chicago is 2026-06-24 and the
Twin Cities MSA is 2026-06-27. This is not a complete historical panel. Listings
removed before those snapshots are absent even if they operated in earlier
years. Historical review months therefore describe surviving snapshot listings,
not the complete past market.
