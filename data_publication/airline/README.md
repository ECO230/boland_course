# U.S. Airline Marketplace 2024

This directory defines an ECO 230 release combining two official Bureau of
Transportation Statistics sources for calendar year 2024:

- Marketing Carrier On-Time Performance, one row per scheduled domestic
  flight; and
- DB1B Market, the quarterly 10 percent sample of domestic airline ticket
  markets with fares and sampled passenger counts.

## Curated tables

- `airline_flights.parquet`: selected flight-level schedule, route, operating
  carrier, completion, delay, cancellation, and delay-cause fields;
- `airline_airports.parquet`: one row per airport represented in the flights;
- `airline_route_quarter.parquet`: ticket-sample passenger and fare summaries
  by directional route, ticketing carrier, year, and quarter;
- `airline_carrier_month.parquet`: monthly operational-quality summaries by
  marketing carrier.

The tables are intentionally not flattened. Ticket prices are observed at a
different grain from flight operations, and students must aggregate to a
defensible common grain before joining them.

The release measures operational customer experience, not subjective
satisfaction. DOT consumer-submission data can be added in a later immutable
version after its report tables receive a separate extraction and validation
pipeline.

## Published release

The verified release was published on September 6, 2026, at:

```text
https://data.60land.com/project1/2026-fall/v1/airline/
```

## Build locally

The full source download is large: twelve monthly on-time archives plus four
quarterly DB1B archives. The build extracts source CSV files only into the
ignored work directory.

```powershell
python data_publication/airline/download_sources.py
python data_publication/airline/build_release.py
python data_publication/airline/verify_release.py
```

Generated archives, extracted CSV files, DuckDB databases, and Parquet files
remain under `.data-build/airline/` and outside Git.
