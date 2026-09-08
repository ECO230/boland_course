# U.S. Airline Marketplace 2024 Data Dictionary

## Analytical cautions

- DB1B is a 10 percent ticket sample, not a complete passenger census.
  `estimated_passengers` is the simple sample expansion and is not a substitute
  for T-100 when exact carrier traffic is required.
- A DB1B market is one directional origin-destination portion of an itinerary.
  It is not necessarily a nonstop flight.
- Ticketing, marketing, and operating carriers have different meanings.
- Flight-delay causes are reported only for qualifying delayed flights. Missing
  delay-cause values are not evidence of zero causal contribution on every
  flight.
- Canceled flights lack realized arrival times and arrival delays. Excluding
  them can make service quality look better.
- Actual times, cancellation, diversion, and delay causes occur after departure
  and create leakage in prediction models intended to run before a flight.
- DOT consumer submissions are not included in this version. Operational
  quality should not be described as a direct customer-satisfaction survey.

## `airline_flights.parquet`

One row per BTS marketing-carrier flight. `flight_key` is release-local. Date,
carrier, flight number, origin/destination, scheduled and actual HHMM times,
delay measures, cancellation/diversion, elapsed time, distance, and reported
delay causes are retained. `aircraft_key` replaces the raw tail number and can
support studies of cascading late-aircraft delays.

`source_duplicate_sequence` distinguishes source rows that share the natural
schedule key. It should not be interpreted as flight order.

## `airline_airports.parquet`

One row per BTS airport ID, including airport code, represented city/state, and
the number of origin or destination endpoints in the flight table.

## `airline_route_quarter.parquet`

One row per directional airport pair, ticketing carrier, and quarter. It
contains source-record count, sampled and simply expanded passenger counts,
passenger-weighted fares, bulk-fare share, distance, coupon count, and nonstop
share. Fare is the DB1B market fare, not necessarily the total round-trip price.

## `airline_carrier_month.parquet`

One row per marketing carrier and month. It contains scheduled, completed,
canceled, diverted, and 15-minute-delayed flight counts; mean arrival delay;
delay/cancellation rates; and reported delay-cause minute totals.
