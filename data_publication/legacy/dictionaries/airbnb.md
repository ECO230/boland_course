# Legacy Airbnb data dictionary

Artifact: `legacy_airbnb_listings.parquet`

Grain: one listing in the original multi-city modeling file. The release adds
`legacy_source_index` as a unique row key and otherwise uses normalized
snake-case names. `listed_price` is the exponentiated nightly-price target;
`log_price` is its source natural-log value. Capacity, property type, room type,
bed type, cancellation policy, host-response indicators, review dates and
scores, city/neighborhood, ZIP code, and approximate coordinates retain their
source meanings. `amenities` is the source structured amenity list.

Listing `name`, free-text `description`, and `thumbnail_url` are excluded.
This is a historical modeling snapshot, not a current inventory or record of
bookings, occupancy, or realized demand.
