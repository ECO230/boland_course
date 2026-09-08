# Chicago Traffic Data Dictionary

## Table grains and joins

| Table | Grain | Primary key | Important foreign keys |
|---|---|---|---|
| `crashes` | One crash | `crash_record_id` | none |
| `vehicles` | One crash unit | `crash_unit_id` | `crash_record_id` |
| `people` | One person | `person_id` | `crash_record_id`, `vehicle_id` |
| `crash_analysis` | One crash | `crash_record_id` | geographic keys described below |

A crash can have multiple vehicle/unit records and multiple person records.
Joining all three tables without first deciding the desired grain can multiply
rows. That is an analytical decision, not a defect in the data.

## Crash fields

The `crashes` table contains the event timestamp; reported weather, lighting,
road surface, road defect, traffic control, trafficway, collision, work-zone,
hit-and-run, dooring, and contributory-cause fields; address and coordinate
fields; unit and injury counts; estimated damage; and report classification.

Important fields include:

| Field | Meaning |
|---|---|
| `crash_record_id` | City-generated crash key used across all three source tables. |
| `crash_datetime` | Local Chicago date and wall-clock time reported for the crash. |
| `reported_weather_condition` | Officer-reported categorical weather. |
| `most_severe_injury` | Most serious injury reported for anyone in the crash. |
| `injuries_*` | Source crash-level counts by injury classification. |
| `unit_count` | Vehicles or other independently moving road users in the crash. |
| `primary_contributory_cause` | Officer-judged primary contributing factor. |
| `latitude`, `longitude` | Source crash coordinates; missing and source `0,0` placeholders are represented as missing. |

Boolean fields can be missing when the source did not report a yes/no value.

## Vehicle/unit fields

The `vehicles` table contains unit type, passenger and occupant counts,
vehicle make/model/year/type/use, movement and maneuver, towing, fire, speed,
first contact point, and selected commercial-vehicle classifications.
Pedestrians and pedal cyclists can be represented as units, so not every field
applies to every row.

The public source includes additional carrier, permit, towing, and hazardous
materials identifiers. Those identifiers are intentionally excluded from this
course release while useful categorical attributes are retained.

## Person fields

The `people` table contains person type, vehicle link, seat position, age, sex,
safety equipment, airbag deployment, ejection, injury classification, driver
action/vision/physical condition, pedestrian or pedal-cyclist circumstances,
BAC result, and cell-phone-use category.

Source home geography, hospital/EMS identifiers, and driver-license fields are
not included. Missing values mean unavailable or inapplicable, not zero.

## Analysis table and numerical weather

`crash_analysis` contains every crash field plus derived calendar fields and
the nearest qualifying NOAA observation from O'Hare or Midway:

| Field | Meaning |
|---|---|
| `weather_station_id`, `weather_station_name` | NOAA station selected by distance. |
| `weather_observed_lstd` | NOAA observation time in Local Standard Time. |
| `weather_station_distance_miles` | Approximate crash-to-station distance. |
| `weather_time_difference_minutes` | Absolute time difference from the converted crash time. |
| `temperature_f`, `dew_point_f` | Air and dew-point temperatures in Fahrenheit. |
| `relative_humidity_percent` | Relative humidity percentage. |
| `precipitation_inches`, `precipitation_trace` | Measured precipitation and trace indicator. |
| `visibility_miles` | Station visibility in miles. |
| `wind_direction_degrees`, `wind_speed_mph`, `wind_gust_mph` | Station wind measurements. |
| `station_pressure_inches`, `sea_level_pressure_inches` | Atmospheric pressure measurements. |
| `observed_weather_type`, `observed_sky_conditions` | NOAA observation codes/text. |
| `crash_year`, `crash_month`, `crash_weekday`, `crash_hour` | Derived time parts. |
| `time_period` | Broad course convenience category based on crash hour. |

NOAA LCD does not use daylight saving time. Crash timestamps are converted
from Chicago local wall time to Central Standard Time before selecting the
closest observation within 90 minutes. Airport weather is contextual and may
not reproduce the condition at the exact crash location.

## Geographic join keys

| Field | Join use |
|---|---|
| `state_fips` | Illinois FIPS code (`17`). |
| `city_geoid` | Census place GEOID for Chicago (`1714000`). |
| `zip5` | ZIP-region key supplied through Chicago's boundary lookup. |
| `police_beat` | Four-character Chicago Police beat identifier. |
| `latitude`, `longitude` | Spatial joins to tracts, community areas, wards, roads, transit, or other boundaries. |
| `zip_region_id` | Chicago Data Portal's internal ZIP polygon feature identifier. |

No demographic or economic values are attached. Students should identify a
compatible geography and vintage, verify join cardinality, and explain why the
chosen enrichment answers their research question.
