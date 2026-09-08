# Legacy traffic data dictionary

Artifact: `legacy_us_accidents_100k.parquet`

Grain: one sampled traffic incident. `legacy_source_index` retains the sampled
source row index and is the release key; `id` is the source incident ID.
Start/end timestamps and coordinates describe the reported traffic event.
`severity` measures the reported impact on traffic flow (1 least, 4 greatest),
not injuries. Weather fields describe a nearby station observation. Boolean
road-feature fields indicate nearby mapped infrastructure. Twilight fields are
categorical day/night calculations.

Column names are normalized to snake case. This 100,000-row sample is neither
a complete crash census nor a random sample guaranteed to represent the U.S.
