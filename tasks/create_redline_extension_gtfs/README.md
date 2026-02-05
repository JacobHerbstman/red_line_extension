# Create Red Line Extension GTFS

Builds a modified CTA GTFS feed that adds the four proposed Red Line Extension stations south of 95th.

This file is used by `compute_travel_times` for the extension scenario.

## Running

```bash
cd code && make
```

Main outputs: `output/cta_gtfs_with_extension.zip` and `output/rle_stations.csv`.
