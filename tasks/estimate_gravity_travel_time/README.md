# Estimate Gravity (Travel Time)

Builds a tract-level commuting matrix by merging LODES flows with GTFS-based travel times.

Estimates the commuting gravity parameter with PPML and exports destination fixed effects for inversion.

## Running

```bash
cd code && make
```

Main outputs: `output/gravity_estimates_travel_time.csv`, `output/nu_time_per_min.csv`, and `output/gravity_destination_fe.csv`.
