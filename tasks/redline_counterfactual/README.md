# Red Line Extension Counterfactual

Computes equilibrium effects of the RLE (4 new stations south of 95th/Dan Ryan) on floor prices, population, and welfare.

Uses GTFS-based travel time changes from `compute_travel_times/` and solves for the new equilibrium via exact hat algebra.

## Running

```bash
cd code && make        # all specs
cd code && make main   # just the main specification
```

Main outputs: `counterfactual_main_spec.csv`, `welfare_summary.csv`, and the map PDFs.
