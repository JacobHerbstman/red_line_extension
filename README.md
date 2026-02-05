# Red Line Extension Welfare Analysis

Estimates the welfare effects of the proposed CTA Red Line Extension on Chicago's South Side using a quantitative spatial model (simplified Monte et al. 2018).

## Running

Run everything (all tasks in order, then paper) from the project root:
```bash
make
```

To run only the analysis pipeline (no paper build):
```bash
make tasks
```

To run the task Makefiles individually, in order:
1. `tasks/setup_environment/code`
2. `tasks/download_census_tracts/code`
3. `tasks/download_network_data/code`
4. `tasks/download_lodes/code`
5. `tasks/download_residential_floorspace/code`
6. `tasks/download_commercial_floorspace/code`
7. `tasks/create_floorspace_data/code`
8. `tasks/map_floorspace_shares/code`
9. `tasks/compute_travel_costs/code`
10. `tasks/create_redline_extension_gtfs/code`
11. `tasks/compute_travel_times/code`
12. `tasks/estimate_gravity_travel_time/code`
13. `tasks/invert_model/code`
14. `tasks/redline_counterfactual/code`
15. `paper/`

Each task has its own Makefile:
```bash
cd tasks/[task_name]/code
make
```

You'll need a Census API key for tract data:
```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```

## Structure

- `tasks/` - Analysis tasks (data download, model estimation, counterfactual)
- `paper/` - LaTeX writeup

Key outputs: `invert_model/output/model_fundamentals.csv` (recovered wages, prices, amenities) and `redline_counterfactual/output/` (welfare effects).
