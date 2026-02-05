# Red Line Extension Welfare Analysis

Estimates the welfare effects of the proposed CTA Red Line Extension on Chicago's South Side using a quantitative spatial model (simplified Monte et al. 2018).

## Running

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
