# Model Inversion

Recovers tract-level wages, floor prices, and amenities from observed commuting flows and residential populations.

Uses travel-time-based gravity estimates (nu ~ 0.039/min from PPML) and iterates on the commuting market clearing condition until wages converge, then backs out prices and amenities.

## Running

```bash
cd code && make
```

Outputs go to `output/model_fundamentals.csv`.
