# Create Floorspace Data

Merges assessor parcel coordinates with residential, commercial, and multifamily floorspace records.

Then assigns parcels to 2010 census tracts and aggregates floorspace totals by tract.

## Running

```bash
cd code && make
```

Main output: `output/floorspace_by_tract.csv` (plus `output/floorspace_by_pin.csv`).
