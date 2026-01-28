# Spatial Economics HW1: Red Line Extension Welfare Analysis

## Research Question
What are the welfare effects of the proposed CTA Red Line Extension on the South Side of Chicago?

## Model Overview
Simplified quantitative spatial model with:
- Workers choosing where to live and work
- Commuting costs that depend on travel time
- Residential land market
- No inter-location goods trade (Chicago treated as single goods market)

Based on: Monte et al. (2018), Ahlfeldt et al. (2015), Allen & Arkolakis (2022)

## Data Year Choice: 2019 (Pre-COVID)

**Why 2019?** COVID-19 dramatically disrupted commuting patterns starting in 2020. To analyze transit infrastructure impacts on a "normal" commuting equilibrium, we use 2019 data throughout:

- **LODES**: 2019 with LODES7 (2010 Census tract geography)
- **ACS**: 2019 5-year estimates (2015-2019)
- **Tract boundaries**: 2010 Census (to match LODES7)

This means our analysis captures pre-pandemic commuting patterns where downtown employment was at normal levels.

## Project Structure
```
spatial_hw1/
├── tasks/                          # Analysis tasks
│   ├── setup_environment/          # Package installation
│   ├── download_lodes/             # LEHD commuting data (2019)
│   ├── download_census_tracts/     # Census tract boundaries & ACS (2010/2019)
│   ├── [future tasks...]
│   ├── shell_functions.sh          # Shell helper functions
│   ├── shell_functions.make        # Make helper functions
│   └── generic.make                # Generic make rules
├── paper/                          # LaTeX paper (to be created)
└── README.md
```

## Data Sources
1. **LODES 2019** (LEHD Origin-Destination Employment Statistics)
   - Bilateral commuting flows at tract level
   - Workplace Area Characteristics (jobs by location)
   - Residence Area Characteristics (workers by residence)
   - Uses LODES7 with 2010 Census geography

2. **Census/ACS 2019**
   - 2010 tract boundaries (to match LODES7)
   - Population, income, housing from 2019 ACS 5-year

3. **Cook County Assessor** (from aldermanic privilege project)
   - Land/property values

4. **CTA GTFS** (to be added)
   - Transit travel times

## Running the Code
Each task follows the standard workflow:
```bash
cd tasks/[task_name]/code
make
```

**Note**: You need a Census API key for ACS data:
```r
tidycensus::census_api_key("YOUR_KEY", install = TRUE)
```
Get one at: https://api.census.gov/data/key_signup.html

## Key Parameters (to be estimated/calibrated)
- ε (Fréchet shape parameter): ~6-7
- κ (commuting cost decay): ~0.01
- (1-α) (housing expenditure share): ~0.3-0.4
- ν = εκ (semi-elasticity of commuting): ~0.07

## Timeline
- Week 1: Data acquisition, gravity equation estimation
- Week 2: Counterfactual analysis, write-up
