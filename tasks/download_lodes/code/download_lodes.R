# download_lodes.R
# Download LODES data for Illinois, filter to Chicago/Cook County tracts
# Outputs: OD flows, RAC, WAC at tract level
#
# NOTE: Using 2019 (pre-COVID) data with LODES7 (2010 Census geography)
# to capture normal commuting patterns before pandemic disruption

source("../../setup_environment/code/packages.R")

# =============================================================================
# PARAMETERS
# =============================================================================

STATE <- "il"
YEAR <- 2019          # Last pre-COVID year
VERSION <- "LODES7"   # 2010 Census blocks (required for 2019)
JOB_TYPE <- "JT00"    # All jobs
SEGMENT <- "S000"     # All segments (total count)

# Cook County FIPS (where Chicago is located)
COOK_COUNTY_FIPS <- "17031"

message("=============================================================")
message("Downloading LODES data for Illinois")
message(sprintf("Year: %s (pre-COVID), Version: %s (2010 Census geography)", YEAR, VERSION))
message("=============================================================")

# =============================================================================
# 1. DOWNLOAD ORIGIN-DESTINATION DATA
# =============================================================================

message("\n[1/3] Downloading Origin-Destination (OD) data...")
message("This downloads commuting flows between census tracts...")

tic("OD download")

# Download OD data aggregated to tract level
# state_part = "main" excludes flows where workplace is out of state
od_main <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "od",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  state_part = "main",  # Within-state flows only
  agg_geo = "tract"
)

toc()

message(sprintf("Downloaded %s OD flows (tract-to-tract)", format(nrow(od_main), big.mark = ",")))

# Filter to flows involving Cook County
# h_tract = home/residence tract, w_tract = work tract
od_cook <- od_main %>%
  filter(
    str_sub(h_tract, 1, 5) == COOK_COUNTY_FIPS |
    str_sub(w_tract, 1, 5) == COOK_COUNTY_FIPS
  )

message(sprintf("After filtering to Cook County: %s flows", format(nrow(od_cook), big.mark = ",")))

# =============================================================================
# 2. DOWNLOAD RESIDENCE AREA CHARACTERISTICS (RAC)
# =============================================================================

message("\n[2/3] Downloading Residence Area Characteristics (RAC)...")

tic("RAC download")

rac <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "rac",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  agg_geo = "tract"
)

toc()

message(sprintf("Downloaded RAC for %s tracts", format(nrow(rac), big.mark = ",")))

# Filter to Cook County
rac_cook <- rac %>%
  filter(str_sub(h_tract, 1, 5) == COOK_COUNTY_FIPS)

message(sprintf("After filtering to Cook County: %s tracts", format(nrow(rac_cook), big.mark = ",")))

# =============================================================================
# 3. DOWNLOAD WORKPLACE AREA CHARACTERISTICS (WAC)
# =============================================================================

message("\n[3/3] Downloading Workplace Area Characteristics (WAC)...")

tic("WAC download")

wac <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "wac",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  agg_geo = "tract"
)

toc()

message(sprintf("Downloaded WAC for %s tracts", format(nrow(wac), big.mark = ",")))

# Filter to Cook County
wac_cook <- wac %>%
  filter(str_sub(w_tract, 1, 5) == COOK_COUNTY_FIPS)

message(sprintf("After filtering to Cook County: %s tracts", format(nrow(wac_cook), big.mark = ",")))

# =============================================================================
# 4. SUMMARIZE AND SAVE
# =============================================================================

message("\n=============================================================")
message("Summary of downloaded data:")
message("=============================================================")

# OD summary
od_summary <- od_cook %>%
  summarize(
    n_flows = n(),
    total_jobs = sum(S000),
    n_home_tracts = n_distinct(h_tract),
    n_work_tracts = n_distinct(w_tract)
  )

message(sprintf("\nOD Data:"))
message(sprintf("  - Total flows: %s", format(od_summary$n_flows, big.mark = ",")))
message(sprintf("  - Total jobs: %s", format(od_summary$total_jobs, big.mark = ",")))
message(sprintf("  - Unique home tracts: %s", od_summary$n_home_tracts))
message(sprintf("  - Unique work tracts: %s", od_summary$n_work_tracts))

# RAC summary
message(sprintf("\nRAC Data (Workers by Residence):"))
message(sprintf("  - Total tracts: %s", nrow(rac_cook)))
message(sprintf("  - Total resident workers: %s", format(sum(rac_cook$C000), big.mark = ",")))

# WAC summary
message(sprintf("\nWAC Data (Jobs by Workplace):"))
message(sprintf("  - Total tracts: %s", nrow(wac_cook)))
message(sprintf("  - Total jobs: %s", format(sum(wac_cook$C000), big.mark = ",")))

# Save outputs
message("\nSaving outputs...")

write_csv(od_cook, "../output/lodes_od_cook_county_2019.csv")
message("  - Saved: ../output/lodes_od_cook_county_2019.csv")

write_csv(rac_cook, "../output/lodes_rac_cook_county_2019.csv")
message("  - Saved: ../output/lodes_rac_cook_county_2019.csv")

write_csv(wac_cook, "../output/lodes_wac_cook_county_2019.csv")
message("  - Saved: ../output/lodes_wac_cook_county_2019.csv")

# Also save the full state data for reference (in case we need flows outside Cook)
write_csv(od_main, "../output/lodes_od_illinois_full_2019.csv")
message("  - Saved: ../output/lodes_od_illinois_full_2019.csv")

message("\n=============================================================")
message("LODES download complete!")
message("NOTE: Using 2019 pre-COVID data with 2010 Census tract definitions")
message("=============================================================")

# =============================================================================
# 5. VARIABLE DOCUMENTATION
# =============================================================================
# 
# OD Variables:
#   h_tract: Home (residence) census tract GEOID (2010 definition)
#   w_tract: Work census tract GEOID (2010 definition)
#   S000: Total jobs
#   SA01: Jobs for workers age 29 or younger
#   SA02: Jobs for workers age 30-54
#   SA03: Jobs for workers age 55 or older
#   SE01: Jobs with earnings $1,250/month or less
#   SE02: Jobs with earnings $1,251-$3,333/month
#   SE03: Jobs with earnings > $3,333/month
#   SI01: Jobs in Goods Producing industries
#   SI02: Jobs in Trade, Transportation, Utilities
#   SI03: Jobs in All Other Services
#
# RAC Variables (C000 = total, then breakdowns by age/earnings/industry):
#   h_tract: Home census tract GEOID
#   C000: Total jobs (count of workers living in tract)
#   
# WAC Variables:
#   w_tract: Work census tract GEOID
#   C000: Total jobs (count of jobs in tract)
#
# =============================================================================
