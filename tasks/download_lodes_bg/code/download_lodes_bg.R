# download_lodes_bg.R
# Download LODES data at BLOCK GROUP level for Illinois, filter to Cook County
#
# Key change from tract-level: agg_geo = "bg" instead of "tract"
# Block group GEOIDs are 12 digits (vs 11 for tracts)
#
# NOTE: Using 2019 (pre-COVID) data with LODES7 (2010 Census geography)

source("../../setup_environment/code/packages.R")

# =============================================================================
# PARAMETERS
# =============================================================================

STATE <- "il"
YEAR <- 2019          # Last pre-COVID year
VERSION <- "LODES7"   # 2010 Census geography
JOB_TYPE <- "JT00"    # All jobs
SEGMENT <- "S000"     # All segments (total count)

# Cook County FIPS (first 5 digits of GEOID)
COOK_COUNTY_FIPS <- "17031"

message("=============================================================")
message("Downloading LODES data at BLOCK GROUP level")
message(sprintf("Year: %s (pre-COVID), Version: %s", YEAR, VERSION))
message("=============================================================")

# =============================================================================
# 1. DOWNLOAD ORIGIN-DESTINATION DATA AT BLOCK GROUP LEVEL
# =============================================================================

message("\n[1/3] Downloading Origin-Destination (OD) data at block group level...")
message("This may take a few minutes (more granular than tract level)...")

tic("OD download")

# Download OD data aggregated to BLOCK GROUP level
# Key change: agg_geo = "bg"
od_bg <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "od",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  state_part = "main",  # Within-state flows only
  agg_geo = "bg"        # BLOCK GROUP level aggregation
)

toc()

message(sprintf("Downloaded %s OD flows (block group level)", format(nrow(od_bg), big.mark = ",")))

# Check column names - should be h_bg and w_bg at block group level
message(sprintf("Columns: %s", paste(names(od_bg), collapse = ", ")))

# Filter to flows involving Cook County
# h_bg = home/residence block group, w_bg = work block group
# Block group GEOIDs are 12 digits, first 5 are state+county
od_cook <- od_bg %>%
  filter(
    str_sub(h_bg, 1, 5) == COOK_COUNTY_FIPS |
    str_sub(w_bg, 1, 5) == COOK_COUNTY_FIPS
  )

message(sprintf("After filtering to Cook County: %s flows", format(nrow(od_cook), big.mark = ",")))

# Verify GEOID format
message(sprintf("Home BG GEOID length: %d (expected: 12)", nchar(od_cook$h_bg[1])))
message(sprintf("Work BG GEOID length: %d (expected: 12)", nchar(od_cook$w_bg[1])))

# =============================================================================
# 2. DOWNLOAD RESIDENCE AREA CHARACTERISTICS (RAC)
# =============================================================================

message("\n[2/3] Downloading Residence Area Characteristics (RAC)...")

tic("RAC download")

rac_bg <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "rac",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  agg_geo = "bg"  # BLOCK GROUP level
)

toc()

message(sprintf("Downloaded RAC for %s block groups", format(nrow(rac_bg), big.mark = ",")))

# Filter to Cook County
rac_cook <- rac_bg %>%
  filter(str_sub(h_bg, 1, 5) == COOK_COUNTY_FIPS)

message(sprintf("After filtering to Cook County: %s block groups", format(nrow(rac_cook), big.mark = ",")))

# =============================================================================
# 3. DOWNLOAD WORKPLACE AREA CHARACTERISTICS (WAC)
# =============================================================================

message("\n[3/3] Downloading Workplace Area Characteristics (WAC)...")

tic("WAC download")

wac_bg <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "wac",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  agg_geo = "bg"  # BLOCK GROUP level
)

toc()

message(sprintf("Downloaded WAC for %s block groups", format(nrow(wac_bg), big.mark = ",")))

# Filter to Cook County
wac_cook <- wac_bg %>%
  filter(str_sub(w_bg, 1, 5) == COOK_COUNTY_FIPS)

message(sprintf("After filtering to Cook County: %s block groups", format(nrow(wac_cook), big.mark = ",")))

# =============================================================================
# 4. SUMMARIZE AND SAVE
# =============================================================================

message("\n=============================================================")
message("Summary of LODES Block Group Data:")
message("=============================================================")

# OD summary
od_summary <- od_cook %>%
  summarize(
    n_flows = n(),
    total_jobs = sum(S000),
    n_home_bgs = n_distinct(h_bg),
    n_work_bgs = n_distinct(w_bg)
  )

message(sprintf("\nOD Data:"))
message(sprintf("  - Total flows: %s", format(od_summary$n_flows, big.mark = ",")))
message(sprintf("  - Total jobs: %s", format(od_summary$total_jobs, big.mark = ",")))
message(sprintf("  - Unique home block groups: %s", od_summary$n_home_bgs))
message(sprintf("  - Unique work block groups: %s", od_summary$n_work_bgs))

# RAC summary
message(sprintf("\nRAC Data (Workers by Residence):"))
message(sprintf("  - Total block groups: %s", nrow(rac_cook)))
message(sprintf("  - Total resident workers: %s", format(sum(rac_cook$C000), big.mark = ",")))

# WAC summary
message(sprintf("\nWAC Data (Jobs by Workplace):"))
message(sprintf("  - Total block groups: %s", nrow(wac_cook)))
message(sprintf("  - Total jobs: %s", format(sum(wac_cook$C000), big.mark = ",")))

# Save outputs
message("\nSaving outputs...")

write_csv(od_cook, "../output/lodes_od_bg_2019.csv")
message("  - Saved: ../output/lodes_od_bg_2019.csv")

write_csv(rac_cook, "../output/lodes_rac_bg_2019.csv")
message("  - Saved: ../output/lodes_rac_bg_2019.csv")

write_csv(wac_cook, "../output/lodes_wac_bg_2019.csv")
message("  - Saved: ../output/lodes_wac_bg_2019.csv")

message("\n=============================================================")
message("LODES block group download complete!")
message("Block group GEOIDs are 12 digits: STATE(2) + COUNTY(3) + TRACT(6) + BG(1)")
message("=============================================================")

# =============================================================================
# VARIABLE DOCUMENTATION
# =============================================================================
#
# OD Variables (block group level):
#   h_bg: Home (residence) block group GEOID (12 digits)
#   w_bg: Work block group GEOID (12 digits)
#   S000: Total jobs
#   SA01-SA03: Jobs by worker age
#   SE01-SE03: Jobs by earnings category
#   SI01-SI03: Jobs by industry
#
# RAC Variables:
#   h_bg: Home block group GEOID
#   C000: Total jobs (workers living in block group)
#
# WAC Variables:
#   w_bg: Work block group GEOID
#   C000: Total jobs (jobs in block group)
#
# =============================================================================
