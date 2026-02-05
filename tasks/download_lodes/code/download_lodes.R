# Download LODES data for Illinois, filter to Cook County
# Using 2019 pre-COVID data with LODES7 (2010 Census geography)

source("../../setup_environment/code/packages.R")

STATE <- "il"
YEAR <- 2019
VERSION <- "LODES7"
JOB_TYPE <- "JT00"
SEGMENT <- "S000"
COOK_COUNTY_FIPS <- "17031"

message("Downloading LODES data for Illinois")
message(sprintf("Year: %s, Version: %s", YEAR, VERSION))

# --- OD data ---
message("Downloading Origin-Destination data...")

tic("OD download")

od_main <- grab_lodes(
  state = STATE,
  year = YEAR,
  version = VERSION,
  lodes_type = "od",
  job_type = JOB_TYPE,
  segment = SEGMENT,
  state_part = "main",
  agg_geo = "tract"
)

toc()

message(sprintf("Downloaded %s OD flows", format(nrow(od_main), big.mark = ",")))

od_cook <- od_main %>%
  filter(
    str_sub(h_tract, 1, 5) == COOK_COUNTY_FIPS |
    str_sub(w_tract, 1, 5) == COOK_COUNTY_FIPS
  )

message(sprintf("After filtering to Cook County: %s flows", format(nrow(od_cook), big.mark = ",")))

# --- RAC data ---
message("Downloading Residence Area Characteristics...")

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

rac_cook <- rac %>%
  filter(str_sub(h_tract, 1, 5) == COOK_COUNTY_FIPS)

message(sprintf("After filtering to Cook County: %s tracts", format(nrow(rac_cook), big.mark = ",")))

# --- WAC data ---
message("Downloading Workplace Area Characteristics...")

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

wac_cook <- wac %>%
  filter(str_sub(w_tract, 1, 5) == COOK_COUNTY_FIPS)

message(sprintf("After filtering to Cook County: %s tracts", format(nrow(wac_cook), big.mark = ",")))

# --- Summary ---
od_summary <- od_cook %>%
  summarize(
    n_flows = n(),
    total_jobs = sum(S000),
    n_home_tracts = n_distinct(h_tract),
    n_work_tracts = n_distinct(w_tract)
  )

message(sprintf("OD: %s flows, %s jobs",
                format(od_summary$n_flows, big.mark = ","),
                format(od_summary$total_jobs, big.mark = ",")))
message(sprintf("RAC: %s resident workers", format(sum(rac_cook$C000), big.mark = ",")))
message(sprintf("WAC: %s jobs", format(sum(wac_cook$C000), big.mark = ",")))

# Save
message("Saving outputs...")

write_csv(od_cook, "../output/lodes_od_cook_county_2019.csv")
write_csv(rac_cook, "../output/lodes_rac_cook_county_2019.csv")
write_csv(wac_cook, "../output/lodes_wac_cook_county_2019.csv")
write_csv(od_main, "../output/lodes_od_illinois_full_2019.csv")

message("LODES download complete")

# Variable documentation:
# OD: h_tract (home), w_tract (work), S000 (total jobs), SA01-03 (age), SE01-03 (earnings), SI01-03 (industry)
# RAC: h_tract, C000 (workers living in tract)
# WAC: w_tract, C000 (jobs in tract)
