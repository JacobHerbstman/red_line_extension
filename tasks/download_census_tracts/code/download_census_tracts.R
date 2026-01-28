# download_census_tracts.R
# Download 2010 Census tract boundaries and 2019 ACS data for Cook County/Chicago
# Uses tigris for boundaries and tidycensus for demographic/economic data
#
# NOTE: Using 2010 tract boundaries to match LODES7 (2019 data)
# and 2019 ACS (pre-COVID) for consistency

source("../../setup_environment/code/packages.R")

# =============================================================================
# PARAMETERS
# =============================================================================

STATE_FIPS <- "17"       # Illinois
COUNTY_FIPS <- "031"     # Cook County
COOK_COUNTY_FIPS <- "17031"
TRACT_YEAR <- 2010       # Match LODES7 geography
ACS_YEAR <- 2019         # Pre-COVID, 5-year estimates (2015-2019)

message("=============================================================")
message("Downloading Census Tract Data for Cook County, IL")
message(sprintf("Tract boundaries: %s (to match LODES7)", TRACT_YEAR))
message(sprintf("ACS data: %s (pre-COVID)", ACS_YEAR))
message("=============================================================")

# =============================================================================
# 1. DOWNLOAD TRACT BOUNDARIES
# =============================================================================

message("\n[1/4] Downloading 2010 Census tract boundaries...")

# Set tigris to cache downloads
options(tigris_use_cache = TRUE)

# Download tract boundaries for Cook County
# Using 2010 boundaries to match LODES7 geography
tracts_sf <- tracts(
  state = STATE_FIPS,
  county = COUNTY_FIPS,
  year = TRACT_YEAR,
  cb = TRUE  # Use cartographic boundary (smaller file, cleaner edges)
)

message(sprintf("Downloaded %s tract boundaries", nrow(tracts_sf)))

# Basic cleaning - column names differ between tigris years
# 2010 cb=TRUE returns: GEO_ID, STATE, COUNTY, TRACT, NAME, CENSUSAREA
# Later years return: GEOID, STATEFP, COUNTYFP, TRACTCE, NAME, ALAND, AWATER
col_names <- names(tracts_sf)

# Only rename if the target name doesn't already exist
rename_map <- c()
if ("GEO_ID" %in% col_names && !("GEOID" %in% col_names)) rename_map <- c(rename_map, GEOID = "GEO_ID")
if ("STATE" %in% col_names && !("STATEFP" %in% col_names)) rename_map <- c(rename_map, STATEFP = "STATE")
if ("COUNTY" %in% col_names && !("COUNTYFP" %in% col_names)) rename_map <- c(rename_map, COUNTYFP = "COUNTY")
if ("TRACT" %in% col_names && !("TRACTCE" %in% col_names)) rename_map <- c(rename_map, TRACTCE = "TRACT")
# Also handle "10" suffix variants
if ("GEOID10" %in% col_names && !("GEOID" %in% col_names)) rename_map <- c(rename_map, GEOID = "GEOID10")
if ("STATEFP10" %in% col_names && !("STATEFP" %in% col_names)) rename_map <- c(rename_map, STATEFP = "STATEFP10")
if ("COUNTYFP10" %in% col_names && !("COUNTYFP" %in% col_names)) rename_map <- c(rename_map, COUNTYFP = "COUNTYFP10")
if ("TRACTCE10" %in% col_names && !("TRACTCE" %in% col_names)) rename_map <- c(rename_map, TRACTCE = "TRACTCE10")
if ("NAME10" %in% col_names && !("NAME" %in% col_names)) rename_map <- c(rename_map, NAME = "NAME10")

if (length(rename_map) > 0) {
  tracts_sf <- tracts_sf %>% rename(!!!rename_map)
}

# For 2010, GEOID is in format "1400000US17031010100" - extract the 11-digit FIPS
tracts_sf <- tracts_sf %>%
  mutate(
    GEOID = as.character(GEOID),
    GEOID = ifelse(nchar(GEOID) > 11, str_extract(GEOID, "\\d{11}$"), GEOID)
  )

# Select available columns and compute area
# 2010 cb=TRUE has CENSUSAREA (sq mi) but no ALAND/AWATER
tracts_sf <- tracts_sf %>%
  select(GEOID, STATEFP, COUNTYFP, TRACTCE, NAME,
         any_of(c("ALAND", "AWATER", "CENSUSAREA"))) %>%
  mutate(
    land_area_sqmi = if ("CENSUSAREA" %in% names(.)) {
      as.numeric(CENSUSAREA)
    } else {
      as.numeric(ALAND) / 2589988.11
    },
    total_area_sqmi = if ("CENSUSAREA" %in% names(.)) {
      as.numeric(CENSUSAREA)  # CENSUSAREA is land only in cb files
    } else {
      (as.numeric(ALAND) + as.numeric(AWATER)) / 2589988.11
    }
  )

# =============================================================================
# 2. DOWNLOAD ACS DATA - POPULATION AND HOUSING
# =============================================================================

message("\n[2/4] Downloading 2019 ACS population and housing data...")

# Check if Census API key is set
if (Sys.getenv("CENSUS_API_KEY") == "") {
  message("WARNING: No CENSUS_API_KEY found.")
  message("You may need to set one with: tidycensus::census_api_key('YOUR_KEY', install = TRUE)")
  message("Get a key at: https://api.census.gov/data/key_signup.html")
}

# Define variables to download
# See: https://api.census.gov/data/2019/acs/acs5/variables.html
acs_vars <- c(
  # Total population
  "B01003_001",  # Total population
  
  # Housing
  "B25001_001",  # Total housing units
  "B25002_002",  # Occupied housing units
  "B25002_003",  # Vacant housing units
  "B25077_001",  # Median home value
  "B25064_001",  # Median gross rent
  
  # Income and employment
  "B19013_001",  # Median household income
  "B23025_002",  # In labor force
  "B23025_005",  # Unemployed
  
  # Workers and commuting
  "B08301_001",  # Total workers 16+
  "B08303_001",  # Travel time to work (aggregate minutes, for avg calculation)
  
  # Earnings
  "B20002_001"   # Median earnings (total)
)

acs_data <- tryCatch({
  get_acs(
    geography = "tract",
    variables = acs_vars,
    state = STATE_FIPS,
    county = COUNTY_FIPS,
    year = ACS_YEAR,
    survey = "acs5",
    output = "wide"
  )
}, error = function(e) {
  message(sprintf("Error downloading ACS data: %s", e$message))
  message("Continuing without ACS data...")
  NULL
})

if (!is.null(acs_data)) {
  message(sprintf("Downloaded ACS data for %s tracts", nrow(acs_data)))
  
  # Rename variables to be more readable
  acs_clean <- acs_data %>%
    select(
      GEOID,
      NAME,
      total_pop = B01003_001E,
      total_housing_units = B25001_001E,
      occupied_housing = B25002_002E,
      vacant_housing = B25002_003E,
      median_home_value = B25077_001E,
      median_rent = B25064_001E,
      median_hh_income = B19013_001E,
      labor_force = B23025_002E,
      unemployed = B23025_005E,
      total_workers = B08301_001E,
      median_earnings = B20002_001E
    ) %>%
    mutate(
      GEOID = as.character(GEOID),
      # Calculate derived variables
      unemployment_rate = ifelse(labor_force > 0, unemployed / labor_force, NA),
      vacancy_rate = ifelse(total_housing_units > 0, vacant_housing / total_housing_units, NA)
    )
} else {
  acs_clean <- NULL
}

# =============================================================================
# 3. DOWNLOAD CHICAGO CITY BOUNDARY
# =============================================================================

message("\n[3/4] Downloading Chicago city boundary...")

chicago_boundary <- tryCatch({
  places(state = STATE_FIPS, year = 2019, cb = TRUE) %>%
    filter(str_detect(toupper(NAME), "CHICAGO"))
}, error = function(e) {
  message(sprintf("Error downloading city boundary: %s", e$message))
  NULL
})

if (!is.null(chicago_boundary) && nrow(chicago_boundary) > 0) {
  message("Downloaded Chicago city boundary")
  
  # Identify which tracts are in Chicago
  # Use centroid-based assignment (tract is "in Chicago" if centroid is within boundary)
  sf_use_s2(FALSE)  # Disable spherical geometry for intersection operations
  
  tract_centroids <- st_centroid(tracts_sf)
  in_chicago <- st_within(tract_centroids, chicago_boundary, sparse = FALSE)[, 1]
  
  tracts_sf <- tracts_sf %>%
    mutate(in_chicago = in_chicago)
  
  n_chicago <- sum(tracts_sf$in_chicago)
  message(sprintf("Identified %s tracts within Chicago city limits", n_chicago))
} else {
  message("Could not download Chicago boundary, marking all tracts as potentially in Chicago")
  tracts_sf <- tracts_sf %>%
    mutate(in_chicago = TRUE)  # Conservative: include all Cook County
}

# =============================================================================
# 4. MERGE AND SAVE
# =============================================================================

message("\n[4/4] Merging data and saving outputs...")

# Merge tract boundaries with ACS data
if (!is.null(acs_clean)) {
  tracts_final <- tracts_sf %>%
    left_join(acs_clean %>% select(-NAME), by = "GEOID")
} else {
  tracts_final <- tracts_sf
}

# Calculate centroids for CSV output
centroids <- st_centroid(tracts_final)
centroid_coords <- st_coordinates(centroids)

# Summary statistics
message("\n=============================================================")
message("Summary of tract data:")
message("=============================================================")
message(sprintf("Total tracts in Cook County: %s", nrow(tracts_final)))
message(sprintf("Tracts in Chicago: %s", sum(tracts_final$in_chicago, na.rm = TRUE)))
if (!is.null(acs_clean)) {
  message(sprintf("Total population: %s", format(sum(tracts_final$total_pop, na.rm = TRUE), big.mark = ",")))
  message(sprintf("Total workers: %s", format(sum(tracts_final$total_workers, na.rm = TRUE), big.mark = ",")))
}

# Save as geopackage (preserves geometry and attributes)
st_write(tracts_final, "../output/cook_county_tracts_2010.gpkg", delete_dsn = TRUE, quiet = TRUE)
message("\nSaved: ../output/cook_county_tracts_2010.gpkg")

# Also save as CSV (without geometry) for easier viewing
tracts_csv <- tracts_final %>%
  st_drop_geometry() %>%
  mutate(
    centroid_lon = centroid_coords[, 1],
    centroid_lat = centroid_coords[, 2]
  )

write_csv(tracts_csv, "../output/cook_county_tracts_2010.csv")
message("Saved: ../output/cook_county_tracts_2010.csv")

# Save Chicago boundary separately
if (!is.null(chicago_boundary) && nrow(chicago_boundary) > 0) {
  st_write(chicago_boundary, "../output/chicago_boundary.gpkg", delete_dsn = TRUE, quiet = TRUE)
  message("Saved: ../output/chicago_boundary.gpkg")
}

# Save just Chicago tracts
chicago_tracts <- tracts_final %>%
  filter(in_chicago == TRUE)

st_write(chicago_tracts, "../output/chicago_tracts_2010.gpkg", delete_dsn = TRUE, quiet = TRUE)
message("Saved: ../output/chicago_tracts_2010.gpkg")

# Centroids for Chicago tracts
chicago_centroids <- st_centroid(chicago_tracts)
chicago_centroid_coords <- st_coordinates(chicago_centroids)

chicago_tracts_csv <- chicago_tracts %>%
  st_drop_geometry() %>%
  mutate(
    centroid_lon = chicago_centroid_coords[, 1],
    centroid_lat = chicago_centroid_coords[, 2]
  )

write_csv(chicago_tracts_csv, "../output/chicago_tracts_2010.csv")
message("Saved: ../output/chicago_tracts_2010.csv")

message("\n=============================================================")
message("Census tract download complete!")
message(sprintf("Chicago has %s tracts for analysis", nrow(chicago_tracts)))
message("Using 2010 tract boundaries (LODES7 compatible) with 2019 ACS data")
message("=============================================================")
