# download_block_groups.R
# Download 2019 ACS block group boundaries for Cook County
# Compute centroids for r5r routing
#
# Block groups are sub-tract geographies, identified by 12-digit GEOIDs:
# STATE (2) + COUNTY (3) + TRACT (6) + BLOCK GROUP (1)
# Example: 170318392001 = IL (17) + Cook (031) + Tract 839200 + BG 1

source("../../setup_environment/code/packages.R")

# =============================================================================
# PARAMETERS
# =============================================================================

STATE_FIPS <- "17"       # Illinois
COUNTY_FIPS <- "031"     # Cook County
ACS_YEAR <- 2019         # Match LODES vintage (pre-COVID)

message("=============================================================")
message("Downloading Block Group Data for Cook County, IL")
message(sprintf("ACS year: %s (to match LODES)", ACS_YEAR))
message("=============================================================")

# =============================================================================
# 1. DOWNLOAD BLOCK GROUP BOUNDARIES WITH ACS DATA
# =============================================================================

message("\n[1/3] Downloading block group boundaries with ACS data...")

# Set tigris to cache downloads
options(tigris_use_cache = TRUE)

# Define key variables to download
# Keep it simple - we mainly need geography and basic demographics
acs_vars <- c(
  "B01003_001",  # Total population
  "B19013_001",  # Median household income
  "B08301_001",  # Total workers 16+
  "B25001_001"   # Total housing units
)

# Download block groups with geometry
block_groups_sf <- get_acs(
  geography = "block group",
  variables = acs_vars,
  state = STATE_FIPS,
  county = COUNTY_FIPS,
  year = ACS_YEAR,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
)

message(sprintf("Downloaded %s block groups", nrow(block_groups_sf)))

# Rename variables
block_groups_sf <- block_groups_sf %>%
  select(
    GEOID,
    NAME,
    total_pop = B01003_001E,
    median_hh_income = B19013_001E,
    total_workers = B08301_001E,
    total_housing_units = B25001_001E,
    geometry
  )

# Verify GEOID format (should be 12 digits)
message(sprintf("GEOID length: %d (expected: 12)", nchar(block_groups_sf$GEOID[1])))

# =============================================================================
# 2. COMPUTE CENTROIDS
# =============================================================================

message("\n[2/3] Computing block group centroids...")

# Transform to projected CRS for accurate centroid calculation
# NAD83 / Illinois East (EPSG:26971) is good for Chicago
block_groups_proj <- st_transform(block_groups_sf, 26971)

# Compute centroids
centroids_proj <- st_centroid(block_groups_proj)

# Transform back to WGS84 for r5r (which uses lat/lon)
centroids_wgs84 <- st_transform(centroids_proj, 4326)

# Extract coordinates
centroid_coords <- st_coordinates(centroids_wgs84)

# Create centroids dataframe for r5r
# r5r expects columns: id, lon, lat
centroids_df <- data.frame(
  id = block_groups_sf$GEOID,
  lon = centroid_coords[, 1],
  lat = centroid_coords[, 2],
  total_pop = block_groups_sf$total_pop,
  median_hh_income = block_groups_sf$median_hh_income,
  total_workers = block_groups_sf$total_workers,
  total_housing_units = block_groups_sf$total_housing_units
)

# Verify coordinates are in Chicago area
lon_range <- range(centroids_df$lon)
lat_range <- range(centroids_df$lat)
message(sprintf("Longitude range: %.3f to %.3f (expected: -88.3 to -87.5)", lon_range[1], lon_range[2]))
message(sprintf("Latitude range: %.3f to %.3f (expected: 41.6 to 42.2)", lat_range[1], lat_range[2]))

# =============================================================================
# 3. SAVE OUTPUTS
# =============================================================================

message("\n[3/3] Saving outputs...")

# Save full geopackage (with geometry)
st_write(block_groups_sf, "../output/cook_county_block_groups.gpkg", delete_dsn = TRUE, quiet = TRUE)
message("Saved: ../output/cook_county_block_groups.gpkg")

# Save centroids CSV for r5r
write_csv(centroids_df, "../output/block_group_centroids.csv")
message("Saved: ../output/block_group_centroids.csv")

# =============================================================================
# SUMMARY
# =============================================================================

message("\n=============================================================")
message("Block Group Download Summary")
message("=============================================================")
message(sprintf("Total block groups: %s", nrow(block_groups_sf)))
message(sprintf("Total population: %s", format(sum(block_groups_sf$total_pop, na.rm = TRUE), big.mark = ",")))
message(sprintf("Total workers: %s", format(sum(block_groups_sf$total_workers, na.rm = TRUE), big.mark = ",")))
message(sprintf("GEOID format: %d digits (state+county+tract+bg)", nchar(block_groups_sf$GEOID[1])))
message("=============================================================")
