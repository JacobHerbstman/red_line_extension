# Download 2010 Census tract boundaries and 2019 ACS data for Cook County
# Uses 2010 tracts to match LODES7 geography

source("../../setup_environment/code/packages.R")

STATE_FIPS <- "17"
COUNTY_FIPS <- "031"
COOK_COUNTY_FIPS <- "17031"
TRACT_YEAR <- 2010
ACS_YEAR <- 2019

message("Downloading Census tract data for Cook County")
message(sprintf("Tract boundaries: %s, ACS data: %s", TRACT_YEAR, ACS_YEAR))

# --- Tract boundaries ---
message("Downloading tract boundaries...")

options(tigris_use_cache = TRUE)

tracts_sf <- tracts(
  state = STATE_FIPS,
  county = COUNTY_FIPS,
  year = TRACT_YEAR,
  cb = TRUE
)

message(sprintf("Downloaded %s tract boundaries", nrow(tracts_sf)))

# Standardize column names across tigris versions
col_names <- names(tracts_sf)
rename_map <- c()
if ("GEO_ID" %in% col_names && !("GEOID" %in% col_names)) rename_map <- c(rename_map, GEOID = "GEO_ID")
if ("STATE" %in% col_names && !("STATEFP" %in% col_names)) rename_map <- c(rename_map, STATEFP = "STATE")
if ("COUNTY" %in% col_names && !("COUNTYFP" %in% col_names)) rename_map <- c(rename_map, COUNTYFP = "COUNTY")
if ("TRACT" %in% col_names && !("TRACTCE" %in% col_names)) rename_map <- c(rename_map, TRACTCE = "TRACT")
if ("GEOID10" %in% col_names && !("GEOID" %in% col_names)) rename_map <- c(rename_map, GEOID = "GEOID10")
if ("STATEFP10" %in% col_names && !("STATEFP" %in% col_names)) rename_map <- c(rename_map, STATEFP = "STATEFP10")
if ("COUNTYFP10" %in% col_names && !("COUNTYFP" %in% col_names)) rename_map <- c(rename_map, COUNTYFP = "COUNTYFP10")
if ("TRACTCE10" %in% col_names && !("TRACTCE" %in% col_names)) rename_map <- c(rename_map, TRACTCE = "TRACTCE10")
if ("NAME10" %in% col_names && !("NAME" %in% col_names)) rename_map <- c(rename_map, NAME = "NAME10")

if (length(rename_map) > 0) {
  tracts_sf <- tracts_sf %>% rename(!!!rename_map)
}

# Extract 11-digit FIPS from 2010 format
tracts_sf <- tracts_sf %>%
  mutate(
    GEOID = as.character(GEOID),
    GEOID = ifelse(nchar(GEOID) > 11, str_extract(GEOID, "\\d{11}$"), GEOID)
  )

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
      as.numeric(CENSUSAREA)
    } else {
      (as.numeric(ALAND) + as.numeric(AWATER)) / 2589988.11
    }
  )

# --- ACS data ---
message("Downloading ACS data...")

if (Sys.getenv("CENSUS_API_KEY") == "") {
  message("No CENSUS_API_KEY found. Get one at: https://api.census.gov/data/key_signup.html")
}

acs_vars <- c(
  "B01003_001",  # Total population
  "B25001_001",  # Total housing units
  "B25002_002",  # Occupied housing units
  "B25002_003",  # Vacant housing units
  "B25077_001",  # Median home value
  "B25064_001",  # Median gross rent
  "B19013_001",  # Median household income
  "B23025_002",  # In labor force
  "B23025_005",  # Unemployed
  "B08301_001",  # Total workers 16+
  "B08303_001",  # Travel time to work
  "B20002_001"   # Median earnings
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
  NULL
})

if (!is.null(acs_data)) {
  message(sprintf("Downloaded ACS data for %s tracts", nrow(acs_data)))

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
      unemployment_rate = ifelse(labor_force > 0, unemployed / labor_force, NA),
      vacancy_rate = ifelse(total_housing_units > 0, vacant_housing / total_housing_units, NA)
    )
} else {
  acs_clean <- NULL
}

# --- Chicago boundary ---
message("Downloading Chicago city boundary...")

chicago_boundary <- tryCatch({
  places(state = STATE_FIPS, year = 2019, cb = TRUE) %>%
    filter(str_detect(toupper(NAME), "CHICAGO"))
}, error = function(e) {
  message(sprintf("Error downloading city boundary: %s", e$message))
  NULL
})

if (!is.null(chicago_boundary) && nrow(chicago_boundary) > 0) {
  sf_use_s2(FALSE)

  tract_centroids <- st_centroid(tracts_sf)
  in_chicago <- st_within(tract_centroids, chicago_boundary, sparse = FALSE)[, 1]

  tracts_sf <- tracts_sf %>%
    mutate(in_chicago = in_chicago)

  message(sprintf("Identified %s tracts within Chicago", sum(tracts_sf$in_chicago)))
} else {
  tracts_sf <- tracts_sf %>%
    mutate(in_chicago = TRUE)
}

# --- Merge and save ---
message("Merging and saving...")

if (!is.null(acs_clean)) {
  tracts_final <- tracts_sf %>%
    left_join(acs_clean %>% select(-NAME), by = "GEOID")
} else {
  tracts_final <- tracts_sf
}

centroids <- st_centroid(tracts_final)
centroid_coords <- st_coordinates(centroids)

message(sprintf("Total tracts: %s, in Chicago: %s",
                nrow(tracts_final),
                sum(tracts_final$in_chicago, na.rm = TRUE)))

st_write(tracts_final, "../output/cook_county_tracts_2010.gpkg", delete_dsn = TRUE, quiet = TRUE)

tracts_csv <- tracts_final %>%
  st_drop_geometry() %>%
  mutate(
    centroid_lon = centroid_coords[, 1],
    centroid_lat = centroid_coords[, 2]
  )

write_csv(tracts_csv, "../output/cook_county_tracts_2010.csv")

if (!is.null(chicago_boundary) && nrow(chicago_boundary) > 0) {
  st_write(chicago_boundary, "../output/chicago_boundary.gpkg", delete_dsn = TRUE, quiet = TRUE)
}

chicago_tracts <- tracts_final %>%
  filter(in_chicago == TRUE)

st_write(chicago_tracts, "../output/chicago_tracts_2010.gpkg", delete_dsn = TRUE, quiet = TRUE)

chicago_centroids <- st_centroid(chicago_tracts)
chicago_centroid_coords <- st_coordinates(chicago_centroids)

chicago_tracts_csv <- chicago_tracts %>%
  st_drop_geometry() %>%
  mutate(
    centroid_lon = chicago_centroid_coords[, 1],
    centroid_lat = chicago_centroid_coords[, 2]
  )

write_csv(chicago_tracts_csv, "../output/chicago_tracts_2010.csv")

message(sprintf("Census tract download complete. Chicago has %s tracts.", nrow(chicago_tracts)))
