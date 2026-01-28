# Create floorspace dataset by merging parcel coordinates with floorspace data
# and aggregating to census tract level
# Uses spatial join to assign 2010 census tracts (parcel data has 2020 tracts)
source("../../setup_environment/code/packages.R")

# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading parcel universe...")
tic("Load parcels")
parcels <- fread("../input/parcel_universe.csv",
                 select = c("pin", "township_code", "latitude", "longitude"))
toc()

message(sprintf("  Total parcels: %s", format(nrow(parcels), big.mark = ",")))
message(sprintf("  Unique PINs: %s", format(uniqueN(parcels$pin), big.mark = ",")))

# Filter for valid coordinates
parcels <- parcels[!is.na(latitude) & !is.na(longitude) &
                   latitude != 0 & longitude != 0]
message(sprintf("  After removing missing coordinates: %s",
                format(nrow(parcels), big.mark = ",")))

# Deduplicate PINs (keep first occurrence - should already be unique but verify)
if (uniqueN(parcels$pin) != nrow(parcels)) {
  message("  WARNING: Duplicate PINs found, keeping first occurrence")
  parcels <- parcels[!duplicated(pin)]
}

# Convert PIN to character for consistent merging
parcels[, pin := as.character(pin)]

message("\nLoading floorspace data...")
residential <- fread("../input/residential_floorspace_pins.csv")
commercial <- fread("../input/commercial_floorspace_pins.csv")
multifamily <- fread("../input/multifamily_floorspace_pins.csv")

# Convert PINs to character
residential[, pin := as.character(pin)]
commercial[, pin := as.character(pin)]
multifamily[, pin := as.character(pin)]

message(sprintf("  Residential PINs: %s", format(nrow(residential), big.mark = ",")))
message(sprintf("  Commercial PINs: %s", format(nrow(commercial), big.mark = ",")))
message(sprintf("  Multifamily PINs: %s", format(nrow(multifamily), big.mark = ",")))

# Verify no duplicates in floorspace data
stopifnot("Duplicate PINs in residential data" = uniqueN(residential$pin) == nrow(residential))
stopifnot("Duplicate PINs in commercial data" = uniqueN(commercial$pin) == nrow(commercial))
stopifnot("Duplicate PINs in multifamily data" = uniqueN(multifamily$pin) == nrow(multifamily))

message("\nLoading 2010 census tract geometries...")
tic("Load tract geometries")
tracts_sf <- st_read("../input/chicago_tracts_2010.gpkg", quiet = TRUE)
toc()
message(sprintf("  Census tracts: %s", format(nrow(tracts_sf), big.mark = ",")))

# Also load tract attributes for final merge
tracts_attr <- fread("../input/chicago_tracts_2010.csv",
                     select = c("GEOID", "NAME", "land_area_sqmi", "total_pop",
                               "total_housing_units", "centroid_lon", "centroid_lat"))
setnames(tracts_attr, "GEOID", "census_tract_geoid")
# Convert to character for consistent merging
tracts_attr[, census_tract_geoid := as.character(census_tract_geoid)]

# =============================================================================
# MERGE FLOORSPACE DATA WITH PARCEL COORDINATES
# =============================================================================

message("\n" , strrep("=", 60))
message("MERGING FLOORSPACE DATA WITH PARCEL COORDINATES")
message(strrep("=", 60))

# Residential merge
message("\nResidential merge:")
res_merged <- merge(residential,
                    parcels[, .(pin, latitude, longitude)],
                    by = "pin", all.x = FALSE, all.y = FALSE)
res_merge_rate <- nrow(res_merged) / nrow(residential) * 100
message(sprintf("  Matched: %s of %s (%.1f%%)",
                format(nrow(res_merged), big.mark = ","),
                format(nrow(residential), big.mark = ","),
                res_merge_rate))
stopifnot("Residential merge rate over 100%" = res_merge_rate <= 100)

# Commercial merge
message("\nCommercial merge:")
comm_merged <- merge(commercial,
                     parcels[, .(pin, latitude, longitude)],
                     by = "pin", all.x = FALSE, all.y = FALSE)
comm_merge_rate <- nrow(comm_merged) / nrow(commercial) * 100
message(sprintf("  Matched: %s of %s (%.1f%%)",
                format(nrow(comm_merged), big.mark = ","),
                format(nrow(commercial), big.mark = ","),
                comm_merge_rate))
stopifnot("Commercial merge rate over 100%" = comm_merge_rate <= 100)

# Multifamily merge
message("\nMultifamily merge:")
mf_merged <- merge(multifamily,
                   parcels[, .(pin, latitude, longitude)],
                   by = "pin", all.x = FALSE, all.y = FALSE)
mf_merge_rate <- nrow(mf_merged) / nrow(multifamily) * 100
message(sprintf("  Matched: %s of %s (%.1f%%)",
                format(nrow(mf_merged), big.mark = ","),
                format(nrow(multifamily), big.mark = ","),
                mf_merge_rate))
stopifnot("Multifamily merge rate over 100%" = mf_merge_rate <= 100)

# =============================================================================
# SPATIAL JOIN TO ASSIGN 2010 CENSUS TRACTS
# =============================================================================

message("\n" , strrep("=", 60))
message("SPATIAL JOIN TO ASSIGN 2010 CENSUS TRACTS")
message(strrep("=", 60))

# Combine all merged data first
res_merged[, property_type := "residential"]
comm_merged[, property_type := "commercial"]
mf_merged[, property_type := "multifamily"]

# Standardize columns for binding
res_out <- res_merged[, .(pin, building_sqft, year_built, property_type, latitude, longitude)]
comm_out <- comm_merged[, .(pin, building_sqft, year_built, property_type, latitude, longitude)]
mf_out <- mf_merged[, .(pin, building_sqft, year_built, property_type, latitude, longitude)]

floorspace_all <- rbindlist(list(res_out, comm_out, mf_out))
message(sprintf("\nTotal records to geocode: %s", format(nrow(floorspace_all), big.mark = ",")))

# OPTIMIZATION: Get unique coordinate pairs to reduce spatial join workload
# Many parcels share exact coordinates (e.g., condos in same building)
message("\nOptimization: Finding unique coordinate pairs...")
tic("Find unique coords")
coords_unique <- unique(floorspace_all[, .(latitude, longitude)])
toc()
message(sprintf("  Unique coordinate pairs: %s (%.1f%% of total)",
                format(nrow(coords_unique), big.mark = ","),
                nrow(coords_unique) / nrow(floorspace_all) * 100))

# Convert unique coordinates to sf points
message("\nConverting to spatial points...")
tic("Create sf points")
coords_sf <- st_as_sf(coords_unique,
                      coords = c("longitude", "latitude"),
                      crs = 4326)  # WGS84
toc()

# Ensure tracts are in same CRS
tracts_sf <- st_transform(tracts_sf, 4326)

# Perform spatial join
message("\nPerforming spatial join (this may take a moment)...")
tic("Spatial join")
coords_with_tract <- st_join(coords_sf, tracts_sf[, "GEOID"],
                              join = st_within)
toc()

# Convert back to data.table
coords_tract <- as.data.table(coords_with_tract)
coords_tract[, geometry := NULL]

# Extract coordinates back (they were consumed by st_as_sf)
coords_tract[, c("longitude", "latitude") := .(
  st_coordinates(coords_with_tract)[, 1],
  st_coordinates(coords_with_tract)[, 2]
)]
setnames(coords_tract, "GEOID", "census_tract_geoid")

# Check for points outside all tracts
outside_tracts <- sum(is.na(coords_tract$census_tract_geoid))
if (outside_tracts > 0) {
  message(sprintf("  Warning: %s coordinate pairs outside all tract boundaries",
                  format(outside_tracts, big.mark = ",")))
}

# Join tract assignments back to full dataset
message("\nJoining tract assignments back to full dataset...")
tic("Join back")
floorspace_pins <- merge(floorspace_all, coords_tract,
                         by = c("latitude", "longitude"),
                         all.x = TRUE)
toc()

# Remove records that couldn't be assigned to a tract
n_before <- nrow(floorspace_pins)
floorspace_pins <- floorspace_pins[!is.na(census_tract_geoid)]
n_after <- nrow(floorspace_pins)
if (n_before != n_after) {
  message(sprintf("  Removed %s records outside tract boundaries",
                  format(n_before - n_after, big.mark = ",")))
}

# =============================================================================
# VERIFY AND SUMMARIZE
# =============================================================================

message("\n" , strrep("=", 60))
message("PIN-LEVEL SUMMARY")
message(strrep("=", 60))

message(sprintf("\nTotal PINs with floorspace and tract assignment: %s",
                format(nrow(floorspace_pins), big.mark = ",")))
message(sprintf("  Residential: %s", format(sum(floorspace_pins$property_type == "residential"), big.mark = ",")))
message(sprintf("  Commercial: %s", format(sum(floorspace_pins$property_type == "commercial"), big.mark = ",")))
message(sprintf("  Multifamily: %s", format(sum(floorspace_pins$property_type == "multifamily"), big.mark = ",")))

# Check for any PINs appearing in multiple categories
dup_pins <- floorspace_pins[duplicated(pin) | duplicated(pin, fromLast = TRUE)]
if (nrow(dup_pins) > 0) {
  message(sprintf("\nNote: %s PINs appear in multiple property types",
                  format(uniqueN(dup_pins$pin), big.mark = ",")))
}

# Verify all tracts are 2010 tracts
tracts_in_data <- unique(floorspace_pins$census_tract_geoid)
tracts_valid <- tracts_in_data[tracts_in_data %in% tracts_attr$census_tract_geoid]
message(sprintf("\nTracts assigned: %s (all valid 2010 tracts: %s)",
                length(tracts_in_data),
                length(tracts_in_data) == length(tracts_valid)))

# =============================================================================
# AGGREGATE TO TRACT LEVEL
# =============================================================================

message("\n" , strrep("=", 60))
message("AGGREGATING TO TRACT LEVEL")
message(strrep("=", 60))

# Aggregate by tract and property type
tract_by_type <- floorspace_pins[, .(
  total_sqft = sum(building_sqft, na.rm = TRUE),
  num_buildings = .N,
  avg_year_built = mean(year_built, na.rm = TRUE)
), by = .(census_tract_geoid, property_type)]

# Reshape to wide format
tract_wide <- dcast(tract_by_type,
                    census_tract_geoid ~ property_type,
                    value.var = c("total_sqft", "num_buildings"),
                    fill = 0)

# Calculate totals
tract_wide[, total_sqft_all := total_sqft_residential + total_sqft_commercial +
             total_sqft_multifamily]
tract_wide[, num_buildings_all := num_buildings_residential + num_buildings_commercial +
             num_buildings_multifamily]

# Merge with tract characteristics (keep all tracts, even those with no floorspace)
floorspace_tracts <- merge(tracts_attr, tract_wide, by = "census_tract_geoid", all.x = TRUE)

# Fill NA values with 0 for tracts with no floorspace data
sqft_cols <- grep("^total_sqft_|^num_buildings_", names(floorspace_tracts), value = TRUE)
for (col in sqft_cols) {
  floorspace_tracts[is.na(get(col)), (col) := 0]
}

# Calculate density measures
floorspace_tracts[, sqft_per_sqmi := total_sqft_all / land_area_sqmi]
floorspace_tracts[, sqft_per_capita := fifelse(total_pop > 0, total_sqft_all / total_pop, NA_real_)]
floorspace_tracts[, sqft_per_housing_unit := fifelse(total_housing_units > 0,
                                                      total_sqft_residential / total_housing_units,
                                                      NA_real_)]

message(sprintf("\nTracts in output: %s of %s",
                format(nrow(floorspace_tracts), big.mark = ","),
                format(nrow(tracts_attr), big.mark = ",")))
message(sprintf("Tracts with floorspace data: %s",
                format(sum(floorspace_tracts$total_sqft_all > 0), big.mark = ",")))

message("\nTotal floorspace by type (millions of sqft):")
message(sprintf("  Residential: %.1f M", sum(floorspace_tracts$total_sqft_residential) / 1e6))
message(sprintf("  Commercial: %.1f M", sum(floorspace_tracts$total_sqft_commercial) / 1e6))
message(sprintf("  Multifamily: %.1f M", sum(floorspace_tracts$total_sqft_multifamily) / 1e6))
message(sprintf("  Total: %.1f M", sum(floorspace_tracts$total_sqft_all) / 1e6))

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

message("\n" , strrep("=", 60))
message("SAVING OUTPUTS")
message(strrep("=", 60))

# Reorder columns for pin output
setcolorder(floorspace_pins, c("pin", "building_sqft", "year_built", "property_type",
                                "latitude", "longitude", "census_tract_geoid"))

fwrite(floorspace_pins, "../output/floorspace_by_pin.csv")
message(sprintf("Saved: ../output/floorspace_by_pin.csv (%s rows)",
                format(nrow(floorspace_pins), big.mark = ",")))

fwrite(floorspace_tracts, "../output/floorspace_by_tract.csv")
message(sprintf("Saved: ../output/floorspace_by_tract.csv (%s rows)",
                format(nrow(floorspace_tracts), big.mark = ",")))

message("\nDone!")
