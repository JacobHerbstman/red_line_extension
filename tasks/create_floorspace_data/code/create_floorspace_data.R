# Merge parcel coordinates with floorspace data and aggregate to tract level
# Uses spatial join to assign 2010 census tracts

source("../../setup_environment/code/packages.R")

# --- Load data ---
message("Loading parcel universe...")
tic("Load parcels")
parcels <- fread("../input/parcel_universe.csv",
                 select = c("pin", "township_code", "latitude", "longitude"))
toc()

message(sprintf("  Total parcels: %s", format(nrow(parcels), big.mark = ",")))

parcels <- parcels[!is.na(latitude) & !is.na(longitude) &
                   latitude != 0 & longitude != 0]
message(sprintf("  After removing missing coords: %s", format(nrow(parcels), big.mark = ",")))

if (uniqueN(parcels$pin) != nrow(parcels)) {
  message("  Duplicate PINs found, keeping first occurrence")
  parcels <- parcels[!duplicated(pin)]
}

parcels[, pin := as.character(pin)]

message("Loading floorspace data...")
residential <- fread("../input/residential_floorspace_pins.csv")
commercial <- fread("../input/commercial_floorspace_pins.csv")
multifamily <- fread("../input/multifamily_floorspace_pins.csv")

residential[, pin := as.character(pin)]
commercial[, pin := as.character(pin)]
multifamily[, pin := as.character(pin)]

message(sprintf("  Residential: %s, Commercial: %s, Multifamily: %s",
                format(nrow(residential), big.mark = ","),
                format(nrow(commercial), big.mark = ","),
                format(nrow(multifamily), big.mark = ",")))

stopifnot("Duplicate PINs in residential" = uniqueN(residential$pin) == nrow(residential))
stopifnot("Duplicate PINs in commercial" = uniqueN(commercial$pin) == nrow(commercial))
stopifnot("Duplicate PINs in multifamily" = uniqueN(multifamily$pin) == nrow(multifamily))

message("Loading tract geometries...")
tic("Load tract geometries")
tracts_sf <- st_read("../input/chicago_tracts_2010.gpkg", quiet = TRUE)
toc()
message(sprintf("  Census tracts: %s", nrow(tracts_sf)))

tracts_attr <- fread("../input/chicago_tracts_2010.csv",
                     select = c("GEOID", "NAME", "land_area_sqmi", "total_pop",
                               "total_housing_units", "centroid_lon", "centroid_lat"))
setnames(tracts_attr, "GEOID", "census_tract_geoid")
tracts_attr[, census_tract_geoid := as.character(census_tract_geoid)]

# --- Merge with coordinates ---
message("Merging floorspace with parcel coordinates...")

res_merged <- merge(residential,
                    parcels[, .(pin, latitude, longitude)],
                    by = "pin", all.x = FALSE, all.y = FALSE)
message(sprintf("Residential matched: %s of %s (%.1f%%)",
                format(nrow(res_merged), big.mark = ","),
                format(nrow(residential), big.mark = ","),
                nrow(res_merged) / nrow(residential) * 100))

comm_merged <- merge(commercial,
                     parcels[, .(pin, latitude, longitude)],
                     by = "pin", all.x = FALSE, all.y = FALSE)
message(sprintf("Commercial matched: %s of %s (%.1f%%)",
                format(nrow(comm_merged), big.mark = ","),
                format(nrow(commercial), big.mark = ","),
                nrow(comm_merged) / nrow(commercial) * 100))

mf_merged <- merge(multifamily,
                   parcels[, .(pin, latitude, longitude)],
                   by = "pin", all.x = FALSE, all.y = FALSE)
message(sprintf("Multifamily matched: %s of %s (%.1f%%)",
                format(nrow(mf_merged), big.mark = ","),
                format(nrow(multifamily), big.mark = ","),
                nrow(mf_merged) / nrow(multifamily) * 100))

# --- Spatial join to 2010 tracts ---
message("Performing spatial join to assign tracts...")

res_merged[, property_type := "residential"]
comm_merged[, property_type := "commercial"]
mf_merged[, property_type := "multifamily"]

res_out <- res_merged[, .(pin, building_sqft, year_built, property_type, latitude, longitude)]
comm_out <- comm_merged[, .(pin, building_sqft, year_built, property_type, latitude, longitude)]
mf_out <- mf_merged[, .(pin, building_sqft, year_built, property_type, latitude, longitude)]

floorspace_all <- rbindlist(list(res_out, comm_out, mf_out))
message(sprintf("Total records to geocode: %s", format(nrow(floorspace_all), big.mark = ",")))

# Optimize by finding unique coordinate pairs
message("Finding unique coordinate pairs...")
tic("Find unique coords")
coords_unique <- unique(floorspace_all[, .(latitude, longitude)])
toc()
message(sprintf("  Unique pairs: %s (%.1f%% of total)",
                format(nrow(coords_unique), big.mark = ","),
                nrow(coords_unique) / nrow(floorspace_all) * 100))

message("Converting to spatial points...")
tic("Create sf points")
coords_sf <- st_as_sf(coords_unique,
                      coords = c("longitude", "latitude"),
                      crs = 4326)
toc()

tracts_sf <- st_transform(tracts_sf, 4326)

message("Spatial join...")
tic("Spatial join")
coords_with_tract <- st_join(coords_sf, tracts_sf[, "GEOID"],
                              join = st_within)
toc()

coords_tract <- as.data.table(coords_with_tract)
coords_tract[, geometry := NULL]

coords_tract[, c("longitude", "latitude") := .(
  st_coordinates(coords_with_tract)[, 1],
  st_coordinates(coords_with_tract)[, 2]
)]
setnames(coords_tract, "GEOID", "census_tract_geoid")

outside_tracts <- sum(is.na(coords_tract$census_tract_geoid))
if (outside_tracts > 0) {
  message(sprintf("  %s coordinate pairs outside tract boundaries", format(outside_tracts, big.mark = ",")))
}

message("Joining tract assignments back...")
tic("Join back")
floorspace_pins <- merge(floorspace_all, coords_tract,
                         by = c("latitude", "longitude"),
                         all.x = TRUE)
toc()

n_before <- nrow(floorspace_pins)
floorspace_pins <- floorspace_pins[!is.na(census_tract_geoid)]
n_after <- nrow(floorspace_pins)
if (n_before != n_after) {
  message(sprintf("  Removed %s records outside boundaries", format(n_before - n_after, big.mark = ",")))
}

# --- Summary ---
message(sprintf("Total PINs with tract assignment: %s", format(nrow(floorspace_pins), big.mark = ",")))
message(sprintf("  Residential: %s", format(sum(floorspace_pins$property_type == "residential"), big.mark = ",")))
message(sprintf("  Commercial: %s", format(sum(floorspace_pins$property_type == "commercial"), big.mark = ",")))
message(sprintf("  Multifamily: %s", format(sum(floorspace_pins$property_type == "multifamily"), big.mark = ",")))

# --- Aggregate to tract level ---
message("Aggregating to tract level...")

tract_by_type <- floorspace_pins[, .(
  total_sqft = sum(building_sqft, na.rm = TRUE),
  num_buildings = .N,
  avg_year_built = mean(year_built, na.rm = TRUE)
), by = .(census_tract_geoid, property_type)]

tract_wide <- dcast(tract_by_type,
                    census_tract_geoid ~ property_type,
                    value.var = c("total_sqft", "num_buildings"),
                    fill = 0)

tract_wide[, total_sqft_all := total_sqft_residential + total_sqft_commercial +
             total_sqft_multifamily]
tract_wide[, num_buildings_all := num_buildings_residential + num_buildings_commercial +
             num_buildings_multifamily]

floorspace_tracts <- merge(tracts_attr, tract_wide, by = "census_tract_geoid", all.x = TRUE)

sqft_cols <- grep("^total_sqft_|^num_buildings_", names(floorspace_tracts), value = TRUE)
for (col in sqft_cols) {
  floorspace_tracts[is.na(get(col)), (col) := 0]
}

floorspace_tracts[, sqft_per_sqmi := total_sqft_all / land_area_sqmi]
floorspace_tracts[, sqft_per_capita := fifelse(total_pop > 0, total_sqft_all / total_pop, NA_real_)]
floorspace_tracts[, sqft_per_housing_unit := fifelse(total_housing_units > 0,
                                                      total_sqft_residential / total_housing_units,
                                                      NA_real_)]

message(sprintf("Tracts in output: %s, with data: %s",
                nrow(floorspace_tracts),
                sum(floorspace_tracts$total_sqft_all > 0)))

message(sprintf("Total floorspace: Residential %.1fM, Commercial %.1fM, Multifamily %.1fM sqft",
                sum(floorspace_tracts$total_sqft_residential) / 1e6,
                sum(floorspace_tracts$total_sqft_commercial) / 1e6,
                sum(floorspace_tracts$total_sqft_multifamily) / 1e6))

# --- Save ---
message("Saving outputs...")

setcolorder(floorspace_pins, c("pin", "building_sqft", "year_built", "property_type",
                                "latitude", "longitude", "census_tract_geoid"))

fwrite(floorspace_pins, "../output/floorspace_by_pin.csv")
fwrite(floorspace_tracts, "../output/floorspace_by_tract.csv")

message("Done")
