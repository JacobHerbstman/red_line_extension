# ============================================================================
# Red Line Extension: Station Locations and Affected Tracts
# ============================================================================

library(tidyverse)
library(sf)

# ----------------------------------------------------------------------------
# New RLE Station Locations (approximate coordinates)
# ----------------------------------------------------------------------------

# Based on CTA documentation:
# - 103rd Street station: near 103rd & Eggleston Ave
# - 111th Street station: near 111th & Eggleston Ave  
# - Michigan Avenue station: near 116th & Michigan Ave
# - 130th Street station: near 130th & Cottage Grove (by Altgeld Gardens)

# Approximate coordinates (looked up from addresses)
rle_stations <- tibble(
  station_name = c("103rd Street", "111th Street", "Michigan Avenue", "130th Street"),
  lat = c(41.7068, 41.6926, 41.6839, 41.6534),
  lon = c(-87.6326, -87.6326, -87.6217, -87.6046)
)

# Also include existing 95th/Dan Ryan for reference
existing_95th <- tibble(
  station_name = "95th/Dan Ryan (existing)",
  lat = 41.7222,
  lon = -87.6243
)

# Key downtown stations for reference (destinations)
downtown_stations <- tibble(
  station_name = c("Roosevelt", "Jackson", "Monroe", "Lake"),
  lat = c(41.8674, 41.8781, 41.8808, 41.8857),
  lon = c(-87.6266, -87.6277, -87.6277, -87.6277)
)

cat(strrep("=", 60), "\n")
cat("RED LINE EXTENSION STATIONS\n")
cat(strrep("=", 60), "\n\n")

print(rle_stations)

# ----------------------------------------------------------------------------
# Load Census Tracts
# ----------------------------------------------------------------------------

tracts_sf <- st_read("../input/chicago_tracts.gpkg", quiet = TRUE)
cat("\nLoaded", nrow(tracts_sf), "census tracts\n")

# ----------------------------------------------------------------------------
# Find Tracts Near Each Station
# ----------------------------------------------------------------------------

# Convert stations to sf
stations_sf <- st_as_sf(rle_stations, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(tracts_sf))

# Define catchment radius (how far people will walk to a station)
# Typical transit planning uses 0.5 mile (800m) for rail
CATCHMENT_RADIUS_M <- 800

# Buffer around each station
station_buffers <- st_buffer(stations_sf, CATCHMENT_RADIUS_M)

# Find tracts that intersect each station buffer
cat("\n" , strrep("=", 60), "\n")
cat("TRACTS WITHIN", CATCHMENT_RADIUS_M, "m OF EACH STATION\n")
cat(strrep("=", 60), "\n\n")

affected_tracts <- list()

for (i in 1:nrow(stations_sf)) {
  station <- rle_stations$station_name[i]
  buffer <- station_buffers[i, ]
  
  # Find intersecting tracts
  intersects <- st_intersects(tracts_sf, buffer, sparse = FALSE)[, 1]
  tracts_near <- tracts_sf$GEOID[intersects]
  
  cat(station, "station:\n")
  cat("  Tracts:", paste(tracts_near, collapse = ", "), "\n")
  cat("  Count:", length(tracts_near), "\n\n")
  
  affected_tracts[[station]] <- tracts_near
}

# All unique affected tracts
all_affected <- unique(unlist(affected_tracts))
cat("Total unique tracts affected:", length(all_affected), "\n")

# ----------------------------------------------------------------------------
# Compute Travel Time Savings
# ----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("ESTIMATING TRAVEL TIME CHANGES\n")
cat(strrep("=", 60), "\n\n")

# Current situation: residents in far south side must take bus to 95th, then Red Line
# New situation: can walk to new station, take Red Line directly

# Key facts from CTA:
# - 30 minute time savings from 130th to Loop
# - 5.6 miles of new track

# Red Line travel time: approximately 2 min between stations (average)
# - 95th to Roosevelt (downtown): about 25-30 minutes
# - Each new station adds ~2-3 minutes

# Current travel time from affected areas:
# - Bus to 95th: 15-30 min depending on location
# - Wait for train: 5-10 min
# - Train to Loop: 25-30 min
# Total: 45-70 min

# New travel time:
# - Walk to station: 5-10 min
# - Wait for train: 5-10 min  
# - Train to Loop: 30-45 min (depending on station)
# Total: 40-65 min

# Net savings: 5-30 min depending on exact origin/destination

# For our model, we'll translate this to a DISTANCE EQUIVALENT reduction
# Our gravity parameter ν ≈ 0.11 means each km adds about 11% to commuting disutility
# A 20-minute time savings at typical speeds (~30 km/h) ≈ 10 km equivalent

# Approach: For tracts near new stations, reduce effective distance to 
# Red Line accessible destinations

# Distance from each new station to Loop (Roosevelt station as proxy)
loop_lat <- 41.8674
loop_lon <- -87.6266

station_to_loop_km <- rle_stations %>%
  mutate(
    dist_to_loop_km = sqrt((lat - loop_lat)^2 + (lon - loop_lon)^2) * 111  # rough conversion
  )

cat("Distance from new stations to Loop (Roosevelt):\n")
print(station_to_loop_km %>% select(station_name, dist_to_loop_km))

# Current 95th to Loop
dist_95th_to_loop <- sqrt((41.7222 - loop_lat)^2 + (-87.6243 - loop_lon)^2) * 111
cat("\n95th/Dan Ryan to Loop:", round(dist_95th_to_loop, 2), "km\n")

# ----------------------------------------------------------------------------
# Calibrating κ Change
# ----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CALIBRATING COMMUTING COST CHANGE\n")
cat(strrep("=", 60), "\n\n")

# Our commuting cost: κ_ni = exp(κ * d_ni) where κ = ν/ε ≈ 0.016

# Key insight: The RLE doesn't just reduce distance, it provides RAIL access
# which is faster than bus/car for the same distance.

# Approach 1: Mode shift premium
# Rail is roughly 2x faster than bus for same distance in urban areas
# So rail access effectively halves the "distance" in utility terms

# Approach 2: Direct time calibration
# CTA says 30 min savings. At our parameters:
# - 30 min ≈ 15 km at 30 km/h average
# - Utility change: exp(-0.016 * 15) ≈ 0.79, so about 21% utility gain

# For simplicity, we'll use:
# - Tracts within 800m of new station get rail access
# - Their effective distance to all Red Line destinations is reduced
# - Reduction factor: captures both distance saved and mode shift

# Conservative approach: use actual distance savings plus a mode premium

cat("Model parameters:\n")
cat("  ν (semi-elasticity): 0.11\n")
cat("  ε (Fréchet shape): 6.83\n")
cat("  κ = ν/ε: 0.016\n\n")

# For a tract currently at centroid distance d from Loop:
# - Current: must travel to 95th (d1), then Red Line (d2), total utility cost exp(κ*d)
# - New: walk to new station (small), Red Line to Loop
# - Savings come from (1) shorter rail distance, (2) no bus leg

cat("Proposed approach:\n")
cat("  1. For tracts within 800m of new stations:\n")
cat("  2. Compute new effective distance to Red Line destinations\n")
cat("  3. d'_ni = d(tract to station) + d(station to destination via rail)\n")
cat("  4. Compare to current Euclidean distance d_ni\n")
cat("  5. Use minimum of old and new distance\n\n")

# ----------------------------------------------------------------------------
# Save Station and Tract Info
# ----------------------------------------------------------------------------

# Save station locations
write_csv(rle_stations, "../output/rle_stations.csv")
cat("Saved station locations to: rle_stations.csv\n")

# Save affected tracts
affected_df <- tibble(
  tract = all_affected,
  near_station = sapply(all_affected, function(t) {
    stations <- names(affected_tracts)[sapply(affected_tracts, function(x) t %in% x)]
    paste(stations, collapse = "; ")
  })
)
write_csv(affected_df, "../output/rle_affected_tracts.csv")
cat("Saved affected tracts to: rle_affected_tracts.csv\n")

# ----------------------------------------------------------------------------
# Map
# ----------------------------------------------------------------------------

cat("\nCreating map...\n")

# Get tract centroids
tract_centroids <- st_centroid(tracts_sf) %>%
  mutate(
    affected = GEOID %in% all_affected
  )

# Plot
p <- ggplot() +
  geom_sf(data = tracts_sf, fill = "gray95", color = "gray70", size = 0.1) +
  geom_sf(data = tracts_sf %>% filter(GEOID %in% all_affected), 
          fill = "lightblue", color = "blue", size = 0.3) +
  geom_sf(data = station_buffers, fill = NA, color = "red", linetype = "dashed", size = 0.8) +
  geom_sf(data = stations_sf, color = "red", size = 3) +
  geom_text(data = rle_stations, aes(x = lon, y = lat, label = station_name),
            nudge_y = 0.005, size = 3, fontface = "bold") +
  labs(
    title = "Red Line Extension: New Stations and Affected Tracts",
    subtitle = paste0("800m catchment radius; ", length(all_affected), " tracts affected"),
    caption = "Blue tracts are within walking distance of new stations"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("../output/rle_stations_map.pdf", p, width = 8, height = 10)
cat("Saved map to: rle_stations_map.pdf\n")

# Zoomed map of just the RLE area
p_zoom <- ggplot() +
  geom_sf(data = tracts_sf, fill = "gray95", color = "gray70", size = 0.2) +
  geom_sf(data = tracts_sf %>% filter(GEOID %in% all_affected), 
          fill = "lightblue", color = "blue", size = 0.5) +
  geom_sf(data = station_buffers, fill = NA, color = "red", linetype = "dashed", size = 1) +
  geom_sf(data = stations_sf, color = "red", size = 4) +
  geom_text(data = rle_stations, aes(x = lon, y = lat, label = station_name),
            nudge_x = 0.015, size = 3.5, fontface = "bold", hjust = 0) +
  coord_sf(xlim = c(-87.70, -87.55), ylim = c(41.63, 41.74)) +
  labs(
    title = "Red Line Extension: Detail View",
    subtitle = paste0("New stations and ", length(all_affected), " affected tracts")
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("../output/rle_stations_map_zoom.pdf", p_zoom, width = 8, height = 8)
cat("Saved zoomed map to: rle_stations_map_zoom.pdf\n")

cat("\nDone!\n")
