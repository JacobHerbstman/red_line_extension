# Red Line Extension: station locations and affected tracts

library(tidyverse)
library(sf)

# --- Station locations ---
# From CTA documentation

rle_stations <- tibble(
  station_name = c("103rd Street", "111th Street", "Michigan Avenue", "130th Street"),
  lat = c(41.7068, 41.6926, 41.6839, 41.6534),
  lon = c(-87.6326, -87.6326, -87.6217, -87.6046)
)

existing_95th <- tibble(
  station_name = "95th/Dan Ryan (existing)",
  lat = 41.7222,
  lon = -87.6243
)

downtown_stations <- tibble(
  station_name = c("Roosevelt", "Jackson", "Monroe", "Lake"),
  lat = c(41.8674, 41.8781, 41.8808, 41.8857),
  lon = c(-87.6266, -87.6277, -87.6277, -87.6277)
)

cat(strrep("=", 60), "\n")
cat("Red Line Extension Stations\n")
cat(strrep("=", 60), "\n\n")

print(rle_stations)

# --- Load tracts ---
tracts_sf <- st_read("../input/chicago_tracts.gpkg", quiet = TRUE)
cat("\nLoaded", nrow(tracts_sf), "census tracts\n")

# --- Find affected tracts ---
stations_sf <- st_as_sf(rle_stations, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(tracts_sf))

CATCHMENT_RADIUS_M <- 800

station_buffers <- st_buffer(stations_sf, CATCHMENT_RADIUS_M)

cat("\nTracts within", CATCHMENT_RADIUS_M, "m of each station:\n\n")

affected_tracts <- list()

for (i in 1:nrow(stations_sf)) {
  station <- rle_stations$station_name[i]
  buffer <- station_buffers[i, ]

  intersects <- st_intersects(tracts_sf, buffer, sparse = FALSE)[, 1]
  tracts_near <- tracts_sf$GEOID[intersects]

  cat(station, ":\n")
  cat("  Tracts:", paste(tracts_near, collapse = ", "), "\n")
  cat("  Count:", length(tracts_near), "\n\n")

  affected_tracts[[station]] <- tracts_near
}

all_affected <- unique(unlist(affected_tracts))
cat("Total unique tracts affected:", length(all_affected), "\n")

# --- Distance to Loop ---
cat("\nDistance from new stations to Loop (Roosevelt):\n")

loop_lat <- 41.8674
loop_lon <- -87.6266

station_to_loop_km <- rle_stations %>%
  mutate(dist_to_loop_km = sqrt((lat - loop_lat)^2 + (lon - loop_lon)^2) * 111)

print(station_to_loop_km %>% select(station_name, dist_to_loop_km))

dist_95th_to_loop <- sqrt((41.7222 - loop_lat)^2 + (-87.6243 - loop_lon)^2) * 111
cat("\n95th/Dan Ryan to Loop:", round(dist_95th_to_loop, 2), "km\n")

# --- Save ---
write_csv(rle_stations, "../output/rle_stations.csv")
cat("\nSaved: rle_stations.csv\n")

affected_df <- tibble(
  tract = all_affected,
  near_station = sapply(all_affected, function(t) {
    stations <- names(affected_tracts)[sapply(affected_tracts, function(x) t %in% x)]
    paste(stations, collapse = "; ")
  })
)
write_csv(affected_df, "../output/rle_affected_tracts.csv")
cat("Saved: rle_affected_tracts.csv\n")

# --- Map ---
cat("\nCreating map...\n")

tract_centroids <- st_centroid(tracts_sf) %>%
  mutate(affected = GEOID %in% all_affected)

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
    subtitle = paste0("800m catchment; ", length(all_affected), " tracts affected")
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())

ggsave("../output/rle_stations_map.pdf", p, width = 8, height = 10)
cat("Saved: rle_stations_map.pdf\n")

# Zoomed map
p_zoom <- ggplot() +
  geom_sf(data = tracts_sf, fill = "gray95", color = "gray70", size = 0.2) +
  geom_sf(data = tracts_sf %>% filter(GEOID %in% all_affected),
          fill = "lightblue", color = "blue", size = 0.5) +
  geom_sf(data = station_buffers, fill = NA, color = "red", linetype = "dashed", size = 1) +
  geom_sf(data = stations_sf, color = "red", size = 4) +
  geom_text(data = rle_stations, aes(x = lon, y = lat, label = station_name),
            nudge_x = 0.015, size = 3.5, fontface = "bold", hjust = 0) +
  coord_sf(xlim = c(-87.70, -87.55), ylim = c(41.63, 41.74)) +
  labs(title = "Red Line Extension: Detail View",
       subtitle = paste0(length(all_affected), " affected tracts")) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())

ggsave("../output/rle_stations_map_zoom.pdf", p_zoom, width = 8, height = 8)
cat("Saved: rle_stations_map_zoom.pdf\n")

cat("\nDone\n")
