# Map floorspace shares by census tract
# Classification: residential = Class 2xx + 3xx, commercial = Class 5xx

source("../../setup_environment/code/packages.R")

# --- Load data ---
message("Loading data...")

floorspace <- fread("../input/floorspace_by_tract.csv")
floorspace[, census_tract_geoid := as.character(census_tract_geoid)]
message(sprintf("  Loaded %s tracts", nrow(floorspace)))

tracts_sf <- st_read("../input/chicago_tracts_2010.gpkg", quiet = TRUE)
message(sprintf("  Loaded %s tract geometries", nrow(tracts_sf)))

# --- Recalculate shares ---
message("Calculating shares (residential = 2xx + 3xx)...")

floorspace[, total_sqft_residential_combined := total_sqft_residential + total_sqft_multifamily]
floorspace[, total_sqft_commercial_only := total_sqft_commercial]
floorspace[, total_sqft_all_combined := total_sqft_residential_combined + total_sqft_commercial_only]

floorspace[, residential_share := fifelse(total_sqft_all_combined > 0,
                                           total_sqft_residential_combined / total_sqft_all_combined,
                                           NA_real_)]
floorspace[, commercial_share := fifelse(total_sqft_all_combined > 0,
                                          total_sqft_commercial_only / total_sqft_all_combined,
                                          NA_real_)]

floorspace[, sqft_per_sqmi := total_sqft_all_combined / land_area_sqmi]
floorspace[, log_commercial_sqft := log(total_sqft_commercial_only + 1)]

# --- Summary ---
message("Summary statistics...")

total_res <- sum(floorspace$total_sqft_residential_combined, na.rm = TRUE)
total_comm <- sum(floorspace$total_sqft_commercial_only, na.rm = TRUE)
total_all <- total_res + total_comm

message(sprintf("  Residential: %.1fM sqft (%.1f%%)", total_res / 1e6, total_res / total_all * 100))
message(sprintf("  Commercial:  %.1fM sqft (%.1f%%)", total_comm / 1e6, total_comm / total_all * 100))

valid_shares <- floorspace[!is.na(residential_share)]
message(sprintf("  Mean residential share: %.1f%%, Median: %.1f%%",
                mean(valid_shares$residential_share) * 100,
                median(valid_shares$residential_share) * 100))

n_high_res <- sum(valid_shares$residential_share > 0.90, na.rm = TRUE)
n_high_comm <- sum(valid_shares$commercial_share > 0.50, na.rm = TRUE)

message(sprintf("  Tracts >90%% residential: %d, >50%% commercial: %d", n_high_res, n_high_comm))

# Distance to Loop
loop_lat <- 41.8819
loop_lon <- -87.6278

floorspace[, dist_to_loop_km := sqrt((centroid_lat - loop_lat)^2 +
                                      (centroid_lon - loop_lon)^2) * 111]

valid_for_corr <- floorspace[!is.na(commercial_share) & !is.na(dist_to_loop_km)]
corr <- cor(valid_for_corr$commercial_share, valid_for_corr$dist_to_loop_km)
message(sprintf("  Correlation (commercial share vs distance to Loop): %.3f", corr))

# --- Maps ---
message("Creating maps...")

tracts_sf <- tracts_sf %>%
  left_join(floorspace[, .(census_tract_geoid, residential_share, commercial_share,
                           log_commercial_sqft, sqft_per_sqmi, total_sqft_commercial_only)],
            by = c("GEOID" = "census_tract_geoid"))

city_mean_res_share <- total_res / total_all

landmarks <- data.frame(
  name = c("Loop", "O'Hare", "Midway"),
  lon = c(-87.6278, -87.9073, -87.7522),
  lat = c(41.8819, 41.9742, 41.7868)
)

# Residential share
p1 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = residential_share), color = "gray40", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#4575b4",
    midpoint = city_mean_res_share,
    limits = c(0, 1), na.value = "gray80",
    labels = scales::percent
  ) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "black", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), legend.position = "right") +
  labs(title = "Residential Floorspace Share",
       subtitle = sprintf("City mean = %.0f%%", city_mean_res_share * 100),
       fill = "Residential\nShare")

ggsave("../output/residential_share_map.pdf", p1, width = 10, height = 12, dpi = 150)
message("  Saved: residential_share_map.pdf")

# Commercial share
p2 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = commercial_share), color = "gray40", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 1),
                       na.value = "gray80", labels = scales::percent) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "white", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold", color = "white") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), legend.position = "right") +
  labs(title = "Commercial Floorspace Share", fill = "Commercial\nShare")

ggsave("../output/commercial_share_map.pdf", p2, width = 10, height = 12, dpi = 150)
message("  Saved: commercial_share_map.pdf")

# Log commercial sqft
p3 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = log_commercial_sqft), color = "gray40", linewidth = 0.05) +
  scale_fill_viridis_c(option = "magma", na.value = "gray80") +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "white", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold", color = "white") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), legend.position = "right") +
  labs(title = "Commercial Floorspace (log sqft)", fill = "log(sqft + 1)")

ggsave("../output/log_commercial_sqft_map.pdf", p3, width = 10, height = 12, dpi = 150)
message("  Saved: log_commercial_sqft_map.pdf")

# Density
density_cap <- quantile(tracts_sf$sqft_per_sqmi, 0.99, na.rm = TRUE)
tracts_sf$sqft_per_sqmi_capped <- pmin(tracts_sf$sqft_per_sqmi, density_cap)

p4 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = sqft_per_sqmi_capped / 1e6), color = "gray40", linewidth = 0.05) +
  scale_fill_viridis_c(option = "inferno", na.value = "gray80",
                       labels = function(x) paste0(round(x, 1), "M")) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "white", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold", color = "white") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), legend.position = "right") +
  labs(title = "Total Floorspace Density",
       subtitle = sprintf("Capped at 99th pct (%.0fM sqft/sqmi)", density_cap / 1e6),
       fill = "M sqft/sqmi")

ggsave("../output/total_floorspace_density_map.pdf", p4, width = 10, height = 12, dpi = 150)
message("  Saved: total_floorspace_density_map.pdf")

# --- Save model data ---
message("Saving model data...")

model_data <- floorspace[, .(
  census_tract_geoid,
  total_sqft_residential = total_sqft_residential_combined,
  total_sqft_commercial = total_sqft_commercial_only,
  total_sqft_all = total_sqft_all_combined,
  residential_share,
  commercial_share,
  land_area_sqmi,
  centroid_lat,
  centroid_lon
)]

fwrite(model_data, "../output/tract_floorspace_for_model.csv")
message(sprintf("Saved: tract_floorspace_for_model.csv (%s rows)", nrow(model_data)))

message(sprintf("Verification: all shares sum to 1: %s",
                all(abs(model_data$residential_share + model_data$commercial_share - 1) < 0.001, na.rm = TRUE)))

message("Done")
