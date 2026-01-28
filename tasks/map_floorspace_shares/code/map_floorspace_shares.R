# Map floorspace shares by census tract
# Uses Option 2 classification: residential = Class 2xx + Class 3xx, commercial = Class 5xx
source("../../setup_environment/code/packages.R")

# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading data...")

# Load floorspace by tract
floorspace <- fread("../input/floorspace_by_tract.csv")
# Convert census_tract_geoid to character for consistent joining
floorspace[, census_tract_geoid := as.character(census_tract_geoid)]
message(sprintf("  Loaded %s tracts", nrow(floorspace)))

# Load tract geometries
tracts_sf <- st_read("../input/chicago_tracts_2010.gpkg", quiet = TRUE)
message(sprintf("  Loaded %s tract geometries", nrow(tracts_sf)))

# =============================================================================
# RECALCULATE SHARES WITH OPTION 2 CLASSIFICATION
# =============================================================================

message("\n" , strrep("=", 60))
message("RECALCULATING SHARES (Option 2: Residential = 2xx + 3xx)")
message(strrep("=", 60))

# Combine residential + multifamily as "residential" (where people LIVE)
# Keep commercial (Class 5xx) separate (where people WORK)
floorspace[, total_sqft_residential_combined := total_sqft_residential + total_sqft_multifamily]
floorspace[, total_sqft_commercial_only := total_sqft_commercial]
floorspace[, total_sqft_all_combined := total_sqft_residential_combined + total_sqft_commercial_only]

# Calculate shares
# Handle tracts with zero floorspace by setting share to NA
floorspace[, residential_share := fifelse(total_sqft_all_combined > 0,
                                           total_sqft_residential_combined / total_sqft_all_combined,
                                           NA_real_)]
floorspace[, commercial_share := fifelse(total_sqft_all_combined > 0,
                                          total_sqft_commercial_only / total_sqft_all_combined,
                                          NA_real_)]

# Calculate density
floorspace[, sqft_per_sqmi := total_sqft_all_combined / land_area_sqmi]
floorspace[, log_commercial_sqft := log(total_sqft_commercial_only + 1)]

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

message("\n" , strrep("=", 60))
message("SUMMARY STATISTICS")
message(strrep("=", 60))

# City-wide totals
total_res <- sum(floorspace$total_sqft_residential_combined, na.rm = TRUE)
total_comm <- sum(floorspace$total_sqft_commercial_only, na.rm = TRUE)
total_all <- total_res + total_comm

message("\nCity-wide totals:")
message(sprintf("  Residential (2xx + 3xx): %.1f M sqft (%.1f%%)",
                total_res / 1e6, total_res / total_all * 100))
message(sprintf("  Commercial (5xx only):   %.1f M sqft (%.1f%%)",
                total_comm / 1e6, total_comm / total_all * 100))
message(sprintf("  Total:                   %.1f M sqft", total_all / 1e6))

# Distribution of shares across tracts
valid_shares <- floorspace[!is.na(residential_share)]
message("\nDistribution of residential share across tracts:")
message(sprintf("  Mean:   %.1f%%", mean(valid_shares$residential_share) * 100))
message(sprintf("  Median: %.1f%%", median(valid_shares$residential_share) * 100))
message(sprintf("  P10:    %.1f%%", quantile(valid_shares$residential_share, 0.10) * 100))
message(sprintf("  P90:    %.1f%%", quantile(valid_shares$residential_share, 0.90) * 100))
message(sprintf("  Min:    %.1f%%", min(valid_shares$residential_share) * 100))
message(sprintf("  Max:    %.1f%%", max(valid_shares$residential_share) * 100))

# Count extreme tracts
n_high_res <- sum(valid_shares$residential_share > 0.90, na.rm = TRUE)
n_high_comm <- sum(valid_shares$commercial_share > 0.50, na.rm = TRUE)
n_zero <- sum(floorspace$total_sqft_all_combined == 0)

message("\nTract counts:")
message(sprintf("  Tracts >90%% residential: %d (%.1f%%)",
                n_high_res, n_high_res / nrow(valid_shares) * 100))
message(sprintf("  Tracts >50%% commercial:  %d (%.1f%%)",
                n_high_comm, n_high_comm / nrow(valid_shares) * 100))
message(sprintf("  Tracts with zero floorspace: %d", n_zero))

# Distance to Loop analysis
# The Loop is roughly at tract 17031839100 or nearby (around 41.88, -87.63)
loop_lat <- 41.8819
loop_lon <- -87.6278

floorspace[, dist_to_loop_km := sqrt((centroid_lat - loop_lat)^2 +
                                      (centroid_lon - loop_lon)^2) * 111]  # rough km conversion

# Correlation
valid_for_corr <- floorspace[!is.na(commercial_share) & !is.na(dist_to_loop_km)]
corr <- cor(valid_for_corr$commercial_share, valid_for_corr$dist_to_loop_km)
message(sprintf("\nCorrelation: commercial_share vs distance_to_loop = %.3f", corr))
message("  (Negative correlation expected: commercial share decreases with distance)")

# =============================================================================
# PREPARE DATA FOR MAPPING
# =============================================================================

message("\n" , strrep("=", 60))
message("PREPARING MAPS")
message(strrep("=", 60))

# Merge floorspace data with geometries
tracts_sf <- tracts_sf %>%
  left_join(floorspace[, .(census_tract_geoid, residential_share, commercial_share,
                           log_commercial_sqft, sqft_per_sqmi, total_sqft_commercial_only)],
            by = c("GEOID" = "census_tract_geoid"))

# City-wide mean for reference line
city_mean_res_share <- total_res / total_all

# Key locations for annotation
landmarks <- data.frame(
  name = c("Loop", "O'Hare", "Midway"),
  lon = c(-87.6278, -87.9073, -87.7522),
  lat = c(41.8819, 41.9742, 41.7868)
)

# =============================================================================
# MAP 1: RESIDENTIAL SHARE
# =============================================================================

message("\nCreating residential share map...")

p1 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = residential_share), color = "gray40", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#d73027",      # red for low residential (high commercial)
    mid = "white",
    high = "#4575b4",     # blue for high residential
    midpoint = city_mean_res_share,
    limits = c(0, 1),
    na.value = "gray80",
    labels = scales::percent
  ) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "black", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Residential Floorspace Share by Census Tract",
    subtitle = sprintf("Option 2: Residential = Class 2xx + 3xx (city mean = %.0f%%)", city_mean_res_share * 100),
    fill = "Residential\nShare"
  )

ggsave("../output/residential_share_map.pdf", p1, width = 10, height = 12, dpi = 150)
message("  Saved: ../output/residential_share_map.pdf")

# =============================================================================
# MAP 2: COMMERCIAL SHARE
# =============================================================================

message("Creating commercial share map...")

p2 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = commercial_share), color = "gray40", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "plasma",
    limits = c(0, 1),
    na.value = "gray80",
    labels = scales::percent
  ) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "white", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold", color = "white") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Commercial Floorspace Share by Census Tract",
    subtitle = "Class 5xx only (offices, retail, industrial)",
    fill = "Commercial\nShare"
  )

ggsave("../output/commercial_share_map.pdf", p2, width = 10, height = 12, dpi = 150)
message("  Saved: ../output/commercial_share_map.pdf")

# =============================================================================
# MAP 3: LOG COMMERCIAL SQFT (LEVELS)
# =============================================================================

message("Creating log commercial sqft map...")

p3 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = log_commercial_sqft), color = "gray40", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "gray80"
  ) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "white", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold", color = "white") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Commercial Floorspace (log sqft) by Census Tract",
    subtitle = "Shows intensity of commercial activity",
    fill = "log(sqft + 1)"
  )

ggsave("../output/log_commercial_sqft_map.pdf", p3, width = 10, height = 12, dpi = 150)
message("  Saved: ../output/log_commercial_sqft_map.pdf")

# =============================================================================
# MAP 4: TOTAL FLOORSPACE DENSITY
# =============================================================================

message("Creating floorspace density map...")

# Cap extreme values for visualization
density_cap <- quantile(tracts_sf$sqft_per_sqmi, 0.99, na.rm = TRUE)
tracts_sf$sqft_per_sqmi_capped <- pmin(tracts_sf$sqft_per_sqmi, density_cap)

p4 <- ggplot(tracts_sf) +
  geom_sf(aes(fill = sqft_per_sqmi_capped / 1e6), color = "gray40", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "inferno",
    na.value = "gray80",
    labels = function(x) paste0(round(x, 1), "M")
  ) +
  geom_point(data = landmarks, aes(x = lon, y = lat),
             color = "white", size = 2, shape = 4, stroke = 1.5) +
  geom_text(data = landmarks, aes(x = lon, y = lat, label = name),
            nudge_y = 0.02, size = 3, fontface = "bold", color = "white") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "Total Floorspace Density by Census Tract",
    subtitle = sprintf("Capped at 99th percentile (%.0f M sqft/sqmi)", density_cap / 1e6),
    fill = "M sqft/sqmi"
  )

ggsave("../output/total_floorspace_density_map.pdf", p4, width = 10, height = 12, dpi = 150)
message("  Saved: ../output/total_floorspace_density_map.pdf")

# =============================================================================
# SAVE CLEAN TRACT FILE FOR MODEL
# =============================================================================

message("\n" , strrep("=", 60))
message("SAVING MODEL DATA")
message(strrep("=", 60))

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
message(sprintf("Saved: ../output/tract_floorspace_for_model.csv (%s rows)", nrow(model_data)))

# Final verification
message("\nVerification:")
message(sprintf("  All shares sum to 1: %s",
                all(abs(model_data$residential_share + model_data$commercial_share - 1) < 0.001, na.rm = TRUE)))
message(sprintf("  Tracts with valid shares: %d of %d",
                sum(!is.na(model_data$residential_share)), nrow(model_data)))

message("\nDone!")
