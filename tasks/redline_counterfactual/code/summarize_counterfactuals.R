# ============================================================================
# Summarize Red Line Extension Counterfactual Results
# ============================================================================

library(tidyverse)
library(sf)
library(scales)
library(gridExtra)
library(knitr)

# ----------------------------------------------------------------------------
# Load Data
# ----------------------------------------------------------------------------

cat("Loading data...\n")

# Specification grid
catchment_radii <- c(800)
kappa_reductions <- c(20)

# Load all welfare results
welfare_results <- map_dfr(catchment_radii, function(r) {
  map_dfr(kappa_reductions, function(k) {
    file <- sprintf("../output/welfare_r%d_k%d.csv", r, k)
    if (file.exists(file)) {
      read_csv(file, show_col_types = FALSE)
    } else {
      NULL
    }
  })
})

cat("Loaded", nrow(welfare_results), "specifications\n")

# Load main specification counterfactual (800m, 20%)
main_spec <- read_csv("../output/counterfactual_r800_k20.csv", show_col_types = FALSE) %>%
  mutate(tract = as.character(tract))  # Convert to character for joining

# Load tract geometries
tracts_sf <- st_read("../input/chicago_tracts.gpkg", quiet = TRUE) %>%
  mutate(GEOID = as.character(GEOID))

# Debug: Check join compatibility
cat("\nJoin diagnostics:\n")
cat("  Tracts in shapefile:", nrow(tracts_sf), "\n")
cat("  Tracts in counterfactual:", nrow(main_spec), "\n")
cat("  Sample GEOID from shapefile:", head(tracts_sf$GEOID, 3), "\n")
cat("  Sample tract from counterfactual:", head(main_spec$tract, 3), "\n")

# Merge with main spec results
main_spec_sf <- tracts_sf %>%
  left_join(main_spec, by = c("GEOID" = "tract"))

# Check join success
matched <- sum(!is.na(main_spec_sf$Q_pct_change))
unmatched <- sum(is.na(main_spec_sf$Q_pct_change))
cat("  Matched tracts:", matched, "\n")
cat("  Unmatched tracts (will show as gray):", unmatched, "\n")

# ----------------------------------------------------------------------------
# Print Welfare Summary Table
# ----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("WELFARE RESULTS BY SPECIFICATION\n")
cat(strrep("=", 70), "\n\n")

welfare_table <- welfare_results %>%
  select(catchment_radius_m, kappa_reduction_pct, n_treated_tracts, 
         welfare_change_pct, mean_Q_hat_treated, mean_L_R_hat_treated) %>%
  mutate(
    welfare_change_pct = round(welfare_change_pct, 4),
    mean_Q_hat_treated = round(mean_Q_hat_treated, 4),
    mean_L_R_hat_treated = round(mean_L_R_hat_treated, 4)
  ) %>%
  arrange(catchment_radius_m, kappa_reduction_pct)

print(welfare_table)

# ----------------------------------------------------------------------------
# Save Welfare Summary
# ----------------------------------------------------------------------------

write_csv(welfare_table, "../output/welfare_summary.csv")
cat("\nSaved: welfare_summary.csv\n")

# ----------------------------------------------------------------------------
# Main Specification Summary
# ----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("MAIN SPECIFICATION (800m, 20% reduction)\n")
cat(strrep("=", 70), "\n\n")

main_welfare <- welfare_results %>%
  filter(catchment_radius_m == 800, kappa_reduction_pct == 20)

cat(sprintf("Treated tracts: %d\n", main_welfare$n_treated_tracts))
cat(sprintf("Welfare change: %.4f%%\n", main_welfare$welfare_change_pct))
cat(sprintf("Mean floor price change (treated): %.2f%%\n", (main_welfare$mean_Q_hat_treated - 1) * 100))
cat(sprintf("Mean population change (treated): %.2f%%\n", (main_welfare$mean_L_R_hat_treated - 1) * 100))

# Treated vs untreated comparison
cat("\nTreated tracts:\n")
main_spec %>%
  filter(treated) %>%
  summarise(
    n = n(),
    mean_Q_pct_change = mean(Q_pct_change),
    mean_L_R_pct_change = mean(L_R_pct_change),
    median_Q_pct_change = median(Q_pct_change),
    median_L_R_pct_change = median(L_R_pct_change)
  ) %>%
  print()

cat("\nUntreated tracts:\n")
main_spec %>%
  filter(!treated) %>%
  summarise(
    n = n(),
    mean_Q_pct_change = mean(Q_pct_change),
    mean_L_R_pct_change = mean(L_R_pct_change),
    median_Q_pct_change = median(Q_pct_change),
    median_L_R_pct_change = median(L_R_pct_change)
  ) %>%
  print()

# Save main spec
write_csv(main_spec, "../output/counterfactual_main_spec.csv")
cat("\nSaved: counterfactual_main_spec.csv\n")

# ----------------------------------------------------------------------------
# Robustness Table (for paper)
# ----------------------------------------------------------------------------

robustness_table <- welfare_results %>%
  mutate(
    spec_label = sprintf("r=%dm, κ↓%d%%", catchment_radius_m, kappa_reduction_pct),
    main_spec = (catchment_radius_m == 800 & kappa_reduction_pct == 20)
  ) %>%
  select(
    spec_label,
    main_spec,
    n_treated_tracts,
    welfare_change_pct,
    mean_Q_hat_treated,
    mean_L_R_hat_treated
  ) %>%
  rename(
    Specification = spec_label,
    `Main Spec` = main_spec,
    `Treated Tracts` = n_treated_tracts,
    `Welfare Δ (%)` = welfare_change_pct,
    `Mean Q̂ (treated)` = mean_Q_hat_treated,
    `Mean L̂_R (treated)` = mean_L_R_hat_treated
  )

write_csv(robustness_table, "../output/robustness_table.csv")
cat("\nSaved: robustness_table.csv\n")

# ----------------------------------------------------------------------------
# Maps
# ----------------------------------------------------------------------------

cat("\nCreating maps...\n")

map_theme <- theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

# RLE station locations for annotation
rle_stations <- tibble(
  station = c("103rd St", "111th St", "Michigan Ave", "130th St"),
  lat = c(41.7068, 41.6926, 41.6839, 41.6534),
  lon = c(-87.6326, -87.6326, -87.6217, -87.6046)
)

# 1. Floor price change map
# Let ggplot2 determine color scale limits dynamically from data
# Use na.value to handle unmatched tracts
p_price <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = Q_pct_change), color = "gray80", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "% Change",
    na.value = "gray90",
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "black", size = 2, shape = 17) +
  labs(
    title = "Floor Price Changes from Red Line Extension",
    subtitle = "Main specification: 800m catchment, 20% kappa reduction"
  ) +
  map_theme

ggsave("../output/map_price_change.pdf", p_price, width = 8, height = 10)
cat("  Saved: map_price_change.pdf\n")

# 2. Population change map
# Let ggplot2 determine color scale limits dynamically from data
p_pop <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = L_R_pct_change), color = "gray80", linewidth = 0.05) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "% Change",
    na.value = "gray90",
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "black", size = 2, shape = 17) +
  labs(
    title = "Residential Population Changes from Red Line Extension",
    subtitle = "Main specification: 800m catchment, 20% kappa reduction"
  ) +
  map_theme

ggsave("../output/map_population_change.pdf", p_pop, width = 8, height = 10)
cat("  Saved: map_population_change.pdf\n")

# 3. Treatment intensity map (showing continuous spatial decay)
# This shows the Gaussian decay of treatment intensity from stations
p_intensity <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = intensity), color = "gray70", linewidth = 0.05) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Treatment\nIntensity",
    trans = "sqrt",  # Square root transform to show gradient better
    limits = c(0, 1),
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "white", size = 3, shape = 17) +
  geom_text(data = rle_stations, aes(x = lon, y = lat, label = station),
            nudge_x = 0.015, size = 2.5, hjust = 0, color = "white") +
  labs(
    title = "Treatment Intensity (Spatial Decay from RLE Stations)",
    subtitle = sprintf("Gaussian decay with σ = 800m; %d tracts with intensity > 10%%", 
                       sum(main_spec$intensity > 0.1, na.rm = TRUE))
  ) +
  map_theme

ggsave("../output/map_welfare_by_tract.pdf", p_intensity, width = 8, height = 10)
cat("  Saved: map_welfare_by_tract.pdf\n")

# 4. Zoomed map of RLE area
p_zoom <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = Q_pct_change), color = "gray50", size = 0.2) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Floor Price\n% Change"
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "black", size = 4, shape = 17) +
  geom_text(data = rle_stations, aes(x = lon, y = lat, label = station),
            nudge_x = 0.012, size = 3, hjust = 0, fontface = "bold") +
  coord_sf(xlim = c(-87.70, -87.55), ylim = c(41.63, 41.75)) +
  labs(
    title = "Red Line Extension: Floor Price Changes (Detail)",
    subtitle = "Main specification: 800m catchment, 20% κ reduction"
  ) +
  map_theme

ggsave("../output/map_price_change_zoom.pdf", p_zoom, width = 8, height = 8)
cat("  Saved: map_price_change_zoom.pdf\n")

# 5. Combined counterfactual maps (2x2 grid like diagnostic maps)
cat("  Creating combined counterfactual maps...\n")

# Common map theme for grid
grid_map_theme <- theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# Wage changes map
p_wages <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = w_pct_change), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Wage % Change",
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "white", size = 1.5, shape = 17) +
  labs(title = "Wage Changes") +
  grid_map_theme

# Floor price changes map  
p_prices <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = Q_pct_change), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Price % Change",
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "white", size = 1.5, shape = 17) +
  labs(title = "Floor Price Changes") +
  grid_map_theme

# Population changes map
p_popgrid <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = L_R_pct_change), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Pop % Change",
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "white", size = 1.5, shape = 17) +
  labs(title = "Population Changes") +
  grid_map_theme

# Treatment intensity map
p_intensity_grid <- ggplot() +
  geom_sf(data = main_spec_sf, aes(fill = intensity), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Intensity",
    trans = "sqrt",
    limits = c(0, 1),
    oob = scales::squish
  ) +
  geom_point(data = rle_stations, aes(x = lon, y = lat), 
             color = "white", size = 1.5, shape = 17) +
  labs(title = "Treatment Intensity") +
  grid_map_theme

# Combine into 2x2 grid
p_combined <- grid.arrange(
  p_wages, p_prices,
  p_popgrid, p_intensity_grid,
  nrow = 2, ncol = 2,
  top = "Red Line Extension Counterfactual Effects"
)

ggsave("../output/counterfactual_maps.pdf", p_combined, width = 10, height = 12)
cat("  Saved: counterfactual_maps.pdf\n")

# ----------------------------------------------------------------------------
# Robustness Plot
# ----------------------------------------------------------------------------

cat("\nCreating robustness plot...\n")

# Plot welfare change across specifications
p_robust <- welfare_results %>%
  mutate(
    catchment_label = factor(sprintf("%dm", catchment_radius_m)),
    reduction_label = factor(sprintf("%d%%", kappa_reduction_pct)),
    main_spec = (catchment_radius_m == 800 & kappa_reduction_pct == 20)
  ) %>%
  ggplot(aes(x = reduction_label, y = welfare_change_pct, 
             color = catchment_label, group = catchment_label)) +
  geom_line(size = 1) +
  geom_point(aes(shape = main_spec), size = 3) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18), guide = "none") +
  scale_color_brewer(palette = "Set1", name = "Catchment\nRadius") +
  labs(
    title = "Welfare Effects Across Specifications",
    subtitle = "Diamond indicates main specification (800m, 20%)",
    x = "Commuting Cost Reduction",
    y = "Welfare Change (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("../output/robustness_plot.pdf", p_robust, width = 8, height = 5)
cat("  Saved: robustness_plot.pdf\n")

# ----------------------------------------------------------------------------
# Final Summary
# ----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Welfare change across specifications:\n")
cat(sprintf("  Min:  %.4f%% (r=%dm, k=%d%%)\n", 
            min(welfare_results$welfare_change_pct),
            welfare_results$catchment_radius_m[which.min(welfare_results$welfare_change_pct)],
            welfare_results$kappa_reduction_pct[which.min(welfare_results$welfare_change_pct)]))
cat(sprintf("  Max:  %.4f%% (r=%dm, k=%d%%)\n", 
            max(welfare_results$welfare_change_pct),
            welfare_results$catchment_radius_m[which.max(welfare_results$welfare_change_pct)],
            welfare_results$kappa_reduction_pct[which.max(welfare_results$welfare_change_pct)]))
cat(sprintf("  Main: %.4f%% (r=800m, k=20%%)\n", 
            main_welfare$welfare_change_pct))

cat("\nDone!\n")
