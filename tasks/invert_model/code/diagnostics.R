# ============================================================================
# Diagnostic Plots and Summary Statistics for Model Inversion
# ============================================================================

library(tidyverse)
library(sf)
library(scales)
library(gridExtra)

# ----------------------------------------------------------------------------
# Load Data
# ----------------------------------------------------------------------------

cat("Loading data...\n")

# Model outputs
fundamentals <- read_csv("../output/model_fundamentals.csv",
                         col_types = cols(tract = col_character(),
                                          .default = col_double()))
params <- read_csv("../output/model_parameters.csv", show_col_types = FALSE)

# Census tract geometries for mapping
tracts_sf <- st_read("../input/chicago_tracts.gpkg", quiet = TRUE)

# Merge fundamentals with geometries
fundamentals_sf <- tracts_sf %>%
  left_join(fundamentals, by = c("GEOID" = "tract"))

cat("Loaded", nrow(fundamentals), "tracts\n\n")

# ----------------------------------------------------------------------------
# Print Parameters
# ----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("MODEL PARAMETERS\n")
cat(strrep("=", 60), "\n\n")

for (i in 1:nrow(params)) {
  cat(sprintf("  %s: %s\n", params$parameter[i], params$value[i]))
}
cat("\n")

# ----------------------------------------------------------------------------
# Summary Statistics
# ----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("SUMMARY STATISTICS\n")
cat(strrep("=", 60), "\n\n")

# Function to print summary stats
print_summary <- function(x, name) {
  cat(sprintf("%s:\n", name))
  cat(sprintf("  Mean:   %10.4f\n", mean(x, na.rm = TRUE)))
  cat(sprintf("  Median: %10.4f\n", median(x, na.rm = TRUE)))
  cat(sprintf("  SD:     %10.4f\n", sd(x, na.rm = TRUE)))
  cat(sprintf("  Min:    %10.4f\n", min(x, na.rm = TRUE)))
  cat(sprintf("  Max:    %10.4f\n", max(x, na.rm = TRUE)))
  cat(sprintf("  P10:    %10.4f\n", quantile(x, 0.10, na.rm = TRUE)))
  cat(sprintf("  P90:    %10.4f\n", quantile(x, 0.90, na.rm = TRUE)))
  cat("\n")
}

print_summary(fundamentals$wage, "Wages (w_i)")
print_summary(fundamentals$floor_price, "Floor Prices (Q_n)")
print_summary(fundamentals$amenity, "Amenities (B_n)")
print_summary(fundamentals$expected_income, "Expected Income (v̄_n)")
print_summary(fundamentals$commuting_access, "Commuting Market Access (W_n)")

# ----------------------------------------------------------------------------
# Model Fit
# ----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("MODEL FIT\n")
cat(strrep("=", 60), "\n\n")

# Correlation between observed and model-predicted
corr_residents <- cor(fundamentals$residents_observed, fundamentals$residents_model, use = "complete.obs")
corr_workers <- cor(fundamentals$workers_observed, fundamentals$workers_model, use = "complete.obs")

cat(sprintf("Correlation (Observed vs Model Residents): %.4f\n", corr_residents))
cat(sprintf("Correlation (Observed vs Model Workers):   %.4f\n", corr_workers))

# RMSE
rmse_residents <- sqrt(mean((fundamentals$residents_observed - fundamentals$residents_model)^2, na.rm = TRUE))
rmse_workers <- sqrt(mean((fundamentals$workers_observed - fundamentals$workers_model)^2, na.rm = TRUE))

cat(sprintf("RMSE (Residents): %.2f\n", rmse_residents))
cat(sprintf("RMSE (Workers):   %.2f\n", rmse_workers))
cat("\n")

# ----------------------------------------------------------------------------
# Correlations Between Fundamentals
# ----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("CORRELATIONS BETWEEN FUNDAMENTALS\n")
cat(strrep("=", 60), "\n\n")

vars_to_corr <- c("wage", "floor_price", "amenity", "expected_income", 
                  "commuting_access", "residents_observed", "workers_observed",
                  "floor_space_residential")

corr_matrix <- cor(fundamentals[, vars_to_corr], use = "pairwise.complete.obs")
print(round(corr_matrix, 3))
cat("\n")

# ----------------------------------------------------------------------------
# Identify Notable Tracts
# ----------------------------------------------------------------------------

cat(strrep("=", 60), "\n")
cat("NOTABLE TRACTS\n")
cat(strrep("=", 60), "\n\n")

# Highest wages (likely downtown/Loop)
cat("Top 10 Tracts by Wage:\n")
fundamentals %>%
  arrange(desc(wage)) %>%
  select(tract, wage, workers_observed, floor_price) %>%
  head(10) %>%
  print()
cat("\n")

# Highest floor prices
cat("Top 10 Tracts by Floor Price:\n")
fundamentals %>%
  arrange(desc(floor_price)) %>%
  select(tract, floor_price, residents_observed, amenity) %>%
  head(10) %>%
  print()
cat("\n")

# Highest amenities
cat("Top 10 Tracts by Amenity:\n")
fundamentals %>%
  arrange(desc(amenity)) %>%
  select(tract, amenity, floor_price, residents_observed) %>%
  head(10) %>%
  print()
cat("\n")

# ----------------------------------------------------------------------------
# Create Diagnostic Plots
# ----------------------------------------------------------------------------

cat("Creating diagnostic plots...\n")

# Theme for maps
map_theme <- theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  )

# 1. Wage Map
p_wage <- ggplot(fundamentals_sf) +
  geom_sf(aes(fill = wage), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Wage (normalized)") +
  labs(title = "Recovered Wages by Tract") +
  map_theme

# 2. Floor Price Map
p_floor <- ggplot(fundamentals_sf) +
  geom_sf(aes(fill = log(floor_price)), color = NA) +
  scale_fill_viridis_c(option = "magma", name = "Log Floor Price") +
  labs(title = "Recovered Floor Prices by Tract") +
  map_theme

# 3. Amenity Map
p_amenity <- ggplot(fundamentals_sf) +
  geom_sf(aes(fill = log(amenity)), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Log Amenity") +
  labs(title = "Recovered Amenities by Tract") +
  map_theme

# 4. Commuting Market Access Map
p_access <- ggplot(fundamentals_sf) +
  geom_sf(aes(fill = log(commuting_access)), color = NA) +
  scale_fill_viridis_c(option = "cividis", name = "Log Commuting Access") +
  labs(title = "Commuting Market Access by Tract") +
  map_theme

# Save maps
pdf("../output/diagnostic_maps.pdf", width = 12, height = 10)
grid.arrange(p_wage, p_floor, p_amenity, p_access, ncol = 2)
dev.off()
cat("  Saved: diagnostic_maps.pdf\n")

# 5. Model Fit Scatter Plots
p_fit_residents <- ggplot(fundamentals, aes(x = residents_observed, y = residents_model)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = sprintf("Model Fit: Residents (corr = %.3f)", corr_residents),
    x = "Observed Residents",
    y = "Model Residents"
  ) +
  theme_minimal()

p_fit_workers <- ggplot(fundamentals, aes(x = workers_observed, y = workers_model)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = sprintf("Model Fit: Workers (corr = %.3f)", corr_workers),
    x = "Observed Workers",
    y = "Model Workers"
  ) +
  theme_minimal()

pdf("../output/diagnostic_fit.pdf", width = 10, height = 5)
grid.arrange(p_fit_residents, p_fit_workers, ncol = 2)
dev.off()
cat("  Saved: diagnostic_fit.pdf\n")

# 6. Distribution of Fundamentals
p_dist_wage <- ggplot(fundamentals, aes(x = wage)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Wages", x = "Wage", y = "Count") +
  theme_minimal()

p_dist_floor <- ggplot(fundamentals, aes(x = log(floor_price))) +
  geom_histogram(bins = 50, fill = "darkred", color = "white") +
  labs(title = "Distribution of Log Floor Prices", x = "Log Floor Price", y = "Count") +
  theme_minimal()

p_dist_amenity <- ggplot(fundamentals, aes(x = log(amenity))) +
  geom_histogram(bins = 50, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Log Amenities", x = "Log Amenity", y = "Count") +
  theme_minimal()

p_dist_access <- ggplot(fundamentals, aes(x = log(commuting_access))) +
  geom_histogram(bins = 50, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Log Commuting Access", x = "Log Commuting Access", y = "Count") +
  theme_minimal()

pdf("../output/diagnostic_distributions.pdf", width = 10, height = 8)
grid.arrange(p_dist_wage, p_dist_floor, p_dist_amenity, p_dist_access, ncol = 2)
dev.off()
cat("  Saved: diagnostic_distributions.pdf\n")

# 7. Scatter plots of fundamentals relationships
p_wage_floor <- ggplot(fundamentals, aes(x = wage, y = log(floor_price))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Wage vs Floor Price", x = "Wage", y = "Log Floor Price") +
  theme_minimal()

p_wage_amenity <- ggplot(fundamentals, aes(x = wage, y = log(amenity))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Wage vs Amenity", x = "Wage", y = "Log Amenity") +
  theme_minimal()

p_floor_amenity <- ggplot(fundamentals, aes(x = log(floor_price), y = log(amenity))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Floor Price vs Amenity", x = "Log Floor Price", y = "Log Amenity") +
  theme_minimal()

p_access_floor <- ggplot(fundamentals, aes(x = log(commuting_access), y = log(floor_price))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Commuting Access vs Floor Price", x = "Log Commuting Access", y = "Log Floor Price") +
  theme_minimal()

pdf("../output/diagnostic_relationships.pdf", width = 10, height = 8)
grid.arrange(p_wage_floor, p_wage_amenity, p_floor_amenity, p_access_floor, ncol = 2)
dev.off()
cat("  Saved: diagnostic_relationships.pdf\n")

# 8. Residents vs Floor Space (check land market clearing makes sense)
p_land_market <- ggplot(fundamentals, aes(x = log(floor_space_residential), y = log(residents_observed))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Floor Space vs Residents",
    subtitle = sprintf("Correlation: %.3f", cor(log(fundamentals$floor_space_residential), 
                                                  log(fundamentals$residents_observed), 
                                                  use = "complete.obs")),
    x = "Log Residential Floor Space",
    y = "Log Residents"
  ) +
  theme_minimal()

pdf("../output/diagnostic_land_market.pdf", width = 6, height = 5)
print(p_land_market)
dev.off()
cat("  Saved: diagnostic_land_market.pdf\n")

# ----------------------------------------------------------------------------
# 9. Model vs Observed Residential Earnings
# ----------------------------------------------------------------------------

cat("Computing model vs observed residential earnings diagnostic...\n")

# Load LODES RAC data with earnings categories
lodes_rac <- read_csv("../input/lodes_rac.csv", show_col_types = FALSE) %>%
  mutate(h_tract = as.character(h_tract))

# Compute observed average monthly earnings using midpoints:
# CE01: $1,250/month or less -> midpoint $625
# CE02: $1,251-$3,333/month -> midpoint $2,292
# CE03: >$3,333/month -> assume midpoint $5,000
lodes_earnings <- lodes_rac %>%
  filter(h_tract %in% fundamentals$tract) %>%
  mutate(
    observed_avg_earnings = (CE01 * 625 + CE02 * 2292 + CE03 * 5000) / C000,
    high_wage_share = CE03 / C000,
    low_wage_share = CE01 / C000
  ) %>%
  select(h_tract, observed_avg_earnings, high_wage_share, low_wage_share, C000)

# Merge with model fundamentals
fundamentals_with_observed <- fundamentals %>%
  left_join(lodes_earnings, by = c("tract" = "h_tract"))

# Correlation between model expected income and observed earnings
corr_income <- cor(fundamentals_with_observed$expected_income,
                   fundamentals_with_observed$observed_avg_earnings,
                   use = "complete.obs")

# Correlation between model expected income and high-wage share
corr_highwage <- cor(fundamentals_with_observed$expected_income,
                     fundamentals_with_observed$high_wage_share,
                     use = "complete.obs")

cat(sprintf("  Correlation (Model Expected Income vs Observed Earnings): %.4f\n", corr_income))
cat(sprintf("  Correlation (Model Expected Income vs High-Wage Share): %.4f\n", corr_highwage))

# Scatter plot: Model expected income vs observed earnings
p_income_validation <- ggplot(fundamentals_with_observed,
                               aes(x = observed_avg_earnings, y = expected_income)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Model vs Observed Residential Earnings",
    subtitle = sprintf("Correlation: %.3f (Model lacks worker heterogeneity)", corr_income),
    x = "Observed Avg Monthly Earnings (LODES)",
    y = "Model Expected Income (v̄_n)"
  ) +
  theme_minimal()

# Scatter plot: Model amenity vs high-wage share (do amenities explain sorting?)
p_amenity_sorting <- ggplot(fundamentals_with_observed,
                             aes(x = high_wage_share, y = log(amenity))) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Amenities vs High-Wage Worker Share",
    subtitle = sprintf("Correlation: %.3f",
                       cor(fundamentals_with_observed$high_wage_share,
                           log(fundamentals_with_observed$amenity),
                           use = "complete.obs")),
    x = "High-Wage Worker Share (LODES CE03/C000)",
    y = "Log Amenity (B_n)"
  ) +
  theme_minimal()

pdf("../output/diagnostic_income_validation.pdf", width = 10, height = 5)
grid.arrange(p_income_validation, p_amenity_sorting, ncol = 2)
dev.off()
cat("  Saved: diagnostic_income_validation.pdf\n")

# Store for summary output
income_validation_stats <- list(
  corr_income = corr_income,
  corr_highwage = corr_highwage,
  corr_amenity_sorting = cor(fundamentals_with_observed$high_wage_share,
                              log(fundamentals_with_observed$amenity),
                              use = "complete.obs")
)

# ----------------------------------------------------------------------------
# Save Summary to Text File
# ----------------------------------------------------------------------------

sink("../output/diagnostic_summary.txt")

cat(strrep("=", 70), "\n")
cat("MODEL INVERSION DIAGNOSTIC SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Parameters:\n")
for (i in 1:nrow(params)) {
  cat(sprintf("  %s: %s\n", params$parameter[i], params$value[i]))
}
cat("\n")

cat(strrep("-", 70), "\n")
cat("FUNDAMENTALS SUMMARY\n")
cat(strrep("-", 70), "\n\n")

print_summary(fundamentals$wage, "Wages (w_i)")
print_summary(fundamentals$floor_price, "Floor Prices (Q_n)")
print_summary(fundamentals$amenity, "Amenities (B_n)")
print_summary(fundamentals$expected_income, "Expected Income (v̄_n)")
print_summary(fundamentals$commuting_access, "Commuting Market Access (W_n)")

cat(strrep("-", 70), "\n")
cat("MODEL FIT\n")
cat(strrep("-", 70), "\n\n")
cat(sprintf("Correlation (Observed vs Model Residents): %.4f\n", corr_residents))
cat(sprintf("Correlation (Observed vs Model Workers):   %.4f\n", corr_workers))
cat(sprintf("RMSE (Residents): %.2f\n", rmse_residents))
cat(sprintf("RMSE (Workers):   %.2f\n", rmse_workers))
cat("\n")

cat(strrep("-", 70), "\n")
cat("CORRELATION MATRIX\n")
cat(strrep("-", 70), "\n\n")
print(round(corr_matrix, 3))
cat("\n")

cat(strrep("-", 70), "\n")
cat("TOP 10 TRACTS BY WAGE\n")
cat(strrep("-", 70), "\n\n")
fundamentals %>%
  arrange(desc(wage)) %>%
  select(tract, wage, workers_observed, floor_price) %>%
  head(10) %>%
  print()
cat("\n")

cat(strrep("-", 70), "\n")
cat("TOP 10 TRACTS BY FLOOR PRICE\n")
cat(strrep("-", 70), "\n\n")
fundamentals %>%
  arrange(desc(floor_price)) %>%
  select(tract, floor_price, residents_observed, amenity) %>%
  head(10) %>%
  print()
cat("\n")

cat(strrep("-", 70), "\n")
cat("TOP 10 TRACTS BY AMENITY\n")
cat(strrep("-", 70), "\n\n")
fundamentals %>%
  arrange(desc(amenity)) %>%
  select(tract, amenity, floor_price, residents_observed) %>%
  head(10) %>%
  print()
cat("\n")

cat(strrep("-", 70), "\n")
cat("MODEL LIMITATION: RESIDENTIAL INCOME SORTING\n")
cat(strrep("-", 70), "\n\n")
cat("The model assumes homogeneous workers, so expected income variation is limited.\n")
cat("Observed LODES data shows much larger residential earnings differences.\n\n")
cat(sprintf("Correlation (Model Expected Income vs Observed Earnings): %.4f\n",
            income_validation_stats$corr_income))
cat(sprintf("Correlation (Model Expected Income vs High-Wage Share):   %.4f\n",
            income_validation_stats$corr_highwage))
cat(sprintf("Correlation (Log Amenity vs High-Wage Share):             %.4f\n",
            income_validation_stats$corr_amenity_sorting))
cat("\nInterpretation: Amenities capture residential sorting by income that the\n")
cat("model cannot explain through wage differences alone.\n")

sink()

cat("  Saved: diagnostic_summary.txt\n")

cat("\nDone!\n")
