# estimate_gravity_bg.R
# Estimate gravity equations at BLOCK GROUP level
#
# THREE TRAVEL TIME SPECIFICATIONS:
# 1. Transit only
# 2. Car only (NEW)
# 3. Min(transit, car) (NEW)
#
# For each travel time, estimate:
# - Poisson PML (Primary - handles zeros, consistent)
# - OLS on positive flows (Comparison)
#
# CRITICAL: NO log(x+1) transformations! (Santos Silva & Tenreyro 2006)

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Estimating Gravity Equations at Block Group Level")
message("Transit, Car, and Min(transit, car) specifications")
message("=============================================================")
message("Santos Silva & Tenreyro (2006): Use PPML, not log(x+1)!")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

message("\n[1/6] Loading data...")

# Block group LODES data
lodes_od <- fread("../input/lodes_od_bg_2019.csv",
                  colClasses = c(h_bg = "character", w_bg = "character"))
message(sprintf("LODES OD flows: %s rows", format(nrow(lodes_od), big.mark = ",")))

# Travel time matrix with all modes (transit, car, min)
travel_times <- fread("../input/travel_time_all_modes_bg.csv",
                      colClasses = c(origin_bg = "character", dest_bg = "character"))
message(sprintf("Travel time matrix: %s rows", format(nrow(travel_times), big.mark = ",")))

# Check available columns
message(sprintf("Travel time columns: %s", paste(names(travel_times), collapse = ", ")))

# Block group centroids
centroids <- fread("../input/block_group_centroids.csv",
                   colClasses = c(id = "character"))
message(sprintf("Block groups: %d", nrow(centroids)))

# =============================================================================
# 2. BUILD GRAVITY DATASET
# =============================================================================

message("\n[2/6] Building gravity dataset...")

# Rename LODES columns for consistency
lodes_od <- lodes_od %>%
  rename(origin_bg = h_bg, dest_bg = w_bg, commuters = S000)

# Merge LODES with travel times
gravity_data <- merge(
  lodes_od[, .(origin_bg, dest_bg, commuters)],
  travel_times,
  by = c("origin_bg", "dest_bg"),
  all.x = TRUE
)

message(sprintf("After merging: %s rows", format(nrow(gravity_data), big.mark = ",")))

# Check for travel time columns
has_transit <- "travel_time_transit_baseline" %in% names(gravity_data)
has_car <- "travel_time_car" %in% names(gravity_data)
has_min <- "travel_time_min_baseline" %in% names(gravity_data)

message(sprintf("Has transit times: %s", has_transit))
message(sprintf("Has car times: %s", has_car))
message(sprintf("Has min(transit, car): %s", has_min))

# Filter to rows with valid travel time (at least one mode)
if (has_transit) {
  valid_rows <- !is.na(gravity_data$travel_time_transit_baseline)
} else {
  valid_rows <- rep(TRUE, nrow(gravity_data))
}

gravity_data <- gravity_data[valid_rows]
message(sprintf("After filtering: %s rows", format(nrow(gravity_data), big.mark = ",")))

# Create within-BG indicator
gravity_data[, within_bg := (origin_bg == dest_bg)]

# Log commuters ONLY for positive flows (for OLS)
gravity_data[, log_commuters := ifelse(commuters > 0, log(commuters), NA_real_)]

# Summary
message(sprintf("\nPositive flows: %s (%.1f%%)",
                format(sum(gravity_data$commuters > 0), big.mark = ","),
                100 * mean(gravity_data$commuters > 0)))
message(sprintf("Within-BG flows: %s",
                format(sum(gravity_data$within_bg), big.mark = ",")))

if (has_transit) {
  message(sprintf("Mean transit time: %.1f min", mean(gravity_data$travel_time_transit_baseline, na.rm = TRUE)))
}
if (has_car) {
  message(sprintf("Mean car time: %.1f min", mean(gravity_data$travel_time_car, na.rm = TRUE)))
}
if (has_min) {
  message(sprintf("Mean min(transit, car) time: %.1f min", mean(gravity_data$travel_time_min_baseline, na.rm = TRUE)))
}

# Convert to factors for fixest
gravity_data[, origin_bg := as.factor(origin_bg)]
gravity_data[, dest_bg := as.factor(dest_bg)]

# =============================================================================
# 3. ESTIMATE GRAVITY FOR EACH TRAVEL TIME MEASURE
# =============================================================================

# Function to estimate gravity with a given travel time variable
estimate_gravity <- function(data, tt_var, label) {
  message(sprintf("\n--- Estimating gravity with %s ---", label))

  # Prepare data - filter to valid travel times and between-BG
  df <- data[!is.na(get(tt_var)) & get(tt_var) > 0 & within_bg == FALSE]
  message(sprintf("Sample size: %s rows", format(nrow(df), big.mark = ",")))

  if (nrow(df) < 1000) {
    message("WARNING: Too few observations, skipping")
    return(list(ppml = NULL, ols = NULL))
  }

  # PPML
  message("Estimating PPML...")
  ppml <- tryCatch({
    fepois(
      as.formula(paste("commuters ~", tt_var, "| origin_bg + dest_bg")),
      data = df,
      glm.iter = 100,
      verbose = 0
    )
  }, error = function(e) {
    message(sprintf("PPML failed: %s", e$message))
    NULL
  })

  if (!is.null(ppml)) {
    coef_ppml <- coef(ppml)[tt_var]
    se_ppml <- se(ppml)[tt_var]
    message(sprintf("  PPML coef: %.6f (SE = %.6f), nu = %.4f", coef_ppml, se_ppml, -coef_ppml))
  }

  # OLS on positive flows
  df_pos <- df[commuters > 0 & !is.na(log_commuters)]
  message("Estimating OLS (positive flows only)...")

  ols <- tryCatch({
    feols(
      as.formula(paste("log_commuters ~", tt_var, "| origin_bg + dest_bg")),
      data = df_pos
    )
  }, error = function(e) {
    message(sprintf("OLS failed: %s", e$message))
    NULL
  })

  if (!is.null(ols)) {
    coef_ols <- coef(ols)[tt_var]
    se_ols <- se(ols)[tt_var]
    message(sprintf("  OLS coef: %.6f (SE = %.6f), nu = %.4f", coef_ols, se_ols, -coef_ols))
  }

  return(list(ppml = ppml, ols = ols, label = label, tt_var = tt_var))
}

# Store all results
all_results <- list()

message("\n=============================================================")
message("[3/6] TRANSIT Travel Time Estimation")
message("=============================================================")

if (has_transit) {
  all_results$transit <- estimate_gravity(gravity_data, "travel_time_transit_baseline", "Transit")
} else {
  message("Transit travel times not available")
}

message("\n=============================================================")
message("[4/6] CAR Travel Time Estimation")
message("=============================================================")

if (has_car) {
  all_results$car <- estimate_gravity(gravity_data, "travel_time_car", "Car")
} else {
  message("Car travel times not available")
}

message("\n=============================================================")
message("[5/6] MIN(Transit, Car) Travel Time Estimation")
message("=============================================================")

if (has_min) {
  all_results$min <- estimate_gravity(gravity_data, "travel_time_min_baseline", "Min(transit, car)")
} else {
  message("Min travel times not available")
}

# =============================================================================
# 4. COMPILE RESULTS TABLE
# =============================================================================

message("\n=============================================================")
message("[6/6] Compiling and Saving Results")
message("=============================================================")

# Build results table
results_list <- list()

for (mode in names(all_results)) {
  res <- all_results[[mode]]
  if (is.null(res)) next

  # PPML row
  if (!is.null(res$ppml)) {
    results_list[[paste0(mode, "_ppml")]] <- data.table(
      mode = mode,
      specification = paste("PPML", res$label),
      estimator = "Poisson PML",
      travel_time_var = res$tt_var,
      coef = coef(res$ppml)[res$tt_var],
      se = se(res$ppml)[res$tt_var],
      nu = -coef(res$ppml)[res$tt_var],
      n_obs = res$ppml$nobs,
      pct_change_per_min = 100 * (1 - exp(coef(res$ppml)[res$tt_var]))
    )
  }

  # OLS row
  if (!is.null(res$ols)) {
    results_list[[paste0(mode, "_ols")]] <- data.table(
      mode = mode,
      specification = paste("OLS", res$label),
      estimator = "OLS",
      travel_time_var = res$tt_var,
      coef = coef(res$ols)[res$tt_var],
      se = se(res$ols)[res$tt_var],
      nu = -coef(res$ols)[res$tt_var],
      n_obs = res$ols$nobs,
      pct_change_per_min = 100 * (1 - exp(coef(res$ols)[res$tt_var]))
    )
  }
}

results <- rbindlist(results_list)

# Save results
fwrite(results, "../output/gravity_estimates_bg.csv")
message("Saved: ../output/gravity_estimates_bg.csv")

# Save models
models <- all_results
saveRDS(models, "../output/gravity_models_bg.rds")
message("Saved: ../output/gravity_models_bg.rds")

# =============================================================================
# 5. CREATE COMPARISON PLOTS
# =============================================================================

message("\nCreating comparison plots...")

# Comparison bar chart of nu values
if (nrow(results) > 0) {
  p_compare <- ggplot(results, aes(x = specification, y = nu, fill = mode)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0.035, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = 0.5, y = 0.037, label = "Berlin benchmark (0.035/min)",
             hjust = 0, color = "red", size = 3) +
    labs(
      title = "Distance Semi-Elasticity (nu) by Travel Time Measure",
      subtitle = "Block Group Level Gravity Estimation",
      x = NULL,
      y = "nu (distance semi-elasticity)",
      fill = "Travel Time Mode"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()

  ggsave("../output/gravity_comparison_bg.pdf", p_compare, width = 10, height = 6)
  message("Saved: ../output/gravity_comparison_bg.pdf")
}

# Binscatter for car (if available)
if (!is.null(all_results$car$ols)) {
  df_car <- gravity_data[!is.na(travel_time_car) & travel_time_car > 0 &
                          within_bg == FALSE & commuters > 0]

  # Get residuals
  df_car[, resid_log_flow := residuals(all_results$car$ols)]
  tt_resid <- feols(travel_time_car ~ 1 | origin_bg + dest_bg, data = df_car)
  df_car[, resid_tt := residuals(tt_resid)]

  # Binned means
  df_car[, bin := cut(resid_tt, breaks = 20, labels = FALSE)]
  bin_means <- df_car[!is.na(bin), .(
    mean_tt = mean(resid_tt, na.rm = TRUE),
    mean_flow = mean(resid_log_flow, na.rm = TRUE),
    n = .N
  ), by = bin]

  nu_car <- -coef(all_results$car$ols)["travel_time_car"]

  p_car <- ggplot(bin_means, aes(x = mean_tt, y = mean_flow)) +
    geom_point(aes(size = n), alpha = 0.7, color = "forestgreen") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
    labs(
      title = "Gravity Relationship: CAR Travel Time",
      subtitle = sprintf("Block Group Level | nu = %.4f | Residualized binscatter", nu_car),
      x = "Residualized Car Travel Time (minutes)",
      y = "Residualized Log(Commuters)",
      size = "N obs"
    ) +
    theme_minimal(base_size = 12)

  ggsave("../output/gravity_binscatter_car_bg.pdf", p_car, width = 10, height = 7)
  message("Saved: ../output/gravity_binscatter_car_bg.pdf")
}

# Binscatter for transit (if available)
if (!is.null(all_results$transit$ols)) {
  df_transit <- gravity_data[!is.na(travel_time_transit_baseline) & travel_time_transit_baseline > 0 &
                              within_bg == FALSE & commuters > 0]

  # Get residuals
  df_transit[, resid_log_flow := residuals(all_results$transit$ols)]
  tt_resid_transit <- feols(travel_time_transit_baseline ~ 1 | origin_bg + dest_bg, data = df_transit)
  df_transit[, resid_tt := residuals(tt_resid_transit)]

  # Binned means
  df_transit[, bin := cut(resid_tt, breaks = 20, labels = FALSE)]
  bin_means_transit <- df_transit[!is.na(bin), .(
    mean_tt = mean(resid_tt, na.rm = TRUE),
    mean_flow = mean(resid_log_flow, na.rm = TRUE),
    n = .N
  ), by = bin]

  nu_transit <- -coef(all_results$transit$ols)["travel_time_transit_baseline"]

  p_transit <- ggplot(bin_means_transit, aes(x = mean_tt, y = mean_flow)) +
    geom_point(aes(size = n), alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
    labs(
      title = "Gravity Relationship: TRANSIT Travel Time",
      subtitle = sprintf("Block Group Level | nu = %.4f | Residualized binscatter", nu_transit),
      x = "Residualized Transit Travel Time (minutes)",
      y = "Residualized Log(Commuters)",
      size = "N obs"
    ) +
    theme_minimal(base_size = 12)

  ggsave("../output/gravity_binscatter_transit_bg.pdf", p_transit, width = 10, height = 7)
  message("Saved: ../output/gravity_binscatter_transit_bg.pdf")
}

# Keep backwards compatible binscatter
if (!is.null(all_results$transit$ols)) {
  file.copy("../output/gravity_binscatter_transit_bg.pdf", "../output/gravity_binscatter_bg.pdf", overwrite = TRUE)
  message("Copied: ../output/gravity_binscatter_bg.pdf (transit)")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

message("\n=============================================================")
message("GRAVITY ESTIMATION SUMMARY")
message("=============================================================")

message("\n--- RESULTS BY TRAVEL TIME MEASURE ---")
print(results[, .(specification, nu = round(nu, 4), n_obs, pct_change_per_min = round(pct_change_per_min, 2))])

message("\n--- KEY COMPARISON ---")
if (!is.null(all_results$transit$ols) && !is.null(all_results$car$ols)) {
  nu_transit <- -coef(all_results$transit$ols)["travel_time_transit_baseline"]
  nu_car <- -coef(all_results$car$ols)["travel_time_car"]
  message(sprintf("Transit nu: %.4f", nu_transit))
  message(sprintf("Car nu: %.4f", nu_car))
  message(sprintf("Ratio (car/transit): %.2f", nu_car / nu_transit))
}

message("\n--- BENCHMARK ---")
message("Berlin paper: nu ~ 0.035 per minute (0.07 per km at 30 km/h)")
message("If car nu is close to 0.03-0.05, the weak transit signal is due to mode choice mismatch")

message("\n=============================================================")
message("Gravity Estimation Complete!")
message("=============================================================")
