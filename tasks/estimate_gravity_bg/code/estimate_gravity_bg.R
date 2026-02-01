# estimate_gravity_bg.R
# Estimate gravity equations at BLOCK GROUP level
#
# TWO SPECIFICATIONS ONLY (as requested):
# 1. Poisson PML (Primary - handles zeros, consistent)
# 2. OLS on positive flows (Comparison)
#
# CRITICAL: NO log(x+1) transformations! (Santos Silva & Tenreyro 2006)

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Estimating Gravity Equations at Block Group Level")
message("=============================================================")
message("Santos Silva & Tenreyro (2006): Use PPML, not log(x+1)!")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

message("\n[1/5] Loading data...")

# Block group LODES data
lodes_od <- fread("../input/lodes_od_bg_2019.csv",
                  colClasses = c(h_bg = "character", w_bg = "character"))
message(sprintf("LODES OD flows: %s rows", format(nrow(lodes_od), big.mark = ",")))

# Travel time matrix (baseline)
travel_times <- fread("../input/travel_time_matrix_baseline_bg.csv",
                      colClasses = c(origin_bg = "character", dest_bg = "character"))
message(sprintf("Travel time matrix: %s rows", format(nrow(travel_times), big.mark = ",")))

# Block group centroids (for distance calculation)
centroids <- fread("../input/block_group_centroids.csv",
                   colClasses = c(id = "character"))
message(sprintf("Block groups: %d", nrow(centroids)))

# =============================================================================
# 2. BUILD GRAVITY DATASET
# =============================================================================

message("\n[2/5] Building gravity dataset...")

# Rename LODES columns for consistency
lodes_od <- lodes_od %>%
  rename(origin_bg = h_bg, dest_bg = w_bg, commuters = S000)

# Merge LODES with travel times
gravity_data <- merge(
  lodes_od[, .(origin_bg, dest_bg, commuters)],
  travel_times[, .(origin_bg, dest_bg, travel_time_min)],
  by = c("origin_bg", "dest_bg"),
  all.x = TRUE
)

message(sprintf("After merging: %s rows", format(nrow(gravity_data), big.mark = ",")))
message(sprintf("Rows with travel time: %s",
                format(sum(!is.na(gravity_data$travel_time_min)), big.mark = ",")))

# Filter to rows with valid travel time
gravity_data <- gravity_data[!is.na(travel_time_min)]
message(sprintf("After filtering missing travel times: %s rows",
                format(nrow(gravity_data), big.mark = ",")))

# Create within-BG indicator
gravity_data[, within_bg := (origin_bg == dest_bg)]

# Log travel time for gravity specification (for positive times)
gravity_data[, log_travel_time := ifelse(travel_time_min > 0, log(travel_time_min), NA_real_)]

# Log commuters ONLY for positive flows (for OLS)
# NEVER do log(commuters + 1) - that's econometrically invalid!
gravity_data[, log_commuters := ifelse(commuters > 0, log(commuters), NA_real_)]

# Summary
message(sprintf("\nPositive flows: %s (%.1f%%)",
                format(sum(gravity_data$commuters > 0), big.mark = ","),
                100 * mean(gravity_data$commuters > 0)))
message(sprintf("Zero flows: %s (%.1f%%)",
                format(sum(gravity_data$commuters == 0), big.mark = ","),
                100 * mean(gravity_data$commuters == 0)))
message(sprintf("Within-BG flows: %s",
                format(sum(gravity_data$within_bg), big.mark = ",")))

# Convert to factors for fixest
gravity_data[, origin_bg := as.factor(origin_bg)]
gravity_data[, dest_bg := as.factor(dest_bg)]

# =============================================================================
# 3. POISSON PML ESTIMATION (PRIMARY)
# =============================================================================

message("\n=============================================================")
message("[3/5] POISSON PML ESTIMATION (Primary Specification)")
message("=============================================================")
message("This is the preferred method - handles zeros, consistent with theory")

tic("PPML estimation")

# Between-BG flows only (exclude within-BG where travel_time = 0)
df_between <- gravity_data[within_bg == FALSE & !is.na(travel_time_min)]
message(sprintf("Between-BG pairs for PPML: %s", format(nrow(df_between), big.mark = ",")))

# Primary specification: Poisson PML with two-way FE
message("\nEstimating PPML with origin + destination FE...")
message("(This may take several minutes with large number of FEs)")

ppml_twoway <- tryCatch({
  fepois(
    commuters ~ travel_time_min | origin_bg + dest_bg,
    data = df_between,
    glm.iter = 100,
    verbose = 1
  )
}, error = function(e) {
  message(sprintf("PPML two-way FE failed: %s", e$message))
  message("Trying with fewer iterations...")
  tryCatch({
    fepois(
      commuters ~ travel_time_min | origin_bg + dest_bg,
      data = df_between,
      glm.iter = 50
    )
  }, error = function(e2) {
    message(sprintf("Still failed: %s", e2$message))
    NULL
  })
})

toc()

if (!is.null(ppml_twoway)) {
  # Get coefficients
  ppml_coef <- coef(ppml_twoway)["travel_time_min"]
  ppml_se <- se(ppml_twoway)["travel_time_min"]
  ppml_nu <- -ppml_coef

  message(sprintf("\n=== PPML RESULTS ==="))
  message(sprintf("Coefficient on travel_time_min: %.6f (SE = %.6f)", ppml_coef, ppml_se))
  message(sprintf("Distance semi-elasticity (nu): %.4f", ppml_nu))
  message(sprintf("Interpretation: 1 minute increase -> %.2f%% fewer commuters",
                  100 * (1 - exp(ppml_coef))))
  message(sprintf("N observations: %s", format(ppml_twoway$nobs, big.mark = ",")))
} else {
  message("PPML estimation failed - see errors above")
  ppml_coef <- NA
  ppml_se <- NA
  ppml_nu <- NA
}

# =============================================================================
# 4. OLS ESTIMATION (COMPARISON)
# =============================================================================

message("\n=============================================================")
message("[4/5] OLS ESTIMATION (Comparison - positive flows only)")
message("=============================================================")
message("Note: OLS drops zeros, which may bias results")

tic("OLS estimation")

# Positive flows only for OLS
df_positive <- gravity_data[commuters > 0 & within_bg == FALSE & !is.na(log_commuters)]
message(sprintf("Positive between-BG flows for OLS: %s", format(nrow(df_positive), big.mark = ",")))

# OLS with two-way FE
message("\nEstimating OLS with origin + destination FE...")

ols_twoway <- tryCatch({
  feols(
    log_commuters ~ travel_time_min | origin_bg + dest_bg,
    data = df_positive
  )
}, error = function(e) {
  message(sprintf("OLS failed: %s", e$message))
  NULL
})

toc()

if (!is.null(ols_twoway)) {
  # Get coefficients with clustered SEs
  ols_coef <- coef(ols_twoway)["travel_time_min"]
  ols_se <- se(ols_twoway)["travel_time_min"]
  ols_nu <- -ols_coef

  # Two-way clustered SEs
  ols_cl <- summary(ols_twoway, vcov = ~origin_bg + dest_bg)
  ols_se_cl <- se(ols_cl)["travel_time_min"]

  message(sprintf("\n=== OLS RESULTS ==="))
  message(sprintf("Coefficient on travel_time_min: %.6f", ols_coef))
  message(sprintf("SE (default): %.6f", ols_se))
  message(sprintf("SE (two-way clustered): %.6f", ols_se_cl))
  message(sprintf("Distance semi-elasticity (nu): %.4f", ols_nu))
  message(sprintf("Interpretation: 1 minute increase -> %.2f%% fewer commuters",
                  100 * (1 - exp(ols_coef))))
  message(sprintf("N observations: %s", format(ols_twoway$nobs, big.mark = ",")))
  message(sprintf("R-squared (within): %.4f", r2(ols_twoway, "wr2")))
} else {
  message("OLS estimation failed")
  ols_coef <- NA
  ols_se <- NA
  ols_se_cl <- NA
  ols_nu <- NA
}

# =============================================================================
# 5. SAVE RESULTS AND CREATE OUTPUTS
# =============================================================================

message("\n=============================================================")
message("[5/5] Saving Results")
message("=============================================================")

# Results table
results <- data.table(
  specification = c("PPML Two-way FE", "OLS Two-way FE"),
  estimator = c("Poisson PML", "OLS"),
  sample = c("All flows (between-BG)", "Positive flows (between-BG)"),
  coef_travel_time = c(ppml_coef, ols_coef),
  se = c(ppml_se, ols_se),
  se_clustered = c(NA, ols_se_cl),
  nu = c(ppml_nu, ols_nu),
  n_obs = c(
    ifelse(!is.null(ppml_twoway), ppml_twoway$nobs, NA),
    ifelse(!is.null(ols_twoway), ols_twoway$nobs, NA)
  ),
  pct_change_per_min = c(
    ifelse(!is.na(ppml_coef), 100 * (1 - exp(ppml_coef)), NA),
    ifelse(!is.na(ols_coef), 100 * (1 - exp(ols_coef)), NA)
  )
)

fwrite(results, "../output/gravity_estimates_bg.csv")
message("Saved: ../output/gravity_estimates_bg.csv")

# Save models for later use
models <- list(
  ppml = ppml_twoway,
  ols = ols_twoway
)
saveRDS(models, "../output/gravity_models_bg.rds")
message("Saved: ../output/gravity_models_bg.rds")

# Save gravity data for diagnostics/visualization
fwrite(gravity_data[, .(origin_bg, dest_bg, commuters, travel_time_min, within_bg)],
       "../output/gravity_data_bg.csv")
message("Saved: ../output/gravity_data_bg.csv")

# =============================================================================
# 6. CREATE BINSCATTER PLOT (Berlin Figure 4A style)
# =============================================================================

message("\n=============================================================")
message("Creating Binscatter Visualization")
message("=============================================================")

if (!is.null(ols_twoway)) {
  # Residualize both log(flow) and travel_time on origin + destination FE
  # This is what the binscatter should show - partial correlation

  # Get residuals from OLS model
  df_positive[, resid_log_flow := residuals(ols_twoway)]

  # Residualize travel time on same FEs
  travel_time_resid_model <- feols(travel_time_min ~ 1 | origin_bg + dest_bg, data = df_positive)
  df_positive[, resid_travel_time := residuals(travel_time_resid_model)]

  # Create bins
  n_bins <- 20
  df_positive[, bin := cut(resid_travel_time, breaks = n_bins, labels = FALSE)]

  bin_means <- df_positive[!is.na(bin), .(
    mean_resid_travel_time = mean(resid_travel_time, na.rm = TRUE),
    mean_resid_log_flow = mean(resid_log_flow, na.rm = TRUE),
    n = .N
  ), by = bin]

  # Create plot
  p_binscatter <- ggplot(bin_means, aes(x = mean_resid_travel_time, y = mean_resid_log_flow)) +
    geom_point(aes(size = n), alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", size = 1) +
    labs(
      title = "Gravity Relationship: Commuting Flows vs Travel Time",
      subtitle = sprintf("Block Group Level | ν = %.4f | Binned scatter after partialling out FE", ols_nu),
      x = "Residualized Travel Time (minutes)",
      y = "Residualized Log(Commuters)",
      size = "N obs",
      caption = "Source: LODES 2019, r5r travel times\nNote: Both axes residualized on origin + destination FE"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right"
    )

  ggsave("../output/gravity_binscatter_bg.pdf", p_binscatter, width = 10, height = 7)
  message("Saved: ../output/gravity_binscatter_bg.pdf")

  # Also create raw scatter (not residualized) for intuition
  raw_bin_means <- df_positive[, .(
    mean_travel_time = mean(travel_time_min, na.rm = TRUE),
    mean_log_flow = mean(log_commuters, na.rm = TRUE),
    n = .N
  ), by = .(bin = cut(travel_time_min, breaks = 20, labels = FALSE))]

  p_raw <- ggplot(raw_bin_means[!is.na(bin)], aes(x = mean_travel_time, y = mean_log_flow)) +
    geom_point(aes(size = n), alpha = 0.7, color = "coral") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(
      title = "Raw Gravity Relationship (No FE)",
      subtitle = "Block Group Level | Binned means",
      x = "Mean Travel Time (minutes)",
      y = "Mean Log(Commuters)",
      size = "N obs"
    ) +
    theme_minimal(base_size = 12)

  ggsave("../output/gravity_raw_scatter_bg.pdf", p_raw, width = 10, height = 7)
  message("Saved: ../output/gravity_raw_scatter_bg.pdf")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

message("\n=============================================================")
message("GRAVITY ESTIMATION SUMMARY (Block Group Level)")
message("=============================================================")

message("\n--- RESULTS ---")
print(results[, .(specification, nu = round(nu, 4), n_obs, pct_change_per_min = round(pct_change_per_min, 2))])

message("\n--- INTERPRETATION ---")
if (!is.na(ppml_nu)) {
  message(sprintf("PPML (preferred): 1 minute longer commute -> %.2f%% fewer commuters",
                  abs(100 * (1 - exp(-ppml_nu)))))
}
if (!is.na(ols_nu)) {
  message(sprintf("OLS (comparison): 1 minute longer commute -> %.2f%% fewer commuters",
                  abs(100 * (1 - exp(-ols_nu)))))
}

message("\n--- BENCHMARK COMPARISON ---")
message("Ahlfeldt et al. (2015) Berlin: nu ~ 0.07 (per km)")
message("Note: Our estimates are per MINUTE of travel time, not per km")
message("To compare: if avg speed is 30 km/h, 1 km ≈ 2 min")
message(sprintf("Our implied nu per 2 min: %.4f", 2 * ifelse(!is.na(ppml_nu), ppml_nu, ols_nu)))

message("\n=============================================================")
message("Block Group Gravity Estimation Complete!")
message("=============================================================")
