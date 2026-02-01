# estimate_gravity_bg_tract_times.R
# Estimate gravity equations at BLOCK GROUP level
# Using TRACT-LEVEL travel times as proxy (BG inherits its tract's travel time)
#
# This is a workaround when r5r/Java isn't available locally.
# Once you compute BG-level travel times on a server, use estimate_gravity_bg.R
#
# TWO SPECIFICATIONS:
# 1. Poisson PML (Primary - handles zeros, consistent)
# 2. OLS on positive flows (Comparison)
#
# CRITICAL: NO log(x+1) transformations! (Santos Silva & Tenreyro 2006)

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Estimating Gravity at Block Group Level")
message("Using TRACT-level travel times as proxy")
message("=============================================================")
message("NOTE: BG GEOID first 11 digits = Tract GEOID")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

message("\n[1/5] Loading data...")

# Block group LODES data
lodes_od <- fread("../input/lodes_od_bg_2019.csv",
                  colClasses = c(h_bg = "character", w_bg = "character"))
message(sprintf("LODES OD flows: %s rows", format(nrow(lodes_od), big.mark = ",")))

# TRACT-level travel time matrix (baseline)
travel_times_tract <- fread("../input/travel_time_matrix_baseline.csv",
                            colClasses = c(origin_tract = "character", dest_tract = "character"))
message(sprintf("Tract travel time matrix: %s rows", format(nrow(travel_times_tract), big.mark = ",")))

# =============================================================================
# 2. MAP BLOCK GROUPS TO TRACTS
# =============================================================================

message("\n[2/5] Mapping block groups to tracts...")

# Extract tract GEOID from block group GEOID (first 11 of 12 digits)
# BG GEOID: STATE(2) + COUNTY(3) + TRACT(6) + BG(1) = 12 digits
# Tract GEOID: STATE(2) + COUNTY(3) + TRACT(6) = 11 digits

lodes_od[, origin_tract := substr(h_bg, 1, 11)]
lodes_od[, dest_tract := substr(w_bg, 1, 11)]

# Check mapping
n_origin_tracts <- uniqueN(lodes_od$origin_tract)
n_dest_tracts <- uniqueN(lodes_od$dest_tract)
message(sprintf("Unique origin tracts: %d", n_origin_tracts))
message(sprintf("Unique destination tracts: %d", n_dest_tracts))

# Rename for merging
setnames(lodes_od, c("h_bg", "w_bg"), c("origin_bg", "dest_bg"))
setnames(lodes_od, "S000", "commuters")

# =============================================================================
# 3. MERGE WITH TRACT TRAVEL TIMES
# =============================================================================

message("\n[3/5] Merging with tract travel times...")

# Merge LODES BG data with tract-level travel times
gravity_data <- merge(
  lodes_od[, .(origin_bg, dest_bg, origin_tract, dest_tract, commuters)],
  travel_times_tract[, .(origin_tract, dest_tract, travel_time_min)],
  by = c("origin_tract", "dest_tract"),
  all.x = TRUE
)

message(sprintf("After merging: %s rows", format(nrow(gravity_data), big.mark = ",")))
message(sprintf("Rows with travel time: %s (%.1f%%)",
                format(sum(!is.na(gravity_data$travel_time_min)), big.mark = ","),
                100 * mean(!is.na(gravity_data$travel_time_min))))

# Filter to rows with valid travel time
gravity_data <- gravity_data[!is.na(travel_time_min)]
message(sprintf("After filtering missing travel times: %s rows",
                format(nrow(gravity_data), big.mark = ",")))

# Create within-BG indicator
gravity_data[, within_bg := (origin_bg == dest_bg)]
gravity_data[, within_tract := (origin_tract == dest_tract)]

# Log travel time for gravity specification
gravity_data[, log_travel_time := ifelse(travel_time_min > 0, log(travel_time_min), NA_real_)]

# Log commuters ONLY for positive flows (for OLS)
# NEVER do log(commuters + 1)!
gravity_data[, log_commuters := ifelse(commuters > 0, log(commuters), NA_real_)]

# Summary
message(sprintf("\nPositive flows: %s (%.1f%%)",
                format(sum(gravity_data$commuters > 0), big.mark = ","),
                100 * mean(gravity_data$commuters > 0)))
message(sprintf("Zero flows: %s (%.1f%%)",
                format(sum(gravity_data$commuters == 0), big.mark = ","),
                100 * mean(gravity_data$commuters == 0)))
message(sprintf("Total commuters: %s", format(sum(gravity_data$commuters), big.mark = ",")))

# Convert to factors for fixest
gravity_data[, origin_bg := as.factor(origin_bg)]
gravity_data[, dest_bg := as.factor(dest_bg)]

# =============================================================================
# 4. POISSON PML ESTIMATION (PRIMARY)
# =============================================================================

message("\n=============================================================")
message("[4/5] POISSON PML ESTIMATION (Primary Specification)")
message("=============================================================")
message("This is the preferred method - handles zeros, consistent with theory")

tic("PPML estimation")

# Between-tract flows only (since we're using tract travel times,
# within-tract travel times are 0 which makes no sense for gravity)
df_between <- gravity_data[within_tract == FALSE & !is.na(travel_time_min) & travel_time_min > 0]
message(sprintf("Between-tract pairs for PPML: %s", format(nrow(df_between), big.mark = ",")))
message(sprintf("(Note: using tract-level travel times, so within-tract = travel_time 0)"))

# Primary specification: Poisson PML with two-way FE
message("\nEstimating PPML with origin + destination FE...")
message("(This may take several minutes with many FEs)")

ppml_twoway <- tryCatch({
  fepois(
    commuters ~ travel_time_min | origin_bg + dest_bg,
    data = df_between,
    glm.iter = 100,
    verbose = 1
  )
}, error = function(e) {
  message(sprintf("PPML two-way FE failed: %s", e$message))
  message("Trying simpler specification without BG FE...")
  tryCatch({
    # Fall back to tract-level FE if BG-level fails
    fepois(
      commuters ~ travel_time_min | origin_tract + dest_tract,
      data = df_between,
      glm.iter = 100
    )
  }, error = function(e2) {
    message(sprintf("Tract FE also failed: %s", e2$message))
    NULL
  })
})

toc()

if (!is.null(ppml_twoway)) {
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
  message("PPML estimation failed")
  ppml_coef <- NA
  ppml_se <- NA
  ppml_nu <- NA
}

# =============================================================================
# 5. OLS ESTIMATION (COMPARISON)
# =============================================================================

message("\n=============================================================")
message("[5/5] OLS ESTIMATION (Comparison - positive flows only)")
message("=============================================================")

tic("OLS estimation")

# Positive flows only for OLS
df_positive <- gravity_data[commuters > 0 & within_tract == FALSE & !is.na(log_commuters)]
message(sprintf("Positive between-tract flows for OLS: %s", format(nrow(df_positive), big.mark = ",")))

# OLS with two-way FE
message("\nEstimating OLS with origin + destination FE...")

ols_twoway <- tryCatch({
  feols(
    log_commuters ~ travel_time_min | origin_bg + dest_bg,
    data = df_positive
  )
}, error = function(e) {
  message(sprintf("OLS with BG FE failed: %s", e$message))
  message("Trying tract-level FE...")
  tryCatch({
    feols(
      log_commuters ~ travel_time_min | origin_tract + dest_tract,
      data = df_positive
    )
  }, error = function(e2) {
    message(sprintf("Tract FE also failed: %s", e2$message))
    NULL
  })
})

toc()

if (!is.null(ols_twoway)) {
  ols_coef <- coef(ols_twoway)["travel_time_min"]
  ols_se <- se(ols_twoway)["travel_time_min"]
  ols_nu <- -ols_coef

  # Two-way clustered SEs
  ols_cl <- tryCatch({
    summary(ols_twoway, vcov = ~origin_bg + dest_bg)
  }, error = function(e) {
    summary(ols_twoway)  # Fall back to default SEs
  })
  ols_se_cl <- se(ols_cl)["travel_time_min"]

  message(sprintf("\n=== OLS RESULTS ==="))
  message(sprintf("Coefficient on travel_time_min: %.6f", ols_coef))
  message(sprintf("SE (default): %.6f", ols_se))
  message(sprintf("SE (clustered): %.6f", ols_se_cl))
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
# SAVE RESULTS
# =============================================================================

message("\n=============================================================")
message("Saving Results")
message("=============================================================")

# Results table
results <- data.table(
  specification = c("PPML Two-way FE", "OLS Two-way FE"),
  estimator = c("Poisson PML", "OLS"),
  sample = c("All flows (between-tract)", "Positive flows (between-tract)"),
  travel_time_source = c("Tract-level (proxy)", "Tract-level (proxy)"),
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

# Save models
models <- list(
  ppml = ppml_twoway,
  ols = ols_twoway
)
saveRDS(models, "../output/gravity_models_bg.rds")
message("Saved: ../output/gravity_models_bg.rds")

# Save gravity data
fwrite(gravity_data[, .(origin_bg, dest_bg, origin_tract, dest_tract,
                        commuters, travel_time_min, within_bg, within_tract)],
       "../output/gravity_data_bg.csv")
message("Saved: ../output/gravity_data_bg.csv")

# =============================================================================
# CREATE BINSCATTER PLOT
# =============================================================================

if (!is.null(ols_twoway)) {
  message("\nCreating binscatter visualization...")

  # Get residuals
  df_positive[, resid_log_flow := residuals(ols_twoway)]

  # Residualize travel time
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
      subtitle = sprintf("Block Group Level (tract travel times) | ν = %.4f", ols_nu),
      x = "Residualized Travel Time (minutes)",
      y = "Residualized Log(Commuters)",
      size = "N obs",
      caption = "Source: LODES 2019 (block groups), tract-level travel times\nBoth axes residualized on origin + destination FE"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

  ggsave("../output/gravity_binscatter_bg.png", p_binscatter, width = 10, height = 7, dpi = 300)
  message("Saved: ../output/gravity_binscatter_bg.png")
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

message("\n=============================================================")
message("GRAVITY ESTIMATION SUMMARY")
message("=============================================================")
message("Using tract-level travel times mapped to block groups")

message("\n--- RESULTS ---")
print(results[, .(specification, nu = round(nu, 4), n_obs,
                  pct_change_per_min = round(pct_change_per_min, 2))])

message("\n--- NOTES ---")
message("1. Travel times are at tract level, so within-tract pairs have travel_time=0")
message("2. Block groups within same tract share identical travel times")
message("3. For proper BG-level analysis, compute BG travel times with r5r on server")
message("4. PPML is preferred over OLS (handles zeros, consistent)")

message("\n=============================================================")
message("Estimation Complete!")
message("=============================================================")
