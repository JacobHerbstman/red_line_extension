# estimate_gravity_travel_time.R
# Gravity equation estimation using r5r transit travel times
#
# Key specifications:
# 1. PPML Two-way FE (primary - handles zeros, consistent under heteroskedasticity)
# 2. OLS Two-way FE (positive flows only)
# 3. OLS on flows >= 10 (Berlin/Ahlfeldt approach)
# 4. Log-log elasticity specification
#
# CRITICAL: No log(x+1) transformations! (Santos Silva & Tenreyro 2006)

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Estimating Gravity Equations (Travel Time)")
message("=============================================================")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

message("\n[1/6] Loading data...")

commuting <- fread("../output/commuting_matrix_travel_time.csv",
                   colClasses = c(origin_tract = "character", dest_tract = "character"))

message(sprintf("Commuting matrix: %s rows", format(nrow(commuting), big.mark = ",")))
message(sprintf("Positive flows: %s", format(sum(commuting$flow > 0), big.mark = ",")))

# =============================================================================
# 2. FILTER PROBLEMATIC TRACTS
# =============================================================================

message("\n[2/6] Filtering problematic tracts...")

# Load floor space data to filter out problematic tracts
# (airports, pure commercial areas that break the residential choice model)
floorspace <- fread("../input/tract_floorspace.csv",
                    colClasses = c(census_tract_geoid = "character"))

# Same filtering as invert_model.jl for consistency
MIN_RESIDENTIAL_SQFT <- 100000

excluded_tracts <- floorspace[is.na(total_sqft_residential) |
                               total_sqft_residential < MIN_RESIDENTIAL_SQFT,
                              census_tract_geoid]

message(sprintf("Excluding %d tracts with < %s sqft residential:",
                length(excluded_tracts), format(MIN_RESIDENTIAL_SQFT, big.mark = ",")))
show_tracts <- head(excluded_tracts, 10)
for (tract in show_tracts) {
  sqft <- floorspace[census_tract_geoid == tract, total_sqft_residential]
  message(sprintf("  - %s (%s sqft)", tract,
                  ifelse(is.na(sqft) | sqft == 0, "0", format(sqft, big.mark = ","))))
}
if (length(excluded_tracts) > length(show_tracts)) {
  message(sprintf("  ... and %d more", length(excluded_tracts) - length(show_tracts)))
}

# Filter commuting matrix
n_before <- nrow(commuting)
commuting <- commuting[!(origin_tract %in% excluded_tracts) &
                       !(dest_tract %in% excluded_tracts)]
n_after <- nrow(commuting)

message(sprintf("\nFiltered commuting matrix: %s -> %s rows (%.1f%% reduction)",
                format(n_before, big.mark = ","),
                format(n_after, big.mark = ","),
                100 * (1 - n_after/n_before)))
message(sprintf("Positive flows after filtering: %s",
                format(sum(commuting$flow > 0), big.mark = ",")))

# =============================================================================
# 3. PREPARE DATA FOR ESTIMATION
# =============================================================================

message("\n[3/6] Preparing estimation samples...")

# Convert to factors for fixest
commuting[, origin_tract := as.factor(origin_tract)]
commuting[, dest_tract := as.factor(dest_tract)]

# Subsets for estimation
df_positive <- commuting[flow > 0]                        # For OLS (log specification)
df_between <- commuting[within_tract == FALSE & flow > 0] # Between-tract positive
df_full_between <- commuting[within_tract == FALSE]       # For PPML (excludes within-tract)
df_large <- commuting[flow >= 10]                         # Berlin approach

message(sprintf("Positive flow obs (OLS sample): %s", format(nrow(df_positive), big.mark = ",")))
message(sprintf("Between-tract positive flows: %s", format(nrow(df_between), big.mark = ",")))
message(sprintf("Full between-tract (PPML): %s", format(nrow(df_full_between), big.mark = ",")))
message(sprintf("Flows >= 10 (Berlin): %s", format(nrow(df_large), big.mark = ",")))

# =============================================================================
# 4. ESTIMATE GRAVITY EQUATIONS
# =============================================================================

message("\n[4/6] Estimating gravity equations...")

results_list <- list()

# ---------------------------------------------------------------------------
# Spec 1: PPML Two-way FE (PRIMARY)
# ---------------------------------------------------------------------------
message("\n--- [1/4] PPML Two-way FE (primary) ---")
tic("PPML")
ppml_between <- tryCatch({
  fepois(flow ~ travel_time_min | origin_tract + dest_tract,
         data = df_full_between, glm.iter = 100, verbose = 0)
}, error = function(e) {
  message(sprintf("PPML failed: %s", e$message))
  NULL
})
toc()

if (!is.null(ppml_between)) {
  nu_ppml <- -coef(ppml_between)["travel_time_min"]
  se_ppml <- se(ppml_between)["travel_time_min"]
  message(sprintf("PPML: coef = %.6f (SE = %.6f)", coef(ppml_between)["travel_time_min"], se_ppml))
  message(sprintf("  => nu = %.5f", nu_ppml))
  message(sprintf("  => %.2f%% fewer commuters per minute", 100 * (1 - exp(-nu_ppml))))

  if (nu_ppml <= 0) {
    stop("Estimated nu is non-positive. Check travel_time_min and flow data.")
  }

  results_list[["PPML Two-way FE"]] <- data.table(
    specification = "PPML Two-way FE",
    estimator = "Poisson PML",
    sample = "All flows (between-tract)",
    coef_travel_time = coef(ppml_between)["travel_time_min"],
    se = se_ppml,
    se_clustered = NA_real_,
    nu = nu_ppml,
    n_obs = ppml_between$nobs,
    pct_change_per_min = 100 * (1 - exp(-nu_ppml))
  )
}

# ---------------------------------------------------------------------------
# Spec 2: OLS Two-way FE (positive flows)
# ---------------------------------------------------------------------------
message("\n--- [2/4] OLS Two-way FE (positive flows) ---")
tic("OLS")
ols_twoway <- feols(ln_flow ~ travel_time_min | origin_tract + dest_tract,
                    data = df_positive)
toc()

nu_ols <- -coef(ols_twoway)["travel_time_min"]
se_ols <- se(ols_twoway)["travel_time_min"]
message(sprintf("OLS: coef = %.6f (SE = %.6f)", coef(ols_twoway)["travel_time_min"], se_ols))
message(sprintf("  => nu = %.5f", nu_ols))
message(sprintf("  R² (within) = %.4f", r2(ols_twoway, type = "wr2")))

# Two-way clustered SEs
ols_twoway_cl <- summary(ols_twoway, vcov = ~origin_tract + dest_tract)
se_ols_cl <- se(ols_twoway_cl)["travel_time_min"]
message(sprintf("  Two-way clustered SE: %.6f", se_ols_cl))

results_list[["OLS Two-way FE"]] <- data.table(
  specification = "OLS Two-way FE",
  estimator = "OLS",
  sample = "Positive flows",
  coef_travel_time = coef(ols_twoway)["travel_time_min"],
  se = se_ols,
  se_clustered = se_ols_cl,
  nu = nu_ols,
  n_obs = ols_twoway$nobs,
  pct_change_per_min = 100 * nu_ols
)

# ---------------------------------------------------------------------------
# Spec 3: OLS on flows >= 10 (Berlin/Ahlfeldt approach)
# ---------------------------------------------------------------------------
message("\n--- [3/4] OLS (flows >= 10, Berlin approach) ---")
ols_large <- feols(ln_flow ~ travel_time_min | origin_tract + dest_tract,
                   data = df_large)

nu_large <- -coef(ols_large)["travel_time_min"]
se_large <- se(ols_large)["travel_time_min"]
message(sprintf("OLS (L>=10): coef = %.6f (SE = %.6f)", coef(ols_large)["travel_time_min"], se_large))
message(sprintf("  => nu = %.5f", nu_large))

results_list[["OLS (L>=10)"]] <- data.table(
  specification = "OLS (L>=10)",
  estimator = "OLS",
  sample = "Flows >= 10 (Berlin)",
  coef_travel_time = coef(ols_large)["travel_time_min"],
  se = se_large,
  se_clustered = NA_real_,
  nu = nu_large,
  n_obs = ols_large$nobs,
  pct_change_per_min = 100 * nu_large
)

# ---------------------------------------------------------------------------
# Spec 4: Log-log elasticity
# ---------------------------------------------------------------------------
message("\n--- [4/4] Log-log elasticity ---")
df_loglog <- df_between[!is.na(ln_travel_time) & is.finite(ln_travel_time)]
ols_loglog <- feols(ln_flow ~ ln_travel_time | origin_tract + dest_tract,
                    data = df_loglog)

elast <- -coef(ols_loglog)["ln_travel_time"]
se_elast <- se(ols_loglog)["ln_travel_time"]
message(sprintf("Log-log: coef = %.6f (SE = %.6f)", coef(ols_loglog)["ln_travel_time"], se_elast))
message(sprintf("  => elasticity = %.3f", elast))

results_list[["Log-log elasticity"]] <- data.table(
  specification = "Log-log elasticity",
  estimator = "OLS",
  sample = "Between-tract positive",
  coef_travel_time = coef(ols_loglog)["ln_travel_time"],
  se = se_elast,
  se_clustered = NA_real_,
  nu = elast,  # This is an elasticity, not semi-elasticity
  n_obs = ols_loglog$nobs,
  pct_change_per_min = NA_real_  # Not directly interpretable
)

# Combine results
results <- rbindlist(results_list)

# =============================================================================
# 5. SAVE OUTPUTS
# =============================================================================

message("\n[5/6] Saving outputs...")

fwrite(results, "../output/gravity_estimates_travel_time.csv")
message("Saved: ../output/gravity_estimates_travel_time.csv")

# Save model objects
models <- list(
  ppml = ppml_between,
  ols = ols_twoway,
  ols_large = ols_large,
  ols_loglog = ols_loglog
)
saveRDS(models, "../output/gravity_models_travel_time.rds")
message("Saved: ../output/gravity_models_travel_time.rds")

# Save nu for downstream inversion
if (!is.null(ppml_between)) {
  nu_out <- data.table(parameter = "nu_time_per_min", value = nu_ppml)
  fwrite(nu_out, "../output/nu_time_per_min.csv")
  message("Saved: ../output/nu_time_per_min.csv")

  # Save destination fixed effects for wage initialization
  fe <- fixef(ppml_between)
  if (!is.null(fe$dest_tract)) {
    dest_fe <- data.table(dest_tract = names(fe$dest_tract),
                          dest_fe = as.numeric(fe$dest_tract))
    fwrite(dest_fe, "../output/gravity_destination_fe.csv")
    message("Saved: ../output/gravity_destination_fe.csv")
  }
}

# Save merged data for downstream use
fwrite(commuting, "../output/gravity_data_travel_time.csv")
message("Saved: ../output/gravity_data_travel_time.csv")

# =============================================================================
# 6. DIAGNOSTIC PLOTS
# =============================================================================

message("\n[6/6] Creating diagnostic plots...")

pdf("../output/diagnostic_plots_travel_time.pdf", width = 10, height = 8)

# ---------------------------------------------------------------------------
# Plot 1: Binscatter (residualized)
# ---------------------------------------------------------------------------
message("Creating binscatter...")

# Residualize both variables on origin + destination FE
resid_y <- feols(ln_flow ~ 1 | origin_tract + dest_tract, data = df_between)
resid_x <- feols(travel_time_min ~ 1 | origin_tract + dest_tract, data = df_between)

plot_data <- data.table(
  resid_y = residuals(resid_y),
  resid_x = residuals(resid_x)
)

# Create bins
plot_data[, bin := cut(resid_x, breaks = 20, labels = FALSE)]
binned <- plot_data[!is.na(bin), .(
  mean_x = mean(resid_x),
  mean_y = mean(resid_y),
  se_y = sd(resid_y) / sqrt(.N),
  n = .N
), by = bin]

p1 <- ggplot(binned, aes(x = mean_x, y = mean_y)) +
  geom_errorbar(aes(ymin = mean_y - 1.96*se_y, ymax = mean_y + 1.96*se_y),
                width = 0.5, alpha = 0.3) +
  geom_point(aes(size = n), color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  labs(
    title = sprintf("Gravity: Log Commuters vs Travel Time (nu = %.4f)", nu_ols),
    subtitle = "Residualized binscatter, between-tract positive flows, 20 bins",
    x = "Travel Time (residualized, minutes)",
    y = "Log Commuters (residualized)",
    caption = "Both variables residualized on origin + destination FE"
  ) +
  theme_minimal(base_size = 12)
print(p1)

# Save the key gravity figure as a standalone file for paper inclusion
ggsave("../output/gravity_binscatter_residualized.pdf", p1, width = 10, height = 7)
message("Saved: ../output/gravity_binscatter_residualized.pdf")

# ---------------------------------------------------------------------------
# Plot 2: Coefficient comparison
# ---------------------------------------------------------------------------
message("Creating coefficient comparison...")

coef_plot <- results[specification != "Log-log elasticity"]

p2 <- ggplot(coef_plot, aes(x = nu, y = reorder(specification, nu))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = nu - 1.96 * se, xmax = nu + 1.96 * se),
                 height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0.035, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.038, y = 0.5, label = "Berlin\n(0.035)", color = "red",
           hjust = 0, size = 3) +
  labs(
    title = "Travel Time Semi-Elasticity Estimates",
    subtitle = "Comparison across specifications (red dashed = Berlin benchmark)",
    x = expression(nu ~ " (per minute)"),
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10))
print(p2)

# ---------------------------------------------------------------------------
# Plot 3: Flow distribution
# ---------------------------------------------------------------------------
message("Creating flow distribution...")

p3 <- ggplot(df_positive, aes(x = flow)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of Commuting Flows (positive only)",
    x = "Flow (log scale)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)
print(p3)

# ---------------------------------------------------------------------------
# Plot 4: Travel time distribution
# ---------------------------------------------------------------------------
message("Creating travel time distribution...")

p4 <- ggplot(df_between, aes(x = travel_time_min)) +
  geom_histogram(bins = 80, fill = "coral", alpha = 0.7) +
  labs(
    title = "Travel Time Distribution (between-tract positive flows)",
    x = "Transit Travel Time (minutes)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)
print(p4)

# ---------------------------------------------------------------------------
# Plot 5: Flow-weighted travel time
# ---------------------------------------------------------------------------
p5 <- ggplot(df_between, aes(x = travel_time_min, weight = flow)) +
  geom_histogram(bins = 80, fill = "darkgreen", alpha = 0.7) +
  labs(
    title = "Flow-Weighted Travel Time Distribution",
    x = "Transit Travel Time (minutes)",
    y = "Weighted Count (by flow)"
  ) +
  theme_minimal(base_size = 12)
print(p5)

# ---------------------------------------------------------------------------
# Plot 6: Residuals vs fitted
# ---------------------------------------------------------------------------
message("Creating residual plots...")

resid_data <- data.table(
  travel_time = df_positive$travel_time_min,
  residual = residuals(ols_twoway),
  fitted = fitted(ols_twoway),
  flow = df_positive$flow,
  within_tract = df_positive$within_tract
)

p6 <- ggplot(resid_data, aes(x = fitted, y = residual)) +
  geom_bin2d(bins = 50) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "OLS Two-way FE: Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 12)
print(p6)

# ---------------------------------------------------------------------------
# Plot 7: Binned means without residualization (raw relationship)
# ---------------------------------------------------------------------------
message("Creating raw binned scatter...")

df_between[, tt_bin := cut(travel_time_min, breaks = seq(0, ceiling(max(travel_time_min)), by = 5))]
bin_means_raw <- df_between[!is.na(tt_bin), .(
  mean_ln_flow = mean(ln_flow),
  mean_tt = mean(travel_time_min),
  n = .N
), by = tt_bin]

p7 <- ggplot(bin_means_raw, aes(x = mean_tt, y = mean_ln_flow)) +
  geom_point(aes(size = n), alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  labs(
    title = "Raw Relationship: Mean Log Flow vs Travel Time (5-min bins)",
    subtitle = "Without fixed effects - shows unconditional relationship",
    x = "Travel Time (minutes)",
    y = "Mean log(flow)",
    caption = "Between-tract positive flows only"
  ) +
  theme_minimal(base_size = 12)
print(p7)

dev.off()
message("Saved: ../output/diagnostic_plots_travel_time.pdf")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

message("\n=============================================================")
message("GRAVITY ESTIMATION SUMMARY")
message("=============================================================")

message("\n--- RESULTS ---")
print(results[, .(specification, nu = round(nu, 5), se = round(se, 6), n_obs)])

if (!is.null(ppml_between)) {
  message(sprintf("\n--- MAIN ESTIMATE (PPML) ---"))
  message(sprintf("nu = %.5f (SE = %.5f)", nu_ppml, se_ppml))
  message(sprintf("Interpretation: %.2f%% fewer commuters per minute of travel time",
                  100 * (1 - exp(-nu_ppml))))
}

message(sprintf("\n--- OLS ESTIMATE ---"))
message(sprintf("nu = %.5f (SE = %.5f, two-way clustered)", nu_ols, se_ols_cl))

message(sprintf("\n--- BENCHMARK COMPARISON ---"))
message(sprintf("Berlin (Ahlfeldt et al.): nu ~ 0.035 per minute"))
message(sprintf("Your PPML estimate: %.5f", nu_ppml))
message(sprintf("Ratio (yours / Berlin): %.2f", nu_ppml / 0.035))

if (nu_ppml > 0.02 && nu_ppml < 0.10) {
  message("\n SUCCESS: Estimate is in plausible range!")
} else if (nu_ppml < 0.01) {
  message("\n WARNING: Estimate very small - check GEOID merge or data quality")
} else if (nu_ppml > 0.10) {
  message("\n NOTE: Estimate larger than Berlin - may reflect transit-specific elasticity")
}

message("\n=============================================================")
message("Estimation complete!")
message("=============================================================")
