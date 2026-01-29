# estimate_gravity.R
# Estimate gravity equations for commuting flows using fixest
# OLS and PPML with origin/destination fixed effects
# Recovers distance semi-elasticity (nu) for QSM counterfactual

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Estimating Gravity Equations for Commuting Flows")
message("=============================================================")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

commuting <- fread("../output/commuting_matrix.csv",
                   colClasses = c(origin_tract = "character", dest_tract = "character"))

chicago_tracts <- fread("../input/chicago_tracts_2010.csv",
                        colClasses = c(GEOID = "character"))

# Load floor space data to filter out problematic tracts
# (airports, pure commercial areas that break the residential choice model)
floorspace <- fread("../input/tract_floorspace.csv",
                    colClasses = c(census_tract_geoid = "character"))

message(sprintf("Commuting matrix: %s rows", format(nrow(commuting), big.mark = ",")))
message(sprintf("Positive flows: %s", format(sum(commuting$flow > 0), big.mark = ",")))

# =============================================================================
# 1b. FILTER OUT PROBLEMATIC TRACTS
# =============================================================================

# Same filtering as invert_model.jl for consistency:
# - Exclude tracts with zero residential floor space (airports, pure commercial)
# - Exclude tracts with < 100,000 sqft residential (mostly commercial)
MIN_RESIDENTIAL_SQFT <- 100000

excluded_tracts <- floorspace[is.na(total_sqft_residential) |
                               total_sqft_residential < MIN_RESIDENTIAL_SQFT,
                              census_tract_geoid]

message(sprintf("\nExcluding %d tracts with < %s sqft residential floor space:",
                length(excluded_tracts), format(MIN_RESIDENTIAL_SQFT, big.mark = ",")))
for (tract in excluded_tracts) {
  sqft <- floorspace[census_tract_geoid == tract, total_sqft_residential]
  message(sprintf("  - %s (%s sqft)", tract,
                  ifelse(is.na(sqft) | sqft == 0, "0", format(sqft, big.mark = ","))))
}

# Filter commuting matrix to exclude these tracts as BOTH origins and destinations
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

# Convert tract IDs to factors for fixest
commuting[, origin_tract := as.factor(origin_tract)]
commuting[, dest_tract := as.factor(dest_tract)]

# Subsets for estimation
df_positive <- commuting[flow > 0]           # For OLS (log specification)
df_between <- commuting[within_tract == FALSE & flow > 0]  # Exclude within-tract
df_full <- commuting                          # For PPML (includes zeros)
df_full_between <- commuting[within_tract == FALSE]  # PPML excluding within-tract

message(sprintf("Positive flow obs (OLS sample): %s", format(nrow(df_positive), big.mark = ",")))
message(sprintf("Between-tract positive flows: %s", format(nrow(df_between), big.mark = ",")))

# =============================================================================
# 2. OLS ESTIMATION
# =============================================================================

message("\n=============================================================")
message("OLS Gravity Estimates (ln_flow ~ distance)")
message("=============================================================")

tic("OLS estimation")

# Spec 1: No fixed effects (pooled OLS)
message("\n[1/5] OLS: No fixed effects...")
ols_basic <- feols(ln_flow ~ distance_km, data = df_positive)

# Spec 2: Origin FE only
message("[2/5] OLS: Origin FE...")
ols_origin <- feols(ln_flow ~ distance_km | origin_tract, data = df_positive)

# Spec 3: Destination FE only
message("[3/5] OLS: Destination FE...")
ols_dest <- feols(ln_flow ~ distance_km | dest_tract, data = df_positive)

# Spec 4: Two-way FE (main specification)
message("[4/5] OLS: Two-way FE (main spec)...")
ols_twoway <- feols(ln_flow ~ distance_km | origin_tract + dest_tract, data = df_positive)

# Spec 5: Log-distance specification (log-log elasticity)
message("[5/6] OLS: Two-way FE with log(distance)...")
ols_logdist <- feols(ln_flow ~ ln_distance | origin_tract + dest_tract,
                     data = df_between)  # Exclude within-tract (distance=0)

# Spec 6: Log-distance without FE (to compare pooled log-log)
message("[6/6] OLS: No FE with log(distance) (standard gravity)...")
ols_logdist_nofe <- feols(ln_flow ~ ln_distance, data = df_between)

toc()

# Print OLS results
message("\n--- OLS Results ---")
ols_models <- list(
  "No FE" = ols_basic,
  "Origin FE" = ols_origin,
  "Dest FE" = ols_dest,
  "Two-way FE" = ols_twoway,
  "Two-way FE (log dist)" = ols_logdist,
  "No FE (log dist)" = ols_logdist_nofe
)

# Extract results into table
ols_results <- data.table(
  specification = names(ols_models),
  coef_distance = sapply(ols_models, function(m) coef(m)[1]),
  se_default = sapply(ols_models, function(m) se(m)[1]),
  r2 = sapply(ols_models, function(m) r2(m, "r2")),
  r2_within = sapply(ols_models, function(m) {
    r <- r2(m, "wr2")
    if (length(r) == 0 || is.null(r)) NA_real_ else r
  }),
  n_obs = sapply(ols_models, function(m) m$nobs)
)

# Two-way clustered SEs for main specification
ols_twoway_cl <- summary(ols_twoway, vcov = ~origin_tract + dest_tract)
message(sprintf("\nTwo-way FE coefficient on distance: %.5f (SE = %.5f)",
                coef(ols_twoway)[1], se(ols_twoway_cl)[1]))
message(sprintf("  => nu = %.4f", -coef(ols_twoway)[1]))
message(sprintf("  Interpretation: 1 km increase -> %.1f%% fewer commuters",
                100 * (1 - exp(coef(ols_twoway)[1]))))

# =============================================================================
# 3. PPML ESTIMATION
# =============================================================================

message("\n=============================================================")
message("PPML Gravity Estimates (flow ~ distance)")
message("=============================================================")

tic("PPML estimation")

# PPML with two-way FE on between-tract flows
# Full matrix (633k obs) with ~1600 FEs can be slow; try it
message("\n[1/2] PPML: Two-way FE (between-tract only)...")
ppml_between <- tryCatch({
  fepois(flow ~ distance_km | origin_tract + dest_tract,
         data = df_full_between, glm.iter = 100)
}, error = function(e) {
  message(sprintf("PPML between-tract failed: %s", e$message))
  NULL
})

# PPML including within-tract flows (with distance = 0)
message("[2/4] PPML: Two-way FE (all pairs)...")
ppml_full <- tryCatch({
  fepois(flow ~ distance_km | origin_tract + dest_tract,
         data = df_full, glm.iter = 100)
}, error = function(e) {
  message(sprintf("PPML full failed: %s", e$message))
  NULL
})

# PPML with log-distance (log-log specification, comparable to Monte et al.)
message("[3/4] PPML: Two-way FE with log(distance)...")
ppml_logdist <- tryCatch({
  fepois(flow ~ ln_distance | origin_tract + dest_tract,
         data = df_full_between[!is.na(ln_distance)], glm.iter = 100)
}, error = function(e) {
  message(sprintf("PPML log-dist failed: %s", e$message))
  NULL
})

# PPML log-distance without FE (standard gravity for comparison)
message("[4/4] PPML: No FE with log(distance)...")
ppml_logdist_nofe <- tryCatch({
  fepois(flow ~ ln_distance,
         data = df_full_between[!is.na(ln_distance)], glm.iter = 100)
}, error = function(e) {
  message(sprintf("PPML log-dist no FE failed: %s", e$message))
  NULL
})

toc()

# Print PPML results
ppml_models <- list()
if (!is.null(ppml_between)) {
  ppml_models[["PPML between-tract"]] <- ppml_between
  message(sprintf("\nPPML (between-tract) coefficient on distance: %.5f (SE = %.5f)",
                  coef(ppml_between)[1], se(ppml_between)[1]))
  message(sprintf("  => nu = %.4f", -coef(ppml_between)[1]))
}
if (!is.null(ppml_full)) {
  ppml_models[["PPML all pairs"]] <- ppml_full
  message(sprintf("\nPPML (all pairs) coefficient on distance: %.5f (SE = %.5f)",
                  coef(ppml_full)[1], se(ppml_full)[1]))
  message(sprintf("  => nu = %.4f", -coef(ppml_full)[1]))
}
if (!is.null(ppml_logdist)) {
  ppml_models[["PPML Two-way FE (log dist)"]] <- ppml_logdist
  message(sprintf("\nPPML (log-dist, two-way FE) coefficient: %.5f (SE = %.5f)",
                  coef(ppml_logdist)[1], se(ppml_logdist)[1]))
  message(sprintf("  => log-log elasticity: %.4f", -coef(ppml_logdist)[1]))
}
if (!is.null(ppml_logdist_nofe)) {
  ppml_models[["PPML No FE (log dist)"]] <- ppml_logdist_nofe
  message(sprintf("\nPPML (log-dist, no FE) coefficient: %.5f (SE = %.5f)",
                  coef(ppml_logdist_nofe)[1], se(ppml_logdist_nofe)[1]))
  message(sprintf("  => log-log elasticity: %.4f", -coef(ppml_logdist_nofe)[1]))
}

# =============================================================================
# 4. ROBUSTNESS CHECKS
# =============================================================================

message("\n=============================================================")
message("Robustness Checks")
message("=============================================================")

tic("Robustness checks")

# Rob 1: Exclude within-tract flows from OLS
message("\n[Rob 1] OLS two-way FE, excluding within-tract...")
ols_no_within <- feols(ln_flow ~ distance_km | origin_tract + dest_tract,
                       data = df_between)
message(sprintf("  coef = %.5f, nu = %.4f", coef(ols_no_within)[1], -coef(ols_no_within)[1]))

# Rob 2: Exclude very long commutes (>30km)
message("[Rob 2] OLS two-way FE, distance < 30km...")
ols_short <- feols(ln_flow ~ distance_km | origin_tract + dest_tract,
                   data = df_positive[distance_km < 30])
message(sprintf("  coef = %.5f, nu = %.4f", coef(ols_short)[1], -coef(ols_short)[1]))

# Rob 3: Quadratic distance
message("[Rob 3] OLS two-way FE, quadratic distance...")
ols_quad <- feols(ln_flow ~ distance_km + I(distance_km^2) | origin_tract + dest_tract,
                  data = df_positive)
message(sprintf("  linear coef = %.5f, quadratic coef = %.7f",
                coef(ols_quad)[1], coef(ols_quad)[2]))

# Rob 4: By earnings category
message("[Rob 4] OLS two-way FE by earnings group...")
df_positive[, ln_flow_high := ifelse(flow_high_earn > 0, log(flow_high_earn), NA_real_)]
df_positive[, ln_flow_low := ifelse(flow_low_earn > 0, log(flow_low_earn), NA_real_)]
ols_high_earn <- feols(ln_flow_high ~ distance_km | origin_tract + dest_tract,
                       data = df_positive[!is.na(ln_flow_high)])
ols_low_earn <- feols(ln_flow_low ~ distance_km | origin_tract + dest_tract,
                      data = df_positive[!is.na(ln_flow_low)])
message(sprintf("  High earners: coef = %.5f, nu = %.4f",
                coef(ols_high_earn)[1], -coef(ols_high_earn)[1]))
message(sprintf("  Low earners: coef = %.5f, nu = %.4f",
                coef(ols_low_earn)[1], -coef(ols_low_earn)[1]))

toc()

# =============================================================================
# 5. COMPILE RESULTS TABLE
# =============================================================================

message("\n=============================================================")
message("Compiling Results")
message("=============================================================")

# Build unified results table
all_models <- c(ols_models, ppml_models, list(
  "OLS no within-tract" = ols_no_within,
  "OLS dist < 30km" = ols_short,
  "OLS quadratic" = ols_quad,
  "OLS high earners" = ols_high_earn,
  "OLS low earners" = ols_low_earn
))

# Dynamically determine estimator and dep_var based on model names
n_ols <- length(ols_models)
n_ppml <- length(ppml_models)
n_robust <- 5  # ols_no_within, ols_short, ols_quad, ols_high_earn, ols_low_earn

# Build estimator vector
estimator_vec <- c(rep("OLS", n_ols),
                   rep("PPML", n_ppml),
                   rep("OLS", n_robust))

# Build dep_var vector (ln_flow for OLS, flow for PPML)
dep_var_vec <- c(rep("ln_flow", n_ols),
                 rep("flow", n_ppml),
                 "ln_flow", "ln_flow", "ln_flow", "ln_flow_high", "ln_flow_low")

results <- data.table(
  specification = names(all_models),
  estimator = estimator_vec,
  coef_distance = sapply(all_models, function(m) {
    cc <- coef(m)
    if ("distance_km" %in% names(cc)) cc["distance_km"] else cc[1]
  }),
  se_distance = sapply(all_models, function(m) {
    ss <- se(m)
    if ("distance_km" %in% names(ss)) ss["distance_km"] else ss[1]
  }),
  nu = sapply(all_models, function(m) {
    cc <- coef(m)
    -(if ("distance_km" %in% names(cc)) cc["distance_km"] else cc[1])
  }),
  n_obs = sapply(all_models, function(m) m$nobs),
  dep_var = dep_var_vec
)

# Add two-way clustered SE for main spec
results[specification == "Two-way FE",
        se_twoway_clustered := se(ols_twoway_cl)["distance_km"]]

fwrite(results, "../output/gravity_estimates.csv")
message("Saved: ../output/gravity_estimates.csv")

# Print formatted table
message("\n--- ESTIMATION RESULTS ---")
print(results[, .(specification, estimator, coef_distance = round(coef_distance, 5),
                   se_distance = round(se_distance, 5),
                   nu = round(nu, 4), n_obs)])

# =============================================================================
# 6. DIAGNOSTIC PLOTS
# =============================================================================

message("\n=============================================================")
message("Creating Diagnostic Plots")
message("=============================================================")

pdf("../output/diagnostic_plots.pdf", width = 10, height = 8)

# Plot 1: Distribution of flows (positive only)
p1 <- ggplot(df_positive, aes(x = flow)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Distribution of Commuting Flows (positive only)",
       x = "Flow (log scale)", y = "Count") +
  theme_minimal()
print(p1)

# Plot 2: Distribution of distances (positive flows)
p2 <- ggplot(df_positive[within_tract == FALSE], aes(x = distance_km)) +
  geom_histogram(bins = 80, fill = "coral", alpha = 0.7) +
  labs(title = "Distance Distribution (positive between-tract flows)",
       x = "Euclidean Distance (km)", y = "Count") +
  theme_minimal()
print(p2)

# Plot 3: Flow-weighted distance distribution
p3 <- ggplot(df_positive[within_tract == FALSE], aes(x = distance_km, weight = flow)) +
  geom_histogram(bins = 80, fill = "darkgreen", alpha = 0.7) +
  labs(title = "Flow-Weighted Distance Distribution",
       x = "Euclidean Distance (km)", y = "Weighted Count (by flow)") +
  theme_minimal()
print(p3)

# Plot 4: Log flow vs distance (raw scatter with binned means)
bin_means <- df_between[, .(
  mean_ln_flow = mean(ln_flow),
  mean_flow = mean(flow),
  n = .N
), by = .(dist_bin = cut(distance_km, breaks = seq(0, ceiling(max(distance_km)), by = 1)))]
bin_means[, dist_mid := as.numeric(gsub("\\(|\\]", "", gsub(",.*", "", dist_bin))) + 0.5]

p4 <- ggplot(bin_means[!is.na(dist_mid)], aes(x = dist_mid, y = mean_ln_flow)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Mean Log Flow vs Distance (1km bins, between-tract)",
       x = "Distance (km)", y = "Mean ln(flow)",
       size = "N pairs") +
  theme_minimal()
print(p4)

# Plot 5: Residual plot from two-way FE model
resid_data <- data.table(
  distance_km = df_positive$distance_km,
  residual = residuals(ols_twoway),
  fitted = fitted(ols_twoway),
  flow = df_positive$flow,
  within_tract = df_positive$within_tract
)

p5 <- ggplot(resid_data, aes(x = fitted, y = residual)) +
  geom_bin2d(bins = 50) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "OLS Two-way FE: Residuals vs Fitted",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()
print(p5)

# Plot 6: Residuals vs distance
p6 <- ggplot(resid_data[within_tract == FALSE],
             aes(x = distance_km, y = residual)) +
  geom_bin2d(bins = 50) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "red") +
  labs(title = "OLS Two-way FE: Residuals vs Distance (between-tract)",
       x = "Distance (km)", y = "Residuals") +
  theme_minimal()
print(p6)

# Plot 7: Coefficient comparison across specifications
coef_plot <- results[specification %in% c("No FE", "Origin FE", "Dest FE",
                                           "Two-way FE", "OLS no within-tract",
                                           "OLS dist < 30km")]
coef_plot[, specification := factor(specification, levels = rev(specification))]

p7 <- ggplot(coef_plot, aes(x = nu, y = specification)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = nu - 1.96 * se_distance, xmax = nu + 1.96 * se_distance),
                 height = 0.2) +
  geom_vline(xintercept = 0.07, color = "red", linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 0.07, y = 0.5, label = "Ahlfeldt et al.\n(0.07)", color = "red",
           hjust = -0.1, size = 3) +
  labs(title = "Distance Semi-Elasticity (nu) Across Specifications",
       x = expression(nu ~ "(distance semi-elasticity)"),
       y = NULL) +
  theme_minimal()
print(p7)

# Plot 8: Sparsity of commuting matrix
origin_stats <- commuting[, .(
  n_positive = sum(flow > 0),
  total_flow = sum(flow)
), by = origin_tract]

p8a <- ggplot(origin_stats, aes(x = n_positive)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Number of Destinations per Origin Tract",
       x = "Number of destinations with positive flow",
       y = "Count of origin tracts") +
  theme_minimal()

p8b <- ggplot(origin_stats, aes(x = total_flow)) +
  geom_histogram(bins = 50, fill = "coral", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Total Outflow per Origin Tract",
       x = "Total outgoing commuters (log scale)",
       y = "Count of origin tracts") +
  theme_minimal()

print(p8a + p8b)

dev.off()

message("Saved: ../output/diagnostic_plots.pdf")

# =============================================================================
# 7. FINAL SUMMARY
# =============================================================================

message("\n=============================================================")
message("GRAVITY ESTIMATION SUMMARY")
message("=============================================================")

main_nu <- -coef(ols_twoway)["distance_km"]
main_se <- se(ols_twoway_cl)["distance_km"]

message(sprintf("\nMain estimate (OLS Two-way FE):"))
message(sprintf("  nu = %.4f (SE = %.4f, two-way clustered)", main_nu, main_se))
message(sprintf("  Interpretation: 1 km -> %.1f%% fewer commuters", 100 * (1 - exp(-main_nu))))
message(sprintf("  R-squared: %.4f", r2(ols_twoway, "r2")))
message(sprintf("  Within R-squared: %.4f", r2(ols_twoway, "wr2")))

if (!is.null(ppml_between)) {
  ppml_nu <- -coef(ppml_between)["distance_km"]
  message(sprintf("\nPPML estimate (between-tract):"))
  message(sprintf("  nu = %.4f (SE = %.4f)", ppml_nu, se(ppml_between)["distance_km"]))
}

message(sprintf("\nAhlfeldt et al. (2015) benchmark: nu ~ 0.07"))
message(sprintf("Our estimate is %s the Berlin benchmark",
                ifelse(main_nu > 0.07, "above", "below")))

message("\n=============================================================")
message("Estimation complete!")
message("=============================================================")
