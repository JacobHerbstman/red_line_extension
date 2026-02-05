# Gravity equation estimation using transit travel times

source("../../setup_environment/code/packages.R")

message("Estimating gravity equations (travel time)")

# --- Load data ---
message("Loading data...")

commuting <- fread("../output/commuting_matrix_travel_time.csv",
                   colClasses = c(origin_tract = "character", dest_tract = "character"))

message(sprintf("Commuting matrix: %s rows, positive: %s",
                format(nrow(commuting), big.mark = ","),
                format(sum(commuting$flow > 0), big.mark = ",")))

# --- Filter problematic tracts ---
message("Filtering tracts...")

floorspace <- fread("../input/tract_floorspace.csv",
                    colClasses = c(census_tract_geoid = "character"))

MIN_RESIDENTIAL_SQFT <- 100000

excluded_tracts <- floorspace[is.na(total_sqft_residential) |
                               total_sqft_residential < MIN_RESIDENTIAL_SQFT,
                              census_tract_geoid]

message(sprintf("Excluding %d tracts with < %s sqft residential",
                length(excluded_tracts), format(MIN_RESIDENTIAL_SQFT, big.mark = ",")))

n_before <- nrow(commuting)
commuting <- commuting[!(origin_tract %in% excluded_tracts) &
                       !(dest_tract %in% excluded_tracts)]
n_after <- nrow(commuting)

message(sprintf("Filtered: %s -> %s rows",
                format(n_before, big.mark = ","),
                format(n_after, big.mark = ",")))

# --- Prepare samples ---
message("Preparing estimation samples...")

commuting[, origin_tract := as.factor(origin_tract)]
commuting[, dest_tract := as.factor(dest_tract)]

df_positive <- commuting[flow > 0]
df_between <- commuting[within_tract == FALSE & flow > 0]
df_full_between <- commuting[within_tract == FALSE]
df_large <- commuting[flow >= 10]

message(sprintf("Positive: %s, Between positive: %s, Full between: %s, L>=10: %s",
                format(nrow(df_positive), big.mark = ","),
                format(nrow(df_between), big.mark = ","),
                format(nrow(df_full_between), big.mark = ","),
                format(nrow(df_large), big.mark = ",")))

# --- Estimate ---
message("Estimating gravity equations...")

results_list <- list()

# PPML two-way FE
message("  PPML two-way FE...")
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
  message(sprintf("  PPML: nu = %.5f, %.2f%% fewer commuters per minute",
                  nu_ppml, 100 * (1 - exp(-nu_ppml))))

  if (nu_ppml <= 0) {
    stop("Estimated nu is non-positive")
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

# OLS two-way FE
message("  OLS two-way FE...")
tic("OLS")
ols_twoway <- feols(ln_flow ~ travel_time_min | origin_tract + dest_tract,
                    data = df_positive)
toc()

nu_ols <- -coef(ols_twoway)["travel_time_min"]
se_ols <- se(ols_twoway)["travel_time_min"]
message(sprintf("  OLS: nu = %.5f", nu_ols))

ols_twoway_cl <- summary(ols_twoway, vcov = ~origin_tract + dest_tract)
se_ols_cl <- se(ols_twoway_cl)["travel_time_min"]

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

# OLS flows >= 10
message("  OLS (flows >= 10)...")
ols_large <- feols(ln_flow ~ travel_time_min | origin_tract + dest_tract,
                   data = df_large)

nu_large <- -coef(ols_large)["travel_time_min"]
se_large <- se(ols_large)["travel_time_min"]
message(sprintf("  OLS (L>=10): nu = %.5f", nu_large))

results_list[["OLS (L>=10)"]] <- data.table(
  specification = "OLS (L>=10)",
  estimator = "OLS",
  sample = "Flows >= 10",
  coef_travel_time = coef(ols_large)["travel_time_min"],
  se = se_large,
  se_clustered = NA_real_,
  nu = nu_large,
  n_obs = ols_large$nobs,
  pct_change_per_min = 100 * nu_large
)

# Log-log elasticity
message("  Log-log elasticity...")
df_loglog <- df_between[!is.na(ln_travel_time) & is.finite(ln_travel_time)]
ols_loglog <- feols(ln_flow ~ ln_travel_time | origin_tract + dest_tract,
                    data = df_loglog)

elast <- -coef(ols_loglog)["ln_travel_time"]
se_elast <- se(ols_loglog)["ln_travel_time"]
message(sprintf("  Log-log: elasticity = %.3f", elast))

results_list[["Log-log elasticity"]] <- data.table(
  specification = "Log-log elasticity",
  estimator = "OLS",
  sample = "Between-tract positive",
  coef_travel_time = coef(ols_loglog)["ln_travel_time"],
  se = se_elast,
  se_clustered = NA_real_,
  nu = elast,
  n_obs = ols_loglog$nobs,
  pct_change_per_min = NA_real_
)

results <- rbindlist(results_list)

# --- Save ---
message("Saving outputs...")

fwrite(results, "../output/gravity_estimates_travel_time.csv")

models <- list(ppml = ppml_between, ols = ols_twoway, ols_large = ols_large, ols_loglog = ols_loglog)
saveRDS(models, "../output/gravity_models_travel_time.rds")

if (!is.null(ppml_between)) {
  nu_out <- data.table(parameter = "nu_time_per_min", value = nu_ppml)
  fwrite(nu_out, "../output/nu_time_per_min.csv")

  fe <- fixef(ppml_between)
  if (!is.null(fe$dest_tract)) {
    dest_fe <- data.table(dest_tract = names(fe$dest_tract),
                          dest_fe = as.numeric(fe$dest_tract))
    fwrite(dest_fe, "../output/gravity_destination_fe.csv")
  }
}

fwrite(commuting, "../output/gravity_data_travel_time.csv")

# --- Plots ---
message("Creating diagnostic plots...")

pdf("../output/diagnostic_plots_travel_time.pdf", width = 10, height = 8)

# Binscatter
resid_y <- feols(ln_flow ~ 1 | origin_tract + dest_tract, data = df_between)
resid_x <- feols(travel_time_min ~ 1 | origin_tract + dest_tract, data = df_between)

plot_data <- data.table(resid_y = residuals(resid_y), resid_x = residuals(resid_x))
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
    subtitle = "Residualized binscatter, 20 bins",
    x = "Travel Time (residualized, minutes)",
    y = "Log Commuters (residualized)"
  ) +
  theme_minimal(base_size = 12)
print(p1)

ggsave("../output/gravity_binscatter_residualized.pdf", p1, width = 10, height = 7)

# Coefficient comparison
coef_plot <- results[specification != "Log-log elasticity"]

p2 <- ggplot(coef_plot, aes(x = nu, y = reorder(specification, nu))) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = nu - 1.96 * se, xmax = nu + 1.96 * se),
                 height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0.035, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 0.038, y = 0.5, label = "Berlin\n(0.035)", color = "red", hjust = 0, size = 3) +
  labs(title = "Travel Time Semi-Elasticity Estimates",
       x = expression(nu ~ " (per minute)"), y = NULL) +
  theme_minimal(base_size = 12)
print(p2)

# Flow distribution
p3 <- ggplot(df_positive, aes(x = flow)) +
  geom_histogram(bins = 100, fill = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Distribution of Commuting Flows", x = "Flow (log scale)", y = "Count") +
  theme_minimal(base_size = 12)
print(p3)

# Travel time distribution
p4 <- ggplot(df_between, aes(x = travel_time_min)) +
  geom_histogram(bins = 80, fill = "coral", alpha = 0.7) +
  labs(title = "Travel Time Distribution", x = "Travel Time (minutes)", y = "Count") +
  theme_minimal(base_size = 12)
print(p4)

# Flow-weighted travel time
p5 <- ggplot(df_between, aes(x = travel_time_min, weight = flow)) +
  geom_histogram(bins = 80, fill = "darkgreen", alpha = 0.7) +
  labs(title = "Flow-Weighted Travel Time", x = "Travel Time (minutes)", y = "Weighted Count") +
  theme_minimal(base_size = 12)
print(p5)

# Residuals
resid_data <- data.table(
  travel_time = df_positive$travel_time_min,
  residual = residuals(ols_twoway),
  fitted = fitted(ols_twoway)
)

p6 <- ggplot(resid_data, aes(x = fitted, y = residual)) +
  geom_bin2d(bins = 50) +
  scale_fill_viridis_c() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "OLS: Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal(base_size = 12)
print(p6)

# Raw binned scatter
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
  labs(title = "Raw: Mean Log Flow vs Travel Time (5-min bins)",
       x = "Travel Time (minutes)", y = "Mean log(flow)") +
  theme_minimal(base_size = 12)
print(p7)

dev.off()

# --- Summary ---
message("\nResults:")
print(results[, .(specification, nu = round(nu, 5), se = round(se, 6), n_obs)])

if (!is.null(ppml_between)) {
  message(sprintf("\nPPML: nu = %.5f (%.2f%% fewer commuters per minute)",
                  nu_ppml, 100 * (1 - exp(-nu_ppml))))
  message(sprintf("OLS: nu = %.5f (clustered SE = %.5f)", nu_ols, se_ols_cl))
  message(sprintf("Berlin benchmark: 0.035, ratio: %.2f", nu_ppml / 0.035))
}

message("\nGravity estimation complete")
