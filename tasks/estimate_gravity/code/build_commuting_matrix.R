# build_commuting_matrix.R
# Merge LODES origin-destination flows with Euclidean distances for Chicago tracts
# Output: commuting_matrix.csv (all OD pairs including zeros) and summary statistics

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

message("=============================================================")
message("Building Commuting Matrix for Chicago Tracts")
message("=============================================================")

# Chicago tract list (796 tracts)
chicago_tracts <- fread("../input/chicago_tracts_2010.csv", colClasses = c(GEOID = "character"))
chicago_geoids <- chicago_tracts$GEOID
message(sprintf("\nChicago tracts: %d", length(chicago_geoids)))

# LODES origin-destination flows
lodes <- fread("../input/lodes_od_cook_county_2019.csv",
               colClasses = c(h_tract = "character", w_tract = "character"))
message(sprintf("LODES OD rows (Cook County): %s", format(nrow(lodes), big.mark = ",")))

# Distance matrix (796 x 796 = 633,616 pairs)
distances <- fread("../input/distance_matrix_km.csv",
                    colClasses = c(origin_tract = "character", dest_tract = "character"))
message(sprintf("Distance matrix rows: %s", format(nrow(distances), big.mark = ",")))

# =============================================================================
# 2. FILTER LODES TO CHICAGO-TO-CHICAGO FLOWS
# =============================================================================

message("\n[1/4] Filtering LODES to Chicago-Chicago flows...")

# Keep flows where BOTH origin (home) and destination (work) are Chicago tracts
lodes_chi <- lodes[h_tract %in% chicago_geoids & w_tract %in% chicago_geoids]
message(sprintf("Chicago-Chicago OD pairs with positive flows: %s", format(nrow(lodes_chi), big.mark = ",")))
message(sprintf("Total jobs in Chicago-Chicago flows: %s", format(sum(lodes_chi$S000), big.mark = ",")))

# Aggregate flows by OD pair (in case of duplicates)
# LODES should be unique by h_tract x w_tract, but verify
lodes_agg <- lodes_chi[, .(
  flow = sum(S000),
  flow_low_earn = sum(SE01),
  flow_mid_earn = sum(SE02),
  flow_high_earn = sum(SE03)
), by = .(origin_tract = h_tract, dest_tract = w_tract)]

message(sprintf("Unique positive OD pairs: %s", format(nrow(lodes_agg), big.mark = ",")))

# =============================================================================
# 3. MERGE WITH DISTANCE MATRIX (FULL MATRIX INCLUDING ZEROS)
# =============================================================================

message("\n[2/4] Merging flows with distance matrix...")

# Start with full distance matrix (all 796x796 pairs)
commuting <- merge(distances, lodes_agg,
                   by = c("origin_tract", "dest_tract"),
                   all.x = TRUE)

# Fill missing flows with 0 (these are OD pairs with no commuters in LODES)
commuting[is.na(flow), flow := 0L]
commuting[is.na(flow_low_earn), flow_low_earn := 0L]
commuting[is.na(flow_mid_earn), flow_mid_earn := 0L]
commuting[is.na(flow_high_earn), flow_high_earn := 0L]

# Create log variables for OLS (only defined for positive flows)
commuting[, ln_flow := ifelse(flow > 0, log(flow), NA_real_)]
commuting[, ln_distance := ifelse(distance_km > 0, log(distance_km), NA_real_)]

# Flag within-tract flows
commuting[, within_tract := origin_tract == dest_tract]

n_total <- nrow(commuting)
n_positive <- sum(commuting$flow > 0)
n_zero <- sum(commuting$flow == 0)
n_within <- sum(commuting$within_tract)

message(sprintf("Total OD pairs: %s (= %d x %d)", format(n_total, big.mark = ","),
                length(chicago_geoids), length(chicago_geoids)))
message(sprintf("Positive flows: %s (%.1f%%)", format(n_positive, big.mark = ","),
                100 * n_positive / n_total))
message(sprintf("Zero flows: %s (%.1f%%)", format(n_zero, big.mark = ","),
                100 * n_zero / n_total))
message(sprintf("Within-tract pairs: %d", n_within))

# =============================================================================
# 4. SUMMARY STATISTICS
# =============================================================================

message("\n[3/4] Computing summary statistics...")

# Flow distribution (positive flows only)
pos_flows <- commuting[flow > 0]
within_flows <- commuting[within_tract == TRUE]

sink("../output/commuting_summary.txt")

cat("=============================================================\n")
cat("COMMUTING MATRIX SUMMARY STATISTICS\n")
cat("Chicago Census Tracts, LODES 2019\n")
cat("=============================================================\n\n")

cat("--- MATRIX DIMENSIONS ---\n")
cat(sprintf("Number of Chicago tracts: %d\n", length(chicago_geoids)))
cat(sprintf("Total possible OD pairs: %s\n", format(n_total, big.mark = ",")))
cat(sprintf("Positive flow pairs: %s (%.1f%%)\n", format(n_positive, big.mark = ","),
            100 * n_positive / n_total))
cat(sprintf("Zero flow pairs: %s (%.1f%%)\n", format(n_zero, big.mark = ","),
            100 * n_zero / n_total))
cat(sprintf("Within-tract pairs: %d\n", n_within))
cat(sprintf("Within-tract pairs with positive flow: %d (%.1f%%)\n",
            sum(within_flows$flow > 0), 100 * sum(within_flows$flow > 0) / n_within))

cat("\n--- FLOW DISTRIBUTION (positive flows only) ---\n")
cat(sprintf("Total jobs: %s\n", format(sum(pos_flows$flow), big.mark = ",")))
cat(sprintf("Mean flow: %.2f\n", mean(pos_flows$flow)))
cat(sprintf("Median flow: %d\n", as.integer(median(pos_flows$flow))))
cat(sprintf("P10: %d\n", as.integer(quantile(pos_flows$flow, 0.10))))
cat(sprintf("P25: %d\n", as.integer(quantile(pos_flows$flow, 0.25))))
cat(sprintf("P75: %d\n", as.integer(quantile(pos_flows$flow, 0.75))))
cat(sprintf("P90: %d\n", as.integer(quantile(pos_flows$flow, 0.90))))
cat(sprintf("P99: %d\n", as.integer(quantile(pos_flows$flow, 0.99))))
cat(sprintf("Max flow: %d\n", max(pos_flows$flow)))

cat("\n--- WITHIN-TRACT COMMUTING ---\n")
cat(sprintf("Total within-tract jobs: %s (%.1f%% of all jobs)\n",
            format(sum(within_flows$flow), big.mark = ","),
            100 * sum(within_flows$flow) / sum(pos_flows$flow)))
cat(sprintf("Mean within-tract flow: %.1f\n", mean(within_flows$flow)))
cat(sprintf("Median within-tract flow: %d\n", as.integer(median(within_flows$flow))))

cat("\n--- DISTANCE DISTRIBUTION (positive flows, excluding within-tract) ---\n")
between_flows <- pos_flows[within_tract == FALSE]
cat(sprintf("Mean distance (km): %.2f\n", mean(between_flows$distance_km)))
cat(sprintf("Median distance (km): %.2f\n", median(between_flows$distance_km)))
cat(sprintf("P10 distance: %.2f\n", quantile(between_flows$distance_km, 0.10)))
cat(sprintf("P25 distance: %.2f\n", quantile(between_flows$distance_km, 0.25)))
cat(sprintf("P75 distance: %.2f\n", quantile(between_flows$distance_km, 0.75)))
cat(sprintf("P90 distance: %.2f\n", quantile(between_flows$distance_km, 0.90)))
cat(sprintf("Max distance: %.2f\n", max(between_flows$distance_km)))

cat("\n--- FLOW-WEIGHTED DISTANCE (positive flows, excluding within-tract) ---\n")
cat(sprintf("Flow-weighted mean distance (km): %.2f\n",
            weighted.mean(between_flows$distance_km, between_flows$flow)))
cat(sprintf("Flow-weighted median distance (km): %.2f\n", {
  # Weighted median
  ord <- order(between_flows$distance_km)
  cumw <- cumsum(between_flows$flow[ord]) / sum(between_flows$flow)
  between_flows$distance_km[ord][which(cumw >= 0.5)[1]]
}))

cat("\n--- TRACT-LEVEL SUMMARY ---\n")
origin_totals <- commuting[, .(total_outflow = sum(flow), n_destinations = sum(flow > 0)), by = origin_tract]
dest_totals <- commuting[, .(total_inflow = sum(flow), n_origins = sum(flow > 0)), by = dest_tract]
cat(sprintf("Mean tract outflow (residents commuting): %.0f\n", mean(origin_totals$total_outflow)))
cat(sprintf("Mean tract inflow (workers arriving): %.0f\n", mean(dest_totals$total_inflow)))
cat(sprintf("Mean destinations per origin: %.0f\n", mean(origin_totals$n_destinations)))
cat(sprintf("Mean origins per destination: %.0f\n", mean(dest_totals$n_origins)))
cat(sprintf("Tracts with zero total outflow: %d\n", sum(origin_totals$total_outflow == 0)))
cat(sprintf("Tracts with zero total inflow: %d\n", sum(dest_totals$total_inflow == 0)))

sink()

message("Summary statistics written to ../output/commuting_summary.txt")

# =============================================================================
# 5. SAVE OUTPUT
# =============================================================================

message("\n[4/4] Saving commuting matrix...")

fwrite(commuting, "../output/commuting_matrix.csv")
message(sprintf("Saved: ../output/commuting_matrix.csv (%s rows)",
                format(nrow(commuting), big.mark = ",")))

message("\n=============================================================")
message("Commuting matrix build complete!")
message("=============================================================")
