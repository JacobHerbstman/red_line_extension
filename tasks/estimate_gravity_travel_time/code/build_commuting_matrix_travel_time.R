# build_commuting_matrix_travel_time.R
# Merge LODES origin-destination flows with r5r travel times for Chicago tracts

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Building Commuting Matrix with Travel Times")
message("=============================================================")

# Load Chicago tract list (796 tracts)
chicago_tracts <- fread("../input/chicago_tracts_2010.csv", colClasses = c(GEOID = "character"))
chicago_geoids <- chicago_tracts$GEOID
message(sprintf("Chicago tracts: %d", length(chicago_geoids)))

# Load LODES (h_tract = home/origin, w_tract = work/destination)
lodes <- fread("../input/lodes_od_cook_county_2019.csv",
               colClasses = c(h_tract = "character", w_tract = "character"))
message(sprintf("LODES OD rows: %s", format(nrow(lodes), big.mark = ",")))

# Load travel times (origin_tract, dest_tract, travel_time_min)
travel_times <- fread("../input/travel_time_matrix_baseline.csv",
                      colClasses = c(origin_tract = "character", dest_tract = "character"))
message(sprintf("Travel time matrix rows: %s", format(nrow(travel_times), big.mark = ",")))

# =============================================================================
# GEOID Format Verification
# =============================================================================

message("\n--- GEOID Format Check ---")
message(sprintf("LODES h_tract sample: %s (len %d)", lodes$h_tract[1], nchar(lodes$h_tract[1])))
message(sprintf("Travel times origin sample: %s (len %d)", travel_times$origin_tract[1], nchar(travel_times$origin_tract[1])))
message(sprintf("Chicago tracts sample: %s (len %d)", chicago_geoids[1], nchar(chicago_geoids[1])))

# Check overlap
lodes_origins <- unique(lodes$h_tract)
tt_origins <- unique(travel_times$origin_tract)
lodes_chi_origins <- lodes_origins[lodes_origins %in% chicago_geoids]
tt_chi_origins <- tt_origins[tt_origins %in% chicago_geoids]

message(sprintf("\nLODES home tracts in Chicago: %d of %d", length(lodes_chi_origins), length(chicago_geoids)))
message(sprintf("Travel times origins in Chicago: %d of %d", length(tt_chi_origins), length(chicago_geoids)))

# =============================================================================
# Filter LODES to Chicago-Chicago flows
# =============================================================================

message("\n--- Filtering LODES to Chicago-Chicago flows ---")
lodes_chi <- lodes[h_tract %in% chicago_geoids & w_tract %in% chicago_geoids]
message(sprintf("Chicago-Chicago OD pairs with flows: %s", format(nrow(lodes_chi), big.mark = ",")))
message(sprintf("Total jobs: %s", format(sum(lodes_chi$S000), big.mark = ",")))

# Aggregate by OD pair (rename h_tract -> origin_tract, w_tract -> dest_tract)
lodes_agg <- lodes_chi[, .(
  flow = sum(S000),
  flow_low_earn = sum(SE01),
  flow_mid_earn = sum(SE02),
  flow_high_earn = sum(SE03)
), by = .(origin_tract = h_tract, dest_tract = w_tract)]

message(sprintf("Unique positive OD pairs: %s", format(nrow(lodes_agg), big.mark = ",")))

# =============================================================================
# Merge with travel times
# =============================================================================

message("\n--- Merging flows with travel times ---")

# Left join on travel times to keep all reachable pairs
commuting <- merge(travel_times, lodes_agg,
                   by = c("origin_tract", "dest_tract"),
                   all.x = TRUE)

# Fill missing flows with 0
commuting[is.na(flow), flow := 0L]
commuting[is.na(flow_low_earn), flow_low_earn := 0L]
commuting[is.na(flow_mid_earn), flow_mid_earn := 0L]
commuting[is.na(flow_high_earn), flow_high_earn := 0L]

# Create log variables
commuting[, ln_flow := ifelse(flow > 0, log(flow), NA_real_)]
commuting[, ln_travel_time := ifelse(travel_time_min > 0, log(travel_time_min), NA_real_)]

# Flag within-tract
commuting[, within_tract := origin_tract == dest_tract]

n_total <- nrow(commuting)
n_positive <- sum(commuting$flow > 0)
n_zero <- sum(commuting$flow == 0)
n_within <- sum(commuting$within_tract)

message(sprintf("\nTotal OD pairs: %s", format(n_total, big.mark = ",")))
message(sprintf("Positive flows: %s (%.1f%%)", format(n_positive, big.mark = ","), 100 * n_positive / n_total))
message(sprintf("Zero flows: %s (%.1f%%)", format(n_zero, big.mark = ","), 100 * n_zero / n_total))
message(sprintf("Within-tract pairs: %d", n_within))

# =============================================================================
# Summary statistics
# =============================================================================

message("\n--- Travel Time Statistics ---")
message(sprintf("Mean travel time: %.1f min", mean(commuting$travel_time_min, na.rm = TRUE)))
message(sprintf("Median travel time: %.1f min", median(commuting$travel_time_min, na.rm = TRUE)))
message(sprintf("P10: %.1f min", quantile(commuting$travel_time_min, 0.10, na.rm = TRUE)))
message(sprintf("P90: %.1f min", quantile(commuting$travel_time_min, 0.90, na.rm = TRUE)))

# Flow-weighted statistics (positive flows only)
pos_flows <- commuting[flow > 0 & within_tract == FALSE]
message(sprintf("\nFlow-weighted mean travel time: %.1f min",
                weighted.mean(pos_flows$travel_time_min, pos_flows$flow, na.rm = TRUE)))

# Write summary
sink("../output/commuting_summary_travel_time.txt")

cat("COMMUTING MATRIX WITH TRAVEL TIMES - SUMMARY\n")
cat("=============================================\n")
cat(sprintf("Created: %s\n\n", Sys.time()))

cat("--- MATRIX DIMENSIONS ---\n")
cat(sprintf("Number of Chicago tracts: %d\n", length(chicago_geoids)))
cat(sprintf("Total OD pairs: %s\n", format(n_total, big.mark = ",")))
cat(sprintf("Positive flow pairs: %s (%.1f%%)\n", format(n_positive, big.mark = ","), 100 * n_positive / n_total))
cat(sprintf("Zero flow pairs: %s (%.1f%%)\n", format(n_zero, big.mark = ","), 100 * n_zero / n_total))
cat(sprintf("Within-tract pairs: %d\n", n_within))

cat("\n--- FLOW STATISTICS ---\n")
cat(sprintf("Total jobs: %s\n", format(sum(commuting$flow), big.mark = ",")))
cat(sprintf("Mean flow (positive): %.2f\n", mean(commuting[flow > 0, flow])))
cat(sprintf("Median flow (positive): %d\n", as.integer(median(commuting[flow > 0, flow]))))

cat("\n--- TRAVEL TIME STATISTICS ---\n")
cat(sprintf("Mean: %.1f min\n", mean(commuting$travel_time_min, na.rm = TRUE)))
cat(sprintf("Median: %.1f min\n", median(commuting$travel_time_min, na.rm = TRUE)))
cat(sprintf("P10: %.1f min\n", quantile(commuting$travel_time_min, 0.10, na.rm = TRUE)))
cat(sprintf("P25: %.1f min\n", quantile(commuting$travel_time_min, 0.25, na.rm = TRUE)))
cat(sprintf("P75: %.1f min\n", quantile(commuting$travel_time_min, 0.75, na.rm = TRUE)))
cat(sprintf("P90: %.1f min\n", quantile(commuting$travel_time_min, 0.90, na.rm = TRUE)))
cat(sprintf("Max: %.1f min\n", max(commuting$travel_time_min, na.rm = TRUE)))

cat("\n--- FLOW-WEIGHTED TRAVEL TIME (positive, between-tract) ---\n")
cat(sprintf("Flow-weighted mean: %.1f min\n",
            weighted.mean(pos_flows$travel_time_min, pos_flows$flow, na.rm = TRUE)))

sink()

# =============================================================================
# Save output
# =============================================================================

fwrite(commuting, "../output/commuting_matrix_travel_time.csv")

message("\n=============================================================")
message(sprintf("Saved: ../output/commuting_matrix_travel_time.csv (%s rows)", format(nrow(commuting), big.mark = ",")))
message("Saved: ../output/commuting_summary_travel_time.txt")
message("=============================================================")
