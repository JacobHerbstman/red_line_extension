# Merge LODES OD flows with r5r travel times for Chicago tracts

source("../../setup_environment/code/packages.R")

message("Building commuting matrix with travel times")

# Load data
chicago_tracts <- fread("../input/chicago_tracts_2010.csv", colClasses = c(GEOID = "character"))
chicago_geoids <- chicago_tracts$GEOID
message(sprintf("Chicago tracts: %d", length(chicago_geoids)))

lodes <- fread("../input/lodes_od_cook_county_2019.csv",
               colClasses = c(h_tract = "character", w_tract = "character"))
message(sprintf("LODES OD rows: %s", format(nrow(lodes), big.mark = ",")))

travel_times <- fread("../input/travel_time_matrix_baseline.csv",
                      colClasses = c(origin_tract = "character", dest_tract = "character"))
message(sprintf("Travel time rows: %s", format(nrow(travel_times), big.mark = ",")))

# Check GEOID formats
message("GEOID format check:")
message(sprintf("  LODES sample: %s (len %d)", lodes$h_tract[1], nchar(lodes$h_tract[1])))
message(sprintf("  Travel times sample: %s (len %d)", travel_times$origin_tract[1], nchar(travel_times$origin_tract[1])))

lodes_chi_origins <- unique(lodes$h_tract)[unique(lodes$h_tract) %in% chicago_geoids]
tt_chi_origins <- unique(travel_times$origin_tract)[unique(travel_times$origin_tract) %in% chicago_geoids]

message(sprintf("  LODES home tracts in Chicago: %d", length(lodes_chi_origins)))
message(sprintf("  Travel times origins in Chicago: %d", length(tt_chi_origins)))

# Filter to Chicago-Chicago flows
message("Filtering to Chicago-Chicago flows...")
lodes_chi <- lodes[h_tract %in% chicago_geoids & w_tract %in% chicago_geoids]
message(sprintf("Chicago-Chicago flows: %s, total jobs: %s",
                format(nrow(lodes_chi), big.mark = ","),
                format(sum(lodes_chi$S000), big.mark = ",")))

lodes_agg <- lodes_chi[, .(
  flow = sum(S000),
  flow_low_earn = sum(SE01),
  flow_mid_earn = sum(SE02),
  flow_high_earn = sum(SE03)
), by = .(origin_tract = h_tract, dest_tract = w_tract)]

message(sprintf("Unique positive OD pairs: %s", format(nrow(lodes_agg), big.mark = ",")))

# Merge with travel times
message("Merging with travel times...")

commuting <- merge(travel_times, lodes_agg,
                   by = c("origin_tract", "dest_tract"),
                   all.x = TRUE)

commuting[is.na(flow), flow := 0L]
commuting[is.na(flow_low_earn), flow_low_earn := 0L]
commuting[is.na(flow_mid_earn), flow_mid_earn := 0L]
commuting[is.na(flow_high_earn), flow_high_earn := 0L]

commuting[, ln_flow := ifelse(flow > 0, log(flow), NA_real_)]
commuting[, ln_travel_time := ifelse(travel_time_min > 0, log(travel_time_min), NA_real_)]
commuting[, within_tract := origin_tract == dest_tract]

n_total <- nrow(commuting)
n_positive <- sum(commuting$flow > 0)
n_zero <- sum(commuting$flow == 0)

message(sprintf("Total pairs: %s, positive: %s (%.1f%%), zero: %s",
                format(n_total, big.mark = ","),
                format(n_positive, big.mark = ","),
                100 * n_positive / n_total,
                format(n_zero, big.mark = ",")))

# Summary stats
message(sprintf("Mean travel time: %.1f min, Median: %.1f min",
                mean(commuting$travel_time_min, na.rm = TRUE),
                median(commuting$travel_time_min, na.rm = TRUE)))

pos_flows <- commuting[flow > 0 & within_tract == FALSE]
message(sprintf("Flow-weighted mean travel time: %.1f min",
                weighted.mean(pos_flows$travel_time_min, pos_flows$flow, na.rm = TRUE)))

# Write summary
sink("../output/commuting_summary_travel_time.txt")

cat("Commuting Matrix with Travel Times Summary\n")
cat(sprintf("Created: %s\n\n", Sys.time()))

cat("Matrix dimensions:\n")
cat(sprintf("  Tracts: %d\n", length(chicago_geoids)))
cat(sprintf("  Total pairs: %s\n", format(n_total, big.mark = ",")))
cat(sprintf("  Positive flows: %s (%.1f%%)\n", format(n_positive, big.mark = ","), 100 * n_positive / n_total))
cat(sprintf("  Total jobs: %s\n", format(sum(commuting$flow), big.mark = ",")))

cat("\nTravel time stats:\n")
cat(sprintf("  Mean: %.1f min\n", mean(commuting$travel_time_min, na.rm = TRUE)))
cat(sprintf("  Median: %.1f min\n", median(commuting$travel_time_min, na.rm = TRUE)))
cat(sprintf("  P10: %.1f, P90: %.1f\n",
            quantile(commuting$travel_time_min, 0.10, na.rm = TRUE),
            quantile(commuting$travel_time_min, 0.90, na.rm = TRUE)))
cat(sprintf("  Flow-weighted mean: %.1f min\n",
            weighted.mean(pos_flows$travel_time_min, pos_flows$flow, na.rm = TRUE)))

sink()

fwrite(commuting, "../output/commuting_matrix_travel_time.csv")

message(sprintf("Saved: commuting_matrix_travel_time.csv (%s rows)", format(nrow(commuting), big.mark = ",")))
