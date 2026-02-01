# compute_travel_times_bg.R
# Compute block-group-to-block-group multimodal travel time matrices using r5r
#
# Computes travel times for two scenarios:
# 1. Baseline: Current CTA network
# 2. Extension: CTA network with Red Line Extension
#
# NOTE: Block group level has ~2,500 points -> ~6.25M OD pairs
# This requires significant memory (12-14GB Java heap recommended)
# May need to run on a server if local memory is insufficient

# =============================================================================
# CRITICAL: Set Java parameters BEFORE loading any packages
# =============================================================================

# JAVA_HOME should be set in the Makefile before calling R
# This ensures Java 21 is available for r5r/rJava
if (Sys.getenv("JAVA_HOME") != "") {
  message(sprintf("JAVA_HOME is set to: %s", Sys.getenv("JAVA_HOME")))
} else {
  warning("JAVA_HOME not set - r5r may fail. Set in Makefile or shell environment.")
}

# r5r requires Java 21+ with sufficient heap memory for BG-level computation
# Set to 12GB (leaves ~4GB for R on a 16GB machine - may be tight)
options(java.parameters = "-Xmx12G")

# Now load packages
source("../../setup_environment/code/packages.R")

# Load r5r separately (requires Java)
if (!require(r5r, quietly = TRUE)) {
  stop("ERROR: r5r package not installed. Please install r5r and ensure Java 21+ is available.")
}

message("=============================================================")
message("Computing Block Group Travel Time Matrices with r5r")
message("=============================================================")
message("NOTE: This is memory-intensive (~6.25M OD pairs)")
message("Java heap set to 12GB. If you get memory errors, run on a server.")

# =============================================================================
# PARAMETERS
# =============================================================================

# Routing parameters
DEPARTURE_DATE <- "2026-01-07"  # Wednesday (typical weekday) within GTFS calendar
DEPARTURE_TIME <- "08:00:00"    # Morning peak
MODE_TRANSIT <- c("WALK", "TRANSIT")
MODE_CAR <- c("CAR")
MAX_WALK_TIME <- 30             # minutes
MAX_TRIP_DURATION <- 90         # minutes
TIME_WINDOW <- 30               # Average over 30-minute window
PERCENTILES <- 50               # Median travel time

departure_datetime <- as.POSIXct(
  paste(DEPARTURE_DATE, DEPARTURE_TIME),
  format = "%Y-%m-%d %H:%M:%S"
)

message(sprintf("\nRouting parameters:"))
message(sprintf("  - Departure: %s", departure_datetime))
message(sprintf("  - Transit mode: %s", paste(MODE_TRANSIT, collapse = " + ")))
message(sprintf("  - Car mode: %s", paste(MODE_CAR, collapse = " + ")))
message(sprintf("  - Max walk time: %d minutes", MAX_WALK_TIME))
message(sprintf("  - Max trip duration: %d minutes", MAX_TRIP_DURATION))

# =============================================================================
# 1. LOAD BLOCK GROUP CENTROIDS
# =============================================================================

message("\n[1/7] Loading block group centroids...")

centroids <- read_csv("../input/block_group_centroids.csv", show_col_types = FALSE)

# r5r expects columns: id, lon, lat
points <- centroids %>%
  select(id, lon, lat)

n_bgs <- nrow(points)
n_pairs <- n_bgs^2

message(sprintf("Loaded %d block group centroids", n_bgs))
message(sprintf("Will compute %s OD pairs", format(n_pairs, big.mark = ",")))
message(sprintf("Estimated output size: ~%.0f MB", n_pairs * 30 / 1e6))

# Check for memory warnings
if (n_pairs > 5e6) {
  message("\nWARNING: Large number of OD pairs. If you encounter memory errors:")
  message("  1. Increase Java heap (options(java.parameters = '-Xmx14G'))")
  message("  2. Run on a server with more RAM")
  message("  3. Process in batches (not implemented here)")
}

# =============================================================================
# 2. COMPUTE BASELINE TRAVEL TIMES
# =============================================================================

message("\n[2/7] Computing BASELINE TRANSIT travel times (current network)...")

# Create temp directory for baseline network
baseline_dir <- "../temp/network_baseline"
dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)

# Copy network files to baseline directory
file.copy("../input/chicago.osm.pbf",
          file.path(baseline_dir, "chicago.osm.pbf"),
          overwrite = TRUE)
file.copy("../input/cta_gtfs.zip",
          file.path(baseline_dir, "cta_gtfs.zip"),
          overwrite = TRUE)

message("Building baseline network graph...")
tic("Baseline network build")

r5r_baseline <- build_network(data_path = baseline_dir, verbose = TRUE)

toc()

message("Computing baseline travel time matrix (this may take 30-60 minutes)...")
tic("Baseline travel time matrix")

ttm_baseline_transit <- travel_time_matrix(
  r5r_network = r5r_baseline,
  origins = points,
  destinations = points,
  mode = MODE_TRANSIT,
  departure_datetime = departure_datetime,
  max_walk_time = MAX_WALK_TIME,
  max_trip_duration = MAX_TRIP_DURATION,
  time_window = TIME_WINDOW,
  percentiles = PERCENTILES,
  verbose = TRUE
)

toc()

message(sprintf("Baseline transit matrix: %s rows", format(nrow(ttm_baseline_transit), big.mark = ",")))

# =============================================================================
# 3. COMPUTE BASELINE CAR TRAVEL TIMES
# =============================================================================

message("\n[3/7] Computing BASELINE CAR travel times...")
tic("Baseline car travel time matrix")

ttm_baseline_car <- travel_time_matrix(
  r5r_network = r5r_baseline,
  origins = points,
  destinations = points,
  mode = MODE_CAR,
  departure_datetime = departure_datetime,
  max_trip_duration = MAX_TRIP_DURATION,
  verbose = TRUE
)

toc()

message(sprintf("Baseline car matrix: %s rows", format(nrow(ttm_baseline_car), big.mark = ",")))

# Clean up to free memory
r5r::stop_r5(r5r_baseline)
rm(r5r_baseline)
rJava::.jgc(R.gc = TRUE)
gc()

# =============================================================================
# 3. COMPUTE EXTENSION TRAVEL TIMES
# =============================================================================

message("\n[4/7] Computing EXTENSION TRANSIT travel times (with Red Line Extension)...")

# Create temp directory for extension network
extension_dir <- "../temp/network_extension"
dir.create(extension_dir, recursive = TRUE, showWarnings = FALSE)

# Copy network files (using modified GTFS)
file.copy("../input/chicago.osm.pbf",
          file.path(extension_dir, "chicago.osm.pbf"),
          overwrite = TRUE)
file.copy("../input/cta_gtfs_with_extension.zip",
          file.path(extension_dir, "cta_gtfs.zip"),
          overwrite = TRUE)

message("Building extension network graph...")
tic("Extension network build")

r5r_extension <- build_network(data_path = extension_dir, verbose = TRUE)

toc()

message("Computing extension travel time matrix...")
tic("Extension travel time matrix")

ttm_extension_transit <- travel_time_matrix(
  r5r_network = r5r_extension,
  origins = points,
  destinations = points,
  mode = MODE_TRANSIT,
  departure_datetime = departure_datetime,
  max_walk_time = MAX_WALK_TIME,
  max_trip_duration = MAX_TRIP_DURATION,
  time_window = TIME_WINDOW,
  percentiles = PERCENTILES,
  verbose = TRUE
)

toc()

message(sprintf("Extension transit matrix: %s rows", format(nrow(ttm_extension_transit), big.mark = ",")))

# =============================================================================
# 5. COMPUTE EXTENSION CAR TRAVEL TIMES
# =============================================================================
# NOTE: Car times don't change with Red Line Extension, but compute for completeness

message("\n[5/7] Computing EXTENSION CAR travel times...")
tic("Extension car travel time matrix")

ttm_extension_car <- travel_time_matrix(
  r5r_network = r5r_extension,
  origins = points,
  destinations = points,
  mode = MODE_CAR,
  departure_datetime = departure_datetime,
  max_trip_duration = MAX_TRIP_DURATION,
  verbose = TRUE
)

toc()

message(sprintf("Extension car matrix: %s rows", format(nrow(ttm_extension_car), big.mark = ",")))

# Clean up
r5r::stop_r5(r5r_extension)
rm(r5r_extension)
rJava::.jgc(R.gc = TRUE)
gc()

# =============================================================================
# 4. COMPUTE TRAVEL TIME CHANGES
# =============================================================================

message("\n[6/7] Computing travel time changes...")

# Handle different column name possibilities
baseline_transit_col <- names(ttm_baseline_transit)[grep("travel_time", names(ttm_baseline_transit))][1]
extension_transit_col <- names(ttm_extension_transit)[grep("travel_time", names(ttm_extension_transit))][1]
baseline_car_col <- names(ttm_baseline_car)[grep("travel_time", names(ttm_baseline_car))][1]
extension_car_col <- names(ttm_extension_car)[grep("travel_time", names(ttm_extension_car))][1]

if (is.na(baseline_transit_col)) baseline_transit_col <- "travel_time"
if (is.na(extension_transit_col)) extension_transit_col <- "travel_time"
if (is.na(baseline_car_col)) baseline_car_col <- "travel_time"
if (is.na(extension_car_col)) extension_car_col <- "travel_time"

# Rename for merging
ttm_baseline_transit_rename <- ttm_baseline_transit %>%
  rename(travel_time_transit_baseline = !!sym(baseline_transit_col))

ttm_extension_transit_rename <- ttm_extension_transit %>%
  rename(travel_time_transit_extension = !!sym(extension_transit_col))

ttm_baseline_car_rename <- ttm_baseline_car %>%
  rename(travel_time_car = !!sym(baseline_car_col))

# Merge transit baseline and extension
travel_time_change <- ttm_baseline_transit_rename %>%
  left_join(
    ttm_extension_transit_rename %>% select(from_id, to_id, travel_time_transit_extension),
    by = c("from_id", "to_id")
  ) %>%
  left_join(
    ttm_baseline_car_rename %>% select(from_id, to_id, travel_time_car),
    by = c("from_id", "to_id")
  ) %>%
  mutate(
    travel_time_change = travel_time_transit_extension - travel_time_transit_baseline,
    travel_time_pct_change = if_else(
      travel_time_transit_baseline > 0,
      (travel_time_change / travel_time_transit_baseline) * 100,
      NA_real_
    ),
    # Min of car and transit (for gravity estimation)
    travel_time_min_baseline = pmin(travel_time_transit_baseline, travel_time_car, na.rm = TRUE),
    travel_time_min_extension = pmin(travel_time_transit_extension, travel_time_car, na.rm = TRUE)
  )

# Summary statistics
message("\nTRANSIT travel time change summary:")
message(sprintf("  - Mean baseline: %.1f min",
                mean(travel_time_change$travel_time_transit_baseline, na.rm = TRUE)))
message(sprintf("  - Mean extension: %.1f min",
                mean(travel_time_change$travel_time_transit_extension, na.rm = TRUE)))
message(sprintf("  - Mean change: %.2f min",
                mean(travel_time_change$travel_time_change, na.rm = TRUE)))

message("\nCAR travel time summary:")
message(sprintf("  - Mean: %.1f min",
                mean(travel_time_change$travel_time_car, na.rm = TRUE)))
message(sprintf("  - Car valid rows: %s",
                format(sum(!is.na(travel_time_change$travel_time_car)), big.mark = ",")))

message("\nMIN(transit, car) travel time summary:")
message(sprintf("  - Mean baseline: %.1f min",
                mean(travel_time_change$travel_time_min_baseline, na.rm = TRUE)))

# Count improved pairs (transit)
improved <- sum(travel_time_change$travel_time_change < 0, na.rm = TRUE)
total_valid <- sum(!is.na(travel_time_change$travel_time_change))
message(sprintf("\n  - OD pairs with reduced transit time: %s (%.1f%%)",
                format(improved, big.mark = ","),
                100 * improved / total_valid))

# =============================================================================
# 5. SAVE OUTPUTS
# =============================================================================

message("\n[7/7] Saving outputs...")

# Rename columns to match project conventions
# TRANSIT baseline
ttm_baseline_transit_out <- ttm_baseline_transit %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  )
names(ttm_baseline_transit_out)[grep("travel_time", names(ttm_baseline_transit_out))] <- "travel_time_min"

# TRANSIT extension
ttm_extension_transit_out <- ttm_extension_transit %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  )
names(ttm_extension_transit_out)[grep("travel_time", names(ttm_extension_transit_out))] <- "travel_time_min"

# CAR baseline
ttm_baseline_car_out <- ttm_baseline_car %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  )
names(ttm_baseline_car_out)[grep("travel_time", names(ttm_baseline_car_out))] <- "travel_time_min"

# Combined output with transit, car, and min
travel_time_all <- travel_time_change %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  ) %>%
  select(origin_bg, dest_bg,
         travel_time_transit_baseline, travel_time_transit_extension,
         travel_time_car,
         travel_time_min_baseline, travel_time_min_extension,
         travel_time_change, travel_time_pct_change)

# Save as CSV - TRANSIT (keep original names for backwards compatibility)
write_csv(ttm_baseline_transit_out, "../output/travel_time_matrix_baseline_bg.csv")
message("Saved: ../output/travel_time_matrix_baseline_bg.csv (transit)")

write_csv(ttm_extension_transit_out, "../output/travel_time_matrix_extension_bg.csv")
message("Saved: ../output/travel_time_matrix_extension_bg.csv (transit)")

# Save CAR travel times
write_csv(ttm_baseline_car_out, "../output/travel_time_matrix_car_baseline_bg.csv")
message("Saved: ../output/travel_time_matrix_car_baseline_bg.csv")

# Save combined file with all travel time measures
write_csv(travel_time_all, "../output/travel_time_all_modes_bg.csv")
message("Saved: ../output/travel_time_all_modes_bg.csv")

# Save transit change file (backwards compatibility)
travel_time_change_out <- travel_time_change %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  ) %>%
  select(origin_bg, dest_bg,
         travel_time_baseline = travel_time_transit_baseline,
         travel_time_extension = travel_time_transit_extension,
         travel_time_change, travel_time_pct_change)

write_csv(travel_time_change_out, "../output/travel_time_change_bg.csv")
message("Saved: ../output/travel_time_change_bg.csv")

# Save summary statistics
summary_stats <- tibble(
  metric = c(
    "n_block_groups",
    "n_od_pairs_transit",
    "n_od_pairs_car",
    "mean_transit_baseline_min",
    "mean_transit_extension_min",
    "mean_car_baseline_min",
    "mean_min_baseline_min",
    "mean_transit_change_min",
    "n_improved_pairs",
    "pct_improved_pairs"
  ),
  value = c(
    n_bgs,
    nrow(ttm_baseline_transit),
    nrow(ttm_baseline_car),
    mean(travel_time_change$travel_time_transit_baseline, na.rm = TRUE),
    mean(travel_time_change$travel_time_transit_extension, na.rm = TRUE),
    mean(travel_time_change$travel_time_car, na.rm = TRUE),
    mean(travel_time_change$travel_time_min_baseline, na.rm = TRUE),
    mean(travel_time_change$travel_time_change, na.rm = TRUE),
    improved,
    100 * improved / total_valid
  )
)

write_csv(summary_stats, "../output/travel_time_summary_bg.csv")
message("Saved: ../output/travel_time_summary_bg.csv")

message("\n=============================================================")
message("Block group travel time computation complete!")
message(sprintf("Block groups: %d", n_bgs))
message(sprintf("Transit OD pairs: %s", format(nrow(ttm_baseline_transit), big.mark = ",")))
message(sprintf("Car OD pairs: %s", format(nrow(ttm_baseline_car), big.mark = ",")))
message(sprintf("Mean transit time: %.1f min", mean(travel_time_change$travel_time_transit_baseline, na.rm = TRUE)))
message(sprintf("Mean car time: %.1f min", mean(travel_time_change$travel_time_car, na.rm = TRUE)))
message(sprintf("Transit improved pairs: %s (%.1f%%)", format(improved, big.mark = ","),
                100 * improved / total_valid))
message("=============================================================")
