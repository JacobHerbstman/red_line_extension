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
MODE <- c("WALK", "TRANSIT")
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
message(sprintf("  - Mode: %s", paste(MODE, collapse = " + ")))
message(sprintf("  - Max walk time: %d minutes", MAX_WALK_TIME))
message(sprintf("  - Max trip duration: %d minutes", MAX_TRIP_DURATION))

# =============================================================================
# 1. LOAD BLOCK GROUP CENTROIDS
# =============================================================================

message("\n[1/5] Loading block group centroids...")

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

message("\n[2/5] Computing BASELINE travel times (current network)...")

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

ttm_baseline <- travel_time_matrix(
  r5r_network = r5r_baseline,
  origins = points,
  destinations = points,
  mode = MODE,
  departure_datetime = departure_datetime,
  max_walk_time = MAX_WALK_TIME,
  max_trip_duration = MAX_TRIP_DURATION,
  time_window = TIME_WINDOW,
  percentiles = PERCENTILES,
  verbose = TRUE
)

toc()

# Clean up to free memory
r5r::stop_r5(r5r_baseline)
rm(r5r_baseline)
rJava::.jgc(R.gc = TRUE)
gc()

message(sprintf("Baseline matrix: %s rows", format(nrow(ttm_baseline), big.mark = ",")))

# =============================================================================
# 3. COMPUTE EXTENSION TRAVEL TIMES
# =============================================================================

message("\n[3/5] Computing EXTENSION travel times (with Red Line Extension)...")

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

ttm_extension <- travel_time_matrix(
  r5r_network = r5r_extension,
  origins = points,
  destinations = points,
  mode = MODE,
  departure_datetime = departure_datetime,
  max_walk_time = MAX_WALK_TIME,
  max_trip_duration = MAX_TRIP_DURATION,
  time_window = TIME_WINDOW,
  percentiles = PERCENTILES,
  verbose = TRUE
)

toc()

# Clean up
r5r::stop_r5(r5r_extension)
rm(r5r_extension)
rJava::.jgc(R.gc = TRUE)
gc()

message(sprintf("Extension matrix: %s rows", format(nrow(ttm_extension), big.mark = ",")))

# =============================================================================
# 4. COMPUTE TRAVEL TIME CHANGES
# =============================================================================

message("\n[4/5] Computing travel time changes...")

# Handle different column name possibilities
baseline_time_col <- names(ttm_baseline)[grep("travel_time", names(ttm_baseline))][1]
extension_time_col <- names(ttm_extension)[grep("travel_time", names(ttm_extension))][1]

if (is.na(baseline_time_col)) baseline_time_col <- "travel_time"
if (is.na(extension_time_col)) extension_time_col <- "travel_time"

# Rename for merging
ttm_baseline_rename <- ttm_baseline %>%
  rename(travel_time_baseline = !!sym(baseline_time_col))

ttm_extension_rename <- ttm_extension %>%
  rename(travel_time_extension = !!sym(extension_time_col))

# Merge and compute changes
travel_time_change <- ttm_baseline_rename %>%
  left_join(
    ttm_extension_rename %>% select(from_id, to_id, travel_time_extension),
    by = c("from_id", "to_id")
  ) %>%
  mutate(
    travel_time_change = travel_time_extension - travel_time_baseline,
    travel_time_pct_change = if_else(
      travel_time_baseline > 0,
      (travel_time_change / travel_time_baseline) * 100,
      NA_real_
    )
  )

# Summary statistics
message("\nTravel time change summary:")
message(sprintf("  - Mean baseline travel time: %.1f min",
                mean(travel_time_change$travel_time_baseline, na.rm = TRUE)))
message(sprintf("  - Mean extension travel time: %.1f min",
                mean(travel_time_change$travel_time_extension, na.rm = TRUE)))
message(sprintf("  - Mean change: %.2f min",
                mean(travel_time_change$travel_time_change, na.rm = TRUE)))

# Count improved pairs
improved <- sum(travel_time_change$travel_time_change < 0, na.rm = TRUE)
total_valid <- sum(!is.na(travel_time_change$travel_time_change))
message(sprintf("  - OD pairs with reduced travel time: %s (%.1f%%)",
                format(improved, big.mark = ","),
                100 * improved / total_valid))

# =============================================================================
# 5. SAVE OUTPUTS
# =============================================================================

message("\n[5/5] Saving outputs...")

# Rename columns to match project conventions
ttm_baseline_out <- ttm_baseline %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  )
names(ttm_baseline_out)[grep("travel_time", names(ttm_baseline_out))] <- "travel_time_min"

ttm_extension_out <- ttm_extension %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  )
names(ttm_extension_out)[grep("travel_time", names(ttm_extension_out))] <- "travel_time_min"

travel_time_change_out <- travel_time_change %>%
  rename(
    origin_bg = from_id,
    dest_bg = to_id
  ) %>%
  select(origin_bg, dest_bg, travel_time_baseline, travel_time_extension,
         travel_time_change, travel_time_pct_change)

# Save as CSV
write_csv(ttm_baseline_out, "../output/travel_time_matrix_baseline_bg.csv")
message("Saved: ../output/travel_time_matrix_baseline_bg.csv")

write_csv(ttm_extension_out, "../output/travel_time_matrix_extension_bg.csv")
message("Saved: ../output/travel_time_matrix_extension_bg.csv")

write_csv(travel_time_change_out, "../output/travel_time_change_bg.csv")
message("Saved: ../output/travel_time_change_bg.csv")

# Save summary statistics
summary_stats <- tibble(
  metric = c(
    "n_block_groups",
    "n_od_pairs",
    "mean_baseline_travel_time_min",
    "mean_extension_travel_time_min",
    "mean_travel_time_change_min",
    "n_improved_pairs",
    "pct_improved_pairs",
    "max_improvement_min"
  ),
  value = c(
    n_bgs,
    nrow(travel_time_change),
    mean(travel_time_change$travel_time_baseline, na.rm = TRUE),
    mean(travel_time_change$travel_time_extension, na.rm = TRUE),
    mean(travel_time_change$travel_time_change, na.rm = TRUE),
    improved,
    100 * improved / total_valid,
    abs(min(travel_time_change$travel_time_change, na.rm = TRUE))
  )
)

write_csv(summary_stats, "../output/travel_time_summary_bg.csv")
message("Saved: ../output/travel_time_summary_bg.csv")

message("\n=============================================================")
message("Block group travel time computation complete!")
message(sprintf("Block groups: %d", n_bgs))
message(sprintf("OD pairs: %s", format(nrow(travel_time_change_out), big.mark = ",")))
message(sprintf("Improved pairs: %s (%.1f%%)", format(improved, big.mark = ","),
                100 * improved / total_valid))
message("=============================================================")
