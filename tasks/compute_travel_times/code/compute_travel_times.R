# Compute transit travel-time matrices with r5r for baseline and RLE scenarios

options(java.parameters = "-Xmx8G")

source("../../setup_environment/code/packages.R")

if (!require(r5r, quietly = TRUE)) {
  stop("r5r package not installed. Please install r5r and ensure Java 11+ is available.")
}

message("Computing travel time matrices with r5r")

# Routing parameters
# Date must be within GTFS calendar range
DEPARTURE_DATE <- "2026-01-07"
DEPARTURE_TIME <- "08:00:00"
MODE <- c("WALK", "TRANSIT")
MAX_WALK_TIME <- 30
MAX_TRIP_DURATION <- 90
TIME_WINDOW <- 30
PERCENTILES <- 50

departure_datetime <- as.POSIXct(
  paste(DEPARTURE_DATE, DEPARTURE_TIME),
  format = "%Y-%m-%d %H:%M:%S"
)

message(sprintf("Departure: %s, Mode: %s", departure_datetime, paste(MODE, collapse = " + ")))

# --- Load centroids ---
message("Loading tract centroids...")

centroids <- read_csv("../input/tract_centroids.csv", show_col_types = FALSE)

points <- centroids %>%
  rename(id = GEOID) %>%
  select(id, lon, lat)

n_tracts <- nrow(points)
message(sprintf("Loaded %d tracts, will compute %s OD pairs",
                n_tracts, format(n_tracts^2, big.mark = ",")))

# Helper to clear r5r cache
reset_r5_network_cache <- function(network_dir) {
  cache_files <- file.path(network_dir, c("network.dat", "network_settings.json"))
  unlink(cache_files[file.exists(cache_files)])
}

# --- Baseline travel times ---
message("Computing baseline travel times...")

baseline_dir <- "../temp/network_baseline"
dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)

file.copy("../input/chicago.osm.pbf",
          file.path(baseline_dir, "chicago.osm.pbf"),
          overwrite = TRUE)
file.copy("../input/cta_gtfs.zip",
          file.path(baseline_dir, "cta_gtfs.zip"),
          overwrite = TRUE)
reset_r5_network_cache(baseline_dir)

message("Building baseline network...")
tic("Baseline network build")
r5r_baseline <- build_network(data_path = baseline_dir, verbose = FALSE)
toc()

message("Computing baseline matrix...")
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
  verbose = FALSE
)

toc()

r5r::stop_r5(r5r_baseline)
rJava::.jgc(R.gc = TRUE)
gc()

message(sprintf("Baseline matrix: %s rows", format(nrow(ttm_baseline), big.mark = ",")))

# --- Extension travel times ---
message("Computing extension travel times (with RLE)...")

extension_dir <- "../temp/network_extension"
dir.create(extension_dir, recursive = TRUE, showWarnings = FALSE)

file.copy("../input/chicago.osm.pbf",
          file.path(extension_dir, "chicago.osm.pbf"),
          overwrite = TRUE)
file.copy("../input/cta_gtfs_with_extension.zip",
          file.path(extension_dir, "cta_gtfs.zip"),
          overwrite = TRUE)
reset_r5_network_cache(extension_dir)

message("Building extension network...")
tic("Extension network build")
r5r_extension <- build_network(data_path = extension_dir, verbose = FALSE)
toc()

message("Computing extension matrix...")
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
  verbose = FALSE
)

toc()

r5r::stop_r5(r5r_extension)
rJava::.jgc(R.gc = TRUE)
gc()

message(sprintf("Extension matrix: %s rows", format(nrow(ttm_extension), big.mark = ",")))

# --- Compute changes ---
message("Computing travel time changes...")

baseline_time_col <- names(ttm_baseline)[grep("travel_time", names(ttm_baseline))][1]
extension_time_col <- names(ttm_extension)[grep("travel_time", names(ttm_extension))][1]

if (is.na(baseline_time_col)) baseline_time_col <- "travel_time"
if (is.na(extension_time_col)) extension_time_col <- "travel_time"

ttm_baseline_rename <- ttm_baseline %>%
  rename(travel_time_baseline = !!sym(baseline_time_col))

ttm_extension_rename <- ttm_extension %>%
  rename(travel_time_extension = !!sym(extension_time_col))

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

message(sprintf("Mean baseline: %.1f min, Mean extension: %.1f min, Mean change: %.2f min",
                mean(travel_time_change$travel_time_baseline, na.rm = TRUE),
                mean(travel_time_change$travel_time_extension, na.rm = TRUE),
                mean(travel_time_change$travel_time_change, na.rm = TRUE)))

improved <- sum(travel_time_change$travel_time_change < 0, na.rm = TRUE)
total_valid <- sum(!is.na(travel_time_change$travel_time_change))
message(sprintf("Improved pairs: %s (%.1f%%)", format(improved, big.mark = ","),
                100 * improved / total_valid))

# Origin-level summary
tract_change_summary <- travel_time_change %>%
  group_by(from_id) %>%
  summarize(
    n_valid_pairs = sum(!is.na(travel_time_change)),
    mean_baseline_min = mean(travel_time_baseline, na.rm = TRUE),
    mean_extension_min = mean(travel_time_extension, na.rm = TRUE),
    mean_change_min = mean(travel_time_change, na.rm = TRUE),
    median_change_min = median(travel_time_change, na.rm = TRUE),
    p10_change_min = as.numeric(quantile(travel_time_change, 0.10, na.rm = TRUE)),
    p90_change_min = as.numeric(quantile(travel_time_change, 0.90, na.rm = TRUE)),
    pct_pairs_improved = 100 * mean(travel_time_change < 0, na.rm = TRUE),
    pct_pairs_worsened = 100 * mean(travel_time_change > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(origin_tract = from_id) %>%
  arrange(mean_change_min)

top_n <- 20
top_tract_changes <- bind_rows(
  tract_change_summary %>%
    filter(mean_change_min < 0) %>%
    slice_head(n = top_n) %>%
    mutate(rank_group = "most_improved", rank_within_group = row_number()),
  tract_change_summary %>%
    filter(mean_change_min > 0) %>%
    arrange(desc(mean_change_min)) %>%
    slice_head(n = top_n) %>%
    mutate(rank_group = "most_worsened", rank_within_group = row_number())
) %>%
  select(rank_group, rank_within_group, everything())

top_improvements <- travel_time_change %>%
  filter(!is.na(travel_time_change)) %>%
  arrange(travel_time_change) %>%
  head(10)

message("Top 10 travel time reductions:")
for (i in 1:nrow(top_improvements)) {
  row <- top_improvements[i, ]
  message(sprintf("  %s -> %s: %.1f min", row$from_id, row$to_id, abs(row$travel_time_change)))
}

# --- Save ---
message("Saving outputs...")

ttm_baseline_out <- ttm_baseline %>%
  rename(origin_tract = from_id, dest_tract = to_id)
names(ttm_baseline_out)[grep("travel_time", names(ttm_baseline_out))] <- "travel_time_min"

ttm_extension_out <- ttm_extension %>%
  rename(origin_tract = from_id, dest_tract = to_id)
names(ttm_extension_out)[grep("travel_time", names(ttm_extension_out))] <- "travel_time_min"

travel_time_change_out <- travel_time_change %>%
  rename(origin_tract = from_id, dest_tract = to_id) %>%
  select(origin_tract, dest_tract, travel_time_baseline, travel_time_extension,
         travel_time_change, travel_time_pct_change)

write_csv(ttm_baseline_out, "../output/travel_time_matrix_baseline.csv")
write_csv(ttm_extension_out, "../output/travel_time_matrix_extension.csv")
write_csv(travel_time_change_out, "../output/travel_time_change.csv")
write_csv(tract_change_summary, "../output/tract_commute_change_summary.csv")
write_csv(top_tract_changes, "../output/top_tract_commute_changes.csv")

summary_stats <- tibble(
  metric = c(
    "n_tracts", "n_od_pairs",
    "mean_baseline_travel_time_min", "mean_extension_travel_time_min",
    "mean_travel_time_change_min", "n_improved_pairs",
    "pct_improved_pairs", "max_improvement_min"
  ),
  value = c(
    n_tracts, nrow(travel_time_change),
    mean(travel_time_change$travel_time_baseline, na.rm = TRUE),
    mean(travel_time_change$travel_time_extension, na.rm = TRUE),
    mean(travel_time_change$travel_time_change, na.rm = TRUE),
    improved, 100 * improved / total_valid,
    abs(min(travel_time_change$travel_time_change, na.rm = TRUE))
  )
)

write_csv(summary_stats, "../output/travel_time_summary.csv")

message(sprintf("Travel time computation complete. Improved pairs: %s (%.1f%%)",
                format(improved, big.mark = ","), 100 * improved / total_valid))
