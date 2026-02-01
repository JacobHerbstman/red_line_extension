# create_redline_extension_gtfs.R
# Create a modified CTA GTFS with Red Line Extension stations
#
# Adds 4 new stations extending Red Line south from 95th/Dan Ryan:
# - 103rd Street
# - 111th Street
# - Michigan Avenue (116th)
# - 130th Street
#
# Output:
# - cta_gtfs_with_extension.zip (modified GTFS)
# - rle_stations.csv (metadata about new stations)

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Creating Red Line Extension GTFS")
message("=============================================================")

# =============================================================================
# PARAMETERS: NEW STATION SPECIFICATIONS
# =============================================================================

# Red Line Extension station specifications
# Coordinates from CTA project documentation
new_stations <- tibble(
  stop_id = c("RLE_103", "RLE_111", "RLE_MICH", "RLE_130"),
  stop_name = c("103rd Street", "111th Street", "Michigan Avenue", "130th Street"),
  stop_lat = c(41.7067, 41.6925, 41.6850, 41.6585),
  stop_lon = c(-87.6247, -87.6247, -87.6170, -87.6090),
  station_order = 1:4  # Order from 95th southward
)

# Travel times between stations (in seconds)
# Based on typical CTA elevated train speeds (~2-3 min between stations)
inter_station_times <- c(
  180,  # 95th -> 103rd: 3 minutes
  180,  # 103rd -> 111th: 3 minutes
  180,  # 111th -> Michigan: 3 minutes
  240   # Michigan -> 130th: 4 minutes (longer distance)
)

# Cumulative times from 95th/Dan Ryan
cumulative_times <- cumsum(inter_station_times)
new_stations$time_from_95th <- cumulative_times

# Dwell time at each station (seconds)
DWELL_TIME <- 30

message("\nNew stations to add:")
print(new_stations)

# =============================================================================
# 1. READ ORIGINAL GTFS
# =============================================================================

message("\n[1/5] Reading original CTA GTFS...")

gtfs <- read_gtfs("../input/cta_gtfs.zip")

message(sprintf("Original GTFS has:"))
message(sprintf("  - %d routes", nrow(gtfs$routes)))
message(sprintf("  - %d stops", nrow(gtfs$stops)))
message(sprintf("  - %d trips", nrow(gtfs$trips)))
message(sprintf("  - %s stop_times", format(nrow(gtfs$stop_times), big.mark = ",")))

# =============================================================================
# 2. IDENTIFY RED LINE ROUTE AND TERMINUS
# =============================================================================

message("\n[2/5] Identifying Red Line route and terminus...")

# Find Red Line route
red_line_route <- gtfs$routes %>%
  filter(str_detect(route_long_name, regex("Red", ignore_case = TRUE)) |
         str_detect(route_short_name, regex("Red", ignore_case = TRUE)))

if (nrow(red_line_route) == 0) {
  stop("ERROR: Could not find Red Line route in GTFS")
}

red_line_route_id <- red_line_route$route_id[1]
message(sprintf("Red Line route_id: %s", red_line_route_id))
message(sprintf("Red Line name: %s", red_line_route$route_long_name[1]))

# Find all Red Line trips
red_line_trips <- gtfs$trips %>%
  filter(route_id == red_line_route_id)

message(sprintf("Red Line trips: %d", nrow(red_line_trips)))

# Find 95th/Dan Ryan stop (current southern terminus)
# CTA uses parent_station for main station (40450) and child stops for platforms (30088, 30089)
# Strategy: Find all stops used by Red Line trips, then identify the southern terminus

# First, get all stops used by Red Line trips
red_line_trip_ids <- red_line_trips$trip_id

red_line_stop_times <- gtfs$stop_times %>%
  filter(trip_id %in% red_line_trip_ids)

red_line_stop_ids <- unique(red_line_stop_times$stop_id)
message(sprintf("Red Line uses %d unique stops", length(red_line_stop_ids)))

# Get info about these stops
red_line_stops <- gtfs$stops %>%
  filter(stop_id %in% red_line_stop_ids)

# Find 95th/Dan Ryan by looking for stops with "95th" in name that are used by Red Line
dan_ryan_candidates <- red_line_stops %>%
  filter(str_detect(stop_name, regex("95", ignore_case = TRUE)) |
         str_detect(stop_name, regex("Dan.?Ryan", ignore_case = TRUE)))

if (nrow(dan_ryan_candidates) == 0) {
  # Alternative: Find the southernmost Red Line stop (lowest latitude)
  dan_ryan_candidates <- red_line_stops %>%
    arrange(stop_lat) %>%
    head(2)  # Get southernmost stops (could be 2 platforms)
  message("Using geographic search for 95th terminus (southernmost stops)")
}

dan_ryan_stop_ids <- dan_ryan_candidates$stop_id

message(sprintf("95th/Dan Ryan platform stops found: %d", length(dan_ryan_stop_ids)))
message(sprintf("  Stop IDs: %s", paste(dan_ryan_stop_ids, collapse = ", ")))
message(sprintf("  Stop names: %s", paste(dan_ryan_candidates$stop_name, collapse = ", ")))

# Get stop times at 95th for Red Line trips
red_line_at_95th <- gtfs$stop_times %>%
  filter(stop_id %in% dan_ryan_stop_ids) %>%
  filter(trip_id %in% red_line_trip_ids)

message(sprintf("Red Line stop_times at 95th: %d", nrow(red_line_at_95th)))

# =============================================================================
# 3. ADD NEW STOPS TO stops.txt
# =============================================================================

message("\n[3/5] Adding new stops to stops.txt...")

# Prepare new stops in GTFS format
# Copy column structure from existing stops
stop_cols <- names(gtfs$stops)

new_stops_gtfs <- new_stations %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %>%
  mutate(
    stop_code = stop_id,
    stop_desc = paste("Red Line Extension -", stop_name),
    zone_id = NA_character_,
    stop_url = NA_character_,
    location_type = 0L,  # 0 = stop/platform, 1 = station
    parent_station = NA_character_,
    stop_timezone = NA_character_,
    wheelchair_boarding = 1L  # 1 = accessible
  )

# Ensure columns match existing stops table
for (col in stop_cols) {
  if (!col %in% names(new_stops_gtfs)) {
    new_stops_gtfs[[col]] <- NA
  }
}
new_stops_gtfs <- new_stops_gtfs[, stop_cols]

# Append new stops
gtfs$stops <- bind_rows(gtfs$stops, new_stops_gtfs)

message(sprintf("Added %d new stops. Total stops: %d",
                nrow(new_stops_gtfs), nrow(gtfs$stops)))

# =============================================================================
# 4. EXTEND STOP_TIMES FOR RED LINE TRIPS
# =============================================================================

message("\n[4/5] Extending stop_times for Red Line trips...")

# Strategy:
# - Find trips that end at 95th (southbound) - extend them south
# - Find trips that start at 95th (northbound) - add new stations at beginning

# Get max stop_sequence at 95th for each trip to identify direction
trip_at_95th <- gtfs$stop_times %>%
  filter(stop_id %in% dan_ryan_stop_ids) %>%
  filter(trip_id %in% red_line_trip_ids) %>%
  group_by(trip_id) %>%
  slice(1) %>%  # Take first row per trip (in case of duplicates)
  ungroup() %>%
  select(
    trip_id,
    dan_ryan_stop_id = stop_id,
    stop_sequence_at_95th = stop_sequence,
    arrival_time,
    departure_time
  )

# Get max stop_sequence per trip
max_sequences <- gtfs$stop_times %>%
  filter(trip_id %in% trip_at_95th$trip_id) %>%
  group_by(trip_id) %>%
  summarize(max_seq = max(stop_sequence), min_seq = min(stop_sequence), .groups = "drop")

trip_at_95th <- trip_at_95th %>%
  left_join(max_sequences, by = "trip_id") %>%
  mutate(
    is_southbound = stop_sequence_at_95th == max_seq,
    is_northbound = stop_sequence_at_95th == min_seq
  )

southbound_trips <- trip_at_95th %>% filter(is_southbound)
northbound_trips <- trip_at_95th %>% filter(is_northbound)

message(sprintf("Southbound trips (end at 95th): %d", nrow(southbound_trips)))
message(sprintf("Northbound trips (start at 95th): %d", nrow(northbound_trips)))

# Helper function to add seconds to GTFS time string
add_seconds_to_gtfs_time <- function(time_str, seconds_to_add) {
  # GTFS times are HH:MM:SS, can exceed 24:00:00 for overnight
  parts <- str_split(time_str, ":", simplify = TRUE)
  hours <- as.integer(parts[1])
  mins <- as.integer(parts[2])
  secs <- as.integer(parts[3])

  total_secs <- hours * 3600 + mins * 60 + secs + seconds_to_add

  new_hours <- total_secs %/% 3600
  new_mins <- (total_secs %% 3600) %/% 60
  new_secs <- total_secs %% 60

  sprintf("%02d:%02d:%02d", new_hours, new_mins, new_secs)
}

# Subtract seconds from GTFS time string
subtract_seconds_from_gtfs_time <- function(time_str, seconds_to_sub) {
  parts <- str_split(time_str, ":", simplify = TRUE)
  hours <- as.integer(parts[1])
  mins <- as.integer(parts[2])
  secs <- as.integer(parts[3])

  total_secs <- hours * 3600 + mins * 60 + secs - seconds_to_sub
  if (total_secs < 0) total_secs <- 0  # Safety

  new_hours <- total_secs %/% 3600
  new_mins <- (total_secs %% 3600) %/% 60
  new_secs <- total_secs %% 60

  sprintf("%02d:%02d:%02d", new_hours, new_mins, new_secs)
}

# ---- EXTEND SOUTHBOUND TRIPS ----
# Add stop_times for new stations after 95th

new_stop_times_south <- list()

if (nrow(southbound_trips) > 0) {
  for (i in 1:nrow(southbound_trips)) {
    trip <- southbound_trips[i, ]

    # Create new stop times for each extension station
    for (j in 1:nrow(new_stations)) {
      station <- new_stations[j, ]

      arrival <- add_seconds_to_gtfs_time(trip$departure_time, station$time_from_95th)
      departure <- add_seconds_to_gtfs_time(arrival, DWELL_TIME)

      new_st <- tibble(
        trip_id = trip$trip_id,
        arrival_time = arrival,
        departure_time = departure,
        stop_id = station$stop_id,
        stop_sequence = trip$stop_sequence_at_95th + station$station_order,
        stop_headsign = NA_character_,
        pickup_type = 0L,
        drop_off_type = 0L,
        shape_dist_traveled = NA_real_,
        timepoint = 1L
      )

      new_stop_times_south[[length(new_stop_times_south) + 1]] <- new_st
    }
  }
}

new_stop_times_southbound <- bind_rows(new_stop_times_south)
message(sprintf("Created %d new stop_times for southbound trips",
                nrow(new_stop_times_southbound)))

# ---- EXTEND NORTHBOUND TRIPS ----
# Add stop_times for new stations before 95th (with negative offsets)
# Also need to shift all existing stop_sequences by +4

# First, shift existing stop_sequences for northbound trips
if (nrow(northbound_trips) > 0) {
  gtfs$stop_times <- gtfs$stop_times %>%
    mutate(
      stop_sequence = if_else(
        trip_id %in% northbound_trips$trip_id,
        stop_sequence + 4L,
        stop_sequence
      )
    )
}

new_stop_times_north <- list()

# Total time from 130th to 95th
total_extension_time <- max(new_stations$time_from_95th)

if (nrow(northbound_trips) > 0) {
  for (i in 1:nrow(northbound_trips)) {
    trip <- northbound_trips[i, ]

    # Create new stop times for each extension station (in reverse order: 130th first)
    for (j in nrow(new_stations):1) {
      station <- new_stations[j, ]

      # Time before 95th = total_time - time_from_95th for this station
      time_before_95th <- total_extension_time - station$time_from_95th + inter_station_times[1]

      arrival <- subtract_seconds_from_gtfs_time(trip$arrival_time,
                                                  total_extension_time - station$time_from_95th + cumulative_times[1])
      departure <- add_seconds_to_gtfs_time(arrival, DWELL_TIME)

      # Stop sequence: 130th=1, Michigan=2, 111th=3, 103rd=4
      new_seq <- nrow(new_stations) - station$station_order + 1

      new_st <- tibble(
        trip_id = trip$trip_id,
        arrival_time = arrival,
        departure_time = departure,
        stop_id = station$stop_id,
        stop_sequence = as.integer(new_seq),
        stop_headsign = NA_character_,
        pickup_type = 0L,
        drop_off_type = 0L,
        shape_dist_traveled = NA_real_,
        timepoint = 1L
      )

      new_stop_times_north[[length(new_stop_times_north) + 1]] <- new_st
    }
  }
}

new_stop_times_northbound <- bind_rows(new_stop_times_north)
message(sprintf("Created %d new stop_times for northbound trips",
                nrow(new_stop_times_northbound)))

# Combine all new stop_times
all_new_stop_times <- bind_rows(new_stop_times_southbound, new_stop_times_northbound)

# Ensure columns match existing stop_times
stop_time_cols <- names(gtfs$stop_times)
for (col in stop_time_cols) {
  if (!col %in% names(all_new_stop_times)) {
    all_new_stop_times[[col]] <- NA
  }
}
all_new_stop_times <- all_new_stop_times[, stop_time_cols]

# Convert the existing gtfs stop_times to character for time columns
# This avoids issues with hms not handling times > 24:00:00
gtfs$stop_times <- gtfs$stop_times %>%
  mutate(
    arrival_time = as.character(arrival_time),
    departure_time = as.character(departure_time)
  )

# Append to existing stop_times (both are now character)
gtfs$stop_times <- bind_rows(gtfs$stop_times, all_new_stop_times)

message(sprintf("Total stop_times added: %d", nrow(all_new_stop_times)))
message(sprintf("New total stop_times: %s", format(nrow(gtfs$stop_times), big.mark = ",")))

# =============================================================================
# 5. SAVE MODIFIED GTFS AND METADATA
# =============================================================================

message("\n[5/5] Saving modified GTFS...")

# Write modified GTFS
write_gtfs(gtfs, "../output/cta_gtfs_with_extension.zip")
message("Saved: ../output/cta_gtfs_with_extension.zip")

# Save station metadata
write_csv(new_stations, "../output/rle_stations.csv")
message("Saved: ../output/rle_stations.csv")

# Validate the output GTFS
message("\nValidating modified GTFS...")
gtfs_check <- read_gtfs("../output/cta_gtfs_with_extension.zip")

# Check new stops are present
new_stop_check <- gtfs_check$stops %>%
  filter(stop_id %in% new_stations$stop_id)

message(sprintf("Verification: %d new stops found in output GTFS", nrow(new_stop_check)))

# Check stop_times for new stops
new_stop_times_check <- gtfs_check$stop_times %>%
  filter(stop_id %in% new_stations$stop_id)

message(sprintf("Verification: %s stop_times for new stations",
                format(nrow(new_stop_times_check), big.mark = ",")))

message("\n=============================================================")
message("Red Line Extension GTFS created successfully!")
message(sprintf("New stations added: %d", nrow(new_stations)))
message(sprintf("Southbound trips extended: %d", nrow(southbound_trips)))
message(sprintf("Northbound trips extended: %d", nrow(northbound_trips)))
message("=============================================================")
