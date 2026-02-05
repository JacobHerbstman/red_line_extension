# Create CTA GTFS with Red Line Extension stations south of 95th

source("../../setup_environment/code/packages.R")

message("Creating Red Line Extension GTFS")

# New station specs from CTA documentation
new_stations <- tibble(
  stop_id = c("RLE_103", "RLE_111", "RLE_MICH", "RLE_130"),
  stop_name = c("103rd Street", "111th Street", "Michigan Avenue", "130th Street"),
  stop_lat = c(41.7067, 41.6925, 41.6850, 41.6585),
  stop_lon = c(-87.6247, -87.6247, -87.6170, -87.6090),
  station_order = 1:4
)

inter_station_times <- c(180, 180, 180, 240)  # seconds
cumulative_times <- cumsum(inter_station_times)
new_stations$time_from_95th <- cumulative_times

DWELL_TIME <- 30

message("New stations:")
print(new_stations)

# --- Read GTFS ---
message("Reading original CTA GTFS...")

gtfs <- read_gtfs("../input/cta_gtfs.zip")

message(sprintf("Original: %d routes, %d stops, %d trips, %s stop_times",
                nrow(gtfs$routes), nrow(gtfs$stops), nrow(gtfs$trips),
                format(nrow(gtfs$stop_times), big.mark = ",")))

# --- Find Red Line ---
message("Finding Red Line route...")

red_line_route <- gtfs$routes %>%
  filter(str_detect(route_long_name, regex("Red", ignore_case = TRUE)) |
         str_detect(route_short_name, regex("Red", ignore_case = TRUE)))

if (nrow(red_line_route) == 0) {
  stop("Could not find Red Line route")
}

red_line_route_id <- red_line_route$route_id[1]
message(sprintf("Red Line route_id: %s", red_line_route_id))

red_line_trips <- gtfs$trips %>%
  filter(route_id == red_line_route_id)

message(sprintf("Red Line trips: %d", nrow(red_line_trips)))

red_line_trip_ids <- red_line_trips$trip_id
red_line_stop_times <- gtfs$stop_times %>%
  filter(trip_id %in% red_line_trip_ids)
red_line_stop_ids <- unique(red_line_stop_times$stop_id)
red_line_stops <- gtfs$stops %>%
  filter(stop_id %in% red_line_stop_ids)

# Find 95th/Dan Ryan
dan_ryan_candidates <- red_line_stops %>%
  filter(str_detect(stop_name, regex("95", ignore_case = TRUE)) |
         str_detect(stop_name, regex("Dan.?Ryan", ignore_case = TRUE)))

if (nrow(dan_ryan_candidates) == 0) {
  dan_ryan_candidates <- red_line_stops %>%
    arrange(stop_lat) %>%
    head(2)
}

dan_ryan_stop_ids <- dan_ryan_candidates$stop_id
message(sprintf("95th/Dan Ryan platforms: %d", length(dan_ryan_stop_ids)))

red_line_at_95th <- gtfs$stop_times %>%
  filter(stop_id %in% dan_ryan_stop_ids) %>%
  filter(trip_id %in% red_line_trip_ids)

message(sprintf("Stop_times at 95th: %d", nrow(red_line_at_95th)))

# --- Add stops ---
message("Adding new stops...")

stop_cols <- names(gtfs$stops)

new_stops_gtfs <- new_stations %>%
  select(stop_id, stop_name, stop_lat, stop_lon) %>%
  mutate(
    stop_code = stop_id,
    stop_desc = paste("Red Line Extension -", stop_name),
    zone_id = NA_character_,
    stop_url = NA_character_,
    location_type = 0L,
    parent_station = NA_character_,
    stop_timezone = NA_character_,
    wheelchair_boarding = 1L
  )

for (col in stop_cols) {
  if (!col %in% names(new_stops_gtfs)) {
    new_stops_gtfs[[col]] <- NA
  }
}
new_stops_gtfs <- new_stops_gtfs[, stop_cols]

gtfs$stops <- bind_rows(gtfs$stops, new_stops_gtfs)

message(sprintf("Added %d stops. Total: %d", nrow(new_stops_gtfs), nrow(gtfs$stops)))

# --- Extend stop_times ---
message("Extending stop_times...")

trip_at_95th <- gtfs$stop_times %>%
  filter(stop_id %in% dan_ryan_stop_ids) %>%
  filter(trip_id %in% red_line_trip_ids) %>%
  group_by(trip_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    trip_id,
    dan_ryan_stop_id = stop_id,
    stop_sequence_at_95th = stop_sequence,
    arrival_time,
    departure_time
  )

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

message(sprintf("Southbound: %d, Northbound: %d", nrow(southbound_trips), nrow(northbound_trips)))

# Time helpers
add_seconds_to_gtfs_time <- function(time_str, seconds_to_add) {
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

subtract_seconds_from_gtfs_time <- function(time_str, seconds_to_sub) {
  parts <- str_split(time_str, ":", simplify = TRUE)
  hours <- as.integer(parts[1])
  mins <- as.integer(parts[2])
  secs <- as.integer(parts[3])

  total_secs <- hours * 3600 + mins * 60 + secs - seconds_to_sub
  if (total_secs < 0) total_secs <- 0

  new_hours <- total_secs %/% 3600
  new_mins <- (total_secs %% 3600) %/% 60
  new_secs <- total_secs %% 60

  sprintf("%02d:%02d:%02d", new_hours, new_mins, new_secs)
}

gtfs_time_to_seconds <- function(time_vec) {
  parts <- str_split_fixed(as.character(time_vec), ":", 3)
  suppressWarnings(
    as.numeric(parts[, 1]) * 3600 +
      as.numeric(parts[, 2]) * 60 +
      as.numeric(parts[, 3])
  )
}

# Southbound extensions
new_stop_times_south <- list()

if (nrow(southbound_trips) > 0) {
  for (i in 1:nrow(southbound_trips)) {
    trip <- southbound_trips[i, ]

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
message(sprintf("Southbound stop_times: %d", nrow(new_stop_times_southbound)))

# Northbound extensions
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

if (nrow(northbound_trips) > 0) {
  for (i in 1:nrow(northbound_trips)) {
    trip <- northbound_trips[i, ]

    for (j in nrow(new_stations):1) {
      station <- new_stations[j, ]

      arrival <- subtract_seconds_from_gtfs_time(trip$arrival_time, station$time_from_95th)
      departure <- add_seconds_to_gtfs_time(arrival, DWELL_TIME)

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
message(sprintf("Northbound stop_times: %d", nrow(new_stop_times_northbound)))

all_new_stop_times <- bind_rows(new_stop_times_southbound, new_stop_times_northbound)

stop_time_cols <- names(gtfs$stop_times)
for (col in stop_time_cols) {
  if (!col %in% names(all_new_stop_times)) {
    all_new_stop_times[[col]] <- NA
  }
}
all_new_stop_times <- all_new_stop_times[, stop_time_cols]

gtfs$stop_times <- gtfs$stop_times %>%
  mutate(
    arrival_time = as.character(arrival_time),
    departure_time = as.character(departure_time)
  )

gtfs$stop_times <- bind_rows(gtfs$stop_times, all_new_stop_times)

message(sprintf("Total stop_times added: %d", nrow(all_new_stop_times)))

# Validate monotonic times
modified_trip_ids <- unique(c(southbound_trips$trip_id, northbound_trips$trip_id))
violations <- gtfs$stop_times %>%
  filter(trip_id %in% modified_trip_ids) %>%
  mutate(arrival_seconds = gtfs_time_to_seconds(arrival_time)) %>%
  arrange(trip_id, stop_sequence) %>%
  group_by(trip_id) %>%
  summarize(has_decrease = any(diff(arrival_seconds) < 0, na.rm = TRUE), .groups = "drop") %>%
  filter(has_decrease)

if (nrow(violations) > 0) {
  stop(sprintf("%d modified trips have decreasing stop_times", nrow(violations)))
}

# --- Save ---
message("Saving modified GTFS...")

write_gtfs(gtfs, "../output/cta_gtfs_with_extension.zip")
write_csv(new_stations, "../output/rle_stations.csv")

# Validate
message("Validating...")
gtfs_check <- read_gtfs("../output/cta_gtfs_with_extension.zip")

new_stop_check <- gtfs_check$stops %>%
  filter(stop_id %in% new_stations$stop_id)
message(sprintf("New stops in output: %d", nrow(new_stop_check)))

new_stop_times_check <- gtfs_check$stop_times %>%
  filter(stop_id %in% new_stations$stop_id)
message(sprintf("Stop_times for new stations: %s", format(nrow(new_stop_times_check), big.mark = ",")))

message(sprintf("Red Line Extension GTFS created. %d stations, %d southbound, %d northbound trips extended.",
                nrow(new_stations), nrow(southbound_trips), nrow(northbound_trips)))
