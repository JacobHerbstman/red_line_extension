# download_network_data.R
# Download OpenStreetMap network and CTA GTFS feed for r5r routing
#
# Output:
# - chicago.osm.pbf (OpenStreetMap network data)
# - cta_gtfs.zip (CTA transit schedule)

source("../../setup_environment/code/packages.R")

message("=============================================================")
message("Downloading Network Data for r5r Travel Time Computation")
message("=============================================================")

# =============================================================================
# 1. DOWNLOAD OPENSTREETMAP DATA
# =============================================================================

message("\n[1/3] Downloading OpenStreetMap data for Chicago...")

# Define Chicago metro bounding box
# lat 41.6-42.1, lon -88.0 to -87.5
chicago_bbox <- c(xmin = -88.0, xmax = -87.5, ymin = 41.6, ymax = 42.1)

# Use osmextract to download Chicago area OSM data
# Try bbbike provider first (has Chicago-specific extract, ~100MB)
# Fall back to geofabrik Illinois if needed

tic("OSM download")

osm_result <- tryCatch({
  # bbbike has smaller city-specific extracts
  osm_file <- oe_get(
    place = "Chicago",
    provider = "bbbike",
    download_only = TRUE,
    skip_vectortranslate = TRUE,  # Keep as .osm.pbf format
    quiet = FALSE
  )
  osm_file
}, error = function(e) {
  message(sprintf("bbbike download failed: %s", e$message))
  message("Trying geofabrik Illinois extract...")

  # Fall back to geofabrik Illinois and clip to bbox
  osm_file <- oe_get(
    place = chicago_bbox,
    provider = "geofabrik",
    download_only = TRUE,
    skip_vectortranslate = TRUE,
    quiet = FALSE
  )
  osm_file
})

toc()

# Copy to output directory
message(sprintf("OSM file location: %s", osm_result))
file.copy(osm_result, "../output/chicago.osm.pbf", overwrite = TRUE)

osm_size <- file.size("../output/chicago.osm.pbf") / 1e6
message(sprintf("Saved: ../output/chicago.osm.pbf (%.1f MB)", osm_size))

# =============================================================================
# 2. DOWNLOAD CTA GTFS FEED
# =============================================================================

message("\n[2/3] Downloading CTA GTFS feed...")

gtfs_url <- "http://www.transitchicago.com/downloads/sch_data/google_transit.zip"

tic("GTFS download")

download.file(
  url = gtfs_url,
  destfile = "../output/cta_gtfs.zip",
  mode = "wb",
  quiet = FALSE
)

toc()

gtfs_size <- file.size("../output/cta_gtfs.zip") / 1e6
message(sprintf("Saved: ../output/cta_gtfs.zip (%.1f MB)", gtfs_size))

# =============================================================================
# 3. VALIDATE DOWNLOADS
# =============================================================================

message("\n[3/3] Validating downloads...")

# Check OSM file exists and has reasonable size
if (!file.exists("../output/chicago.osm.pbf")) {
  stop("ERROR: OSM file not found!")
}
if (osm_size < 10) {
  warning("WARNING: OSM file seems small - may be incomplete")
}

# Validate GTFS using tidytransit
gtfs <- tryCatch({
  read_gtfs("../output/cta_gtfs.zip")
}, error = function(e) {
  stop(sprintf("ERROR: GTFS validation failed: %s", e$message))
})

# Log GTFS summary
message("\nGTFS Summary:")
message(sprintf("  - Routes: %d", nrow(gtfs$routes)))
message(sprintf("  - Stops: %d", nrow(gtfs$stops)))
message(sprintf("  - Trips: %d", nrow(gtfs$trips)))
message(sprintf("  - Stop times: %s", format(nrow(gtfs$stop_times), big.mark = ",")))

# Check for Red Line
red_line <- gtfs$routes %>%
  filter(str_detect(route_long_name, "Red") | route_short_name == "Red")

if (nrow(red_line) == 0) {
  warning("WARNING: Red Line not found in GTFS - check route naming")
} else {
  message(sprintf("  - Red Line route_id: %s", red_line$route_id[1]))

  # Count Red Line trips
  red_trips <- gtfs$trips %>%
    filter(route_id == red_line$route_id[1])
  message(sprintf("  - Red Line trips: %d", nrow(red_trips)))
}

# Find 95th/Dan Ryan station (current Red Line terminus)
dan_ryan <- gtfs$stops %>%
  filter(str_detect(stop_name, "95th") & str_detect(stop_name, "Dan Ryan|Dan-Ryan"))

if (nrow(dan_ryan) > 0) {
  message(sprintf("  - 95th/Dan Ryan stop_id: %s", dan_ryan$stop_id[1]))
} else {
  # Try alternate search
  dan_ryan <- gtfs$stops %>%
    filter(str_detect(stop_name, "95th"))
  if (nrow(dan_ryan) > 0) {
    message(sprintf("  - 95th Street stops found: %d", nrow(dan_ryan)))
    message(sprintf("    Names: %s", paste(dan_ryan$stop_name, collapse = ", ")))
  }
}

message("\n=============================================================")
message("Network data download complete!")
message(sprintf("OSM file: %.1f MB", osm_size))
message(sprintf("GTFS file: %.1f MB", gtfs_size))
message("=============================================================")
