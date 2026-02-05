# Download OSM network and CTA GTFS for r5r routing

source("../../setup_environment/code/packages.R")

message("Downloading network data for r5r")

# --- OSM data ---
message("Downloading OpenStreetMap data...")

chicago_bbox <- c(xmin = -88.0, xmax = -87.5, ymin = 41.6, ymax = 42.1)

tic("OSM download")

osm_result <- tryCatch({
  osm_file <- oe_get(
    place = "Chicago",
    provider = "bbbike",
    download_only = TRUE,
    skip_vectortranslate = TRUE,
    quiet = FALSE
  )
  osm_file
}, error = function(e) {
  message(sprintf("bbbike download failed: %s", e$message))
  message("Trying geofabrik Illinois extract...")

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

file.copy(osm_result, "../output/chicago.osm.pbf", overwrite = TRUE)
osm_size <- file.size("../output/chicago.osm.pbf") / 1e6
message(sprintf("Saved: chicago.osm.pbf (%.1f MB)", osm_size))

# --- GTFS ---
message("Downloading CTA GTFS feed...")

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
message(sprintf("Saved: cta_gtfs.zip (%.1f MB)", gtfs_size))

# --- Validation ---
message("Validating downloads...")

if (!file.exists("../output/chicago.osm.pbf")) {
  stop("OSM file not found")
}
if (osm_size < 10) {
  warning("OSM file seems small - may be incomplete")
}

gtfs <- tryCatch({
  read_gtfs("../output/cta_gtfs.zip")
}, error = function(e) {
  stop(sprintf("GTFS validation failed: %s", e$message))
})

message(sprintf("GTFS: %d routes, %d stops, %d trips",
                nrow(gtfs$routes), nrow(gtfs$stops), nrow(gtfs$trips)))

red_line <- gtfs$routes %>%
  filter(str_detect(route_long_name, "Red") | route_short_name == "Red")

if (nrow(red_line) > 0) {
  message(sprintf("Red Line route_id: %s", red_line$route_id[1]))
  red_trips <- gtfs$trips %>% filter(route_id == red_line$route_id[1])
  message(sprintf("Red Line trips: %d", nrow(red_trips)))
}

dan_ryan <- gtfs$stops %>%
  filter(str_detect(stop_name, "95th") & str_detect(stop_name, "Dan Ryan|Dan-Ryan"))

if (nrow(dan_ryan) > 0) {
  message(sprintf("95th/Dan Ryan stop_id: %s", dan_ryan$stop_id[1]))
} else {
  dan_ryan <- gtfs$stops %>% filter(str_detect(stop_name, "95th"))
  if (nrow(dan_ryan) > 0) {
    message(sprintf("95th Street stops found: %d", nrow(dan_ryan)))
  }
}

message("Network data download complete")
