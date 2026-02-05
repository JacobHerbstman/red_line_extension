# Compute Euclidean distance matrix between tract centroids

source("../../setup_environment/code/packages.R")

CRS_PROJECTED <- 3435  # Illinois State Plane East

message("Computing Euclidean distance matrix")

# --- Load tracts ---
message("Loading tract data...")

tracts <- st_read("../input/chicago_tracts_2010.gpkg", quiet = TRUE)
message(sprintf("Loaded %s tracts", nrow(tracts)))

tracts_proj <- st_transform(tracts, CRS_PROJECTED)
centroids_proj <- st_centroid(tracts_proj)

centroids_wgs84 <- st_transform(centroids_proj, 4326)
centroid_coords <- st_coordinates(centroids_wgs84)

centroids_df <- data.frame(
  GEOID = tracts$GEOID,
  lon = centroid_coords[, 1],
  lat = centroid_coords[, 2]
)

message(sprintf("Computed centroids for %s tracts", nrow(centroids_df)))

# --- Distance matrix ---
message("Computing distance matrix...")

tic("Distance matrix")

coords_proj <- st_coordinates(centroids_proj)
n_tracts <- nrow(coords_proj)
message(sprintf("Computing %s x %s pairwise distances...", n_tracts, n_tracts))

# Convert from feet to km
dist_matrix_km <- as.matrix(dist(coords_proj)) / 3280.84

rownames(dist_matrix_km) <- tracts$GEOID
colnames(dist_matrix_km) <- tracts$GEOID

dist_long <- expand.grid(
  origin_tract = tracts$GEOID,
  dest_tract = tracts$GEOID,
  stringsAsFactors = FALSE
) %>%
  mutate(
    distance_km = as.vector(dist_matrix_km)
  )

toc()

nonzero_dist <- dist_long$distance_km[dist_long$distance_km > 0]
message(sprintf("Min distance: %.2f km, Max: %.2f km, Mean: %.2f km",
                min(nonzero_dist), max(dist_long$distance_km), mean(dist_long$distance_km)))

# --- Save ---
message("Saving outputs...")

write_csv(dist_long, "../output/distance_matrix_km.csv")
saveRDS(dist_matrix_km, "../output/distance_matrix_km.rds")
write_csv(data.frame(idx = 1:n_tracts, GEOID = tracts$GEOID), "../output/tract_index.csv")
write_csv(centroids_df, "../output/tract_centroids.csv")

message(sprintf("Distance matrix complete: %s x %s", n_tracts, n_tracts))
