# compute_travel_costs.R
# Compute Euclidean distance matrix between Chicago tract centroids
#
# Output:
# - distance_matrix_km.csv (long format)
# - distance_matrix_km.rds (matrix format for fast loading)
# - tract_centroids.csv
# - tract_index.csv

source("../../setup_environment/code/packages.R")

# =============================================================================
# PARAMETERS
# =============================================================================

# CRS for distance calculations (Illinois State Plane East, feet)
CRS_PROJECTED <- 3435

message("=============================================================")
message("Computing Euclidean Distance Matrix Between Tract Centroids")
message("=============================================================")

# =============================================================================
# 1. LOAD TRACT DATA
# =============================================================================

message("\n[1/3] Loading tract data...")

tracts <- st_read("../input/chicago_tracts_2010.gpkg", quiet = TRUE)
message(sprintf("Loaded %s tracts", nrow(tracts)))

# Compute centroids in projected CRS for accurate distance calculation
tracts_proj <- st_transform(tracts, CRS_PROJECTED)
centroids_proj <- st_centroid(tracts_proj)

# Also get centroids in WGS84
centroids_wgs84 <- st_transform(centroids_proj, 4326)
centroid_coords <- st_coordinates(centroids_wgs84)

# Create centroid dataframe
centroids_df <- data.frame(
  GEOID = tracts$GEOID,
  lon = centroid_coords[, 1],
  lat = centroid_coords[, 2]
)

message(sprintf("Computed centroids for %s tracts", nrow(centroids_df)))

# =============================================================================
# 2. COMPUTE EUCLIDEAN DISTANCE MATRIX
# =============================================================================

message("\n[2/3] Computing Euclidean distance matrix...")

tic("Distance matrix")

# Get centroid coordinates in projected CRS (feet)
coords_proj <- st_coordinates(centroids_proj)

# Compute pairwise distances
n_tracts <- nrow(coords_proj)
message(sprintf("Computing %s x %s = %s pairwise distances...", 
                n_tracts, n_tracts, format(n_tracts^2, big.mark = ",")))

# Distance matrix in km (convert from feet)
dist_matrix_km <- as.matrix(dist(coords_proj)) / 3280.84

# Add row/column names
rownames(dist_matrix_km) <- tracts$GEOID
colnames(dist_matrix_km) <- tracts$GEOID

# Convert to long format for storage
dist_long <- expand.grid(
  origin_tract = tracts$GEOID,
  dest_tract = tracts$GEOID,
  stringsAsFactors = FALSE
) %>%
  mutate(
    distance_km = as.vector(dist_matrix_km)
  )

toc()

# Summary stats
message("\nDistance matrix summary:")
nonzero_dist <- dist_long$distance_km[dist_long$distance_km > 0]
message(sprintf("  - Total tract pairs: %s", format(nrow(dist_long), big.mark = ",")))
message(sprintf("  - Min distance (non-zero): %.2f km", min(nonzero_dist)))
message(sprintf("  - Max distance: %.2f km", max(dist_long$distance_km)))
message(sprintf("  - Mean distance: %.2f km", mean(dist_long$distance_km)))
message(sprintf("  - Median distance: %.2f km", median(dist_long$distance_km)))

# =============================================================================
# 3. SAVE OUTPUTS
# =============================================================================

message("\n[3/3] Saving outputs...")

# Save distance matrix (long format)
write_csv(dist_long, "../output/distance_matrix_km.csv")
message("Saved: ../output/distance_matrix_km.csv")

# Save as RDS for faster loading in downstream tasks
saveRDS(dist_matrix_km, "../output/distance_matrix_km.rds")
message("Saved: ../output/distance_matrix_km.rds (matrix format)")

# Save tract IDs for matrix indexing
write_csv(data.frame(idx = 1:n_tracts, GEOID = tracts$GEOID), 
          "../output/tract_index.csv")
message("Saved: ../output/tract_index.csv")

# Save centroids
write_csv(centroids_df, "../output/tract_centroids.csv")
message("Saved: ../output/tract_centroids.csv")

message("\n=============================================================")
message("Distance matrix computation complete!")
message(sprintf("Matrix dimensions: %s x %s", n_tracts, n_tracts))
message("=============================================================")
