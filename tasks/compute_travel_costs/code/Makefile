SHELL := bash
include ../../shell_functions.make

all: ../output/distance_matrix_km.csv

# Main outputs
../output/distance_matrix_km.csv ../output/distance_matrix_km.rds ../output/tract_centroids.csv ../output/tract_index.csv: compute_travel_costs.R ../input/chicago_tracts_2010.gpkg | ../output
	$(R) compute_travel_costs.R

# Input dependencies (symlinks to upstream outputs)
../input/chicago_tracts_2010.gpkg: ../../download_census_tracts/output/chicago_tracts_2010.gpkg | ../input
	ln -sf $< $@

include ../../generic.make
