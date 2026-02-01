SHELL := bash
include ../../shell_functions.make

all: ../output/gravity_estimates_bg.csv ../output/gravity_binscatter_bg.pdf

# Input symlinks
../input/lodes_od_bg_2019.csv: | ../input
	ln -sf ../../download_lodes_bg/output/lodes_od_bg_2019.csv $@

../input/travel_time_matrix_baseline_bg.csv: | ../input
	ln -sf ../../compute_travel_times_bg/output/travel_time_matrix_baseline_bg.csv $@

../input/block_group_centroids.csv: | ../input
	ln -sf ../../download_block_groups/output/block_group_centroids.csv $@

../input/cook_county_block_groups.gpkg: | ../input
	ln -sf ../../download_block_groups/output/cook_county_block_groups.gpkg $@

# Main estimation
../output/gravity_estimates_bg.csv ../output/gravity_binscatter_bg.pdf ../output/gravity_data_bg.csv ../output/gravity_models_bg.rds: estimate_gravity_bg.R ../input/lodes_od_bg_2019.csv ../input/travel_time_matrix_baseline_bg.csv ../input/block_group_centroids.csv | ../output
	$(R) estimate_gravity_bg.R

include ../../generic.make
