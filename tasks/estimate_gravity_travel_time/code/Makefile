SHELL := bash
include ../../shell_functions.make

all: ../output/gravity_estimates_travel_time.csv ../output/diagnostic_plots_travel_time.pdf ../output/gravity_binscatter_residualized.pdf ../output/nu_time_per_min.csv ../output/gravity_destination_fe.csv

# Step 1: Build commuting matrix with travel times
../output/commuting_matrix_travel_time.csv ../output/commuting_summary_travel_time.txt: build_commuting_matrix_travel_time.R ../input/lodes_od_cook_county_2019.csv ../input/travel_time_matrix_baseline.csv ../input/chicago_tracts_2010.csv | ../output
	$(R) build_commuting_matrix_travel_time.R

# Step 2: Estimate gravity equations
../output/gravity_estimates_travel_time.csv ../output/diagnostic_plots_travel_time.pdf ../output/gravity_binscatter_residualized.pdf ../output/nu_time_per_min.csv ../output/gravity_destination_fe.csv: estimate_gravity_travel_time.R ../output/commuting_matrix_travel_time.csv ../input/chicago_tracts_2010.csv ../input/tract_floorspace.csv | ../output
	$(R) estimate_gravity_travel_time.R

# Input symlinks
../input/lodes_od_cook_county_2019.csv: | ../input
	ln -sf ../../download_lodes/output/lodes_od_cook_county_2019.csv $@

../input/travel_time_matrix_baseline.csv: | ../input
	ln -sf ../../compute_travel_times/output/travel_time_matrix_baseline.csv $@

../input/chicago_tracts_2010.csv: | ../input
	ln -sf ../../download_census_tracts/output/chicago_tracts_2010.csv $@

../input/tract_floorspace.csv: | ../input
	ln -sf ../../map_floorspace_shares/output/tract_floorspace_for_model.csv $@

include ../../generic.make
