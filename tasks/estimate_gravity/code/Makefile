SHELL := bash
include ../../shell_functions.make

all: ../output/gravity_estimates.csv ../output/diagnostic_plots.pdf

# Step 1: Build commuting matrix
../output/commuting_matrix.csv ../output/commuting_summary.txt: build_commuting_matrix.R ../input/lodes_od_cook_county_2019.csv ../input/distance_matrix_km.csv ../input/chicago_tracts_2010.csv | ../output
	$(R) build_commuting_matrix.R

# Step 2: Estimate gravity equations
../output/gravity_estimates.csv ../output/diagnostic_plots.pdf: estimate_gravity.R ../output/commuting_matrix.csv ../input/chicago_tracts_2010.csv | ../output
	$(R) estimate_gravity.R

# Input dependencies (symlinks to upstream outputs)
../input/lodes_od_cook_county_2019.csv: ../../download_lodes/output/lodes_od_cook_county_2019.csv | ../input
	ln -sf $< $@

../input/distance_matrix_km.csv: ../../compute_travel_costs/output/distance_matrix_km.csv | ../input
	ln -sf $< $@

../input/chicago_tracts_2010.csv: ../../download_census_tracts/output/chicago_tracts_2010.csv | ../input
	ln -sf $< $@

include ../../generic.make
