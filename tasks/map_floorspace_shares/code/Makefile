SHELL := bash
include ../../shell_functions.make

all: ../output/tract_floorspace_for_model.csv ../output/residential_share_map.pdf ../output/commercial_share_map.pdf ../output/log_commercial_sqft_map.pdf ../output/total_floorspace_density_map.pdf

../output/tract_floorspace_for_model.csv ../output/residential_share_map.pdf ../output/commercial_share_map.pdf ../output/log_commercial_sqft_map.pdf ../output/total_floorspace_density_map.pdf: map_floorspace_shares.R ../input/floorspace_by_tract.csv ../input/chicago_tracts_2010.gpkg | ../output
	$(R) map_floorspace_shares.R

# Symlinks to inputs
../input/floorspace_by_tract.csv: ../../create_floorspace_data/output/floorspace_by_tract.csv | ../input
	ln -sf $< $@

../input/chicago_tracts_2010.gpkg: ../../download_census_tracts/output/chicago_tracts_2010.gpkg | ../input
	ln -sf $< $@

include ../../generic.make
