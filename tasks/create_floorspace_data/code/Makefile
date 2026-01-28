SHELL := bash
include ../../shell_functions.make

PARCEL_FILE := ../../../data_raw/Assessor_-_Parcel_Universe_(Current_Year_Only)_20260128.csv

all: ../output/floorspace_by_pin.csv ../output/floorspace_by_tract.csv

../output/floorspace_by_pin.csv ../output/floorspace_by_tract.csv: create_floorspace_data.R ../input/parcel_universe.csv ../input/residential_floorspace_pins.csv ../input/commercial_floorspace_pins.csv ../input/multifamily_floorspace_pins.csv ../input/chicago_tracts_2010.csv ../input/chicago_tracts_2010.gpkg | ../output
	$(R) create_floorspace_data.R

# Symlinks to inputs
../input/parcel_universe.csv: | ../input
	ln -sf "$(PARCEL_FILE)" "$@"

../input/residential_floorspace_pins.csv: ../../download_residential_floorspace/output/residential_floorspace_pins.csv | ../input
	ln -sf $< $@

../input/commercial_floorspace_pins.csv: ../../download_commercial_floorspace/output/commercial_floorspace_pins.csv | ../input
	ln -sf $< $@

../input/multifamily_floorspace_pins.csv: ../../download_commercial_floorspace/output/multifamily_floorspace_pins.csv | ../input
	ln -sf $< $@

../input/chicago_tracts_2010.csv: ../../download_census_tracts/output/chicago_tracts_2010.csv | ../input
	ln -sf $< $@

../input/chicago_tracts_2010.gpkg: ../../download_census_tracts/output/chicago_tracts_2010.gpkg | ../input
	ln -sf $< $@

include ../../generic.make
