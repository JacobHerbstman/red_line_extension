SHELL := bash
include ../../shell_functions.make

all: ../output/chicago_tracts_2010.gpkg ../output/chicago_tracts_2010.csv

../output/chicago_tracts_2010.gpkg ../output/chicago_tracts_2010.csv ../output/cook_county_tracts_2010.gpkg ../output/cook_county_tracts_2010.csv ../output/chicago_boundary.gpkg: download_census_tracts.R | ../output
	$(R) download_census_tracts.R

include ../../generic.make
