SHELL := bash
include ../../shell_functions.make

all: ../output/lodes_od_cook_county_2019.csv ../output/lodes_rac_cook_county_2019.csv ../output/lodes_wac_cook_county_2019.csv

../output/lodes_od_cook_county_2019.csv ../output/lodes_rac_cook_county_2019.csv ../output/lodes_wac_cook_county_2019.csv ../output/lodes_od_illinois_full_2019.csv: download_lodes.R | ../output
	$(R) download_lodes.R

include ../../generic.make
