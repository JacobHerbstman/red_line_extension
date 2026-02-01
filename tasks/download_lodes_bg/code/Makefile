SHELL := bash
include ../../shell_functions.make

all: ../output/lodes_od_bg_2019.csv ../output/lodes_rac_bg_2019.csv ../output/lodes_wac_bg_2019.csv

../output/lodes_od_bg_2019.csv ../output/lodes_rac_bg_2019.csv ../output/lodes_wac_bg_2019.csv: download_lodes_bg.R | ../output
	$(R) download_lodes_bg.R

include ../../generic.make
