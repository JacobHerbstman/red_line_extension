SHELL := bash
include ../../shell_functions.make

all: ../output/cook_county_block_groups.gpkg ../output/block_group_centroids.csv

../output/cook_county_block_groups.gpkg ../output/block_group_centroids.csv: download_block_groups.R | ../output
	$(R) download_block_groups.R

include ../../generic.make
