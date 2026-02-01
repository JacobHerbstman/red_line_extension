SHELL := bash
include ../../shell_functions.make

all: ../output/cta_gtfs_with_extension.zip

../output/cta_gtfs_with_extension.zip ../output/rle_stations.csv: create_redline_extension_gtfs.R ../input/cta_gtfs.zip | ../output
	$(R) create_redline_extension_gtfs.R

../input/cta_gtfs.zip: ../../download_network_data/output/cta_gtfs.zip | ../input
	ln -sf $< $@

include ../../generic.make
