SHELL := bash
include ../../shell_functions.make

all: ../output/chicago.osm.pbf ../output/cta_gtfs.zip

../output/chicago.osm.pbf ../output/cta_gtfs.zip: download_network_data.R | ../output
	$(R) download_network_data.R

include ../../generic.make
