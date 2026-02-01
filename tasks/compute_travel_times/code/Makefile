SHELL := bash
include ../../shell_functions.make

all: ../output/travel_time_matrix_baseline.csv \
     ../output/travel_time_matrix_extension.csv \
     ../output/travel_time_change.csv \
     ../output/travel_time_summary.csv \
     ../output/tract_commute_change_summary.csv \
     ../output/top_tract_commute_changes.csv

../output/travel_time_matrix_baseline.csv ../output/travel_time_matrix_extension.csv ../output/travel_time_change.csv ../output/travel_time_summary.csv ../output/tract_commute_change_summary.csv ../output/top_tract_commute_changes.csv: compute_travel_times.R ../input/tract_centroids.csv ../input/chicago.osm.pbf ../input/cta_gtfs.zip ../input/cta_gtfs_with_extension.zip | ../output
	$(R) compute_travel_times.R

../input/tract_centroids.csv: ../../compute_travel_costs/output/tract_centroids.csv | ../input
	ln -sf $< $@

../input/chicago.osm.pbf: ../../download_network_data/output/chicago.osm.pbf | ../input
	ln -sf $< $@

../input/cta_gtfs.zip: ../../download_network_data/output/cta_gtfs.zip | ../input
	ln -sf $< $@

../input/cta_gtfs_with_extension.zip: ../../create_redline_extension_gtfs/output/cta_gtfs_with_extension.zip | ../input
	ln -sf $< $@

include ../../generic.make
