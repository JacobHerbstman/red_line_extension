SHELL := bash
include ../../shell_functions.make

# Set JAVA_HOME for r5r (requires Java 21+)
# Homebrew on Apple Silicon installs to /opt/homebrew
export JAVA_HOME := /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home
export PATH := $(JAVA_HOME)/bin:$(PATH)

all: ../output/travel_time_matrix_baseline_bg.csv ../output/travel_time_matrix_extension_bg.csv ../output/travel_time_change_bg.csv ../output/travel_time_matrix_car_baseline_bg.csv ../output/travel_time_all_modes_bg.csv

# Input symlinks
../input/block_group_centroids.csv: | ../input
	ln -sf ../../download_block_groups/output/block_group_centroids.csv $@

../input/chicago.osm.pbf: | ../input
	ln -sf ../../download_network_data/output/chicago.osm.pbf $@

../input/cta_gtfs.zip: | ../input
	ln -sf ../../download_network_data/output/cta_gtfs.zip $@

../input/cta_gtfs_with_extension.zip: | ../input
	ln -sf ../../create_redline_extension_gtfs/output/cta_gtfs_with_extension.zip $@

# Main computation (outputs transit, car, and combined travel times)
../output/travel_time_matrix_baseline_bg.csv ../output/travel_time_matrix_extension_bg.csv ../output/travel_time_change_bg.csv ../output/travel_time_matrix_car_baseline_bg.csv ../output/travel_time_all_modes_bg.csv: compute_travel_times_bg.R ../input/block_group_centroids.csv ../input/chicago.osm.pbf ../input/cta_gtfs.zip ../input/cta_gtfs_with_extension.zip | ../output
	$(R) compute_travel_times_bg.R

include ../../generic.make
