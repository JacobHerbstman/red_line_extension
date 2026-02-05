SHELL := bash

.PHONY: all tasks paper

all: paper

tasks:
	$(MAKE) -C tasks/setup_environment/code all
	$(MAKE) -C tasks/download_census_tracts/code all
	$(MAKE) -C tasks/download_network_data/code all
	$(MAKE) -C tasks/download_lodes/code all
	$(MAKE) -C tasks/download_residential_floorspace/code all
	$(MAKE) -C tasks/download_commercial_floorspace/code all
	$(MAKE) -C tasks/create_floorspace_data/code all
	$(MAKE) -C tasks/map_floorspace_shares/code all
	$(MAKE) -C tasks/compute_travel_costs/code all
	$(MAKE) -C tasks/create_redline_extension_gtfs/code all
	$(MAKE) -C tasks/compute_travel_times/code all
	$(MAKE) -C tasks/estimate_gravity_travel_time/code all
	$(MAKE) -C tasks/invert_model/code all
	$(MAKE) -C tasks/redline_counterfactual/code all

paper: tasks
	$(MAKE) -C paper all
