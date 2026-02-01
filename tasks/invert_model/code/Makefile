SHELL := bash
include ../../shell_functions.make

# Julia command
JULIA = julia

# Main outputs
MODEL_OUTPUTS := ../output/model_fundamentals.csv ../output/model_parameters.csv \
                 ../output/commuting_shares_model.csv
DIAGNOSTIC_OUTPUTS := ../output/diagnostic_maps.pdf ../output/diagnostic_fit.pdf \
                      ../output/diagnostic_distributions.pdf ../output/diagnostic_relationships.pdf \
                      ../output/diagnostic_land_market.pdf ../output/diagnostic_income_validation.pdf \
                      ../output/diagnostic_summary.txt
RLE_OUTPUTS := ../output/rle_stations.csv ../output/rle_affected_tracts.csv \
               ../output/rle_stations_map.pdf ../output/rle_stations_map_zoom.pdf

all: $(MODEL_OUTPUTS) $(DIAGNOSTIC_OUTPUTS) $(RLE_OUTPUTS)

# Model inversion (Julia)
$(MODEL_OUTPUTS): invert_model.jl ../input/commuting_matrix_travel_time.csv ../input/lodes_rac.csv ../input/lodes_wac.csv ../input/tract_floorspace.csv ../input/nu_time_per_min.csv ../input/gravity_destination_fe.csv | ../output
	$(JULIA) invert_model.jl

# Diagnostics (R) - depends on model outputs
$(DIAGNOSTIC_OUTPUTS): diagnostics.R $(MODEL_OUTPUTS) ../input/chicago_tracts.gpkg | ../output
	$(R) diagnostics.R

# RLE station identification (R)
$(RLE_OUTPUTS): identify_rle_stations.R ../input/chicago_tracts.gpkg | ../output
	$(R) identify_rle_stations.R

# Symlinks to inputs from other tasks
../input/commuting_matrix_travel_time.csv: ../../estimate_gravity_travel_time/output/commuting_matrix_travel_time.csv | ../input
	ln -sf $< $@

../input/lodes_rac.csv: ../../download_lodes/output/lodes_rac_cook_county_2019.csv | ../input
	ln -sf $< $@

../input/lodes_wac.csv: ../../download_lodes/output/lodes_wac_cook_county_2019.csv | ../input
	ln -sf $< $@

../input/tract_floorspace.csv: ../../map_floorspace_shares/output/tract_floorspace_for_model.csv | ../input
	ln -sf $< $@

../input/nu_time_per_min.csv: ../../estimate_gravity_travel_time/output/nu_time_per_min.csv | ../input
	ln -sf $< $@

../input/gravity_destination_fe.csv: ../../estimate_gravity_travel_time/output/gravity_destination_fe.csv | ../input
	ln -sf $< $@

../input/chicago_tracts.gpkg: ../../download_census_tracts/output/chicago_tracts_2010.gpkg | ../input
	ln -sf $< $@

# Create directories
../input ../output:
	mkdir -p $@

# Clean
clean:
	rm -f ../output/*.csv ../output/*.pdf ../output/*.txt ../input/*.csv ../input/*.gpkg

# Just run model (skip diagnostics)
model: $(MODEL_OUTPUTS)

# Just run diagnostics (assumes model already run)
diagnostics: $(DIAGNOSTIC_OUTPUTS)

# Just identify RLE stations
rle: $(RLE_OUTPUTS)

.PHONY: all clean model diagnostics rle
