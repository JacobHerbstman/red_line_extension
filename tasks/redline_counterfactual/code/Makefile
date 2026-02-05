# Declare default target first (before includes)
.DEFAULT_GOAL := all

SHELL := bash
include ../../shell_functions.make
include ../../generic.make

# Julia command
JULIA = julia

COUNTERFACTUAL_OUTPUTS := ../output/counterfactual.csv ../output/welfare.csv
SUMMARY_OUTPUTS := ../output/welfare_summary.csv \
                   ../output/counterfactual_main_spec.csv \
                   ../output/map_welfare_by_tract.pdf \
                   ../output/map_treatment_intensity.pdf \
                   ../output/map_population_change.pdf \
                   ../output/map_price_change.pdf \
                   ../output/robustness_table.csv

ALL_OUTPUTS := $(COUNTERFACTUAL_OUTPUTS) $(SUMMARY_OUTPUTS)

all: $(ALL_OUTPUTS)

main: $(ALL_OUTPUTS)

$(COUNTERFACTUAL_OUTPUTS): run_counterfactual.jl \
                           ../input/model_fundamentals.csv \
                           ../input/model_parameters.csv \
                           ../input/commuting_shares_model.csv \
                           ../input/travel_time_change.csv \
                           ../input/tract_floorspace.csv \
                           | ../output
	$(JULIA) run_counterfactual.jl

$(SUMMARY_OUTPUTS): summarize_counterfactuals.R $(COUNTERFACTUAL_OUTPUTS) \
                    ../input/chicago_tracts.gpkg | ../output
	$(R) summarize_counterfactuals.R ../output/welfare.csv

../input/model_fundamentals.csv: ../../invert_model/output/model_fundamentals.csv | ../input
	ln -sf $< $@

../input/model_parameters.csv: ../../invert_model/output/model_parameters.csv | ../input
	ln -sf $< $@

../input/travel_time_change.csv: ../../compute_travel_times/output/travel_time_change.csv | ../input
	ln -sf $< $@

../input/commuting_shares_model.csv: ../../invert_model/output/commuting_shares_model.csv | ../input
	ln -sf $< $@

../input/chicago_tracts.gpkg: ../../download_census_tracts/output/chicago_tracts_2010.gpkg | ../input
	ln -sf $< $@

../input/tract_floorspace.csv: ../../map_floorspace_shares/output/tract_floorspace_for_model.csv | ../input
	ln -sf $< $@

clean:
	rm -f ../output/*.csv ../output/*.pdf ../input/*

.PHONY: all main clean link-inputs

link-inputs: ../input/model_fundamentals.csv ../input/model_parameters.csv \
             ../input/commuting_shares_model.csv \
             ../input/travel_time_change.csv ../input/chicago_tracts.gpkg \
             ../input/tract_floorspace.csv
