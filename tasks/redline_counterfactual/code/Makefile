# Declare default target first (before includes)
.DEFAULT_GOAL := all

SHELL := bash
include ../../shell_functions.make
include ../../generic.make

# Julia command
JULIA = julia

# ============================================================================
# SPECIFICATION GRID
# ============================================================================
# Catchment radii (meters): 400, 800 (main), 1200
# GTFS shock scaling: full shock only (100%)

CATCHMENT_RADII := 800
KAPPA_REDUCTIONS := 100

# Main specification
MAIN_CATCHMENT := 800
MAIN_REDUCTION := 100

# ============================================================================
# GENERATE OUTPUT FILENAMES
# ============================================================================

# Each specification produces:
#   - counterfactual_r{radius}_k{reduction}.csv (tract-level results)
#   - welfare_r{radius}_k{reduction}.csv (aggregate welfare)

define GEN_COUNTERFACTUAL_FILES
$(foreach radius,$(CATCHMENT_RADII), \
$(foreach reduction,$(KAPPA_REDUCTIONS), \
    ../output/counterfactual_r$(radius)_k$(reduction).csv \
))
endef

define GEN_WELFARE_FILES
$(foreach radius,$(CATCHMENT_RADII), \
$(foreach reduction,$(KAPPA_REDUCTIONS), \
    ../output/welfare_r$(radius)_k$(reduction).csv \
))
endef

COUNTERFACTUAL_OUTPUTS := $(call GEN_COUNTERFACTUAL_FILES)
WELFARE_OUTPUTS := $(call GEN_WELFARE_FILES)

# Summary outputs (combine all specs)
SUMMARY_OUTPUTS := ../output/welfare_summary.csv \
                   ../output/counterfactual_main_spec.csv \
                   ../output/map_welfare_by_tract.pdf \
                   ../output/map_treatment_intensity.pdf \
                   ../output/map_population_change.pdf \
                   ../output/map_price_change.pdf \
                   ../output/robustness_table.csv

ALL_OUTPUTS := $(COUNTERFACTUAL_OUTPUTS) $(WELFARE_OUTPUTS) $(SUMMARY_OUTPUTS)

# ============================================================================
# MAIN TARGETS
# ============================================================================

all: $(ALL_OUTPUTS)

# Main specification only (for quick runs)
main: ../output/counterfactual_r$(MAIN_CATCHMENT)_k$(MAIN_REDUCTION).csv \
      ../output/welfare_r$(MAIN_CATCHMENT)_k$(MAIN_REDUCTION).csv

# ============================================================================
# RULES FOR EACH SPECIFICATION
# ============================================================================

# Generate rules for each (radius, reduction) pair
define GEN_COUNTERFACTUAL_RULES
../output/counterfactual_r$(radius)_k$(reduction).csv ../output/welfare_r$(radius)_k$(reduction).csv: \
    run_counterfactual.jl \
    ../input/model_fundamentals.csv \
    ../input/model_parameters.csv \
    ../input/commuting_shares_model.csv \
    ../input/travel_time_change.csv \
    ../input/tract_floorspace.csv \
    | ../output
	$(JULIA) run_counterfactual.jl $(radius) $(reduction)
endef

$(foreach radius,$(CATCHMENT_RADII), \
$(foreach reduction,$(KAPPA_REDUCTIONS), \
    $(eval $(GEN_COUNTERFACTUAL_RULES)) \
))

# ============================================================================
# SUMMARY AND VISUALIZATION
# ============================================================================

$(SUMMARY_OUTPUTS): summarize_counterfactuals.R $(COUNTERFACTUAL_OUTPUTS) $(WELFARE_OUTPUTS) \
                    ../input/chicago_tracts.gpkg | ../output
	$(R) summarize_counterfactuals.R $(WELFARE_OUTPUTS)

# ============================================================================
# INPUT SYMLINKS
# ============================================================================

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

# ============================================================================
# CLEAN
# ============================================================================

clean:
	rm -f ../output/*.csv ../output/*.pdf ../input/*

# ============================================================================
# PHONY TARGETS
# ============================================================================

.PHONY: all main clean link-inputs

link-inputs: ../input/model_fundamentals.csv ../input/model_parameters.csv \
             ../input/commuting_shares_model.csv \
             ../input/travel_time_change.csv ../input/chicago_tracts.gpkg \
             ../input/tract_floorspace.csv
