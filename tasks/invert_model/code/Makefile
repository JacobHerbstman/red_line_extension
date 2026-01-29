SHELL := bash
include ../../shell_functions.make

# Julia command
JULIA = julia

# Main outputs
MODEL_OUTPUTS := ../output/model_fundamentals.csv ../output/model_parameters.csv
DIAGNOSTIC_OUTPUTS := ../output/diagnostic_maps.pdf ../output/diagnostic_fit.pdf \
                      ../output/diagnostic_distributions.pdf ../output/diagnostic_relationships.pdf \
                      ../output/diagnostic_land_market.pdf ../output/diagnostic_income_validation.pdf \
                      ../output/diagnostic_summary.txt

all: $(MODEL_OUTPUTS) $(DIAGNOSTIC_OUTPUTS)

# Model inversion (Julia)
$(MODEL_OUTPUTS): invert_model.jl ../input/commuting_matrix.csv ../input/lodes_rac.csv ../input/lodes_wac.csv ../input/tract_floorspace.csv | ../output
	$(JULIA) invert_model.jl

# Diagnostics (R) - depends on model outputs
$(DIAGNOSTIC_OUTPUTS): diagnostics.R $(MODEL_OUTPUTS) ../input/chicago_tracts.gpkg | ../output
	$(R) diagnostics.R

# Symlinks to inputs from other tasks
../input/commuting_matrix.csv: ../../estimate_gravity/output/commuting_matrix.csv | ../input
	ln -sf $< $@

../input/lodes_rac.csv: ../../download_lodes/output/lodes_rac_cook_county_2019.csv | ../input
	ln -sf $< $@

../input/lodes_wac.csv: ../../download_lodes/output/lodes_wac_cook_county_2019.csv | ../input
	ln -sf $< $@

../input/tract_floorspace.csv: ../../map_floorspace_shares/output/tract_floorspace_for_model.csv | ../input
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

.PHONY: all clean model diagnostics
