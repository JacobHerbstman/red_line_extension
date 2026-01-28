SHELL := bash
include ../../shell_functions.make

all: ../output/commercial_floorspace_pins.csv ../output/multifamily_floorspace_pins.csv ../output/commercial_download_diagnostics.txt

../output/commercial_floorspace_pins.csv ../output/multifamily_floorspace_pins.csv ../output/commercial_download_diagnostics.txt: download_commercial_floorspace.R | ../output
	$(R) download_commercial_floorspace.R

include ../../generic.make
