SHELL := bash
include ../../shell_functions.make

all: ../output/residential_floorspace_pins.csv ../output/residential_download_diagnostics.txt

../output/residential_floorspace_pins.csv ../output/residential_download_diagnostics.txt: download_residential_floorspace.R | ../output
	$(R) download_residential_floorspace.R

include ../../generic.make
