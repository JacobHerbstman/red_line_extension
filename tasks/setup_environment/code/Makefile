SHELL := bash
include ../../shell_functions.make

all: ../output/R_packages.txt

include ../../generic.make

../output/R_packages.txt: packages.R | ../output
	$(R) $<


