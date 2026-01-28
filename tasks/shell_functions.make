R = @source ../../shell_functions.sh; R_pc_and_slurm

# If 'make -n' option is invoked
ifneq (,$(findstring n,$(MAKEFLAGS)))
R := R
endif
