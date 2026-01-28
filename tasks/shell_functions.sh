# Shell functions for running R scripts
# Simplified version for local execution

R_pc_and_slurm() {
    if [ "$1" == "--no-job-name" ]; then
        shift;
    fi;
    print_info R $@;
    Rscript $@;
}

print_info() {
    software=$1;
    shift;
    if [ $# == 1 ]; then
        echo "Running ${1} via ${software}, waiting...";
    else
        echo "Running ${1} via ${software} with args = ${@:2}, waiting...";
    fi
}

clean_task() {
    find ${1} -type l -delete;
    PARENT_DIR=${1%/code};
    rm -f ${1}/*.log;
    rm -rf ${PARENT_DIR}/input ${PARENT_DIR}/output ${1}/slurmlogs;
}
