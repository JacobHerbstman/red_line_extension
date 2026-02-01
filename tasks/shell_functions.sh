# Shell functions for running R scripts
# Simplified version for local execution

set_java_home() {
    if [ -n "${JAVA_HOME:-}" ] && [ -x "${JAVA_HOME}/bin/java" ]; then
        return
    fi

    if command -v /usr/libexec/java_home >/dev/null 2>&1; then
        JAVA_HOME_CANDIDATE=$(/usr/libexec/java_home 2>/dev/null || true)
        if [ -n "${JAVA_HOME_CANDIDATE}" ] && [ -x "${JAVA_HOME_CANDIDATE}/bin/java" ]; then
            export JAVA_HOME="${JAVA_HOME_CANDIDATE}"
            export PATH="${JAVA_HOME}/bin:${PATH}"
            return
        fi
    fi

    for JAVA_HOME_CANDIDATE in \
        /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home \
        /opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home \
        /opt/homebrew/opt/openjdk@11/libexec/openjdk.jdk/Contents/Home \
        /usr/local/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home \
        /usr/local/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home \
        /usr/local/opt/openjdk@11/libexec/openjdk.jdk/Contents/Home
    do
        if [ -x "${JAVA_HOME_CANDIDATE}/bin/java" ]; then
            export JAVA_HOME="${JAVA_HOME_CANDIDATE}"
            export PATH="${JAVA_HOME}/bin:${PATH}"
            return
        fi
    done
}

R_pc_and_slurm() {
    if [ "$1" == "--no-job-name" ]; then
        shift;
    fi;
    set_java_home;
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
