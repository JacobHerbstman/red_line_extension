# Package bootstrap for Red Line Extension analysis

cran_packages <- c(
  "tidyverse",
  "data.table",
  "arrow",
  "sf",
  "tigris",
  "tidycensus",
  "lehdr",
  "fixest",
  "ggplot2",
  "patchwork",
  "scico",
  "RColorBrewer",
  "here",
  "glue",
  "tictoc",
  "osmextract",
  "tidytransit"
)

optional_packages <- c(
  "r5r"  # Requires Java 11+
)

# Library paths
user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load packages
output <- character()
load_pkg <- function(pkg) {
  suppressPackageStartupMessages(
    require(pkg, character.only = TRUE, quietly = TRUE)
  )
}

for (pkg in cran_packages) {
  if (!load_pkg(pkg)) {
    message(sprintf("Installing %s ...", pkg))

    success <- FALSE
    tryCatch({
      install.packages(pkg, dependencies = TRUE, type = "binary")
      success <- TRUE
    }, error = function(e) {
      message(sprintf("Binary install failed for %s: %s", pkg, e$message))
    })

    if (!success) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, type = "source")
      }, error = function(e) {
        message(sprintf("Source install failed for %s: %s", pkg, e$message))
      })
    }

    if (!load_pkg(pkg)) {
      warning(sprintf("Package %s could not be installed or loaded.", pkg))
    }
  }

  version <- tryCatch({ packageDescription(pkg, fields = "Version") }, error = function(e) NA)
  output <- c(output, paste(pkg, version, sep = " : "))
}

# Optional packages
for (pkg in optional_packages) {
  if (load_pkg(pkg)) {
    version <- tryCatch({ packageDescription(pkg, fields = "Version") }, error = function(e) NA)
    output <- c(output, paste(pkg, version, "[optional]", sep = " : "))
  } else {
    output <- c(output, paste(pkg, "NOT INSTALLED [optional]", sep = " : "))
  }
}

options(tigris_use_cache = TRUE)

# Check Census API key
census_key <- Sys.getenv("CENSUS_API_KEY")
if (census_key == "") {
  warning("CENSUS_API_KEY not found in environment. ACS downloads may fail.")
  output <- c(output, "CENSUS_API_KEY : NOT SET")
} else {
  output <- c(output, paste("CENSUS_API_KEY :", substr(census_key, 1, 8), "...[set]"))
}

message(sprintf("Loaded %d packages (including optional checks).", length(output)))

# Write output log
script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file) || !nzchar(script_file)) {
  script_file <- tryCatch(commandArgs(trailingOnly = FALSE), error = function(e) character(0))
  script_file <- script_file[grepl("^--file=", script_file)]
  script_file <- if (length(script_file) > 0) sub("^--file=", "", script_file[1]) else ""
}

packages_script_dir <- if (nzchar(script_file)) dirname(script_file) else "."
packages_output_path <- file.path(packages_script_dir, "../output/R_packages.txt")
dir.create(dirname(packages_output_path), showWarnings = FALSE, recursive = TRUE)
writeLines(output, packages_output_path)
message(sprintf("Package list written to %s", packages_output_path))
