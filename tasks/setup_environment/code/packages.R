# --- Package bootstrap script for Spatial Economics HW1 ---
# Red Line Extension Welfare Analysis

rm(list = ls())

# CRAN Packages needed for this project
cran_packages <- c(
  # Data manipulation
  "tidyverse",
  "data.table",
  "arrow",
  
  # Spatial data
  "sf",
  "tigris",
  "tidycensus",
  
  # LODES data
  "lehdr",
  
  # Econometrics
  "fixest",
  
  # Visualization
  "ggplot2",
  "patchwork",
  "scico",
  "RColorBrewer",
  
 # Utilities
  "here",
  "glue",
  "tictoc"
)

# Optional packages (may require additional setup)
optional_packages <- c(
  "r5r"  # Requires Java 11+
)

# --- Step 1: ensure library paths are aligned ---
user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(user_lib, .libPaths()))

# --- Step 2: choose a reliable CRAN mirror ---
options(repos = c(CRAN = "https://cloud.r-project.org"))

# --- Step 3: install & load CRAN packages ---
output <- character()

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(sprintf("Installing %s ...", pkg))
    
    # Try binary first
    success <- FALSE
    tryCatch({
      install.packages(pkg, dependencies = TRUE, type = "binary")
      success <- TRUE
    }, error = function(e) {
      message(sprintf("Binary install failed for %s: %s", pkg, e$message))
    })
    
    # If binary fails, try source
    if (!success) {
      tryCatch({
        install.packages(pkg, dependencies = TRUE, type = "source")
      }, error = function(e) {
        message(sprintf("Source install failed for %s: %s", pkg, e$message))
      })
    }
    
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      warning(sprintf("Package %s could not be installed or loaded.", pkg))
    }
  }
  
  version <- tryCatch({ packageDescription(pkg, fields = "Version") }, error = function(e) NA)
  output <- c(output, paste(pkg, version, sep = " : "))
}

# --- Step 4: Try optional packages (don't fail if unavailable) ---
for (pkg in optional_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    version <- tryCatch({ packageDescription(pkg, fields = "Version") }, error = function(e) NA)
    output <- c(output, paste(pkg, version, "[optional]", sep = " : "))
  } else {
    output <- c(output, paste(pkg, "NOT INSTALLED [optional]", sep = " : "))
  }
}

# --- Step 5: Set tigris options for caching ---
options(tigris_use_cache = TRUE)

# --- Step 6: Check for Census API key ---
census_key <- Sys.getenv("CENSUS_API_KEY")
if (census_key == "") {
  warning("CENSUS_API_KEY not found in environment. ACS downloads may fail.")
  output <- c(output, "CENSUS_API_KEY : NOT SET")
} else {
  output <- c(output, paste("CENSUS_API_KEY :", substr(census_key, 1, 8), "...[set]"))
}

# --- Step 7: Write output log ---
output_log <- paste("Packages loaded:", paste(output, collapse = "\n"), sep = "\n")
cat(output_log, "\n")

# Write to file for Makefile target (resolve path relative to this script, not cwd)
packages_script_dir <- dirname(sys.frame(1)$ofile %||% ".")
packages_output_path <- file.path(packages_script_dir, "../output/R_packages.txt")
dir.create(dirname(packages_output_path), showWarnings = FALSE, recursive = TRUE)
writeLines(output, packages_output_path)
message(sprintf("\nPackage list written to %s", packages_output_path))
