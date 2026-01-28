# Download Cook County residential floorspace data for Chicago
# Gets residential building square footage by PIN
# Note: Coordinates can be merged later from parcel universe in data_raw/
source("../../setup_environment/code/packages.R")

# PARAMETERS
BASE_URL <- "https://datacatalog.cookcountyil.gov/resource/x54s-btds.csv"
# Chicago township codes: 70-77
CHICAGO_TOWNSHIPS <- c("70", "71", "72", "73", "74", "75", "76", "77")

# Set longer timeout for downloads (10 minutes per request)
options(timeout = 600)

# DOWNLOAD in batches by township to avoid API timeout
message("Downloading residential improvements from Cook County Open Data Portal...")
message("Downloading by township to avoid API timeout...")

all_res <- list()

tic("Total download")
for (twp in CHICAGO_TOWNSHIPS) {
  message(sprintf("\nDownloading township %s...", twp))

  query_params <- paste0(
    "?$limit=1000000",
    "&$select=pin,year,char_bldg_sf,char_yrblt,class",
    "&$where=", URLencode(sprintf("township_code='%s'", twp))
  )
  url <- paste0(BASE_URL, query_params)

  tic(sprintf("Township %s", twp))
  tryCatch({
    temp_file <- tempfile(fileext = ".csv")
    download.file(url, temp_file, mode = "wb", quiet = FALSE)
    twp_data <- fread(temp_file, showProgress = FALSE)
    unlink(temp_file)

    message(sprintf("  Downloaded %s rows", format(nrow(twp_data), big.mark = ",")))
    all_res[[twp]] <- twp_data
  }, error = function(e) {
    message(sprintf("  ERROR downloading township %s: %s", twp, e$message))
  })
  toc()
}
toc()

# Combine all townships
res_data <- rbindlist(all_res)
message(sprintf("\nTotal downloaded: %s rows", format(nrow(res_data), big.mark = ",")))

# DIAGNOSTICS - open sink for diagnostics file
sink("../output/residential_download_diagnostics.txt")
cat("Residential Floorspace Download Diagnostics\n")
cat(sprintf("Download date: %s\n", Sys.time()))
cat(sprintf("Total rows downloaded: %s\n\n", format(nrow(res_data), big.mark = ",")))

cat("Tax year distribution:\n")
print(table(res_data$year, useNA = "ifany"))
cat("\n")

cat("Property class distribution (top 20):\n")
print(head(sort(table(res_data$class), decreasing = TRUE), 20))
cat("\n")

# DEDUPLICATE: Keep most recent tax year per PIN, then largest sqft if ties
message("\nDeduplicating: keeping most recent year, then largest sqft per PIN...")

# Rename columns for clarity
setnames(res_data, c("char_bldg_sf", "char_yrblt"), c("building_sqft", "year_built"))

# Convert to numeric
res_data[, building_sqft := as.numeric(building_sqft)]
res_data[, year_built := as.integer(year_built)]
res_data[, year := as.integer(year)]

# Sort and deduplicate
setorder(res_data, pin, -year, -building_sqft)
res_dedup <- res_data[, .SD[1], by = pin]

message(sprintf("After deduplication: %s unique PINs", format(nrow(res_dedup), big.mark = ",")))

cat(sprintf("Unique PINs after deduplication: %s\n", format(nrow(res_dedup), big.mark = ",")))
cat("\nBuilding sqft summary:\n")
print(summary(res_dedup$building_sqft))
cat("\n")

cat("Year built summary:\n")
print(summary(res_dedup$year_built))
cat("\n")

sink()

# SAVE (PIN + floorspace data only, no coordinates)
output <- res_dedup[, .(pin, building_sqft, year_built)]
fwrite(output, "../output/residential_floorspace_pins.csv")
message(sprintf("\nSaved: ../output/residential_floorspace_pins.csv (%s rows)",
                format(nrow(output), big.mark = ",")))
message("Saved: ../output/residential_download_diagnostics.txt")
message("\nNote: Merge with parcel universe from data_raw/ to add coordinates")
