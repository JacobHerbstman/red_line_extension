# Download Cook County residential floorspace data
# Coordinates can be merged from parcel universe later

source("../../setup_environment/code/packages.R")

BASE_URL <- "https://datacatalog.cookcountyil.gov/resource/x54s-btds.csv"
CHICAGO_TOWNSHIPS <- c("70", "71", "72", "73", "74", "75", "76", "77")

options(timeout = 600)

message("Downloading residential improvements from Cook County")

all_res <- list()

tic("Total download")
for (twp in CHICAGO_TOWNSHIPS) {
  message(sprintf("Downloading township %s...", twp))

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
    message(sprintf("  Error downloading township %s: %s", twp, e$message))
  })
  toc()
}
toc()

res_data <- rbindlist(all_res)
message(sprintf("Total downloaded: %s rows", format(nrow(res_data), big.mark = ",")))

# Diagnostics
sink("../output/residential_download_diagnostics.txt")
cat("Residential Floorspace Download Diagnostics\n")
cat(sprintf("Download date: %s\n", Sys.time()))
cat(sprintf("Total rows: %s\n\n", format(nrow(res_data), big.mark = ",")))

cat("Tax year distribution:\n")
print(table(res_data$year, useNA = "ifany"))
cat("\n")

cat("Property class distribution (top 20):\n")
print(head(sort(table(res_data$class), decreasing = TRUE), 20))
cat("\n")

# Deduplicate
message("Deduplicating by PIN...")

setnames(res_data, c("char_bldg_sf", "char_yrblt"), c("building_sqft", "year_built"))

res_data[, building_sqft := as.numeric(building_sqft)]
res_data[, year_built := as.integer(year_built)]
res_data[, year := as.integer(year)]

setorder(res_data, pin, -year, -building_sqft)
res_dedup <- res_data[, .SD[1], by = pin]

message(sprintf("After deduplication: %s unique PINs", format(nrow(res_dedup), big.mark = ",")))

cat(sprintf("Unique PINs: %s\n", format(nrow(res_dedup), big.mark = ",")))
cat("\nBuilding sqft summary:\n")
print(summary(res_dedup$building_sqft))
cat("\nYear built summary:\n")
print(summary(res_dedup$year_built))
sink()

# Save
output <- res_dedup[, .(pin, building_sqft, year_built)]
fwrite(output, "../output/residential_floorspace_pins.csv")
message(sprintf("Saved: residential_floorspace_pins.csv (%s rows)", format(nrow(output), big.mark = ",")))
