# Download Cook County commercial floorspace data
# Coordinates can be merged from parcel universe later

source("../../setup_environment/code/packages.R")

options(timeout = 600)

BASE_URL <- "https://datacatalog.cookcountyil.gov/resource/csik-bsws.csv"
CHICAGO_TOWNSHIPS <- c("West Chicago", "South Chicago", "Jefferson", "North Chicago",
                       "Lake View", "Rogers Park", "Hyde Park", "Lake")

township_filter <- paste0("township in ('", paste(CHICAGO_TOWNSHIPS, collapse = "','"), "')")
query_params <- paste0(
  "?$limit=500000",
  "&$select=keypin,bldgsf,landsf,yearbuilt,class_es,category,tot_units,township,year",
  "&$where=", URLencode(paste0(township_filter, " AND year='2021'"))
)
url <- paste0(BASE_URL, query_params)

message("Downloading commercial valuations from Cook County")

tic("Download")
temp_file <- tempfile(fileext = ".csv")
download.file(url, temp_file, mode = "wb", quiet = FALSE)
comm_data <- fread(temp_file, showProgress = TRUE)
unlink(temp_file)
toc()

message(sprintf("Downloaded %s rows", format(nrow(comm_data), big.mark = ",")))

# Diagnostics
sink("../output/commercial_download_diagnostics.txt")
cat("Commercial Floorspace Download Diagnostics\n")
cat(sprintf("Download date: %s\n", Sys.time()))
cat(sprintf("Total rows: %s\n\n", format(nrow(comm_data), big.mark = ",")))

cat("Township distribution:\n")
print(table(comm_data$township, useNA = "ifany"))
cat("\n")

cat("Property class distribution:\n")
print(table(comm_data$class_es, useNA = "ifany"))
cat("\n")

cat("Category distribution:\n")
print(table(comm_data$category, useNA = "ifany"))
cat("\n")

# Clean
message("Cleaning PIN format...")
comm_data[, pin := gsub("-", "", keypin)]

setnames(comm_data, c("bldgsf", "yearbuilt", "tot_units"),
         c("building_sqft", "year_built", "total_units"))

comm_data[, building_sqft := as.numeric(building_sqft)]
comm_data[, year_built := as.integer(year_built)]
comm_data[, total_units := as.integer(total_units)]

# Classify: commercial = Class 5xx, multifamily = Class 3xx or apartment category
message("Classifying properties...")

comm_data[, class_prefix := substr(class_es, 1, 1)]
comm_data[, is_commercial := (class_prefix == "5")]
comm_data[, is_multifamily := (class_prefix == "3") |
            grepl("Multifamily|Apartment|Condo", category, ignore.case = TRUE)]

cat(sprintf("Commercial (Class 5xx): %s\n",
            format(sum(comm_data$is_commercial, na.rm = TRUE), big.mark = ",")))
cat(sprintf("Multifamily (Class 3xx or MF category): %s\n",
            format(sum(comm_data$is_multifamily, na.rm = TRUE), big.mark = ",")))

# Deduplicate
message("Deduplicating by PIN...")

commercial <- comm_data[is_commercial == TRUE]
setorder(commercial, pin, -building_sqft)
commercial_dedup <- commercial[, .SD[1], by = pin]
message(sprintf("Commercial: %s unique PINs", format(nrow(commercial_dedup), big.mark = ",")))

multifamily <- comm_data[is_multifamily == TRUE]
setorder(multifamily, pin, -building_sqft)
multifamily_dedup <- multifamily[, .SD[1], by = pin]
message(sprintf("Multifamily: %s unique PINs", format(nrow(multifamily_dedup), big.mark = ",")))

cat(sprintf("\nCommercial unique PINs: %s\n", format(nrow(commercial_dedup), big.mark = ",")))
cat(sprintf("Multifamily unique PINs: %s\n\n", format(nrow(multifamily_dedup), big.mark = ",")))

if (nrow(commercial_dedup) > 0) {
  cat("Commercial sqft summary:\n")
  print(summary(commercial_dedup$building_sqft))
  cat("\n")
}

if (nrow(multifamily_dedup) > 0) {
  cat("Multifamily sqft summary:\n")
  print(summary(multifamily_dedup$building_sqft))
  cat("\nMultifamily units summary:\n")
  print(summary(multifamily_dedup$total_units))
}

sink()

# Save
commercial_output <- commercial_dedup[, .(pin, building_sqft, year_built, property_class = class_es)]
fwrite(commercial_output, "../output/commercial_floorspace_pins.csv")
message(sprintf("Saved: commercial_floorspace_pins.csv (%s rows)", format(nrow(commercial_output), big.mark = ",")))

multifamily_output <- multifamily_dedup[, .(pin, building_sqft, year_built, total_units)]
fwrite(multifamily_output, "../output/multifamily_floorspace_pins.csv")
message(sprintf("Saved: multifamily_floorspace_pins.csv (%s rows)", format(nrow(multifamily_output), big.mark = ",")))
