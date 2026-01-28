# Download Cook County commercial floorspace data for Chicago
# Gets commercial and large multifamily building data, separated into two output files
# Note: Coordinates can be merged later from parcel universe in data_raw/
source("../../setup_environment/code/packages.R")

# Set longer timeout for downloads (10 minutes per request)
options(timeout = 600)

# PARAMETERS
BASE_URL <- "https://datacatalog.cookcountyil.gov/resource/csik-bsws.csv"
# Chicago townships by NAME (this API uses names)
CHICAGO_TOWNSHIPS <- c("West Chicago", "South Chicago", "Jefferson", "North Chicago",
                       "Lake View", "Rogers Park", "Hyde Park", "Lake")

# BUILD API URL with SoQL query
township_filter <- paste0("township in ('", paste(CHICAGO_TOWNSHIPS, collapse = "','"), "')")
query_params <- paste0(
  "?$limit=500000",
  "&$select=keypin,bldgsf,landsf,yearbuilt,class_es,category,tot_units,township,year",
  "&$where=", URLencode(paste0(township_filter, " AND year='2021'"))
)
url <- paste0(BASE_URL, query_params)

# DOWNLOAD
message("Downloading commercial valuations from Cook County Open Data Portal...")
message(sprintf("URL: %s", url))
tic("Download")
temp_file <- tempfile(fileext = ".csv")
download.file(url, temp_file, mode = "wb", quiet = FALSE)
comm_data <- fread(temp_file, showProgress = TRUE)
unlink(temp_file)
toc()

message(sprintf("Downloaded %s rows", format(nrow(comm_data), big.mark = ",")))

# DIAGNOSTICS - open sink for diagnostics file
sink("../output/commercial_download_diagnostics.txt")
cat("Commercial Floorspace Download Diagnostics\n")
cat(sprintf("Download date: %s\n", Sys.time()))
cat(sprintf("Total rows downloaded: %s\n\n", format(nrow(comm_data), big.mark = ",")))

cat("Township distribution:\n")
print(table(comm_data$township, useNA = "ifany"))
cat("\n")

cat("Property class distribution:\n")
print(table(comm_data$class_es, useNA = "ifany"))
cat("\n")

cat("Category distribution:\n")
print(table(comm_data$category, useNA = "ifany"))
cat("\n")

# CLEAN PIN: Remove dashes from keypin
message("\nCleaning PIN format...")
comm_data[, pin := gsub("-", "", keypin)]

# Rename columns for clarity
setnames(comm_data, c("bldgsf", "yearbuilt", "tot_units"),
         c("building_sqft", "year_built", "total_units"))

# Convert to numeric
comm_data[, building_sqft := as.numeric(building_sqft)]
comm_data[, year_built := as.integer(year_built)]
comm_data[, total_units := as.integer(total_units)]

# CLASSIFY PROPERTIES
# True Commercial: Class 5xx (first character is "5")
# Large Multifamily: Class 3xx OR category contains apartment/multifamily indicators
message("\nClassifying properties...")

comm_data[, class_prefix := substr(class_es, 1, 1)]
comm_data[, is_commercial := (class_prefix == "5")]
comm_data[, is_multifamily := (class_prefix == "3") |
            grepl("Multifamily|Apartment|Condo", category, ignore.case = TRUE)]

cat(sprintf("True commercial (Class 5xx): %s rows\n",
            format(sum(comm_data$is_commercial, na.rm = TRUE), big.mark = ",")))
cat(sprintf("Large multifamily (Class 3xx or MF category): %s rows\n",
            format(sum(comm_data$is_multifamily, na.rm = TRUE), big.mark = ",")))
cat("\n")

# DEDUPLICATE: Keep largest bldgsf per PIN for each category
message("Deduplicating: keeping largest sqft per PIN...")

# Commercial
commercial <- comm_data[is_commercial == TRUE]
setorder(commercial, pin, -building_sqft)
commercial_dedup <- commercial[, .SD[1], by = pin]
message(sprintf("Commercial: %s unique PINs", format(nrow(commercial_dedup), big.mark = ",")))

# Multifamily
multifamily <- comm_data[is_multifamily == TRUE]
setorder(multifamily, pin, -building_sqft)
multifamily_dedup <- multifamily[, .SD[1], by = pin]
message(sprintf("Multifamily: %s unique PINs", format(nrow(multifamily_dedup), big.mark = ",")))

cat(sprintf("Commercial unique PINs: %s\n", format(nrow(commercial_dedup), big.mark = ",")))
cat(sprintf("Multifamily unique PINs: %s\n\n", format(nrow(multifamily_dedup), big.mark = ",")))

if (nrow(commercial_dedup) > 0) {
  cat("Commercial building sqft summary:\n")
  print(summary(commercial_dedup$building_sqft))
  cat("\n")
}

if (nrow(multifamily_dedup) > 0) {
  cat("Multifamily building sqft summary:\n")
  print(summary(multifamily_dedup$building_sqft))
  cat("\n")

  cat("Multifamily total units summary:\n")
  print(summary(multifamily_dedup$total_units))
  cat("\n")
}

sink()

# SAVE (PIN + floorspace data only, no coordinates)
commercial_output <- commercial_dedup[, .(pin, building_sqft, year_built, property_class = class_es)]
fwrite(commercial_output, "../output/commercial_floorspace_pins.csv")
message(sprintf("\nSaved: ../output/commercial_floorspace_pins.csv (%s rows)",
                format(nrow(commercial_output), big.mark = ",")))

multifamily_output <- multifamily_dedup[, .(pin, building_sqft, year_built, total_units)]
fwrite(multifamily_output, "../output/multifamily_floorspace_pins.csv")
message(sprintf("Saved: ../output/multifamily_floorspace_pins.csv (%s rows)",
                format(nrow(multifamily_output), big.mark = ",")))

message("Saved: ../output/commercial_download_diagnostics.txt")
message("\nNote: Merge with parcel universe from data_raw/ to add coordinates")
