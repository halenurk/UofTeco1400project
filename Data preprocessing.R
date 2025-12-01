# -------------------------------------------------------
# LOCAL PRE-PROCESSING SCRIPT
# -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr)

# 1. Load Data (Adjust path to where your file is locally)
path_to_raw_data <- "CEX_1980_2012.dta" 

message("Reading and filtering raw data...")
CEX <- read_dta(path_to_raw_data, col_select = c(
  "consumption_nondurables", 
  "total_consumption",      # <--- ADDED THIS
  "total_income", "assets_0", "durables",
  "hh_age", "family_size", "marital", "unemp",
  "region", "year"
))

# 2. Apply Filters
# We filter out rows where EITHER consumption measure is missing/zero
CEX <- subset(
  CEX,
  consumption_nondurables > 0 &
    total_consumption > 0 &      # <--- ADDED FILTER
    total_income > 0 &
    !is.na(consumption_nondurables) &
    !is.na(total_consumption) &  # <--- ADDED CHECK
    !is.na(total_income) &
    !is.na(assets_0) &
    !is.na(durables)
)

# 3. Merge with Annual Non-Durables Index
# Load the annual index data (ensure the CSV is in your working directory)
nondurables_index <- read.csv("annual_nondurables_1980_2012.csv")

# Merge the index into the main CEX dataframe
# We use 'left_join' to keep all CEX rows and match 'year' (CEX) to 'Year' (CSV)
CEX <- left_join(CEX, nondurables_index, by = c("year" = "Year"))

# Optional: Check if the merge was successful
message("Data merged. New column 'Average_Index' added.")
head(CEX)

# Convert to factors
cols_to_factor <- c("region", "year", "marital")
CEX[cols_to_factor] <- lapply(CEX[cols_to_factor], as.factor)

# 3. Save compressed file
saveRDS(CEX, "CEX_Cleaned.rds", compress = "xz") 

# 4. Check size
file_size_mb <- file.size("CEX_Cleaned.rds") / 10^6
message(paste0("Done! File saved as 'CEX_Cleaned.rds'. Size: ", round(file_size_mb, 2), " MB"))