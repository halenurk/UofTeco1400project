# -------------------------------------------------------
# LOCAL PRE-PROCESSING SCRIPT
# -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr, readr)

# 1. Load Data
# -------------------------------------------------------
path_to_raw_data <- "CEX_1980_2012.dta" 

message("Reading raw data...")
CEX <- read_dta(path_to_raw_data, col_select = c(
  "household_id",             
  "year",
  "survey_year",
  "survey_month",             # <--- ADDED for sorting
  "durables",                 # <--- ADDED for GMM (K_t)
  "consumption_nondurables", 
  "total_consumption",      
  "total_income",
  "assets_0",
  "hh_age",
  "family_size",
  "region",
  "marital",
  "unemp"
))

# 2. Merge with Annual Non-Durables Index (P_D)
# -------------------------------------------------------
if(file.exists("annual_nondurables_1980_2012.csv")) {
  nondurables_index <- read_csv("annual_nondurables_1980_2012.csv", show_col_types = FALSE)
  
  # Ensure column types match for join
  CEX$year <- as.numeric(as.character(CEX$year))
  nondurables_index$Year <- as.numeric(nondurables_index$Year)
  
  CEX <- left_join(CEX, nondurables_index, by = c("year" = "Year"))
  message("Merged with Non-Durables Index.")
} else {
  warning("annual_nondurables_1980_2012.csv not found!")
}

# 3. Apply Filters
# -------------------------------------------------------
# We remove 'assets_0' from filters as it is not in the GMM equation
CEX_Cleaned <- subset(
  CEX,
  consumption_nondurables > 0 &
    total_consumption > 0 &
    total_income > 0 &
    durables > 0 &            # <--- ADDED: Log(Durables) requires positive values
    !is.na(household_id) &
    !is.na(year) &
    !is.na(survey_month) &    # <--- ADDED
    !is.na(consumption_nondurables) &
    !is.na(total_consumption) &
    !is.na(total_income) &
    !is.na(durables)          # <--- ADDED
)

# Convert categorical variables
cols_to_factor <- c("region", "year", "marital")
CEX_Cleaned[cols_to_factor] <- lapply(CEX_Cleaned[cols_to_factor], as.factor)

# Save
saveRDS(CEX_Cleaned, "CEX_Cleaned.rds")
message("Preprocessing complete. File saved as CEX_Cleaned.rds")

# 4. Check size
file_size_mb <- file.size("CEX_Cleaned.rds") / 10^6
message(paste0("Done! File saved as 'CEX_Cleaned.rds'. Size: ", round(file_size_mb, 2), " MB"))