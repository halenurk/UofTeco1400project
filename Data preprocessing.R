# -------------------------------------------------------
# LOCAL PRE-PROCESSING SCRIPT
# -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr)

# 1. Load Data (Adjust path to where your file is locally)
#    Note: 'col_select' saves memory by only reading what we need
path_to_raw_data <- "CEX_1980_2012.dta" 

message("Reading and filtering raw data...")
CEX <- read_dta(path_to_raw_data, col_select = c(
  "consumption_nondurables", "total_income", "assets_0",
  "hh_age", "family_size", "marital", "educa", "unemp",
  "region", "survey_year"
))

# 2. Apply Filters (Same logic as your partner's code)
CEX <- subset(
  CEX,
  consumption_nondurables > 0 &
    total_income > 0 &
    !is.na(consumption_nondurables) &
    !is.na(total_income) &
    !is.na(assets_0)
)

# 3. Create Variables
CEX$log_cons_nd   <- log(CEX$consumption_nondurables)
CEX$log_inc_total <- log(CEX$total_income)

median_assets0    <- median(CEX$assets_0, na.rm = TRUE)
CEX$low_wealth    <- as.numeric(CEX$assets_0 <= median_assets0)

# Convert to factors for compression efficiency
cols_to_factor <- c("region", "survey_year", "marital", "educa")
CEX[cols_to_factor] <- lapply(CEX[cols_to_factor], as.factor)

# 4. Save compressed file
#    .rds is much smaller and faster than .dta or .csv
saveRDS(CEX, "CEX_Cleaned.rds", compress = "xz") 

# 5. Check size
file_size_mb <- file.size("CEX_Cleaned.rds") / 10^6
message(paste0("Done! File saved as 'CEX_Cleaned.rds'. Size: ", round(file_size_mb, 2), " MB"))