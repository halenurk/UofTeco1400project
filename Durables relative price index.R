# -------------------------------------------------------
# GENERATE ANNUAL PRICE INDICES AND RATIO
# -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, lubridate, readr)

# 1. Define File Paths (Adjust names if your files are different)
file_nondurables <- "CUUR0000SAN.csv" # Non-Durables (CPI-U)
file_durables    <- "CUUR0000SAD.csv" # Durables (CPI-U)

# Safety check
if (!file.exists(file_nondurables) || !file.exists(file_durables)) {
  stop("Error: Input CSV files not found. Please ensure both 'CUUR0000SAN.csv' and 'CUUR0000SAD.csv' are in the working directory.")
}

# 2. Function to Process Each Index
process_index <- function(filepath, col_name) {
  df <- read_csv(filepath, show_col_types = FALSE)
  
  # Ensure date column exists and is formatted correctly
  # FRED usually names it 'observation_date' or 'DATE'
  date_col <- names(df)[1] # Assuming first column is date
  val_col  <- names(df)[2] # Assuming second column is the value
  
  df %>%
    mutate(
      observation_date = as.Date(!!sym(date_col)),
      Year = year(observation_date)
    ) %>%
    group_by(Year) %>%
    summarise(
      !!col_name := round(mean(!!sym(val_col), na.rm = TRUE), 3)
    )
}

# 3. Get Annual Averages
df_nondur <- process_index(file_nondurables, "annual_nondur_index")
df_dur    <- process_index(file_durables, "annual_dur_index")

# 4. Merge and Calculate Ratio
price_data <- inner_join(df_nondur, df_dur, by = "Year") %>%
  mutate(
    # The Normalized Price of Durables (in units of Non-Durables)
    # P_D = Price_Durables / Price_NonDurables
    normalized_durables_index = round(annual_dur_index / annual_nondur_index, 4)
  ) %>%
  # Filter for your specific analysis period (optional, but good for cleanliness)
  filter(Year >= 1956)

# Check if price_data was created successfully
if (!exists("price_data")) {
  stop("Error: 'price_data' object was not created. Check if input files have matching Years.")
}

# 5. View and Save
print(head(price_data, 10))

output_filename <- "annual_price_indices.csv"
write_csv(price_data, output_filename)

message(paste("Success! File saved as:", output_filename))