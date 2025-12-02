# Load necessary library
library(dplyr)
library(lubridate)

# 1. Read the CSV file
# Ensure the file 'CUUR0000SAN.csv' is in your working directory
data <- read.csv("CUUR0000SAN.csv")

# 2. Convert observation_date to Date object
data$observation_date <- as.Date(data$observation_date, format = "%Y-%m-%d")

# 3. Extract Year from the date
data$Year <- year(data$observation_date)

# 4. Filter for years 1977-2013
filtered_data <- data %>%
  filter(Year >= 1977 & Year <= 2013)

# 5. Calculate the annual average
annual_averages <- filtered_data %>%
  group_by(Year) %>%
  summarise(Average_Index = round(mean(CUUR0000SAN, na.rm = TRUE), 3))

# 6. View the result
print(annual_averages, n=40) # n=40 ensures all rows are shown

# Optional: Save to a new CSV
write.csv(annual_averages, "annual_nondurables_1980_2012.csv", row.names = FALSE)