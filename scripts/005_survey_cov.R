#----------------
#Project: Multi-season occupancy analysis
#Purpose: To prepare observation/survey covariates matrix
#---------------

# 1. load required packages
library(dplyr)
library(lubridate)

# 2. load required csv files
data <- read.csv("D:/R_projects/multiseason_occupancy/data/processed/station_occasion.csv")

# 3. Convert the deploy_date and end_date from character to date format
head(data)

data <- data %>%
  mutate(
    deploy_date = mdy(deploy_date),
    ended_date = mdy(ended_date)
  )

str(data)

# 4. Create all possible occasion for every site_year - one row per site * occasion
effort_long <- data %>%
  rowwise() %>%
  mutate(
    occasion = list(1:n_occasions)
  ) %>%
  unnest(occasion)

# 5. Calculate start and end date of each occasion
effort_long <- effort_long %>%
  mutate(
    occ_start = deploy_date + (occasion - 1) * 5,   #calculate start of each occasion
    occ_end = pmin(occ_start + 4, ended_date),      #calculate ending day of each occasion
    effort = as.numeric(occ_end - occ_start) + 1    #calculate active camera days
  )

# 6. Trim the data frame and keep only needed columns
effort_long <- effort_long %>%
  select(site_year,year,occasion, effort)

# 7. Create Effort Matrix
effort_matrix <- effort_long %>%
  pivot_wider(
    names_from = occasion,
    values_from = effort,
    names_prefix = "occ_"
  )

# Create Year Matrix
year_matrix <- effort_long %>%
  mutate(year_cov = year) %>%
  select(site_year, occasion, year_cov) %>%
  pivot_wider(
    names_from = occasion,
    values_from = year_cov,
    names_prefix = "occ_"
  )
# Remove year column from effort matrix
effort_matrix <- effort_matrix %>%
  select(-year)
str(effort_matrix)

# Export the observation covariate matrices
write.csv(effort_matrix, "D:/R_projects/multiseason_occupancy/output/effort_matrix.csv", row.names = FALSE)
write.csv(year_matrix, "D:/R_projects/multiseason_occupancy/output/year_matrix.csv", row.names = FALSE)






