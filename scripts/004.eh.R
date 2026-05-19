#----------------
#Project: Multi-season occupancy analysis
#Purpose: To create encounter history matrix
#---------------

# 1. Load required packages
library(dplyr)
library(tidyr)
library(lubridate)

# 2. Load the location and metadata csv files
station <- read.csv("D:/R_projects/multiseason_occupancy/data/processed/cam_station.csv")
metadata <- read.csv("D:/R_projects/multiseason_occupancy/data/processed/detection_metadata.csv")

# 3. Convert dates
# 3.1 of station table
head(station)

station <- station %>%
  mutate(
    deploy_date = ymd(deploy_date),
    ended_date = ymd(ended_date)
  )

str(station)

# 3.2 Detection metadata
head(metadata)

metadata <- metadata %>%
  mutate(
    datetime = ymd_hms(datetime),
    date = as.Date(datetime)
  )

str(metadata)

# 4. Create 5-day survey occasions
# 4.1 Add occasion number to detections
metadata <- metadata %>%
  left_join(
    station %>%
      select(site_year, deploy_date),
    by = "site_year"
  ) %>%
  mutate(
    days_since_deploy = as.numeric(date - deploy_date),
    occasion = floor(days_since_deploy/5) + 1
  ) 

# 5. Collapse multiple detection within occasion
detection <- metadata %>%
  group_by(site_year, occasion) %>%
  summarise(detected = 1, .groups = "drop")

# 6. Calculate total possible occasions per site
station_occ <- station %>%
  mutate(
    total_days = as.numeric(ended_date - deploy_date) + 1,
    n_occasions = ceiling(total_days / 5)
  )

# 7. Create all site * occasion combinations - creating all possible occasions for every site-year 
all_occasions <- station_occ %>%
  select(site_year, n_occasions) %>%
  rowwise() %>%
  mutate(occasion = list(1:n_occasions)) %>%
  unnest(occasion)

# 8. Merge detections
encounter_long <- all_occasions %>%
  left_join(
    detection,
    by = c("site_year", "occasion")
  ) %>%
  mutate(
    detected = ifelse(is.na(detected), 0, detected)
  )

# 9. Convert to encounter history matrix
eh <- encounter_long %>%
  pivot_wider(
    names_from = occasion,
    values_from = detected,
    names_prefix = "occ_"
  )

# 10. Assign missing columns to be NA
max_occ <- max(station_occ$n_occasions)

eh <- eh %>%
  select(site_year, paste0("occ_", 1:max_occ))

# 11. Export the final result
write.csv(station_occ, "D:/R_projects/multiseason_occupancy/data/processed/station_occasion.csv", row.names = FALSE)
write.csv(eh, "D:/R_projects/multiseason_occupancy/output/encounter_history.csv", row.names = FALSE)

