#----------------
#Project: Multi-season occupancy analysis
#Purpose: Assign siteID to the metadata file
#---------------

# Load required packages
library(dplyr)

# Import the metadata csv file: processed/img_metadata.csv and processed/cam_station.csv
metadata <- read.csv("D:/R_projects/multiseason_occupancy/data/processed/img_metadata.csv")
station <- read.csv("D:/R_projects/multiseason_occupancy/data/processed/cam_station.csv")

# Assign siteID to the metadata based on gridID
head(metadata)
head(station)

Join <- metadata %>%
  left_join(
    station %>% select(gridID,siteID, year),
    by = c("gridID", "year") 
  )
# Create a combined identifier in metadata and station
Join$site_year <- paste(Join$siteID,
                           Join$year,
                           sep = "_")

station$site_year <- paste(station$siteID,
                           station$year,
                           sep = "_")

# Export it as a csv
write.csv(Join, "D:/R_projects/multiseason_occupancy/data/processed/detection_metadata.csv", row.names = FALSE)
write.csv(station, "D:/R_projects/multiseason_occupancy/data/processed/cam_station.csv", row.names = FALSE)
