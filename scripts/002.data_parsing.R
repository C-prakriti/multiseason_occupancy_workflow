#----------------
#Project: Multi-season occupancy analysis
#Purpose: To parse the filenames and get relevant data
#---------------

# Load required packages
library(tidyverse)
library(lubridate)
library(dplyr)

# Read the metadata csv
metadata <- read.csv("D:/R_projects/multiseason_occupancy/data/raw/img_metadata_raw.csv")
station <- read.csv("D:/R_projects/multiseason_occupancy/data/processed/cam_station.csv")
head(metadata)
str(metadata)

#------------------------------------------------------------------------------------------------------------------------
# Objective 1: Extract siteID from the FileName column such that filename is in format:C1001_2017-01-01_16_44_32-CAM60495.jpg
#------------------------------------------------------------------------------------------------------------------------
str(metadata$FileName)

# 1.1Standardize the filename
metadata$FileName_clean <- sub("-","_",metadata$FileName)
 
# 1.2 Extract siteID
metadata$gridID <- sub("[-_].*","",metadata$FileName_clean)

#-----------------------------------------------------------------------------------------------------------------------------
# Objective 2: Parsing DateTime column from character to datetime format in metadata and cam_station
#-----------------------------------------------------------------------------------------------------------------------------
str(metadata$DateTimeOriginal)

metadata$DateTimeOriginal <- as.POSIXct(
  metadata$DateTimeOriginal,
  format = "%Y:%m:%d %H:%M:%S"
)

station$deploy_date <- mdy(station$deploy_date)
station$ended_date <- mdy(station$ended_date)

#----------------------------------------------------------------------------------------------------------------------------
# Objective 3: Create a Year column from the DateTimeOriginal Column
#----------------------------------------------------------------------------------------------------------------------------
metadata$year <- format(metadata$DateTimeOriginal, "%Y")
station$year <- format(station$deploy_date, "%Y")

#----------------------------------------------------------------------------------------------------------------------------
# Objective 4: To remove the SourceFile column and FileName column and rename the datetimeoriginal column
#----------------------------------------------------------------------------------------------------------------------------
metadata <- metadata %>%
  select(-c("SourceFile", "FileName", "FileName_clean")) %>%
  rename(datetime = DateTimeOriginal)

# Export the processed image_metadata_file
write.csv(metadata, "D:/R_projects/multiseason_occupancy/data/processed/img_metadata.csv", row.names = FALSE)
write.csv(station, "D:/R_projects/multiseason_occupancy/data/processed/cam_station.csv", row.names = FALSE)




