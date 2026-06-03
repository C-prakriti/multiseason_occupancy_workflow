#----------------
#Project: Multi-season occupancy analysis
#Purpose: To extract metadata from the camera trap image files
#---------------

#Install and load packages
install.packages("exifr")
install.packages("dplyr")
library(exifr)
library(dplyr)

#Set the directory containing the media files
dir <- "D:/R_projects/multiseason_occupancy/leopard"

#read the image files
files <- list.files(dir,
                    pattern = "\\.jpg$",
                    full.names = TRUE,
                    ignore.case = TRUE,
                    recursive = TRUE)

# Read metadata of all images from the folder and sub-folders
metadata <- read_exif(files)

# View the metadata 
head (metadata)

# Select the required variables
data <- metadata[,c(
  "FileName",
  "SourceFile",
  "DateTimeOriginal"
)]

#Export the metadata as a csv file
write.csv (data, "D:/R_projects/multiseason_occupancy/data/raw/img_metadata_raw.csv", row.names = FALSE)


