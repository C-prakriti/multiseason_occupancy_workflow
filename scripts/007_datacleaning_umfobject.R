#---------------------------------------------------------------------------------------------
#Project: Multi-season occupancy analysis
#Purpose: To prepare the data into necessary format and prepare unmarkedMultFrame object
#------------------------------------------------------------------------------------------------

# Install and load necessary packages
library(corrplot)
library(unmarked)
library(dplyr)
library(tidyr)

# Load the necessary files
eh <- read.csv("output/encounter_history.csv")   #encounter history matrix
dist_set <- read.csv("output/dist_settlement_cov.csv")   #dynamic site cov
fcover <- read.csv("output/forestcover_loss.csv")   #dynamic site cov
ndvi <- read.csv("output/ndvi_cov.csv")     #dynamic site cov
tri <- read.csv("output/tri_cov.csv")           #static site cov
eff <- read.csv("output/effort_matrix.csv")     #observation cov
year <- read.csv("output/year_matrix.csv")      #observation cov

eh <- read.csv("encounter_history.csv")   #encounter history matrix
dist_set <- read.csv("dist_settlement_cov.csv")   #dynamic site cov
fcover <- read.csv("forestcover_loss.csv")   #dynamic site cov
ndvi <- read.csv("ndvi_cov.csv")     #dynamic site cov
tri <- read.csv("tri_cov.csv")           #static site cov
eff <- read.csv("effort_matrix.csv")     #observation cov
year <- read.csv("year_matrix.csv")      #observation cov

#------------------------------------------------------------------------------------------------------------------
# 1. Combine all dynamic covariates into a single dyn_cov
#---------------------------------------------------------------------------------------------------------------
# 1.1 Check the data structure
str(fcover)
str(ndvi)
str(dist_set)

# 1.2 Keep only required columns
fcover_clean <- fcover %>%
  select(siteID, per_fl_2017, per_fl_2018, per_fl_2021, per_fc_2017_Per_fc_207mean, per_fc_2018_Per_fc_2018mean, per_fc_2021_Per_fc_2021mean)

ndvi_clean <- ndvi %>%
  select(siteID, ndvi_2017, ndvi_2018, ndvi_2021)

dist_set_clean <- dist_set %>%
  select(siteID, dist_set_2015, dist_set_2018, dist_set_2020)

tri_clean <- tri %>%
  select(siteID, TRI_mean)

 # 1.3 Rename the variables to meaningful names
fcover_clean <- fcover_clean %>%
  rename(
    floss_2017 = per_fl_2017,
    floss_2018 = per_fl_2018,
    floss_2021 = per_fl_2021,
    fcover_2017 = per_fc_2017_Per_fc_207mean,
    fcover_2018 = per_fc_2018_Per_fc_2018mean,
    fcover_2021 = per_fc_2021_Per_fc_2021mean
  )

dist_set_clean <- dist_set_clean %>%
  rename(
    dist_set_2017 = dist_set_2015,
    dist_set_2018 = dist_set_2018,
    dist_set_2021 = dist_set_2020
  )

tri_clean <- tri_clean %>%
  rename(
    tri = TRI_mean
  )

# 1.4 Merge the dynamic covariates into one using siteID
dyn_cov <- fcover_clean %>%
  left_join(ndvi_clean,
            by = c("siteID"))

dyn_cov <- dyn_cov %>%
  left_join(dist_set_clean,
            by = "siteID")

# 1.5 Check for any missing values
colSums(is.na(dyn_cov))
summary(dyn_cov)

# 1.7 Save the dynamic covariates file
write.csv(dyn_cov, "output/dynamic_covariates.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------------------------
# 2. Standardize the covariates
#-------------------------------------------------------------------------------------------------------------------------------
str(dyn_cov)

# 2.1 Convert the distances from m to km
# Convert distances to km
dyn_cov$dist_set_2017 <- dyn_cov$dist_set_2017 / 1000
dyn_cov$dist_set_2018 <- dyn_cov$dist_set_2018 / 1000
dyn_cov$dist_set_2021 <- dyn_cov$dist_set_2021 / 1000

# 2.2 Standardize the covariates by scaling
str(tri_clean)
str(eff)

tri_z <- tri_clean 
tri_z[, -1] <- scale(tri_clean[, -1])

dyncov_z <- dyn_cov
dyncov_z[, -1] <- scale(dyn_cov[, -1])

eff_z <- eff
eff_z[, -1] <- scale(eff_z[,-1])

# 2.3 Check for the variable ranges after scaling, the properly standardized variable should have sd ~ 1 and mean ~ 0.
summary(tri_z)
sd(tri_z$tri)
mean(tri_z$tri)

summary(dyncov_z)
sd(dyncov_z$ndvi_2018)

summary(eff_z)

colSums(is.na(eff))
apply(eff[,-1], 2, sd, na.rm = TRUE)

# Since my effort occasions are majorly uniform returning NaN in the mean summary, I will be using non-scaled version of effort matrix i.e. eff moving on
# 2.4 Download the scaled csv files
write.csv(tri_z, "output/Tri_scaled.csv", row.names = FALSE)
write.csv(dyncov_z, "output/dyncov_scaled.csv", row.names = FALSE)
write.csv(eff_z, "output/effort_scaled.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------------------
#3. Check the multicollinearity among the site covariates
#-------------------------------------------------------------------------------------------------------------------------
# 3.1 Remove the siteID column from the dynamic scaled matrix
dyn_num <- dyncov_z[ , !names(dyncov_z) %in% c("siteID")]
str(dyn_num)

# 3.2. Build correlation matrix
dyncor_matrix <- cor(dyn_num[sapply(dyn_num, is.numeric)], use = "complete.obs", method = "pearson")
dyncor_matrix

# 3.3. Examining cross variable correlation within same year
cov2021 <- dyn_num[ ,c("dist_set_2021", "fcover_2021", "floss_2021", "ndvi_2021" )]
cov2021_cor <- cor(cov2021, method = "pearson")
cov2021_cor
# Here, value greater than 0.7 will be considered as highly correlated. In our case fcover and ndvi show high correlation.

# 3.4. Examining temporal correlation across years
dist_set <- dyn_num[ , c("dist_set_2017", "dist_set_2018", "dist_set_2021")]
dist_set_cor <- cor(dist_set, method = "pearson")
dist_set_cor

# 3.5 Compare static covariate tri with the dynamic covariates of one year
site_covs <- data.frame(
  tri_z = tri_z$tri,
  ndvi_2021 = dyn_num$ndvi_2021,
  fcover_2021 = dyn_num$fcover_2021,
  floss_2021 = dyn_num$floss_2021
)
cor(site_covs)

# Same variable across years show very high correlation, inferring that there was no drastic change in the variable over the years.

# 3.6. Visualization of correlations
corrplot(dyncor_matrix, method = "color", type = "upper", tl.cex = 0.6)

#-----------------------------------------------------------------------------------------------------------------------------------
#4. Convert encounter history matrices into a standard format
#-----------------------------------------------------------------------------------------------------------------------------------
str(eh)

# 4.1 If any column except site_year is other than integer, then fix that logic column to integer
eh[,2] <- as.integer(eh[,2])
str(eh)

# 4.2 Separate year column
eh <- eh %>%
  separate(site_year,
           into = c("Site", "Year"),
           sep = "_") 

# 4.3 Create master site list
all_sites <- data.frame(
  Site = sort(unique(eh$Site))
)

# 4.4 Split by year
y2017 <- subset(eh, Year == 2017)
y2018 <- subset(eh, Year == 2018)
y2021 <- subset(eh, Year == 2021)

# 4.5 If you have any extra columns at this point, keep only site and detection columns
str(y2017)
y2017 <- y2017[, c("Site", paste0("occ_", 1:8))]
y2018 <- y2018[,c("Site", paste0("occ_", 1:8))]
y2021 <- y2021[, c("Site", paste0("occ_", 1:8))]

# 4.5 Rename columns by year to prevent duplicate column names after merging
names(y2017)[-1] <- paste0("Y2017_", 1:8)
names(y2018)[-1] <- paste0("Y2018_", 1:8)
names(y2021)[-1] <- paste0("Y2021_", 1:8)

# 4.6 Merge all years into master list
master <- all_sites

master <- merge(master, y2017,
                by = "Site",
                all.x = TRUE)
master <- merge(master, y2018,
                by = "Site",
                all.x = TRUE)
master <- merge(master, y2021,
                by = "Site",
                all.x = TRUE)

# 4.7 Sort sites
master <- master[order(master$Site), ]

# 4.8 Create detection matrix
y <- as.matrix(master[, -1])
dim(y)
str(y)
y

#---------------------------------------------------------------------------------------------------------------------------
#5. Convert detection covariate matrices into standard format (Using the same workflow as above)
#---------------------------------------------------------------------------------------------------------------------------
str(eff)

# 4.1 Separate year column
eff <- eff %>%
  separate(site_year,
           into = c("Site", "Year"),
           sep = "_") 

# 4.2 Create master site list
all_sites <- data.frame(
  Site = sort(unique(eh$Site))
)

# 4.4 Split by year
y2017 <- subset(eff, Year == 2017)
y2018 <- subset(eff, Year == 2018)
y2021 <- subset(eff, Year == 2021)

# 4.5 If you have any extra columns at this point, keep only site and detection columns
str(y2017)
y2017 <- y2017[, c("Site", paste0("occ_", 1:8))]
y2018 <- y2018[,c("Site", paste0("occ_", 1:8))]
y2021 <- y2021[, c("Site", paste0("occ_", 1:8))]

# 4.5 Rename columns by year to prevent duplicate column names after merging
names(y2017)[-1] <- paste0("Y2017_", 1:8)
names(y2018)[-1] <- paste0("Y2018_", 1:8)
names(y2021)[-1] <- paste0("Y2021_", 1:8)

# 4.6 Merge all years into master list
master <- all_sites

master <- merge(master, y2017,
                by = "Site",
                all.x = TRUE)
master <- merge(master, y2018,
                by = "Site",
                all.x = TRUE)
master <- merge(master, y2021,
                by = "Site",
                all.x = TRUE)

# 4.7 Sort sites
master <- master[order(master$Site), ]

# 4.8 Create detection matrix
eff <- as.matrix(master[, -1])
dim(eff)
str(eff)
eff

# Repeat the workflow for year matrix. The dimension of the effort and year covariate should be same as that of the encounter history matrix.
str(year)

# 4.1 Separate year column
year <- year %>%
  separate(site_year,
           into = c("Site", "Year"),
           sep = "_") 

# 4.2 Create master site list
all_sites <- data.frame(
  Site = sort(unique(eh$Site))
)

# 4.4 Split by year
y2017 <- subset(year, Year == 2017)
y2018 <- subset(year, Year == 2018)
y2021 <- subset(year, Year == 2021)

# 4.5 If you have any extra columns at this point, keep only site and detection columns
str(y2017)
y2017 <- y2017[, c("Site", paste0("occ_", 1:8))]
y2018 <- y2018[,c("Site", paste0("occ_", 1:8))]
y2021 <- y2021[, c("Site", paste0("occ_", 1:8))]

# 4.5 Rename columns by year to prevent duplicate column names after merging
names(y2017)[-1] <- paste0("Y2017_", 1:8)
names(y2018)[-1] <- paste0("Y2018_", 1:8)
names(y2021)[-1] <- paste0("Y2021_", 1:8)

# 4.6 Merge all years into master list
master <- all_sites

master <- merge(master, y2017,
                by = "Site",
                all.x = TRUE)
master <- merge(master, y2018,
                by = "Site",
                all.x = TRUE)
master <- merge(master, y2021,
                by = "Site",
                all.x = TRUE)

# 4.7 Sort sites
master <- master[order(master$Site), ]

# 4.8 Create detection matrix
year <- as.matrix(master[, -1])
dim(year)
str(year)
year

#----------------------------------------------------------------------------
#5. Create yearlySiteCovs from the dynamic covariates
#--------------------------------------------------------------------------------------

#5.1 Check for the alignment of rows of the site covariates with the encounter history matrix
str(master)
str(dyncov_z)

dyncov_z <- dyncov_z[
  match(master$Site,
        dyncov_z$siteID),
]

all(master$Site == dyncov_z$siteID)

# 5.2 Check the order of the rows in the matrices
head(master$Site, 10)
head(dyncov_z, 10)
tail(master$Site, 10)
tail(dyncov_z, 10)

# 5.3 Create separate matrices for the different site covariates
dim(y)
# 5.3.1 Distance to settlement
dist_set_mat <- as.matrix(
  dyncov_z[,c(
    "dist_set_2017",
    "dist_set_2018",
    "dist_set_2021"
  )]
)

dim(dist_set_mat)
str(dist_set_mat)

# 5.3.2 Forest cover
fcov_mat <- as.matrix(
  dyncov_z[,c(
    "fcover_2017",
    "fcover_2018",
    "fcover_2021"
  )]
)
dim(fcov_mat)
str(fcov_mat)

# 5.3.3 Forest loss
floss_mat <- as.matrix(
  dyncov_z[,c(
    "floss_2017",
    "floss_2018",
    "floss_2021"
  )]
)
dim(floss_mat)
str(floss_mat)

# 5.3.4 NDVI
ndvi_mat <- as.matrix(
  dyncov_z[,c(
    "ndvi_2017",
    "ndvi_2018",
    "ndvi_2021"
  )]
)
dim(ndvi_mat)
str(ndvi_mat)

# 5.4 Create yearlySiteCovs
yearlySiteCovs <- list(
  dist_set = dist_set_mat,
  fcover = fcov_mat,
  floss = floss_mat,
  ndvi = ndvi_mat
)

str(yearlySiteCovs)

# 5.5 Verify that site order matches the encounter history matrix
rownames(y) <- master$Site
rownames(eff) <- master$Site
rownames(year) <- master$Site
rownames(dist_set_mat) <- master$Site
rownames(fcov_mat) <- master$Site
rownames(floss_mat) <- master$Site
rownames(ndvi_mat) <- master$Site

#------------------------------------------------------------------------------------------------
# 6. Create statCovs data frame and list observation covariates
#--------------------------------------------------------------------------------------------------
statCovs <- data.frame(
  tri_z = tri_z$tri
)
nrow(statCovs)

obsCovs <- list(
  effort = eff,
  year = year
)

all(rownames(y) == rownames(statCovs$tri_z))

# 5.6 Final structural check
stopifnot(nrow(y) == nrow(statCovs))
stopifnot(all(dim(eff) == dim(y)))
stopifnot(all(dim(year) == dim(y)))
stopifnot(all(sapply(yearlySiteCovs, nrow) == nrow(y)))
stopifnot(all(sapply(yearlySiteCovs, ncol) == 3))

#-------------------------------------------------------------------------------------------------------------------------------
# 7. Create unmarkedMultFrame
#-------------------------------------------------------------------------------------------------------------------------------
# 7.1 Check the classes of our variables
names(statCovs)
class(y)
class(statCovs)
class(obsCovs)
class(yearlySiteCovs)

str(obsCov)
str(yearlySiteCovs)

dim(y)
dim(statCovs)
lapply(obsCovs, dim)
lapply(yearlySiteCovs, dim)

# 7.2 Then create unmarkedMultFrame():
umf <- unmarkedMultFrame(
  y = y,
  siteCovs = statCovs,
  yearlySiteCovs = yearlySiteCovs,
  obsCovs = obsCovs,
  numPrimary = 3
)
#----------------------------------------------------------------------------------------------------------------------------
# 6. Quick checks
#---------------------------------------------------------------------------------------------------------------------------
umf
class(umf)
summary(umf)
dim(getY(umf))
obsCovNames(umf)
siteCovNames(umf)
sum(is.na(getY(umf)))

packageVersion("unmarked")

#------------------------------------------------------------------------------------------------------------------------------
# 7. Save the umf data as a R object
#-----------------------------------------------------------------------------------------------------------------------------
saveRDS(umf, "D:/R_projects/multiseason_occupancy/output/umf_object.rds")
