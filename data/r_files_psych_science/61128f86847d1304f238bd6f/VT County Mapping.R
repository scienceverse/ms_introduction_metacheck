# Zipcode / Municipality to County Mapping
# for Illinois (IL), Vermont (VT), Maryland (MD), Nebraska (NE), New Jersey (NJ), & Missouri (MO)

rm(list = ls())
#install.packages("stringi")
library(stringi)
setwd("~/Desktop/OP Task/Statewide VT korrigiert")

#### VERMONT ----
# Retrieves county information for N = 282,572 (99.7% of the data)
# from zipcodes (retrieved from https://www.huduser.gov/portal/datasets/usps_crosswalk.html)
# Crosswalk Type: ZIP-COUNTY
# Data Year and Quarter: 2nd Quarter 2013 (OP data time range:  07/2010-12/2015)

# Read & prepare data
VT <- readRDS("yg821jf8611_vt_statewide_2020_04_01.rds")
VT.zips <- read.csv2("ZIPS_VT_2013(2).csv", header = T, colClasses=c("ZIP"="character","COUNTY"="character"))
VT.zips <- VT.zips[VT.zips$COUNTY%in%seq(50000,50999,1),] # removes ZIPS belonging to other States
VT.mapping <- read.csv2("VT_Coding_final.csv", header = T, stringsAsFactors = F)

# Extract last 5-digit number from location variable
# more or less than 5 digits --> NA
VT$zipcode <- unlist(stri_extract_last_regex(VT$location, '(?<!\\d)\\d{5}\\b'))

# County selection: One zipcode can span over multiple counties
# RES_RATIO: Ratio of resident adresses within/outside of county
# BUS_RATIO: Ratio of business adresses within/outside of county
# OTH_RATIO: Ratio of other adresses within/outside of county
# TOT_RATIO: Ratio of total adresses within/outside of county --> chosen criterion

# Selects county with highest criterion value (TOT_RATIO)
for (i in unique(VT.zips$ZIP[duplicated(VT.zips$ZIP)])) {
  cache <- subset(VT.zips, VT.zips$ZIP==i) # rows with given zipcode
  cache[cache$TOT_RATIO!=max(cache$TOT_RATIO),] <- NA # empty non-max rows
  VT.zips[VT.zips$ZIP==i&!is.na(VT.zips$ZIP),] <- cache} # write into data.frame
VT.zips <- VT.zips[rowSums(is.na(VT.zips)) != ncol(VT.zips), ] # Removes rows with only NA
rm(i,cache)

# Four Towns are consistently paired with zipcodes that are not included in zip-files
# Corresponding FIPS-Codes are therefore added manually (info retrieved from Wikipedia)
VT.zips <- rbind(VT.zips,c("05063","50027","1","1","1","1")) # Add zipcode for Royalton used in OP data
VT.zips <- rbind(VT.zips,c("05752","50001","1","1","1","1")) # Add zipcode for Leicester used in OP data
VT.zips <- rbind(VT.zips,c("05703","50021","1","1","1","1")) # Add wrong zipcode for Sudbury used in OP data
VT.zips <- rbind(VT.zips,c("05749","50021","1","1","1","1")) # Add wrong zipcode for Hubbardton used in OP data

# Zipcode to County Mapping
for (i in VT.zips$ZIP) {
  index <- which(VT$zipcode==i)
  VT[index,"FIPS"] <- VT.zips$COUNTY[VT.zips$ZIP==i]}
rm(index,i)

# N = 18,577 locations do not include zipcode
# N = 17,864 of these locations follow the following pattern:
# two or more segments divided by ',' containing 'VT' in the last segment and
# the name of a town in the second to last segment, which is extracted by the following code
VT.locsplit <- strsplit(VT$location[is.na(VT$FIPS)], split = ",")
VT.location_clean <- vector()

for (i in 1:length(VT.locsplit)) {
  if (length(VT.locsplit[[i]]) <= 1) {
    VT.location_clean[i] <- NA
  } else if (length(VT.locsplit[[i]])>1 & grepl(pattern = "VT", tail(VT.locsplit[[i]], n=1))) {
    VT.location_clean[i] <- tolower(trimws(VT.locsplit[[i]][length(VT.locsplit[[i]])-1], which = "left"))
  } else {
    VT.location_clean[i] <- NA
  }
}
VT$location_clean <- NA
VT$location_clean[is.na(VT$FIPS)] <- VT.location_clean

# Municipality to county mapping for N = 17,864
# Mapping data were manually retrieved from Wikipedia
# Leaves only N = 713 with unassigned county
for (i in VT.mapping$municipality) {
  index <- which(VT$location_clean==i)
  VT[index,"FIPS"] <- VT.mapping$county_fips[VT.mapping$municipality==i]}

# Write CSV File
write.csv(VT, "vt_statewide_2020_04_01_korrigiert.csv")
