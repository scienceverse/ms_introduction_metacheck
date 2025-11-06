# Zipcode / Municipality to County Mapping
# for Illinois (IL)

rm(list = ls())
#install.packages("stringi")
library(stringi)
setwd("~/Desktop/OP Task/Statewide IL korrigiert")

#### ILLINOIS ----
# Retrieves county information for N = 11,558,801 (90.7% of the data)
# from zipcodes (retrieved from https://www.huduser.gov/portal/datasets/usps_crosswalk.html)
# Crosswalk Type: ZIP-COUNTY
# Data Year and Quarter: 1st Quarter 2015 (OP data time range: 01/2012-12/2017)

# Read & prepare data
IL <- readRDS("yg821jf8611_il_statewide_2020_04_01.rds")
IL.zips <- read.csv2("ZIPS_IL_2015(1).csv", header = T, colClasses=c("ZIP"="character","COUNTY"="character"))
IL.zips <- IL.zips[IL.zips$COUNTY%in%seq(17000,17999,1),] # removes ZIPS belonging to other States

# Extract 5-digit zip codes from location variable
# plus-four codes are removed (e.g. 99577-0727 --> 99577)
# more or less than 5 digits --> NA
IL$zipcode <- unlist(stri_extract_all_regex(IL$location, '(?<!\\d)\\d{5}\\b'))

# County selection: One zipcode can span over multiple counties
# RES_RATIO: Ratio of resident adresses within/outside of county
# BUS_RATIO: Ratio of business adresses within/outside of county
# OTH_RATIO: Ratio of other adresses within/outside of county
# TOT_RATIO: Ratio of total adresses within/outside of county --> chosen criterion

# Selects county with highest criterion value (TOT_RATIO)
for (i in unique(IL.zips$ZIP[duplicated(IL.zips$ZIP)])) {
  cache <- subset(IL.zips, IL.zips$ZIP==i) # rows with given zipcode
  cache[cache$TOT_RATIO!=max(cache$TOT_RATIO),] <- NA # empty non-max rows
  IL.zips[IL.zips$ZIP==i&!is.na(IL.zips$ZIP),] <- cache} # write into data.frame
IL.zips <- IL.zips[rowSums(is.na(IL.zips)) != ncol(IL.zips), ] # Removes rows with only NA
rm(i,cache)

# Mapping zipcodes to county-fips
for (i in IL.zips$ZIP) {
  index <- which(IL$zipcode==i) # select rows with zipcode i
  IL[index,"FIPS"] <- IL.zips$COUNTY[IL.zips$ZIP==i]} # write respective fips-code into dataframe
rm(index,i)

# Write CSV File
write.csv(IL, "il_statewide_2020_04_01_korrigiert.csv")
