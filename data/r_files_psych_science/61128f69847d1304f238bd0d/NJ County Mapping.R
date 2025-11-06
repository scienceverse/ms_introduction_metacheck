# Zipcode / Municipality to County Mapping
# for Illinois (IL), Vermont (VT), Maryland (MD), Nebraska (NE), New Jersey (NJ), & Missouri (MO)

rm(list = ls())
#install.packages("stringi")
library(stringi)
library(usmap)
setwd("~/Desktop/OP Task/Statewide NJ korrigiert")

#### NEW JERSEY ----
# Retrieves county information for N = 3,549,022 (92.3% of the data) using the raw_TOWNSHIP variable
# Rest is not assignable because of multiple identically named municipalities

# Read data
NJ <- readRDS("yg821jf8611_nj_statewide_2020_04_01.rds")
NJ.mapping <- read.csv2("NJ_Coding_final.csv", header = T, stringsAsFactors = F)

# Remove municipalities that exist in more than one county and are therefore not assignable
# This unfortunately excludes N = 283,218 (7.4% of the data)
NJ.mapping <- NJ.mapping[-which(NJ.mapping$municipality%in%c("lawrence twp","hopewell twp","hamilton twp",
                                                             "franklin twp","monroe twp","ocean twp",
                                                             "springfield twp","fairfield twp")),]
NJ$raw_TOWNSHIP <- tolower(NJ$raw_TOWNSHIP) # change strings to lower case

# Municipality to county/FIPS mapping
for (i in 1:NROW(NJ.mapping)) {
  index <- which(NJ$raw_TOWNSHIP==NJ.mapping$municipality[i])
  indexed.county <- NJ.mapping$county[i]
  NJ[index,"county_name"] <- indexed.county
  NJ[index,"FIPS"] <- fips("New Jersey", indexed.county)
}
rm(i,index,indexed.county)

# Write CSV File
write.csv(NJ, "nj_statewide_2020_04_01_korrigiert.csv")
