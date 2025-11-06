# ~~ Abschnitt 0: Header ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----
# Ursprungsskript 12.6.2019 Carsten Sander
# Letzte Änderung: 08.10.2021 Marleen Stelter

library(data.table)
library(tools)
library(plyr)
library(lubridate)
library(here)
library(lutz)
library(usmap)
library(suncalc)
library(tidyverse)

here::here()

# ~~ 1: Overview over data sets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----

# Create list with names of all datasets (datasets were downloaded here: https://openpolicing.stanford.edu/data/ on Dec-16-2020 and saved in the folder ("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Statewide"))
files.statewide <- list.files(path = here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Statewide"))

# Read first row of all data sets in list to give an overview over existing variables

# info.data is a dataset that includes names and paths of all data sets
info.data.statewide <- c(here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Statewide", files.statewide))
info.data.statewide <- data.frame("names"= substr(c(files.statewide),1,12),"path" = info.data.statewide,stringsAsFactors = F)

# info.vars is a list that includes names of data sets and their variable names
d <- list()
info.vars.statewide <- list("dataset" = list(), "names" = list())

# Create overview of variables for state patrol data
for (i in 1:NROW(info.data.statewide)) {
  d[[i]] <- assign(paste("d", info.data.statewide$names[i], sep = "_"), read.csv(info.data.statewide$path[i],nrows = 1))
  info.vars.statewide$dataset[[i]] <- info.data.statewide$names[i]
  info.vars.statewide$names[[i]] <- t(colnames(d[[i]]))}

# remove individual data sets
rm(list = paste("d_",info.data.statewide$names,sep = ""))

# Create table with frequencies of variables in all data sets
freq.vars.statewide <- as.data.frame(table(unlist(info.vars.statewide$names)))
freq.vars.statewide[order(-freq.vars.statewide$Freq),]

# Variables included in at least one of the data sets:
# "arrest_made"                "beat"                       "citation_issued"            "contraband_alcohol"         "contraband_drugs"          
# "contraband_found"           "contraband_other"           "contraband_weapons"         "county_name"                "date"                      
# "department_id"              "department_name"            "disposition"                "district"                   "division"                  
# "frisk_performed"            "lat"                        "lng"                        "location"                   "neighborhood"              
# "officer_age"                "officer_assignment"         "officer_id_hash"            "officer_race"               "officer_sex"               
# "officer_years_of_service"   "outcome"                    "police_grid_number"         "posted_speed"               "precinct"                  
# "raw_row_number"             "reason_for_arrest"          "reason_for_frisk"           "reason_for_search"          "reason_for_stop"           
# "region"                     "search_basis"               "search_conducted"           "search_person"              "search_vehicle"            
# "sector"                     "service_area"               "speed"                      "subdistrict"                "subdivision"               
# "subject_age"                "subject_race"               "subject_sex"                "subsector"                  "substation"                
# "time"                       "type"                       "unit"                       "use_of_force_description"   "use_of_force_reason"       
# "vehicle_color"              "vehicle_make"               "vehicle_model"              "vehicle_registration_state" "vehicle_type"              
# "vehicle_year"               "violation"                  "warning_issued"             "zone"     


# ~~ 2: Read raw data and merge  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----

#### GEOGRAPHIC INFORMATION ####
# read table with county geography information (latitude, longitude), retrieved from https://en.wikipedia.org/wiki/User:Michael_J/County_table
county_geography <- fread(here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "county_geography.csv"), select = c("FIPS", "Longitude", "Latitude"))
names(county_geography) <- c("FIPS", "lon", "lat")
county_geography$lon <- as.numeric(county_geography$lon) # convert to numeric
county_geography$lat <- as.numeric(county_geography$lat)  # convert to numeric
county_geography$tz <- lutz::tz_lookup_coords(county_geography$lat, county_geography$lon, warn = F) # add time zone for each county

# create empty data frames
data.statewide.excluded <- data.frame(state = character(), county_name = logical(), subject_race = logical())


#### Read State Patrol data sets, Compute daylight times, select variables and merge. ####

# if data file does not include "county_name" or "subject_race", create an entry in "excluded", in which the state name is listed and exclude file from further analysis
for (i in 1:NROW(info.data.statewide)) {
  cache <- fread(info.data.statewide$path[1]) # read data file
if("county_name" %in% names(cache) == FALSE |"subject_race" %in% names(cache) == FALSE) {
  excluded <- data.frame(info.data.statewide$names[i], "county_name" %in% names(cache), "subject_race" %in% names(cache))
  names(excluded)<-c("state","county_name", "subject_race")
  data.statewide.excluded <- rbind(data.statewide.excluded, excluded) # add data to data.statewide.excluded file
  # delete cache files
  rm(cache, excluded)
  }
}  
write.csv(data.statewide.excluded, "data.statewide.excluded.csv", row.names = FALSE)

# read in all other files
for (i in 30:NROW(info.data.statewide)) { # this loop takes up a lot of computational power. Alternatively, the code can be applied to individial state files, using the line of code below
 #for (i in 1:1) { # activate this line and select the respective state in case of insufficient computational power
   # this code does not apply to IL, NJ, and VT (see separate code for these states below)
   if (info.data.statewide$names[i] !=  "il_statewide" & info.data.statewide$names[i] !=  "vt_statewide"  & info.data.statewide$names[i] !=  "nj_statewide"){

      # read data file
     cache <- fread(info.data.statewide$path[i])
     
     # if data file does not include "county_name" or "subject_race", create an entry in "excluded", in which the state name is listed and exclude file from further analysis
     if("county_name" %in% names(cache) == FALSE |"subject_race" %in% names(cache) == FALSE) {rm(cache)}
     
     # for all other files, 
     else {
       # clean up county names delete non-existing county names from data files
       cache$county_name <- gsub( "ST ", "ST. ", cache$county_name) # add "." "st xxx" in county names
       cache$county_name <- gsub( "St ", "St. ", cache$county_name) # add "." "st xxx" in county names
       cache[cache$county_name == "Dillion",]$county_name <- "Dillon"       
       cache[cache$county_name == "Dillion County",]$county_name <- "Dillon County"
       cache[cache$county_name == "LINCOLN #2",]$county_name <- "LINCOLN"
       cache[cache$county_name == "Tyrell County",]$county_name <- "Tyrrell County"
       cache[cache$county_name == "Claredon",]$county_name <- "Clarendon"
       cache[cache$county_name == "Claredon County",]$county_name <- "Clarendon County"
       cache[cache$county_name == "Albermarle County",]$county_name <- "Albemarle County"
       cache[cache$county_name == "Lincoln #2 County",]$county_name <- "Lincoln County"
       # move to "Cleaning up Wyoming"
       cache[cache$county_name == "Laramie  County",]$county_name <- "Laramie County"
       cache[cache$county_name == "Campbell So County",]$county_name <- "Campbell County"
       # remove the following county names, because unable to retrieve FIPS (most of them are city names)
       if(info.data.statewide$names[i] == "co_statewide") {cache <- cache[cache$county_name != "Academy County",]}
       if(info.data.statewide$names[i] == "fl_statewide") {cache <- cache[cache$county_name != "BALDWIN" & cache$county_name != "Baldwin County" & cache$county_name != "Saint Lucie County" &
                                                                            cache$county_name != "Charlton County" & cache$county_name != "Saint Johns County" & cache$county_name != "Dekalb County" & 
                                                                            cache$county_name != "De Soto County" & cache$county_name != "Camden County" &
                                                                            cache$county_name != "THOMAS" & cache$county_name != "Thomas County",]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "ga_statewide") {cache <- cache[cache$county_name != "G213 County" & cache$county_name != "G115 County" &
                                                                            cache$county_name != "G047 County" & cache$county_name != "G223 County" &
                                                                            cache$county_name != "G059 County" & cache$county_name != "G143 County" &
                                                                            cache$county_name != "G139 County" ,]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "mt_statewide") {cache <- cache[cache$county_name != "SALISH & KOOTENAI" & cache$county_name != "Salish & Kootenai County",]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "mt_statewide") {cache <- cache[cache$county_name != "BLACKFEET NATION" & cache$county_name != "Blackfeet Nation County",]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "mt_statewide") {cache <- cache[cache$county_name != "FORT PECK"& cache$county_name != "Fort Peck County",]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "sc_statewide") {cache <- cache[cache$county_name != "0 County",]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "ny_statewide") {cache <- cache[cache$county_name != "St-17 County",]} # remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "wy_statewide") {cache <- cache[cache$county_name != "Fairfield County",]}# remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "wy_statewide") {cache <- cache[cache$county_name != "Driggs County",]}# remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "wy_statewide") {cache <- cache[cache$county_name != "Henderson County",]}# remove this county, because unable to retrieve FIPS code
       if(info.data.statewide$names[i] == "wy_statewide") {cache <- cache[cache$county_name != "Salt Lake City County",]}# remove this county, because unable to retrieve FIPS code
       
       
       # add FIPS codes and merge with geography data
       if ("lat" %in% names(cache) == TRUE) cache$lat <- NULL # remove existing latitude variable
       if ("lon" %in% names(cache) == TRUE) cache$lon <- NULL # remove existing longitude variable
       county_name <- as.data.frame(na.omit(unique(cache$county_name))) # create list of all counties in cache data file
       names(county_name)[names(county_name) == "na.omit(unique(cache$county_name))"] <- "county_name" # rename variable
       county_name$county_name <- as.character(county_name$county_name)
       county_name <- subset(county_name, county_name != 0)
       county_name_subset <- data.frame(county_name = character(), FIPS = numeric()) # create empty data frame and fill with FIPS codes
       for (j in 1:NROW(county_name)) { 
         FIPS <- merge(county_name[j,], as.numeric(as.character(fips(substr(info.data.statewide$names[1], 1, 2), county_name[j,])))) # look up FIPS code from usmaps package using fips()
         names(FIPS) <- c("county_name", "FIPS")
         county_name_subset <- rbind(county_name_subset, FIPS)
       }
       cache <- merge(cache, county_name_subset, by = ("county_name"), all.x = TRUE) # merge cache data file with FIPS numbers
       cache <- merge(cache, county_geography, by = ("FIPS"), all.x = TRUE) # merge cache data file with geography data (lat and lon) and time zones
       rm(county_name_subset, FIPS, county_name) # clean up
    
    # If the file contains "time" and time is not all NA, compute sunlight times
    if("time" %in% names(cache) == TRUE & length(cache$time) != length(cache[is.na(cache$time),]$time)) {
      # for each time zone in cache data file, compute sunset and sunrise times
      cache$date <- as.Date.character(cache$date) # convert date to Date format YYYY-MM-DD, to pass on to getSunlightTimes-function
      sunlight <- data.frame() # create data frame for sunlight times
      tz_counties <- na.omit(unique(cache$tz)) # extract different time zones in data file
      # for each time zone, compute sunlight times for each day
      for (k in 1:NROW(tz_counties)) { 
        sunlight_subset <- cache %>%
          select(date, lat, lon, tz) %>%
          filter(tz == tz_counties[k])%>%
          unique()
          sunlight_subset <- getSunlightTimes(data = sunlight_subset, tz = tz_counties[k], keep = c("dawn", "dusk", "sunrise", "sunset"))
          sunlight_subset$date <- as.character(sunlight_subset$date)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
          sunlight_subset$dusk <- as.character(sunlight_subset$dusk)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
          sunlight_subset$dawn <- as.character(sunlight_subset$dawn)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
          sunlight <- bind_rows(sunlight, sunlight_subset) # sunlight contains sunrise and sunset times for each date and county in cache data file
          rm(sunlight_subset)
      }
      cache$date <- as.character(cache$date)
      cache <- merge(cache, sunlight, by = c("date", "lat", "lon"), all.x = TRUE) # merge cache data with sunlight times
      cache$day_night <- character(length(cache$time)) # create new variable for day/night
      cache$time.numeric <- as.numeric(gsub(":", "", cache$time))
      cache$dawn.numeric <- as.numeric(gsub(":", "", substr(cache$dawn,12,19)))
      cache$dusk.numeric <- as.numeric(gsub(":", "", substr(cache$dusk,12,19)))
      cache$sunrise.numeric <- as.numeric(gsub(":", "", substr(cache$sunrise,12,19)))
      cache$sunset.numeric <- as.numeric(gsub(":", "", substr(cache$sunset,12,19)))
      cache[cache$time.numeric < cache$dawn.numeric | cache$time.numeric > cache$dusk.numeric,]$day_night <- "night"
      cache[cache$time.numeric > cache$sunrise.numeric & cache$time.numeric < cache$sunset.numeric]$day_night <- "day"
      cache[cache$time.numeric >= cache$dawn.numeric & cache$time.numeric <= cache$sunrise.numeric | cache$time.numeric >= cache$sunset.numeric & cache$time.numeric <= cache$dusk.numeric]$day_night <- "ambiguous"
      
      # Prepare data for intertwilight period
      time_to_minute <- function(time) {
        hour(hms(time)) * 60 + minute(hms(time))
      }
      
      intertwilight.individual <- cache %>% 
        subset(., !is.na(time) & !is.na(dusk) & !is.na(sunset)) %>% # Exclude cases without time data
        mutate(., minute = time_to_minute(time), # change time format to minutes
               dusk_minute = time_to_minute(substr(dusk, 12, 19)),
               sunset_minute = time_to_minute(substr(sunset, 12, 19)),
               is_dark = minute >= dusk_minute, # binary variable that has value TRUE if it was dark at the time of stop
               min_dusk_minute = min(dusk_minute), # earliest time of dusk
               max_dusk_minute = max(dusk_minute), # latest time of dusk
               is_black = subject_race == "black") %>% # binary variable that has value TRUE if subject was black
        filter(., minute >= min_dusk_minute, minute <= max_dusk_minute, # Filter to get only the intertwilight period
               !(minute > sunset_minute & minute < dusk_minute)) # Remove ambiguous period between sunset and dusk
      write.csv(intertwilight.individual, here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Intertwilight.individual",paste(info.data.statewide$names[1],".csv")), row.names = FALSE)
      rm(sunlight)
      
      # aggregate intertwilight data by county_name, subject_race and other grouping variables (i.e., count number of cases for combinations of grouping variables)
      intertwilight.aggregated <- ddply(intertwilight.individual, .(FIPS, county_name, subject_race, subject_race, is_dark, type), summarize, n = length(date))
      intertwilight.aggregated$state <- toupper(substr(info.data.statewide$names[1], 1, 2)) # save name of the state
      intertwilight.aggregated$county_name <- as.character(intertwilight.aggregated$county_name)  
      write.csv(intertwilight.aggregated, here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Intertwilight.aggregated",paste(info.data.statewide$names[i],".csv")), row.names = FALSE)
      }
    
    # check which grouping variables are available (subject_sex, type, outcome, or day_night) and aggregate
    variables <- c("FIPS", "county_name", "subject_race") # create vector for group variables
    if ("subject_sex" %in% names(cache) == TRUE) {variables <- c(variables, "subject_sex")} # if sex is included, add sex to grouping variables
    if ("type" %in% names(cache) == TRUE) {variables <- c(variables, "type")} # if type is included, add type to grouping variables
    if ("outcome" %in% names(cache) == TRUE) {variables <- c(variables, "outcome")} # if outcoume is included, add outcome to grouping variables
    if ("citation_issued" %in% names(cache) == TRUE) {variables <- c(variables, "citation_issued")} # if outcoume is included, add outcome to grouping variables
    if ("warning_issued" %in% names(cache) == TRUE) {variables <- c(variables, "warning_issued")} # if outcoume is included, add outcome to grouping variables
    if ("arrest_made" %in% names(cache) == TRUE) {variables <- c(variables, "arrest_made")} # if outcoume is included, add outcome to grouping variables
    if ("reason_for_stop" %in% names(cache) == TRUE) {variables <- c(variables, "reason_for_stop")} # if outcoume is included, add outcome to grouping variables
    if ("day_night" %in% names(cache) == TRUE) {variables <- c(variables, "day_night")} # if outcome is included, add outcome to grouping variables
    
    # aggregate main data by county_name, subject_race and other grouping variables (i.e., count number of cases for combinations of grouping variables)
    cache <- ddply(cache, c(variables), summarize, n = length(date))
    cache$state <- toupper(substr(info.data.statewide$names[1], 1, 2)) # save name of the state
    cache$county_name <- as.character(cache$county_name)  
    # create final data set
    data.statewide <- data.frame(state = character(), FIPS = integer(), county_name = character(), subject_race = character(), subject_sex = character(), type = character(), outcome= character(), day_night = character())
    data.statewide <- rbind.fill(data.statewide, cache) # add data to data.statewide file
    write.csv(data.statewide, here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Statewide_aggregated",paste(info.data.statewide$names[1],".csv")), row.names = FALSE)
    
    # delete cache files
    rm(cache, variables, data.statewide, intertwilight.individual, intertwilight.aggregated)
  }
 }
}


##### Il, NJ and VT are treated separately, FIPS codes are retrieved from Zip codes ####

############
# VT
############
cache <- fread(here::here("CSV-Dateien", "Preprocessing of raw data","Statewide VT korrigiert" ,"vt_statewide_2020_04_01_korrigiert.csv"))

#merge with geography data
cache$lat <- NULL # remove existing latitude variable
cache$lng <- NULL # remove existing longitude variable
cache <- merge(cache, county_geography, by = ("FIPS"), all.x = TRUE)

# If the file contains "time" and time is not all NA, compute sunlight times
if("time" %in% names(cache) == TRUE & length(cache$time) != length(cache[is.na(cache$time),]$time)) {
  # for each time zone in cache data file, compute sunset and sunrise times
  cache$date <- as.Date.character(cache$date) # convert date to Date format YYYY-MM-DD, to pass on to getSunlightTimes-function
  sunlight <- data.frame() # create data frame for sunlight times
  tz_counties <- na.omit(unique(cache$tz)) # extract different time zones in data file
  # for each time zone, compute sunlight times for each day
  for (k in 1:NROW(tz_counties)) { 
    sunlight_subset <- cache %>%
      select(date, lat, lon, tz) %>%
      filter(tz == tz_counties[k])%>%
      unique()
    sunlight_subset <- getSunlightTimes(data = sunlight_subset, tz = tz_counties[k], keep = c("dawn", "dusk", "sunrise", "sunset"))
    sunlight_subset$date <- as.character(sunlight_subset$date)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
    sunlight_subset$dusk <- as.character(sunlight_subset$dusk)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
    sunlight_subset$dawn <- as.character(sunlight_subset$dawn)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
    sunlight <- bind_rows(sunlight, sunlight_subset) # sunlight contains sunrise and sunset times for each date and county in cache data file
    rm(sunlight_subset)
  }
  cache$date <- as.character(cache$date)
  cache <- merge(cache, sunlight, by = c("date", "lat", "lon"), all.x = TRUE) # merge cache data with sunlight times
  cache$day_night <- character(length(cache$time)) # create new variable for day/night
  cache$time.numeric <- as.numeric(gsub(":", "", cache$time))
  cache$dawn.numeric <- as.numeric(gsub(":", "", substr(cache$dawn,12,19)))
  cache$dusk.numeric <- as.numeric(gsub(":", "", substr(cache$dusk,12,19)))
  cache$sunrise.numeric <- as.numeric(gsub(":", "", substr(cache$sunrise,12,19)))
  cache$sunset.numeric <- as.numeric(gsub(":", "", substr(cache$sunset,12,19)))
  cache[cache$time.numeric < cache$dawn.numeric | cache$time.numeric > cache$dusk.numeric,]$day_night <- "night"
  cache[cache$time.numeric >= cache$dawn.numeric & cache$time.numeric <= cache$dusk.numeric]$day_night <- "day"
  
  # save separate file for intertwilight period
  #intertwilight <- cache %>%
  #  filter(time.numeric > min(dawn.numeric, na.rm = TRUE) & time.numeric < max(dawn.numeric, na.rm = TRUE)|
  #           time.numeric > min(dusk.numeric, na.rm = TRUE) & time.numeric < max(dusk.numeric, na.rm = TRUE))
  #write.csv(intertwilight, here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Statewide_aggregated","intertwilight vt_statewide.csv"), row.names = FALSE)
  #rm(sunlight)
  
  
  # check which grouping variables are available (subject_sex, type, outcome, or day_night) and aggregate
  variables <- c("FIPS", "subject_race") # create vector for group variables
  if ("subject_sex" %in% names(cache) == TRUE) {variables <- c(variables, "subject_sex")} # if sex is included, add sex to grouping variables
  if ("type" %in% names(cache) == TRUE) {variables <- c(variables, "type")} # if type is included, add type to grouping variables
  if ("outcome" %in% names(cache) == TRUE) {variables <- c(variables, "outcome")} # if outcoume is included, add outcome to grouping variables
  if ("citation_issued" %in% names(cache) == TRUE) {variables <- c(variables, "citation_issued")} # if outcoume is included, add outcome to grouping variables
  if ("warning_issued" %in% names(cache) == TRUE) {variables <- c(variables, "warning_issued")} # if outcoume is included, add outcome to grouping variables
  if ("arrest_made" %in% names(cache) == TRUE) {variables <- c(variables, "arrest_made")} # if outcoume is included, add outcome to grouping variables
  if ("reason_for_stop" %in% names(cache) == TRUE) {variables <- c(variables, "reason_for_stop")} # if outcoume is included, add outcome to grouping variables
  if ("day_night" %in% names(cache) == TRUE) {variables <- c(variables, "day_night")} # if outcoume is included, add outcome to grouping variables
  
  # aggregate by county_name, subject_race and other grouping variables (i.e., count number of cases for combinations of grouping variables)
  cache <- ddply(cache, c(variables), summarize, n = length(date))
  cache$state <- "VT" # save name of the state
  # create final data set
  data.statewide <- data.frame(state = character(), FIPS = integer(), county_name = character(), subject_race = character(), subject_sex = character(), type = character(), outcome= character(), day_night = character())
  data.statewide <- rbind.fill(data.statewide, cache) # add data to data.statewide file
  write.csv(data.statewide, here::here("CSV-Dateien", "Statewide_aggregated", "vt_statewide.csv"), row.names = FALSE)
  # delete cache files
  rm(cache, variables, data.statewide)
  
############
# IL
############
cache <- fread(here::here("CSV-Dateien", "Preprocessing of raw data","Statewide IL korrigiert" ,"il_statewide_2020_04_01_korrigiert.csv"))

#merge with geography data
cache <- merge(cache, county_geography, by = ("FIPS"), all.x = TRUE)

# If the file contains "time" and time is not all NA, compute sunlight times
if("time" %in% names(cache) == TRUE & length(cache$time) != length(cache[is.na(cache$time),]$time)) {
  # for each time zone in cache data file, compute sunset and sunrise times
  cache$date <- as.Date.character(cache$date) # convert date to Date format YYYY-MM-DD, to pass on to getSunlightTimes-function
  sunlight <- data.frame() # create data frame for sunlight times
  tz_counties <- na.omit(unique(cache$tz)) # extract different time zones in data file
  # for each time zone, compute sunlight times for each day
  for (k in 1:NROW(tz_counties)) { 
    sunlight_subset <- cache %>%
      select(date, lat, lon, tz) %>%
      filter(tz == tz_counties[k])%>%
      unique()
    sunlight_subset <- getSunlightTimes(data = sunlight_subset, tz = tz_counties[k], keep = c("dawn", "dusk", "sunrise", "sunset"))
    sunlight_subset$date <- as.character(sunlight_subset$date)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
    sunlight_subset$dusk <- as.character(sunlight_subset$dusk)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
    sunlight_subset$dawn <- as.character(sunlight_subset$dawn)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
    sunlight <- bind_rows(sunlight, sunlight_subset) # sunlight contains sunrise and sunset times for each date and county in cache data file
    rm(sunlight_subset)
  }
  cache$date <- as.character(cache$date)
  cache <- merge(cache, sunlight, by = c("date", "lat", "lon"), all.x = TRUE) # merge cache data with sunlight times
  cache$day_night <- character(length(cache$time)) # create new variable for day/night
  cache$time.numeric <- as.numeric(gsub(":", "", cache$time))
  cache$dawn.numeric <- as.numeric(gsub(":", "", substr(cache$dawn,12,19)))
  cache$dusk.numeric <- as.numeric(gsub(":", "", substr(cache$dusk,12,19)))
  cache$sunrise.numeric <- as.numeric(gsub(":", "", substr(cache$sunrise,12,19)))
  cache$sunset.numeric <- as.numeric(gsub(":", "", substr(cache$sunset,12,19)))
  cache[cache$time.numeric < cache$dawn.numeric | cache$time.numeric > cache$dusk.numeric,]$day_night <- "night"
  cache[cache$time.numeric >= cache$dawn.numeric & cache$time.numeric <= cache$dusk.numeric]$day_night <- "day"
  
  # save separate file for intertwilight period
  #intertwilight <- cache %>%
  #  filter(time.numeric > min(dawn.numeric, na.rm = TRUE) & time.numeric < max(dawn.numeric, na.rm = TRUE)|
  #           time.numeric > min(dusk.numeric, na.rm = TRUE) & time.numeric < max(dusk.numeric, na.rm = TRUE))
  #write.csv(intertwilight, here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Statewide_aggregated","intertwilight vt_statewide.csv"), row.names = FALSE)
  #rm(sunlight)

# check which grouping variables are available (subject_sex, type, outcome, or day_night) and aggregate
variables <- c("FIPS", "subject_race") # create vector for group variables
if ("subject_sex" %in% names(cache) == TRUE) {variables <- c(variables, "subject_sex")} # if sex is included, add sex to grouping variables
if ("type" %in% names(cache) == TRUE) {variables <- c(variables, "type")} # if type is included, add type to grouping variables
if ("outcome" %in% names(cache) == TRUE) {variables <- c(variables, "outcome")} # if outcoume is included, add outcome to grouping variables
if ("citation_issued" %in% names(cache) == TRUE) {variables <- c(variables, "citation_issued")} # if outcoume is included, add outcome to grouping variables
if ("warning_issued" %in% names(cache) == TRUE) {variables <- c(variables, "warning_issued")} # if outcoume is included, add outcome to grouping variables
if ("arrest_made" %in% names(cache) == TRUE) {variables <- c(variables, "arrest_made")} # if outcoume is included, add outcome to grouping variables
if ("reason_for_stop" %in% names(cache) == TRUE) {variables <- c(variables, "reason_for_stop")} # if outcoume is included, add outcome to grouping variables
if ("day_night" %in% names(cache) == TRUE) {variables <- c(variables, "day_night")} # if outcoume is included, add outcome to grouping variables

# aggregate by county_name, subject_race and other grouping variables (i.e., count number of cases for combinations of grouping variables)
cache <- ddply(cache, c(variables), summarize, n = length(date))
cache$state <- "IL" # save name of the state
# create final data set
data.statewide <- data.frame(state = character(), FIPS = integer(), county_name = character(), subject_race = character(), subject_sex = character(), type = character(), outcome= character(), day_night = character())
data.statewide <- rbind.fill(data.statewide, cache) # add data to data.statewide file
write.csv(data.statewide, here::here("CSV-Dateien", "Statewide_aggregated", "il_statewide.csv"), row.names = FALSE)
# delete cache files
rm(cache, variables, data.statewide)


############
# NJ
############
cache <- fread("CSV-Dateien", "Preprocessing of raw data","Statewide NJ korrigiert" ,"nj_statewide_2020_04_01_korrigiert.csv"))

# code race variable subject_race based on raw_RACE
cache$subject_race <- "unknown"
cache$subject_race <- ifelse(grepl("ASIAN", cache$raw_RACE) == TRUE, "asian/pacific islander", cache$subject_race)
cache$subject_race <- ifelse(grepl("BLACK", cache$raw_RACE) == TRUE, "black", cache$subject_race)
cache$subject_race <- ifelse(grepl("HISPANIC", cache$raw_RACE) == TRUE, "hispanic", cache$subject_race)
cache$subject_race <- ifelse(grepl("AMERICAN INDIAN", cache$raw_RACE) == TRUE, "other", cache$subject_race)
cache$subject_race <- ifelse(grepl("WHITE", cache$raw_RACE) == TRUE, "white", cache$subject_race)

#merge with geography data
cache <- merge(cache, county_geography, by = ("FIPS"), all.x = TRUE)

# for each time zone in cache data file, compute sunset and sunrise times
cache$date <- as.Date.character(cache$date) # convert date to Date format YYYY-MM-DD, to pass on to getSunlightTimes-function
sunlight <- data.frame() # create data frame for sunlight times
tz_counties<- na.omit(unique(cache$tz)) # extract different time zones in data file
# for each time zone, compute sunlight times for each day
for (k in 1:NROW(tz_counties)) { 
  sunlight_subset <- cache %>%
    select(date, lat, lon, tz) %>%
    filter(tz == tz_counties[k])%>%
    unique()
  sunlight_subset <- getSunlightTimes(data = sunlight_subset, tz = tz_counties[k], keep = c("dawn", "dusk"))
  sunlight_subset$dusk <- as.character(sunlight_subset$dusk)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
  sunlight_subset$dawn <- as.character(sunlight_subset$dawn)# convert time variable to character (so that it will not be corrected for time-zone in r-bind)
  sunlight <- bind_rows(sunlight, sunlight_subset) # sunlight contains sunrise and sunset times for each date and county in cache data file
  rm(sunlight_subset)
}
cache <- merge(cache, sunlight, by = c("date", "lat", "lon"), all.x = TRUE) # merge cache data with sunlight times
cache$day_night <- character(length(cache$time)) # create new variable for day/night
cache$time.numeric <- as.numeric(gsub(":", "", cache$time))
cache$dawn.numeric <- as.numeric(gsub(":", "", substr(cache$dawn,12,19)))
cache$dusk.numeric <- as.numeric(gsub(":", "", substr(cache$dusk,12,19)))
cache[cache$time.numeric < cache$dawn.numeric | cache$time.numeric > cache$dusk.numeric,]$day_night <- "night"
cache[cache$time.numeric >= cache$dawn.numeric & cache$time.numeric <= cache$dusk.numeric]$day_night <- "day"

# save separate file for intertwilight period
intertwilight <- cache %>%
  filter(time.numeric > min(dawn.numeric, na.rm = TRUE) & time.numeric < max(dawn.numeric, na.rm = TRUE)|
           time.numeric > min(dusk.numeric, na.rm = TRUE) & time.numeric < max(dusk.numeric, na.rm = TRUE))
write.csv(intertwilight, here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Statewide_aggregated","intertwilight nj_statewide.csv"), row.names = FALSE)
rm(sunlight)

# check which grouping variables are available (subject_sex, type, outcome, or day_night) and aggregate
variables <- c("FIPS", "subject_race") # create vector for group variables
if ("subject_sex" %in% names(cache) == TRUE) {variables <- c(variables, "subject_sex")} # if sex is included, add sex to grouping variables
if ("type" %in% names(cache) == TRUE) {variables <- c(variables, "type")} # if type is included, add type to grouping variables
if ("outcome" %in% names(cache) == TRUE) {variables <- c(variables, "outcome")} # if outcoume is included, add outcome to grouping variables
if ("citation_issued" %in% names(cache) == TRUE) {variables <- c(variables, "citation_issued")} # if outcoume is included, add outcome to grouping variables
if ("warning_issued" %in% names(cache) == TRUE) {variables <- c(variables, "warning_issued")} # if outcoume is included, add outcome to grouping variables
if ("arrest_made" %in% names(cache) == TRUE) {variables <- c(variables, "arrest_made")} # if outcoume is included, add outcome to grouping variables
if ("reason_for_stop" %in% names(cache) == TRUE) {variables <- c(variables, "reason_for_stop")} # if outcoume is included, add outcome to grouping variables
if ("day_night" %in% names(cache) == TRUE) {variables <- c(variables, "day_night")} # if outcoume is included, add outcome to grouping variables

# aggregate by county_name, subject_race and other grouping variables (i.e., count number of cases for combinations of grouping variables)
cache <- ddply(cache, c(variables), summarize, n = length(date))
cache$state <- "NJ" # save name of the state
# create final data set
data.statewide <- data.frame(state = character(), FIPS = integer(), county_name = character(), subject_race = character(), subject_sex = character(), type = character(), outcome= character(), day_night = character())
data.statewide <- rbind.fill(data.statewide, cache) # add data to data.statewide file
write.csv(data.statewide, here::here("Open Policing [downloaded 16.12.2020]", "CSV-Dateien", "Statewide_aggregated", "nj_statewide.csv"), row.names = FALSE)
# delete cache files
rm(cache, variables, data.statewide)


# ~~ Merge all individual state files ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----
##### merge aggregated state files into a single file ####
files.statewide.aggregated <- list.files(path = here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Statewide_aggregated"))
path.statewide.aggregated <- c(here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Statewide_aggregated", files.statewide.aggregated))
data.statewide <- data.frame(state = character(), FIPS = integer(), county_name = character(), subject_race = character(), subject_sex = character(), type = character(), outcome= character(), day_night = character(), 
                             citation_issued = logical(), warning_issued = logical(), arrest_made = logical(), reason_for_stop = character())
for (i in 1:NROW(path.statewide.aggregated)) {
  cache <- fread(path.statewide.aggregated[i]) # read data file
  data.statewide <- rbind(data.statewide, cache, fill = TRUE)
  rm(cache)
}

write.csv(data.statewide, here::here("Open Policing [downloaded 16.12.2020]", "data.statewide.csv"), row.names = FALSE)

# ~~ Prepare statewide data for further analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----

open_policing.statewide <- fread(here::here("Open Policing [downloaded 16.12.2020]", "data.statewide.csv")) # retrieve data (data preparation in separate file, see Open Policing [downloaded 13.05.2019] folder)

library(tidyverse)

# clean data
open_policing.statewide <- open_policing.statewide %>% 
  filter(type == "vehicular") %>% # include only vehicular data (exclude pedestrians)
  filter(!is.na(FIPS)) %>% # exclude cases for which there is no county information
  # The following states have been excluded in analyses reported on website open policing project (see reported reasons after exclusion of each state)
  filter(state != "GA" ) %>% # GA: "The race field on the warnings form is optional; we have only about 50% race coverage, so GA is omitted from all analyses."
  filter(state != "IA" ) %>% # IA: "Only citations have Ethnicity, which only provides information on whether the driver is Hispanic. We therefore exclude Iowa from our main analysis because race data is lacking."
  filter(state != "MI" ) %>% # MI: Driver race data has more than 50% missing data, so we excluded Michigan from the analysis in the paper.
  filter(state != "NH" ) %>% # NH: coverage rate for race only 63.9% 
  filter(state != "MD" )

write.csv(open_policing.statewide, here::here("Open Policing [downloaded 13.05.2019]", "open_policing.statewide.csv"), row.names = FALSE)


# ~~ Prepare data for veil-of darkness test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----

##### merge aggregated state files into a single file ####
files.Intertwilight.aggregated <- list.files(path = here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Intertwilight.aggregated"))
path.Intertwilight.aggregated <- c(here::here("Open Policing [downloaded 16.12.2020]","CSV-Dateien", "Intertwilight.aggregated", files.Intertwilight.aggregated))
data.Intertwilight.aggregated <- data.frame(state = character(), FIPS = integer(), county_name = character(), subject_race = character(), is_dark = logical(), type = character(), n = integer())
for (i in 1:NROW(path.Intertwilight.aggregated)) {
  cache <- fread(path.Intertwilight.aggregated[i]) # read data file
  data.Intertwilight.aggregated <- rbind(data.Intertwilight.aggregated, cache, fill = TRUE)
  rm(cache)
}
data.Intertwilight.aggregated <- data.Intertwilight.aggregated %>% 
  filter(type == "vehicular") %>% # include only vehicular data (exclude pedestrians)
  # The following states have been excluded in analyses reported on website open policing project (see reported reasons after exclusion of each state)
  filter(state != "GA" ) %>% # GA: "The race field on the warnings form is optional; we have only about 50% race coverage, so GA is omitted from all analyses."
  filter(state != "IA" ) %>% # IA: "Only citations have Ethnicity, which only provides information on whether the driver is Hispanic. We therefore exclude Iowa from our main analysis because race data is lacking."
  filter(state != "MI" ) %>% # MI: Driver race data has more than 50% missing data, so we excluded Michigan from the analysis in the paper.
  filter(state != "NH" ) %>% # NH: coverage rate for race only 63.9% 
  filter(state != "MD" )

write.csv(data.Intertwilight.aggregated, here::here("Open Policing [downloaded 16.12.2020]", "data.intertwilight.csv"), row.names = FALSE)

