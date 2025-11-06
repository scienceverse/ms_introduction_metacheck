################################################
#####     Population Data (CENSUS)         #####
#####           2017                       #####
################################################

library(here)
library(tidyverse)
library(corrr)
library(data.table)
library(stringr)
library(usmap)

# ~~ Population Data (CENSUS) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # ----
census_data <- fread(here::here("Census", "cc-est2017-alldata.csv")) 
census_data <- census_data %>%
  filter(AGEGRP == 0) %>%
  filter(YEAR == 1) %>%
  mutate(FIPS = as.numeric(str_c(str_pad(STATE, 2, pad = "0"),str_pad(COUNTY, 3, pad = "0"), sep = "")))%>%
  select(FIPS, TOT_POP, TOT_MALE, TOT_FEMALE, BA_MALE, BA_FEMALE, WA_MALE, WA_FEMALE)
  #group_by(FIPS) %>% # average for each age group across years
  #summarise(TOT_POP = sum(TOT_POP, na.rm = TRUE), TOT_MALE = sum(TOT_MALE, na.rm = TRUE), TOT_FEMALE = sum(TOT_FEMALE, na.rm = TRUE), BA_MALE = sum(BA_MALE, na.rm = TRUE), BA_FEMALE = sum(BA_FEMALE, na.rm = TRUE), WA_MALE = sum(WA_MALE, na.rm = TRUE), WA_FEMALE = sum(WA_FEMALE, na.rm = TRUE))
write.csv(census_data, here::here("Census","census.data.csv"), row.names = FALSE)
