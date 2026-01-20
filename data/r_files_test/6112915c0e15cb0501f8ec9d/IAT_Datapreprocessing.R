################################################
#####     Project Implicit Data            #####
#####                                      #####
################################################
library(here)
library(tidyverse)
library(data.table)
library(usmap)


# RACE IATs
raceiat <- fread(here::here("Project Implicit data", "Race IAT.public.2002-2017.csv"))
raceiat <- raceiat %>% 
  mutate(STATENo = fips(STATE))  %>% # retrieve state numbers
  mutate(FIPS = as.numeric(str_c(str_pad(STATENo, 2, pad = "0"),str_pad(CountyNo, 3, pad = "0"), sep = ""))) %>% 
  filter(FIPS != "NANA" | is.na(MSANo) == FALSE) %>%  # Select only observations with fips codes
  mutate(race = case_when(raceomb==6|ethnic==5|raceomb_002 == 6|raceombmulti==6 ~ "White",
                          raceomb==5|ethnic==3|raceomb_002 == 5|raceombmulti== 5 ~ "Black", 
                          TRUE ~ "other"))%>%
  group_by(FIPS, race)%>%
  summarise(mean.dscore.ra = mean(D_biep.White_Good_all, na.rm = TRUE), sd.dscore.ra = sd(D_biep.White_Good_all, na.rm = TRUE), n.ra = n())%>%
  pivot_wider(id_cols = FIPS, names_from = race, values_from = c(mean.dscore.ra, sd.dscore.ra, n.ra))

# WEAPONS IAT
weaponsiat <- fread(here::here("Project Implicit data", "Weapons_IAT.public.2004-2018.csv"))
weaponsiat <- weaponsiat %>% 
  mutate(STATENo = fips(STATE))  %>% # retrieve state numbers
  mutate(FIPS = as.numeric(str_c(str_pad(STATENo, 2, pad = "0"),str_pad(CountyNo, 3, pad = "0"), sep = ""))) %>% 
  filter(FIPS != "NANA" | is.na(MSANo) == FALSE) %>%  # Select only observations with fips codes
  mutate(race = case_when(raceomb==6|ethnic==5|raceomb_002 == 6|raceombmulti==6 ~ "White",
                          raceomb==5|ethnic==3|raceomb_002 == 5|raceombmulti== 5 ~ "Black", 
                          TRUE ~ "other"))%>%
  group_by(FIPS, race)%>%
  summarise(mean.dscore.wp = mean(D_biep.Black_Weapons_all, na.rm = TRUE), sd.dscore.wp = sd(D_biep.Black_Weapons_all, na.rm = TRUE), n.wp = n())%>%
  pivot_wider(id_cols = FIPS, names_from = race, values_from = c(mean.dscore.wp, sd.dscore.wp, n.wp))


# Merge Race and Weapon IATs
IAT.data.by.county <- merge(raceiat, weaponsiat, by = c("FIPS"), all = TRUE)
write.csv(IAT.data.by.county, here::here("Project Implicit data","IAT.data.by.county.csv"), row.names = FALSE)
