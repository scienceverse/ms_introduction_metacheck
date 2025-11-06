############################################################
#####     Project Implicit Data                        #####
#####     Self-reported Prejudice and Stereotypes     #####
############################################################

library(here)
library(tidyverse)
library(corrr)
library(data.table)
library(stringr)
library(usmap)

#### Explicit Attitudes ####
# read data
selfreported.prejudice.data <- fread(here::here("Project Implicit data", "Race IAT.public.2002-2017.csv"))

selfreported.prejudice.data <- selfreported.prejudice.data %>% 
  mutate(STATENo = fips(STATE))  %>% # retrieve state numbers
  mutate(FIPS = as.numeric(str_c(str_pad(STATENo, 2, pad = "0"),str_pad(CountyNo, 3, pad = "0"), sep = ""))) %>% 
  filter(FIPS != "NANA" | is.na(MSANo) == FALSE) %>%  # Select only observations with fips codes
  mutate(race = case_when(raceomb==6|ethnic==5|raceomb_002 == 6|raceombmulti==6 ~ "White",
                          raceomb==5|ethnic==3|raceomb_002 == 5|raceombmulti== 5 ~ "Black", 
                          TRUE ~ "other")) %>%
  mutate(tblack_1to11 = tblack_1to11-1) %>% # Recode 1-11 variable into 0-10
  mutate(twhite_1to11 = twhite_1to11-1) %>%  # Recode 1-11 variable into 0-10
  rowwise() %>% 
  mutate(tblacks_all = mean(select(., tblacks, tblack_0to10, tblack_1to11), na.rm = TRUE),
         twhites_all = mean(select(., twhites, twhite_0to10, twhite_1to11), na.rm = TRUE))
  
selfreported.prejudice.data$tblacks_all <- as.numeric(rowMeans(cbind(selfreported.prejudice.data$tblacks, selfreported.prejudice.data$tblack_0to10, selfreported.prejudice.data$tblack_1to11), na.rm = TRUE)) # Combine selfreported.prejudice.data thermometer variables into one variable
selfreported.prejudice.data$twhites_all<- as.numeric(rowMeans(cbind(selfreported.prejudice.data$twhites, selfreported.prejudice.data$twhite_0to10, selfreported.prejudice.data$twhite_1to11), na.rm = TRUE)) # group1 evaluation (selfreported.prejudice.data thermometer)

#selfreported.prejudice.data$tblack_1to11 <- as.numeric(recode(selfreported.prejudice.data$tblack_1to11, "'11'='10'; '10'='9'; '9'='8'; '8'='7'; '7'='6'; '6'='5'; '5'='4'; '4'='3'; '3'='2'; '2'='1'; '1'='0';")) # Recode 1-11 variable into 0-10
#selfreported.prejudice.data$tblacks_all <- as.numeric(rowMeans(cbind(selfreported.prejudice.data$tblacks, selfreported.prejudice.data$tblack_0to10, selfreported.prejudice.data$tblack_1to11), na.rm = TRUE)) # Combine selfreported.prejudice.data thermometer variables into one variable
#selfreported.prejudice.data$twhite_1to11 <- as.numeric(recode(selfreported.prejudice.data$twhite_1to11, "'11'='10'; '10'='9'; '9'='8'; '8'='7'; '7'='6'; '6'='5'; '5'='4'; '4'='3'; '3'='2'; '2'='1'; '1'='0';")) # Recode 1-11 variable into 0-10
#selfreported.prejudice.data$group1 <- as.numeric(rowMeans(cbind(selfreported.prejudice.data$twhites, selfreported.prejudice.data$twhite_0to10, selfreported.prejudice.data$twhite_1to11), na.rm = TRUE)) # group1 evaluation (selfreported.prejudice.data thermometer)
#selfreported.prejudice.data$group2 <- as.numeric(rowMeans(cbind(selfreported.prejudice.data$tblacks_all), na.rm = TRUE)) # group2 evaluation (selfreported.prejudice.data thermometer)

# rescale to range from 0 to 3
# 0 1 2 3 4 5 6 7 8 9 10 | /3+(1/3)
# 0 0.3 0.6 0.9 1.2 1.5 1.8 2.1 2.4 2.7 3.0

selfreported.prejudice.data <- transform(selfreported.prejudice.data, twhites_all.recoded = twhites_all/(3+(1/3)), tblacks_all.recoded = tblacks_all/(3+(1/3)))
selfreported.prejudice.data$feel <- as.numeric(selfreported.prejudice.data$twhites_all.recoded - selfreported.prejudice.data$tblacks_all.recoded) # Difference score/composite score from selfreported.prejudice.data thermometers (group1 - group2); positive scores indicate group1 preference


selfreported.prejudice.data$att_5 <- as.numeric(selfreported.prejudice.data$att_5) 
selfreported.prejudice.data$att_7 <- as.numeric(selfreported.prejudice.data$att_7) 
selfreported.prejudice.data$att7 <- as.numeric(selfreported.prejudice.data$att7) 
selfreported.prejudice.data <- transform(selfreported.prejudice.data, att7.combined  = ifelse(!is.na(att_7), att_7, att7))  

# recode (see Connor et al 201; "We converted scores on both versions into scales ranging from -12 to 12 (5-point range = 0, 6, 12, 18, 24; 7-point range = 0, 4, 8, 12, 16, 20, 24) and combined them")
# Recode 5-point scale
# 1 2 3 4 5  : -3
# -2 -1 0 1 2 : *1.5
# -3 -1.5 0 1.5 3 

# Recode 7-point scale
#1 2 3 4 5 6 7 : -4
# -3 -2 -1 0 1 2 3

selfreported.prejudice.data <- transform(selfreported.prejudice.data, att7.combined.recoded = att7.combined-4, att_5.recoded = (att_5-3)*1.5)

# combine att7 und att5 tp one variable
selfreported.prejudice.data <- transform(selfreported.prejudice.data, att.5.7 = ifelse(!is.na(att7.combined.recoded), att7.combined.recoded, att_5.recoded))  

# aggregate
selfreported.prejudice.data <- selfreported.prejudice.data %>% 
  group_by(FIPS) %>%
  summarize(n.att.5.7 = sum(!is.na(att.5.7)), att.5.7 = mean(att.5.7, na.rm = TRUE), 
            n.feel = sum(!is.na(feel)), feel = mean(feel, na.rm = TRUE))

write.csv(selfreported.prejudice.data, here::here("Project Implicit data",  "selfreported.prejudice.data.csv"), row.names = FALSE)

#### Explicit Stereotypes ####
# read data
selfreported.stereotype.data <- fread(here::here("Project Implicit data", "Weapons_IAT.public.2004-2018.csv"))

selfreported.stereotype.data <- selfreported.stereotype.data %>% 
  mutate(STATENo = fips(STATE))  %>% # retrieve state numbers
  mutate(FIPS = as.numeric(str_c(str_pad(STATENo, 2, pad = "0"),str_pad(CountyNo, 3, pad = "0"), sep = ""))) %>%
  filter(raceomb==6|ethnic==5|raceomb_002 == 6|raceombmulti==6) %>% # Select White participants
  
  mutate(sharmless.recoded = sharmless-4, sweapons.recoded = sweapons-4) %>%
  group_by(FIPS) %>%
  summarise(n.sharmless = sum(!is.na(sharmless)), sharmless.recoded = mean(sharmless.recoded, na.rm = TRUE),
            n.sweapons = sum(!is.na(sweapons)), sweapons.recoded = mean(sweapons.recoded, na.rm = TRUE))

write.csv(selfreported.stereotype.data, here::here("Project Implicit data",  "selfreported.stereotype.data.csv"), row.names = FALSE)
