
loadpackage <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
    }
    #  Load package (after installing)
    library( i , character.only = TRUE )
  }
}

#  Then try/install packages...
loadpackage(c("readr", # import spreadsheet data
              "descr",
              "haven", # to read SPSS into R
              "tidyverse", # includes the packages important for every day data analyses
              "dplyr",
              "reshape"
))
## Set working directory to NZAVS data folder
setwd("~/Documents/Surveys : Data/NZAVS/Copy data_02")
rm(list=ls())


data.T11 <-read_sav("NZAVS T11 (2019) SPSS Base Dataset.sav") %>%
  dplyr::select("Questionnaire.Num","Gender.T11", "Age.T11", "EthnicCats.T11",
         # Felt belongingness
         "Rel.Belong01.T11", "Rel.Belong02r.T11", "Rel.Belong03.T11", "T11.BELONG",
         # Self-esteem
         "SWB.SelfEsteem01.T11", "SWB.SelfEsteem02.T11", "SWB.SelfEsteem03r.T11", "T11.SELF.ESTEEM",
         # Power dependence / control
         "PowerDependence01.T11", "PowerDependence02.T11",
         # Meaning in life
         "LifeMeaning01.T11", "LifeMeaning02.T11", "T11.LIFEMEANING",
         # Conspiracies
         "ConspiracyBeliefs.T11")

data.T12 <-read_sav("NZAVS T12 (2020) SPSS Base Dataset.sav") %>%
  dplyr::select("Questionnaire.Num","Gender.T12", "Age.T12", "EthnicCats.T12",
         # Felt belongingness
         "Rel.Belong01.T12", "Rel.Belong02r.T12", "Rel.Belong03.T12", "T12.BELONG",
         # Self-esteem
         "SWB.SelfEsteem01.T12", "SWB.SelfEsteem02.T12", "SWB.SelfEsteem03r.T12", "T12.SELF.ESTEEM",
         # Power dependence / control
         "PowerDependence01.T12", "PowerDependence02.T12",
         # Meaning in life
         "LifeMeaning01.T12", "LifeMeaning02.T12", "T12.LIFEMEANING",
         # Conspiracies
         "ConspiracyBeliefs.T12")

data.T13 <-read_sav("NZAVS T13 (2021) SPSS Base Dataset.sav") %>%
  dplyr::select("Questionnaire.Num","Gender.T13", "Age.T13", "EthnicCats.T13",
         # Felt belongingness
         "Rel.Belong01.T13", "Rel.Belong02r.T13", "Rel.Belong03.T13", "T13.BELONG",
         # Self-esteem
         "SWB.SelfEsteem01.T13", "SWB.SelfEsteem02.T13", "SWB.SelfEsteem03r.T13", "T13.SELF.ESTEEM",
         # Power dependencs / control
         "PowerDependence01.T13", "PowerDependence02.T13",
         # Meaning in life
         "LifeMeaning01.T13", "LifeMeaning02.T13", "T13.LIFEMEANING",
         # Conspiracies
         "ConspiracyBeliefs.T13")


## Create one data frame in wide format
data_11to13 <- merge(data.T11, data.T12, by = c("Questionnaire.Num"), all = T) %>%
  merge(data.T13, by = c("Questionnaire.Num"), all = T)


### Reverse-code power dependence to control & create one mean per wave
# 1. Reverse-code
control.vars <- colnames(data_11to13[c("PowerDependence01.T11", "PowerDependence02.T11",
                                       "PowerDependence01.T12", "PowerDependence02.T12",
                                       "PowerDependence01.T13", "PowerDependence02.T13")])

data_11to13 <- data_11to13 %>% 
  mutate(across(control.vars, function(x) 8-x, .names = "{.col}_r"))

# 2. Average control value per wave
data_11to13_min1 <- data_11to13 %>% 
  mutate (T11.CONTROL = rowMeans (dplyr::select (data_11to13, "PowerDependence01.T11_r", "PowerDependence02.T11_r" ), na.rm = TRUE))%>% 
  mutate (T12.CONTROL = rowMeans (dplyr::select (data_11to13, "PowerDependence01.T12_r", "PowerDependence02.T12_r" ), na.rm = TRUE)) %>%
  mutate (T13.CONTROL = rowMeans (dplyr::select (data_11to13, "PowerDependence01.T13_r", "PowerDependence02.T13_r" ), na.rm = TRUE)) %>%
  # Keep only ppts who have answered conspiracy belief in one of the waves
  filter (ConspiracyBeliefs.T11 %in% c(1:7) | 
          ConspiracyBeliefs.T12 %in% c(1:7) | 
          ConspiracyBeliefs.T13 %in% c(1:7))


# Save data in project working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
write.csv(data_11to13_min1,'Needs-Consp_data11_13_221110.csv', row.names=FALSE)

