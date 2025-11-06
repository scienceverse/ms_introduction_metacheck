if(!require(dplyr)){install.packages("dplyr")}
if(!require(readxl)){install.packages("readxl")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(psych)){install.packages("psych")}
if(!require(writexl)){install.packages("writexl")}

#setwd("TBA")

#Reading in data
data_screen<- read_excel("Self_verification_Screen.xlsx", col_names = TRUE) %>%
  filter(Progress==100 & age <= 40) %>% # Selects only those who completed the survey and are below or 40
  rename_at(vars(starts_with("Q2_")), funs(sub("Q2_","TSBI_", .)))

#Recoding variables
data_screen$TSBI_1R <- recode(data_screen$TSBI_1, `0`=4L, `1`= 3L, `2`= 2L, `3`= 1L,`4`= 0L)#reverse scored
data_screen$TSBI_5R <- recode(data_screen$TSBI_5, `0`=4L, `1`= 3L, `2`= 2L, `3`= 1L,`4`= 0L)#reverse scored
data_screen$TSBI_6R <- recode(data_screen$TSBI_6, `0`=4L, `1`= 3L, `2`= 2L, `3`= 1L,`4`= 0L)#reverse scored
data_screen$TSBI_12R <- recode(data_screen$TSBI_12, `0`=4L, `1`= 3L, `2`= 2L, `3`= 1L,`4`= 0L)#reverse scored
data_screen$TSBI_13R <- recode(data_screen$TSBI_13, `0`=4L, `1`= 3L, `2`= 2L, `3`= 1L,`4`= 0L)#reverse scored
data_screen$TSBI_15R <- recode(data_screen$TSBI_15, `0`=4L, `1`= 3L, `2`= 2L, `3`= 1L,`4`= 0L)#reverse scored

# Aggregating data
TSBI<-data_screen %>%
  dplyr::select(TSBI_1R, TSBI_2,TSBI_3,TSBI_4,
         TSBI_5R,TSBI_6R,TSBI_7,TSBI_8,
         TSBI_9,TSBI_10,TSBI_11,TSBI_12R,
         TSBI_13R,TSBI_14,TSBI_15R,TSBI_16)

#Reliability
psych::alpha(TSBI)

data_screen <- data_screen %>%
  mutate(TSBI=rowMeans(as.matrix(TSBI),na.rm=TRUE))
data_screen <- data_screen %>%
  arrange(TSBI)

# Saving the file
write_xlsx(data_screen, path = "/Users/Ewa/OneDrive/Autoweryfikacja OPUS/Studies/Study 5/Data/Screen/Self_verification_Screen_Select.xlsx", col_names = TRUE,
           format_headers = TRUE)
         