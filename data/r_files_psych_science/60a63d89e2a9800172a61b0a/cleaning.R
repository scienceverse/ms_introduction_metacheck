# Script 1: Cleaning ------------------------------------------------------------------------------------------
# This script loads and cleans the ALSPAC data. 

# The script performs these tasks:
# 1. Loading libraries and data
# 2. Cleaning data
# 3. Assessing and correcting missingness
# 4. Constructing and recoding variables
# 5. Saving data

input = ""
output = ""


# 1. Loading libraries and data --------------------------------------------------------------------------------
# 1a. Load libraries ----

library(tidyverse)
library(summarytools)
library(finalfit)
library(missForest)
library(polycor)
library(corrplot)

# 1b. Load data ----

# Load all the alspac data
orig_data <- read.csv(file= paste(input,"/alspac_data_original.csv", sep = ""))

# Extract the variables from original dataset
full_proj_data <- orig_data %>%
  select(kz021,kz011b, in_core, # Participant sex, alive at one year, enrolled in phase 1
         f242a,g322a,h232a,j322a,k4022,l4022, # Partner physically cruel to mum
         f256a,g336a,h246a,j336a,k4036,l4036, # Partner emotionally cruel to mum
         f246a,g326a,h236a,j326a,k4026,l4026, # Partner physically cruel to child
         f247a,g327a,h237a,j327a,k4027,l4027, # Mum physically cruel to child
         f257a,g337a,h247a,j337,k4037,l4037, # Partner emotionally cruel to child
         f258a,g338a,h248a,j338a,k4038,l4038, # Mum emotionally cruel to child
         f800,g835,h730,k6200,m5170, # Difficulty affording food
         f801,g836,h731,k6201,m5171, # Difficulty affording clothes
         f802,g837,h732,k6202,m5172, # Difficulty affording heating
         f803,g838,h733,k6203,m5173, # Difficulty affording rent/mortgage
         f804,g839,h734,k6204,m5174, # Difficulty affording items for child
         kd504a,kf454a,kj464a,kl474,kq364a, # Physical harm by any perpetrator
         kd505a,kf455a,kj465a,kl475,kq365a, # Sexual abuse by any perpetrator 
         kd511a,kf461a,kj471a,kl481,kq372a, # Change of child's caregiver
         kd506a,kf456a,kj466a,kl476,kq367a, # Child separated from mum for > 1 wk
         kd507a,kf457a,kj467a,kl477,kq368a, # Child separated from mum's partner for > 1 wk
         kb554,kb556,kb558,kb560,kb562,kb564,kb566,	# Mum interaction variables wave 1 (6 mo)
         kb570,kb572,kb573,kb574,kb575,kb576,kb577,kb578,kb579, # Partner interaction variables wave 1 (6 mo)
         kd405,kd406,kd407,kd408,kd409,kd410,kd411,kd412,kd413, # Mum interaction variables wave 2 (18 mo)
         kd391,kd392,kd393,kd394,kd395,kd396,kd397,kd398,kd399, # Partner interaction variables wave 2 (18 mo)
         kg250,kg251,kg252,kg253,kg254,kg255,kg256,kg257,kg258, # Mum interaction variables wave 3 (38 mo)
         kg230,kg231,kg232,kg233,kg234,kg235,kg236,kg237,kg238, # Partner interaction variables wave 3 (38 mo)
         kl420,kl421,kl422,kl423,kl424,kl425,kl426,kl427,kl428,kl429,kl430,kl431, # Mum interaction variables wave 4 (57 mo)
         kl400,kl401,kl402,kl403,kl404,kl405,kl406,kl407,kl408,kl409,kl410,kl411, # Partner interaction variables wave 4 (57 mo)
         kq575,kq576,kq577,kq578,kq579,kq580,kq581,kq582,kq583,kq584,kq585,kq586,kq587,kq588,kq589,kq590,kq591,kq592, # Mum interaction variables wave 5 (81 mo)
         kq600,kq601,kq602,kq603,kq604,kq605,kq606,kq607,kq608,kq609,kq610,kq611,kq612,kq613,kq614,kq615,kq616,kq617, # Partner interaction variables wave 5 (81 mo)
         tc4025a,tc4025b,tc4025c,tc4025d, # SDQ emotional, conduct, hyperactivity, peer problems subscales at 16 yrs
         fh6280,# WASI total IQ score at 15.5 yrs
         fh6940,fh6950, # Stop-signal  correct type s, correct type ss, at 15.5 years; 
         FJNB050, FJNB100 # N-back mean accuracy to identify non-targets and targets in 2-back condition at 17 years
  ) 

# Filter only participants alive at 1 year 
full_proj_data <- full_proj_data %>% filter(kz011b == 1) 
full_proj_data <- subset(full_proj_data, select = -kz011b)  

# Filter only participants enrolled as part of original core sample, Phase 1
full_proj_data <- full_proj_data %>% filter(in_core == 1) 
full_proj_data <- subset(full_proj_data, select = -in_core)  

# Add a participant ID
full_proj_data <- tibble::rowid_to_column(full_proj_data, "pid")

# Visualize the dataset
glimpse(full_proj_data)

# 2. Cleaning data --------------------------------------------------------------------------------
# 2a.  Rename and label variables properly ----
# Rename variables
full_proj_data %<>% dplyr::rename(
  sex = kz021, # Participant sex (male = 1, female = 2)
  dom_viol_phys.1a=f242a,dom_viol_phys.1b=g322a,dom_viol_phys.2=h232a,dom_viol_phys.3=j322a,dom_viol_phys.4=k4022,dom_viol_phys.5=l4022, # Partner physically cruel to mum
  dom_viol_emo.1a=f256a,dom_viol_emo.1b=g336a,dom_viol_emo.2=h246a,dom_viol_emo.3=j336a,dom_viol_emo.4=k4036,dom_viol_emo.5=l4036, # Partner emotionally cruel to mum 
  phys_cruel_dad.1a=f246a,phys_cruel_dad.1b=g326a,phys_cruel_dad.2=h236a,phys_cruel_dad.3=j326a,phys_cruel_dad.4=k4026,phys_cruel_dad.5=l4026, # Partner physically cruel to child
  phys_cruel_mum.1a=f247a,phys_cruel_mum.1b=g327a,phys_cruel_mum.2=h237a,phys_cruel_mum.3=j327a,phys_cruel_mum.4=k4027,phys_cruel_mum.5=l4027, # Mum physically cruel to child
  emo_cruel_dad.1a=f257a,emo_cruel_dad.1b=g337a,emo_cruel_dad.2=h247a,emo_cruel_dad.3=j337,emo_cruel_dad.4=k4037,emo_cruel_dad.5=l4037, # Partner emotionally cruel to child
  emo_cruel_mum.1a=f258a,emo_cruel_mum.1b=g338a,emo_cruel_mum.2=h248a,emo_cruel_mum.3=j338a,emo_cruel_mum.4=k4038,emo_cruel_mum.5=l4038, # Mum emotionally cruel to child
  fin_diff_food.1=f800,fin_diff_food.2=g835,fin_diff_food.3=h730,fin_diff_food.4=k6200,fin_diff_food.5=m5170, # Difficulty affording food
  fin_diff_clothes.1=f801,fin_diff_clothes.2=g836,fin_diff_clothes.3=h731,fin_diff_clothes.4=k6201,fin_diff_clothes.5=m5171, # Difficulty affording clothes
  fin_diff_heat.1=f802,fin_diff_heat.2=g837,fin_diff_heat.3=h732,fin_diff_heat.4=k6202,fin_diff_heat.5=m5172, # Difficulty affording heating
  fin_diff_rent.1=f803,fin_diff_rent.2=g838,fin_diff_rent.3=h733,fin_diff_rent.4=k6203,fin_diff_rent.5=m5173, # Difficulty affording rent
  fin_diff_items.1=f804,fin_diff_items.2=g839,fin_diff_items.3=h734,fin_diff_items.4=k6204,fin_diff_items.5=m5174, # Difficulty affording items for child
  phys_abuse.1=kd504a,phys_abuse.2=kf454a,phys_abuse.3=kj464a,phys_abuse.4=kl474,phys_abuse.5=kq364a, # Physical harm by any perpetrator 
  sex_abuse.1=kd505a,sex_abuse.2=kf455a,sex_abuse.3=kj465a,sex_abuse.4=kl475,sex_abuse.5=kq365a, # Sexual abuse by any perpetrator 
  care_change.1=kd511a,care_change.2=kf461a,care_change.3=kj471a,care_change.4=kl481,care_change.5=kq372a, # Change of child's caregiver
  separate_mum.1=kd506a,separate_mum.2=kf456a,separate_mum.3=kj466a,separate_mum.4=kl476,separate_mum.5=kq367a, # Child separated from mum for > 1 wk
  separate_dad.1=kd507a,separate_dad.2=kf457a,separate_dad.3=kj467a,separate_dad.4=kl477,separate_dad.5=kq368a, # Child separated from partner for > 1 wk
  interact_mum.1a=kb554,interact_mum.1b=kb556,interact_mum.1c=kb558,interact_mum.1d=kb560,interact_mum.1e=kb562,interact_mum.1f=kb564,interact_mum.1g=kb566, # Mum's interaction score wave 1
  interact_dad.1a=kb570,interact_dad.1b=kb572,interact_dad.1c=kb573,interact_dad.1d=kb574,interact_dad.1e=kb575,interact_dad.1f=kb576,interact_dad.1g=kb577,interact_dad.1h=kb578,interact_dad.1i=kb579, # Partner's interaction score wave 2
  interact_mum.2a=kd405,interact_mum.2b=kd406,interact_mum.2c=kd407,interact_mum.2d=kd408,interact_mum.2e=kd409,interact_mum.2f=kd410,interact_mum.2g=kd411,interact_mum.2h=kd412,interact_mum.2i=kd413,
  interact_dad.2a=kd391,interact_dad.2b=kd392,interact_dad.2c=kd393,interact_dad.2d=kd394,interact_dad.2e=kd395,interact_dad.2f=kd396,interact_dad.2g=kd397,interact_dad.2h=kd398,interact_dad.2i=kd399,
  interact_mum.3a=kg250,interact_mum.3b=kg251,interact_mum.3c=kg252,interact_mum.3d=kg253,interact_mum.3e=kg254,interact_mum.3f=kg255,interact_mum.3g=kg256,interact_mum.3h=kg257,interact_mum.3i=kg258,
  interact_dad.3a=kg230,interact_dad.3b=kg231,interact_dad.3c=kg232,interact_dad.3d=kg233,interact_dad.3e=kg234,interact_dad.3f=kg235,interact_dad.3g=kg236,interact_dad.3h=kg237,interact_dad.3i=kg238,
  interact_mum.4a=kl420,interact_mum.4b=kl421,interact_mum.4c=kl422,interact_mum.4d=kl423,interact_mum.4e=kl424,interact_mum.4f=kl425,interact_mum.4g=kl426,interact_mum.4h=kl427,interact_mum.4i=kl428,interact_mum.4j=kl429,interact_mum.4k=kl430,interact_mum.4l=kl431,
  interact_dad.4a=kl400,interact_dad.4b=kl401,interact_dad.4c=kl402,interact_dad.4d=kl403,interact_dad.4e=kl404,interact_dad.4f=kl405,interact_dad.4g=kl406,interact_dad.4h=kl407,interact_dad.4i=kl408,interact_dad.4j=kl409,interact_dad.4k=kl410,interact_dad.4l=kl411,
  interact_mum.5a=kq575,interact_mum.5b=kq576,interact_mum.5c=kq577,interact_mum.5d=kq578,interact_mum.5e=kq579,interact_mum.5f=kq580,interact_mum.5g=kq581,interact_mum.5h=kq582,interact_mum.5i=kq583,interact_mum.5j=kq584,interact_mum.5k=kq585,interact_mum.5l=kq586,interact_mum.5m=kq587,interact_mum.5n=kq588,interact_mum.5o=kq589,interact_mum.5p=kq590,interact_mum.5q=kq591,interact_mum.5r=kq592,
  interact_dad.5a=kq600,interact_dad.5b=kq601,interact_dad.5c=kq602,interact_dad.5d=kq603,interact_dad.5e=kq604,interact_dad.5f=kq605,interact_dad.5g=kq606,interact_dad.5h=kq607,interact_dad.5i=kq608,interact_dad.5j=kq609,interact_dad.5k=kq610,interact_dad.5l=kq611,interact_dad.5m=kq612,interact_dad.5n=kq613,interact_dad.5o=kq614,interact_dad.5p=kq615,interact_dad.5q=kq616,interact_dad.5r=kq617,
  sdq_emo=tc4025a,sdq_conduct=tc4025b,sdq_hyper=tc4025c,sdq_peer=tc4025d, # Emotional conduct, hyperactivity, peer problems subscales of SDQ
  wasi=fh6280, # WASI total IQ score
  correct_s=fh6940,correct_ss=fh6950, # Stop-signal delay times and correct responses for s and ss trials  
  nontargets=FJNB050,targets=FJNB100, # N-back accuracy on nontargets and targets in two-back condition
)


# 2b. Re-code variables ----
# Replace invalid responses to NA
adversitycols <- c("dom_viol_phys.1a","dom_viol_phys.1b","dom_viol_phys.2","dom_viol_phys.3","dom_viol_phys.4","dom_viol_phys.5",
                   "dom_viol_emo.1a","dom_viol_emo.1b","dom_viol_emo.2","dom_viol_emo.3","dom_viol_emo.4","dom_viol_emo.5",
                   "phys_cruel_dad.1a","phys_cruel_dad.1b","phys_cruel_dad.2","phys_cruel_dad.3","phys_cruel_dad.4","phys_cruel_dad.5",
                   "phys_cruel_mum.1a","phys_cruel_mum.1b","phys_cruel_mum.2","phys_cruel_mum.3","phys_cruel_mum.4","phys_cruel_mum.5",
                   "emo_cruel_dad.1a","emo_cruel_dad.1b","emo_cruel_dad.2","emo_cruel_dad.3","emo_cruel_dad.4","emo_cruel_dad.5",
                   "emo_cruel_mum.1a","emo_cruel_mum.1b","emo_cruel_mum.2","emo_cruel_mum.3","emo_cruel_mum.4","emo_cruel_mum.5",
                   "phys_abuse.1","phys_abuse.2","phys_abuse.3","phys_abuse.4","phys_abuse.5",
                   "sex_abuse.1","sex_abuse.2","sex_abuse.3","sex_abuse.4","sex_abuse.5",
                   "care_change.1","care_change.2","care_change.3","care_change.4","care_change.5",
                   "separate_mum.1","separate_mum.2","separate_mum.3","separate_mum.4","separate_mum.5",
                   "separate_dad.1","separate_dad.2","separate_dad.3","separate_dad.4","separate_dad.5")
outcomecols <- c("sdq_emo","sdq_conduct","sdq_hyper","sdq_peer","wasi","correct_s","correct_ss","nontargets","targets")
interactcols <- c("interact_mum.1a","interact_mum.1b","interact_mum.1c","interact_mum.1d","interact_mum.1e","interact_mum.1f","interact_mum.1g",
                  "interact_dad.1a","interact_dad.1b","interact_dad.1c","interact_dad.1d","interact_dad.1e","interact_dad.1f","interact_dad.1g","interact_dad.1h","interact_dad.1i",
                  "interact_mum.2a","interact_mum.2b","interact_mum.2c","interact_mum.2d","interact_mum.2e","interact_mum.2f","interact_mum.2g","interact_mum.2h","interact_mum.2i",
                  "interact_dad.2a","interact_dad.2b","interact_dad.2c","interact_dad.2d","interact_dad.2e","interact_dad.2f","interact_dad.2g","interact_dad.2h","interact_dad.2i",
                  "interact_mum.3a","interact_mum.3b","interact_mum.3c","interact_mum.3d","interact_mum.3e","interact_mum.3f","interact_mum.3g","interact_mum.3h","interact_mum.3i",
                  "interact_dad.3a","interact_dad.3b","interact_dad.3c","interact_dad.3d","interact_dad.3e","interact_dad.3f","interact_dad.3g","interact_dad.3h","interact_dad.3i",
                  "interact_mum.4a","interact_mum.4b","interact_mum.4c","interact_mum.4d","interact_mum.4e","interact_mum.4f","interact_mum.4g","interact_mum.4h","interact_mum.4i","interact_mum.4j","interact_mum.4k","interact_mum.4l",
                  "interact_dad.4a","interact_dad.4b","interact_dad.4c","interact_dad.4d","interact_dad.4e","interact_dad.4f","interact_dad.4g","interact_dad.4h","interact_dad.4i","interact_dad.4j","interact_dad.4k","interact_dad.4l",
                  "interact_mum.5a","interact_mum.5b","interact_mum.5c","interact_mum.5d","interact_mum.5e","interact_mum.5f","interact_mum.5g","interact_mum.5h","interact_mum.5i","interact_mum.5j","interact_mum.5k","interact_mum.5l","interact_mum.5m","interact_mum.5n","interact_mum.5o","interact_mum.5p","interact_mum.5q","interact_mum.5r",
                  "interact_dad.5a","interact_dad.5b","interact_dad.5c","interact_dad.5d","interact_dad.5e","interact_dad.5f","interact_dad.5g","interact_dad.5h","interact_dad.5i","interact_dad.5j","interact_dad.5k","interact_dad.5l","interact_dad.5m","interact_dad.5n","interact_dad.5o","interact_dad.5p","interact_dad.5q","interact_dad.5r")
findiffcols <- c("fin_diff_food.1","fin_diff_food.2","fin_diff_food.3","fin_diff_food.4","fin_diff_food.5",
                 "fin_diff_clothes.1","fin_diff_clothes.2","fin_diff_clothes.3","fin_diff_clothes.4","fin_diff_clothes.5",
                 "fin_diff_heat.1","fin_diff_heat.2","fin_diff_heat.3","fin_diff_heat.4","fin_diff_heat.5",
                 "fin_diff_rent.1","fin_diff_rent.2","fin_diff_rent.3","fin_diff_rent.4","fin_diff_rent.5",
                 "fin_diff_items.1","fin_diff_items.2","fin_diff_items.3","fin_diff_items.4","fin_diff_items.5")

full_proj_data[full_proj_data == -9999] <- NA # "consent withdrawn" 
full_proj_data[full_proj_data == -1] <- NA # "missing" or "no response"
full_proj_data[full_proj_data == -10] <- NA # "not completed"
full_proj_data[full_proj_data == -11] <- NA # "triplet/quadruplet"
full_proj_data[,adversitycols][full_proj_data[,adversitycols] == -6] <- NA # "section D omitted" on some waves
full_proj_data[,adversitycols][full_proj_data[,adversitycols] == -8] <- NA # "see text" response on some waves
full_proj_data[,interactcols][full_proj_data[,interactcols] == -2] <- NA # "no partner" on dad variables
full_proj_data[,interactcols][full_proj_data[,interactcols] == -8] <- NA # "text response" on some variables
full_proj_data[,interactcols][full_proj_data[,interactcols] == 7] <- NA # "no partner" on dad variables
full_proj_data[,!names(full_proj_data) %in% c(outcomecols,"pid")][full_proj_data[,!names(full_proj_data) %in% c(outcomecols,"pid")] == 9] <- NA  # "do not know" except for outcomes
full_proj_data[,outcomecols][full_proj_data[,outcomecols] == -7] <- NA # "TF outreach"
full_proj_data[,outcomecols][full_proj_data[,outcomecols] == -6] <- NA # "did not start session"
full_proj_data[,outcomecols][full_proj_data[,outcomecols] == -5] <- NA # "did not start task"
full_proj_data[,outcomecols][full_proj_data[,outcomecols] == -4] <- NA # "did not complete task"

# Recode variables to standardize across dataset
full_proj_data[,c("phys_abuse.1","sex_abuse.1","care_change.1","separate_mum.1","separate_dad.1",
                  "emo_cruel_dad.3","dom_viol_phys.4","dom_viol_phys.5","dom_viol_emo.4","dom_viol_emo.5","phys_cruel_dad.4",
                  "phys_cruel_dad.5","phys_cruel_mum.4","phys_cruel_mum.5","emo_cruel_dad.4","emo_cruel_dad.5","emo_cruel_mum.4",
                  "emo_cruel_mum.5","phys_abuse.4","sex_abuse.4","care_change.4","separate_mum.4","separate_dad.4")] <-
  ifelse(full_proj_data[,c("phys_abuse.1","sex_abuse.1","care_change.1","separate_mum.1","separate_dad.1",
                           "emo_cruel_dad.3","dom_viol_phys.4","dom_viol_phys.5","dom_viol_emo.4","dom_viol_emo.5","phys_cruel_dad.4",
                           "phys_cruel_dad.5","phys_cruel_mum.4","phys_cruel_mum.5","emo_cruel_dad.4","emo_cruel_dad.5","emo_cruel_mum.4",
                           "emo_cruel_mum.5","phys_abuse.4","sex_abuse.4","care_change.4","separate_mum.4","separate_dad.4")] < 5, 1, 0)
full_proj_data[,c("dom_viol_phys.1a","dom_viol_phys.1b","dom_viol_phys.2","dom_viol_emo.1a","dom_viol_emo.1b","dom_viol_emo.2",
                  "phys_cruel_dad.1a","phys_cruel_dad.1b","phys_cruel_dad.2","phys_cruel_mum.1a","phys_cruel_mum.1b","phys_cruel_mum.2",
                  "emo_cruel_dad.1a","emo_cruel_dad.1b","emo_cruel_dad.2","emo_cruel_mum.1a","emo_cruel_mum.1b","emo_cruel_mum.2",
                  "phys_abuse.2","phys_abuse.3","phys_abuse.5","sex_abuse.2","sex_abuse.3","sex_abuse.5","care_change.2","care_change.3",
                  "care_change.5","separate_mum.2","separate_mum.3","separate_mum.5","separate_dad.2","separate_dad.3","separate_dad.5")] <-
  ifelse(full_proj_data[,c("dom_viol_phys.1a","dom_viol_phys.1b","dom_viol_phys.2","dom_viol_emo.1a","dom_viol_emo.1b","dom_viol_emo.2",
                           "phys_cruel_dad.1a","phys_cruel_dad.1b","phys_cruel_dad.2","phys_cruel_mum.1a","phys_cruel_mum.1b","phys_cruel_mum.2",
                           "emo_cruel_dad.1a","emo_cruel_dad.1b","emo_cruel_dad.2","emo_cruel_mum.1a","emo_cruel_mum.1b","emo_cruel_mum.2",
                           "phys_abuse.2","phys_abuse.3","phys_abuse.5","sex_abuse.2","sex_abuse.3","sex_abuse.5","care_change.2","care_change.3",
                           "care_change.5","separate_mum.2","separate_mum.3","separate_mum.5","separate_dad.2","separate_dad.3","separate_dad.5")] == 2, 0, 1)
full_proj_data$interact_dad.1a <- (full_proj_data$interact_dad.1a)*-1 + 6

# Recode financial difficulty so higher value is greater difficulty
full_proj_data[,findiffcols] <- abs(full_proj_data[,findiffcols] - 4)

# Recode binary adversity variables and sex as factors
full_proj_data[,c(adversitycols,"sex")] <- lapply(full_proj_data[,c(adversitycols,"sex")], as.factor)

# 3. Assessing and correcting missingness  ---------------------------------------------------------------------
# 3a. Identify missing values in all participants ----

# Quantify missing values 
full_proj_data %>% 
  select(1:100) %>%
  dfSummary()
full_proj_data %>% 
  select(101:219) %>%
  dfSummary()

missing_vars_full <- colMeans(is.na(full_proj_data))
missing_participants_full <- rowMeans(is.na(full_proj_data))

print(psych::describe(missing_vars_full),digits=3)
print(psych::describe(missing_participants_full),digits=3)

# Plot rates of missing data by participant
hist(missing_participants_full,
     main = "Rates of Missing Data",
     xlab = "Percentage missing",
     ylab = "Number of participants")
abline(v = mean(missing_participants_full), col = "blue", lwd = 2)

# Plot rates of missing data by variable
hist(missing_vars_full,
     main = "Rates of Missing Data",
     xlab = "Percentage missing",
     ylab = "Number of variables")
abline(v = mean(missing_vars_full), col = "blue", lwd = 2)

# Visually inspect plots for associations between missing and observed data
full_proj_data %>% missing_pairs()

# Find significant predictors of missingness in adolescence
missingness_predictors = c("sex",adversitycols,interactcols,findiffcols)
missingness_comparisons = length(missingness_predictors) 
missingness_pval = 0.05/missingness_comparisons 
full_missing_wasi <- full_proj_data %>% missing_compare("wasi",missingness_predictors,digits = c(1,1,6,1)) 
full_missing_wasi$psig <- (full_missing_wasi$p < missingness_pval)
full_missing_sdq <- full_proj_data %>% missing_compare("sdq_emo",missingness_predictors,digits = c(1,1,6,1))
full_missing_sdq$psig <- (full_missing_sdq$p < missingness_pval)
full_missing_stopsig <- full_proj_data %>% missing_compare("correct_s",missingness_predictors,digits = c(1,1,6,1))
full_missing_stopsig$psig <- (full_missing_stopsig$p < missingness_pval)
full_missing_nback <- full_proj_data %>% missing_compare("targets",missingness_predictors,digits = c(1,1,6,1))
full_missing_nback$psig <- (full_missing_nback$p < missingness_pval)

# 3b. Apply exclusion criteria ----
# Set threshold for missingness 
threshold = 0.3

# Determine whether participants meet exclusion criteria
full_proj_data <- full_proj_data %>%
  mutate(missing_all_adolescence = is.na(full_proj_data$wasi) & is.na(full_proj_data$correct_ss) & 
           is.na(full_proj_data$correct_s) &is.na(full_proj_data$nontargets) & is.na(full_proj_data$targets) & 
           is.na(full_proj_data$sdq_emo) &is.na(full_proj_data$sdq_conduct) & is.na(full_proj_data$sdq_hyper) & 
           is.na(full_proj_data$sdq_peer),
         missing_threshold = rowMeans(is.na(full_proj_data)) >= threshold,
         outlier_status = rowSums((abs(scale(full_proj_data$wasi)) > 3 | 
                                     abs(scale(full_proj_data$correct_ss)) > 3 | abs(scale(full_proj_data$correct_s)) > 3 | 
                                     abs(scale(full_proj_data$nontargets)) > 3 | abs(scale(full_proj_data$targets)) > 3), na.rm = TRUE),
  )
full_proj_data[,"outlier_status"] <- ifelse(full_proj_data[,"outlier_status"] == 0, FALSE, TRUE)

# Get rates of meeting individual exclusion criteria
dfSummary(full_proj_data[,c("missing_all_adolescence","missing_threshold","outlier_status")])

# Add variable indicating inclusion in the study
full_proj_data <- full_proj_data %>%
  mutate(inclusion = !(missing_all_adolescence | missing_threshold | outlier_status))
dfSummary(full_proj_data$inclusion) 

# Make data frame of retained participants only
proj_data <- subset(full_proj_data,inclusion == TRUE) # Retain only participants that meet criteria
proj_data <- subset(proj_data,select=-c(missing_all_adolescence,missing_threshold,outlier_status,inclusion))

# 3c. Re-quantify missing values and re-determine patterns ----

# Quantify missing values by variable (variables missing more than 30% will be excluded)
proj_data %>% 
  select(1:100) %>%
  dfSummary()
proj_data %>% 
  select(101:219) %>%
  dfSummary()

missing_vars <- colMeans(is.na(subset(proj_data,select=-c(nontargets,targets)))) # Remove nback
missing_participants <- rowMeans(is.na(subset(proj_data,select=-c(nontargets,targets))))  # Remove nback

print(psych::describe(missing_vars),digits=3)
print(psych::describe(missing_participants),digits=3)

# Plot rates of missing data by participant
hist(missing_participants,
     main = "Rates of Missing Data",
     xlab = "Percentage missing",
     ylab = "Number of participants",
)
abline(v = mean(missing_participants), col = "blue", lwd = 2)

# Plot rates of missing data by variable
hist(missing_vars,
     main = "Rates of Missing Data",
     xlab = "Percentage missing",
     ylab = "Number of variables")
abline(v = mean(missing_vars), col = "blue", lwd = 2)

# Visually inspect plots for associations between missing and observed data
proj_data %>% missing_pairs()

# Identify variables that continue to predict missingness in adolescence
proj_missing_wasi <- full_proj_data %>% missing_compare("wasi",missingness_predictors,digits = c(1,1,6,1))
proj_missing_wasi$psig <- (proj_missing_wasi$p < missingness_pval)
proj_missing_sdq <- full_proj_data %>% missing_compare("sdq_emo",missingness_predictors,digits = c(1,1,6,1)) 
proj_missing_sdq$psig <- (proj_missing_sdq$p < missingness_pval)
proj_missing_stopsig <- full_proj_data %>% missing_compare("correct_s",missingness_predictors,digits = c(1,1,6,1)) 
proj_missing_stopsig$psig <- (proj_missing_stopsig$p < missingness_pval)

# 3d. Correct for missing data ----

imputed <-missForest(proj_data) # Impute missing data
proj_data_imp <- imputed$ximp # Save imputed values
imputation_error <- imputed$OOBerror # Save estimated imputation error
proj_data_imp[,c(findiffcols,interactcols)] <- round(proj_data_imp[,c(findiffcols,interactcols)]) # Round imputed numeric vars to nearest integer 


# 4. Construct and recode new variables --------------------------------------------------------------------------
# 4a. Write function ----
cleaning_fun <- function(x) { 
  x %>% dplyr::mutate(
    # Combine emotional and physical hurt into one domestic violence variable
    dom_viol_phys.1=dom_viol_phys.1a+dom_viol_phys.1b,
    dom_viol_emo.1=dom_viol_emo.1a+dom_viol_emo.1b,
    dom_viol.1=dom_viol_phys.1+dom_viol_emo.1,dom_viol.2=dom_viol_phys.2+dom_viol_emo.2,
    dom_viol.3=dom_viol_phys.3+dom_viol_emo.3,dom_viol.4=dom_viol_phys.4+dom_viol_emo.4,dom_viol.5=dom_viol_phys.5+dom_viol_emo.5,
    # Combine partner and mum into one physical cruelty variable
    phys_cruel_dad.1=phys_cruel_dad.1a+phys_cruel_dad.1b,
    phys_cruel_mum.1=phys_cruel_mum.1a+phys_cruel_mum.1b,
    phys_cruel.1=phys_cruel_dad.1+phys_cruel_mum.1,phys_cruel.2=phys_cruel_dad.2+phys_cruel_mum.2,
    phys_cruel.3=phys_cruel_dad.3+phys_cruel_mum.3,phys_cruel.4=phys_cruel_dad.4+phys_cruel_mum.4,phys_cruel.5=phys_cruel_dad.5+phys_cruel_mum.5,
    # Combine partner and mum into one emotional cruelty variable
    emo_cruel_dad.1=emo_cruel_dad.1a+emo_cruel_dad.1b,
    emo_cruel_mum.1=emo_cruel_mum.1a+emo_cruel_mum.1b,
    emo_cruel.1=emo_cruel_dad.1+emo_cruel_mum.1,emo_cruel.2=emo_cruel_mum.2+emo_cruel_dad.2,
    emo_cruel.3=emo_cruel_mum.3+emo_cruel_dad.3,emo_cruel.4=emo_cruel_mum.4+emo_cruel_dad.4,emo_cruel.5=emo_cruel_mum.5+emo_cruel_dad.5,
    # Combine dad and mum into one separation variable
    separate.1=separate_mum.1+separate_dad.1,separate.2=separate_mum.2+separate_dad.2,separate.3=separate_mum.3+separate_dad.3,
    separate.4=separate_mum.4+separate_dad.4,separate.5=separate_mum.5+separate_dad.5, 
    # Combine hyperactivity and conduct into externalising 
    extern = sdq_hyper + sdq_conduct,
    # Combine emotional and peer problems into internalising
    intern = sdq_emo + sdq_peer,
    # Combine "type s" and "type ss" trials into total correct stop signal trials 
    stopsig = correct_s + correct_ss,
    # Calculate discrimination index in n-back task (z-score of hit rate minus z-score of false alarm rate, where false alarm rate is (1 - correct ID of nontargets))
    nback = (scale(targets) - scale(1-nontargets)),
    # Sum financial diff scores
    fin_diff.1 = fin_diff_food.1+fin_diff_clothes.1+fin_diff_heat.1+fin_diff_rent.1+fin_diff_items.1,
    fin_diff.2 = fin_diff_food.2+fin_diff_clothes.2+fin_diff_heat.2+fin_diff_rent.2+fin_diff_items.2,
    fin_diff.3 = fin_diff_food.3+fin_diff_clothes.3+fin_diff_heat.3+fin_diff_rent.3+fin_diff_items.3,
    fin_diff.4 = fin_diff_food.4+fin_diff_clothes.4+fin_diff_heat.4+fin_diff_rent.4+fin_diff_items.4,
    fin_diff.5 = fin_diff_food.5+fin_diff_clothes.5+fin_diff_heat.5+fin_diff_rent.5+fin_diff_items.5,  
    # Sum interaction scores
    interact_mum.1=interact_mum.1a+interact_mum.1b+interact_mum.1c+interact_mum.1d+interact_mum.1e+interact_mum.1f+interact_mum.1g,
    interact_dad.1=interact_dad.1a+interact_dad.1b+interact_dad.1c+interact_dad.1d+interact_dad.1e+interact_dad.1f+interact_dad.1g+interact_dad.1h+interact_dad.1i,
    interact_mum.2=interact_mum.2a+interact_mum.2b+interact_mum.2c+interact_mum.2d+interact_mum.2e+interact_mum.2f+interact_mum.2g+interact_mum.2h+interact_mum.2i,
    interact_dad.2=interact_dad.2a+interact_dad.2b+interact_dad.2c+interact_dad.2d+interact_dad.2e+interact_dad.2f+interact_dad.2g+interact_dad.2h+interact_dad.2i,
    interact_mum.3=interact_mum.3a+interact_mum.3b+interact_mum.3c+interact_mum.3d+interact_mum.3e+interact_mum.3f+interact_mum.3g+interact_mum.3h+interact_mum.3i,
    interact_dad.3=interact_dad.3a+interact_dad.3b+interact_dad.3c+interact_dad.3d+interact_dad.3e+interact_dad.3f+interact_dad.3g+interact_dad.3h+interact_dad.3i,
    interact_mum.4=interact_mum.4a+interact_mum.4b+interact_mum.4c+interact_mum.4d+interact_mum.4e+interact_mum.4f+interact_mum.4g+interact_mum.4h+interact_mum.4i+interact_mum.4j+interact_mum.4k+interact_mum.4l,
    interact_dad.4=interact_dad.4a+interact_dad.4b+interact_dad.4c+interact_dad.4d+interact_dad.4e+interact_dad.4f+interact_dad.4g+interact_dad.4h+interact_dad.4i+interact_dad.4j+interact_dad.4k+interact_dad.4l,
    interact_mum.5=interact_mum.5a+interact_mum.5b+interact_mum.5c+interact_mum.5d+interact_mum.5e+interact_mum.5f+interact_mum.5g+interact_mum.5h+interact_mum.5i+interact_mum.5j+interact_mum.5k+interact_mum.5l+interact_mum.5m+interact_mum.5n+interact_mum.5o+interact_mum.5p+interact_mum.5q+interact_mum.5r,
    interact_dad.5=interact_dad.5a+interact_dad.5b+interact_dad.5c+interact_dad.5d+interact_dad.5e+interact_dad.5f+interact_dad.5g+interact_dad.5h+interact_dad.5i+interact_dad.5j+interact_dad.5k+interact_dad.5l+interact_dad.5m+interact_dad.5n+interact_dad.5o+interact_dad.5p+interact_dad.5q+interact_dad.5r)
}       

# 4b. Apply to data ----
numeric_fun <- function(x) {as.numeric(levels(x))[x]}

full_proj_data_newvars <- full_proj_data
full_proj_data_newvars[,adversitycols] <- lapply(full_proj_data_newvars[,adversitycols],numeric_fun)
full_proj_data_newvars <- cleaning_fun(full_proj_data_newvars) 

proj_data_newvars <- proj_data
proj_data_newvars[,adversitycols] <- lapply(proj_data_newvars[,adversitycols],numeric_fun)
proj_data_newvars <- cleaning_fun(proj_data_newvars)  

proj_data_imp_newvars <- proj_data_imp
proj_data_imp_newvars[,adversitycols] <- lapply(proj_data_imp_newvars[,adversitycols],numeric_fun)
proj_data_imp_newvars <- cleaning_fun(proj_data_imp_newvars) 

# 4c. Recode new variables properly ----
# For variables that have been summed code 1 (happened) or 0 (did not happen)
full_proj_data_newvars[,c("dom_viol_phys.1","dom_viol_emo.1","dom_viol.1","dom_viol.2","dom_viol.3","dom_viol.4","dom_viol.5","phys_cruel_dad.1","phys_cruel_mum.1",
                          "phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5","emo_cruel_dad.1","emo_cruel_mum.1",
                          "emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5","separate.1","separate.2","separate.3","separate.4","separate.5",
                          "dom_viol_phys","dom_viol_emo","dom_viol","phys_cruel_dad","phys_cruel_mum","phys_cruel","emo_cruel_dad","emo_cruel_mum","emo_cruel",
                          "phys_abuse","sex_abuse","care_change","separate")] <-
  ifelse(full_proj_data_newvars[,c("dom_viol_phys.1","dom_viol_emo.1","dom_viol.1","dom_viol.2","dom_viol.3","dom_viol.4","dom_viol.5","phys_cruel_dad.1","phys_cruel_mum.1",
                                   "phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5","emo_cruel_dad.1","emo_cruel_mum.1",
                                   "emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5","separate.1","separate.2","separate.3","separate.4","separate.5",
                                   "dom_viol_phys","dom_viol_emo","dom_viol","phys_cruel_dad","phys_cruel_mum","phys_cruel","emo_cruel_dad","emo_cruel_mum","emo_cruel",
                                   "phys_abuse","sex_abuse","care_change","separate")] > 0, 1, 0)

proj_data_newvars[,c("dom_viol_phys.1","dom_viol_emo.1","dom_viol.1","dom_viol.2","dom_viol.3","dom_viol.4","dom_viol.5","phys_cruel_dad.1","phys_cruel_mum.1",
                     "phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5","emo_cruel_dad.1","emo_cruel_mum.1",
                     "emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5","separate.1","separate.2","separate.3","separate.4","separate.5",
                     "dom_viol_phys","dom_viol_emo","dom_viol","phys_cruel_dad","phys_cruel_mum","phys_cruel","emo_cruel_dad","emo_cruel_mum","emo_cruel",
                     "phys_abuse","sex_abuse","care_change","separate")] <-
  ifelse(proj_data_newvars[,c("dom_viol_phys.1","dom_viol_emo.1","dom_viol.1","dom_viol.2","dom_viol.3","dom_viol.4","dom_viol.5","phys_cruel_dad.1","phys_cruel_mum.1",
                              "phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5","emo_cruel_dad.1","emo_cruel_mum.1",
                              "emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5","separate.1","separate.2","separate.3","separate.4","separate.5",
                              "dom_viol_phys","dom_viol_emo","dom_viol","phys_cruel_dad","phys_cruel_mum","phys_cruel","emo_cruel_dad","emo_cruel_mum","emo_cruel",
                              "phys_abuse","sex_abuse","care_change","separate")] > 0, 1, 0)

proj_data_imp_newvars[,c("dom_viol_phys.1","dom_viol_emo.1","dom_viol.1","dom_viol.2","dom_viol.3","dom_viol.4","dom_viol.5","phys_cruel_dad.1","phys_cruel_mum.1",
                         "phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5","emo_cruel_dad.1","emo_cruel_mum.1",
                         "emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5","separate.1","separate.2","separate.3","separate.4","separate.5",
                         "dom_viol_phys","dom_viol_emo","dom_viol","phys_cruel_dad","phys_cruel_mum","phys_cruel","emo_cruel_dad","emo_cruel_mum","emo_cruel",
                         "phys_abuse","sex_abuse","care_change","separate")] <-
  ifelse(proj_data_imp_newvars[,c("dom_viol_phys.1","dom_viol_emo.1","dom_viol.1","dom_viol.2","dom_viol.3","dom_viol.4","dom_viol.5","phys_cruel_dad.1","phys_cruel_mum.1",
                                  "phys_cruel.1","phys_cruel.2","phys_cruel.3","phys_cruel.4","phys_cruel.5","emo_cruel_dad.1","emo_cruel_mum.1",
                                  "emo_cruel.1","emo_cruel.2","emo_cruel.3","emo_cruel.4","emo_cruel.5","separate.1","separate.2","separate.3","separate.4","separate.5",
                                  "dom_viol_phys","dom_viol_emo","dom_viol","phys_cruel_dad","phys_cruel_mum","phys_cruel","emo_cruel_dad","emo_cruel_mum","emo_cruel",
                                  "phys_abuse","sex_abuse","care_change","separate")] > 0, 1, 0)

# 4d. Write second function for cumulative ----
cleaning_fun_2 <- function(x) { 
  x %>% dplyr::mutate(
    dom_viol_phys_cu = dom_viol_phys.1+dom_viol_phys.2+dom_viol_phys.3+dom_viol_phys.4+dom_viol_phys.5,
    dom_viol_emo_cu = dom_viol_emo.1+dom_viol_emo.2+dom_viol_emo.3+dom_viol_emo.4+dom_viol_emo.5,
    phys_cruel_cu = phys_cruel.1+phys_cruel.2+phys_cruel.3+phys_cruel.4+phys_cruel.5,
    emo_cruel_cu = emo_cruel.1+emo_cruel.2+emo_cruel.3+emo_cruel.4+emo_cruel.5,
    phys_abuse_cu = phys_abuse.1+phys_abuse.2+phys_abuse.3+phys_abuse.4+phys_abuse.5,
    sex_abuse_cu = sex_abuse.1+sex_abuse.2+sex_abuse.3+sex_abuse.4+sex_abuse.5,
    care_change_cu = care_change.1+care_change.2+care_change.3+care_change.4+care_change.5,
    separate_cu = separate.1+separate.2+separate.3+separate.4+separate.5,
    fin_diff_cu = fin_diff.1+fin_diff.2+fin_diff.3+fin_diff.4+fin_diff.5,
    interact_mum_cu = interact_mum.1/7+interact_mum.2/9+interact_mum.3/9+interact_mum.4/12+interact_mum.5/18,
    interact_dad_cu = interact_dad.1/9+interact_dad.2/9+interact_dad.3/9+interact_dad.4/12+interact_dad.5/18) 
}

# 4e. Apply to data ----
# numeric_fun <- function(x) {as.numeric(levels(x))[x]}

full_proj_data_newvars_2 <- full_proj_data_newvars
full_proj_data_newvars_2 <- cleaning_fun_2(full_proj_data_newvars_2) 

proj_data_newvars_2 <- proj_data_newvars
proj_data_newvars_2 <- cleaning_fun_2(proj_data_newvars_2)  

proj_data_imp_newvars_2 <- proj_data_imp_newvars
proj_data_imp_newvars_2 <- cleaning_fun_2(proj_data_imp_newvars_2) 


# 5. Save data   --------------------------------------------------------------------------------

write.csv(full_proj_data_newvars_2, file = paste(output,"/full_proj_data.csv",sep = ""))
write.csv(proj_data_newvars_2, file = paste(output,"/proj_data.csv",sep = ""))
write.csv(proj_data_imp_newvars_2, file = paste(output,"/proj_data_imp.csv",sep = ""))


