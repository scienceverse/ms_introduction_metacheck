########## CLEANING and RUNNING MODEL IN WHO DATA ############

library(tidyverse)
library(haven)
library(hms)
library(lubridate)
library(zoo)
library(lme4)

SA <- read_dta("SouthAfricaINDData.dta")
Russia <- read_dta("RussiaINDData.dta")
India <- read_dta("IndiaINDData.dta")
China <- read_dta("ChinaINDData.dta")
Mexico <-read_dta("MexicoINDData.dta")
Ghana <- read_dta("GhanaINDData.dta")

SA_tr <- SA %>% select(id, q0008, contains(c("q701", "q702", "q705", "q710", "q715", "q720", "q725", "q730", "q735", "q740", "q745", "Q7500"))) %>%
                mutate(country = "South Africa") %>% 
                mutate(across(ends_with("m") | ends_with("h"), ~sjlabelled::remove_all_labels(.x))) %>% 
                as_factor %>%
                mutate(across(where(is.factor), ~as.factor(tolower(as.character(.x)))))

Russia_tr <- Russia %>% select(id, q0008, contains(c("q701", "q702", "q705", "q710", "q715", "q720", "q725", "q730", "q735", "q740", "q745", "Q7500"))) %>%
                        mutate(country = "Russia") %>% 
                        mutate(across(ends_with("m") | ends_with("h"), ~sjlabelled::remove_all_labels(.x))) %>% 
                        as_factor %>%
                        mutate(across(where(is.factor), ~as.factor(tolower(as.character(.x)))))

India_tr <- India %>% select(id, q0008, contains(c("q701", "q702", "q705", "q710", "q715", "q720", "q725", "q730", "q735", "q740", "q745", "Q7500"))) %>%
                      mutate(country = "India") %>% 
                      mutate(across(ends_with("m") | ends_with("h"), ~sjlabelled::remove_all_labels(.x))) %>% 
                      as_factor %>%
                      mutate(across(where(is.factor), ~as.factor(tolower(as.character(.x)))))

China_tr <- China %>% select(id, q0008, contains(c("q701", "q702", "q705", "q710", "q715", "q720", "q725", "q730", "q735", "q740", "q745", "Q7500"))) %>%
                      mutate(country = "China") %>% 
                      mutate(across(ends_with("m") | ends_with("h"), ~sjlabelled::remove_all_labels(.x))) %>% 
                      as_factor %>%
                      mutate(across(where(is.factor), ~as.factor(tolower(as.character(.x)))))

Mexico_tr <- Mexico %>% select(id, q0008, contains(c("q701", "q702", "q705", "q710", "q715", "q720", "q725", "q730", "q735", "q740", "q745", "Q7500"))) %>%
                        mutate(country = "Mexico") %>% 
                        mutate(across(ends_with("m") | ends_with("h"), ~sjlabelled::remove_all_labels(.x))) %>% 
                        as_factor %>%
                        mutate(across(where(is.factor), ~as.factor(tolower(as.character(.x)))))

Ghana_tr <- Ghana %>% select(id, q0008, contains(c("q701", "q702", "q705", "q710", "q715", "q720", "q725", "q730", "q735", "q740", "q745", "Q7500"))) %>%
                      mutate(country = "Ghana")  %>% 
                      mutate(across(ends_with("m") | ends_with("h"), ~sjlabelled::remove_all_labels(.x))) %>% 
                      as_factor %>%
                      mutate(across(where(is.factor), ~as.factor(tolower(as.character(.x)))))

data_all <- bind_rows(SA_tr, Russia_tr, India_tr, China_tr, Mexico_tr, Ghana_tr)


#### SELECT VARIABLES OF INTEREST ###

split_by_rotation <- split(data_all, data_all$q0008)

extras <- split_by_rotation[[1]] %>% filter(!is.na(aq7015) | !is.na(bq7015) | !is.na(cq7015) | !is.na(dq7014h))

rotationA <- split_by_rotation$A %>% select(id, q0008, country, starts_with("aq7"))
gen_names <- str_replace_all(names(rotationA), "aq7", "q7")
names(rotationA) <- gen_names

rotationB <- split_by_rotation$B %>% select(id, q0008, country, starts_with("bq7"))
gen_names <- str_replace_all(names(rotationB), "bq7", "q7")
names(rotationB) <- gen_names

rotationC <- split_by_rotation$C %>% select(id, q0008, country, starts_with("cq7"))
gen_names <- str_replace_all(names(rotationC), "cq7", "q7")
names(rotationC) <- gen_names

# rotationD <- split_by_rotation$D %>% select(id, q0008, country, starts_with("dq7"))
  ## seems different - omit?

data_all_full <- bind_rows(rotationA, rotationB, rotationC) %>% select(-c(4:7))


#### MAKE LONGER ####

names(data_all_full)

names_data <- c("id", "rotation", "country", "wake_up_h", "wake_up_m", "first_act_begin_h", "first_act_begin_m", 
                "activity", "act_dur_h", "act_dur_m", "alone", "spouse", "adult_children", "young_children", 
                "other_family", "friends", "co-workers", "other", "friendliness", "worried", "rushed", "irritated_angry",
                "depressed", "tense_stressed", "calm_relaxed", "enjoying", "obs_no_for_day")

time1 <- data_all_full %>% select(1:5, 9:10, 6:26) %>% mutate(obs_no_for_day=1)
names(time1)
names(time1) <- c(names_data)

time2 <- data_all_full %>% select(1:5, 9:10, 27:45) %>% mutate(obs_no_for_day=2)
names(time2)
names(time2) <- c(names_data)

time3 <- data_all_full %>% select(1:5, 9:10, 46:64) %>% mutate(obs_no_for_day=3)
names(time3)
names(time3) <- c(names_data)

time4 <- data_all_full %>% select(1:5, 9:10, 65:83) %>% mutate(obs_no_for_day=4)
names(time4)
names(time4) <- c(names_data)

time5 <- data_all_full %>% select(1:5, 9:10, 84:102) %>% mutate(obs_no_for_day=5)
names(time5)
names(time5) <- c(names_data)

time6 <- data_all_full %>% select(1:5, 9:10, 103:121) %>% mutate(obs_no_for_day=6)
names(time6)
names(time6) <- c(names_data)

time7 <- data_all_full %>% select(1:5, 9:10, 122:140) %>% mutate(obs_no_for_day=7)
names(time7)
names(time7) <- c(names_data)

time8 <- data_all_full %>% select(1:5, 9:10, 141:159) %>% mutate(obs_no_for_day=8)
names(time8)
names(time8) <- c(names_data)

time9 <- data_all_full %>% select(1:5, 9:10, 160:178) %>% mutate(obs_no_for_day=9)
names(time9)
names(time9) <- c(names_data)

time10 <- data_all_full %>% select(1:5, 9:10, 179:197) %>% mutate(obs_no_for_day=10)
names(time10)
names(time10) <- c(names_data)

data_full_long <- bind_rows(time1, time2, time3, time4, time5, time6, time7, time8, time9, time10) %>%
                  mutate(across(11:18, ~ifelse(as.character(.x)=="no", 0,
                                               ifelse(as.character(.x)=="yes", 1, NA)))) %>%
                  mutate(across(20:26, ~ifelse(as.character(.x)=="not at all", 1,
                                               ifelse(as.character(.x)=="a little", 2,
                                                      ifelse(as.character(.x)=="very much", 3, NA))))) %>%
                  mutate(mood = rowMeans(.[25:26], na.rm=T) - rowMeans(.[20:24], na.rm=T)) %>%
                  mutate(activity = fct_recode(activity, NULL = c("0"))) %>%
                  filter(!is.na(mood)) %>%
                  group_by(id) %>%
                  add_count(name="day_n_obs") %>%
                  mutate(mood_cent = mood - mean(mood)) %>%
                  ungroup()

### SORT OUT TIME ###

names(data_full_long)
data_full_long <- data_full_long %>% mutate(across(ends_with("_h"), ~ifelse(.x > 24 | .x < 0, NA, ifelse(.x==24, 0, .x)))) %>%
                                     mutate(across(ends_with("_m"), ~ifelse(.x > 60 | .x < 0, NA, ifelse(.x==60, 0, .x)))) %>%
                                     mutate(wake_up_time = paste(wake_up_h, ":", ifelse(str_length(as.character(wake_up_m))==1, 
                                                                                        ifelse(wake_up_m==0, "00", 
                                                                                               paste("0", wake_up_m, sep="")), wake_up_m), ":00", sep="")) %>%
                                     mutate(first_act_time = paste(first_act_begin_h, ":", ifelse(str_length(as.character(first_act_begin_m))==1, 
                                                                                        ifelse(first_act_begin_m==0, "00", 
                                                                                               paste("0", first_act_begin_m, sep="")), first_act_begin_m), ":00", sep="")) %>%
                                     mutate(act_dur_time = paste(act_dur_h, ":", ifelse(str_length(as.character(act_dur_m))==1, 
                                                                                        ifelse(act_dur_m==0, "00", 
                                                                                               paste("0", act_dur_m, sep="")), act_dur_m), ":00", sep="")) %>%
                                     mutate(across(ends_with("_time"), ~ifelse(grepl("NA", .x), NA, .x))) %>%
                                     mutate(across(ends_with("_time"), ~as_hms(.x))) %>% 
                                     group_by(id) %>%
                                     arrange(obs_no_for_day) %>%
                                     mutate(time_since_first_act = cumsum(as.integer(act_dur_time))) %>%
                                     ungroup() %>%
                                     mutate(act_end_time = as_hms(first_act_time + time_since_first_act)) %>%
                                     select(-time_since_first_act)

#### LAG ####

names(data_full_long)

data_lag <- data_full_long %>% select(id, 8:28, mood_cent, act_dur_time, act_end_time) %>%
                               mutate(obs_no_for_day = obs_no_for_day + 1)
names(data_lag) <- c("id", paste(names(data_lag)[2:20], "_prev", sep=""), "obs_no_for_day", "mood_prev", "mood_cent_prev", "act_dur_time_prev", "act_start_time")
names(data_lag)

data_full_lagged <- data_full_long %>% left_join(data_lag, by=c("id", "obs_no_for_day")) %>%
                                       mutate(act_start_time = ifelse(obs_no_for_day==1, first_act_time, act_start_time)) %>%
                                       mutate(act_start_time = as_hms(act_start_time)) %>% 
                                       filter(as.numeric(act_start_time)<86400)

data_full_lagged$time_bin <- cut(as.POSIXct(data_full_lagged$act_start_time), breaks="2 hours", labels=seq(0, 11, 1))

save(data_full_lagged, file="WHO.ro")