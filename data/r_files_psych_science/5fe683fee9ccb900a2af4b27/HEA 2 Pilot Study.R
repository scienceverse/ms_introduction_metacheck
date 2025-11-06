library(tidyverse)

pildat.raw <- read.csv("HEA Pilot.csv",
                   strip.white = T, header = T)

# Data cleaning

pildat.raw <- pildat.raw[5:nrow(pildat.raw),]

pildat <- pildat.raw %>% 
  rename(duration = Duration..in.seconds., 
         fatigue = drivers_1,
         habit = drivers_2,
         thirst = drivers_3,
         taste = drivers_4,
         meal = drivers_5,
         social = drivers_6,
         date = StartDate
         ) %>% 
  dplyr::select(date, duration, fatigue:ethnicity) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  filter(habit != "", duration > 15) %>% # Remove test response and incomplete responses
  mutate_at(vars(fatigue:freq, age), as.numeric)

# Demographics

table(pildat$gender)

# Means and SD

pilsum.raw <- pildat %>% 
  dplyr::select(fatigue:social) 

pilsum <- pilsum.raw %>%
  summarize_all(list(~ mean(.), ~ sd(.))) %>% 
  gather(key = "var", value = "value", 1:12) %>% 
  separate(var, into = c("var","stat"),sep = "_") %>% 
  spread(key = "stat", value = "value") %>% 
  mutate_at(vars(mean, sd), round, digits = 2) %>% 
  arrange(desc(mean))

# Means and standard deviations for perceived drivers of coffee drinking

pilsum

# Test difference between habit and fatigue

t.test(pildat$fatigue, pildat$habit, paired = T)

