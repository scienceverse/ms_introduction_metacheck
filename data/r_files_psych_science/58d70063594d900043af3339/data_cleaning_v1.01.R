library(tidyverse)

s1_mt <- read.csv('study1_mt.csv', header=TRUE)
s1_mt$condition <- as.factor(s1_mt$code) # code corresponds to condition in study 1
s1_mt$conditionRC <- ifelse(s1_mt$condition == 'conflict', 1, 0) # 1 if self-control conflict, 0 if comparison trial
s1_choice <- read.csv('study1_choices.csv', header=TRUE) # datafile with choice information
s1_raw <- left_join(s1_mt, s1_choice, by='subject') # match choice data with mouse-tracking data
s1 <- s1_raw %>% # get rid of trials greater than 3 SDs from the mean on RT, AUC, and initialization time
  filter(RT < (3*sd(RT) + mean(RT)), AUC < (3*sd(AUC) + mean(AUC)), init.time < (3*sd(init.time) + mean(init.time))) %>%
  filter(error == 0) # get rid of errors 
s1$mdAbrupt <- ifelse(s1$MD >= .9, 1, 0) # if maximum deviation is greater than .9, label it as abrupt
s1_agg <- s1 %>% # creates aggregate data for choice analyses
  group_by(subject, condition) %>%
  summarize(meanAUC = mean(AUC), rt = mean(RT), choice = mean(choiceRC), abrupt = mean(mdAbrupt), md = mean(MD)) %>%
  gather(variable, val, -subject, -condition, -choice) %>% 
  unite(temp1, condition, variable, sep = "_") %>% 
  spread(temp1, val) 

# Study 2
# Study 2 is the only study in which I compiled mouse-tracking and questionnaire data outside of R 
# Contact me (paul.e.stillman@gmail.com) if you want the python code I used to do variable creation and merging 
# The majority of the variables created in python, however, were for trial-by-trial individual ratings of foods, analyses of which we don't report in the main text 
s2_raw <- read.csv("study2.csv") 
s2_raw$conditionRC <- ifelse(s2_raw$isConflict_updated == 1, 'conflict', ifelse(s2_raw$isConflict_updated == -1, 'comparison', NA)) # isConflict_updated corresponds to the condition (1 = self-control, -1 = comparison)
s2_raw$mdAbrupt <- ifelse(s2_raw$MD >= .9, 1, 0)
s2 <- s2_raw %>% # again filter based on RT, AUC, and initialization time
  filter(RT < (3*sd(RT) + mean(RT)), AUC < (3*sd(AUC) + mean(AUC)), init.time < (3*sd(init.time) + mean(init.time))) %>%
  filter(!is.na(isConflict_updated))

s2_agg <- s2 %>% group_by(subject, conditionRC) %>% # again create aggregated data
  summarise(meanAUC = mean(AUC), rt = mean(RT), abrupt = mean(mdAbrupt), md = mean(MD),
            gpa = first(GPA), diet = first(diet_currentStatus_avg), sc = first(allSC),
            bmi = first(BMI)) %>%
  gather(variable, val, meanAUC, rt, abrupt, md) %>% 
  unite(temp1, conditionRC, variable, sep = "_") %>% 
  spread(temp1, val) 

# Study 3a
# SS = smaller-sooner (e.g., $5 today)
# LL = larger-later (e.g., $25 in 3 months)
s3a_mt <- read.csv('study3a_mt.csv') # mouse-tracker data for study 3a
s3a_condition <- read.csv('study3a_condition.csv') # condition information for Study 3a
s3a_raw <- inner_join(s3a_mt, s3a_condition, by = 'subject') # retain only information if we have both MT and condition information
s3a_raw$mdAbrupt <- ifelse(s3a_raw$MD >= .9, 1, 0)
s3a_raw$choseLL <- ifelse(s3a_raw$error == 0, 1, 0) # MT files were programmed such that choosing the smaller sooner meant error = 1, otherwise error = 0. ChoseLL thus = 1 if they chose the larger later, 0 otherwise
s3a_raw$choiceRC <- ifelse(s3a_raw$choseLL == 1, "LL", "SS") # sometimes useful to have 

s3a <- s3a_raw %>% # filter again
  filter(RT < (3*sd(RT) + mean(RT)), AUC < (3*sd(AUC) + mean(AUC)), init.time < (3*sd(init.time) + mean(init.time)))

s3a_agg <- s3a %>% group_by(subject, choiceRC) %>% # aggregate data
  summarise(meanAUC = mean(AUC), rt = mean(RT), abrupt = mean(mdAbrupt), md = mean(MD), # get average AUC, MD, etc.
            LL_first = first(LL_first), numDec = sum(choseLL)) %>% # numDec = sum of LL decisions
  gather(variable, val, meanAUC, rt, abrupt, numDec, md) %>% 
  unite(temp1, choiceRC, variable, sep = "_") %>% 
  spread(temp1, val) %>%
  rename(numDec = LL_numDec) %>% select(-SS_numDec) # LL_numDec is the same as SS_numDec, so get rid of one and rename the other

# Study 3b

s3b_raw <- read.csv("study3b.csv") # alredy has condition information coded in, as well as whether they chose the LL or not
s3b_raw$construalRC <- ifelse(s3b_raw$construal == 'high', 1, -1) # effects code construal level
s3b_raw$magDiff <- s3b_raw$ll - s3b_raw$ss # calculate the magnitude difference between larger-later and smaller sooner (not used in main text analyses)
s3b_raw$mdAbrupt <- ifelse(s3b_raw$MD >= .9, 1, 0) 
s3b_raw$choiceRC <- ifelse(s3b_raw$choseLL == 1, 'LL', 'SS')

s3b <- s3b_raw %>% # filter data
  filter(RT < (3*sd(RT) + mean(RT)), AUC < (3*sd(AUC) + mean(AUC)), init.time < (3*sd(init.time) + mean(init.time)))

s3b_agg <- s3b %>% group_by(subject, choiceRC) %>% # aggregate
  summarise(meanAUC = mean(AUC), rt = mean(RT), abrupt = mean(mdAbrupt), md = mean(MD),
            construal = first(construalRC), numDec = sum(choseLL)) %>%
  gather(variable, val, meanAUC, rt, abrupt, numDec, md) %>% 
  unite(temp1, choiceRC, variable, sep = "_") %>% 
  spread(temp1, val) %>%
  rename(numDec = LL_numDec) %>% select(-SS_numDec)

save(s1,s2,s3a,s3b,s1_agg,s2_agg,s3a_agg,s3b_agg, file = 'allStudies_final.RData')
