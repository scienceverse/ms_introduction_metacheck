if (!require('pacman')) install.packages('pacman')

p_load(tidyverse, forcats, performance, 
       psych, sjPlot, apaTables, ggeffects, WebPower, effsize)

import::from(labelled, remove_attributes, set_variable_labels)

theme_set(theme_minimal())

set.seed(1)

#############################
# Power Analyses
#############################

wp.logistic(power = .80, p0 = 0.45, p1 = 0.55, family='Bernoulli')

##################################
# Data Cleaning - Uncomment to run
##################################

# clin <- list()
# 
# lst <- list.files(pattern = "*.csv")
# 
# for (j in (1:length(lst))) {
# 
# #Read raw file
# 
# file.raw <- try(read_csv(lst[j], col_types = cols()))
# 
# if (file.info(lst[j])[['size']] > 1 &
#     ("key_resp.keys" %in% names(file.raw)) &
#     ('att_text' %in% names(file.raw))) {
# 
# #Correct messed-up row order for "" column
# 
# for (i in 1:(nrow(file.raw)-1)) {
#   file.raw$stimulus[i] <- file.raw$stimulus[i+1]
#   file.raw$stimulus[i+1] <- ""
#   file.raw$trials.thisN[i] <- file.raw$trials.thisN[i+1]
#   file.raw$trials.thisN[i+1] <- ""
# }
# 
# file.raw[file.raw == ""] <- NA
# 
# emot.file <- file.raw %>%
#   dplyr::select(PROLIFIC_PID, date, emotWord, panasSlider.response, panasSlider.rt) %>%
#   filter(emotWord != "") %>%
#   pivot_wider(names_from = emotWord, values_from = c(panasSlider.response, panasSlider.rt))
# 
# file <- file.raw %>%
#   dplyr::select(starts_with("cond_"),
#          OS,
#          key_resp_5.rt, mood_text,
#          response.keys, stimulus, response.corr, trials.thisN, key_resp.keys,
#          att_text, slider_habit.response, slider_mood.response,
#          slider_hab_all.response, slider_mood_all.response,
#          mancheckSlide.response,
#          ends_with(".rt"), -panasSlider.rt) %>%
#   mutate(
#     stimulus = case_when (
#       (!(is.na(stimulus))) ~ stimulus,
#       (!(is.na(response.keys))) & is.na(stimulus) & response.corr == 1 ~ response.keys,
#       (!(is.na(response.keys))) & is.na(stimulus) & response.corr == 0 & response.keys == "m" ~ "z",
#       (!(is.na(response.keys))) & is.na(stimulus) & response.corr == 0 & response.keys == "z" ~ "m"
#     ),
#     cond_hab = case_when(
#       cond_dom == "Left" & cond_fin == "Right" | cond_dom == "Right" & cond_fin == "Left" ~ "strong",
#       cond_dom == "Ctrl" ~ "weak"
#     )
#   ) %>%
#   rename(trialn = trials.thisN, mancheck = mancheckSlide.response)
# 
# #Remove empty rows
# 
# rowRem <- !(apply(file, 1, function(y) sum(is.na(y))) == ncol(file))
# file <- file[rowRem,]
# 
# #Autocomplete between-subjects rows and remove first and second rows
# 
# lstna <- sapply(file, function(y) sum(is.na(y))) == nrow(file) - 1
# lstna <- attr(lstna[lstna == T], "names")
# file[,names(file) %in% lstna] <- sapply(file, function(y) y = unique(y[!(is.na(y))]))[names(file) %in% lstna]
# 
# file <- mutate(file, ask = case_when(
#   key_resp.keys == "m" & cond_fin == "Right" | key_resp.keys == "z" & cond_fin == "Left" ~ "yes",
#   key_resp.keys == "z" & cond_fin == "Right" | key_resp.keys == "m" & cond_fin == "Left" ~ "no"
# ))
# 
# file <- file[-c(1, nrow(file)),]
# 
# file <- merge(file, emot.file)
# 
# 
# clin[[j]] <- file
# 
#   }
# }
# 
# #End loop - data cleaning
# 
# 
# fulldat.raw <- clin %>%
#   map_dfr(as.data.frame)
# 
# # Create dataframe of participants who completed the task twice
# 
# tabd <- fulldat.raw %>%
#   group_by(PROLIFIC_PID, date) %>%
#   dplyr::summarize(num = n()) %>%
#   mutate(dup=row_number()) %>%
#   filter(dup==2)
# 
# # Remove second completion from data
# 
# fulldat.raw <- anti_join(fulldat.raw, tabd, by=c('PROLIFIC_PID','date'))
# 
# fulldat.raw <- dplyr::select(fulldat.raw, PROLIFIC_PID, starts_with("cond_"), ask, everything())
# 
# fulldatrow <- fulldat.raw %>%
#   filter(!(is.na(stimulus))) %>%
#   group_by(PROLIFIC_PID) %>%
#   mutate(mean_rt = mean(response.rt, na.rm = T),
#          median_rt = median(response.rt, na.rm = T),
#          sd_rt = sd(response.rt, na.rm = T),
#          num = n(),
#          corr_prop = (sum(response.corr == 1, na.rm=T) / num),
#          domKey = ifelse(cond_dom=='Right','m',
#                          ifelse(cond_dom=='Left','z',NA)),
#          trialCon = ifelse(is.na(domKey) | is.na(stimulus),NA,
#                            ifelse(stimulus == domKey, 1, 0)),
#          trialNum = row_number()
#   ) %>%
#   ungroup()
# 
# fulldatcon <- fulldatrow %>%
#   group_by(PROLIFIC_PID) %>%
#   filter(trialCon == 1) %>%
#   mutate(numCon = n(),
#     corr_con_prop = (sum(response.corr == 1) / numCon)) %>%
#   dplyr::select(PROLIFIC_PID, corr_con_prop) %>%
#   ungroup() %>%
#   filter(!(duplicated(.)))
# 
# fulldatincon <- fulldatrow %>%
#   group_by(PROLIFIC_PID) %>%
#   filter(trialCon == 0) %>%
#   mutate(numIncon = n(),
#          corr_incon_prop = (sum(response.corr == 1) / numIncon)) %>%
#   dplyr::select(PROLIFIC_PID, corr_incon_prop) %>%
#   ungroup() %>%
#   filter(!(duplicated(.)))
# 
# fulldatsum <- merge(fulldatcon, fulldatincon, by = "PROLIFIC_PID")
# 
# fulldat_long <- merge(fulldatrow, fulldatsum, all.x=T, by = "PROLIFIC_PID")
# 
# fulldat <- fulldat_long %>%
#   dplyr::select(-response.keys,-stimulus,-response.corr,-trialn, -response.rt) %>%
#   filter(!(duplicated(PROLIFIC_PID)))
# 
# final.clean <- fulldat %>%
#   mutate(eyeballTime = rowSums(dplyr::select(fulldat, ends_with(".rt"))),
#          mancheckCor = ifelse(mancheck %in% c(1, 2, 4),"Incorrect","Correct"),
#          pos = rowMeans(dplyr::select(fulldat,
#                                         panasSlider.response_Active,
#                                         panasSlider.response_Alert,
#                                         panasSlider.response_Attentive,
#                                         panasSlider.response_Determined,
#                                         panasSlider.response_Inspired,
#                                         panasSlider.response_Proud), na.rm = T),
#          neg = rowMeans(dplyr::select(fulldat,
#                                       panasSlider.response_Afraid,
#                                       panasSlider.response_Ashamed,
#                                       panasSlider.response_Guilty,
#                                       panasSlider.response_Upset,
#                                       panasSlider.response_Nervous,
#                                       panasSlider.response_Hostile), na.rm = T),
#          ratio = slider_mood.response / slider_habit.response,
#          pos.c = pos - mean(pos),
#          neg.c = neg - mean(neg),
#          medrt.c = median_rt - mean(median_rt, na.rm = T)
#          ) %>%
#   mutate_at(vars(ask, starts_with("cond")), factor)
# 
# # Open-Text coding data
# 
# opentex_raw <- read_csv('HEA 4 Open Text Coding.csv',
#                         na = c('NA','/A','N/A','<NA>','?','na','Na'))
# 
# final.clean <- merge(final.clean, opentex_raw, by.x = 'PROLIFIC_PID', by.y = 'ID', all = T)
# 
# #Read in qualtrics data
# 
# # Helper function for reading Qualtrics data
# 
# read_csv_qual <- function(file, namfile = file) {
# 
#   df <- read_csv(file,
#                  skip = 3,
#                  col_names = as_vector(read.csv(namfile,nrows=1,header=F)[1,]))
# 
#   lab <- remove_attributes(as_vector(read.csv(file, skip = 1, nrows = 1, header = F)[1,]),
#                            'names')
# 
#   df <- df %>% set_variable_labels(.labels = lab)
# 
#   return(df)
# }
# 
# # Qualtrics file for main study - change file name to account for file location
# 
# qual.raw <- read_csv_qual("HEA+4+Prolific+End_September+22,+2020_21.27.csv")
# 
# qual <- qual.raw %>%
#   dplyr::select(PROLIFIC_PID, date = StartDate, duration = 'Duration (in seconds)',
#          starts_with('srbai'), hand:ethnicity_7_TEXT
#   ) %>%
#   filter(!(is.na(PROLIFIC_PID)), PROLIFIC_PID != 12345)  # Remove test trials
# 
# qual <- mutate(qual, srbai_avg = rowMeans(dplyr::select(qual, starts_with('srbai')), na.rm = T),
#                srbai_avg_c = srbai_avg - mean(srbai_avg, na.rm = T))
# 
# 
# # Merge qualtrics and reaction time files. This keeps Pavlovia responses without qualtrics, but not
# # Qualtrics responses without Pavlovia
# 
# final.clean <- merge(final.clean, qual, all.x = T, by = "PROLIFIC_PID")
# 
# setwd("Complete Data")
# 
# # Write main study data
# 
# write.csv(final.clean, "HEA 1 Complete Data Clean.csv", row.names = F)

#############################

#Data analysis

#############################

# Set working directory and clean data

final_raw <- read_csv("HEA 1 Complete Data Clean.csv", 
                      trim_ws = T, col_types = cols())

# Create factor and numeric variables, binary variables for making/not making errors.

final <- final_raw %>% 
  mutate(ask = factor(ask, levels = c('no','yes')),
         asknum = as.numeric(ask)-1,
         cond_hab = relevel(factor(cond_hab, levels = c('weak','strong')), ref='strong'),
         cond_mood = factor(cond_mood, levels = c('control','sad','happy'))
         ) %>% 
  rename(slider_habit_all.response = slider_hab_all.response)

# Set up dummy coding for mood and effects coding for habit

contrasts(final$cond_mood) <- contr.treatment(3)
contrasts(final$cond_hab) <- contr.sum(2)

final <- filter(final, !(duplicated(PROLIFIC_PID))) # For the 19 participants who seem to have completed the study more than once, include only the first iteration

final.mood <- filter(final, cond_mood != "control") # Create dataset with only positive and negative mood conditions

final.all <- final # Create copy of data before exclusions

final <- final %>% 
  filter(mancheckCor == "Correct") %>% 
  group_by(cond_mood) %>% 
  mutate(out = ifelse(pos > (mean(pos)+(sd(pos)*2)) | pos < (mean(pos)-(sd(pos)*2)) |
                      neg > (mean(neg)+(sd(neg)*2)) | neg < (mean(neg)-(sd(neg)*2)), 
                      1, 0),
         median_rt = median_rt * 1000) %>% 
  ungroup() %>% 
  mutate(outrt = ifelse(median_rt > 500, 1, 0))

nrow(final.all) - nrow(final) # Participants who didn't pass attention check

final.out <- final 

final <- final %>% 
  filter(out == 0 & outrt == 0) %>% 
  ungroup()

nrow(final.out) - nrow(final) # Participants who were outliers on PANAS or RT

# How many overall participants?

nrow(final)

# Detailed table for participants who were outliers on affect or rt?

table(final.out$out, final.out$outrt)

# Demographics

table(final$gender, useNA = "always")
psych::describe(final$age)

#Mood reliability

names(final) <- gsub('panasSlider.response','panas',names(final))
alpha(dplyr::select(final, panas_Hostile, panas_Upset, panas_Ashamed, panas_Afraid, panas_Guilty, panas_Nervous))
alpha(dplyr::select(final, panas_Inspired, panas_Active, panas_Alert, panas_Determined, panas_Attentive, panas_Proud))

# SRBAI reliability

alpha(dplyr::select(final, srbai_1, srbai_2, srbai_3, srbai_4))

# SRBAI condition differences

t.test(srbai_avg ~ cond_hab, data = final)
cohen.d(srbai_avg ~ cond_hab, data = final)

# Table 1 (Correlation Table)

cortab <- final %>%
  dplyr::select(slider_habit.response, slider_mood.response, slider_habit_all.response, slider_mood_all.response, 
                pos, neg, median_rt) %>% 
  rename(`Habit attribution`=slider_habit.response, `Mood attribution`=slider_mood.response, 
         `Habit attribution (others)`=slider_habit_all.response, `Mood attribution (others)`=slider_mood_all.response,
         `Positive affect`=pos, `Negative affect`=neg,
         RT=median_rt)

apa.cor.table(cortab, file = 'Correlation Table.doc')

# Data for Table 2 (% helped by habit and mood condition)

(tabcond <- final %>% 
  group_by(cond_mood, cond_hab) %>% 
  count())

(tab <- final %>% 
  group_by(cond_mood, cond_hab) %>% 
  count(ask) %>% 
  mutate(Proportion = prop.table(n)) %>% 
  filter(ask=='yes') %>% 
  dplyr::select(-ask) %>% 
  mutate(Proportion=scales::percent(Proportion, accuracy=1)))

(tabmood <- final %>% 
  group_by(cond_mood) %>% 
  count(ask) %>% 
  mutate(Proportion = prop.table(n)) %>% 
  filter(ask=='yes') %>% 
  dplyr::select(-ask) %>% 
  mutate(Proportion=scales::percent(Proportion, accuracy=1)))

(tabhab <- final %>% 
  group_by(cond_hab) %>% 
  count(ask) %>% 
  mutate(Proportion = prop.table(n)) %>% 
  filter(ask=='yes') %>% 
  dplyr::select(-ask) %>% 
  mutate(Proportion=scales::percent(Proportion, accuracy=1)))

(taball <-  final %>% 
  count(ask) %>% 
  mutate(Proportion = prop.table(n)) %>% 
  filter(ask=='yes') %>% 
  dplyr::select(-ask) %>% 
  mutate(Proportion=scales::percent(Proportion, accuracy=1)))

#Mood manipulation check

summary(posmod <- lm(pos ~ cond_mood * cond_hab, data = final))
summary(negmod <- lm(neg ~ cond_mood * cond_hab, data = final))

tab_model(posmod, show.std=T)
tab_model(negmod, show.std=T)

# Habit manipulation check

summary(rtmod <- lm(median_rt ~ cond_mood * cond_hab, data = final))
tab_model(rtmod, show.std = T)

# Habit and Mood Effects on Helping - Actual
 
summary(m1 <- glm(ask ~ cond_hab + cond_mood, data = final, family = binomial))
tab_model(m1)

# [not in text] - Exploratory model - including mood * habit interaction

summary(mint <- glm(ask ~ cond_hab * cond_mood, data = final, family = binomial))
tab_model(mint)

# Habit and Mood Effects on Helping - Attributed

t.test(final$slider_mood.response, final$slider_habit.response, paired = T)

cohen.d(final$slider_mood.response, final$slider_habit.response, 
        paired = T, na.rm = T)

# Incentivized other-attribution measure

t.test(final$slider_mood_all.response, final$slider_habit_all.response, paired = T)

cohen.d(final$slider_mood_all.response, final$slider_habit_all.response, 
        paired = T, na.rm = T)

# Exploratory Analyses: Intensity of Experience and Attributions 

cor.test(final$slider_mood.response, final$pos)
cor.test(final$slider_mood.response, final$neg)
cor.test(final$slider_mood.response, final$median_rt)

cor.test(final$slider_habit.response, final$pos)
cor.test(final$slider_habit.response, final$neg)
cor.test(final$slider_habit.response, final$median_rt)

##################################
# Supplemental Materials Analyses
##################################

# Sensitivity analyses - replicating main analysis without excluding participants

summary(m1all <- glm(asknum ~ cond_hab + cond_mood, data = final.all, family = binomial))
tab_model(m1all)

# Fit Mood-only and Habit-only models and calculate Tjur's R2

mmood <- glm(ask ~ cond_mood, data = final, family = binomial)
mhab <- glm(ask ~ cond_hab, data = final, family = binomial)

(rmood <- performance::r2(mmood)[[1]])
(rhab <- performance::r2(mhab)[[1]])

# Compare perceived and actual explained variance

testtab <- final %>% 
  dplyr::select(slider_habit.response, slider_mood.response) %>% 
  mutate(rmood = rmood,
         rhab = rhab,
         diff = (rmood - rhab) * 100,
         percdiff = slider_mood.response - slider_habit.response,
  )

t.test(testtab$percdiff, mu = testtab$diff[1])

cohen.d(testtab$percdiff, testtab$diff, na.rm = T)

# SRBAI and PANAS main analyses

summary(m1_proc <- glm(ask ~ srbai_avg_c + pos.c + neg.c, 
                       data = final, family = binomial))
tab_model(m1_proc)



