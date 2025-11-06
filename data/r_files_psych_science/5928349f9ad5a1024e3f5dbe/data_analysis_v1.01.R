##### Setup ######

# load output from data_cleaning_final
load('allStudies_final.RData')

# load functions I've written used throughout the documents
source('auxillary_functions_v0.10.R') 

## packages ##
library(tidyverse)
library(lme4)
library(lmerTest)
library(diptest)

##### Study 1 #####

# First test whether curvature is influenced by trial type
# AUC = Area under the curve (spatial conflict)
# conditionRC = trial type: 1 = self-control trials, 0 = comparison trials
lmer(AUC ~ conditionRC + (1|subject), data = s1) %>% summary() # significant greater conflict in the self-control trials

# Next we look at aggregate AUC scores (aggregate within participants) 
# in self-control conflict and comparison trials predicting choice
# choice: 1 = chose apple, 0 = chose chocolate
# conflict_meanAUC = mean AUC in self-control conflict trials 
# comparison_meanAUC = mean AUC in comparison trials
glm(choice ~ conflict_meanAUC, family = 'binomial', data = s1_agg) %>% summary() # marginal effect of average AUC in self-control trials on choice
glm(choice ~ conflict_meanAUC + comparison_meanAUC, family = 'binomial', data = s1_agg) %>% summary() # still marginal when controlling for average AUC in comparison trials

# Maximum deviation > .9
# table 1 (Study 1): proportion of abrupt trials
t1_1 <- group_by(s1, condition) %>% count(mdAbrupt)
t1_1$mdAbrupt <- recode(t1_1$mdAbrupt, `1` = 'Abrupt', `0` = 'Smooth')
t1_1 %>% spread(condition, n)

# choice as a function of proportion of abrupt trajectories in the self-control trials
# conflict_abrupt: proportion of abrupt trajectories in the self-control trials for each participant 
glm(choice ~ conflict_abrupt, family = 'binomial', data = s1_agg) %>% summary() # more abrupt trajectories --> less likely to choose the apple

# Bimodality tests, done using the diptest package
# Gives a p-value for the null hypothesis of unimodality (so significant = not unimodal)
# select either the self-control conflict or comparison trials, then test the bimodality 
filter(s1, condition == 'conflict')$AUC %>% dip.test() # bimodality of self-control trials
filter(s1, condition == 'comparison')$AUC %>% dip.test() # bimodality of comparison trials

##### Study 2 #####

# First look at main effects of trial type on AUC
# isConflict_updated: 1 = self-control trials, 0 = comparison trials
lmer(AUC ~ isConflict_updated + (1|subject), data = s2) %>% summary() # greater conflict in the comparison trials

# Two-way interaction of trial type and self-control on AUC:
# allSC is a composite of the four individual measures (tangneySC, BIS, chen_efficacy, and kalk_efficacy)
# higher = greater self-reported self-control
lmer(AUC ~ allSC * isConflict_updated + (1|subject), data = s2) %>% summary() # significant interaction between trial type and self-control
probeInteraction(AUC ~ allSC * isConflict_updated + (1|subject), s2, lmer) # self-control leads to less conflict, but only for self-control trials
# note, for details on the probeInteraction function, see the "auxillary_functions" file

# Three-way interaction of trial type, self-control, and dieting status 
# diet_currentStatus_avg = higher numbers correspond to greater agreement with being on a diet
lmer(AUC ~ allSC * diet_currentStatus_avg * isConflict_updated + (1|subject), data = s2) %>% summary() # significant 3-way interaction
probeInteraction(AUC ~ allSC * diet_currentStatus_avg * isConflict_updated + (1|subject), s2, lmer) # self-control leads to less conflict, but only for self-control trials and only for dieters

# Figure 4 #
# Note, the following line takes a long time to run. For graphing, I transferred the values to excel
f4_data <- estimate_traj(AUC ~ allSC * diet_currentStatus_avg * isConflict_updated + (1|subject), s2, lmer)
f4_data2 <- filter(f4_data, str_detect(condition, "highisConflict_updated")) # only interested in self-control trials
ggplot(f4_data2, aes(x = x, y = y, fill = condition, color = condition)) + geom_line()


# Controlling for reaction times:
# RT = reaction time on the trial
lmer(AUC ~ allSC * isConflict_updated + RT + (1|subject), data = s2) %>% summary() # not quite significant
lmer(AUC ~ allSC * diet_currentStatus_avg * isConflict_updated + RT + (1|subject), data = s2) %>% summary() # still significant
probeInteraction(AUC ~ allSC * diet_currentStatus_avg * isConflict_updated + RT + (1|subject), s2, lmer) # simple slopes remain significant

# Dieting efficacy:
# diet_efficacy_avg = degree to which participants think they are good at dieting
lmer(AUC ~ diet_efficacy_avg * isConflict_updated + (1|subject), data = s2) %>% summary() # marginally significant
probeInteraction(AUC ~ diet_efficacy_avg * isConflict_updated + (1|subject), s2, lmer) # no significant simple slopes
lmer(AUC ~ diet_efficacy_avg * diet_currentStatus_avg * isConflict_updated + (1|subject), data = s2) %>% summary() # Marginally significant
probeInteraction(AUC ~ diet_efficacy_avg * diet_currentStatus_avg * isConflict_updated + (1|subject), s2, lmer) # no significant simple slopes

## Maximum deviation > .9 ##
# table 1 (Study 2)
t1_2 <- group_by(s2, conditionRC) %>% count(mdAbrupt)
t1_2$mdAbrupt <- recode(t1_2$mdAbrupt, `1` = 'Abrupt', `0` = 'Smooth')
t1_2 %>% spread(conditionRC, n)

# Test whether showing an abrupt trajectory is related to self-reported self-control
# mdAbrupt = whether or not MD > .9 or not
# 1 = abrupt, 0 = smooth
glmer(mdAbrupt ~ allSC * isConflict_updated + (1|subject), family = 'binomial', data = s2) %>% summary() # significant interaction
probeInteraction(mdAbrupt ~ allSC * isConflict_updated + (1|subject), s2, glmer, family = 'binomial') # heightened self-control related to lower probability of an abrupt trajectory, but only in self-control trials
glmer(mdAbrupt ~ allSC * diet_currentStatus_avg * isConflict_updated + (1|subject), family = 'binomial', data = s2) %>% summary() # significant interaction
probeInteraction(mdAbrupt ~ allSC * diet_currentStatus_avg * isConflict_updated + (1|subject), s2, glmer, family = 'binomial') # heightened self-control related to lower probability of an abrupt trajectory, but only in self-control trials and only for dieters

# Figure 5 #
disc_sc_trial_diet = estimatePoints(mdAbrupt ~ allSC * isConflict_updated * diet_currentStatus_avg + (1|subject), s2, glmer, family='binomial')
disc_sc_trial_diet$diet = as.factor(disc_sc_trial_diet$diet_currentStatus_avg)
levels(disc_sc_trial_diet$diet) = c('Non-Dieters', 'Dieters')
f5 <- ggplot(data=disc_sc_trial_diet, aes(y = estimate, x = as.factor(isConflict_updated), fill = as.factor(allSC))) +
  facet_wrap( ~ diet) +
  geom_bar(position = position_dodge(), stat='identity', color = 'black', size = .3) +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), size = .3, width = .2, position = position_dodge(.9)) +
  theme_apa(textSize = 10) +
  xlab("Trial Type") +
  scale_x_discrete(labels = c('Comparison', 'Self-Control')) +
  ylab("Log-odds of a abrupt trajectory") +
  #scale_fill_hue(name = "Composite Self-Control", 
  #labels = c("Low (-1 SD)", "High (+1 SD)")) +
  scale_fill_grey(name = "Composite Self-Control", 
                  labels = c("Low (-1 SD)", "High (+1 SD)"))
# ggsave('./figure5.pdf', plot = f4, width = 6.5, height = 4.05, units = 'in')

## Bimodality ##
filter(s2, conditionRC == 'conflict')$AUC %>% dip.test() # no evidence of bimodality in self-control trials
filter(s2, conditionRC == 'comparison')$AUC %>% dip.test() # evidence of bimodality in comparison trials

##### Study 3a/3b #####
# SS stands for smaller-sooner (e.g., $5 today)
# LL stands for larger-later (e.g., $25 in 3 months)
# choseLL = whether they chose the LL or the SS (1 = chose LL, 0 = chose SS) 

## Effect of manipulations on AUC ##
# Since choice is unrestricted, always need to look at the interaction with 
# choice (we make strongest predictions when they chose the LL)

# Study 3a:
# LL_first = whether or not they listed reasons to chose the LL (vs. SS) first in the query order manipulation 
# 1 = listed LL reasons first, -1 = listed SS reasons first
lmer(AUC ~ LL_first * choseLL + (1|subject), data = s3a) %>% summary() # significant interaction
probeInteraction(AUC ~ LL_first * choseLL + (1|subject), s3a, lmer) # when choosing the larger later, having listed reasons for the LL first makes people significantly more direct

# Study 3b:
# construal = manipulated construal level, either high or low
# construalRC = recoded construal level, 1 = high-level construal, -1 = low-level construal
lmer(AUC ~ construalRC * choseLL + (1|subject), data = s3b) %>% summary() # significant interaction
probeInteraction(AUC ~ construalRC * choseLL + (1|subject), s3b, lmer) # when choosing the larger later, high-level construal makes individuals more direct

# Figures 7 and 8 #
# NOTE: these take a long time to run (each requires 202 lmer models)
# Figure 7
f7_data <- estimate_traj(AUC ~ LL_first * choseLL + (1|subject), s3a, lmer)
f7_data$x <- ifelse(f7_data$condition == 'lowLL_first_lowchoseLL' | f7_data$condition == 'highLL_first_lowchoseLL', -f7_data$x, f7_data$x) # recode to rightward if chose the SS
ggplot(f7_data, aes(x = x, y = y, fill = condition, color = condition)) + geom_line()

# Figure 8
f8_data <- estimate_traj(AUC ~ construalRC * choseLL + (1|subject), s3b, lmer)
f8_data$x <- ifelse(f8_data$condition == 'lowconstrualRC_lowchoseLL' | f8_data$condition == 'highconstrualRC_lowchoseLL', -f8_data$x, f8_data$x) # recode to rightward if chose the SS
ggplot(f8_data, aes(x = x, y = y, fill = condition, color = condition)) + geom_line()


## Effect of curvature on decisions ##
# Next test whether the total number of larger-later decisions is predicted by 
# average curvature in trials in which participants chose the LL or the SS
# numDec = total number of LL decisions made
# SS_meanAUC = subject mean AUC for trials in which they selected the SS
# LL_meanAUC = subject mean AUC for trials in which they selected the LL
# Also test whether the effects of curvature hold when controlling for RT:
# SS_rt = subject mean reaction time for trials in which they selected the SS
# LL_rt = subject mean reaction time for trials in which they selected the LL
# All analyses done controlling for the manipulation (results don't change when not controlling for manipulation)

# Study 3a
lm(numDec ~ SS_meanAUC + LL_meanAUC + LL_first, data = s3a_agg) %>% summary() # highly significant for both trial averages
lm(numDec ~ SS_meanAUC + LL_meanAUC + SS_rt + LL_rt + LL_first, data = s3a_agg) %>% summary() # still significant when controlling for RT

# Study 3b
lm(numDec ~ SS_meanAUC + LL_meanAUC + construal, data = s3b_agg) %>% summary() # highly significant for both trial averages
lm(numDec ~ SS_meanAUC + LL_meanAUC + SS_rt + LL_rt + construal, data = s3b_agg) %>% summary() # still significant when controlling for RT

# One concern with above analyses is that they might constitute "double dipping"
# The following analyses calculate mean curvature on the first half of trials 
# then predict choices on the second half of trials
# Thanks to Eliot Smith for this analysis suggestion

# first indicate whether a trial was in the first half or second half
s3a$half <- ifelse(s3a$trial < 102, 'first', 'second')
s3b$half <- ifelse(s3b$trial < 120, 'first', 'second')

# Then we need to re-aggregate based on what trial
# The following gives us a dataframe with one row per participant, and
# AUC (for both LL and SS decisions) and numDec for the first and second half of the experiment
s3a_RS <- s3a %>% group_by(subject, half, choiceRC) %>%
  summarise(auc = mean(AUC), dec = sum(choseLL), rt = mean(RT), md = mean(MD), LL_first = first(LL_first)) %>%
  gather(variable, val, -subject, -choiceRC, -half, -LL_first) %>% 
  unite(temp1, half, variable, choiceRC, sep = "_") %>% 
  spread(temp1, val) %>%
  rename(first_dec = first_dec_LL, second_dec = second_dec_LL) %>%
  select(-second_dec_SS, -first_dec_SS)

s3b_RS <- s3b %>% group_by(subject, half, choiceRC) %>%
  summarise(auc = mean(AUC), dec = sum(choseLL), rt = mean(RT), md = mean(MD), construal = first(construalRC)) %>%
  gather(variable, val, -subject, -choiceRC, -half, -construal) %>% 
  unite(temp1, half, variable, choiceRC, sep = "_") %>% 
  spread(temp1, val) %>%
  rename(first_dec = first_dec_LL, second_dec = second_dec_LL) %>%
  select(-second_dec_SS, -first_dec_SS)

# predict number of larger later decisions in the second half (= second_dec) from
# first-half AUC for both LL (= first_auc_LL) and SS (=first_auc_SS) trials (while controlling for manipulation)
# Then do same analysis controlling for RT in the first half
lm(second_dec ~ first_auc_LL + first_auc_SS + LL_first, data = s3a_RS) %>% summary() # highly significant for both trial types
lm(second_dec ~ first_auc_LL + first_auc_SS + first_rt_LL + first_rt_SS + LL_first, data = s3a_RS) %>% summary() # still significant when controlling for RT

lm(second_dec ~ first_auc_LL + first_auc_SS + construal, data = s3b_RS) %>% summary() # highly significant for both trial types
lm(second_dec ~ first_auc_LL + first_auc_SS + first_rt_LL + first_rt_SS + construal, data = s3b_RS) %>% summary() # effect in LL trials stays significant, but SS drops to marginal

## Effect of manipulation on choices ##
# look to see if our manipulations influenced choice, ignoring mouse-tracking metrics
glmer(choseLL ~ LL_first + (1|subject), family = 'binomial', data = s3a) %>% summary() # ns
glmer(choseLL ~ construalRC + (1|subject), family = 'binomial', data = s3b) %>% summary() # (barely) significant

## Mediation analysis for study 3b ##
# look to see if changes in average curvature when choosing the LL mediates 
# the effect of construal on choice
# use the mediation package to test these (not loaded, as it conflicts with other packages)
s3b_med_fit <- lm(LL_meanAUC ~ construal, data = s3b_agg)
s3b_out_fit <- lm(numDec ~ LL_meanAUC + construal, data = s3b_agg)
s3b_med <- mediation::mediate(s3b_med_fit, s3b_out_fit, sims = 20000, boot = TRUE, boot.ci.type = 'bca', 
                     treat = 'construal', mediator = 'LL_meanAUC', conf.level = .99)
summary(s3b_med) # significant indirect effect (ACME)

# controlling for mean curvature in the smaller-sooner trials
s3b_medC_fit <- lm(LL_meanAUC ~ construal + SS_meanAUC, data = s3b_agg)
s3b_outC_fit <- lm(numDec ~ LL_meanAUC + construal + SS_meanAUC, data = s3b_agg)
s3b_medC <- mediation::mediate(s3b_medC_fit, s3b_outC_fit, sims = 20000, boot = TRUE, boot.ci.type = 'bca', 
                    treat = 'construal', mediator = 'LL_meanAUC', conf.level = .99)
summary(s3b_medC) # still significant when controlling for SS_meanAUC

## Maximum deviation ##
# Next look at trials that appear abrupt (MD >= .9) or smooth (MD < .9)
# Table 2:
# Study 3a:
t2_a <- group_by(s3a, choseLL, LL_first) %>% count(mdAbrupt)
t2_a$mdAbrupt <- recode(t2_a$mdAbrupt, `1` = 'Abrupt', `0` = 'Smooth')
t2_a$LL_first <- recode(t2_a$LL_first, `1` = 'LL_first', `-1` = 'SS_first')
t2_a$choseLL <- recode(t2_a$choseLL, '0' = 'choseSS', `1` = 'choseLL')
t2_a %>% unite(choice.condition, choseLL, LL_first, sep = '.') %>% spread(choice.condition, n)

# Study 3b:
t2_b <- group_by(s3b, choseLL, construalRC) %>% count(mdAbrupt)
t2_b$mdAbrupt <- recode(t2_b$mdAbrupt, `1` = 'Abrupt', `0` = 'Smooth')
t2_b$construalRC <- recode(t2_b$construalRC, `1` = 'HLC', `-1` = 'LLC') # HLC = high-level construal, LLC = low-level construal
t2_b$choseLL <- recode(t2_a$choseLL, '0' = 'choseSS', `1` = 'choseLL')
t2_b %>% unite(choice.condition, choseLL, construalRC, sep = '.') %>% spread(choice.condition, n)

# Predict whether an abrupt trajectory is predicted by condition interacted with choice
# Study 3a
glmer(mdAbrupt ~ choseLL * LL_first + (1|subject), family = 'binomial', data = s3a) %>% summary() # significant interaction
probeInteraction(mdAbrupt ~ LL_first * choseLL + (1|subject), data = s3a, glmer, family = 'binomial') # less likely to show an abrupt trajectory if listed LL reasons first, but only when choosing the LL

# Study 3b
glmer(mdAbrupt ~ choseLL * construalRC + (1|subject), family = 'binomial', data = s3b) %>% summary() # significant interaction
probeInteraction(mdAbrupt ~ construalRC * choseLL + (1|subject), data = s3b, glmer, family = 'binomial') # less likely to show an abrupt trajectory under high-level construal, but only when choosing the LL

# Figure 10 #
e_cond_choice <- estimatePoints(mdAbrupt ~ LL_first * choseLL + (1|subject), s3a, glmer, family='binomial')
e_construal_choice <- estimatePoints(mdAbrupt ~ construalRC * choseLL + (1|subject), s3b, glmer, family='binomial')
e_s3a_new <- rename(e_cond_choice, Self_Control = LL_first)  
e_s3b_new <- rename(e_construal_choice, Self_Control = construalRC)  

e_both <- rbind(e_s3a_new, e_s3b_new)
e_both$study <- as.factor(c('Study 3a','Study 3a','Study 3a','Study 3a','Study 3b','Study 3b','Study 3b','Study 3b'))
f10 <- ggplot(data = e_both, aes(y = estimate, x = as.factor(choseLL), fill = as.factor(Self_Control))) +
  facet_wrap( ~ study) +
  geom_bar(position = position_dodge(), stat='identity', color = 'black', size = .3) +
  geom_errorbar(aes(ymin = estimate - se, ymax = estimate + se), size = .3, width = .2, position = position_dodge(.9)) +
  theme_apa(textSize = 9) +
  xlab("Choice") +
  scale_x_discrete(labels = c('Smaller-Sooner', 'Larger-Later')) +
  ylab("Log-odds of a abrupt trajectory") +
  scale_fill_grey(name = "Manipulated Self-Control", 
                  labels = c("Low", "High"))

# ggsave('./figure10.pdf', plot = f9, width = 6.5, height = 4.05, units = 'in')

## Bimodality ##
# Study 3a
# first focus on decisions where participants chose the LL 
dip.test(filter(s3a, choseLL == 1)$AUC) # evidence of bimodality
# then can break down by choice and condition:
dip.test(filter(s3a, choseLL == 1, LL_first == 1)$AUC) # evidence of bimodality
dip.test(filter(s3a, choseLL == 1, LL_first == -1)$AUC) # evidence of bimodality
dip.test(filter(s3a, choseLL == 0, LL_first == 1)$AUC) # evidence of bimodality
dip.test(filter(s3a, choseLL == 0, LL_first == -1)$AUC) # evidence of bimodality

# Study 3b
dip.test(filter(s3b, choseLL == 1)$AUC) # No evidence of bimodality
dip.test(filter(s3b, choseLL == 1, construalRC == 1)$AUC) # no evidence of bimodality
dip.test(filter(s3b, choseLL == 1, construalRC == -1)$AUC) # no evidence of bimodality
dip.test(filter(s3b, choseLL == 0, construalRC == 1)$AUC) # no evidence of bimodality
dip.test(filter(s3b, choseLL == 0, construalRC == -1)$AUC) # almost marginal evidence of bimodality

##### Coda #####
# Want to look at bimodality across all studies since study 3a is different from the others
# get a dataframe that contains all AUC information across studies:
s1$conditionRC <- as.character(s1$condition)
s2$conditionRC <- as.character(s2$isConflict_updated)
s3a$conditionRC <- as.character(s3a$LL_first)
s3b$conditionRC <- as.character(s3b$construal)
s1_AUC <- dplyr::select(s1, AUC, conditionRC) %>% # (note, the dplyr::select is needed because mediation loads the MASS package, which masks the select command)
  mutate(study = 'Study 1', choseLL = NA, primaryInterest = ifelse(conditionRC == 'conflict', 1, 0))
s2_AUC <- dplyr::select(s2, AUC, conditionRC) %>% 
  mutate(study = 'Study 2', choseLL = NA, primaryInterest = ifelse(conditionRC == 1, 1, 0))
s3a_AUC <- dplyr::select(s3a, AUC, choseLL, conditionRC) %>%
  mutate(study = 'Study 3a', primaryInterest = ifelse(conditionRC == 1 & choseLL == 1, 1, 0))
s3b_AUC <- dplyr::select(s3b, AUC, choseLL, conditionRC) %>% 
  mutate(study = 'Study 3b', primaryInterest = ifelse(conditionRC == 'high' & choseLL == 1, 1, 0))

allAUC <- rbind(s1_AUC, s2_AUC, s3a_AUC, s3b_AUC) # dataframe of all trajectories
# Want to use only the trajectories we make predictions about 
# --> self-control trials in s1-2 and LL choices + high manipulated self-control in s3a-b
primaryAUC <- filter(allAUC, primaryInterest == 1)
dip.test(primaryAUC$AUC) # no evidence of bimodality

# Figure 3
primary_all <- primaryAUC
primary_all$study <- 'All Four Studies'
primary_merged <- rbind(primaryAUC, primary_all)
primary_merged$study_f <- factor(primary_merged$study, levels = c('Study 1', 'Study 2', 'Study 3a', 'Study 3b', 'All Four Studies'))

f3 <- ggplot(primary_merged, aes(x=AUC)) + 
  facet_wrap(~study_f, ncol=1, scales = "free_y") +
  geom_histogram(bins = 50) + 
  theme_apa(11) + 
  xlab("Spatial Conflict") + 
  ylab("Count") +
  scale_x_continuous(limits = c(-2, 6.5))

# ggsave('./figure3.pdf', plot = f10, width = 6.5, height = 5.8, units = 'in')




