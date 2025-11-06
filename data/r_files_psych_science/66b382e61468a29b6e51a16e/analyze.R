## Primary analysis code for Tandoc et al. (2024, Psych Sci) — Directing attention hurts learning in adults but not children
# Marlie Tandoc (July 2024)

# Please see readme file for description of input data and manuscript for details of statistical analyses and models
# If you have any questions please contact Marlie, tandoc@sas.upenn.edu

#### INSTALL AND LOAD PACKAGES AND DEFINE FUNCTIONS ####

# Install pacman if it's not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Load pacman
library(pacman)

# Use pacman to install and load packages
pacman::p_load(rmarkdown, tidyverse, ggplot2, ggpubr, gridExtra,
               lme4, lmerTest, emmeans, ggsignif, ggeffects,
               ggbeeswarm, psych, effectsize, lsr, MuMIn,
               sjPlot, rstudioapi)

## EFFECT SIZE FUNCTIONS ##
#functions for effect size calculations
cohensD_posthoc = function(dat) {
  
  means_sds <- dat %>%
    group_by(age_group,block_num) %>%
    summarize(mean = mean(learning_sensitivity),
              sd = sd(learning_sensitivity),
              len = n()) %>%
    mutate(id = paste(age_group,block_num, sep = "_"))
  # 
  
  
  # Print the dataframe
  #list of all possible combinations
  combs<- as.data.frame(t(combn(means_sds$id, 2)))
  
  #add identifier column (id) to original dat too
  dat <- dat %>%
    mutate(id = paste(age_group,block_num, sep = "_"))
  
  #init dataframe to save effect sizes
  df <- data.frame(group1 = character(nrow(combs)),group2 =character(nrow(combs)), d = numeric(nrow(combs)), CI_low= numeric(nrow(combs)), CI_up= numeric(nrow(combs)))
  
  #loop through each possible combination and calculate effect size
  for (c in 1:nrow(combs)) {
    
    print(combs[c,])
    cur <- combs[c,]
    
    #manual cohens d sanity check
    # g1 <- filter(means_sds, id == cur$V1) # group 1 info
    # g2 <- filter(means_sds, id == cur$V2) # group 2 info
    # 
    # pooled_sd <- sqrt(((g1$len-1)*g1$sd^2 + (g2$len-1)*g2$sd^2) / (g1$len+g2$len-2))
    # 
    # diff = mean(g1$mean) - mean(g2$mean)
    # 
    # diff/pooled_sd
    
    # its equivalent to using normal cohensD function from lsr package so lets just use that
    group1 <-  filter(dat,id == cur$V1)
    group2 <-filter(dat,id == cur$V2)
    d <- cohensD(group1$learning_sensitivity, group2$learning_sensitivity)
    
    CIs = cohen.d.ci(d =d, 
                     n1 =nrow(group1), 
                     n2 = nrow(group2))
    
    
    print(d)
    #save in resulting data frame
    df$group1[c] <- cur$V1[1]
    df$group2[c] <- cur$V2[1]
    df$d[c] <- d
    df$CI_low[c] <- as.numeric(CIs[1,][1])
    df$CI_up[c] <- as.numeric(CIs[1,][3])
    
    
  }
  
  df
  
  
}

#for across experiment two-way anova
cohensD_posthoc_across = function(dat) {
  
  means_sds <- dat %>%
    group_by(age_group,experiment) %>%
    summarize(mean = mean(learning_sensitivity),
              sd = sd(learning_sensitivity),
              len = n()) %>%
    mutate(id = paste(age_group,experiment, sep = "_"))
  # 
  
  
  # Print the dataframe
  #list of all possible combinations
  combs<- as.data.frame(t(combn(means_sds$id, 2)))
  
  #add identifier column (id) to original dat too
  dat <- dat %>%
    mutate(id = paste(age_group,experiment, sep = "_"))
  
  #init dataframe to save effect sizes
  df <- data.frame(group1 = character(nrow(combs)),group2 =character(nrow(combs)), d = numeric(nrow(combs)), CI_low= numeric(nrow(combs)), CI_up= numeric(nrow(combs)))
  
  #for manual calculation
  
  #loop through each possible combination and calculate effect size
  for (c in 1:nrow(combs)) {
    
    print(combs[c,])
    cur <- combs[c,]
    
    #manual cohens d sanity check
    # g1 <- filter(means_sds, id == cur$V1) # group 1 info
    # g2 <- filter(means_sds, id == cur$V2) # group 2 info
    # 
    # pooled_sd <- sqrt(((g1$len-1)*g1$sd^2 + (g2$len-1)*g2$sd^2) / (g1$len+g2$len-2))
    # 
    # diff = mean(g1$mean) - mean(g2$mean)
    # 
    # diff/pooled_sd
    
    #yes its equivalent to using normal cohensD function from lsr package so lets just se that
    group1 <-  filter(dat,id == cur$V1)
    group2 <-filter(dat,id == cur$V2)
    d <- cohensD(group1$learning_sensitivity, group2$learning_sensitivity)
    
    CIs = cohen.d.ci(d =d, 
                     n1 =nrow(group1), 
                     n2 = nrow(group2))
    
    
    print(d)
    #save in resulting data frame
    df$group1[c] <- cur$V1[1]
    df$group2[c] <- cur$V2[1]
    df$d[c] <- d
    df$CI_low[c] <- as.numeric(CIs[1,][1])
    df$CI_up[c] <- as.numeric(CIs[1,][3])
    
    
  }
  
  df
  
  
}

#for across experiment three-way anova
cohensD_posthoc_across_three = function(dat) {
  
  means_sds <- dat %>%
    group_by(age_group,experiment, block_num) %>%
    summarize(mean = mean(learning_sensitivity),
              sd = sd(learning_sensitivity),
              len = n()) %>%
    mutate(id = paste(age_group,experiment,block_num, sep = "_"))
  # 
  
  
  # Print the dataframe
  #list of all possible combinations
  combs<- as.data.frame(t(combn(means_sds$id, 2)))
  
  #add identifier column (id) to original dat too
  dat <- dat %>%
    mutate(id = paste(age_group,experiment,block_num, sep = "_"))
  
  #init dataframe to save effect sizes
  df <- data.frame(group1 = character(nrow(combs)),group2 =character(nrow(combs)), d = numeric(nrow(combs)), CI_low= numeric(nrow(combs)), CI_up= numeric(nrow(combs)))
  
  #for manual calculation
  
  #loop through each possible combination and calculate effect size
  for (c in 1:nrow(combs)) {
    
    print(combs[c,])
    cur <- combs[c,]
    
    #manual cohens d sanity check
    # g1 <- filter(means_sds, id == cur$V1) # group 1 info
    # g2 <- filter(means_sds, id == cur$V2) # group 2 info
    # 
    # pooled_sd <- sqrt(((g1$len-1)*g1$sd^2 + (g2$len-1)*g2$sd^2) / (g1$len+g2$len-2))
    # 
    # diff = mean(g1$mean) - mean(g2$mean)
    # 
    # diff/pooled_sd
    
    #yes its equivalent to using normal cohensD function from lsr package so lets just se that
    group1 <-  filter(dat,id == cur$V1)
    group2 <-filter(dat,id == cur$V2)
    d <- cohensD(group1$learning_sensitivity, group2$learning_sensitivity)
    
    CIs = cohen.d.ci(d =d, 
                     n1 =nrow(group1), 
                     n2 = nrow(group2))
    
    
    print(d)
    #save in resulting data frame
    df$group1[c] <- cur$V1[1]
    df$group2[c] <- cur$V2[1]
    df$d[c] <- d
    df$CI_low[c] <- as.numeric(CIs[1,][1])
    df$CI_up[c] <- as.numeric(CIs[1,][3])
    
    
  }
  
  df
  
  
}

#### CLEAN AND PREP DATA ####

#Set directory to where current analysis script is
script_path <- rstudioapi::getActiveDocumentContext()$path
script_dir <- dirname(script_path)
setwd(script_dir)

#Read in data
test_data <- read.csv('data/pic_frag_completion_both_exp.csv') # both experiment picture fragment completion task (test data)
nback_data <-read.csv('data/nback_data_exp_2.csv') #nback task data from experiment 2

#reorder age group as factors
test_data$age_group <- factor(test_data$age_group,levels=c("child","adult"))
nback_data$age_group <- factor(nback_data$age_group,levels=c("child","adult"))

#Wclude incorrect trials correct response trials only 
corr_test_trials = filter(test_data, response_correct == 1)
#Note: the input dataframe already excludes trials that were too fast (e.g. lower than 300ms). As reported in the paper there were very few of these to begin with.

## MIXED EFFECTS MODEL PREP ##

#Contrast code/center variables (c) for model
corr_test_trials <- corr_test_trials %>% mutate(block_c = ifelse(block_num == 1, -1, 1),
                               old_c = ifelse(old == 1, 1,-1), 
                               age_group_c = ifelse(age_group == 'child',1,-1),
                               experiment_c = ifelse(experiment == '1',1,-1))

#Prepare model dataframes for each expereiment
e1_only <-  filter(corr_test_trials, experiment == '1')
e2_only <-  filter(corr_test_trials, experiment == '2')


#### EXPERIMENT 1 PRIMARY MAIN ANALYSES ####

#Mixed effects model results

#orignal model has singularity issues
#m1_lm <- lmer(identified_level ~ old_c * age_group_c *block_c + (old + block_c|participant), e2_only)
#summary(m1_lm)

#Singularity issue still with optomizer
#m1_lm <- lmer(identified_level~ old_c * age_group_c *block_c + (old + block_c|participant), data=e2_only,
#   control=lmerControl(optimizer="bobyqa",
# optCtrl=list(maxfun=10000)))

# Model reported in paper in Table 1. Double || fixes singularity issue
#variables centered (c) for interpretation
m1_lm <- lmer(identified_level~ old_c * age_group_c *block_c + (old_c + block_c||participant), data=e1_only,
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=100000)))
summary(m1_lm)
r.squaredGLMM(m1_lm)
tab_model(m1_lm) 

#Post-hoc tests on model point estimates

#estimated marginal means and contrasts in each block
m1_emms <- emmeans(m1_lm, pairwise ~ old_c*age_group_c | block_c,pbkrtest.limit = 3400)
summary(m1_emms) #why are these slightly different lol then paper lol?

#to compare learning sensitivity effect/slopes averaged across blocks
m1_priming_slopes_ignore_block <- emtrends(m1_lm, specs = pairwise ~ age_group_c, var="old_c",infer = T,pbkrtest.limit = 3400)
summary(m1_priming_slopes_ignore_block)

#compare learning sensitivity effect/slopes (for old_c) for children and adults in each block
m1_priming_slopes <- emtrends(m1_lm, specs = pairwise ~ block_c*age_group_c, var="old_c",infer = T,pbkrtest.limit = 3400)
summary(m1_priming_slopes)

#### EXPERIMENT 1 PRIMARY SUPPLEMENTARY ANALYSES ####
#Also includes stats for Figure 2B-C

#test data to analyze in experiment 1
e1_test_data <- filter(corr_test_trials, experiment == '1')

#summary dataframes of experiment 1
e1_test_summary = e1_test_data  %>% 
  group_by(participant,experiment) %>% 
  summarise(mean_old = mean(identified_level[which(old == 1)]),
            mean_new = mean(identified_level[which(old == 0)]),
            age = age[1],
            age_group = age_group[1],
            n_correct = length(participant)) %>%
  mutate(learning_sensitivity = mean_new - mean_old)

e1_test_summary_block = e1_test_data %>% 
  group_by(participant, block_num,experiment) %>% 
  summarise(mean_old = mean(identified_level[which(old == 1)]),
            mean_new = mean(identified_level[which(old == 0)]),
            age = age[1],
            age_group = age_group[1],
            n_correct = length(participant)) %>%
  mutate(learning_sensitivity = mean_new - mean_old)

#Do children and adults differ in how fast they correctly identify OLD images?
t.test(mean_old ~ age_group, filter(e1_test_summary), var.equal = TRUE)

cohen.d.ci(d =cohensD(mean_old ~ age_group, filter(e1_test_summary)), 
           n1 =nrow(e1_test_summary[which(e1_test_summary$age_group == 'child'),]), 
           n2 = nrow(e1_test_summary[which(e1_test_summary$age_group == 'adult'),]))

#Do children and adults differ in how fast they correctly identify NEW images?
t.test(mean_new ~ age_group, filter(e1_test_summary), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_new ~ age_group, filter(e1_test_summary)), 
           n1 =nrow(e1_test_summary[which(e1_test_summary$age_group == 'child'),]), 
           n2 = nrow(e1_test_summary[which(e1_test_summary$age_group == 'adult'),]))

#Do adults show significant learning (above-zero learning sensitivity scores)?
t.test(e1_test_summary$learning_sensitivity[which(e1_test_summary$age_group == 'adult')], mu = 0)
cohen.d.ci(d =cohensD(e1_test_summary$learning_sensitivity[which(e1_test_summary$age_group == 'adult')], mu = 0), 
           n1 =nrow(e1_test_summary[which(e1_test_summary$age_group == 'adult'),]), 
           n2 = nrow(e1_test_summary[which(e1_test_summary$age_group == 'adult'),]))# mu = 0 so n2 is same as n1

#Do children show significant learning (above-zero learning sensitivity scores)?
t.test(e1_test_summary$learning_sensitivity[which(e1_test_summary$age_group == 'child')], mu = 0)
cohen.d.ci(d =cohensD(e1_test_summary$learning_sensitivity[which(e1_test_summary$age_group == 'child')], mu = 0), 
           n1 =nrow(e1_test_summary[which(e1_test_summary$age_group == 'child'),]), 
           n2 = nrow(e1_test_summary[which(e1_test_summary$age_group == 'child'),]))# mu = 0 so n2 is same as n1

#Do children show more priming (learning sensitivity scores) than adults?
t.test(learning_sensitivity~ age_group, filter(e1_test_summary), var.equal = TRUE)
cohen.d.ci(d =cohensD(learning_sensitivity ~ age_group, filter(e1_test_summary)), 
           n1 =nrow(e1_test_summary[which(e1_test_summary$age_group == 'child'),]), 
           n2 = nrow(e1_test_summary[which(e1_test_summary$age_group == 'adult'),]))

#Interaction anova age group x test block 
a1 <- aov(learning_sensitivity ~ as.factor(age_group) * as.factor(block_num), filter(e1_test_summary_block))
summary(a1)
eta_squared(a1, partial = TRUE)

#post hoc tukeys
tukey1 <- TukeyHSD(a1)
tukey1
cohensD_posthoc(e1_test_summary_block)

e1_test_data['full_rt'] <- NA #absolute RT (response_time var is the RT at each level so max is 1.5)

#Calculate total RT it takes to identify image
for (i in 1:nrow(e1_test_data)) {
  #We multiply it by 1.5 because each level is 1.5 seconds... subtract 1 off identified_level to make first level clock start at 0
  #And then we also add the response_time because this (is always less then 1.5) and is the time it takes at that particular level 
  e1_test_data$full_rt[i] <- ((e1_test_data$identified_level[i]-1) * 1.5) + e1_test_data$response_time[i]

}

#summarize RTs
rt_sum_e1 = filter(e1_test_data, !(is.na(full_rt))) %>% 
  group_by(participant, experiment) %>% 
  summarise(mean_rt_old = mean(full_rt[which(old == 1)]),
            mean_rt_new = mean(full_rt[which(old == 0)]),
            age_group = age_group[1]) %>%
  mutate(rt_score = mean_rt_new - mean_rt_old) # higher score here means identify new images slower than old ones


#RT for old items 
t.test(mean_rt_old ~ age_group, filter(rt_sum_e1), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_rt_old ~ age_group, filter(rt_sum_e1)), 
           n1 = nrow(rt_sum_e1[which(rt_sum_e1$age_group == 'child'),]), 
           n2 = nrow(rt_sum_e1[which(rt_sum_e1$age_group == 'adult'),]))

#RT for New items
t.test(mean_rt_new ~ age_group, filter(rt_sum_e1), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_rt_new ~ age_group, filter(rt_sum_e1)), 
           n1 = nrow(rt_sum_e1[which(rt_sum_e1$age_group == 'child'),]), 
           n2 = nrow(rt_sum_e1[which(rt_sum_e1$age_group == 'adult'),]))

#RT learning score: New - old 
t.test(rt_score ~ age_group, filter(rt_sum_e1), var.equal = TRUE)
cohen.d.ci(d =cohensD(rt_score ~ age_group, filter(rt_sum_e1)), 
           n1 = nrow(rt_sum_e1[which(rt_sum_e1$age_group == 'child'),]), 
           n2 = nrow(rt_sum_e1[which(rt_sum_e1$age_group == 'adult'),]))














#### EXPERIMENT 2 PRIMARY MAIN ANALYSES ####

#N-back summary dataframe
nback_summary <- filter(nback_data) %>% group_by(participant,age_group) %>%
  summarise(mean_nback_level = mean(nback_level), #mean n-back level across all blocks
            nback_acc = length(which(response_correct == 1))/92, #overall accuracy across all blocks
            block_1_acc =length(which(response_correct == 1 & block_num == 0))/23,#accuracy in each block 1-4
            block_2_acc =length(which(response_correct == 1 & block_num == 1))/23,
            block_3_acc =length(which(response_correct == 1 & block_num == 2))/23,
            block_4_acc =length(which(response_correct == 1 & block_num == 3))/23,
            hit_key = length(which(response ==1)), #how often the key was hit overall
            nback_acc_targets = length(which(response_correct == 1 & is_repeat == 1))/length(which(is_repeat == 1)),
            mean_target_RT = mean(response_time[which(is_repeat == 1 & response == 1)]),
            hit_rate = (length(which(signal_det =='hit'))+0.5)/(length(which(is_repeat == 1))+(1)),
            FA_rate = (length(which(signal_det =='FA'))+0.5)/(length(which(is_repeat == 0))+(1)),
            nback_level_1 = nback_level[which(block_num == 0)][1], #N-back level participant was at in block 1-4
            nback_level_2 = nback_level[which(block_num == 1)][1],
            nback_level_3 = nback_level[which(block_num == 2)][2],
            nback_level_4 = nback_level[which(block_num == 3)][3]) %>%
  mutate(nback_dprime = qnorm(hit_rate)-qnorm(FA_rate))


#summary dataframes of experiment 2
e2_test_summary = e2_only  %>% 
  group_by(participant,experiment) %>% 
  summarise(mean_old = mean(identified_level[which(old == 1)]),
            mean_new = mean(identified_level[which(old == 0)]),
            age = age[1],
            age_group = age_group[1],
            n_correct = length(participant)) %>%
  mutate(learning_sensitivity = mean_new - mean_old)

e2_test_summary_block = e2_only %>% 
  group_by(participant, block_num,experiment) %>% 
  summarise(mean_old = mean(identified_level[which(old == 1)]),
            mean_new = mean(identified_level[which(old == 0)]),
            age = age[1],
            age_group = age_group[1],
            n_correct = length(participant)) %>%
  mutate(learning_sensitivity = mean_new - mean_old)

#merges nback and pictire fragment test data 
e2_master_summary <- merge(nback_summary, e2_test_summary, by = 'participant')

#add n-back variables to dataframe for models that control for n-back performance
e2_only['nback_mean_level'] <- NA
e2_only['nback_dprime'] <- NA
for (i in 1:nrow(e2_only)) {
  curr <- filter(e2_master_summary, participant == e2_only$participant[i])
  e2_only$nback_mean_level[i] <- curr$mean_nback_level[1]
  e2_only$nback_dprime[i] <- curr$nback_dprime[1]
  
}

#mean center/scale n-back dprime for model
e2_only <- e2_only %>% mutate(nback_dprime_c = scale(nback_dprime)) 

#N-back Results (Figure 3A-C)
#mean n-back level children vs. adults
t.test(mean_nback_level ~ age_group.x, filter(e2_master_summary), var.equal = TRUE)
cohen.d.ci(d = cohensD(mean_nback_level ~ age_group.x, filter(e2_master_summary)), 
           n1 = nrow(e2_master_summary[which(e2_master_summary$age_group.x == 'child'),]), 
           n2 = nrow(e2_master_summary[which(e2_master_summary$age_group.x == 'adult'),]))

#n'back performance (dprime) in children vs adults
t.test(nback_dprime ~ age_group.x, filter(e2_master_summary), var.equal = TRUE)
cohen.d.ci(d = cohensD(nback_dprime ~ age_group.x, filter(e2_master_summary)), 
           n1 = nrow(e2_master_summary[which(e2_master_summary$age_group.x == 'child'),]), 
           n2 = nrow(e2_master_summary[which(e2_master_summary$age_group.x == 'adult'),]))

#Nback dprime in block 1-4 children vs adults
nback_sum_by_block_dprime <- filter(nback_data) %>% 
  group_by(participant,age_group,block_num) %>%
  summarise(hit_rate = (length(which(signal_det =='hit'))+0.5)/(length(which(is_repeat == 1))+1),
            FA_rate = (length(which(signal_det =='FA'))+0.5)/(length(which(is_repeat == 0))+1)) %>%
  mutate(nback_dprime = qnorm(hit_rate)-qnorm(FA_rate))

t.test(nback_dprime ~ age_group, filter(nback_sum_by_block_dprime,block_num == 0), var.equal = TRUE)
t.test(nback_dprime ~ age_group, filter(nback_sum_by_block_dprime,block_num == 1), var.equal = TRUE)
t.test(nback_dprime ~ age_group, filter(nback_sum_by_block_dprime,block_num == 2), var.equal = TRUE)
t.test(nback_dprime ~ age_group, filter(nback_sum_by_block_dprime,block_num == 3), var.equal = TRUE)

#correlation between n-back measures and 
only_children <- filter(e2_master_summary, age_group.x == 'child')
only_ad <- filter(e2_master_summary, age_group.x != 'child')

#correlation average n-back level and identification of old images and learning sensitivity
cor.test(only_ad$mean_nback_level, only_ad$mean_old)
cor.test(only_children$mean_nback_level, only_children$mean_old)
cor.test(only_ad$mean_nback_level, only_ad$learning_sensitivity)
cor.test(only_children$mean_nback_level, only_children$learning_sensitivity)

#correlations between nback dprime and identification of old images and learning sensitvitiy
cor.test(only_children$nback_dprime, only_children$mean_old)
cor.test(only_ad$nback_dprime, only_ad$mean_old)
cor.test(only_children$nback_dprime, only_children$learning_sensitivity)
cor.test(only_ad$nback_dprime, only_ad$learning_sensitivity)

#Mixed effects model results

#singularity issue
# m2_lm <- lmer(identified_level~ old_c * age_group *block_c + nback_dprime_c + (old_c + block_c||participant), data=e2_only)
# 
# #Singularity issue still wtih optomizer
# m2_lm <- lmer(identified_level~ old_c * age_group *block_c + nback_dprime_c + (old_c + block_c|participant), data=e2_only,
#               control=lmerControl(optimizer="bobyqa",
#                                   optCtrl=list(maxfun=10000)))
# 
# #Still singularity issue despite double ||
# m2_lm <- lmer(identified_level~ old_c * age_group *block_c + nback_dprime_c + (old_c + block_c||participant), data=e2_only,
#               control=lmerControl(optimizer="bobyqa",
#                                   optCtrl=list(maxfun=100000)))
# 

## Final model as follows (Table 2), all variables centered
m2_lm_c<- lmer(identified_level~ old_c * age_group_c *block_c + nback_dprime_c + (1|participant), data=filter(e2_only))
summary(m2_lm_c)
tab_model(m2_lm_c)
r.squaredGLMM(m2_lm_c)

#estimated marginal means overall (ignoring block)
m2_emms_nb <- emmeans(m2_lm_c, pairwise ~ old_c*age_group_c,infer = T,pbkrtest.limit = 4842)
summary(m2_emms_nb)

#estimated marginal means and contrasts in each block
m2_emms <- emmeans(m2_lm_c, pairwise ~ old_c*age_group_c | block_c,infer = T,pbkrtest.limit = 4842)
summary(m2_emms)

#compare priming effect/slopes for children and adults in each block
m2_priming_slopes <- emtrends(m2_lm_c, specs = pairwise ~ block_c*age_group_c, var="old_c",infer = T,pbkrtest.limit = 4842)
summary(m2_priming_slopes)








#### EXPERIMENT 2 PRIMARY SUPPLEMENT ANALYSES ####
#Also includes stats for Figure 4B-C

#Do children and adults differ in how fast they correctly identify OLD images?
t.test(mean_old ~ age_group, filter(e2_test_summary), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_old ~ age_group, filter(e2_test_summary)), 
           n1 =nrow(e2_test_summary[which(e2_test_summary$age_group == 'child'),]), 
           n2 = nrow(e2_test_summary[which(e2_test_summary$age_group == 'adult'),]))


#Do children and adults differ in how fast they correctly identify NEW images?
t.test(mean_new ~ age_group, filter(e2_test_summary), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_new ~ age_group, filter(e2_test_summary)), 
           n1 =nrow(e2_test_summary[which(e2_test_summary$age_group == 'child'),]), 
           n2 = nrow(e2_test_summary[which(e2_test_summary$age_group == 'adult'),]))


#Do adults show significant learning(above-zero learning sensitivity scores)?
t.test(e2_test_summary$learning_sensitivity[which(e2_test_summary$age_group == 'adult')], mu = 0)
cohen.d.ci(d =cohensD(e2_test_summary$learning_sensitivity[which(e2_test_summary$age_group == 'adult')], mu = 0), 
           n1 =nrow(e2_test_summary[which(e2_test_summary$age_group == 'adult'),]), 
           n2 = nrow(e2_test_summary[which(e2_test_summary$age_group == 'adult'),]))# mu = 0 so n2 is same as n1

#Do children show significant learning (above-zero learning sensitivity scores)?
t.test(e2_test_summary$learning_sensitivity[which(e2_test_summary$age_group == 'child')], mu = 0)
cohen.d.ci(d =cohensD(e2_test_summary$learning_sensitivity[which(e2_test_summary$age_group == 'child')], mu = 0), 
           n1 =nrow(e2_test_summary[which(e2_test_summary$age_group == 'child'),]), 
           n2 = nrow(e2_test_summary[which(e2_test_summary$age_group == 'child'),]))# mu = 0 so n2 is same as n1


#Do children show more priming (learning sensitivity scores) than adults?
t.test(learning_sensitivity ~ age_group, filter(e2_test_summary), var.equal = TRUE)
cohen.d.ci(d =cohensD(learning_sensitivity ~ age_group, filter(e2_test_summary)), 
           n1 =nrow(e2_test_summary[which(e2_test_summary$age_group == 'child'),]), 
           n2 = nrow(e2_test_summary[which(e2_test_summary$age_group == 'adult'),]))

#Interaction anova age group x test block
a2 <- aov(learning_sensitivity ~ as.factor(age_group) * as.factor(block_num), filter(e2_test_summary_block))
summary(a2)
eta_squared(a2, partial = TRUE)

#post hoc tukeys
tukey2 <- TukeyHSD(a2)
tukey2
cohensD_posthoc(e2_test_summary_block)


#RT analyses 
e2_only['full_rt'] <- NA #absolute RT (response_time var is the RT at each level so max is 1.5)

#Calculate max RT
for (i in 1:nrow(e2_only)) {
  #We multiply it by 1.5 because each level is 1.5... subtract 1 off identified_level to make first level clock start at 0
  #And then we also add the response_time because this (is always less then 1.5) and is the time it takes at that particular level 
  e2_only$full_rt[i] <- ((e2_only$identified_level[i]-1) * 1.5) + e2_only$response_time[i]
}

#Summarize RTs
rt_sum_e2 = filter(e2_only, !(is.na(full_rt))) %>% 
  group_by(participant, experiment) %>% 
  summarise(mean_rt_old = mean(full_rt[which(old == 1)]),
            mean_rt_new = mean(full_rt[which(old == 0)]),
            age_group = age_group[1]) %>%
  mutate(rt_score = mean_rt_new - mean_rt_old)

#RT: Old items
t.test(mean_rt_old ~ age_group, filter(rt_sum_e2), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_rt_old ~ age_group, filter(rt_sum_e2)), 
           n1 = nrow(rt_sum_e2[which(rt_sum_e2$age_group == 'child'),]), 
           n2 = nrow(rt_sum_e2[which(rt_sum_e2$age_group == 'adult'),]))

#RT: New items
t.test(mean_rt_new ~ age_group, filter(rt_sum_e2), var.equal = TRUE)
cohen.d.ci(d =cohensD(mean_rt_new ~ age_group, filter(rt_sum_e2)), 
           n1 = nrow(rt_sum_e2[which(rt_sum_e2$age_group == 'child'),]), 
           n2 = nrow(rt_sum_e2[which(rt_sum_e2$age_group == 'adult'),]))

#RT Learning score: New - Old 
t.test(rt_score ~ age_group, filter(rt_sum_e2), var.equal = TRUE)
cohen.d.ci(d =cohensD(rt_score ~ age_group, filter(rt_sum_e2)), 
           n1 = nrow(rt_sum_e2[which(rt_sum_e2$age_group == 'child'),]), 
           n2 = nrow(rt_sum_e2[which(rt_sum_e2$age_group == 'adult'),]))


#### ACROSS EXPERIMENT PRIMARY MAIN ANALYSES #####

# #Singularity issue still with optimizer and fails to converge
# across_exp_lm  <- lmer(identified_level~ old_c * age_group *block_c * experiment+ (old_c + block_c|participant), data = correct_prime_trials,
#    control=lmerControl(optimizer="bobyqa",
#  optCtrl=list(maxfun=10000)))


# #Double || still has singularity issue
# across_exp_lm  <- lmer(identified_level~ old_c * age_group *block_c * experiment+ (old_c + block_c||participant), data = correct_prime_trials,
#                        control=lmerControl(optimizer="bobyqa",
#                                            optCtrl=list(maxfun=10000)))

#model used in paper (Table 3)
across_exp_lm_c <- lmer(identified_level~ old_c * age_group_c *block_c * experiment_c + (1|participant), corr_test_trials)
summary(across_exp_lm_c)
tab_model(across_exp_lm_c)

#basic emmeans contrasts
across_exp_emms <- emmeans(across_exp_lm_c, pairwise ~ old_c*age_group_c*experiment_c | block_c,infer = T,pbkrtest.limit = 8242)
summary(across_exp_emms)

#collapses across block for overall learning sensitivity slopes
across_exp_learning_slopes_no_block <- emtrends(across_exp_lm_c, specs = pairwise ~ age_group_c*experiment_c, var="old_c",infer = T,pbkrtest.limit = 8242)
summary(across_exp_learning_slopes_no_block)

#in each block  learning senstivity slopes   
across_exp_learning_slopes <- emtrends(across_exp_lm_c, specs = pairwise ~ block_c*age_group_c*experiment_c, var="old_c",infer = T,pbkrtest.limit = 8242)
summary(across_exp_learning_slopes)



#### ACROSS EXPERIMENT PRIMARY SUPPLEMENTARY ANALYSES #####
#Also includes stats for Figure 5 A-C

#Summary dataframes
both_exp_test_summary = corr_test_trials  %>% 
  group_by(participant,experiment) %>% 
  summarise(mean_old = mean(identified_level[which(old == 1)]),
            mean_new = mean(identified_level[which(old == 0)]),
            age = age[1],
            age_group = age_group[1],
            n_correct = length(participant)) %>%
  mutate(learning_sensitivity = mean_new - mean_old)

#by block
both_exp_test_summary_block = corr_test_trials  %>% 
  group_by(participant,experiment,block_num) %>% 
  summarise(mean_old = mean(identified_level[which(old == 1)]),
            mean_new = mean(identified_level[which(old == 0)]),
            age = age[1],
            age_group = age_group[1],
            n_correct = length(participant)) %>%
  mutate(learning_sensitivity = mean_new - mean_old)


#Is adult learning different across experiments?
t.test(learning_sensitivity ~ experiment, filter(both_exp_test_summary, age_group == 'adult'), var.equal = TRUE)

#Is child learning different across experimnets?
t.test(learning_sensitivity ~ experiment, filter(both_exp_test_summary, age_group == 'child'), var.equal = TRUE)

#Anova with age and experiment (but not block)
a_nb <- aov(learning_sensitivity ~ as.factor(experiment) * as.factor(age_group),
            both_exp_test_summary) # there wa san error here before where it was using the block dataframe but i fixed it as of may 17 2023
summary(a_nb)
effectsize(a_nb, partial = TRUE)

#post hoc tukeys
TukeyHSD(a_nb)
cohensD_posthoc_across(both_exp_test_summary)

#3-way anova with block and experiment and age
a3 <- aov(learning_sensitivity ~ as.factor(experiment) * as.factor(block_num) * as.factor(age_group), both_exp_test_summary_block)
summary(a3)
effectsize(a3, partial = TRUE)

#post hoc tukeys
TukeyHSD(a3)
cohensD_posthoc_across_three(both_exp_test_summary_block)



