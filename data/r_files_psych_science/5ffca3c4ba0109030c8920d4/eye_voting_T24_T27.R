library(readxl)
library(mice)
library(miceadds)
library(multiwayvcov)
library(estimatr)
options(scipen = 3)
#options(scipen=999)

study1 <- read_excel('Study1_07_15_20.xlsx')
study1 <-as.data.frame(study1)

study2 <- read_excel('Study2_07_15_20.xlsx')
study2 <-as.data.frame(study2)

###Subsets data to eye movement-related trials that satisfy inclusion criteria###

study1_valid_trials <- study1[study1$include_trial == '1',]
study2_valid_trials <- study2[study2$include_trial == '1',]

###Further subsets study 1 and study 2 to only trials in which lab participants rendered a directional vote###
study1_valid_trials_only_votes <- study1_valid_trials[study1_valid_trials$abstain == '0',]
study2_valid_trials_only_votes <- study2_valid_trials[study2_valid_trials$abstain == '0',]


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S24] Study 1 Abstention Bivariate Analyses#####################################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model137 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration, cluster= "subj_id")
summary(model137)

model138 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix, cluster= "subj_id")
summary(model138)

model139 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration, cluster= "subj_id")
summary(model139) 

model140 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_regression, cluster= "subj_id")
summary(model140)

model141 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_fix, cluster= "subj_id")
summary(model141)

model142 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_duration, cluster= "subj_id")
summary(model142)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S25] Study 1 Opposition Bivariate Analyses#####################################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model143 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration, cluster= "subj_id")
summary(model143)

model144 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix, cluster= "subj_id")
summary(model144)

model145 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration, cluster= "subj_id")
summary(model145)

model146 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression, cluster= "subj_id")
summary(model146)

model147 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix, cluster= "subj_id")
summary(model147)

model148 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration, cluster= "subj_id")
summary(model148)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S26] Study 2 Abstention Bivariate Analyses#####################################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model149 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration , cluster= "subj_id")
summary(model149)

model150 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix, cluster= "subj_id")
summary(model150)

model151 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration, cluster= "subj_id")
summary(model151)

model152 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_regression, cluster= "subj_id")
summary(model152)

model153 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_fix , cluster= "subj_id")
summary(model153)

model154 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_duration , cluster= "subj_id")
summary(model154)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S27] Study 2 Opposition Bivariate Analyses#####################################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model155 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration , cluster= "subj_id")
summary(model155)

model156 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix , cluster= "subj_id")
summary(model156)

model157 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration  , cluster= "subj_id")
summary(model157)

model158 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression , cluster= "subj_id")
summary(model158)

model159 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix, cluster= "subj_id")
summary(model159)

model160 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration , cluster= "subj_id")
summary(model160)