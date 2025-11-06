library(readxl)
library(mice)
library(miceadds)
library(multiwayvcov)
library(estimatr)
options(scipen = 3)

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
###[Table S22] Study 1 Alternative Opposition Analyses with Preregistered Covariates#####################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model125 <- lm.cluster(data=study1_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model125)

model126 <- lm.cluster(data=study1_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model126)

model127 <- lm.cluster(data=study1_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model127)

model128 <- lm.cluster(data=study1_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model128)

model129 <- lm.cluster(data=study1_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model129)

model130 <- lm.cluster(data=study1_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model130)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S23] Study 2 Alternative Opposition Analyses with Preregistered Covariates#####################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model131 <- lm.cluster(data=study2_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model131)

model132 <- lm.cluster(data=study2_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model132)

model133 <- lm.cluster(data=study2_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model133)

model134 <- lm.cluster(data=study2_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model134)

model135 <- lm.cluster(data=study2_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model135)

model136 <- lm.cluster(data=study2_valid_trials, formula = no_vote_perc_over_all_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model136)