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
###[Table S14] Study 1 Abstention Analyses without Normative Ratings#####################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model77 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model77)

model78 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model78)

model79 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model79)

model80 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model80)

model81 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model81)

model82 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model82)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S15] Study 1 Opposition Analyses without Normative Ratings#####################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model83 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model83)

model84 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model84)

model85 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model85)

model86 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model86)

model87 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model87)

model88 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + income + education + political_knowledge + age + female, cluster= "subj_id")
summary(model88)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S16] Study 2 Abstention Analyses without Normative Ratings and Subset of Demographic Variables#################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model89 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model89)

model90 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model90)

model91 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model91)

model92 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model92)

model93 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model93)

model94 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model94)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S17] Study 2 Opposition Analyses without Normative Ratings and Subset of Demographic Variables#################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model95 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model95)

model96 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model96)

model97 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model97)

model98 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model98)

model99 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model99)

model100 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + political_knowledge + female, cluster= "subj_id")
summary(model100)