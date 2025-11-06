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
###[Table S18] Study 1 Opposition Analyses with In-Lab Voting############################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model101 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + oppose, cluster= "subj_id")
summary(model101)

model102 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + oppose, cluster= "subj_id")
summary(model102)

model103 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + oppose, cluster= "subj_id")
summary(model103)

model104 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + oppose, cluster= "subj_id")
summary(model104)

model105 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + oppose, cluster= "subj_id")
summary(model105)

model106 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + oppose, cluster= "subj_id")
summary(model106)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S19] Study 2 Opposition Analyses with In-Lab Voting############################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model107 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + oppose, cluster= "subj_id")
summary(model107)

model108 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + oppose, cluster= "subj_id")
summary(model108)

model109 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + oppose, cluster= "subj_id")
summary(model109)

model110 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + oppose, cluster= "subj_id")
summary(model110)

model111 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + oppose, cluster= "subj_id")
summary(model111)

model112 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + oppose, cluster= "subj_id")
summary(model112)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S20] Study 1 Abstention Analyses with In-Lab Voting############################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model113 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + abstain, cluster= "subj_id")
summary(model113)

model114 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + abstain, cluster= "subj_id")
summary(model114)

model115 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + abstain, cluster= "subj_id")
summary(model115)

model116 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + abstain, cluster= "subj_id")
summary(model116)

model117 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + abstain, cluster= "subj_id")
summary(model117)

model118 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + ballot_info_addition + abstain, cluster= "subj_id")
summary(model118)

#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S21] Study 2 Abstention Analyses with In-Lab Voting############################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model119 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + abstain, cluster= "subj_id")
summary(model119)

model120 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + abstain, cluster= "subj_id")
summary(model120)

model121 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + abstain, cluster= "subj_id")
summary(model121)

model122 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + abstain, cluster= "subj_id")
summary(model122)

model123 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + abstain, cluster= "subj_id")
summary(model123)

model124 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience + norming_importance + norming_interest + income + education + political_knowledge + age + female + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + abstain, cluster= "subj_id")
summary(model124)