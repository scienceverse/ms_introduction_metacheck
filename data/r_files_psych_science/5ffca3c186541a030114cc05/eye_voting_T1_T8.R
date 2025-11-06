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
###[Table S1] Study 1 Abstention Analyses with Preregistered Covariates##################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model1 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model1)

model2 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model2)

model3 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration  + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model3) 

model4 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model4)

model5 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model5)

model6 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model6)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S2] Study 1 Opposition Analyses with Preregistered Covariates##################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model7 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration  +  modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model7)

model8 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model8)

model9 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model9)

model10 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model10)

model11 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model11)

model12 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model12)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S3] Study 1 Abstention Analyses with Preregistered Covariates and Additional Covariates########################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model13 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total + ballot_info_addition, cluster= "subj_id")
summary(model13)

model14 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total + ballot_info_addition, cluster= "subj_id")
summary(model14)

model15 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total + ballot_info_addition, cluster= "subj_id")
summary(model15) 

model16 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total + ballot_info_addition, cluster= "subj_id")
summary(model16)

model17 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total + ballot_info_addition, cluster= "subj_id")
summary(model17)

model18 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total + ballot_info_addition, cluster= "subj_id")
summary(model18)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S4] Study 1 Opposition Analyses with Preregistered Covariates and Additional Covariates########################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model19 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + ballot_info_addition, cluster= "subj_id")
summary(model19)

model20 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + ballot_info_addition, cluster= "subj_id")
summary(model20)

model21 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + ballot_info_addition, cluster= "subj_id")
summary(model21)

model22 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + ballot_info_addition, cluster= "subj_id")
summary(model22)

model23 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + ballot_info_addition, cluster= "subj_id")
summary(model23)

model24 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + ballot_info_addition, cluster= "subj_id")
summary(model24)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S5] Study 2 Abstention Analyses with Preregistered Covariates##################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model25 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model25)

model26 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model26)

model27 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model27)

model28 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model28)

model29 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model29)

model30 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model30)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S6] Study 2 Opposition Analyses with Preregistered Covariates##################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model31 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model31)

model32 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model32)

model33 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration   + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model33)

model34 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model34)

model35 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model35)

model36 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female, cluster= "subj_id")
summary(model36)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S7] Study 2 Abstention Analyses with Preregistered Covariates and Additional Covariates########################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model37 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model37)

model38 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model38)

model39 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration  + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model39)

model40 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model40)

model41 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model41)

model42 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + norming_salience  + norming_importance  + norming_interest + income  + education  + political_knowledge  + age  + female + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model42)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S8] Study 2 Opposition Analyses with Preregistered Covariates and Additional Covariates########################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model43 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education + political_knowledge  + age  + female + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model43)

model44 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education + political_knowledge  + age  + female + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model44)

model45 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education + political_knowledge  + age  + female + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model45)

model46 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education + political_knowledge  + age  + female + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model46)

model47 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education + political_knowledge  + age  + female + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model47)

model48 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body + norming_salience  + norming_importance  + norming_interest + income  + education + political_knowledge  + age  + female + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed, cluster= "subj_id")
summary(model48)