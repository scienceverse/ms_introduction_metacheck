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
###[Table S9] Study 1 Abstention Analyses with Preregistered and Linguistic Covariates###################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model49 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition  + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model49)

model50 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix +  subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model50)

model51 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition  + subtlex_median_body + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model51) 

model52 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model52)

model53 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_fix +  subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model53)

model54 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model54)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S10] Study 1 Opposition Analyses with Preregistered and Linguistic Covariates##################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model55 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration + subtlex_median_body + flesch_reading_grade_body + eds_total  + ballot_info_addition  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest , cluster= "subj_id")
summary(model55)

model56 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix  +  subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition +  modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model56)

model57 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration  + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model57)

model58 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression +  subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model58)

model59 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model59)

model60 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + subtlex_median_body + flesch_reading_grade_body + eds_total + ballot_info_addition  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model60)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S11] Study 2 Abstention Analyses with Preregistered and Linguistic Covariates and Additional Covariates########################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model61 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration +  subtlex_median_body + flesch_reading_grade_body + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model61)

model62 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix +  subtlex_median_body + flesch_reading_grade_body + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model62)

model63 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration  +  subtlex_median_body + flesch_reading_grade_body + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money  + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model63)

model64 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_regression + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money  + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model64)

model65 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_fix + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model65)

model66 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ body_average_duration + subtlex_median_body + flesch_reading_grade_body + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model66)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S12] Study 2 Opposition Analyses with Preregistered and Linguistic Covariates##################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################

model67 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_fix_duration  + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see  + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest , cluster= "subj_id")
summary(model67)

model68 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_fix  + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model68)

model69 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_first_pass_duration  + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model69)

model70 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_regression + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model70)

model71 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_fix + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model71)

model72 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ body_average_duration + subtlex_median_body + flesch_reading_grade_body + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model72)


#########################################################################################################################################################################################################################################################################################################################################################################################################################################
###[Table S13] Total Reading Times#######################################################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################


model73 <- lm.cluster(data=study1_valid_trials, formula = ln_perc_abstensions ~ Advance_RT  + eds_total + ballot_info_addition  + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model73)

model74 <- lm.cluster(data=study1_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ Advance_RT + eds_total + ballot_info_addition  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model74)

model75 <- lm.cluster(data=study2_valid_trials, formula = ln_perc_abstensions ~ Advance_RT + eds_total  + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed + modified_body_word_count + position_within_other_proposals + clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model75)

model76 <- lm.cluster(data=study2_valid_trials_only_votes, formula = no_vote_perc_over_sub_votes ~ Advance_RT + eds_total + voters_likely_did_not_see + ballot_info_addition + ballot_money + language_changed  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
summary(model76)