
#
# Study 1c
#


############################################################################
#
# IMPORT PACKAGES
#
#############################################################################

library(groundhog)

groundhog_day = '2020-01-01'
groundhog.library('tidyverse', groundhog_day) #data manipulation
groundhog.library('testit', groundhog_day) #unit tests
groundhog.library('lme4', groundhog_day) #mixed effects models
groundhog.library('lmerTest', groundhog_day) #more test stats for mixed effects models
groundhog.library('mediation', groundhog_day) #mediation
groundhog.library('ggplot2', groundhog_day) #graphing

############################################################################
#
# DATA CLEANING
#
#############################################################################

#Import data
raw <- read.csv("C:/Users/Admin/Dropbox/Drive Sync/Berkeley PhD/Research Projects/02 -- Ongoing Research Projects/Fancy Regret (w ERK SB)/2019-10-28 -- Outcomes influenced elicitation and WTP/data/full data.csv", comment.char="#", stringsAsFactors = F)
temp = raw[5:nrow(raw),] #remove previews and qualtrics headers


#Create a lookup table from embedded data in the survey
#one column is the number of the wtp column, 
#the other is starting probability 
lookup_vals = temp[1,407:446]
lookup_vals = unlist((lookup_vals))
lookup_names = names(raw[1,407:446])
lookup_table = data.frame("question_no" = lookup_names, "start_prob" =  lookup_vals)
names(lookup_table) = c("question_no", "start_prob")
lookup_table = lookup_table %>% filter(grepl("start", question_no, fixed=T))
lookup_table$question_no = as.numeric(gsub("[^0-9.-]", "", lookup_table$question_no))
lookup_table$start_prob = as.numeric(as.character(lookup_table$start_prob))/100
assert("Lookup table is wrong # of rows", nrow(lookup_table) == 20) #checks that the above code works


#Add an ID variable
temp$id = 1:nrow(temp)

#Rename the catch trials so that we can do an easier 
#selection by column name later on
temp$exclude30to30Cond1_w = temp$WTP30to30_1
temp$exclude60to60Cond1_w = temp$wtp60to60_1
#we did not preregister any exclusions based on answers
#to the 0 change regret questions
temp$exclude30to30Cond1_r = temp$r30to30_1
temp$exclude60to60Cond1_r = temp$r60to60_1

temp$exclude60to60Cond2_w = temp$wtp60to60_1.1
temp$exclude30to30Cond2_w = temp$r30to30_1.1 # this was mislabeled in qualtrics as a regret question -- it's a wtp question, those were the only type asked in this condition

#Drop the old exclusions columns
exclude_col_drops = c("WTP30to30_1","wtp60to60_1","r30to30_1","r60to60_1","wtp60to60_1.1","r30to30_1.1")
temp = temp[,! names(temp) %in% exclude_col_drops]
assert("Exclusions columns not renamed", ! "WTP30to30_1" %in% names(temp)) #checks that the above code works


#create a new condition column
temp$condition = ifelse(temp$conditionOutcomesAndWTP1WTPOnly2=="1", "regret_elicitation", "wtp_only")
condition_regret_elicitation_n = table(temp$conditionOutcomesAndWTP1WTPOnly2)[1]
condition_wtp_only_n = table(temp$conditionOutcomesAndWTP1WTPOnly2)[2]

assert("count/condition is wrong", condition_regret_elicitation_n == table(temp$condition)[1]) #make sure we still haev the same # of people in each condition
assert("count/condition is wrong", condition_wtp_only_n == table(temp$condition)[2])
temp = temp[,! names(temp) %in% c("conditionOutcomesAndWTP1WTPOnly2")]




#Temporarily split the data to clean each separately
cond1_temp = temp[temp$condition=="regret_elicitation",]
cond2_temp = temp[temp$condition=="wtp_only",]

#
# Condition 1 cleaning
#

#Go wide to long
cond1_temp_long_r = cond1_temp %>% 
  pivot_longer(cols=ends_with('r_1'), names_to="regret_question", names_prefix="X", values_to="regret")
keeps = c("id", "regret_question", "regret")
cond1_temp_long_r = cond1_temp_long_r[, names(cond1_temp_long_r) %in% keeps]

cond1_temp_long_wtp = cond1_temp %>% 
  pivot_longer(cols=(contains('wtp')), names_to="wtp_question", names_prefix="X", values_to="wtp")
#question 18 is named X19wtp_1
#question 19 is named 19wtp_1.1
keeps = c("condition", "genderQ", "ageQ", "wtp_question", "wtp", "exclude30to30Cond1_w","exclude60to60Cond1_w", "id")
cond1_temp_long_wtp = cond1_temp_long_wtp[, names(cond1_temp_long_wtp) %in% keeps]

#check that the row counts for both long datasets match
assert("Mismatch in row count between dataframes", nrow(cond1_temp_long_r) == nrow(cond1_temp_long_wtp))


cond1_temp_long = cond1_temp_long_wtp
cond1_temp_long$regret_question = cond1_temp_long_r$regret_question
cond1_temp_long$regret = cond1_temp_long_r$regret


cond1_temp_long$question_no = gsub("[^0-9.-]", "", cond1_temp_long$regret_question)
cond1_temp_long$question_no = substr(cond1_temp_long$question_no, 1, nchar(cond1_temp_long$question_no) -1)
cond1_temp_long$question_no = as.numeric(cond1_temp_long$question_no)

#because not touching the slider would leave it at 0, if people did not respond to the no change questions it is 
#likely because they left it at 0, so they are not included in this exclusion
cond1_temp_long$wtp_exclusion_either = ifelse(cond1_temp_long$exclude30to30Cond1_w>.50 | cond1_temp_long$exclude60to60Cond1_w>.50, 1, 0)
cond1_temp_long$wtp_exclusion_both = ifelse(cond1_temp_long$exclude30to30Cond1_w>.50 & cond1_temp_long$exclude60to60Cond1_w>.50, 1, 0)

#
# Condition 2 cleaning
#

#Now, make the second condition's long dataset 
cond2_temp_long = cond2_temp %>% 
  pivot_longer(cols=starts_with('X'), names_to="wtp_question", names_prefix="X", values_to="wtp")
keeps = c("genderQ", "ageQ", "wtp_question", "wtp", "exclude30to30Cond2_w", "exclude60to60Cond2_w", "id", "condition")
cond2_temp_long = cond2_temp_long[, names(cond2_temp_long) %in% keeps]

#Remove all the empty rows in this condition
cond2_temp_long = cond2_temp_long[cond2_temp_long$wtp>=0,]
assert("Incorrect row deletion", length(unique(cond2_temp_long$id))*20 == 2760)

cond2_temp_long$question_no = gsub("[^0-9.-]", "", cond2_temp_long$wtp_question)
cond2_temp_long$question_no = substr(cond2_temp_long$question_no, 1, nchar(cond2_temp_long$question_no) -1)
cond2_temp_long$question_no = as.numeric(cond2_temp_long$question_no)

cond2_temp_long$wtp_exclusion_either = ifelse(cond2_temp_long$exclude30to30Cond2_w>.50 | cond2_temp_long$exclude60to60Cond2_w>.50, 1, 0)
cond2_temp_long$wtp_exclusion_both = ifelse(cond2_temp_long$exclude30to30Cond2_w>.50 & cond2_temp_long$exclude60to60Cond2_w>.50, 1, 0)

#create an NA column for regret so we can rbind later
cond2_temp_long$regret = NA

#
# Merge the two long dataframes row-wise
#

keep_cols_final = c("genderQ", "ageQ", "id", "wtp", "question_no", "regret", "wtp_exclusion_either", "wtp_exclusion_both", "condition")

cond1_temp_long = cond1_temp_long[, names(cond1_temp_long) %in% keep_cols_final]
cond2_temp_long = cond2_temp_long[, names(cond2_temp_long) %in% keep_cols_final]

#Rbind into a final dataframe
long = rbind(cond1_temp_long, cond2_temp_long)
assert("Long dataset is the wrong number of rows", nrow(long)==8480)

long = merge(long, lookup_table, by = "question_no")

long$wtp = as.numeric(long$wtp)
long$regret = as.numeric(long$regret)
long$ageQ = as.numeric(long$ageQ)

long %>% filter(long$condition == "regret_elicitation") %>% group_by(long$id[long$condition == "regret_elicitation"]) %>% summarize(meanR = mean(wtp, na.rm=T))

#Exclude anyone who answered 0 to all of the regret questions
regret_sums_by_id = aggregate(regret ~ id, data=long[long$condition=="regret_elicitation",], mean)
no_ragrets = regret_sums_by_id[regret_sums_by_id$regret==0,]
regret_exclusion_ids = no_ragrets$id
long$regret_exclusion = ifelse(long$id %in% regret_exclusion_ids,1,0)

#Check that column values all make sense
assert("WTP has values above possible range", max(long$wtp, na.rm=T)<3.1)
assert("Regret has values above possible range",max(long$regret, na.rm=T)<101)
assert("Starting probability has values above possible range", max(long$start_prob, na.rm=T)<1)

#Make an ending probability column
long$end_prob = long$start_prob + .1

#Add columns required for fitting models
#Add a percent reduction column
long$perc_reduction = 1 - (1.000000000001-long$end_prob)/(1.00000000000001-long$start_prob)
long$perc_reduction = round(long$perc_reduction, 5)
assert("Percent reduction has values above possible range", max(long$perc_reduction, na.rm=T)<=1)

long$change = long$end_prob - long$start_prob

#Add a final exclusion indicator
#Exclude if they answer above $0.50 to either 0% change question
#Or if they answered 0 for their regret in all trials
long$exclude = ifelse(long$wtp_exclusion_either==1 | long$regret_exclusion==1, 1, 0)

clean = long

#remove unneeded dataframes
rm(cond2_temp, raw, cond2_temp_long, cond1_temp, cond1_temp_long, cond1_temp_long_r, cond1_temp_long_wtp, temp, no_ragrets, regret_sums_by_id, lookup_table, long)

N = length(unique(clean$id[clean$exclude==0]))
N_nox = length(unique(clean$id))

N_cond1_nox = length(unique(clean$id[clean$condition=="regret_elicitation"]))
N_cond1 = length(unique(clean$id[clean$condition=="regret_elicitation" & clean$exclude==0]))

N_cond2_nox = length(unique(clean$id[clean$condition=="wtp_only"]))
N_cond2 = length(unique(clean$id[clean$condition=="wtp_only"& clean$exclude==0]))




#Get demographics
N
N_nox

mean(as.numeric(as.character(clean$ageQ[clean$exclude==0])), na.rm=T)
sd(as.numeric(as.character(clean$ageQ[clean$exclude==0])), na.rm=T)

table(clean_by_id$genderQ[clean_by_id$exclude==0])


clean_by_id = clean %>% distinct(id, .keep_all= TRUE)



############################################################################
#
# ANALYSIS
#
############################################################################



#We can't have subject level random effects because the intervention was
#fully between subjects
#Condition effects, exclusions
cr_full_lm = lm(wtp ~ perc_reduction*condition, data=clean[clean$exclude==0,])
summary(cr_full_lm)

#Condition effects, no exclusions
cr_full_lm_nox = lm(wtp ~ perc_reduction*condition, data=clean)
summary(cr_full_lm_nox)



#
# WTP Analysis
#

#Conditional regret model fits WTP significantly better than both other models in 
# both conditions

#Regret elicitation condition, exclusions
null_cond1_lmer = lmer(wtp ~ 1 + (1 | id), data=clean[clean$exclude==0 & clean$condition=="regret_elicitation",], REML = FALSE)
ob_cond1_lmer = lmer(wtp ~ end_prob + (1  | id), data=clean[clean$exclude==0 & clean$condition=="regret_elicitation",], REML=FALSE)
cr_cond1_lmer = lmer(wtp ~ perc_reduction + (1  | id), data=clean[clean$exclude==0 & clean$condition=="regret_elicitation",], REML=FALSE)

anova(null_cond1_lmer, ob_cond1_lmer, cr_cond1_lmer)


#Regret elicitation condition, no exclusions
null_cond1_lmer_nox = lmer(wtp ~ change + (1 | id), data=clean[clean$condition=="regret_elicitation",], REML = FALSE)
ob_cond1_lmer_nox = lmer(wtp ~ end_prob + (1  | id), data=clean[clean$condition=="regret_elicitation",], REML=FALSE)
cr_cond1_lmer_nox = lmer(wtp ~ perc_reduction + (1  | id), data=clean[clean$condition=="regret_elicitation",], REML=FALSE)

anova(null_cond1_lmer_nox, ob_cond1_lmer_nox, cr_cond1_lmer_nox)


#WTP only control condition, exclusions
null_cond2_lmer = lmer(wtp ~ change + (1 | id), data=clean[clean$exclude==0 & clean$condition=="wtp_only",], REML = FALSE)
ob_cond2_lmer = lmer(wtp ~ end_prob + (1  | id), data=clean[clean$exclude==0 & clean$condition=="wtp_only",], REML=FALSE)
cr_cond2_lmer = lmer(wtp ~ perc_reduction + (1  | id), data=clean[clean$exclude==0 & clean$condition=="wtp_only",], REML=FALSE)

anova(null_cond2_lmer, ob_cond2_lmer, cr_cond2_lmer)


#WTP only control condition, no exclusions
null_cond2_lmer_nox = lmer(wtp ~ change + (1 | id), data=clean[clean$condition=="wtp_only",], REML = FALSE)
ob_cond2_lmer_nox = lmer(wtp ~ end_prob + (1  | id), data=clean[clean$condition=="wtp_only",], REML=FALSE)
cr_cond2_lmer_nox = lmer(wtp ~ perc_reduction + (1  | id), data=clean[clean$condition=="wtp_only",], REML=FALSE)

anova(null_cond2_lmer_nox, ob_cond2_lmer_nox, cr_cond2_lmer_nox)




#
# Regret analysis
#

# Conditional regret fits regret significantly better than both other models

#Regret elicitation condition, exclusions
null_cond2_lmer = lmer(regret ~ 1 + (1 | id), data=clean[clean$exclude==0 & clean$condition=="regret_elicitation",], REML = FALSE)
ob_cond2_lmer = lmer(regret ~ end_prob + (1  | id), data=clean[clean$exclude==0 & clean$condition=="regret_elicitation",], REML=FALSE)
cr_cond2_lmer = lmer(regret ~ perc_reduction + (1  | id), data=clean[clean$exclude==0 & clean$condition=="regret_elicitation",], REML=FALSE)

anova(null_cond2_lmer, ob_cond2_lmer, cr_cond2_lmer)


#Regret elicitation condition, no exclusions
null_cond2_lmer_nox = lmer(regret ~ change + (1 | id), data=clean[clean$condition=="regret_elicitation",], REML = FALSE)
ob_cond2_lmer_nox = lmer(regret ~ end_prob + (1  | id), data=clean[clean$condition=="regret_elicitation",], REML=FALSE)
cr_cond2_lmer_nox = lmer(regret ~ perc_reduction + (1  | id), data=clean[clean$condition=="regret_elicitation",], REML=FALSE)

anova(null_cond2_lmer_nox, ob_cond2_lmer_nox, cr_cond2_lmer_nox)






#
# Mediation of WTP by % of Outcomes regretted
#

clean2 = clean[complete.cases(clean),]

fitM <- lm(regret ~ perc_reduction, data=clean2[clean$condition=="regret_elicitation",])
fitY <- lm(wtp ~ perc_reduction + regret, data=clean2[clean$condition=="regret_elicitation",])

summary(fitM)
summary(fitY)

#simple
fitMed <-mediation::mediate(fitM, fitY, treat="perc_reduction", mediator="regret")

summary(fitMed)
plot(fitMed)


#with bootstrap
fitMedBoot = mediation::mediate(fitM, fitY, boot=TRUE, sims=1000, treat='perc_reduction', mediator='regret')
summary(fitMedBoot)
plot(fitMedBoot)




############################################################################
#
# PLOTTING AND FIGURES
#
############################################################################

mean_wtp = clean[clean$exclude==0,] %>%
  dplyr::group_by(start_prob) %>%
  dplyr::summarize(mean_wtp = mean(wtp, na.rm=T), se_wtp = sd(wtp, na.rm=T)/sqrt(length(unique(clean$id))))

#Figure 6S
ggplot(data = mean_wtp, aes(x = start_prob, y = mean_wtp)) + geom_line() +
  geom_point() + ggtitle("Willingness to pay by starting probability") +
  xlab("Starting Probability") + ylab("WTP ($)")  + scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  geom_errorbar(aes(ymin = mean_wtp - se_wtp, ymax = mean_wtp + se_wtp))




mean_wtp_by_condition = clean[clean$exclude==0,] %>%
  dplyr::group_by(start_prob, condition) %>%
  dplyr::summarize(mean_wtp = mean(wtp, na.rm=T), se_wtp = sd(wtp, na.rm=T)/sqrt(length(unique(clean$id))))


ggplot(data = mean_wtp_by_condition, aes(x = start_prob, y = mean_wtp, group=condition)) + geom_line() +
  geom_point() + ggtitle("Willingness to pay for 10% increase in chances to win by starting probability and condition") +
  xlab("Starting Probability") + ylab("WTP ($)")  + scale_x_continuous(labels = scales::percent, limits = c(0,1)) +ylim(0,1.5) +
  geom_errorbar(aes(ymin = mean_wtp - se_wtp, ymax = mean_wtp + se_wtp))






