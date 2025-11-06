#
#
# Study 1b
#
#


##########################
#
# IMPORT PACKAGES
#
##########################
library(groundhog)
groundhog_day = "2020-01-01"


groundhog.library(reshape2, groundhog_day) 
groundhog.library(lme4, groundhog_day) 
groundhog.library(lmerTest, groundhog_day)
groundhog.library(stringr, groundhog_day) 
groundhog.library(ggplot2, groundhog_day) 
groundhog.library(psycho, groundhog_day) 
groundhog.library(dplyr, groundhog_day)



##########################
#
# CLEAN DATA
#
##########################

path_to_csv = "INSERT FILE PATH HERE"
raw <- read.csv(path_to_csv, comment.char="#") #imports 137 responses


column_names = colnames(raw)

#
# Data cleaning
#

#Expects questions to have the following formats:
#wtp questions
#X30to35_1

#unknown % wtp questions:
#X5percent_1

#Regret questions
#X1to6_1

#CRT questions
#CRT_XX

#willingness to pay questions
wtp_columnsRaw = column_names[grepl('_1$', column_names)]
wtp_columns = wtp_columnsRaw[1:43]
#regret questions
regret_columns = column_names[grepl('regret', column_names)]
#crt questions
crt_columns = c("CRT_mary", "CRT_flock", "CRT_bear", "CRT_jerry")

#demographic questions
demographic_columns = c("genderQ", "ageQ", "income", "whatSurveyAsking", "whatWasWeird")
#attention/english checks
attn_columns = c("englishCheckClock", "englishCheckBall", "effortQuestion", "attentionCheck")

#clean each dataframe then column bind them together
#WTP
wtp = raw[,wtp_columns]
wtp = wtp[3:nrow(wtp),]
wtp = sapply(wtp, function(x) as.numeric(as.character(x)))


#Regret
regret = raw[,regret_columns]
regret = regret[3:nrow(regret),]
regret = sapply(regret, function(x) as.numeric(as.character(gsub("[^0-9.]", "", x)))) #strip out non numerics

#CRT
crt = raw[,crt_columns]
crt = crt[3:nrow(crt),]
crt = sapply(crt, function(x) as.character(x))

#Demographics
demo = raw[,demographic_columns]
demo = demo[3:nrow(demo),]
demo$genderQ = demo$genderQ
demo$ageQ = as.numeric(as.character(demo$ageQ))
demo$income = as.character(demo$income)
demo$whatSurveyAsking = as.character(demo$whatSurveyAsking)
demo$whatWasWeird = as.character(demo$whatWasWeird)


#Attention check
attn = raw[attn_columns,]
attn = attn[3:nrow(attn),]
attn = sapply(attn, function(x) as.character(x))

#Bind all demo columns together post-cleaning 
demo_wide = cbind(crt, demo) #errors in attention check, so not bound
demo_wide$id = 1:nrow(demo_wide)

#Bind all repeated columns together post-cleaning
wide = cbind(wtp, regret) #errors in attention check, so not bound
wide = as.data.frame(wide)
wide$id = 1:nrow(wide)
wtp_long = melt(wide, id.vars=c('id'))

colnames(wtp_long) = c('id', 'var', 'response')
wtp_long$id = as.numeric(as.character(wtp_long$id))
wtp_long$var = as.character(wtp_long$var)
wtp_long$response = as.numeric(as.character(wtp_long$response))

#create question type column
wtp_long$type = "wtp"
wtp_long$type = ifelse(grepl('regret', wtp_long$var), "regret", wtp_long$type)

#create initial and final values
wtp_long$initial = rep(99, nrow(wtp_long)) 
wtp_long$final =  rep(99, nrow(wtp_long))

#Create for WTP questions
wtp_long$var[wtp_long$type=='wtp'] = sapply(wtp_long$var[wtp_long$type=='wtp'], function(x) str_sub(x,2,-3)) #numbers only
wtp_long$initial[wtp_long$type=='wtp'] = sapply(wtp_long$var[wtp_long$type=='wtp'], function(x) strsplit(x, "[to]")[[1]][1])
wtp_long$final[wtp_long$type=='wtp'] = sapply(wtp_long$var[wtp_long$type=='wtp'], function(x) strsplit(x, "[o]")[[1]][2])

#Create for regret questions
wtp_long$var[wtp_long$type=='regret'] = sapply(wtp_long$var[wtp_long$type=='regret'], function(x) str_sub(x,7,-1)) #numbers only
wtp_long$initial[wtp_long$type=='regret'] = sapply(wtp_long$var[wtp_long$type=='regret'], function(x) strsplit(x, "[to]")[[1]][1])
wtp_long$final[wtp_long$type=='regret'] = sapply(wtp_long$var[wtp_long$type=='regret'], function(x) strsplit(x, "[o]")[[1]][2])

#Make it a numeric column
wtp_long$initial = as.numeric(as.character(wtp_long$initial))
wtp_long$final = as.numeric(as.character(wtp_long$final))

#scale initial and final to 0 to 1 instead of 0 to 100
wtp_long$initial = wtp_long$initial/100
wtp_long$final = wtp_long$final/100

#Create change variable
wtp_long$change = wtp_long$final - wtp_long$initial
wtp_long$change = round(wtp_long$change, 2) #round to get rid of slightly off numbers

#Percent reduction and increase calculation to create those variables
wtp_long$perc_reduction = 1 - (1.001-wtp_long$final)/(1.001-wtp_long$initial) #add a small number to account for zeroes
wtp_long$perc_increase = (wtp_long$final - wtp_long$initial)/wtp_long$initial


#
# Merge on demographics and attention checks
#

clean = merge(wtp_long, demo_wide, by='id')
#subset to exclude our test responses
clean = clean[clean$id>9,]

#
# Scoring
#
#Score CRT Column
clean$crt_mary_corr = ifelse(clean$CRT_mary=="Mary" | clean$CRT_mary=="mary",1,0)
clean$crt_bear_corr = ifelse(clean$CRT_bear=="1250 pounds",1,0)
clean$crt_flock_corr = ifelse(clean$CRT_flock=="4 days",1,0)
clean$crt_jerry_corr = ifelse(clean$CRT_jerry=="29 kids",1,0)
clean$crt_score = clean$crt_mary_corr + clean$crt_bear_corr + clean$crt_flock_corr + clean$crt_jerry_corr


#
# Create datasets with exclusions
#

### Based on no change questions
#Set the maximum WTP to be allowed for the no change WTP questions
wtp_cutoffhi = .5 #prereg exclusion
wtp_cutofflo = .1

#Get IDs to exclude for the high cutoff
exclude_ids_hi = clean$id[clean$change == 0 & clean$response > wtp_cutoffhi & clean$type=="wtp"]
exclude_ids_hi = unique(exclude_ids_hi)

#Get IDs to exclude for the low cutoff
exclude_ids_lo = clean$id[clean$change == 0 & clean$response > wtp_cutofflo & clean$type=="wtp"]
exclude_ids_lo = unique(exclude_ids_lo)

length(exclude_ids_hi)/length(unique(clean$id))
#20% excluded
length(exclude_ids_lo)/length(unique(clean$id))
#25% excluded

clean_nolo = clean[!(clean$id %in% exclude_ids_lo),]
clean_nohi = clean[!(clean$id %in% exclude_ids_hi),]


#Demographics
mean(clean_nohi$ageQ)
sd(clean_nohi$ageQ)
table(clean_nohi$genderQ[!duplicated(clean_nohi$id)])



### Based on english checks
# No exclusions made


##########################
#
# ANALYSIS
#
##########################

#
# WTP analyses
#

# No exclusions
nullLMER_nox = lmer(response ~ change+ (1 | id), data=clean[clean$type=='wtp',], REML=FALSE)
conditionalRegretLMER_nox = lmer(response ~ change + perc_reduction + (1  + perc_reduction| id), data=clean[clean$type=='wtp',], REML=FALSE)
outcomeBiasLMER_nox = lmer(response ~ change + final + (1 + final| id), data=clean[clean$type=='wtp',], REML=FALSE)

anova(nullLMER_nox, outcomeBiasLMER_nox, conditionalRegretLMER_nox)


# Exclude those who pay more than $0.50 (preregistered analysis)
nullLMER = lmer(response ~ change+ (1 | id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)
conditionalRegretLMER = lmer(response ~ change + perc_reduction + (1 + perc_reduction| id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)
outcomeBiasLMER = lmer(response ~ change + final + (1 + final| id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)

anova(nullLMER, outcomeBiasLMER, conditionalRegretLMER)

#
# Regret analyses
#

#With exclusions
nullLMERr = lmer(response ~ 1 + (1  | id) , data=clean_nohi[clean_nohi$type=='regret',])
regretLMERr = lmer(response ~ perc_reduction + (1 + perc_reduction| id) , data=clean_nohi[clean_nohi$type=='regret',])
outcomeBiasLMERr = lmer(response ~ final + (1 + final| id) , data=clean_nohi[clean_nohi$type=='regret',])
anova(nullLMERr, outcomeBiasLMERr, conditionalRegretLMERr)

#Without exclusions
nullLMERr = lmer(response ~ 1 + (1  | id) , data=clean[clean$type=='regret',])
regretLMERr = lmer(response ~ perc_reduction + (1 + perc_reduction| id) , data=clean[clean$type=='regret',])
outcomeBiasLMERr = lmer(response ~ final + (1 + final| id) , data=clean[clean$type=='regret',])
anova(nullLMERr, outcomeBiasLMERr, conditionalRegretLMERr)

#
# Exploratory CRT Analysis
#

#Distribution of CRT scores
indiv_level = clean_nohi[!duplicated(clean_nohi$id),]
indiv_level$crt_score
ggplot(data=indiv_level, aes(x=crt_score)) + geom_bar() + ggtitle("Distribution of CRT Scores")

prop.table(table(indiv_level$crt_score))


# CRT and WTP
#Regression with CRT included
#Not significant
#continuous CRT score version
fancyRegretLMERnohiCRT = lmer(response ~ change + perc_reduction + crt_score + (1 + perc_reduction | id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)
summary(fancyRegretLMERnohiCRT)

#categorical crt score version
fancyRegretLMERnohiCRT = lmer(response ~ change + perc_reduction + as.factor(crt_score) + (1 + perc_reduction | id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)
summary(fancyRegretLMERnohiCRT)

#Basic regression relationship at all!?
summary(lm(response ~ crt_score, data = clean_nohi[clean_nohi$type=='wtp',]))


# Plot CRT and WTP
#Get means
wtp_changesCRT = aggregate(response ~ initial + change_for_plot + crt_score, data=clean[clean$type=='wtp' & (clean$change_for_plot==.05 | clean$change_for_plot==.1),], mean)
#Get standard deviation
wtp_changesCRTsd = aggregate(response ~ initial + change_for_plot + crt_score, data=clean[clean$type=='wtp' & (clean$change_for_plot==.05 | clean$change_for_plot==.1),], sd)
wtp_changes_dfrealCRTsd = wtp_changesCRTsd
#Get standard error
wtp_changesCRT$sd = wtp_changes_dfrealCRTsd$response
wtp_changesCRT$se = wtp_changesCRT$sd/sqrt(length(unique(clean_nohi$id)))
#Split off for basic graph
wtp_changes_dfrealCRT = wtp_changesCRT
wtp_changes_dfrealCRT$increase = ifelse(wtp_changesCRT$change_for_plot==.1, 'Ten %', 'Five %')

#Subset to 10% increase only and graph
wtp_changes_dfrealCRT10 = wtp_changes_dfrealCRT[wtp_changes_dfrealCRT$increase=="Ten %",]

#Plot just WTP actual
ggplot(data =wtp_changes_dfrealCRT10[wtp_changes_dfrealCRT10$crt_score>0,] , aes(x = initial, y = response, group=as.factor(crt_score), color = as.factor(crt_score))) + geom_line() + ggtitle("Willingness to pay by starting probability and CRT score; 10% increase") + xlab('Starting probability') + ylab('WTP in $') + geom_errorbar(aes(ymin=response-se, ymax=response+se))

#
# CRT and regret
#

#Regression with CRT included
#Not significant
#continuous CRT score version
fancyRegretLMERnohiCRT = lmer(response ~ change + perc_reduction + crt_score + (1 + perc_reduction | id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)
summary(fancyRegretLMERnohiCRT)

#categorical crt score version
fancyRegretLMERnohiCRT = lmer(response ~ change + perc_reduction + as.factor(crt_score) + (1 + perc_reduction | id), data=clean_nohi[clean_nohi$type=='wtp',], REML=FALSE)
summary(fancyRegretLMERnohiCRT)

#Basic regression relationship at all!?
regretCRTLMERhi = lmer(response ~ perc_reduction + crt_score +(1 + perc_reduction| id) , data=clean[clean_nohi$type=='regret',])
summary(regretCRTLMERhi)

regretCRTonlyLMERhi = lmer(response ~ crt_score +(1 + perc_reduction| id) , data=clean[clean_nohi$type=='regret',])
summary(regretCRTLMERhi)


study1MeansByPR = wtp_and_prop_decreaseALL10
length(unique(clean_nohi$id))


average_wtps = data.frame(aggregate(response ~ change + initial, data=clean_nohi[clean_nohi$type=='wtp',], mean))
average_wtps_sd = data.frame(aggregate(response ~ change + initial, data=clean_nohi[clean_nohi$type=='wtp',], sd))

average_wtps$SE = average_wtps_sd$response/sqrt(length(unique(clean_nohi$id)))

average_wtps = average_wtps[average_wtps$change==.10,2:4]

study1MeansbyInitial = average_wtps

##########################
#
# FIGURE CREATION
#
##########################

#Transform data for Fig S3
wtpStats = clean_nohi[clean_nohi$type=='wtp' & (clean_nohi$change==.05 | clean_nohi$change==.1),] %>%
  group_by(initial, change) %>%
  summarize(meanWTP = mean(response, na.rm=T), SEWTP = sd(response, na.rm=T)/sqrt(length(unique(clean_nohi$id))))

wtpStatsPR = clean_nohi[clean_nohi$type=='wtp' & (clean_nohi$change==.05 | clean_nohi$change==.1),] %>%
  group_by(perc_reduction, change) %>%
  summarize(meanWTP = mean(response, na.rm=T), SEWTP = sd(response, na.rm=T)/sqrt(length(unique(clean_nohi$id))))


wtpStats$Percent_Increase = ifelse(wtpStats$change==.1,"10% Increase", "5% Increase")
wtpStatsPR$Percent_Increase = ifelse(wtpStatsPR$change==.1,"10% Increase", "5% Increase")

#Figure S3
ggplot(data = wtpStats, aes(x = initial, y = meanWTP, fill=Percent_Increase, color=Percent_Increase, group=Percent_Increase)) + geom_line() + geom_point() + ggtitle("Willingness to pay by starting probability of lottery") + xlab("Starting probability of lottery") + ylab("WTP ($)")  + scale_x_continuous(labels = scales::percent, limits = c(0,1)) +ylim(0,1.5) + geom_errorbar(aes(ymin = meanWTP - SEWTP, ymax = meanWTP + SEWTP))

#Transform data for Fig S4
average_wtps = data.frame(aggregate(response ~ change + initial + final, data=clean_nohi[clean_nohi$type=='wtp',], mean))
average_wtps$Increase = as.factor(paste0(average_wtps$change*100,'%'))

#Figure S4
ggplot(data=average_wtps[average_wtps$change==.1| average_wtps$change ==.09,], aes(x= final, y=response, color=Increase)) + geom_point() + ggtitle("WTP by Final Probability, ~10% Increase") + scale_x_continuous('Final % Chance to Win', labels = scales::percent) + scale_y_continuous('WTP in $s')
