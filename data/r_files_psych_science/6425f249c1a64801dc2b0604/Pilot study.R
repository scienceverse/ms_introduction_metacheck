############################
#
# Pilot Study
#
###############################

library(groundhog)


groundhog_day_lmer = "2020-01-01"
groundhog.library('nlme',groundhog_day_lmer)
groundhog.library('MASS',groundhog_day_lmer)
groundhog.library('lattice',groundhog_day_lmer)
groundhog.library('Matrix',groundhog_day_lmer)
groundhog.library(lmerTest, groundhog_day_lmer)
groundhog.library(lme4, groundhog_day_lmer)


groundhog_day = "2020-01-01"

groundhog.library(tidyverse, groundhog_day)
groundhog.library(Hmisc, groundhog_day)
groundhog.library(ggplot2, groundhog_day)
groundhog.library(scales, groundhog_day)
groundhog.library(plyr, groundhog_day)
groundhog.library(sjPlot, groundhog_day)

#Import data
raw <- read.csv("xxx", comment.char="#", stringsAsFactors = F)

#for saving tables of regression output for inclusion in the paper
regression_table_path = "xxxx"




#########################################
#
# Cleaning
#
########################################

######
# Make main dataset
######

#get rid of the random qualtrics cruft rows, as well as one test run
raw = raw[4:nrow(raw),]

#conditions
#condition1changefirst2enterfirst

#format
#10enter = enter lottery at 10%
#89change = change lottery at 89%
#always a 10% increase for the change trials, unless it is 0 and an attention check



#get the change and enter questions
enterColsIdx = seq(from=100,to=145, by=5) #also getting the 0 entry q
changeColsIdx = seq(from=38,to=83, by=5)
names(raw)[enterColsIdx]
names(raw)[changeColsIdx]
length(names(raw)[changeColsIdx]) == 10
#should be ten of each

#merge it
enterCols = raw[,enterColsIdx]
changeCols = raw[,changeColsIdx]
merged = cbind(enterCols, changeCols)
#id for analysis
merged$id = 1:nrow(merged)

#make it long and fix classes of vars
long = tidyr::gather(merged, key=typeRaw, value=wtp, enter50_1:X89change_1)
long$type = ifelse( grepl('enter', long$typeRaw), "enter", "change")
long$wtp = as.numeric(long$wtp)

#Pull out number associated with it
long$prob = gsub("[^0-9.-]", "", long$typeRaw)
long$prob = substr(long$prob, 1, nchar(long$prob) -1)
long$prob = as.numeric(long$prob)

#Make starting probability variable
long$startingProb = long$prob

#Make ending probability variable
long$endingProb = long$prob + 10
#Make it na if it's an entrance question
long$endingProb = ifelse(long$type == "enter", NA,long$endingProb)

#percentiles!
long$endingProb = long$endingProb/100
long$startingProb =long$startingProb/100

#Add percent reduction column
long$percReduction = 1 - (1.00001-long$endingProb)/(1.000001-long$startingProb)
long$percReduction = round(long$percReduction, 2)
#Make it na if it's an entrance question
long$percReduction = ifelse(long$type == "enter", NA,long$percReduction)


#Make a variable for the expected value
long$EV = NA
long$EV = ifelse(long$type=='enter', 10*long$startingProb, 1)

long$wtpByEv = long$wtp/long$EV


######
# Make an order variable
######



######
# Make attention check exclusion lists
######
#attention checks
#for change trials
#0 change trials
#X30to30attn_1
#X60to60attn_1

#describeQs
#understandOutcome

#for entry trials
#no money trials for enter
#X0enterattn_1

#describeqsenter
#chanceofwinningenter

#overall attention checks
#attn.check
#enteredYN

#0 change attn checks
raw$X30to30attn_1 = as.numeric(as.character(raw$X30to30attn_1))
raw$X60to60attn_1 = as.numeric(as.character(raw$X60to60attn_1))

raw$X0enterattn_1 = as.numeric(as.character(raw$X0enterattn_1))

#exclude based on change comprehension
keep_ids = raw$chanceofwinningenter == "0%, I wouldn't be entered into the lottery"
chanceProbPassAttnIds = 1:nrow(merged)
chanceProbPassAttnIds = chanceProbPassAttnIds[keep_ids]

#Exclude people in the enter condition if they couldn't describe the experiment, the consequences of payment, or if they paid more than $.50 for a 0% chance to win lottery
raw$excludeEnter = ifelse(raw$describeqsenter!="I was not already entered into a lottery, and I was asked what I would pay to enter a lottery with a given chance of winning." | raw$chanceofwinningenter != "0%, I wouldn't be entered into the lottery" | raw$X0enterattn_1>.50, 1, 0)

#Exclude people in the change condition if they couldn't describe the experiment, the consequences of payment, or if they paid more than $.50 for a 0% change in chance to win lottery
raw$excludeChange =  ifelse(raw$describeQs!="I was already entered into a lottery with a given chance of winning, and I was asked what I would pay to change my chances of winning." | raw$understandOutcome != "89%" | raw$X30to30attn_1>.50 | raw$X60to60attn_1>.50, 1, 0)

raw$exclude = ifelse(raw$excludeChange==1 | raw$excludeEnter==1,1 ,0)
passIDs = 1:nrow(merged)
passIDs = passIDs[raw$exclude==0]
#97 pass
#########################################
#
# Analysis for appendix
#
#########################################

#No exclusions

#regret ratio model comparison
#Make null models
nullWTPLMER = lmer(wtp ~ 1 + (1 | id), data=long[long$type=='change',], REML = FALSE)
#Make final outcomee bias models
obWTPLMER = lmer(wtp ~ endingProb + (1  | id), data=long[long$type=='change',], REML=FALSE)
#Make fancy regret models
frWTPLMER = lmer(wtp ~ percReduction + (1  | id), data=long[long$type=='change',], REML=FALSE)
#Compare models
anova(nullWTPLMER, obWTPLMER, frWTPLMER)

#this is probably the best test, but more complex to explain...
fullMod = lmer(wtpByEv ~ startingProb * type + (1 | id), data=long)
summary(fullMod)
plot_model(fullMod, type='int')



long$is20vs80 = ifelse(long$startingProb==.8, "eighty", NA)
long$is20vs80 = ifelse(long$startingProb==.2, "twenty", long$is20vs80)

library(sjPlot)
simpleMod = lm(wtpByEv ~ is20vs80 * type, data=long[!is.na(long$is20vs80),])
summary(simpleMod)
plot_model(simpleMod, type='int')


#Create updated figure
simpleMod2 = lm(wtpByEv ~ startingProb * type, data=long)
summary(simpleMod)
plot_model(simpleMod2, type='int')
sjPlot::tab_model(simpleMod2, file = paste0(regression_table_path, "Study S2 Regression without Exclusions.doc"))


library(dplyr)

wtpTableStats = long %>%
  dplyr::group_by(startingProb, type) %>%
  dplyr::summarize(meanWTP = mean(wtpByEv, na.rm=T), SEWTP = sd(wtp, na.rm=T)/sqrt(length(unique(longExclude$id))))


ggplot(data=wtpTableStats, aes(x=startingProb, y=meanWTP, group=type, color=type)) + geom_point() +ggtitle("WTP per dollar EV to enter/change a lottery (No exclusions)") + geom_line() + xlab("Initial Probability") + ylab("WTP per dollar EV") +theme_light() + ylim(0,5) + geom_errorbar(data=wtpTableStats, aes(ymin= meanWTP - SEWTP, ymax = meanWTP + SEWTP))


#
# With exclusions
#


long_exclusions = long[long$id %in% passIDs,]

#regret ratio model comparison
#Make null models
nullWTPLMER_nox = lmer(wtp ~ 1 + (1 | id), data=long_exclusions[long_exclusions$type=='change',], REML = FALSE)
#Make final outcomee bias models
obWTPLMER_nox = lmer(wtp ~ endingProb + (1  | id), data=long_exclusions[long_exclusions$type=='change',], REML=FALSE)
#Make fancy regret models
frWTPLMER_nox = lmer(wtp ~ percReduction + (1  | id), data=long_exclusions[long_exclusions$type=='change',], REML=FALSE)
#Compare models
anova(nullWTPLMER_nox, obWTPLMER_nox, frWTPLMER_nox)


#this is probably the best test, but more complex to explain...
fullMod = lmer(wtpByEv ~ startingProb * (type=='change') + (1 | id), data=long_exclusions)
summary(fullMod)


long_exclusions$is20vs80 = ifelse(long_exclusions$startingProb==.8, "eighty", NA)
long_exclusions$is20vs80 = ifelse(long_exclusions$startingProb==.2, "twenty", long$is20vs80)

library(sjPlot)
simpleMod = lm(wtpByEv ~ is20vs80 * type, data=long_exclusions[!is.na(long_exclusions$is20vs80),])
summary(simpleMod)
plot_model(simpleMod, type='int')
#Create updated figure



simpleMod2 = lm(wtpByEv ~ startingProb * type, data=long_exclusions)
summary(simpleMod)
plot_model(simpleMod2, type='int')


sjPlot::tab_model(simpleMod2, file = paste0(regression_table_path, "Study S2 Regression with Exclusions.doc"))



library(dplyr)

wtpTableStats = long_exclusions %>%
  dplyr::group_by(startingProb, type) %>%
  dplyr::summarize(meanWTP = mean(wtpByEv, na.rm=T), SEWTP = sd(wtp, na.rm=T)/sqrt(length(unique(long_exclusions$id))))


ggplot(data=wtpTableStats, aes(x=startingProb, y=meanWTP, group=type, color=type)) + geom_point() +ggtitle("WTP per dollar EV to enter/change a lottery ") + geom_line() + xlab("Initial Probability") + ylab("WTP per dollar EV") +theme_light() + ylim(0,5) + geom_errorbar(data=wtpTableStats, aes(ymin= meanWTP - SEWTP, ymax = meanWTP + SEWTP))





#########################################
#
# Analysis
#
#########################################

#Basic graphs of attn checks
#for enter
table(raw$describeqsenter) #148 82%
table(raw$chanceofwinningenter) #123 68%
#for change
table(raw$describeQs) #158 87%
table(raw$understandOutcome) #144 80%
#for 0 questions
plot(raw$X30to30attn_1[order(raw$X30to30attn_1)])
table(raw$X30to30attn_1 < .5) #48 above

plot(raw$X60to60attn_1[order(raw$X60to60attn_1)])
table(raw$X60to60attn_1 < .5) #65 above


plot(raw$X0enterattn_1[order(raw$X0enterattn_1)])
table(raw$X0enterattn_1 < .5) #29 above


#Graph of WTP for change/enter


#Graph WTP to enter and change by prob
averageWTPS = aggregate(wtp ~ prob + type, data=long, mean)

ggplot(data=averageWTPS, aes(x=prob, y=wtp, group=type, color=type)) + geom_point() +ggtitle(paste0("WTP to enter and change a lottery, N=",length(unique(long$id)))) + geom_line() + xlab("Initial Probability") + ylab("WTP") +theme_light()

#Convert it to EV

averageWTPS$wtpByEV = averageWTPS$wtp

averageWTPS$wtpByEV[averageWTPS$type=='enter'] = averageWTPS$wtp[averageWTPS$type=='enter']/(10*(averageWTPS$prob[averageWTPS$type=='enter']/100))


ggplot(data=averageWTPS, aes(x=prob, y=wtpByEV, group=type, color=type)) + geom_point() +ggtitle(paste0("WTP per dollar EV to enter and change a lottery, N=",length(unique(long$id)))) + geom_line() + xlab("Initial Probability") + ylab("WTP ($)") +theme_light() + ylim(0,4)



#Do it with basic exclusions
longExclude = long[long$id %in% passIDs,]
#Graph WTP to enter and change by prob

averageWTPSE$diff = c(NA, diff(averageWTPSE$wtp))
averageWTPSE$wtp[averageWTPSE$type=='enter' & averageWTPSE$prob!=10] = averageWTPSE$diff[averageWTPSE$type=='enter' & averageWTPSE$prob!=10]



ggplot(data=averageWTPSE, aes(x=prob, y=wtp, group=type, color=type)) + geom_point() +ggtitle(paste0("WTP per dollar EV to enter/change a lottery, N=",length(unique(longExclude$id)))) + geom_line() + xlab("Initial Probability") + ylab("WTP ($)") +theme_light() + ylim(0,4)


#Modeling
wtpByEvChange10 = long$wtpByEv[long$type=="change" & long$startingProb==.2]
wtpByEvChange90 = long$wtpByEv[long$type=="change" & long$startingProb==.9]
t.test(wtpByEvChange10, wtpByEvChange90)


wtpByEvEnter10 = long$wtpByEv[long$type=="enter" & long$startingProb==.2]
wtpByEvEnter90 = long$wtpByEv[long$type=="enter" & long$startingProb==.9]

t.test(wtpByEvEnter10, wtpByEvEnter90)


summary(lm(wtpByEv ~ startingProb, data=long[long$type=='enter',]))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.89586    0.04384  20.435  < 2e-16 ***
#  startingProb -0.55038    0.07264  -7.577 5.64e-14 ***

summary(lm(wtpByEv ~ startingProb, data=long[long$type=='change',]))
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    0.9784     0.1064   9.199   <2e-16 ***
#startingProb   2.2246     0.1826  12.184   <2e-16 ***

lmerEnter = lmer(wtpByEv ~ startingProb + (1  | id), data=long[long$type=='enter',])
summary(lmerEnter)

lmerChange = lmer(wtpByEv ~ startingProb + (1  | id), data=long[long$type=='change',])
summary(lmerChange)


#
# Statistics for the paper
#

#this is probably the best test, but more complex to explain...
fullMod = lmer(wtpByEv ~ startingProb * (type=='change') + (1 | id), data=long)
summary(fullMod)


long$is20vs80 = ifelse(long$startingProb==.8, "eighty", NA)
long$is20vs80 = ifelse(long$startingProb==.2, "twenty", long$is20vs80)

library(sjPlot)
simpleMod = lm(wtpByEv ~ is20vs80 * type, data=long[!is.na(long$is20vs80),])
summary(simpleMod)
plot_model(simpleMod, type='int')
#Create updated figure

simpleMod2 = lm(wtpByEv ~ startingProb * type, data=long)
summary(simpleMod)
plot_model(simpleMod2, type='int')



library(dplyr)

wtpTableStats = longExclude %>%
  dplyr::group_by(startingProb, type) %>%
  dplyr::summarize(meanWTP = mean(wtpByEv, na.rm=T), SEWTP = sd(wtp, na.rm=T)/sqrt(length(unique(longExclude$id))))


ggplot(data=wtpTableStats, aes(x=startingProb, y=meanWTP, group=type, color=type)) + geom_point() +ggtitle("WTP per dollar EV to enter/change a lottery") + geom_line() + xlab("Initial Probability") + ylab("WTP per dollar EV") +theme_light() + ylim(0,5) + geom_errorbar(data=wtpTableStats, aes(ymin= meanWTP - SEWTP, ymax = meanWTP + SEWTP))


wtpTableStats_nox = long %>%
  dplyr::group_by(startingProb, type) %>%
  dplyr::summarize(meanWTP = mean(wtpByEv, na.rm=T), SEWTP = sd(wtp, na.rm=T)/sqrt(length(unique(long$id))))


ggplot(data=wtpTableStats_nox, aes(x=startingProb, y=meanWTP, group=type, color=type)) + geom_point() +ggtitle("WTP per dollar EV to enter/change a lottery") + geom_line() + xlab("Initial Probability") + ylab("WTP per dollar EV") +theme_light() + ylim(0,5) + geom_errorbar(data=wtpTableStats_nox, aes(ymin= meanWTP - SEWTP, ymax = meanWTP + SEWTP))

#########################################
#
# Demographics
#
#########################################
#Mean age
mean(as.numeric(as.character(raw[passIDs,]$age)), na.rm=T)
#SD age
sd(as.numeric(as.character(raw[passIDs,]$age)), na.rm=T)

#Gender table
table(raw[passIDs,]$gender)
