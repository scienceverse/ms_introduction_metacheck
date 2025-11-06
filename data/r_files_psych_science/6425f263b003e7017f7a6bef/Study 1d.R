#Study 1d

##########################
#
# IMPORT PACKAGES
#
##########################

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
groundhog.library(latticeExtra, groundhog_day)
groundhog.library(mediation, groundhog_day)

##########################
#
# CLEAN DATA
#
##########################

data_path = "INSERT HERE"

raw <- read.csv(data_path, comment.char="#", stringsAsFactors = FALSE)
names(raw)

#
# DATA CLEANING
#
#delete qualtrics headers
raw = raw[3:nrow(raw),]


wtpColsIdx = seq(from=34,to=204, by=10)
WTPforZeroIdxs = c(214,224)

rColsIdx = seq(from=29,to=199, by=10)
rforZeroIdxs = c(209,219)

names(raw)[wtpColsIdx]
names(raw)[rColsIdx]


mergedWTP = raw[,wtpColsIdx]
mergedWTP$id = 1:nrow(mergedWTP)

mergedR = raw[,rColsIdx]
mergedR$id = 1:nrow(mergedR)

longWTP = gather(mergedWTP, key = questionRawText, value=wtp, X1wtp_1:X18wtp_1)
longR = gather(mergedR, key = questionRawText, value=r, X1r_1:X18r_1)

#Make a ton of changes to longWTP
#get out the question number
longWTP$qNumber  =gsub("[^0-9.-]", "", longWTP$questionRawText)
longWTP$qNumber = substr(longWTP$qNumber, 1, nchar(longWTP$qNumber) -1)
longWTP$lookupTableKeyStart = paste0("q" , longWTP$qNumber , "start")

#Merge on starting probability values from the embedded data in the survey
startingValues = raw[1,303:342]
startValueLookup = t(startingValues)
lookupsStart = unlist(row.names(startValueLookup))
startValueLookupdf = data.frame(startValueLookup)
startValueLookupdf$lookupVal = lookupsStart
colnames(startValueLookupdf) = c("startingProb", "lookupTableKeyStart")
longWTP = merge(longWTP, startValueLookupdf, by="lookupTableKeyStart")
#convert away from factor to numeric
longWTP$startingProb = as.numeric(as.character(longWTP$startingProb))

#Create ending probabilities
longWTP$endingProb = longWTP$startingProb + 10

longWTP$startingProb = longWTP$startingProb/100 #TODO -- keep changing from long to longWTP
longWTP$endingProb = longWTP$endingProb/100

#add in percent reduction
longWTP$percReduction = 1 - (1.00001-longWTP$endingProb)/(1.000001-longWTP$startingProb)
longWTP$percReduction = round(longWTP$percReduction, 2)

#convert wtp to numeric
longWTP$wtp = as.numeric(longWTP$wtp)

#Clean regret questions #########
temp = longR
#Convert longR to be ready to merge
longR$qNumber  =gsub("[^0-9.-]", "", longR$questionRawText)
longR$qNumber = substr(longR$qNumber, 1, nchar(longR$qNumber) -1)
longR$r = as.numeric(longR$r)
longR$r = longR$r/100

#order longWTP and longR identically
longR$qNumber = as.numeric(as.character(longR$qNumber))
longWTP$qNumber = as.numeric(as.character(longWTP$qNumber))

longR = longR[order(longR$id, longR$qNumber),]
longWTP = longWTP[order(longWTP$id, longWTP$qNumber),]


#Merge on longR
merged = longWTP
merged$r = longR$r


dropCols = c("qNumber", "questionRawText", "lookupTableKeyStart")

merged = merged[, !names(merged) %in% dropCols]

clean = merged

#make a change variable
clean$change = .10


#
# Check the WTP for zero change
#
raw[, WTPforZeroIdxs] > .5

#72
#77

#Remove test runs which are not actual participants
test_runs = ifelse(is.na(as.numeric(raw$ID)),1,0)
ids = 1:length(test_runs)
ids[test_runs==1]
#36 58 67 70


#Dataframe with all participants

test_runs_to_exclude = c(36, 58, 67, 70)
test_runs_and_overpayers_to_exclude = c(36, 58, 67, 70, 72, 77)


clean = clean[clean$id != 36,]
clean = clean[clean$id != 58,]
clean = clean[clean$id != 67,]
clean = clean[clean$id != 70,]

#Dataframe with all participants and exclusions
bigExclusions = clean
bigExclusions = bigExclusions[bigExclusions$id != 72,]
bigExclusions = bigExclusions[bigExclusions$id != 77,]


##########################
#
# ANALYSIS
#
##########################


#Make null models
nullWTPLMER = lmer(wtp ~ change + (1 | id), data=bigExclusions, REML = FALSE)

#Make final outcomee bias models
obWTPLMER = lmer(wtp ~ endingProb + (1  | id), data=bigExclusions, REML=FALSE)
summary(obWTPLMER)

#Make fancy regret models
frWTPLMER = lmer(wtp ~ percReduction + (1  | id), data=bigExclusions, REML=FALSE)
summary(frWTPLMER)


#Compare models
anova(nullWTPLMER, obWTPLMER, frWTPLMER)

#Graph it
wtpMeans = aggregate(wtp ~ startingProb,data=bigExclusions[!is.na(bigExclusions$wtp),],  mean)
wtpSEs = aggregate(wtp ~ startingProb,data=bigExclusions[!is.na(bigExclusions$wtp),],  function(x) sd(x)/sqrt(length(unique(bigExclusions$id))))
wtpMeans$SE = wtpSEs$wtp

ggplot(data = wtpMeans, aes(x = startingProb, y = wtp)) + geom_line() + ggtitle("Willingness to pay by starting probability") + xlab('Starting probability') + ylab('WTP in $') + geom_errorbar(aes(ymin=wtp-SE, ymax=wtp+SE)) + scale_x_continuous(labels = scales::percent)



#Make null models
nullWTPLMER_nox = lmer(wtp ~ change + (1 | id), data=clean, REML = FALSE)

#Make final outcomee bias models
obWTPLMER_nox = lmer(wtp ~ endingProb + (1  | id), data=clean, REML=FALSE)
summary(obWTPLMER_nox)

#Make fancy regret models
frWTPLMER_nox = lmer(wtp ~ percReduction + (1  | id), data=clean, REML=FALSE)
summary(frWTPLMER_nox)


#Compare models
anova(nullWTPLMER_nox, obWTPLMER_nox, frWTPLMER_nox)


#
# Graph the regret questions
#


#with wtp exclusions
rMeansE = aggregate(r ~ startingProb,data=bigExclusions[!is.na(bigExclusions$r),],  mean)
rSEsE = aggregate(r ~ startingProb,data=bigExclusions[!is.na(bigExclusions$r),],  function(x) sd(x)/sqrt(length(unique(bigExclusions$id))))
rMeansE$SE = rSEsE$r

ggplot(data = rMeansE, aes(x = startingProb, y = r)) + geom_line() + ggtitle("Proportion of outcomes regretted by starting probability") + xlab('Starting probability') + ylab('Regret (% of outcomes)') + geom_errorbar(aes(ymin=r-SE, ymax=r+SE)) + scale_x_continuous(labels = scales::percent)


#with wtp exclusions, perc reduction
rMeansE = aggregate(r ~ percReduction,data=bigExclusions[!is.na(bigExclusions$r),],  mean)
rSEsE = aggregate(r ~ percReduction,data=bigExclusions[!is.na(bigExclusions$r),],  function(x) sd(x)/sqrt(length(unique(bigExclusions$id))))
rMeansE$SE = rSEsE$r

ggplot(data = rMeansE, aes(x = percReduction, y = r)) + geom_line() + ggtitle("Proportion of outcomes regretted by percent reduction in bad outcomes") + xlab('Percent reduction in bad outcomes') + ylab('Regret (% of outcomes)') + geom_errorbar(aes(ymin=r-SE, ymax=r+SE)) + scale_x_continuous(labels = scales::percent)
#
# Graph regret & WTP answers together
#


names(rMeans) = c("startingProbability", "Regret", "regretSE")
names(wtpMeans) = c("startingProbability", "WTP", "wtpSE")

wtpChart = xyplot(WTP ~ startingProbability, wtpMeans, type='l', lwd=2, main="Regret and WTP for Increases by Starting %")
rChart = xyplot(r ~ startingProb, rMeansE, type='l', lwd=2)


doubleYScale(rChart, wtpChart, text = c("Regret (%)", "WTP ($)"), add.ylab2=TRUE)


#write.csv(bigExclusions, "C:/Users/User/Dropbox/Drive Sync/Berkeley PhD/Research Projects/02 -- Ongoing Research Projects/Fancy Regret (w ERK SB)/04 - Aggregate analyses across experiments/raw data/lab_study.csv")



#
# Figure 2020-7-21
#

require(dplyr)

#Get data into a format which makes it easier to analyze
wtpStats = bigExclusions %>%
  dplyr::group_by(startingProb) %>%
  dplyr::summarize(meanWTP = mean(wtp, na.rm=T), SEWTP = sd(wtp, na.rm=T)/sqrt(length(unique(clean$id))))


wtpStatsPR = bigExclusions %>%
  dplyr::group_by(percReduction) %>%
  dplyr::summarize(meanWTP = mean(wtp, na.rm=T), SEWTP = sd(wtp, na.rm=T)/sqrt(length(unique(clean$id))))


#Figure 7
ggplot(data = wtpStats, aes(x = startingProb, y = meanWTP)) + geom_line() + geom_point() + ggtitle("Willingness to pay for 10% increase in chances to win by starting probability of lottery") + xlab("Starting probability of winning lottery") + ylab("WTP ($)")  + scale_x_continuous(labels = scales::percent, limits = c(0,1)) +ylim(0,1.5) + geom_errorbar(aes(ymin = meanWTP - SEWTP, ymax = meanWTP + SEWTP))

#Figure 8
ggplot(data = wtpStatsPR, aes(x = percReduction, y = meanWTP)) + geom_line() + geom_point() + ggtitle("Willingness to pay for 10% increase in chances to win by percent reduction in bad outcomes") + xlab("Percent reduction in bad outcomes") + ylab("WTP ($)")  + scale_x_continuous(labels = scales::percent, limits = c(0,1)) +ylim(0,1.5) + geom_errorbar(aes(ymin = meanWTP - SEWTP, ymax = meanWTP + SEWTP))





#
# Testing for non multilevel mediation (totally normal lm)
#
fitM <- lm(r ~ percReduction, data=bigExclusions[complete.cases(bigExclusions),])
fitY <- lm(wtp ~ percReduction + r, data=bigExclusions[complete.cases(bigExclusions),])

summary(fitM)
summary(fitY)

#simple
fitMed <-mediation::mediate(fitM, fitY, treat="percReduction", mediator="r")

summary(fitMed)
plot(fitMed)



#ACME  average causal mediation effects
#ADE average direct effect
#Total effect (combo of both)
#prop mediated -- ratio of these two 



#
# Demographics
#


keep_ids = unique(bigExclusions$id)

mean(as.numeric(raw[keep_ids,]$age), na.rm=T)
sd(as.numeric(raw[keep_ids,]$age), na.rm=T)

#1 is male 2 is female
table(raw[keep_ids,]$genderQ)
