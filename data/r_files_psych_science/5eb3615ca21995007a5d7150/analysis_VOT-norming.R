library(plyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(lme4)
library(lattice)
library(quickpsy)
library(pwr)
#library(pwr2)

###################
## set up directories and load data

# prefix for each OS
os <- .Platform$OS.type

# data
data.raw <- read.delim(paste(dataDir,'psiturk_VOT-norming_spring2019.txt', sep='/'),header = T)
summary(data.raw)

data <- data.raw

##############

# remove pilot subjects

pilotSubjs <- c(
  'debug1KAKT3:debugP7ZN4Q',
  'debugTUB5XJ:debugYZT19X',
  'debugHFMXDY:debugM7WH7R'
)

length(unique(data$uniqueid))
data <- subset(data, !(data$uniqueid %in% pilotSubjs))
length(unique(data$uniqueid))

############################
## headphone check

hpdata <- subset(data, hpCheck_didPass != '')
hpdata <- ddply(hpdata, .(uniqueid, hpCheck_num_hp_check, hpCheck_correctThreshold), summarise,
                totalCorrect = max(hpCheck_totalCorrect),
                totalTrials = max(hpCheck_totalTrials))
hpdata

# to exclude (subjects)
tmpHP <- ddply(hpdata, .(uniqueid), summarise,
               minCorrect = min(totalCorrect),
               maxCorrect = max(totalCorrect),
               numAttempts = max(hpCheck_num_hp_check),
               maxCorrectUnderThreshold = 1*(maxCorrect < max(hpdata$hpCheck_correctThreshold)))
tmpHP

#debug4756 3x
#debug6053 2x (1st failed)
#debug6533 6x (failed 1st, 3rd, 4th, 5th, 6th) --> completed tho so that's good!

hp_fail_subj <- c()

length(unique(data$uniqueid))
data <- subset(data, !(data$uniqueid %in% hp_fail_subj))
length(unique(data$uniqueid))

############################
## num trials for each

expectedNum <- 1053
expectedNumNonTarget <- 891
expectedNumTarget <- expectedNum - expectedNumNonTarget
data <- ddply(subset(data, blockTrialNum > 0), .(uniqueid), transform,
              numTrialsComplete = length(blockTrialNum),
              propComplete = length(blockTrialNum)/expectedNum)
tmp <- ddply(subset(data, blockTrialNum > 0), .(uniqueid), summarise,
             numTrialsComplete = length(blockTrialNum),
             propComplete = length(blockTrialNum)/expectedNum)
tmp

# data <- ddply(subset(data, blockTrialNum > 0), .(uniqueid), transform,
#               numTrialsComplete = length(blockTrialNum),
#               propComplete = length(blockTrialNum)/expectedNum)
# tmp <- ddply(subset(data, blockTrialNum > 0), .(uniqueid, list), summarise,
#              numTrialsComplete = length(blockTrialNum),
#              propComplete = length(blockTrialNum)/expectedNum)
# tmp
# ddply(subset(data, blockTrialNum > 0 & list != 0), .(list), summarise,
#       numSubj = length(unique(uniqueid)),
#       meanNumTrials = mean(length(uniqueid)),
#       meanNumTrialsPerSubj = meanNumTrials/numSubj)
# ddply(subset(tmp, list>0), .(list), summarise,
#       numCompleteSubj = sum(numTrialsComplete >= expectedNumNonTarget),
#       numIncomplete = sum(numTrialsComplete < expectedNumNonTarget))
# ddply(subset(tmp, list==0), .(list), summarise,
#       numCompleteSubj = sum(numTrialsComplete >= expectedNumTarget),
#       numIncomplete = sum(numTrialsComplete < expectedNumTarget))

data$incomplete <- 1*(data$numTrialsComplete < expectedNum)
data$complete <- 1*(!data$incomplete)

#5  debug5042:debug5042               678   0.64387464    2
#13 debug6050:debug6050              1053   1.00000000    2
#18 debug6295:debug6295              1053   1.00000000    2
#31 debug6529:debug6529              1053   1.00000000    2

##############
##############

# pre-processing and add coding
data$uniqueid <- factor(data$uniqueid)
data$index <- factor(data$index)
data$t_choice <- 1*(data$t.d_choice=='t')
data$VOT_centered <- scale(data$VOT, center=T, scale=F)
data$blockTrialNum_centered <- scale(data$blockTrialNum, center=T, scale=F)

data <- subset(data, blockTrialNum > 0)

data$t.word <- as.character(data$t.word)
data$t.word[data$t.word=='ta' & data$index == 1] <- 'ta_1_SHIFTED'
data$t.word[data$t.word=='ta' & data$index == 2] <- 'ta_2_SHIFTED'
data$t.word[data$t.word=='ta_1_SHIFTED' & grepl('ORIGf0', data$soundFile)] <- 'ta_1_ORIG'
data$t.word[data$t.word=='ta_2_SHIFTED' & grepl('ORIGf0', data$soundFile)] <- 'ta_2_ORIG'
data$t.word <- as.factor(data$t.word)

testItems <- c('ta_1_SHIFTED','ta_1_ORIG','ta_2_SHIFTED','ta_2_ORIG')

############################
## 25% RTs < 75ms in any trial type?

#rtLow <- 200
rtLow <- 75
minExcl <- 0.20

data <- ddply(data, .(uniqueid), transform,
             mean.lowRT = mean(rt < rtLow, na.rm=T)
)
data$exclude.lowRT <- 1*(data$mean.lowRT > minExcl)

rtTooFastSubj <- unique(data$uniqueid[data$exclude.lowRT==1])
rtTooFastSubj

unique(subset(data, select = c(uniqueid, mean.lowRT, exclude.lowRT)))

############################
## 3 lowest VOTs not sig diff from 3 highest VOTs?

tmp <- ddply(data, .(uniqueid), summarise,
             mean.t_choice.lowVOT = mean(t_choice[VOT <= 40], na.rm=T),
             sd.t_choice.lowVOT = sd(t_choice[VOT <= 40], na.rm=T),
             n.t_choice.lowVOT = length(!(is.na(t_choice[VOT <= 40]))),
             mean.t_choice.highVOT = mean(t_choice[VOT >= 60], na.rm=T),
             sd.t_choice.highVOT = sd(t_choice[VOT >= 60], na.rm=T),
             n.t_choice.highVOT = length(!(is.na(t_choice[VOT >= 60])))
             )
tmp$t <- (tmp$mean.t_choice.highVOT - tmp$mean.t_choice.lowVOT) / ( mean(c(tmp$sd.t_choice.lowVOT,tmp$sd.t_choice.highVOT)) / sqrt(tmp$n.t_choice.lowVOT) )
tmp$p <- 2*pt(tmp$t, tmp$n.t_choice.lowVOT, lower=FALSE)

tmp$exclude.noPsychometric <- 1*(tmp$t < 0 | tmp$p > .05)

data <- merge(data, subset(tmp, select = c(uniqueid, exclude.noPsychometric)), sort=F)

exclPsychSubj <- unique(data$uniqueid[data$exclude.noPsychometric == 1])
exclPsychSubj

############################
## new criteria (not in pre-reg): <n% or >(1-n)% at first and last vot levels?

cutoffPercentage <- 1/3
#cutoffPercentage <- 0.15
data <- ddply(data, .(uniqueid), transform,
             mean.lowestVOT = mean(t_choice[VOT == min(data$VOT)], na.rm=T),
             mean.highestVOT = mean(t_choice[VOT == max(data$VOT)], na.rm=T)
)
data$excludePercentage <- 1*(data$mean.lowestVOT > cutoffPercentage | data$mean.highestVOT < (1 - cutoffPercentage))

exclCutoffSubj <- unique(data$uniqueid[data$excludePercentage==1])
exclCutoffSubj

############################
## final complete subj list

# can see subjects to exclude b/c of headphone check below
tmpHP

# code which test exemplars they got
data <- ddply(data, .(uniqueid), transform,
              shifted_exemplars = max(t.word=='ta_1_SHIFTED'))

# excluded subjects
data$excludeSubj <- 1*(data$exclude.lowRT | data$exclude.noPsychometric | data$excludePercentage)

# details about subjects and lists
tmpSubjSum <- unique(subset(data, list != 0, select = c('uniqueid', 'list', 'complete', 'shifted_exemplars', 'excludeSubj')))

tmpSubjSumSum <- ddply(tmpSubjSum, .(list, complete, shifted_exemplars, excludeSubj), summarise,
      numSubj = sum(complete==complete))
tmpSubjSumSum

subset(tmpSubjSumSum, excludeSubj==0 & complete==1, select = -excludeSubj)
subset(tmpSubjSumSum, excludeSubj==0 & complete==0, select = -excludeSubj)

subset(tmpSubjSumSum, excludeSubj==1 & complete==1, select = -excludeSubj)
subset(tmpSubjSumSum, excludeSubj==1 & complete==0, select = -excludeSubj)

# excluded subj
unique(data[data$excludeSubj==1,c('uniqueid','exclude.lowRT','exclude.noPsychometric','excludePercentage',
                                  'mean.lowestVOT','mean.highestVOT','mean.lowRT')])

############################
## data subset

# only include complete subjects
data.subset <- subset(data, excludeSubj==0 & complete==1)

# mark low RT trials as exclude
data.subset$exclude.trial <- 1*(data.subset$rt < rtLow)

# number of trials
ddply(data.subset, .(uniqueid, shifted_exemplars), summarise,
      numTrials = sum(exclude.trial==exclude.trial),
      numTrialsInclude = sum(exclude.trial==0),
      numTrialsExclude = sum(exclude.trial==1))

# exclude extra trials from subjects
# row number
data.subset$rownum.orig <- as.numeric(rownames(data.subset))
data.subset$diffs <- c(diff(data.subset$blockTrialNum),1)
data.subset$diffDistinct <- 1*(data.subset$diffs < 1)
tmp <- subset(data.subset, diffDistinct==1, select = c(uniqueid, blockTrialNum))

# exclude bad subjects
badSubj <- unique(tmp$uniqueid[tmp$blockTrialNum < expectedNum])
data.subset <- subset(data.subset, !(uniqueid %in% badSubj))

# summmary
tmpSubjSum <- unique(subset(data.subset, list != 0, select = c('uniqueid', 'list', 'shifted_exemplars')))
ddply(tmpSubjSum, .(list, shifted_exemplars), summarise,
     numSubj = sum(list==list))

data.subset <- subset(data.subset, exclude.trial==0)

############################

## plot together
data.subj <- ddply(data.subset, .(uniqueid, VOT, t.word), summarise,
                   mean_t_choice = mean(t_choice),
                   nTrials = length(t_choice))
pl <- ggplot(data.subj,
             aes(x=VOT, y=mean_t_choice)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  #stat_summary(geom="errorbar",fun.data="mean_cl_normal",
  #             size=1, width=0.25) +
  stat_summary(geom="errorbar",fun.data="mean_se",
               size=1, width=0.25) +
  geom_hline(yintercept=0,colour = "black",size=1) +
  facet_wrap(~ t.word, ncol = 5)
pl

###################
## fit thresholds

fit2 <- quickpsy(subset(data.subset),
                x = VOT, k = t_choice,
                grouping = .(uniqueid, t.word),
                bootstrap = 'none')
la2 <- logliks(fit2)
p <- ggplot(la2, aes(x = 1, y = loglik))
p + geom_jitter(width = 0.2)
p <- ggplot(la2, aes(x = 1, y = loglik))
p <- p + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  geom_jitter(width = 0.2)
p

threshlik <- -25
summary(la2$loglik)
sum(la2$loglik < threshlik)

fit2.subset <- subset(fit2, la2$loglik > threshlik)
fit2.subset <- data.frame(fit2.subset$thresholds)
sum(1*!(fit2.subset$thre > 0 & fit2.subset$thre < 100))
mean(1*!(fit2.subset$thre > 0 & fit2.subset$thre < 100))

fit2.subset <- subset(fit2.subset, thre > 0 & thre < 100)

# each word (across subj)
all.thresh <- ddply(fit2.subset, .(t.word), summarise,
           meanThresh = mean(thre),
           medianThresh = median(thre),
           minThresh = min(thre),
           maxThresh = max(thre))
nontest.thresh <- subset(a, !(t.word %in% testItems))
summary(all.thresh$meanThresh)
summary(nontest.thresh$meanThresh)

# median threshold
medianThresh <- median(nontest.thresh$meanThresh)

# plot thresholds by word
pl.fit <- ggplot(fit2.subset,
                 aes(x=t.word, y=thre)) +
  geom_hline(yintercept=medianThresh,colour = "red",size=0.5) +
  stat_summary(fun.y=mean, geom="point") +
  #stat_summary(geom="errorbar",fun.data="mean_cl_normal",
  #             size=1, width=0.25) +
  stat_summary(geom="errorbar",fun.data="mean_se",
               size=1, width=0.25) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
pl.fit

# plot thresholds by word
pl.fit <- ggplot(fit2.subset,
                 aes(x=t.word, y=thre)) +
  geom_hline(yintercept=medianThresh,colour = "red",size=0.5) +
  geom_violin(trim=FALSE, fill='lightblue')+
  geom_boxplot(width=0.1, fill="white")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
pl.fit
