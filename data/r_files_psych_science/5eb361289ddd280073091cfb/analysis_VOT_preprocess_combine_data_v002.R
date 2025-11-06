library(plyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(lme4)
library(lattice)
library(quickpsy)
#library(pwr)
#library(pwr2)
#library(simr)

###################
## set up directories and load data

# prefix for each OS/user
  # insert your Dropbox base directory here; comment out the others
os.prefix <- '/Users/ahafri1/Dropbox'
# directories
dataDir <- file.path(os.prefix, 'GRAD_SCHOOL/DATED_DOCS/2017_FALL/PROJECTS/VOT_adaptation/data')

#####################
## load data

data.lists = list()

# exp 1 (first version)
data.lists[[1]] <- read.delim(file.path(dataDir, 'fall2018_version', 'psiturk_VOT_fall2018.txt'),
                      header = T, stringsAsFactors = F)
data.lists[[1]]$t_choice <- 1*(data.lists[[1]]$t.d_choice=='t')
data.lists[[1]]$experimentName <- 'Exp1_(original)'

# exp 2 ("category-to-category" version)
data.lists[[2]] <- read.delim(file.path(dataDir, 'summer2019_v2_version', 'psiturk_VOT_v2_summer2019.txt'),
                      header = T, stringsAsFactors = F)
data.lists[[2]]$experimentName <- 'Exp2_(category-mapping)'

# exp 3 (replication of exp 1)
data.lists[[3]] <- read.delim(file.path(dataDir, 'summer2019_v3_allConds', 'psiturk_VOT_v3_summer2019.txt'),
                   header = T, stringsAsFactors = F)
# data.lists[[3]] <- read.delim(file.path(dataDir, 'summer2019_v3_allConds', 'psiturk_VOT_v3_summer2019_FINAL-DATA.txt'),
#                    header = T, sep=';')
# this is important b/c this data file also includes Exp1 and 2 (because the data is extracted from the database in full)
exp3only <- unique(subset(data.lists[[3]], trialNum==1, select = c(uniqueid,experimentName)))
data.lists[[3]] <- subset(data.lists[[3]], uniqueid %in% exp3only$uniqueid[exp3only$experimentName=='Exp 3'])
#data.lists[[3]] <- subset(data.lists[[3]], experimentName == 'Exp 3')
data.lists[[3]]$experimentName <- 'Exp3_(replication)'

# merge together
data.raw <- bind_rows(data.lists[[1]], data.lists[[2]], data.lists[[3]])

length(unique(data.lists[[1]]$uniqueid))
length(unique(data.lists[[2]]$uniqueid))
length(unique(data.lists[[3]]$uniqueid))

data <- data.raw

##############
# remove debug/pilot subjects

# for exp 1 (in lab), a specific list of subjects
pilotSubjs <- c(
  'debug0OLLLX:debugR3SYV0',
  'debugVJUVHI:debugMAIUJ4',
  'debug1H9L8M:debugMYQLFA',
  'debugTS4AWY:debugTIWCZD',
  'debug1OR91U:debug6COCSK',
  'debugQ01B00:debugPSMT97',
  'debugBYQGS9:debugDRHNFG',
  'debugE1DIC8:debugH8218C' # weird condition assignment
)
data <- subset(data, !(uniqueid %in% pilotSubjs))

# our lab worker id
data <- subset(data, !grepl('A3B3O5I2LCNS6N',uniqueid)) # our lab worker id

# the rest
data <- subset(data, !(grepl('debug',uniqueid) & experimentName != 'Exp1_(original)'))

##############
## only the right conditions

data <- subset(data, condition %in% c(0,1,3,4))

# number subjects
tmp <- ddply(data,
             .(experimentName, uniqueid, condition), summarise,
             nTrials = sum(uniqueid==uniqueid))
ddply(tmp, .(experimentName, condition), summarise,
      nSubj = length(unique(uniqueid)))

##############
## add some data info

data$experimentPhase <- ''
data$experimentPhase[data$trial_type %in% c('Filler','Target')] <- 'training'
data$experimentPhase[data$trial_type %in% c('Test')] <- 'testing'
data$experimentPhase[data$hpCheck_didPass != ''] <- 'hpCheck'

# remove non-essential rows
data <- subset(data, experimentPhase != '')
data$experimentPhase <- factor(data$experimentPhase)

###############
## trim out non-complete datasets

# max trials
tmp.sum <- ddply(data, .(experimentName, experimentPhase), summarise,
             maxTrialNum = max(trialNum))
# FIX: this is the actual number
tmp.sum$maxTrialNum[tmp.sum$experimentPhase == 'training'] <- 142
tmp.sum$maxTrialNum[tmp.sum$experimentPhase == 'testing'] <- 162
tmp.sum$maxTrialNum[tmp.sum$experimentPhase == 'hpCheck'] <- 0
# each subject
tmp <- ddply(data, .(experimentName, experimentPhase, uniqueid), summarise,
             numTrials = sum(experimentPhase == experimentPhase))
tmp <- merge(tmp, tmp.sum, sort=F)
tmp$insuffTrials <- tmp$numTrials < tmp$maxTrialNum
# any phase insufficient
tmp.sum <- ddply(tmp, .(experimentName, uniqueid), summarise,
             anyInsufficient = sum(insuffTrials))

data <- subset(data, !(uniqueid %in% tmp.sum$uniqueid[tmp.sum$anyInsufficient == 1]))

# number subjects
tmp <- ddply(data,
             .(experimentName, uniqueid, condition), summarise,
             nTrials = sum(uniqueid==uniqueid))
ddply(tmp, .(experimentName, condition), summarise,
      nSubj = length(unique(uniqueid)))

############################
## confirm headphone check worked; should not have any subjects in here who did not pass the threshold (for the experiments that had it)

hpdata <- subset(data, hpCheck_didPass != '')
hpdata <- ddply(hpdata, .(experimentName, uniqueid, hpCheck_num_hp_check, hpCheck_correctThreshold), summarise,
                totalCorrect = max(hpCheck_totalCorrect),
                totalTrials = max(hpCheck_totalTrials))

# to exclude (subjects)
tmpHP <- ddply(hpdata, .(experimentName, uniqueid), summarise,
               minCorrect = min(totalCorrect),
               maxCorrect = max(totalCorrect),
               numAttempts = max(hpCheck_num_hp_check),
               maxCorrectUnderThreshold = 1*(maxCorrect < max(hpdata$hpCheck_correctThreshold)))

# exclusion list
excludeHP <- tmpHP$uniqueid[tmp$maxCorrectUnderThreshold == 1]

##############

# code condition
# 0: left ambig-t, 1: left ambig-d, 2: left control
# 3: right ambig-t, 4: right ambig-d, 5: right control

data$contextCondition <- 'L'
data$contextCondition[data$condition==3 | data$condition==4 | data$condition==5] <- 'R'
data$contextCondition <- factor(data$contextCondition)
data$ambigCondition <- 'ambig'
data$ambigCondition[data$condition==2 | data$condition==5] <- 'control'
data$ambigCondition <- factor(data$ambigCondition)
data$ambigPhoneme <- ''
data$ambigPhoneme[data$condition==0 | data$condition==3] <- 't'
data$ambigPhoneme[data$condition==1 | data$condition==4] <- 'd'
data$ambigPhoneme <- factor(data$ambigPhoneme)

data <- subset(data, condition != 'NA')

# number of trials and subjects per condition remaining
tmp <- ddply(data, .(experimentName, uniqueid, condition, ambigCondition, ambigPhoneme, contextCondition), summarise,
             nTrials = sum(uniqueid==uniqueid))
ddply(tmp, .(experimentName, condition, ambigCondition, ambigPhoneme, contextCondition), summarise,
      nSubj = length(unique(uniqueid)))

# double-check assignments
tmp <- subset(data, trial_type == 'Target')
unique(tmp[c('condition', 'phoneme_ambiguity', 'phoneme_type',
             'contextCondition', 'ambigCondition', 'ambigPhoneme')])

##############
## remove unneeded columns

data <- subset(data, select = c(VOT, accuracy, blockTrialNum, condition, index, phoneme_type,
                                resp_loc, response, rt, soundFile, t.d_choice, t_loc, trialNum, trial_match,
                                d_status, t_status, experimentName, uniqueid, trial_type,
                                phoneme_ambiguity,
                                experimentPhase, contextCondition, ambigCondition, ambigPhoneme))

##############
## factorize etc.

summary(data)

# pre-processing and add coding
data$uniqueid <- factor(data$uniqueid)
data$phoneme_type <- factor(data$phoneme_type)
data$phoneme_ambiguity <- factor(data$phoneme_ambiguity)
data$resp_loc <- factor(data$resp_loc)
data$index <- factor(data$index)
data$trial_match <- factor(data$trial_match)
data$t_loc <- factor(data$t_loc)
data$d_status <- factor(data$d_status)
data$experimentName <- factor(data$experimentName)
data$trial_type <- factor(data$trial_type)

data$t_choice <- 1*(data$t.d_choice=='t')
data$VOT_centered <- scale(data$VOT, center=T, scale=F)
data$blockTrialNum_centered <- scale(data$blockTrialNum, center=T, scale=F)

# segment of experiment
data$blockOrder <- ''
data$blockOrder[data$blockTrialNum <= 40] <- 'first_q'
data$blockOrder[data$blockTrialNum > 122] <- 'last_q'
data$blockOrder <- factor(data$blockOrder)

data$blockOrderHalf <- ''
data$blockOrderHalf[data$blockTrialNum <= 72] <- 'first_half'
data$blockOrderHalf[data$blockTrialNum > 72] <- 'last_half'
data$blockOrderHalf <- factor(data$blockOrderHalf)

summary(data)

##############

# filter so only training or test items
data.train <- subset(data, trial_type=='Target')
data.filler <- subset(data, trial_type=='Filler')
data.test <- subset(data, trial_type=='Test')

# block number
  # there were 9 test blocks, so number these 1 to 9
data.test$blockNum <- ceil(data.test$blockTrialNum / (length(unique(data.test$VOT)) * length(unique(data.test$index))))
# center it
data.test$blockNum_centered <- scale(data.test$blockNum, center=T, scale=F)

############################
## training performance
  # exclude subjects performing low on:
    # Ambig Targets or Unambig Targets
    # consistent or inconsistent Fillers
accThresh_targets <- 0.80
accThresh_fillers <- 0.80

train.acc <- ddply(data.train, .(experimentName, uniqueid, ambigCondition, ambigPhoneme, contextCondition,
                                 phoneme_ambiguity, phoneme_type), summarise,
                   meanAcc = mean(accuracy))
filler.acc <- ddply(data.filler, .(experimentName, uniqueid, ambigCondition, ambigPhoneme, contextCondition,
                                   trial_match), summarise,
                   meanAcc = mean(accuracy))

# reshape
train.acc.wide <- reshape(train.acc, idvar=c('experimentName', 'uniqueid', 'ambigCondition', 'ambigPhoneme', 'contextCondition'),
                          timevar=c('phoneme_ambiguity'), direction='wide')
filler.acc.wide <- reshape(filler.acc, idvar=c('experimentName', 'uniqueid', 'ambigCondition', 'ambigPhoneme', 'contextCondition'),
                           timevar=c('trial_match'), direction='wide')

# calculate exclusions
train.acc.wide$excludeAcc.targets <- 1*(train.acc.wide$meanAcc.orig < accThresh_targets | train.acc.wide$meanAcc.ambig < accThresh_targets)
filler.acc.wide$excludeAcc.fillers <- 1*(filler.acc.wide$meanAcc.consistent < accThresh_fillers | filler.acc.wide$meanAcc.inconsistent < accThresh_fillers)

# summary overall performance BEFORE exclusion
ddply(train.acc.wide, .(experimentName, ambigCondition, ambigPhoneme, contextCondition), summarise,
      meanMeanAcc.orig = mean(meanAcc.orig),
      meanMeanAcc.ambig = mean(meanAcc.ambig),
      nExclude = sum(excludeAcc.targets))
ddply(filler.acc.wide, .(experimentName, ambigCondition, ambigPhoneme, contextCondition), summarise,
      meanMeanAcc.consistent = mean(meanAcc.consistent),
      meanMeanAcc.inconsistent = mean(meanAcc.inconsistent),
      nExclude = sum(excludeAcc.fillers))

# make the exclusion list
subjExclList <- merge(subset(train.acc.wide, select = c(experimentName, uniqueid, excludeAcc.targets)),
                      subset(filler.acc.wide, select = c(experimentName, uniqueid, excludeAcc.fillers)),
                      sort=F)

############################
## 25% RTs < 100ms in any trial type?

tmp <- ddply(data, .(uniqueid, trial_type), summarise,
             mean.lowRT = mean(rt < 150),
             exclude.lowRT = 1*(mean.lowRT > 0.5)
)
tmp.wide <- reshape(tmp, idvar='uniqueid', timevar='trial_type', direction='wide')
tmp.wide$exclude.lowRT <- 1*(tmp.wide$exclude.lowRT.Filler | tmp.wide$exclude.lowRT.Target | tmp.wide$exclude.lowRT.Test)
subjExclList <- merge(subjExclList, subset(tmp.wide, select = c(uniqueid, exclude.lowRT)), sort=F)

############################
## 3 lowest VOTs not sig diff from 3 highest VOTs?

tmp <- ddply(data.test, .(uniqueid), summarise,
             mean.t_choice.lowVOT = mean(t_choice[VOT <= 40]),
             sd.t_choice.lowVOT = sd(t_choice[VOT <= 40]),
             n.t_choice.lowVOT = length(t_choice[VOT <= 40]),
             mean.t_choice.highVOT = mean(t_choice[VOT >= 60]),
             sd.t_choice.highVOT = sd(t_choice[VOT >= 60]),
             n.t_choice.highVOT = length(t_choice[VOT >= 60])
)
tmp$t <- (tmp$mean.t_choice.highVOT - tmp$mean.t_choice.lowVOT) / ( mean(c(tmp$sd.t_choice.lowVOT,tmp$sd.t_choice.highVOT)) / sqrt(tmp$n.t_choice.lowVOT) )
tmp$p <- 2*pt(tmp$t, tmp$n.t_choice.lowVOT, lower=FALSE)

tmp$exclude.noPsychometric <- 1*(tmp$t < 0 | tmp$p > .05)

subjExclList <- merge(subjExclList, subset(tmp, select = c(uniqueid, exclude.noPsychometric)), sort=F)

############################
## new criteria (not in pre-reg): <n% or >(1-n)% at first and last vot levels?
  # this may hurt our ability to detect the effect! so consider that in the analyses

cutoffPercentage <- 0.15
tmp <- ddply(data.test, .(uniqueid), summarise,
             mean.lowestVOT = mean(t_choice[VOT == min(data.test$VOT)]),
             mean.highestVOT = mean(t_choice[VOT == max(data.test$VOT)])
)
tmp$excludePntge.aboveFloor <- 1*(tmp$mean.lowestVOT > cutoffPercentage)
tmp$excludePntge.belowCeil <- 1*(tmp$mean.highestVOT < (1 - cutoffPercentage))
tmp$excludePntge.either <- 1*(tmp$excludePntge.aboveFloor | tmp$excludePntge.belowCeil)
tmp$excludePntge.both <- 1*(tmp$excludePntge.aboveFloor & tmp$excludePntge.belowCeil)

subjExclList <- merge(subjExclList, subset(tmp,
                      select = c(uniqueid, excludePntge.both, excludePntge.either, excludePntge.aboveFloor, excludePntge.belowCeil)),
                      sort=F)

############################
## exclude

# add condition back in
subjExclList <- merge(subjExclList,
                      subset(data, trialNum==1,
                             select = c(uniqueid, ambigCondition, ambigPhoneme, contextCondition)),
                      all.x=T, sort=F)

# list them
  # excludePntge.either: this is the "cutoff" we may not want
ddply(subjExclList, .(experimentName, ambigPhoneme, contextCondition), summarise,
      excludeAcc.targets = sum(excludeAcc.targets),
      excludeAcc.fillers = sum(excludeAcc.fillers),
      exclude.lowRT = sum(exclude.lowRT),
      exclude.noPsychometric = sum(exclude.noPsychometric),
      excludePntge.either = sum(excludePntge.either)
)

# columns for exclusion
subjExclList$exclude.anyPrereg <- 1*(
  subjExclList$excludeAcc.targets == 1 |
  subjExclList$excludeAcc.fillers == 1 |
  subjExclList$exclude.lowRT |
  subjExclList$exclude.noPsychometric
)
names(subjExclList)[names(subjExclList) == 'excludePntge.either'] <- 'excludePntge.prereg.2.3'

summary(subjExclList)


# merge back into the data file
data.final <- merge(data, subset(subjExclList, select = c(uniqueid,
                                                          excludeAcc.targets, excludeAcc.fillers, exclude.lowRT, exclude.noPsychometric,
                                                          exclude.anyPrereg, excludePntge.prereg.2.3)),
                         sort=F)

# number of subjects included
  # with and without the extra % cutoff criterion
ddply(subset(data.final),
      .(experimentName, ambigCondition, ambigPhoneme, contextCondition), summarise,
      nSubj.beforeExcl = length(unique(uniqueid)),
      nSubj.anyPrereg = length(unique(uniqueid[exclude.anyPrereg == 0])),
      nDiff = nSubj.beforeExcl - nSubj.anyPrereg,
      nSubj.alsoCutoff = length(unique(uniqueid[exclude.anyPrereg == 0 & excludePntge.prereg.2.3 == 0])),
      nDiff.alsoCutoff = nSubj.anyPrereg - nSubj.alsoCutoff)

#########
# add block number

# test only
tmp_test <- subset(data.final, trial_type == 'Test')
tmp_test$blockNum <- ceil(tmp_test$blockTrialNum / (length(unique(tmp_test$VOT)) * length(unique(tmp_test$index))))
# center it
tmp_test$blockNum_centered <- scale(tmp_test$blockNum, center=T, scale=F)

# non-test
tmp_notTest <- subset(data.final, trial_type != 'Test')

# merge
data.final.final <- bind_rows(tmp_test, tmp_notTest)

#########
## export csv

write.table(data.final.final,
            file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_preprocessed_data.txt'),
            row.names = FALSE,
            quote = FALSE,
            sep = '\t')
