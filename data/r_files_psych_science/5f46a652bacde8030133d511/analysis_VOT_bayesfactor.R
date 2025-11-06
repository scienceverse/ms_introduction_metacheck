if (!suppressMessages(require(lme4))) {install.packages("lme4"); require(lme4)}
if (!suppressMessages(require(brms))) {install.packages("brms"); require(brms)}
if (!suppressMessages(require(plyr))) {install.packages("plyr"); require(plyr)}
library(plyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(lme4)
library(lattice)
library(quickpsy)
library(pwr)

args = commandArgs(trailingOnly=TRUE)
if (length(args)<5) {
  stop("Need to supply two arguments: num iterations to run for BRMS, and currMachine name.n", call.=FALSE)
  nIter <- 1000
  expNum <- 1
  timingCondition <- 'Text-Before'
  delta_arg <- 2
  delta_param <- 0.95
  curr_machine <- 'SpencerLaptopWindows'
}
nIter <- args[1]
expNum <- args[2]
timingCondition <- args[3]
delta_arg <- args[4]
curr_machine <- args[5]

###################
## COLUMN EXPLANATION

# uniqueid: subject number
# index: which of two test exemplars
# ambigPhoneme: 'expanded' phoneme (t or d)
# contextCondition: L or R (text-before and text-after)
# VOT, VOT_centered: VOT (plus its centered version)
# blockNum, blockNum_centered: 1 to 9 (the block of stimuli in the test phase; 1 is beginning of test phase, 9 is end); _centered is centered version
# blockOrderHalf: portion of the study (first or last half)
# t_choice: chose T in that trial at the test phase (not D)

# Note contrasts:
  # ambigPhoneme:
  # d    1
  # t   -1
  # contextCondition:
  # L    1
  # R   -1


###################
## set up directories and load data

# prefix for each OS/user
  # insert your Dropbox base directory here; comment out the others
curr_machine = "SpencerDesktopLinux"

if (curr_machine == "Spencer") {
  os.prefix <- 'D:\\SpencerExternal\\Dropbox'
  dataDir <- file.path(os.prefix, '\\VOT_adaptation\\data', fsep='')
} else if (curr_machine == "SpencerLaptop") {
  os.prefix <- '/home/scaplan/Dropbox'
  dataDir <- file.path(os.prefix, '/VOT_adaptation/data', fsep='')
} else if (curr_machine == "SpencerLaptopWindows") {
  os.prefix <- 'C:\\Users\\Spencer\\Dropbox'
  dataDir <- file.path(os.prefix, '\\VOT_adaptation\\data', fsep='')
} else if (curr_machine == "SpencerDesktopLinux") {
  os.prefix <- '/home/spencer/Dropbox'
  dataDir <- file.path(os.prefix, '/VOT_adaptation/data', fsep='')
} else {
  os.prefix <- '/Users/ahafri1/Dropbox/GRAD_SCHOOL/DATED_DOCS/2017_FALL/PROJECTS'
  dataDir <- file.path(os.prefix, 'VOT_adaptation/data')
}


#####################
## load data
data.raw <- read.delim(file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_preprocessed_data.txt'),
                       header = T)


#####################
#####################
# choose included subjects
# subjList.incl.anyPrereg is all criteria EXCEPT the far from ceiling/floor cutoff
# subjList.incl.anyPrereg.plus.cutoff is all, INCLUDING that cutoff

subjExclList <- ddply(data.raw, .(experimentName, uniqueid), summarise,
                      excludeAcc.targets = max(excludeAcc.targets),
                      excludeAcc.fillers = max(excludeAcc.fillers),
                      exclude.lowRT = max(exclude.lowRT),
                      exclude.noPsychometric = max(exclude.noPsychometric),
                      exclude.anyPrereg = max(exclude.anyPrereg),
                      excludePntge.prereg.2.3 = max(excludePntge.prereg.2.3)
)

# list of subjects excluded in each
subjList.incl.anyPrereg <- subjExclList$uniqueid[subjExclList$exclude.anyPrereg == 0]
subjList.incl.anyPrereg.plus.cutoff <- subjExclList$uniqueid[subjExclList$exclude.anyPrereg == 0 & subjExclList$excludePntge.prereg.2.3 == 0]

####################
## data subsets and pre-processing
# and set up contrasts

data.train.final <- subset(data.raw, trial_type %in% c('Target'))
data.filler.final <- subset(data.raw, trial_type %in% c('Filler'))
data.test.final <- subset(data.raw, trial_type %in% c('Test'))

# VOT column as factor also
data.test.final$VOT_f <- as.factor(data.test.final$VOT)

# index as a FACTOR ("oh dear...!")
data.test.final$index <- as.factor(data.test.final$index)

# set up contrasts (for test data)
# ambigPhoneme:
# d    1
# t   -1
# contextCondition:
# L    1
# R   -1
# blockOrderHalf:
# first_half    -1
# last_half      1
data.test.final$ambigPhoneme <- factor(data.test.final$ambigPhoneme)
data.test.final$contextCondition <- factor(data.test.final$contextCondition)
data.test.final$blockOrderHalf <- factor(data.test.final$blockOrderHalf)

contrasts(data.test.final$ambigPhoneme) <- contr.sum(levels(data.test.final$ambigPhoneme))
contrasts(data.test.final$contextCondition) <- contr.sum(levels(data.test.final$contextCondition))
contrasts(data.test.final$blockOrderHalf) <- rev(contr.sum(levels(data.test.final$blockOrderHalf)))



######################
######################
## model comparison
# choices

if (expNum == 1) {
  currExp = 'Exp1_(original)'
} else if (expNum == 2) {
  currExp = 'Exp3_(replication)'
} else {
  stop("Bad experiment number argument.n", call.=FALSE)
}

if (timingCondition == "Text-Before") {
  contextConditionSetting = 'L'
  contextString = '_textBefore_'
} else if (timingCondition == "Text-After") {
  contextConditionSetting = 'R'
  contextString = '_textAfter_'
} else {
  stop("Bad context condition argument.n", call.=FALSE)
}

if (delta_arg == 1) {
  delta_param = 0.999
} else if (delta_arg == 2) {
  delta_param = 0.99
} else {
  stop("Bad delta setting.n", call.=FALSE)
}


ceilFloorCutoffs <- c(TRUE, FALSE)
halves <- c('first_half', 'last_half')
contextConditions <- c('L', 'R')

############################
############################
## brms

curCutoff <- ceilFloorCutoffs[2] # should always be [2] for the main manuscript, i.e. not using the pre-reg ceil/floor cutoff
curCutoff_s <- as.character(curCutoff)
curHalf <- halves[1]    # first half only [1] (no need to test last half)

# get subjects of data we need (the particular exclusions)
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}



############################
############################
## Power Analyses

data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
curExp <- exps[1]    # in the paper, is Exp 1 is [1], Exp 2 is [3]
full_lmer.exp1 <- glmer(t_choice ~ 1 + VOT_centered + ambigPhoneme * contextCondition * blockOrderHalf +
                          (1 | uniqueid) + (1 | index),
                        data = subset(data.test.cur, experimentName == curExp),
                        control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")),
                        family = binomial(link = 'logit'))
summary(full_lmer.exp1)


curExp <- exps[1]    # in the paper, is Exp 1 is [1], Exp 2 is [3]
curHalf <- halves[1]    # first half only [1] (no need to test last half)
curContext <- contextConditions[1]    # c('L', 'R')
half_lmer_c.exp1.first.L <- glmer(t_choice ~ 1 + VOT_centered + ambigPhoneme + 
                                    (0 + VOT_centered | uniqueid) + (1 | uniqueid) + (1 | index),
                                  data = subset(data.test.cur, experimentName == curExp & blockOrderHalf == curHalf & 
                                                  contextCondition == curContext),
                                  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")),
                                  family = binomial(link = 'logit'))
summary(half_lmer_c.exp1.first.L)

library(simr)
powerSim(half_lmer_c.exp1.first.L, test = "ambigPhoneme1")


powerSim(full_lmer.exp1, test = "ambigPhoneme1")


data.test.final <- read.table(
  paste(dataDir,'/summer2019_v3_version/psiturk_VOT_v3_summer2019_FINAL-DATA.txt', sep='/'),
  sep = ';', 
  header = T)


data.test.exp1 <- subset(data.test.cur, experimentName == "Exp1_(original)" & contextCondition == "L")

fit <- quickpsy(subset(data.test.exp1), 
                x = VOT, k = t_choice,
                grouping = .(uniqueid, ambigPhoneme, contextCondition, blockOrderHalf),
                bootstrap = 'none')
a2 <- fit$thresholds

tmpdata <- subset(a2, blockOrderHalf=='first_half')
meandiff <- mean(tmpdata$thre[tmpdata$ambigPhoneme == 't']) - mean(tmpdata$thre[tmpdata$ambigPhoneme == 'd'])
sddiff <- sqrt( (var(tmpdata$thre[tmpdata$ambigPhoneme == 't']) + var(tmpdata$thre[tmpdata$ambigPhoneme == 'd']) ) /2)
cohens.d <- meandiff / sddiff
pwr.t.test(n = NULL, d = cohens.d, sig.level = .05, power = 0.8, type = 'two')
pwr.t.test(n = NULL, d = cohens.d, sig.level = .05, power = 0.9, type = 'two')

lm.0 <- lmer(thre ~ 1 + (1 | uniqueid) + (1 | index), data = a2)
lm.5 <- lmer(thre ~ 1 + ambigPhoneme + blockOrderHalf + (1 | uniqueid) + (1 | index), data = a2)


data.contr <- data.test.final

data.contr$ambigPhoneme <- factor(data.contr$ambigPhoneme)
data.contr$contextCondition <- factor(data.contr$contextCondition)

contrasts(data.contr$ambigPhoneme) <- contr.sum(levels(data.contr$ambigPhoneme))
data.contr$contextCondition <- factor(data.contr$contextCondition)
contrasts(data.contr$contextCondition) <- contr.sum(levels(data.contr$contextCondition))

lm.c.1.h <- glmer(t_choice ~ 1 + VOT_centered + ambigPhoneme + contextCondition + (1 | uniqueid) + (1 | index), 
                  data = subset(data.contr, blockOrderHalf == 'first_half'), 
                  family = binomial(link = 'logit'))
lm.c.2.h <- glmer(t_choice ~ 1 + VOT_centered + ambigPhoneme * contextCondition + (1 | uniqueid) + (1 | index), 
                  data = subset(data.contr, blockOrderHalf == 'first_half'), 
                  family = binomial(link = 'logit'))
# lm.c.1.h <- glmer(t_choice ~ 1 + VOT_centered + ambigPhoneme + contextCondition + (1 | uniqueid) + (1 | index), 
#                   data = subset(data.contr, blockOrderHalf == 'first_half'), 
#                   family = binomial(link = 'probit'))
# lm.c.2.h <- glmer(t_choice ~ 1 + VOT_centered + ambigPhoneme * contextCondition + (1 | uniqueid) + (1 | index), 
#                   data = subset(data.contr, blockOrderHalf == 'first_half'), 
#                   family = binomial(link = 'probit'))
anova(lm.c.1.h, lm.c.2.h)
summary(lm.c.1.h)
summary(lm.c.2.h)

###############
###############
# BRMS Bayes factors

# may want to (or need to?) run the model with sample_priors="only"? see below

output_param_name = paste0("brms_Exp", expNum, contextString, nIter, "iterations_", delta_param,"delta.txt")
sink(output_param_name)

###
full_brms_bernoulli = brm(t_choice ~ 1 + VOT_centered + ambigPhoneme + 
          (0 + VOT_centered | uniqueid) + (1 | uniqueid) + (1 | index), 
          data = subset(data.test.cur, experimentName == currExp & blockOrderHalf == curHalf & contextCondition == contextConditionSetting),
          #prior = priors_mixed, # if we wanted to set our own priors instead of use the defaults
          # control = list(adapt_delta = delta_param),
          control = list(adapt_delta = delta_param, max_treedepth = 15),
          family = bernoulli,
          save_all_pars = TRUE,
          iter = nIter,
          cores = getOption("mc.cores", 4L))

null_brms_bernoulli = brm(t_choice ~ 1 + VOT_centered + 
          (0 + VOT_centered | uniqueid) + (1 | uniqueid) + (1 | index), 
          data = subset(data.test.cur, experimentName == currExp & blockOrderHalf == curHalf & contextCondition == contextConditionSetting),
          #prior = priors_mixed, # if we wanted to set our own priors instead of use the defaults
          # control = list(adapt_delta = delta_param),
          control = list(adapt_delta = delta_param, max_treedepth = 15),
          family = bernoulli,
          save_all_pars = TRUE, 
          iter = nIter,
          cores = getOption("mc.cores", 4L))

BF_brms_bridge = bayes_factor(full_brms_bernoulli, null_brms_bernoulli)

# For printing model output to sink
full_brms_bernoulli
null_brms_bernoulli
BF_brms_bridge
sink()



##########
## NOTES on selecting priors:
# https://discourse.mc-stan.org/t/setting-priors-for-categorical-models/14921
#
# Welcome to the community!
#   
#   I would run the model with sample_priors="only" and then check how the combination of all priors and 
#       the likelihood looks like using pp_check(myModel, type="hist").
#       That way you will be able to see how the priors look like on the outcome scale.
#       I always find it quite hard to reason about priors and, hence, opt for plotting as much as possible.
#       If you get your priors for your model (maybe you need to add the data also),
# 
# p <- get_prior(y~x+z, family=categorical, sample_priors="only")
# you can then set priors, e.g.,
# 
# p$prior[1] <- "normal(0,5)"
# and then rerun the sampling (where you only sample from the priors). Don’t forget to include prior = p in the brm call.

