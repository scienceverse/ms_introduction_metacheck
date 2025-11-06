library(plyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(lme4)

###################
## exclusion criteria notes

## include all pre-regg'ed criteria AND the ceil/floor cutoff? or all except the ceil/floor cutoff?
# it was pre-regg'ed in Exps 2 and 3...
# but afterward, we suspected it could hurt chances to show the effect precisely because those people excluded might be the ones showing the effect

# pre-reg criteria:
# exclude.anyPrereg --> participants meets any of the below (does NOT do excludePntge.prereg.2.3)
# excludeAcc.targets --> exclude for low acc on targets (training)
# excludeAcc.fillers --> exclude for low acc on fillers (training)
# exclude.lowRT --> exclude for too many low RTs
# exclude.noPsychometric --> exclude if high VOTs are not significantly different from low VOTs (testing)
# excludePntge.prereg.2.3 --> exclude if lowest/highest VOT values are far from floor/ceiling (see explanation above)

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

# directories
dataDir <- file.path(os.prefix, 'VOT_adaptation/data')

#####################
## load data

# exp 1 (first version)
data.raw <- read.delim(file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_preprocessed_data.txt'),
                       header = T)

#####################
# number of subjects included
# with and without the extra % cutoff criterion
ddply(subset(data.raw),
      .(experimentName, ambigCondition, ambigPhoneme, contextCondition), summarise,
      nSubj.beforeExcl = length(unique(uniqueid)),
      nSubj.anyPrereg = length(unique(uniqueid[exclude.anyPrereg == 0])),
      nDiff = nSubj.beforeExcl - nSubj.anyPrereg,
      nSubj.cutoff = length(unique(uniqueid[exclude.anyPrereg == 0 & excludePntge.prereg.2.3 == 0])),
      nDiff.cutoff = nSubj.anyPrereg - nSubj.cutoff)

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
contrasts(data.test.final$ambigPhoneme) <- contr.sum(levels(data.test.final$ambigPhoneme))
contrasts(data.test.final$contextCondition) <- contr.sum(levels(data.test.final$contextCondition))

contrasts(data.test.final$blockOrderHalf) <- rev(contr.sum(levels(data.test.final$blockOrderHalf)))




########################
########################
## modeling on trial-level data


######################
######################
## fixed effects

# store all here
mlist <- list()

# vars to loop through
ceilFloorCutoffs <- c(FALSE)
contexts <- c('L','R')
#ceilFloorCutoffs <- c(TRUE, FALSE)
exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
halves <- c('first_half', 'last_half')

# note about "cutoffs":
#ceilFloorCutoff <- TRUE   # do all pre-reg criteria AND the ceil/floor cutoff (INCLUDING for Exp 1, where it was NOT pre-registered)
#ceilFloorCutoff <- FALSE   # do all pre-reg criteria EXCEPT the ceil/floor cutoff (even though it WAS pre-registered in Exps 2 and 3)

curCutoff <- ceilFloorCutoffs[1]
curCutoff_s <- as.character(curCutoff)

#############################
# set up data
# subset the group
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}

# loop through data sets...
for (iiCx in c(1:length(contexts))) {
  print(paste('Context',iiCx,'of',length(contexts),'...'))

  curContext <- contexts[iiCx]

  mlist[[curContext]] <- list()

  data.test.cur <- subset(data.test.final,
                          uniqueid %in% curSubjects &
                            contextCondition == curContext)


  #############################
  # loop through experiments...
  for (iiE in c(1:length(exps))) {
    curExp <- exps[iiE]

    print(paste('Experiment',iiE,'of',length(exps),'...'))

    mlist[[curContext]][[curExp]] <- list()
    mlist[[curContext]][[curExp]][['full']] <- list()

    #############################
    # run the full models

    mlist[[curContext]][[curExp]][['full']][[1]] <- glmer(t_choice ~ 1 + blockOrderHalf + VOT_centered +
                                                             (1 | uniqueid) +
                                                             (1 | index),
                                                           data = subset(data.test.cur, experimentName == curExp),
                                                           family = binomial(link = 'logit'),
                                                           control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))

    mlist[[curContext]][[curExp]][['full']][[2]] <- update(mlist[[curContext]][[curExp]][['full']][[1]],
                                                            . ~ . +
                                                              ambigPhoneme)

    mlist[[curContext]][[curExp]][['full']][[3]] <- update(mlist[[curContext]][[curExp]][['full']][[1]],
                                                            . ~ . +
                                                              ambigPhoneme * blockOrderHalf)

    #############################
    # then loop through experiment halves

    mlist[[curContext]][[curExp]][['halves']] <- list()
    mlist[[curContext]][[curExp]][['halves_simple']] <- list()

    for (iiH in c(1:length(halves))) {
      curHalf <- halves[iiH]

      print(paste('Half',iiH,'of',length(halves),'...'))

      ## this has a slightly more complex ranef structure (uncorr int/slope of VOT_centered, by participant)
      mlist[[curContext]][[curExp]][['halves']][[curHalf]] <- list()

      mlist[[curContext]][[curExp]][['halves']][[curHalf]][[1]] <- glmer(t_choice ~ 1 + VOT_centered +
                                                                            (1 | uniqueid) +
                                                                            (0 + VOT_centered | uniqueid) +
                                                                            (1 | index),
                                                                          data = subset(data.test.cur,
                                                                                        experimentName == curExp &
                                                                                          blockOrderHalf == curHalf),
                                                                          family = binomial(link = 'logit'),
                                                                         control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))

      mlist[[curContext]][[curExp]][['halves']][[curHalf]][[2]] <- update(mlist[[curContext]][[curExp]][['halves']][[curHalf]][[1]],
                                                                           . ~ . +
                                                                             ambigPhoneme)

      mlist[[curContext]][[curExp]][['halves']][[curHalf]][[3]] <- update(mlist[[curContext]][[curExp]][['halves']][[curHalf]][[1]],
                                                                           . ~ . +
                                                                             ambigPhoneme * VOT_centered)

      ## simpler (intercept-only)
      mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]] <- list()

      mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[1]] <- glmer(t_choice ~ 1 + VOT_centered +
                                                                                   (1 | uniqueid) +
                                                                                   (1 | index),
                                                                                 data = subset(data.test.cur,
                                                                                               experimentName == curExp &
                                                                                                 blockOrderHalf == curHalf),
                                                                                 family = binomial(link = 'logit'),
                                                                                control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))

      mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[2]] <- update(mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[1]],
                                                                                  . ~ . +
                                                                                    ambigPhoneme)

      mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[3]] <- update(mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[1]],
                                                                                  . ~ . +
                                                                                    ambigPhoneme * VOT_centered)

    }
  }
}

print('Finished!')

##################
## save

save.image(file=file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_lmer_indConds_models.RData'))




##################
## test


load(file=file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_lmer_indConds_models.RData'))

##################

contexts <- c('L','R')
exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
halves <- c('first_half', 'last_half')

curContext <- contexts[2]
curExp <- exps[3]

for (ii in c(1:3)) {
  print('###################')
  print('###################')
  print( mlist[[curContext]][[curExp]][['full']][[ii]]@optinfo$conv$lme4$code )
}

anova(
  mlist[[curContext]][[curExp]][['full']][[1]],
  mlist[[curContext]][[curExp]][['full']][[2]],
  mlist[[curContext]][[curExp]][['full']][[3]]
)

anova(
  mlist[[curContext]][[curExp]][['full']][[1]],
  # mlist[[curContext]][[curExp]][['full']][[2]],
  mlist[[curContext]][[curExp]][['full']][[3]]
)

##################
## test half

curExp <- exps[2]
curContext <- contexts[2]
curHalf <- halves[1]

for (ii in c(1:3)) {
  print('###################')
  print('###################')
  print( mlist[[curContext]][[curExp]][['halves']][[curHalf]][[ii]]@optinfo$conv$lme4$code )
}

anova(
  mlist[[curContext]][[curExp]][['halves']][[curHalf]][[1]],
  mlist[[curContext]][[curExp]][['halves']][[curHalf]][[2]],
  mlist[[curContext]][[curExp]][['halves']][[curHalf]][[3]]
)

anova(
  mlist[[curContext]][[curExp]][['halves']][[curHalf]][[1]],
#  mlist[[curContext]][[curExp]][['halves']][[curHalf]][[2]],
  mlist[[curContext]][[curExp]][['halves']][[curHalf]][[3]]
)

# simple

for (ii in c(1:3)) {
  print('###################')
  print('###################')
  print( mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[ii]]@optinfo$conv$lme4$code )
}

anova(
  mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[1]],
  mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[2]],
  mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[3]]
)

# simple
anova(
  mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[1]],
  # mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[2]],
  mlist[[curContext]][[curExp]][['halves_simple']][[curHalf]][[3]]
)
