library(plyr)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(lme4)
library(lattice)
#library(quickpsy)
#library(pwr)
#library(pwr2)
#library(simr)

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

#load(file=file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_lmer_models.RData'))
load(file=file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_lmer_models_fixedNonConvergences.RData'))

#####################
# number of subjects included
# with and without the extra % cutoff criterion

tmp <- ddply(subset(data.raw),
      .(experimentName, ambigPhoneme, contextCondition), summarise,
      nSubj.beforeExcl = length(unique(uniqueid)),
      nSubj.anyPrereg = length(unique(uniqueid[exclude.anyPrereg == 0])),
      nDiff = nSubj.beforeExcl - nSubj.anyPrereg,
      nSubj.cutoff = length(unique(uniqueid[exclude.anyPrereg == 0 & excludePntge.prereg.2.3 == 0])),
      nDiff.cutoff = nSubj.anyPrereg - nSubj.cutoff)
tmp$propExcl.anyPrereg <- 1 - tmp$nSubj.anyPrereg/tmp$nSubj.beforeExcl
tmp$propExcl.cutoff <- 1 - tmp$nSubj.cutoff/tmp$nSubj.beforeExcl
tmp

tmp <- ddply(subset(data.raw),
             .(experimentName, contextCondition), summarise,
             nSubj.beforeExcl = length(unique(uniqueid)),
             nSubj.anyPrereg = length(unique(uniqueid[exclude.anyPrereg == 0])),
             nDiff = nSubj.beforeExcl - nSubj.anyPrereg,
             nSubj.cutoff = length(unique(uniqueid[exclude.anyPrereg == 0 & excludePntge.prereg.2.3 == 0])),
             nDiff.cutoff = nSubj.anyPrereg - nSubj.cutoff)
tmp$propExcl.anyPrereg <- 1 - tmp$nSubj.anyPrereg/tmp$nSubj.beforeExcl
tmp$propExcl.cutoff <- 1 - tmp$nSubj.cutoff/tmp$nSubj.beforeExcl
tmp



curExp <- 'Exp1_(original)'
curExp <- 'Exp3_(replication)'
curExp <- 'Exp2_(category-mapping)'
sum(tmp$nSubj.beforeExcl[tmp$experimentName == curExp])
sum(tmp$nSubj.anyPrereg[tmp$experimentName == curExp])
sum(tmp$nSubj.cutoff[tmp$experimentName == curExp])


######################
######################
## test results

curCutoff <- ceilFloorCutoffs[1]
curCutoff_s <- as.character(curCutoff)

valsTrain[curCutoff_s]

######################
######################
## model comparison



ceilFloorCutoffs <- c(TRUE, FALSE)
exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
halves <- c('first_half', 'last_half')

# note about "cutoffs":
#ceilFloorCutoff <- TRUE   # do all pre-reg criteria AND the ceil/floor cutoff (INCLUDING for Exp 1, where it was NOT pre-registered)
#ceilFloorCutoff <- FALSE   # do all pre-reg criteria EXCEPT the ceil/floor cutoff (even though it WAS pre-registered in Exps 2 and 3)



############################
############################
## model comparison

# set up a "backup" mlist for manually run models
mlist2 <- mlist

############################
## full models

# ceilFloorCutoffs <- c(TRUE, FALSE)
# exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
# halves <- c('first_half', 'last_half')

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[2]

for (ii in c(1:4)) {
  print('###################')
  print('###################')
  print( mlist2[[curCutoff_s]][[curExp]][['full']][[ii]]@optinfo$conv$lme4$code )
  #print( summary(mlist[[curCutoff_s]][[curExp]][['full']][[ii]], cor=F) )
}

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

mlist2[[curCutoff_s]][[curExp]][['full']][[4]] <- update(
              mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
              control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[4]]@optinfo$conv$lme4$code )

# this is needed for exp 2 (where there is no interaction with ambigPhoneme and contextCondition)
mlist2[[curCutoff_s]][[curExp]][['full']][[5]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  . ~ . + blockOrderHalf * ambigPhoneme + blockOrderHalf * contextCondition,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[5]]@optinfo$conv$lme4$code )

# also:
mlist2[[curCutoff_s]][[curExp]][['full']][[6]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  . ~ . + blockOrderHalf * ambigPhoneme + blockOrderHalf * contextCondition,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[6]]@optinfo$conv$lme4$code )

mlist2[[curCutoff_s]][[curExp]][['full']][[7]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  . ~ . + blockOrderHalf * ambigPhoneme,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[7]]@optinfo$conv$lme4$code )

mlist2[[curCutoff_s]][[curExp]][['full']][[8]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  . ~ . + ambigPhoneme,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[8]]@optinfo$conv$lme4$code )

mlist2[[curCutoff_s]][[curExp]][['full']][[9]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  . ~ . + ambigPhoneme * blockOrderHalf,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[9]]@optinfo$conv$lme4$code )


anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[7]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[8]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[6]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[7]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[8]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  # mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  # mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[6]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[7]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[8]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)

summary(mlist2[[curCutoff_s]][[curExp]][['full']][[9]])

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[8]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[8]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[7]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)
anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[9]]
)



anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]]
)


anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[5]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]]
)
anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]]
)
anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[6]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]]
)
anova(
  mlist2[[curCutoff_s]][[curExp]][['full']][[5]],
  mlist2[[curCutoff_s]][[curExp]][['full']][[4]]
)



summary(mlist2[[curCutoff_s]][[curExp]][['full']][[3]])
summary(mlist2[[curCutoff_s]][[curExp]][['full']][[4]])
summary(mlist2[[curCutoff_s]][[curExp]][['full']][[5]])
summary(mlist2[[curCutoff_s]][[curExp]][['full']][[6]])





############################
## simpler full data ranef model

# ceilFloorCutoffs <- c(TRUE, FALSE)
# exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
# halves <- c('first_half', 'last_half')

curCutoff <- ceilFloorCutoffs[1]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[3]

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

tmp <- list()
tmp[[1]] <- glmer(t_choice ~ 1 + blockOrderHalf + VOT_centered +
                    (1 | uniqueid),
                  data = subset(data.test.cur, experimentName == curExp),
                  family = binomial(link = 'logit'),
                  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
tmp[[2]] <- update(tmp[[1]],
                   . ~ . + ambigPhoneme + contextCondition +
                     blockOrderHalf:ambigPhoneme + blockOrderHalf:contextCondition)
tmp[[3]] <- update(tmp[[1]],
                   . ~ . + ambigPhoneme + contextCondition +
                     blockOrderHalf:ambigPhoneme + blockOrderHalf:contextCondition +
                     ambigPhoneme:contextCondition)
tmp[[4]] <- update(tmp[[1]],
                   . ~ . + ambigPhoneme + contextCondition +
                     blockOrderHalf:ambigPhoneme + blockOrderHalf:contextCondition +
                     ambigPhoneme:contextCondition +
                     blockOrderHalf:ambigPhoneme:contextCondition)
anova(
  tmp[[1]],
  tmp[[2]],
  tmp[[3]],
  tmp[[4]]
)

anova(
  tmp[[1]],
  tmp[[2]],
  #tmp[[3]],
  tmp[[4]]
)

anova(
  tmp[[1]],
  #tmp[[2]],
  tmp[[3]],
  tmp[[4]]
)

anova(
  tmp[[1]],
  #tmp[[2]],
  #tmp[[3]],
  tmp[[4]]
)

############################
## model halves: complex

# ceilFloorCutoffs <- c(TRUE, FALSE)
# exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
# halves <- c('first_half', 'last_half')

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[2]
curHalf <- halves[2]

for (ii in c(1:5)) {
  print('###################')
  print('###################')
  print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[ii]]@optinfo$conv$lme4$code )
  #print( summary(mlist[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[ii]], cor=F) )
}

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

# mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]] <- update(
#   mlist[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
#   control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
# mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]] <- update(
#   mlist[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]],
# control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
# mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]] <- update(
#   mlist[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
#   control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]] <- update(
  mlist[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]],
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]] <- update(
  mlist[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]],
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]]@optinfo$conv$lme4$code )
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]]@optinfo$conv$lme4$code )
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]]@optinfo$conv$lme4$code )
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]]@optinfo$conv$lme4$code )
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]]@optinfo$conv$lme4$code )

# add a model
mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]],
  . ~ . + ambigPhoneme:VOT_centered + contextCondition:VOT_centered,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]]@optinfo$conv$lme4$code )

mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
  . ~ . + ambigPhoneme,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]]@optinfo$conv$lme4$code )

mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[8]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
  . ~ . + ambigPhoneme * VOT_centered,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[8]]@optinfo$conv$lme4$code )



anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[8]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
  # mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]],
  # mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
  # mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]],
  # mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[8]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]],
  #mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
  #mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]],
  #mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]],
  # mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[8]]
)

  anova(
    mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
    mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
    mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]]
  )

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[8]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]]
)


anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]]
)


anova(
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]],
  #mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]],
  mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]]
)

summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[1]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[2]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[3]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[4]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[5]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[6]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves']][[curHalf]][[7]])


############################
## model halves: simple
  # in case above does not converge


# ceilFloorCutoffs <- c(TRUE, FALSE)
# exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
# halves <- c('first_half', 'last_half')

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[1]
curHalf <- halves[1]

for (ii in c(1:5)) {
  print('###################')
  print('###################')
  print( mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[ii]]@optinfo$conv$lme4$code )
  #print( summary(mlist[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[ii]], cor=F) )
}

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]] <- update(
  mlist[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]],
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]] <- update(
  mlist[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]],
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]]@optinfo$conv$lme4$code )
print( mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]]@optinfo$conv$lme4$code )

# add a model
mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[2]],
  . ~ . + ambigPhoneme:VOT_centered + contextCondition:VOT_centered,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]]@optinfo$conv$lme4$code )


anova(
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[1]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[2]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[3]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[1]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[2]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[3]],
  # mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]],
  # mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[1]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[2]],
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[3]],
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[1]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[2]],
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[3]],
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]]
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[1]],
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[2]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[3]],
  #mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]],
  mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]]
)



summary(mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[3]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[4]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[5]])
summary(mlist2[[curCutoff_s]][[curExp]][['halves_simple']][[curHalf]][[6]])





############################
############################
## SAVING NEW DATA

#rm(mlist)
save.image(file=file.path(dataDir, 'combined_data', 'Exp1_Exp2_Exp3_lmer_models_fixedNonConvergences.RData'))







############################
############################
## test effects sep for each phoneme




# ceilFloorCutoffs <- c(TRUE, FALSE)
# exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
# halves <- c('first_half', 'last_half')

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[2]
curHalf <- halves[1]

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

curPhoneme <- 't'
# curPhoneme <- 'd'

lm.0 <- list()
lm.0[[curPhoneme]] <- list()
lm.0[[curPhoneme]][[1]] <- glmer(t_choice ~ VOT_centered +
                (1 | uniqueid) + (1 | index),
              data = subset(data.test.cur, experimentName == curExp & blockOrderHalf == curHalf &
                              ambigPhoneme == curPhoneme),
              family = binomial(link = 'logit'),
              control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
lm.0[[curPhoneme]][[2]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + contextCondition)
lm.0[[curPhoneme]][[3]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + VOT_centered*contextCondition)

anova(
  lm.0[[curPhoneme]][[1]],
  lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)

anova(
  lm.0[[curPhoneme]][[1]],
  #lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)




# diff ranef



# ceilFloorCutoffs <- c(TRUE, FALSE)
# exps <- c('Exp1_(original)', 'Exp2_(category-mapping)', 'Exp3_(replication)')
# halves <- c('first_half', 'last_half')

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[3]
curHalf <- halves[1]

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

curPhoneme <- 't'
# curPhoneme <- 'd'

lm.0 <- list()
lm.0[[curPhoneme]] <- list()
lm.0[[curPhoneme]][[1]] <- glmer(t_choice ~ VOT_centered +
                                   (1 | uniqueid) +
                                   (0 + VOT_centered | uniqueid) +
                                   (1 | index),
                                 data = subset(data.test.cur,
                                                 experimentName == curExp &
                                                 blockOrderHalf == curHalf &
                                                 ambigPhoneme == curPhoneme),
                                 family = binomial(link = 'logit'),
                                 control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
lm.0[[curPhoneme]][[2]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + contextCondition)
lm.0[[curPhoneme]][[3]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + VOT_centered*contextCondition)

anova(
  lm.0[[curPhoneme]][[1]],
  lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)

anova(
  lm.0[[curPhoneme]][[1]],
  #lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)



#########
## no half

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[1]

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

curPhoneme <- 't'
# curPhoneme <- 'd'

lm.0 <- list()
lm.0[[curPhoneme]] <- list()
lm.0[[curPhoneme]][[1]] <- glmer(t_choice ~ VOT_centered +
                                   (1 | uniqueid) + (1 | index),
                                 data = subset(data.test.cur, experimentName == curExp &
                                                 ambigPhoneme == curPhoneme),
                                 family = binomial(link = 'logit'),
                                 control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
lm.0[[curPhoneme]][[2]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + contextCondition)
lm.0[[curPhoneme]][[3]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + VOT_centered*contextCondition)

anova(
  lm.0[[curPhoneme]][[1]],
  lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)

anova(
  lm.0[[curPhoneme]][[1]],
  #lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)

summary(lm.0[[curPhoneme]][[2]])




#########
## no half
#diff ranef

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[3]

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

curPhoneme <- 't'
# curPhoneme <- 'd'

lm.0 <- list()
lm.0[[curPhoneme]] <- list()
lm.0[[curPhoneme]][[1]] <- glmer(t_choice ~ VOT_centered +
                                   (1 | uniqueid) + (0 + VOT_centered | uniqueid) + (1 | index),
                                 data = subset(data.test.cur, experimentName == curExp &
                                                 ambigPhoneme == curPhoneme),
                                 family = binomial(link = 'logit'),
                                 control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
lm.0[[curPhoneme]][[2]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + contextCondition)
lm.0[[curPhoneme]][[3]] <- update(lm.0[[curPhoneme]][[1]],
                                  . ~ .  + VOT_centered*contextCondition)

anova(
  lm.0[[curPhoneme]][[1]],
  lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)

anova(
  lm.0[[curPhoneme]][[1]],
  #lm.0[[curPhoneme]][[2]],
  lm.0[[curPhoneme]][[3]]
)

summary(lm.0[[curPhoneme]][[2]])






########################
########################

curCutoff <- ceilFloorCutoffs[2]
curCutoff_s <- as.character(curCutoff)
curExp <- exps[3]

# fix models
if (curCutoff == FALSE) {
  curSubjects <- subjList.incl.anyPrereg
} else {
  curSubjects <- subjList.incl.anyPrereg.plus.cutoff
}
data.test.cur <- subset(data.test.final, uniqueid %in% curSubjects)

mlist2[[curCutoff_s]][[curExp]][['full']][[5]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[3]],
  . ~ . + blockOrderHalf * ambigPhoneme + blockOrderHalf * contextCondition,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))

mlist2[[curCutoff_s]][[curExp]][['full']][[6]] <- update(
  mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
  . ~ . + blockOrderHalf * ambigPhoneme + blockOrderHalf * contextCondition,
  control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
print( mlist2[[curCutoff_s]][[curExp]][['full']][[6]]@optinfo$conv$lme4$code )

# mlist2[[curCutoff_s]][[curExp]][['full']][[7]] <- update(
#   mlist2[[curCutoff_s]][[curExp]][['full']][[2]],
#   . ~ . + blockOrderHalf * ambigPhoneme,
#   control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
# print( mlist2[[curCutoff_s]][[curExp]][['full']][[7]]@optinfo$conv$lme4$code )
#
# mlist2[[curCutoff_s]][[curExp]][['full']][[8]] <- update(
#   mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
#   . ~ . + ambigPhoneme,
#   control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
# print( mlist2[[curCutoff_s]][[curExp]][['full']][[8]]@optinfo$conv$lme4$code )
#
# mlist2[[curCutoff_s]][[curExp]][['full']][[9]] <- update(
#   mlist2[[curCutoff_s]][[curExp]][['full']][[1]],
#   . ~ . + ambigPhoneme * blockOrderHalf,
#   control = glmerControl(optimizer = c("Nelder_Mead", "bobyqa")))
# print( mlist2[[curCutoff_s]][[curExp]][['full']][[9]]@optinfo$conv$lme4$code )

anova(
  mlist2[[curCutoff_s]][[curExp]][["full"]][[1]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[2]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[3]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[6]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[5]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[4]]
)

anova(
  mlist2[[curCutoff_s]][[curExp]][["full"]][[1]],
  # mlist2[[curCutoff_s]][[curExp]][["full"]][[2]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[3]],
  # mlist2[[curCutoff_s]][[curExp]][["full"]][[6]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[5]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[4]]
)


anova(
  mlist2[[curCutoff_s]][[curExp]][["full"]][[1]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[2]],
  # mlist2[[curCutoff_s]][[curExp]][["full"]][[3]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[5]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[4]]
)


anova(
  mlist2[[curCutoff_s]][[curExp]][["full"]][[1]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[2]],
  # mlist2[[curCutoff_s]][[curExp]][["full"]][[3]],
  # mlist2[[curCutoff_s]][[curExp]][["full"]][[5]],
  mlist2[[curCutoff_s]][[curExp]][["full"]][[4]]
)
