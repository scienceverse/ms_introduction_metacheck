# Analysis script for Fleming, Maniscalco, Ko, Amendi, Ro & Lau (2014) "Action-specific disruption of perceptual confidence"
#
# Requires "lme4" and "car" packages to be installed
# Steve Fleming 2014 sf102@nyu.edu

# Set working directory to where the data are located
setwd("~/Dropbox/Research/Metacognition/TMS/analysis")

rm(list=ls())
require(lme4)
require(car)
options(contrasts = c("contr.treatment", "contr.poly")) # This is R defaults but set it anyway to be safe

## EXP 1 - PMd group
# Confidence, RT and performance analysis
data <- read.table("allData_contrast_PMC.txt")
names(data) <- c("subj", "conf", "correct", "cond", "tms_time", "responseRT", "ratingRT")

tms_data <- data[data$tms_time == 1 | data$tms_time == 2, ]
tms_data$tms_time <- factor(tms_data$tms_time)
tms_data$cond <- factor(tms_data$cond)
tms_data$correct <- factor(tms_data$correct)
tms_data$subj <- factor(tms_data$subj)

# Confidence
conf_exp1_PMC.lmer_full = lmer(conf ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(conf_exp1_PMC.lmer_full, type=3)
# Performance
perf_exp1_PMC.lmer_full = lmer(correct ~ cond*tms_time + (1 + cond*tms_time | subj), data=tms_data, family="binomial")
summary(perf_exp1_PMC.lmer_full)
# RT
RT_exp1_PMC.lmer_full = lmer(responseRT ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(RT_exp1_PMC.lmer_full, type=3)
# Rating RT
ratingRT_exp1_PMC.lmer_full = lmer(ratingRT ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(ratingRT_exp1_PMC.lmer_full, type=3)

## EXP 1 - M1 group
data <- read.table("allData_contrast_M1.txt")
names(data) <- c("subj", "conf", "correct", "cond", "tms_time", "responseRT", "ratingRT")
tms_data <- data[data$tms_time == 1 | data$tms_time == 2, ]
tms_data$tms_time <- factor(tms_data$tms_time)
tms_data$cond <- factor(tms_data$cond)
tms_data$correct <- factor(tms_data$correct)
tms_data$subj <- factor(tms_data$subj)

# Confidence
conf_exp1_M1.lmer_full = lmer(conf ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(conf_exp1_M1.lmer_full, type=3)
# Performance
perf_exp1_M1.lmer_full = lmer(correct ~ cond*tms_time + (1 + cond*tms_time | subj), data=tms_data, family="binomial")
summary(perf_exp1_M1.lmer_full, type=3)
# RT
RT_exp1_M1.lmer_full = lmer(responseRT ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(RT_exp1_M1.lmer_full, type=3)
# Rating RT
ratingRT_exp1_M1.lmer_full = lmer(ratingRT ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(ratingRT_exp1_M1.lmer_full, type=3)

## EXP 2
# Confidence, performance + RT analysis
data <- read.table("allData_orientation.txt")
names(data) <- c("subj", "conf", "adj_conf", "correct", "cond", "tms_time", "responseRT", "ratingRT")
tms_data <- data[data$tms_time == 1 | data$tms_time == 2, ]
tms_data$tms_time <- factor(tms_data$tms_time)
tms_data$cond <- factor(tms_data$cond)
tms_data$correct <- factor(tms_data$correct)
tms_data$subj <- factor(tms_data$subj)

# Confidence
conf_exp2.lmer_full = lmer(conf ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(conf_exp2.lmer_full, type=3)
# Performance
perf_exp2.lmer_full = lmer(correct ~ cond*tms_time + (1 + cond*tms_time | subj), data=tms_data, family="binomial")
summary(perf_exp2.lmer_full, type=3)
# RT
RT_exp2.lmer_full = lmer(responseRT ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(RT_exp2.lmer_full, type=3)
# Rating RT
ratingRT_exp2.lmer_full = lmer(ratingRT ~ correct*cond*tms_time + (1 + correct*cond*tms_time | subj), data=tms_data)
Anova(ratingRT_exp2.lmer_full, type=3)

## Confidence model with Experiment as factor (concatenate data matrices)
m1_data <- read.table("allData_contrast_M1.txt")
pmc_data <- read.table("allData_contrast_PMC.txt")
pmc_data[ ,1] <- pmc_data[ ,1] + max(m1_data[ ,1])      # adjust subject numbers in one dataset to ensure subject-level term is correct
concat_data <- rbind(m1_data, pmc_data)
names(concat_data) <- c("subj", "conf", "correct", "cond", "tms_time", "responseRT", "ratingRT")
concat_data$expt <- c(rep(1,nrow(m1_data)), rep(2,nrow(pmc_data)))
tms_data <- concat_data[concat_data$tms_time == 1 | concat_data$tms_time == 2, ]
tms_data$tms_time <- factor(tms_data$tms_time)
tms_data$cond <- factor(tms_data$cond)
tms_data$correct <- factor(tms_data$correct)
tms_data$subj <- factor(tms_data$subj)
tms_data$expt <- factor(tms_data$expt)
conf_exp1_concat.lmer_full = lmer(conf ~ correct*cond*expt + (1 + correct*cond | subj), data=tms_data)

## Mratio analysis for Exp 1 and 2
data <- read.table("Mratio_all.txt")
names(data) <- c("subj", "Mratio", "metad", "cond", "tms_time", "expt")
tms_data <- data[data$tms_time == 1 | data$tms_time == 2, ]
tms_data$tms_time <- factor(tms_data$tms_time)
tms_data$cond <- factor(tms_data$cond)
tms_data$expt <- factor(tms_data$expt)
tms_data$subj <- factor(tms_data$subj)

mratio_PMC.aov_full = aov(Mratio ~ cond*tms_time*expt + Error(subj/(cond*tms_time)), data=tms_data)
summary(mratio_PMC.aov_full)
metad_PMC.aov_full = aov(metad ~ cond*tms_time*expt + Error(subj/(cond*tms_time)), data=tms_data)
summary(metad_PMC.aov_full)

## Mratio Exp 1 M1

data <- read.table("Mratio_contrast_M1.txt")
names(data) <- c("subj", "Mratio", "meta-d", "cond", "tms_time")
tms_data <- data[data$tms_time == 1 | data$tms_time == 2, ]
tms_data$tms_time <- factor(tms_data$tms_time)
tms_data$cond <- factor(tms_data$cond)
tms_data$subj <- factor(tms_data$subj)

mratio_M1.aov_full = aov(Mratio ~ cond*tms_time + Error(subj/(cond*tms_time)), data=tms_data)
summary(mratio_M1.aov_full)

