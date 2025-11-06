parietal_increase <- read.table("PSC_parietalincrease.txt", 
                   header=TRUE, sep="\t", na.strings="NA", dec=",", fill=TRUE, strip.white=TRUE)

#exclude subject because of a lack of valid data at at least one timepoint
parietal_increase <- subset(parietal_increase,ID!="1133")

#factorize into age groups
attach(parietal_increase)
parietal_increase$groups[groups == 3] <- "kindergarteners"
parietal_increase$groups[groups == 2] <- "first graders"

#factorize into 2 time points
attach(parietal_increase)
parietal_increase$time[time == 1] <- "T1"
parietal_increase$time[time == 2] <- "T2"

#factorize into 2 waves
parietal_increase$wave <- factor(parietal_increase$wave, labels=c('wave1','wave2'))

#set effect coding
options(contrasts=c("contr.sum","contr.poly"))

#Mixed ANOVA for right PPC effect at T2
library(ez)
aov_PSC_rPPC <- ezANOVA(parietal_increase, dv=rPPC_frontoparietal_Mask, wid=ID, within=time, between=.(groups),type=3)
aov_PSC_rPPC_stats <- ezStats(parietal_increase, dv=rPPC_frontoparietal_Mask, wid=ID, within=time, between=groups)

#Mixed ANOVA for right PFC effect at T2
library(ez)
aov_PSC_rPFC <- ezANOVA(parietal_increase, dv=rPFC_frontoparietal_Mask, wid=ID, within=time, between=.(groups),type=3)
aov_PSC_rPFC_stats <- ezStats(parietal_increase, dv=rPFC_frontoparietal_Mask, wid=ID, within=time, between=groups)

#Mixed ANOVA for left PPC effect at T2
library(ez)
aov_PSC_lPPC <- ezANOVA(parietal_increase, dv=lPPC_frontoparietal_Mask, wid=ID, within=time, between=.(groups),type=3)
aov_PSC_lPPC_stats <- ezStats(parietal_increase, dv=lPPC_frontoparietal_Mask, wid=ID, within=time, between=groups)

#Mixed ANOVA for left PFC effect at T2
library(ez)
aov_PSC_lPFC <- ezANOVA(parietal_increase, dv=lPFC_frontoparietal_Mask, wid=ID, within=time, between=.(groups),type=3)
aov_PSC_lPFC_stats <- ezStats(parietal_increase, dv=lPFC_frontoparietal_Mask, wid=ID, within=time, between=groups)

#mixed ANOVA for parietal increase analysis 
library(ez)
aov_PSC <- ezANOVA(parietal_increase, dv=parietal_increase_T1, wid=ID, within=time, between=.(groups),type=3)
aov_PSC_stats <- ezStats(parietal_increase, dv=parietal_increase_T1, wid=ID, within=time, between=groups)

