# KvS1 - Mixed Effects Models #

# Relationship is coded as "0.Stranger" and "1.Kin" so that the reference category is always Stranger" #
# Choice is coded as "0.Choice" and "1.NoChoice" so that the reference category is always Choice #


#### Install and Load Packages / Clean Data ####

install.packages("lme4")
install.packages("lmerTest")
install.packages("dplyr")
install.packages("emmeans")

library(lme4)
library(lmerTest)
library(dplyr)
library(emmeans)

# Load Data #

library(readxl)

KvS1_Mixed_Effects_Data <- read_excel("BC Experiments/Kin V Stranger/Manuscript for Psych Science/Revision/KvS-1/KvS1 - Mixed Effects Data.xlsx")### CHANGE TO OSF PAGE LINK WHEN READY ###
View(KvS1_Mixed_Effects_Data)


# Recode Conditions #

KvS1_Mixed_Effects_Data$R[KvS1_Mixed_Effects_Data$Relationship=="Stranger"]<-"0.Stranger"
KvS1_Mixed_Effects_Data$R[KvS1_Mixed_Effects_Data$Relationship=="Kin"]<-"1.Kin"

KvS1_Mixed_Effects_Data$C[KvS1_Mixed_Effects_Data$Choice=="NoChoice"]<-"1.NoChoice"
KvS1_Mixed_Effects_Data$C[KvS1_Mixed_Effects_Data$Choice=="Choice"]<-"0.Choice"


# Clean Data #

KvS1_Clean<-KvS1_Mixed_Effects_Data %>%
  filter(PassedChecks>1)


#### Mixed Effects Models ####

## MORALITY ##

int_Morality=lmer(Morality~R*C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(int_Morality)

maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(maineffs_Morality)
# maineffs_Morality failed to converge - so the optimizer was changed (and the model converged) #
maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(maineffs_Morality)

# compare models #
anova(int_Morality,maineffs_Morality)

# pairwise comparisons #
emmeans(int_Morality,list(pairwise~R*C),adjust="none")


## TRUSTWORTHINESS ##

int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(int_Trust)

maineffs_Trust=lmer(Trust~R+C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(maineffs_Trust)

# compare models #
anova(int_Trust, maineffs_Trust)

# pairwise comparisons #
emmeans(int_Trust,list(pairwise~R*C),adjust="none")


## WRONGNESS ##

int_Wrong=lmer(Wrong~R*C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(int_Wrong)

maineffs_Wrong=lmer(Wrong~R+C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(maineffs_Wrong)

# compare models #
anova(int_Wrong, maineffs_Wrong)

# pairwise comparisons #
emmeans(int_Wrong,list(pairwise~R*C),adjust="none")


## DIAGNOSTICITY ##

int_Diag=lmer(Diag~R*C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(int_Diag)

maineffs_Diag=lmer(Diag~R+C+(1|Subject)+(1|Scenario),data=KvS1_Clean, REML=FALSE)
summary(maineffs_Diag)

# compare models #
anova(int_Diag, maineffs_Diag)

# pairwise comparisons #
emmeans(int_Diag,list(pairwise~R*C),adjust="none")