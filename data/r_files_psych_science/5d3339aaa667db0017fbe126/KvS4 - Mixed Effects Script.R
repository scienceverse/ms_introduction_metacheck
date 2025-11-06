# KvS4 - Mixed Effects Models #

# Relationship is coded as "0.Stranger" and "1.Kin" so that the reference category is always Stranger" #
# Choice is coded as "0.Choice", "1.NoChoice", and "2.Failed" so that the reference category is always Choice #
# This coding allows all interactions of theoertical interest to be tested with one coding scheme #


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

KvS4_Mixed_Effects_Data <- read_excel("BC Experiments/Kin V Stranger/Manuscript for Psych Science/Revision/KvS-4/KvS4 - Mixed Effects Data.xlsx")### CHANGE TO OSF PAGE LINK WHEN READY ###
View(KvS4_Mixed_Effects_Data)


# Recode Conditions #

KvS4_Mixed_Effects_Data$R[KvS4_Mixed_Effects_Data$Relationship=="Stranger"]<-"0.Stranger"
KvS4_Mixed_Effects_Data$R[KvS4_Mixed_Effects_Data$Relationship=="Kin"]<-"1.Kin"

KvS4_Mixed_Effects_Data$C[KvS4_Mixed_Effects_Data$Choice=="NoChoice"]<-"1.NoChoice"
KvS4_Mixed_Effects_Data$C[KvS4_Mixed_Effects_Data$Choice=="Choice"]<-"0.Choice"
KvS4_Mixed_Effects_Data$C[KvS4_Mixed_Effects_Data$Choice=="Failed"]<-"2.Failed"


# Clean Data #

KvS4_Clean<-KvS4_Mixed_Effects_Data %>%
  filter(PassedChecks>1)


#### Mixed Effects Models ####

## MORALITY ##

int_Morality=lmer(Morality~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Morality)

maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Morality)

# compare models #
anova(int_Morality,maineffs_Morality)

# pairwise comparisons #
emmeans(int_Morality,list(pairwise~R*C),adjust="none")


## TRUSTWORTHINESS ##

int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Trust)
# maineffs_Morality failed to converge - so the optimizer was changed (and the model converged) #
int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(int_Trust)

maineffs_Trust=lmer(Trust~R+C+(1|Subject),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Trust)

# compare models #
anova(int_Trust,maineffs_Trust)

# pairwise comparisons #
emmeans(int_Trust,list(pairwise~R*C),adjust="none")


## OBLIGATION ##

int_Oblig=lmer(Oblig~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Oblig)

maineffs_Oblig=lmer(Oblig~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Oblig)

# compare models #
anova(int_Oblig,maineffs_Oblig)

# pairwise comparisons #
emmeans(int_Oblig,list(pairwise~R*C),adjust="none")


## PRAISEWORTHINESS ##

int_Praise=lmer(Praise~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Praise)

maineffs_Praise=lmer(Praise~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Praise)

# compare models #
anova(int_Praise,maineffs_Praise)

# pairwise comparisons #
emmeans(int_Praise,list(pairwise~R*C),adjust="none")


## RIGHTNESS ##

int_Right=lmer(Right~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Right)

maineffs_Right=lmer(Right~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Right)

# compare models #
anova(int_Right,maineffs_Right)

# pairwise comparisons #
emmeans(int_Right,list(pairwise~R*C),adjust="none")


## FAIRNESS ##

int_Fair=lmer(Fair~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Fair)

maineffs_Fair=lmer(Fair~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Fair)

# compare models #
anova(int_Fair,maineffs_Fair)

# pairwise comparisons #
emmeans(int_Fair,list(pairwise~R*C),adjust="none")


## PREDICTABILITY ##

int_Predict=lmer(Predict~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Predict)

maineffs_Predict=lmer(Predict~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Predict)

# compare models #
anova(int_Predict,maineffs_Predict)

# pairwise comparisons #
emmeans(int_Predict,list(pairwise~R*C),adjust="none")


## TYPICALITY ##

int_Typ=lmer(Typ~R*C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(int_Typ)

maineffs_Typ=lmer(Typ~R+C+(1|Subject)+(1|Scenario),data=KvS4_Clean, REML=FALSE)
summary(maineffs_Typ)

# compare models #
anova(int_Typ,maineffs_Typ)

# pairwise comparisons #
emmeans(int_Typ,list(pairwise~R*C),adjust="none")