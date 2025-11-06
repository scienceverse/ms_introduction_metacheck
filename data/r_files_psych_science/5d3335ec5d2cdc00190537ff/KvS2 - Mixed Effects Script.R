# KvS2 - Mixed Effects Models #

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

KvS2_Mixed_Effects_Data <- read_excel("BC Experiments/Kin V Stranger/Manuscript for Psych Science/Revision/KvS-2/KvS2 - Mixed Effects Data.xlsx")### CHANGE TO OSF PAGE LINK WHEN READY ###
View(KvS2_Mixed_Effects_Data)


# Recode Conditions #

KvS2_Mixed_Effects_Data$R[KvS2_Mixed_Effects_Data$Relationship=="Stranger"]<-"0.Stranger"
KvS2_Mixed_Effects_Data$R[KvS2_Mixed_Effects_Data$Relationship=="Kin"]<-"1.Kin"

KvS2_Mixed_Effects_Data$C[KvS2_Mixed_Effects_Data$Choice=="NoChoice"]<-"1.NoChoice"
KvS2_Mixed_Effects_Data$C[KvS2_Mixed_Effects_Data$Choice=="Choice"]<-"0.Choice"


# Clean Data #

KvS2_Clean<-KvS2_Mixed_Effects_Data %>%
  filter(PassedChecks>1)


#### Mixed Effects Models ####

## MORALITY ##

int_Morality=lmer(Morality~R*C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(int_Morality)

maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(maineffs_Morality)
# maineffs_Morality failed to converge - so the optimizer was changed (and the model converged) #
maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE,
                       control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(maineffs_Morality)

# compare models #
anova(int_Morality, maineffs_Morality)

# pairwise comparisons #
emmeans(int_Morality,list(pairwise~R*C),adjust="none")


## TRUSTWORTHINESS ##

int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(int_Trust)
# Model failed to converge. Dropped by-scenario intercept because there was almost no variance for the random effect, and re-ran model #
int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE,
               control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(int_Trust)

maineffs_Trust=lmer(Trust~R+C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(maineffs_Trust)

# compare models #
anova(int_Trust, maineffs_Trust)

# pairwise comparisons #
emmeans(int_Trust,list(pairwise~R*C),adjust="none")


## HARM ##

int_Harm=lmer(Harm~R*C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(int_Harm)

maineffs_Harm=lmer(Harm~R+C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(maineffs_Harm)

# compare models #
anova(int_Harm, maineffs_Harm)

# pairwise comparisons #
emmeans(int_Harm,list(pairwise~R*C),adjust="none")


## UNPREDICTABILITY ##

int_Unpredict=lmer(Unpredict~R*C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(int_Unpredict)

maineffs_Unpredict=lmer(Unpredict~R+C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(maineffs_Unpredict)

# compare models #
anova(int_Unpredict, maineffs_Unpredict)

# pairwise comparisons #
emmeans(int_Unpredict,list(pairwise~R*C),adjust="none")


## TYPICALITY ##

int_Typ=lmer(Typ~R*C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(int_Typ)

maineffs_Typ=lmer(Typ~R+C+(1|Subject)+(1|Scenario),data=KvS2_Clean, REML=FALSE)
summary(maineffs_Typ)

# compare models #
anova(int_Typ, maineffs_Typ)

# pairwise comparisons #
emmeans(int_Typ,list(pairwise~R*C),adjust="none")
