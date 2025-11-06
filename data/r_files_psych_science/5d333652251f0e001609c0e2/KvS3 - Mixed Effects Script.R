# KvS3 - Mixed Effects Models #

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

KvS3_Mixed_Effects_Data <- read_excel("BC Experiments/Kin V Stranger/Manuscript for Psych Science/Revision/KvS-3/KvS3 - Mixed Effects Data.xlsx")### CHANGE TO OSF PAGE LINK WHEN READY ###
View(KvS3_Mixed_Effects_Data)


# Recode Conditions #

KvS3_Mixed_Effects_Data$R[KvS3_Mixed_Effects_Data$Relationship=="Stranger"]<-"0.Stranger"
KvS3_Mixed_Effects_Data$R[KvS3_Mixed_Effects_Data$Relationship=="Kin"]<-"1.Kin"

KvS3_Mixed_Effects_Data$C[KvS3_Mixed_Effects_Data$Choice=="NoChoice"]<-"1.NoChoice"
KvS3_Mixed_Effects_Data$C[KvS3_Mixed_Effects_Data$Choice=="Choice"]<-"0.Choice"
KvS3_Mixed_Effects_Data$C[KvS3_Mixed_Effects_Data$Choice=="Failed"]<-"2.Failed"


# Clean Data #

KvS3_Clean<-KvS3_Mixed_Effects_Data %>%
  filter(PassedChecks>1)


#### Mixed Effects Models ####

## MORALITY ##

int_Morality=lmer(Morality~R*C+(1|Subject)+(1|Scenario),data=KvS3_Clean, REML=FALSE)
summary(int_Morality)

maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS3_Clean, REML=FALSE)
summary(maineffs_Morality)

# compare models #
anova(int_Morality,maineffs_Morality)

# pairwise comparisons #
emmeans(int_Morality,list(pairwise~R*C),adjust="none")


## UNPREDICTABILITY ##

int_Unpredict=lmer(Unpredict~R*C+(1|Subject)+(1|Scenario),data=KvS3_Clean, REML=FALSE)
summary(int_Unpredict)

maineffs_Unpredict=lmer(Unpredict~R+C+(1|Subject)+(1|Scenario),data=KvS3_Clean, REML=FALSE)
summary(maineffs_Unpredict)

# compare models #
anova(int_Unpredict,maineffs_Unpredict)

# pairwise comparisons #
emmeans(int_Unpredict,list(pairwise~R*C),adjust="none")