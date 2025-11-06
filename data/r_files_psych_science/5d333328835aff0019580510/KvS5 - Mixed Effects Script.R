# KvS5 - Mixed Effects Models #

# Relationship is coded as "0.Stranger" and "1.Kin" so that the reference category is always Stranger" #
# Choice is coded as "0.Choice", "1.NoChoice", and "2.Failed" so that the reference category is always Choice #
# Context is coded as "0.Partial" and "1.Impartial" so that the reference category is always Partial #
# This coding allows all interactions of theoertical interest to be tested with one coding scheme #


## Install and load packages / Clean Data ####

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

KvS5_Mixed_Effects_Data <- read_excel("BC Experiments/Kin V Stranger/Manuscript for Psych Science/Revision/KvS-5/KvS5 - Mixed Effects Data.xlsx")### CHANGE TO OSF PAGE LINK WHEN READY ###
View(KvS5_Mixed_Effects_Data)


# Recode Conditions #

KvS5_Mixed_Effects_Data$R[KvS5_Mixed_Effects_Data$Relationship=="Stranger"]<-"0.Stranger"
KvS5_Mixed_Effects_Data$R[KvS5_Mixed_Effects_Data$Relationship=="Kin"]<-"1.Kin"

KvS5_Mixed_Effects_Data$C[KvS5_Mixed_Effects_Data$Choice=="NoChoice"]<-"1.NoChoice"
KvS5_Mixed_Effects_Data$C[KvS5_Mixed_Effects_Data$Choice=="Choice"]<-"0.Choice"
KvS5_Mixed_Effects_Data$C[KvS5_Mixed_Effects_Data$Choice=="Failed"]<-"2.Failed"

KvS5_Mixed_Effects_Data$Context[KvS5_Mixed_Effects_Data$Context=="Partial"]<-"0.Partial"
KvS5_Mixed_Effects_Data$Context[KvS5_Mixed_Effects_Data$Context=="Impartial"]<-"1.Impartial"

# Clean Data #

KvS5_CleanData<-KvS5_Mixed_Effects_Data %>%
  filter(PassedChecks>0)



#### Mixed Effects Models ####

## MORALITY ##

Morality_3way=lmer(Morality~R*C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Morality_3way)

# compare models #
Morality_No3way=lmer(Morality~R*C+R*Context+C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Morality_No3way)

anova(Morality_3way,Morality_No3way)

# pairwise comparisons #
emmeans(Morality_3way,list(pairwise~R*C*Context),adjust="none")


## TRUSTWORTHINESS ##

Trust_3way=lmer(Trust~R*C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Trust_3way)

# compare models #
Trust_No3way=lmer(Trust~R*C+R*Context+C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Trust_No3way)

anova(Trust_3way,Trust_No3way)

# pairwise comparisons #
emmeans(Trust_3way,list(pairwise~R*C*Context),adjust="none")


## OBLIGATION ##

Oblig_3way=lmer(Oblig~R*C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Oblig_3way)

# compare models #
Oblig_No3way=lmer(Oblig~R*C+R*Context+C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Oblig_No3way)

anova(Oblig_3way,Oblig_No3way)

# pairwise comparisons #
emmeans(Oblig_3way,list(pairwise~R*C*Context),adjust="none")


## PREDICTABILITY ##

Predict_3way=lmer(Predict~R*C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Predict_3way)

# compare models #
Predict_No3way=lmer(Predict~R*C+R*Context+C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Predict_No3way)

anova(Predict_3way,Predict_No3way)

# pairwise comparisons #
emmeans(Predict_3way,list(pairwise~R*C*Context),adjust="none")


## TYPICALITY ##

Typ_3way=lmer(Typ~R*C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Typ_3way)

# compare models #
Typ_No3way=lmer(Typ~R*C+R*Context+C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Typ_No3way)

anova(Typ_3way,Typ_No3way)

# pairwise comparisons #
emmeans(Typ_3way,list(pairwise~R*C*Context),adjust="none")


## EXPECTEDNESS ##

Exp_3way=lmer(Exp~R*C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Exp_3way)

# compare models #
Exp_No3way=lmer(Exp~R*C+R*Context+C*Context+(1|Subject)+(1|Scenario),data=KvS5_CleanData, REML=FALSE)
summary(Exp_No3way)

anova(Exp_3way,Exp_No3way)

# pairwise comparisons #
emmeans(Exp_3way,list(pairwise~R*C*Context),adjust="none")



#### Split Data by Partial/Impartial ####

KvS5_PartialClean<-KvS5_CleanData %>%
  filter(Context=='0.Partial')

KvS5_ImpartialClean<-KvS5_CleanData %>%
  filter(Context=='1.Impartial')



#### PARTIAL CONTEXT MODELS ####

## MORALITY ##

P_int_Morality=lmer(Morality~R*C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_int_Morality)

P_maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_maineffs_Morality)

# compare models #
anova(P_int_Morality,P_maineffs_Morality)

# pairwise comparisons #
emmeans(P_int_Morality,list(pairwise~R*C),adjust="none")


## TRUSTWORTHINESS ##

P_int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_int_Trust)

P_maineffs_Trust=lmer(Trust~R+C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_maineffs_Trust)

# compare models #
anova(P_int_Trust,P_maineffs_Trust)

# pairwise comparisons #
emmeans(P_int_Trust,list(pairwise~R*C),adjust="none")


## OBLIGATION ##

P_int_Oblig=lmer(Oblig~R*C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_int_Oblig)

P_maineffs_Oblig=lmer(Oblig~R+C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_maineffs_Oblig)

# compare models #
anova(P_int_Oblig,P_maineffs_Oblig)

# pairwise comparisons #
emmeans(P_int_Oblig,list(pairwise~R*C),adjust="none")


## PREDICTABILITY ##

P_int_Predict=lmer(Predict~R*C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_int_Predict)

P_maineffs_Predict=lmer(Predict~R+C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_maineffs_Predict)

# compare models #
anova(P_int_Predict,P_maineffs_Predict)

# pairwise comparisons #
emmeans(P_int_Predict,list(pairwise~R*C),adjust="none")


## TYPICALITY ##

P_int_Typ=lmer(Typ~R*C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_int_Typ)

P_maineffs_Typ=lmer(Typ~R+C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_maineffs_Typ)

# compare models #
anova(P_int_Typ,P_maineffs_Typ)

# pairwise comparisons #
emmeans(P_int_Typ,list(pairwise~R*C),adjust="none")


## EXPECTEDNESS ##

P_int_Exp=lmer(Exp~R*C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_int_Exp)

P_maineffs_Exp=lmer(Exp~R+C+(1|Subject)+(1|Scenario),data=KvS5_PartialClean, REML=FALSE)
summary(P_maineffs_Exp)

# compare models #
anova(P_int_Exp,P_maineffs_Exp)

# pairwise comparisons #
emmeans(P_int_Exp,list(pairwise~R*C),adjust="none")


#### IMPARTIAL CONTEXT MODELS ####

## MORALITY ##

I_int_Morality=lmer(Morality~R*C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Morality)

I_maineffs_Morality=lmer(Morality~R+C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_maineffs_Morality)

# compare models #

anova(I_int_Morality,I_maineffs_Morality)

# pairwise comparisons #

emmeans(I_int_Morality,list(pairwise~R*C),adjust="none")


## TRUSTWORTHINESS ##

I_int_Trust=lmer(Trust~R*C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Trust)

I_maineffs_Trust=lmer(Trust~R+C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_maineffs_Trust)

# compare models #
anova(I_int_Trust,I_maineffs_Trust)

# pairwise comparisons #
emmeans(I_int_Trust,list(pairwise~R*C),adjust="none")


## OBLIGATION ##

I_int_Oblig=lmer(Oblig~R*C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Oblig)

I_maineffs_Oblig=lmer(Oblig~R+C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_maineffs_Oblig)

# compare models #
anova(I_int_Oblig,I_maineffs_Oblig)

# pairwise comparisons #
emmeans(I_int_Oblig,list(pairwise~R*C),adjust="none")


## PREDICTABILITY ##

I_int_Predict=lmer(Predict~R*C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Predict)
# Model provided singular fit due to there being no by-scenario variance. Model was re-run dropping by-scenario intercept #
I_int_Predict=lmer(Predict~R*C+(1|Subject),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Predict)

I_maineffs_Predict=lmer(Predict~R+C+(1|Subject),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_maineffs_Predict)

# compare models #
anova(I_int_Predict,I_maineffs_Predict)

# pairwise comparisons #
emmeans(I_int_Predict,list(pairwise~R*C),adjust="none")


## TYPICALITY ##

I_int_Typ=lmer(Typ~R*C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Typ)
# Model provided singular fit due to there being no by-scenario variance. Model was re-run dropping by-scenario intercept #
I_int_Typ=lmer(Typ~R*C+(1|Subject),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Typ)

I_maineffs_Typ=lmer(Typ~R+C+(1|Subject),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_maineffs_Typ)

# compare models #
anova(I_int_Typ,I_maineffs_Typ)

# pairwise comparisons #
emmeans(I_int_Typ,list(pairwise~R*C),adjust="none")


## EXPECTEDNESS ##

I_int_Exp=lmer(Exp~R*C+(1|Subject)+(1|Scenario),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Exp)
# Model provided singular fit due to there being no by-scenario variance. Model was re-run dropping by-scenario intercept #
I_int_Exp=lmer(Exp~R*C+(1|Subject),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_int_Exp)

I_maineffs_Exp=lmer(Exp~R+C+(1|Subject),data=KvS5_ImpartialClean, REML=FALSE)
summary(I_maineffs_Exp)

# compare models #
anova(I_int_Exp,I_maineffs_Exp)

# pairwise comparisons #
emmeans(I_int_Exp,list(pairwise~R*C),adjust="none")