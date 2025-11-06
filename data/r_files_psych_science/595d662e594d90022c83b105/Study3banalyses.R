##load Study3bdata.csv##

library(xlsx)
library(car)
library(QuantPsyc)
library(yhat)

cor(Study3bdata[,3:9]) #correlation matrix 

ScientificComfort<-8-ScientificDiscomfort #reverse code scientific possibility

Study3bMultivariateModel<-lm(cbind(ScientificPossibility,ScientificComfort)~HumanExceptionalism+NormativelyGood+Introspection+FirstPersonAccess+ConsciousWill)
summary(Study3bMultivariateModel)
Manova(Study3bMultivariateModel)

ScientificPossibilityModel<-lm(ScientificPossibility~HumanExceptionalism+NormativelyGood+Introspection+FirstPersonAccess+ConsciousWill)
summary(ScientificPossibilityModel)
lm.beta(ScientificPossibilityModel)

ScientificComfortModel<-lm(ScientificComfort~HumanExceptionalism+NormativelyGood+Introspection+FirstPersonAccess+ConsciousWill)
summary(ScientificComfortModel)
lm.beta(ScientificComfortModel)

##Load Study3b_ParticipantLevelData.csv##
cor.test(AverageScientificPossibilityRating,NumPsychClasses) 
cor.test(AverageScientificDiscomfortRating,NumPsychClasses) 

##Load Study3a_ParticipantLevelData.csv and merge with previous file##
MergedData<-rbind(Study3a_ParticipantLevelData,Study3b_ParticipantLevelData)

SamplePossibilityDiffModel<-lm(AverageScientificPossibilityRating~Sample)
summary(SamplePossibilityDiffModel)

SampleComfortDiffModel<-lm((8-AverageScientificDiscomfortRating)~Sample)
summary(SampleComfortDiffModel)
