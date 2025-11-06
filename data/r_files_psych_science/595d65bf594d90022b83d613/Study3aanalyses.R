##load Study3adata.csv##

library(xlsx)
library(car)
library(QuantPsyc)
library(yhat)

cor(Study3adata[,3:9]) #correlation matrix 

ScientificComfort<-8-ScientificDiscomfort #reverse code scientific possibility

Study3aMultivariateModel<-lm(cbind(ScientificPossibility,ScientificComfort)~HumanExceptionalism+NormativelyGood+Introspection+FirstPersonAccess+ConsciousWill)
summary(Study3aMultivariateModel)
Manova(Study3aMultivariateModel)

ScientificPossibilityModel<-lm(ScientificPossibility~HumanExceptionalism+NormativelyGood+Introspection+FirstPersonAccess+ConsciousWill)
summary(ScientificPossibilityModel)
lm.beta(ScientificPossibilityModel)

ScientificComfortModel<-lm(ScientificComfort~HumanExceptionalism+NormativelyGood+Introspection+FirstPersonAccess+ConsciousWill)
summary(ScientificComfortModel)
lm.beta(ScientificComfortModel)