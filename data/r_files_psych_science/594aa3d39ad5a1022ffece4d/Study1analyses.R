##load Study1data.xlsx##

library(xlsx)
library(car)
library(QuantPsyc)
library(yhat)

cor(Study1data[,3:10]) #correlation matrix

ScientificComfort<-ScientificDiscomfort*-1 #reverse code scientific discomfort 

MultivariateModel<-lm(cbind(ScientificPossibility,ScientificComfort)~UniquelyHuman+IntrospectionPhenomenology+ConsciousWill+Complex+Identity+AbnormalFunctioning)
summary(MultivariateModel)
Manova(MultivariateModel)

ScientificPossibilityModel<-lm(ScientificPossibility~UniquelyHuman+IntrospectionPhenomenology+ConsciousWill+Complex+Identity+AbnormalFunctioning)
summary(ScientificPossibilityModel)
lm.beta(ScientificPossibilityModel)

ScientificComfortModel<-lm(ScientificComfort~UniquelyHuman+IntrospectionPhenomenology+ConsciousWill+Complex+Identity+AbnormalFunctioning)
summary(ScientificComfortModel)
lm.beta(ScientificComfortModel)
