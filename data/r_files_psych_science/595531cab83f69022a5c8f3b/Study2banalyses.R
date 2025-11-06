##load Study2bdata.xlsx##

library(xlsx)
library(car)
library(QuantPsyc)
library(yhat)

cor(Study2bdata[,3:6]) #correlation matrix 

ScientificComfort<-8-ScientificDiscomfort #reverse code scientific possibility

Study2bMultivariateModel<-lm(cbind(ScientificPossibility,ScientificComfort)~UniqueAbility+HumanExceptionalism)
summary(Study2bMultivariateModel)
Manova(Study2bMultivariateModel)

ScientificPossibilityModel<-lm(ScientificPossibility~UniqueAbility+HumanExceptionalism)
summary(ScientificPossibilityModel)
lm.beta(ScientificPossibilityModel)

ScientificComfortModel<-lm(ScientificComfort~UniqueAbility+HumanExceptionalism)
summary(ScientificComfortModel)
lm.beta(ScientificComfortModel)

ScientificPossibilityDominance<-aps(Study2bdata,"ScientificPossibility",list("UniqueAbility","HumanExceptionalism"))
dominance(ScientificPossibilityDominance)

ScientificDiscomfortDominance<-aps(Study2bdata,"ScientificDiscomfort",list("UniqueAbility","HumanExceptionalism"))
dominance(ScientificDiscomfortDominance)
