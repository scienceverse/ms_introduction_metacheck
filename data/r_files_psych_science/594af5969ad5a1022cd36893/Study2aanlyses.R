##load Study2adata.xlsx##

library(xlsx)
library(car)
library(QuantPsyc)
library(yhat)

cor(Study2adata[,3:7]) #correlation matrix

ScientificComfort<-ScientificDiscomfort*-1 #reverse code scientific discomfort 

Study2aMultivariateModel<-lm(cbind(ScientificPossibility,ScientificComfort)~FirstPerson+Introspection+SubjectiveExperience)
summary(Study2aMultivariateModel)
Manova(Study2aMultivariateModel)

ScientificPossibilityModel<-lm(ScientificPossibility~FirstPerson+Introspection+SubjectiveExperience)
summary(ScientificPossibilityModel)
lm.beta(ScientificPossibilityModel)

ScientificComfortModel<-lm(ScientificComfort~FirstPerson+Introspection+SubjectiveExperience)
summary(ScientificComfortModel)
lm.beta(ScientificComfortModel)

ScientificPossibilityDominance<-aps(Study2adata,"ScientificPossibility",list("FirstPerson","Introspection","SubjectiveExperience"))
dominance(ScientificPossibilityDominance)

ScientificDiscomfortDominance<-aps(Study2adata,"ScientificDiscomfort",list("FirstPerson","Introspection","SubjectiveExperience"))
dominance(ScientificDiscomfortDominance)
