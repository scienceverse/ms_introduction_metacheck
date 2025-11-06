##load Study2bdata.xlsx##

library(xlsx)
library(car)
library(QuantPsyc)
library(yhat)

cor(Study2cdata[,3:6]) #correlation matrix 

ScientificComfort<-8-ScientificDiscomfort #reverse code scientific possibility

Study2cMultivariateMod<-lm(cbind(ScientificPossibility,ScientificComfort)~HighFrequency+Normative)
summary(Study2cMultivariateMod)
Manova(Study2cMultivariateMod)

ScientificPossibilityModel<-lm(ScientificPossibility~HighFrequency+Normative)
summary(ScientificPossibilityModel)
lm.beta(ScientificPossibilityModel)

ScientificComfortModel<-lm(ScientificComfort~HighFrequency+Normative)
summary(ScientificComfortModel)
lm.beta(ScientificComfortModel)

cor.test(HighFrequency,Normative)

ScientificPossibilityDominance<-aps(Study2cdata,"ScientificPossibility",list("HighFrequency","Normative"))
dominance(ScientificPossibilityDominance)

ScientificDiscomfortDominance<-aps(Study2cdata,"ScientificDiscomfort",list("HighFrequency","Normative"))
dominance(ScientificDiscomfortDominance)
