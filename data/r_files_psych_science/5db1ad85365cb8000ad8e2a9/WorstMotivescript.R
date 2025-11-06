require(dplyr)
require(beepr)
install.packages("tidyverse")
install.packages("plotrix")
install.packages("graphics")
install.packages("reshape")
library(plotrix)
library(ggplot2)
library(dplyr)
library(tidyr)
library (lme4)
library (beepr)
library(tidyverse)

#FilingCabinet#

WM <- read.csv("jan2019MUNGE.csv", header = T, sep=";")
WMplot <-read.csv("jan2019graph.csv", header=T, sep=";")
WMplot2 <- read.csv("jan2019ownother.csv", header=T, sep=";")

#rename vignette
names(WM)[names(WM) == "condition"] <- "vignette"
names(WMplot)[names(WMplot) == "condition"] <- "vignette"
names(WMplot2)[names(WMplot2) == "condition"] <- "vignette"

###FULL NULL COMPARISON####

#Is rating a significant predictor of whether an option will be expected to be chosen by other? 
#Testing for difference in ratings of options as a predictor, along with own choice and difference in 
#absolute rating (to see if the mere magnitude of the rating, whether good or bad, was the driver of the result).
#Term names: the prediction of what the protagonist is going to do is called 'other.gb'; the 
#difference between ratings of bad and good options is called 'bdmingd'. The difference
#between the absolute ratings is called 'absbdmingd'. The participant's own preference
#is called 'own.gb'. 

full1=glm(other.gb~bdmingd*absbdmingd+own.gb+vignette+duration, data=WM, family=binomial(link="logit"))
null=glm(other.gb~absbdmingd+own.gb+vignette+duration, data=WM, family=binomial(link="logit"))
anova(full1, null, test="Chisq")


# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd * absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ absbdmingd + own.gb + vignette + duration
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
# 1       314     420.10                       
# 2       316     426.44 -2  -6.3425  0.04195 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

full2=glm(other.gb~bdmingd+absbdmingd+own.gb+vignette+duration, data=WM, family=binomial(link="logit"))
anova(full1, full2, test="Chisq")

# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd * absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ bdmingd + absbdmingd + own.gb + vignette + duration
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1       314     420.10                     
# 2       315     420.64 -1 -0.54194   0.4616

##test for effect of absolute rating###
full3=glm(other.gb~bdmingd+own.gb+vignette+duration, data=WM, family=binomial(link="logit"))
anova(full1, full3, test="Chisq")

# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd * absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ bdmingd + own.gb + vignette + duration
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
# 1       314     420.10                        
# 2       316     429.64 -2  -9.5331  0.00851 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary (full1)
 
# # 
# Call:
#   glm(formula = other.gb ~ bdmingd * absbdmingd + own.gb + vignette + 
#         duration, family = binomial(link = "logit"), data = WM)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6735  -1.0075  -0.7959   1.2087   1.7199  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)        -0.6017630  0.3959870  -1.520   0.1286  
# bdmingd             0.0410667  0.0165360   2.483   0.0130 *
#   absbdmingd         -0.1953714  0.0909094  -2.149   0.0316 *
#   own.gbgood          0.7879609  0.3064833   2.571   0.0101 *
#   vignetteparty       0.0944522  0.3303290   0.286   0.7749  
# vignettesummer      0.5218024  0.3473735   1.502   0.1331  
# vignettetown        0.4969909  0.3481142   1.428   0.1534  
# duration            0.0004107  0.0003798   1.081   0.2795  
# bdmingd:absbdmingd -0.0054818  0.0075410  -0.727   0.4673  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 441.48  on 322  degrees of freedom
# Residual deviance: 420.10  on 314  degrees of freedom
# AIC: 438.1
# 
# Number of Fisher Scoring iterations: 4

###full1 plus drop1 test to check remaining terms###

drop1(full1, test="Chisq")

#Single term deletions

# Model:
#   other.gb ~ bdmingd * absbdmingd + own.gb + vignette + duration
# Df Deviance    AIC    LRT Pr(>Chi)   
# <none>                  420.10 438.10                   
# own.gb              1   426.97 442.97 6.8660 0.008785 **
#   vignette            3   423.56 435.56 3.4621 0.325711   
# duration            1   421.37 437.37 1.2680 0.260133   
# bdmingd:absbdmingd  1   420.64 436.64 0.5419 0.461628   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Plots with SE#

ggplot(WMplot, aes(y=rating, x=other.chosen, colour = other.chosen))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.2, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  stat_summary(geom="crossbar", location = "behind") +
  facet_grid(~vignette)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "Expected of Protagonist")

ggplot(WMplot2, aes(y=rating, x=chosen, colour = chosen))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.1, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  stat_summary(geom="crossbar", location = "behind") +
  facet_grid(~own.other)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "")

##With95% confidence intervals##

##Across Vignettes##

ggplot(WMplot, aes(y=rating, x=other.chosen, colour = other.chosen))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.2, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  geom_crossbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))+
  facet_grid(~vignette)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "Expected of Protagonist")

###Other Only##

ggplot(WMplot, aes(y=rating, x=other.chosen, colour = other.chosen))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.2, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  geom_crossbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "Expected of Protagonist")

##Both##

ggplot(WMplot2, aes(y=rating, x=chosen, colour = chosen))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.1, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  geom_crossbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))+
  facet_grid(~own.other)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "")
