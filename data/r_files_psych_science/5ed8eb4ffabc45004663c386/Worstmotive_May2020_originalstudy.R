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
null=glm(other.gb~vignette+duration, data=WM, family=binomial(link="logit"))
anova(full1, null, test="Chisq")

# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd * absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ vignette + duration
# Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
# 1       314     420.10                          
# 2       318     439.26 -4  -19.162 0.0007305 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Test for effect of interaction##

full2=glm(other.gb~bdmingd+absbdmingd+own.gb+vignette+duration, data=WM, family=binomial(link="logit"))
anova(full1, full2, test="Chisq")

# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd * absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ bdmingd + absbdmingd + own.gb + vignette + duration
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1       314     420.10                     
# 2       315     420.64 -1 -0.54194   0.4616

##No intereaction, so remove interaction term and compare model with bdmingd to model withoutg###

full3=glm(other.gb~absbdmingd+own.gb+vignette+duration, data=WM, family=binomial(link="logit"))
anova(full2, full3, test="Chisq")

# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd + absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ absbdmingd + own.gb + vignette + duration
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
# 1       315     420.64                       
# 2       316     426.44 -1  -5.8005  0.01602 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Compare models with and without "own choice"##

full4=glm(other.gb~bdmingd+absbdmingd+vignette+duration, data=WM, family=binomial(link="logit"))
anova(full2, full4, test="Chisq")

# Analysis of Deviance Table
# 
# Model 1: other.gb ~ bdmingd + absbdmingd + own.gb + vignette + duration
# Model 2: other.gb ~ bdmingd + absbdmingd + vignette + duration
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
# 1       315     420.64                       
# 2       316     427.25 -1  -6.6075  0.01016 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Conclusion: there is a significant effect of rating, even including absolute rating in the model; 
##There is also an effect of own choice - participants significantly prefer the opposite of protagonist.

summary(full1)

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



############################################
#### Test with vignette as random intercept####
############################################



#Transform and Z-Center Factors and Trial NOC#


duration.n=as.numeric(WM$duration)
duration.m=duration.n-mean(duration.n)

own.gb.n=as.numeric(WM$own.gb)
own.gb.m=own.gb.n-mean(own.gb.n)

bdmingd.n=as.numeric(WM$bdmingd)
bdmingd.m=bdmingd.n-mean(bdmingd.n)

absbdmingd.n=as.numeric(WM$absbdmingd)
absbdmingd.m=absbdmingd.n-mean(absbdmingd.n)

#full model#

full1vrand=glmer(other.gb~bdmingd.m*absbdmingd.m+own.gb.m+duration.m+(1|vignette), data=WM, family=binomial(link="logit"))

#model failed to converge: remove 'duration'

full1vrand=glmer(other.gb~bdmingd.m*absbdmingd.m+own.gb.m+(1|vignette), data=WM, family=binomial(link="logit"))
null1vrand=glmer(other.gb~(1|vignette), data=WM, family=binomial(link="logit"))
anova(full1vrand, null1vrand, test="Chisq")

# Data: WM
# Models:
#   null1vrand: other.gb ~ (1 | vignette)
# full1vrand: other.gb ~ bdmingd * absbdmingd + own.gb + (1 | vignette)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)   
# null1vrand  2 445.48 453.04 -220.74   441.48                           
# full1vrand  6 436.96 459.63 -212.48   424.96 16.52      4   0.002395 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#test for effect of interaction#

red1vrand=glmer(other.gb~bdmingd.m+absbdmingd.m+own.gb.m+(1|vignette), data=WM, family=binomial(link="logit"))
anova(full1vrand, red1vrand, test="Chisq")

# Data: WM
# Models:
#   red1vrand: other.gb ~ bdmingd + absbdmingd + own.gb + (1 | vignette)
# full1vrand: other.gb ~ bdmingd * absbdmingd + own.gb + (1 | vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# red1vrand   5 435.39 454.28 -212.70   425.39                         
# full1vrand  6 436.96 459.63 -212.48   424.96 0.4305      1     0.5117

#no effect - removing interaction, test for effect of bdmingd#

red2vrand=glmer(other.gb~absbdmingd.m+own.gb.m+(1|vignette), data=WM, family=binomial(link="logit"))
anova(red1vrand, red2vrand, test="Chisq")

# Data: WM
# Models:
#   red2vrand: other.gb ~ absbdmingd.m + own.gb.m + (1 | vignette)
# red1vrand: other.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# red2vrand  4 437.83 452.94 -214.91   429.83                           
# red1vrand  5 435.39 454.28 -212.70   425.39 4.4343      1    0.03522 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#  -> significant effect of rating#

#test for effect of own choice#

red3vrand=glmer(other.gb~bdmingd.m+absbdmingd.m+(1|vignette), data=WM, family=binomial(link="logit"))
anova(red1vrand, red3vrand, test="Chisq")

# Data: WM
# Models:
#   red3vrand: other.gb ~ bdmingd.m + absbdmingd.m + (1 | vignette)
# red1vrand: other.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | vignette)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)  
# red3vrand  4 437.82 452.93 -214.91   429.82                          
# red1vrand  5 435.39 454.28 -212.70   425.39 4.428      1    0.03535 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#  -> significant effect of own choice#


red4vrand=glmer(other.gb~bdmingd.m+own.gb.m+(1|vignette), data=WM, family=binomial(link="logit"))
anova(red1vrand, red4vrand, test="Chisq")

# Data: WM
# Models:
#   red4vrand: other.gb ~ bdmingd.m + own.gb.m + (1 | vignette)
# red1vrand: other.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# red4vrand  4 442.17 457.28 -217.09   434.17                            
# red1vrand  5 435.39 454.28 -212.70   425.39 8.7769      1   0.003051 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# -> significant effect of absolute rating

##Plots##

##Other Only##

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
  scale_x_discrete(name = "Actions Expected of Another or Oneself")
