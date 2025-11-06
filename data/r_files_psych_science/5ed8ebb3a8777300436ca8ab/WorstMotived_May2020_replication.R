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

WMrepli <- read.csv("WM_replication.csv", header = T, sep=";")
ownothch <-read.csv("ownother_chosennot.csv", header=T, sep=";")

## Replication

## Test with vignette as random factor

## Filing cabinet


own.gb.n=as.numeric(WMrepli$own.chosen.gb)
own.gb.m=own.gb.n-mean(own.gb.n)

bdmingd.n=as.numeric(WMrepli$bdmingd)
bdmingd.m=bdmingd.n-mean(bdmingd.n)

absbdmingd.n=as.numeric(WMrepli$abs.bdmingd)
absbdmingd.m=absbdmingd.n-mean(absbdmingd.n)


## Model 1

fullrepli=glmer(other.chosen.gb~bdmingd.m*absbdmingd.m+own.gb.m+(1|vignette), data=WMrepli, family=binomial(link="logit"))
nullrepli=glmer(other.chosen.gb~(1|vignette), data=WMrepli, family=binomial(link="logit"))
anova(fullrepli, nullrepli, test="Chisq")

summary(fullrepv)

# Data: WMrepli
# Models:
#   nullrepli: other.chosen.gb ~ (1 | vignette)
# fullrepli: other.chosen.gb ~ bdmingd.m * absbdmingd.m + own.gb.m + (1 | 
#                                                                       fullrepli:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# nullrepli  2 1308.6 1318.4 -652.32   1304.6                             
# fullrepli  6 1174.7 1203.9 -581.33   1162.7 141.98      4  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Check interaction

fullrepli2=glmer(other.chosen.gb~bdmingd.m+absbdmingd.m+own.gb.m+(1|vignette), data=WMrepli, family=binomial(link="logit"))
anova(fullrepli, fullrepli2, test="Chisq")

# Data: WMrepli
# Models:
#   fullrepli2: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                          fullrepli2:     vignette)
# fullrepli: other.chosen.gb ~ bdmingd.m * absbdmingd.m + own.gb.m + (1 | 
#                                                                       fullrepli:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# fullrepli2  5 1172.7 1197.0 -581.34   1162.7                         
# fullrepli   6 1174.7 1203.9 -581.33   1162.7 0.0134      1     0.9079

##No interaction between absolute difference and simple difference, remove from model
##and check for effect of bdmingd

fullrepli3=glmer(other.chosen.gb~absbdmingd.m+own.gb.m+(1|vignette), data=WMrepli, family=binomial(link="logit"))
anova(fullrepli3, fullrepli2, test="Chisq")

# Data: WMrepli
# Models:
#   fullrepli3: other.chosen.gb ~ absbdmingd.m + own.gb.m + (1 | vignette)
# fullrepli2: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                        fullrepli2:     vignette)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# fullrepli3  4 1171.7 1191.2 -581.84   1163.7                        
# fullrepli2  5 1172.7 1197.0 -581.34   1162.7 1.007      1     0.3156

#No effect of simple difference; -> original main effect failed to replicate

#check for effect of own.gb

fullrepli4=glmer(other.chosen.gb~bdmingd.m+absbdmingd.m+(1|vignette), data=WMrepli, family=binomial(link="logit"))
anova(fullrepli4, fullrepli2, test="Chisq")

# Data: WMrepli
# Models:
#   fullrepli4: other.chosen.gb ~ bdmingd.m + absbdmingd.m + (1 | vignette)
# fullrepli2: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                        fullrepli2:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# fullrepli4  4 1267.8 1287.3 -629.91   1259.8                             
# fullrepli2  5 1172.7 1197.0 -581.34   1162.7 97.141      1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#extremely significant effect of own choice; confirmation of this aspect of WMF replicated successfully.

fullrepli5=glmer(other.chosen.gb~bdmingd.m+own.gb.m+(1|vignette), data=WMrepli, family=binomial(link="logit"))
anova(fullrepli5, fullrepli2, test="Chisq")

# Data: WMrepli
# Models:
#   fullrepli5: other.chosen.gb ~ bdmingd.m + own.gb.m + (1 | vignette)
# fullrepli2: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                        fullrepli2:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# fullrepli5  4 1210.4 1229.9 -601.19   1202.4                             
# fullrepli2  5 1172.7 1197.0 -581.34   1162.7 39.712      1  2.943e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##also significant effect of absolute rating or extremeness; in this case may explain why 
##there is no overall effect of rating - extremeness in good direction counteracts rating effect in bad##


##Plots with with95% confidence intervals

names(ownothch)[names(ownothch) == "condition"] <- "vignette"

ggplot(ownothch, aes(y=rating, x=chosen.not, colour = chosen.not))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.05, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  geom_crossbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))+
  facet_grid(~own.other)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "Actions expected of another or oneself")


########################################################################

### FURTHER ANALYSES ###

########################################################################

#FilingCabinet#

WMrepli <- read.csv("WM_replication.csv", header = T, sep=";")

WMnew <-subset(WMrepli, original=="0")
WMorig <- subset(WMrepli, original=="1")

ownother_ch <-read.csv("ownother_ch.csv", header=T, sep=";")
plotorig <-subset(ownother_ch, original=="1")
plotnew <-subset(ownother_ch, original=="0")

###################
###Original Only###

##z-center vectors##

own.gb.n=as.numeric(WMorig$own.chosen.gb)
own.gb.m=own.gb.n-mean(own.gb.n)

bdmingd.n=as.numeric(WMorig$bdmingd)
bdmingd.m=bdmingd.n-mean(bdmingd.n)

absbdmingd.n=as.numeric(WMorig$abs.bdmingd)
absbdmingd.m=absbdmingd.n-mean(absbdmingd.n)

##Full Null##

fullorig=glmer(other.chosen.gb~bdmingd.m*absbdmingd.m+own.gb.m+(1|vignette), data=WMorig, family=binomial(link="logit"))
nullorig=glmer(other.chosen.gb~(1|vignette), data=WMorig, family=binomial(link="logit"))
anova(fullorig, nullorig, test="Chisq")

# Data: WMorig
# Models:
#   nullorig: other.chosen.gb ~ (1 | vignette)
# fullorig: other.chosen.gb ~ bdmingd.m * absbdmingd.m + own.gb.m + (1 | 
#                                                                      fullorig:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# nullorig  2 451.56 459.17 -223.78   447.56                             
# fullorig  6 426.33 449.16 -207.17   414.33 33.226      4  1.074e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Full-null is significant. Check for interaction

red1orig=glmer(other.chosen.gb~bdmingd.m+absbdmingd.m+own.gb.m+(1|vignette), data=WMorig, family=binomial(link="logit"))
anova(fullorig, red1orig, test="Chisq")

# Data: WMorig
# Models:
#   red1orig: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                        red1orig:     vignette)
# fullorig: other.chosen.gb ~ bdmingd.m * absbdmingd.m + own.gb.m + (1 | 
#                                                                      fullorig:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# red1orig  5 425.16 444.18 -207.58   415.16                         
# fullorig  6 426.33 449.16 -207.17   414.33 0.8255      1     0.3636

##no interaction. Remove from model. Compare models with and without bdmingd##

red2orig=glmer(other.chosen.gb~absbdmingd.m+own.gb.m+(1|vignette), data=WMorig, family=binomial(link="logit"))
anova(red1orig, red2orig, test="Chisq")

# Data: WMorig
# Models:
#   red2orig: other.chosen.gb ~ absbdmingd.m + own.gb.m + (1 | vignette)
# red1orig: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                      red1orig:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# red2orig  4 429.06 444.28 -210.53   421.06                           
# red1orig  5 425.16 444.18 -207.58   415.16 5.9072      1    0.01508 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###Primary main effect persists in original vignettes###

red3orig=glmer(other.chosen.gb~bdmingd.m+own.gb.m+(1|vignette), data=WMorig, family=binomial(link="logit"))
anova(red1orig, red3orig, test="Chisq")

# Data: WMorig
# Models:
#   red3orig: other.chosen.gb ~ bdmingd.m + own.gb.m + (1 | vignette)
# red1orig: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                      red1orig:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# red3orig  4 441.75 456.97 -216.87   433.75                             
# red1orig  5 425.16 444.18 -207.58   415.16 18.589      1  1.621e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##Also significant effect of absolute rating, or extremeness; however
##because this is included in the model when the effect of badness is tested
##it cannot explain the effect of badness.


#############
###New Only##

##z-center vectors##

own.gb.n=as.numeric(WMnew$own.chosen.gb)
own.gb.m=own.gb.n-mean(own.gb.n)

bdmingd.n=as.numeric(WMnew$bdmingd)
bdmingd.m=bdmingd.n-mean(bdmingd.n)

absbdmingd.n=as.numeric(WMnew$abs.bdmingd)
absbdmingd.m=absbdmingd.n-mean(absbdmingd.n)

## Full Null##

fullnew=glmer(other.chosen.gb~bdmingd.m*absbdmingd.m+own.gb.m+(1|vignette), data=WMnew, family=binomial(link="logit"))
nullnew=glmer(other.chosen.gb~(1|vignette), data=WMnew, family=binomial(link="logit"))
anova(fullnew, nullnew, test="Chisq")

# 
# Data: WMnew
# Models:
#   nullnew: other.chosen.gb ~ (1 | vignette)
# fullnew: other.chosen.gb ~ bdmingd.m * absbdmingd.m + own.gb.m + (1 | 
#                                                                     fullnew:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# nullnew  2 859.57 868.47 -427.78   855.57                             
# fullnew  6 739.97 766.69 -363.99   727.97 127.59      4  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##full-null significant. Check for effect of interaction###

red1new=glmer(other.chosen.gb~bdmingd.m+absbdmingd.m+own.gb.m+(1|vignette), data=WMnew, family=binomial(link="logit"))
anova(fullnew, red1new, test="Chisq")

# Data: WMnew
# Models:
#   red1new: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                       red1new:     vignette)
# fullnew: other.chosen.gb ~ bdmingd.m * absbdmingd.m + own.gb.m + (1 | 
#                                                                     fullnew:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# red1new  5 738.82 761.09 -364.41   728.82                         
# fullnew  6 739.97 766.69 -363.99   727.97 0.8479      1     0.3572


###No interaction. Check for effect of rating###

red2new=glmer(other.chosen.gb~absbdmingd.m+own.gb.m+(1|vignette), data=WMnew, family=binomial(link="logit"))
anova(red1new, red2new, test="Chisq")

# Data: WMnew
# Models:
#   red2new: other.chosen.gb ~ absbdmingd.m + own.gb.m + (1 | vignette)
# red1new: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                     red1new:     vignette)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# red2new  4 738.15 755.96 -365.07   730.15                        
# red1new  5 738.82 761.09 -364.41   728.82 1.328      1     0.2492

##No effect of rating. Check for effect of absolute rating###

red3new=glmer(other.chosen.gb~bdmingd.m+own.gb.m+(1|vignette), data=WMnew, family=binomial(link="logit"))
anova(red3new, red1new, test="Chisq")

# Data: WMnew
# Models:
#   red3new: other.chosen.gb ~ bdmingd.m + own.gb.m + (1 | vignette)
# red1new: other.chosen.gb ~ bdmingd.m + absbdmingd.m + own.gb.m + (1 | 
#                                                                     red1new:     vignette)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# red3new  4 756.85 774.67 -374.43   748.85                             
# red1new  5 738.82 761.09 -364.41   728.82 20.034      1   7.61e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

###Also effect of absolute rating; likely explains failure to find basic effect of rating###

###PLOTS###

ggplot(plotorig, aes(y=rating, x=chosen.not, colour = chosen.not))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.12, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  geom_crossbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))+
  facet_grid(~other.own)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "Actions expected in Original Vignettes from Replication")

ggplot(plotnew, aes(y=rating, x=chosen.not, colour = chosen.not))+
  geom_dotplot(binwidth = 0.5, binaxis='y', stackdir='center',stackratio=.1, shape = 21, alpha = 0.9, size = 4, fill = "white") +
  geom_crossbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96))+
  facet_grid(~other.own)+
  theme(text = element_text(size = 24), legend.position="none", axis.text.x=element_text(colour=c("black", "black"))) +
  scale_y_continuous(name = "Evaluation of Motives", breaks = seq(-10, 10, 1))+
  scale_x_discrete(name = "Actions expected in New Vignettes")



