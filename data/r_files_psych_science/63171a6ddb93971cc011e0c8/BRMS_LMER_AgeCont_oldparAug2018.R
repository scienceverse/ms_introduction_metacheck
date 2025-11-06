## This script allows you to reproduce Bayesian distributional analyses and linear-mixed effect Gaussian analyses reported by 
# Lindsay, Gambi, & Rabagliati - Preschoolers optimize the timing of their conversational turns through flexible coordination of language comprehension and production
# Psychological Science, 2018

# This script allows you to run analyses that treat age in month as a continuous predictor (children only)
# To run anlyses that treat age as a categorical predictor, comparing adults to children and younger and older children, please see BRMS_LMER.r

######## IMPORTANT NOTE ###########
# The following code will reproduce the analyses reported in the paper
# only if you are using a version of package "brms" prior to the 13th April 2018 update (<2.2.0)
# This is because brms version 2.2.0 introduced a new parametrization of the exgauassian distribution
# In the old parametrization (implemented till version 2.1.0, and used in the analyses reported in the paper)
# the main predictor in the exgaussian famility corresponds to mu, the mean of the Gaussian component
# In the new parametrization (implemented from version 2.2.0), the main predictor is instead the
# mean of the exgaussian distribution (mu + beta, where beta is the tau paramter)

#If you want to reproduce our analyses, you have two options
# Option 1
## Install brms version 2.1.0 and run the code below. If so, run the following code first:
require(devtools)
remove.packages("brms") # this removes the current installation of brms from your system
install_version("brms", version="2.1.0",repos="http://cran.us.r-project.org")
library(brms)
#Option 2
## Use the latest version of brms and implement a custom family distribution which reproduces the old parameterization of the exGaussian
## If you would like to go for option 2, please see the script: BRMS_Custom_Family.r
# Feel free to get in touch with gambic@cardiff.ac.uk if you have any questions

library(stringr)
library(retimes)
library(rstan)
library(ggplot2)
library(doBy)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

ads <- read.csv("adults.csv")
fives <- read.csv("fives.csv")
threes <- read.csv("threes.csv")

ads$Age <- "Adult"
fives$Age <- "Five"
threes$Age <- "Three"

ads$Age1 <- "Adult"
fives$Age1 <- "Child"
threes$Age1 <- "Child"

##Legend
# Subject: subject numerical identifier
# Level: block (level of the maze game)
# TrialNo: progressive trial number, within a block
# Pred: Maze Type (Pred = predictable one-animal maze; Unpred = unpredictable two-animal maze)
# Early.Late: Information Structure (early = character name is mentioned early "Is Po hiding the tiger?" vs, late = character name is mentioned late "Is the tiger behind Po?") 
# Right.Wrong: Answer Type (right = required answer is yes; wrong = required answer is no) 
# Length: factor, is the character name a short or a long word
# SoundFile: name of the sound file played on the trial
# QuLength: Length of the question in seconds.
# CharacterLength: Length of the final word of the question in seconds (i.e., the character name in the late condition, the animal name in the early condition)
# QuOnset: onset of question from beginning of recording (in seconds)
# QuOffset: offset of question from beginning of recording (in seconds)
# Response.Onset: onset of participant's response from beginning of recording (in seconds)
# RT.s: response time (in seconds)
# QuLength.ms: Length of the question in milliseconds
# CharacterLength.ms: Length of the final word of the question in milliseconds (i.e., the character name in the late condition, the animal name in the early condition)
# QuOnset.ms: onset of question from beginning of recording (in milliseconds)        
# QuOffset.ms: offset of question from beginning of recording (in milliseconds)     
# ResponseOnset.ms: onset of participant's response from beginning of recording (in milliseconds)
# RT.ms: response time (in milliseconds)           
# Response: What did the participant respond?
# Accuracy: did the participant respond accurately? (yes to a right answer and no to a wrong answer)
# Comments: additional notes (e.g., background noise, difluent responses)


#### Now add age in months for children
agelist3yo<-read.csv("3yo_agelist.csv",header=T)#71 (included 58)
agelist5yo<-read.csv("5yo_agelist.csv",header=T)#51 (included 48)

names(agelist3yo)[2]<-"AgeMonth"
summary(threes)#2515
threes<-merge(threes,agelist3yo,by=c("Subject"),sort=F)
summary(threes)
names(agelist5yo)[2]<-"AgeMonth"
summary(fives)#3014
fives<-merge(fives,agelist5yo,by=c("Subject"),sort=F)
summary(fives)

# do the same for adults, so we can still combine the datasets
agelistads<-read.csv("adult_agelist.csv",header=T)# should be 48
summary(agelistads)
names(agelistads)[2]<-"AgeMonth"
agelistads$X<-NA
summary(ads)#4601

ads<-merge(ads,agelistads,by=c("Subject"),sort=F)
summary(ads)


tt.expt <- rbind(ads,threes,fives)
tt.expt$Subject <- paste(tt.expt$Age,tt.expt$Subject, sep = "")
tt.expt$Item = str_split_fixed(gsub(" ","_",tt.expt$SoundFile),"_",2)[,1]
tt.expt$Age <- as.factor(tt.expt$Age)
tt.expt$scale_CharacterLength.ms = scale(log(tt.expt$CharacterLength.ms))

tt.expt$AgeC = 0.5
tt.expt[tt.expt$Age == "Three",]$AgeC = -0.5

tt.expt$Age1C = 0.5
tt.expt[tt.expt$Age1 == "Child",]$Age1C = -0.5

contrasts(tt.expt$Early.Late)[1] = -0.5
contrasts(tt.expt$Early.Late)[2] = 0.5
contrasts(tt.expt$Pred)[1] = -0.5
contrasts(tt.expt$Pred)[2] = 0.5
contrasts(tt.expt$Right.Wrong)[1] = -0.5
contrasts(tt.expt$Right.Wrong)[2] = 0.5

#prop of incorrect responses (does not include disfluent response) by age group
#(1-summaryBy(Accuracy~Age,data=tt.expt))*100
# > summaryBy(Accuracy~Age,data=tt.expt)
# Age Accuracy.mean
# 1 Adult     0.9919583
# 2  Five     0.9585269
# 3 Three     0.8214712
# > (1-summaryBy(Accuracy~Age,data=tt.expt))*100
# Age Accuracy.mean
# 1  NA      0.804173
# 2  NA      4.147313
# 3  NA     17.852883

tt.expt$rt_excl <- NA
for (i in levels(tt.expt$Age)){
  tt.expt[tt.expt$Age == i,]$rt_excl <- (tt.expt[tt.expt$Age == i,]$RT.ms - mean(tt.expt[tt.expt$Age == i,]$RT.ms))/(sd(tt.expt[tt.expt$Age == i,]$RT.ms))
}
#perc outliers by Agegroup
# tt.expt$Outlier<-0
# tt.expt$Outlier[tt.expt$rt_excl > 5000 | tt.expt$rt_excl < -1.5]<-1
# summaryBy(Outlier~Age,data=tt.expt)*100
# Age Outlier.mean
# 1 Adult  0.008259074
# 2  Five  0.007631055
# 3 Three  0.002783300

tt.expt<- subset(tt.expt, rt_excl <= 5000 & rt_excl >= -1.5)


summaryBy(RT.ms ~ Subject, data = tt.expt, FUN = length) -> n.trials
# number of participants excluded by age group
# tt.expt<- subset(tt.expt, Accuracy == 1)
# unique(n.trials[n.trials$RT.ms.length < 20,]$Subject)#"Three12" "Three33" "Three35" "Three50" "Three61

tt.expt <- subset(tt.expt, Subject %in% n.trials[n.trials$RT.ms.length >= 20,]$Subject & Accuracy == 1)

tt.expt$rt <- (tt.expt$RT.ms - mean(tt.expt$RT.ms))/(2.5*sd(tt.expt$RT.ms))
tt.expt$rt <- tt.expt$rt + abs(min(tt.expt$rt))+0.1

# # number of included trials per age group
# summaryBy(rt~Age, data= tt.expt,FUN=length)
# Age rt.length
# 1 Adult      4528
# 2  Five      2867
# 3 Three      1996

#### brms analyses (children) with Age in month (continuous scaled and centred predictor); see Table S2 in the Supplemntary material.
tt.expt.c<-tt.expt[tt.expt$Age!="Adult",]
tt.expt.c$AgeMonthC<-scale(tt.expt.c$AgeMonth,T,T)

expt2.formula.child.cont <- brmsformula(rt ~ 1  +AgeMonthC  * Early.Late * Pred+ scale_CharacterLength.ms +Right.Wrong+(1+Early.Late||Subject) , 
                                   beta ~ 1  +AgeMonthC  * Early.Late * Pred+ scale_CharacterLength.ms +Right.Wrong+(1+Early.Late||Subject) , 
                                   sigma ~ 1  +AgeMonthC * Early.Late * Pred+ scale_CharacterLength.ms +Right.Wrong+(1+Early.Late||Subject) )
my_family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log")
expt2.model_kids.cont <- brm(expt2.formula.child.cont, 
                        data = tt.expt.c, 
                        family = my_family, chains = 4,
                        inits="0")
save(expt2.model_kids.cont, file = "BRMS_Dist_Aug18_Agecont_version2.1.0.RDATA")
summary(expt2.model_kids.cont)


###lmer analyses using age in months (see Table S4 in the Supplementary material)
summary(lmer(log(rt) ~ Early.Late * Pred*AgeMonthC+ scale(CharacterLength.ms)+Right.Wrong+ 
               (1+Early.Late|Subject) + (1+Early.Late * Pred|Item), 
             data = subset(tt.expt.c, RT.ms <1500)))
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)                         -0.424457   0.015930 -26.645
# Early.Latelate                       0.195652   0.017565  11.139
# PredUnpred                           0.085016   0.028952   2.936
# AgeMonthC                           -0.015547   0.013620  -1.141
# scale(CharacterLength.ms)           -0.074787   0.007552  -9.903
# Right.Wrongwrong                     0.046204   0.014811   3.119
# Early.Latelate:PredUnpred           -0.064648   0.028521  -2.267
# Early.Latelate:AgeMonthC            -0.005349   0.011331  -0.472
# PredUnpred:AgeMonthC                -0.027997   0.027223  -1.028
# Early.Latelate:PredUnpred:AgeMonthC -0.018782   0.022634  -0.830

confint(lmer(log(rt) ~ Early.Late * Pred*AgeMonthC+ scale(CharacterLength.ms)+Right.Wrong+ 
               (1+Early.Late|Subject) + (1+Early.Late * Pred|Item), 
             data = subset(tt.expt.c, RT.ms <1500)), method="Wald")

# (Intercept)                         -0.45567902 -0.393235311
# Early.Latelate                       0.16122417  0.230078860
# PredUnpred                           0.02827047  0.141761351
# AgeMonthC                           -0.04224113  0.011147487
# scale(CharacterLength.ms)           -0.08958949 -0.059984917
# Right.Wrongwrong                     0.01717415  0.075233396
# Early.Latelate:PredUnpred           -0.12054804 -0.008748256
# Early.Latelate:AgeMonthC            -0.02755706  0.016859812
# PredUnpred:AgeMonthC                -0.08135320  0.025358696
# Early.Latelate:PredUnpred:AgeMonthC -0.06314411  0.025580273