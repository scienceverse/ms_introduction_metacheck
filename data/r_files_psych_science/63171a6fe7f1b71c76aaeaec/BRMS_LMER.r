## This script allows you to reproduce Bayesian distributional analyses and linear-mixed effect Gaussian analyses reported by 
# Lindsay, Gambi, & Rabagliati - Preschoolers optimize the timing of their conversational turns through flexible coordination of language comprehension and production
# Psychological Science, 2018

# This script allows you to run analyses that treat age as a categorical predictor, comparing adults to children and younger and older children.
# To run anlyses that treat age in month as a continuous predictor (children only), please see BRMS_LMER_AgeCont_oldparAug2018.R

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

## Prepare the dataset
tt.expt <- rbind(ads,threes,fives)
tt.expt$Subject <- paste(tt.expt$Age,tt.expt$Subject, sep = "")
tt.expt$Item = str_split_fixed(gsub(" ","_",tt.expt$SoundFile),"_",2)[,1]
tt.expt$Age <- as.factor(tt.expt$Age)
tt.expt$scale_CharacterLength.ms = scale(log(tt.expt$CharacterLength.ms))

# This is the age contrast used in the child-only analyses (Tables S1 and S3 in the Supplementary material)
tt.expt$AgeC = 0.5
tt.expt[tt.expt$Age == "Three",]$AgeC = -0.5

# This is the age contrast used in analyses comparing children to adults (Tables 1 and 2 in the paper)
tt.expt$Age1C = 0.5
tt.expt[tt.expt$Age1 == "Child",]$Age1C = -0.5

contrasts(tt.expt$Early.Late)[1] = -0.5
contrasts(tt.expt$Early.Late)[2] = 0.5
contrasts(tt.expt$Pred)[1] = -0.5
contrasts(tt.expt$Pred)[2] = 0.5
contrasts(tt.expt$Right.Wrong)[1] = -0.5
contrasts(tt.expt$Right.Wrong)[2] = 0.5

# The following code removes extreme outliers on the left-hand side of the distribution
tt.expt$rt_excl <- NA
for (i in levels(tt.expt$Age)){
  tt.expt[tt.expt$Age == i,]$rt_excl <- (tt.expt[tt.expt$Age == i,]$RT.ms - mean(tt.expt[tt.expt$Age == i,]$RT.ms))/(sd(tt.expt[tt.expt$Age == i,]$RT.ms))
}
tt.expt<- subset(tt.expt, rt_excl <= 5000 & rt_excl >= -1.5)

# The following code removes incorrect answers and discards any participant that yield fewer than 20 observations
summaryBy(RT.ms ~ Subject, data = tt.expt, FUN = length) -> n.trials
tt.expt <- subset(tt.expt, Subject %in% n.trials[n.trials$RT.ms.length >= 20,]$Subject & Accuracy == 1)

#Standardise RTs and add a constant to avoid negative values (overlaps)
tt.expt$rt <- (tt.expt$RT.ms - mean(tt.expt$RT.ms))/(2.5*sd(tt.expt$RT.ms))
tt.expt$rt <- tt.expt$rt + abs(min(tt.expt$rt))+0.1

# LMER Analysis (Table 3 in the paper)
library(lme4) # Analyses run in version 1.1-13
# Note - later versions of lmer may return convergence warnings but coefficient estimates should be similar
summary(lmer(log(rt) ~ Early.Late * Pred*Age1C+ scale(CharacterLength.ms)+Right.Wrong+ 
               (1+Early.Late|Subject) + (1+Early.Late * Pred|Item), 
             data = subset(tt.expt, RT.ms <1500)))

confint(lmer(log(rt) ~ Early.Late * Pred*Age1C+ scale(CharacterLength.ms)+Right.Wrong+ 
                   (1+Early.Late|Subject) + (1+Early.Late * Pred|Item), 
                 data = subset(tt.expt, RT.ms <1500)), method = "Wald")


# BRMS Analysis
# This is the model formula for the analysis reported in Table S1
expt.formula.child <- brmsformula(rt ~ 1  +AgeC  * Early.Late * Pred+ scale_CharacterLength.ms +Right.Wrong+(1+Early.Late||Subject) , 
                                   beta ~ 1  +AgeC  * Early.Late * Pred+ scale_CharacterLength.ms +Right.Wrong+(1+Early.Late||Subject) , 
                                   sigma ~ 1  +AgeC * Early.Late * Pred+ scale_CharacterLength.ms +Right.Wrong+(1+Early.Late||Subject) )

# This is the model formula for the analysis reported in Table 2
expt.formula.adult <- brmsformula(rt ~ 1  +Age1C  * Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject) , 
                                   beta ~ 1  +Age1C  * Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject) , 
                                   sigma ~ 1  +Age1C * Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject))

#Below are the model formulae for additional analyses (not reported in the paper)
expt.formula.noage <- brmsformula(rt ~ 1  +Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject) ,
                                    beta ~ 1  +Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject)  ,
                                    sigma ~ 1  +Early.Late * Pred+scale_CharacterLength.ms+Right.Wrong+(1+Early.Late||Subject))

expt.formula.child.Pred <- brmsformula(rt ~ 1 + Pred+scale_CharacterLength.ms+Right.Wrong+(1|Subject),
                                    beta ~ 1  +Pred+scale_CharacterLength.ms+Right.Wrong+(1|Subject) ,
                                    sigma ~ 1  +Pred+scale_CharacterLength.ms+Right.Wrong+(1|Subject))

expt.formula.Pred <- brmsformula(rt ~ 1 + Pred*Age1C+scale_CharacterLength.ms+Right.Wrong+(1|Subject),
                                         beta ~ 1  +Pred*Age1C+scale_CharacterLength.ms+Right.Wrong+(1|Subject) ,
                                         sigma ~ 1  +Pred*Age1C+scale_CharacterLength.ms+Right.Wrong+(1|Subject))

# This is the model formula for fitting a lognormal distribution in lieu of the exGaussian (not run below)
# expt.formula.lognormal.Pred <- brmsformula(rt ~ 1 + Pred+(1|Subject) ,
#                                          sigma ~ 1  +Pred+(1|Subject))



my_family = exgaussian(link = "identity", link_sigma = "log", link_beta = "log")

#my_log_family = lognormal(link = "identity", link_sigma = "log")

# The code below fits a number of hierachical Bayesian models

# only to the adult data (not reported in the paper)
expt.model_adults <- brm(expt.formula.noage,
                           data = subset(tt.expt, Age == "Adult"),
                           family = my_family, chains = 4,
                           inits="0")

# This fits the model reported in Table S1
expt.model_kids <- brm(expt.formula.child, 
                        data = subset(tt.expt, Age != "Adult"), 
                        family = my_family, chains = 4,
                        inits="0")

# This fits the model reported in Table 2
expt.model_adults_v_kids <- brm(expt.formula.adult, 
                                 data = tt.expt, 
                                 family = my_family, chains = 4, 
                                 inits="0")

# The code below fits a number of additional models (not reported in the paper)
# To the child data, only for trials were critical information occurs early
expt.model_kids.Early <- brm(expt.formula.child.Pred, 
                        data = subset(tt.expt, Age != "Adult" & Early.Late == "early"), 
                        family = my_family, chains = 4, 
                        inits="0")
# To the child data, only for trials were critical information occurs late
expt.model_kids.Late <- brm(expt.formula.child.Pred, 
                             data = subset(tt.expt, Age != "Adult" & Early.Late == "late"), 
                             family = my_family, chains = 4, 
                             inits="0")

# To both child and adult data, only for trials were critical information occurs early
expt.model.Early = brm(expt.formula.Pred, 
                        data = subset(tt.expt,  Early.Late == "early"), 
                        family = my_family, chains = 4, 
                        inits="0", control = list(adapt_delta = 0.95))

# To both child and adult data, only for trials were critical information occurs late
expt.model.Late = brm(expt.formula.Pred, 
                        data = subset(tt.expt,  Early.Late == "late"), 
                        family = my_family, chains = 4, 
                        inits="0", control = list(adapt_delta = 0.95))

# To the adult data, only for trials were critical information occurs early
expt.model_adult.Early <- brm(expt.formula.child.Pred, 
                              data = subset(tt.expt, Age == "Adult" & Early.Late == "early"), 
                              family = my_family, chains = 4, 
                              inits="0")
# To the adult data, only for trials were critical information occurs late
expt.model_adult.Late <- brm(expt.formula.child.Pred, 
                             data = subset(tt.expt, Age == "Adult" & Early.Late == "late"), 
                             family = my_family, chains = 4, 
                             inits="0")

# This fits a Gaussian distribution for comparison
expt.model_adult.Gaussian <- brm(bf(rt ~ 1 + Early.Late * Pred+(1+Early.Late|Subject) + (1|CharacterLength.ms), 
                                 sigma ~ 1  +Early.Late * Pred+(1+Early.Late|Subject) + (1|CharacterLength.ms)), 
                              data = subset(tt.expt, Age == "Adult" ), 
                              family = gaussian(), chains = 4, 
                              inits="0", control = list(adapt_delta = 0.95))


save(expt.model.Late,expt.model.Early,expt.model_kids.Late,expt.model_kids.Early,expt.model_adults_v_kids, expt.model_kids, 
     file = "BRMS_Dist_Dec28.RDATA")

