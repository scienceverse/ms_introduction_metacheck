# Title: Physical effort exertion for peer feedback reveals evolving social motivations from adolescence to young adulthood
# Authors: [blinded]
# Code written by [blinded]
# Last updated: 07/1/22 | FIGURES AND TABLES

########################################################
#### libraries and load data ####
clc <- function() {
  rm(list = ls(envir = globalenv()),envir = globalenv()) ## clear Vars from global enviroment
  gc()  ## garbage colector
  cat("\014") ## clc
  .rs.restartR() ## clear session
}    ## this function cleans the environment and restarts R
clc()

ipak <- function (pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("pscl", "lme4", "reghelper","MuMIn", "Hmisc", "ISLR", "sjPlot",
              "mgcv","itsadug", "ggplot2", "psych", "mgcViz","ggeffects")

ipak(packages) # May need up to date R to load all packages (R v. 3.6.3 used for these analyses)

load(file = "SocaMotiveLong_OSF.RData")
load(file = "SocaMoneyMotive_OSF.RData")

##############################################################################
##                                                                          ##
## ----------------- FIG 2: Money vs. Social Task by Age ------------------ ##
##                                                                          ##
##############################################################################
# As explained in the manuscript, ti() for categorical predictors computes a smoothed fit on each level, but
# it does not provide a direct statistical test of the interaction term. Instead, we use itsadug to inspect the 
# relative difference between fits by task and compute 95% CI around it.

#### PEAK GRIP #### 
#Models rerun without centering age for plotting purposes.
peak.ms.int.gam.plot<-gamm(Peak_Wins ~ Task + ti(Age,by=Task) + Difficulty + Max + TrialTotal,
                           random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)

## Plotting relative difference in smoothed fits of age for social and money tasks with 95% CI 
#(this accounts for the correlation between errors, so its ok if the CI are overlapping in a marginal effects plot with both functions[as in plot_model])
class(peak.ms.int.gam.plot$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gamm
plot_diff(peak.ms.int.gam.plot$gam, view = "Age",comp = list(Task = c("Social", "Money")),col.diff = "royalblue4",col = "royalblue1", ylim=c(-1,3), cond=list(Difficulty=0))

#### SPEED ####
#Models rerun without centering age for plotting purposes.
speed.ms.int.gam.plot<-gamm(Speed_log_Wins ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal, 
                            random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)

## Plotting relative difference in smoothed fits of age for social and money tasks with 95% CI 
#(this accounts for the correlation between errors, so its ok if the CI are overlapping in a marginal effects plot with both functions[as in plot_model])
class(speed.ms.int.gam.plot$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gamm
plot_diff(speed.ms.int.gam.plot$gam, view = "Age",comp = list(Task = c("Money", "Social")),col.diff = "darkgreen",col = "forestgreen", ylim=c(-.3,0), cond=list(Difficulty=0))

#### OPT OUT ####
#Models rerun without centering age for plotting purposes.
optout.ms.int.gam.plot<-gamm(OptOut ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal, 
                             random=list(Subject=~1), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)

## Plotting relative difference in smoothed fits of age for social and money tasks with 95% CI 
#(this accounts for the correlation between errors, so its ok if the CI are overlapping in a marginal effects plot with both functions[as in plot_model])
class(optout.ms.int.gam.plot$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gamm
plot_diff(optout.ms.int.gam.plot$gam, view = "Age",comp = list(Task = c("Money", "Social")),col.diff = "indianred",col = "indianred3", ylim=c(-1,6), cond=list(Difficulty=0))

##############################################################################
##                                                                          ##
## ----------------- FIG 3: Acceptance Expectancy by Age ------------------ ##
##                                                                          ##
##############################################################################
# Heatmaps are easily influenced by extreme values. Thus, in Fig 3 and 4, the predictors expectancy and desirability were winsorized (+/- 3SD) 
# so the displayed patterns were not overly impacted by outliers. 

#### PEAK GRIP #### 
#Models rerun without centering age for plotting purposes.
peak.p.int.gam.plot<-gamm(Peak_Wins ~ s(Age) + s(Predictionz_Wins) + ti(Age, Predictionz_Wins) + Difficulty + Max + TrialNumber,
                         random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)

fvisgam(peak.p.int.gam.plot$gam, view=c("Age", "Predictionz_Wins"), xlim = c(12,24),
        labcex=.1,contour.col = "black",add.color.legend = F,lwd = .5,cond=list(Difficulty=0),
        color = c("white","steelblue1","black"),show.diff = T, col.diff = "red", alpha.diff=.3,sim.ci=T)

#### SPEED ####
#Models rerun without centering age for plotting purposes.
speed.p.int.gam.plot<-gamm(Speed_log_Wins ~ s(Age) + s(Predictionz_Wins) + ti(Age, Predictionz_Wins) + Difficulty + Max + TrialNumber,
                           random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)

fvisgam(speed.p.int.gam.plot$gam, view=c("Age", "Predictionz_Wins"),xlim = c(12,24),
        labcex=.1,contour.col = "black",add.color.legend = F,lwd = .5,cond=list(Difficulty=0),
        color = c("black","forestgreen","white"),show.diff = T, col.diff = "red", alpha.diff=.3,sim.ci=T)

#### OPT OUT ####
#Models rerun without centering age for plotting purposes.
optout.p.int.gam.plot<-gamm(OptOut ~ s(Age) + s(Predictionz_Wins) + ti(Age, Predictionz_Wins) + Difficulty + Max + TrialNumber,
                            random=list(Subject=~1), family=binomial, data=SocaMotiveLong_OSF, na.action=na.omit)

fvisgam(optout.p.int.gam.plot$gam, view=c("Age", "Predictionz_Wins"),xlim = c(12,24),
        labcex=.1,contour.col = "black",add.color.legend = F,lwd = .5,cond=list(Difficulty=0),
        color = c("black","indianred2","white"),show.diff = T, col.diff = "green", alpha.diff=.3,sim.ci=T)


##############################################################################
##                                                                          ##
## ----------------- FIG 4: Peer Desirability by Age ---------------------- ##
##                                                                          ##
##############################################################################
# Heatmaps are easily influenced by extreme values. Thus, in Fig 3 and 4, the predictors expectancy and desirability were winsorized (+/- 3SD) 
# so the displayed patterns were not overly impacted by outliers. (NB: there was one exception for the plot displaying the effect of age and 
# desirability on opt-out behavior, whereby the winsorized value resulted in a nonlinear pattern, deviating from the original model estimation of 
# a linear pattern; thus, the use of the non-winsorized value of desirability was retained). 

#### PEAK GRIP ####
#Models rerun without centering age for plotting purposes.
peak.r.int.gam.plot<-gamm(Peak_Wins ~ s(Age) + s(Ratingz_Wins) + ti(Age, Ratingz_Wins) + Difficulty + Max + TrialNumber, 
                          random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)

fvisgam(peak.r.int.gam.plot$gam, view= c("Age", "Ratingz_Wins"), xlim = c(12,24),
        labcex=.1,contour.col = "black",add.color.legend = F,lwd = .5,cond=list(Difficulty=0),
        color = c("white","steelblue1","black"),show.diff = T, col.diff = "red", alpha.diff=.3,sim.ci=T)

#### SPEED ####
#Models rerun without centering age for plotting purposes.
speed.r.int.gam.plot<-gamm(Speed_log_Wins ~ s(Age) + s(Ratingz_Wins) + ti(Age, Ratingz_Wins) + Difficulty + Max + TrialNumber, 
                           random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)

fvisgam(speed.r.int.gam.plot$gam, view= c("Age", "Ratingz_Wins"), xlim = c(12,24),
        labcex=.1,contour.col = "black",add.color.legend = F,lwd = .5,cond=list(Difficulty=0),
        color = c("black","forestgreen","white"),show.diff = T, col.diff = "red", alpha.diff=.3,sim.ci=T)

#### OPT OUT ####
#Models rerun without centering age for plotting purposes. Plot computed with non-winsorized term, see note above.
optout.r.int.gam.plot<-gamm(OptOut ~ s(Age) + s(Ratingz) + ti(Age, Ratingz) + Difficulty + Max + TrialNumber,
                            random=list(Subject=~1),family=binomial,data = SocaMotiveLong_OSF,na.action=na.exclude)

fvisgam(optout.r.int.gam.plot$gam, view= c("Age", "Ratingz"), xlim = c(12,24),
        labcex=.1,contour.col = "black",add.color.legend = F,lwd = .5,cond=list(Difficulty=0),
        color = c("black","indianred2","white"),show.diff = F, col.diff = "green", alpha.diff=.3,sim.ci=T)

##############################################################################
##                                                                          ##
## ------------------ SUPPLEMENTAL FIGURES/TABLES  ------------------------ ##
##                                                                          ##
##############################################################################

#### --------------- FIGURE S1: HISTOGRAM OF AGE DISTRIBUTION -----------------####
x<-sort(SocaMoneyMotive_OSF$Age[SocaMoneyMotive_OSF$TrialTotal==1])
hist(x, ylim=c(0,20), col = "white")

#### --------------- FIGURE S3: MAIN EFFECT OF EXPECTANCY ON OUTCOMES -----------------####
#### PEAK GRIP ####
#Models rerun without centering age for plotting purposes.
peak.p.me.gam.plot<-gamm(Peak_Wins ~ s(Predictionz_Wins) + s(Age) + Difficulty + Max + TrialNumber, 
                           random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
plot_model(model=peak.p.me.gam.plot, type = "pred",terms = "Predictionz_Wins[all]", colors = c("royalblue1"), ci.lvl=.475,axis.lim = c(66,76))

#### SPEED ####
#Models rerun without centering age for plotting purposes.
speed.p.me.gam.plot<-gamm(Speed_log_Wins ~ s(Predictionz_Wins) + s(Age) + Difficulty + Max + TrialNumber,
                            random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)

plot_model(model=speed.p.me.gam.plot, type = "pred",terms = "Predictionz_Wins[all]", colors = c("forestgreen"), ci.lvl=.475,axis.lim = c(6.6,6.9))

#### OPT OUT ####
# Models rerun without centering age for plotting purposes. Using the predictor Prediction_Wins resulted in a nonlinear pattern, deviating from the original model estimation of a linear pattern; thus, the use of the term Predictionz_Wins was held linear (unsmoothed).
optout.p.me.gam.plot<-gamm(OptOut ~ Predictionz_Wins + s(Age) + Difficulty + Max + TrialNumber, 
                             random=list(Subject=~1), family=binomial,data=SocaMotiveLong_OSF, na.action = na.omit)
plot_model(model=optout.p.me.gam.plot, type = "pred",terms = "Predictionz_Wins[all]", colors = c("indianred3"), ci.lvl=.475,axis.lim = c(0,.006))

#### --------------- FIGURE S4: MAIN EFFECTS OF DESIRABILITY ON OUTCOMES -----------------####
#### PEAK GRIP ####
#Models rerun without centering age for plotting purposes.
peak.r.me.gam.plot<-gamm(Peak_Wins ~ s(Ratingz_Wins) + s(Age) + Difficulty + Max + TrialNumber, 
                           random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
plot_model(model=peak.r.me.gam.plot, type = "pred",terms = "Ratingz_Wins[all]", colors = c("royalblue1"), ci.lvl=.475,axis.lim  = c(66,74))

#### SPEED ####
#Models rerun without centering age for plotting purposes.
speed.r.me.gam.plot<-gamm(Speed_log_Wins ~ s(Ratingz_Wins) + s(Age) +  Difficulty + Max + TrialNumber,
                            random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
plot_model(model=speed.r.me.gam.plot, type = "pred",terms = "Ratingz_Wins[all]",  colors = c("forestgreen"), ci.lvl=.475,axis.lim  =  c(6.5,6.9))

#### OPT OUT ####
# Models rerun without centering age for plotting purposes. Using the predictor Ratingz_Wins resulted in a nonlinear pattern, deviating from the original model estimation of a linear pattern; thus, the use of the term Ratingz_Wins was held linear (unsmoothed).
optout.r.me.gam.plot<-gamm(OptOut ~ Ratingz_Wins + s(Age) + Difficulty + Max + TrialNumber, 
                             random=list(Subject=~1), family=binomial,data = SocaMotiveLong_OSF,na.action=na.omit)

plot_model(model=optout.r.me.gam.plot, type = "pred",terms = "Ratingz_Wins[all]", colors = c("indianred3"), ci.lvl=.475,axis.lim  =  c(0,.008))

#### --------------- FIGURE S5: DESIRABILITY/EXPECTANCY (RAW) BY AGE -----------------####

raw.p.age<-lme(Prediction ~ Age, random=list(Subject=~1), data=SocaMotiveLong_OSF,na.action=na.omit)
plot_model(model=raw.p.age, type = "pred",terms = "Age[all]", ci.lvl=.475,axis.lim=c(0,100))

raw.r.age<-lme(Rating ~ Age, random=list(Subject=~1), data=SocaMotiveLong_OSF,na.action=na.omit)
plot_model(model=raw.r.age, type = "pred",terms = "Age[all]", ci.lvl=.475,axis.lim=c(0,100))

#### --------------- FIGURE S6: DESIRABILITY/EXPECTANCY (RAW) BY AGE*GENDER*PEER GENDER -----------------####

raw.p.age.g.pg<-lme(Prediction ~ Age*StimGender*Gender, random=list(Subject=~1), data=SocaMotiveLong_OSF,na.action=na.omit)
plot_model(model=raw.p.age.g.pg, type = "pred",terms = c("Age[all]","StimGender","Gender"), ci.lvl=.475,axis.lim=c(0,100))

raw.r.age.g.pg<-lme(Rating ~ Age*StimGender*Gender, random=list(Subject=~1), data=SocaMotiveLong_OSF,na.action=na.omit)
plot_model(model=raw.r.age.g.pg, type = "pred",terms = c("Age[all]","StimGender","Gender"), ci.lvl=.475,axis.lim=c(0,100))

#### --------------- FIGURE S7: MONEY VS. SOCIAL TASKS BY AGE INCLUDING RARNDOM SLOPE OF TASK-----------------####
#### PEAK GRIP #### 
#Models rerun without centering age for plotting purposes.
peak.ms.int.gam.rs.plot<-gamm(Peak_Wins ~ Task + ti(Age,by=Task) + Difficulty + Max + TrialTotal,
                           random=list(Subject=~1+Task), data=SocaMoneyMotive_OSF, na.action = na.omit)

## Plotting relative difference in smoothed fits of age for social and money tasks with 95% CI 
#(this accounts for the correlation between errors, so its ok if the CI are overlapping in a marginal effects plot with both functions[as in plot_model])
class(peak.ms.int.gam.rs.plot$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gamm
plot_diff(peak.ms.int.gam.rs.plot$gam, view = "Age",comp = list(Task = c("Social", "Money")),col.diff = "royalblue4",col = "royalblue1", ylim=c(-1,5), cond=list(Difficulty=0))

#### SPEED ####
#Models rerun without centering age for plotting purposes.
speed.ms.int.gam.rs.plot<-gamm(Speed_log_Wins ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal, 
                            random=list(Subject=~1+Task), data=SocaMoneyMotive_OSF, na.action = na.omit)

## Plotting relative difference in smoothed fits of age for social and money tasks with 95% CI 
#(this accounts for the correlation between errors, so its ok if the CI are overlapping in a marginal effects plot with both functions[as in plot_model])
class(speed.ms.int.gam.rs.plot$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gamm
plot_diff(speed.ms.int.gam.rs.plot$gam, view = "Age",comp = list(Task = c("Money", "Social")),col.diff = "darkgreen",col = "forestgreen", ylim=c(-.3,0), cond=list(Difficulty=0))

#### OPT OUT ####
#Models rerun without centering age for plotting purposes.
optout.ms.int.gam.rs.plot<-gamm(OptOut ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal, 
                             random=list(Subject=~1+Task), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)

## Plotting relative difference in smoothed fits of age for social and money tasks with 95% CI 
#(this accounts for the correlation between errors, so its ok if the CI are overlapping in a marginal effects plot with both functions[as in plot_model])
class(optout.ms.int.gam.rs.plot$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gamm
plot_diff(optout.ms.int.gam.rs.plot$gam, view = "Age",comp = list(Task = c("Money", "Social")),col.diff = "indianred",col = "indianred3", ylim=c(-1,6), cond=list(Difficulty=0))

#### --------------- TABLE S1: FATIGUE CONTROL ANALYSES ---------------- ##########
## Including Pre-Post Task Max Calibration (Fatigue) as a control variable for Money vs. Social Targets
## PEAK GRIP ##
peak.ms.int.f.gam<-gamm(Peak_Wins ~ Task + ti(AgeC,by=Task) + Difficulty + Max + TrialTotal + Max3Max1Diff,
                        random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
#plotting and calculate 95% CI relative difference in money vs. social task
class(peak.ms.int.f.gam$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gam
plot_diff(peak.ms.int.f.gam$gam, view = "AgeC", comp = list(Task = c("Social", "Money")),col.diff = "royalblue4",col = "royalblue1", ylim=c(-1,4))
#findings hold

## SPEED ##
speed.ms.int.f.gam<-gamm(Speed_log_Wins ~ Task + ti(AgeC,by=Task) + Difficulty + Max + TrialTotal + Max3Max1Diff, 
                         random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
#plotting and calculate 95% CI relative difference in money vs. social task
class(speed.ms.int.f.gam$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gam
plot_diff(speed.ms.int.f.gam$gam, view = "AgeC", comp = list(Task = c("Money", "Social")),col.diff = "darkgreen",col = "forestgreen", ylim=c(-.3,0))
#findings hold

## OPT OUT ##
optout.ms.int.f.gam<-gamm(OptOut ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal + Max3Max1Diff, 
                          random=list(Subject=~1), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)
#plotting and calculate 95% CR relative difference in money vs. social task
class(optout.ms.int.f.gam$gam) <- c("gam", "glm", "lm")     ## little hack to make plot_diff work for gam
plot_diff(optout.ms.int.f.gam$gam, view = "Age", comp = list(Task = c("Money", "Social")), se = 1.96,col.diff = "indianred",col = "indianred")#, ylim=c(-1,4))
#findings hold

#### --------------- TABLE S3: DESCRIPTIVES OF EXCTRACTED MARGINAL MEANS-----------------####
# Marginal means for Table S3 extracted at various points of age and predictors (each level for categorical variables; min, mean, max for continuous variables).

#### MONEY VS. SOCIAL TASK BY AGE ####
## PEAK GRIP ##
peak.ms.int.gam.mm<-gamm(Peak_Wins ~ Task + ti(Age,by=Task) + Difficulty + Max + TrialTotal,
                           random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
# Marginal Means extraction (Table S3) using points of non-centered age (min, mean, max) at each level of the task
ggpredict(peak.ms.int.gam.mm$gam, terms = c("Task", "Age[12.03, 18.20, 23.77]"))
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text.

## SPEED ##
speed.ms.int.gam.mm<-gamm(Speed_log_Wins ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal, 
                            random=list(Subject=~1), data=SocaMoneyMotive_OSF, na.action = na.omit)
# Marginal Means extraction (Table S3) using points of non-centered age (min, mean, max) at each level of the task
ggpredict(speed.ms.int.gam.mm$gam, terms = c("Task", "Age[12.03, 18.20, 23.77]"))
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text (and back-transformed from log using exp()).

## OPT OUT ##
#Models rerun without centering age for plotting purposes.
optout.ms.int.gam.mm<-gamm(OptOut ~ Task + ti(Age, by=Task) + Difficulty + Max + TrialTotal, 
                             random=list(Subject=~1), family=binomial(link=logit),data = SocaMoneyMotive_OSF, na.action=na.omit)
# Marginal Means extraction (Table S3) using points of non-centered age (min, mean, max) at each level of the task
ggpredict(optout.ms.int.gam.mm$gam, terms = c("Task", "Age[12.03, 18.20, 23.77]"))
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text.

#### EXPECTANCY BY AGE ####
## PEAK GRIP ##
# Marginal Means extraction (Table S3) using points of non-centered age at points of predictor of interest (min, mean, max)
peak.p.int.gam.mm<-gamm(Peak_Wins ~ s(Predictionz) + s(Age) + ti(Age, Predictionz) + Difficulty + Max + TrialNumber, 
                        random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
ggpredict(peak.p.int.gam.mm$gam, terms = c("Predictionz[-5.025099,0,3.789813]", "Age[12.03, 18.20, 23.77]"))   
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text.

## SPEED ##
# Marginal Means extraction (Table S3) using points of non-centered age at points of predictor of interest (min, mean, max)
speed.p.int.gam.mm<-gamm(Speed_log_Wins ~ s(Predictionz) + s(Age) + ti(Age, Predictionz) + Difficulty + Max + TrialNumber, 
                         random=list(Subject=~1),data=SocaMotiveLong_OSF, na.action = na.omit)
ggpredict(speed.p.int.gam.mm$gam, terms = c("Predictionz[-5.025099,0,3.789813]", "Age[12.03, 18.20, 23.77]"))   
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text (and back-transformed from log using exp()).

## OPT OUT ##
# Marginal Means extraction (Table S3) using points of non-centered age at points of predictor of interest (min, mean, max)
optout.p.int.gam.mm<-gamm(OptOut ~ s(Predictionz) + s(Age) + ti(Age, Predictionz) + Difficulty + Max + TrialNumber, 
                          random=list(Subject=~1), family=binomial(link=logit),data = SocaMotiveLong_OSF,na.action=na.omit)
ggpredict(optout.p.int.gam.mm$gam, terms = c("Predictionz[-5.025099,0,3.789813]", "Age[12.03, 18.20, 23.77]"))   
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text.

#### DESIRABILITY BY AGE ####
## PEAK GRIP ##
# Marginal Means extraction (Table S3) using points of non-centered age at points of predictor of interest (min, mean, max)
peak.r.int.gam.mm<-gamm(Peak_Wins ~ s(Ratingz) + s(Age) + ti(Age, Ratingz) + Difficulty + Max + TrialNumber, 
                        random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
ggpredict(peak.r.int.gam.mm$gam, terms = c("Ratingz[-6.461911,0,3.608695]", "Age[12.03, 18.20, 23.77]"))   
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text.

## SPEED ##
# Marginal Means extraction (Table S3) using points of non-centered age at points of predictor of interest (min, mean, max)
speed.r.int.gam.mm<-gamm(Speed_log_Wins ~ s(Ratingz) + s(Age) + ti(Age, Ratingz) + Difficulty + Max + TrialNumber, 
                         random=list(Subject=~1), data=SocaMotiveLong_OSF, na.action = na.omit)
ggpredict(speed.r.int.gam.mm$gam, terms = c("Ratingz[-6.461911,0,3.608695]", "Age[12.03, 18.20, 23.77]"))   
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text (and back-transformed from log using exp()).

# Marginal Means extraction (Table S3) using points of non-centered age at points of predictor of interest (min, mean, max)
optout.r.int.gam.mm<-gamm(OptOut ~ s(Ratingz) + s(AgeC) +ti(AgeC, Ratingz) + Difficulty + Max + TrialNumber, 
                          random=list(Subject=~1), family=binomial, data=SocaMotiveLong_OSF,na.action=na.omit)
ggpredict(optout.r.int.gam.mm$gam, terms = c("Ratingz[-6.461911,0,3.608695]", "Age[12.03, 18.20, 23.77]"))   
#Age[all] was used to extract marginal means at all ages to calculate descriptions of differences across various age ranges in the text.
