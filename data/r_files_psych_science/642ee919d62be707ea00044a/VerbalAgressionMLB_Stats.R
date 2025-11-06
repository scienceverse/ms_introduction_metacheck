###load libraries
library(mgcv)
library(parallel)
library(splancs)
library(PBSmapping)
library(readxl)
library(ggeffects)
library(dplyr)
library(tidyverse)
library(emmeans)
library(lme4)
library(car)
library(rms)
library(report)

###Data preparation

################Load Data 2010-2019 & Create sample for games with ejections in pbpEjectReason###################
pbpEjectReason <- read_csv("pbpFinalWithCatcher.csv", col_names = TRUE)
head(pbpEjectReason)

#Keep only pitches : inside 2 feet of the plate on each side, crossing the plate over the ground and lower than 5 feet high
pbpEjectReason <- subset(pbpEjectReason, pbpEjectReason$pitchData.coordinates.pX>=-2.8391667 & 
                           pbpEjectReason$pitchData.coordinates.pX<=2.8391667 & 
                           pbpEjectReason$pitchData.coordinates.pZ>=0 & 
                           pbpEjectReason$pitchData.coordinates.pZ<=5)

#####Visualize et suppress NA's for main variables
summary(pbpEjectReason)
pbpEjectReason <- subset(pbpEjectReason, !is.na(pbpEjectReason$matchup.batter.id))
summary(pbpEjectReason)

################Load Data 2010-2019 & Create sample for games without ejection in pbpFull########################
pbpFull <- read_csv("PicthByPitch_2010-2019.csv", col_names = TRUE)
head(pbpFull)

#Keep only called pitches
pbpFull <- subset(pbpFull, pbpFull$details.call.code=="C" | pbpFull$details.call.code=="B")

#Keep only pitches : inside 2 feet of the plate on each side, crossing the plate over the ground and lower than 5 feet high
pbpFull <- subset(pbpFull, pbpFull$pitchData.coordinates.pX>=-2.8391667 & 
                    pbpFull$pitchData.coordinates.pX<=2.8391667 & 
                    pbpFull$pitchData.coordinates.pZ>=0 & 
                    pbpFull$pitchData.coordinates.pZ<=5)

#####Visualize et suppress NA's for main variables
summary(pbpFull)
pbpFull <- subset(pbpFull, !is.na(pbpFull$matchup.batter.id))
pbpFull <- subset(pbpFull, !is.na(pbpFull$pitchData.coordinates.y))
pbpFull <- subset(pbpFull, !is.na(pbpFull$pitchData.coordinates.x))
summary(pbpFull)

#####Import game Pks
gamePks <- as.vector(read_excel("GamePks_GamesInDatabaseOnly.xlsx", sheet = 1, col_names = T))
head(gamePks)
length(gamePks)

###Clear games with ejection using pbpEjectReason in gamePks
pbpFullCleaned  <- pbpFull[ ! pbpFull$game_pk %in% gamePks, ]

###Create Year variable
pbpFullCleaned$Year <- substr(pbpFullCleaned$game_date, 1, 4)

###Adjust count to represent the reel pre-pitch count (to correct for coding problem in source)
pbpFullCleaned$count.balls.reel <- ifelse(pbpFullCleaned$details.call.code=="B", 
                                          pbpFullCleaned$count.balls.start - 1, 
                                          pbpFullCleaned$count.balls.start)
pbpFullCleaned <- subset(pbpFullCleaned, pbpFullCleaned$count.balls.reel < 4)

pbpFullCleaned$count.strikes.reel <- ifelse(pbpFullCleaned$details.call.code=="C", 
                                            pbpFullCleaned$count.strikes.start - 1, 
                                            pbpFullCleaned$count.strikes.start)
pbpFullCleaned <- subset(pbpFullCleaned, pbpFullCleaned$count.strikes.reel < 3)

pbpFullCleaned$count.reel <-paste(pbpFullCleaned$count.balls.reel,pbpFullCleaned$count.strikes.reel,sep="-")

###Create factors for nominal variables
pbpFullCleaned$matchup.batSide.code <- factor(pbpFullCleaned$matchup.batSide.code)
pbpFullCleaned$matchup.pitchHand.code <- factor(pbpFullCleaned$matchup.pitchHand.code)
pbpFullCleaned$count.reel <- factor(pbpFullCleaned$count.reel)
pbpFullCleaned$details.type.code <- factor(pbpFullCleaned$details.type.code)
pbpFullCleaned$Year <- factor(pbpFullCleaned$Year)

####Create variables calledStrike (ball = 0, strike = 1)
pbpFullCleaned$calledStrike <- ifelse(pbpFullCleaned$details.call.code=="B", 0, 1)
mean(pbpFullCleaned$calledStrike)


###Data analysis - baseline model

####################Create baseline GAM model for called strike probability####################################
###indicate number of cores used for parallel processing
if (detectCores()>1) {
  cl <- makeCluster(detectCores()-1)
} else cl <- NULL
cl

###set the random seed for cross-validation
set.seed(1234)

###Create an object to plot theoretical SZ latter, before cleaning variables not needed for modeling
pbpSZsize <- pbpFullCleaned   ### Only for Supplementary material GGPlot geom_contour

###Keep only variables of interest
pbpFullCleaned <- pbpFullCleaned %>% dplyr::select(calledStrike,
                                            pitchData.coordinates.pX,pitchData.coordinates.pZ,
                                            matchup.batSide.code,matchup.pitchHand.code,
                                            count.reel,details.type.code,Year)

###GAM model (long to process, so available lower in "probStrikeGAM_FINAL.rds")
# probStrikeGAM <- bam(calledStrike ~ s(pitchData.coordinates.pX, pitchData.coordinates.pZ, k = 50, 
#                                      by=interaction(matchup.batSide.code, matchup.pitchHand.code)) +
#                                        count.reel + details.type.code + Year, 
#                     method="GCV.Cp", data = pbpFullCleaned, family=binomial(link="logit"), cluster=cl)

# saveRDS(probStrikeGAM, file = "probStrikeGAM_FINAL.rds")

probStrikeGAM <- readRDS("probStrikeGAM_FINAL.rds")

#####Creating cs_prob from the GAM model
cs_prob <- predict(probStrikeGAM, type = "response", newdata = pbpEjectReason)
cs_prob <- as.vector(cs_prob)
pbpEjectReason <- cbind(pbpEjectReason, cs_prob)
mean(pbpEjectReason$cs_prob)

####Creating a variable to calculate the effectiveness of the predictions
pbpEjectReason$predict <- with(pbpEjectReason, ifelse(calledStrike == 0 & cs_prob < 0.5, 1,
                                                      ifelse(calledStrike == 1 & cs_prob >= 0.5, 1, 0)))

mean(pbpEjectReason$predict)

####Draw a ROC curve to have another way of measuring effectiveness
library(ROCR)

pred <- prediction(c(cs_prob), pbpEjectReason$calledStrike)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE, print.auc=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))


###Data analysis - main analysis 

################################### Create different glmer ###########################################

###First, Create factors for nominal variables
pbpEjectReason$Ejection <- factor(pbpEjectReason$Ejection)
pbpEjectReason$EjectedTeam <- factor(pbpEjectReason$EjectedTeam)
pbpEjectReason$EjectionReason <- factor(pbpEjectReason$EjectionReason)
pbpEjectReason$matchup.batSide.code <- factor(pbpEjectReason$matchup.batSide.code)
pbpEjectReason$matchup.pitchHand.code <- factor(pbpEjectReason$matchup.pitchHand.code)
pbpEjectReason$OnBases <- factor(pbpEjectReason$OnBases)
pbpEjectReason$count.reel <- factor(pbpEjectReason$count.reel)
pbpEjectReason$details.type.code <- factor(pbpEjectReason$details.type.code)
pbpEjectReason$about.halfInning <- factor(pbpEjectReason$about.halfInning)
pbpEjectReason$UMPIRENAME <- factor(pbpEjectReason$UMPIRENAME)
pbpEjectReason$matchup.batter.fullName <- factor(pbpEjectReason$matchup.batter.fullName)
pbpEjectReason$matchup.pitcher.fullName <- factor(pbpEjectReason$matchup.pitcher.fullName)
pbpEjectReason$fielder_2 <- factor(pbpEjectReason$fielder_2)
pbpEjectReason$Year <- factor(pbpEjectReason$Year)
pbpEjectReason$JOB <- factor(pbpEjectReason$JOB)

#Divide attendance by 1000 for easier scale to read
pbpEjectReason$Attendance <- pbpEjectReason$Attendance/1000



################################### Ejection for SZ criticism Model (model 1) ###########################################
###Keep only variables of interest to avoid NAs
pbpEjectSZ <- subset(pbpEjectReason, pbpEjectReason$EjectionReason==0)

dataMod1 <- pbpEjectSZ %>% dplyr::select(calledStrike, UMPIRENAME, fielder_2,
                                         matchup.batter.fullName, matchup.pitcher.fullName,
                                         Ejection, EjectedTeam, cs_prob,
                                         about.halfInning, result.homeScore, result.awayScore)

# Change baseline for post-hoc direction
dataMod1$EjectedTeam <- factor(dataMod1$EjectedTeam, c("1", "0"))
dataMod1$Ejection <- factor(dataMod1$Ejection, c("1", "0"))

# set contrasts
options(contrasts  =c("contr.treatment", "contr.poly"))
# create distance matrix
dataMod1.dist <- datadist(dataMod1)
# include distance matrix in options
options(datadist = "dataMod1.dist")

###Making sure the random effects structure is justified
# baseline model glm
model1_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod1) 
AIC(logLik(model1_m0.glm))

# base-line mixed-model
model1_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod1, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model1_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model1_m0.glm) + 2 * logLik(model1_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model1_m1.glmer <- update(model1_m0.glmer, .~.+cs_prob + about.halfInning)
anova(model1_m1.glmer, model1_m0.glmer, test = "Chi") 

### Add Ejection
model1_m2.glmer <- update(model1_m1.glmer, .~.+ Ejection)
car::vif(model1_m2.glmer) ### test for multicolinearity
anova(model1_m2.glmer, model1_m1.glmer, test = "Chi") 
Anova(model1_m2.glmer, test = "Chi")

### Add EjectedTeam
model1_m3.glmer <- update(model1_m2.glmer, .~.+ EjectedTeam)
car::vif(model1_m3.glmer) ### test for multicolinearity
anova(model1_m3.glmer, model1_m2.glmer, test = "Chi") 
Anova(model1_m3.glmer, test = "Chi")

### Add Ejection*EjectedTeam
model1_m4.glmer <- update(model1_m3.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model1_m4.glmer) ### test for multicolinearity
anova(model1_m4.glmer, model1_m3.glmer, test = "Chi") 
Anova(model1_m4.glmer, test = "Chi")

### Add score_diff
dataMod1$score_diff <- dataMod1$result.homeScore - dataMod1$result.awayScore
model1_m5.glmer <- update(model1_m4.glmer, .~.+ score_diff)
car::vif(model1_m5.glmer) ### test for multicolinearity
anova(model1_m5.glmer, model1_m4.glmer, test = "Chi") 
Anova(model1_m5.glmer, test = "Chi")

### Best model = Model 4. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model1_m4.glmer)))
somers2(probs, as.numeric(dataMod1$calledStrike))

### Model diagnostic
# plot(model1_m4.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model1_m4.glmer)

# Report results
# report::report(model1_m4.glmer)  

# Post hoc analysis with emmeans
# emmeans(model1_m4.glmer, pairwise ~ EjectedTeam*Ejection, adjust="tukey", mode="linear.predictor", type="Score")

# 95% CI
# confint(pairs(emmeans(model1_m4.glmer, ~ EjectedTeam*Ejection,type="response")))  #type="response" for Odds ratio, remove for log-odds



################################### Ejection reason Model (model 2) ###########################################
###Keep only variables of interest to avoid NAs

dataMod2 <- pbpEjectReason %>% dplyr::select(calledStrike, UMPIRENAME, fielder_2,
                                             matchup.batter.fullName, matchup.pitcher.fullName,
                                             Ejection, EjectedTeam, EjectionReason, cs_prob,
                                             about.halfInning, result.homeScore, result.awayScore)

# Change baseline for post-hoc direction
dataMod2$EjectedTeam <- factor(dataMod2$EjectedTeam, c("1", "0"))
dataMod2$Ejection <- factor(dataMod2$Ejection, c("1", "0"))

# set contrasts
options(contrasts  =c("contr.treatment", "contr.poly"))
# create distance matrix
dataMod2.dist <- datadist(dataMod2)
# include distance matrix in options
options(datadist = "dataMod2.dist")

###Making sure the random effects structure is justified
# baseline model glm
model2_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod2) 
AIC(logLik(model2_m0.glm))

# base-line mixed-model
model2_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod2, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model2_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model2_m0.glm) + 2 * logLik(model2_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model2_m1.glmer <- update(model2_m0.glmer, .~.+cs_prob + about.halfInning)
anova(model2_m1.glmer, model2_m0.glmer, test = "Chi") 

### Add Ejection*EjectedTeam*EjectedReason
model2_m2.glmer <- update(model2_m1.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model2_m2.glmer) ### test for multicolinearity
anova(model2_m2.glmer, model2_m1.glmer, test = "Chi") 
Anova(model2_m2.glmer, test = "Chi")

### Add Ejection*EjectedTeam*EjectedReason
model2_m3.glmer <- update(model2_m2.glmer, .~.+ Ejection*EjectedTeam*EjectionReason)
car::vif(model2_m3.glmer) ### test for multicolinearity
anova(model2_m3.glmer, model2_m2.glmer, test = "Chi") 
Anova(model2_m3.glmer, test = "Chi")

### Add score_diff
dataMod2$score_diff <- dataMod2$result.homeScore - dataMod2$result.awayScore
model2_m4.glmer <- update(model2_m3.glmer, .~.+ score_diff)
car::vif(model2_m4.glmer) ### test for multicolinearity
anova(model2_m4.glmer, model2_m3.glmer, test = "Chi") 
Anova(model2_m4.glmer, test = "Chi")

### Best model = Model 3. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model2_m3.glmer)))
somers2(probs, as.numeric(dataMod2$calledStrike))

### Model diagnostic
# plot(model2_m3.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model2_m3.glmer)

# Report results
# report::report(model2_m3.glmer)  

# Post hoc analysis with emmeans
# emmeans(model2_m3.glmer, pairwise ~ EjectedTeam*Ejection|EjectionReason, adjust="tukey", mode="linear.predictor", type="Score")

# 95% CI
# confint(pairs(emmeans(model2_m3.glmer, ~ EjectedTeam*Ejection|EjectionReason,type="response")))  #type="response" for Odds ratio, remove for log-odds



################################### Role of the ejected individual Model (model 3) ###########################################

### Subset ejection for arging SZ and Keep only variables of interest to avoid NAs
dataMod3 <- subset(pbpEjectReason, pbpEjectReason$EjectionReason == 0 & pbpEjectReason$EJECTEENUMBER == 1)

dataMod3$JOB <- recode(dataMod3$JOB, C = "C", M = "C", P = "P")

dataMod3 <- dataMod3 %>% dplyr::select(calledStrike, UMPIRENAME, fielder_2,
                                       matchup.batter.fullName, matchup.pitcher.fullName,
                                       Ejection, EjectedTeam, JOB, cs_prob,
                                       about.halfInning, result.homeScore, result.awayScore)

# Change baseline for post-hoc direction
dataMod3$EjectedTeam <- factor(dataMod3$EjectedTeam, c("1", "0"))
dataMod3$Ejection <- factor(dataMod3$Ejection, c("1", "0"))

# set contrasts
# create distance matrix
dataMod3.dist <- datadist(dataMod3)
# include distance matrix in options
options(datadist = "dataMod3.dist")

###Making sure the random effects structure is justified
# baseline model glm
model3_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod3) 
AIC(logLik(model3_m0.glm))

# base-line mixed-model
model3_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod3, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model3_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model3_m0.glm) + 2 * logLik(model3_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model3_m1.glmer <- update(model3_m0.glmer, .~.+cs_prob + about.halfInning)
anova(model3_m1.glmer, model3_m0.glmer, test = "Chi") 

### Add Ejection*EjectedTeam
model3_m2.glmer <- update(model3_m1.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model3_m2.glmer) ### test for multicolinearity
anova(model3_m2.glmer, model3_m1.glmer, test = "Chi") 
Anova(model3_m2.glmer, test = "Chi")

### Add Ejection*EjectedTeam*JOB
dataMod3 <- within(dataMod3, JOB <- relevel(JOB, ref = "P"))
model3_m3.glmer <- update(model3_m2.glmer, .~.+ Ejection*EjectedTeam*JOB)
car::vif(model3_m3.glmer) ### test for multicolinearity
(car::vif(model3_m3.glmer)[, 3, drop = FALSE])^2 ### test for multicolinearity
anova(model3_m3.glmer, model3_m2.glmer, test = "Chi") 
Anova(model3_m3.glmer, test = "Chi")

### Add score_diff
dataMod3$score_diff <- dataMod3$result.homeScore - dataMod3$result.awayScore
model3_m4.glmer <- update(model3_m3.glmer, .~.+ score_diff)
car::vif(model3_m4.glmer) ### test for multicolinearity
anova(model3_m4.glmer, model3_m3.glmer, test = "Chi") 
Anova(model3_m4.glmer, test = "Chi")

### Best model = Model 3. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model3_m3.glmer)))
somers2(probs, as.numeric(dataMod3$calledStrike))

### Model diagnostic
# plot(model3_m3.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model3_m2.glmer)
# sjPlot::tab_model(model3_m3.glmer)

# Report results
# report(model3_m3.glmer)  ### Update R to access the package

# Post hoc analysis with emmeans
# emmeans(model3_m3.glmer, pairwise ~ EjectedTeam*Ejection|JOB, adjust="tukey", mode="linear.predictor", type="Score")

# 95% CI
# confint(pairs(emmeans(model3_m3.glmer, ~ EjectedTeam*Ejection|JOB,type="response")))  #type="response" for Odds ratio, remove for log-odds



################################### All-Star status Model (model 4) ###########################################

### Subset ejection for arging SZ, ejected players only, and Keep only variables of interest to avoid NAs
dataMod4 <- subset(pbpEjectReason, pbpEjectReason$EjectionReason == 0 & pbpEjectReason$JOB == "P" & EJECTEENUMBER == 1)

dataMod4 <- dataMod4 %>% select(calledStrike, UMPIRENAME, fielder_2,
                                matchup.batter.fullName, matchup.pitcher.fullName,
                                Ejection, EjectedTeam, EjecteeAllStar, cs_prob,
                                about.halfInning, result.homeScore, result.awayScore)

# set contrasts
# create distance matrix
dataMod4.dist <- datadist(dataMod4)
# include distance matrix in options
options(datadist = "dataMod4.dist")

###Making sure the random effects structure is justified
# baseline model glm
model4_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod4) 
AIC(logLik(model4_m0.glm))

# base-line mixed-model
model4_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod4, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model4_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model4_m0.glm) + 2 * logLik(model4_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model4_m1.glmer <- update(model4_m0.glmer, .~.+cs_prob)
anova(model4_m1.glmer, model4_m0.glmer, test = "Chi") 

### Add Ejection*EjectedTeam
model4_m2.glmer <- update(model4_m1.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model4_m2.glmer) ### test for multicolinearity
anova(model4_m2.glmer, model4_m1.glmer, test = "Chi") 
Anova(model4_m2.glmer, test = "Chi")

### Add Ejection*EjectedTeam*All-Star
model4_m3.glmer <- update(model4_m2.glmer, .~.+ Ejection*EjectedTeam*EjecteeAllStar)
car::vif(model4_m3.glmer) ### test for multicolinearity
anova(model4_m3.glmer, model4_m2.glmer, test = "Chi") 
Anova(model4_m3.glmer, test = "Chi")

### Add about.halfInning
model4_m4.glmer <- update(model4_m3.glmer, .~.+ about.halfInning)
car::vif(model4_m4.glmer) ### test for multicolinearity
anova(model4_m4.glmer, model4_m3.glmer, test = "Chi") 
Anova(model4_m4.glmer, test = "Chi")

### Add score_diff
dataMod4$score_diff <- dataMod4$result.homeScore - dataMod4$result.awayScore
model4_m5.glmer <- update(model4_m4.glmer, .~.+ score_diff)
car::vif(model4_m5.glmer) ### test for multicolinearity
anova(model4_m5.glmer, model4_m4.glmer, test = "Chi") 
Anova(model4_m5.glmer, test = "Chi")

### Best model = Model 3. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model4_m3.glmer)))
somers2(probs, as.numeric(dataMod4$calledStrike))

### Model diagnostic
# plot(model4_m3.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model4_m3.glmer)

# Report results
# report(model4_m3.glmer)  



################################### All-Star appearances Model (model 5) ###########################################

### Subset ejection for arging SZ, ejected players only, and Keep only variables of interest to avoid NAs
dataMod5 <- subset(pbpEjectReason, pbpEjectReason$EjectionReason == 0 & pbpEjectReason$JOB == "P" & EJECTEENUMBER == 1)

dataMod5 <- dataMod5 %>% select(calledStrike, UMPIRENAME, fielder_2,
                                matchup.batter.fullName, matchup.pitcher.fullName,
                                Ejection, EjectedTeam, EjecteeNumberAllStar, cs_prob,
                                about.halfInning, result.homeScore, result.awayScore)

# set contrasts
# create distance matrix
dataMod5.dist <- datadist(dataMod5)
# include distance matrix in options
options(datadist = "dataMod5.dist")

###Making sure the random effects structure is justified
# baseline model glm
model5_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod5) 
AIC(logLik(model5_m0.glm))

# base-line mixed-model
model5_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod5, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model5_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model5_m0.glm) + 2 * logLik(model5_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model5_m1.glmer <- update(model5_m0.glmer, .~.+cs_prob)
anova(model5_m1.glmer, model5_m0.glmer, test = "Chi") 

### Add Ejection*EjectedTeam
model5_m2.glmer <- update(model5_m1.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model5_m2.glmer) ### test for multicolinearity
anova(model5_m2.glmer, model5_m1.glmer, test = "Chi") 
Anova(model5_m2.glmer, test = "Chi")

### Add Ejection*EjectedTeam*All-Star number
### First, center continuous variable EjecteeNumberAllStar
dataMod5$EjectNumASCenter <- dataMod5$EjecteeNumberAllStar - (mean(dataMod5$EjecteeNumberAllStar))
model5_m3.glmer <- update(model5_m2.glmer, .~.+ Ejection*EjectedTeam*EjectNumASCenter)
car::vif(model5_m3.glmer) ### test for multicolinearity
anova(model5_m3.glmer, model5_m2.glmer, test = "Chi") 
Anova(model5_m3.glmer, test = "Chi")

### Add about.halfInning
model5_m4.glmer <- update(model5_m3.glmer, .~.+ about.halfInning)
car::vif(model5_m4.glmer) ### test for multicolinearity
anova(model5_m4.glmer, model5_m3.glmer, test = "Chi") 
Anova(model5_m4.glmer, test = "Chi")

### Add score_diff
dataMod5$score_diff <- dataMod5$result.homeScore - dataMod5$result.awayScore
model5_m5.glmer <- update(model5_m4.glmer, .~.+ score_diff)
car::vif(model5_m5.glmer) ### test for multicolinearity
anova(model5_m5.glmer, model5_m4.glmer, test = "Chi") 
Anova(model5_m5.glmer, test = "Chi")

### Best model = Model 3. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model5_m3.glmer)))
somers2(probs, as.numeric(dataMod5$calledStrike))

### Model diagnostic
# plot(model5_m3.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model5_m3.glmer)

# Report results
# report(model5_m3.glmer) 



################################### Number of seasons Model (model 6) ###########################################
### Subset ejection for arging SZ, ejected players only, and Keep only variables of interest to avoid NAs
dataMod6 <- subset(pbpEjectReason, pbpEjectReason$EjectionReason == 0 & pbpEjectReason$JOB == "P" & EJECTEENUMBER == 1)

dataMod6 <- dataMod6 %>% select(calledStrike, UMPIRENAME, fielder_2,
                                matchup.batter.fullName, matchup.pitcher.fullName,
                                Ejection, EjectedTeam, EjecteeMLBSeasons, cs_prob,
                                about.halfInning, result.homeScore, result.awayScore)

# set contrasts
# create distance matrix
dataMod6.dist <- datadist(dataMod6)
# include distance matrix in options
options(datadist = "dataMod6.dist")

###Making sure the random effects structure is justified
# baseline model glm
model6_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod6) 
AIC(logLik(model6_m0.glm))

# base-line mixed-model
model6_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod6, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model6_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model6_m0.glm) + 2 * logLik(model6_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model6_m1.glmer <- update(model6_m0.glmer, .~.+cs_prob)
anova(model6_m1.glmer, model6_m0.glmer, test = "Chi") 

### Add Ejection*EjectedTeam
model6_m2.glmer <- update(model6_m1.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model6_m2.glmer) ### test for multicolinearity
anova(model6_m2.glmer, model6_m1.glmer, test = "Chi") 
Anova(model6_m2.glmer, test = "Chi")

### Add Ejection*EjectedTeam*All-Star number
### First, center continuous variable EjecteeMLBSeasons
dataMod6$EjecteeMLBSeasons[is.na(dataMod6$EjecteeMLBSeasons)]<-9    ## Manually enter missing data for Matt Wieters
dataMod6$EjectSeasonsCenter <- dataMod6$EjecteeMLBSeasons - (mean(dataMod6$EjecteeMLBSeasons))
model6_m3.glmer <- update(model6_m2.glmer, .~.+ Ejection*EjectedTeam*EjectSeasonsCenter)
car::vif(model6_m3.glmer) ### test for multicolinearity
anova(model6_m3.glmer, model6_m2.glmer, test = "Chi") 
Anova(model6_m3.glmer, test = "Chi")

### Add about.halfInning
model6_m4.glmer <- update(model6_m3.glmer, .~.+ about.halfInning)
car::vif(model6_m4.glmer) ### test for multicolinearity
anova(model6_m4.glmer, model6_m3.glmer, test = "Chi") 
Anova(model6_m4.glmer, test = "Chi")

### Add score_diff
dataMod6$score_diff <- dataMod6$result.homeScore - dataMod6$result.awayScore
model6_m5.glmer <- update(model6_m4.glmer, .~.+ score_diff)
car::vif(model6_m5.glmer) ### test for multicolinearity
anova(model6_m5.glmer, model6_m4.glmer, test = "Chi") 
Anova(model6_m5.glmer, test = "Chi")

### Best model = Model 3. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model6_m3.glmer)))
somers2(probs, as.numeric(dataMod6$calledStrike))

### Model diagnostic
# plot(model6_m3.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model6_m3.glmer)

# Report results
# report(model6_m3.glmer)  



################################### WAR Model (model 7) ###########################################
### Subset ejection for arging SZ, ejected players only, and Keep only variables of interest to avoid NAs
dataMod7 <- subset(pbpEjectReason, pbpEjectReason$EjectionReason == 0 & pbpEjectReason$JOB == "P" & EJECTEENUMBER == 1)

dataMod7 <- dataMod7 %>% select(calledStrike, UMPIRENAME, fielder_2,
                                matchup.batter.fullName, matchup.pitcher.fullName,
                                Ejection, EjectedTeam, EjecteeWAR, cs_prob,
                                about.halfInning, result.homeScore, result.awayScore)

# set contrasts
# create distance matrix
dataMod7.dist <- datadist(dataMod7)
# include distance matrix in options
options(datadist = "dataMod7.dist")

###Making sure the random effects structure is justified
# baseline model glm
model7_m0.glm = glm(calledStrike ~ 1, family = binomial, data = dataMod7) 
AIC(logLik(model7_m0.glm))

# base-line mixed-model
model7_m0.glmer = glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod7, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 
AIC(logLik(model7_m0.glmer))

# test random effects (test whether the mixed-effects minimal base-line model explains significantly more variance 
# by applying a Model Likelihood Ratio Test to the fixed- and the mixed-effects minimal baseline models.)
null.id = -2 * logLik(model7_m0.glm) + 2 * logLik(model7_m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

### Add cs_prob
model7_m1.glmer <- update(model7_m0.glmer, .~.+cs_prob)
anova(model7_m1.glmer, model7_m0.glmer, test = "Chi") 

### Add Ejection*EjectedTeam
model7_m2.glmer <- update(model7_m1.glmer, .~.+ Ejection*EjectedTeam)
car::vif(model7_m2.glmer) ### test for multicolinearity
anova(model7_m2.glmer, model7_m1.glmer, test = "Chi") 
Anova(model7_m2.glmer, test = "Chi")

### Add Ejection*EjectedTeam*WAR
### First, center continuous variable WAR
dataMod7$EjectWARCenter <- dataMod7$EjecteeWAR - (mean(dataMod7$EjecteeWAR))
model7_m3.glmer <- update(model7_m2.glmer, .~.+ Ejection*EjectedTeam*EjectWARCenter)
car::vif(model7_m3.glmer) ### test for multicolinearity
anova(model7_m3.glmer, model7_m2.glmer, test = "Chi") 
Anova(model7_m3.glmer, test = "Chi")

### Add about.halfInning
model7_m4.glmer <- update(model7_m3.glmer, .~.+ about.halfInning)
car::vif(model7_m4.glmer) ### test for multicolinearity
anova(model7_m4.glmer, model7_m3.glmer, test = "Chi") 
Anova(model7_m4.glmer, test = "Chi")

### Add score_diff
dataMod7$score_diff <- dataMod7$result.homeScore - dataMod7$result.awayScore
model7_m5.glmer <- update(model7_m4.glmer, .~.+ score_diff)
car::vif(model7_m5.glmer) ### test for multicolinearity
anova(model7_m5.glmer, model7_m4.glmer, test = "Chi") 
Anova(model7_m5.glmer, test = "Chi")

### Best model = Model 3. Extract model fit parameters (Baayen 2008, 281).
probs = 1/(1+exp(-fitted(model7_m3.glmer)))
somers2(probs, as.numeric(dataMod7$calledStrike))

### Model diagnostic
# plot(model7_m4.glmer, pch = 20, col = "black", lty = "dotted")

# summarize final model
# sjPlot::tab_model(model7_m3.glmer)

# Report results
# report(model7_m3.glmer)  

