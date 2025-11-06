#POWER ANALYSES
#These analyses were conducted after the data were collected and analyzed, but
#they do not rely on the observed data in any way.

#This R code was written by Mike DeKay, based on the procedures in
#Kumle et al. (2021, Scenario 3).

#NOTE: In most analyses, we used 10,000 runs (n_sim = 10000), but that can take
#a very long time (many hours). Running the entire file might take days.
#We have set n_sim = 100 here (grossly insufficient), in case anyone tries to
#run the file.

#LOAD PACKAGES.
library(lme4)
library(psych)
library(gmodels)
#Packages for power analysis
library(mixedpower)
library(simr)

#POWER ANLAYSES NOT BASED ON DATA COLLECTED FOR THE STUDIES IN QUESTION

#MAKE NEW DATA FRAME AS IF THERE WERE NO DATA YET.
#This is like the MTurk study with 900 participants and 4 choices each.
#But there are no values for the dependent variable (choice).

Subject <- rep(seq(1:900), 4)
Domain <- c(rep(1, 900), rep(2, 900), rep(3, 900), rep(4,900))
Frame <- c(rep(c(rep(-0.5, 450), rep(0.5, 450)), 4))
CertainValence <- c(rep(c(rep(1, 30), rep(0, 45), rep(-1, 90),
                 rep(1, 45), rep(0, 30), rep(-1, 45),
                 rep(1, 90), rep(0, 45), rep(-1, 30),
                 rep(1, 30), rep(0, 45), rep(-1, 90),
                 rep(1, 45), rep(0, 30), rep(-1, 45),
                 rep(1, 90), rep(0, 45), rep(-1, 30)), 4))
RiskyValence <- c(rep(c(rep(1, 30), rep(1, 45), rep(1, 90),
                 rep(0, 45), rep(0, 30), rep(0, 45),
                 rep(-1, 90), rep(-1, 45), rep(-1, 30),
                 rep(1, 30), rep(1, 45), rep(1, 90),
                 rep(0, 45), rep(0, 30), rep(0, 45),
                 rep(-1, 90), rep(-1, 45), rep(-1, 30)), 4))
ValenceDiff <- RiskyValence - CertainValence
StdValDiff <- ValenceDiff/sd(ValenceDiff)
FakeData <- data.frame(Subject, Domain, Frame, CertainValence, RiskyValence, ValenceDiff, StdValDiff)
FakeData$Interaction <- FakeData$ValenceDiff*FakeData$Frame
FakeData$StdInt <- FakeData$StdValDiff*FakeData$Frame

#Sort by Subject and Domain.
FakeData <- FakeData[order(Subject, Domain), ]
head(FakeData)
tail(FakeData)

#Check the standard deviation of ValenceDiff and the standardized version.
describe(FakeData$ValenceDiff) #1.41
describe(FakeData$StdValDiff)

#Check the structure of FakeData. Looks good.
CrossTable(FakeData$RiskyValence, FakeData$CertainValence,
           prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
CrossTable(FakeData$Frame, FakeData$ValenceDiff,
           prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

#MAKE THE ARTIFICIAL DATA SET (a generalized linear mixed model or GLMM).

#Set the fixed effects (Intercept, ValenceDiff, Frame, Interaction).
#Baseline models from DeKay et al.'s (2022) metastudies paper had intercept =
#0.78 in Exp1, 0.73 in Exp2, 0.32 in Exp 3.
#In DeKay and Garge (2023), intercept was 0.80 in main model, 0.90 for certain vs. risky.
#Use intercept = -0.7 here (negative because now predicting the RISKY option).
#Use small effects for slopes and 0 for the interaction.
#0.36 is the slope that corresponds to the small value of Cohen's d (0.2)
#Here and elsewhere, slope = 1.81*d, where 1.81 = pi/sqrt(3), which is the standard
#deviation of the standard logistic distribution.
FixedEffects <- c(-0.7, 0.36, 0.36, 0)

#Set the variance of the random intercept.
#Intercept variance in baseline models of DeKay et al. (2022) was 1.36 in Exp1,
#1.00 in Exp2, 0.78 in Exp 3.
#In DeKay and Garge (2023), it was 0.45 in the main model and 0.58 for certain vs. risky.
#Use 1.0 here. This seems "conservative," as power increases with lower values.
RandomVariance <- 1

#Make the artificial dataset.
FakeGlmer <- makeGlmer(formula = Choice ~ ValenceDiff + Frame + Interaction + (1|Subject),
                      fixef = FixedEffects, VarCorr = RandomVariance,
                      family = "binomial", data = FakeData)
#Look at the results.
summary(FakeGlmer)

#RUN THE POWER ANALYSIS.

#Use sample sizes of 500 and 900 (900 was our target sample size).
#0.36 is the slope that corresponds to the small value of Cohen's d (0.2).
#Use the SESOI (smallest effect size of interest) to test very small slopes.
#0.18 is very small (d = 0.1).
#NOTE: We used n_sim = 10000, but that can take a very long time.
SESOI <- c(-0.7, 0.18, 0.18, 0)
power_Fake <- mixedpower(model = FakeGlmer, data = FakeData,
                            fixed_effects = c("ValenceDiff", "Frame", "Interaction"),
                            simvar = "Subject", steps = c(500, 900),
                            critical_value = 1.96, n_sim = 50,
                            SESOI = SESOI)
power_Fake

#REQUIRED SAMPLE SIZE FOR VERY SMALL EFFECT OF FRAME

#How large a sample is required to have power > 0.80 for a very small effect of Frame?
#NOTE: We used n_sim = 5000, but that can take a very long time.
powerSESOI_FakeSmaller <- mixedpower(model = FakeGlmer, data = FakeData,
                                     fixed_effects = c("ValenceDiff", "Frame", "Interaction"),
                                     simvar = "Subject", steps = c(2000, 2100, 2200),
                                     critical_value = 1.96, n_sim = 100,
                                     SESOI = SESOI, databased = F)
powerSESOI_FakeSmaller

#POWER FOR A ONE-STANDARD-DEVIATION CHANGE IN ValenceDiff

#Use the standardized valence difference instead ValenceDiff.
#This is less meaningful because a 1.41-unit difference in ValenceDiff does not
#correspond to the difference between any two conditions.
#Make a new artificial dataset.
StdFakeGlmer <- makeGlmer(formula = Choice ~ StdValDiff + Frame + StdInt + (1|Subject),
                       fixef = FixedEffects, VarCorr = RandomVariance,
                       family = "binomial", data = FakeData)
summary(StdFakeGlmer)

#Run the power analysis.
#NOTE: We used n_sim = 10000, but that can take a very long time.
power_StdFake <- mixedpower(model = StdFakeGlmer, data = FakeData,
                            fixed_effects = c("StdValDiff", "Frame", "StdInt"),
                            simvar = "Subject", steps = c(500, 900),
                            critical_value = 1.96, n_sim = 100,
                            SESOI = SESOI)
power_StdFake

#POWER FOR COMPARING INDIVIDUAL PAIRS OF CELLS

#MAKE A NEW, SMALLER FAKE DATASET (45 SUBJECTS PER CELL).
Subject <- rep(seq(1:90), 4)
Domain <- c(rep(1, 90), rep(2, 90), rep(3, 90), rep(4,90))
Frame <- c(rep(c(rep(-0.5, 45), rep(0.5, 45)), 4))
FakeData2 <- data.frame(Subject, Domain, Frame)

#Sort by Subject and Domain.
FakeData2 <- FakeData2[order(Subject, Domain), ]
head(FakeData2)
tail(FakeData2)

#Set the fixed effects (Intercept, Frame).
#We used the same intercept as above.
#Power with effect size from Many Labs (Klein et al. 2016):
#1.09 is the slope that corresponds to d = 0.60.
FixedEffects2 <- c(-0.7, 1.09)
#0.91 is the slope that corresponds to the medium value of d (0.5)
SESOI2 <- c(-0.7, 0.91)
#0.36 is the slope that corresponds to the small value of d (0.2)
SESOI3 <- c(-0.7, 0.36)

#Set the variance of the random intercept.
#We used the same value as above.
#Could justify a lower value, like 0.58 for the standard FE (certain vs. risky)
#in DeKay and Garge (2023). Lower values increase power.
RandomVariance2 <- 1 

#Make the artificial dataset
FakeGlmer2 <- makeGlmer(formula = Choice ~ Frame + (1|Subject),
                       fixef = FixedEffects2, VarCorr = RandomVariance2,
                       family = "binomial", data = FakeData2)
summary(FakeGlmer2)

#Run power analysis for d = 0.60 (Many Labs) and 0.50 (medium)
#NOTE: We used n_sim = 10000, but that can take a long time.
power_FE <- mixedpower(model = FakeGlmer2, data = FakeData2,
                               fixed_effects = "Frame",
                               simvar = "Subject", steps = c(60, 90),
                               critical_value = 1.96, n_sim = 100,
                               SESOI = SESOI2)
power_FE

#Run power analysis for d = 0.20 (small)
#NOTE: We used n_sim = 10000, but that can take a long time.
power_FEsmall <- mixedpower(model = FakeGlmer2, data = FakeData2,
                       fixed_effects = "Frame",
                       simvar = "Subject", steps = c(60, 90),
                       critical_value = 1.96, n_sim = 100,
                       SESOI = SESOI3, databased = F)
power_FEsmall

#Results for the three effect sizes in these last two analyses were combined into
#a single table in the Supplemental Material.

#END OF CODE
