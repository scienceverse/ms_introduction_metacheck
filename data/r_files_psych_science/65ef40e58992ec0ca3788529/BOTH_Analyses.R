#The initial R code for the experiment was written by Shiyu Dou.
#It has been edited and extended by Mike DeKay.

#This file is for BOTH the MTurk (CloudResearch) data and the REP (student) data.
#The analyses are the same as those for the separate samples, but the data files
#have been combined.

#READ IN THE DATA FILE.
BOTH <- read.csv("BOTH_CleanData.csv")

#LOAD PACKAGES.
library(readr)
library(dplyr)
library(tidyr)
library(rcompanion)
library(MASS)
library(gtools)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())
#Packages for regression analysis
library(lme4)
require(lsmeans)
library(gridExtra)
library(grid)
library(lattice)
library(ggforce)
#Packages added later
library(psych)
library(gmodels)
library(lmerTest)

#CREATE NEW VARIABLES, RECODE VARIABLES, ETC.
#These are in no particular order.
BOTH <- within(BOTH, {
  
  #Create a variable for Domain Position (called DomainOrder here)
  DomainOrder <- NA
  #Assign a unique position variable to each answer (4 answers/participants in random order)
  #We know the actual order of the 4 domains being presented is stored in attributes FirstDomain:FourthDomain;
  #We also know the domain of each answer(row), stored in "Domain" 
  #Match the "Domain" variable to "FirstDomain:FourthDomain"; store result in "DomainOrder";
  #we get each answer (row) matched to a position (1,2,3 or 4).
  for (i in 1:dim(BOTH)[1]){
    DomainOrder[i] <- ifelse(Domain[i] == FirstDomain[i], 1,
                             ifelse(Domain[i] == SecondDomain[i], 2,
                                    ifelse(Domain[i] == ThirdDomain[i], 3,
                                           ifelse(Domain[i] == FourthDomain[i], 4, NA))))
  }
  #Center the Domain Position variable to get values of -1.5, -0.5, 0.5, 1.5.
  DomainOrderCode <- DomainOrder - 2.5
  
  #Create orthogonal contrast codes for the four domains, for consistency with
  #the preregistration.
  DomainCode1 <- NA
  DomainCode1[Domain == "Disease"] <- 3/4 #Disease vs. others
  DomainCode1[Domain == "Invest"] <- -1/4
  DomainCode1[Domain == "Wildfire"] <- -1/4
  DomainCode1[Domain == "Drought"] <- -1/4

  DomainCode2 <- NA
  DomainCode2[Domain == "Disease"] <- 0
  DomainCode2[Domain == "Invest"] <- 2/3 #Invest vs. Wildfire and Drought
  DomainCode2[Domain == "Wildfire"] <- -1/3
  DomainCode2[Domain == "Drought"] <- -1/3
  
  DomainCode3 <- NA
  DomainCode3[Domain == "Disease"] <- 0
  DomainCode3[Domain == "Invest"] <- 0
  DomainCode3[Domain == "Wildfire"] <- 1/2 #Wildfire vs. Drought
  DomainCode3[Domain == "Drought"] <- -1/2
  
  #Create orthogonal polynomial contrast codes for the ValenceDiff variable,
  #for consistency with the preregistration.
  #Here and elsewhere, VDA is the ValenceDiff variable.
  VDAlin <- VDA
  
  VDAquad <- NA
  VDAquad[VDA == -2] <- 2
  VDAquad[VDA == -1] <- -1
  VDAquad[VDA == 0] <- -2
  VDAquad[VDA == 1] <- -1
  VDAquad[VDA == 2] <- 2
  
  VDAcub <- NA
  VDAcub[VDA == -2] <- -1
  VDAcub[VDA == -1] <- 2
  VDAcub[VDA == 0] <- 0
  VDAcub[VDA == 1] <- -2
  VDAcub[VDA == 2] <- 1
  
  #Create variables for two alternative specifications of EVA
  #(VDA is the ValenceDiff variable)
  #Equal valence differences
  EVAequal <- VDA
  EVAequal[VDA == -2] <- -1
  EVAequal[VDA == 2] <- 1
  
  #Free valence differences
  EVApm <- NA #positive valence vs. mixed valence
  EVAmn <- NA #mixed valence vs. negative valence
  EVApn <- NA #positive valence vs. negative valence
  
  #The excluded condition is cells with matched valence.
  EVApm[Cell %in% c("G1", "G5", "G9", "L1", "L5", "L9")] <- 0
  EVAmn[Cell %in% c("G1", "G5", "G9", "L1", "L5", "L9")] <- 0
  EVApn[Cell %in% c("G1", "G5", "G9", "L1", "L5", "L9")] <- 0
  
  #Cells with positive vs. mixed valence favoring the risky option
  EVApm[Cell %in% c("G2", "L2")] <- 1
  EVAmn[Cell %in% c("G2", "L2")] <- 0
  EVApn[Cell %in% c("G2", "L2")] <- 0 
  #Cells with positive vs. mixed valence favoring the certain option
  EVApm[Cell %in% c("G4", "L4")] <- -1
  EVAmn[Cell %in% c("G4", "L4")] <- 0
  EVApn[Cell %in% c("G4", "L4")] <- 0
  
  #Cells with mixed vs. negative valence favoring the risky option
  EVApm[Cell %in% c("G6", "L6")] <- 0
  EVAmn[Cell %in% c("G6", "L6")] <- 1
  EVApn[Cell %in% c("G6", "L6")] <- 0 
  #Cells with mixed vs. negative valence favoring the certain option
  EVApm[Cell %in% c("G8", "L8")] <- 0
  EVAmn[Cell %in% c("G8", "L8")] <- -1
  EVApn[Cell %in% c("G8", "L8")] <- 0
  
  #Cells with positive vs. negative valence favoring the risky option
  EVApm[Cell %in% c("G3", "L3")] <- 0
  EVAmn[Cell %in% c("G3", "L3")] <- 0
  EVApn[Cell %in% c("G3", "L3")] <- 1 
  #Cells with positive vs. negative valence favoring the certain option
  EVApm[Cell %in% c("G7", "L7")] <- 0
  EVAmn[Cell %in% c("G7", "L7")] <- 0
  EVApn[Cell %in% c("G7", "L7")] <- -1
  
  #Create variables for two specifications (interpretations) of FTT.
  #See the Supplemental Material for detailed explanations.
  
  #FTT Interpretation 1
  #Here, we assume that "none saved" is preferred to "some not saved,"
  #as seems to be implied in Broniatowski and Reyna (2018),
  FTT1 <- NA
  FTT1[Cell == "L3"] <- 2
  FTT1[Cell %in% c("G2", "G3", "G6", "G9", "L6")] <- 1
  FTT1[Cell %in% c("G1", "G5", "G8", "L2", "L5", "L9")] <- 0
  FTT1[Cell %in% c("G4", "L1", "L4", "L7", "L8")] <- -1
  FTT1[Cell == "G7"] <- -2

  #FTT Interpretation 2
  #Here, we assume that "none saved" and "some not saved" are either equally
  #preferred or not comparable.
  #The gist values are the same as our ValenceDiff (VDA) values except in G3 and L7.
  FTT2 <- VDA
  FTT2[Cell == "G3"] <- 1 #rather than 2 in VDA
  FTT2[Cell == "L7"] <- -1 #rather than -2 in VDA
  
  #Make Aspect Order (Order here) be NA for the corner cells, where only one
  #aspect of each option is mentioned.
  #The relevant analysis will be for 10 cells rather than 18.
  Order[Cell %in% c("G1", "G3", "G7", "G9", "L1", "L3", "L7", "L9")] <- NA
  
  #Change values of Choice = -Inf and Rate = -Inf to NA (some are -Inf in Excel)
  #This just undoes a step from an earlier data-preparation process.
  #In the two separate files the values were -Inf, but they were changed to 99999 in the BOTH data
  #(because Excel was being difficult).
  Choice[Choice == "-Inf"] <- NA
  Rate[Rate == "-Inf"] <- NA
  Choice[Choice == 99999] <- NA
  Rate[Rate == 99999] <- NA
  
  #Code demographic variables to test interactions
  Female <- NA
  Female[Gender == 1] <- -0.5
  Female[Gender == 2] <- 0.5
  
  #White (exclusive of mixed)
  WhiteInc <- NA
  WhiteInc[Race %in% c(5, 15, 25, 35, 56, 135, 245)] <- 0.5 #(Includes mixed if White in mix)
  WhiteInc[Race %in% c(1, 2, 3, 4, 6, 24, 36)] <- -0.5
  
  #White (inclusive of mixed)
  WhiteExc <- NA
  WhiteExc[Race == 5] <- 0.5 #(Excludes mixed)
  WhiteExc[Race != 5] <- -0.5
  
  #Dummy codes for White, Black, Asian (exlcusive of mixed, White is excluded category, others missing)
  BlackD <- NA
  BlackD[Race == 2] <- 0 #Asian
  BlackD[Race == 3] <- 1 #Black
  BlackD[Race == 5] <- 0 #White
  
  AsianD <- NA
  AsianD[Race == 2] <- 1 #Asian
  AsianD[Race == 3] <- 0 #Black
  AsianD[Race == 5] <- 0 #White
  
  Hispanic <- NA
  Hispanic[IsHispanic == 1] <- 0.5
  Hispanic[IsHispanic == 2] <- -0.5
  
  AgeDev <- NA
  AgeDev <- Age - mean(Age, na.rm=TRUE)
  AgeDevSq <- AgeDev*AgeDev
  
  MTurkSamp <- NA
  MTurkSamp[Sample == 1] <- 0.5
  MTurkSamp[Sample == 2] <- -0.5
  
  #US Census regions (9 = DC, 40 = PR, both put in South) 
  Region <- NA
  Region[State %in% c(7, 20, 22, 30, 31, 33, 39, 41, 47)] <- "Northeast"
  Region[State %in% c(1, 4, 8, 9, 10, 11, 18, 19, 21, 25, 34, 37, 40, 42, 44, 45, 48, 50)] <- "South"
  Region[State %in% c(14, 15, 16, 17, 23, 24, 26, 28, 35, 36, 43, 51)] <- "Midwest"
  Region[State %in% c(2, 3, 5, 6, 12, 13, 27, 29, 32, 38, 46, 49, 52)] <- "West"
  
  NortheastD <- NA
  NortheastD[Region == "Northeast"] <- 1
  NortheastD[Region != "Northeast"] <- 0
  SouthD <- NA
  SouthD[Region == "South"] <- 1
  SouthD[Region != "South"] <- 0
  MidwestD <- NA
  MidwestD[Region == "Midwest"] <- 1
  MidwestD[Region != "Midwest"] <- 0
  WestD <- NA
  WestD[Region == "West"] <- 1
  WestD[Region != "West"] <- 0
  
})

#Check to see if the codes are as intended. They are.
#CrossTable(BOTH$DomainOrder, BOTH$DomainOrderCode, 
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$Domain, BOTH$DomainCode1,
#          prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$Domain, BOTH$DomainCode2,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$Domain, BOTH$DomainCode3,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$Cell, BOTH$Order,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$VDA, BOTH$FTT1,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$VDA, BOTH$FTT2,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$Cell, BOTH$FTT1,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#CrossTable(BOTH$Cell, BOTH$FTT2,
#           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)

#DEMOGRAPHICS
#Note that we have a tall file with 4 lines for every subject.
#Trim to one line per subject by getting rid of duplicate IDs (this works).
onerow <-BOTH[!duplicated(BOTH$ID), ]
#head(onerow)

describe(onerow$ID, skew=F)
#Sample
CrossTable(onerow$MTurkSamp)
#Age
describe(onerow$Age, skew=F)
summary(onerow$Age)
describe(onerow$AgeDev, skew=F)
#For Gender, 1 = male, 2 = female, 3 = other
CrossTable(onerow$Gender)
#For Race, 1 = American Indian or Alaska Native, 2 = Asian, 3 = Black or African American,
#4 = Native Hawaiian or Other Pacific Islander, #5 = White, 6 = Other,
#Multiple digits = Mixed race
#Categories 1, 4, and 6 are combined in the paper.
CrossTable(onerow$Race)
#White (inclusive or exclusive of mixed)
CrossTable(onerow$WhiteInc)
CrossTable(onerow$WhiteExc)
#For Hispanic, 1 = yes, 2 = no
CrossTable(onerow$IsHispanic)
#States (9 = DC, 40 = PR, 53 = Not US -- kept in student sample, but not assigned a region)
CrossTable(onerow$State)
CrossTable(onerow$Region)

#PROPORTIONS CHOOSING THE RISKY OPTION IN ALL CHOICES IN EACH FRAME
#For Choice, Risky = 1, Certain = 0
#For Frame, Losses = +0.5, Gains = -0.5
#Statistical tests are not appropriate here, because there were 4 choices per subject.
CrossTable(BOTH$Frame, BOTH$Choice, prop.c=F, prop.t=F, prop.chisq=F)

#Proportions for each of the 18 cells (for a potential summary table)
#Statistical tests are not appropriate here, because there were 4 choices per subject.
CrossTable(BOTH$Cell, BOTH$Choice, prop.c=F, prop.t=F, prop.chisq=F)

#Proportions for standard FE (G4 and L6)
#Statistical tests are not appropriate here, because there were 4 choices per subject.
standard <- subset(BOTH, Cell %in% c("G4", "L6"))
CrossTable(standard$Frame, standard$Choice, prop.c=F, prop.t=F, prop.chisq=F)

#Proportions for complete cells (G5 and L5)
#Statistical tests are not appropriate here, because there were 4 choices per subject.
complete <- subset(BOTH, Cell %in% c("G5", "L5"))
CrossTable(complete$Frame, complete$Choice, prop.c=F, prop.t=F, prop.chisq=F)

#MIXED-EFFECTS LOGISTIC REGRESSIONS

#STANDARD FRAMING EFFECT ONLY (L6 VS.G4)
StdFE <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G4' | Cell == 'L6'))
summary(StdFE)
#Confidence intervals and odds ratios
#Wald CIs for paper (fast)
ccS <- confint(StdFE,parm="beta_", method="Wald")
ctabS <- cbind(est=fixef(StdFE),ccS)
print(ctabS, digits=4) #digits seems a bit flaky, so got more for rounding purposes
###Odds ratios
ORsS <- exp(ctabS)
print(ORsS, digits=4)

#COMPLETE CELLS ONLY (L5 VS.G5)
CompFE <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G5' | Cell == 'L5'))
summary(CompFE)
#Confidence intervals and odds ratios
#Wald CIs for paper (fast)
ccC <- confint(CompFE,parm="beta_", method="Wald")
ctabC <- cbind(est=fixef(CompFE),ccC)
print(ctabC, digits=4) #digits seems a bit flaky, so got more for rounding purposes
###Odds ratios
ORsC <- exp(ctabC)
print(ORsC, digits=4)

#FRAMING EFFECT ONLY, FOR ALL CELLS (for the rightmost point in Figure 2, if I add that)
mlr0 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr0)
#Confidence intervals and odds ratios
#Wald CIs for paper (fast)
cc0 <- confint(mlr0,parm="beta_", method="Wald")
ctab0 <- cbind(est=fixef(mlr0),cc0)
print(ctab0, digits=4) #digits seems a bit flaky, so got more for rounding purposes
###Odds ratios
ORs0 <- exp(ctab0)
print(ORs0, digits=4)

#PRIMARY MODEL  
#The coefficient for ValenceDiff (VDA) in this model and others differs from the
#results in Shiyu Dou's thesis, because Frame was coded 0,1 there.
mlr1 <- glmer(Choice ~ VDA + Frame + VDA:Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr1)
#Confidence intervals and odds ratios
#Profile CIs (slow, and don't work for all cell pairs below)
#cc <- confint(mlr1,parm="beta_")
#Wald CIs for paper (fast)
cc <- confint(mlr1,parm="beta_", method="Wald")
ctab <- cbind(est=fixef(mlr1),cc)
print(ctab, digits=4) #digits seems a bit flaky, so got more for rounding purposes
###Odds ratios
ORs <- exp(ctab)
print(ORs, digits=2)

#Endpoint predictions for plotting lines in a later figure (like Figure 3 of the paper)
#These are used much further down.
VDA <- c(-2,2,-2,2)
Frame <- c(-0.5,-0.5,0.5,0.5)
Vars <- data.frame(VDA, Frame)
Preds <- predict(mlr1, newdata=Vars, re.form=NA)
mlr1_Lines <- data.frame(Vars, Preds)
#mlr1_Lines
GainLeft <- mlr1_Lines[1,3]
GainRight <- mlr1_Lines[2,3]
LossLeft <- mlr1_Lines[3,3]
LossRight <- mlr1_Lines[4,3]

#ALTERNATIVE MIXED-EFFECTS LOGISTIC REGRESSION MODELS (for Table 3 of the paper)

#Primary model with no interaction for comparison
mlr1NI <- glmer(Choice ~ VDA + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr1NI)

#EVA with all nonzero gist differences being the same (+/-1 instead of +/-2)
mlrEVAequalNI <- glmer(Choice ~ EVAequal + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrEVAequalNI)

#EVA with separate variables for each type of valence difference (no interactions)
mlrEVAdiffNI <- glmer(Choice ~ EVApm + EVAmn + EVApn + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrEVAdiffNI)

#FTT interpretation 1
mlrFTT1NI <- glmer(Choice ~ FTT1 + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrFTT1NI)

#FTT interpretation 2
mlrFTT2NI <- glmer(Choice ~ FTT2 + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrFTT2NI)

#For completeness, here are the alternative models WITH the interaction(s).
#Interactions never help, according to BIC.

#EVA with all nonzero gist differences being the same (+/-1 instead of +/-2)
mlrEVAequal <- glmer(Choice ~ EVAequal + Frame + EVAequal:Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrEVAequal)

#EVA with separate variables for each type of valence difference
mlrEVAdiff <- glmer(Choice ~ EVApm + EVAmn + EVApn + Frame + EVApm:Frame + EVAmn:Frame + EVApn:Frame + (1|ID),
                    family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrEVAdiff)

#FTT interpretation 1
mlrFTT1 <- glmer(Choice ~ FTT1 + Frame + FTT1:Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrFTT1)

#FTT interpretation 2
mlrFTT2 <- glmer(Choice ~ FTT2 + Frame + FTT2:Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrFTT2)

#HEAD-TO-HEAD MODELS, TO SEE IF FTT ADDS ANYTHING WHEN EVA IS IN THE MODEL

#Interval EVA + FTT1
EVA1FTT1 <- glmer(Choice ~ VDA + FTT1 + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(EVA1FTT1)

#Interval EVA + FTT2
EVA1FTT2 <- glmer(Choice ~ VDA + FTT2 + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(EVA1FTT2)

#Equal EVA + FTT1
EVA2FTT1 <- glmer(Choice ~ EVAequal + FTT1 + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(EVA2FTT1)

#Equal EVA + FTT2
EVA2FTT2 <- glmer(Choice ~ EVAequal + FTT2 + Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(EVA2FTT2)

#MORE LOGISTIC MODELS, INCLUDING TESTS OF MODERATORS

#Check for NONLINEAR effects of ValenceDiff (VBA), to comply with preregistration.
#No interactions here.
mlrNonlin <- glmer(Choice ~ VDAlin + VDAquad + VDAcub + 
                      Frame + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrNonlin)
#Compare to the model only the linear code for ValenceDiff (also compare BICs).
anova(mlr1NI, mlrNonlin)

#Moderating effects of ASPECT ORDER
#These models are for just for 10 relevant cells, not all 18.
#The coding of aspect order is explained in the Supplemental Material.
#It is positive (0.5) if we thought it would help the risky option and negative (-0.5)
#if we thought it would help the certain option. It is 0 in the center cells, G5 and L5.
mlr_Order <- glmer(Choice ~ VDA + Frame + Order + VDA:Frame + Order:Frame + Order:VDA + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr_Order)

#Separate primary models at EACH LEVEL OF ASPECT ORDER
#The BICs are not useful for comparing the orders.

#Aspect order is POSITIVE.
mlr_OrderPos <- glmer(Choice ~ VDA + Frame + VDA:Frame + (1|ID), family = binomial,
                       data = BOTH, subset=(Order==0.5), control= glmerControl(optimizer="bobyqa"))
summary(mlr_OrderPos)

#Aspect order is ZERO (center cells only, VDA is always zero, so the model is rank deficient).
mlr_Order0 <- glmer(Choice ~ VDA + Frame + VDA:Frame + (1|ID), family = binomial,
                       data = BOTH, subset=(Order==0), control= glmerControl(optimizer="bobyqa"))
summary(mlr_Order0)

#Aspect order is NEGATIVE.
mlr_OrderNeg <- glmer(Choice ~ VDA + Frame + VDA:Frame + (1|ID), family = binomial,
                       data = BOTH, subset=(Order==-0.5), control= glmerControl(optimizer="bobyqa"))
summary(mlr_OrderNeg)

#Moderating effects of DOMAIN (contrast coded above, for consistency with preregistration)
#Full model
mlr_DomainFull <- glmer(Choice ~ VDA + Frame + VDA:Frame + 
                        DomainCode1 + DomainCode2 + DomainCode3 +
                        VDA*DomainCode1 + VDA*DomainCode2 + VDA*DomainCode3 +
                        Frame*DomainCode1 + Frame*DomainCode2 + Frame*DomainCode3 +
                        (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr_DomainFull)

#Without VDA interactions (to test them)
mlr_DomainNoVDAInt <- glmer(Choice ~ VDA + Frame + VDA:Frame + 
                        DomainCode1 + DomainCode2 + DomainCode3 +
                        Frame*DomainCode1 + Frame*DomainCode2 + Frame*DomainCode3 +
                        (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr_DomainNoVDAInt)

#Without Frame interactions (to test them)
mlr_DomainNoFrameInt <- glmer(Choice ~ VDA + Frame + VDA:Frame + 
                        DomainCode1 + DomainCode2 + DomainCode3 +
                        VDA*DomainCode1 + VDA*DomainCode2 + VDA*DomainCode3 +
                        (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr_DomainNoFrameInt)

#Without either set of interactions (to test Domain)
mlr_DomainSimple <- glmer(Choice ~ VDA + Frame + VDA:Frame + 
                        DomainCode1 + DomainCode2 + DomainCode3 +
                        (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr_DomainSimple)

#Test of Domain main effects
anova(mlr1, mlr_DomainSimple)
#Test of VDA*Domain interactions
anova(mlr_DomainNoVDAInt, mlr_DomainFull)
#Test of Frame*Domain interactions
anova(mlr_DomainNoFrameInt, mlr_DomainFull)

#Separate primary models for EACH DOMAIN
#No random effect because there is only one data point per subject.
#The BICs are not useful for comparing the domains.

#Domain is DISEASE.
glm_Disease <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                   data = BOTH, subset=(Domain=="Disease"))
summary(glm_Disease)
BIC(glm_Disease)

#Domain is INVESTMENT.
glm_Invest <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                   data = BOTH, subset=(Domain=="Invest"))
summary(glm_Invest)
BIC(glm_Invest)

#Domain is WILDFIRE.
glm_Wildfire <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                   data = BOTH, subset=(Domain=="Wildfire"))
summary(glm_Wildfire)
BIC(glm_Wildfire)

#Domain is DROUGHT.
glm_Drought <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                   data = BOTH, subset=(Domain=="Drought"))
summary(glm_Drought)
BIC(glm_Drought)

#Moderating effects of DOMAIN POSITION (DomainOrderCode)
#Coded 1, 2, 3, 4, but then centered to -1.5, -0.5, 0.5, 1.5. Higher position is later.
mlr_Position <- glmer(Choice ~ VDA + Frame + VDA:Frame + DomainOrderCode + DomainOrderCode:Frame + DomainOrderCode:VDA + (1|ID), family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlr_Position)

#Separate primary models for EACH DOMAIN POSITION
#No random effect because there is only one data point per subject.
#The BICs are not useful for comparing the domain positions.

#Domain Position is FIRST.
glm_Pos1 <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                   data = BOTH, subset=(DomainOrder==1))
summary(glm_Pos1)
BIC(glm_Pos1)

#Domain Position is SECOND.
glm_Pos2 <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                  data = BOTH, subset=(DomainOrder==2))
summary(glm_Pos2)
BIC(glm_Pos2)

#Domain Position is THIRD.
glm_Pos3 <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                    data = BOTH, subset=(DomainOrder==3))
summary(glm_Pos3)
BIC(glm_Pos3)

#Domain Position is FOURTH.
glm_Pos4 <- glm(Choice ~ VDA + Frame + VDA:Frame, family = binomial,
                   data = BOTH, subset=(DomainOrder==4))
summary(glm_Pos4)
BIC(glm_Pos4)

#TEST OF INTERACTIONS WITH DEMOGRAPHIC VARIABLES, FOR GENERALITY
#All without the VDA:Frame interaction

#THESE TESTS CONTROL FOR SAMPLE AND ITS INTERACTIONS WITH VDA AND FRAME
#(IF DEMOGRAPHIC EFFECTS ARE REAL RATHER THAN SPURIOUS, THEY SHOULD SURVIVE.)

#Sample
mlrMTurkSamp <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp + (1|ID),
                       family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrMTurkSamp)

#Sample and Age (centered)
#Linear
mlrSampAgeDev <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp + 
                          AgeDev + VDA:AgeDev + Frame:AgeDev + (1|ID),
                        family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrSampAgeDev)

#Sample and Age (centered)
#Linear and Quadratic, Fails to converge
mlrSampAgeDevSq <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp + 
                           AgeDev + AgeDevSq + VDA:AgeDev + Frame:AgeDev + 
                           VDA:AgeDevSq + Frame:AgeDevSq + (1|ID),
                         family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrSampAgeDevSq)

#Sample and Female
mlrSampFemale <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                      Female + VDA:Female + Frame:Female + (1|ID),
                    family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrSampFemale)

#Sample and White (inclusive of mixed)
mlrWhiteInc <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                        WhiteInc + VDA:WhiteInc + Frame:WhiteInc + (1|ID),
                      family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrWhiteInc)

#Sample and White (exclusive of mixed)
mlrWhiteExc <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                        WhiteExc + VDA:WhiteExc + Frame:WhiteExc + (1|ID),
                      family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrWhiteExc)

#Sample and Dummies for Asian and Black (White is excluded category, all others missing)
#Be careful interpreting anything other than highest-order interactions.
mlrSampAsianBlack <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp + 
                              AsianD + BlackD + 
                              VDA:AsianD + Frame:AsianD + VDA:BlackD + Frame:BlackD + (1|ID),
                            family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrSampAsianBlack)

#Sample and Dummies for Asian and Black (White is excluded category, all others missing)
#NO VDA INTERACTIONS for chi-square test.
mlrSampAsianBlackNoVDAInt <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp + 
                                      AsianD + BlackD + 
                                      Frame:AsianD + Frame:BlackD + (1|ID),
                                    family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
#summary(mlrSampAsianBlackNoVDAInt)

#Test of VDA*Race interactions
anova(mlrSampAsianBlack, mlrSampAsianBlackNoVDAInt)

#Sample and Dummies for Asian and Black (White is excluded category, all others missing)
#NO FRAME INTERACTIONS for chi-square test.
mlrSampAsianBlackNoFrameInt <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp + 
                                        AsianD + BlackD + 
                                        VDA:AsianD + VDA:BlackD + (1|ID),
                                      family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
#summary(mlrSampAsianBlackNoFrameInt)

#Test of Frame*Race interactions
anova(mlrSampAsianBlack, mlrSampAsianBlackNoFrameInt)

#Sample and Hispanic
mlrSampHispanic <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                            Hispanic + VDA:Hispanic + Frame:Hispanic + (1|ID),
                          family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrSampHispanic)

#Sample and Dummies for Region (Midwest is excluded category)
#Be careful interpreting anything other than highest-order interactions.
mlrSampRegion <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                          NortheastD + SouthD + WestD +
                          VDA:NortheastD + Frame:NortheastD + VDA:SouthD + Frame:SouthD + VDA:WestD + Frame:WestD + (1|ID),
                        family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
summary(mlrSampRegion)

#NO VDA INTERACTIONS for chi-square test.
mlrSampRegionNoVDAInt <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                                  NortheastD + SouthD + WestD +
                                  Frame:NortheastD + Frame:SouthD + Frame:WestD + (1|ID),
                                family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
#summary(mlrSampRegionNoVDAInt)

#Test of VDA*Region interactions
anova(mlrSampRegion, mlrSampRegionNoVDAInt)

#NO FRAME INTERACTIONS for chi-square test.
mlrSampRegionNoFrameInt <- glmer(Choice ~ VDA + Frame + MTurkSamp + VDA:MTurkSamp + Frame:MTurkSamp +
                                    NortheastD + SouthD + WestD +
                                    VDA:NortheastD + VDA:SouthD + VDA:WestD + (1|ID),
                                  family = binomial, data = BOTH, control= glmerControl(optimizer="bobyqa"))
#summary(mlrSampRegionNoFrameInt)

#Test of Frame*Region interactions
anova(mlrSampRegion, mlrSampRegionNoFrameInt)

#ALTERNATIVE LINEAR MODELS (mostly without interactions)

#For a table in the Supplemental Material.
#We used lmer() from the lmertest package to get p-values here.
#Results very similar to the mixed-effects logistic models, as expected.

#Primary linear model
lin1 <- lmer(Rate ~ VDA + Frame + VDA:Frame + (1|ID), data = BOTH, REML = FALSE)
summary(lin1)

#Primary model with no interaction for comparison
lin1NI <- lmer(Rate ~ VDA + Frame + (1|ID), data = BOTH, REML = FALSE)
summary(lin1NI)

#EVA with all nonzero gist differences being the same (+/-1 instead of +/-2)
linEVAequalNI <- lmer(Rate ~ EVAequal + Frame + (1|ID), data = BOTH, REML = FALSE)
summary(linEVAequalNI)

#EVA with separate variables for each type of valence difference (no interactions)
linEVAdiffNI <- lmer(Rate ~ EVApm + EVAmn + EVApn + Frame + (1|ID), data = BOTH, REML = FALSE)
summary(linEVAdiffNI)

#FTT interpretation 1
linFTT1NI <- lmer(Rate ~ FTT1 + Frame + (1|ID), data = BOTH, REML = FALSE)
summary(linFTT1NI)

#FTT interpretation 2
linFTT2NI <- lmer(Rate ~ FTT2 + Frame + (1|ID), data = BOTH, REML = FALSE)
summary(linFTT2NI)
anova(linFTT2NI)

#TESTS OF FRAMING EFFECTS IN MATCHED AND MISMATCHED COMPARISONS

#And confidence intervals for figures.
#There were errors when generating profile confidence intervals for some comparisons,
#so we generated Wald confidence intervals, which are symmetric.

#NINE MATCHED COMPARISONS

#Create tables for holding the results.
#FE_Coef_CI is the table for the estimated marginal mean and confidence interval
#in each of the 18 cells (for Figure 3).
x<- c("Mean","CI_lb","CI_ub")
FE_Coef_CI <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(FE_Coef_CI) <- x

#Overall_FE_Coef() stores the coefficient for Frame (and its CI) in each logistic regression,
#for use in apples-to-apples comparisons (for Figure 2).
y <- c("Mean","CI_lb","CI_ub")
Overall_FE_Coef <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(Overall_FE_Coef) <- y

#G1 vs L1 (with more detail and checks)

#For the primary model plot (Figure 3), we will need to get the estimated marginal
#mean and confidence interval in each of the 18 cells.
#The lsmeans() function gives least square means and confidence interval for factors
#of a fixed part of mixed effect model of lmer object (from web).
#Note: lsmeans() has now been BOTHlaced by emmeans(), but it still works.

#For the apples-to-apples comparison plot, we need confidence interval of the
#coefficient of Frame. conf() is sufficient. It gives the specified xx% CI.

gL1 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G1' | Cell == 'L1'))
#summary(gL1)

#Means and CIs in each frame
lsmean1 <- data.frame(lsmeans(gL1, ~ Frame, conf = .95))
#lsmean1
#FE_Coef_CI is the table for the estimated marginal mean and confidence interval
#in each of the 18 cells (for Figure 3).
FE_Coef_CI[1,] <- lsmean1[1,c(2,5,6)]
FE_Coef_CI[2,] <- lsmean1[2,c(2,5,6)]

#Estimate and CI for the Frame coefficient
#Overall_FE_Coef() stores the coefficient for Frame (and its CI) in each logistic regression,
#for use in apples-to-apples comparisons (for Figure 2).
tab1 <- confint(gL1, level = 0.95, method = "Wald")
#tab1
Overall_FE_Coef[1,1] <- summary(gL1)$coef[2,1]
Overall_FE_Coef[1,2:3] <- tab1[3,]

#G2 vs L2
gL2 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G2' | Cell == 'L2'))
#summary(gL2)
lsmean2 <- data.frame(lsmeans(gL2, ~ Frame, conf = .95))
FE_Coef_CI[3,] <- lsmean2[1,c(2,5,6)]
FE_Coef_CI[4,] <- lsmean2[2,c(2,5,6)]
tab2 <- confint(gL2, level = 0.95, method = "Wald")
Overall_FE_Coef[2,1] <- summary(gL2)$coef[2,1]
Overall_FE_Coef[2,2:3] <- tab2[3,]

#G3 vs L3
gL3 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G3' | Cell == 'L3'))
#summary(gL3)
lsmean3 <- data.frame(lsmeans(gL3, ~ Frame, conf = .95))
FE_Coef_CI[5,] <- lsmean3[1,c(2,5,6)]
FE_Coef_CI[6,] <- lsmean3[2,c(2,5,6)]
tab3 <- confint(gL3, level = 0.95, method = "Wald")
Overall_FE_Coef[3,1] <- summary(gL3)$coef[2,1]
Overall_FE_Coef[3,2:3] <- tab3[3,]

#G4 vs L4
gL4 <- glmer(Choice ~ Frame + (1|ID),  family = binomial, data = filter(BOTH, Cell == 'G4' | Cell == 'L4'))
#summary(gL4)
lsmean4 <- data.frame(lsmeans(gL4, ~ Frame, conf = .95))
FE_Coef_CI[7,] <- lsmean4[1,c(2,5,6)]
FE_Coef_CI[8,] <- lsmean4[2,c(2,5,6)]
tab4 <- confint(gL4, level = 0.95, method = "Wald")
Overall_FE_Coef[4,1] <- summary(gL4)$coef[2,1]
Overall_FE_Coef[4,2:3] <- tab4[3,]

#G5 vs L5
gL5 <- glmer(Choice ~ Frame + (1|ID),  family = binomial, data = filter(BOTH, Cell == 'G5' | Cell == 'L5'))
#summary(gL5)
lsmean5 <- data.frame(lsmeans(gL5, ~ Frame, conf = .95))
FE_Coef_CI[9,] <- lsmean5[1,c(2,5,6)]
FE_Coef_CI[10,] <- lsmean5[2,c(2,5,6)]
tab5 <- confint(gL5, level = 0.95, method = "Wald")
Overall_FE_Coef[5,1] <- summary(gL5)$coef[2,1]
Overall_FE_Coef[5,2:3] <- tab5[3,]

#G6 vs L6
gL6 <- glmer(Choice ~ Frame + (1|ID),  family = binomial, data = filter(BOTH, Cell == 'G6' | Cell == 'L6'))
#summary(gL6)
lsmean6 <- data.frame(lsmeans(gL6, ~ Frame, conf = .95))
FE_Coef_CI[11,] <- lsmean6[1,c(2,5,6)]
FE_Coef_CI[12,] <- lsmean6[2,c(2,5,6)]
tab6 <- confint(gL6, level = 0.95, method = "Wald")
Overall_FE_Coef[6,1] <- summary(gL6)$coef[2,1]
Overall_FE_Coef[6,2:3] <- tab6[3,]

#G7 vs L7
gL7 <- glmer(Choice ~ Frame + (1|ID),  family = binomial, data = filter(BOTH, Cell == 'G7' | Cell == 'L7'))
#summary(gL7)
lsmean7 <- data.frame(lsmeans(gL7, ~ Frame, conf = .95))
FE_Coef_CI[13,] <- lsmean7[1,c(2,5,6)]
FE_Coef_CI[14,] <- lsmean7[2,c(2,5,6)]
tab7 <- confint(gL7, level = 0.95, method = "Wald")
Overall_FE_Coef[7,1] <- summary(gL7)$coef[2,1]
Overall_FE_Coef[7,2:3] <- tab7[3,]

#G8 vs L8
gL8 <- glmer(Choice ~ Frame + (1|ID),  family = binomial, data = filter(BOTH, Cell == 'G8' | Cell == 'L8'))
#summary(gL8)
lsmean8 <- data.frame(lsmeans(gL8, ~ Frame, conf = .95))
FE_Coef_CI[15,] <- lsmean8[1,c(2,5,6)]
FE_Coef_CI[16,] <- lsmean8[2,c(2,5,6)]
tab8 <- confint(gL8, level = 0.95, method = "Wald")
Overall_FE_Coef[8,1] <- summary(gL8)$coef[2,1]
Overall_FE_Coef[8,2:3] <- tab8[3,]

#G9 vs L9
gL9 <- glmer(Choice ~ Frame + (1|ID),  family = binomial, data = filter(BOTH, Cell == 'G9' | Cell == 'L9'))
#summary(gL9)
lsmean9 <- data.frame(lsmeans(gL9, ~ Frame, conf = .95))
FE_Coef_CI[17,] <- lsmean9[1,c(2,5,6)]
FE_Coef_CI[18,] <- lsmean9[2,c(2,5,6)]
tab9 <- confint(gL9, level = 0.95, method = "Wald")
Overall_FE_Coef[9,1] <- summary(gL9)$coef[2,1]
Overall_FE_Coef[9,2:3] <- tab9[3,]

#PLOT FOR THE PRIMARY MODEL (SIMILAR TO FIGURE 3 IN THE PAPER)

#Offset X values so points don't overlap.
FE_Coef_CI$VDA <- c(-0.13, -0.13, 0.935, 0.935, 2, 2, -1.065, -1.065, 0, 0, 1.065, 1.065, -2, -2, -0.935, -0.935, 0.13, 0.13)
FE_Coef_CI$Frame <- c(-0.5, 0.5, -0.5, 0.5,-0.5, 0.5,-0.5, 0.5,-0.5, 0.5,-0.5, 0.5,-0.5, 0.5,-0.5, 0.5,-0.5, 0.5)
FE_Coef_CI$Cell <- c("G1","L1","G2","L2","G3","L3","G4","L4","G5","L5","G6","L6","G7","L7","G8","L8","G9","L9")
FE_Coef_CI$Mean <- signif(FE_Coef_CI$Mean, 5)
FE_Coef_CI$CI_lb <- signif(FE_Coef_CI$CI_lb, 5)
FE_Coef_CI$CI_ub <- signif(FE_Coef_CI$CI_ub, 5)
FE_Coef_CI$Frame_text <- 0

#Convert logits to proportions (not used for paper).
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

FE_Coef_CI$Prob <- logit2prob(FE_Coef_CI$Mean)
FE_Coef_CI$Prob_lb <- logit2prob(FE_Coef_CI$CI_lb)
FE_Coef_CI$Prob_ub <- logit2prob(FE_Coef_CI$CI_ub)

dim(FE_Coef_CI)[1]

for (i in 1:dim(FE_Coef_CI)[1]){
  if(FE_Coef_CI[i , 5] == 0.5){
    FE_Coef_CI[i, 7] <- "Loss"
  }else{
    FE_Coef_CI[i, 7] <- "Gain"
  }
}

#Output files for DataGraph (used for creating the figures in the paper).
#Line plots
#FE_Coef_CI
#write.csv(FE_Coef_CI, file="BOTHLinePlotPoints.csv")
#mlr1_Lines
#write.csv(mlr1_Lines, file="BOTHLinePlotLines.csv")
#Matched comparisons
#write.csv(Overall_FE_Coef, "BOTHMatched.csv")

set.seed(0815)
df <- data.frame(x = FE_Coef_CI$VDA,
                 FE_Coef = FE_Coef_CI$Mean,
                 pt_text = FE_Coef_CI$Cell,
                 L = FE_Coef_CI$CI_lb,
                 U = FE_Coef_CI$CI_ub, 
                 Frame = FE_Coef_CI$Frame_text,
                 prob = FE_Coef_CI$Prob,
                 prob_L = FE_Coef_CI$Prob_lb,
                 prob_U = FE_Coef_CI$Prob_ub)

BOTH$Frame.text <- "O"
typeof(BOTH$Frame.text)
typeof(BOTH$Frame)

#Transform the Frame attribute so "gain" "loss" will appear as text in the label.
for (i in 1:dim(BOTH)[1]){
  if(BOTH[i,"Frame"] == 0.5){
    BOTH[i,17] <- "Loss"
  }else{
    BOTH[i,17] <- "Gain"
  }
}
BOTH$Frame.text <- as.factor(BOTH$Frame.text)
BOTH$Frame <- as.factor(BOTH$Frame)

pd <- position_dodge(width = 0)

#The code for the Gain and Loss lines uses the endpoints (e.g., GainLeft) defined
#right after the initial run of the primary model (far above).
p2 <- ggplot(df, aes(x = x, y = FE_Coef, color = Frame, shape = Frame)) + geom_point(size = 4.2, position = pd) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + geom_errorbar(aes(ymax = U, ymin = L), width = 0.1, position = pd) +
  geom_segment(aes(x = -2, xend = 2, y = GainLeft, yend = GainRight), color = "#F8766D", size = 1) + 
  geom_segment(aes(x = -2, xend = 2, y = LossLeft, yend = LossRight), color = "#00BFC4", size = 1) + 
  theme_bw() +  
  geom_text(aes(label = pt_text), size = 3.8, color = "black", alpha = 0.8, pos = pd, hjust = -0.15, family = "Helvetica") + 
  xlab("VDA(Risky) — VDA(Sure)") + ylab("Proportion of Participants Choosing \n the Risky Option (Logit Scale)") + theme_bw() + 
  theme( # Change legend key size and key width 
    plot.title = element_text(size = (30), hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 14, colour = "black"),
    legend.background = element_rect(size=0.5),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = c(0.9, 0.2), legend.key.size = unit(0.8, "cm"),
    legend.key.width = unit(0.4,"cm"),
    axis.text=element_text(size = 13, family = "Helvetica"),
    axis.title=element_text(size = 14, family = "Helvetica")) + 
    xlim(-2.05, 2.05) + ylim(-4.2, 3.2) 
p2
#Save the plot.
#ggsave(file="BOTH_18_pts(logit).png", p2)

#PLOT FOR NINE MATCHED COMPARISONS (SIMILAR TO FIGURE 2 IN THE PAPER)

set.seed(0815)
df.1 <- data.frame(x = 1:9,
                   FE_Coef = Overall_FE_Coef[,1],
                   Nine_L = Overall_FE_Coef[,2],
                   Nine_U = Overall_FE_Coef[,3])

abbrev_x <- c("L1—G1","L2—G2","L3—G3","L4—G4","L5—G5","L6—G6","L7—G7","L8—G8","L9—G9")

comparison1 <- ggplot(df.1, aes(x = x, y = FE_Coef)) + geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_y_continuous(minor_breaks = seq(-2, 4, 2), limits = c(-1.75,4), breaks=(seq(-2, 4, 1))) + 
  
  geom_errorbar(data = df.1[1,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[2,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[3,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[5,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[6,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[7,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[8,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[4,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.1[9,], aes(ymax = Nine_U, ymin = Nine_L), width=0.2, size=1, color="black") +
  annotate("text", x = 2.3, y = -1.25, label = c("Error bars BOTHresent 95% CIs."), color = "black", size = 4.5) +
  
  geom_point(size = 3, shape=19, color ="black", alpha = 0.5) + scale_x_continuous(minor_breaks = seq(1, 9, 2),breaks = c(1:9), labels = abbrev_x) +  #scale_y_continuous(limits = c(-5, 8.5), breaks=(seq(-4, 8, 1))) + 
  xlab("Matched Cell Pairs") + ylab("Framing Effect (Coefficient = ln(OR))") + 
  theme_bw() + theme( # Change legend key size and key width 
    axis.text=element_text(size = 12, family = "Helvetica"),
    axis.title=element_text(size = 14, family = "Helvetica")) 
comparison1
#Save the plot.
#ggsave(file="9_matched_BOTH.png", comparison1)

#PLOT FOR NINE MISMATCHED COMPARISONS (SIMILAR TO HIGHLIHTED CASES IN FIGURE 1 OF THE PAPER)

#Create a table for the results.
x<- c("Mean","CI_lb","CI_ub")
Mismatched_FE_Coef <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(Mismatched_FE_Coef) <- x

#L1 VS.G9
mismatched1 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G9' | Cell == 'L1'))
#summary(mismatched1)
CI_1 <- confint(mismatched1, level = 0.95, method = "Wald")
#CI_1
Mismatched_FE_Coef[1,1] <- summary(mismatched1)$coef[2,1]
Mismatched_FE_Coef[1,2:3] <- CI_1[3,]

#L2 VS.G8
mismatched2 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G8' | Cell == 'L2'))
CI_2 <- confint(mismatched2, level = 0.95, method = "Wald")
Mismatched_FE_Coef[2,1] <- summary(mismatched2)$coef[2,1]
Mismatched_FE_Coef[2,2:3] <- CI_2[3,]

#L3 VS.G7
mismatched3 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G7' | Cell == 'L3'))
#summary(mismatched3)
CI_3 <- confint(mismatched3, level = 0.95, method = "Wald")
CI_3
Mismatched_FE_Coef[3,1] <- summary(mismatched3)$coef[2,1]
Mismatched_FE_Coef[3,2:3] <- CI_3[3,]

#L4 VS.G6
mismatched4 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G6' | Cell == 'L4'))
summary(mismatched4)
CI_4 <- confint(mismatched4, level = 0.95, method = "Wald")
Mismatched_FE_Coef[4,1] <- summary(mismatched4)$coef[2,1]
Mismatched_FE_Coef[4,2:3] <- CI_4[3,]

#L5 VS.G5
mismatched5 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G5' | Cell == 'L5'))
#summary(mismatched5)
CI_5 <- confint(mismatched5, level = 0.95, method = "Wald")
Mismatched_FE_Coef[5,1] <- summary(mismatched5)$coef[2,1]
Mismatched_FE_Coef[5,2:3] <- CI_5[3,]

#L6 VS.G4
mismatched6 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G4' | Cell == 'L6'))
#summary(mismatched6)
CI_6 <- confint(mismatched6, level = 0.95, method = "Wald")
Mismatched_FE_Coef[6,1] <- summary(mismatched6)$coef[2,1]
Mismatched_FE_Coef[6,2:3] <- CI_6[3,]

#L7 VS.G3
mismatched7 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G3' | Cell == 'L7'))
CI_7 <- confint(mismatched7, level = 0.95, method = "Wald")
Mismatched_FE_Coef[7,1] <- summary(mismatched7)$coef[2,1]
Mismatched_FE_Coef[7,2:3] <- CI_7[3,]

#L8 VS.G2
mismatched8 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G2' | Cell == 'L8'))
#summary(mismatched8)
CI_8 <- confint(mismatched8, level = 0.95, method = "Wald")
Mismatched_FE_Coef[8,1] <- summary(mismatched8)$coef[2,1]
Mismatched_FE_Coef[8,2:3] <- CI_8[3,]

#L9 VS.G1
mismatched9 <- glmer(Choice ~ Frame + (1|ID), family = binomial, data = filter(BOTH, Cell == 'G1' | Cell == 'L9'))
#summary(mismatched9)
CI_9 <- confint(mismatched9, level = 0.95, method = "Wald")
Mismatched_FE_Coef[9,1] <- summary(mismatched9)$coef[2,1]
Mismatched_FE_Coef[9,2:3] <- CI_9[3,]

#Convert logits to proportions (not used in paper).
Mismatched_FE_Coef$Prob <- logit2prob(Mismatched_FE_Coef$Mean)
Mismatched_FE_Coef$Prob_lb <- logit2prob(Mismatched_FE_Coef$CI_lb)
Mismatched_FE_Coef$Prob_ub <- logit2prob(Mismatched_FE_Coef$CI_ub)

#Output file for DataGraph (used for creating the figure in the paper).
#write.csv(Mismatched_FE_Coef, "BOTHMisatched.csv")

set.seed(0815)
df.2 <- data.frame(x = 1:9,
                   mismatched9_Coef = Mismatched_FE_Coef[,1],
                   mismatched9_L = Mismatched_FE_Coef[,2],
                   mismatched9_U = Mismatched_FE_Coef[,3],
                   mismatched9_prob = Mismatched_FE_Coef[,4],
                   mismatched9_prob_L = Mismatched_FE_Coef[,5],
                   mismatched9_prob_U = Mismatched_FE_Coef[,6])

abbrev_x_new_reorder <- c("L7—G3","L4—G6","L8—G2","L9—G1","L5—G5","L1—G9","L6—G4","L2—G8","L3—G7")

df.2.reordered <- df.2[c(7,4,8,9,5,1,6,2,3),]
df.2.reordered$new_x <- seq.int(nrow(df.2.reordered))

comparison_new_reorder <- ggplot(df.2.reordered, aes(x = new_x, y = mismatched9_Coef)) + geom_hline(aes(yintercept = 0), linetype = "dashed") +
  
  geom_point(data = df.2.reordered[1,], size = 3, shape=19, color ="firebrick3", alpha = 0.5) +
  geom_point(data = df.2.reordered[2,], size = 3, shape=19, color ="purple2", alpha = 0.5) +
  geom_point(data = df.2.reordered[3,], size = 3, shape=19, color ="purple2", alpha = 0.5) +
  geom_point(data = df.2.reordered[4,], size = 3, shape=19, color ="black", alpha = 0.5) +
  geom_point(data = df.2.reordered[5,], size = 3, shape=19, color ="black", alpha = 0.5) +
  geom_point(data = df.2.reordered[6,], size = 3, shape=19, color ="black", alpha = 0.5) +
  geom_point(data = df.2.reordered[7,], size = 3, shape=19, color ="royalblue3", alpha = 0.5) +
  geom_point(data = df.2.reordered[8,], size = 3, shape=19, color ="royalblue3", alpha = 0.5) +
  geom_point(data = df.2.reordered[9,], size = 3, shape=19, color ="palegreen4", alpha = 0.5) +
  
  scale_x_continuous(minor_breaks = seq(1, 9, 2), breaks = c(1:9), labels = abbrev_x_new_reorder) + 
  xlab("Mismatched Cell Pairs") + ylab("Framing Effect (Coefficient = ln(OR))") + 
  scale_y_continuous(minor_breaks = seq(-4, 9, 2), limits = c(-4.2, 9.52), breaks=(seq(-4, 9, 1))) + 
  theme_bw() + 
  
  geom_errorbar(data = df.2.reordered[1,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="firebrick3") + 
  geom_errorbar(data = df.2.reordered[2,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="purple2") +
  geom_errorbar(data = df.2.reordered[3,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="purple2") +
  geom_errorbar(data = df.2.reordered[4,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="black") + 
  geom_errorbar(data = df.2.reordered[5,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.2.reordered[6,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="black") +
  geom_errorbar(data = df.2.reordered[7,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="royalblue3") + 
  geom_errorbar(data = df.2.reordered[8,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="royalblue3") +
  geom_errorbar(data = df.2.reordered[9,], aes(ymax = mismatched9_U, ymin = mismatched9_L), width=0.2, size=1, color="palegreen4") +
  
  annotate("text", x = 2, y = -4.2, label = c("Stronger Reversed FE"), color = "firebrick3", size = 4.5) +
  annotate("text", x = 2.5, y = -2, label = c("Reversed FE"), color = "purple2", size = 4.5) + 
  annotate("text", x = 5, y = -1, label = c("Eliminated FE"), color = "black", size = 4.5) + 
  annotate("text", x = 7.5, y = 0.5, label = c("Standard FE"), color = "royalblue3", size = 4.5) +
  annotate("text", x = 7, y = 5, label = c("Standard ADP"), color = "royalblue3", size = 4.5) +
  annotate("text", x = 8.7, y = 4.9, label = c("Amplified FE"), color = "palegreen4", size = 4.5) +
  annotate("text", x = 2.7, y = 7, label = c("Colors BOTHresent predictions based on \n the VDA variation tables only."), color = "black", size = 4.5) +
  theme_bw() + theme( # Change legend key size and key width 
    #panel.grid.minor = element_line(size = 2), 
    axis.text=element_text(size = 12, family = "Helvetica"),
    axis.title=element_text(size = 14, family = "Helvetica")) +
    geom_segment(aes(x = 7, y = 4.6, xend = 7, yend = 3.6), color = "royalblue3",
                 arrow = arrow(length = unit(0.5, "cm")))
comparison_new_reorder

#Save the plot.
#ggsave(file="9_mismatched_BOTH.png", comparison_new_reorder)

#FRAMING EFFECTS FOR ALL 81 POSSIBLE COMPARISONS OF GAIN AND LOSS CELLS
#No plot here. Just an output file for the plot in DataGraph (for Figure 1 in the paper).

#Create two counter variables for loops
BOTH <- within(BOTH, {
  GainCell <- NA
  LossCell <- NA
  GainCell[Cell == "G1"] <- 1
  GainCell[Cell == "G2"] <- 2
  GainCell[Cell == "G3"] <- 3
  GainCell[Cell == "G4"] <- 4
  GainCell[Cell == "G5"] <- 5
  GainCell[Cell == "G6"] <- 6
  GainCell[Cell == "G7"] <- 7
  GainCell[Cell == "G8"] <- 8
  GainCell[Cell == "G9"] <- 9
  LossCell[Cell == "L1"] <- 1
  LossCell[Cell == "L2"] <- 2
  LossCell[Cell == "L3"] <- 3
  LossCell[Cell == "L4"] <- 4
  LossCell[Cell == "L5"] <- 5
  LossCell[Cell == "L6"] <- 6
  LossCell[Cell == "L7"] <- 7
  LossCell[Cell == "L8"] <- 8
  LossCell[Cell == "L9"] <- 9
})

#Create a table for the results.
x<- c("Comp", "LossCell", "GainCell", "Mean","CI_lb","CI_ub")
All_FE_Coef <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(All_FE_Coef) <- x

#Run a mixed-effects logistic regression in each of the 81 combinations.
for(L in 1:9){
  for(G in 1:9){
    rownum <- 9*(L - 1) + G
    all_LG <- glmer(Choice ~ Frame + (1|ID), family = binomial,
                   data = filter(BOTH, LossCell == L | GainCell == G))
    CI_LG <- confint(all_LG, level = 0.95, method = "Wald")
    All_FE_Coef[rownum,1] <- rownum
    All_FE_Coef[rownum,2] <- L
    All_FE_Coef[rownum,3] <- G
    All_FE_Coef[rownum,4] <- summary(all_LG)$coef[2,1]
    All_FE_Coef[rownum,5:6] <- CI_LG[3,]
  }
}

#Add a few things to the file.
All_FE_Coef <- within(All_FE_Coef, {
  Error <- CI_ub - Mean #Errors symmetric with Wald
  #Recreate Valence Differences (VDA) and implied Framing Effects
  LossVDA <- NA
  GainVDA <- NA
  LossVDA[LossCell == 3] <- 2
  LossVDA[LossCell %in% c(2, 6)] <- 1
  LossVDA[LossCell %in% c(1, 5, 9)] <- 0
  LossVDA[LossCell %in% c(4, 8)] <- -1
  LossVDA[LossCell == 7] <- -2
  GainVDA[GainCell == 3] <- 2
  GainVDA[GainCell %in% c(2, 6)] <- 1
  GainVDA[GainCell %in% c(1, 5, 9)] <- 0
  GainVDA[GainCell %in% c(4, 8)] <- -1
  GainVDA[GainCell == 7] <- -2
  PredFE = (LossVDA - GainVDA)/2
})
  
#Check the file and output it for DataGraph.
#All_FE_Coef
#write.csv(All_FE_Coef, file="All81FEsBOTH.csv")

#END OF CODE
