## Charlesworth, & Banaji Patterns of Change I.
## Script version date: January 21st, 2018

####################################################################################################################################

## This file fits all of the multiple regression models, with the covariates and the interactions with time...

## This file also illustrates how one would perform the simple slopes analyses for the directly relevant covariates (splitting samples by demographic subgroups)
# These are not reported analyses in the text or supplementals, but may be of additional interest.

## starting with the full.demogs.RData data object, which includes the US only participants with cleaned data and centered variables
# note: the full.demogs files also include US zipcodes, and are therefore secure data

####################################################################################################################################

## Load the data
load("full.demogs.NOZIP_012518.RData")


# Install packages
install.packages("lsr", dependencies = TRUE)
install.packages("lm.beta", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("nlme", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)

# Load packages
require(lsr)
require(lm.beta)
require(car)
require(nlme)
require(lmtest)


#######################################################################################################################
#######################################################################################################################
## IMPLICIT -----------------------------------------------------------------------------------------------------------

# quick cleaning: center all the order variables
age.full.demogs$order.center <- scale(as.numeric(as.numeric(age.full.demogs$order.new), center = TRUE, scale = FALSE))
dis.full.demogs$order.center <- scale(as.numeric(dis.full.demogs$order.new), center = TRUE, scale = FALSE)
race.full.demogs$order.center <- scale(as.numeric(race.full.demogs$order.new), center = TRUE, scale = FALSE)
sex.full.demogs$order.center <- scale(as.numeric(sex.full.demogs$order.new), center = TRUE, scale = FALSE)
skin.full.demogs$order.center <- scale(as.numeric(skin.full.demogs$order.new), center = TRUE, scale = FALSE)
weight.full.demogs$order.center <- scale(as.numeric(weight.full.demogs$order.new), center = TRUE, scale = FALSE)

# keep only 2007 onwards for all but weightface
age2007 <- age.full.demogs[age.full.demogs$month.cent>37,]
dis2007 <- dis.full.demogs[dis.full.demogs$month.cent>37,]
race2007 <- race.full.demogs[race.full.demogs$month.cent>37,]
sex2007 <- sex.full.demogs[sex.full.demogs$month.cent>37,]
skin2007 <- skin.full.demogs[skin.full.demogs$month.cent>37,]
weight2007 <- weight.full.demogs[weight.full.demogs$month.cent>37,]


## Age -----------------------------------------------------------------------------------------------------------
dim(age2007)[1]
age.model1 <- lm(dscore ~ month.cent, data = age2007)
summary(age.model1); etaSquared(age.model1); lm.beta(age.model1)

age.fullmodel <- lm(dscore ~ month.cent + order.center + ethnicrace.center  + sex.center + age.center +
                      politicalid.center + edu.center +
                      month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                      month.cent*politicalid.center + month.cent*edu.center, data = age2007)

summary(age.fullmodel); etaSquared(age.fullmodel); lm.beta(age.fullmodel)

dwtest(age.fullmodel) # significant autocorrelation; DW = 1.9928, p-value = 0.008198

# predictions
# number of months = -b/m
summary(age.fullmodel)$coefficients["(Intercept)", "Estimate"] # b
summary(age.fullmodel)$coefficients["month.cent", "Estimate"] # m
# number of months = 
-summary(age.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(age.fullmodel)$coefficients["month.cent", "Estimate"]
# number of years = 
-summary(age.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(age.fullmodel)$coefficients["month.cent", "Estimate"]/12

## conclusion = remaining stable


## Disability -----------------------------------------------------------------------------------------------------------
dim(dis2007)[1]
dis.model1 <- lm(dscore ~ month.cent, data = dis2007)
summary(dis.model1); etaSquared(dis.model1); lm.beta(dis.model1)

dis.fullmodel <- lm(dscore ~ month.cent + order.center + ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                              disabled.center + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                              month.cent*politicalid.center + month.cent*edu.center + month.cent*disabled.center, data = dis2007)
summary(dis.fullmodel); etaSquared(dis.fullmodel); lm.beta(dis.fullmodel)

dwtest(dis.fullmodel) # non-significant autocorrelation; DW = 1.9946, p-value = 0.1327

# predictions
# number of months for y = 2b, thus months = b/m
summary(dis.fullmodel)$coefficients["(Intercept)", "Estimate"] # b
summary(dis.fullmodel)$coefficients["month.cent", "Estimate"] # m
# number of months = 
- summary(dis.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(dis.fullmodel)$coefficients["month.cent", "Estimate"]
# number of years = 
- summary(dis.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(dis.fullmodel)$coefficients["month.cent", "Estimate"]/12

## conclusion = remaining stable

## Race -----------------------------------------------------------------------------------------------------------
dim(race2007)[1]
race.model1 <- lm(dscore ~ month.cent, data = race2007)
summary(race.model1); etaSquared(race.model1); lm.beta(race.model1)

race.fullmodel <- lm(dscore ~ month.cent + order.center + ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                               month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                               month.cent*politicalid.center + month.cent*edu.center, data = race2007)
summary(race.fullmodel); etaSquared(race.fullmodel); lm.beta(race.fullmodel)

dwtest(race.fullmodel) # significant autocorrelation; DW = 1.9607, p-value < 2.2e-16

# predictions
# number of months = -b/m
summary(race.fullmodel)$coefficients["(Intercept)", "Estimate"] # b
summary(race.fullmodel)$coefficients["month.cent", "Estimate"] # m
-summary(race.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(race.fullmodel)$coefficients["month.cent", "Estimate"]
-summary(race.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(race.fullmodel)$coefficients["month.cent", "Estimate"]/12

# conclusion = change to neutral



## Sexuality -----------------------------------------------------------------------------------------------------------
dim(sex2007)[1]
# (first center the new covariate not included elsewhere)
sex2007$Stimuli.center <- as.numeric(sex2007$Stimuli)
sex2007$Stimuli.center <- scale(sex2007$Stimuli.center, scale = FALSE)

sex.model1 <- lm(dscore ~ month.cent, data = sex2007)
summary(sex.model1); etaSquared(sex.model1); lm.beta(sex.model1)

sex.fullmodel <- lm(dscore ~ month.cent + order.center + Stimuli.center + ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                              sexuality.center + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                              month.cent*politicalid.center + month.cent*edu.center + month.cent*sexuality.center, 
                            data = sex2007)
summary(sex.fullmodel); etaSquared(sex.fullmodel); lm.beta(sex.fullmodel)

dwtest(sex.fullmodel) # significant autocorrelation; DW = 1.983, p-value = 1.221e-11

# predictions
# number of months = -b/m
summary(sex.fullmodel)$coefficients["(Intercept)", "Estimate"] # b
summary(sex.fullmodel)$coefficients["month.cent", "Estimate"] # m
-summary(sex.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(sex.fullmodel)$coefficients["month.cent", "Estimate"]
-summary(sex.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(sex.fullmodel)$coefficients["month.cent", "Estimate"]/12

# conclusion = change to neutral



## SkinTone -----------------------------------------------------------------------------------------------------------
dim(skin2007)[1]
skin.model1 <- lm(dscore ~ month.cent, data = skin2007)
summary(skin.model1); etaSquared(skin.model1); lm.beta(skin.model1)

skin.fullmodel <- lm(dscore ~ month.cent + order.center + ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                               month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                               month.cent*politicalid.center + month.cent*edu.center, data = skin2007)
summary(skin.fullmodel); etaSquared(skin.fullmodel); lm.beta(skin.fullmodel)

dwtest(skin.fullmodel) # significant autocorrelation; DW = 1.9693, p-value < 2.2e-16

# predictions
# number of months = -b/m
summary(skin.fullmodel)$coefficients["(Intercept)", "Estimate"] # b
summary(skin.fullmodel)$coefficients["month.cent", "Estimate"] # m
-summary(skin.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(skin.fullmodel)$coefficients["month.cent", "Estimate"]
-summary(skin.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(skin.fullmodel)$coefficients["month.cent", "Estimate"]/12

# conclusion = change to neutral (but slower than race)



## Weight (Body) -----------------------------------------------------------------------------------------------------------
table(weight.full.demogs$Stimuli.new)
weight.full.demogs$stim.collapse <- recode(weight.full.demogs$Stimuli.new, "'body figure v1' = 'body'; 'body figure v2' = 'body'")
weightbody <- weight.full.demogs[weight.full.demogs$stim.collapse=="body",]
table(weightbody$month)

dim(weightbody)[1]

weight.model1 <- lm(dscore ~ month.cent, data = weightbody)
summary(weight.model1); etaSquared(weight.model1); lm.beta(weight.model1)

weight.fullmodel <- lm(dscore ~ month.cent + order.center + ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                                 weight.center + + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                                 month.cent*politicalid.center + month.cent*edu.center + month.cent*weight.center, data = weightbody)
summary(weight.fullmodel); etaSquared(weight.fullmodel); lm.beta(weight.fullmodel)

dwtest(weight.fullmodel) # significant autocorrelation, DW = 1.9795, p-value = 3.127e-07

# predictions
# number of months = b/m
summary(weight.fullmodel)$coefficients["(Intercept)", "Estimate"] # b
summary(weight.fullmodel)$coefficients["month.cent", "Estimate"] # m
summary(weight.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(weight.fullmodel)$coefficients["month.cent", "Estimate"]
summary(weight.fullmodel)$coefficients["(Intercept)", "Estimate"]/summary(weight.fullmodel)$coefficients["month.cent", "Estimate"]/12

# conclusion = stable

# dwtest(weight.fullmodel)


## Weight (Face) -----------------------------------------------------------------------------------------------------------
weightface <- weight.full.demogs[weight.full.demogs$stim.collapse=="face",]
table(weightface$month)

dim(weightface)[1]

weight.model1face <- lm(dscore ~ month.cent, data = weightface)
summary(weight.model1face); etaSquared(weight.model1face); lm.beta(weight.model1face)

weight.fullmodelface <- lm(dscore ~ month.cent + order.center + ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                         weight.center + + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                         month.cent*politicalid.center + month.cent*edu.center + month.cent*weight.center, data = weightface)
summary(weight.fullmodelface); etaSquared(weight.fullmodelface); lm.beta(weight.fullmodelface)

dwtest(weight.fullmodelface) # significant autocorrelation; DW = 1.9904, p-value = 0.004991

# predictions
# number of months = b/m
summary(weight.fullmodelface)$coefficients["(Intercept)", "Estimate"] # b
summary(weight.fullmodelface)$coefficients["month.cent", "Estimate"] # m
summary(weight.fullmodelface)$coefficients["(Intercept)", "Estimate"]/summary(weight.fullmodelface)$coefficients["month.cent", "Estimate"]
summary(weight.fullmodelface)$coefficients["(Intercept)", "Estimate"]/summary(weight.fullmodelface)$coefficients["month.cent", "Estimate"]/12

# conclusion = changing to double
# dwtest(weight.fullmodelface)



############################################################################################################################################
############################################################################################################################################
## EXPLICIT ---------------------------------------------------------------------------------------------------------------------

## Age -----------------------------------------------------------------------------------------------------------
dim(age2007)[1]

age.model1.e <- lm(att.center1 ~ month.cent, data = age2007)
summary(age.model1.e); etaSquared(age.model1.e); lm.beta(age.model1.e)

age.fullmodel.e <- lm(att.center1 ~ month.cent +  ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                      month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                      month.cent*politicalid.center + month.cent*edu.center, data = age2007)
summary(age.fullmodel.e); etaSquared(age.fullmodel.e); lm.beta(age.fullmodel.e) 

dwtest(age.fullmodel.e) # significant autocorrelation, DW = 1.9761, p-value = 1.016e-15

# predictions
# number of months = -b/m
summary(age.fullmodel.e)$coefficients["(Intercept)", "Estimate"] # b
summary(age.fullmodel.e)$coefficients["month.cent", "Estimate"] # m
-summary(age.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(age.fullmodel.e)$coefficients["month.cent", "Estimate"]
-summary(age.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(age.fullmodel.e)$coefficients["month.cent", "Estimate"]/12

## conclusion = change towards neutrality


## Disability -----------------------------------------------------------------------------------------------------------
dim(dis2007)[1]

dis.model1.e <- lm(att.center1 ~ month.cent, data = dis2007)
summary(dis.model1.e); etaSquared(dis.model1.e); lm.beta(dis.model1.e)

dis.fullmodel.e <- lm(att.center1 ~ month.cent +  ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                      disabled.center + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                      month.cent*politicalid.center + month.cent*edu.center + month.cent*disabled.center, data = dis2007)
summary(dis.fullmodel.e); etaSquared(dis.fullmodel.e); lm.beta(dis.fullmodel.e) 

dwtest(dis.fullmodel.e) # significant autocorrelation; DW = 1.9813, p-value = 6.541e-05

# predictions
# number of months = -b/m
summary(dis.fullmodel.e)$coefficients["(Intercept)", "Estimate"] # b
summary(dis.fullmodel.e)$coefficients["month.cent", "Estimate"] # m
-summary(dis.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(dis.fullmodel.e)$coefficients["month.cent", "Estimate"]
-summary(dis.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(dis.fullmodel.e)$coefficients["month.cent", "Estimate"]/12

## conclusion = changing to neutral

## Race -----------------------------------------------------------------------------------------------------------
dim(race2007)[1]
race.model1.e <- lm(att.center1 ~ month.cent, data = race2007)
summary(race.model1.e); etaSquared(race.model1.e); lm.beta(race.model1.e)

race.fullmodel.e <- lm(att.center1 ~ month.cent +  ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                       month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                       month.cent*politicalid.center + month.cent*edu.center, data = race2007)
summary(race.fullmodel.e); etaSquared(race.fullmodel.e); lm.beta(race.fullmodel.e) 

dwtest(race.fullmodel.e) # significant autocorrelation; DW = 1.9085, p-value < 2.2e-16

# predictions
# number of months = -b/m
summary(race.fullmodel.e)$coefficients["(Intercept)", "Estimate"] # b
summary(race.fullmodel.e)$coefficients["month.cent", "Estimate"] # m
-summary(race.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(race.fullmodel.e)$coefficients["month.cent", "Estimate"]
-summary(race.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(race.fullmodel.e)$coefficients["month.cent", "Estimate"]/12

## conclusion = changing to neutral

## Sexuality -----------------------------------------------------------------------------------------------------------
dim(sex2007)[1]
sex.model1.e <- lm(att.center1 ~ month.cent, data = sex2007)
summary(sex.model1.e); etaSquared(sex.model1.e); lm.beta(sex.model1.e)

sex.fullmodel.e <- lm(att.center1 ~ month.cent +  ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                      sexuality.center + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                      month.cent*politicalid.center + month.cent*edu.center + month.cent*sexuality.center, 
                    data = sex2007)
summary(sex.fullmodel.e); etaSquared(sex.fullmodel.e); lm.beta(sex.fullmodel.e)

dwtest(sex.fullmodel.e) # significant autocorrelation; DW = 1.9761, p-value < 2.2e-16

# predictions
# number of months = -b/m
summary(sex.fullmodel.e)$coefficients["(Intercept)", "Estimate"] # b
summary(sex.fullmodel.e)$coefficients["month.cent", "Estimate"] # m
-summary(sex.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(sex.fullmodel.e)$coefficients["month.cent", "Estimate"]
-summary(sex.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(sex.fullmodel.e)$coefficients["month.cent", "Estimate"]/12

## conclusion = changing to neutral

## SkinTone -----------------------------------------------------------------------------------------------------------
dim(skin2007)[1]
skin.model1.e <- lm(att.center1 ~ month.cent, data = skin2007)
summary(skin.model1.e); etaSquared(skin.model1.e); lm.beta(skin.model1.e)

skin.fullmodel.e <- lm(att.center1 ~ month.cent +  ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                       month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                       month.cent*politicalid.center + month.cent*edu.center, data = skin2007)
summary(skin.fullmodel.e); etaSquared(skin.fullmodel.e); lm.beta(skin.fullmodel.e) 

dwtest(skin.fullmodel.e) # significant autocorrelation; DW = 1.9709, p-value < 2.2e-16

# predictions
# number of months = -b/m
summary(skin.fullmodel.e)$coefficients["(Intercept)", "Estimate"] # b
summary(skin.fullmodel.e)$coefficients["month.cent", "Estimate"] # m
-summary(skin.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(skin.fullmodel.e)$coefficients["month.cent", "Estimate"]
-summary(skin.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(skin.fullmodel.e)$coefficients["month.cent", "Estimate"]/12

## conclusion = remaining stable.

## Weight -----------------------------------------------------------------------------------------------------------
dim(weight2007)[1]
weight.model1.e <- lm(att.center1 ~ month.cent, data = weight2007)
summary(weight.model1.e); etaSquared(weight.model1.e); lm.beta(weight.model1.e)

weight.fullmodel.e <- lm(att.center1 ~ month.cent +  ethnicrace.center  + sex.center + age.center + politicalid.center + edu.center +
                         weight.center + + month.cent*ethnicrace.center + month.cent*sex.center + month.cent*age.center +
                         month.cent*politicalid.center + month.cent*edu.center + month.cent*weight.center, data = weight2007)
summary(weight.fullmodel.e); etaSquared(weight.fullmodel.e); lm.beta(weight.fullmodel.e) 

dwtest(weight.fullmodel.e) # significant autocorrelation; DW = 1.976, p-value = 6.346e-15

# predictions
# number of months = -b/m
summary(weight.fullmodel.e)$coefficients["(Intercept)", "Estimate"] # b
summary(weight.fullmodel.e)$coefficients["month.cent", "Estimate"] # m
-summary(weight.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(weight.fullmodel.e)$coefficients["month.cent", "Estimate"]
-summary(weight.fullmodel.e)$coefficients["(Intercept)", "Estimate"]/summary(weight.fullmodel.e)$coefficients["month.cent", "Estimate"]/12

## conclusion = changing to neutral


## average R2 ----------
summary(age.fullmodel) # r2 = 0.014
summary(age.fullmodel.e) # r2 = 0.065
summary(dis.fullmodel) # r2 = 0.062
summary(dis.fullmodel.e) # r2 = 0.029
summary(race.fullmodel) # r2 = 0.087
summary(race.fullmodel.e) # r2 = 0.11
summary(sex.fullmodel) # r2 = 0.24
summary(sex.fullmodel.e) # r2 = 0.33
summary(skin.fullmodel) # r2 = 0.078
summary(skin.fullmodel.e) # r2 = 0.046
summary(weight.fullmodel) # r2 = 0.031
summary(weight.fullmodel.e) # r2 = 0.092
summary(weight.fullmodelface) # r2 = 0.035

mean(c(0.014, 0.065, 0.062, 0.029, 0.087, 0.11, 0.24, 0.33, 0.078, 0.046, 0.031, 0.092, 0.035)) # 0.094
median(c(0.014, 0.065, 0.062, 0.029, 0.087, 0.11, 0.24, 0.33, 0.078, 0.046, 0.031, 0.092, 0.035)) # 0.065

