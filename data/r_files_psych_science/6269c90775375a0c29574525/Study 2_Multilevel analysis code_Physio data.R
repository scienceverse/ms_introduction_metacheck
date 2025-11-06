#####################################################
##    Multilevel Analysis of Physiological Data    ##
#####################################################

#Load packages
library(foreign)
library(nlme)

#Load data
data <- read.spss("Study 2_Physio Data_long format.sav", to.data.frame = T)
#Apply filter to exclude participants
include <- (which(data$filter_ExcludedParticipants == "Selected"))
data <- data[include,]
#Check number of participants
length(unique(data$ppID)) # N = 353, which is correct
#Check levels of emotion condition (categorical variable)
levels(data$EmotionCondition) # 'Awe' condition is the reference category

############################
##           RSA          ##
############################

# Fit three RSA models
RSA_fixedintercept <- gls(RSA ~ 1 + Time*EmotionCondition + baseline_mean_RSA + practice_mean_RSA, data = data, method = "ML", na.action = na.exclude)

RSA_randomintercept <- lme (RSA ~ 1 + Time*EmotionCondition 
                    + baseline_mean_RSA + practice_mean_RSA,
                    random = ~ 1 | ppID,
                    data = data, method = "ML", na.action = na.exclude)

RSA_randomAR1 <- lme (RSA ~ 1 + Time*EmotionCondition 
                           + baseline_mean_RSA + practice_mean_RSA,
                           random = ~ 1 | ppID, 
                           correlation = corAR1 (form = ~ Time | ppID),
                           data = data, method = "ML", na.action = na.exclude)

# Compare goodness of fit
anova(RSA_fixedintercept, RSA_randomintercept, RSA_randomAR1)

# Summary of final model
summary(RSA_randomAR1)

# Test polynomial contrasts for Time * Emotion Condition interaction
RSA_polyModel <- lme (RSA ~ 1 + EmotionCondition*poly(Time, 2) 
                      + baseline_mean_RSA + practice_mean_RSA,
                      random = ~ 1 | ppID, 
                      correlation = corAR1 (form = ~ Time | ppID),
                      data = data, method = "ML")

summary(RSA_polyModel)

##############################
##          SCL_SQ          ##
##############################

# Fit three SCL_SQ models
SCL_fixedintercept <- gls(SCL_SQ ~ 1 + Time*EmotionCondition + SQ_baseline_mean_SCL + SQ_practice_mean_SCL, data = data, method = "ML", na.action = na.exclude)

SCL_randomintercept <- lme (SCL_SQ ~ 1 + Time*EmotionCondition 
                            + SQ_baseline_mean_SCL + SQ_practice_mean_SCL,
                            random = ~ 1 | ppID,
                            data = data, method = "ML", na.action = na.exclude)

SCL_randomAR1 <- lme (SCL_SQ ~ 1 + Time*EmotionCondition 
                             + SQ_baseline_mean_SCL + SQ_practice_mean_SCL,
                             random = ~ 1 | ppID, 
                             correlation = corAR1 (form = ~ Time | ppID),
                             data = data, method = "ML")

# Compare goodness of fit
anova(SCL_fixedintercept, SCL_randomintercept, SCL_randomAR1)

# Summary of final model
summary(SCL_randomAR1)

# Test polynomial contrasts for Time * Emotion Condition interaction
SCL_polyModel <- lme (SCL_SQ ~ 1 + EmotionCondition*poly(Time, 2) 
                        + SQ_baseline_mean_SCL + SQ_practice_mean_SCL,
                        random = ~ 1 | ppID, 
                        correlation = corAR1 (form = ~ Time | ppID),
                        data = data, method = "ML")

summary(SCL_polyModel)

##############################
##            HR            ##
##############################

# Fit three HR models
HR_fixedintercept <- gls(HR ~ 1 + Time*EmotionCondition + baseline_mean_HR + practice_mean_HR, data = data, method = "ML", na.action = na.exclude)

HR_randomintercept <- lme (HR ~ 1 + Time*EmotionCondition 
                            + baseline_mean_HR + practice_mean_HR,
                            random = ~ 1 | ppID,
                            data = data, method = "ML", na.action = na.exclude)

HR_randomAR1 <- lme (HR ~ 1 + Time*EmotionCondition 
                        + baseline_mean_HR + practice_mean_HR,
                        random = ~ 1 | ppID, 
                        correlation = corAR1 (form = ~ Time | ppID),
                        data = data, method = "ML", na.action = na.exclude)

# Compare goodness of fit
anova(HR_fixedintercept, HR_randomintercept, HR_randomAR1)

# Summary of final model
summary(HR_randomAR1)
