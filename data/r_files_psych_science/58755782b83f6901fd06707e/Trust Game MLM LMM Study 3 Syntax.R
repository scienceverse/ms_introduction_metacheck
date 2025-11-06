library (nlme)

#####the complete omnibus model with two dichotomous predictor variables (Condition and group)######

mixed.model <- lme(trust~condition*group, random=~1+group|Participant, data = Trust.Game.Data.Study.3.10.25.2016)
summary(mixed.model)
intervals(mixed.model)

##the simple slopes are not followed up wth because the omnibus interaction is not significant##