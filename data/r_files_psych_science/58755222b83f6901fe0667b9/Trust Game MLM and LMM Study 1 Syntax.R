library (nlme)

Participant <- Trust.Game.Data.Study.1.10.25.2016$Participant
Condition <- Trust.Game.Data.Study.1.10.25.2016$Condition
group <- Trust.Game.Data.Study.1.10.25.2016$group
trust <- Trust.Game.Data.Study.1.10.25.2016$trust
DC_ritual <- Trust.Game.Data.Study.1.10.25.2016$DC_ritual
DC_control <- Trust.Game.Data.Study.1.10.25.2016$DC_control
DC_IG <- Trust.Game.Data.Study.1.10.25.2016$DC_IG
DC_OG <- Trust.Game.Data.Study.1.10.25.2016$DC_OG

#####the complete omnibus model with two dichotomous predictor variables (Condition and group)######

mixed.model <- lme(trust~Condition*group, random=~1+group|Participant)
summary(mixed.model)
intervals(mixed.model)


#####getting precise p-values####

tstat <- c(5.05,4.86)
print(2* (1-pt(abs(tstat), 99)), digits=15)

##### probing for simple slopes: Condition dummy coded (Ritual versus Control); as well as group membership (IG versus OG)

mixed.ritual <- lme (trust~group*DC_ritual, random=~1 + group|Participant, data=Trust.Game.Data.Study.1.10.25.2016)
summary(mixed.ritual) ## ritual coded 0 (level of interest)

mixed.control <- lme (trust~group*DC_control, random=~1 + group|Participant, data=Trust.Game.Data.Study.1.10.25.2016)
summary(mixed.control) ## control coded 0 (level of interest)

mixed.IG <- lme (trust~Condition*DC_IG, random=~1 + group|Participant, data=Trust.Game.Data.Study.1.10.25.2016)
mixed.OG <- lme (trust~Condition*DC_OG, random=~1 + group|Participant, data=Trust.Game.Data.Study.1.10.25.2016)
summary(mixed.IG)
summary(mixed.OG)