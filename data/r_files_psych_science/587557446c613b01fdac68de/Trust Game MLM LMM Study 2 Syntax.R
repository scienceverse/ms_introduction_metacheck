library (nlme)


##run your omnibus model with all of your predictors thrown in 

omnibus.model <- lme(trust ~ cond_el*group + cond__si*group, random = ~group|participant, method = "ML", data=Trust.Game.Data.Study.2.10.25.2016)
summary(omnibus.model)
intervals(omnibus.model)


## Here, we do a model comparison comparing our omnibus model with the model with unested models ##
## testing for a main effect of condition: you have to do a comparison of models; you create a model without the two 
##effects coded variables of, in this case, condition; you then run an ANOVA chi square test to see if the difference in 
##he residual sum of squares in the models is signiciantly different ##

no.condition.main.effects.model <- update(omnibus.model, . ~ . - cond_el - cond_si, method="ML")
anova(omnibus.model,no.condition.main.effects.model)

## running model comparison comparing omnibus model to the model without the interaction ##

no.interaction.effect.model <- update(omnibus.model, .~. - cond_el:group - cond_si:group)
anova(omnibus.model,no.interaction.effect.model)

##probing for simple effects: 3-level categorical predictor variable (6 dummy code variables) & a 2-level categorical predicto variable (2 code variables)##: 
## first test of simple effects looks at the simple slopes of each level of the predictor variable, "condition"; in other words, it looks at whether "group"## 
##membership predicts amount of money entrusted for each condition; below, I ran three separate regression equations: each testing whether the condition was significantly different from zero##

##Elaborate ritual condition##
  
omnibus.er <- lme(trust ~ DC_er1*group + DC_er2*group, random = ~group|participant, data=Trust.Game.Data.Study.2.10.25.2016)
summary(omnibus.er)

##Simple ritual condition##

omnibus.sr <- lme(trust ~ DC_sr1*group + DC_sr2*group, random = ~group|participant, data=Trust.Game.Data.Study.2.10.25.2016)
summary(omnibus.sr)

##Control condition##

omnibus.cc <- lme(trust ~ DC_cc1*group + DC_cc2*group, random = ~group|participant, data=Trust.Game.Data.Study.2.10.25.2016)
summary(omnibus.cc)


