#### R CODE FOR CHILDREN SHOW A GENDER GAP IN NEGOTIATION

##################################################
##                   PACKAGES                   ##
##################################################

#### install packages ####
# packages I'll need
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# load packages
packages <- c("binom", "ggplot2", "scales", "lme4", "multcomp", "reshape2", "arm", "texreg", "plyr", "effects", "grid", "optimx","ggfortify","MASS","effects")
ipak(packages)

##################################################

#### MAIN ANALYSES: TRIAL 1 ####
mod1.cont <- glm.nb(trial.1 ~ 
                      age.calculated + 
                      Participant + 
                      Evaluator +
                      Participant:Evaluator +
                      age.calculated:Participant+
                      age.calculated:Evaluator +
                      age.calculated:Evaluator:Participant, 
                    data = negotiation)
summary(mod1.cont)
drop1(mod1.cont, test = "Chisq")
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(mod1.cont), confint(mod1.cont)))

### significant three-way interaction, will explore by subsetting by gender

#### trial 1 by participant gender ####
## just girls
negotiation.girls <- subset(negotiation, Participant == "F")
mod1.girls.cont <- glm.nb(trial.1 ~ 
                            age.calculated + 
                            Evaluator +
                            age.calculated:Evaluator,
                          data = negotiation.girls)
summary(mod1.girls.cont)
drop1(mod1.girls.cont, test = "Chisq")
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(mod1.girls.cont), confint(mod1.girls.cont)))

### significant interaction between age x evaluator

## just boys
negotiation.boys <- subset(negotiation, Participant == "M")
mod1.boys.cont <- glm.nb(trial.1 ~ 
                           age.calculated + 
                           Evaluator +
                           age.calculated:Evaluator,
                         data = negotiation.boys)
summary(mod1.boys.cont)
drop1(mod1.boys.cont, test = "Chisq")
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(mod1.boys.cont), confint(mod1.boys.cont)))

### interaction between age x evaluator is not significant

#### trial 1 simple slopes ####
## Girls with Male evaluator
girlsMale1 <- glm.nb(trial.1 ~ age.calculated, data=subset(negotiation, Evaluator == "M" & Participant=="F"))
summary(girlsMale1)
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(girlsMale1), confint(girlsMale1)))

## Girls with Female evaluator
girlsFemale1 <- glm.nb(trial.1 ~ age.calculated, data=subset(negotiation, Evaluator == "F" & Participant=="F"))
summary(girlsFemale1)
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(girlsFemale1), confint(girlsFemale1)))

## Boys with Male Evaluator
boysMale1 <- glm.nb(trial.1 ~ age.calculated, data=subset(negotiation, Evaluator == "M" & Participant=="M"))
summary(boysMale1)
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(boysMale1), confint(boysMale1)))

## Boys with Female Evaluator
boysFemale1 <- glm.nb(trial.1 ~ age.calculated, data=subset(negotiation, Evaluator == "F" & Participant=="M"))
summary(boysFemale1)
#effect size: odds ratio & 95% CI
exp(cbind(OR = coef(boysFemale1), confint(boysFemale1)))

#### MAIN FIGURE: TRIAL 1 ####

effects5 <- allEffects(mod1.cont)
effects5_df <- lapply(effects5, as.data.frame)
predicted.values <- effects5_df$`age.calculated:Participant:Evaluator`

predicted.values$Participant <- factor(predicted.values$Participant, levels = c("F", "M"), labels = c("Girls", "Boys"))
predicted.values$Evaluator <- factor(predicted.values$Evaluator, levels=c("F","M"), labels = c("Female", "Male"))

#### predicted effects graph ####
negotiation.predicted.effects <- ggplot(predicted.values, aes(x=age.calculated, y=fit, color = Evaluator)) + 
  geom_line(size = 1.5) +
  geom_ribbon(data = predicted.values, 
              mapping = aes(y = fit, ymin = lower, ymax = upper, fill = Evaluator), 
              alpha = .4, linetype = 0) +
  scale_size_area() + 
  scale_color_discrete(name="Experimenter Gender",
                       breaks=c("Female", "Male"),
                       labels=c("Female", "Male")) +
  facet_wrap(~Participant) +
  coord_cartesian(ylim=c(0,10)) +
  ylab("Predicted Effects (Number of Stickers)")+
  xlab ("Age in Years") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size=18),
        axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13),
        legend.text=element_text(size=13),
        strip.text = element_text(size = 18)) +
  guides(fill=FALSE) +
  theme(text = element_text(size=18))+
  ggtitle("Trial 1 Bonus Requested")+
  scale_color_manual(breaks = c("Female", "Male"), 
                     values=c("deepskyblue3", "green4"))+
  scale_fill_manual(breaks = c("Female", "Male"), 
                    values=c("deepskyblue3", "green4"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))

negotiation.predicted.effects

#### raw data graph ####
negotiation.figure <- negotiation
negotiation.figure$Participant <- revalue(negotiation.figure$Participant, c("F"="Girls", "M"="Boys"))
negotiation.figure$Evaluator <- revalue(negotiation.figure$Evaluator, c("F"="Female", "M"="Male"))

fig.dot <- ggplot(negotiation.figure, aes(y = trial.1, x = age.calculated, group = Evaluator, color = Evaluator, fill= Evaluator)) + 
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method='lm',formula=y~x, show.legend = TRUE) +
  ylab ("Number of stickers requested") +
  xlab ("Age in years") +
  labs(fill = "Evaluator")+
  facet_wrap(~Participant) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=20),
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
        legend.text=element_text(size=15),
        strip.text = element_text(size = 20)) +
  guides(fill=FALSE) +
  scale_color_manual(values=c("deepskyblue3", "green4"))+
  scale_fill_manual(values=c("deepskyblue3", "green4"))

fig.dot

#### SECONDARY ANALYSES: TRIALS COMPLETED (PERSISTENCE) ####

negotiationOrdinal <- negotiation
negotiationOrdinal$persistence = as.factor(negotiation$persistence)
str(negotiationOrdinal)
m <- polr(persistence ~ 
            age.calculated + 
            Participant + 
            Evaluator +
            Participant:Evaluator +
            age.calculated:Participant+
            age.calculated:Evaluator +
            age.calculated:Evaluator:Participant,
          data=negotiationOrdinal, Hess=TRUE)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m))
# effect size: OR and 95% CI
exp(cbind(OR = coef(m), confint(m)))

drop1(m, test = "Chisq")

### persistence by participant gender
## just girls
negotiationOrdinal.girls <- subset(negotiationOrdinal, Participant == "F")
str(negotiationOrdinal.girls)

girls.pers <- polr(persistence ~ 
                     age.calculated+
                     Evaluator +
                     age.calculated:Evaluator,
                   data=negotiationOrdinal.girls, Hess=TRUE)
(ctable.girls <- coef(summary(girls.pers)))
p.girls <- pnorm(abs(ctable.girls[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable.girls, "p value" = p.girls))

# effect size: OR and 95% CI
exp(cbind(OR = coef(girls.pers), confint(girls.pers)))

drop1(girls.pers, test = "Chisq")

## just boys
negotiationOrdinal.boys <- subset(negotiationOrdinal, Participant == "M")
str(negotiationOrdinal.boys)

boys.pers <- polr(persistence ~ 
                    age.calculated +
                    Evaluator +
                    age.calculated:Evaluator,
                  data=negotiationOrdinal.boys, Hess=TRUE)
(ctable.boys <- coef(summary(boys.pers)))
p.boys <- pnorm(abs(ctable.boys[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable.boys, "p value" = p.boys))

# effect size: OR and 95% CI
exp(cbind(OR = coef(boys.pers), confint(boys.pers)))

drop1(boys.pers, test = "Chisq")

### persistence simple slopes ####
negotiationOrdinal.girls.Fem <- subset(negotiationOrdinal.girls, Evaluator == "F")
negotiationOrdinal.girls.Male <- subset(negotiationOrdinal.girls, Evaluator == "M")

##girls with male evaluator
girls.Male <- polr(persistence ~ 
                     age.calculated,
                   data=negotiationOrdinal.girls.Male, Hess=TRUE)
(ctable.girls.Male <- coef(summary(girls.Male)))
p.girls.Male <- pnorm(abs(ctable.girls.Male[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable.girls.Male, "p value" = p.girls.Male))
# effect size: OR and 95% CI
exp(cbind(OR = coef(girls.Male), confint(girls.Male)))

##girls with female evaluator
girls.Fem <- polr(persistence ~ 
                    age.calculated,
                  data=negotiationOrdinal.girls.Fem, Hess=TRUE)
(ctable.girls.Fem <- coef(summary(girls.Fem)))
p.girls.Fem <- pnorm(abs(ctable.girls.Fem[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable.girls.Fem, "p value" = p.girls.Fem))
# effect size: OR and 95% CI
exp(cbind(OR = coef(girls.Fem), confint(girls.Fem)))


##boys with male evaluator
boys.Male <- polr(persistence ~ age.calculated,
                       data=subset(negotiationOrdinal.boys, Evaluator == "M"), Hess=TRUE)
(ctable.boys.Male <- coef(summary(boys.Male)))
p.boys.Male <- pnorm(abs(ctable.boys.Male[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable.boys.Male, "p value" = p.boys.Male))
# effect size: OR and 95% CI
exp(cbind(OR = coef(boys.Male), confint(boys.Male)))

##boys with female evaluator
boys.Female <- polr(persistence ~ age.calculated,
                  data=subset(negotiationOrdinal.boys, Evaluator == "F"), Hess=TRUE)
(ctable.boys.Female <- coef(summary(boys.Female)))
p.boys.Female <- pnorm(abs(ctable.boys.Female[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable.boys.Female, "p value" = p.boys.Female))
# effect size: OR and 95% CI
exp(cbind(OR = coef(boys.Female), confint(boys.Female)))

################################################################################
###########                          SOM                             ###########         
################################################################################

#### tables for SOM -> main models in text
# Initial Request Regression Table (Bootstrapped 95% CI)
htmlreg(
  list(mod1.cont, mod1.girls.cont, mod1.boys.cont), 
  file = "Initial_Request_postR&R.doc",  
  stars = c(0.001, 0.01, 0.05),
  center = TRUE,                    
  digits = 2,
  method = "confint",
  nsim = 1000,
  single.row = TRUE,                
  leading.zero = TRUE,
  custom.model.names = c("All Participants",
                         "Girls",
                         "Boys"),
  custom.coef.names = c("Intercept", 
                        "Continuous Age", 
                        "Participant (Male)", 
                        "Evaluator (Male)",
                        "Participant (Male) x Evaluator (Male)",
                        "Continuous Age x Participant (Male)",
                        "Continuous Age x Evaluator (Male)",
                        "Continuous Age x Participant (Male) x Evaluator (Male)"),
  inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)

# Persistence Regression Table  (Bootstrapped 95% CI)
htmlreg(
  list(m, girls.pers,boys.pers), 
  file = "Persistence_postR&R.doc", 
  stars = c(0.001, 0.01, 0.05),
  center = TRUE,                     
  digits = 2,
  method = "boot",
  level = 0.95, nsim = 1000,
  single.row = TRUE,                
  leading.zero = TRUE,
  custom.model.names = c("All Participants",
                         "Girls",
                         "Boys"),
  custom.coef.names = c("Continuous Age", 
                        "Participant (Male)", 
                        "Evaluator (Male)",
                        "Participant (Male) x Evaluator (Male)",
                        "Continuous Age x Participant (Male)",
                        "Continuous Age x Evaluator (Male)",
                        "Continuous Age x Participant (Male) x Evaluator (Male)"),
  inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)

## Additional Controls for Initial Request Regression Models
# main model
mod1.cont.Act.Control <- glm.nb(trial.1 ~ 
                                  age.calculated + 
                                  Participant + 
                                  Evaluator +
                                  Participant:Evaluator +
                                  age.calculated:Participant+
                                  age.calculated:Evaluator +
                                  age.calculated:Evaluator:Participant+
                                  prior.activity, 
                                data = negotiation)
summary(mod1.cont.Act.Control)
drop1(mod1.cont.Act.Control, test = "Chisq")
anova(mod1.cont.Act.Control,mod1.cont)

#girls
mod1.girls.cont.Act.Control <- glm.nb(trial.1 ~ 
                                        age.calculated + 
                                        Evaluator +
                                        age.calculated:Evaluator+
                                        prior.activity,
                                      data = negotiation.girls)
summary(mod1.girls.cont.Act.Control)
drop1(mod1.girls.cont.Act.Control, test = "Chisq")
anova(mod1.girls.cont.Act.Control,mod1.girls.cont)

#boys
mod1.boys.cont.Act.Control <- glm.nb(trial.1 ~ 
                                       age.calculated + 
                                       Evaluator +
                                       age.calculated:Evaluator+
                                       prior.activity,
                                     data = negotiation.boys)
summary(mod1.boys.cont.Act.Control)
drop1(mod1.boys.cont.Act.Control, test = "Chisq")
anova(mod1.boys.cont.Act.Control,mod1.boys.cont)

# regression table
htmlreg(
  list(mod1.cont.Act.Control, mod1.girls.cont.Act.Control, mod1.boys.cont.Act.Control), 
  file = "Initial_Request_ActControl.doc",  
  stars = c(0.001, 0.01, 0.05),
  center = TRUE,                    
  digits = 2,                       
  single.row = TRUE,                
  leading.zero = TRUE,
  custom.model.names = c("All Participants",
                         "Girls",
                         "Boys"),
  custom.coef.names = c("Intercept", 
                        "Continuous Age", 
                        "Participant (Male)", 
                        "Evaluator (Male)",
                        "Prior Activity (IPAD)",
                        "Prior Activity (NUM)",
                        "Prior Activity (PCPD)",
                        "Prior Activity (PUG)",
                        "Prior Activity (RoPNE)",
                        "Participant (Male) x Evaluator (Male)",
                        "Continuous Age x Participant (Male)",
                        "Continuous Age x Evaluator (Male)",
                        "Continuous Age x Participant (Male) x Evaluator (Male)"),
  inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)

## analyses by evaluator gender instead of participant gender
# just female evaluator
negotiation.fem <- subset(negotiation, Evaluator == "F")
mod1.fem.cont <- glm.nb(trial.1 ~ 
                          age.calculated + 
                          Participant +
                          age.calculated:Participant,
                        data = negotiation.fem)
summary(mod1.fem.cont)
drop1(mod1.fem.cont, test = "Chisq")

# just male evaluator
negotiation.male <- subset(negotiation, Evaluator == "M")
mod1.male.cont <- glm.nb(trial.1 ~ 
                           age.calculated + 
                           Participant +
                           age.calculated:Participant,
                         data = negotiation.male)
summary(mod1.male.cont)
drop1(mod1.male.cont, test = "Chisq")

# regression table
htmlreg(
  list(mod1.cont, mod1.fem.cont, mod1.male.cont), 
  file = "Initial_Request_ByEval.doc",  
  stars = c(0.001, 0.01, 0.05),
  center = TRUE,                    
  digits = 2,                       
  single.row = TRUE,                
  leading.zero = TRUE,
  custom.model.names = c("Both Evaluators",
                         "Female",
                         "Male"),
  custom.coef.names = c("Intercept", 
                        "Continuous Age", 
                        "Participant (Male)", 
                        "Evaluator (Male)",
                        "Participant (Male) x Evaluator (Male)",
                        "Continuous Age x Participant (Male)",
                        "Continuous Age x Evaluator (Male)",
                        "Continuous Age x Participant (Male) x Evaluator (Male)"),
  inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)

# figure
negotiation.predicted.effects.byEvaluator <- ggplot(predicted.values, aes(x=age.calculated, y=fit, color = Participant)) + 
  geom_line(size = 1.5) +
  geom_ribbon(data = predicted.values, 
              mapping = aes(y = fit, ymin = lower, ymax = upper, fill = Participant), 
              alpha = .4, linetype = 0) +
  scale_size_area() + 
  scale_color_discrete(name="Participant Gender",
                       breaks=c("Girls", "Boys"),
                       labels=c("Girls", "Boys")) +
  facet_wrap(~Evaluator) +
  coord_cartesian(ylim=c(0,10)) +
  ylab("Number of Stickers")+
  xlab ("Age") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size=20),
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
        legend.text=element_text(size=15),
        strip.text = element_text(size = 20)) +
  guides(fill=FALSE) +
  theme(text = element_text(size=18))+
  ggtitle("Trial 1 Bonus Requested")+
  scale_color_manual(breaks = c("Girls", "Boys"), 
                     values=c("yellow", "#56B4E9"))+
  scale_fill_manual(breaks = c("Girls", "Boys"), 
                    values=c("yellow", "#56B4E9"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10))

negotiation.predicted.effects.byEvaluator

#### POST HOC CONTRAST ####
library(emmeans)
negotiation.glm.nb <- glm.nb(trial.1 ~ Condition*age.calculated, data=negotiation)
( negotiation.emt <- emtrends(negotiation.glm.nb, "Condition", var = "age.calculated") )
pairs(negotiation.emt)

#### P/F COMP CHECKS ####

summary(negotiationOrdinal$persistence)
summary(negotiationOrdinal$comp.checks)

## drop participants who did not make it to persistence 1
negotiationOrdinal.pass <- subset(negotiationOrdinal, comp.checks != ".")
summary(negotiationOrdinal.pass$comp.checks)
library(gdata)
negotiationOrdinal.pass$comp.checks <- drop.levels(negotiationOrdinal.pass$comp.checks, drop = TRUE)
summary(negotiationOrdinal.pass$pers1.vs.2)
negotiationOrdinal.pass$pers1.vs.2 <- drop.levels(negotiationOrdinal.pass$pers1.vs.2, drop = TRUE)
summary(negotiationOrdinal.pass$pers1.vs.2)

## model of only participants who asked for more than 2 stickers on first ask
## three-way interaction with comp.checks (pass = 1, fail = 0) as a control variable
persistence.2vs3 <- glm(pers1.vs.2 ~ 
            age.calculated + 
            Participant + 
            Evaluator +
            Participant:Evaluator +
            age.calculated:Participant+
            age.calculated:Evaluator +
            age.calculated:Evaluator:Participant+
           comp.checks,
          data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3)
drop1(persistence.2vs3, test = "Chisq")
# three-way interaction is not significant -> find a best fit model

persistence.2vs3.gen1 <- glm(pers1.vs.2 ~ 
                              (age.calculated+Participant+Evaluator)^2,
                            data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen1)
drop1(persistence.2vs3.gen1, test = "Chisq")
# age * participant interaction has the least predictive power, drop

persistence.2vs3.gen2 <- glm(pers1.vs.2 ~ age.calculated +Participant+Evaluator+
                               age.calculated:Evaluator+Participant:Evaluator,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen2)
drop1(persistence.2vs3.gen2, test = "Chisq")
# age * evaluator has the least predictive power, drop

persistence.2vs3.gen3 <- glm(pers1.vs.2 ~ age.calculated +Participant+Evaluator+
                               Participant:Evaluator,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen3)
drop1(persistence.2vs3.gen3, test = "Chisq")
# age has the least predictive power and does not improve model fit

## best fit model
persistence.2vs3.gen4 <- glm(pers1.vs.2 ~ Participant*Evaluator,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen4)
drop1(persistence.2vs3.gen4, test = "Chisq")
# effect size: OR and 95% CI
exp(cbind(OR = coef(persistence.2vs3.gen4), confint(persistence.2vs3.gen4)))

##now add comp checks:
persistence.2vs3.gen5 <- glm(pers1.vs.2 ~ Participant*Evaluator*comp.checks,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen5)
drop1(persistence.2vs3.gen5, test = "Chisq")
# effect size: OR and 95% CI
exp(cbind(OR = coef(persistence.2vs3.gen5), confint(persistence.2vs3.gen5)))
# three-way interaction is not significant -> find model of best fit

persistence.2vs3.gen6 <- glm(pers1.vs.2 ~ (Participant+Evaluator+comp.checks)^2,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen6)
drop1(persistence.2vs3.gen6, test = "Chisq")
# evaluator * comp checks has the least predictive power, drop 

persistence.2vs3.gen7 <- glm(pers1.vs.2 ~ Participant+Evaluator+comp.checks+
                               Participant:Evaluator+Participant:comp.checks,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen7)
drop1(persistence.2vs3.gen7, test = "Chisq")
# participant * comp checks has the least predictive power, drop

persistence.2vs3.gen8 <- glm(pers1.vs.2 ~ Participant*Evaluator+comp.checks,
                             data=negotiationOrdinal.pass, family = "binomial")
summary(persistence.2vs3.gen8)
drop1(persistence.2vs3.gen8, test = "Chisq")
# effect size: OR and 95% CI
exp(cbind(OR = coef(persistence.2vs3.gen8), confint(persistence.2vs3.gen8)))
# including comp checks does not improve model fit

htmlreg(
  list(persistence.2vs3.gen4, persistence.2vs3.gen5, persistence.2vs3.gen8), 
  file = "Additional_Controls_Persistence.doc",  
  stars = c(0.001, 0.01, 0.05),
  center = TRUE,                    
  digits = 2,
  single.row = TRUE,                
  leading.zero = TRUE,
  #custom.model.names = c("All Participants","Girls","Boys"),
  #custom.coef.names = c("Intercept", 
                        #"Continuous Age", 
                        #"Participant (Male)", 
                        #"Evaluator (Male)",
                        #"Participant (Male) x Evaluator (Male)",
                        #"Continuous Age x Participant (Male)",
                        #"Continuous Age x Evaluator (Male)",
                        #"Continuous Age x Participant (Male) x Evaluator (Male)"),
  inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)