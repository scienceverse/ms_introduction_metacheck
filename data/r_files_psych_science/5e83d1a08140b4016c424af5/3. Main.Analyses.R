require(haven)
require(PsychMisc)
require(lmerTest)
require(optimx)
require(glmmTMB)
require(DHARMa)
require(bbmle)
require(broom.mixed)
require(redres)
require(DescTools)
require(psych)
require(lm.beta)
require(olsrr)
require(broom)
require(powerMediation)
require(WebPower)
require(RSPS)
require(tidyverse)

# Functions ---------------------------------------------------------------


# code to invert log links in term estimates:

exponentiate.tidy <- function(ret){
  vv <- intersect(c("estimate", "conf.low", "conf.high"), names(ret))
  ret <- (ret %>% mutate_at(vars(vv), ~exp(.)) %>% 
            mutate(std.error = std.error * estimate))
  return(ret)}


# printing things for the paper:
printout.nb <- function(mod){
  tidy(mod, conf.int = TRUE) %>% 
    filter(term == "narc_z") %>% 
    mutate(
      output = 
        paste0(
          "B = ", Format(estimate, digits = 2, sci = 5), ", ",
          "95% CI = [", Format(conf.low, digits = 2, sci = 5), ", ", 
          Format(conf.high, digits = 2, sci = 5), "], ",
          "IRR = ", Format(exp(estimate), digits = 2, sci = 5), ", ",
          ifelse(p.value >= .001, 
                 paste0("p = ", Format(p.value, digits = 3, sci = 5, leading = "drop")),
                 "p < .001")
        )
    ) %>% 
    pull(output)
}

printout <- function(mod){
  tidy(mod, conf.int = TRUE) %>% 
    filter(term == "narc_z") %>% 
    mutate(
      output = 
        paste0(
          "B = ", Format(estimate, digits = 2, sci = 5), ", ",
          "95% CI = [", Format(conf.low, digits = 2, sci = 5), ", ", 
          Format(conf.high, digits = 2, sci = 5), "], ",
          "ß = ", Format(lm.beta(mod)$standardized.coefficients["narc_z"], 
                         digits = 2, sci = 5, leading = "drop"), ", ",
          ifelse(p.value >= .001, 
                 paste0("p = ", Format(p.value, digits = 3, sci = 5, leading = "drop")),
                 "p < .001")
        )
    ) %>% 
    pull(output)
}

# Logistic models:
printout.logistic <- function(mod, .term = "narc_z"){
  tidy(mod, conf.int = TRUE) %>% 
    filter(term == .term) %>% 
    mutate(
      output = 
        paste0(
          "B = ", Format(estimate, digits = 2, sci = 5), ", ",
          "95% CI = [", Format(conf.low, digits = 2, sci = 5), ", ", 
          Format(conf.high, digits = 2, sci = 5), "], ",
          "OR = ", Format(exp(estimate), digits = 2, sci = 5), ", ",
          ifelse(p.value >= .001, 
                 paste0("p = ", Format(p.value, digits = 3, sci = 5, leading = "drop")),
                 "p < .001")
        )
    ) %>% 
    pull(output)
}

snip <-  function(input) {
  #Standard snip: Outputs as indvidual lines
  pb <- pipe("pbcopy", "w")
  write(print(input), file=pb)
  close(pb)
}


# data load ---------------------------------------------------------------

load("./processeddata/data.processed.rda")
load("./processeddata/data.processed.wide.rda")

# ANALYSES ----------------------------------------------------------------

# .Nomination models ------------------------------------------------------

# ..Fit models ------------------------------------------------------------


nomformula <- Lnominations ~ narc + (narc | class)
ll_nomformula <- Lnominations_log ~ narc + (narc | class)
od_nomformula <- Lnominations ~ narc + (1 | obs_effect) + (narc | class)
nomformula_z <- Lnominations ~ narc_z + (narc_z | class)
ll_nomformula_z <- Lnominations_log ~ narc_z + (narc_z | class)
od_nomformula_z <- Lnominations ~ narc_z + (1 | obs_effect) + (narc_z | class)

nomformula_ri <- Lnominations ~ narc + (1 | class)
ll_nomformula_ri <- Lnominations_log ~ narc + (1 | class)
od_nomformula_ri <- Lnominations ~ narc + (1 | obs_effect) + (1 | class)

nomformula_z_ri <- Lnominations ~ narc_z + (1 | class)
ll_nomformula_z_ri <- Lnominations_log ~ narc_z + (1 | class)
od_nomformula_z_ri <- Lnominations ~ narc_z + (1 | obs_effect) + (1 | class)

nomformula_z_uncor <- Lnominations ~ narc_z + (1 | class) + (narc-1 | class)
ll_nomformula_z_uncor <- Lnominations_log ~ narc_z + (1 | class) + (narc-1 | class)
od_nomformula_z_uncor <- Lnominations ~ narc_z + (1 | obs_effect) + (1 | class) + (narc-1 | class)


# Run various models for review
main.models <- 
  list(
    nb1.rs = glmmTMB(nomformula, dat, family = "nbinom1", offset = Lnominators_log),
    nb2.rs = glmmTMB(nomformula, dat, family = "nbinom2", offset = Lnominators_log),
    poiss.rs = glmmTMB(nomformula, dat, family = "poisson", offset = Lnominators_log),
    opoiss.rs = glmmTMB(od_nomformula, dat, family = "poisson", offset = Lnominators_log)
    # ll.rs = lmer(ll_nomformula, dat, offset = Lnominators_log)
  )

# Scaled narcissism:
scaled.models <- 
  list(
    nb1.rs.z = glmmTMB(nomformula_z, dat, family = "nbinom1", offset = Lnominators_log),
    nb2.rs.z = glmmTMB(nomformula_z, dat, family = "nbinom2", offset = Lnominators_log),
    poiss.rs.z = glmmTMB(nomformula_z, dat, family = "poisson", offset = Lnominators_log),
    opoiss.rs.z = glmmTMB(od_nomformula_z, dat, family = "poisson", offset = Lnominators_log)
    # ll.rs.z = lmer(ll_nomformula_z, dat, offset = Lnominators_log)
  )

# Random intercepts only:
ri.models <- 
  list(
    nb1.ri = glmmTMB(nomformula_z_ri, dat, family = "nbinom1", offset = Lnominators_log),
    nb2.ri = glmmTMB(nomformula_z_ri, dat, family = "nbinom2", offset = Lnominators_log),
    poiss.ri = glmmTMB(nomformula_z_ri, dat, family = "poisson", offset = Lnominators_log),
    opoiss.ri = glmmTMB(od_nomformula_z_ri, dat, family = "poisson", offset = Lnominators_log)
    # ll.ri = glmmTMB(ll_nomformula_ri, dat, family = "gaussian", offset = Lnominators_log)
  )

# Uncorrelated random effects:
uncor.models <- 
  list(
    nb1.rs.z.uncor = glmmTMB(nomformula_z_uncor, dat, family = "nbinom1", offset = Lnominators_log),
    nb2.rs.z.uncor = glmmTMB(nomformula_z_uncor, dat, family = "nbinom2", offset = Lnominators_log),
    poiss.rs.z.uncor = glmmTMB(nomformula_z_uncor, dat, family = "poisson", offset = Lnominators_log),
    opoiss.rs.z.uncor = glmmTMB(od_nomformula_z_uncor, dat, family = "poisson", offset = Lnominators_log)
    # ll.rs.z = lmer(ll_nomformula_z, dat, offset = Lnominators_log)
  )

linear.models <-
  list(
    ll.rs = lmer(ll_nomformula, dat, offset = Lnominators_log), 
    ll.scaled = lmer(ll_nomformula_z, dat, offset = Lnominators_log), 
    ll.ri = lmer(ll_nomformula_ri, dat, offset = Lnominators_log),
    ll.rs.z.uncor = lmer(ll_nomformula_z_uncor, dat, offset = Lnominators_log)
  )


# ..model comparisons -----------------------------------------------------

# ...likelihood ------------------------------------------------------


# Compare against other model forms:
# Strongly preferred over poisson: X2(1) = 152.03, p < .001
anova(scaled.models$nb1.rs, 
      scaled.models$poiss.rs)
# AIC preferred over overdispersed (same df), dAIC = 31.7
anova(main.models$nb1.rs, 
      main.models$nb2.rs)
# AIC preferred over overdispersed (same df), dAIC = 30.5
anova(main.models$nb1.rs, 
      main.models$opoiss.rs)

# Compare against random terms:
# Strongly preferred over uncorrelated random terms: X2(2) = 12.52, p = .002
anova(scaled.models$nb1.rs, 
      ri.models$nb1.ri)
# Strongly preferred over uncorrelated random terms: X2(1) = 12.52, p < .001
anova(scaled.models$nb1.rs.z, 
      uncor.models$nb1.rs.z.uncor)

summary(scaled.models$nb1.rs.z)
# ...residuals/AIC --------------------------------------------------------

model.df <-
  tibble(
    model.name = names(scaled.models),
    model = scaled.models,
    # Run tests of residuals with DHARMa:
    residualtest = map(model, simulateResiduals, n = 1000),
    dispersiontest = map(residualtest, testDispersion, alternative = "greater")
  )


# NB-1 is to be preferred for suitability to data (clean, normal residuals), 
# and general fit (lowest AIC):
model.df.all <-   
  model.df %>% 
  bind_cols(
    model.df$residualtest %>% 
      map_dfr(
        ~testUniformity(.x, plot = F) %>% 
          tidy %>% 
          select(ks_test = statistic, ks_p = p.value)
      )
  ) %>% 
  left_join(
    AICtab(model.df$model, mnames = model.df$model.name) %>% 
      as_tibble(rownames = "model.name")
  ) %>% 
  arrange(dAIC)

# Loglinear model not comparable, but trying:                               
linearmodel.df <-
  tibble(
    model.name = names(linear.models),
    model = linear.models,
    # Examine the residuals (will throw warning, appropriately):
    residualtest = map(model, simulateResiduals, n = 1000)
  )

linearmodel.df.all <-   
  linearmodel.df %>% 
  bind_cols(
    linearmodel.df$residualtest %>% 
      map_dfr(
        ~testUniformity(.x, plot = F) %>% 
          tidy %>% 
          select(ks_test = statistic, ks_p = p.value)
      )
  ) %>% 
  left_join(
    AICtab(linearmodel.df$model, mnames = linearmodel.df$model.name) %>% 
      as_tibble(rownames = "model.name")
  ) %>% 
  arrange(dAIC)



# ...L2 examination -------------------------------------------------------

# Narcissism levels are slightly higher in bigger classes:
dat %>% 
  ungroup() %>% 
  select(narc, Lnominators) %>% 
  cor(use="pairwise.complete")

# But estimation seems to handle this okay, best for the NB models
independence.test <-
  model.df$model %>% 
  set_names(model.df$model.name) %>% 
  map(~ranef(.x)) %>% 
  map("cond") %>% 
  map("class") %>% 
  map(~rename(.x, Intercept = `(Intercept)`)) %>%
  map(as_tibble, rownames = "class") %>% 
  map(~left_join(.x, 
                 dat %>% 
                   group_by(class) %>% 
                   summarize(size = log(unique(Lnominators)))
  )) %>% 
  map(~.x %>% select(-class) %>% cor %>% round(digits = 3))

# Linear model doesn't handle size variation as well (not terrible):
independence.test.linear <-
  linearmodel.df$model %>% 
  set_names(linearmodel.df$model.name) %>% 
  map(~ranef(.x)) %>% 
  map("class") %>% 
  map(~rename(.x, Intercept = `(Intercept)`)) %>%
  map(as_tibble, rownames = "class") %>% 
  map(~left_join(.x, 
                 dat %>% 
                   group_by(class) %>% 
                   summarize(size = log(unique(Lnominators)))
  )) %>% 
  map(~.x %>% select(-class) %>% cor %>% round(digits = 3))


# ...Cook's distance ------------------------------------------------------

source(system.file("other_methods","influence_mixed.R", package="glmmTMB"))

nb1_influence <- 
  influence_mixed(scaled.models$nb1.rs.z, groups="class")

inf <- 
  as.data.frame(nb1_influence[["fixed.effects[-class]"]])

inf <-
  transform(inf,class=rownames(inf),
            cooks=cooks.distance(nb1_influence))

car::infIndexPlot(nb1_influence)

nb1_influence_case <- 
  influence_mixed(scaled.models$nb1.rs.z)

inf_case <- 
  as.data.frame(nb1_influence_case[["fixed.effects[-case]"]])

inf_case <- 
  transform(inf_case,case=rownames(inf_case),
            cooks=cooks.distance(nb1_influence_case))

car::infIndexPlot(nb1_influence_case)



# .Group task --------------------------------------------------------------


# a. Covariates
# 
# 1.	None
# 2.	Age
# 3.	Relative age (to group members)
# 4.	Gender
# 5.	Gender composition group (percentage female)
# 6.	Mean narcissism of group + SD narcissism of group
# 7.	Experimenter
# 8.	Task version
# 9.	Time in which task was completed
# 10.	Heterogeneity of class composition (1 = same class group; 2 = mixed class group)
# 

# 
# c. Type of analysis
# 14.	OLS regression
# 15.	linear mixed model with a random intercept to account for group clustering

# ..Analyses ----------------------------------------------------------------

# ..ratings, self ------------------------------------------------------------------

# Authority, self
(auth_self_ri <- lmer(auth_self ~ narc_z + (1| class), datw_short)) %>% summary
(auth_self <- lm(auth_self ~ narc_z, datw_short)) %>% summary
(auth_self_rlog <- lm(auth_self_rlog ~ narc_z, datw_short)) %>% summary
(auth_self_rsqrt <- lm(auth_self_rsqrt ~ narc_z, datw_short)) %>% summary

anova(auth_self_ri, auth_self)

# untransformed is fine:
ols_test_normality(auth_self)
plot(auth_self)
ols_test_normality(auth_self_rlog)
ols_test_normality(auth_self_rsqrt)

lm.beta(auth_self)

# Effectiveness, self
(effect_self_ri <- lmer(effect_self ~ narc_z + (1| class), datw_short)) %>% summary
(effect_self <- lm(effect_self ~ narc_z, datw_short)) %>% summary
(effect_self_rlog <- lm(effect_self_rlog ~ narc_z, datw_short)) %>% summary
(effect_self_rsqrt <- lm(effect_self_rsqrt ~ narc_z, datw_short)) %>% summary

anova(effect_self_ri, effect_self)

# some issues with all of these, bootstrapping in s-curve may be useful:
ols_test_normality(effect_self)
plot(effect_self)
ols_test_normality(effect_self_rlog)
ols_test_normality(effect_self_rsqrt)

lm.beta(effect_self)



# ..ratings, others ------------------------------------------------------------------

# Authority, self
(auth_others_ri <- lmer(auth_others ~ narc_z + (1| class), datw_short)) %>% summary
(auth_others <- lm(auth_others ~ narc_z, datw_short)) %>% summary
(auth_others_rlog <- lm(auth_others_rlog ~ narc_z, datw_short)) %>% summary
(auth_others_rsqrt <- lm(auth_others_rsqrt ~ narc_z, datw_short)) %>% summary

anova(auth_others_ri, auth_others)

# untransformed is okay:
ols_test_normality(auth_others)
plot(auth_others)
ols_test_normality(auth_others_rlog)
ols_test_normality(auth_others_rsqrt)

lm.beta(auth_others)

# Effectiveness, self
(effect_others_ri <- lmer(effect_others ~ narc_z + (1| class), datw_short)) %>% summary
(effect_others <- lm(effect_others ~ narc_z, datw_short)) %>% summary
(effect_others_rlog <- lm(effect_others_rlog ~ narc_z, datw_short)) %>% summary
(effect_others_rsqrt <- lm(effect_others_rsqrt ~ narc_z, datw_short)) %>% summary

anova(effect_others_ri, effect_others)

# untransformed is fine:
ols_test_normality(effect_others)
plot(effect_others)
ols_test_normality(effect_others_rlog)
ols_test_normality(effect_others_rsqrt)

lm.beta(effect_others)



# ..Rankings ---------------------------------------------------------------

(rankings_self <- lm(leader_self ~ narc_z, datw_short)) %>% summary
(rankings_others <- lm(leader_others ~ narc_z, datw_short)) %>% summary

tidy(rankings_self, conf.int = TRUE)
tidy(rankings_others, conf.int = TRUE)

lm.beta(rankings_self)
lm.beta(rankings_others)


# ..coded functioning -------------------------------------------------------
# "coded_LB" "coded_LB_sqrt" "coded_LB_log"
# "coded_LF"

(LB_ri <- lmer(coded_LB ~ narc + (1| class), datw_short)) %>% summary
(LB <- lm(coded_LB ~ narc_z, datw_short)) %>% summary
(LB_log <- lm(coded_LB_log ~ narc, datw_short)) %>% summary
(LB_sqrt <- lm(coded_LB_sqrt ~ narc, datw_short)) %>% summary

anova(LB_ri, LB)

# Log may be preferred, but nonsig any way so reporting main one and letting s-curve speak
# for broader set of options:
ols_test_normality(LB)
ols_test_normality(LB_log)
ols_test_normality(LB_sqrt)

lm.beta(LB)
tidy(LB, conf.int = TRUE )
lm.beta(LB_log)
lm.beta(LB_sqrt)

(LF_ri <- lmer(coded_LF ~ narc + (1| class), datw_short)) %>% summary
(LF <- lm(coded_LF ~ narc_z, datw_short)) %>% summary

anova(LF_ri, LF)

# Issues w/normality
ols_test_normality(LF)

lm.beta(LF)
tidy(LF, conf.int = TRUE)



# ..sharing ---------------------------------------------------------------


(si_ri <- lmer(si_leader ~ narc + (1 | class), datw_short)) %>% summary
(si_log_ri <- lmer(si_leader_log ~ narc + (1 | class), datw_short)) %>% summary
(si_sqrt_ri <- lmer(si_leader_sqrt ~ narc + (1 | class), datw_short)) %>% summary
(si <- lm(si_leader ~ narc_z , datw_short)) %>% summary
(si_log <- lm(si_leader_log ~ narc, datw_short)) %>% summary
(si_sqrt <- lm(si_leader_sqrt ~ narc, datw_short)) %>% summary

(ui_ri <- lmer(ui_leader ~ narc + (1 | class), datw_short)) %>% summary
(ui_log_ri <- lmer(ui_leader_log ~ narc + (1 | class), datw_short)) %>% summary
(ui_sqrt_ri <- lmer(ui_leader_sqrt ~ narc + (1 | class), datw_short)) %>% summary
(ui <- lm(ui_leader ~ narc_z, datw_short)) %>% summary
(ui_log <- lm(ui_leader_log ~ narc, datw_short)) %>% summary
(ui_sqrt <- lm(ui_leader_sqrt ~ narc, datw_short)) %>% summary


ols_test_normality(si)
ols_test_normality(si_log)
ols_test_normality(si_sqrt)
ols_test_normality(ui)
ols_test_normality(ui_log)
ols_test_normality(ui_sqrt)

anova(si_sqrt_ri, si_sqrt)
anova(ui_ri, ui)

# B = 0.444, 95% CI = [-0.796, 1.68], ß = .069, p = .479
# B = 0.351, 95% CI = [-0.72, 2.78], ß = .063, p = .519

tidy(si, conf.int = TRUE)
lm.beta(si)
tidy(ui, conf.int = TRUE)
lm.beta(ui)


# ..group performance  ------------------------------------------------------

(gp <-
   glm(group_correct_binary ~ narc_z, family = "binomial", datw_short)) %>% summary

# OR = 1.03, B = 0.027, 95% CI = [-0.385, 0.437], p = .898
tidy(gp, conf.int = TRUE)
tidy(gp, conf.int = TRUE, exponentiate = TRUE)

lm.beta(gp)

# ..printouts -------------------------------------------------------------


printout(auth_self) %>% snip
printout(effect_self) %>% snip
printout(auth_others) %>% snip
printout(effect_others) %>% snip
printout(rankings_self) %>% snip
printout(rankings_others) %>% snip
printout(LB) %>% snip
printout(LF) %>% snip
printout(si) %>% snip
printout(ui) %>% snip
printout.logistic(gp) %>% snip

