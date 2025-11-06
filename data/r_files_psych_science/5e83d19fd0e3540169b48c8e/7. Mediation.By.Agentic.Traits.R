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
require(MplusAutomation)
require(lavaan)
require(tidyverse)


# data load ---------------------------------------------------------------

load("./processeddata/data.processed.rda")
load("./processeddata/data.processed.wide.rda")

# Mediation ---------------------------------------------------------------

# Looking at correlation w/agentic + communal traits:
dat %>% 
  select(narc_z, agentic_z, communal) %>% 
  as.matrix %>% 
  corstars



# write data for Mplus ML mediation model:

write.table(dat %>% 
              mutate(class = group_indices(., class)
              ) %>% 
              select(class,
                     narc_z, agentic_z, 
                     Lnominations, Lnominators_log),
            na = "-9999", sep = ", ",
            col.names = FALSE, row.names = FALSE,
            "./Mplus/Leader.Emergence.Mediation/leadership.emergence.mediation.dat.csv")

# Load back in:
nb.mediation <- 
  readModels("./Mplus/Leader.Emergence.Mediation/nom.mediation.simple.out", 
             what = "parameters")
  

nb.mediation



# Mediation of authority/effectiveness

med.model <- 
  "
  auth_self ~ c*narc_z 
  # agentic_z ~ a*narc_z
  
  # direct := c
  # indirect := a*b
  # total := c + a*b
  "

med.model2 <- 
  "
  auth_self ~ c*narc_z + b*agentic_z
  agentic_z ~ a*narc_z
  
  direct := c
  indirect := a*b
  total := c + a*b
  "

med.model.e <- 
  "
  effect_self ~ c*narc_z 
  # agentic_z ~ a*narc_z
  
  # direct := c
  # indirect := a*b
  # total := c + a*b
  "

med.model.e2 <- 
  "
  effect_self ~ c*narc_z + b*agentic_z
  agentic_z ~ a*narc_z
  
  direct := c
  indirect := a*b
  total := c + a*b
  "


mod.test <-
  sem(model = med.model,
      data = datw_short)
mod.test2 <-
  sem(model = med.model2,
      data = datw_short)

mod.test.e <-
  sem(model = med.model.e,
      data = datw_short)
mod.test.e2 <-
  sem(model = med.model.e2,
      data = datw_short)

summary(mod.test)
summary(mod.test2)
summary(mod.test.e)
summary(mod.test.e2)

