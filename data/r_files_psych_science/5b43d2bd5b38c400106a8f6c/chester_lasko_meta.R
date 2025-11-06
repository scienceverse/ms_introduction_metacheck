# Meta-analysis of provocation effect on hot sauce from Chester & Lasko (2018)
library(tidyverse)
library(compute.es)
library(metafor)

# PAIN 100% hot sauce (habenero-based, 40,600 scoville units)
# Youtube videos make it look a little thicker, closer to sriracha than tabasco
# Framed as a measure of "pain regulation" rather than taste test
# Happens after CRTT and before computerized Voodoo Doll Task

# Load study 1 data
dat <- read_csv("Chester_data/Study1_Data_WideFormat_ForUnivariateAnalyses.csv") %>% 
# discard failures of deception
    filter(FILTER_ExpressedTotalDisbelief == 0)

# run test
m <- lm(HotSauceWeight_ToOpponent ~ ProvocationCondition, data = dat)
summary(m)
# use test statistic to get ESCI
d1 <- tes(.608, 100, 94) # d = .09 [-0.2, 0.37]

m.log <- lm(HotSauceWeight_ToOpponent_LogTransformed ~ 
              ProvocationCondition, data = dat)
summary(m.log)
d1.log <- tes(.049, 100, 94) # d = .01 [-0.28, 0.29]

# Load study 2 data
dat2 <- read_csv("Chester_data/Study2_Data_WideFormat_ForUnivariateAnalyses.csv") %>% 
  filter(FILTER_ExpressedTotalDisbelief == 0)
# I'm not sure even the SPSS file makes it clear the task sequence
# dat2_spss <- read_sav("Chester_data/Study2_Data_WideFormat_ForUnivariateAnalyses.sav")

# Get N/cell
filter(dat2, !is.na(HotSauceWeight_ToOpponent)) %>% 
  with(., table(ProvocationCondition))

m2 <- lm(HotSauceWeight_ToOpponent ~ ProvocationCondition, data = dat2)
summary(m2)
d2 <- tes(1.069, 90, 96) # d = 0.16 [-0.13, 0.45]

m2.log <- lm(HotSauceWeight_ToOpponent_LogTransformed ~ ProvocationCondition, data = dat2)
summary(m2.log)
d2.log <- tes(.303, 90, 96) # d = 0.04 [-0.25, 0.33]

# meta-analysis ----
# in gram units
dat.ma1 <- data.frame(d = c(d1$d, d2$d),
                      v = c(d1$var.d, d2$var.d))
ma <- rma(yi = d, vi = v, data = dat.ma1)
summary(ma) # d = 0.125 [-0.07, 0.321]

# in log(gram) units
dat.ma1.log <- data.frame(d = c(d1.log$d, d2.log$d),
                          v = c(d1.log$var.d, d2.log$var.d))
ma.log <- rma(yi = d, vi = v, data = dat.ma1.log)
summary(ma.log) # even smaller
