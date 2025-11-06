
if(!require(SpecCurve)){
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("jmobrien/SpecCurve", ref = "randomeffects")
  require(SpecCurve)
}
require(lmerTest)
require(glmmTMB)
require(sandwich)
require(lmtest)
require(broom.mixed)
require(broom)
require(tidyverse)

load("./processeddata/data.processed.rda")

# nbinom1 -----------------------------------------------------------------


start.time.nbinom1 <- Sys.time()
nbinom1.centered.boot <-
  s.curve(
    dat = dat,
    outcomes = "Lnominations",
    treatment = "narc_z",
    cov.list = list(age = "age_c",
                    rel_age = "relative_age_c",
                    gen = "gender_c",
                    mean_gen = "mean_gender_c"
    ),
    ranef.list =   list(school = c("(1 | class)",
                                   "(narc_z | class)")),
    mod.type = c("glmmTMB"),
    mod.family = c("nbinom1"),
    offset = "Lnominators_log",
    control = newcontrol,
    keep.full.models = TRUE,
    rng.seed = 5678,
    resample.type = "bootstrap",
    iterations = 500,
    parallel = TRUE
  )
end.time.nbinom1 <- Sys.time()
nbinom1.runtime <- 
  difftime(start.time.nbinom1, end.time.nbinom1, units = "mins")

save(nbinom1.centered.boot, file = "./scurve.output/leadershipemergence/nbinom1.centered.rda")


# Poisson -----------------------------------------------------------------


start.time.poisson <- Sys.time()
poisson.centered.boot <-
  s.curve(
    dat = dat,
    outcomes = "Lnominations",
    treatment = "narc_z",
    cov.list = list(age = "age_c",
                    rel_age = "relative_age_c",
                    gen = "gender_c",
                    mean_gen = "mean_gender_c"
    ),
    ranef.list =   list(school = c("(1 | class)",
                                   "(narc_z | class)")),
    mod.type = c("glmmTMB"),
    mod.family = c("poisson"),
    offset = "Lnominators_log",
    control = newcontrol,
    keep.full.models = TRUE,
    rng.seed = 5678,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
end.time.poisson <- Sys.time()
poisson.runtime <- 
  difftime(start.time.poisson, end.time.poisson, units = "mins")


save(poisson.centered.boot, file = "./scurve.output/leadershipemergence/poisson.centered.rda")

# nbinom2 -----------------------------------------------------------------


start.time.nbinom2 <- Sys.time()
nbinom2.centered.boot <-
  s.curve(
    dat = dat,
    outcomes = "Lnominations",
    treatment = "narc_z",
    cov.list = list(age = "age_c",
                    rel_age = "relative_age_c",
                    gen = "gender_c",
                    mean_gen = "mean_gender_c"
    ),
    ranef.list =   list(school = c("(1 | class)",
                                   "(narc_z | class)")),
    mod.type = c("glmmTMB"),
    mod.family = c("nbinom2"),
    offset = "Lnominators_log",
    control = newcontrol,
    keep.full.models = TRUE,
    rng.seed = 5678,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
end.time.nbinom2 <- Sys.time()
nbinom2.runtime <- 
  difftime(start.time.nbinom2, end.time.nbinom2, units = "mins")

save(nbinom2.centered.boot, file = "./scurve.output/leadershipemergence/nbinom2.centered.rda")

# loglinear -----------------------------------------------------------------


start.time.loglinear <- Sys.time()
loglinear.centered.boot <-
  s.curve(
    dat = dat,
    outcomes = "Lnominations_log",
    treatment = "narc_z",
    cov.list = list(age = "age_c",
                    rel_age = "relative_age_c",
                    gen = "gender_c",
                    mean_gen = "mean_gender_c"
    ),
    ranef.list =   list(school = c("(1 | class)",
                                   "(narc_z | class)")),
    mod.type = c("lmer"),
    control = lmerControl(optCtrl = list(maxeval=20000, algorithm="NLOPT_LN_NELDERMEAD")),
    offset = "Lnominators_log",
    keep.full.models = TRUE,
    rng.seed = 5678,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
end.time.loglinear <- Sys.time()
loglinear.runtime <- 
  difftime(start.time.loglinear, end.time.loglinear, units = "mins")


save(loglinear.centered.boot, file = "./scurve.output/leadershipemergence/loglinear.centered.rda")

# overdispersed -----------------------------------------------------------------
newcontrol <-  
  glmmTMBControl(optCtrl =  list(iter.max=900, eval.max=1200))

start.time.overdispersed <- Sys.time()
overdispersed.centered.boot <-
  s.curve(
    dat = dat,
    outcomes = "Lnominations",
    treatment = "narc_z",
    cov.list = list(age = "age_c",
                    rel_age = "relative_age_c",
                    gen = "gender_c",
                    mean_gen = "mean_gender_c"
    ),
    ranef.list =   list(school = c("(1 | obs_effect) + (1 | class)", 
                                   "(1 | obs_effect) + (narc_z | class)")
    ),
    mod.type = c("glmmTMB"),
    mod.family = c("poisson"),
    offset = "Lnominators_log",
    control = newcontrol,
    keep.full.models = TRUE,
    rng.seed = 5678,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
end.time.overdispersed <- Sys.time()
overdispersed.runtime <- 
  difftime(start.time.overdispersed, end.time.overdispersed, units = "mins")


save(overdispersed.centered.boot, file = "./scurve.output/leadershipemergence/overdispersed.centered.rda")


