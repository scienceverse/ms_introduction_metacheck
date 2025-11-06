
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

load("./processeddata/data.processed.wide.rda")

auth.scurve <- 
  s.curve(
    dat = datw_short, 
    outcomes = c(
      "auth_self", "auth_self_rsqrt", "auth_self_rlog"
    ),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    mod.type = "lm",
    keep.full.models = TRUE,
    rng.seed = 5678,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )

save(auth.scurve, file = "./scurve.output/grouptask/auth.self.scurve.rda") 


effect.scurve <- 
  s.curve(
    dat = datw_short, 
    outcomes = c(
      "effect_self", "effect_self_rsqrt", "effect_self_rlog"
    ),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ), 
    mod.type = "lm",
    rng.seed = 5678,
    keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )

save(effect.scurve, file = "./scurve.output/grouptask/effect.self.scurve.rda") 


ranking.self.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(leader_self)), 
    outcomes = c(
      "leader_self"
    ),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(ranking.self.scurve, file = "./scurve.output/grouptask/ranking.self.scurve.rda") 

dat <- datw_short %>% 
  filter(!is.na(auth_others))

auth.others.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(auth_others)), 
    outcomes = c(
      "auth_others", "auth_others_rsqrt", "auth_others_rlog"
    ),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(auth.others.scurve, file = "./scurve.output/grouptask/auth.others.scurve.rda") 


effect.others.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(effect_others)), 
    outcomes = c(
      "effect_others", "effect_others_rsqrt", "effect_others_rlog"
    ),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(effect.others.scurve, file = "./scurve.output/grouptask/effect.others.scurve.rda") 


ranking.others.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(leader_others)), 
    outcomes = c(
      "leader_others"
    ),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(ranking.others.scurve, file = "./scurve.output/grouptask/ranking.others.scurve.rda") 

codedbehavior.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(coded_LB)), 
    outcomes = c("coded_LB", "coded_LB_sqrt", "coded_LB_log"),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(codedbehavior.scurve, file = "./scurve.output/grouptask/codedbehavior.scurve.rda") 

codedfunction.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(coded_LF)), 
    outcomes = "coded_LF",
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    mod.type = "lm",
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(codedfunction.scurve, file = "./scurve.output/grouptask/codedfunction.scurve.rda")

sharedinfo.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(si_leader)), 
    outcomes = c("si_leader","si_leader_sqrt", "si_leader_log"),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(sharedinfo.scurve, file = "./scurve.output/grouptask/sharedinfo.scurve.rda") 

unsharedinfo.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(ui_leader)), 
    outcomes = c("ui_leader","ui_leader_sqrt", "ui_leader_log"),
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ),
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(unsharedinfo.scurve, file = "./scurve.output/grouptask/unsharedinfo.scurve.rda")

group_correct.scurve <- 
  s.curve(
    dat = datw_short %>% 
      filter(!is.na(group_correct)), 
    outcomes = "group_correct",
    treatment = "narc_z",
    cov.list = list(
      age = "age"
      , rel_age = "relative_age_group"
      , gen = "gender"
      , mean_gen = "gender_group"
      , experimenter = "experimenter_dummy"
      , task_version = "version1_dummy + version2_dummy"
      , time = "session_length"
      , group_homogeneity = "group_homogeneous_dummy"
    ), 
    mod.type = "glm",
    mod.family = "binomial",
    rng.seed = 5678, keep.full.models = TRUE,
    resample.type = "bootstrap",
    iterations = 2000,
    parallel = TRUE
  )
save(group_correct.scurve, file = "./scurve.output/grouptask/group_correct.scurve.rda")



