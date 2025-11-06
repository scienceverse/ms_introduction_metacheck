options(tibble.print_max = 40)


if(!require(SpecCurve)){
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("jmobrien/SpecCurve", ref = "randomeffects")
  require(SpecCurve)
}
require(lmerTest)
require(glmmTMB)
require(tidyverse)


# Read the files in the relevant output folder:
files <-
  list.files("./scurve.output/leadershipemergence/", full.names = TRUE)

# Load them up:
files %>% walk(load, envir = .GlobalEnv)

# Stack them:
nom.results <-
  bind_rows(
    nbinom1.centered.boot$results,
    nbinom2.centered.boot$results,
    poisson.centered.boot$results,
    overdispersed.centered.boot$results,
    loglinear.centered.boot$results
  )

nom.results.boot <-
  list(
    nbinom1.centered.boot$resample.test,
    nbinom2.centered.boot$resample.test,
    poisson.centered.boot$resample.test,
    overdispersed.centered.boot$resample.test,
    loglinear.centered.boot$resample.test
  ) %>% pmap(bind_rows)



nom.results.boot <-
  list(
    nbinom1.centered.boot$resample.test,
    nbinom2.centered.boot$resample.test,
    poisson.centered.boot$resample.test,
    overdispersed.centered.boot$resample.test,
    loglinear.centered.boot$resample.test
  ) %>% pmap(bind_rows)


# Copy framework from single output:
nom.scurve <- nbinom1.centered.boot
nom.scurve$results <- nom.results
nom.scurve$resample.test <- nom.results.boot

# Outputs NA's due to some random samples not being estimable, addressed below
nom.scurve <- s.curve.update(nom.scurve)
  
# Get data for convenience:
dat <- nom.scurve$dat

# LEADERSHIP EMERGENCE -------------------------------------------------------------




# Checking on the ones that had warnings (2 of them):
(nom.scurve$results$haswarnings <- 
    !vapply(nom.scurve$results$warnings, is.null, TRUE)) %>% table

# Looking at warning subset:
warning.subset <- 
  s.curve.subset(nom.scurve, haswarnings)

# Models w/warnings:
warning.subset$results$warnings # 2 of these are warnings during estimation that seem to have already resolved
warning.subset$results$formulas
warning.subset$results %>% select(estimate, p.value, significant)
warning.subset$results$tidy.models


# See if we can correct these other two, we can:
startvals <- list(-1.5, .15, -.1, 1)
lmer(Lnominations_log ~ narc_z + age_c + relative_age_c + (1 | Classroom_anonymized), dat, offset = Lnominators_log, 
     start = startvals,
     control = lmerControl(optCtrl = list(maxeval=200000, algorithm="NLOPT_LN_NELDERMEAD"))) %>% summary

lmer(Lnominations_log ~ narc_z + mean_gender_c + (1 | Classroom_anonymized), dat, offset = Lnominators_log, 
     start = startvals,
     control = lmerControl(optCtrl = list(maxeval=20000, algorithm="NLOPT_LN_NELDERMEAD"))) %>% summary


# Summaries can thus ignore warnings in main data.

# Make a list of results: 
nom.key.comparisons <- 
  c(mean.est = 
      nom.scurve$results$estimate %>% mean,
    sd.est = 
      nom.scurve$results$estimate %>% sd,
    median.est = 
      nom.scurve$results$estimate %>% median,
    sig.count = nom.scurve$results$significant %>% sum,
    sig.rate = nom.scurve$results$significant %>% mean
  )

# Note warnings in bootstrapping:
nom.scurve$resample.test <- 
  nom.scurve$resample.test %>% 
  map(~.x %>% 
        mutate(
          # If there are warnings in each permutation, mark them:
          haswarnings = 
            warnings %>% 
            map_lgl(is.null) %>% 
            {!.}))


# Reviewing warnings in bootstrapping:
warnings.test <- 
  nom.scurve$resample.test %>% 
  map_dfr(~.x %>% 
            select(model.family, p.value, significant, 
                   estimate, haswarnings, warnings, 
                   obs_effect = `obs_effect_sd__(Intercept).est`))

warnings.test %>% 
  group_by(model.family, is.na(obs_effect)) %>% 
  summarize(size = n(), 
            percent.w.warnings = mean(haswarnings, na.rm=T), 
            meansig = mean(significant, na.rm=T), 
            cor = cor(significant, haswarnings, use="pairwise.complete"),
            meanest = mean(estimate, na.rm=T),
            sdest = sd(estimate, na.rm=T),
            na.sum = sum(is.na(significant))
  )

warnings.test %>% 
  group_by(model.family, obs) %>% 
  summarize(cor = cor(significant, haswarnings, use = "pairwise.complete"))
summarize(size = n(), percent.w.warnings = mean(haswarnings))

# Basically, we get warnings where forcing the null has reduced 
# L2 slope variance down nearly zero.  
# But, if we exclude warnings we will make the comparisons less conservative,
# so keeping them in is more responsible for a significance test.


# Generating comparison statistics for s-curve against bootstrapping, adjusting for NA's
nom.test.summary <- 
  nom.scurve$resample.test %>% 
  map_dfr(~.x %>% 
            summarize(
              # Descriptives
              total = n(),
              NA_count = sum(is.na(significant)),
              warning_count = filter(., haswarnings) %>% nrow,
              warning_proportion = (filter(., haswarnings) %>% nrow)/total,
              NA_rows_excluding_warnings = filter(., !haswarnings) %>% pull(significant) %>% is.na %>% sum,
              
              # Significance counts/rates:
              count_significant = sum(significant, na.rm=T),
              prop_significant = mean(significant, na.rm = T),
              
              # Mean over total (lower, liberal)
              prop_sig_NAinclusive = count_significant/total,
              prop_sig_NAexclusive = count_significant/(total-NA_count),
              
              # Estimates:
              # Did this iteration have an equal or greater count/rate of significant
              # specifications?
              sig_count_equiv = count_significant >= nom.key.comparisons[["sig.count"]],
              sig_prop_equiv = prop_significant >= nom.key.comparisons[["sig.rate"]],
              sig_prop_NAinc_equiv = prop_sig_NAinclusive >= nom.key.comparisons[["sig.rate"]],
              sig_prop_NAexc_equiv = prop_sig_NAexclusive >= nom.key.comparisons[["sig.rate"]],
              
              # Comparison of coefficient estimates:
              mean_estimate = mean(estimate, na.rm = T),
              sd_estimate = sd(estimate, na.rm = T),
              median_estimate = median(estimate, na.rm = T),
              
              mean_absestimate = mean(abs(estimate), na.rm = T),
              sd_absestimate = sd(abs(estimate), na.rm = T),
              median_absestimate = median(abs(estimate), na.rm = T),
              
              mean_IRR = mean(exp(estimate), na.rm=T),
              median_IRR = median(exp(estimate), na.rm=T),
              
              medianest_compare = median_estimate >= nom.key.comparisons[["median.est"]]
              
            )
  )



(nom.test.summary.summary <-
    nom.test.summary %>% 
    summarize_all(.funs = c(mean = mean, sd = sd, sum = sum)) %>% 
    pivot_longer(cols = everything(),
                 names_to = c("estimate", ".value"),
                 names_pattern = "(.*)_(.*)"
    ))

# Plot generation--underdeveloped, but still useful.  
# Unsorted, right side shows log-linear models that are the nonsig ones:

nom.plot <- s.curve.plot(nom.scurve, title = "Nomination model plot" )


# Final review:

# Main s-curve results:
nom.key.comparisons

# Bootstrap test results:
nom.test.summary.summary

save(nom.plot, nom.scurve, nom.key.comparisons, nom.test.summary, nom.test.summary.summary,
     file = "./processedresults/summary.results.leadershipemergence.scurve.rda")




# group task --------------------------------------------------------------

grouptaskfiles <-
  list.files("./scurve.output/grouptask/", full.names = TRUE)

grouptaskfiles %>% 
  walk(load, envir = .GlobalEnv)

s.curve.report(ranking.self.scurve) # 0/256
s.curve.report(effect.others.scurve) #0/768
s.curve.report(auth.others.scurve) # 0/768
s.curve.report(ranking.others.scurve) # 0/256
s.curve.report(codedbehavior.scurve) # 0/256
s.curve.report(codedfunction.scurve) # 0/256
s.curve.report(group_correct.scurve) # 0/256

# Output Median estimates to table (uses PsychMisc & DescTools, not loaded)
# list(
#   auth.others.scurve,
#   effect.others.scurve,
#   ranking.others.scurve,
#   ranking.self.scurve,
#   codedbehavior.scurve,
#   codedfunction.scurve,
#   sharedinfo.scurve,
#   unsharedinfo.scurve,
#   group_correct.scurve
# ) %>% 
#   map_dbl("median.estimate") %>% 
#   Format(digits = 2, sci = NA) %>% 
#   esnip


# Remove these from memory for performance reasons, nothing further
# to do with them:
rm(ranking.self.scurve, effect.others.scurve,
   auth.others.scurve, ranking.others.scurve,
   codedbehavior.scurve, codedfunction.scurve,
   group_correct.scurve)


# Focus on these: 
s.curve.report(auth.scurve)
s.curve.report(effect.scurve)

auth.key.comparisons <- 
  c(mean.est = 
      auth.scurve$results$estimate %>% mean,
    sd.est = 
      auth.scurve$results$estimate %>% sd,
    median.est = 
      auth.scurve$results$estimate %>% median,
    sig.count = auth.scurve$results$significant %>% sum,
    sig.rate = auth.scurve$results$significant %>% mean
  )

effect.key.comparisons <- 
  c(mean.est = 
      effect.scurve$results$estimate %>% mean,
    sd.est = 
      effect.scurve$results$estimate %>% sd,
    median.est = 
      effect.scurve$results$estimate %>% median,
    sig.count = effect.scurve$results$significant %>% sum,
    sig.rate = effect.scurve$results$significant %>% mean
  )


auth.summary <- 
  auth.scurve$resample.test %>% 
  map_dfr(~.x %>% 
            summarize(
              # Descriptives
              total = n(),
               
              # Significance counts/rates:
              count_significant = sum(significant, na.rm=T),
              prop_significant = mean(significant, na.rm = T),
              
              # Estimates:
              # Did this iteration have an equal or greater count/rate of significant
              # specifications?
              sig_count_equiv = count_significant >= auth.key.comparisons[["sig.count"]],
              sig_prop_equiv = prop_significant >= auth.key.comparisons[["sig.rate"]],
              
              # Comparison of coefficient estimates:
              mean_estimate = mean(estimate, na.rm = T),
              sd_estimate = sd(estimate, na.rm = T),
              median_estimate = median(estimate, na.rm = T),
              
              mean_absestimate = mean(abs(estimate), na.rm = T),
              sd_absestimate = sd(abs(estimate), na.rm = T),
              median_absestimate = median(abs(estimate), na.rm = T),
              
              medianest_compare = median_estimate >= auth.key.comparisons[["median.est"]]
              
            )
  )

effect.summary <- 
  effect.scurve$resample.test %>% 
  map_dfr(~.x %>% 
            summarize(
              # Descriptives
              total = n(),
              
              # Significance counts/rates:
              count_significant = sum(significant, na.rm=T),
              prop_significant = mean(significant, na.rm = T),
              
              # Estimates:
              # Did this iteration have an equal or greater count/rate of significant
              # specifications?
              sig_count_equiv = count_significant >= auth.key.comparisons[["sig.count"]],
              sig_prop_equiv = prop_significant >= auth.key.comparisons[["sig.rate"]],
              
              # Comparison of coefficient estimates:
              mean_estimate = mean(estimate, na.rm = T),
              sd_estimate = sd(estimate, na.rm = T),
              median_estimate = median(estimate, na.rm = T),
              
              mean_absestimate = mean(abs(estimate), na.rm = T),
              sd_absestimate = sd(abs(estimate), na.rm = T),
              median_absestimate = median(abs(estimate), na.rm = T),
              
              medianest_compare = median_estimate >= auth.key.comparisons[["median.est"]]
              
            )
  )

# Final review:

# Main curve findings:
auth.key.comparisons

# Summary of bootstrap findings:
auth.summary.summary <-
  auth.summary %>% 
  summarize_all(.funs = c(mean = mean, sd = sd)) %>% 
  pivot_longer(cols = everything(),
               names_to = c("estimate", ".value"),
               names_pattern = "(.*)_(.*)"
  )


# Main curve findings:
effect.key.comparisons

# Summary of bootstrap findings
effect.summary.summary <-
  effect.summary %>% 
  summarize_all(.funs = c(mean = mean, sd = sd)) %>% 
  pivot_longer(cols = everything(),
               names_to = c("estimate", ".value"),
               names_pattern = "(.*)_(.*)"
  )

auth.plot <- s.curve.plot(auth.scurve, title = "Self-Rated Authority S-Curve")
effect.plot <- s.curve.plot(effect.scurve, title = "Self-Rated Effectiveness S-Curve")


# Final review: 

# Main curve findings:
auth.key.comparisons

# Summary of bootstrap findings:
auth.summary.summary

# Main curve findings:
effect.key.comparisons

# Summary of bootstrap findings
effect.summary.summary

save(auth.plot, effect.plot, auth.key.comparisons, effect.key.comparisons, 
     auth.summary, auth.summary.summary, effect.summary, effect.summary.summary, 
     file = "./processedresults/summary.results.grouptask.selfratings.scurve.rda")
