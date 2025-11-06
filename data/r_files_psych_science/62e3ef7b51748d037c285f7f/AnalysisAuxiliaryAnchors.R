rm(list = ls())

###### AUXILIARY ANCHOR GUIDANCE - Data analysis
###### In this script, we analyze the preprocessed experimental data

# get all necessary packages
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(afex, car, ggplot2, grid, lme4, lmerTest, MASS, MuMIn, Rmisc)

# set up the working directory (must contain the "DataAuxiliaryAnchors.RData" file)
setwd(" ") # your working directory goes here

# load the preprocessed data
load("AuxiliaryAnchors_Data.RData")

# Questions of the analysis: Are the processes of (1) locating and (2) identifying local objects 
# (a) speeded by the presence of anchor objects (vs. cuboids) in consistent scenes and
# (b) slowed down by those anchors in inconsistent scenes (faster for cuboids)?
# (c) Is this also reflected in more or less efficient body locomotion?

# Dependent variables
# (i) for locating efficiency: time to first fixation, number of fixations, scanpath length
# (ii) for object identification speed: decision time
# (iii) for how much participants moved: length of movement, spatial extent of movement

# Predictors besides scene consistency and anchor presence are
# - incidental gaze duration: how long was the target looked at on previous trials where it was a distractor
# - target angle: angle between gaze at trial on- and offset (measure of how spatially difficult search is)
# - trial number: increased familiarity could speed search 
#   (in part because of incidental gaze, but maybe also beyond that)
# - episode number: should improve from episode to episode (especially from 1st to 2nd)

#### data preparation ####

# contrast coding
# prepare contrasts for the conditions
(contrasts(trials$consistency) <- contr.sdif(2))
(contrasts(trials$anchors) <- contr.sdif(2))

# factor for the two episode transitions (from 1 to 2, from 2 to 3)
trials$episode <- factor(trials$episode)
(contrasts(trials$episode) <- contr.sdif(3))

# prepare variable transforms for modelling
# log transform dependent measures (box-cox transform selection see below)
trials$firstfix_t_log <- log(trials$firstfix_t)
trials$fix_total_log <- log(trials$fix_total)
trials$fixpath_len_log <- log(trials$fixpath_len)
trials$dec_t_log <- log(trials$dec_t)
trials$move_len_log <- log(trials$move_len)
trials$move_area_log <- log(trials$move_area)
# scale and center numerical predictors
trials$trial_c <- scale(trials$trial)
trials$gaze_inc_c <- scale(trials$gaze_inc)
trials$angle_c <- scale(trials$angle)

# subsetting
# subset of all trials with correct response
hits <- trials[trials$acc == 1,]
nrow(hits) / nrow(trials) # that's ~ 97.1% of trials
sum(as.character(hits$gaze_obj) == as.character(hits$target_id)) / nrow(hits) # ~ 92.9% of hits had on-target gaze at trigger pull (other hits coded 1 post hoc)
nrow(trials[trials$timeout == 1,]) / (nrow(trials) - nrow(hits)) # ~ 47.1% of inaccurate trials are timeouts

# another subset of this: all hits with a target fixation
hits_fix <- hits[!is.na(hits$firstfix_t),]
nrow(hits_fix) / nrow(hits) # that's ~ 92.8% of hits

# yet another subset: those hits with target fixation that did not start at trial onset
hits_fix_t <- hits_fix[hits_fix$firstfix_t != 0,]
nrow(hits_fix_t) / nrow(hits_fix) # ~ 91.0% of hits with a target fixation
# we can see that trials with initial target fixations increase with episode number 
aggregate(firstfix_t + 1 ~ episode, FUN = sum, data = hits_fix[hits_fix$firstfix_t == 0,])

# lastly, we create a subset with trials that have more than one fixation (and thus an interpretable scanpath length)
hits_fix_path <- hits_fix_t[hits_fix_t$fix_total > 1,]
nrow(hits_fix_path) / nrow(hits_fix_t) # ~ 95.5% of hits with a target fixation that is not initial

# look at dependent variables' distributions and residuals
# 1) time to first fixation
ggplot(hits_fix_t, aes(x = firstfix_t)) + geom_density()
qqmath(hits_fix_t$firstfix_t)
lm_ffix <- lm(firstfix_t ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix_t)
qplot(lm_ffix$residuals, bins = 500)
plot(lm_ffix$fitted.values, lm_ffix$residuals, pch = ".")
# Box-Cox procedure
lambdas_ffix <- boxcox(firstfix_t ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix_t)
(lambda_ffix <- lambdas_ffix$x[which.max(lambdas_ffix$y)]) # it's ~ 0.10
# so we will log-transform:
ggplot(hits_fix_t, aes(x = log(firstfix_t))) + geom_density()
qqmath(log(hits_fix_t$firstfix_t))
lm_ffix_log <- lm(log(firstfix_t) ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix_t)
qplot(lm_ffix_log$residuals, bins = 500)
plot(lm_ffix_log$fitted.values, lm_ffix_log$residuals, pch = ".")

# 2) number of fixations
ggplot(hits_fix, aes(x = fix_total)) + geom_histogram(binwidth = 1)
qqmath(hits_fix$fix_total)
lm_fixnum <- lm(fix_total ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix)
qplot(lm_fixnum$residuals, bins = 500)
plot(lm_fixnum$fitted.values, lm_fixnum$residuals, pch = ".")
# Box-Cox procedure
lambdas_fixnum <- boxcox(fix_total ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix)
(lambda_fixnum <- lambdas_fixnum$x[which.max(lambdas_fixnum$y)]) # it's ~ -0.10
# go for a Poisson GLMM

# 3) scanpath length
ggplot(hits_fix_path, aes(x = fixpath_len)) + geom_density()
qqmath(hits_fix$fixpath_len)
lm_scanpath <- lm(fixpath_len ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix_path)
qplot(lm_scanpath$residuals, bins = 500)
plot(lm_scanpath$fitted.values, lm_scanpath$residuals, pch = ".")
# Box-Cox procedure
lambdas_scanpath <- boxcox(fixpath_len ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix_path)
(lambda_scanpath <- lambdas_scanpath$x[which.max(lambdas_scanpath$y)]) # it's ~ 0.18
# log-transform here too:
ggplot(hits_fix_path, aes(x = log(fixpath_len))) + geom_density()
qqmath(log(hits_fix_path$fixpath_len))
lm_scanpath_log <- lm(log(fixpath_len) ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits_fix_path)
qplot(lm_scanpath_log$residuals, bins = 500)
plot(lm_scanpath_log$fitted.values, lm_scanpath_log$residuals, pch = ".")

# 4) decision time
ggplot(hits_fix_t, aes(x = dec_t)) + geom_density() + xlim(0, 5)
qqmath(hits_fix_t$dec_t)
lm_dec <- lm(dec_t ~ (consistency + anchors + scale(trial) + episode) ^ 2, data = hits_fix_t)
qplot(lm_dec$residuals, bins = 500) + xlim(-1, 5)
plot(lm_dec$fitted.values, lm_dec$residuals, pch = ".")
# Box-Cox procedure
lambdas_dec <- boxcox(dec_t ~ (consistency + anchors + scale(trial) + episode) ^ 2, data = hits_fix_t)
(lambda_dec <- lambdas_dec$x[which.max(lambdas_dec$y)]) # it's ~ -0.14
# again, log-transform:
ggplot(hits_fix, aes(x = log(dec_t))) + geom_density()
qqmath(log(hits_fix_t$dec_t))
lm_dec_log <- lm(log(dec_t) ~ (consistency + anchors + scale(trial) + episode) ^ 2, data = hits_fix_t)
qplot(lm_dec_log$residuals, bins = 500)
plot(lm_dec_log$fitted.values, lm_dec_log$residuals, pch = ".")

# 5) length of movement
ggplot(hits, aes(x = move_len)) + geom_density() + xlim(0, 4)
qqmath(hits$move_len)
lm_movelen <- lm(move_len ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits)
qplot(lm_movelen$residuals, bins = 500)
plot(lm_movelen$fitted.values, lm_movelen$residuals, pch = ".")
# Box-Cox procedure
lambdas_movelen <- boxcox(move_len ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits)
(lambda_movelen <- lambdas_movelen$x[which.max(lambdas_movelen$y)]) # it's ~ 0.10
# log-transform:
ggplot(hits, aes(x = log(move_len))) + geom_density()
qqmath(log(hits$move_len))
lm_movelen_log <- lm(log(move_len) ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits)
qplot(lm_movelen_log$residuals, bins = 500)
plot(lm_movelen_log$fitted.values, lm_movelen_log$residuals, pch = ".")

# 6) spatial extent of movement
ggplot(hits, aes(x = move_area)) + geom_density() + xlim(0, 0.25)
qqmath(hits$move_area)
lm_movespace <- lm(move_area ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits)
qplot(lm_movespace$residuals, bins = 500) + xlim(-0.1, 0.2)
plot(lm_movespace$fitted.values, lm_movespace$residuals, pch = ".")
# Box-Cox procedure
lambdas_movespace <- boxcox(move_area ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits)
(lambda_movespace <- lambdas_movespace$x[which.max(lambdas_movespace$y)]) # it's ~ 0.06
# log-transform:
ggplot(hits, aes(x = log(move_area))) + geom_density()
qqmath(log(hits$move_area))
lm_movespace_log <- lm(log(move_area) ~ (consistency + anchors + scale(trial) + episode + scale(gaze_inc) + scale(angle)) ^ 2, data = hits)
qplot(lm_movespace_log$residuals, bins = 500)
plot(lm_movespace_log$fitted.values, lm_movespace_log$residuals, pch = ".")


#### model selection ####
# using (G)LMMs to predict eye and motion tracking variables

# we start with maximal full models and then (if need be) reduce random effects structures based on what is supported by the data
# as fixed effects, we include our manipulations, the other predictors as fixed covariates, and all possible two-way interactions 
# (higher order interactions hardly improve the model and would strongly inflate it, e.g. 20 three-way interactions, causing convergence issues and overparameterization)
# for the random effects, we start with subject and target object intercepts and by-subject slopes for our conditions

# time to first fixation
# maximal model (all possible random intercepts and slopes, correlated random effects)
(summary(m_ffix_full <- lmer_alt(firstfix_t_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                  + (1 + consistency/anchors | subject)
                                  + (1 | target_id),
                                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                  data = hits_fix_t))) # converges
# inspect dimensionality to check for overparameterization 
summary(rePCA(m_ffix_full)) # looks ok, last two subject components hardly add variance though
# compare to a model with fixed zero correlations of random effects
(summary(m_ffix_zc <- lmer_alt(firstfix_t_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                  + (1 + consistency/anchors || subject)
                                  + (1 | target_id),
                                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                  data = hits_fix_t)))
summary(rePCA(m_ffix_zc)) # not much change here (last subject component still rather low in adding variance)
anova(m_ffix_full, m_ffix_zc) # correlation parameters do not sig. improve fit (only add complexity)  
# we continue with the uncorrelated model (better AIC/BIC, not sig. worse than the full model's fit)
# try a simpler model without the by-subject anchor slopes
(summary(m_ffix_s <- lmer_alt(firstfix_t_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                  + (1 + consistency || subject)
                                  + (1 | target_id),
                                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                  data = hits_fix_t)))
summary(rePCA(m_ffix_s)) # random effects structure looks fine
anova(m_ffix_zc, m_ffix_s) # the simpler model is sig. worse
# so our optimal model is the one with uncorrelated random effects (full otherwise)
summary(m_ffix_zc)
r.squaredGLMM(m_ffix_zc) # explained variance
vif(m_ffix_zc) # multicollinearity all fine
qplot(resid(m_ffix_zc), bins = 500) # check residual normal distribution

# number of fixations
# maximal poisson model (all possible random intercepts and slopes, correlated random effects)
(summary(m_fixnum_full <- lmer_alt(fix_total ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                  + (1 + consistency/anchors | subject)
                                  + (1 | target_id),
                                  family = poisson,
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                  data = hits_fix))) # converges
# check if the model is degenerate
summary(rePCA(m_fixnum_full)) # looks good, again rather weak last component
# compare to a model with fixed zero correlations of random effects
(summary(m_fixnum_zc <- lmer_alt(fix_total ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                   + (1 + consistency/anchors || subject)
                                   + (1 | target_id),
                                   family = poisson,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                   data = hits_fix)))
summary(rePCA(m_fixnum_zc)) # fine, largely unchanged
anova(m_fixnum_full, m_fixnum_zc) # correlations do not sig. improve fit 
# we continue with the uncorrelated model (better AIC/BIC, not sig. worse than the full model's fit)
# try a simpler model without the by-subject anchor slopes
(summary(m_fixnum_s <- lmer_alt(fix_total ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                   + (1 + consistency || subject)
                                   + (1 | target_id),
                                   family = poisson,
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                   data = hits_fix)))
summary(rePCA(m_fixnum_s)) # perfect
anova(m_fixnum_zc, m_fixnum_s) # the simplification is not supported by the data
# so our optimal model is the one with subject and target intercepts, by-subject consistency and anchor slopes, and uncorrelated random effects
summary(m_fixnum_zc)
r.squaredGLMM(m_fixnum_zc) # explained variance
vif(m_fixnum_zc) # multicollinearity all fine
qplot(resid(m_fixnum_zc), bins = 500) # check residual normal distribution

# scanpath length
# maximal model (all possible random intercepts and slopes, correlated random effects)
(summary(m_scanpath_full <- lmer_alt(fixpath_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                     + (1 + consistency/anchors | subject)
                                     + (1 | target_id),
                                     control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                     data = hits_fix_path))) # does not converge
# check dimensionality
summary(rePCA(m_scanpath_full)) # degenerate (comp. #4)
# compare to a model with correlated random effects
(summary(m_scanpath_zc <- lmer_alt(fixpath_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                       + (1 + consistency/anchors || subject)
                                       + (1 | target_id),
                                       control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                       data = hits_fix_path)))
summary(rePCA(m_scanpath_zc)) # not degenerate (weak last comp. though)
anova(m_scanpath_full, m_scanpath_zc) # choose the uncorrelated model (better AIC/BIC, no sig. chi sq. difference)
# simplified model without the by-subject anchor slopes
(summary(m_scanpath_s <- lmer_alt(fixpath_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                   + (1 + consistency || subject)
                                   + (1 | target_id),
                                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                   data = hits_fix_path)))
summary(rePCA(m_scanpath_s)) # perfect
anova(m_scanpath_zc, m_scanpath_s) # choose the parsimonious model (no sig. difference)
# compare to an intercept model
(summary(m_scanpath_icp <- lmer_alt(fixpath_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                + (1 | subject)
                                + (1 | target_id),
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                data = hits_fix_path)))
anova(m_scanpath_s, m_scanpath_icp) # intercept model is worse (AIC/BIC and sig. chisq. fit test)
# we stick to the model with the by-subject consistency slope and uncorrelated random effects
summary(m_scanpath_s)
r.squaredGLMM(m_scanpath_s) # explained variance
vif(m_scanpath_s) # multicollinearity all fine
qplot(resid(m_scanpath_s), bins = 500) # check residual normal distribution

# decision time
# full max. model (all possible random intercepts and slopes, correlated random effects)
(summary(m_dec_full <- lmer_alt(dec_t_log ~ (consistency/anchors + episode + gaze_inc_c + angle_c) ^ 2
                                + (1 + consistency/anchors | subject)
                                + (1 | target_id),
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                data = hits_fix_t))) # does not converge
# check if degenerate
summary(rePCA(m_dec_full)) # degenerate (0 comp. #4)
# compare to a model with uncorrelated random effects
(summary(m_dec_zc <- lmer_alt(dec_t_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                  + (1 + consistency/anchors || subject)
                                  + (1 | target_id),
                                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                  data = hits_fix_t))) # converges
summary(rePCA(m_dec_zc)) # better, but last component still very weak
anova(m_dec_full, m_dec_zc) # cannot reduce the full (not-converging) model like this
# instead, try a model with correlations but without the anchor-slope
(summary(m_dec_s <- lmer_alt(dec_t_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                  + (1 + consistency | subject)
                                  + (1 | target_id),
                                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                  data = hits_fix_t))) #  converges
summary(rePCA(m_dec_s)) # alright (though comp. 2 is still very weak)
anova(m_dec_full, m_dec_s) # still, the full model wins
# we stick to the full model (though it is degenerate and does not converge, none of the simplifications are supported)
summary(m_dec_full)
r.squaredGLMM(m_dec_full) # explained variance
vif(m_dec_full) # multicollinearity all fine
qplot(resid(m_dec_full), bins = 500) # check residual normal distribution

# length of movement
# full model (all possible random intercepts and slopes, correlated random effects)
(summary(m_movelen_full <- lmer_alt(move_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                    + (1 + consistency/anchors | subject)
                                    + (1 | target_id),
                                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                    data = hits))) # converges
# check if the model is degenerate
summary(rePCA(m_movelen_full)) # looks ok, weak comp. #4 though
# compare to a model with uncorrelated random effects
(summary(m_movelen_zc <- lmer_alt(move_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                      + (1 + consistency/anchors || subject)
                                      + (1 | target_id),
                                      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                      data = hits)))
summary(rePCA(m_movelen_zc)) # not degenerate
anova(m_movelen_full, m_movelen_zc) # uncorrelated wins (better inf. criteria, no sig. fit diff.)
# simplified model without the by-subject anchor slopes
(summary(m_movelen_s <- lmer_alt(move_len_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                 + (1 + consistency || subject)
                                 + (1 | target_id),
                                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                 data = hits)))
summary(rePCA(m_movelen_s)) # not degenerate
anova(m_movelen_s, m_movelen_zc) # anchor slope should stay in
# we stick to the full, uncorrelated model
summary(m_movelen_zc)
r.squaredGLMM(m_movelen_zc) # explained variance
vif(m_movelen_zc) # multicollinearity all fine
qplot(resid(m_movelen_zc), bins = 500) # check residual normal distribution

# spatial extent of movement
# max. model (all possible random intercepts and slopes, correlated random effects)
(summary(m_movespace_full <- lmer_alt(move_area_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                      + (1 + consistency/anchors | subject)
                                      + (1 | target_id),
                                      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                      data = hits))) # does not converge
# check model dimensionality
summary(rePCA(m_movespace_full)) # degenerate (comp. #4)
# compare to a model with uncorrelated random effects
(summary(m_movespace_zc <- lmer_alt(move_area_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                        + (1 + consistency/anchors || subject)
                                        + (1 | target_id),
                                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                        data = hits))) # converges
summary(rePCA(m_movespace_zc)) # ok, weak last comp.
anova(m_movespace_full, m_movespace_zc) # full model is sig. better
# try a simplification without the anchor slope (by-subject) but with random correlations
(summary(m_movespace_s <- lmer_alt(move_area_log ~ (consistency/anchors + trial_c + episode + gaze_inc_c + angle_c) ^ 2
                                    + (1 + consistency | subject)
                                    + (1 | target_id),
                                    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                                    data = hits))) # converges
summary(rePCA(m_movespace_s)) # perfect
anova(m_movespace_full, m_movespace_s) # full model still wins
# so we stick to the full model (not-converging, degenerate)
summary(m_movespace_full)
r.squaredGLMM(m_movespace_full) # explained variance
vif(m_movespace_full) # multicollinearity all fine
qplot(resid(m_movespace_full), bins = 500) # check residual normal distribution

##### result plotting #####
# plotting the consistency x anchor presence interactions

# plot blueprint function
con_x_anc_plt <- function(data, title) {
  return(
    ggplot(data, aes(x = anchors, y = data[,4], color = interaction(consistency, anchors))) + 
      scale_alpha_manual(values = c(1, 0.5)) +
      geom_errorbar(aes(ymin = data[,4] - se, ymax = data[,4] + se), width = 0.1, size = 0.7) +
      geom_point(size = 2.5) +
      theme(panel.border = element_rect(color = "black", fill = NA, size = .5)) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(color = "black")) +
      theme(axis.text.y = element_text(color = "black")) +
      theme(axis.ticks.x = element_line(color = "black")) +
      theme(axis.ticks.y = element_line(color = "black")) +
      labs(x = element_blank(), y = title) +
      scale_color_manual(values = c("#00a2ff", "#ee220c", "#8ad0fe", "#f6908b")) +
      scale_x_discrete(labels = c("Anchors", "Cuboids", "Anchors", "Cuboids")) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0))) +
      theme(panel.spacing.x = unit(0.5, "line"), strip.background = element_blank(), strip.text = element_text(color = "black", size = 10))
  )
}

# ... time to first fixation (log)
agg_ffix_log_con_x_anc_subj <- aggregate(firstfix_t_log ~ consistency + anchors + subject, FUN = mean, data = hits_fix_t)
agg_ffix_log_con_x_anc <- summarySEwithin(data = agg_ffix_log_con_x_anc_subj, measurevar = "firstfix_t_log", withinvars = c("consistency", "anchors"), idvar = "subject")
(plot_ffix_con_x_anc_log <- con_x_anc_plt(agg_ffix_log_con_x_anc, "Time to first fixation (log s)"))

# ... number of fixations (log)
agg_fixnum_log_con_x_anc_subj <- aggregate(fix_total_log ~ consistency + anchors + subject, FUN = mean, data = hits_fix)
agg_fixnum_log_con_x_anc <- summarySEwithin(data = agg_fixnum_log_con_x_anc_subj, measurevar = "fix_total_log", withinvars = c("consistency", "anchors"), idvar = "subject")
(plot_fixnum_con_x_anc_log <- con_x_anc_plt(agg_fixnum_log_con_x_anc, "Number of fixations (log)"))

# ... scanpath length (log)
agg_scanpath_log_con_x_anc_subj <- aggregate(fixpath_len_log ~ consistency + anchors + subject, FUN = mean, data = hits_fix_path)
agg_scanpath_log_con_x_anc <- summarySEwithin(data = agg_scanpath_log_con_x_anc_subj, measurevar = "fixpath_len_log", withinvars = c("consistency", "anchors"), idvar = "subject")
(plot_scanpath_con_x_anc_log <- con_x_anc_plt(agg_scanpath_log_con_x_anc, "Scanpath length (log m)"))

# ... decision time (log)
agg_dec_log_con_x_anc_subj <- aggregate(dec_t_log ~ consistency + anchors + subject, FUN = mean, data = hits_fix_t)
agg_dec_log_con_x_anc <- summarySEwithin(data = agg_dec_log_con_x_anc_subj, measurevar = "dec_t_log", withinvars = c("consistency", "anchors"), idvar = "subject")
(plot_dec_con_x_anc_log <- con_x_anc_plt(agg_dec_log_con_x_anc, "Decision time (log s)"))

# ... movement path length (log)
agg_movelen_log_con_x_anc_subj <- aggregate(move_len_log ~ consistency + anchors + subject, FUN = mean, data = hits)
agg_movelen_log_con_x_anc <- summarySEwithin(data = agg_movelen_log_con_x_anc_subj, measurevar = "move_len_log", withinvars = c("consistency", "anchors"), idvar = "subject")
(plot_movelen_con_x_anc_log <- con_x_anc_plt(agg_movelen_log_con_x_anc, "Movement path length (log m)"))

# ... spatial extent of movement (log)
agg_movespace_log_con_x_anc_subj <- aggregate(move_area_log ~ consistency + anchors + subject, FUN = mean, data = hits)
agg_movespace_log_con_x_anc <- summarySEwithin(data = agg_movespace_log_con_x_anc_subj, measurevar = "move_area_log", withinvars = c("consistency", "anchors"), idvar = "subject")
(plot_movespace_con_x_anc_log <- con_x_anc_plt(agg_movespace_log_con_x_anc, "Spatial extent of movement (log m²)"))

# export figures 
pdf("ffix_con_x_anchor_log.pdf", width = 2.2, height = 3); grid.draw(plot_ffix_con_x_anc_log); dev.off()
pdf("fixnum_con_x_anchor_log.pdf", width = 2.2, height = 3); grid.draw(plot_fixnum_con_x_anc_log); dev.off()
pdf("scanpath_con_x_anchor_log.pdf", width = 2.2, height = 3); grid.draw(plot_scanpath_con_x_anc_log); dev.off()
pdf("dec_con_x_anchor_log.pdf", width = 2.2, height = 3); grid.draw(plot_dec_con_x_anc_log); dev.off()
pdf("movelen_con_x_anchor_log.pdf", width = 2.2, height = 3); grid.draw(plot_movelen_con_x_anc_log); dev.off()
pdf("movespace_con_x_anchor_log.pdf", width = 2.2, height = 3); grid.draw(plot_movespace_con_x_anc_log); dev.off()