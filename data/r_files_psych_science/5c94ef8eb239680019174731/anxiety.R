##################################################################
### Linking trait anxiety and dishonesty                       ###
###                                                            ###
### Contact: Daniel Heck, heck@uni-mannheim.de                 ###
###                                                            ###
### For technical details, see:                                ###
### Heck, D. W., Thielmann, I., Moshagen, M., & Hilbig, B. E.  ###
### (2018). Who lies? A large-scale reanalysis linking basic   ###
### personality traits to unethical decision making. Judgment  ###
### and Decision Making, 13(4), 356–371.                       ###
### http://journal.sjdm.org/18/18322/jdm18322.pdf              ###
###                                                            ###
##################################################################


######################### load packages / data / analysis functions

library("tidyverse")
library("rstan")
library("bridgesampling")
library("LaplacesDemon")
library("logspline")
library("xtable")
library("Formula")
library("GLMMRR")

source("cheating_functions.R")          # functions for analysis (Bayes factors)
load("data/cheating.rda", verbose = TRUE)  # merged data set (16 studies)


# MCMC settings
iter <- 22000 
chains <- 8
adapt_delta <- .97
set.seed(12345)


######################### prior settings for scaling parameter (rscale)

# prior scale parameter (cf. ?BayesFactor::regressionBF)
# rscale <- 1/4     # very small
# rscale <- sqrt(2)/4  # small 
rscale <- 1/2        # medium
1 - 2*pcauchy(-.5, 0, rscale)  # probability for beta-coefficient in [-.5, .5]
1 - 2*pcauchy(-1, 0, rscale)   # probability for beta-coefficient in [ -1,  1]

# prior distribution of odds ratios (only plot odds ratios between 1 and 5):
or <- exp(abs(rcauchy(1e6, 0, rscale)))
mean(or < 5)   # (P(odds ratio < 5))


######################### Regression Models for Bayesian analysis

##### Notes: 
# - the baseline probability "p" of receiving the payoff is *not* a covariate, 
#   but a design factor of the cheating paradigm. Hence, "p" is written after 
#   the bar symbol:  win ~ XXX | p
# - covariates must be centered and standardized by the user!

# z-standardization (mean = 0, SD = 1) and centering (mean = 0)
cheating <- cheating %>% 
  mutate_at(vars(hex_hh:op_Unconventionality), function(x) c(scale(x)))
cheating

# only for continuous covariates (factors need to be effect-coded manually!)
models <- list(
  "em"      = win ~ hex_em | p,
  "anx"     = win ~ em_Anxiety | p
)

# direction of hypotheses 
# ("<"/">" for negative/positive effect; "!=" for undirected test)
order <- list(
  "em"     = ">",
  "anx"    = ">"
)


#########################  Bayesian analysis: compute BF for each predictor

stopifnot(all(names(models) == names(order)))
dat <- summ <- bs <- models
for (i in seq_along(models)){
  print(Sys.time())
  cat("i =", i, " \n")
  dat[[i]] <- cheating_data(models[[i]], cheating,
                            group = "dataset",  # random intercept
                            rscale = rscale, center = FALSE)
  stanfit <- sampling(cheatreg, data = dat[[i]],
                      pars = c("mu", "delta", "sigma", "alpha", "g"),
                      chains = chains, iter = iter, warmup = 2000,
                      control = list(adapt_delta = adapt_delta, max_treedepth = 25L))
  summ[[i]] <- cheating_summary(stanfit, dat[[i]], order = order[[i]], bf = TRUE)
  print(round(summ[[i]], 2))
  # save(stanfit, file = paste0("results/", i, "_", names(models)[i], ".RData"))
  save(summ, file = paste0("results/summary_rscale=", round(rscale, 3), ".RData"))
  rm(stanfit) ; gc()
}

# * N per analysis
# * note: 147 participants in within-subject design in Hilbig & Zettler (2015)
sapply(dat, "[[", "N")  


# Print results
sink(paste0("results/summary_rscale=", round(rscale, 3), ".txt"))
cat("############## Prior rscale =", rscale, "\n\n")
cat("############## iter =", iter, "; chains=", chains, "\n\n")
print(lapply(summ, round, 2))
sink()




######################### Frequentist analysis: p-values of Wald test

# significance level and effect size under H1 to compute approximate power
alpha <- .01
beta_H1 <- log(1.25)  # weak effect: odds ratio = 1.25 (cf. Heck et al., 2018)


# HEXACO Emotionality
fit_em <- RRglmer(win ~ hex_em + (1 | dataset), data = cheating, link = "RRlink.logit", 
                  RRmodel = rep("Forced", nrow(cheating)), 
                  p1 = 1 - p, p2 = rep(.99999999, nrow(cheating)))
coef_em <- summary(fit_em)$coef
power_em <- 1 - pnorm(qnorm(1 - alpha/2), beta_H1 / coef_em["hex_em", 2])
results_em <- cbind(coef_em, "power" = c(NA, power_em))
round(results_em, 3)


# HEXACO: Anxiety facet
fit_anx <- RRglmer(win ~ em_Anxiety + (1 | dataset), data = cheating, link = "RRlink.logit", 
                   RRmodel = rep("Forced", nrow(cheating)), 
                   p1 = 1 - p, p2 = rep(.99999999, nrow(cheating)))
coef_anx <- summary(fit_anx)$coef
power_anx <- 1 - pnorm(qnorm(1 - alpha/2), beta_H1 / coef_anx["em_Anxiety", 2])
results_anx <- cbind(coef_anx, "power" = c(NA, power_anx))
round(results_anx, 3)


# print results
sink("results/summary_pvalues.txt")
cat("############## Significance level alpha =", alpha, "\n\n")
cat("############## H1 effect size: odds ratio =", exp(beta_H1), "\n\n")
print(round(results_em, 3))
cat("\n")
print(round(results_anx, 3))
sink()

