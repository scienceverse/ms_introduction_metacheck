# Multilevel Bayesian Meta-analysis

require(brms)
load("meta_data.Rda")

# code by paul
int_prior1 <- set_prior("normal(0, .3)")
int_prior2 <- set_prior("normal(0, .6)")
tau_prior1 <- set_prior("cauchy(0, 0.03125)", class = "sd")
tau_prior2 <- set_prior("cauchy(0, 0.0625)", class = "sd")
tau_prior3 <- set_prior("cauchy(0, 0.125)", class = "sd")
tau_prior4 <- set_prior("cauchy(0, 0.25)", class = "sd")
tau_prior5 <- set_prior("cauchy(0, 0.5)", class = "sd")
tau_prior6 <- set_prior("cauchy(0, 1)", class = "sd")
priors <- list(c(int_prior1),
               c(int_prior1, tau_prior1),
               c(int_prior1, tau_prior2),
               c(int_prior1, tau_prior3),
               c(int_prior1, tau_prior4),
               c(int_prior1, tau_prior5),
               c(int_prior1, tau_prior6),
               c(int_prior1),
               c(int_prior2),
               c(int_prior2, tau_prior1),
               c(int_prior2, tau_prior2),
               c(int_prior2, tau_prior3),
               c(int_prior2, tau_prior4),
               c(int_prior2, tau_prior5),
               c(int_prior2, tau_prior6),
               c(int_prior2))

dat <- data.frame(tau_prior = rep(c(0, 0.03, 0.06, 0.13, 0.25, 0.5, 1, "Flat"), 2),
                  int_prior = rep(c("N(0, 0.3)", "N(0, 0.6)"), each = 8),
                  estimate = NA, l_95 = NA, u_95 = NA, bf = NA)

base_args <- list(data = metadata, sample_prior = TRUE, iter = 20000, 
                  warmup = 3000, control = list(adapt_delta = .99), seed = 1234)
for (i in 1:nrow(dat)) {
  print(dat[i, c("tau_prior", "int_prior")])
  args <- c(list(prior = priors[[i]]), base_args)
  if (dat$tau_prior[i] == "0") {
    # fixed effects model
    args$formula <- yi | se(sqrt(vi)) ~ 0 + intercept
  } else {
    # random effects model
    args$formula <- yi | se(sqrt(vi)) ~ 0 + intercept + (1|id)
  }
  capture.output(fit <- do.call(brm, args))
  hyp <- hypothesis(fit, "intercept = 0")
  dat[i, c("estimate", "l_95", "u_95")] <- 
    hyp$hypothesis[1, c("Estimate", "l-95% CI", "u-95% CI")]
  dat$bf[i] <- 1 / (hyp$hypothesis$Evid.Ratio)
}
save(dat, file = "plot_data.Rda")
