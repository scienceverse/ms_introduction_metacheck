# This script uses some of the R and JAGS code
# that was provided by Scheibehenne et al. in their rejoinder
# See osf.io/hjt65 to download the necessary files
source("bridge_sampling_functions.R")
source("jags_functions.R")

sim_studies <- function(nstudies, mu, tau, v) {
  true_eff <- rnorm(nstudies, mu, tau)
  # use bootstrapping to sample sampling variances
  v <- sample(v, nstudies, replace = TRUE)
  y <- rnorm(nstudies, true_eff, sqrt(v))
  data.frame(y, v)
}

sim_models <- function(i, conds, metadata, nsim) {
  require(LaplacesDemon)
  require(truncnorm)
  require(R2jags)
  require(metafor)
  require(mvtnorm)
  require(Brobdingnag)
  message("Condition: ", i)
  results <- vector("list", nsim)
  for (n in seq_len(nsim)) {
    # simulate data
    sim_data <- sim_studies(
      conds$nstudies[i], conds$mu[i], conds$tau[i], metadata$vi
    )
    # fixed effects models
    data_fixed <- list(
      y = sim_data$y, V = sim_data$v, Nstud = dim(sim_data)[1]
    )
    # fixed MA model: H0
    logml.fixed.H0 <- log.ml.fixed.H0(data_fixed)
    # fixed MA model: H1 (unrestricted)
    post.samples.fixed <- get.samples.fixed.H1.unrestricted(data_fixed)
    lb <- -Inf
    ub <- Inf
    names(lb) <- names(ub) <- "d.fixed"
    bs.fixed.H1.unrestricted <- bridge.sampler(
      post.samples = post.samples.fixed,
      log.posterior = log.posterior.fixed.H1.unrestricted,
      data = data_fixed, lb = lb, ub = ub
    )
    logml.fixed.H1.unrestricted <- bs.fixed.H1.unrestricted$logml
    # random effects models
    data_random <- list(
      y = sim_data$y, V = sim_data$v, 
      Nstud = dim(sim_data)[1],
      prior.scaleTau = conds$scaleTau[i]
    )
    # random MA model: H0
    post.samples.random.H0 <- get.samples.random.H0(data_random)
    lb <- rep(-Inf, ncol(post.samples.random.H0))
    names(lb) <- colnames(post.samples.random.H0)
    lb[[ "tau" ]] <- 0
    ub <- rep(Inf, ncol(post.samples.random.H0))
    names(ub) <- colnames(post.samples.random.H0)
    bs.random.H0 <- bridge.sampler(
      post.samples = post.samples.random.H0,
      log.posterior = log.posterior.random.H0,
      data = data_random, lb = lb, ub = ub
    )
    logml.random.H0 <- bs.random.H0$logml
    # random MA model: H1 (unrestricted)
    post.samples.random <- get.samples.random.H1.unrestricted(data_random)
    lb <- rep(-Inf, ncol(post.samples.random))
    names(lb) <- colnames(post.samples.random)
    lb[[ "tau" ]] <- 0
    ub <- rep(Inf, ncol(post.samples.random))
    names(ub) <- colnames(post.samples.random)
    bs.random.H1.unrestricted <- bridge.sampler(
      post.samples = post.samples.random,
      log.posterior = log.posterior.random.H1.unrestricted,
      data = data_random, lb = lb, ub = ub
    )
    logml.random.H1.unrestricted <- bs.random.H1.unrestricted$logml
    
    # frequentist analysis
    rma_fit <- metafor::rma(y ~ 1, vi = v, data = sim_data)
    rma_fit_summary <- summary(rma_fit)
    
    # save results
    results[[n]] <- list(
      logOR_fixed = mean(post.samples.fixed[, "d.fixed"]),
      logOR_fixed_lower = quantile(post.samples.fixed[, "d.fixed"], 0.025),
      logOR_fixed_upper = quantile(post.samples.fixed[, "d.fixed"], 0.975),
      logOR_random = mean(post.samples.random[, "d.rand"]),
      logOR_random_lower = quantile(post.samples.random[, "d.rand"], 0.025),
      logOR_random_upper = quantile(post.samples.random[, "d.rand"], 0.975),
      tau = mean(post.samples.random[, "tau"]),
      tau_lower = quantile(post.samples.random[, "tau"], 0.025),
      tau_upper = quantile(post.samples.random[, "tau"], 0.975),
      BF_fixed_random = exp(
        logml.fixed.H1.unrestricted - logml.random.H1.unrestricted
      ),
      BF10_fixed = exp(logml.fixed.H1.unrestricted - logml.fixed.H0),
      BF10_random = exp(logml.random.H1.unrestricted - logml.random.H0),
      logOR_rma = rma_fit_summary$b[1, 1],
      logOR_rma_lower = rma_fit_summary$ci.lb[1],
      logOR_rma_upper = rma_fit_summary$ci.ub[1],
      p10_rma = rma_fit_summary$pval[1],
      tau_rma = sqrt(rma_fit_summary$tau2),
      Q_rma = rma_fit_summary$QE,
      Qp_rma = rma_fit_summary$QEp
    )
  }
  results <- do.call(rbind.data.frame, results)
  conds$results[[i]] <- rbind(conds$results[[i]], results)
  conds[i, , drop = FALSE]
  # save after every condition in order not to loose any trials
  # save(conds, file = "sim_results.Rda")
}

# -------- simulations -----------
load("meta_data.Rda")

# set to FALSE to extend existing simulation results
new_sim <- FALSE
if (new_sim) {
  conds <- expand.grid(
    mu = 0.25, 
    tau = c(0, 0.05, 0.1, 0.20, 0.3, 0.4),
    nstudies = c(7, 14, 28),
    sdTau = c(1/64, 1/32, 1/16, 1/8, 1/4, 1/2, 1, 10, 100)
  )
  conds$scaleTau <- 1 / conds$sdTau^2
  conds$results <- vector("list", nrow(conds))
} else {
  conds <- load("sim_results.Rda")
}

nsim <- 1

library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
conds_new <- foreach(i = 1:2, .combine = rbind) %dopar% {
  sim_models(i, conds = conds, metadata = metadata, nsim = nsim) 
}

