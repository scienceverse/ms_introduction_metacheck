##################################################################
### Who lies? A large-scale reanalysis linking basic           ###
### personality traits to unethical decision making            ###
###                                                            ###
### Heck, Thielmann, Moshagen, & Hilbig (2018).                ###
### contact: heck@uni-mannheim.de                              ###
###                                                            ###
### This script provides the core analysis functions.          ###
##################################################################


library("tidyverse")
library("bridgesampling")
library("LaplacesDemon")
library("logspline")
library("Formula")
library("rstan")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


######## compile stan models
# - check working directory. models must be in the folder "/models/xxx.stan"
# - these are models for nested data
# - use "cheating.stan" and "cheating_null.stan" for the i.i.d. versions

cheatreg      <- stan_model("models/cheating_nested.stan")
cheatreg_null <- stan_model("models/cheating_nested_null.stan")


######## prepare a list with data structures for analysis with Stan
cheating_data <- function(formula, data, rscale = 1/2, 
                          group = "dataset", center = FALSE){
  
  mf <- model.frame(Formula(formula), data)
  
  # to extract baseline probability p
  p_label <- attr(terms(Formula(formula), rhs = 2), "term.labels")
  
  # extract regression formula
  reg <- formula(Formula(formula), rhs = 1)
  win <- model.response(mf)
  X <- model.matrix(update(reg, .~ . -1), data = data)

  # Bayesian model requires centered covariates!
  if (center){
    X <- scale(X, scale = FALSE)
    attr(X, "scaled:center") <- NULL
  }

  dat.list <- list(N = nrow(X), p = mf[,p_label], win = win, rscale = rscale)
  if(ncol(X) > 0){
    dat.list$P <- ncol(X)
    dat.list$X <- X
    dat.list$V <- solve(t(X) %*% X / nrow(X))  # inverse of covariance matrix for scaled JZS prior
  }
  
  if (!missing(group) && !is.null(group)){
    data_subset <- na.omit(data[,c(names(mf), group)])
    dat.list$group <- as.integer(factor(data_subset[[group]]))
    dat.list$G <- length(unique(dat.list$group))
    if(nrow(data_subset) != nrow(mf))
      stop("group indicator missing for some observations")
  }
  dat.list
}

######## nice summary (with or without Bayes factor)
cheating_summary <- function(fit, data, bf = TRUE, order = rep("!=", data$P)){
  
  summ <- summary(fit, pars = c("mu", "alpha"), 
                  probs = c(0.025, 0.975))$summary[,1:5]
  rownames(summ) <- c("(Intercept)", colnames(data$X))
  if (bf) 
    bf <- cheating_bf(fit, data, order)
  else 
    bf <- NULL
  cbind(summ[,-2, drop = FALSE], bf)
}


######## compute Bayes factor
# A) with bridge sampling
# B) with generalized Savage-Dickey ratio (normal approximation; better for large BF)
# C) with generalized Savage-Dickey ratio (logspline approx.; better for small BF)
#
# order: direction of test (either: "<" , ">" , "!=")
cheating_bf <- function(fit, data, order = rep("!=", data$P)){
  
  # array to store BFs
  bfs <- cbind(bridge_b10 = rep(NA, data$P + 1), bridge_b01 = NA, 
               sdr_b10 = NA, sdr_b01 = NA,
               sdr_spline_b10 = NA, sdr_spline_b01 = NA)
  
  # marginal likelihood for full model
  bs1 <- bridge_sampler(fit, method = "warp3", silent = TRUE, use_neff = FALSE)
  
  alpha <- rstan::extract(fit, "alpha")[[1]]
  stan_args <- fit@stan_args[[1]]
  par_nested <- fit@model_pars[fit@model_pars %in% c("delta", "sigma")]
  
  bf_order <- rep(1, data$P)
  cat("Computing Bayes factors for", data$P, "predictors: ")
  for (i in seq.int(data$P)){
    cat(i, ", ")
    gc()  # garbage collection (reduce memory load)
    
    if (data$P > 1){
      ######## fit nested model without predictor i
      data0 <- data
      data0$P <- data$P - 1
      data0$X <- data$X[, -i, drop = FALSE]
      data0$V <- solve(t(data0$X) %*% data0$X / data$N)
      fit0 <- sampling(cheatreg, data = data0, chains = length(fit@stan_args), 
                       pars = c("mu", "alpha", "g", par_nested),
                       iter = stan_args$iter, warmup = stan_args$warmup,
                       control = stan_args$control)
      
      ######## BF with method by Verdinelli & Wasserman (1995)
      # Note: naive Savage-Dickey Ratio gives wrong BF estimates!
      
      # prior under constrained model:
      psi0 <- rstan::extract(fit0, "alpha")[[1]]
      S0 <- data0$rscale^2 * as.positive.definite(data0$V)
      p0 <- dmvc(x = psi0, mu = 0, S = S0, log = TRUE)
      
      # prior under full model conditional on the constrait: alpha[i] == 0
      if (i == 1){
        psi0_ext <- cbind(0, psi0)
      } else if (i == (ncol(psi0) + 1)){
        psi0_ext <- cbind(psi0, 0)
      } else {
        psi0_ext <- cbind(psi0[,1:(i-1)], 0, psi0[,i:ncol(psi0)])
      }
      S1 <- as.positive.definite(data$rscale^2 * data$V)
      p1 <- dmvc(x = psi0_ext, mu = rep(0, data$P), S = S1, log = TRUE)
      C <- mean(exp(p0 - p1))
      
      dpost_ext_spline <- dlogspline(0, logspline(alpha[,i])) # nonparametric estimate
      dpost_ext <- dnorm(0, mean(alpha[,i]), sd(alpha[,i]))
      hist(alpha[,i], 100, freq = FALSE, col="lightblue",
           main = paste0("Predictor ", i, " - ", colnames(data$X)[i]))
      curve(dnorm(x, mean(alpha[,i]), sd(alpha[,i])), add = TRUE)
      
      bf_sdr <- c(bf_10 = 1/(dpost_ext * C), bf_01 = dpost_ext * C)
      bf_sdr_spline <- c(bf_10 = 1/(dpost_ext_spline * C), 
                         bf_01 = dpost_ext_spline * C)
      
    } else { # special case: BF for one predictor (HH-->cheating)
      
      ######## fit nested model without predictor
      data0 <- data
      data0$P <- data0$X <- data0$V <- data0$rscale <- NULL
      fit0 <- sampling(cheatreg_null, data = data0, chains = length(fit@stan_args), 
                       pars = c("mu", par_nested), 
                       iter = stan_args$iter, warmup = stan_args$warmup,
                       control = stan_args$control)
      
      ######## BF with simple Savage-Dickey Ratio: ok for one predictor
      s <- data$rscale * sqrt(data$V[1,1])  # predictor is scaled
      dprior <- dcauchy(0, 0, scale = s)
      dpost_spline <- dlogspline(0, logspline(alpha))  # nonparametric estimate: if posterior is nonnormal/skewed
      dpost <- dnorm(0, mean(alpha), sd(alpha))
      # hist(alpha, 100, freq = FALSE, main = paste0("Predictor ", i, " - ", colnames(data$X)))
      # curve(dnorm(x, mean(alpha), sd(alpha)), add = TRUE, col = 2)
      # plot(logspline(alpha), add = TRUE, col = 2)
      bf_sdr <- c(b10 = dprior/dpost, b01 = dpost/dprior)
      bf_sdr_spline <- c(b10 = dprior/dpost_spline, b01 = dpost_spline/dprior)
    }
    
    ######## test of order constraint
    if (order[i] == "<"){
      bf_order[i] <- mean(alpha[,i] < 0) * 2
    } else if(order[i] == ">"){
      bf_order[i] <- mean(alpha[,i] > 0) * 2
    }
    
    
    ######## BF with bridge sampling
    bf_bs <- c(NA, NA)
    try({
      bs0 <- bridge_sampler(fit0, method = "warp3", silent = TRUE, use_neff = FALSE)
      bf_bs <- c(b10 = bayes_factor(bs1, bs0)$bf, 
                 b01 = bayes_factor(bs0, bs1)$bf)
    })
    
    bfs[i+1,] <- 
      c(bf_bs, bf_sdr, bf_sdr_spline) *   # = two-sided BF
      c(bf_order[i], 1/bf_order[i])       # required to get two-sided BF
  }
  cat(" Finished.\n")
  cbind(order = c(NA, ifelse(order == "<", -1, ifelse(order == ">", +1, 0))), bfs)
}


#####  generate simulated data sets
# n: number of observations
# or: true odds ratio (for z-standardized predictor)
# d: mean probability of dishonest behavior
# p: probability of obtaining the favorable outcome
cheating_sim <- function(n, odds_ratio, d_mean, p){
  
  x <- rnorm(n)
  beta <- log(odds_ratio)
  d <- plogis(qlogis(d_mean) + beta * x)
  p_yes <- d + (1-d) * p
  win <- rbinom(n, 1, p_yes)
  df <- data.frame(win = win, x = x, p = p)
  #### check:
  # fit <- RRreg::RRlog(win ~ x, df, "FR", c(0,p))
  # plogis(fit$coefficients[1])
  # exp(fit$coefficients[2])
  df
}