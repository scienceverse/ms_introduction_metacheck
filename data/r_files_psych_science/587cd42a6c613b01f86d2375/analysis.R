#-------------------------------------------------------------------------------
# This R-script reproduces the results and figures reported in
# Scheibehenne, Gronau, Jamil, & Wagenmakers, 2017
#-------------------------------------------------------------------------------

rm(list = ls())

source("bridge_sampling_functions.R")
library(LaplacesDemon)
library(truncnorm)
library(R2jags)

#-------------------------------------------------------------------------------
# functions
#-------------------------------------------------------------------------------

get.samples.fixed.H1 <- function(data, myniter = 600000, mynburnin = 2000,
                                 mynthin = 20) {
  
  samples <- jags(data, inits = NULL, parameters.to.save = "d.fixed",
                  model.file = "model_fixed_H1.txt", n.chains = 3,
                  n.iter = myniter, n.burnin = mynburnin, n.thin = mynthin,
                  DIC = TRUE)
  out <- samples$BUGSoutput$sims.matrix[,"d.fixed", drop = FALSE]
  
  return(out)
  
}

get.samples.fixed.H1.unrestricted <- function(data, myniter = 600000,
                                              mynburnin = 2000, mynthin = 20) {
  
  samples <- jags(data, inits = NULL, parameters.to.save = "d.fixed",
                  model.file = "model_fixed_H1_unrestricted.txt", n.chains = 3,
                  n.iter = myniter, n.burnin = mynburnin, n.thin = mynthin,
                  DIC = TRUE)
  out <- samples$BUGSoutput$sims.matrix[,"d.fixed", drop = FALSE]
  
  return(out)
  
}

get.samples.random.H0 <- function(data, myniter = 600000, mynburnin = 2000,
                                  mynthin = 20) {
  
  samples <- jags(data, inits = NULL, parameters.to.save = c("tau", "d"),
                  model.file = "model_random_H0.txt", n.chains = 3,
                  n.iter = myniter, n.burnin = mynburnin, n.thin = mynthin,
                  DIC = TRUE)
  out <- samples$BUGSoutput$sims.matrix[,c("tau", paste0("d[", seq_len(data$Nstud), "]"))]
  
  return(out)
  
}

get.samples.random.H1 <- function(data, myniter = 600000, mynburnin = 2000,
                                  mynthin = 20) {
  
  samples <- jags(data, inits = NULL, parameters.to.save = c("d.rand", "tau", "d"),
                  model.file = "model_random_H1.txt", n.chains = 3,
                  n.iter = myniter, n.burnin = mynburnin, n.thin = mynthin,
                  DIC = TRUE)
  out <- samples$BUGSoutput$sims.matrix[,c("d.rand", "tau",
                                           paste0("d[", seq_len(data$Nstud), "]"))]
  
  return(out)
  
}

get.samples.random.H1.unrestricted <- function(data, myniter = 600000,
                                               mynburnin = 2000, mynthin = 20) {
  
  samples <- jags(data, inits = NULL, parameters.to.save = c("d.rand", "tau", "d"),
                  model.file = "model_random_H1_unrestricted.txt", n.chains = 3,
                  n.iter = myniter, n.burnin = mynburnin, n.thin = mynthin,
                  DIC = TRUE)
  out <- samples$BUGSoutput$sims.matrix[,c("d.rand", "tau",
                                           paste0("d[", seq_len(data$Nstud), "]"))]
  
  return(out)
  
}

log.posterior.fixed.H1 <- function(s.row, data) {
  
  d.fixed <- s.row[[ "d.fixed" ]]
  
  sum(dnorm(data$y, mean = d.fixed, sd = sqrt(data$V), log = TRUE)) +
    log(dtruncnorm(d.fixed, a = 0, b = Inf, mean = 0, sd = 0.3))
  
}

log.posterior.fixed.H1.unrestricted <- function(s.row, data) {
  
  d.fixed <- s.row[[ "d.fixed" ]]
  
  sum(dnorm(data$y, mean = d.fixed, sd = sqrt(data$V), log = TRUE)) +
    dnorm(d.fixed, mean = 0, sd = 0.3, log = TRUE)
  
}

log.ml.fixed.H0 <- function(data) {
  
  sum(dnorm(data$y, mean = 0, sd = sqrt(data$V), log = TRUE))
  
}

log.posterior.random.H1 <- function(s.row, data) {
  
  d.random <- s.row[[ "d.rand" ]]
  tau <- s.row[[ "tau" ]]
  d <- s.row[ paste0("d[", seq_len(data$Nstud), "]") ]
  
  sum(dnorm(data$y, mean = d, sd = sqrt(data$V), log = TRUE)) +
    sum(dnorm(d, mean = d.random, sd = tau, log = TRUE)) +
    log(dtruncnorm(d.random, a = 0, b = Inf, mean = 0, sd = 0.3)) +
    dhalfcauchy(tau, scale = 1/sqrt(data$prior.scaleTau), log = TRUE)
  
}

log.posterior.random.H1.unrestricted <- function(s.row, data) {
  
  d.random <- s.row[[ "d.rand" ]]
  tau <- s.row[[ "tau" ]]
  d <- s.row[ paste0("d[", seq_len(data$Nstud), "]") ]
  
  sum(dnorm(data$y, mean = d, sd = sqrt(data$V), log = TRUE)) +
    sum(dnorm(d, mean = d.random, sd = tau, log = TRUE)) +
    dnorm(d.random, mean = 0, sd = 0.3, log = TRUE) +
    dhalfcauchy(tau, scale = 1/sqrt(data$prior.scaleTau), log = TRUE)
  
}

log.posterior.random.H0 <- function(s.row, data) {
  
  tau <- s.row[[ "tau" ]]
  d <- s.row[ paste0("d[", seq_len(data$Nstud), "]") ]
  
  sum(dnorm(data$y, mean = d, sd = sqrt(data$V), log = TRUE)) +
    sum(dnorm(d, mean = 0, sd = tau, log = TRUE)) +
    dhalfcauchy(tau, scale = 1/sqrt(data$prior.scaleTau), log = TRUE)
  
}

post_inclusion <- function(logml.fixed.H0, logml.fixed.H1, logml.random.H0,
                           logml.random.H1) {
  
  denom <- sum(exp(c(logml.fixed.H0, logml.fixed.H1,
                     logml.random.H0, logml.random.H1)))
  
  post.fixed.H0 <- exp(logml.fixed.H0)/denom
  post.fixed.H1 <- exp(logml.fixed.H1)/denom
  post.random.H0 <- exp(logml.random.H0)/denom
  post.random.H1 <- exp(logml.random.H1)/denom
  
  # sanity check
  if ( ! all.equal( sum(c(post.fixed.H0, post.fixed.H1,
                          post.random.H0, post.random.H1)), 1))
    stop("Posterior model probabilities do not sum to one!")
  
  post.incl <- post.fixed.H1 + post.random.H1
  
  return(list(post.incl = post.incl,
              post.fixed.H0 = post.fixed.H0, post.fixed.H1 = post.fixed.H1,
              post.random.H0 = post.random.H0, post.random.H1 = post.random.H1))
  
}

#-------------------------------------------------------------------------------
# load and re-code raw data
#-------------------------------------------------------------------------------

rawData <- read.csv2("towelData.csv")

exList <- unique(rawData$Source) # list with unique experiments

# sort data into nSuccess and totalN
data <- NULL
for(i in 1:length(exList)) {
  
  thisEx <- rawData[rawData$Source == exList[i],]
  
  data <- rbind(data,c(yesControl = thisEx[1,"Count"],
                       nControl  = sum(thisEx[c(1,3), "Count"]),
                       yesSocial = thisEx[2,"Count"],
                       nSocial   = sum(thisEx[c(2,4), "Count"])))
  
  rownames(data)[i] <- exList[i]
}      


# calculate log OR
oddsSocial <- data[,"yesSocial"]/(data[,"nSocial"] - data[,"yesSocial"])
oddsControl <- data[,"yesControl"]/(data[,"nControl"] - data[,"yesControl"])
or <- oddsSocial/oddsControl
logOR <- log(or)
data <- cbind(data, logOR = logOR)

# calculate variance of logOR
v <-  1/data[,"yesSocial"] +
  1/data[,"yesControl"] +
  1/(data[,"nSocial"] - data[,"yesSocial"])+
  1/(data[,"nControl"] - data[,"yesControl"])

data <- cbind(data,v)

# values for tau prior
sdVec <- c(1/64, 1/32, 1/16, 1/8, 1/4, 1/2, 1, 10, 100)
scaleTauVec <- 1/sdVec^2

#-------------------------------------------------------------------------------
# fixed effect model
#-------------------------------------------------------------------------------

set.seed(1234)

data_fixed <- list(y = data[,"logOR"], V = data[,"v"], Nstud = dim(data)[1])

# fixed MA model: H0
logml.fixed.H0 <- log.ml.fixed.H0(data_fixed)

# fixed MA model: H1
post.samples <- get.samples.fixed.H1(data_fixed)
lb <- 0
ub <- Inf
names(lb) <- names(ub) <- "d.fixed"
bs.fixed.H1 <- bridge.sampler(post.samples = post.samples,
                              log.posterior = log.posterior.fixed.H1,
                              data = data_fixed, lb = lb, ub = ub)
logml.fixed.H1 <- bs.fixed.H1$logml

# fixed MA model: H1 (unrestricted)
post.samples <- get.samples.fixed.H1.unrestricted(data_fixed)
lb <- -Inf
ub <- Inf
names(lb) <- names(ub) <- "d.fixed"
bs.fixed.H1.unrestricted <- bridge.sampler(post.samples = post.samples,
                                           log.posterior = log.posterior.fixed.H1.unrestricted,
                                           data = data_fixed, lb = lb, ub = ub)
logml.fixed.H1.unrestricted <- bs.fixed.H1.unrestricted$logml

# posterior logOR (fixed effect model)
logOR.fixed <- get.samples.fixed.H1.unrestricted(data_fixed)
save(logOR.fixed, file = "logOR.fixed.Rdata")


#-------------------------------------------------------------------------------
# random effects model
#-------------------------------------------------------------------------------

results <- vector("list", length(scaleTauVec))
logOR.rand <- NULL

for (i in seq_along(results)) {
  
  data_random <- list(y = data[,"logOR"], V = data[,"v"], Nstud = dim(data)[1],
                      prior.scaleTau = scaleTauVec[i])
  
  # random MA model: H0
  post.samples <- get.samples.random.H0(data_random)
  lb <- rep(-Inf, ncol(post.samples))
  names(lb) <- colnames(post.samples)
  lb[[ "tau" ]] <- 0
  ub <- rep(Inf, ncol(post.samples))
  names(ub) <- colnames(post.samples)
  bs.random.H0 <- bridge.sampler(post.samples = post.samples,
                                 log.posterior = log.posterior.random.H0,
                                 data = data_random, lb = lb, ub = ub)
  logml.random.H0 <- bs.random.H0$logml
  
  # random MA model: H1
  post.samples <- get.samples.random.H1(data_random)
  lb <- rep(-Inf, ncol(post.samples))
  names(lb) <- colnames(post.samples)
  lb[[ "d.rand" ]] <- 0
  lb[[ "tau" ]] <- 0
  ub <- rep(Inf, ncol(post.samples))
  names(ub) <- colnames(post.samples)
  bs.random.H1 <- bridge.sampler(post.samples = post.samples,
                                 log.posterior = log.posterior.random.H1,
                                 data = data_random, lb = lb, ub = ub)
  logml.random.H1 <- bs.random.H1$logml
  
  # random MA model: H1 (unrestricted)
  post.samples <- get.samples.random.H1.unrestricted(data_random)
  lb <- rep(-Inf, ncol(post.samples))
  names(lb) <- colnames(post.samples)
  lb[[ "tau" ]] <- 0
  ub <- rep(Inf, ncol(post.samples))
  names(ub) <- colnames(post.samples)
  bs.random.H1.unrestricted <- bridge.sampler(post.samples = post.samples,
                                              log.posterior = log.posterior.random.H1.unrestricted,
                                              data = data_random, lb = lb, ub = ub)
  logml.random.H1.unrestricted <- bs.random.H1.unrestricted$logml
  
  # inclusion probability (model-averaged posterior probability of effect)
  post.incl <- post_inclusion(logml.fixed.H0 = logml.fixed.H0,
                              logml.fixed.H1 = logml.fixed.H1,
                              logml.random.H0 = logml.random.H0,
                              logml.random.H1 = logml.random.H1)
  
  # model-averaged Bayes factor in favor of effect
  BFincl <- post.incl$post.incl/(1 - post.incl$post.incl)
  
  results[[i]] <- list(bs.fixed.H1 = bs.fixed.H1,
                       bs.fixed.H1.unrestricted = bs.fixed.H1.unrestricted,
                       bs.random.H1 = bs.random.H1,
                       bs.random.H1.unrestricted = bs.random.H1.unrestricted,
                       bs.random.H0 = bs.random.H0,
                       logml.fixed.H1 = logml.fixed.H1,
                       logml.fixed.H1.unrestricted = logml.fixed.H1.unrestricted,
                       logml.fixed.H0 = logml.fixed.H0,
                       logml.random.H1 = logml.random.H1,
                       logml.random.H1.unrestricted = logml.random.H1.unrestricted,
                       logml.random.H0 = logml.random.H0,
                       BF_fixed_random = exp(logml.fixed.H1 - logml.random.H1),
                       BF_fixed_random_unres = exp(logml.fixed.H1.unrestricted -
                                                   logml.random.H1.unrestricted),
                       BF10_fixed = exp(logml.fixed.H1.unrestricted - logml.fixed.H0),
                       BF10_random = exp(logml.random.H1.unrestricted - logml.random.H0),
                       BFplus0_fixed = exp(logml.fixed.H1 - logml.fixed.H0),
                       BFplus0_random = exp(logml.random.H1 - logml.random.H0),
                       post.incl = post.incl, BFincl = BFincl)
  
  save(results, file = "results.Rdata")
  
  # posterior logOR (random effects model)
  logOR.rand.tmp <- get.samples.random.H1.unrestricted(data_random)[,"d.rand"]
  logOR.rand <- rbind(logOR.rand, logOR.rand.tmp)
  save(logOR.rand, file = "logOR.rand.Rdata")
  
}

# calculate posterior of log OR
m.fixed <- mean(logOR.fixed)
m.rand <- apply(logOR.rand, 1, mean)
hpd95.fixed <- HPDinterval(as.mcmc(logOR.fixed))
hpd95.rand <- apply(logOR.rand, 1, function(x) HPDinterval(as.mcmc(x)))

# calculate weighted posterior of log OR by combining
# mcmc samples from fixed and rand proportional to BF_fixed_random_unres
BF_fixed_random_unres <- sapply(results, function(x) x$BF_fixed_random_unres)
targetLength <- 80000

m.mixed <- hpd95.mixed <- NULL
for(i in 1:length(BF_fixed_random_unres)){
  nFixed <- round(targetLength*BF_fixed_random_unres[i]/(BF_fixed_random_unres[i] + 1))
  nRand <- targetLength - nFixed
  mixedVec <- c(sample(logOR.fixed, size = nFixed, replace = TRUE),
                sample(logOR.rand[i,], size = nRand, replace = TRUE))
  
  m.mixed <- c(m.mixed, mean(mixedVec))
  hpd95.mixed <- cbind(hpd95.mixed, t(HPDinterval(as.mcmc(mixedVec))))
}


#-------------------------------------------------------------------------------
# plot results
#-------------------------------------------------------------------------------

BF_fixed_random <- sapply(results, function(x) x$BF_fixed_random)
BFplus0_random <- sapply(results, function(x) x$BFplus0_random)
BFplus0_fixed <- sapply(results, function(x) x$BFplus0_fixed)
BFplus0_random <- sapply(results, function(x) x$BFplus0_random)
BFincl <- sapply(results, function(x) x$BFincl)

# plot BF 
windows(5.5, 9)
layout(matrix(1:2, nrow = 2))
par(mar = c(5.1, 4.1, 1, 1.1))

pchVec <- c(23, 24, 21) # fixed, random, weighted
colVec <- c("white", "white", "lightgrey")

yrange <- range(log(BF_fixed_random))*1.1
xpos <- 1:length(BF_fixed_random)
plot(xpos,log(BF_fixed_random), ylim = yrange, ylab="Bayes Factor",
     type = "l", lwd = 2, bty = "l",
     xlab = expression(paste("Scale (SD) of Half Cauchy Prior on ", tau)),
     xaxt = "n", yaxt = "n")
axis(1, at = 1:length(sdVec), labels = round(sdVec, 2))
BFlab <- c(1, 3, 10, 30, 100, 300, 1000)
axis(2, at = log(BFlab), labels = BFlab, las = 1)
abline(h = c(0, log(3)), lty = "dashed", col = "black")
BFlabel <- round(BF_fixed_random, 1)

# add BF points
gap <- .15
points(xpos, log(BF_fixed_random), cex = 1.3, pch = 22, bg = "black")

lines(xpos - gap ,log(BFplus0_fixed), lwd = 2)
points(xpos - gap,log(BFplus0_fixed), pch = pchVec[1], bg = colVec[1], cex = 1.3)

lines(xpos + gap,log(BFplus0_random), lwd = 2)
points(xpos + gap, log(BFplus0_random), pch = pchVec[2], bg = colVec[2], cex = 1.1)

lines(xpos, log(BFincl), lwd = 2)
points(xpos, log(BFincl), pch = pchVec[3], bg = colVec[3], cex = 1.3)


legend(x = xpos[1], y = yrange[2], pch = c(22, pchVec), pt.bg = c("black", colVec),
       ncol = 1, pt.cex = 1.3, xjust = 0, yjust = 1, lwd = 2, bty = "n",
       legend = c("Fixed over random effect model", 
                "H1 over H0 (fixed effect model)", 
                "H1 over H0 (random effect model)", 
                "H1 over H0 (weighted)"))

# plot posterior log-odds ratio for both models and the model average
par(mar = c(5.1, 4.1, 1, 1.1))
gap <- .2
xpos <- 1:length(sdVec)
ylim <- 1.4*range(cbind(hpd95.fixed[1,], hpd95.rand))
plot(NULL, xlim = c(min(xpos) - gap, max(xpos) + gap), ylab = "Log Odds Ratio",
     xlab = expression(paste("Scale (SD) of Half Cauchy Prior on ", tau)), 
     xaxt = "n", ylim = ylim, bty = "l", las = 1)
axis(1, at = xpos, labels = round(sdVec, 2))
arrows(xpos +gap, hpd95.rand[1,], xpos + gap,hpd95.rand[2,], code = 3, length = .03, angle = 90)
arrows(xpos - gap, hpd95.fixed[1], xpos - gap,hpd95.fixed[2], code = 3, length = .03, angle = 90)
arrows(xpos, hpd95.mixed[1,], xpos, hpd95.mixed[2,], code = 3, length = .03, angle = 90)

points(xpos - gap, rep(m.fixed, length(xpos)), pch = pchVec[1], bg = colVec[1], cex = 1.3)
points(xpos + gap, m.rand, pch = pchVec[2], bg = colVec[2], cex = 1.1)
points(xpos, m.mixed, pch = pchVec[3], bg = colVec[3], cex = 1.3)

legend(x = mean(xpos) + .05, y = ylim[2]*.8, pch = pchVec, pt.bg = colVec,
       legend = c("fixed effect", "random effect", "weighted effect"),
       ncol = 3, pt.cex = 1.3, xjust = .5, cex = .9, yjust = 0, bty = "n")
abline(h = 0, lty = "dashed", col = "black")

savePlot("Figure1.eps", type = "eps")
savePlot("Figure1.jpeg", type = "jpg")

