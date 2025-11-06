### Simulation-Based Calibration of BF for within-subjects regression model, testing for support for RS in a first step and including them if and only if they are supported
## Variant in which the model parameters are sampled from the priors

rm(list = ls())
graphics.off()
library(BayesFactor)
library(LaplacesDemon)# for inverse Chi-square distribution

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

N = 20      # subject sample size
nrep <- 1000  #number of replications
contrast1 <- c(-sqrt(2)/2, sqrt(2)/2)  # 1st IV always has 2 levels; contrasts coded as in BayesFactor package
contrast2 <- c(-sqrt(2)/2, sqrt(2)/2) # 2nd IV, too
Ntrials <- 120  #total number of trials
nlevels2 <- 2 # levels of second IV
SD0 <- 1      # SD of random intercept
priorM0 <- 0.5  # prior for the null model
Threshold.BF.RS <- 1  # threshold for BF for random slopes for the decision to keep or omit them

BFfile <- "Simulation.ANOVA.RandomSlopes.Calibration2.RData"
existBF = file.access(BFfile, mode=0)

if (existBF==0) {
  load(BFfile)
  idx <- dim(BF)[1] + 1  
} else {
  BF <- as.data.frame(matrix(NA, nrep, 12))
  names(BF) <- c("rep", "model", "ES1", "ES2", "SD1", "SD2", "Sigma", "sampleeffect0", 
                 "log-BF4RS", "log-BF.noRS", "log-BF.RS", "log-BF.bestRS")
  idx <- 1
}

ES1 <- rcauchy(nrep,0,scale=1/2)  # default rscaleFixed is "medium", which is scale = 1/2
ES2 <- rcauchy(nrep,0,scale=1/2)  # default rscaleFixed is "medium", which is scale = 1/2

SD1 <- rgamma(nrep, 2, 10)   # a plausible distribution of random-slope SDs, with mean = 0.2, median ~.166, for which BF is sometimes in favor of random slopes and sometimes against
SD2 <- rgamma(nrep, 2, 10)
Sigma <- rep(1, nrep)        # SD of trial-by-trial noise (residual)

model <- runif(nrep) > priorM0   # 0 = null model, 1 = alternative model

for (rep in 1:nrep) {
  
  effectsize1 <- ES1[rep] * model[rep]
  effectsize2 <- ES2[rep]
  
  ntrials <- round(Ntrials/(2*nlevels2))  #number of trials per cell
  
  # Simulate means for each subject in each design cell
  m <- rnorm(N, 0, SD0)
  effect1 <- rnorm(N, effectsize1, SD1[rep])  # effects of individual subjects
  effect2 <- rnorm(N, effectsize2, SD2[rep])  # effects of individual subjects
  cellmeans <- array(0, dim=c(2, nlevels2, N))
  for (iv1 in 1:2) {
    for (iv2 in 1:nlevels2) {
      cellmeans[iv1, iv2, ] <- m + Sigma[rep]*contrast1[iv1]*effect1 + Sigma[rep]*contrast2[iv2]*effect2  # means for each subject in each design cell
    }
  }
  
  # Simulate the data of each trial
  dv <- NULL
  subj <- NULL
  condition1 <- NULL
  condition2 <- NULL
  trial <- NULL
  for (id in 1:N) {
    for (iv1 in 1:2) {
      for (iv2 in 1:nlevels2) {
        dv <- c(dv, rnorm(ntrials, cellmeans[iv1, iv2, id], Sigma[rep]))  # data on individual trials
        condition1 <- c(condition1, rep(iv1, ntrials))
        condition2 <- c(condition2, rep(iv2, ntrials))
        subj <- c(subj, rep(id, ntrials))
        trial <- c(trial, 1:ntrials)
      }
    }
  }
  
  # Put together data frame
  
  D0 <- data.frame(subj, trial, condition1, condition2, dv)
  
  D0$subj <- as.factor(D0$subj)
  D0$condition1 <- as.factor(D0$condition1)
  D0$condition2 <- as.factor(D0$condition2)
  sampleeffect0 <- mean(D0$dv[D0$condition1==2] - D0$dv[D0$condition1==1])/
    sd(D0$dv[D0$condition1==2] - D0$dv[D0$condition1==1])
  
  # without random slopes
  
  M0.add <- lmBF(dv ~ condition1 + condition2 + subj, whichRandom="subj", data=D0, iterations=100000)
  M0.nocond1 <- lmBF(dv ~ condition2 + subj, whichRandom="subj", data=D0, iterations=100000)
  BF0.cond1 <- M0.add/M0.nocond1
  
  # with random slopes
  
  M0.RS.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
  M0.RS.nocond1 <- lmBF(dv ~ condition2 + subj + condition1:subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
  BF0.RS.cond1 <- M0.RS.add/M0.RS.nocond1
  
  # evidence for random slopes
  
  BF0.RS <- M0.RS.add/M0.add
  if (as.vector(BF0.RS) > Threshold.BF.RS) BF0.bestRS.cond1 <- BF0.RS.cond1
  if (as.vector(BF0.RS) < Threshold.BF.RS) BF0.bestRS.cond1 <- BF0.cond1
  
  BF[idx, ] <- c(rep, model[rep], ES1[rep], ES2[rep], SD1[rep], SD2[rep], Sigma[rep], sampleeffect0, 
                 log10(as.vector(BF0.RS)), log10(as.vector(BF0.cond1)), log10(as.vector(BF0.RS.cond1)), log10(as.vector(BF0.bestRS.cond1)))
  
  save(BF, file=BFfile)
  print(BF[idx,])
  idx <- idx + 1
  
}


load(BFfile)

priorModelOdds <- 1

# from Schad et al. (2021): 
# Obtain 95% confidence intervals from a logistic linear model
# and transform confidence intervals into probabilities

pH1 <- matrix(NA, 3, 3)  # 3 rows for the 3 versions of RS-structure, 3 columns for lower-CI, mean, and higher-CI
BFname <- c("log-BF.noRS", "log-BF.RS", "log-BF.bestRS")
rownames(pH1) <- BFname
colnames(pH1) <- c("CI 2.5%", "Mean", "CI 97.5%")

aggBF <- list()

for (rs in 1:3) {
  
  bfdata <- BF[, c("rep", "model", BFname[rs])]   # create a reduced data frame for this particular BF version
  names(bfdata)[3] <- "BF"

  postModelOdds <- 10^bfdata[, "BF"] * priorModelOdds
  bfdata$postProbH1 <- postModelOdds/(postModelOdds+1)
  bfdata$postProbH0 <- 1/(postModelOdds+1)
  
  BM <- glm(bfdata$postProbH1~1,family="binomial")
  CIs <- 1/(1+exp(-confint(BM)))
  ME <- as.numeric(1/(1+exp(-coef(BM))))
  pH1[rs,]=round(c(CI=CIs[1], mean=ME, CI=CIs[2]),2)

  aggBF[[rs]] <- aggregate(cbind(postProbH0, postProbH1) ~ model, data=bfdata, FUN=mean)

}

print(pH1)
print(aggBF)

### How often was RS included, as a function of true SD of RS?

BF$RSincluded <- BF[, "log-BF4RS"] > 1
table(BF$RSincluded)

x11()
plot(BF$SD1, BF[, "log-BF4RS"], type="p")

## Proportion of serious FA (BF > 10)

BF$BF.lt.10.noRS <- BF[, "log-BF.noRS"] > 1
BF$BF.lt.10.RS <- BF[, "log-BF.RS"] > 1
BF$BF.lt.10.bestRS <- BF[, "log-BF.bestRS"] > 1

table(BF$model, BF$BF.lt.10.noRS)
table(BF$model, BF$BF.lt.10.RS)
table(BF$model, BF$BF.lt.10.bestRS)

