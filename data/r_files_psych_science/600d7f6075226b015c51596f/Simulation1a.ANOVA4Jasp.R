### Simulation reported in Oberauer, "The Importance of Random Slopes in Bayesian Mixed Models"
### Simulates data from (reduced version of) Simulation 1 for analysis with Jasp

rm(list = ls())
graphics.off()
library("tidyr")

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

N = 20    # subject sample size
SD = 0.5  # between-subjects SD of effect (i.e., the true SD of the "random slopes")
sd = 1      # within-subjects sd (across trials, so this is trial-by-trial noise)
nrep <- 100  #number of replications of the simulation
nlevels2 <- 6  # number of levels of IV 2
contrast1 <- c(-0.5, 0.5)  # 1st IV always has 2 levels
contrast2 <- rep(contrast1, ceiling(nlevels2/2))  # alternating +0.5 and -0.5
ntrials <- 30   # number of trials per cell
Ntrials <- 2*nlevels2 * ntrials  #total number of trials
effectsize1 <- 0                 # effect size of IV 1
effectsize2 <- 0.5               # effect size of IV 2


for (rep in 1:nrep) {
  
  # Simulate means for each subject in each design cell
  m <- rnorm(N, 0, SD)
  effect1 <- rnorm(N, effectsize1, SD)  # effects of individual subjects
  effect2 <- rnorm(N, effectsize2, SD)  # effects of individual subjects
  cellmeans <- array(0, dim=c(2, nlevels2, N))
  for (iv1 in 1:2) {
    for (iv2 in 1:nlevels2) {
      cellmeans[iv1, iv2, ] <- m + contrast1[iv1]*effect1 + contrast2[iv2]*effect2  # means for each subject in each design cell
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
        dv <- c(dv, rnorm(ntrials, cellmeans[iv1, iv2, id], sd))  # data on individual trials: cell mean plus noise
        condition1 <- c(condition1, rep(iv1, ntrials))
        condition2 <- c(condition2, rep(iv2, ntrials))
        subj <- c(subj, rep(id, ntrials))
        trial <- c(trial, 1:ntrials)
      }
    }
  }
  
  # Put together data frame - needs to be in wide format for ANOVA in Jasp
  
  D0 <- data.frame(subj, trial, condition1, condition2, dv)
  Dagg <- aggregate(dv ~ subj+condition2+condition1, data=D0, FUN=mean)
  Dwide <- Dagg %>% pivot_wider(values_from="dv", names_from=c("condition1", "condition2"))
  names(Dwide) <- c("subj", paste0("cond1.", 1:nlevels2), paste0("cond2.", 1:nlevels2))  

  
  write.table(Dwide, file=paste0("./simdata/Anova", rep, ".txt"), row.names=F)
  
}

BF <- read.table("./simdata/BF.Jasp.txt", header=T)
mean(BF$BF10>3)
mean(BF$BF10>10)

