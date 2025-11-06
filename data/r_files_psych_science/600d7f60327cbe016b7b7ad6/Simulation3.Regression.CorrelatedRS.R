### Simulation 3 of Oberauer, "The Importance of Random Slopes in Bayesian Mixed Models"
### Simulation of BF for within-subjects effect in a 2-predictor regression model
### Testing the importance of including correlations between random slopes

rm(list = ls())
graphics.off()
library(brms)

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

nRuns <- 40    # required number of replications before data are analyzed
analysis <- F

N = 20                      # subject sample size
SD = c(0.10, 0.25, 0.5, 1)  # between-subjects SD of effect
Correlation = c(0, 0.2, 0.5, 0.7)  # true correlation between random effects of IV1 and IV2
sd = 1      # within-subjects sd (across trials)
nrep <- 1  #number of replications
neffect <- 2  #number of different effect sizes of continuous IV
nlevels <- 5  # number of levels of effects of continuous IV
nlevels2 <- 5  # number of levels of effects of a potential second continuous IV
efactor <- 0.25 #multiplier for the levels of effect of continuous IV
effectsize2 <- 0.25  # effect size of second IV
Ntrials <- nlevels*30  #total number of trials
zX <- (c(1:nlevels) - mean(c(1:nlevels)))/sd(c(1:nlevels))  # z-standardized continuous predictor

BFfile <- "Simulation.Regr.RandomSlopes.2.RData"
existBF = file.access(BFfile, mode=0)


if (existBF==0) {
  load(BFfile)
  idx2 <- dim(BF)[1] + 1
  BFnew <- as.data.frame(matrix(NA, neffect*length(SD)*length(Correlation)*nrep, 8))
  names(BFnew) <- c("TrueEffect", "BetweenSD", "rep", "correlation", "sampleeffect0", 
                 "BF.noRS.0", "BF.RS.0", "BF.RScorr.0")
  BF <- rbind(BF, BFnew)
  if (dim(BF)[1]/32 == nRuns) analysis <- T
} else {
  BF <- as.data.frame(matrix(NA, neffect*length(SD)*length(Correlation)*nrep, 8))
  names(BF) <- c("TrueEffect", "BetweenSD", "rep", "correlation", "sampleeffect0", 
                 "BF.noRS.0", "BF.RS.0", "BF.RScorr.0")
  Rhat <- as.data.frame(matrix(NA, neffect*length(SD)*length(Correlation)*nrep, 10))
  names(Rhat) <- c("TrueEffect", "BetweenSD", "rep", "correlation", 
                   "Rhat.add", "Rhat.noX", "Rhat.RS.add", "Rhat.RS.noX", "Rhat.RScorr.add", "Rhat.RScorr.noX" )
  idx2 <- 1
}

options (mc.cores=parallel::detectCores()) # Run on multiple cores
nIter <- 10000
# Set the priors
fixefPrior <- set_prior("cauchy(0,0.5)", class="b")
# Cauchy priors on effect sizes: 0.5 for continuous predictors, analogous to contrast-coded nominal predictors
ranefPrior <- set_prior("gamma(1,0.01)", class="sd")  
# moderately uninformative prior on standard deviation of random effects

M0add <- list()
M0base <- list()
M1add <- list()
M1base <- list()
M2add <- list()
M2base <- list()
idx <- 1  # this is the index for the lists, which are always re-initialized above

for (rep in 1:nrep) {
  
  for (eff in 0:(neffect-1)) {
    
    effectsize1 <- eff*efactor
    
    for (c in 1:length(Correlation)) {
      
      correlation <- Correlation[c]
      
      for (bsd in 1:length(SD)) {
        
        ntrials <- round(Ntrials/(nlevels))  #number of trials per cell
        m <- rep(rnorm(N, mean=0, sd=1), each=Ntrials)  #individual-subject mean

        zX2 <- sample(zX) # shuffle in random order
        IV <- rep(rep(zX, each=ntrials), times=N)
        IV2 <- rep(rep(zX2, each=ntrials), times=N)
        error = rnorm(N*Ntrials, mean = 0, sd=1)
        b1 <- rnorm(N, mean = effectsize1, sd = SD[bsd])
        b2 <- correlation*b1 + sqrt(1-correlation^2)*rnorm(N, mean = effectsize2, sd = SD[bsd])
        beta <- rep(b1, each=Ntrials) #individual-subject beta
        beta2 <- rep(b2, each=Ntrials) #individual-subject beta
        DV <- m + beta*IV + beta2*IV2 + error
        
        D0 <- data.frame(subj = rep(1:N, each = Ntrials),
                         trial = rep(1:Ntrials, times=N),  
                         iv = IV, 
                         iv2 = IV2,
                         dv = DV)
        
        D0$subj <- as.factor(D0$subj)
        
        sampleeffect0 <- 0
        for (level in 1:nlevels) sampleeffect0 <- sampleeffect0 + zX[level] * mean(D0$dv[D0$iv==zX[level]])
        
        # without random slopes
        M0.add <- brm(dv ~ iv + iv2 + (1|subj), 
                      data = D0, prior = c(fixefPrior, ranefPrior),
                      iter = nIter, warmup = 1000, chains = 3, save_all_pars=T)
        M0.noX <- brm(dv ~ iv2 + (1|subj), 
                      data = D0, prior = c(fixefPrior, ranefPrior),
                      iter = nIter, warmup = 1000, chains = 3, save_all_pars=T)
        
        
        # with random slopes
        M1.RS.add <- brm(dv ~ iv + iv2 + (1 + iv + iv2 || subj), # double || sets correlations to 0 
                         data = D0, prior = c(fixefPrior, ranefPrior),
                         iter = nIter, warmup = 1000, chains = 3, save_all_pars=T)
        M1.RS.noX <- brm(dv ~ iv2 + (1 + iv + iv2 || subj), # double || sets correlations to 0 
                         data = D0, prior = c(fixefPrior, ranefPrior),
                         iter = nIter, warmup = 1000, chains = 3, save_all_pars=T)
        
        
        # with random slopes and correlations
        M2.RScorr.add <- brm(dv ~ iv + iv2 + (1 + iv + iv2 | subj), # single | leaves correlations free
                             data = D0, prior = c(fixefPrior, ranefPrior),
                             iter = nIter, warmup = 1000, chains = 3, save_all_pars=T)
        M2.RScorr.noX <- brm(dv ~ iv2 + (1 + iv + iv2 | subj), 
                             data = D0, prior = c(fixefPrior, ranefPrior),
                             iter = nIter, warmup = 1000, chains = 3, save_all_pars=T)
        
        M0add[[idx]] <- M0.add
        M0base[[idx]] <- M0.noX
        M1add[[idx]] <- M1.RS.add
        M1base[[idx]] <- M1.RS.noX
        M2add[[idx]] <- M2.RScorr.add
        M2base[[idx]] <- M2.RScorr.noX
        
        Rhat[idx,] = c(eff*efactor, SD[bsd], rep, correlation, 
                       mean(summary(M0.add)$fixed[,"Rhat"]), mean(summary(M0.noX)$fixed[,"Rhat"]), 
                       mean(summary(M1.RS.add)$fixed[,"Rhat"]), mean(summary(M1.RS.noX)$fixed[,"Rhat"]), 
                       mean(summary(M2.RScorr.add)$fixed[,"Rhat"]), mean(summary(M2.RScorr.noX)$fixed[,"Rhat"]))
        print(Rhat[idx,])
        idx <- idx + 1
        
        rm(M0.RS.add)
        rm(M0.RS.noX)
        rm(M1.RS.add)
        rm(M1.RS.noX)
        rm(M2.RScorr.add)
        rm(M2.RScorr.noX)
        
      }
    }
  }
}


idx <- 1
for (rep in 1:nrep) {
  for (eff in 0:(neffect-1)) {
    effectsize1 <- eff*efactor
    for (c in 1:length(Correlation)) {
      correlation <- Correlation[c]
      for (bsd in 1:length(SD)) {
        ntrials <- round(Ntrials/(nlevels))  #number of trials per cell

        M0.add <- M0add[[idx]]
        M0.noX <- M0base[[idx]]
        M1.RS.add <- M1add[[idx]]
        M1.RS.noX <- M1base[[idx]]
        M2.RScorr.add <- M2add[[idx]]
        M2.RScorr.noX <- M2base[[idx]]
        
        BF0.X <- bayes_factor(M0.add, M0.noX)
        BF1.RS.X <- bayes_factor(M1.RS.add, M1.RS.noX)
        BF2.RScorr.X <- bayes_factor(M2.RScorr.add, M2.RScorr.noX)     
        
        BF[idx2, ] <- c(eff*efactor, SD[bsd], rep, correlation, sampleeffect0, 
                       log10(as.numeric(BF0.X)[1]), log10(as.numeric(BF1.RS.X)[1]), log10(as.numeric(BF2.RScorr.X)[1]))
        save(BF, Rhat, file=BFfile)
        print(BF[idx2,])
        idx <- idx + 1
        idx2 <- idx2 + 1
      }
    }
  }
}


if (analysis) {
  
  BFfile <- "Simulation.Regr.RandomSlopes.2.RData"
  load(BFfile)
  plotoption <- 1  # 0: x11(), 1: tiff
  
  ######## Analysis of BF for True Effect = 0 ##################################################
  
  BF0 <- subset(BF, TrueEffect==0)
  
  FA <- as.data.frame(matrix(0, 3, 2))
  rownames(FA) <- c("NoRS.0", "RS.0", "RS.corr.0")
  colnames(FA) <- c("BF > 3", "BF > 10")
  
  # mild false alarms
  FA[1, 1] <- mean(BF0$BF.noRS.0 > log10(3))
  FA[2, 1] <- mean(BF0$BF.RS.0 > log10(3))
  FA[3, 1] <- mean(BF0$BF.RScorr.0 > log10(3))
  
  # serious false alarms (BF > 10)
  FA[1, 2] <- mean(BF0$BF.noRS.0 > 1)
  FA[2, 2] <- mean(BF0$BF.RS.0 > 1)
  FA[3, 2] <- mean(BF0$BF.RScorr.0 > 1)
  
  BFcols <- c("BF.noRS.0", "BF.RS.0", "BF.RScorr.0")
  BFlabels <- c("noRS", "RS", "RS+Corr")
  ymax <- max(BF0[,BFcols])
  ymin <- min(BF0[,BFcols])
  ymax <- 15   # align with plots for ANVOA2
  
  colors <- c("black", "red", "blue", "green")
  
  # Plot log10(BF) as a function of condition and SD of random slopes
  
  if (plotoption==0) x11(width=12, height=8)
  if (plotoption==1) tiff(paste0("./plots/Regression2.BF0.tiff"), units="in", width=12, height=8, res=500)
  plot(1:3, colMeans(BF0[, BFcols]), xlab="", ylab="log10(BF)", xlim=c(0.5, 3.5), ylim=c(ymin, ymax), 
       axes=F, main="True Effect = 0")
  box()
  axis(side=1, at=1:3, labels=BFlabels)
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(SD) - (mean(1:length(SD))))/(1.3*length(SD))
  for (sd in 1:length(SD)) {
    bf0 <- subset(BF0, BetweenSD == SD[sd])
    nObs <- dim(bf0)[1]
    for (cond in 1:3) points(jitter(rep(cond+dev[sd], nObs), amount=0.08), bf0[, BFcols[cond]], col=colors[sd])
  }
  legend(3.5, ymax, SD, pch=1, col = colors, title=bquote(sigma), yjust=1, xjust=1)
  if (plotoption == 1) dev.off()
   
  # Plot log10(BF) as a function of condition and size of correlation
  
  if (plotoption==0) x11(width=12, height=8)
  if (plotoption==1) tiff(paste0("./plots/Regression2.byCorr.BF0.tiff"), units="in", width=12, height=8, res=500)
  plot(1:3, colMeans(BF0[, BFcols]), xlab="", ylab="log10(BF)", xlim=c(0.5, 3.5), ylim=c(ymin, ymax), 
       axes=F, main="True Effect = 0")
  box()
  axis(side=1, at=1:3, labels=BFlabels)
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(Correlation) - (mean(1:length(Correlation))))/(1.3*length(Correlation))
  for (n2 in 1:length(Correlation)) {
    corr <- Correlation[n2]
    bf0 <- subset(BF0, correlation == corr)
    nObs <- dim(bf0)[1]
    for (cond in 1:3) points(jitter(rep(cond+dev[n2], nObs), amount=0.08), bf0[, BFcols[cond]], col=colors[n2])
  }
  legend(3.5, ymax, Correlation, pch=1, col = colors, title="Correlation", yjust=1, xjust=1)
  if (plotoption==1) dev.off()
  
  
  
  ######## Analysis of BF for True Effect > 0 ##################################################
  
  BFtrue <- subset(BF, TrueEffect>0)
  
  Misses <- as.data.frame(matrix(0, 3, 3))
  rownames(Misses) <- c("NoRS.0", "RS.0", "RScorr.0")
  colnames(Misses) <- c("BF < 3", "BF < 1", "BF < 0.33")
  
  # misses (BF < 3)
  Misses[1, 1] <- mean(BFtrue$BF.noRS.0 < log10(3))
  Misses[2, 1] <- mean(BFtrue$BF.RS.0 < log10(3))
  Misses[3, 1] <- mean(BFtrue$BF.RScorr.0 < log10(3))
  
  # mild false negatives (BF < 1)
  Misses[1, 2] <- mean(BFtrue$BF.noRS.0 < 0)
  Misses[2, 2] <- mean(BFtrue$BF.RS.0 < 0)
  Misses[3, 2] <- mean(BFtrue$BF.RScorr.0 < 0)

  # serious false negatives (BF < 1/3)
  Misses[1, 3] <- mean(BFtrue$BF.noRS.0 < log10(1/3))
  Misses[2, 3] <- mean(BFtrue$BF.RS.0 < log10(1/3))
  Misses[3, 3] <- mean(BFtrue$BF.RScorr.0 < log10(1/3))
  
    
  BFcols <- c("BF.noRS.0", "BF.RS.0", "BF.RScorr.0")
  #ymax <- max(BFtrue[,BFcols])
  ymax <- 5
  ymin <- min(BFtrue[,BFcols])
  colors <- c("black", "red", "blue", "green")
  
  # Plot log10(BF) as a function of condition and SD of random slopes
  
  if (plotoption==0) x11(width=12, height=8)
  if (plotoption==1) tiff(paste0("./plots/Regression2.BF1.tiff"), units="in", width=12, height=8, res=500)
  
  plot(1:3, colMeans(BFtrue[, BFcols]), xlab="", ylab="log10(BF)", xlim=c(0.5, 3.5), ylim=c(ymin, ymax), axes=F)
  box()
  axis(side=1, at=1:3, labels=BFlabels)
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(SD) - (mean(1:length(SD))))/(1.3*length(SD))
  for (sd in 1:length(SD)) {
    bftrue <- subset(BFtrue, BetweenSD == SD[sd])
    nObs <- dim(bftrue)[1]
    for (cond in 1:3) points(jitter(rep(cond+dev[sd], nObs), amount=0.08), bftrue[, BFcols[cond]], col=colors[sd])
  }
  legend(0.5, ymin, SD, pch=1, col = colors, title=bquote(sigma), yjust=0, xjust=0)
  if (plotoption==1) dev.off()
  
  # Plot log10(BF) as a function of condition and size of correlation
  
  if (plotoption==0) x11(width=12, height=8)
  if (plotoption==1) tiff(paste0("./plots/Regression2.byCorr.BF1.tiff"), units="in", width=12, height=8, res=500)
  
  plot(1:3, colMeans(BFtrue[, BFcols]), xlab="", ylab="log10(BF)", xlim=c(0.5, 3.5), ylim=c(ymin, ymax), axes=F)
  box()
  axis(side=1, at=1:3, labels=BFlabels)
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(Correlation) - (mean(1:length(Correlation))))/(1.3*length(Correlation))
  for (n2 in 1:length(Correlation)) {
    corr <- Correlation[n2]
    bftrue <- subset(BFtrue, correlation == corr)
    nObs <- dim(bftrue)[1]
    for (cond in 1:3) points(jitter(rep(cond+dev[n2], nObs), amount=0.08), bftrue[, BFcols[cond]], col=colors[n2])
  }
  legend(0.5, ymin, Correlation, pch=1, col = colors, title="Correlation", yjust=0, xjust=0)
  if (plotoption==1) dev.off()
  
} 