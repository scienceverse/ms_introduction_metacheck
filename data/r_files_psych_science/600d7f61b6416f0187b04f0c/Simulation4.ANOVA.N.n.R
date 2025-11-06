### Simulation 4 of Oberauer, "The Importance of Random Slopes in Bayesian Mixed Models"
### Simulation of BF for within-subjects effect with unaggregated and aggregated data, with a 2 x 2 ANOVA design
### Varies the number of subjects (N) and the number of trials per subject (n)

rm(list = ls())
graphics.off()
library(BayesFactor)

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

Nsubj = c(20, 30, 40, 50)                      # subject sample size
Ntrials <- c(80, 120, 160, 240, 360)  #total number of trials

SD = 0.5  # between-subjects SD of effect
sd = 1      # within-subjects sd (across trials)
nrep <- 50  #number of replications
neffect <- 4  #number of levels of effect of 1st IV (1st level = 0)
efactor <- 0.25 #multiplier for the levels of effect of 1st IV
contrast1 <- c(-0.5, 0.5)  # 1st IV always has 2 levels
contrast2 <- c(-0.5, 0.5)  # 2nd IV, too
nlevels2 <- 2 # levels of second IV
Effectsize2 <- 2 # multiplied with efactor
BFfile <- "Simulation.ANOVA.RandomSlopes.3.RData"
existBF = file.access(BFfile, mode=0)

if (existBF==0) {
  load(BFfile)
  idx <- dim(BF)[1] + 1  
} else {
  BF <- as.data.frame(matrix(NA, neffect*length(SD)*nrep, 25))
  names(BF) <- c("TrueEffect", "BetweenSD", "N", "ntrials", "rep", "sampleeffect0", "sampleeffect1", 
                 "BF4RS1.0", "BF4RS1.1",
                 "BF4RS2.0", "BF4RS2.1",
                 "BF4RSboth.0", "BF4RSboth.1",
                 "BF4RSnone.0", "BF4RSnone.1",
                 "BF.noRS.0", "BF.noRS.1", 
                 "BF.RS.0", "BF.RS.1", 
                 "BF.RS1.0", "BF.RS1.1", 
                 "BF.RS2.0", "BF.RS2.1", 
                 "BF.eRS.0", "BF.eRS.1")
  idx <- 1
}


for (eff in 0:(neffect-1)) {
  
  effectsize1 <- eff*efactor
  effectsize2 <- Effectsize2*efactor                    #effect size of second factor
  
  for (nsubj in 1:length(Nsubj)) {
    for (nt in 1:length(Ntrials)) {
      
      N <- Nsubj[nsubj]
      ntrials <- round(Ntrials[nt]/(2*nlevels2))  #number of trials per cell
      
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
              dv <- c(dv, rnorm(ntrials, cellmeans[iv1, iv2, id], sd))  # data on individual trials
              condition1 <- c(condition1, rep(iv1, ntrials))
              condition2 <- c(condition2, rep(iv2, ntrials))
              subj <- c(subj, rep(id, ntrials))
              trial <- c(trial, 1:ntrials)
            }
          }
        }
        
        # Put together data frames on 2 levels of aggregation
        
        D0 <- data.frame(subj, trial, condition1, condition2, dv)
        
        D0$subj <- as.factor(D0$subj)
        D0$condition1 <- as.factor(D0$condition1)
        D0$condition2 <- as.factor(D0$condition2)
        sampleeffect0 <- mean(D0$dv[D0$condition1==2] - D0$dv[D0$condition1==1])/
          sd(D0$dv[D0$condition1==2] - D0$dv[D0$condition1==1])
        
        Dagg1 <- aggregate(dv ~ subj + condition1 + condition2, data=D0, FUN=mean)  # aggregate across trials
        sampleeffect1 <- mean(Dagg1$dv[Dagg1$condition1==2] - Dagg1$dv[Dagg1$condition1==1])/
          sd(Dagg1$dv[Dagg1$condition1==2] - Dagg1$dv[Dagg1$condition1==1])
        
        # without random slopes
        
        M0.add <- lmBF(dv ~ condition1 + condition2 + subj, whichRandom="subj", data=D0, iterations=100000)
        M0.nocond1 <- lmBF(dv ~ condition2 + subj, whichRandom="subj", data=D0, iterations=100000)
        BF0.cond1 <- M0.add/M0.nocond1
        
        M1.add <- lmBF(dv ~ condition1 + condition2 + subj, whichRandom="subj", data=Dagg1, iterations=100000)
        M1.nocond1 <- lmBF(dv ~ condition2 + subj, whichRandom="subj", data=Dagg1, iterations=100000)
        BF1.cond1 <- M1.add/M1.nocond1
        
        # with random slopes
        
        M0.RS.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
        M0.RS.nocond1 <- lmBF(dv ~ condition2 + subj + condition1:subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
        BF0.RS.cond1 <- M0.RS.add/M0.RS.nocond1
        
        M1.RS.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj + condition2:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        M1.RS.nocond1 <- lmBF(dv ~ condition2 + subj + condition1:subj + condition2:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        BF1.RS.cond1 <- M1.RS.add/M1.RS.nocond1
        
        # with random slope of IV1 only, or IV2 only
        
        M0.RS1.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj, whichRandom="subj", data=D0, iterations=100000)
        M0.RS2.add <- lmBF(dv ~ condition1 + condition2 + subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
        M1.RS1.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        M1.RS2.add <- lmBF(dv ~ condition1 + condition2 + subj + condition2:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        
        
        # evidence for random slopes: find the best-supported RS structure
        
        BF0.rs1 <- M0.RS.add/M0.RS2.add
        BF0.rs2 <- M0.RS.add/M0.RS1.add
        BF0.rs12 <- M0.RS.add/max(M0.RS1.add, M0.RS2.add) # evidence for both RS
        BF0.rs0 <- M0.add/max(M0.RS1.add, M0.RS2.add, M0.RS.add)
        BF0.rs <- c(as.vector(BF0.rs1), as.vector(BF0.rs2), as.vector(BF0.rs12), as.vector(BF0.rs0))
        bestRS0 <- which.max(BF0.rs)
        
        BF1.rs1 <- M1.RS.add/M1.RS2.add
        BF1.rs2 <- M1.RS.add/M1.RS1.add
        BF1.rs12 <- M1.RS.add/max(M1.RS1.add, M1.RS2.add) # evidence for both RS
        BF1.rs0 <- M1.add/max(M1.RS1.add, M1.RS2.add, M1.RS.add)
        BF1.rs <- c(as.vector(BF1.rs1), as.vector(BF1.rs2), as.vector(BF1.rs12), as.vector(BF1.rs0))
        bestRS1 <- which.max(BF1.rs)
        
        # compute BF for IV1 with best RS structure
        
        M0.RS1.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj, whichRandom="subj", data=D0, iterations=100000)
        M0.RS1.nocond1 <- lmBF(dv ~ condition2 + subj + condition1:subj, whichRandom="subj", data=D0, iterations=100000)
        BF0.RS1.cond1 <- M0.RS1.add/M0.RS1.nocond1
        
        M0.RS2.add <- lmBF(dv ~ condition1 + condition2 + subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
        M0.RS2.nocond1 <- lmBF(dv ~ condition2 + subj + condition2:subj, whichRandom="subj", data=D0, iterations=100000)
        BF0.RS2.cond1 <- M0.RS2.add/M0.RS2.nocond1
        
        if (bestRS0 == 1) BF0.bestRS.cond1 <- BF0.RS1.cond1
        if (bestRS0 == 2) BF0.bestRS.cond1 <- BF0.RS2.cond1
        if (bestRS0 == 3) BF0.bestRS.cond1 <- BF0.RS.cond1
        if (bestRS0 == 4) BF0.bestRS.cond1 <- BF0.cond1
        
        
        M1.RS1.add <- lmBF(dv ~ condition1 + condition2 + subj + condition1:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        M1.RS1.nocond1 <- lmBF(dv ~ condition2 + subj + condition1:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        BF1.RS1.cond1 <- M1.RS1.add/M1.RS1.nocond1
        
        M1.RS2.add <- lmBF(dv ~ condition1 + condition2 + subj + condition2:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        M1.RS2.nocond1 <- lmBF(dv ~ condition2 + subj + condition2:subj, whichRandom="subj", data=Dagg1, iterations=100000)
        BF1.RS2.cond1 <- M1.RS2.add/M1.RS2.nocond1
        
        if (bestRS1 == 1) BF1.bestRS.cond1 <- BF1.RS1.cond1
        if (bestRS1 == 2) BF1.bestRS.cond1 <- BF1.RS2.cond1
        if (bestRS1 == 3) BF1.bestRS.cond1 <- BF1.RS.cond1
        if (bestRS1 == 4) BF1.bestRS.cond1 <- BF1.cond1
        
        
        BF[idx, ] <- c(eff*efactor, SD, N, ntrials, rep, sampleeffect0, sampleeffect1, 
                       log10(as.vector(BF0.rs1)), log10(as.vector(BF1.rs1)),
                       log10(as.vector(BF0.rs2)), log10(as.vector(BF1.rs2)),
                       log10(as.vector(BF0.rs12)), log10(as.vector(BF1.rs12)),
                       log10(as.vector(BF0.rs0)), log10(as.vector(BF1.rs0)),
                       log10(as.vector(BF0.cond1)), log10(as.vector(BF1.cond1)), 
                       log10(as.vector(BF0.RS.cond1)), log10(as.vector(BF1.RS.cond1)),
                       log10(as.vector(BF0.RS1.cond1)), log10(as.vector(BF1.RS1.cond1)),
                       log10(as.vector(BF0.RS2.cond1)), log10(as.vector(BF1.RS2.cond1)),
                       log10(as.vector(BF0.bestRS.cond1)), log10(as.vector(BF1.bestRS.cond1)) )
        
        save(BF, file=BFfile)
        print(BF[idx,])
        idx <- idx + 1
        
      }
      
    }
  }
  
}


BFfile <- "Simulation.ANOVA.RandomSlopes.3.RData"
load(BFfile)
plotoption <- 1  # 0: x11(), 1: tiff

######## Analysis of BF for True Effect = 0 ##################################################

BF0 <- subset(BF, TrueEffect==0)
FA <- as.data.frame(matrix(0, 5, 4))
colnames(FA) <- c("Agg0, BF>3", "Agg0, BF>10", "Agg1, BF>3", "Agg1, BF>10")
rownames(FA) <- c("noRS", "RS1", "RS2", "RS", "RS(e)")

# mild false alarms
FA[1, 1] <- mean(BF0$BF.noRS.0 > log10(3))
FA[2, 1] <- mean(BF0$BF.RS1.0 > log10(3))
FA[3, 1] <- mean(BF0$BF.RS2.0 > log10(3))
FA[4, 1] <- mean(BF0$BF.RS.0 > log10(3))
FA[5, 1] <- mean(BF0$BF.eRS.0 > log10(3))

FA[1, 3] <- mean(BF0$BF.noRS.1 > log10(3))
FA[2, 3] <- mean(BF0$BF.RS1.1 > log10(3))
FA[3, 3] <- mean(BF0$BF.RS2.1 > log10(3))
FA[4, 3] <- mean(BF0$BF.RS.1 > log10(3))
FA[5, 3] <- mean(BF0$BF.eRS.1 > log10(3))

# serious false alarms (BF > 10)
FA[1, 2] <- mean(BF0$BF.noRS.0 > 1)
FA[2, 2] <- mean(BF0$BF.RS1.0 > 1)
FA[3, 2] <- mean(BF0$BF.RS2.0 > 1)
FA[4, 2] <- mean(BF0$BF.RS.0 > 1)
FA[5, 2] <- mean(BF0$BF.eRS.0 > 1)

FA[1, 4] <- mean(BF0$BF.noRS.1 > 1)
FA[2, 4] <- mean(BF0$BF.RS1.1 > 1)
FA[3, 4] <- mean(BF0$BF.RS2.1 > 1)
FA[4, 4] <- mean(BF0$BF.RS.1 > 1)
FA[5, 4] <- mean(BF0$BF.eRS.1 > 1)

BFcols <- c("BF.noRS.0", "BF.RS1.0", "BF.RS2.0", "BF.RS.0", "BF.noRS.1", "BF.RS1.1", "BF.RS2.1", "BF.RS.1")
ymax <- rep(0,2)
ymax[1] <- max(BF0[,BFcols])
ymax[2] <- max(BF0[,BFcols])

ymax <- rep(15, 2)  # to align with plots of ANOVA 2

ymin <- min(BF0[,BFcols])
colors <- c("black", "red", "blue", "green", "orange")

# Plot log10(BF) as a function of aggregation level, model version, and N

BFlabels <- c("noRS", "RS1", "RS2", "RS", "noRS", "RS1", "RS2", "RS")
if (plotoption==0) x11(width=12, height=8)
if (plotoption==1) tiff(paste0("./plots/ANOVA3.BF0.by.N.tiff"), units="in", width=12, height=8, res=500)
layout(matrix(1:2,1,2))
for (agg in 0:1) {
  plot(1:4, colMeans(BF0[, BFcols[c(1:4)+4*agg]]), xlab="", ylab="log10(BF)", 
       xlim=c(0.5, 4.5), ylim=c(ymin, ymax[agg+1]), axes=F, main=paste0("True Effect = 0, Aggreg. = ", agg))
  box()
  axis(side=1, at=1:4, labels=BFlabels[c(1:4)+4*agg])
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(Nsubj) - (mean(1:length(Nsubj))))/(1.3*length(Nsubj))
  for (nsubj in 1:length(Nsubj)) {
    bf0 <- subset(BF0, N == Nsubj[nsubj])
    nObs <- dim(bf0)[1]
    for (cond in c(1:4)) points(jitter(rep(cond+dev[nsubj], nObs), amount=0.08), bf0[, BFcols[cond+4*agg]], col=colors[nsubj])
  }
  legend(4.5, ymax[agg+1], Nsubj, pch=1, col = colors, title="N", yjust=1, xjust=1)
}
if (plotoption==1) dev.off()


# Plot log10(BF) as a function of aggregation level, model version, and n(trials)

BFlabels <- c("noRS", "RS1", "RS2", "RS", "noRS", "RS1", "RS2", "RS")
if (plotoption==0) x11(width=12, height=8)
if (plotoption==1) tiff(paste0("./plots/ANOVA3.BF0.by.ntrials.tiff"), units="in", width=12, height=8, res=500)
layout(matrix(1:2,1,2))
for (agg in 0:1) {
  plot(1:4, colMeans(BF0[, BFcols[c(1:4)+4*agg]]), xlab="", ylab="log10(BF)", 
       xlim=c(0.5, 4.5), ylim=c(ymin, ymax[agg+1]), axes=F, main=paste0("True Effect = 0, Aggreg. = ", agg))
  box()
  axis(side=1, at=1:4, labels=BFlabels[c(1:4)+4*agg])
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(Ntrials) - (mean(1:length(Ntrials))))/(1.3*length(Ntrials))
  for (nt in 1:length(Ntrials)) {
    bf0 <- subset(BF0, ntrials == Ntrials[nt]/4)
    nObs <- dim(bf0)[1]
    for (cond in c(1:4)) points(jitter(rep(cond+dev[nt], nObs), amount=0.08), bf0[, BFcols[cond+4*agg]], col=colors[nt])
  }
  legend(4.5, ymax[agg+1], Ntrials, pch=1, col = colors, title="N(trials)", yjust=1, xjust=1)
}
if (plotoption==1) dev.off()




# Plot BF as a function of sample effect size 

ymax[1] <- max(BF0[,BFcols[1:4]])
ymax[2] <- max(BF0[,BFcols[5:8]])
if (plotoption==0) x11(width=12, height=6)
if (plotoption==1) tiff(paste0("./plots/ANOVA3.BF0.SampleEffectSize.tiff"), units="in", width=12, height=6, res=500)
layout(matrix(1:2,1,2))
# No aggregation
plot(BF0$sampleeffect0, BF0$BF.noRS.0, xlim=c(-0.3, 0.3), ylim=c(ymin, ymax[1]), xlab="Sample Effect Size", ylab="log10(BF)", main="Aggreg = 0")
points(BF0$sampleeffect0, BF0$BF.RS.0, xlim=c(-0.3, 0.3), col="red", ylim=c(ymin, ymax[1]), xlab="Sample Effect Size", ylab="log10(BF)")
legend(0, ymax[1], c("noRS", "RS"), col=c("black", "red"), pch=21)
# Aggregation
plot(BF0$sampleeffect1, BF0$BF.noRS.1, xlim=c(-0.3, 0.3), ylim=c(ymin, ymax[2]), xlab="Sample Effect Size", ylab="log10(BF)", main="Aggreg = 1")
points(BF0$sampleeffect1, BF0$BF.RS.1, xlim=c(-0.3, 0.3), col="red", ylim=c(ymin, ymax[2]), xlab="Sample Effect Size", ylab="log10(BF)")
legend(0, ymax[2], c("noRS", "RS"), col=c("black", "red"), pch=21)
if (plotoption==1) dev.off()



######## Analysis of BF for True Effect > 0 ##################################################

BFtrue <- subset(BF, TrueEffect>0)

Misses <- as.data.frame(matrix(0, 5, 6))
colnames(Misses) <- c("Agg0, BF<3", "Agg0, BF<1", "Agg0, BF<0.33", "Agg1, BF<3", "Agg1, BF<1", "Agg1, BF<0.33")
rownames(Misses) <- c("noRS", "RS1", "RS2", "RS", "RS(e)")

# very mild misses (BF < 3)
Misses[1, 1] <- mean(BFtrue$BF.noRS.0 < log10(3))
Misses[2, 1] <- mean(BFtrue$BF.RS1.0 < log10(3))
Misses[3, 1] <- mean(BFtrue$BF.RS2.0 < log10(3))
Misses[4, 1] <- mean(BFtrue$BF.RS.0 < log10(3))
Misses[5, 1] <- mean(BFtrue$BF.eRS.0 < log10(3))

Misses[1, 4] <- mean(BFtrue$BF.noRS.1 < log10(3))
Misses[2, 4] <- mean(BFtrue$BF.RS1.1 < log10(3))
Misses[3, 4] <- mean(BFtrue$BF.RS2.1 < log10(3))
Misses[4, 4] <- mean(BFtrue$BF.RS.1 < log10(3))
Misses[5, 4] <- mean(BFtrue$BF.eRS.1 < log10(3))

# mild misses (BF < 1)
Misses[1, 2] <- mean(BFtrue$BF.noRS.0 < 0)
Misses[2, 2] <- mean(BFtrue$BF.RS1.0 < 0)
Misses[3, 2] <- mean(BFtrue$BF.RS2.0 < 0)
Misses[4, 2] <- mean(BFtrue$BF.RS.0 < 0)
Misses[5, 2] <- mean(BFtrue$BF.eRS.0 < 0)

Misses[1, 5] <- mean(BFtrue$BF.noRS.1 < 0)
Misses[2, 5] <- mean(BFtrue$BF.RS1.1 < 0)
Misses[3, 5] <- mean(BFtrue$BF.RS2.1 < 0)
Misses[4, 5] <- mean(BFtrue$BF.RS.1 < 0)
Misses[5, 5] <- mean(BFtrue$BF.eRS.1 < 0)

# mild false negatives (BF < 1/3)
Misses[1, 3] <- mean(BFtrue$BF.noRS.0 < log10(1/3))
Misses[2, 3] <- mean(BFtrue$BF.RS1.0 < log10(1/3))
Misses[3, 3] <- mean(BFtrue$BF.RS2.0 < log10(1/3))
Misses[4, 3] <- mean(BFtrue$BF.RS.0 < log10(1/3))
Misses[5, 3] <- mean(BFtrue$BF.eRS.0 < log10(1/3))

Misses[1, 6] <- mean(BFtrue$BF.noRS.1 < log10(1/3))
Misses[2, 6] <- mean(BFtrue$BF.RS1.1 < log10(1/3))
Misses[3, 6] <- mean(BFtrue$BF.RS2.1 < log10(1/3))
Misses[4, 6] <- mean(BFtrue$BF.RS.1 < log10(1/3))
Misses[5, 6] <- mean(BFtrue$BF.eRS.1 < log10(1/3))


BFcols <- c("BF.noRS.0", "BF.RS1.0", "BF.RS2.0", "BF.RS.0", "BF.noRS.1", "BF.RS1.1", "BF.RS2.1", "BF.RS.1")
ymax <- 20
ymin <- min(BFtrue[,BFcols])
colors <- c("black", "red", "blue", "green", "orange")

# Plot log10(BF) as a function of aggregation level, model version and sample size N

BFlabels <- c("noRS", "RS1", "RS2", "RS", "noRS", "RS1", "RS2", "RS")
if (plotoption==0) x11(width=12, height=8)
if (plotoption==1) tiff(paste0("./plots/ANOVA3.BF1.by.N.tiff"), units="in", width=12, height=8, res=500)
layout(matrix(1:2,1,2))
for (agg in 0:1) {
  plot(1:4, colMeans(BFtrue[, BFcols[c(1:4)+4*agg]]), xlab="", ylab="log10(BF)", 
       xlim=c(0.5, 4.5), ylim=c(ymin, ymax), axes=F, main=paste0("True Effect = ", efactor, ", Aggreg. = ", agg))
  box()
  axis(side=1, at=1:4, labels=BFlabels[c(1:4)+4*agg])
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(Nsubj) - (mean(1:length(Nsubj))))/(1.3*length(Nsubj))
  for (nsubj in 1:length(Nsubj)) {
    bftrue <- subset(BFtrue, N == Nsubj[nsubj])
    nObs <- dim(bftrue)[1]
    for (cond in c(1:4)) points(jitter(rep(cond+dev[nsubj], nObs), amount=0.08), bftrue[, BFcols[cond+4*agg]], col=colors[nsubj])
  }
  legend(4.5, ymax, Nsubj, pch=1, col = colors, title="N", yjust=1, xjust=1)
}
if (plotoption==1) dev.off()


# Plot log10(BF) as a function of aggregation level, model version and number of trials

BFlabels <- c("noRS", "RS1", "RS2", "RS", "noRS", "RS1", "RS2", "RS")
if (plotoption==0) x11(width=12, height=8)
if (plotoption==1) tiff(paste0("./plots/ANOVA3.BF1.by.ntrials.tiff"), units="in", width=12, height=8, res=500)
layout(matrix(1:2,1,2))
for (agg in 0:1) {
  plot(1:4, colMeans(BFtrue[, BFcols[c(1:4)+4*agg]]), xlab="", ylab="log10(BF)", 
       xlim=c(0.5, 4.5), ylim=c(ymin, ymax), axes=F, main=paste0("True Effect = ", efactor, ", Aggreg. = ", agg))
  box()
  axis(side=1, at=1:4, labels=BFlabels[c(1:4)+4*agg])
  axis(side=2)
  abline(0,0, col="red")
  dev <- (1:length(Ntrials) - (mean(1:length(Ntrials))))/(1.3*length(Ntrials))
  for (nt in 1:length(Ntrials)) {
    bftrue <- subset(BFtrue, ntrials == Ntrials[nt]/4)
    nObs <- dim(bftrue)[1]
    for (cond in c(1:4)) points(jitter(rep(cond+dev[nt], nObs), amount=0.08), bftrue[, BFcols[cond+4*agg]], col=colors[nt])
  }
  legend(4.5, ymax, Ntrials, pch=1, col = colors, title="N(trials)", yjust=1, xjust=1)
}
if (plotoption==1) dev.off()


########### Misses and False Negatives as a Function of Nsubj and Ntrials 

# Proportion of Misses with full RS as a function of Nsubj and Ntrials (not-aggregated data)
MissesRS <- matrix(NA, length(Ntrials), length(Nsubj))
for (nsubj in 1:length(Nsubj)) {
  for (nt in 1:length(Ntrials)) {
    bftrue <- subset(BFtrue, N==Nsubj[nsubj] & ntrials==Ntrials[nt]/4) 
    MissesRS[nt,nsubj] <- mean(bftrue$BF.RS.0 < 0)
  }
}

FalseNegativesRS <- matrix(NA, length(Ntrials), length(Nsubj))
for (nsubj in 1:length(Nsubj)) {
  for (nt in 1:length(Ntrials)) {
    bftrue <- subset(BFtrue, N==Nsubj[nsubj] & ntrials==Ntrials[nt]/4) 
    FalseNegativesRS[nt,nsubj] <- mean(bftrue$BF.RS.0 < log10(1/3))
  }
}


# Proportion of Misses with evidence-based RS as a function of Nsubj and Ntrials (not-aggregated data)
MissesRSe <- matrix(NA, length(Ntrials), length(Nsubj))
for (nsubj in 1:length(Nsubj)) {
  for (nt in 1:length(Ntrials)) {
    bftrue <- subset(BFtrue, N==Nsubj[nsubj] & ntrials==Ntrials[nt]/4) 
    MissesRSe[nt,nsubj] <- mean(bftrue$BF.eRS.0 < 0)
  }
}

FalseNegativesRSe <- matrix(NA, length(Ntrials), length(Nsubj))
for (nsubj in 1:length(Nsubj)) {
  for (nt in 1:length(Ntrials)) {
    bftrue <- subset(BFtrue, N==Nsubj[nsubj] & ntrials==Ntrials[nt]/4) 
    FalseNegativesRSe[nt,nsubj] <- mean(bftrue$BF.eRS.0 < log10(1/3))
  }
}


