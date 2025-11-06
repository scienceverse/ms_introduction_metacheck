# Fitting ML mixture model to individual subject's data of all the Adam (original and replication) experiments
# Compare fit of mixture model to uniform model to identify subjects with zero-information states 
# Fit dual-mixture model to estimate the probability of a subject belonging to a population with consistent zero-information states

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

rm(list=ls(all=TRUE))
graphics.off()
library(circular)
library(rjags)
library(coda)
library(Hmisc)
load.module("vonmises")
library(dfoptim)
load.module("vonmises") # from the von-Mises extension to Jags
source("ConsecutiveID.R")
source("circfun.R")
source("MM2.R")
source("HDIofMCMC.R")
source("plotPostKO.R")


plotoption <- 2        # 0 = plot window, 1 = new x11 window, 2 = pdf
doFit <- 2             # 0: load previous fit; 1: fit ML, and Bayesian 2-parameter mixture model; 2: load ML, and fit Bayesian mixture model with p(zero)
Exp <- 1:5             # set of experiments to model

ExpNames <- c("Exp. 1", "Exp. 2", "Exp. 3", "Exp. 4", "Adam et al (2017)")
centralT <- 3          # central tendency to be plotted: 1 = mean, 2 = median, 3 = mode
SS <- c(4,6)  # set sizes for which the model is run

### Preparations for ML fits ###

cnames <- NULL
for (setsize in SS) cnames <- c(cnames, paste0("SS", setsize, ".Out", 1:setsize))

DBIC <- list()
Nzero <- matrix(NA, sum(SS), length(Exp))
rownames(Nzero) <- cnames
colnames(Nzero) <- Exp

### Preparations for subsequent Bayesian-hierarchical fit ###

monitorInd <- T  # monitor individual parameters
modelname <- "mixture3_h.bug"
parnames <- c("Pmem", "Mprecision", "Ptarget")
indparnames <- c("pm", "precision", "pt")
parameters <- parnames
if (monitorInd) parameters <- c(parameters, indparnames)
n.params.prepred <- length(parameters)

# Parameters for MCMC in Bayesian hierarchical modeling
nadapt <- 5000
niterations <- 20000
nchains <- 4
nthin <- 10

postPmem <- matrix(NA, max(Exp), 5)  # mean, median, mode + HDI for the N set sizes and the N output positions
postPNtarget <- matrix(NA, max(Exp), 5)  # mean, median, mode + HDI for the N set sizes and the N output positions
postSD <- matrix(NA, max(Exp), 5)  # mean, median, mode + HDI for the N set sizes and the N output positions
RHat <- matrix(NA, max(Exp), 4)
colnames(RHat) <- c("Overall", "Pmem", "Ptarget", "Kappa")
ErrorZero66 <- list()     # errors from subjects identified as having zero-information state at set-size 6, output position 6
PMEM <- matrix(NA, nchains*niterations/nthin, max(Exp))  # posterior samples of Pmem for set-size 6, output position 6, for the 4 experiments
colnames(PMEM) <- paste0("Exp.", 1:max(Exp))


RHatMix <- matrix(NA, max(Exp), 5)
colnames(RHatMix) <- c("Overall", "Pmem", "Pmix", "Kappa", "loglik")
PMEMMix <- matrix(NA, nchains*niterations/nthin, max(Exp))
PMix <- matrix(NA, nchains*niterations/nthin, max(Exp))
SDMix <- matrix(NA, nchains*niterations/nthin, max(Exp))
postPmemMix <- matrix(NA, max(Exp), 5)
postSDMix <- matrix(NA, max(Exp), 5)
postPMix <- matrix(NA, max(Exp), 5)
Px <- matrix(0, max(Exp), 24) # probability of each subject being in the Mix-information sub-sample in the dual-mixture mixture model
Pm <- matrix(0, max(Exp), 24) # probability of each subject being in the Mix-information sub-sample in the dual-mixture mixture model


for (experiment in Exp) {
  
  ############# Load and prepare the data #########################
  
  # Load the data. Data of Exp. 1-3 are pre-processed in the corresponding R scripts
  if (experiment==1) {load("ColorwheelAdamReplicationLong.RData"); Data <- Dlong}
  if (experiment==2) {load("ColorwheelAdamSimultaneousSlowLong.RData"); Data <- Dlong}
  if (experiment==3) {load("ColorwheelAdamSimultaneousFastLong.RData"); Data <- Dlong}
  if (experiment==4) {load("ColorwheelAdamSimultaneousFast6Long.RData"); Data <- Dlong}
  if (experiment==5) Data <- read.table("color_recall_free.dat")
  if (experiment==5) {
    names(Data) <- c("id", "trial", "setsize", "outpos",  
                     "col1", "col2", "col3", "col4", "col5", "col6", 
                     "response", "errordeg", "guess" )
  }
  
  
  SS2run <- SS
  if (experiment == 4) SS2run <- 6  # there is no other set size in this experiment
  
  Data$id <- ConsecutiveID(Data$id)
  Data <- subset(Data, is.element(setsize, SS2run))
  
  Ntrials <- dim(Data)[1]
  ID <- Data[, "id"]
  idlist <- sort(unique(Data$id))
  Setsize <- Data[, "setsize"]
  Outpos <- Data[,"outpos"]
  memset <- Data[, grep("col", names(Data))]   # colors in the order of output, in radians
  response <- Data[, "response"]  # response in radians
  
  # sort memset so that target of the current output position is always in the 1st column (required by the model)
  if (experiment < 5) { #memset is ordered by input position
    locationrad <- Data[, c("loc1", "loc2", "loc3", "loc4", "loc5", "loc6")]
    locationdist <- locationrad - Data$targetloc
    D1 <- as.matrix(abs(wrap(locationdist))) # spatial distance of each memory set from the target
    
    # sort memset by distance from the target - this ensures that the first item in each row of MM is the target
    MM <- memset
    for (j in 1:Ntrials) {
      sorting <- c(order(D1[j,1:Setsize[j]]))  # sort rows 1:Setsize by location distance to the target;
      MM[j,1:Setsize[j]] <- memset[j,sorting]
    }
  }
  if (experiment==5) {  # memset is ordered by output position
    MM <- matrix(NA, Ntrials, 6)
    for (j in 1:Ntrials) {
      MM[j,1:Setsize[j]] <- c(memset[j, Outpos[j]], unlist(memset[j, setdiff(1:Setsize[j], Outpos[j])]))
    }
  }
  
  if (doFit==1) {
    
    error <- wrap(response-MM[,1])
    errordeg <- round(rad2deg(error))
    errordeg[errordeg==-180] <- 180
    errorIdx <- errordeg + 180  # indices from 1:360
    
    startParms <- c(0.5, 10)
    
    dBIC <- matrix(NA, length(idlist), sum(SS))
    colnames(dBIC) <- cnames
    condIdx <- 1
    for (setsize in SS) {
      for (outpos in 1:setsize) {
        for (subj in idlist) {
          if (is.element(setsize, SS2run)) {
            eIdx <- errorIdx[Setsize==setsize & Outpos==outpos & ID==subj]
            fitRes1 <- nmkb(par=startParms, fn=MM2, error=eIdx, lower=c(0,0), upper=c(1,200))
            fitRes2 <- optim(par=startParms, fn=MM2, error=eIdx, method="L-BFGS-B", lower=c(0,0), upper=c(1,200))
            DevMM2 <- min(fitRes1$value, fitRes2$value)
            DevU <- length(eIdx)*(-2*log(1/(2*pi)))  # deviance of uniform model
            BICMM2 <- DevMM2 + 2*log(length(eIdx))   # penalty for 2 parameters
            BICU <- DevU
            dBIC[subj, condIdx] <- BICU-BICMM2         # positive: BICMM2 is smaller, so better fit of MM2
          }
        }
        Nzero[condIdx, which(Exp==experiment)] <- sum(dBIC[,condIdx]<0)
        condIdx <- condIdx + 1
      }
    }
    
    DBIC[[experiment]] <- dBIC
    print(experiment)
    print(Nzero[,which(Exp==experiment)])
    
  } else {
    load("AdamMixtureZeroSubjects.RData") 
  }
  
  
  ######### Run Bayesian hierarchical mixture model on the subset of subjects with allegedly zero-information states ####
  
  condIdx <- 1
  setsize <- 6
  outpos <- 6
  zeroSubj <- which(DBIC[[experiment]][,"SS6.Out6"] < 0)  # select the subjects suspected of consistent zero information states
  
  mset <- MM[Setsize==setsize & Outpos==outpos & is.element(ID, zeroSubj), 1:setsize]
  ntrials <- dim(mset)[1]
  mset <- cbind(mset, rep(0,ntrials)) #add column of zeros at the end for the Eb component
  idset <- ConsecutiveID(ID[Setsize==setsize & Outpos==outpos & is.element(ID, zeroSubj)])
  
  data = list (
    "m" = mset,
    "x" = response[Setsize==setsize & Outpos==outpos & is.element(ID, zeroSubj)],
    "N" = ntrials,
    "setsize" = setsize,
    "nsubj" = length(zeroSubj),
    "id" = idset
  )
  
  ErrorZero66[[experiment]] <- wrap(data$x - data$m[,1])
  
  if (doFit==1) {
    
    # fitting with rjags
    jagsmodel<-jags.model(modelname, data=data, n.chains=nchains, n.adapt=nadapt)
    codasamples <- coda.samples(jagsmodel, variable.names=parameters,
                                n.iter=niterations, thin=nthin)  # turn object of class "jags" into "mcmc.list"
    samples <- as.matrix(codasamples)
    diagnostics <- gelman.diag(codasamples)
    RHat[experiment, 1] <- diagnostics$mpsrf  #extracts the multivariate Rhat estimate
    RHat[experiment, 2] <- diagnostics$psrf[2,1] #extracts the Rhat for Pmem
    RHat[experiment, 3] <- diagnostics$psrf[1,1] #extracts the Rhat for Kappa
    RHat[experiment, 4] <- diagnostics$psrf[3,1] #extracts the Rhat for Ptarget
    
    Pmsample <- samples[,"Pmem"]
    Ptsample <- samples[,"Ptarget"]
    Pmemsample <- Pmsample * Ptsample  #so this is actually P(remember target)
    Pntsample <- Pmsample * (1-Ptsample)  #this is actually P(remember non-target)
    
    d <- density(Pmemsample)
    postPmem[experiment,1] <- mean(Pmemsample)
    postPmem[experiment,2] <- median(Pmemsample)
    postPmem[experiment,3] <- d$x[which(d$y==max(d$y))]
    postPmem[experiment, 4:5] <- HDIofMCMC(Pmemsample)
    
    d <- density(Pntsample)
    postPNtarget[experiment,1] <- mean(Pntsample)
    postPNtarget[experiment,2] <- median(Pntsample)      
    postPNtarget[experiment,3] <- d$x[which(d$y==max(d$y))]
    postPNtarget[experiment, 4:5] <- HDIofMCMC(Pntsample)
    
    Ksample <- samples[,"Mprecision"]
    SDsample <- as.vector(180*k2sd(Ksample)/pi)   # turn into SD in degrees
    d <- density(SDsample)
    postSD[experiment,1] <- mean(SDsample)
    postSD[experiment,2] <- median(SDsample)
    postSD[experiment,3] <- d$x[which(d$y==max(d$y))]
    postSD[experiment, 4:5] <- HDIofMCMC(SDsample)
    
    PMEM[,experiment] <- Pmemsample  # keep the entire Pmemsample for plotting full posterior
    
  } # doFit==1 (Bayesian 2-parameter mixture model) 
  
  ############## Run Dual-Mixture Model on Full Sample #############################
  
  if (doFit==2) {
    
    # now fit ALL subjects
    mset <- MM[Setsize==setsize & Outpos==outpos, 1:setsize]
    ntrials <- dim(mset)[1]
    idset <- ConsecutiveID(ID[Setsize==setsize & Outpos==outpos])
    nsubj <- length(unique(idset))
    
    data = list (
      "m" = mset,
      "x" = response[Setsize==setsize & Outpos==outpos],
      "N" = ntrials,
      "nsubj" = nsubj,
      "id" = idset
    )
    
    # fitting with rjags
    modelname <- "mixture2_zero_h.bug"
    parameters <- c("Pmem", "Mprecision", "Pmix", "pm", "precision", "mix")
    jagsmodel<-jags.model(modelname, data=data, n.chains=nchains, n.adapt=nadapt)
    codasamples <- coda.samples(jagsmodel, variable.names=parameters,
                                n.iter=2*niterations, thin=2*nthin)  # turn object of class "jags" into "mcmc.list"
    samples <- as.matrix(codasamples)
    diagError <- F
    tryCatch(
      diagnostics <- gelman.diag(codasamples[,parameters[1:(2+(setsize>1))]]),
      error = function(e) {diagError <- T}
    )
    if (diagError) diagnostics <- gelman.diag(codasamples[,parameters[1:(2+(setsize>1))]], multivariate=F)
    if (diagError==F) RHat[experiment, 1] <- diagnostics$mpsrf  #extracts the multivariate Rhat estimate
    RHatMix[experiment, 2] <- diagnostics$psrf[2,1] #extracts the Rhat for Pmem
    RHatMix[experiment, 3] <- diagnostics$psrf[1,1] #extracts the Rhat for Kappa
    RHatMix[experiment, 4] <- diagnostics$psrf[3,1] #extracts the Rhat for Pmix
    
    Pmemsample <- samples[,"Pmem"]
    PMixsample <- samples[,"Pmix"]
    d <- density(Pmemsample)
    postPmemMix[experiment,1] <- mean(Pmemsample)
    postPmemMix[experiment,2] <- median(Pmemsample)
    postPmemMix[experiment,3] <- d$x[which(d$y==max(d$y))]
    postPmemMix[experiment, 4:5] <- HDIofMCMC(Pmemsample)
    
    Ksample <- samples[,"Mprecision"]
    SDsample <- as.vector(180*k2sd(Ksample)/pi)
    d <- density(SDsample)
    postSDMix[experiment,1] <- mean(SDsample)
    postSDMix[experiment,2] <- median(SDsample)
    postSDMix[experiment,3] <- d$x[which(d$y==max(d$y))]
    postSDMix[experiment, 4:5] <- HDIofMCMC(SDsample)
    
    d <- density(PMixsample)
    postPMix[experiment,1] <- mean(PMixsample)
    postPMix[experiment,2] <- median(PMixsample)
    postPMix[experiment,3] <- d$x[which(d$y==max(d$y))]
    postPMix[experiment, 4:5] <- HDIofMCMC(PMixsample)
    
    PMEMMix[,experiment] <- Pmemsample  # keep the entire Pmemsample for plotting full posterior
    PMix[,experiment] <- PMixsample
    SDMix[,experiment] <- SDsample
    
    Mixsamples <- samples[, paste0("mix[", 1:nsubj, "]")]
    Px[experiment, 1:nsubj] <- colMeans(Mixsamples)  # proportion of Mix samples for each subject = posterior probability that this subject comes from the mixture-model sub-population
    PMsamples <- samples[, paste0("pm[", 1:nsubj, "]")]
    Pm[experiment, 1:nsubj] <- colMeans(PMsamples)
    
    
  } # doFit (Bayesian) 
  
  
}  # experiment 

if (doFit==1) save(DBIC, Nzero, RHat, postPmem, postSD, postPNtarget, PMEM,  file="AdamMixtureZeroSubjects.RData") else {
  load("AdamMixtureZeroSubjects.RData") 
}
if (doFit==2) save(DBIC, Nzero, RHat, postPmem, postSD, postPNtarget, PMEM,
                   RHatMix, postPmemMix, postSDMix, postPMix, PMEMMix, PMix, SDMix, Px, Pm,
                   file="AdamMixtureZeroSubjects.RData") else {
                     load("AdamMixtureZeroSubjects.RData") 
                   }


##### Plot Results ########

####### 3-Parameter Mixture Model of Subset of Subjects Identified as 'Zero-Information' Subjects by BIC Difference ##########

## Plot Parameter estimates (central tendency + HDI)

Parnames <- c("P(mem)", "SD", "P(non-target)", "P(uniform)")
if (plotoption == 1) x11(width=12, height=8)
if (plotoption == 2) pdf(paste0(getwd(), "/plots/Adam.ZeroSubj.MMParms.pdf"), width=12, height=8)
if (plotoption == 3) tiff(paste0(getwd(), "/plots/Adam.ZeroSubj.MMParms.tiff"), width=12, height=8, units="in", res=500)
layout(matrix(1:4,2,2, byrow=T))
par(mar=c(4,4,1,2))

for (par in 1:4) {
  if (par == 1) post <- postPmem
  if (par == 2) post <- postSD
  if (par == 3) post <- postPNtarget
  if (par == 4) post <- 1-postPmem-postPNtarget
  if (par == 2) ylim <- c(0, 90) else ylim <- c(0,1)
  errbar(1:length(Exp), post[Exp, centralT], yplus=post[Exp, 5], yminus=post[Exp, 4], 
         xlim=c(0.5, 5.5), ylim=ylim, xlab="Experiment", ylab=Parnames[par], type="p", 
         cex=1.6, pch=21, errbar.col="black", bg="black", axes=F)
  box()
  axis(side=1, at=1:length(Exp), labels=ExpNames)
  axis(side=2, at=ylim[2]*(1:10)/10)
}
if (plotoption==2 | plotoption==3) dev.off()

## Plot full posteriors of P(mem) among the sub-samples of subjects identified as zero-information by the BIC difference

colors <- c("black", "red", "blue", "green", "orange")
if (plotoption == 1) x11(width=8, height=8)
if (plotoption == 2) pdf(paste0(getwd(), "/plots/Adam.ZeroSubj.PostPmem.pdf"), width=8, height=8)
if (plotoption == 3) tiff(paste0(getwd(), "/plots/Adam.ZeroSubj.PostPmem.tiff"), width=8, height=8, units="in", res=500)
plotPostKO(PMEM[, Exp], col=colors, showMean=F, xlim=c(0, 0.5), xlab="P(mem)")
legend(0.5, 10, ExpNames, lty=1, col=colors, xjust=1, yjust=1)
lines(x=c(0,0), y=c(-1,15), lty=2)
if (plotoption==2 | plotoption==3) dev.off()

## Plot error distributions of the sub-samples of subjects identified as zero-information by the BIC difference

nbins <- 45
breakpoints <- seq(-pi, pi, length.out=nbins+1)
if (plotoption == 1) x11(width=8, height=12)
if (plotoption == 2) pdf(paste0(getwd(), "/plots/Adam.ZeroSubj.ErrorDists.pdf"), width=8, height=12)
if (plotoption == 3) tiff(paste0(getwd(), "/plots/Adam.ZeroSubj.ErrorDists.tiff"), width=8, height=12, units="in", res=500)
layout(matrix(1:6,3,2, byrow=T))
#par(mar=c(4,4,1,2))
for (experiment in Exp) {
  hist(ErrorZero66[[experiment]], breaks=breakpoints, xlab="Error (rad)", main=ExpNames[which(Exp==experiment)])
}
if (plotoption==2 | plotoption==3) dev.off()


######### Double-Mixture Model ################

# plot posterior of P(mix) from the hierarchical dual-mixture model

colors <- c("black", "red", "blue", "green", "orange")
if (plotoption == 1) x11(width=8, height=8)
if (plotoption == 2) pdf(paste0(getwd(), "/plots/Adam.PostPmix.pdf"), width=8, height=8)
if (plotoption == 3) tiff(paste0(getwd(), "/plots/Adam.PostPmix.tiff"), width=8, height=8, units="in", res=500)
plotPostKO(PMix[, Exp], col=colors, showMean=F, xlim=c(0, 1), xlab=expression("P"["Mix"]))
legend(0.5, 10, ExpNames, lty=1, col=colors, xjust=1, yjust=1)
lines(x=c(0,0), y=c(-1,15), lty=2)
if (plotoption==2 | plotoption==3) dev.off()


## Plot Pzero against BIC evidence for zero-information state

MinMaxBIC <- c(0,0)
for (experiment in Exp) {
  biczero <- -DBIC[[experiment]][, "SS6.Out6"]  # positive dBIC is evidence for MM2, so the negative of that is evidence for the Uniform model
  MinMaxBIC[1] <- min(MinMaxBIC[1], min(biczero))
  MinMaxBIC[2] <- max(MinMaxBIC[1], max(biczero))
}

MinMaxBIC[1] <- -100  # cut off 1 value of -203 in Exp. 4

if (plotoption == 1) x11(width=10, height=10)
if (plotoption == 2) pdf(paste0(getwd(), "/plots/Adam.Evidence4ZeroSubj.pdf"), width=12, height=10)
if (plotoption == 3) tiff(paste0(getwd(), "/plots/Adam.Evidence4ZeroSubj.tiff"), width=12, height=10, units="in", res=500)
layout(matrix(1:6,2,3, byrow=T))
par(cex=0.9)
for (experiment in Exp) {
  biczero <- -DBIC[[experiment]][, "SS6.Out6"]
  pZero <- 1-Px[experiment,1:length(biczero)]  # 1-Px = posterior probability that this subject comes from the consistent-guessing population; the length of biczero is the number of actual subjects in each experiment
  plot(pZero, biczero, type="p", pch=19, cex=1.3, col="red", xlim=c(0,1), ylim=1.2*MinMaxBIC, main=ExpNames[which(Exp==experiment)],
       xlab="Posterior P(Guessing Population)", ylab=bquote(Delta ~ "BIC(Uniform Model)") ) 
  abline(0, 0, lty=2)
}
if (plotoption==2 | plotoption==3) dev.off()

## Investiate trade-off between Pmem and Pmix of the double-mixture model

x11()
layout(matrix(1:6,3,2))
for (experiment in Exp) {
  plot(PMEMMix[,experiment], PMix[,experiment], 
       xlab="Pmem", ylab="Pmix", 
       main=ExpNames[experiment])
}
  
x11()
layout(matrix(1:6,3,2))
for (experiment in Exp) {
  plot(PMix[,experiment], SDMix[,experiment], 
       xlab="Pmix", ylab="SD", 
       main=ExpNames[experiment])
}

