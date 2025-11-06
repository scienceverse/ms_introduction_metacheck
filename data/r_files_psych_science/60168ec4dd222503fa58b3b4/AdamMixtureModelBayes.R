# Fitting Bayesian mixture model to Adam et al. (2017) original and replication experiments
# Article describing the Bayesian-hierarchical mixture model: Oberauer, K., Stoneking, C., Wabersich, D., & Lin, H.-Y. (2017). Hierarchical Bayesian measurement models for continuous reproduction of visual features from working memory. Journal of Vision, 17, 1-27. doi:10.1167/17.5.11
# Code for Bayesian-hierarchical mixture model (not needed to run this script): https://osf.io/wjg7y/
# For the von-Mises extension to Jags: https://github.com/yeagle/jags-vonmises

rm(list=ls(all=TRUE))
graphics.off()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(rjags)
library(circular)
library(coda)
load.module("vonmises") # from the von-Mises extension to Jags
library("dplyr")
library("tidyr")
source("ConsecutiveID.R")
source("circfun.R")
source("HDIofMCMC.R")
source("plotPostKO.R")

experiment <- 4 
# 1 = Sequential presentation (Exp. 1 in the paper)
# 2 = Simultaneous presentation, slow (Exp. 2 in the paper)
# 3 = Simultaneous presentation, fast (Exp. 3 in the paper)
# 4 = Simultaneous presentation, fast, set-size 6 only (Exp. 4 in the paper)
# 5 = Original color experiment with free report order (Exp. 1a in Adam et al., 2017)

SS <- c(4,6)          # set size(s) for which model is to be applied
if (experiment==4) SS <- 6  # this experiment included only set size 6

monitorInd <- T  # monitor individual parameters
modelname <- "mixture3_h.bug"
parnames <- c("Pmem", "Mprecision", "Ptarget")
indparnames <- c("pm", "precision", "pt")
parameters <- parnames    # parameters to monitor by Jags
if (monitorInd) parameters <- c(parameters, indparnames)
n.params.prepred <- length(parameters)

# Set parameters for MCMC in Jags
nadapt <- 5000
niterations <- 20000
nchains <- 4

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
# Data are in long format, with one row for each response

Data$id <- ConsecutiveID(Data$id)    # Jags needs subject numbers to be consecutive integers
Data <- subset(Data, is.element(setsize, SS))  # select subset of set sizes to be fitted

Ntrials <- dim(Data)[1]          # number of trials in total
ID <- Data[, "id"]               # subject ID
idlist <- sort(unique(Data$id))  
Setsize <- Data[, "setsize"]     # memory set size
Outpos <- Data[,"outpos"]        # output position of each row
memset <- Data[, grep("col", names(Data))]   # colors in the memory set, in the order of output, in radians 
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
  MM <- matrix(NA, dim(Data)[1], 6)
  for (j in 1:dim(Data)[1]) {
    MM[j,1:Setsize[j]] <- c(memset[j, Outpos[j]], unlist(memset[j, setdiff(1:Setsize[j], Outpos[j])]))
  }
}

# Define arrays for the summary statistics of the posteriors
postPmem <- array(NA, dim = c(5, max(SS), max(SS)))  # mean, median, mode + HDI for the N set sizes and the N output positions
postPNtarget <- array(NA, dim = c(5, max(SS), max(SS)))  # mean, median, mode + HDI for the N set sizes and the N output positions
postSD <- array(NA, dim = c(5, max(SS), max(SS)))  # mean, median, mode + HDI for the N set sizes and the N output positions
RHat <- as.data.frame(matrix(0, sum(SS), 5))
colnames(RHat) <- c("Overall", "Pmem", "Ptarget", "Kappa", "loglik")

###### Fit the model ####################

condIdx <- 1   # index for the condition to be modelled
for (setsize in SS) {      # loop over set sizes
  for (outpos in 1:setsize) {    # loop over output positions in each set size
    
    # put together the data from the current condition as list for Jags model
    ntrials <- length(ID[Setsize==setsize & Outpos==outpos])
    mset <- MM[Setsize==setsize & Outpos==outpos, 1:setsize]
    data = list (
      "m" = mset,
      "x" = response[Setsize==setsize & Outpos==outpos],
      "N" = ntrials,
      "setsize" = setsize,
      "nsubj" = length(unique(ID)),
      "id" = ID[Setsize==setsize & Outpos==outpos]
    )
    
    # fitting with rjags
    jagsmodel<-jags.model(modelname, data=data, n.chains=nchains, n.adapt=nadapt)
    codasamples <- coda.samples(jagsmodel, variable.names=parameters,
                                n.iter=niterations, thin=1)  # turn object of class "jags" into "mcmc.list"
    samples <- as.matrix(codasamples)
    diagnostics <- gelman.diag(codasamples[,parameters[1:(2+(setsize>1))]])
    RHat[condIdx, 1] <- diagnostics$mpsrf  #extracts the multivariate Rhat estimate
    RHat[condIdx, 2] <- diagnostics$psrf[2,1] #extracts the Rhat for Pmem
    RHat[condIdx, 3] <- diagnostics$psrf[1,1] #extracts the Rhat for Kappa
    RHat[condIdx, 4] <- diagnostics$psrf[3,1] #extracts the Rhat for Ptarget
    Pmsample <- samples[,"Pmem"]
    Ptsample <- samples[,"Ptarget"]
    Pmemsample <- Pmsample * Ptsample  #so this is actually P(remember target)
    Pntsample <- Pmsample * (1-Ptsample)  #this is actually P(remember non-target)
    d <- density(Pntsample)
    postPNtarget[1,setsize,outpos] <- mean(Pntsample)             # mean of posterior
    postPNtarget[2,setsize,outpos] <- median(Pntsample)           # median of posterior
    postPNtarget[3,setsize,outpos] <- d$x[which(d$y==max(d$y))]   # mode of posterior
    postPNtarget[4:5,setsize,outpos] <- HDIofMCMC(Pntsample)
    d <- density(Pmemsample)
    postPmem[1,setsize,outpos] <- mean(Pmemsample)
    postPmem[2,setsize,outpos] <- median(Pmemsample)
    postPmem[3,setsize,outpos] <- d$x[which(d$y==max(d$y))]
    postPmem[4:5,setsize,outpos] <- HDIofMCMC(Pmemsample)
    
    Ksample <- samples[,"Mprecision"]
    SDsample <- as.vector(180*k2sd(Ksample)/pi)   # turn kappa into SD (degrees)
    d <- density(SDsample)
    postSD[1,setsize,outpos] <- mean(SDsample)
    postSD[2,setsize,outpos] <- median(SDsample)
    postSD[3,setsize,outpos] <- d$x[which(d$y==max(d$y))]
    postSD[4:5,setsize,outpos] <- HDIofMCMC(SDsample)
    condIdx <- condIdx + 1
    print(c(setsize, outpos))
    
  }
  
}

outfile <- paste0('Adam', experiment, '.RData')
save(postPmem, postPNtarget, postSD, file=outfile)

