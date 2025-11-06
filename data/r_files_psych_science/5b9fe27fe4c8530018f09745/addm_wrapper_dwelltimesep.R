# Wrapper that separates by dwell time advantage
task <- "Multiplicative"
setwd("~/Documents/SERVERFive2FoodChoiceADDM")
subjs <- 1:6

midround <- function(x,base){
  base*round(x/base)
}

date <- Sys.Date()
# this is a wrapper script to run a bunch of random parameter combinations and get goodness of fit measures
# it uses time advantage separation!
ptm<-proc.time()#this is for timing the code
indbounds <- 0 # use individual bounds
fulltheta <- 1 # use full range of theta (0,1)
gridtheta <- 0 # use a grid for theta?
iter <- 10
Nclusters <- 20

#define lower and upper bounds on parameters
dmin<-0.000 # 0.0001
dmax<-0.01 # 0.0004
sigmin<-0.00  # 0.01
sigmax<-0.05 # 0.025
thetamin<-0 # 0
thetamax<-1 # 1
ndtmin<-0 # 300
ndtmax<-1000 # 700
dsigmin<-0 # 0.0001
dsigmax<-0 # 0.001
athetamin<-0 #set these to zero for 2 item choice ONLY
athetamax<-0 #set these to zero for 2 item choice ONLY
paramset<-c(dmin,dmax,sigmin,sigmax,thetamin,thetamax,ndtmin,ndtmax,dsigmin,dsigmax,athetamin,athetamax)

#how many parameter combinations and simulations per trial to run
nparams<-2500
nsim<-10

#################################

#this adds the simulation function
source("sim_addm_fun_v2.R")
source("fit_addm_tadvsep_fun_v3.R")
source("fixsumsFNs_v3.R")
bounds <- read.csv("bounds1.csv")
bounds <- bounds[,-c(1)]

subParams <- list()
for (subj in subjs) {
  filename <- paste(task,subj,"tadvsepiter",iter,".RData",sep = "")
  # load the dataset we are trying to fit
  dataset <- paste(task,subj,"_data_prepped.RData",sep = "") # data_mat and/or data
  dataset2 <- "" # the other one, if needed (I include both data_mat and data in the same .RData file)
  load(dataset)
  if(nchar(dataset2)>0) {load(dataset2)}
  
  if (length(data$lastfix) > 0) {
    data$Lastfix <- data$lastfix
    data$Time <- data$fixtime
    data$ROI <- data$roi
  }
   compute fixsums
  if (length(data$revfixnum)>0) {
    data <- fixsumsFN(data)
  }
  if (length(data$revfixnum)==0) {
    data <- fixsumsFNmydata(data)
  }
  data$leftval <- midround(data$leftval, 0.001)
  data$rightval <- midround(data$rightval, 0.001)
  data_mat[,3] <- midround(data_mat[,3], 0.001)
  data_mat[,4] <- midround(data_mat[,4], 0.001)
  
  #randomly sample parameter combinations
  params<-matrix(0,nrow=nparams,ncol=(7+2)) #ncol is number of parameters we are searching over + 2 for goodness of fit
  if (indbounds == 0) {
    params[,1]<-nsim
    params[,2]<-runif(nparams,dmin,dmax)#may want to think about doing this with random sampling at the log scale
    params[,3]<-runif(nparams,sigmin,sigmax)
    params[,4]<-runif(nparams,thetamin,thetamax)
    params[,5]<-round(runif(nparams,ndtmin,ndtmax))
    params[,6]<-runif(nparams,dsigmin,dsigmax)
    params[,7]<-runif(nparams,athetamin,athetamax)
    if (gridtheta == 1) {
      params[,4]<-seq(thetamin,thetamax,length.out=nparams)
    }
  }
  if (indbounds == 1) {
    params[,1]<-nsim
    params[,2]<-runif(nparams,bounds[subj,1],bounds[subj,2])#may want to think about doing this with random sampling at the log scale
    params[,3]<-runif(nparams,bounds[subj,3],bounds[subj,4])
    params[,4]<-runif(nparams,bounds[subj,5],bounds[subj,6])
    params[,5]<-round(runif(nparams,bounds[subj,7],bounds[subj,8]))
    params[,6]<-runif(nparams,dsigmin,dsigmax)
    params[,7]<-runif(nparams,athetamin,athetamax)
    if (fulltheta == 1) {
      params[,4]<-runif(nparams,0,1)
      if (gridtheta == 1) {
        params[,4]<-seq(0,1,length.out=nparams)
      }
    }
  }
  
  params <- params[sample(nrow(params)),]
  # loading parallel computing libraries
  library(foreach)
  library(doParallel)
  
  # creating a virtual cluster, Nclusters should be equal to the number of (virtual) cores
  cl = makeCluster(Nclusters)
  registerDoParallel(cl)
  
  # create an empty matrix to put function values in
  x = matrix(0,nrow(params),2)
  
  # compute function values in parallel
  iterations = nrow(params)
  x = foreach (i = 1:iterations, .combine=rbind) %dopar% {
    fit_addm_tadvsep_fun_v3(data,sim_addm_fun_v2(params[i,1],params[i,2],params[i,3],params[i,4],params[i,5],params[i,6],params[i,7],data_mat))
  }
  
  # put function values into the parameter maxtrix
  params[,8:9] = x
  
  save(params, nsim, paramset, file = filename)
  subParams[[subj]] <- params
  
  # stop the cluster
  stopCluster(cl)
}

proc.time()-ptm #this is for timing the code

