# source('~/Documents/phD_Project2/data_analysis/parameter_recovery/mle_model_fitting.R', echo=TRUE)

rm(list=ls())
gc(reset=T)

library(dfoptim)

exp_vector <- c('images','money')
for (i in 1:length(exp_vector)){
  current_exp <- exp_vector[i]

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
included_ids <- read.csv(paste0('./data/included_ids_list_',current_exp,'.csv'))

gambles <- read.csv(paste0('./data/gambles_all_',current_exp,'.csv'))

gains <- subset(gambles,lottery_payoff>0)#choice: 0=safe, 1=lottery
losses <- subset(gambles, lottery_payoff<0)

choices_all_gains <- gains[ , grepl( "ID" , names( gains ) ) ]
choices_all_losses <- losses[ , grepl( "ID" , names( losses ) ) ]

prospect_cols <- c('lottery_payoff', 'lottery_prob', 'lottery_amb')

#function to calculate lnL for CPT
fitCPT <- function(theta, prospects,choices){
  
  lnL <- rep(0,nrow(prospects))
  choice_probs <- rep(0,nrow(prospects))

  for (i in 1:nrow(prospects)){
    single_prospect <- prospects[i,]
    choice_probs[i]<-get_choice_prob_trial(single_prospect,theta[1],theta[2],theta[3]) #alpha,beta,gamma
    
    choice_probs[i][ choice_probs[i]<0.01 ] <- 0.01 #trembilng hand add: accept 1% error
    choice_probs[i][ choice_probs[i]>0.99 ] <- 0.99
    lnL[i] <- ifelse(choices[i]==1,log((choice_probs[i])), log(1-choice_probs[i]))
    # https://www.r-bloggers.com/logistic-regression/
  }
  return(-2*sum(lnL))
  
}

get_choice_prob_trial <- function(prospect, alpha=1,beta=1,gamma=1,create_data=F){
  sv_safe <- 5
  x<-prospect$lottery_payoff
  p<-prospect$lottery_prob
  A<-prospect$lottery_amb
  
  if (prospect$lottery_payoff>0){
    sv_uncorr_pos <- (p-(beta*(A/2)))*(x^alpha)
    sv_lottery <- sv_uncorr_pos^(1/alpha)
    
  }else if (prospect$lottery_payoff<0){
    sv_uncorr_neg <- (p-(beta*(A/2)))*(-(-x)^alpha)
    sv_lottery <- -(abs(sv_uncorr_neg))^(1/alpha)
    
  }
  if (sv_lottery>=0){
    choice_prob <- 1/(1+exp(-gamma*(sv_lottery-sv_safe)))
    choice_det <- ifelse(sv_lottery>=sv_safe,1,0)
  } else if (sv_lottery<0){
    choice_prob <- 1/(1+exp(-gamma*(sv_lottery+sv_safe)))
    choice_det <- ifelse(sv_lottery>=(-sv_safe),1,0)
  }
  
  out <- c(choice_prob, choice_det)
  if (create_data==F){return(choice_prob)}
  else if (create_data==T){return(out)}
}

ngrid<-2
alphas<-seq(0.05,1.9,length=ngrid)
betas<-seq(-1.05,1.05,length=ngrid)
gammas <- seq(0.02,1.9,length=ngrid)
startPoints <- as.matrix(expand.grid(alphas, betas, gammas))
nrow(startPoints)

outfile <- paste0('./results/results_gambles/frequentist/gambles_frequentist_gains_',current_exp,'.csv')

if(!file.exists(outfile)){
fits_all_gains <- {}
fits_all_losses <- {}

# Start the clock!
ptm <- proc.time()
for (id in included_ids$ID){
  gains_id <- subset(gains, ID==id)
  losses_id <- subset(losses, ID==id)
  choices_gains_id <- gains_id$choice#choices_all_gains[,id]
  choices_losses_id <- losses_id$choice#choices_all_losses[,id]
  prospects_gains_id <- gains_id[prospect_cols]
  prospects_losses_id <- losses_id[prospect_cols]
  print(paste('Fitting subject ',id))
  
  bfit_gains <- {}
  for (sp in 1:dim(startPoints)[1]){# https://www.rdocumentation.org/packages/dfoptim/versions/2018.2-1/topics/nmk
    bfit_gains[[sp]] <- nmkb(par=startPoints[sp,], #Nelder-Mead algorithm for derivative-free optimization
                             fn = function(theta) fitCPT(c(theta[1],theta[2],theta[3]),
                                                         prospects=prospects_gains_id, choices=choices_gains_id),
                             lower=c(0,-1.25,0),#This allows bounds to be placed on parameters
                             upper=c(2,1.25,2),
                             control=list(trace=0))
  }
  deviance_gains <- do.call(rbind,lapply(bfit_gains,function(tx) tx$value)) 
  fits_all_gains[[id]] <- bfit_gains[[which.min(deviance_gains)]] # find lowest deviance value and save in results
  
  
  bfit_losses <- {}
  for (sp in 1:dim(startPoints)[1]){# https://www.rdocumentation.org/packages/dfoptim/versions/2018.2-1/topics/nmk
    bfit_losses[[sp]] <- nmkb(par=startPoints[sp,], #Nelder-Mead algorithm for derivative-free optimization
                              fn = function(theta) fitCPT(c(theta[1],theta[2],theta[3]),
                                                          prospects=prospects_losses_id, choices=choices_losses_id),
                              lower=c(0,-1.25,0),#This allows bounds to be placed on parameters
                              upper=c(2,1.25,2),
                              control=list(trace=0))
  }
  deviance_losses <- do.call(rbind,lapply(bfit_losses,function(tx) tx$value)) 
  fits_all_losses[[id]] <- bfit_losses[[which.min(deviance_losses)]] # find lowest deviance value and save in results
  
  
}
# Stop the clock
proc.time() - ptm


lnLs_gains <- do.call(rbind,lapply(fits_all_gains,function(tx) tx$value)) #The value of the objective function at termination.
lnLs_losses <- do.call(rbind,lapply(fits_all_losses,function(tx) tx$value)) #The value of the objective function at termination.
 
outdir_part1 <- paste0('./results/results_gambles/frequentist/')                                
types=1:2                                    
for (type in types){ 
  if (type==1){
    mytype='gains'
    fits_all <- fits_all_gains
  }else if (type==2){
    mytype='losses'
    fits_all <- fits_all_losses
  }
  
  parms_estimates <- do.call(rbind,lapply(fits_all,function(tx) tx$par))
  
  estimates <- as.data.frame(round(parms_estimates,digits=2))
  colnames(estimates) <- c('alpha','beta','gamma')
  write.csv(estimates,paste0(outdir_part1,'gambles_frequentist_',mytype,'_',current_exp,'.csv'),row.names = F)
}
  save('fits_all_gains', file=paste0(outdir_part1,'all_fits_gains_list','_',current_exp,'.Rdata'))
  save('fits_all_losses', file=paste0(outdir_part1,'all_fits_losses_list','_',current_exp,'.Rdata'))
  write.csv(lnLs_gains,paste0(outdir_part1,'ll_estimates_gains','_',current_exp,'.csv'),row.names = F)
  write.csv(lnLs_losses,paste0(outdir_part1,'ll_estimates_losses','_',current_exp,'.csv'),row.names = F)  

}else {
  print('File already exists. Skipping estimation.')
}
}
