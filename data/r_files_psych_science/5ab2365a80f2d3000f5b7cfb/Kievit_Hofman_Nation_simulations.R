#' This script supports the simulation results in the following paper
#' Mutualistic coupling between vocabulary and reasoning in young children: A replication and extension of Kievit et al. (2017)
#' Authors: Rogier A. Kievit, Abe Hofman & Kate Nation
#' Script author: Rogier Kievit, January 2018 
#' This script demonstrates power to prefer the g factor model over the 
#' Mutualism model if the g factor is the data generating model

rm(list = ls())

library(lavaan)
library(reshape2)
library(ggplot2)

# --------------------------------------------------------------------------------
# Simulations

#### g factor model as LCS

#### Proof of principle: Simulate g factor model of change with same N, fit back.
#### Note: for both models all equality constraints across waves are assumed

gfactor_sim <- '
# Measurement model
gft1 =~ 0.8 * t1ssmatrix + 0.7 * t1ssvocab
gft2 =~ 0.8 * t2ssmatrix + 0.7 * t2ssvocab
gft3 =~ 0.8 * t3ssmatrix + 0.7 * t3ssvocab

# LCS specification
gft2 ~ 1 * gft1
gft3 ~ 1 * gft2

etag1 =~ 1 * gft2
etag2 =~ 1 * gft3

gft2 ~~ 0 * gft2
gft3 ~~ 0 * gft3

# Latent change intercepts
etag1 ~ 1 * 1
etag2 ~ 1 * 1
gft1 ~ 10 * 1
gft2 ~ 0 * 1
gft3 ~ 0 * 1
etag1 ~~ 1.5 * etag1
etag2 ~~ 1.5 * etag2

# Structural model
etag1 ~ .2 * gft1 # replace by 0 for model without feedback (.2 in old version)
etag2 ~ .2 * gft2

# Residual covariances
t1ssmatrix ~~ .5 * t2ssmatrix
t3ssmatrix ~~ .5 * t2ssmatrix
t3ssmatrix ~~ .5 * t1ssmatrix

t1ssvocab ~~ .5 * t2ssvocab
t2ssvocab ~~ .5 * t3ssvocab
t3ssvocab ~~ .5 * t1ssvocab'

# Simulate data of same sample size
sim_dat_gfactor <- simulateData(gfactor_sim, sample.nobs = 227)

# Raw data looks sensible
sim_dat_gfactor$id <- as.factor(1:nrow(sim_dat_gfactor)) # hier zat een foutje
plotdat <- melt(sim_dat_gfactor[, - grep('vocab', names(sim_dat_gfactor))],
                by = 'id') # omit vocab variables
plotdat$age <- rep(c(6.22, 7.22, 8.22), each = nrow(sim_dat_gfactor))

ggplot(plotdat, aes(age, value, group = id)) +
  geom_line(alpha = .7, col = 'dodgerblue', lwd = 1) +
  geom_point(alpha = .7, size = 2, col = 'dodgerblue') +
    ylab('Matrix reasoning score') +
  theme(text = element_text(size = 22)) +
  ylim(4.5, 20) +
  xlab('Age')

# Correlation structure is similar to empirical data
cor(subdat[, !names(subdat) %in% ('Gender')], use = 'complete') # subdat dataset comes from analysis script
cor(sim_dat_gfactor[, - grep('id', names(sim_dat_gfactor))])

# Means
colMeans(subdat[, !names(subdat) %in% ('Gender')], na.rm = TRUE)
colMeans(sim_dat_gfactor[, - grep('id', names(sim_dat_gfactor))])

# SD
apply(subdat[, !names(subdat) %in% ('Gender')], 2, sd, na.rm = TRUE)
apply(sim_dat_gfactor[, - grep('id', names(sim_dat_gfactor))], 2, sd)

# Fit g factor LCS to 'true' g factor data
gfactor_sim_fit <- '
# Measurement model
gft1 =~ fl1 * t1ssmatrix + fl2 * t1ssvocab
gft2 =~ fl1 * t2ssmatrix + fl2 * t2ssvocab
gft3 =~ fl1 * t3ssmatrix + fl2 * t3ssvocab

# LCS specification
gft2 ~ 1 * gft1
gft3 ~ 1 * gft2

etag1 =~ 1 * gft2
etag2 =~ 1 * gft3

gft2 ~~ 0 * gft2
gft3 ~~ 0 * gft3

# Latent change intercepts
etag1 ~ etaint1 * 1
etag2 ~ etaint1 * 1
gft1 ~ 1
gft2 ~ 0 * 1
gft3 ~ 0 * 1
etag1 ~~ etag1
etag2 ~~ etag2

# Structural model
etag1 ~ selffb1 * gft1
etag2 ~ selffb1 * gft2

# Residual variances
t1ssmatrix ~~ resvarmat * t1ssmatrix
t2ssmatrix ~~ resvarmat * t2ssmatrix
t3ssmatrix ~~ resvarmat * t3ssmatrix
t1ssvocab ~~ resvarvoc * t1ssvocab
t2ssvocab ~~ resvarvoc * t2ssvocab
t3ssvocab ~~ resvarvoc * t3ssvocab

# Residual covariances
t1ssmatrix ~~ rescovmat * t2ssmatrix
t3ssmatrix ~~ rescovmat * t2ssmatrix
t3ssmatrix ~~ rescovmat * t1ssmatrix

t1ssvocab ~~ rescovvoc * t2ssvocab
t2ssvocab ~~ rescovvoc * t3ssvocab
t3ssvocab ~~ rescovvoc * t1ssvocab'

est_gfactor_sim_fit <- growth(gfactor_sim_fit, 
                                 data = sim_dat_gfactor,
                                 estimator = 'mlr',
                                 fixed.x = FALSE, missing = 'fiml')
# Excellent fit
summary(est_gfactor_sim_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# fit mutualism LCS to 'true' g factor data
mutualism_sim_fit <- '

t2ssvocab ~ 1 * t1ssvocab
t3ssvocab ~ 1 * t2ssvocab

etavoc1 =~ 1 * t2ssvocab
etavoc2 =~ 1 * t3ssvocab

etavoc1 ~ vocint1 * 1   
etavoc2 ~ vocint1 * 1 # only exception to cross-wave equality constraints 

t1ssvocab ~ 1
t2ssvocab ~ 0 * 1
t3ssvocab ~ 0 * 1

etavoc1 ~~ varetavoc1 * etavoc1
etavoc2 ~~ varetavoc1 * etavoc2

t2ssmatrix ~ 1 * t1ssmatrix
t3ssmatrix ~ 1 * t2ssmatrix

etamat1 =~ 1 * t2ssmatrix
etamat2 =~ 1 * t3ssmatrix

etamat1 ~ matint1 * 1   
etamat2 ~ matint1 * 1   

t1ssmatrix ~ 1
t2ssmatrix ~ 0 * 1
t3ssmatrix ~ 0 * 1

# variances/covariances
etamat1 ~~ varetamat1 * etamat1
etamat2 ~~ varetamat1 * etamat2

t1ssmatrix ~~ t1ssvocab

etamat1 ~ negmat * t1ssmatrix + couplinga * t1ssvocab
etamat2 ~ negmat * t2ssmatrix + couplinga * t2ssvocab

etavoc1 ~ negvoc * t1ssvocab + couplingb * t1ssmatrix
etavoc2 ~ negvoc * t2ssvocab + couplingb * t2ssmatrix

etamat1 ~~ a * etavoc1
etamat2 ~~ a * etavoc2'

est_mutualism_fit <- growth(mutualism_sim_fit, 
                            data = sim_dat_gfactor, 
                            estimator = 'mlr',
                            fixed.x = FALSE,
                            missing = 'fiml')
summary(est_mutualism_fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = T)

# Models prefers g factor 
AIC(est_gfactor_sim_fit)
AIC(est_mutualism_fit)
# Excellent fit
fitMeasures(est_gfactor_sim_fit, c("cfi","rmsea","srmr"))
# Poor fit
fitMeasures(est_mutualism_fit, c("cfi","rmsea","srmr"))

# --------------------------------------------------------------------------------
# Power analysis: how often does AIC prefer mutualism when true data is g factor?
# General specifications
nsims <- 100                                 # Define the number of iterations
samplesize <- 227                             # Define the sample size 
modelcomp <- data.frame(matrix(NA, nsims, 4)) # Create empty object to store logicals for whether the correct model is preferred
colnames(modelcomp) <- c('delta AIC','AIC logical','delta BIC','BIC logical')

for (i in 1:nsims) { #initiate loop
  set.seed(i)
  fitmodelA <- NA   # change model output back to NA
  fitmodelB <- NA   # change model output back to NA
  
  #Simulate data under the the true model 
  simdatgflcs <- simulateData(gfactor_sim, sample.nobs = 227) 
  
  #deselect either of these to replicate simulations using different g factor models
  #simdatgflcs<-simulateData(gfactor_sim_noselffb,sample.nobs = 227)
  #simdatgflcs<-simulateData(gfactor_sim_negselffb,sample.nobs = 227)
  
  # Fit model A, the true model
  fitmodelA <- growth(gfactor_sim_fit, data = simdatgflcs, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')
  fitmodelB <- growth(mutualism_sim_fit, data = simdatgflcs, estimator = 'mlr', fixed.x = FALSE, missing = 'fiml')

  # Likelihood ratio test
  LRTcomp <- anova(fitmodelA, fitmodelB)
  modelaAIC <- AIC(fitmodelA) # grab AIC for the true model fit
  modelbAIC <- AIC(fitmodelB) # grab AIC for the competitor model fit
  modelaBIC <- BIC(fitmodelA) # grab AIC for the true model fit
  modelbBIC <- BIC(fitmodelB) # grab AIC for the competitor model fit
  
  modelcomp[i,1] <- modelbAIC - modelaAIC # delta AIC: positive means A preferred over B
  modelcomp[i,2] <- modelaAIC < modelbAIC # logical, does the alternative model have a
  modelcomp[i,3] <- modelbBIC - modelaBIC # delta AIC: positive means A preferred over B
  modelcomp[i,4] <- modelaBIC < modelbBIC # logical, does the alternative model have a
  
  if(i %% 10 == 0) cat(i, '- ')
}

powerAIC <- (sum(as.numeric(modelcomp[, 2])) / nsims) * 100
powerAIC # How often is the true model preferred by AIC 

powerBIC <- (sum(as.numeric(modelcomp[,4])) / nsims) * 100
powerBIC # How often is the true model preferred by BIC 

#plotting AIC values
ggplot(modelcomp,aes(`delta AIC`)) +
  geom_histogram(fill = 'dodgerblue', bins = 10) +
  geom_vline(xintercept = 0)
ggplot(modelcomp,aes(`delta BIC`)) +
  geom_histogram(fill = 'dodgerblue', bins = 10) +
  geom_vline(xintercept = 0)

summary(fitmodelA, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = T)
summary(fitmodelB, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = T)


# #simulate under conditions of no self-feedback
# gfactor_sim_noselffb <- '
# #Measurement model
# gft1=~0.8*t1ssmatrix+0.7*t1ssvocab
# gft2=~0.8*t2ssmatrix+0.7*t2ssvocab
# gft3=~0.8*t3ssmatrix+0.7*t3ssvocab
# 
# #LCS specification
# gft2~1*gft1
# gft3~1*gft2
# 
# etag1=~ 1*gft2
# etag2=~ 1*gft3
# 
# gft2 ~~ 0*gft2
# gft3 ~~ 0*gft3
# 
# #latent change intercepts
# etag1 ~ 1*1
# etag2 ~ 1*1
# gft1 ~ 10*1
# gft2 ~ 0*1
# gft3 ~ 0*1
# etag1 ~~ 2.5*etag1
# etag2 ~~ 2.5*etag2
# 
# #Structural model
# etag1~0*gft1  #no self feedback
# etag2~0*gft2  #no self feedback
# 
# #Residual covariances
# t1ssmatrix~~.5*t2ssmatrix
# t3ssmatrix~~.5*t2ssmatrix
# t3ssmatrix~~.5*t1ssmatrix
# 
# t1ssvocab~~.5*t2ssvocab
# t2ssvocab~~.5*t3ssvocab
# t3ssvocab~~.5*t1ssvocab
# 
# '
# 
# #Simulate data of same sample size
# 
# simdatgfactor_no_sfb<-simulateData(gfactor_sim_noselffb,sample.nobs = 227)
# 
# 
# #### raw data looks sensible
# simdatgfactor_sim_noselffb$id<-as.factor(1:nrow(simdatgfactor_sim_noselffb))
# plotdat<-melt(simdatgfactor_sim_noselffb[,c(1,3,5,7)],by='id')
# plotdat$age<-rep(c(6.22,7.22,8.22),each=nrow(simdatgflcs))
# ggplot(plotdat,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='dodgerblue')+geom_line(alpha=.7,col='dodgerblue',lwd=1)+ylab('Matrix reasoning score')+theme(text = element_text(size=22))+ylim(4.5,20)+xlab('Age')
# 
# #correlation structure is similar to empirical data
# cor(simdatgfactor_sim_noselffb[,1:6])
# 
# 
# #simulate under conditions of negative feedback, but with larger etag to ensure
# # developmental improvement over time
# gfactor_sim_negselffb <- '
# #Measurement model
# gft1=~0.8*t1ssmatrix+0.7*t1ssvocab
# gft2=~0.8*t2ssmatrix+0.7*t2ssvocab
# gft3=~0.8*t3ssmatrix+0.7*t3ssvocab
# 
# #LCS specification
# gft2~1*gft1
# gft3~1*gft2
# 
# etag1=~ 1*gft2
# etag2=~ 1*gft3
# 
# gft2 ~~ 0*gft2
# gft3 ~~ 0*gft3
# 
# #latent change intercepts
# etag1 ~ 4*1
# etag2 ~ 4*1
# gft1 ~ 10*1
# gft2 ~ 0*1
# gft3 ~ 0*1
# etag1 ~~ 1.5*etag1
# etag2 ~~ 1.5*etag2
# 
# #Structural model
# etag1~-0.2*gft1
# etag2~-0.2*gft2
# 
# #Residual covariances
# t1ssmatrix~~.5*t2ssmatrix
# t3ssmatrix~~.5*t2ssmatrix
# t3ssmatrix~~.5*t1ssmatrix
# 
# t1ssvocab~~.5*t2ssvocab
# t2ssvocab~~.5*t3ssvocab
# t3ssvocab~~.5*t1ssvocab
# 
# '
# 
# #Simulate data of same sample size
# simdatgfactor_neg_sfb<-simulateData(gfactor_sim_negselffb,sample.nobs = 227)
# 
# 
# #### raw data looks sensible
# simdatgfactor_neg_sfb$id<-as.factor(1:nrow(simdatgfactor_neg_sfb))
# plotdat<-melt(simdatgfactor_neg_sfb[,c(1,3,5,7)],by='id')
# plotdat$age<-rep(c(6.22,7.22,8.22),each=nrow(simdatgfactor_neg_sfb))
# ggplot(plotdat,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='dodgerblue')+geom_line(alpha=.7,col='dodgerblue',lwd=1)+ylab('Matrix reasoning score')+theme(text = element_text(size=22))+ylim(4.5,20)+xlab('Age')
# 
# #correlation structure is similar to empirical data
# cor(simdatgfactor_neg_sfb[,1:6])
# 
# 


#Power mutualism model

mutualism_sim <- '

t2ssvocab ~ 1 * t1ssvocab
t3ssvocab ~ 1 * t2ssvocab

etavoc1 =~ 1 * t2ssvocab
etavoc2 =~ 1 * t3ssvocab

etavoc1 ~ 1 * 1   
etavoc2 ~ 1 * 1 # only exception to cross-wave equality constraints 

t1ssvocab ~ 1
t2ssvocab ~ 0 * 1
t3ssvocab ~ 0 * 1

etavoc1 ~~ 1 * etavoc1
etavoc2 ~~ 1 * etavoc2

t2ssmatrix ~ 1 * t1ssmatrix
t3ssmatrix ~ 1 * t2ssmatrix

etamat1 =~ 1 * t2ssmatrix
etamat2 =~ 1 * t3ssmatrix

etamat1 ~ 1 * 1   
etamat2 ~ 1 * 1   

t1ssmatrix ~ 1
t2ssmatrix ~ 0 * 1
t3ssmatrix ~ 0 * 1

# variances/covariances
etamat1 ~~ 1 * etamat1
etamat2 ~~ 1 * etamat2

t1ssmatrix ~~ t1ssvocab

etamat1 ~ 0 * t1ssmatrix + .15 * t1ssvocab
etamat2 ~ 0 * t2ssmatrix + .15 * t2ssvocab

etavoc1 ~ 0 * t1ssvocab + .15 * t1ssmatrix
etavoc2 ~ 0 * t2ssvocab + .15 * t2ssmatrix

etamat1 ~~ .1 * etavoc1
etamat2 ~~ .1 * etavoc2

'

sim_dat_mutualism <- simulateData(mutualism_sim, sample.nobs = 227)



# Power analysis: how often does AIC prefer mutualism when true data is g factor?
# General specifications
nsims <- 100                                 # Define the number of iterations
samplesize <- 227                             # Define the sample size 
modelcomp <- data.frame(matrix(NA, nsims, 7)) # Create empty object to store logicals for whether the correct model is preferred
colnames(modelcomp) <- c('RMSEA','p_voc_to_mat','p_voc_to_mat_log','p_voc_to_mat_std','p_mat_to_voc','p_mat_to_voc_log','p_mat_to_voc_std')

for (i in 1:nsims) { #initiate loop
  set.seed(i)
  fitmodelA <- NA   # change model output back to NA

  #Simulate data under the the true model 
  sim_dat_mutualism <- simulateData(mutualism_sim, sample.nobs = 227)
  # Fit model A, the true model
  fitmodelA <-  est_mutualism_fit <- growth(mutualism_sim_fit, 
                                data = sim_dat_mutualism, 
                                estimator = 'mlr',
                                fixed.x = FALSE,
                                missing = 'fiml')
  #summary(fitmodelA, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, ci = T)
  
  # Likelihood ratio test
  pars<-parameterestimates(fitmodelA)[c(25,29),8]
  
  modelcomp[i,1] <- fitMeasures(fitmodelA,'RMSEA')[1]
  modelcomp[i,2] <- pars[1]  # delta AIC: positive means A preferred over B
  modelcomp[i,3] <- pars[1]<0.05 # delta AIC: positive means A preferred over B
  modelcomp[i,4] <-standardizedSolution(fitmodelA)[25,4]
  modelcomp[i,5] <- pars[2] # logical, does the alternative model have a
  modelcomp[i,6] <- pars[2]<0.05 # logical
  modelcomp[i,7] <-standardizedSolution(fitmodelA)[29,4]
  if(i %% 10 == 0) cat(i, '- ')
}



#create power plot
power_0.025<-modelcomp
power_0.05<-modelcomp
power_0.075<-modelcomp
power_0.1<-modelcomp
power_0.15<-modelcomp
power_0.2<-modelcomp
power_0.3<-modelcomp
power_0.4<-modelcomp

std_0.025<-mean(power_0.025$p_voc_to_mat_std)
std_0.05<-mean(power_0.05$p_voc_to_mat_std)
std_0.075<-mean(power_0.075$p_voc_to_mat_std)
std_0.1<-mean(power_0.1$p_voc_to_mat_std)
std_0.15<-mean(power_0.15$p_voc_to_mat_std)
std_0.2<-mean(power_0.2$p_voc_to_mat_std)
std_0.3<-mean(power_0.3$p_voc_to_mat_std)
std_0.4<-mean(power_0.4$p_voc_to_mat_std)
stdbetas<-c(std_0.025,std_0.05,std_0.075,std_0.1,std_0.15,std_0.2,std_0.3,std_0.4)

powerplotfull<-rbind(power_0.025,power_0.05,power_0.075,power_0.1,power_0.15,power_0.2,power_0.3,power_0.4)
powerplotfull$par<-rep(c(' 0.025/0.017','0.05/0.036','0.075/0.052','0.1/0.071','0.15/0.11','0.2/0.14','0.3/0.21','0.4/0.27'),each=nsims)
 

ggplot(powerplotfull,aes(par,p_voc_to_mat_log/100))+geom_col(fill='dodgerblue')+theme(plot.title = element_text(size = 20),axis.text = element_text( size = 10),axis.title = element_text( size = 17),strip.text = element_text(size=14))+ggtitle("Power to detect a true effect")+xlab("Self-feedback parameter")+ylab('Power')+scale_fill_brewer(palette = "Set1")+theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))+xlab('Coupling parameter (beta/standardized beta)')
