###Children’s Cognitive Reflection Predicts Conceptual Understanding in Science and Mathematics 
###Bayesian regression analyses reported in main text
###Note: exact values will not reproduce because variability in Monte Carlo sampling.


################
###Load packages
################

library(brms)
library(projpred)
library(reshape2)
library(broom)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggstance)

#options(mc.cores = parallel::detectCores()) #Go Fast!

########################################################
###Load data, make variables, scale model data
########################################################

#Data is available at https://osf.io/e72ka/?view_only=57c8b2f8cf7b4193af2df40af14c71d7
data=read.csv("CRTD_DATA.csv")

#CRTD
CRTD<- data[,c("crt.cows","crt.emily","crt.butterfly",
              "crt.pound","crt.sheep","crt.santa",
              "crt.race","crt.apples","crt.mo")]
data$CRTD <- rowSums(CRTD)

#Denominator Neglect
DN <- data[,c("T.1.9.vs.9.91","T.1.9.vs.8.92","T.2.8.vs.18.82","T.2.8.vs.19.81",
              "T.3.7.vs.29.71","T.3.7.vs.28.72")]
data$DN <- rowSums(DN)

#Base Rate Sensitivity
BRS <- data[,c("brs1","brs2","brs3","brs4","brs5")]
data$BRS <- rowSums(BRS)

#NFC
NFC <- data[,c("NFC_1","NFC_2","NFC_3","NFC_4","NFC_5","NFC_6","NFC_7",                 
               "NFC_8","NFC_9","NFC_10","NFC_11","NFC_12", "NFC_13","NFC_14")]
data$NFC <- rowMeans(NFC)

#Mathematical Equivalence: Number Correct and Any Correct
ME <- data[,c("ME1_correct","ME2_correct","ME3_correct","ME4_correct")]
data$ME_Sum <- rowSums(ME,na.rm=FALSE)
data$ME_Any<- ifelse(data$ME_Sum >0,1,0)

#BIO Composite
BIO <- data[,c("BodyParts","ANIMISM_Sum")]
BIO <- scale(BIO)
data$BIOcomp <- rowMeans(scale(BIO),na.rm=FALSE)

#Make model data
mod_data <- data[,c("MOS", 
                    "CRTD",
                    "DCCS","FLANKER","VF","SPAN",
                    "DN","BRS","NFC",
                    "BodyParts","ANIMISM_Sum","BIOcomp","ME_Sum","ME_Any")]   

#Unit scale predictor variables, Biology Composite, & Body Parts Knowledge
pvars <-c("MOS","CRTD","DCCS","FLANKER","VF","SPAN","DN","BRS","NFC","BodyParts","BIOcomp")
scaled_model_data <- mod_data
scaled_model_data[pvars] <- lapply(mod_data[pvars],scale)


####################
###Biology Composite
####################

###Make brms formulas
BIO_Form_no_CRTD <- 
  #Primary BIO model- without CRTD
  bf(BIOcomp ~  mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC)) +
  #Models for data imputation
  bf(MOS | mi() ~  CRTD + mi(NFC)  + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC)+ mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF)+ mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC)  + mi(DN) +mi(BRS)  + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC)  + mi(DN)+mi(BRS)  + mi(VF) +  mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC)+mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

BIO_Form_CRTD <- 
  #Primary BIO model - with CRTD
  bf(BIOcomp ~ mi(MOS) + mi(DCCS)+ mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC)+ mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF)+ mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~  mi(MOS) + CRTD + mi(NFC)  + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~  mi(MOS) + CRTD + mi(NFC)  + mi(DN) + mi(BRS) + mi(VF) +  mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

###Set weakly informative priors
###Betas ~ normal(0,1)
###Intercepts ~ normal(0,5)
###Sigmas ~ exponential(1)
###Note: variables are scaled mean = 0, SD = 1

priors_BIO  <- c(prior(normal(0,1),class = b ),
                  prior(normal(0,5),class = Intercept), 
                  prior(exponential(1),class=sigma,resp=FLANKER),
                  prior(exponential(1),class=sigma,resp=DCCS),
                  prior(exponential(1),class=sigma,resp=MOS),
                  prior(exponential(1),class=sigma,resp=NFC),
                  prior(exponential(1),class=sigma,resp=BRS),
                  prior(exponential(1),class=sigma,resp=DN),
                  prior(exponential(1),class=sigma,resp=SPAN),
                  prior(exponential(1),class=sigma,resp=VF),
                  prior(exponential(1),class=sigma,resp=BIOcomp))

###Fit Full & Control models

fit_BIO_no_CRTD<- brm(BIO_Form_no_CRTD, chains = 4, iter = 4000, prior=priors_BIO, data = scaled_model_data)
fit_BIO_CRTD  <- brm(BIO_Form_CRTD, chains = 4, iter = 4000, prior=priors_BIO, data = scaled_model_data)

###Find similar (or better) submodel via projection predictive selection (projpred)
###Convert brms full model to projpred ref model format

# initialize reference model structure for projpred
drawsBIO_CRTD<- as.matrix(fit_BIO_CRTD) #posterior draws
drawsBIO_CRTD <- drawsBIO_CRTD[,grepl("BIO", colnames(drawsBIO_CRTD))] #subset to relevant parameters
a <- drawsBIO_CRTD[,1] # intercept
b <- drawsBIO_CRTD[,2:(ncol(drawsBIO_CRTD)-1)] # regression coefficients
sigma <- drawsBIO_CRTD[,ncol(drawsBIO_CRTD)] # noise sd
predfun <- function(xt) t( b %*% t(xt) + a ) #prediction function (linear regression)

#make data for projpred loo (complete cases)
BIOprojdata<- scaled_model_data[,c("CRTD","MOS", 
                                   "DCCS",
                                   "FLANKER","VF",
                                   "SPAN", 
                                   "BRS","DN","NFC",
                                   "BIOcomp")]   
BIOprojdata <- na.omit(BIOprojdata)

BIOref <- init_refmodel(as.matrix(BIOprojdata[,1:9]),BIOprojdata[,10], gaussian(), predfun=predfun, dis=sigma) #reference model


#forward projpred variable selection + loo-cv to inform model size
vs_BIO <- cv_varsel(BIOref,nloo=length(BIOprojdata[,10]))

#Select model size
#Modified suggest_size() based on a 2nd decision rule
#1. Choose smallest submodel with 'better' out-of-sample prediction than full model (i.e., elpd_diff/elpd_diff_se is > 2)
#2. If no submodel satisfies rule #1, use default suggest_size() to choose smallest submodel with similar out-of-sample 
#prediction to the full model (i.e., elpd_diff within 1 se)
suggest_size2 <- function(vs_stats_diff){
  comps <- vs_stats_diff$elpd/vs_stats_diff$elpd.se
  sub_mod <- min(which(comps > 2))
  if(sub_mod==Inf){
    sub_mod <- min(which(comps > -1))
  }
  return(sub_mod-1)
}

#projpred results
stats_vs_BIO <- varsel_stats(vs_BIO,deltas=TRUE)  # reference - submodel difference elpds & SEs
bio_size <- suggest_size2(stats_vs_BIO)  #select submodel size
vs_BIO$varsel$vind[1:bio_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_BIO,deltas=TRUE, alpha=.32) #graphical check

#Fit submodel in brms (full data set with missing predictor imputation, facilitates model comparison)
BIO_Form_SUB <- 
  #BIO model
  bf(BIOcomp ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(BRS) + CRTD) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

fit_BIO_SUB  <- brm(BIO_Form_SUB, chains = 4, iter = 4000, prior=priors_BIO, data = scaled_model_data)


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
loo(fit_BIO_no_CRTD,fit_BIO_CRTD,fit_BIO_SUB,resp="BIOcomp")

###Example posterior predictive check (can be done with all models)
#pp_check(fit_BIO_CRTD,newdata=na.omit(fit_BIO_CRTD$data),resp="BIOcomp",nsamples=100)


#######################
###Body Parts Knowledge
#######################

###Make brms formulas

BodyParts_Form_no_CRTD <- 
  #Primary BodyParts model- without CRTD
  bf(BodyParts ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC)) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

BodyParts_Form_CRTD <- 
  #Primary BodyParts model - with CRTD
  bf(BodyParts ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

###Set weakly informative priors
###Betas ~ normal(0,1)
###Intercepts ~ normal(0,5)
###Sigmas ~ exponential(1)
###Note: variables are scaled mean = 0, SD = 1

priors_BodyParts  <- c(prior(normal(0,1),class = b ),
                  prior(normal(0,5),class = Intercept), 
                  prior(exponential(1),class=sigma,resp=FLANKER),
                  prior(exponential(1),class=sigma,resp=DCCS),
                  prior(exponential(1),class=sigma,resp=MOS),
                  prior(exponential(1),class=sigma,resp=NFC),
                  prior(exponential(1),class=sigma,resp=BRS),
                  prior(exponential(1),class=sigma,resp=DN),
                  prior(exponential(1),class=sigma,resp=SPAN),
                  prior(exponential(1),class=sigma,resp=VF),
                  prior(exponential(1),class=sigma,resp=BodyParts))

###Fit Full & Control models

fit_BodyParts_no_CRTD<- brm(BodyParts_Form_no_CRTD, chains = 4, iter = 4000, prior=priors_BodyParts, data = scaled_model_data)
fit_BodyParts_CRTD  <- brm(BodyParts_Form_CRTD, chains = 4, iter = 4000, prior=priors_BodyParts, data = scaled_model_data)

###Find similar (or better) submodel via projection predictive selection (projpred)
###Convert brms full model to projpred ref model format

# initialize the reference model structure
drawsBodyParts_CRTD<- as.matrix(fit_BodyParts_CRTD)
drawsBodyParts_CRTD <- drawsBodyParts_CRTD[,grepl("BodyParts", colnames(drawsBodyParts_CRTD))]
a <- drawsBodyParts_CRTD[,1] # intercept
b <- drawsBodyParts_CRTD[,2:(ncol(drawsBodyParts_CRTD)-1)] # regression coefficients
sigma <- drawsBodyParts_CRTD[,ncol(drawsBodyParts_CRTD)] # noise std
predfun <- function(xt) t( b %*% t(xt) + a ) #prediction function (linear regression)

#make data for projpred loo (complete cases)
BodyPartsprojdata<- scaled_model_data[,c("CRTD","MOS", 
                                    "DCCS",
                                    "FLANKER","VF",
                                    "SPAN", 
                                    "BRS","DN","NFC",
                                    "BodyParts")]   
BodyPartsprojdata <- na.omit(BodyPartsprojdata)

BodyPartsref <- init_refmodel(as.matrix(BodyPartsprojdata[,1:9]),BodyPartsprojdata[,10], gaussian(), predfun=predfun, dis=sigma) #reference model


#forward projpred variable selection + loo-cv to inform model size
vs_BodyParts <- cv_varsel(BodyPartsref,nloo=length(BodyPartsprojdata[,10]))

#projpred results
stats_vs_BodyParts <- varsel_stats(vs_BodyParts,deltas=TRUE)  # reference - submodel difference elpds & SEs
BodyParts_size <- suggest_size2(stats_vs_BodyParts)  #select submodel size
vs_BodyParts$varsel$vind[1:BodyParts_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_BodyParts,deltas=TRUE, alpha=.32) #graphical check

#Fit submodel in brms (full data set via missing predictor imputation, facilitates model comparison)
BodyParts_Form_SUB <- 
  #BodyParts model
  bf(BodyParts  ~  mi(MOS) + mi(VF) + CRTD + mi(DCCS)) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

fit_BodyParts_SUB  <- brm(BodyParts_Form_SUB, chains = 4, iter = 4000, prior=priors_BodyParts, data = scaled_model_data)


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
loo(fit_BodyParts_no_CRTD,fit_BodyParts_CRTD,fit_BodyParts_SUB,resp="BodyParts") 

###Example posterior predictive check (can be done with all models)
#pp_check(fit_BodyParts_CRTD,newdata=na.omit(fit_BodyParts_CRTD$data),resp="BodyParts",nsamples=100)


####################################
###Living Things Knowledge (Animism)
####################################

###Make scaled data for multilevel model (melt by category)
mod_data_Animism <- data[,c("ResponseId","MOS", 
                     "CRTD","NFC","BRS","DN",
                     "VF","SPAN","DCCS","FLANKER","ANIMALS","PLANTS","NATURAL","ARTIFACTS")]   

pvars <-c("MOS","CRTD","BRS","NFC","DCCS","FLANKER","SPAN","DN","VF")

scaled_model_data_Animism <- mod_data_Animism
scaled_model_data_Animism[pvars] <- lapply(mod_data_Animism[pvars],scale)
scaled_model_data_Animism <- melt(scaled_model_data_Animism, variable.name = "category",value.name = "ANIMISM",
                           id=c("ResponseId","MOS","CRTD","BRS","NFC","DCCS","FLANKER","SPAN","DN","VF"))

###Make brms formulas

ANIMISM_Form_no_CRTD <- 
  #Primary ANIMISM
  bf(ANIMISM ~  mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + (1|category) + (1|ResponseId), family=bernoulli()) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

ANIMISM_Form_CRTD <- 
  #Primary ANIMISM
  bf(ANIMISM ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD + (1|category) + (1|ResponseId), family=bernoulli()) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

###Set weakly informative priors
###Animism Intercept & Betas ~ normal(0,2.5)
###Animism sd & Imputation Sigmas ~ exponential(1)
###Imputation Betas ~ normal(0,1)
###Imputation Intercepts ~ normal(0,5)
###Note: predictor variables are scaled mean = 0, SD = 1

priors_ANIMISM<-c(prior(normal(0,2.5), class = b, resp=ANIMISM),
                  prior(normal(0,2.5), class = Intercept, resp=ANIMISM),
                  prior(exponential(1), class = sd, resp=ANIMISM),
                  prior(normal(0,1), class=b, resp=DCCS),
                  prior(normal(0,1), class=b, resp=FLANKER),
                  prior(normal(0,1), class=b, resp=MOS),
                  prior(normal(0,1), class=b, resp=NFC),
                  prior(normal(0,1), class=b, resp=DN),
                  prior(normal(0,1), class=b, resp=BRS),
                  prior(normal(0,1), class=b, resp=SPAN),
                  prior(normal(0,1), class=b, resp=VF),
                  prior(normal(0,5), class=Intercept, resp=DCCS),
                  prior(normal(0,5), class=Intercept, resp=FLANKER),
                  prior(normal(0,5), class=Intercept, resp=MOS),
                  prior(normal(0,5), class=Intercept, resp=NFC),
                  prior(normal(0,5), class=Intercept, resp=DN),
                  prior(normal(0,5), class=Intercept, resp=BRS),
                  prior(normal(0,5), class=Intercept, resp=SPAN),
                  prior(normal(0,5), class=Intercept, resp=VF),
                  prior(exponential(1), class=sigma, resp=DCCS),
                  prior(exponential(1), class=sigma, resp=FLANKER),
                  prior(exponential(1), class=sigma, resp=MOS),
                  prior(exponential(1), class=sigma, resp=NFC),
                  prior(exponential(1), class=sigma, resp=DN),
                  prior(exponential(1), class=sigma, resp=BRS),
                  prior(exponential(1), class=sigma, resp=SPAN),
                  prior(exponential(1), class=sigma, resp=VF))

###Fit Full & Control models

fit_ANIMISM_no_CRTD<- brm(ANIMISM_Form_no_CRTD, chains = 4, iter = 4000, prior=priors_ANIMISM, data = scaled_model_data_Animism,
                         control = list(adapt_delta = 0.99))
fit_ANIMISM_CRTD <- brm(ANIMISM_Form_CRTD, chains = 4, iter = 4000, prior=priors_ANIMISM, data = scaled_model_data_Animism,
                       control = list(adapt_delta = 0.99))


###Find similar (or better) submodel via projection predictive selection (projpred)
###Projection predictive selection has not yet been implemented for multilevel models.
###Thus, fit a related model with complete pooling 

pseudo_ANIMISM_Form_CRTD <- 
  #Primary ANIMISM
  bf(ANIMISM_Sum ~  mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD, family=binomial()) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

###Set weakly informative priors
###Animism Intercept & Betas ~ normal(0,2.5)
##Imputation Sigmas ~ exponential(1)
###Imputation Betas ~ normal(0,1)
###Imputation Intercepts ~ normal(0,5)
###Note: predictor variables are scaled mean = 0, SD = 1

pseudo_priors_ANIMISM<-c(prior(normal(0,2.5), class = b, resp=ANIMISMSum),
                         prior(normal(0,2.5), class = Intercept, resp=ANIMISMSum),
                         prior(normal(0,1), class=b, resp=DCCS),
                         prior(normal(0,1), class=b, resp=FLANKER),
                         prior(normal(0,1), class=b, resp=MOS),
                         prior(normal(0,1), class=b, resp=NFC),
                         prior(normal(0,1), class=b, resp=DN),
                         prior(normal(0,1), class=b, resp=BRS),
                         prior(normal(0,1), class=b, resp=SPAN),
                         prior(normal(0,1), class=b, resp=VF),
                         prior(normal(0,5), class=Intercept, resp=DCCS),
                         prior(normal(0,5), class=Intercept, resp=FLANKER),
                         prior(normal(0,5), class=Intercept, resp=MOS),
                         prior(normal(0,5), class=Intercept, resp=NFC),
                         prior(normal(0,5), class=Intercept, resp=DN),
                         prior(normal(0,5), class=Intercept, resp=BRS),
                         prior(normal(0,5), class=Intercept, resp=SPAN),
                         prior(normal(0,5), class=Intercept, resp=VF),
                         prior(exponential(1), class=sigma, resp=DCCS),
                         prior(exponential(1), class=sigma, resp=FLANKER),
                         prior(exponential(1), class=sigma, resp=MOS),
                         prior(exponential(1), class=sigma, resp=NFC),
                         prior(exponential(1), class=sigma, resp=DN),
                         prior(exponential(1), class=sigma, resp=BRS),
                         prior(exponential(1), class=sigma, resp=SPAN),
                         prior(exponential(1), class=sigma, resp=VF))

#Fit pseudo-Full model
pseudo_fit_ANIMISM_CRTD <- brm(pseudo_ANIMISM_Form_CRTD, chains = 4, iter = 4000,  prior=pseudo_priors_ANIMISM, data = scaled_model_data)

# initialize pseudo reference model structure for projpred
drawsAN_CRTD<- as.matrix(pseudo_fit_ANIMISM_CRTD)
drawsAN_CRTD <- drawsAN_CRTD[,grepl("ANIMISM", colnames(drawsAN_CRTD))]
drawsAN_CRTD <- drawsAN_CRTD[,1:10]
a <- drawsAN_CRTD[,1] # intercept
b <- drawsAN_CRTD[,2:(ncol(drawsAN_CRTD))] # regression coefficients
predfun <- function(xt) t( binomial()$linkinv(b %*% t(xt) + a) )  #prediction function (logistic regression)

#make data for projpred loo (complete cases)
ANprojdata<- scaled_model_data[,c("CRTD","MOS", 
                                  "DCCS",
                                  "FLANKER","VF",
                                  "SPAN", 
                                  "BRS","DN","NFC",
                                  "ANIMISM_Sum")]   
ANprojdata <- na.omit(ANprojdata)
y <- ANprojdata[,10]/4  #make repeated trials useable for projpred
w <- rep(4,length(y)) 

ANref <- init_refmodel(as.matrix(ANprojdata[,1:9]),y,wobs=w, binomial(), predfun=predfun)

#forward projpred variable selection + loo-cv to inform model size
vs_AN <- cv_varsel(ANref,nloo=length(ANprojdata[,10]))

#projpred results
stats_vs_AN <- varsel_stats(vs_AN,deltas=TRUE)  # reference - submodel difference elpds & SEs
AN_size <- suggest_size2(stats_vs_AN)  #select submodel size
vs_AN$varsel$vind[1:AN_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_AN,deltas=TRUE, alpha=.32)  #graphical check


#Fit submodel in brms (full data set with missing predictor imputation, facilitates model comparison)
ANIMISM_Form_SUB <- 
  #Primary ANIMISM
  bf(ANIMISM ~ CRTD +  mi(FLANKER) + mi(SPAN) + (1|category) + (1|ResponseId),family=bernoulli()) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

fit_ANIMISM_SUB <- brm(ANIMISM_Form_SUB, chains = 4, iter = 4000, prior=priors_ANIMISM, data = scaled_model_data_Animism,
                       control = list(adapt_delta = 0.95))


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
loo(fit_ANIMISM_no_CRTD,fit_ANIMISM_CRTD,fit_ANIMISM_SUB,resp="ANIMISM") 

###Example posterior predictive check (can be done with all models)
#pp_check(fit_ANIMISM_CRTD,newdata=na.omit(fit_ANIMISM_CRTD$data),resp="ANIMISM",nsamples=100)


#################################
###Mathematical Equivalence###
#################################

#make brms formulas

ME_Form_no_CRTD <- 
  #Primary ME model - No CRTD
  bf(ME_Any~  mi(MOS) + mi(FLANKER)+ mi(VF) + mi(SPAN) + mi(DN) + mi(BRS) + mi(NFC) + mi(DCCS), family=bernoulli) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

ME_Form_CRTD <- 
  #Primary ME model - CRTD
  bf(ME_Any ~ CRTD + mi(MOS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(DN) + mi(BRS) + mi(NFC) + mi(DCCS), family=bernoulli) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

###Set weakly informative priors
###ME Intercept & Betas ~ normal(0,2.5)
###Imputation Betas ~ normal(0,1)
###Imputation Intercepts ~ normal(0,5)
###Imputation Sigmas ~ exponential(1)
###Note: predictor variables are scaled mean = 0, SD = 1

priors_ME     <-c(prior(normal(0,2.5), class = b, resp=MEAny),
                  prior(normal(0,2.5), class = Intercept, resp=MEAny),
                  prior(normal(0,1), class=b, resp=DN),
                  prior(normal(0,1), class=b, resp=BRS),
                  prior(normal(0,1), class=b, resp=DCCS),
                  prior(normal(0,1), class=b, resp=FLANKER),
                  prior(normal(0,1), class=b, resp=MOS),
                  prior(normal(0,1), class=b, resp=NFC),
                  prior(normal(0,1), class=b, resp=SPAN),
                  prior(normal(0,1), class=b, resp=VF),
                  prior(normal(0,5), class=Intercept, resp=DN),
                  prior(normal(0,5), class=Intercept, resp=BRS),
                  prior(normal(0,5), class=Intercept, resp=DCCS),
                  prior(normal(0,5), class=Intercept, resp=FLANKER),
                  prior(normal(0,5), class=Intercept, resp=MOS),
                  prior(normal(0,5), class=Intercept, resp=NFC),
                  prior(normal(0,5), class=Intercept, resp=SPAN),
                  prior(normal(0,5), class=Intercept, resp=VF),
                  prior(exponential(1), class=sigma, resp=DN),
                  prior(exponential(1), class=sigma, resp=BRS),
                  prior(exponential(1), class=sigma, resp=DCCS),
                  prior(exponential(1), class=sigma, resp=FLANKER),
                  prior(exponential(1), class=sigma, resp=MOS),
                  prior(exponential(1), class=sigma, resp=NFC),
                  prior(exponential(1), class=sigma, resp=SPAN),
                  prior(exponential(1), class=sigma, resp=VF))

###Fit Full & Control Models
fit_ME_no_CRTD <- brm(ME_Form_no_CRTD, chains = 4, iter = 4000, prior=priors_ME, data = scaled_model_data)
fit_ME_CRTD<- brm(ME_Form_CRTD, chains = 4, iter = 4000, prior=priors_ME,data = scaled_model_data)


###Find similar (or better) submodel via projection predictive selection (projpred)
###Convert brms full model to projpred ref model format

# initialize reference model structure for projpred
drawsME_CRTD<- as.matrix(fit_ME_CRTD)
drawsME_CRTD <- drawsME_CRTD[,grepl("MEAny", colnames(drawsME_CRT))]  #subset to relevant parameters
a <- drawsME_CRTD[,1] # intercept
b <- drawsME_CRTD[,2:(ncol(drawsME_CRTD))] # regression coefficients
predfun <- function(xt) t( binomial()$linkinv(b %*% t(xt) + a) ) # prediction function (logistic regression)

#make data for projpred loo (complete cases)
MEprojdata<- scaled_model_data[,c("CRTD","MOS", 
                                  "FLANKER",
                                  "VF","SPAN",
                                  "DN", 
                                  "BRS","NFC","DCCS",
                                  "ME_Any")]   
MEprojdata <- na.omit(MEprojdata)

MEref <- init_refmodel(as.matrix(MEprojdata[,1:9]),MEprojdata[,10], binomial(), predfun=predfun) #reference model

#forward projpred variable selection + loo-cv to inform model size
vs_ME <- cv_varsel(MEref,nloo=length(MEprojdata[,10]))

#projpred results
stats_vs_ME <- varsel_stats(vs_ME,deltas=TRUE)  # reference - submodel difference elpds & ses
ME_size <- suggest_size2(stats_vs_ME)   #select submodel size
vs_ME$varsel$vind[1:ME_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_ME,deltas=TRUE, alpha=.32)  #graphical check

###Fit submodel in brms (on full data set via missing predictor imputation, facilitates model comparison)

ME_Form_SUB <- 
  #Primary ME submodel
  bf(ME_Any ~ CRTD + mi(NFC) + mi(DCCS),family=bernoulli) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

fit_ME_SUB<- brm(ME_Form_SUB,  chains = 4, iter = 4000, prior=priors_ME,data = scaled_model_data)


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
loo(fit_ME_no_CRTD,fit_ME_CRTD,fit_ME_SUB,resp="MEAny") 



##################################################
####Summary model coefficient plots (i.e., Fig. 2)
##################################################

###Bio Comp Summary
tidy_fit_BIO_CRTD<-tidy(fit_BIO_CRTD,prob=.95)
tidy_fit_BIO_CRTD<- filter(tidy_fit_BIO_CRTD,grepl('BIOcomp', term))
tidy_fit_BIO_CRTD <- tidy_fit_BIO_CRTD %>% filter(term != "b_BIOcomp_Intercept",term != "sigma_BIOcomp")

tidy_fit_BIO_CRTD$term <- revalue(tidy_fit_BIO_CRTD$term, c("b_BIOcomp_CRTD"="CRT-D", 
                                                            "bsp_BIOcomp_miMOS"="AGE", 
                                                            "bsp_BIOcomp_miDCCS"="DCCS",
                                                            "bsp_BIOcomp_miVF"="VF",
                                                            "bsp_BIOcomp_miSPAN"="WM",
                                                            "bsp_BIOcomp_miFLANKER"="FLANKER",
                                                            "bsp_BIOcomp_miNFC"="NFC",
                                                            "bsp_BIOcomp_miBRS"="BRS",
                                                            "bsp_BIOcomp_miDN"="DN"))
tidy_fit_BIO_CRTD$term  <- factor(tidy_fit_BIO_CRTD$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))


BIO_Coeff_fig <- ggplot(tidy_fit_BIO_CRTD, aes(x = estimate, y = term, 
                                              xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (SDs)") + ylab("") +
  
  ggtitle("A. Biology Composite") +
  xlim(-.5, .5) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)

BIO_Coeff_fig
#ggsave("BIO_coef.pdf", width = 3.25, height = 3)


###Body Parts Summary
tidy_fit_BodyParts_CRTD<-tidy(fit_BodyParts_CRTD,prob=.95)
tidy_fit_BodyParts_CRTD<- filter(tidy_fit_BodyParts_CRTD,grepl('BodyParts', term))
tidy_fit_BodyParts_CRTD <- tidy_fit_BodyParts_CRTD %>% filter(term != "b_BodyParts_Intercept",term != "sigma_BodyParts")

tidy_fit_BodyParts_CRTD$term <- revalue(tidy_fit_BodyParts_CRTD$term, c("b_BodyParts_CRTD"="CRT-D", 
                                                            "bsp_BodyParts_miMOS"="AGE", 
                                                            "bsp_BodyParts_miDCCS"="DCCS",
                                                            "bsp_BodyParts_miVF"="VF",
                                                            "bsp_BodyParts_miSPAN"="WM",
                                                            "bsp_BodyParts_miFLANKER"="FLANKER",
                                                            "bsp_BodyParts_miNFC"="NFC",
                                                            "bsp_BodyParts_miBRS"="BRS",
                                                            "bsp_BodyParts_miDN"="DN"))
tidy_fit_BodyParts_CRTD$term  <- factor(tidy_fit_BodyParts_CRTD$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))


BP_Coeff_fig <- ggplot(tidy_fit_BodyParts_CRTD, aes(x = estimate, y = term, 
                       xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (SDs)") + ylab("") +

  ggtitle("B. Body Parts Knowledge") +
  xlim(-.5, .5) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)

BP_Coeff_fig
#ggsave("BP_coef.pdf", width = 3.25, height = 3 )


###Living Things Summary
tidy_fit_ANIMISM_CRTD<-tidy(fit_ANIMISM_CRTD,prob=.95)
tidy_fit_ANIMISM_CRTD<- filter(tidy_fit_ANIMISM_CRTD,grepl('ANIMISM', term))
tidy_fit_ANIMISM_CRTD <- tidy_fit_ANIMISM_CRTD[2:10,]

tidy_fit_ANIMISM_CRTD$term <- revalue(tidy_fit_ANIMISM_CRTD$term, c("b_ANIMISM_CRTD"="CRT-D", 
                                              "bsp_ANIMISM_miMOS"="AGE", 
                                              "bsp_ANIMISM_miDCCS"="DCCS",
                                              "bsp_ANIMISM_miFLANKER"="FLANKER",
                                              "bsp_ANIMISM_miVF"="VF",
                                              "bsp_ANIMISM_miSPAN"="WM",
                                              "bsp_ANIMISM_miBRS"="BRS",
                                              "bsp_ANIMISM_miDN"="DN",
                                              "bsp_ANIMISM_miNFC"="NFC"))

tidy_fit_ANIMISM_CRTD$term  <- factor(tidy_fit_ANIMISM_CRTD$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))

AN_fig <- ggplot(tidy_fit_ANIMISM_CRTD, aes(x = estimate, y = term, #colour = model, shape = model,fill=zero,
                       xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(position = position_dodgev(height = .6),size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (log odds)") + ylab("") +
  ggtitle("C. Living Things Knowledge") +
  xlim(-1, 1) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)

AN_fig
#ggsave("AN_coef.pdf", width = 3.25, height = 3)


###Mathematical Equivalence Summary
tidy_fit_ME_CRTD<-tidy(fit_ME_CRTD,prob=.95)
tidy_fit_ME_CRTD<- filter(tidy_fit_ME_CRTD,grepl('MEAny', term))
tidy_fit_ME_CRTD <- tidy_fit_ME_CRTD[2:10,]
tidy_fit_ME_CRTD <- tidy_fit_ME_CRTD%>% filter(term != "b_MEAnyIntercept") 

tidy_fit_ME_CRTD$term <- revalue(tidy_fit_ME_CRTD$term, c("b_MEAny_CRTD"="CRT-D", 
                                                        "bsp_MEAny_miMOS"="AGE", 
                                                        "bsp_MEAny_miDCCS"="DCCS",
                                                        "bsp_MEAny_miFLANKER"="FLANKER",
                                                        "bsp_MEAny_miVF"="VF",
                                                        "bsp_MEAny_miSPAN"="WM",
                                                        "bsp_MEAny_miBRS"="BRS",
                                                        "bsp_MEAny_miDN"="DN",
                                                        "bsp_MEAny_miNFC"="NFC"))

tidy_fit_ME_CRTD$term  <- factor(tidy_fit_ME_CRTD$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))

ME_fig <- ggplot(tidy_fit_ME_CRTD, aes(x = estimate, y = term,
                       xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(position = position_dodgev(height = .6),size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (log odds)") + ylab("") +
  ggtitle("D. Mathematical Equivalence") +
  xlim(-4, 4) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)

ME_fig
#ggsave("ME_coef.pdf", width = 3.25, height = 3)

