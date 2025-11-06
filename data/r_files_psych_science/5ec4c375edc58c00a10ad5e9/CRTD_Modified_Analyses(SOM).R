###Children’s Cognitive Reflection Predicts Conceptual Understanding in Science and Mathematics 
###Bayesian regression analyses with modified CRTDs summarized in main text and reported in supplemental material
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
library(bayestestR)

options(mc.cores = parallel::detectCores()) 

########################################################
###Load data, make variables, scale model data
########################################################

#Data is available at https://osf.io/e72ka/?view_only=57c8b2f8cf7b4193af2df40af14c71d7
data=read.csv("CRTD_DATA.csv")

#CRTD Vitalist Biology Outcomes
CRTD<- data[,c("crt.cows","crt.emily","crt.butterfly",
              "crt.pound","crt.sheep","crt.santa",
              "crt.race","crt.apples","crt.mo")]
data$CRTD <- rowSums(CRTD)

#Modified CRTD for Vitalist Biology Outcomes
CRTD_bio<- data[,c("crt.emily",  
               "crt.pound","crt.sheep","crt.santa",
               "crt.race","crt.apples","crt.mo")]
data$CRTD_bio <- rowSums(CRTD_bio)

#Modified CRTD for Mathematical Equivalence
CRTD_math<- data[,c("crt.cows","crt.emily","crt.butterfly",
               "crt.pound","crt.santa",
               "crt.race","crt.mo")]
data$CRTD_math <- rowSums(CRTD_math)

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
                    "CRTD","CRTD_bio","CRTD_math",
                    "DCCS","FLANKER","VF","SPAN",
                    "DN","BRS","NFC",
                    "BodyParts","ANIMISM_Sum","BIOcomp","ME_Any")]   


#Unit scale predictor variables, Biology Composite, & Body Parts Knowledge
pvars <-c("MOS","CRTD","DCCS","FLANKER","VF","SPAN","DN","BRS","NFC","BodyParts","BIOcomp",
          "CRTD_bio", "CRTD_math")
scaled_model_data <- mod_data
scaled_model_data[pvars] <- lapply(mod_data[pvars],scale)


####################
###Biology Composite
####################

#make brms formulas

BIO_Form_no_CRTD_alt <- 
  #Primary BIO model- without modified CRTD
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

BIO_Form_CRTD_alt <- 
  #Primary BIO model - with modified CRTD
  bf(BIOcomp ~ mi(MOS) + mi(DCCS)+ mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD_bio) +
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

###fit2 Full & Control models
fit2_BIO_no_CRTD<- brm(BIO_Form_no_CRTD_alt, chains = 4, iter = 4000, prior=priors_BIO, data = scaled_model_data)
fit2_BIO_CRTD  <- brm(BIO_Form_CRTD_alt, chains = 4, iter = 4000, prior=priors_BIO, data = scaled_model_data)

###Find similar (or better) submodel via projection predictive selection (projpred)
###Convert brms full model to projpred ref model format

# initialize reference model structure for projpred
drawsBIO_CRTD<- as.matrix(fit2_BIO_CRTD) #posterior draws
drawsBIO_CRTD <- drawsBIO_CRTD[,grepl("BIO", colnames(drawsBIO_CRTD))] #subset to relevant parameters
a <- drawsBIO_CRTD[,1] # intercept
b <- drawsBIO_CRTD[,2:(ncol(drawsBIO_CRTD)-1)] # regression coefficients
sigma <- drawsBIO_CRTD[,ncol(drawsBIO_CRTD)] # noise sd
predfun <- function(xt) t( b %*% t(xt) + a ) #prediction function (linear regression)

#make data for projpred loo (complete cases)
BIOprojdata<- scaled_model_data[,c("CRTD_bio","MOS", 
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
#vs_BIO$varsel$vind[1:bio_size]  #the selected submodel, variables ordered by predictive relevance 
vs_BIO$vind[1:bio_size] 
varsel_plot(vs_BIO,deltas=TRUE, alpha=.32) #graphical check

#fit submodel in brms (full data set with missing predictor imputation, facilitates model comparison)
BIO_Form_SUB_alt <- 
  #BIO submodel
  bf(BIOcomp ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(SPAN) + mi(VF) + mi(BRS) + CRTD_bio) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD_bio + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD_bio + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD_bio + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD_bio + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD_bio + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD_bio + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD_bio + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD_bio + mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
  #Set residual correlations for the models to zero
  set_rescor(FALSE)

fit2_BIO_SUB_alt  <- brm(BIO_Form_SUB_alt, chains = 4, iter = 4000, prior=priors_BIO, data = scaled_model_data)


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
bl1 <- loo(fit2_BIO_no_CRTD_alt,fit2_BIO_CRTD_alt,fit2_BIO_SUB_alt,resp="BIOcomp")
bl1$ic_diffs__

#Probability positive
p_direction(fit2_BIO_CRTD_alt) %>% filter(Parameter==	"b_BIOcomp_CRTD_bio")

#######################
###Body Parts Knowledge
#######################

###Make brms formulas

BodyParts_Form_no_CRTD_bio_alt <- 
  #Primary Bodyparts model- without modified CRTD
  bf(BodyParts ~  mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC)) +
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

BodyParts_Form_CRTD_bio_alt<- 
  #Primary BodyParts model - with modified CRTD
  bf(BodyParts  ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD_bio ) +
  #Models for data imputation
  bf(MOS | mi() ~ CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(NFC | mi() ~ mi(MOS) + CRTD + mi(DN) + mi(VF) + mi(SPAN) + mi(BRS) + mi(FLANKER) + mi(DCCS)) + 
  bf(DCCS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(VF) + mi(BRS) + mi(SPAN) + mi(FLANKER)) + 
  bf(DN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(SPAN) + mi(BRS)  + mi(FLANKER) + mi(DCCS)) + 
  bf(BRS | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(VF) + mi(DN) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(VF | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN) + mi(BRS) + mi(SPAN) + mi(FLANKER) + mi(DCCS)) + 
  bf(SPAN | mi() ~ mi(MOS) + CRTD + mi(NFC) + mi(DN)+mi(BRS) + mi(VF) + mi(FLANKER) + mi(DCCS)) + 
  bf(FLANKER | mi() ~ mi(MOS) + CRTD+ mi(NFC) + mi(BRS) + mi(DN) + mi(VF) + mi(SPAN) + mi(DCCS)) + 
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

###fit Full & Control models

fit2_BodyParts_no_CRTD<- brm(BodyParts_Form_no_CRTD_bio_alt, chains = 4, iter = 4000, prior=priors_BodyParts, data = scaled_model_data)
fit2_BodyParts_CRTD  <- brm(BodyParts_Form_CRTD_bio_alt, chains = 4, iter = 4000, prior=priors_BodyParts, data = scaled_model_data)


###Find similar (or better) submodel via projection predictive selection (projpred)
###Convert brms full model to projpred ref model format

# initialize the reference model structure
drawsBodyParts_CRTD <- as.matrix(fit2_BodyParts_CRTD)
drawsBodyParts_CRTD <- drawsBodyParts_CRTD[,grepl("BodyParts", colnames(drawsBodyParts_CRTD))]
a <- drawsBodyParts_CRTD[,1] # intercept
b <- drawsBodyParts_CRTD[,2:(ncol(drawsBodyParts_CRTD)-1)] # regression coefficients
sigma <- drawsBodyParts_CRTD[,ncol(drawsBodyParts_CRTD)] # noise std
predfun <- function(xt) t( b %*% t(xt) + a ) #prediction function (linear regression)

#make data for projpred loo (complete cases)
BodyPartsprojdata<- scaled_model_data[,c("CRTD_bio","MOS", 
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
#vs_BodyParts$varsel$vind[1:BodyParts_size]  #the selected submodel, variables ordered by predictive relevance 
vs_BodyParts$vind[1:BodyParts_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_BodyParts,deltas=T, alpha=.32) #graphical check

#fit submodel in brms (full data set via missing predictor imputation, facilitates model comparison)
BodyParts_Form_SUB <- 
  #BodyParts submodel
  bf(BodyParts  ~  mi(MOS) + mi(VF) + CRTD_bio  + mi(DCCS) + mi(BRS)) +
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

fit2_BodyParts_SUB  <- brm(BodyParts_Form_SUB, chains = 4, iter = 4000, prior=priors_BodyParts, data = scaled_model_data)


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
bl2 <- loo(fit2_BodyParts_no_CRTD,fit2_BodyParts_CRTD,fit2_BodyParts_SUB,resp="BodyParts") 
bl2$ic_diffs__

#Probability positive
p_direction(fit2_BodyParts_CRTD) %>% filter(Parameter==	"b_BodyParts_CRTD_bio")
eti(fit2_BodyParts_CRTD, ci = c(.89))


####################################
###Living Things Knowledge (Animism)
####################################

###Make scaled data for multilevel model (melt by category)
mod_data_Animism <- data[,c("ResponseId","MOS", 
                     "CRTD","NFC","BRS","DN","CRTD_bio",
                     "VF","SPAN","DCCS","FLANKER","ANIMALS","PLANTS","NATURAL","ARTIFACTS")]   

pvars <-c("MOS","CRTD","BRS","NFC","DCCS","FLANKER","SPAN","DN","VF","CRTD_bio")

scaled_model_data_Animism <- mod_data_Animism
scaled_model_data_Animism[pvars] <- lapply(mod_data_Animism[pvars],scale)
scaled_model_data_Animism <- melt(scaled_model_data_Animism, variable.name = "category",value.name = "ANIMISM",
                           id=c("ResponseId","MOS","CRTD","BRS","NFC","DCCS","FLANKER","SPAN","DN","VF","CRTD_bio"))

###Make brms formulas

ANIMISM_Form_no_CRTD_alt <- 
  #Primary Animism model- without modified CRTD
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

ANIMISM_Form_CRTD_alt <- 
  #Primary Animism model- with modified CRTD
  bf(ANIMISM ~ mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD_bio + (1|category) + (1|ResponseId), family=bernoulli()) +
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

###fit Full & Control models

fit2_ANIMISM_no_CRTD<- brm(ANIMISM_Form_no_CRTD_alt, chains = 4, iter = 4000, prior=priors_ANIMISM, data = scaled_model_data_Animism,
                         control = list(adapt_delta = 0.99))
fit2_ANIMISM_CRTD <- brm(ANIMISM_Form_CRTD_alt, chains = 4, iter = 4000, prior=priors_ANIMISM, data = scaled_model_data_Animism,
                       control = list(adapt_delta = 0.99))


###Find similar (or better) submodel via projection predictive selection (projpred)
###Projection predictive selection has not yet been implemented for multilevel models.
###Thus, fit a related model with complete pooling 

pseudo_ANIMISM_Form_CRTD_alt <- 
  #Pseudo model
  bf(ANIMISM_Sum ~  mi(MOS) + mi(DCCS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(BRS) + mi(DN) + mi(NFC) + CRTD_bio, family=binomial()) +
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

#fit pseudo-Full model
pseudo_fit2_ANIMISM_CRTD <- brm(pseudo_ANIMISM_Form_CRTD_alt, chains = 4, iter = 4000,  prior=pseudo_priors_ANIMISM, data = scaled_model_data)

# initialize pseudo reference model structure for projpred
drawsAN_CRTD<- as.matrix(pseudo_fit2_ANIMISM_CRTD)
drawsAN_CRTD <- drawsAN_CRTD[,grepl("ANIMISM", colnames(drawsAN_CRTD))]
drawsAN_CRTD <- drawsAN_CRTD[,1:10]
a <- drawsAN_CRTD[,1] # intercept
b <- drawsAN_CRTD[,2:(ncol(drawsAN_CRTD))] # regression coefficients
predfun <- function(xt) t( binomial()$linkinv(b %*% t(xt) + a) )  #prediction function (logistic regression)

#make data for projpred loo (complete cases)
ANprojdata<- scaled_model_data[,c("CRTD_bio","MOS", 
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
#vs_AN$varsel$vind[1:AN_size]  #the selected submodel, variables ordered by predictive relevance 
vs_AN$vind[1:AN_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_AN,deltas=TRUE, alpha=.32)  #graphical check


#fit submodel in brms (full data set with missing predictor imputation, facilitates model comparison)
ANIMISM_Form_SUB_alt <- 
  #Primary Animism submodel
  bf(ANIMISM ~ CRTD_bio +  mi(FLANKER) + mi(SPAN) + (1|category) + (1|ResponseId),family=bernoulli()) +
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

fit2_ANIMISM_SUB <- brm(ANIMISM_Form_SUB_alt, chains = 4, iter = 4000, prior=priors_ANIMISM, data = scaled_model_data_Animism,
                       control = list(adapt_delta = 0.95))


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
bl3 <- loo(fit2_ANIMISM_no_CRTD,fit2_ANIMISM_CRTD,fit2_ANIMISM_SUB,resp="ANIMISM") 
bl3$ic_diffs__

#Probability positive
p_direction(fit2_ANIMISM_CRTD) %>% filter(Parameter==	"b_ANIMISM_CRTD_bio")


#################################
###Mathematical Equivalence###
#################################

#make brms formulas

ME_Form_no_CRTD_alt <- 
  #Primary ME model - without modified CRTD
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

ME_Form_CRTD_alt <- 
  #Primary ME model - with modified CRTD
  bf(ME_Any ~ CRTD_math + mi(MOS) + mi(FLANKER) + mi(VF) + mi(SPAN) + mi(DN) + mi(BRS) + mi(NFC) + mi(DCCS), family=bernoulli) +
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

###fit2 Full & Control Models
fit2_ME_no_CRTD <- brm(ME_Form_no_CRTD_alt, chains = 4, iter = 4000, prior=priors_ME, data = scaled_model_data)
fit2_ME_CRTD<- brm(ME_Form_CRTD_alt, chains = 4, iter = 4000, prior=priors_ME,data = scaled_model_data)

###Find similar (or better) submodel via projection predictive selection (projpred)
###Convert brms full model to projpred ref model format

# initialize reference model structure for projpred
drawsME_CRTD <- as.matrix(fit2_ME_CRTD)
drawsME_CRTD <- drawsME_CRTD[,grepl("MEAny", colnames(drawsME_CRTD))]  #subset to relevant parameters
a <- drawsME_CRTD[,1] # intercept
b <- drawsME_CRTD[,2:(ncol(drawsME_CRTD))] # regression coefficients
predfun <- function(xt) t( binomial()$linkinv(b %*% t(xt) + a) ) # prediction function (logistic regression)

#make data for projpred loo (complete cases)
MEprojdata<- scaled_model_data[,c("CRTD_math","MOS", 
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
#vs_ME$varsel$vind[1:ME_size]  #the selected submodel, variables ordered by predictive relevance 
vs_ME$vind[1:ME_size]  #the selected submodel, variables ordered by predictive relevance 
varsel_plot(vs_ME,deltas=TRUE, alpha=.32)  #graphical check

###fit2 submodel in brms (on full data set via missing predictor imputation, facilitates model comparison)

ME_Form_SUB_alt <- 
  #Primary ME submodel
  bf(ME_Any ~ CRTD_math + mi(NFC) + mi(DCCS),family=bernoulli) +
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

fit2_ME_SUB<- brm(ME_Form_SUB_alt,  chains = 4, iter = 4000, prior=priors_ME,data = scaled_model_data)


###LOO Model Comparison: NO_CRTD, CRTD, SUB 
ml <- loo(fit2_ME_no_CRTD,fit2_ME_CRTD,fit2_ME_SUB,resp="MEAny") 
ml$ic_diffs__

#Probability positive
p_direction(fit2_ME_CRTD) %>% filter(Parameter==	"b_MEAny_CRTD_math")




