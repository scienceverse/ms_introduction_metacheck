# ----------------------------------------------------------------------------------------------------------------------
# Program: Saturated model 
# Author: Laura W. Wesseldijk 
# Date: 07 03 2019

## 1. I first test for sex differences univariately per phenotype
## 2. I test for sex differences in a bivarite saturated model, to constrain the cross-trait-cross-twin correlations
## 3. I test the sibling correlations and constrain them to be equal to the twins

# ----------------------------------------------------------------------------------------------------------------------
getwd()
setwd("               ")
data_all <-read.table("Datafile_sex_drugs_twins_plus_sibs_age.dat",na.strings=c("-999"),dec=",", header=T) 
colnames(data_all)

names(data_all)[1]<-paste("zyg")
names(data_all)[4]<-paste("drugv1")
names(data_all)[5]<-paste("drugv2")
names(data_all)[10]<-paste("SOIo1")
names(data_all)[11]<-paste("SOIo2")

names(data_all)[31]<-paste("TDDSsex1")
names(data_all)[32]<-paste("TDDSsex2")
names(data_all)[25]<-paste("sibSOIo")
names(data_all)[33]<-paste("sibTDDSsex")
names(data_all)[22]<-paste("sibdrugv")
colnames(data_all)



require(OpenMx)
require (psych)
require(polycor)


##############################################################################
####################### UNIVARIATE ############################################
##############################################################################

Varnames=colnames(data_all)[c(4,5)] # change per phenotype
mzData <- subset(data_all, zyg_broad==1, Varnames)
dzData <- subset(data_all, zyg_broad==2, Varnames)
dosData <- subset(data_all, zyg_broad==3, Varnames)

mzmData <- subset(data_all, zyg==1, Varnames)
mzfData <- subset(data_all, zyg==3, Varnames)
dzmData <- subset(data_all, zyg==2, Varnames)
dzfData <- subset(data_all, zyg==4, Varnames)
dosData <- subset(data_all, zyg==5, Varnames)
# make sure first DOS twin is male and second is female in SPSS

colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
a<-cov(mzData,use="complete")
b<-cov(dzData,use="complete")
c<-cov(dosData,use="complete")
cov2cor(a)
cov2cor(b)
cov2cor(c)


MZmmodel = mxModel("MZm",
                   mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(5,5), 
                            name="expMeanMZm", labels=c("m1m","m1m") ),
                   mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=(c(1,1)), 
                            name="LMZ", labels=c("sd1m","sd1m")),
                   mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.7), 
                            name="RMZ", labels=
                              matrix(c(NA,"rmzm1",
                                       "rmzm1",NA),2,2,byrow=T)),
                   mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZm"),
                   mxData(mzmData, type="raw"),
                   mxExpectationNormal("expCovMZm", "expMeanMZm", Varnames),
                   mxFitFunctionML( vector = F )) # einde MZmmodel

MZfmodel = mxModel("MZf",
                   mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(5,5), 
                            name="expMeanMZf", labels=c("m1f","m1f") ),
                   mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=(c(1,1)), 
                            name="LMZ", labels=c("sd1f","sd1f")),
                   mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.7), 
                            name="RMZ", labels=
                              matrix(c(NA,"rmzf1",
                                       "rmzf1",NA),2,2,byrow=T)),
                   mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZf"),
                   mxData(mzfData, type="raw"),
                   mxExpectationNormal("expCovMZf", "expMeanMZf", Varnames),
                   mxFitFunctionML( vector = F )) # einde MZfmodel

DZmmodel=mxModel("DZm",
                 mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(5,5),     
                          name="expMeanDZm",labels=c("m1m","m1m") ),
                 mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=(c(1,1)), 
                          name="LDZ", labels=c("sd1m","sd1m")),
                 mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.6), 
                          name="RDZ", labels=
                            matrix(c(NA,"rdzm1",
                                     "rdzm1",NA),2,2,byrow=T)),
                 mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZm"),
                 mxData(dzmData, type="raw"),
                 mxExpectationNormal("expCovDZm", "expMeanDZm", Varnames),
                 mxFitFunctionML( vector = F )) # einde DZmmodel
DZfmodel=mxModel("DZf",
                 mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(5,5),     
                          name="expMeanDZf",labels=c("m1f","m1f") ),
                 mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=(c(1,1)), 
                          name="LDZ", labels=c("sd1f","sd1f")),
                 mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.6), 
                          name="RDZ", labels=
                            matrix(c(NA,"rdzf1",
                                     "rdzf1",NA),2,2,byrow=T)),
                 mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZf"),
                 mxData(dzfData, type="raw"),
                 mxExpectationNormal("expCovDZf", "expMeanDZf", Varnames),
                 mxFitFunctionML( vector = F )) # einde DZfmodel
DOSMFmodel=mxModel("DOSMF",
                   mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(5,5),     
                            name="expMeanDOSMF",labels=c("m1m","m1f") ),
                   mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=(c(1,1)), 
                            name="LDZ", labels=c("sd1m","sd1f")),
                   mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5), 
                            name="RDZ", labels=
                              matrix(c(NA,"rdos1",
                                       "rdos1",NA),2,2,byrow=T)),
                   mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDOSMF"),
                   mxData(dosData, type="raw"),
                   mxExpectationNormal("expCovDOSMF", "expMeanDOSMF", Varnames),
                   mxFitFunctionML( vector = F )) # einde DZfmodel

TwinSatModel<-mxModel("twinsat",MZmmodel, MZfmodel,DZmmodel, DZfmodel, DOSMFmodel, 
                           mxAlgebra(MZm.objective + MZf.objective + DZm.objective + DZf.objective + DOSMF.objective, name="minus2loglikelihood"),
                           mxFitFunctionAlgebra("minus2loglikelihood"),
                      mxCI( c('rdzm1','rmzm1','rdzf1','rmzf1', 'rdos1') ))

TwinSatModelFit <- mxRun(TwinSatModel, intervals=T)
summary(TwinSatModelFit)

## Test for sex differences in 1) means 2) standard deviations  3) correlations MZ=DZ 4) correlations DZ=DOS

## Can the means be constrained to be equal between the sexes?

Sub_EqM <- mxModel(TwinSatModelFit, name="Sub1",
                   mxModel(TwinSatModelFit$MZm, mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(2,2),name="expMeanMZm", labels=c("tw","tw"))),
                   mxModel(TwinSatModelFit$DZm, mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(2,2),name="expMeanDZm", labels=c("tw","tw"))),
                   mxModel(TwinSatModelFit$MZf, mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(2,2),name="expMeanMZf", labels=c("tw","tw"))),
                   mxModel(TwinSatModelFit$DZf, mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(2,2),name="expMeanDZf", labels=c("tw","tw"))),
                   mxModel(TwinSatModelFit$DOSMF, mxMatrix(type="Full", nrow=1, ncol=2, free=T, values=c(2,2),name="expMeanDOSMF", labels=c("tw","tw"))))

Sub_EqM <- mxRun(Sub_EqM, intervals=F)
summary(Sub_EqM)
mxCompare(TwinSatModelFit,Sub_EqM) 

## Can the SDs be constrained to be equal between the sexes?


Sub_EqSD <- mxModel(TwinSatModelFit, name="Sub1a",
                   mxModel(TwinSatModelFit$MZm, mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=c(1,1),name="LMZ", labels=c("sd","sd"))),
                   mxModel(TwinSatModelFit$DZm, mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=c(1,1),name="LDZ", labels=c("sd","sd"))),
                   mxModel(TwinSatModelFit$MZf, mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=c(1,1),name="LMZ", labels=c("sd","sd"))),
                   mxModel(TwinSatModelFit$DZf, mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=c(1,1),name="LDZ", labels=c("sd","sd"))),
                   mxModel(TwinSatModelFit$DOSMF, mxMatrix(type="Diag", nrow=2, ncol=2, free=T, values=c(1,1),name="LDZ", labels=c("sd","sd"))))

Sub_EqSD <- mxRun(Sub_EqSD, intervals=F)
summary(Sub_EqSD)
mxCompare(TwinSatModelFit,Sub_EqSD) 


##  MZM=MZF en DZM=DZF.(QUANTITATIEVE differences)

Sub_EqR <- mxModel(TwinSatModelFit, name="Sub3",
                   mxModel(TwinSatModelFit$MZm,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmz","rmz", NA),2,2,byrow=T))),
                   mxModel(TwinSatModelFit$DZm,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdz","rdz", NA),2,2,byrow=T))),
                   mxModel(TwinSatModelFit$MZf,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmz","rmz", NA),2,2,byrow=T))),
                   mxModel(TwinSatModelFit$DZf,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdz","rdz", NA),2,2,byrow=T))),
                   mxModel(TwinSatModelFit$DOSMF,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdos","rdos", NA),2,2,byrow=T))))

Sub_EqR <- mxRun(Sub_EqR, intervals=F)
summary(Sub_EqR)
mxCompare(TwinSatModelFit,Sub_EqR) 

## DOS = DZ (QUALITATIEVE differences)


Sub_EqRdos <- mxModel(Sub_EqR, name="Sub3",
                   mxModel(Sub_EqR$MZm,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmz","rmz", NA),2,2,byrow=T))),
                   mxModel(Sub_EqR$DZm,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdz","rdz", NA),2,2,byrow=T))),
                   mxModel(Sub_EqR$MZf,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmz","rmz", NA),2,2,byrow=T))),
                   mxModel(Sub_EqR$DZf,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdz","rdz", NA),2,2,byrow=T))),
                   mxModel(Sub_EqR$DOSMF,
                           mxMatrix(type="Stand", nrow=2, ncol=2, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdz","rdz", NA),2,2,byrow=T))))

Sub_EqRdos <- mxRun(Sub_EqRdos, intervals=F)
summary(Sub_EqRdos)
mxCompare(Sub_EqR,Sub_EqRdos) #



##############################################################################
####################### BIVARIATE ############################################
##############################################################################


Varnames=colnames(data_all)[c(4,5,31,32)] 
mzmData <- subset(data_all, zyg==1, Varnames)
mzfData <- subset(data_all, zyg==3, Varnames)
dzmData <- subset(data_all, zyg==2, Varnames)
dzfData <- subset(data_all, zyg==4, Varnames)
dosData <- subset(data_all, zyg==5, Varnames)

MZmmodel = mxModel("MZm",
                  mxMatrix(type="Full", nrow=1, ncol=4, free=T, values=c(2,2,2,2), 
                           name="expMeanMZm", labels=c("m1","m1","m2","m2") ),
                  mxMatrix(type="Diag", nrow=4, ncol=4, free=T, values=(c(2,2,2,2)), 
                           name="LMZ", labels=c("sd1","sd1","sd2","sd2")),
                  mxMatrix(type="Stand", nrow=4, ncol=4, free=T, values=c(.5), 
                           name="RMZ", labels=
                             matrix(c(NA,"rmzm1","rmzm2","rmzm3",
                                      "rmzm1",NA,"rmzm3","rmzm2",
                                      "rmzm2","rmzm3",NA,"rmzm6",
                                      "rmzm3","rmzm2","rmzm6",NA),4,4,byrow=T)),
                  mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZm"),
                  mxData(mzmData, type="raw"),
                  mxExpectationNormal("expCovMZm", "expMeanMZm", Varnames),
                  mxFitFunctionML( vector = F )) # einde MZmmodel

MZfmodel = mxModel("MZf",
                   mxMatrix(type="Full", nrow=1, ncol=4, free=T, values=c(2,2,2,2), 
                            name="expMeanMZf", labels=c("m1f","m1f","m2f","m2f") ),
                   mxMatrix(type="Diag", nrow=4, ncol=4, free=T, values=(c(2,2,2,2)), 
                            name="LMZ", labels=c("sd1f","sd1f","sd2","sd2")),
                   mxMatrix(type="Stand", nrow=4, ncol=4, free=T, values=c(.5), 
                            name="RMZ", labels=
                              matrix(c(NA,"rmzm1","rmzm2","rmzm3",
                                       "rmzm1",NA,"rmzm3","rmzm2",
                                       "rmzm2","rmzm3",NA,"rmzm6",
                                       "rmzm3","rmzm2","rmzm6",NA),4,4,byrow=T)),
                   mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZf"),
                   mxData(mzfData, type="raw"),
                   mxExpectationNormal("expCovMZf", "expMeanMZf", Varnames),
                   mxFitFunctionML( vector = F )) # einde MZmmodel

DZmmodel=mxModel("DZm",
                mxMatrix(type="Full", nrow=1, ncol=4, free=T, values=c(2,2,2,2),     
                         name="expMeanDZm",labels=c("m1","m1","m2","m2") ),
                mxMatrix(type="Diag", nrow=4, ncol=4, free=T, values=(c(2,2,2,2)), 
                         name="LDZ", labels=c("sd1","sd1","sd2","sd2")),
                mxMatrix(type="Stand", nrow=4, ncol=4, free=T, values=c(.5), 
                         name="RDZ", labels=
                           matrix(c(NA,"rdzm1","rdzm2","rdzm3",
                                    "rdzm1",NA,"rdzm3","rdzm2",
                                    "rdzm2","rdzm3",NA,"rdzm6",
                                    "rdzm3","rdzm2","rdzm6",NA),4,4,byrow=T)),
                mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZm"),
                mxData(dzmData, type="raw"),
                mxExpectationNormal("expCovDZm", "expMeanDZm", Varnames),
                mxFitFunctionML( vector = F )) # einde DZmmodel
DZfmodel=mxModel("DZf",
                 mxMatrix(type="Full", nrow=1, ncol=4, free=T, values=c(2,2,2,2),     
                          name="expMeanDZf",labels=c("m1f","m1f","m2f","m2f") ),
                 mxMatrix(type="Diag", nrow=4, ncol=4, free=T, values=(c(2,2,2,2)), 
                          name="LDZ", labels=c("sd1f","sd1f","sd2","sd2")),
                 mxMatrix(type="Stand", nrow=4, ncol=4, free=T, values=c(.5), 
                          name="RDZ", labels=
                            matrix(c(NA,"rdzm1","rdzm2","rdzm3",
                                     "rdzm1",NA,"rdzm3","rdzm2",
                                     "rdzm2","rdzm3",NA,"rdzm6",
                                     "rdzm3","rdzm2","rdzm6",NA),4,4,byrow=T)),
                 mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZf"),
                 mxData(dzfData, type="raw"),
                 mxExpectationNormal("expCovDZf", "expMeanDZf", Varnames),
                 mxFitFunctionML( vector = F )) # einde DZmmodel
DOSMFmodel=mxModel("DOSMF",
                 mxMatrix(type="Full", nrow=1, ncol=4, free=T, values=c(2,2,2,2),     
                          name="expMeanDOSMF",labels=c("m1","m1f","m2","m2f") ),
                 mxMatrix(type="Diag", nrow=4, ncol=4, free=T, values=(c(2,2,2,2)), 
                          name="LDZ", labels=c("sd1","sd1f","sd2","sd2")),
                 mxMatrix(type="Stand", nrow=4, ncol=4, free=T, values=c(.5), 
                          name="RDZ", labels=
                            matrix(c(NA,"rdzm1","rdzm2","rdzm3",
                                     "rdzm1",NA,"rdzm3","rdzm2",
                                     "rdzm2","rdzm3",NA,"rdzm6",
                                     "rdzm3","rdzm2","rdzm6",NA),4,4,byrow=T)),
                 mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDOSMF"),
                 mxData(dosData, type="raw"),
                 mxExpectationNormal("expCovDOSMF", "expMeanDOSMF", Varnames),
                 mxFitFunctionML( vector = F )) # einde DZfmodel

multiTwinSatModel<-mxModel("twinsat",MZmmodel, MZfmodel,DZmmodel, DZfmodel, DOSMFmodel, 
                           mxAlgebra(MZm.objective + MZf.objective + DZm.objective + DZf.objective + DOSMF.objective, name="minus2loglikelihood"),
                           mxFitFunctionAlgebra("minus2loglikelihood"),
                       mxCI( c('rmzm3','rdzm3') ))

multiTwinSatModelFit <- mxTryHard(multiTwinSatModel, intervals = T)
summary(multiTwinSatModelFit)



##############################################################################
####################### SIB TWIN CORRELATIONS ################################
##############################################################################

## I need to check what the sib correlations are and whether 
## the means are equal between sib's and twins (do differ between the sexes!)


selVars=colnames(data_all)[c(31,32,33)] # change per phenotype 
Varnames=colnames(data_all)[c(31,32,33)] # change per phenotype
# Select Data for Analysis
mzmmData <- subset(data_all, Zyg_plus_sib==1, selVars)
mzmfData <- subset(data_all, Zyg_plus_sib==2, selVars)
dzmmData <- subset(data_all, Zyg_plus_sib==3, selVars)
dzmfData <- subset(data_all, Zyg_plus_sib==4, selVars)
mzfmData <- subset(data_all, Zyg_plus_sib==5, selVars)
mzffData <- subset(data_all, Zyg_plus_sib==6, selVars)
dzfmData <- subset(data_all, Zyg_plus_sib==7, selVars)
dzffData <- subset(data_all, Zyg_plus_sib==8, selVars)
dosmData <- subset(data_all, Zyg_plus_sib==9, selVars) # first dos is always male 
dosfData <- subset(data_all, Zyg_plus_sib==10, selVars)

MZmmodel = mxModel("MZmm",
                   mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2), 
                            name="expMeanMZmm", labels=c("m1m","m1m","m2m") ),
                   mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                            name="LMZ", labels=c("sd1m","sd1m","sd2ms")),
                   mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                            name="RMZ", labels=
                              matrix(c(NA,"rmzm1","rsibm",
                                       "rmzm1",NA,"rsibm",
                                       "rsibm","rsibm",NA
                              ),3,3,byrow=T)),
                   mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZmm"),
                   mxData(mzmmData, type="raw"),
                   mxExpectationNormal("expCovMZmm", "expMeanMZmm", Varnames),
                   mxFitFunctionML( vector = F )) # einde MZmmodel
MZmfmodel = mxModel("MZmf",
                    mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2), 
                             name="expMeanMZmf", labels=c("m1m","m1m","m2f") ),
                    mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                             name="LMZ", labels=c("sd1m","sd1m","sd2fs")),
                    mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                             name="RMZ", labels=
                               matrix(c(NA,"rmzm1","rsibfx",
                                        "rmzm1",NA,"rsibfx",
                                        "rsibfx","rsibfx",NA
                               ),3,3,byrow=T)),
                    mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZmf"),
                    mxData(mzmfData, type="raw"),
                    mxExpectationNormal("expCovMZmf", "expMeanMZmf", Varnames),
                    mxFitFunctionML( vector = F )) # einde MZmmodel
MZfmmodel = mxModel("MZfm",
                    mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2), 
                             name="expMeanMZfm", labels=c("m1f","m1f","m2m") ),
                    mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                             name="LMZ", labels=c("sd1f","sd1f","sd2ms")),
                    mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                             name="RMZ", labels=
                               matrix(c(NA,"rmzm1","rsibmx",
                                        "rmzm1",NA,"rsibmx",
                                        "rsibmx","rsibmx",NA
                               ),3,3,byrow=T)),
                    mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZfm"),
                    mxData(mzmmData, type="raw"),
                    mxExpectationNormal("expCovMZfm", "expMeanMZfm", Varnames),
                    mxFitFunctionML( vector = F )) # einde MZmmodel
MZffmodel = mxModel("MZff",
                    mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2), 
                             name="expMeanMZff", labels=c("m1f","m1f","m2f") ),
                    mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                             name="LMZ", labels=c("sd1f","sd1f","sd2fs")),
                    mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                             name="RMZ", labels=
                               matrix(c(NA,"rmzm1","rsibf",
                                        "rmzm1",NA,"rsibf",
                                        "rsibf","rsibf",NA
                               ),3,3,byrow=T)),
                    mxAlgebra(LMZ %*% RMZ %*% t(LMZ), name="expCovMZff"),
                    mxData(mzffData, type="raw"),
                    mxExpectationNormal("expCovMZff", "expMeanMZff", Varnames),
                    mxFitFunctionML( vector = F )) # einde MZfmodel
DZmmmodel=mxModel("DZmm",
                  mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2),     
                           name="expMeanDZmm",labels=c("m1m","m1m","m2m") ),
                  mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                           name="LDZ", labels=c("sd1m","sd1m","sd2ms")),
                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                           name="RDZ", labels=
                             matrix(c(NA,"rdzm1","rsibm",
                                      "rdzm1",NA,"rsibm",
                                      "rsibm","rsibm",NA),3,3,byrow=T)),
                  mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZmm"),
                  mxData(dzmmData, type="raw"),
                  mxExpectationNormal("expCovDZmm", "expMeanDZmm", Varnames),
                  mxFitFunctionML( vector = F )) # einde DZmmodel

DZmfmodel=mxModel("DZmf",
                  mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2),     
                           name="expMeanDZmf",labels=c("m1m","m1m","m2f") ),
                  mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                           name="LDZ", labels=c("sd1m","sd1m","sd2fs")),
                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                           name="RDZ", labels=
                             matrix(c(NA,"rdzm1","rsibfx",
                                      "rdzm1",NA,"rsibfx",
                                      "rsibfx","rsibfx",NA),3,3,byrow=T)),
                  mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZmf"),
                  mxData(dzmfData, type="raw"),
                  mxExpectationNormal("expCovDZmf", "expMeanDZmf", Varnames),
                  mxFitFunctionML( vector = F )) # einde DZmmodel
DZfmmodel=mxModel("DZfm",
                  mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2),     
                           name="expMeanDZfm",labels=c("m1f","m1f","m2m") ),
                  mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                           name="LDZ", labels=c("sd1f","sd1f","sd2ms")),
                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                           name="RDZ", labels=
                             matrix(c(NA,"rdzm1","rsibmx",
                                      "rdzm1",NA,"rsibmx",
                                      "rsibmx","rsibmx",NA),3,3,byrow=T)),
                  mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZfm"),
                  mxData(dzfmData, type="raw"),
                  mxExpectationNormal("expCovDZfm", "expMeanDZfm", Varnames),
                  mxFitFunctionML( vector = F )) # einde DZmmodel
DZffmodel=mxModel("DZff",
                  mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2),     
                           name="expMeanDZff",labels=c("m1f","m1f","m2f") ),
                  mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                           name="LDZ", labels=c("sd1f","sd1f","sd2fs")),
                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                           name="RDZ", labels=
                             matrix(c(NA,"rdzm1","rsibf",
                                      "rdzm1",NA,"rsibf",
                                      "rsibf","rsibf",NA),3,3,byrow=T)),
                  mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDZff"),
                  mxData(dzffData, type="raw"),
                  mxExpectationNormal("expCovDZff", "expMeanDZff", Varnames),
                  mxFitFunctionML( vector = F )) # einde DZmmodel


DOSMFmmodel=mxModel("DOSMFm",
                    mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2),     
                             name="expMeanDOSMFm",labels=c("m1m","m1f","m2m")),
                    mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                             name="LDZ", labels=c("sd1m","sd1f","sd2ms")),
                    mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                             name="RDZ", labels=
                               matrix(c(NA,"rdzm1","rsibmx",
                                        "rdzm1",NA,"rsibmx",
                                        "rsibmx","rsibmx",NA),3,3,byrow=T)),
                    mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDOSMFm"),
                    mxData(dosmData, type="raw"),
                    mxExpectationNormal("expCovDOSMFm", "expMeanDOSMFm", Varnames),
                    mxFitFunctionML( vector = F )) # einde DZfmodel
DOSMFfmodel=mxModel("DOSMFf",
                    mxMatrix(type="Full", nrow=1, ncol=3, free=T, values=c(2,2,2),     
                             name="expMeanDOSMFf",labels=c("m1m","m1f","m2f") ),
                    mxMatrix(type="Diag", nrow=3, ncol=3, free=T, values=(c(2,2,2)), 
                             name="LDZ", labels=c("sd1m","sd1f","sd2fs")),
                    mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5), 
                             name="RDZ", labels=
                               matrix(c(NA,"rdzm1","rsibfx",
                                        "rdzm1",NA,"rsibfx",
                                        "rsibfx","rsibfx",NA),3,3,byrow=T)),
                    mxAlgebra(LDZ %*% RDZ %*% t(LDZ), name="expCovDOSMFf"),
                    mxData(dosfData, type="raw"),
                    mxExpectationNormal("expCovDOSMFf", "expMeanDOSMFf", Varnames),
                    mxFitFunctionML( vector = F )) # einde DZfmodel

SibTwinSatModel<-mxModel("twinsat",MZmmodel,MZmfmodel, MZfmmodel, MZffmodel,DZmmmodel,DZmfmodel, DZffmodel,DZfmmodel, DOSMFmmodel, DOSMFfmodel,
                         mxAlgebra(MZmm.objective + MZfm.objective + DZmm.objective + DZfm.objective + DOSMFm.objective
                                   + MZmf.objective + MZff.objective + DZmf.objective + DZff.objective + DOSMFf.objective, name="minus2loglikelihood"),
                         mxFitFunctionAlgebra("minus2loglikelihood"),
                         mxCI( c('rsib') ))

SibTwinSatModelFit <- mxRun(SibTwinSatModel, intervals = F)
summary(SibTwinSatModelFit)

##  SIB with male or female twin 

Sub_Eqsib_sex <- mxModel(SibTwinSatModel, name="Sub1",
                         mxModel(SibTwinSatModel$MZmm,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsibm",
                                                                                                                         "rmzm1",NA,"rsibm",
                                                                                                                         "rsibm","rsibm",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$MZmf,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsibf",
                                                                                                                         "rmzm1",NA,"rsibf",
                                                                                                                         "rsibf","rsibf",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$MZfm,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsibm",
                                                                                                                         "rmzm1",NA,"rsibm",
                                                                                                                         "rsibm","rsibm",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$MZff,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsibf",
                                                                                                                         "rmzm1",NA,"rsibf",
                                                                                                                         "rsibf","rsibf",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$DZmm,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsibm",
                                                                                                                         "rdzm1",NA,"rsibm",
                                                                                                                         "rsibm","rsibm",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$DZmf,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsibf",
                                                                                                                         "rdzm1",NA,"rsibf",
                                                                                                                         "rsibf","rsibf",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$DZfm,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsibm",
                                                                                                                         "rdzm1",NA,"rsibm",
                                                                                                                         "rsibm","rsibm",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$DZff,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsibf",
                                                                                                                         "rdzm1",NA,"rsibf",
                                                                                                                         "rsibf","rsibf",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$DOSMFm,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsibm",
                                                                                                                         "rdzm1",NA,"rsibm",
                                                                                                                         "rsibm","rsibm",NA),3,3,byrow=T))),
                         mxModel(SibTwinSatModel$DOSMFf,
                                 mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsibf",
                                                                                                                         "rdzm1",NA,"rsibf",
                                                                                                                         "rsibf","rsibf",NA),3,3,byrow=T))))

Sub_Eqsib_sex <- mxRun(Sub_Eqsib_sex, intervals=F)
summary(Sub_Eqsib_sex)
mxCompare(SibTwinSatModelFit,Sub_Eqsib_sex) 

## Is male of female sib diff 



Sub_Eqsib_sex1 <- mxModel(SibTwinSatModel, name="Sub1",
                          mxModel(SibTwinSatModel$MZmm,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                          "rmzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$MZmf,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                          "rmzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$MZfm,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                          "rmzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$MZff,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                          "rmzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$DZmm,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsib",
                                                                                                                          "rdzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$DZmf,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsib",
                                                                                                                          "rdzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$DZfm,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsib",
                                                                                                                          "rdzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$DZff,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsib",
                                                                                                                          "rdzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$DOSMFm,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsib",
                                                                                                                          "rdzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))),
                          mxModel(SibTwinSatModel$DOSMFf,
                                  mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rdzm1","rsib",
                                                                                                                          "rdzm1",NA,"rsib",
                                                                                                                          "rsib","rsib",NA),3,3,byrow=T))))

Sub_Eqsib_sex1 <- mxRun(Sub_Eqsib_sex1, intervals=T)
summary(Sub_Eqsib_sex1)
mxCompare(Sub_Eqsib_sex,Sub_Eqsib_sex1) 


## Siblings = DZ?


Sub_Eqsib_DZ <- mxModel(Sub_Eqsib_sex1, name="Sub1",
                        mxModel(SibTwinSatModel$MZmm,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                        "rmzm1",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$MZmf,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                        "rmzm1",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$MZfm,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                        "rmzm1",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$MZff,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RMZ", labels=matrix(c(NA,"rmzm1","rsib",
                                                                                                                        "rmzm1",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$DZmm,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rsib","rsib",
                                                                                                                        "rsib",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$DZmf,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rsib","rsib",
                                                                                                                        "rsib",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$DZfm,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rsib","rsib",
                                                                                                                        "rsib",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$DZff,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rsib","rsib",
                                                                                                                        "rsib",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$DOSMFm,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rsib","rsib",
                                                                                                                        "rsib",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))),
                        mxModel(SibTwinSatModel$DOSMFf,
                                mxMatrix(type="Stand", nrow=3, ncol=3, free=T, values=c(.5),name="RDZ", labels=matrix(c(NA,"rsib","rsib",
                                                                                                                        "rsib",NA,"rsib",
                                                                                                                        "rsib","rsib",NA),3,3,byrow=T))))

Sub_Eqsib_DZ <- mxRun(Sub_Eqsib_DZ, intervals=T)
summary(Sub_Eqsib_DZ)
mxCompare(Sub_Eqsib_sex1, Sub_Eqsib_DZ) 
