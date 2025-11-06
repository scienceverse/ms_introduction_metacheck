
# ----------------------------------------------------------------------------------------------------------------------
# Program: twoSATc.R
# Author: Hermine Maes
# Date: 10 22 2018
# Adapted by Laura W. Wesseldijk (sex specific means and sex specific ACE) Added age as definition variable
# Date: 05 07 2019

# Twin Bivariate Saturated model to estimate means and (co)variances across multiple groups
# Matrix style model - Raw data - Continuous data
# -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# Load Libraries & Options

rm(list=ls())
getwd()
setwd("                   ")
data_all <-read.table("Datafile_sex_drugs_twins_plus_sibs_age.dat",na.strings=c("-999"),dec=",", header=T) 
colnames(data_all)

names(data_all)[1]<-paste("zyg")
names(data_all)[4]<-paste("drugv1")
names(data_all)[5]<-paste("drugv2")
names(data_all)[10]<-paste("SOIo1")
names(data_all)[11]<-paste("SOIo2")
names(data_all)[12]<-paste("SOIbeh1")
names(data_all)[13]<-paste("SOIbeh2")
names(data_all)[14]<-paste("SOIatt1")
names(data_all)[15]<-paste("SOIatt2")
names(data_all)[16]<-paste("SOIdes1")
names(data_all)[17]<-paste("SOIdes2")
names(data_all)[31]<-paste("TDDSsex1")
names(data_all)[32]<-paste("TDDSsex2")
names(data_all)[25]<-paste("sibSOIo")
names(data_all)[33]<-paste("sibTDDSsex")
names(data_all)[22]<-paste("sibdrugv")
colnames(data_all)

#require(OpenMx)
#require (psych)
#require(polycor)

#mxOption(NULL, "Default optimizer", "NPSOL")
#source('https://openmx.ssri.psu.edu/software/getOpenMx.R')

library(OpenMx)
mxVersion()
library(psych); library(polycor)
source("miFunctions.R")
# Create Output
filename <- "Trivariate_sex_drugs"
sink(paste(filename,".Ro",sep=""), append=FALSE, split=TRUE)

# ----------------------------------------------------------------------------------------------------------------------
# Program: twoACEc.R
# Author: Hermine Maes
# Date: 10 22 2018
#
# Twin Bivariate ACE model to estimate causes of variation across multiple groups
# Matrix style model - Raw data - Continuous data
# -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# Select Variables for Analysis
vars <- c( 'TDDSsex','drugv','SOIo') # list of variables names
nv <- 3 # number of variables
nsib <- 2
#ntv <- nv*2 # number of total variables
#selVars <- paste(vars,c(rep(1,nv),rep(2,nv)),sep="")

ntv <- 9 # number of total variables
selVars   <- c( 'SOIo1','TDDSsex1', 'drugv1','SOIo2','TDDSsex2', 'drugv2', 'sibSOIo','sibTDDSsex','sibdrugv')
defVars   <-c('Age_twin1','Age_twin2','Age_sibling') 
useVars   <- c(selVars,defVars)
# Select Data for Analysis

mzmmData <- subset(data_all, Zyg_plus_sib==1, useVars)
mzmfData <- subset(data_all, Zyg_plus_sib==2, useVars)
dzmmData <- subset(data_all, Zyg_plus_sib==3, useVars)
dzmfData <- subset(data_all, Zyg_plus_sib==4, useVars)
mzfmData <- subset(data_all, Zyg_plus_sib==5, useVars)
mzffData <- subset(data_all, Zyg_plus_sib==6, useVars)
dzfmData <- subset(data_all, Zyg_plus_sib==7, useVars)
dzffData <- subset(data_all, Zyg_plus_sib==8, useVars)
dosmData <- subset(data_all, Zyg_plus_sib==9, useVars)
dosfData <- subset(data_all, Zyg_plus_sib==10, useVars)
describe(data_all)



# Last letter (m or f) indicates the gender of the sib, important for the means... as these are sex-specific.
# There are only 875 twin pairs with a sib, the other i indicated as zygosity with a male sib (but there is no sib)
# This means mzmm and dzmm have larger sample sizes
## In the DOS twins: first is male second is female. 

# Set Starting Values
svMe <- c(4,4,2,4,4,2,4,4,2) # start value for means
svPa <- .5 # start value for path coefficient
svPe <- .5 # start value for path coefficient for e
lbPa <- .0001 # lower bound for path coefficient
lbPaD <- valDiagLU(lbPa,0,NA,nv) # lower bound for diagonal, lower & upper elements of covariance matrix
# ----------------------------------------------------------------------------------------------------------------------
# PREPARE MODEL
# ACE Model
# Create Algebra for expected Mean Matrices - including the definition variable age
meanMm <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=c("meansoim","meantddsm","meandrugsm","meansoim","meantddsm","meandrugsm","meansoim","meantddsm","meandrugsm"), name="meanMm" )
meanFf <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=c("meansoif","meantddsf","meandrugsf","meansoif","meantddsf","meandrugsf","meansoif","meantddsf","meandrugsf"), name="meanFf" )
meanMf <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=c("meansoim","meantddsm","meandrugsm","meansoim","meantddsm","meandrugsm","meansoif","meantddsf","meandrugsf"), name="meanMf" )
meanFm <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=c("meansoif","meantddsf","meandrugsf","meansoif","meantddsf","meandrugsf","meansoim","meantddsm","meandrugsm"), name="meanFm" )
meanDm <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=c("meansoim","meantddsm","meandrugsm","meansoif","meantddsf","meandrugsf","meansoim","meantddsm","meandrugsm"), name="meanDm" )
meanDf <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=c("meansoim","meantddsm","meandrugsm","meansoif","meantddsf","meandrugsf","meansoif","meantddsf","meandrugsf"), name="meanDf" )


defAge    <- mxMatrix(type="Full", nrow=1,ncol=ntv,free=FALSE,
                      labels=c('data.Age_twin1','data.Age_twin1','data.Age_twin1','data.Age_twin2','data.Age_twin2','data.Age_twin2','data.Age_sibling','data.Age_sibling','data.Age_sibling'),name='Age')

pathB     <- mxMatrix( type="Full", nrow=1, ncol=1, free=TRUE, values= .01, 
                       label="b11", name="b")

expmeanMm   <- mxAlgebra(  meanMm + (b%x%Age), name="expmeanMm" )
expmeanFf   <- mxAlgebra(  meanFf + (b%x%Age), name="expmeanFf" )
expmeanMf   <- mxAlgebra(  meanMf + (b%x%Age), name="expmeanMf" )
expmeanFm   <- mxAlgebra(  meanFm + (b%x%Age), name="expmeanFm" )
expmeanDm   <- mxAlgebra(  meanDm + (b%x%Age), name="expmeanDm" )
expmeanDf   <- mxAlgebra(  meanDf + (b%x%Age), name="expmeanDf" )


# Create Matrices for Path Coefficients
pathA <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPa,nv), labels=labLower("a",nv), lbound=lbPaD,
                   name="a" )
pathC <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=F, values=0, labels=labLower("c",nv), lbound=lbPaD,
                   name="c" )
pathE <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=valDiag(svPe,nv), labels=labLower("e",nv), lbound=lbPaD,
                   name="e" )
# Create Algebra for Variance Comptwonts
covA <- mxAlgebra( expression=a %*% t(a), name="A" )
covC <- mxAlgebra( expression=c %*% t(c), name="C" )
covE <- mxAlgebra( expression=e %*% t(e), name="E" )
# Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
covP <- mxAlgebra( expression= A+C+E, name="V" )


## Lots of more covariance matrices due to the sex of the siblings:
## In opposite sex of the sibling compared to the twin, i model rg. 
## First for male twins
covMZ     <- mxAlgebra( expression= rbind( cbind(V ,   A+C,    .5%x%A+C),
                                           cbind(A+C,    V,    .5%x%A+C),
                                           cbind(.5%x%A+C, .5%x%A+C, V)), 
                        name="expCovMZ" )

covDZ     <- mxAlgebra( expression= rbind( cbind(V ,    .5%x%A+C, .5%x%A+C),
                                           cbind(.5%x%A+C,  V,    .5%x%A+C),
                                           cbind(.5%x%A+C,  .5%x%A+C,   V)), 
                        name="expCovDZ" )

# Create Data Objects for Multiple Groups
dataMZmm <- mxData( observed=mzmmData, type="raw" )
dataMZmf <- mxData( observed=mzmfData, type="raw" )
dataDZmm <- mxData( observed=dzmmData, type="raw" )
dataDZmf <- mxData( observed=dzmfData, type="raw" )
dataMZfm <- mxData( observed=mzfmData, type="raw" )
dataMZff <- mxData( observed=mzffData, type="raw" )
dataDZfm <- mxData( observed=dzfmData, type="raw" )
dataDZff <- mxData( observed=dzffData, type="raw" )
dataDOSm <- mxData( observed=dosmData, type="raw" )
dataDOSf <- mxData( observed=dosfData, type="raw" )


# Create Expectation Objects for Multiple Groups
expMZmm <- mxExpectationNormal( covariance="expCovMZ", means="expmeanMm", dimnames=selVars )
expMZmf <- mxExpectationNormal( covariance="expCovMZ", means="expmeanMf", dimnames=selVars )
expDZmm <- mxExpectationNormal( covariance="expCovDZ", means="expmeanMm", dimnames=selVars )
expDZmf <- mxExpectationNormal( covariance="expCovDZ", means="expmeanMf", dimnames=selVars )
expMZfm <- mxExpectationNormal( covariance="expCovMZ", means="expmeanFm", dimnames=selVars )
expMZff <- mxExpectationNormal( covariance="expCovMZ", means="expmeanFf", dimnames=selVars )
expDZfm <- mxExpectationNormal( covariance="expCovDZ", means="expmeanFm", dimnames=selVars )
expDZff <- mxExpectationNormal( covariance="expCovDZ", means="expmeanFf", dimnames=selVars )
expDOSm <- mxExpectationNormal( covariance="expCovDZ", means="expmeanDm", dimnames=selVars )
expDOSf <- mxExpectationNormal( covariance="expCovDZ", means="expmeanDf", dimnames=selVars )
funML <- mxFitFunctionML()
# Create Model Objects for Multiple Groups
pars <- list( meanMm, meanMf, meanFm, meanFf, meanDm, meanDf, expmeanMm, expmeanMf, expmeanFm, expmeanFf, expmeanDm, expmeanDf, defAge, pathB, pathA, pathC, pathE, covA, covC, covE, covP)
outputs   <- list(pathA, pathC, pathE, covA, covC, covE, covP)
modelMZmm <- mxModel( pars,  covMZ, dataMZmm, expMZmm, funML, name="MZmm" )
modelMZmm <- mxModel( pars,  covMZ, dataMZmm, expMZmm, funML, name="MZmm" )
modelMZmf <- mxModel( pars,  covMZ, dataMZmf, expMZmf, funML, name="MZmf" )
modelDZmm <- mxModel( pars,  covDZ, dataDZmm, expDZmm, funML, name="DZmm" )
modelDZmf <- mxModel( pars,  covDZ, dataDZmf, expDZmf, funML, name="DZmf" )
modelMZfm <- mxModel( pars,  covMZ, dataMZfm, expMZfm, funML, name="MZfm" )
modelMZff <- mxModel( pars,  covMZ, dataMZff, expMZff, funML, name="MZff" )
modelDZfm <- mxModel( pars,  covDZ, dataDZfm, expDZfm, funML, name="DZfm" )
modelDZff <- mxModel( pars,  covDZ, dataDZff, expDZff, funML, name="DZff" )
modelDOSm <- mxModel( pars,  covDZ, dataDOSm, expDOSm, funML, name="DOSm" )
modelDOSf <- mxModel( pars,  covDZ, dataDOSf, expDOSf, funML, name="DOSf" )
multi <- mxFitFunctionMultigroup( c("MZmm","DZmm","MZfm","DZfm","DOSm","MZmf","DZmf","MZff","DZff","DOSf"))
# Create Algebra for Standardization
matI <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I" )
invSD <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD" )
# Calculate genetic and environmental correlations
corA <- mxAlgebra( expression=solve(sqrt(I*A))%&%A, name ="rA" ) #cov2cor()
corC <- mxAlgebra( expression=solve(sqrt(I*C))%&%C, name ="rC" )
corE <- mxAlgebra( expression=solve(sqrt(I*E))%&%E, name ="rE" )
# Create Algebra for Variance Components
rowUS <- rep('US',nv)
colUS <- rep(c('A','C','E','SA','SC','SE'),each=nv)
estUS <- mxAlgebra( expression=cbind(A,C,E,A/V,C/V,E/V), name="US", dimnames=list(rowUS,colUS) )
# Create Confidence Interval Objects
odd <- seq(1+3*nv,2*3*nv,nv)
even <- seq(2+3*nv,2*3*nv,nv)
ciACE <- mxCI( c("US[1,odd]","US[2,odd]","US[2,even]") )
# Build Model with Confidence Intervals
calc <- list( matI, invSD, corA, corC, corE, estUS, ciACE)
modelACE <- mxModel( "triACEc", outputs, modelMZmm, modelDZmm, modelMZfm, modelDZfm, modelDOSm,modelMZmf, modelDZmf, modelMZff, modelDZff, modelDOSf, multi, calc )

# ----------------------------------------------------------------------------------------------------------------------
# RUN MODEL
# Run ACE Model
fitACE <- mxTryHard( modelACE, intervals=T, extraTries = 50)
sumACE <- summary( fitACE )
sumACE

# Compare with Saturated Model
#mxCompare( fitSAT, fitACE )
# Print Goodness-of-fit Statistics & Parameter Estimates
fitGofs(fitACE)
fitEstCis(fitACE)
round(rbind(fitACE$US$result),4)
# ----------------------------------------------------------------------------------------------------------------------

# these are the genetic and environmental correlations
fitACE$rA
fitACE$rC
fitACE$rE

# Run AE model
modelAE <- mxModel( fitACE, name="twoAEc" )
modelAE <- omxSetParameters( modelAE, labels=labLower("c",nv), free=FALSE, values=0 )
fitAE <- mxRun( modelAE, intervals=F )
summary(fitAE)
fitGofs(fitAE); fitEstCis(fitAE)
mxCompare( fitACE, fitAE )

# ----------------------------------------------------------------------------------------------------------------------
sink()
save.image(paste(filename,".Ri",sep=""))