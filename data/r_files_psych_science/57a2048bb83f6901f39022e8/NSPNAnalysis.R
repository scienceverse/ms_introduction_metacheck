#Analysis script that produces all analyses for the paper:
#Mutualistic coupling between vocabulary and reasoning supports cognitive 
#development during late adolescence and early adulthood. 
#Rogier A. Kievit, Ulman Lindenberger, Ian M. Goodyer, Peter B. Jones, Peter Fonagy, Ed T. Bullmore, the NSPN Consortium & Raymond J. Dolan
#Script author: Rogier Kievit, August 2016 using Rstudio 0.99.893 running R version 3.2.4 (Very Secure Dishes)

# Note: We are not able to publicly share the raw data for a small subset of covariates
# mentioned in the manuscript due to privacy issues- these can be accessed by 
# sending a request to openNSPN@medschl.cam.ac.uk

#Preamble: install and load relevant packages

# install.packages('ggplot2')
# install.packages('qpcR')
# install.packages('reshape2')
# install.packages('dplyr')
# install.packages('lavaan')
# install.packages('BayesFactor')
# install.packages('moments')
# install.packages('semTools')
# install.packages('e1071')

#Load
library(ggplot2)
library(qpcR)
library(reshape2)
library(dplyr)
library(lavaan)
library(BayesFactor)
library(moments)
library(semTools)
library(e1071)

#Set working directory (location of Cognitivedata.Rdata, as well as place you are happy to save figures)
setwd("xxxx")
load('Cognitivedata.Rdata')

#Analyses are coded in the order in which they appear in the paper.

#Sample characteristics

nrow(CognitiveData) #N at wave 1
length(na.omit(CognitiveData$t1ssmatrix)) #N at wave 1
length(na.omit(CognitiveData$t2ssmatrix)) #N at wave 2

length(na.omit(CognitiveData$t1ssvocab)) #N at wave 1
length(na.omit(CognitiveData$t2ssvocab)) #N at wave 2

#Gender counts
sum(CognitiveData$sex=='Male')
sum(CognitiveData$sex=='Female')

#Age 
range(CognitiveData$age1,na.rm=T)
mean(CognitiveData$age1,na.rm=T)

range(CognitiveData$interval,na.rm=T)
mean(CognitiveData$interval,na.rm=T)

#Define wave 2 missingness as a factor 
CognitiveData$missingt2<-as.factor(as.numeric(is.na(CognitiveData$t2ssvocab)))
levels(CognitiveData$missingt2)<-c('Complete data', 'Missing at T2')

#Vocabulary time 1 for those with complete data versus only T1 
t.test(t1ssvocab~missingt2,data=CognitiveData)
bfdatvoc<-data.frame(CognitiveData$t1ssvocab,CognitiveData$missingt2)
bfdatvoc<-na.omit(bfdatvoc)
colnames(bfdatvoc)<-c('t1ssvocab','Missing')
1/ttestBF(formula=t1ssvocab~Missing,data=bfdatvoc)

#Matrix reasoning time 1 for those with complete data versus only T1 
t.test(t1ssmatrix~missingt2,data=CognitiveData)
bfdatmat<-data.frame(CognitiveData$t1ssmatrix,CognitiveData$missingt2)
bfdatmat<-na.omit(bfdatmat)
colnames(bfdatmat)<-c('t1ssmatrix','Missing')
1/ttestBF(formula=t1ssmatrix~Missing,data=bfdatmat)

#Gender distribution across missingness
sexmissing<-xtabs(~missingt2+sex,data=CognitiveData)
chisq.test(sexmissing)
1/contingencyTableBF(sexmissing,sampleType = "indepMulti",fixedMargin = "cols")

#Age at time 1 for those with complete data versus only T1
bfdatage<-data.frame(CognitiveData$age1,CognitiveData$missingt2)
t.test(age1~missingt2,data=CognitiveData)
bfdatage<-na.omit(bfdatage)
colnames(bfdatage)<-c('Age','Missing')
ttestBF(formula=Age~Missing,data=bfdatage)
t.test(CognitiveData$age1~CognitiveData$missingt2)


#Descriptives (Table 1)
length(na.omit(CognitiveData$t1ssmatrix))
length(na.omit(CognitiveData$t2ssmatrix))
length(na.omit(CognitiveData$t1ssvocab))
length(na.omit(CognitiveData$t2ssvocab))

mean(na.omit(CognitiveData$t1ssmatrix))
mean(na.omit(CognitiveData$t2ssmatrix))
mean(na.omit(CognitiveData$t1ssvocab))
mean(na.omit(CognitiveData$t2ssvocab))

range(na.omit(CognitiveData$t1ssmatrix))
range(na.omit(CognitiveData$t2ssmatrixraw))
range(na.omit(CognitiveData$t1ssvocab))
range(na.omit(CognitiveData$t2ssvocabraw))

sd(na.omit(CognitiveData$t1ssmatrix))
sd(na.omit(CognitiveData$t2ssmatrixraw))
sd(na.omit(CognitiveData$t1ssvocab))
sd(na.omit(CognitiveData$t2ssvocabraw))

skewness(na.omit(CognitiveData$t1ssmatrix))
skewness(na.omit(CognitiveData$t2ssmatrixraw))
skewness(na.omit(CognitiveData$t1ssvocab))
skewness(na.omit(CognitiveData$t2ssvocabraw))

e1071::kurtosis(na.omit(CognitiveData$t1ssmatrix))
e1071::kurtosis(na.omit(CognitiveData$t2ssmatrixraw))
e1071::kurtosis(na.omit(CognitiveData$t1ssvocab))
e1071::kurtosis(na.omit(CognitiveData$t2ssvocabraw))


#Fitting the (bivariate) latent change score models

#Latent change score model, matrix reasoning only
latentdiffmatrix <- '
etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssmatrix

etamat ~ 1  #Change factor intercept

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat  #Change factor variance

t1ssmatrix ~~ t1ssmatrix  
t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1 #Age as covariate
age1~1
'

fitlatentdiffmatrix <- growth(latentdiffmatrix, data=CognitiveData, estimator='mlr', fixed.x=FALSE,missing='fiml')
summary(fitlatentdiffmatrix, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE) 

#Latent change score model for matrix reasoning only without self-feedback path (to estimate change)
latentdiffmatrixnoselffb <- '
etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

#etamat~t1ssmatrix

etamat ~ 1  #Change factor intercept

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat  #Change factor variance

t1ssmatrix ~~ t1ssmatrix  
t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1 #Age as covariate
age1~1
'

fitlatentdiffmatrixnoselffb <- growth(latentdiffmatrixnoselffb, data=CognitiveData, estimator='mlr', fixed.x=FALSE,missing='fiml')
summary(fitlatentdiffmatrixnoselffb, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE) 

#Latent change score model, vocabulary only
latentdiffvocab <- '	#latent change model;

etavoc =~ 1*t2ssvocab
t2ssvocab ~ 1*t1ssvocab
etavoc~t1ssvocab

etavoc ~ 1   #

t1ssvocab ~ 1
t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
age1~1
'

fitlatentdiffvocab <- growth(latentdiffvocab, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitlatentdiffvocab, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Latent change score model for vocabulary only without self-feedback path (to estimate change)

latentdiffvocabselffb <- '	#latent change model;

etavoc =~ 1*t2ssvocab
t2ssvocab ~ 1*t1ssvocab
#etavoc~t1ssvocab

etavoc ~ 1   #

t1ssvocab ~ 1
t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
age1~1
'

fitlatentdiffvocabselffb <- growth(latentdiffvocabselffb, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitlatentdiffvocabselffb, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Longitudinal measurement invariance test for g factor model

#g factor MI test
gfactornocons <- '
gft1 =~ t1ssmatrix+t1ssvocab
gft2=~t2ssmatrix+t2ssvocab
t1ssmatrix~~t2ssmatrix
t1ssvocab~~t2ssvocab
gft2~1*gft1
etag =~ 1*gft2
etag~gft1

etag ~ 1   #replace with eta3 ~ 0 for LR test;

gft1 ~ 1
gft2 ~ 0*1
etag ~~ etag
gft2 ~~ 0*gft2
gft1~age1
age1~1
t1ssmatrix~1
t2ssmatrix~1
t1ssvocab~1
t2ssvocab~1

'
fitgfactornocons <- growth(gfactornocons, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitgfactornocons, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Compute delta-cfi
var1<-c('t1ssmatrix','t1ssvocab')
var2<-c('t2ssmatrix','t2ssvocab')
constrainedVar <- list(var1, var2)
longInvariance(gfactornocons, auto=1, constrainAuto=TRUE,varList=constrainedVar,data=CognitiveData,strict = FALSE,missing='fiml',estimator='mlr',fixed.x=FALSE) #
longInvariance(gfactornocons, auto=1, constrainAuto=TRUE,varList=constrainedVar,data=CognitiveData,strict = FALSE,estimator='mlr',fixed.x=FALSE)


##### Fit three competing models
#Fit g factor model
gfactor <- '
gft1 =~ t1ssmatrix+t1ssvocab
gft2=~equal("gft1 =~ t1ssmatrix")*t2ssmatrix+equal("gft1 =~ t1ssvocab")*t2ssvocab
gft2~1*gft1
t1ssmatrix~~t2ssmatrix
t1ssvocab~~t2ssvocab
etag =~ 1*gft2
etag~gft1

etag ~ 1   
gft1 ~ 1
gft2 ~ 0*1
etag ~~ etag

gft2 ~~ 0*gft2
gft1~age1
age1~1
t1ssmatrix~1
t2ssmatrix~equal("t1ssmatrix~1")*1
t1ssvocab~1
t2ssvocab~equal("t1ssvocab~1")*~1

'
fitgfactor <- growth(gfactor, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitgfactor, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Fit investment model

investment <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~0*t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitinvestment <- growth(investment, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitinvestment, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Fit mutualism model
mutualism <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitmutualism <- growth(mutualism, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualism, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)


#Compute effect sizes:
0.203*0.203 # Standardized coupling from t1ssvocab to etamat
0.144*0.144 # Standardized coupling from t1ssmatrix to etavoc 


#Model comparison using AIC

#Compute akaike weights according to Wagenmakers and Farrell

aicvecall<-c(AIC(fitgfactor),AIC(fitmutualism),AIC(fitinvestment))
valuesweights<-akaike.weights(aicvecall)$weights

#Normalized
valuesweights[2]/(valuesweights[1]+valuesweights[3]+valuesweights[2])

#Relative evidence ratio
valuesweights[2]/valuesweights[1]+valuesweights[3]

#LRT of nested models (note: this test uses the scaled, not raw, chi square)
anova(fitinvestment,fitmutualism)


#Model fit (table 2)

#fit all
fitMeasures(fitgfactor, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
fitMeasures(fitinvestment, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
fitMeasures(fitmutualism, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))



#Further investigation of the best model (mutualism)

#Fixing the variance of change scores to 0 (first vocabulary, then matrix reasoning)

mutualismnovaretavoc <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 0   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitmutualismnovaretavoc <- growth(mutualismnovaretavoc, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualismnovaretavoc, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

mutualismnovaretamat <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 0   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitmutualismnovaretamat <- growth(mutualismnovaretamat, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualismnovaretamat, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Comparing model fit when variance of change scores fixed to 0
anova(fitmutualism,fitmutualismnovaretamat)
anova(fitmutualism,fitmutualismnovaretavoc)


###Run analysis as CCA 

#Fit g factor model
gfactor_CCA <- '
gft1 =~ t1ssmatrix+t1ssvocab
gft2=~equal("gft1 =~ t1ssmatrix")*t2ssmatrix+equal("gft1 =~ t1ssvocab")*t2ssvocab
gft2~1*gft1
t1ssmatrix~~t2ssmatrix
t1ssvocab~~t2ssvocab
etag =~ 1*gft2
etag~gft1

etag ~ 1   
gft1 ~ 1
gft2 ~ 0*1
etag ~~ etag

gft2 ~~ 0*gft2
gft1~age1
age1~1
t1ssmatrix~1
t2ssmatrix~equal("t1ssmatrix~1")*1
t1ssvocab~1
t2ssvocab~equal("t1ssvocab~1")*~1


'
fitgfactor_CCA <- growth(gfactor_CCA, data=CognitiveData, estimator='mlr',fixed.x=FALSE)
summary(fitgfactor_CCA, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Fit investment model

investment_CCA <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~0*t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitinvestment_CCA <- growth(investment_CCA, data=CognitiveData, estimator='mlr',fixed.x=FALSE)
summary(fitinvestment_CCA, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Fit mutualism model
mutualism_CCA <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitmutualism_CCA <- growth(mutualism_CCA, data=CognitiveData, estimator='mlr',fixed.x=FALSE)
summary(fitmutualism_CCA, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Equality constraining model properties across sex doesn't adversely affect model fit

#First a simple multigroup
mutualismmultigroup <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitmutualismmultigroup <- growth(mutualismmultigroup, data=CognitiveData, estimator='mlr',group='sex',fixed.x=FALSE,missing='fiml')
summary(fitmutualismmultigroup, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

mutualismmultigroup_cons <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~age1

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~age1
t1ssmatrix ~~ t1ssvocab
age1~1
'
fitmutualismmultigroup_cons <- growth(mutualismmultigroup_cons, data=CognitiveData, estimator='mlr',group='sex',fixed.x=FALSE,missing='fiml',group.equal = c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", "lv.variances","lv.covariances"))
summary(fitmutualismmultigroup_cons, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

anova(fitmutualismmultigroup_cons,fitmutualismmultigroup)


##Figure 4

parameterEstimates(fitmutualism) 


####Plotting
theme_set(theme_grey(base_size = 18))

#Plotting behavioural scores

#Figure 1: Behavioural scores (note that the scaled scores and ages for time 2 are used)
newcogdatred<-CognitiveData
newcogdatage<-melt(data.frame(newcogdatred$age1,newcogdatred$age2))
newcogdatmat<-melt(data.frame(newcogdatred$t1ssmatrix,newcogdatred$t2ssmatrix))
newcogdatmat$id<-c(factor(1:nrow(newcogdatred)),factor(1:nrow(newcogdatred)))
plotnewcogdatmat<-data.frame(newcogdatmat[,2:3])
plotnewcogdatmat$age<-newcogdatage$value
ggplot(plotnewcogdatmat,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='dodgerblue')+geom_line(alpha=.7,col='dodgerblue',lwd=1)+ylab('Matrix reasoning score')+theme(text = element_text(size=22))+ylim(13,35)+xlab('Age')

ggsave('Matrix.tiff',compression='lzw',height=5,width=8)


newcogdatvoctemp<-CognitiveData
newcogdatage<-melt(data.frame(newcogdatvoctemp$age1,newcogdatvoctemp$age2))
newcogdatvoc<-melt(data.frame(newcogdatvoctemp$t1ssvocab,newcogdatvoctemp$t2ssvocab))
newcogdatvoc$id<-c(factor(1:nrow(newcogdatred)),factor(1:nrow(newcogdatred)))
plotnewcogdatvoc<-data.frame(newcogdatvoc[,2:3])
plotnewcogdatvoc$age<-newcogdatage$value
ggplot(plotnewcogdatvoc,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='#ff7f7f')+geom_line(alpha=.7,col='#ff7f7f',lwd=1)+ylab('Vocabulary score')+theme(text = element_text(size=22))+ylim(16,81)+xlab('Age')

ggsave('Vocab.tiff',compression='lzw',height=5,width=8)

#Figure 3: Plotting model comparison
#Figure 3A: Plot all IC's
values<-c(AIC(fitgfactor),AIC(fitmutualism),AIC(fitinvestment),BIC(fitgfactor),BIC(fitmutualism),BIC(fitinvestment))
IC<-factor(c('AIC','AIC','AIC','BIC','BIC','BIC'))
Model<-factor(c('g factor','Mutualism', 'Investment','g factor','Mutualism','Investment'))
levels(Model)<-c('g factor', 'Investment','Mutualism')
modeldat=data.frame(values,IC,Model)
ggplot(modeldat,aes(IC,values,fill=Model))+geom_bar(stat="identity",position="dodge")+coord_cartesian(ylim=c(18400,18570))+xlab('Information criterion')+theme_grey(base_size = 22)#+scale_fill_brewer(palette="Set1")+

ggsave('ICs.tiff',compression='lzw',height=5,width=8)

#Figure 3B: Plot all as akaike weights

Model<-factor(c('g factor','Mutualism','Investment'))
modeldatweights=data.frame(valuesweights,Model)
ggplot(modeldatweights,aes(Model,valuesweights,fill=Model))+geom_bar(stat="identity",position="dodge")+ylab('Likelihood')+xlab('')+ggtitle('Akaike weights')+theme_grey(base_size = 22)
ggsave('Akaikeweights.tiff',compression='lzw',height=5,width=8)


#Figure 5: Dynamic change/vector plot
#Creating matrix of model implied bivariate pairs
dimlength<-9
t1mat<-seq(from = 20,to=37,length.out = dimlength)
t1voc<-seq(from = 45,to=70,length.out = dimlength)
scorevecmut<-data.frame(rep(t1mat,times=dimlength))
scorevecmut$voc<-rep(t1voc,each=dimlength)
colnames(scorevecmut)[1:2]<-c('t1mat','t1voc')

#Computing model implied scores at T2
scorevecmut$t2voc<-scorevecmut$t1voc+5.540+(-0.169*scorevecmut$t1voc)+(0.163*scorevecmut$t1mat)
scorevecmut$t2mat<-scorevecmut$t1mat+8.403+(-0.374*scorevecmut$t1mat)+(0.051*scorevecmut$t1voc)

#Pick random subset to not clutter figure too much
subsamplesize<-200
samplerows<-sample(1:785,subsamplesize,replace=F)
subset<-CognitiveData[samplerows,]

#Plotting vector field
ggplot(subset,aes(t1ssvocab,t1ssmatrix))+geom_jitter(alpha=.5,size=2)+geom_segment(aes(x=t1voc,y=t1mat,xend=t2voc,yend=t2mat),arrow = arrow(length = unit(0.2,"cm")),data=scorevecmut)+xlab('Vocabulary')+ylab('Matrix Reasoning')+coord_cartesian(ylim = c(21,35),xlim=c(40,75))+annotate("rect",xmin = 50.7, xmax = 54, ymin = 20, ymax = 35,alpha=.2,fill='red')+annotate("rect",xmin = 40, xmax = 75, ymin = 28, ymax = 30,alpha=.2,fill='blue')+theme_bw(base_size = 19)+theme(panel.grid.minor = element_blank())+ylim(20,35)+stat_ellipse(lty=2,level = 0.90,alpha=.7)

ggsave('vector.pdf',width=10,height=7) 



#############

# The role of Age

#Age predicting change scores
mutualism_age_on_change <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix+age1
etavoc~t1ssmatrix+t1ssvocab+age1

etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 


etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab

t1ssmatrix ~~ t1ssvocab

t1ssvocab~age1
t1ssmatrix~age1
age1~1
'
fitmutualism_age_on_change <- growth(mutualism_age_on_change, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualism_age_on_change, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Age predicting change scores not needed
anova(fitmutualism,fitmutualism_age_on_change)
##Age linear and squared

#Fit model with both age linear and squared included
mutualism_age_lin_age_squa <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~agesq+agelin

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~agesq+agelin
t1ssmatrix ~~ t1ssvocab
agesq~1
agelin~1
agesq~~agelin
'
fitmutualism_age_lin_age_squa <- growth(mutualism_age_lin_age_squa, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualism_age_lin_age_squa, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

mutualism_age_lin_age_squa_cons <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~t1ssvocab+t1ssmatrix
etavoc~t1ssmatrix+t1ssvocab


etamat ~ 1   #replace with eta3 ~ 0 for LR test;

t1ssmatrix ~ 1
t2ssmatrix ~ 0*1

etamat ~~ etamat

t2ssmatrix ~~ 0*t2ssmatrix 
t1ssmatrix~0*agesq+agelin

etavoc ~ 1   #replace with eta3 ~ 0 for LR test;

t2ssvocab ~ 0*1

etavoc ~~ etavoc
t1ssvocab ~~ t1ssvocab
t2ssvocab ~~ 0*t2ssvocab
t1ssvocab~0*agesq+agelin
t1ssmatrix ~~ t1ssvocab
agesq~1
agelin~1
agesq~~agelin
'
fitmutualism_age_lin_age_squa_cons <- growth(mutualism_age_lin_age_squa_cons, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualism_age_lin_age_squa_cons, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Compare models with age squared fixed to 0
anova(fitmutualism_age_lin_age_squa,fitmutualism_age_lin_age_squa_cons)

# Include age as predictor of T1 raw scores 
gfactor_diffage_a <- '
gft1 =~ t1ssmatrix+t1ssvocab
gft2=~equal("gft1 =~ t1ssmatrix")*t2ssmatrix+equal("gft1 =~ t1ssvocab")*t2ssvocab
gft2~1*gft1
t1ssmatrix~~t2ssmatrix
t1ssvocab~~t2ssvocab
etag =~ 1*gft2
etag~gft1

etag ~ 1   
gft1 ~ 1
gft2 ~ 0*1
etag ~~ etag

gft2 ~~ 0*gft2
#gft1~age1
age1~1
t1ssmatrix~age1
t1ssvocab~age1
t1ssmatrix~1
t2ssmatrix~equal("t1ssmatrix~1")*1
t1ssvocab~1
t2ssvocab~equal("t1ssvocab~1")*~1

'
fitgfactor_diffage_a <- growth(gfactor_diffage_a, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitgfactor_diffage_a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


# Include age as predictor of T1 raw scores as well as gft1
gfactor_diffage_b <- '
gft1 =~ t1ssmatrix+t1ssvocab
gft2=~equal("gft1 =~ t1ssmatrix")*t2ssmatrix+equal("gft1 =~ t1ssvocab")*t2ssvocab
gft2~1*gft1
t1ssmatrix~~t2ssmatrix
t1ssvocab~~t2ssvocab
etag =~ 1*gft2
etag~gft1

etag ~ 1   
gft1 ~ 1
gft2 ~ 0*1
etag ~~ etag

gft2 ~~ 0*gft2
gft1~age1
age1~1
t1ssmatrix~age1
t1ssvocab~age1
t1ssmatrix~1
t2ssmatrix~equal("t1ssmatrix~1")*1
t1ssvocab~1
t2ssvocab~equal("t1ssvocab~1")*~1

'
fitgfactor_diffage_b <- growth(gfactor_diffage_b, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitgfactor_diffage_b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

BIC(fitgfactor)-BIC(fitmutualism)
BIC(fitgfactor_diffage_a)-BIC(fitmutualism)
BIC(fitgfactor_diffage_b)-BIC(fitmutualism)



