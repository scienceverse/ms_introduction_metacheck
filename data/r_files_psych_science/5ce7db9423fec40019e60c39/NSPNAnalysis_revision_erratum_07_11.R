#Corrigendum https://journals.sagepub.com/doi/full/10.1177/0956797619843895
#This script reanalyses all analyses for the paper:
#Mutualistic coupling between vocabulary and reasoning supports cognitive 
#development during late adolescence and early adulthood. 
#Rogier A. Kievit, Ulman Lindenberger, Ian M. Goodyer, Peter B. Jones, Peter Fonagy, Ed T. Bullmore, the NSPN Consortium & Raymond J. Dolan
#After discovering miscoding errors in the raw scores, all findings were recomputed with the correct scores
#below reports all previous and updated scores, after loading the dummy coded scores for both datasets
#original findings are left aligned, the 'updated' findings are offset with a tab right after

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
load('Cognitivedata_orig.Rdata')
load('Cognitivedata_updated.Rdata')
# load('fulldemos.Rdata') #not publicly shareable
# load('fulldemos_updated.Rdata') #not publicly shareable

#Analyses are coded in the order in which they appear in the paper.

#Sample characteristics
nrow(CognitiveData) #N at wave 1
length(na.omit(CognitiveData$t1ssmatrix)) #N at wave 2
length(na.omit(CognitiveData$t2ssmatrix)) #N at wave 2
length(na.omit(CognitiveData$t1ssvocab)) #N at wave 2
length(na.omit(CognitiveData$t2ssvocab)) #N at wave 2

    
      ##### Updated
      #####
      nrow(CognitiveData_updated) #N at wave 1
      length(na.omit(CognitiveData_updated$t1ssmatrix)) #N at wave 2
      length(na.omit(CognitiveData_updated$t2ssmatrix)) #N at wave 2
      length(na.omit(CognitiveData_updated$t1ssvocab)) #N at wave 2
      length(na.omit(CognitiveData_updated$t2ssvocab)) #N at wave 2



#Gender counts
sum(CognitiveData$sex=='Male')
sum(CognitiveData$sex=='Female')

      ##### Updated
      #####
      sum(CognitiveData_updated$sex=='Male')
      sum(CognitiveData_updated$sex=='Female')
      
#Interval
mean(na.omit(CognitiveData$interval)) 
      
      ##### Updated
      #####
      mean(na.omit(CognitiveData_updated$interval)) 


#Age specs
range(CognitiveData$age1,na.rm=T)
mean(CognitiveData$age1,na.rm=T)

      ##### Updated
      #####
      range(CognitiveData_updated$age1,na.rm=T)
      mean(CognitiveData_updated$age1,na.rm=T)


range(CognitiveData$interval,na.rm=T)
mean(CognitiveData$interval,na.rm=T)

      ##### Updated
      #####
      range(CognitiveData_updated$interval,na.rm=T)
      mean(CognitiveData_updated$interval,na.rm=T)


#Define wave 2 missingness as a factor 
CognitiveData$missingt2<-as.factor(as.numeric(is.na(CognitiveData$t2ssvocab)))
levels(CognitiveData$missingt2)<-c('Complete data', 'Missing at T2')

      ##### Updated
      #####
      CognitiveData_updated$missingt2<-as.factor(as.numeric(is.na(CognitiveData_updated$t2ssvocab)))
      levels(CognitiveData_updated$missingt2)<-c('Complete data', 'Missing at T2')


#Vocabulary time 1 for those with complete data versus only T1 
t.test(t1ssvocab~missingt2,data=CognitiveData)
bfdatvoc<-data.frame(CognitiveData$t1ssvocab,CognitiveData$missingt2)
bfdatvoc<-na.omit(bfdatvoc)
colnames(bfdatvoc)<-c('t1ssvocab','Missing')
1/ttestBF(formula=t1ssvocab~Missing,data=bfdatvoc)

      ##### Updated
      #####
      t.test(t1ssvocab~missingt2,data=CognitiveData_updated)
      bfdatvoc_updated<-data.frame(CognitiveData_updated$t1ssvocab,CognitiveData_updated$missingt2)
      bfdatvoc_updated<-na.omit(bfdatvoc_updated)
      colnames(bfdatvoc_updated)<-c('t1ssvocab_updated','Missing_updated')
      1/ttestBF(formula=t1ssvocab_updated~Missing_updated,data=bfdatvoc_updated)

#Matrix reasoning time 1 for those with complete data versus only T1 
t.test(t1ssmatrix~missingt2,data=CognitiveData)
bfdatmat<-data.frame(CognitiveData$t1ssmatrix,CognitiveData$missingt2)
bfdatmat<-na.omit(bfdatmat)
colnames(bfdatmat)<-c('t1ssmatrix','Missing')
1/ttestBF(formula=t1ssmatrix~Missing,data=bfdatmat)

      ##### Updated
      #####
      t.test(t1ssmatrix~missingt2,data=CognitiveData_updated)
      bfdatmat_updated<-data.frame(CognitiveData_updated$t1ssmatrix,CognitiveData_updated$missingt2)
      bfdatmat_updated<-na.omit(bfdatmat_updated)
      colnames(bfdatmat_updated)<-c('t1ssmatrix_updated','Missing_updated')
      1/ttestBF(formula=t1ssmatrix_updated~Missing_updated,data=bfdatmat_updated)

#Gender distribution across missingness
sexmissing<-xtabs(~missingt2+sex,data=CognitiveData)
chisq.test(sexmissing)
1/contingencyTableBF(sexmissing,sampleType = "indepMulti",fixedMargin = "cols")
    
      ##### Updated
      #####
      sexmissing_updated<-xtabs(~missingt2+sex,data=CognitiveData_updated)
      chisq.test(sexmissing_updated)
      1/contingencyTableBF(sexmissing_updated,sampleType = "indepMulti",fixedMargin = "cols")


#Define wave 2 missingness as a factor for full demographics
fulldemos$missingt2<-as.factor(as.numeric(is.na(fulldemos$t2ssvocab)))
levels(fulldemos$missingt2)<-c('Complete data', 'Missing at T2')
fulldemos[fulldemos=='999']<-NA

      ##### Updated
      #####
      fulldemos_updated$missingt2<-as.factor(as.numeric(is.na(fulldemos_updated$t2ssvocab)))
      levels(fulldemos_updated$missingt2)<-c('Complete data', 'Missing at T2')
      fulldemos[fulldemos=='999']<-NA


#Health for those with complete data versus only T1 
t.test(psq_d1_prob~missingt2,data=fulldemos)  #Current
bfdathealth<-data.frame(fulldemos$psq_d1_prob,fulldemos$missingt2)
bfdathealth<-na.omit(bfdathealth)
colnames(bfdathealth)<-c('psq_d1_prob','Missing')
1/ttestBF(formula=psq_d1_prob~Missing,data=bfdathealth)

      ##### Updated
      #####
      t.test(psq_d1_prob~missingt2,data=fulldemos_updated)  #Current
      bfdathealth_updated<-data.frame(fulldemos_updated$psq_d1_prob,fulldemos_updated$missingt2)
      bfdathealth_updated<-na.omit(bfdathealth_updated)
      colnames(bfdathealth_updated)<-c('psq_d1_prob','Missing')
      1/ttestBF(formula=psq_d1_prob~Missing,data=bfdathealth_updated)

t.test(psq_d2_prob_pt~missingt2,data=fulldemos)   #Past
bfdathealth<-data.frame(fulldemos$psq_d2_prob_pt,fulldemos$missingt2)
bfdathealth<-na.omit(bfdathealth)
colnames(bfdathealth)<-c('psq_d2_prob_pt','Missing')
1/ttestBF(formula=psq_d2_prob_pt~Missing,data=bfdathealth)

      ##### Updated
      #####
      t.test(psq_d2_prob_pt~missingt2,data=fulldemos_updated)   #Past
      bfdathealth<-data.frame(fulldemos_updated$psq_d2_prob_pt,fulldemos_updated$missingt2)
      bfdathealth_updated<-na.omit(bfdathealth_updated)
      colnames(bfdathealth_updated)<-c('psq_d2_prob_pt','Missing')
      1/ttestBF(formula=psq_d2_prob_pt~Missing,data=bfdathealth_updated)

#Left school mother for those with complete data versus only T1 
t.test(psq_b1_leave_school_mum_recode~missingt2,data=fulldemos)
bfdatedumum<-data.frame(fulldemos$psq_b1_leave_school_mum_recode,fulldemos$missingt2)
bfdatedumum<-na.omit(bfdatedumum)
colnames(bfdatedumum)<-c('psq_b1_leave_school_mum_recode','Missing')
1/ttestBF(formula=psq_b1_leave_school_mum_recode~Missing,data=bfdatedumum)

      ##### Updated
      #####
      t.test(psq_b1_leave_school_mum_recode~missingt2,data=fulldemos_updated)
      bfdatedumum_updated<-data.frame(fulldemos_updated$psq_b1_leave_school_mum_recode,fulldemos_updated$missingt2)
      bfdatedumum_updated<-na.omit(bfdatedumum_updated)
      colnames(bfdatedumum_updated)<-c('psq_b1_leave_school_mum_recode','Missing')
      1/ttestBF(formula=psq_b1_leave_school_mum_recode~Missing,data=bfdatedumum_updated)



#Left school father for those with complete data versus only T1 
t.test(psq_b1_leave_school_dad_recode~missingt2,data=fulldemos)
bfdatedudad<-data.frame(fulldemos$psq_b1_leave_school_dad_recode,fulldemos$missingt2)
bfdatedudad<-na.omit(bfdatedudad)
colnames(bfdatedudad)<-c('psq_b1_leave_school_dad_recode','Missing')
1/ttestBF(formula=psq_b1_leave_school_dad_recode~Missing,data=bfdatedudad)

      ##### Updated
      #####
      t.test(psq_b1_leave_school_dad_recode~missingt2,data=fulldemos_updated)
      bfdatedudad_updated<-data.frame(fulldemos_updated$psq_b1_leave_school_dad_recode,fulldemos_updated$missingt2)
      bfdatedudad_updated<-na.omit(bfdatedudad_updated)
      colnames(bfdatedudad_updated)<-c('psq_b1_leave_school_dad_recode','Missing')
      1/ttestBF(formula=psq_b1_leave_school_dad_recode~Missing,data=bfdatedudad_updated)



#Age at time 1 for those with complete data versus only T1
bfdatage<-data.frame(CognitiveData$age1,CognitiveData$missingt2)
t.test(age1~missingt2,data=CognitiveData)
bfdatage<-na.omit(bfdatage)
colnames(bfdatage)<-c('Age','Missing')
ttestBF(formula=Age~Missing,data=bfdatage)
t.test(CognitiveData$age1~CognitiveData$missingt2)

      ##### Updated
      #####
      bfdatage_updated<-data.frame(CognitiveData_updated$age1,CognitiveData_updated$missingt2)
      t.test(age1~missingt2,data=CognitiveData_updated)
      bfdatage_updated<-na.omit(bfdatage_updated)
      colnames(bfdatage_updated)<-c('Age','Missing')
      ttestBF(formula=Age~Missing,data=bfdatage_updated)
      t.test(CognitiveData_updated$age1~CognitiveData_updated$missingt2)


#BIS for those with complete data versus only T1 
t.test(bistotal~missingt2,data=fulldemos)
bfdatBIS<-data.frame(fulldemos$bistotal,fulldemos$missingt2)
bfdatBIS<-na.omit(bfdatBIS)
colnames(bfdatBIS)<-c('bistotal','Missing')
ttestBF(formula=bistotal~Missing,data=bfdatBIS)
cohen.d(bfdatBIS$bistotal~bfdatBIS$Missing)

fulldemos$biscutoff<-as.numeric(fulldemos$bistotal>72)
bisred<-subset(fulldemos,biscutoff==0)


      ##### Updated
      #####
      t.test(bistotal~missingt2,data=fulldemos_updated)
      bfdatBIS_updated<-data.frame(fulldemos_updated$bistotal,fulldemos_updated$missingt2)
      bfdatBIS_updated<-na.omit(bfdatBIS_updated)
      colnames(bfdatBIS_updated)<-c('bistotal','Missing')
      ttestBF(formula=bistotal~Missing,data=bfdatBIS_updated)
      cohen.d(bfdatBIS_updated$bistotal~bfdatBIS_updated$Missing)
      
      fulldemos_updated$biscutoff<-as.numeric(fulldemos_updated$bistotal>72)
      bisred_updated<-subset(fulldemos_updated,biscutoff==0)




#Descriptives (Table 1)
length(na.omit(CognitiveData$t1ssmatrix))
length(na.omit(CognitiveData$t2ssmatrix))
length(na.omit(CognitiveData$t1ssvocab))
length(na.omit(CognitiveData$t2ssvocab))

      ##### Updated
      #####
      length(na.omit(CognitiveData_updated$t1ssmatrix))
      length(na.omit(CognitiveData_updated$t2ssmatrix))
      length(na.omit(CognitiveData_updated$t1ssvocab))
      length(na.omit(CognitiveData_updated$t2ssvocab))

mean(na.omit(CognitiveData$t1ssmatrix))
mean(na.omit(CognitiveData$t2ssmatrix))
mean(na.omit(CognitiveData$t1ssvocab))
mean(na.omit(CognitiveData$t2ssvocab))


      ##### Updated
      #####
      mean(na.omit(CognitiveData_updated$t1ssmatrix))
      mean(na.omit(CognitiveData_updated$t2ssmatrix))
      mean(na.omit(CognitiveData_updated$t1ssvocab))
      mean(na.omit(CognitiveData_updated$t2ssvocab))


range(na.omit(CognitiveData$t1ssmatrix))
range(na.omit(CognitiveData$t2ssmatrixraw))
range(na.omit(CognitiveData$t1ssvocab))
range(na.omit(CognitiveData$t2ssvocabraw))

      ##### Updated
      #####
      range(na.omit(CognitiveData_updated$t1ssmatrix))
      range(na.omit(CognitiveData_updated$t2ssmatrixraw))
      range(na.omit(CognitiveData_updated$t1ssvocab))
      range(na.omit(CognitiveData_updated$t2ssvocabraw))


sd(na.omit(CognitiveData$t1ssmatrix))
sd(na.omit(CognitiveData$t2ssmatrixraw))
sd(na.omit(CognitiveData$t1ssvocab))
sd(na.omit(CognitiveData$t2ssvocabraw))

      ##### Updated
      #####
      sd(na.omit(CognitiveData_updated$t1ssmatrix))
      sd(na.omit(CognitiveData_updated$t2ssmatrixraw))
      sd(na.omit(CognitiveData_updated$t1ssvocab))
      sd(na.omit(CognitiveData_updated$t2ssvocabraw))

e1071::skewness(na.omit(CognitiveData$t1ssmatrix))
e1071::skewness(na.omit(CognitiveData$t2ssmatrixraw))
e1071::skewness(na.omit(CognitiveData$t1ssvocab))
e1071::skewness(na.omit(CognitiveData$t2ssvocabraw))

      ##### Updated
      #####
      e1071::skewness(na.omit(CognitiveData_updated$t1ssmatrix))
      e1071::skewness(na.omit(CognitiveData_updated$t2ssmatrixraw))
      e1071::skewness(na.omit(CognitiveData_updated$t1ssvocab))
      e1071::skewness(na.omit(CognitiveData_updated$t2ssvocabraw))

e1071::kurtosis(na.omit(CognitiveData$t1ssmatrix))
e1071::kurtosis(na.omit(CognitiveData$t2ssmatrixraw))
e1071::kurtosis(na.omit(CognitiveData$t1ssvocab))
e1071::kurtosis(na.omit(CognitiveData$t2ssvocabraw))


      ##### Updated
      #####
      e1071::kurtosis(na.omit(CognitiveData_updated$t1ssmatrix))
      e1071::kurtosis(na.omit(CognitiveData_updated$t2ssmatrixraw))
      e1071::kurtosis(na.omit(CognitiveData_updated$t1ssvocab))
      e1071::kurtosis(na.omit(CognitiveData_updated$t2ssvocabraw))

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

    
      ##### Updated
      #####
      latentdiffmatrix_updated <- '
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
      
      fitlatentdiffmatrix_updated <- growth(latentdiffmatrix_updated, data=CognitiveData_updated, estimator='mlr', fixed.x=FALSE,missing='fiml')
      summary(fitlatentdiffmatrix_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE) 
      #Note we here report the RMSEA CI from the ML estimate, not MLR, as MLR yields an impossible range given the point estimate

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

    
      ##### Updated
      #####
      latentdiffmatrixnoselffb_updated <- '
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
      
      fitlatentdiffmatrixnoselffb_updated <- growth(latentdiffmatrixnoselffb_updated, data=CognitiveData_updated, estimator='mlr', fixed.x=FALSE,missing='fiml')
      summary(fitlatentdiffmatrixnoselffb_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE) 



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


      ##### Updated
      #####
      latentdiffvocab_updated <- '	#latent change model;
      
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
      
      fitlatentdiffvocab_updated <- growth(latentdiffvocab_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitlatentdiffvocab_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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


      ##### Updated
      #####
      latentdiffvocabselffb_updated <- '	#latent change model;
      
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
      
      fitlatentdiffvocabselffb_updated <- growth(latentdiffvocabselffb_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitlatentdiffvocabselffb_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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

      ##### Updated
      #####
      #g factor MI test
      gfactornocons_updated <- '
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
      fitgfactornocons_updated <- growth(gfactornocons_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitgfactornocons_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#Compute delta-cfi
var1<-c('t1ssmatrix','t1ssvocab')
var2<-c('t2ssmatrix','t2ssvocab')
constrainedVar <- list(var1, var2)
longInvariance(gfactornocons, auto=1, constrainAuto=TRUE,varList=constrainedVar,data=CognitiveData,strict = FALSE,missing='fiml',estimator='mlr',fixed.x=FALSE) #
longInvariance(gfactornocons, auto=1, constrainAuto=TRUE,varList=constrainedVar,data=CognitiveData,strict = FALSE,estimator='mlr',fixed.x=FALSE)

      ##### Updated
      #####
      var1<-c('t1ssmatrix','t1ssvocab')
      var2<-c('t2ssmatrix','t2ssvocab')
      constrainedVar <- list(var1, var2)
      longInvariance(gfactornocons_updated, auto=1, constrainAuto=TRUE,varList=constrainedVar,data=CognitiveData_updated,strict = FALSE,missing='fiml',estimator='mlr',fixed.x=FALSE) #
      longInvariance(gfactornocons_updated, auto=1, constrainAuto=TRUE,varList=constrainedVar,data=CognitiveData_updated,strict = FALSE,estimator='mlr',fixed.x=FALSE)
      

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
    
      ##### Updated
      #####
      gfactor_updated <- '
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
      fitgfactor_updated <- growth(gfactor_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitgfactor_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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

  
      ##### Updated
      #####
      investment_updated <- '	#latent change model;
      
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
      fitinvestment_updated <- growth(investment_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitinvestment_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)




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
summary(fitmutualism, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


      ##### Updated
      #####
      mutualism_updated <- '	#latent change model;
      
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
      fitmutualism_updated <- growth(mutualism_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualism_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



#Compute effect sizes:
0.203*0.203 # Coupling from t1ssvocab to etamat
0.144*0.144 # Coupling from t1ssmatrix to etavoc 

      ##### Updated
      #####
      0.203*0.203 # Coupling from t1ssvocab to etamat
      0.155*0.155 # Coupling from t1ssmatrix to etavoc 
    

#Model fit (table 2)

#fit all
fitMeasures(fitgfactor, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
fitMeasures(fitinvestment, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
fitMeasures(fitmutualism, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))

    
      ##### Updated
      #####
      fitMeasures(fitgfactor_updated, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
      fitMeasures(fitinvestment_updated, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
      fitMeasures(fitmutualism_updated, c("chisq.scaled", "df", "pvalue.scaled", "cfi.scaled", "srmr","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled"))
    


#Model comparison using AIC

#Compute akaike weights according to Wagenmakers and Farrell

aicvecall<-c(AIC(fitgfactor),AIC(fitmutualism),AIC(fitinvestment))
valuesweights<-akaike.weights(aicvecall)$weights

      ##### Updated
      #####
      aicvecall_updated<-c(AIC(fitgfactor_updated),AIC(fitmutualism_updated),AIC(fitinvestment_updated))
      valuesweights_updated<-akaike.weights(aicvecall_updated)$weights

      
#Normalized
valuesweights[2]/(valuesweights[1]+valuesweights[3]+valuesweights[2])

      ##### Updated
      #####
      valuesweights_updated[2]/(valuesweights_updated[1]+valuesweights_updated[3]+valuesweights_updated[2])


#Relative evidence ratio
valuesweights[2]/valuesweights[1]+valuesweights[3]

      ##### Updated
      #####
      valuesweights_updated[2]/valuesweights_updated[1]+valuesweights_updated[3]


#Chi square test of nested models:
anova(fitinvestment,fitmutualism)
  
      ##### Updated
      #####
      anova(fitinvestment_updated,fitmutualism_updated)

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

anova(fitmutualismnovaretavoc,fitmutualism)


      ##### Updated
      #####
      mutualismnovaretavoc_updated <- '	#latent change model;
      
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
      fitmutualismnovaretavoc_updated <- growth(mutualismnovaretavoc_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualismnovaretavoc_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
      
      anova(fitmutualismnovaretavoc_updated,fitmutualism_updated)


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



      ##### Updated
      #####
      mutualismnovaretamat_updated <- '	#latent change model;
      
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
      fitmutualismnovaretamat_updated <- growth(mutualismnovaretamat_updated, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualismnovaretamat_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

      #Comparing model fit when variance of change scores fixed to 0
      anova(fitmutualism_updated,fitmutualismnovaretamat_updated)
      

####################
#Comparing model fit with standardized coupling parameters equality constrained. To do so,
# we first have to estimate the variances without any coupling present

mutualism_est_var<- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix


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
fitmutualism_est_var <- growth(mutualism_est_var, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualism_est_var, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


      ##### Updated
      #####
      mutualism_est_var_updated<- '	#latent change model;
      
      t1ssvocab ~ 1
      t2ssvocab ~ 1*t1ssvocab
      etavoc =~ 1*t2ssvocab
      
      t1ssmatrix ~~ t1ssmatrix
      
      etamat =~ 1*t2ssmatrix
      
      t2ssmatrix ~ 1*t1ssmatrix
      
      
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
      fitmutualism_est_var_updated <- growth(mutualism_est_var_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualism_est_var_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Now we insert the variances into the new model and estimate the model
#with free (labelled) coupling parameters

mutualismfreecoupling <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~b1*t1ssvocab+t1ssmatrix
etavoc~b2*t1ssmatrix+t1ssvocab


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

b1s:=b1* sqrt(56.556 / 4.088)
b2s:=b2* sqrt(9.808 / 13.828)


'
fitmutualismfreecoupling <- growth(mutualismfreecoupling, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualismfreecoupling, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

      ##### Updated
      #####
      mutualismfreecoupling_updated <- '	#latent change model;
      
      t1ssvocab ~ 1
      t2ssvocab ~ 1*t1ssvocab
      etavoc =~ 1*t2ssvocab
      
      t1ssmatrix ~~ t1ssmatrix
      
      etamat =~ 1*t2ssmatrix
      
      t2ssmatrix ~ 1*t1ssmatrix
      
      etamat~b1*t1ssvocab+t1ssmatrix
      etavoc~b2*t1ssmatrix+t1ssvocab
      
      
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
      
      b1s:=b1* sqrt(56.556 / 4.088)
      b2s:=b2* sqrt(9.808 / 13.828)
      
      
      '
      fitmutualismfreecoupling_updated <- growth(mutualismfreecoupling_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualismfreecoupling_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



#Now we fit a model with standardized coupling parameters constrained to equality
mutualism_equalcons_coupling <- '	#latent change model;

t1ssvocab ~ 1
t2ssvocab ~ 1*t1ssvocab
etavoc =~ 1*t2ssvocab

t1ssmatrix ~~ t1ssmatrix

etamat =~ 1*t2ssmatrix

t2ssmatrix ~ 1*t1ssmatrix

etamat~b1*t1ssvocab+t1ssmatrix
etavoc~b2*t1ssmatrix+t1ssvocab


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

b1s:=b1* sqrt(56.556 / 4.088)
b2s:=b2* sqrt(9.808 / 13.828)
b2s==b1s

'
fitmutualism_equalcons_coupling <- growth(mutualism_equalcons_coupling, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitmutualism_equalcons_coupling, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#Model with equality constrained standardized coupling doesn't fit worse
anova(fitmutualism_equalcons_coupling,fitmutualismfreecoupling)

      ##### Updated
      #####
      mutualism_equalcons_coupling_updated <- '	#latent change model;
      
      t1ssvocab ~ 1
      t2ssvocab ~ 1*t1ssvocab
      etavoc =~ 1*t2ssvocab
      
      t1ssmatrix ~~ t1ssmatrix
      
      etamat =~ 1*t2ssmatrix
      
      t2ssmatrix ~ 1*t1ssmatrix
      
      etamat~b1*t1ssvocab+t1ssmatrix
      etavoc~b2*t1ssmatrix+t1ssvocab
      
      
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
      
      b1s:=b1* sqrt(56.556 / 4.088)
      b2s:=b2* sqrt(9.808 / 13.828)
      b2s==b1s
      
      '
      fitmutualism_equalcons_coupling_updated <- growth(mutualism_equalcons_coupling_updated, data=CognitiveData, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualism_equalcons_coupling_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

      #Model with equality constrained standardized coupling doesn't fit worse
      anova(fitmutualism_equalcons_coupling_updated,fitmutualismfreecoupling_updated)



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

      ##### Updated
      #####
      gfactor_CCA_updated <- '
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
      fitgfactor_CCA_updated <- growth(gfactor_CCA_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE)
      summary(fitgfactor_CCA_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

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

      ##### Updated
      #####
      investment_CCA_updated <- '	#latent change model;
      
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
      fitinvestment_CCA_updated <- growth(investment_CCA_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE)
      summary(fitinvestment_CCA_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


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

      ##### Updated
      #####
      mutualism_CCA_updated <- '	#latent change model;
      
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
      fitmutualism_CCA_updated <- growth(mutualism_CCA_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE)
      summary(fitmutualism_CCA_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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

      ##### Updated
      #####
      mutualismmultigroup_updated <- '	#latent change model;
      
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
      fitmutualismmultigroup_updated <- growth(mutualismmultigroup_updated, data=CognitiveData_updated, estimator='mlr',group='sex',fixed.x=FALSE,missing='fiml')
      summary(fitmutualismmultigroup_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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

      ##### Updated
      #####
      mutualismmultigroup_cons_updated <- '	#latent change model;
      
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
      fitmutualismmultigroup_cons_updated <- growth(mutualismmultigroup_cons_updated, data=CognitiveData_updated, estimator='mlr',group='sex',fixed.x=FALSE,missing='fiml',group.equal = c("loadings", "intercepts", "means", "thresholds", "regressions", "residuals", "residual.covariances", "lv.variances","lv.covariances"))
      summary(fitmutualismmultigroup_cons_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


anova(fitmutualismmultigroup_cons,fitmutualismmultigroup)

      ##### Updated
      #####
      anova(fitmutualismmultigroup_cons_updated,fitmutualismmultigroup_updated)
      

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


      ##### Updated
      #####
      #Figure 1: Behavioural scores (note that the scaled scores and ages for time 2 are used)
      newcogdatred_updated<-CognitiveData_updated
      newcogdatage_updated<-melt(data.frame(newcogdatred_updated$age1,newcogdatred_updated$age2))
      newcogdatmat_updated<-melt(data.frame(newcogdatred_updated$t1ssmatrix,newcogdatred_updated$t2ssmatrix))
      newcogdatmat_updated$id<-c(factor(1:nrow(newcogdatred_updated)),factor(1:nrow(newcogdatred_updated)))
      plotnewcogdatmat_updated<-data.frame(newcogdatmat_updated[,2:3])
      plotnewcogdatmat_updated$age<-newcogdatage_updated$value
      ggplot(plotnewcogdatmat_updated,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='dodgerblue')+geom_line(alpha=.7,col='dodgerblue',lwd=1)+ylab('Matrix reasoning score')+theme(text = element_text(size=22))+ylim(13,35)+xlab('Age')
      ggsave('Matrix_erratum.tiff',compression='lzw',height=5,width=8)
      
      
newcogdatvoctemp<-CognitiveData
newcogdatage<-melt(data.frame(newcogdatvoctemp$age1,newcogdatvoctemp$age2))
newcogdatvoc<-melt(data.frame(newcogdatvoctemp$t1ssvocab,newcogdatvoctemp$t2ssvocab))
newcogdatvoc$id<-c(factor(1:nrow(newcogdatred)),factor(1:nrow(newcogdatred)))
plotnewcogdatvoc<-data.frame(newcogdatvoc[,2:3])
plotnewcogdatvoc$age<-newcogdatage$value
ggplot(plotnewcogdatvoc,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='#ff7f7f')+geom_line(alpha=.7,col='#ff7f7f',lwd=1)+ylab('Vocabulary score')+theme(text = element_text(size=22))+ylim(16,81)+xlab('Age')

ggsave('Vocab.tiff',compression='lzw',height=5,width=8)

      ##### Updated
      #####
      newcogdatvoctemp_updated<-CognitiveData_updated
      newcogdatage_updated<-melt(data.frame(newcogdatvoctemp_updated$age1,newcogdatvoctemp_updated$age2))
      newcogdatvoc_updated<-melt(data.frame(newcogdatvoctemp_updated$t1ssvocab,newcogdatvoctemp_updated$t2ssvocab))
      newcogdatvoc_updated$id<-c(factor(1:nrow(newcogdatred_updated)),factor(1:nrow(newcogdatred_updated)))
      plotnewcogdatvoc_updated<-data.frame(newcogdatvoc_updated[,2:3])
      plotnewcogdatvoc_updated$age<-newcogdatage_updated$value
      ggplot(plotnewcogdatvoc_updated,aes(age,value,group=id))+geom_point(alpha=.7,size=2,col='#ff7f7f')+geom_line(alpha=.7,col='#ff7f7f',lwd=1)+ylab('Vocabulary score')+theme(text = element_text(size=22))+ylim(16,81)+xlab('Age')
      ggsave('Vocab_erratum.tiff',compression='lzw',height=5,width=8)
      


#Figure 3: Plotting model comparison

#Figure 3A: Plot all IC's
values<-c(AIC(fitgfactor),AIC(fitmutualism),AIC(fitinvestment),BIC(fitgfactor),BIC(fitmutualism),BIC(fitinvestment))
IC<-factor(c('AIC','AIC','AIC','BIC','BIC','BIC'))
Model<-factor(c('g factor','Mutualism', 'Investment','g factor','Mutualism','Investment'))
levels(Model)<-c('g factor', 'Investment','Mutualism')
modeldat=data.frame(values,IC,Model)
ggplot(modeldat,aes(IC,values,fill=Model))+geom_bar(stat="identity",position="dodge")+coord_cartesian(ylim=c(18400,18570))+xlab('Information criterion')+theme_grey(base_size = 22)#+scale_fill_brewer(palette="Set1")+

ggsave('ICs.tiff',compression='lzw',height=5,width=8)

      ##### Updated
      #####
      values_updated<-c(AIC(fitgfactor_updated),AIC(fitmutualism_updated),AIC(fitinvestment_updated),BIC(fitgfactor_updated),BIC(fitmutualism_updated),BIC(fitinvestment_updated))
      IC_updated<-factor(c('AIC','AIC','AIC','BIC','BIC','BIC'))
      Model<-factor(c('g factor','Mutualism', 'Investment','g factor','Mutualism','Investment'))
      levels(Model)<-c('g factor', 'Investment','Mutualism')
      modeldat_updated=data.frame(values_updated,IC_updated,Model)
      ggplot(modeldat_updated,aes(IC_updated,values_updated,fill=Model))+geom_bar(stat="identity",position="dodge")+coord_cartesian(ylim=c(18292,18462))+xlab('Information criterion')+ylab('values')+theme_grey(base_size = 22)#+scale_fill_brewer(palette="Set1")+
      ggsave('ICs_updated.tiff',compression='lzw',height=5,width=8)
      

#Figure 3B: Plot all as akaike weights

Model<-factor(c('g factor','Mutualism','Investment'))
modeldatweights=data.frame(valuesweights,Model)
ggplot(modeldatweights,aes(Model,valuesweights,fill=Model))+geom_bar(stat="identity",position="dodge")+ylab('Likelihood')+xlab('')+ggtitle('Akaike weights')+theme_grey(base_size = 22)

ggsave('Akaikeweights.tiff',compression='lzw',height=5,width=8)

      ##### Updated
      #####
      Model<-factor(c('g factor','Mutualism','Investment'))
      modeldatweights_updated=data.frame(valuesweights_updated,Model)
      ggplot(modeldatweights_updated,aes(Model,valuesweights,fill=Model))+geom_bar(stat="identity",position="dodge")+ylab('Likelihood')+xlab('')+ggtitle('Akaike weights')+theme_grey(base_size = 22)



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

      
      ##### Updated
      #####
      mutualism_age_on_change_updated <- '	#latent change model;
      
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
      fitmutualism_age_on_change_updated <- growth(mutualism_age_on_change_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualism_age_on_change_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
      
      anova(fitmutualism_updated,fitmutualism_age_on_change_updated)
      
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

      ##### Updated
      #####
      mutualism_age_lin_age_squa_updated <- '	#latent change model;
      
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
      fitmutualism_age_lin_age_squa_updated <- growth(mutualism_age_lin_age_squa_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualism_age_lin_age_squa_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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



      ##### Updated
      #####
      mutualism_age_lin_age_squa_cons_updated <- '	#latent change model;
      
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
      fitmutualism_age_lin_age_squa_cons_updated <- growth(mutualism_age_lin_age_squa_cons_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitmutualism_age_lin_age_squa_cons_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
      
      #Compare models with age squared fixed to 0
      anova(fitmutualism_age_lin_age_squa_updated,fitmutualism_age_lin_age_squa_cons_updated)
      

# Include age as predictor of T1 raw scores (same way as mutualism)
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

      ##### Updated
      #####
      gfactor_diffage_a_updated <- '
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
      fitgfactor_diffage_a_updated <- growth(gfactor_diffage_a_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
      summary(fitgfactor_diffage_a_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



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

##### Updated
#####
gfactor_diffage_b_updated <- '
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
fitgfactor_diffage_b_updated <- growth(gfactor_diffage_b_updated, data=CognitiveData_updated, estimator='mlr',fixed.x=FALSE,missing='fiml')
summary(fitgfactor_diffage_b_updated, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)



BIC(fitgfactor)-BIC(fitmutualism)
BIC(fitgfactor_diffage_a)-BIC(fitmutualism)
BIC(fitgfactor_diffage_b)-BIC(fitmutualism)


BIC(fitgfactor_updated)-BIC(fitmutualism_updated)
BIC(fitgfactor_diffage_a_updated)-BIC(fitmutualism_updated)
BIC(fitgfactor_diffage_b_updated)-BIC(fitmutualism_updated)


summary(fitmutualism, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
parameterEstimates(fitmutualism) 






