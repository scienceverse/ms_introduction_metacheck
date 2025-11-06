
######################### Cross Cultural Sex Differences in Mate Preferences #######################
#### Kathryn V. Walter & Daniel Conroy-Beam ####




### load packages ###
library(ggplot2)
library(lmerTest)
library(psych)
library(gridExtra)
library(ggpubr)
library(tidyr)


### set seed ###
set.seed(1302019)


### load data ###
data<-read.csv(file.choose())

### run D and logistic analyses? if no, x=0, if yes, x>0 ###
x<-0





################################ prepare data ############################################

#change categorical data to factors
data$PIN<-as.factor(data$PIN)
data$CIN<-as.factor(data$CIN)
data$sample<-as.factor(data$sample)
data$sex<-as.factor(data$sex)
data$religious<-as.factor(data$religious)
data$relstat<-as.factor(data$relstat)
data$relstat2<-as.factor(data$relstat2)

#divide gross measures of pathogen prevalence by population size 
data$cmc_yll<-data$cmc_yll/data$popsize
data$infect_death<-data$infect_death/data$popsize
data$infect_yll<-data$infect_yll/data$popsize

#change contrasts for the categorical predictor variables
contrasts(data$continent)<-contr.sum(6)
contrasts(data$country_religion)<-contr.sum(14)

#create age difference variable
data$agediff<-(data$mate_age-data$age)

#new pathogen prevalence index (average of deaths and yll)
data$pathindex<-((scale(data$infect_death)+scale(data$infect_yll))/2)

#new gender equality index (PCA of GII, GDI, GGGI)
gepca<-pca(data[,28:30],nfactors=1)
data$gepcascores<-gepca$scores

#standardize outcome variables
data$zagediff<-scale(data$agediff)
data$zideal_resources<-scale(data$ideal_resources)
data$zideal_intelligence<-scale(data$ideal_intelligence)
data$zideal_kindness<-scale(data$ideal_kindness)
data$zideal_health<-scale(data$ideal_health)
data$zideal_physatt<-scale(data$ideal_physatt)

#standardize predictor variables 
data$zcmc_yll<-scale(data$cmc_yll)
data$zpathindex<-scale(data$pathindex)

#create countrylist
countrylist<-unique(data$country)





###################################### Analyses ################################################




################ Sex Differences in Preferences ###################

#Good Financial Prospects
gfpdiff<-lmer(zideal_resources~sex+(1+sex|CIN),data=data)
gfpslopes<-coef(gfpdiff)$CIN[,2]

#Physical Attractiveness
padiff<-lmer(zideal_physatt~sex+(1+sex|CIN),data=data)
paslopes<-coef(padiff)$CIN[,2]

#Health
hdiff<-lmer(zideal_health~sex+(1+sex|CIN),data=data)
hslopes<-coef(hdiff)$CIN[,2]

#Intelligence
intdiff<-lmer(zideal_intelligence~sex+(1+sex|CIN),data=data)
intslopes<-coef(intdiff)$CIN[,2]

#Kindness
kinddiff<-lmer(zideal_kindness~sex+(1+sex|CIN),data=data)
kindslopes<-coef(kinddiff)$CIN[,2]

#Age
#all ages
agediff<-lmer(zagediff~sex+(1+sex|CIN),data=data)
ageslopes<-coef(agediff)$CIN[,2]








################# Gender Equality No Control Variables ###########################



##### Good Financial Prospects #####

#gggi
gggigfp<-lmer(zideal_resources~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
gdi2015gfp<-lmer(zideal_resources~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
giigfp<-lmer(zideal_resources~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
gem1995gfp<-lmer(zideal_resources~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
gdi1995gfp<-lmer(zideal_resources~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
pcagfp<-lmer(zideal_resources~sex*(scale(gepcascores))+(1+sex|CIN),data=data)





##### Physical Attractiveness #####

#gggi
gggipa<-lmer(zideal_physatt~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
gdi2015pa<-lmer(zideal_physatt~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
giipa<-lmer(zideal_physatt~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
gem1995pa<-lmer(zideal_physatt~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
gdi1995pa<-lmer(zideal_physatt~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
pcapa<-lmer(zideal_physatt~sex*gepcascores+(1+sex|CIN),data=data)





##### Health #####

#gggi
gggih<-lmer(zideal_health~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
gdi2015h<-lmer(zideal_health~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
giih<-lmer(zideal_health~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
gem1995h<-lmer(zideal_health~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
gdi1995h<-lmer(zideal_health~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
pcah<-lmer(zideal_health~sex*gepcascores+(1+sex|CIN),data=data)





##### Kindness #####

#gggi
gggik<-lmer(zideal_kindness~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
gdi2015k<-lmer(zideal_kindness~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
giik<-lmer(zideal_kindness~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
gem1995k<-lmer(zideal_kindness~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
gdi1995k<-lmer(zideal_kindness~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
pcakind<-lmer(zideal_kindness~sex*gepcascores+(1+sex|CIN),data=data)





##### Intelligence #####

#gggi
gggiint<-lmer(zideal_intelligence~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
gdi2015int<-lmer(zideal_intelligence~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
giiint<-lmer(zideal_intelligence~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
gem1995int<-lmer(zideal_intelligence~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
gdi1995int<-lmer(zideal_intelligence~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
pcaint<-lmer(zideal_intelligence~sex*gepcascores+(1+sex|CIN),data=data)





##### Age #####

#gggi
#all ages
gggiageall<-lmer(zagediff~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
#all ages
gdi2015ageall<-lmer(zagediff~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
#all ages
giiageall<-lmer(zagediff~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
#all ages
gem1995ageall<-lmer(zagediff~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
#all ages
gdi1995ageall<-lmer(zagediff~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
#all ages
pcaageall<-lmer(zagediff~sex*(scale(gepcascores))+(1+sex|CIN),data=data)











############################# Gender Equality with Control Variables #############################





##### Good Financial Prospects #####

#gggi
gggigfp2<-lmer(zideal_resources~sex*(scale(gggi))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
gdi2015gfp2<-lmer(zideal_resources~sex*(scale(gdi2015))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
giigfp2<-lmer(zideal_resources~sex*(scale(gii))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
gem1995gfp2<-lmer(zideal_resources~sex*(scale(gem1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
gdi1995gfp2<-lmer(zideal_resources~sex*(scale(gdi1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
pcagfp2<-lmer(zideal_resources~sex*gepcascores+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)




##### Physical Attractiveness #####

#gggi
gggipa2<-lmer(zideal_physatt~sex*(scale(gggi))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
gdi2015pa2<-lmer(zideal_physatt~sex*(scale(gdi2015))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
giipa2<-lmer(zideal_physatt~sex*(scale(gii))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
gem1995pa2<-lmer(zideal_physatt~sex*(scale(gem1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
gdi1995pa2<-lmer(zideal_physatt~sex*(scale(gdi1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
pcapa2<-lmer(zideal_physatt~sex*gepcascores+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)




##### Health #####

#gggi
gggih2<-lmer(zideal_health~sex*(scale(gggi))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
gdi2015h2<-lmer(zideal_health~sex*(scale(gdi2015))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
giih2<-lmer(zideal_health~sex*(scale(gii))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
gem1995h2<-lmer(zideal_health~sex*(scale(gem1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
gdi1995h2<-lmer(zideal_health~sex*(scale(gdi1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
pcah2<-lmer(zideal_health~sex*gepcascores+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)




##### Intelligence #####

#gggi
gggiint2<-lmer(zideal_intelligence~sex*(scale(gggi))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
gdi2015int2<-lmer(zideal_intelligence~sex*(scale(gdi2015))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
giiint2<-lmer(zideal_intelligence~sex*(scale(gii))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
gem1995int2<-lmer(zideal_intelligence~sex*(scale(gem1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
gdi1995int2<-lmer(zideal_intelligence~sex*(scale(gdi1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
pcaint2<-lmer(zideal_intelligence~sex*gepcascores+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)




##### Kindness #####

#gggi
gggik2<-lmer(zideal_kindness~sex*(scale(gggi))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
gdi2015k2<-lmer(zideal_kindness~sex*(scale(gdi2015))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
giik2<-lmer(zideal_kindness~sex*(scale(gii))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
gem1995k2<-lmer(zideal_kindness~sex*(scale(gem1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
gdi1995k2<-lmer(zideal_kindness~sex*(scale(gdi1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
pcak2<-lmer(zideal_kindness~sex*gepcascores+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)




##### Age #####

#gggi
#all ages
gggiage2all<-lmer(zagediff~sex*(scale(gggi))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
#all ages
gdi2015age2all<-lmer(zagediff~sex*(scale(gdi2015))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
#all ages
giiage2all<-lmer(zagediff~sex*(scale(gii))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
#all ages
gem1995age2all<-lmer(zagediff~sex*(scale(gem1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
#all ages
gdi1995age2all<-lmer(zagediff~sex*(scale(gdi1995))+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
#all ages
pcaage2all<-lmer(zagediff~sex*gepcascores+scale(lattitude)+country_religion+scale(gdp_percap)+continent+(1+sex|CIN),data=data)







########################## Pathogen Prevalence No Control Variables ############################



###### Gangestad and Buss Index ######

#physical attractivenss 
pppa1<-lmer(zideal_physatt~(scale(gb_path))+(1|CIN),data=data)

#good financial prospects
ppgfp1<-lmer(zideal_resources~(scale(gb_path))+(1|CIN),data=data)

#health
pph1<-lmer(zideal_health~(scale(gb_path))+(1|CIN),data=data)

#kindness
ppk1<-lmer(zideal_kindness~(scale(gb_path))+(1|CIN),data=data)

#intelligece
ppint1<-lmer(zideal_intelligence~(scale(gb_path))+(1|CIN),data=data)

#age
#all ages
ppagediffall<-lmer(zagediff~(scale(gb_path))+(1|CIN),data=data)




##### Years of Life Lost to Communicable Diseases #####

#physical attractivenss
pppa2<-lmer(zideal_physatt~zcmc_yll+(1|CIN),data=data)

#health
pph2<-lmer(zideal_health~zcmc_yll+(1|CIN),data=data)

#intelligence
ppint2<-lmer(zideal_intelligence~zcmc_yll+(1|CIN),data=data)

#kindness
ppkind2<-lmer(zideal_kindness~zcmc_yll+(1|CIN),data=data)

#good financial prospects
ppgfp2<-lmer(zideal_resources~zcmc_yll+(1|CIN),data=data)

#age
#all ages
ppage2all<-lmer(zagediff~zcmc_yll+(1|CIN),data=data)



##### Composite Pathogen Index #####

#physical attractivenss
pppa3<-lmer(zideal_physatt~zpathindex+(1|CIN),data=data)

#health
pph3<-lmer(zideal_health~zpathindex+(1|CIN),data=data)

#intelligence
ppint3<-lmer(zideal_intelligence~zpathindex+(1|CIN),data=data)

#kindness
ppkind3<-lmer(zideal_kindness~zpathindex+(1|CIN),data=data)

#good financial prospects
ppgfp3<-lmer(zideal_resources~zpathindex+(1|CIN),data=data)

#age
#all ages
ppage3all<-lmer(zagediff~zpathindex+(1|CIN),data=data)







################################# Pathogen Prevalence with Control Variables ###########################



#### Gangestad and Buss Index #####

#physical attractiveness
pppa1b<-lmer(zideal_physatt~scale(gb_path)+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#health
pph1b<-lmer(zideal_health~(scale(gb_path))+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#kindness
ppkind1b<-lmer(zideal_kindness~(scale(gb_path))+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#intelligence
ppint1b<-lmer(zideal_intelligence~(scale(gb_path))+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#good financial prospects
ppgfp1b<-lmer(zideal_resources~(scale(gb_path))+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#age
#all ages
ppage1ball<-lmer(zagediff~(scale(gb_path))+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)





##### Years of Life Lost to Communicable Diseases #####

#physical attractiveness
pppa2b<-lmer(zideal_physatt~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#health
pph2b<-lmer(zideal_health~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#kindness
ppkind2b<-lmer(zideal_kindness~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#intelligence
ppint2b<-lmer(zideal_intelligence~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#good financial prospects
ppgfp2b<-lmer(zideal_resources~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#age
#all ages
ppage2ball<-lmer(zagediff~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)




##### Composite Pathogen Index #####

#physical attractiveness
pppa3b<-lmer(zideal_physatt~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#health
pph3b<-lmer(zideal_health~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#kindness
ppkind3b<-lmer(zideal_kindness~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#intelligence
ppint3b<-lmer(zideal_intelligence~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#good financial prospects
ppgfp3b<-lmer(zideal_resources~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#age
#all ages
ppage3ball<-lmer(zagediff~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)






###################################### Mahalanobis D ###########################################


#start D and logistic analyses
if(x>0){
  



  #Del Giudice, (2009)
  
  # function maha(data_A, data_B, alpha=NULL, conf.level=.95)
  # returns the Mahalanobis distance D and the corresponding coefficients of overlap, computed from raw data. Can compute disattenuated estimates if desired. Note: the correlation matrices of the two groups are pooled by taking weighted means before computing D. 
  #
  # Arguments
  # data_A, data_B: raw matrices/data frames for the two groups. Must contain the same variables.
  # alpha: reliabilities vector (optional: only required for disattenuation)
  # conf.level: CI width (optional)
  #
  # Value
  # returns a list object containing some or all of the following:
  # D: mahalanobis D
  # CI_lower: CI lower bound (zero if not solvable)
  # CI_upper: CI upper bound (zero if not solvable)
  # OVL: coefficient of overlap (single distribution)
  # OVL2: Cohen's coefficient of overlap 1-U1 (joint distributions)
  # Dc: disattenuated D
  # OVLc: disattenuated coefficient of overlap (single distribution)
  # OVL2c: disattenuated Cohen's coefficient of overlap (joint distributions)
  # d_values: vector of Cohen's d values
  # dc_values: vector of disattenuated Cohen's d values
  
  
  
  maha <- function(data_A, data_B, alpha=NULL, conf.level=.95) {
    
    # preliminary computations: Ns, pooled variances, pooled correlation matrix
    
    nA=length(complete.cases(data_A))
    nB=length(complete.cases(data_B))
    
    cor_A=cor(data_A, use="pairwise.complete.obs")
    cor_B=cor(data_B, use="pairwise.complete.obs")
    pooled_cor=(cor_A*nA+cor_B*nB)/(nA+nB)
    
    pooled_variances=((nA-1)*diag(var(data_A, na.rm=T))+(nB-1)*diag(var(data_B, na.rm=T)))/(nA+nB)
    
    # compute Cohen's d values and Mahalanobis D
    
    d_values=(sapply(data_A,mean,na.rm=T)-sapply(data_B,mean,na.rm=T))/sqrt(pooled_variances)
    
    D2=mahalanobis(d_values, array(0, dim=c(length(d_values))), pooled_cor)
    output=list()
    output$D=sqrt(D2)
    
   
    # compute overlap coefficients
    
    output$OVL=2*pnorm(-output$D/2)
    output$OVL2=output$OVL/(2-output$OVL)
    
    # disattenuation
    
    if (is.null(alpha)==FALSE) {d_values.d=d_values/sqrt(alpha)
    alpha_matrix=sqrt(crossprod(t(alpha), t(alpha)))
    diag(alpha_matrix)=1
    pooled_cor.d=pooled_cor/alpha_matrix
    D2.d=mahalanobis(d_values.d, array(0, dim=c(length(d_values.d))), pooled_cor.d)
    output$Dc=sqrt(D2.d)
    }
    if (is.null(alpha)==FALSE) {output$OVLc=2*pnorm(-output$Dc/2)
    output$OVL2c=output$OVLc/(2-output$OVLc)
    }
    output$d_values=d_values
    if (is.null(alpha)==FALSE) {output$dc_values=d_values.d
    }
    
    # output
    
    return(output)
    
  }
  
  #End of Marco's script
  
  
  
  
  ##### D For All Preferences #####
  
  #create dataframe to store D, OVL, and confidence intervals
  mahadist<-data.frame("country"=countrylist,"D"=rep(0,45),"overlap"=rep(0,45),"overlap2"=rep(0,45), "DlowCI"=rep(0,45),"DhighCI"=rep(0,45), "OlowCI"=rep(0,45), "OhighCI"=rep(0,45), "d_int"=rep(0,45), "d_kind"=rep(0,45), "d_health"=rep(0,45),"d_physatt"=rep(0,45),"d_resources"=rep(0,45))
  
  
  #use maha function for each country
  for(i in 1:45){
    loopdist<-maha(data[data$sex==1&data$country==countrylist[i],17:21],data[data$sex==0&data$country==countrylist[i],17:21]) 
    mahadist[i,2]<-loopdist$D
    mahadist[i,3]<-loopdist$OVL
    mahadist[i,4]<-loopdist$OVL2
    mahadist[i,9]<-loopdist$d_values[1]
    mahadist[i,10]<-loopdist$d_values[2]
    mahadist[i,11]<-loopdist$d_values[3]
    mahadist[i,12]<-loopdist$d_values[4]
    mahadist[i,13]<-loopdist$d_values[5]
  }
  
  
  
  
  ##### D for Sex Differentiated Preferences #####
  mahadistSD<-data.frame("country"=countrylist,"D"=rep(0,45),"overlap"=rep(0,45),"overlap2"=rep(0,45))
  
  for(i in 1:45){
    loopdistSD<-maha(data[data$sex==1&data$country==countrylist[i],20:21],data[data$sex==0&data$country==countrylist[i],20:21]) 
    mahadistSD[i,2]<-loopdistSD$D
    mahadistSD[i,3]<-loopdistSD$OVL
    mahadistSD[i,4]<-loopdistSD$OVL2
    
  }
  
  
  
  
  
  ##### D for Intelligence, Health, Kindness #####
  mahadistnosd<-data.frame("country"=countrylist,"D"=rep(0,45),"overlap"=rep(0,45),"overlap2"=rep(0,45))
  
  for(i in 1:45){
    loopdistnosd<-maha(data[data$sex==1&data$country==countrylist[i],17:19],data[data$sex==0&data$country==countrylist[i],17:19]) 
    mahadistnosd[i,2]<-loopdistnosd$D
    mahadistnosd[i,3]<-loopdistnosd$OVL
    mahadistnosd[i,4]<-loopdistnosd$OVL2
    
  }
  
  
  
  
  
  ##### Bootstrapped Confidence Intervals  #####
  
  #make a matrix with 45 rows and 10,001 columns
  bootstrapD<-data.frame(matrix(0,45,10001))
  
  #add country list to datafraome
  bootstrapD[,1]<-(countrylist)
  
  #name the columns in data frame
  colnames(bootstrapD)<-c("country",paste0("loop",1:10000))
  
  #same thing for overlap
  bootstrapovl<-data.frame(matrix(0,45,10001))
  bootstrapovl[,1]<-(countrylist)
  colnames(bootstrapovl)<-c("country",paste0("loop",1:10000))
  
  #sample with replacement by country and calculate D and O 10,000 times
  for(b in 2:10001){
    
    for(i in 1:45){
      #because of small sample sizes in some countries- sometimes a random sample has a correlation with a standard deviation of 0
      nacheck<-0
      while (nacheck==0){
       
        bootdata<-data[sample((1:nrow(data))[data$country==countrylist[i]],nrow(data[data$country==countrylist[i],]),replace=T),]
        #if sd is 0 it will take another random sample
        femaleprefs<-bootdata[bootdata$sex==1&bootdata$country==countrylist[i],17:21]
        nacheck<-min(apply(femaleprefs,2,function(x) sd(x,na.rm=T)))
        
        
      }
        
        loopdist<-maha(bootdata[bootdata$sex==1&bootdata$country==countrylist[i],17:21],bootdata[bootdata$sex==0&bootdata$country==countrylist[i],17:21]) 
        bootstrapD[i,b]<-loopdist$D
        bootstrapovl[i,b]<-loopdist$OVL2
      }
      
     
    
  }
  
  
  #calculate confidence intervals for D and O for each country
  for(q in 1:45){
    mahadist$DlowCI[q]<-quantile(bootstrapD[q,2:10001],c(0.025))
    mahadist$DhighCI[q]<-quantile(bootstrapD[q,2:10001],c(0.975))
    mahadist$OlowCI[q]<-quantile(bootstrapovl[q,2:10001],c(0.025))
    mahadist$OhighCI[q]<-quantile(bootstrapovl[q,2:10001],c(0.975))
  }
  
  
  #need to save CIs as vector to convert to csv and graph
  mahadist$DlowCI <-unlist(mahadist$DlowCI)
  mahadist$DhighCI <-unlist(mahadist$DhighCI)
  mahadist$OlowCI<-unlist(mahadist$OlowCI)
  mahadist$OhighCI<-unlist(mahadist$OhighCI)
  
  #save as csv 
  #write.csv(mahadist, file = "MahaDandCIs.csv",row.names=F)
  
  
  ##### Bootstrapped Confidence Intervals for Sex Differentiated Preferences  #####
  
  #make a matrix with 45 rows and 10,001 columns
  bootstrapD2<-data.frame(matrix(0,45,10001))
  
  #add country list to datafraome
  bootstrapD2[,1]<-(countrylist)
  
  #name the columns in data frame
  colnames(bootstrapD2)<-c("country",paste0("loop",1:10000))
  
  #same thing for overlap
  bootstrapovl2<-data.frame(matrix(0,45,10001))
  bootstrapovl2[,1]<-(countrylist)
  colnames(bootstrapovl2)<-c("country",paste0("loop",1:10000))
  
  #sample with replacement by country and calculate D and O 10,000 times
  for(b in 2:10001){
    
    for(i in 1:45){
      nacheck<-0
      while (nacheck==0){
        
        bootdata<-data[sample((1:nrow(data))[data$country==countrylist[i]],nrow(data[data$country==countrylist[i],]),replace=T),]
        femaleprefs<-bootdata[bootdata$sex==1&bootdata$country==countrylist[i],20:21]
        nacheck<-min(apply(femaleprefs,2,function(x) sd(x,na.rm=T)))
        
      }
      
        loopdist<-maha(bootdata[bootdata$sex==1&bootdata$country==countrylist[i],20:21],bootdata[bootdata$sex==0&bootdata$country==countrylist[i],20:21]) 
        bootstrapD2[i,b]<-loopdist$D
        bootstrapovl2[i,b]<-loopdist$OVL2
      
      
    }  
  }
  
  
  #calculate confidence intervals for D and O for each country
  for(q in 1:45){
    mahadistSD$DlowCI[q]<-quantile(bootstrapD2[q,2:10001],c(0.025))
    mahadistSD$DhighCI[q]<-quantile(bootstrapD2[q,2:10001],c(0.975))
    mahadistSD$OlowCI[q]<-quantile(bootstrapovl2[q,2:10001],c(0.025))
    mahadistSD$OhighCI[q]<-quantile(bootstrapovl2[q,2:10001],c(0.975))
  }
  
  
  #need to save CIs as vector to convert to csv and graph
  mahadistSD$DlowCI <-unlist(mahadistSD$DlowCI)
  mahadistSD$DhighCI <-unlist(mahadistSD$DhighCI)
  mahadistSD$OlowCI<-unlist(mahadistSD$OlowCI)
  mahadistSD$OhighCI<-unlist(mahadistSD$OhighCI)
  
  
  ##### Bootstrapped Confidence Intervals for not predicted to be Sex Differentiated Preferences  #####
  
  #make a matrix with 45 rows and 10,001 columns
  bootstrapD3<-data.frame(matrix(0,45,10001))
  
  #add country list to datafraome
  bootstrapD3[,1]<-(countrylist)
  
  #name the columns in data frame
  colnames(bootstrapD3)<-c("country",paste0("loop",1:10000))
  
  #same thing for overlap
  bootstrapovl3<-data.frame(matrix(0,45,10001))
  bootstrapovl3[,1]<-(countrylist)
  colnames(bootstrapovl3)<-c("country",paste0("loop",1:10000))
  
  #sample with replacement by country and calculate D and O 10,000 times
  for(b in 2:10001){
    
    for(i in 1:45){
      
      nacheck<-0
      while (nacheck==0){
        
        bootdata<-data[sample((1:nrow(data))[data$country==countrylist[i]],nrow(data[data$country==countrylist[i],]),replace=T),]
        femaleprefs<-bootdata[bootdata$sex==1&bootdata$country==countrylist[i],17:19]
        nacheck<-min(apply(femaleprefs,2,function(x) sd(x,na.rm=T)))
      
        }
      
      loopdist<-maha(bootdata[bootdata$sex==1&bootdata$country==countrylist[i],17:19],bootdata[bootdata$sex==0&bootdata$country==countrylist[i],17:19]) 
      bootstrapD3[i,b]<-loopdist$D
      bootstrapovl3[i,b]<-loopdist$OVL2
      
    }  
  }
  
  
  #calculate confidence intervals for D and O for each country
  for(q in 1:45){
    mahadistnosd$DlowCI[q]<-quantile(bootstrapD3[q,2:10001],c(0.025))
    mahadistnosd$DhighCI[q]<-quantile(bootstrapD3[q,2:10001],c(0.975))
    mahadistnosd$OlowCI[q]<-quantile(bootstrapovl3[q,2:10001],c(0.025))
    mahadistnosd$OhighCI[q]<-quantile(bootstrapovl3[q,2:10001],c(0.975))
  }
  
  
  #need to save CIs as vector to convert to csv and graph
  mahadistnosd$DlowCI <-unlist(mahadistnosd$DlowCI)
  mahadistnosd$DhighCI <-unlist(mahadistnosd$DhighCI)
  mahadistnosd$OlowCI<-unlist(mahadistnosd$OlowCI)
  mahadistnosd$OhighCI<-unlist(mahadistnosd$OhighCI)
  
  
  
  
  ########################## Predicting Sex from Preferences with Logistic Regression ##########################
  
  
  
  
  #create dataframe of variables we care about: sex and preferences
  logdataraw<-data[,c(1,4,10,17:21)]
  
  #eliminate rows with missing data
  logdata<-na.omit(logdataraw)
  
  #create country list
  countrylist<-unique(logdata$country)
  
  #create blank vector to store logistic outcome values
  logistic<-matrix(nrow=10000,ncol=1)
  colnames(logistic)<-c("accuracy")
  
  #train=90% of data from each country
  #need to loop through country
  
  
  for(k in 1:10000){
  
    #create blank data frame to store train data
    train<-data.frame(matrix(nrow=0,ncol=8))
    colnames(train)<-c("PIN","country","sex","ideal_intelligence","ideal_kindness","ideal_health","ideal_physatt","ideal_resources")
    
    #create blank dataframe to store test data
    test<-data.frame(matrix(nrow=0,ncol=8))
    colnames(test)<-c("PIN","country","sex","ideal_intelligence","ideal_kindness","ideal_health","ideal_physatt","ideal_resources")
    
    #loop through to take 90% of participants from each country
    
    for(c in 1:45){
      
        #1 create throw away loopdata, dataframe for the country of the current loop c
        loopdata<-logdata[logdata$country==countrylist[c],]
        #sample 90% of rows from loopdata
        sample<-sample.int(n = nrow(loopdata), size = floor(.9*nrow(loopdata)), replace = F)
        #create looptrain and looptest from sampled and not sampled rows
        looptrain <- loopdata[sample, ]
        looptest  <- loopdata[-sample, ]
        #use rbind
        #squishes a vector into a dataframe from the bottom (horizontal vector)
        train<-rbind(train,looptrain)
        test<-rbind(test,looptest)
    
    }
    
    #create model
    model <- glm(sex ~ ideal_intelligence + ideal_kindness + ideal_physatt + ideal_health+ ideal_resources, data = train, family = "binomial")
    
    
    #test accuracy
    
    predicted <- plogis(predict(model, test))
    
    predicted<-round(predicted)
    
    accuracy<-mean(predicted==test$sex)
    
    logistic[k,]<-accuracy
  
  }
  
  
  #find average of 10,000 analyses 
  logresults<-mean(logistic)
  
  #find confidence intervals 
  logCIlow<-quantile(logistic,c(0.025))
  logCIhigh<-quantile(logistic,c(0.975))
  
  
  
  
}  
#end D and logistic analyses




############################# Miscellaneous Analyses #####################################


##### Index Correlations #####

##pathogen measures
pathcor<-cor(data[,c(35,45,46)], use="pairwise.complete")

##gender equality measures
gecor<-cor(data[,c(26:30,38)], use="pairwise.complete")

##preferences
prefscor<-cor(data[,c(39:44)], use="pairwise.complete")


#### Sample Size #####

#get n for country
n<-tapply(data$PIN,data$country,function(x) length(x))
#by sex
ns<-tapply(data$sex,data$country,function(x) mean(x==1))

#### Preference Means and Confidence Intervals ####


## Preference Means by sex ##
meangfp<-tapply(data$ideal_resources,data$sex, function(x) mean(x,na.rm=T))
meanpa<-tapply(data$ideal_physatt,data$sex, function(x) mean(x,na.rm=T))
meanint<-tapply(data$ideal_intelligence,data$sex, function(x) mean(x,na.rm=T))
meankind<-tapply(data$ideal_kindness,data$sex, function(x) mean(x,na.rm=T))
meanhealth<-tapply(data$ideal_health,data$sex, function(x) mean(x,na.rm=T))
meanagediff<-tapply(data$agediff,data$sex, function(x) mean(x,na.rm=T))


## Bootstrap Sex Difference in Preference CIs ##

#Number of bootstrap iterations
bootloops<-10000

#Blank dataframes to store resampled preference and choice means
mbootprefs<-data.frame(matrix(,6,bootloops+1))
fbootprefs<-data.frame(matrix(,6,bootloops+1))

#Label each row with the mean it will store
mbootprefs[,1]<-c("int","kind","health","pa","res","agediff")
fbootprefs[,1]<-c("int","kind","health","pa","res","agediff")

#Loop through bootstrap iterations
for(l in 1:bootloops){
  
  #Resample from the data
  bootdata<-data[sample(nrow(data),nrow(data),replace=T),]
  
  #Calculates the average male preference for intelligence across countries
  mbootprefs[mbootprefs[,1]=="int",l+1]<-mean(bootdata$ideal_intelligence[bootdata$sex==1],na.rm=T)
  
  #Calculates the average female preference for intelligence across countries
  fbootprefs[fbootprefs[,1]=="int",l+1]<-mean(bootdata$ideal_intelligence[bootdata$sex==0],na.rm=T)
  
  #The below do the same for kindness, health, physical attractiveness, resources, and age difference
  mbootprefs[mbootprefs[,1]=="kind",l+1]<-mean(bootdata$ideal_kindness[bootdata$sex==1],na.rm=T)
  fbootprefs[fbootprefs[,1]=="kind",l+1]<-mean(bootdata$ideal_kindness[bootdata$sex==0],na.rm=T)
  
  mbootprefs[mbootprefs[,1]=="health",l+1]<-mean(bootdata$ideal_health[bootdata$sex==1],na.rm=T)
  fbootprefs[fbootprefs[,1]=="health",l+1]<-mean(bootdata$ideal_health[bootdata$sex==0],na.rm=T)
  
  mbootprefs[mbootprefs[,1]=="pa",l+1]<-mean(bootdata$ideal_physatt[bootdata$sex==1],na.rm=T)
  fbootprefs[fbootprefs[,1]=="pa",l+1]<-mean(bootdata$ideal_physatt[bootdata$sex==0],na.rm=T)
  
  mbootprefs[mbootprefs[,1]=="res",l+1]<-mean(bootdata$ideal_resources[bootdata$sex==1],na.rm=T)
  fbootprefs[fbootprefs[,1]=="res",l+1]<-mean(bootdata$ideal_resources[bootdata$sex==0],na.rm=T)
  
  mbootprefs[mbootprefs[,1]=="agediff",l+1]<-mean(bootdata$agediff[bootdata$sex==1],na.rm=T)
  fbootprefs[fbootprefs[,1]=="agediff",l+1]<-mean(bootdata$agediff[bootdata$sex==0],na.rm=T)
  
}

#Calculate lower and upper bound of 95% CIs for males
malelcis<-apply(mbootprefs[,-1],1,function(y) quantile(y,.025,na.rm=T))
maleucis<-apply(mbootprefs[,-1],1,function(y) quantile(y,.975,na.rm=T))

#Do the same for females
femalelcis<-apply(fbootprefs[,-1],1,function(y) quantile(y,.025,na.rm=T))
femaleucis<-apply(fbootprefs[,-1],1,function(y) quantile(y,.975,na.rm=T))

#Create a dataframe to store CIs
prefcis<-data.frame("sex"=c("female","male"))

#Cbind all the data together. The weird c(rbind()) line weaves the CI vectors together so that they alternate correctly
prefcis<-cbind(prefcis,rbind(c(rbind(femalelcis,femaleucis)),c(rbind(malelcis,maleucis))))

#Name the columns
colnames(prefcis)[2:13]<-paste0(rep(c("int","kind","health","pa","res","agediff"),each=2),rep(c("_lci","_uci"),times=6))






########################################### Plots #############################################




##### Sex Differences in Preferences #####

#health
datahealth<-data.frame("country"=unique(data$country),"slopes"=coef(hdiff)$CIN[,2],"n"=tapply(data$zideal_health,data$country,length))
healthslopes<-qplot(slopes,country,color=country,data=datahealth,xlab="Sex Difference in Health Preference", ylab="Country",size=I(4))+theme_classic(base_size=25)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-1.25,1.25))

#physical attractiveness
datapa<-data.frame("country"=unique(data$country),"slopes"=coef(padiff)$CIN[,2],"n"=tapply(data$zideal_physatt,data$country,length))
physattslopes<-qplot(slopes,country,color=country,data=datapa,xlab="Sex Difference in Physical Attractiveness Preference", ylab="",size=I(4))+theme_classic(base_size=20)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-1.25,1.25))

#good financial prospects
datagfp<-data.frame("country"=unique(data$country),"slopes"=coef(gfpdiff)$CIN[,2],"n"=tapply(data$zideal_resources,data$country,length))
resourceslopes<-qplot(slopes,country,color=country,data=datagfp,xlab="Sex Difference in Good Financial Prospects Preference",ylab="Country",size=I(4))+theme_classic(base_size=20)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-1.25,1.25))

#kindness
datakind<-data.frame("country"=unique(data$country),"slopes"=coef(kinddiff)$CIN[,2],"n"=tapply(data$zideal_kindness,data$country,length))
kindslopes<-qplot(slopes,country,color=country,data=datakind,xlab="Sex Difference in Kindness Preference", ylab="Country", size=I(4))+theme_classic(base_size=25)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-1.25,1.25))

#intelligence
dataint<-data.frame("country"=unique(data$country),"slopes"=coef(intdiff)$CIN[,2],"n"=tapply(data$zideal_intelligence,data$country,length))
intslopes<-qplot(slopes,country,color=country,data=dataint,xlab="Sex Difference in Intelligence Preference",ylab="Country",size=I(4))+theme_classic(base_size=25)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-1.25,1.25))


#age (all)
data2<-data[complete.cases(data$zagediff),]
levels(data2$sex)<-c("Females","Males")
data2$country<-factor(data2$country)
dataage<-data.frame("country"=unique(data2$country),"slopes"=coef(agediff)$CIN[,2],"n"=tapply(data2$zagediff,data2$country,length))
ageslopes<-qplot(slopes,country,color=country,data=dataage,xlab="Sex Difference in Actual Age of Partner", ylab="", size=I(4))+theme_classic(base_size=15)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-1.25,1.25))



#### Panel Plot of Sex Differences ####

## all in one plot 3 rows, 2 columns
sexdifspanelplot<-ggarrange(resourceslopes,physattslopes,kindslopes,intslopes,healthslopes,ageslopes,labels=c("A","B","C","D","E","F"), nrow=3, ncol=2)


##### Megaplot with All Preferences Together #####

#need to adjust age variable to eliminate mate_age under 10
### subset data to only include those who reported a mate age ### n = 8920
data3<-data[complete.cases(data$mate_age),]
### subset data to include only those with mates older than 10 ### n = 8614
data3<-data3[data3$mate_age>10,]

#create age difference variable
data3$agediff<-(data3$mate_age-data3$age)
#standardize outcome variable
data3$zagediff<-scale(data3$agediff)

#get slopes
agediff2<-lmer(zagediff~sex+(1+sex|CIN),data=data3)
ageslopes2<-coef(agediff2)$CIN[,2]

#create plotting dataframe for age difference w/o mate age under 10
levels(data3$sex)<-c("Females","Males")
data3$country<-factor(data3$country)
dataage2<-data.frame("country"=unique(data3$country),"slopes"=coef(agediff2)$CIN[,2],"n"=tapply(data3$zagediff,data3$country,length))


#tag dataframe with preference variable they refer to
datahealth$variable<-"Health"
datapa$variable<-"Physical Attractiveness"
datagfp$variable<-"Good Financial Prospects"
dataage2$variable<-"Age Choice"
datakind$variable<-"Kindness"
dataint$variable<-"Intelligence"

#create dataframe
superdata<-rbind(datahealth,datapa,datagfp,dataage2,datakind,dataint)
superdata$variable<-as.factor(superdata$variable)
superdata$variable<-factor(superdata$variable,levels=c("Age Choice","Good Financial Prospects","Physical Attractiveness","Intelligence","Kindness","Health"))

#plot super slopes
superslopes<-qplot(slopes,variable,color=variable,data=superdata,xlab="Sex Difference (Males-Females)",ylab="Difference Variable",geom="blank")+theme_classic(base_size=25)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+geom_jitter(size=I(2.5),height = .30)+scale_x_continuous(breaks=seq(-2,2,.5),limits=c(-2,2))



##### Actual Age of Mate Across Ages #####

#all ages (Kenrick & Keefe plot)
agediffkk<-qplot(age,agediff,color=sex,data=data,xlab="Participant Age",ylab="Age Difference (Partner-Self)",alpha=I(.5))+geom_smooth(size=I(3),method="loess")+theme_classic(base_size=20)+geom_hline(yintercept=0)+scale_color_manual(name="Sex",labels=c("Female","Male"),values=c("dodgerblue","limegreen"))+scale_x_continuous(breaks=seq(10,100,10))+scale_y_continuous(breaks=seq(-50,40,10))




##### Gender Equality #####

#Find the value of each gender equality variable for each country
gem1995av<-tapply(data$gem1995,data$country,mean)
gdi1995av<-tapply(data$gdi1995,data$country,mean)
giiav<-tapply(data$gii,data$country,mean)
gdi2015av<-tapply(data$gdi2015,data$country,mean)
gggiav<-tapply(data$gggi,data$country,mean)
ge_compav<-tapply(data$gepcascores,data$country,mean)

#new dataframe with national level gender equality variables separated by sex
ge_plotdata<-data.frame("country"=rep(countrylist,2),"sex"=rep(c("Female","Male"),each=45),"gem1995"=rep(gem1995av,2),"gdi1995"=rep(gdi1995av,2),"gii"=rep(giiav,2),"gdi2015"=rep(gdi2015av,2),"gggi"=rep(gggiav,2),"ge_comp"=rep(ge_compav,2))

#make dataframe for age difference
age_ge_plotdata<-data.frame(ge_plotdata,"agedif"=c(tapply(data$agediff[data$sex==0],data$country[data$sex==0],function(x) mean(x,na.rm=T)),tapply(data$agediff[data$sex==1],data$country[data$sex==1],function(x) mean(x,na.rm=T))))

#plot gender equality composite and agediff (all ages)
agege<-qplot(ge_comp,agedif,color=sex,data=age_ge_plotdata,xlab="Gender Equality Composite",ylab="Age Difference (Partner-Self)",size=I(2))+geom_smooth(method="lm",se=TRUE,size=2)+theme_classic(base_size=20)+scale_color_manual(name="Sex",values=c("dodgerblue","limegreen"))

##simple slopes for age and gender equality 
pcaagef<-lmer(zagediff~scale(gepcascores)+(1|CIN),data=data[data$sex==0,])
pcaagem<-lmer(zagediff~scale(gepcascores)+(1|CIN),data=data[data$sex==1,])

# plot gender equality (gggi) and agediff (all ages)
agegggi<-qplot(gggi,agedif,color=sex,data=age_ge_plotdata,xlab="Gender Equality Composite",ylab="Age Difference (Partner-Self)",size=I(2))+geom_smooth(method="lm",se=TRUE,size=2)+theme_classic(base_size=20)+scale_color_manual(name="Sex",values=c("dodgerblue","limegreen"))

##simple slopes for age and gggi
gggiagef<-lmer(zagediff~scale(gggi)+(1|CIN),data=data[data$sex==0,])
gggiagem<-lmer(zagediff~scale(gggi)+(1|CIN),data=data[data$sex==1,])

#make dataframe for good financial prospect preference
gfp_ge_plotdata<-data.frame(ge_plotdata,"gfppref"=c(tapply(data$zideal_resources[data$sex==0],data$country[data$sex==0],function(x) mean(x,na.rm=T)),tapply(data$zideal_resources[data$sex==1],data$country[data$sex==1],function(x) mean(x,na.rm=T))))

#plot gender equality (gggi and gfp)
gfp_ge_gggi<-qplot(gggi,gfppref,color=sex,data=gfp_ge_plotdata,xlab="Global Gender Gap Index (GGGI)",ylab="Preference for Good Fin. Prosp.")+geom_smooth(method="lm",se=FALSE)+theme_classic(base_size=50)+scale_color_manual(name="Sex",values=c("dodgerblue","limegreen"))



##### Mahalanobis D #####

if(x>0){

  #D by country
  Dplot<-qplot(D,country,color=country,data=mahadist,xlab="Mahalanobis D",ylab="Country",size=I(6))+theme_classic(base_size=25)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+geom_errorbarh(aes(xmax = DhighCI, xmin = DlowCI, height = .3))
  #funnel plot of D by sample size
  dfunnel<-qplot(D,n,color=country,data=dataint,xlab="Sex Difference in Intelligence Preference", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(dataint$slopes), size=2)+theme(legend.position="none")
  
}



##### Funnel Plots #####


hfun<-qplot(mahadist$d_health,n,color=country,data=datahealth,xlab="Sex Difference in Health Preference", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(mahadist$d_health), size=2)+theme(legend.position="none")
kfun<-qplot(mahadist$d_kind,n,color=country,data=datahealth,xlab="Sex Difference in Kind Preference", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(mahadist$d_kind), size=2)+theme(legend.position="none")
ifun<-qplot(mahadist$d_int,n,color=country,data=datahealth,xlab="Sex Difference in Intell. Preference", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(mahadist$d_int), size=2)+theme(legend.position="none")
pafun<-qplot(mahadist$d_physatt,n,color=country,data=datahealth,xlab="Sex Difference in Phys. Att. Preference", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(mahadist$d_physatt), size=2)+theme(legend.position="none")
gfpfun<-qplot(mahadist$d_resources,n,color=country,data=datahealth,xlab="Sex Difference in Good Fin. Prosp. Preference", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(mahadist$d_resources), size=2)+theme(legend.position="none")

#load Cohen's d for age data
aged<-read.csv(file.choose())
agefun<-qplot(d,n,color=country,data=aged,xlab="Sex Difference in Age Choice", ylab="Sample Size",size=I(3))+theme_classic(base_size=20)+geom_vline(xintercept =mean(aged$d), size=2)+theme(legend.position="none")

# panel plot for funnels 
funpanelplot<-ggarrange(pafun,gfpfun,ifun,kfun,hfun,agefun,labels=c("A","B","C","D","E","F"), nrow=3, ncol=2)

