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


#create results dataframe
results<-data.frame(matrix(ncol = 7, nrow = 1),stringsAsFactors=FALSE)
x<-c("objectname","ge_b","pp_b","ge_se","pp_se","ge_p","pp_p")
colnames(results) <- x

###################################### Analyses ################################################



############################# Sex*Gender Equaltiy and Sex*Pathogen Prevalence #############################

########### Pathogen Index from Gangestad and Buss (1993) ##############


##### Good Financial Prospects #####
gggigfp3a<-lmer(zideal_resources~sex*(scale(gggi))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015gfp3a<-lmer(zideal_resources~sex*(scale(gdi2015))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giigfp3a<-lmer(zideal_resources~sex*(scale(gii))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995gfp3a<-lmer(zideal_resources~sex*(scale(gem1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995gfp3a<-lmer(zideal_resources~sex*(scale(gdi1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcagfp3a<-lmer(zideal_resources~sex*gepcascores+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa3a<-lmer(zideal_physatt~sex*(scale(gggi))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015pa3a<-lmer(zideal_physatt~sex*(scale(gdi2015))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giipa3a<-lmer(zideal_physatt~sex*(scale(gii))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995pa3a<-lmer(zideal_physatt~sex*(scale(gem1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995pa3a<-lmer(zideal_physatt~sex*(scale(gdi1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcapa3a<-lmer(zideal_physatt~sex*gepcascores+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Health #####
gggih3a<-lmer(zideal_health~sex*(scale(gggi))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015h3a<-lmer(zideal_health~sex*(scale(gdi2015))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giih3a<-lmer(zideal_health~sex*(scale(gii))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995h3a<-lmer(zideal_health~sex*(scale(gem1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995h3a<-lmer(zideal_health~sex*(scale(gdi1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcah3a<-lmer(zideal_health~sex*gepcascores+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint3a<-lmer(zideal_intelligence~sex*(scale(gggi))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015int3a<-lmer(zideal_intelligence~sex*(scale(gdi2015))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giiint3a<-lmer(zideal_intelligence~sex*(scale(gii))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995int3a<-lmer(zideal_intelligence~sex*(scale(gem1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995int3a<-lmer(zideal_intelligence~sex*(scale(gdi1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint3a<-lmer(zideal_intelligence~sex*gepcascores+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik3a<-lmer(zideal_kindness~sex*(scale(gggi))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015k3a<-lmer(zideal_kindness~sex*(scale(gdi2015))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giik3a<-lmer(zideal_kindness~sex*(scale(gii))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995k3a<-lmer(zideal_kindness~sex*(scale(gem1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995k3a<-lmer(zideal_kindness~sex*(scale(gdi1995))+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcak3a<-lmer(zideal_kindness~sex*gepcascores+sex*(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)


########### Years of Life Lost to Communicable Diseases ##############
##### Good Financial Prospects #####
gggigfp3b<-lmer(zideal_resources~sex*(scale(gggi))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+continent+country_religion+(1+sex|CIN),data=data)
gdi2015gfp3b<-lmer(zideal_resources~sex*(scale(gdi2015))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giigfp3b<-lmer(zideal_resources~sex*(scale(gii))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995gfp3b<-lmer(zideal_resources~sex*(scale(gem1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995gfp3b<-lmer(zideal_resources~sex*(scale(gdi1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcagfp3b<-lmer(zideal_resources~sex*gepcascores+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+country_religion+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa3b<-lmer(zideal_physatt~sex*(scale(gggi))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015pa3b<-lmer(zideal_physatt~sex*(scale(gdi2015))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giipa3b<-lmer(zideal_physatt~sex*(scale(gii))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995pa3b<-lmer(zideal_physatt~sex*(scale(gem1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995pa3b<-lmer(zideal_physatt~sex*(scale(gdi1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcapa3b<-lmer(zideal_physatt~sex*gepcascores+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Health #####
gggih3b<-lmer(zideal_health~sex*(scale(gggi))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015h3b<-lmer(zideal_health~sex*(scale(gdi2015))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giih3b<-lmer(zideal_health~sex*(scale(gii))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995h3b<-lmer(zideal_health~sex*(scale(gem1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995h3b<-lmer(zideal_health~sex*(scale(gdi1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcah3b<-lmer(zideal_health~sex*gepcascores+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint3b<-lmer(zideal_intelligence~sex*(scale(gggi))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015int3b<-lmer(zideal_intelligence~sex*(scale(gdi2015))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giiint3b<-lmer(zideal_intelligence~sex*(scale(gii))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995int3b<-lmer(zideal_intelligence~sex*(scale(gem1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995int3b<-lmer(zideal_intelligence~sex*(scale(gdi1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint3b<-lmer(zideal_intelligence~sex*gepcascores+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik3b<-lmer(zideal_kindness~sex*(scale(gggi))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015k3b<-lmer(zideal_kindness~sex*(scale(gdi2015))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giik3b<-lmer(zideal_kindness~sex*(scale(gii))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995k3b<-lmer(zideal_kindness~sex*(scale(gem1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995k3b<-lmer(zideal_kindness~sex*(scale(gdi1995))+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcak3b<-lmer(zideal_kindness~sex*gepcascores+sex*zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)


########### Pathogen Composite ##############
##### Good Financial Prospects #####
gggigfp3c<-lmer(zideal_resources~sex*(scale(gggi))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+continent+country_religion+(1+sex|CIN),data=data)
gdi2015gfp3c<-lmer(zideal_resources~sex*(scale(gdi2015))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giigfp3c<-lmer(zideal_resources~sex*(scale(gii))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995gfp3c<-lmer(zideal_resources~sex*(scale(gem1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995gfp3c<-lmer(zideal_resources~sex*(scale(gdi1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcagfp3c<-lmer(zideal_resources~sex*gepcascores+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa3c<-lmer(zideal_physatt~sex*(scale(gggi))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015pa3c<-lmer(zideal_physatt~sex*(scale(gdi2015))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giipa3c<-lmer(zideal_physatt~sex*(scale(gii))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995pa3c<-lmer(zideal_physatt~sex*(scale(gem1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995pa3c<-lmer(zideal_physatt~sex*(scale(gdi1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcapa3c<-lmer(zideal_physatt~sex*gepcascores+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Health #####
gggih3c<-lmer(zideal_health~sex*(scale(gggi))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015h3c<-lmer(zideal_health~sex*(scale(gdi2015))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giih3c<-lmer(zideal_health~sex*(scale(gii))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995h3c<-lmer(zideal_health~sex*(scale(gem1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995h3c<-lmer(zideal_health~sex*(scale(gdi1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcah3c<-lmer(zideal_health~sex*gepcascores+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint3c<-lmer(zideal_intelligence~sex*(scale(gggi))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015int3c<-lmer(zideal_intelligence~sex*(scale(gdi2015))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giiint3c<-lmer(zideal_intelligence~sex*(scale(gii))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995int3c<-lmer(zideal_intelligence~sex*(scale(gem1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995int3c<-lmer(zideal_intelligence~sex*(scale(gdi1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint3c<-lmer(zideal_intelligence~sex*gepcascores+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik3c<-lmer(zideal_kindness~sex*(scale(gggi))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015k3c<-lmer(zideal_kindness~sex*(scale(gdi2015))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giik3c<-lmer(zideal_kindness~sex*(scale(gii))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995k3c<-lmer(zideal_kindness~sex*(scale(gem1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995k3c<-lmer(zideal_kindness~sex*(scale(gdi1995))+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcak3c<-lmer(zideal_kindness~sex*gepcascores+sex*zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)


###### results #######
#Table S8aa
results<-rbind(results,c("gdi1995gfp3a",as.numeric(summary(gdi1995gfp3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi1995pa3a",as.numeric(summary(gdi1995pa3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi1995k3a",as.numeric(summary(gdi1995k3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi1995int3a",as.numeric(summary(gdi1995int3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi1995h3a",as.numeric(summary(gdi1995h3a)$coefficients[10:11,c(1,2,5)])))

results<-rbind(results,c("gem1995gfp3a",as.numeric(summary(gem1995gfp3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gem1995pa3a",as.numeric(summary(gem1995pa3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gem1995k3a",as.numeric(summary(gem1995k3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gem1995int3a",as.numeric(summary(gem1995int3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gem1995h3a",as.numeric(summary(gem1995h3a)$coefficients[10:11,c(1,2,5)])))

results<-rbind(results,c("giigfp3a",as.numeric(summary(giigfp3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("giipa3a",as.numeric(summary(giipa3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("giik3a",as.numeric(summary(giik3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("giiint3a",as.numeric(summary(giiint3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("giih3a",as.numeric(summary(giih3a)$coefficients[9:10,c(1,2,5)])))

results<-rbind(results,c("gggigfp3a",as.numeric(summary(gggigfp3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gggipa3a",as.numeric(summary(gggipa3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gggik3a",as.numeric(summary(gggik3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gggiint3a",as.numeric(summary(gggiint3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gggih3a",as.numeric(summary(gggih3a)$coefficients[10:11,c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp3a",as.numeric(summary(gdi2015gfp3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi2015pa3a",as.numeric(summary(gdi2015pa3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi2015k3a",as.numeric(summary(gdi2015k3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi2015int3a",as.numeric(summary(gdi2015int3a)$coefficients[10:11,c(1,2,5)])))
results<-rbind(results,c("gdi2015h3a",as.numeric(summary(gdi2015h3a)$coefficients[10:11,c(1,2,5)])))


results<-rbind(results,c("pcagfp3a",as.numeric(summary(pcagfp3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("pcapa3a",as.numeric(summary(pcapa3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("pcak3a",as.numeric(summary(pcak3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("pcaint3a",as.numeric(summary(pcaint3a)$coefficients[9:10,c(1,2,5)])))
results<-rbind(results,c("pcah3a",as.numeric(summary(pcah3a)$coefficients[9:10,c(1,2,5)])))

#Table S8ab
results<-rbind(results,c("gdi1995gfp3b",as.numeric(summary(gdi1995gfp3b)$coefficients[23:24,c(1,2,5)])))
results<-rbind(results,c("gdi1995pa3b",as.numeric(summary(gdi1995pa3b)$coefficients[23:24,c(1,2,5)])))
results<-rbind(results,c("gdi1995k3b",as.numeric(summary(gdi1995k3b)$coefficients[23:24,c(1,2,5)])))
results<-rbind(results,c("gdi1995int3b",as.numeric(summary(gdi1995int3b)$coefficients[12:13,c(1,2,5)])))
results<-rbind(results,c("gdi1995h3b",as.numeric(summary(gdi1995h3b)$coefficients[23:24,c(1,2,5)])))

results<-rbind(results,c("gem1995gfp3b",as.numeric(summary(gem1995gfp3b)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995pa3b",as.numeric(summary(gem1995pa3b)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995k3b",as.numeric(summary(gem1995k3b)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995int3b",as.numeric(summary(gem1995int3b)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995h3b",as.numeric(summary(gem1995h3b)$coefficients[22:23,c(1,2,5)])))

results<-rbind(results,c("giigfp3b",as.numeric(summary(giigfp3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giipa3b",as.numeric(summary(giipa3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giik3b",as.numeric(summary(giik3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giiint3b",as.numeric(summary(giiint3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giih3b",as.numeric(summary(giih3b)$coefficients[25:26,c(1,2,5)])))

results<-rbind(results,c("gggigfp3b",as.numeric(summary(gggigfp3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggipa3b",as.numeric(summary(gggipa3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggik3b",as.numeric(summary(gggik3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggiint3b",as.numeric(summary(gggiint3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggih3b",as.numeric(summary(gggih3b)$coefficients[25:26,c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp3b",as.numeric(summary(gdi2015gfp3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015pa3b",as.numeric(summary(gdi2015pa3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015k3b",as.numeric(summary(gdi2015k3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015int3b",as.numeric(summary(gdi2015int3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015h3b",as.numeric(summary(gdi2015h3b)$coefficients[25:26,c(1,2,5)])))


results<-rbind(results,c("pcagfp3b",as.numeric(summary(pcagfp3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcapa3b",as.numeric(summary(pcapa3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcak3b",as.numeric(summary(pcak3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcaint3b",as.numeric(summary(pcaint3b)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcah3b",as.numeric(summary(pcah3b)$coefficients[25:26,c(1,2,5)])))

#Table S8ac
results<-rbind(results,c("gdi1995gfp3c",as.numeric(summary(gdi1995gfp3c)$coefficients[23:24,c(1,2,5)])))
results<-rbind(results,c("gdi1995pa3c",as.numeric(summary(gdi1995pa3c)$coefficients[23:24,c(1,2,5)])))
results<-rbind(results,c("gdi1995k3c",as.numeric(summary(gdi1995k3c)$coefficients[23:24,c(1,2,5)])))
results<-rbind(results,c("gdi1995int3c",as.numeric(summary(gdi1995int3c)$coefficients[12:13,c(1,2,5)])))
results<-rbind(results,c("gdi1995h3c",as.numeric(summary(gdi1995h3c)$coefficients[23:24,c(1,2,5)])))

results<-rbind(results,c("gem1995gfp3c",as.numeric(summary(gem1995gfp3c)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995pa3c",as.numeric(summary(gem1995pa3c)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995k3c",as.numeric(summary(gem1995k3c)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995int3c",as.numeric(summary(gem1995int3c)$coefficients[22:23,c(1,2,5)])))
results<-rbind(results,c("gem1995h3c",as.numeric(summary(gem1995h3c)$coefficients[22:23,c(1,2,5)])))

results<-rbind(results,c("giigfp3c",as.numeric(summary(giigfp3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giipa3c",as.numeric(summary(giipa3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giik3c",as.numeric(summary(giik3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giiint3c",as.numeric(summary(giiint3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("giih3c",as.numeric(summary(giih3c)$coefficients[25:26,c(1,2,5)])))

results<-rbind(results,c("gggigfp3c",as.numeric(summary(gggigfp3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggipa3c",as.numeric(summary(gggipa3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggik3c",as.numeric(summary(gggik3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggiint3c",as.numeric(summary(gggiint3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gggih3c",as.numeric(summary(gggih3c)$coefficients[25:26,c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp3c",as.numeric(summary(gdi2015gfp3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015pa3c",as.numeric(summary(gdi2015pa3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015k3c",as.numeric(summary(gdi2015k3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015int3c",as.numeric(summary(gdi2015int3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("gdi2015h3c",as.numeric(summary(gdi2015h3c)$coefficients[25:26,c(1,2,5)])))


results<-rbind(results,c("pcagfp3c",as.numeric(summary(pcagfp3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcapa3c",as.numeric(summary(pcapa3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcak3c",as.numeric(summary(pcak3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcaint3c",as.numeric(summary(pcaint3c)$coefficients[25:26,c(1,2,5)])))
results<-rbind(results,c("pcah3c",as.numeric(summary(pcah3c)$coefficients[25:26,c(1,2,5)])))


############################# Gender Equaltiy and Pathogen Prevalence #############################

########### Pathogen Index from Gangestad and Buss (1993) ##############


##### Good Financial Prospects #####
gggigfp4a<-lmer(zideal_resources~(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015gfp4a<-lmer(zideal_resources~(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giigfp4a<-lmer(zideal_resources~(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995gfp4a<-lmer(zideal_resources~(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995gfp4a<-lmer(zideal_resources~(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcagfp4a<-lmer(zideal_resources~gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa4a<-lmer(zideal_physatt~(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015pa4a<-lmer(zideal_physatt~(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giipa4a<-lmer(zideal_physatt~(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995pa4a<-lmer(zideal_physatt~(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+(1+sex|CIN),data=data)
gdi1995pa4a<-lmer(zideal_physatt~(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcapa4a<-lmer(zideal_physatt~gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Health #####
gggih4a<-lmer(zideal_health~(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015h4a<-lmer(zideal_health~(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giih4a<-lmer(zideal_health~(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995h4a<-lmer(zideal_health~(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995h4a<-lmer(zideal_health~(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcah4a<-lmer(zideal_health~gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint4a<-lmer(zideal_intelligence~(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015int4a<-lmer(zideal_intelligence~(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giiint4a<-lmer(zideal_intelligence~(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995int4a<-lmer(zideal_intelligence~(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995int4a<-lmer(zideal_intelligence~(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+(1+sex|CIN),data=data)
pcaint4a<-lmer(zideal_intelligence~gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik4a<-lmer(zideal_kindness~(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015k4a<-lmer(zideal_kindness~(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giik4a<-lmer(zideal_kindness~(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995k4a<-lmer(zideal_kindness~(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995k4a<-lmer(zideal_kindness~(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcak4a<-lmer(zideal_kindness~gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)


########### Years of Life Lost to Communicable Diseases ##############
##### Good Financial Prospects #####
gggigfp4b<-lmer(zideal_resources~(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+continent+country_religion+(1+sex|CIN),data=data)
gdi2015gfp4b<-lmer(zideal_resources~(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giigfp4b<-lmer(zideal_resources~(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995gfp4b<-lmer(zideal_resources~(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995gfp4b<-lmer(zideal_resources~(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcagfp4b<-lmer(zideal_resources~gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+country_religion+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa4b<-lmer(zideal_physatt~(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015pa4b<-lmer(zideal_physatt~(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giipa4b<-lmer(zideal_physatt~(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995pa4b<-lmer(zideal_physatt~(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995pa4b<-lmer(zideal_physatt~(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcapa4b<-lmer(zideal_physatt~gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Health #####
gggih4b<-lmer(zideal_health~(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015h4b<-lmer(zideal_health~(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giih4b<-lmer(zideal_health~(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995h4b<-lmer(zideal_health~(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995h4b<-lmer(zideal_health~(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcah4b<-lmer(zideal_health~gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint4b<-lmer(zideal_intelligence~(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015int4b<-lmer(zideal_intelligence~(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giiint4b<-lmer(zideal_intelligence~(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995int4b<-lmer(zideal_intelligence~(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995int4b<-lmer(zideal_intelligence~(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint4b<-lmer(zideal_intelligence~gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik4b<-lmer(zideal_kindness~(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015k4b<-lmer(zideal_kindness~(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giik4b<-lmer(zideal_kindness~(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995k4b<-lmer(zideal_kindness~(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995k4b<-lmer(zideal_kindness~(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcak4b<-lmer(zideal_kindness~gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)


########### Pathogen Composite ##############
##### Good Financial Prospects #####
gggigfp4c<-lmer(zideal_resources~(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+continent+country_religion+(1+sex|CIN),data=data)
gdi2015gfp4c<-lmer(zideal_resources~(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giigfp4c<-lmer(zideal_resources~(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995gfp4c<-lmer(zideal_resources~(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995gfp4c<-lmer(zideal_resources~(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcagfp4c<-lmer(zideal_resources~gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa4c<-lmer(zideal_physatt~(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015pa4c<-lmer(zideal_physatt~(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giipa4c<-lmer(zideal_physatt~(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995pa4c<-lmer(zideal_physatt~(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995pa4c<-lmer(zideal_physatt~(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcapa4c<-lmer(zideal_physatt~gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Health #####
gggih4c<-lmer(zideal_health~(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015h4c<-lmer(zideal_health~(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giih4c<-lmer(zideal_health~(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995h4c<-lmer(zideal_health~(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995h4c<-lmer(zideal_health~(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcah4c<-lmer(zideal_health~gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint4c<-lmer(zideal_intelligence~(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015int4c<-lmer(zideal_intelligence~(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giiint4c<-lmer(zideal_intelligence~(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995int4c<-lmer(zideal_intelligence~(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995int4c<-lmer(zideal_intelligence~(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint4c<-lmer(zideal_intelligence~gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik4c<-lmer(zideal_kindness~(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015k4c<-lmer(zideal_kindness~(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giik4c<-lmer(zideal_kindness~(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995k4c<-lmer(zideal_kindness~(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995k4c<-lmer(zideal_kindness~(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcak4c<-lmer(zideal_kindness~gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)



###### results #######
#Table S8ba
results<-rbind(results,c("gdi1995gfp4a",as.numeric(summary(gdi1995gfp4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995pa4a",as.numeric(summary(gdi1995pa4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995k4a",as.numeric(summary(gdi1995k4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995int4a",as.numeric(summary(gdi1995int4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995h4a",as.numeric(summary(gdi1995h4a)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gem1995gfp4a",as.numeric(summary(gem1995gfp4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995pa4a",as.numeric(summary(gem1995pa4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995k4a",as.numeric(summary(gem1995k4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995int4a",as.numeric(summary(gem1995int4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995h4a",as.numeric(summary(gem1995h4a)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("giigfp4a",as.numeric(summary(giigfp4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giipa4a",as.numeric(summary(giipa4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giik4a",as.numeric(summary(giik4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giiint4a",as.numeric(summary(giiint4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giih4a",as.numeric(summary(giih4a)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gggigfp4a",as.numeric(summary(gggigfp4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggipa4a",as.numeric(summary(gggipa4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggik4a",as.numeric(summary(gggik4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggiint4a",as.numeric(summary(gggiint4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggih4a",as.numeric(summary(gggih4a)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp4a",as.numeric(summary(gdi2015gfp4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015pa4a",as.numeric(summary(gdi2015pa4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015k4a",as.numeric(summary(gdi2015k4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015int4a",as.numeric(summary(gdi2015int4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015h4a",as.numeric(summary(gdi2015h4a)$coefficients[2:3,c(1,2,5)])))


results<-rbind(results,c("pcagfp4a",as.numeric(summary(pcagfp4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcapa4a",as.numeric(summary(pcapa4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcak4a",as.numeric(summary(pcak4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcaint4a",as.numeric(summary(pcaint4a)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcah4a",as.numeric(summary(pcah4a)$coefficients[2:3,c(1,2,5)])))

#Table S8bb
results<-rbind(results,c("gdi1995gfp4b",as.numeric(summary(gdi1995gfp4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995pa4b",as.numeric(summary(gdi1995pa4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995k4b",as.numeric(summary(gdi1995k4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995int4b",as.numeric(summary(gdi1995int4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995h4b",as.numeric(summary(gdi1995h4b)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gem1995gfp4b",as.numeric(summary(gem1995gfp4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995pa4b",as.numeric(summary(gem1995pa4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995k4b",as.numeric(summary(gem1995k4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995int4b",as.numeric(summary(gem1995int4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995h4b",as.numeric(summary(gem1995h4b)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("giigfp4b",as.numeric(summary(giigfp4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giipa4b",as.numeric(summary(giipa4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giik4b",as.numeric(summary(giik4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giiint4b",as.numeric(summary(giiint4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giih4b",as.numeric(summary(giih4b)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gggigfp4b",as.numeric(summary(gggigfp4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggipa4b",as.numeric(summary(gggipa4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggik4b",as.numeric(summary(gggik4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggiint4b",as.numeric(summary(gggiint4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggih4b",as.numeric(summary(gggih4b)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp4b",as.numeric(summary(gdi2015gfp4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015pa4b",as.numeric(summary(gdi2015pa4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015k4b",as.numeric(summary(gdi2015k4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015int4b",as.numeric(summary(gdi2015int4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015h4b",as.numeric(summary(gdi2015h4b)$coefficients[2:3,c(1,2,5)])))


results<-rbind(results,c("pcagfp4b",as.numeric(summary(pcagfp4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcapa4b",as.numeric(summary(pcapa4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcak4b",as.numeric(summary(pcak4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcaint4b",as.numeric(summary(pcaint4b)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcah4b",as.numeric(summary(pcah4b)$coefficients[2:3,c(1,2,5)])))

#Table S8bc
results<-rbind(results,c("gdi1995gfp4c",as.numeric(summary(gdi1995gfp4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995pa4c",as.numeric(summary(gdi1995pa4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995k4c",as.numeric(summary(gdi1995k4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995int4c",as.numeric(summary(gdi1995int4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi1995h4c",as.numeric(summary(gdi1995h4c)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gem1995gfp4c",as.numeric(summary(gem1995gfp4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995pa4c",as.numeric(summary(gem1995pa4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995k4c",as.numeric(summary(gem1995k4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995int4c",as.numeric(summary(gem1995int4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gem1995h4c",as.numeric(summary(gem1995h4c)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("giigfp4c",as.numeric(summary(giigfp4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giipa4c",as.numeric(summary(giipa4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giik4c",as.numeric(summary(giik4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giiint4c",as.numeric(summary(giiint4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("giih4c",as.numeric(summary(giih4c)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gggigfp4c",as.numeric(summary(gggigfp4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggipa4c",as.numeric(summary(gggipa4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggik4c",as.numeric(summary(gggik4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggiint4c",as.numeric(summary(gggiint4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gggih4c",as.numeric(summary(gggih4c)$coefficients[2:3,c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp4c",as.numeric(summary(gdi2015gfp4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015pa4c",as.numeric(summary(gdi2015pa4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015k4c",as.numeric(summary(gdi2015k4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015int4c",as.numeric(summary(gdi2015int4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("gdi2015h4c",as.numeric(summary(gdi2015h4c)$coefficients[2:3,c(1,2,5)])))


results<-rbind(results,c("pcagfp4c",as.numeric(summary(pcagfp4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcapa4c",as.numeric(summary(pcapa4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcak4c",as.numeric(summary(pcak4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcaint4c",as.numeric(summary(pcaint4c)$coefficients[2:3,c(1,2,5)])))
results<-rbind(results,c("pcah4c",as.numeric(summary(pcah4c)$coefficients[2:3,c(1,2,5)])))



############################# Sex*Gender Equaltiy and Pathogen Prevalence #############################

########### Pathogen Index from Gangestad and Buss (1993) ##############


##### Good Financial Prospects #####
gggigfp5a<-lmer(zideal_resources~sex*(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015gfp5a<-lmer(zideal_resources~sex*(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giigfp5a<-lmer(zideal_resources~sex*(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995gfp5a<-lmer(zideal_resources~sex*(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995gfp5a<-lmer(zideal_resources~sex*(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcagfp5a<-lmer(zideal_resources~sex*gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa5a<-lmer(zideal_physatt~sex*(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015pa5a<-lmer(zideal_physatt~sex*(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giipa5a<-lmer(zideal_physatt~sex*(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995pa5a<-lmer(zideal_physatt~sex*(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995pa5a<-lmer(zideal_physatt~sex*(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcapa5a<-lmer(zideal_physatt~sex*gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Health #####
gggih5a<-lmer(zideal_health~sex*(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015h5a<-lmer(zideal_health~sex*(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giih5a<-lmer(zideal_health~sex*(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995h5a<-lmer(zideal_health~sex*(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995h5a<-lmer(zideal_health~sex*(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcah5a<-lmer(zideal_health~sex*gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint5a<-lmer(zideal_intelligence~sex*(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015int5a<-lmer(zideal_intelligence~sex*(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giiint5a<-lmer(zideal_intelligence~sex*(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995int5a<-lmer(zideal_intelligence~sex*(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995int5a<-lmer(zideal_intelligence~sex*(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint5a<-lmer(zideal_intelligence~sex*gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik5a<-lmer(zideal_kindness~sex*(scale(gggi))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi2015k5a<-lmer(zideal_kindness~sex*(scale(gdi2015))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
giik5a<-lmer(zideal_kindness~sex*(scale(gii))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gem1995k5a<-lmer(zideal_kindness~sex*(scale(gem1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
gdi1995k5a<-lmer(zideal_kindness~sex*(scale(gdi1995))+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcak5a<-lmer(zideal_kindness~sex*gepcascores+(scale(gb_path))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)


########### Years of Life Lost to Communicable Diseases ##############
##### Good Financial Prospects #####
gggigfp5b<-lmer(zideal_resources~sex*(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+continent+country_religion+(1+sex|CIN),data=data)
gdi2015gfp5b<-lmer(zideal_resources~sex*(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giigfp5b<-lmer(zideal_resources~sex*(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995gfp5b<-lmer(zideal_resources~sex*(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995gfp5b<-lmer(zideal_resources~sex*(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcagfp5b<-lmer(zideal_resources~sex*gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+country_religion+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa5b<-lmer(zideal_physatt~sex*(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015pa5b<-lmer(zideal_physatt~sex*(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giipa5b<-lmer(zideal_physatt~sex*(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995pa5b<-lmer(zideal_physatt~sex*(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995pa5b<-lmer(zideal_physatt~sex*(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcapa5b<-lmer(zideal_physatt~sex*gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Health #####
gggih5b<-lmer(zideal_health~sex*(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015h5b<-lmer(zideal_health~sex*(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giih5b<-lmer(zideal_health~sex*(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995h5b<-lmer(zideal_health~sex*(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995h5b<-lmer(zideal_health~sex*(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcah5b<-lmer(zideal_health~sex*gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint5b<-lmer(zideal_intelligence~sex*(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015int5b<-lmer(zideal_intelligence~sex*(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giiint5b<-lmer(zideal_intelligence~sex*(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995int5b<-lmer(zideal_intelligence~sex*(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995int5b<-lmer(zideal_intelligence~sex*(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint5b<-lmer(zideal_intelligence~sex*gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik5b<-lmer(zideal_kindness~sex*(scale(gggi))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015k5b<-lmer(zideal_kindness~sex*(scale(gdi2015))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giik5b<-lmer(zideal_kindness~sex*(scale(gii))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995k5b<-lmer(zideal_kindness~sex*(scale(gem1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995k5b<-lmer(zideal_kindness~sex*(scale(gdi1995))+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcak5b<-lmer(zideal_kindness~sex*gepcascores+zcmc_yll+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)


########### Pathogen Composite ##############
##### Good Financial Prospects #####
gggigfp5c<-lmer(zideal_resources~sex*(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+continent+country_religion+(1+sex|CIN),data=data)
gdi2015gfp5c<-lmer(zideal_resources~sex*(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giigfp5c<-lmer(zideal_resources~sex*(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995gfp5c<-lmer(zideal_resources~sex*(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995gfp5c<-lmer(zideal_resources~sex*(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcagfp5c<-lmer(zideal_resources~sex*gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Physical Attractiveness #####
gggipa5c<-lmer(zideal_physatt~sex*(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015pa5c<-lmer(zideal_physatt~sex*(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giipa5c<-lmer(zideal_physatt~sex*(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995pa5c<-lmer(zideal_physatt~sex*(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995pa5c<-lmer(zideal_physatt~sex*(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcapa5c<-lmer(zideal_physatt~sex*gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Health #####
gggih5c<-lmer(zideal_health~sex*(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015h5c<-lmer(zideal_health~sex*(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giih5c<-lmer(zideal_health~sex*(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995h5c<-lmer(zideal_health~sex*(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995h5c<-lmer(zideal_health~sex*(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcah5c<-lmer(zideal_health~sex*gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Intelligence #####
gggiint5c<-lmer(zideal_intelligence~sex*(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015int5c<-lmer(zideal_intelligence~sex*(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giiint5c<-lmer(zideal_intelligence~sex*(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995int5c<-lmer(zideal_intelligence~sex*(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995int5c<-lmer(zideal_intelligence~sex*(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)
pcaint5c<-lmer(zideal_intelligence~sex*gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)

##### Kindness #####
gggik5c<-lmer(zideal_kindness~sex*(scale(gggi))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi2015k5c<-lmer(zideal_kindness~sex*(scale(gdi2015))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
giik5c<-lmer(zideal_kindness~sex*(scale(gii))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gem1995k5c<-lmer(zideal_kindness~sex*(scale(gem1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
gdi1995k5c<-lmer(zideal_kindness~sex*(scale(gdi1995))+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)
pcak5c<-lmer(zideal_kindness~sex*gepcascores+zpathindex+scale(lattitude)+scale(gdp_percap)+country_religion+continent+(1+sex|CIN),data=data)


###### results #######
#Table S8ca
results<-rbind(results,c("gdi1995gfp5a",as.numeric(summary(gdi1995gfp5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi1995pa5a",as.numeric(summary(gdi1995pa5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi1995k5a",as.numeric(summary(gdi1995k5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi1995int5a",as.numeric(summary(gdi1995int5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi1995h5a",as.numeric(summary(gdi1995h5a)$coefficients[c(4,10),c(1,2,5)])))

results<-rbind(results,c("gem1995gfp5a",as.numeric(summary(gem1995gfp5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gem1995pa5a",as.numeric(summary(gem1995pa5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gem1995k5a",as.numeric(summary(gem1995k5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gem1995int5a",as.numeric(summary(gem1995int5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gem1995h5a",as.numeric(summary(gem1995h5a)$coefficients[c(4,10),c(1,2,5)])))

results<-rbind(results,c("giigfp5a",as.numeric(summary(giigfp5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("giipa5a",as.numeric(summary(giipa5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("giik5a",as.numeric(summary(giik5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("giiint5a",as.numeric(summary(giiint5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("giih5a",as.numeric(summary(giih5a)$coefficients[c(4,9),c(1,2,5)])))

results<-rbind(results,c("gggigfp5a",as.numeric(summary(gggigfp5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gggipa5a",as.numeric(summary(gggipa5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gggik5a",as.numeric(summary(gggik5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gggiint5a",as.numeric(summary(gggiint5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gggih5a",as.numeric(summary(gggih5a)$coefficients[c(4,10),c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp5a",as.numeric(summary(gdi2015gfp5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi2015pa5a",as.numeric(summary(gdi2015pa5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi2015k5a",as.numeric(summary(gdi2015k5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi2015int5a",as.numeric(summary(gdi2015int5a)$coefficients[c(4,10),c(1,2,5)])))
results<-rbind(results,c("gdi2015h5a",as.numeric(summary(gdi2015h5a)$coefficients[c(4,10),c(1,2,5)])))


results<-rbind(results,c("pcagfp5a",as.numeric(summary(pcagfp5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("pcapa5a",as.numeric(summary(pcapa5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("pcak5a",as.numeric(summary(pcak5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("pcaint5a",as.numeric(summary(pcaint5a)$coefficients[c(4,9),c(1,2,5)])))
results<-rbind(results,c("pcah5a",as.numeric(summary(pcah5a)$coefficients[c(4,9),c(1,2,5)])))

#Table S8cb
results<-rbind(results,c("gdi1995gfp5b",as.numeric(summary(gdi1995gfp5b)$coefficients[c(4,23),c(1,2,5)])))
results<-rbind(results,c("gdi1995pa5b",as.numeric(summary(gdi1995pa5b)$coefficients[c(4,23),c(1,2,5)])))
results<-rbind(results,c("gdi1995k5b",as.numeric(summary(gdi1995k5b)$coefficients[c(4,23),c(1,2,5)])))
results<-rbind(results,c("gdi1995int5b",as.numeric(summary(gdi1995int5b)$coefficients[c(4,12),c(1,2,5)])))
results<-rbind(results,c("gdi1995h5b",as.numeric(summary(gdi1995h5b)$coefficients[c(4,23),c(1,2,5)])))

results<-rbind(results,c("gem1995gfp5b",as.numeric(summary(gem1995gfp5b)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995pa5b",as.numeric(summary(gem1995pa5b)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995k5b",as.numeric(summary(gem1995k5b)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995int5b",as.numeric(summary(gem1995int5b)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995h5b",as.numeric(summary(gem1995h5b)$coefficients[c(4,22),c(1,2,5)])))

results<-rbind(results,c("giigfp5b",as.numeric(summary(giigfp5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giipa5b",as.numeric(summary(giipa5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giik5b",as.numeric(summary(giik5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giiint5b",as.numeric(summary(giiint5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giih5b",as.numeric(summary(giih5b)$coefficients[c(4,25),c(1,2,5)])))

results<-rbind(results,c("gggigfp5b",as.numeric(summary(gggigfp5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggipa5b",as.numeric(summary(gggipa5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggik5b",as.numeric(summary(gggik5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggiint5b",as.numeric(summary(gggiint5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggih5b",as.numeric(summary(gggih5b)$coefficients[c(4,25),c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp5b",as.numeric(summary(gdi2015gfp5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015pa5b",as.numeric(summary(gdi2015pa5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015k5b",as.numeric(summary(gdi2015k5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015int5b",as.numeric(summary(gdi2015int5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015h5b",as.numeric(summary(gdi2015h5b)$coefficients[c(4,25),c(1,2,5)])))

results<-rbind(results,c("pcagfp5b",as.numeric(summary(pcagfp5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcapa5b",as.numeric(summary(pcapa5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcak5b",as.numeric(summary(pcak5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcaint5b",as.numeric(summary(pcaint5b)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcah5b",as.numeric(summary(pcah5b)$coefficients[c(4,25),c(1,2,5)])))

#Table S8cc
results<-rbind(results,c("gdi1995gfp5c",as.numeric(summary(gdi1995gfp5c)$coefficients[c(4,23),c(1,2,5)])))
results<-rbind(results,c("gdi1995pa5c",as.numeric(summary(gdi1995pa5c)$coefficients[c(4,23),c(1,2,5)])))
results<-rbind(results,c("gdi1995k5c",as.numeric(summary(gdi1995k5c)$coefficients[c(4,23),c(1,2,5)])))
results<-rbind(results,c("gdi1995int5c",as.numeric(summary(gdi1995int5c)$coefficients[c(4,12),c(1,2,5)])))
results<-rbind(results,c("gdi1995h5c",as.numeric(summary(gdi1995h5c)$coefficients[c(4,23),c(1,2,5)])))

results<-rbind(results,c("gem1995gfp5c",as.numeric(summary(gem1995gfp5c)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995pa5c",as.numeric(summary(gem1995pa5c)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995k5c",as.numeric(summary(gem1995k5c)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995int5c",as.numeric(summary(gem1995int5c)$coefficients[c(4,22),c(1,2,5)])))
results<-rbind(results,c("gem1995h5c",as.numeric(summary(gem1995h5c)$coefficients[c(4,22),c(1,2,5)])))

results<-rbind(results,c("giigfp5c",as.numeric(summary(giigfp5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giipa5c",as.numeric(summary(giipa5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giik5c",as.numeric(summary(giik5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giiint5c",as.numeric(summary(giiint5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("giih5c",as.numeric(summary(giih5c)$coefficients[c(4,25),c(1,2,5)])))

results<-rbind(results,c("gggigfp5c",as.numeric(summary(gggigfp5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggipa5c",as.numeric(summary(gggipa5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggik5c",as.numeric(summary(gggik5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggiint5c",as.numeric(summary(gggiint5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gggih5c",as.numeric(summary(gggih5c)$coefficients[c(4,25),c(1,2,5)])))

results<-rbind(results,c("gdi2015gfp5c",as.numeric(summary(gdi2015gfp5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015pa5c",as.numeric(summary(gdi2015pa5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015k5c",as.numeric(summary(gdi2015k5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015int5c",as.numeric(summary(gdi2015int5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("gdi2015h5c",as.numeric(summary(gdi2015h5c)$coefficients[c(4,25),c(1,2,5)])))

results<-rbind(results,c("pcagfp5c",as.numeric(summary(pcagfp5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcapa5c",as.numeric(summary(pcapa5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcak5c",as.numeric(summary(pcak5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcaint5c",as.numeric(summary(pcaint5c)$coefficients[c(4,25),c(1,2,5)])))
results<-rbind(results,c("pcah5c",as.numeric(summary(pcah5c)$coefficients[c(4,25),c(1,2,5)])))



