######## Cross-Cultural Sex Differences in Age of Mate with Mate Age 18 and Older #######
#### Kathryn Walter and Dan Conroy-Beam ####


### load packages ###
library(ggplot2)
library(lmerTest)
library(psych)
library(gridExtra)
library(ggpubr)
library(effsize)


### set seed ###
set.seed(1302019)


### load data ###
data<-read.csv(file.choose())



################################ prepare data ############################################

#change categorical data to factors
data$PIN<-as.factor(data$PIN)
data$CIN<-as.factor(data$CIN)
data$sample<-as.factor(data$sample)
data$sex<-as.factor(data$sex)
data$religious<-as.factor(data$religious)
data$relstat<-as.factor(data$relstat)
data$relstat2<-as.factor(data$relstat2)

#divide gross measures of pathogen by population size 
data$cmc_yll<-data$cmc_yll/data$popsize
data$infect_death<-data$infect_death/data$popsize
data$infect_yll<-data$infect_yll/data$popsize

#change contrasts for the categorical predictor variables
contrasts(data$continent)<-contr.sum(6)
contrasts(data$country_religion)<-contr.sum(14)

#new pathogen prevalence index (average of deaths and yll)
data$pathindex<-((scale(data$infect_death)+scale(data$infect_yll))/2)

#new gender equality index (PCA of GII, GDI, GGGI)
gepca<-pca(data[,28:30],nfactors=1)
data$gepcascores<-gepca$scores

#standardize outcome variables
data$zideal_resources<-scale(data$ideal_resources)
data$zideal_intelligence<-scale(data$ideal_intelligence)
data$zideal_kindness<-scale(data$ideal_kindness)
data$zideal_health<-scale(data$ideal_health)
data$zideal_physatt<-scale(data$ideal_physatt)

#standardize predictor variables 
data$zcmc_yll<-scale(data$cmc_yll)
data$zpathindex<-scale(data$pathindex)

### subset data to only include those who reported a mate age ### n = 8920
data<-data[complete.cases(data$mate_age),]
### subset data to include only those with mates older than 10 ### n = 8614
data<-data[data$mate_age>10,]

#create age difference variable
data$agediff<-(data$mate_age-data$age)
#standardize outcome variable
data$zagediff<-scale(data$agediff)



############### Overall Sex Difference in Age of Mate ##################

agediff<-lmer(zagediff~sex+(1+sex|CIN),data=data)
ageslopes<-coef(agediff)$CIN[,2]

#average for males and females
meanage<-tapply(data$agediff,data$sex,function(x) mean(x))

#bootstrapped confidence intervals

#Number of bootstrap iterations
bootloops<-10000

#Blank dataframes to store resampled preference and choice means
mbootprefs<-data.frame(matrix(,1,bootloops+1))
fbootprefs<-data.frame(matrix(,1,bootloops+1))

#Label each row with the mean it will store
mbootprefs[,1]<-c("agediff")
fbootprefs[,1]<-c("agediff")

#Loop through bootstrap iterations
for(l in 1:bootloops){
  
  #Resample from the data
  bootdata<-data[sample(nrow(data),nrow(data),replace=T),]
  
  #Calculates the average male preference and female agediff across countries
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




################ Mate Age Difference as a function of Gender Equality #################

#gggi
gggiage<-lmer(zagediff~sex*(scale(gggi))+(1+sex|CIN),data=data)

#gdi2015
gdi2015age<-lmer(zagediff~sex*(scale(gdi2015))+(1+sex|CIN),data=data)

#gii
giiage<-lmer(zagediff~sex*(scale(gii))+(1+sex|CIN),data=data)

#gem1995
gem1995age<-lmer(zagediff~sex*(scale(gem1995))+(1+sex|CIN),data=data)

#gdi1995
gdi1995age<-lmer(zagediff~sex*(scale(gdi1995))+(1+sex|CIN),data=data)

#PCA
pcaage<-lmer(zagediff~sex*(scale(gepcascores))+(1+sex|CIN),data=data)


######################## Gender Equality Analyses With Controls #########################

#gggi
gggiage2<-lmer(zagediff~sex*(scale(gggi))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi2015
gdi2015age2<-lmer(zagediff~sex*(scale(gdi2015))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gii
giiage2<-lmer(zagediff~sex*(scale(gii))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gem1995
gem1995age2<-lmer(zagediff~sex*(scale(gem1995))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#gdi1995
gdi1995age2<-lmer(zagediff~sex*(scale(gdi1995))+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)

#pca
pcaage2<-lmer(zagediff~sex*gepcascores+scale(lattitude)+scale(gdp_percap)+continent+(1+sex|CIN),data=data)




################################# Pathogen Prevalence ###################################

#gb index
ppage1<-lmer(zagediff~(scale(gb_path))+(1|CIN),data=data)

#communicable
ppage2<-lmer(zagediff~zcmc_yll+(1|CIN),data=data)

#average yll and avg deaths
ppage3<-lmer(agediff~zpathindex+(1|CIN),data=data)


######################## Pathogen Prevalence Analyses With Controls ##########################

#gbindex
ppage1b<-lmer(zagediff~(scale(gb_path))+scale(gdp_percap)+scale(lattitude)+country_religion+(1|CIN),data=data)

#communicable
ppage2b<-lmer(zagediff~zcmc_yll+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)

#average yll and average deaths
ppage3b<-lmer(zagediff~zpathindex+scale(gdp_percap)+scale(lattitude)+continent+country_religion+(1|CIN),data=data)



######### Cohen's d ##################

#for funnel plot in supplementary materials

data2<-data
data2$country<-factor(data2$country)
countrylist2<-unique(data2$country)
aged<-data.frame("country"=countrylist2,"d"=rep(0,41), "n"=rep(0,41))


for(k in 1:41){
  
  d<-cohen.d(agediff~sex, data=data2[data2$country==countrylist2[k],])
  #fix order issue for Cohen's d 
  aged$d[k]<-abs(d$estimate)*(-1)
  aged$n[k]<-nrow(data2[data2$country==countrylist2[k],])
  
}

#save to load into other script for funnel plot
write.csv(aged, file = "AgeCohensd.csv",row.names=F)


############################ Plots #############################

#### overall sex difference ####

#create ploting dataframe for overall sex difference in age of mate
data2<-data
levels(data2$sex)<-c("Females","Males")
data2$country<-factor(data2$country)
dataage<-data.frame("country"=unique(data2$country),"slopes"=coef(agediff)$CIN[,2],"n"=tapply(data2$zagediff,data2$country,length))

#graph of age slope (sex dif) by country
ageslopes<-qplot(slopes,country,color=country,data=dataage,xlab="Sex Difference in Age of Partner", ylab="Country", size=I(6))+theme_classic(base_size=25)+geom_vline(xintercept =0, size=2)+theme(legend.position="none")+coord_cartesian(xlim=c(-2,2))

#dif between self and mate age across ages seperated by sex (Kenrick and Keefe)
agediffkk<-qplot(age,agediff,color=sex,data=data2,xlab="Participant Age",ylab="Age Difference (Partner-Self)",alpha=I(.30))+geom_smooth(size=I(3.5),method="loess")+theme_classic(base_size=20)+geom_hline(yintercept=0)+scale_color_manual(name="Sex",labels=c("Female","Male"),values=c("dodgerblue","limegreen"))+scale_x_continuous(breaks=seq(10,100,10))+scale_y_continuous(breaks=seq(-50,40,10))


##### Gender Equality #####

#create plotting dataset
data3<-data
#make country a factor
data3$country<-factor(data3$country)

#Find the value of each gender equality variable for each country
gem1995av<-tapply(data3$gem1995,data3$country,mean)
gdi1995av<-tapply(data3$gdi1995,data3$country,mean)
giiav<-tapply(data3$gii,data3$country,mean)
gdi2015av<-tapply(data3$gdi2015,data3$country,mean)
gggiav<-tapply(data3$gggi,data3$country,mean)
ge_compav<-tapply(data3$gepcascores,data3$country,mean)

#new dataframe with national level gender equality variables separated by sex
ge_plotdata<-data.frame("country"=rep(unique(data3$country),2),"sex"=rep(c("Female","Male"),each= 41),"gem1995"=rep(gem1995av,2),"gdi1995"=rep(gdi1995av,2),"gii"=rep(giiav,2),"gdi2015"=rep(gdi2015av,2),"gggi"=rep(gggiav,2),"ge_comp"=rep(ge_compav,2))

#create dataframe for plotting age of mate sex difference and gender equality
age_ge_plotdata<-data.frame(ge_plotdata,"agedif"=c(tapply(data3$agediff[data3$sex==0],data3$country[data3$sex==0],function(x) mean(x,na.rm=T)),tapply(data3$agediff[data3$sex==1],data2$country[data3$sex==1],function(x) mean(x,na.rm=T))))

#plot gender equality composite and agediff
agepca<-qplot(ge_comp,agedif,color=sex,data=age_ge_plotdata,xlab="Gender Equality Composite",ylab="Age Difference (Partner-Self)",size=I(2))+geom_smooth(method="lm",se=TRUE,size=2)+theme_classic(base_size=20)+scale_color_manual(name="Sex",values=c("dodgerblue","limegreen"))

##simple slopes for age and gender equality composite
pcaagef<-lmer(zagediff~scale(gepcascores)+(1|CIN),data=data[data$sex==0,])
pcaagem<-lmer(zagediff~scale(gepcascores)+(1|CIN),data=data[data$sex==1,])

#agediff and gggi (to include countries that don't have information for gii or gdi2015)
agegggi<-qplot(gggi,agedif,color=sex,data=age_ge_plotdata,xlab="Global Gender Gap Index",ylab="Age Difference (Partner-Self)",size=I(2))+geom_smooth(method="lm",se=TRUE,size=2)+theme_classic(base_size=20)+scale_color_manual(name="Sex",values=c("dodgerblue","limegreen"))

##simple slopes for age and gggi
gggiagef<-lmer(zagediff~scale(gggi)+(1|CIN),data=data[data$sex==0,])
gggiagem<-lmer(zagediff~scale(gggi)+(1|CIN),data=data[data$sex==1,])


