library(lm.beta)
library(nFactors)
library(GPArotation)

data<-read.csv("~/Desktop/Study 1 Data.csv")
d<-subset(data, data$AttnCheck == 1 & data$Atheist != 1 & data$Agnostic !=1 )

##########################################
## Conflict and Negative Events Indices ##
##########################################

threats<-data.frame("naturaldisaster"=d$Threat_1,
                    "climatechange"=d$Threat_2,
                    "pollution"=d$Threat_3,
                    "terrorattack"=d$Threat_4,
                    "crimesurge"=d$Threat_5,
                    "gunviolence"=d$Threat_6,
                    "legalinjustice"=d$Threat_7,
                    "discrimination"=d$Threat_8,
                    "poverty"=d$Threat_9,
                    "debt"=d$Threat_10,
                    "illegaldrug"=d$Threat_11,
                    "lackofjobs"=d$Threat_12,
                    "lossofhousing"=d$Threat_13,
                    "fooddeprivation"=d$Threat_14,
                    "diseases"=d$Threat_15,
                    "lackmedical"=d$Threat_16,
                    "prescriptiondrug"=d$Threat_17,
                    "overcrowding"=d$Threat_18,
                    "Syrianrefugees"=d$Threat_19,
                    "illegalimmigration"=d$Threat_20,
                    "NorthKoreaAttack"=d$Threat_21,
                    "IranAttack"=d$Threat_22,
                    "RussiaAttack"=d$Threat_23,
                    "Corruption"=d$Threat_24)

ev <- eigen(cor(threats)) # get eigenvalues
print(ev)
ap <- parallel(subject=nrow(threats),var=ncol(threats),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qpea)
plotnScree(nS)

#Maximum Likelihood Factor Analysis, Varimax rotation
fit1<- kaiser(fa(threats, nfactors = 2,rotate = "varimax",fm="ml", missing = TRUE))
print(fit1, digits=2, cut=.39, sort=TRUE)

# plot factor 1 by factor 2 
load1 <- fit1$loadings[,1:2] 
plot(load1,type="n") # set up plot 
text(load1,labels=names(threats),cex=.7) # add variable names

#Creating separate indices
NegEvents<-data.frame("naturaldisaster"=d$Threat_1,
                    "climatechange"=d$Threat_2,
                    "pollution"=d$Threat_3,
                    "gunviolence"=d$Threat_6,
                    "legalinjustice"=d$Threat_7,
                    "discrimination"=d$Threat_8,
                    "poverty"=d$Threat_9,
                    "debt"=d$Threat_10,
                    "illegaldrug"=d$Threat_11,
                    "lackofjobs"=d$Threat_12,
                    "lossofhousing"=d$Threat_13,
                    "fooddeprivation"=d$Threat_14,
                    "lackmedical"=d$Threat_16,
                    "prescriptiondrug"=d$Threat_17,
                    "overcrowding"=d$Threat_18,
                    "Corruption"=d$Threat_24)

Conflict<-data.frame("terrorattack"=d$Threat_4,
                    "Syrianrefugees"=d$Threat_19,
                    "illegalimmigration"=d$Threat_20,
                    "NorthKoreaAttack"=d$Threat_21,
                    "IranAttack"=d$Threat_22,
                    "RussiaAttack"=d$Threat_23)

psych::alpha(NegEvents)
d$NegEvents<-rowMeans(NegEvents,na.rm=T)
psych::alpha(Conflict)
d$Conflict<-rowMeans(Conflict,na.rm=T)

##########################################
#######         God Indices        #######
##########################################

VOG<-data.frame("helping"=d$GodTrait_1,
                "generous"=d$GodTrait_2,
                "compassionate"=d$GodTrait_3,
                "gracious"=d$GodTrait_4,
                "tolerant"=d$GodTrait_5,
                "caring"=d$GodTrait_6,
                "accepting"=d$GodTrait_7,
                "merciful"=d$GodTrait_8,
                "forgiving"=d$GodTrait_9,
                "controlling"=d$GodTrait_10,
                "restricting"=d$GodTrait_11,
                "stern"=d$GodTrait_12,
                "commanding"=d$GodTrait_13,
                "strict"=d$GodTrait_14,
                "angry"=d$GodTrait_15,
                "judging"=d$GodTrait_16,
                "punishing"=d$GodTrait_17,
                "wrathful"=d$GodTrait_18)


ev2 <- eigen(cor(VOG)) # get eigenvalues
print(ev2)
ap2 <- parallel(subject=nrow(VOG),var=ncol(VOG),
               rep=100,cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qpea)
plotnScree(nS2)

#Maximum likelihood FA, varimax rotation
fit2<- kaiser(fa(VOG,nfactors = 2,rotate = "varimax",fm="ml"))
print(fit2, digits=2, cut=.39, sort=TRUE)

# plot factor 1 by factor 2 
load2 <- fit2$loadings[,1:2] 
plot(load2,type="n") # set up plot 
text(load2,labels=names(threats),cex=.7) # add variable names

#Creating separate indices
Loving<-data.frame("helping"=d$GodTrait_1,
                "generous"=d$GodTrait_2,
                "compassionate"=d$GodTrait_3,
                "gracious"=d$GodTrait_4,
                "tolerant"=d$GodTrait_5,
                "caring"=d$GodTrait_6,
                "accepting"=d$GodTrait_7,
                "merciful"=d$GodTrait_8,
                "forgiving"=d$GodTrait_9)

Punitive<-data.frame("controlling"=d$GodTrait_10,
                   "restricting"=d$GodTrait_11,
                   "stern"=d$GodTrait_12,
                   "commanding"=d$GodTrait_13,
                   "strict"=d$GodTrait_14,
                   "angry"=d$GodTrait_15,
                   "judging"=d$GodTrait_16,
                   "punishing"=d$GodTrait_17,
                   "wrathful"=d$GodTrait_18)

psych::alpha(Loving)
d$Loving<-rowMeans(Loving,na.rm=T)
psych::alpha(Punitive)
d$Punitive<-rowMeans(Punitive,na.rm=T)

##########################################
#######    Multiple Regressions    #######
##########################################

model1<-(lm(d$Punitive~d$Conflict+d$NegEvents))
summary(model1)
lm.beta(model1)

model2<-(lm(d$Punitive~d$Conflict+d$NegEvents+d$Religiosity+d$Conservatism))
summary(model2)
lm.beta(model2)

model3<-(lm(d$Loving~d$Conflict+d$NegEvents))
summary(model3)
lm.beta(model3)

##########################################
#######    Measure Correlations    #######
##########################################
library(Hmisc)
correlations<-data.frame(d$Conflict, d$NegEvents, d$Loving, d$Punitive, 
                         d$Religiosity, d$Conservatism)
rcorr(as.matrix(correlations))
