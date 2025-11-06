library(nFactors)

data<-read.csv("~/Desktop/Study 2 Pilot Data.csv")
d<-subset(data, data$Condition==1 & data$Religion != 8 & data$Religion != 0
          & data$Religion != 10) #selects religious participants in control condition


d$Singapore<-ifelse(d$Nation==1,1,0)
d$Brazil<-ifelse(d$Nation==2,1,0)
d$Germany<-ifelse(d$Nation==3,1,0)
d$USA<-ifelse(d$Nation==4,1,0)
d$NationTL<-ifelse(d$Nation==1,1,
                   ifelse(d$Nation==2,0,
                          ifelse(d$Nation==3,1,
                                 ifelse(d$Nation==4,0,
                                        NA))))

##########################################
## Conflict and Negative Events Indices ##
##########################################

threats<-data.frame("naturaldisaster"=d$SocietalThreat_1,
                    "climatechange"=d$SocietalThreat_2,
                    "pollution"=d$SocietalThreat_3,
                    "terrorist"=d$SocietalThreat_4,
                    "crimesurge"=d$SocietalThreat_5,
                    "discrimination"=d$SocietalThreat_6,
                    "debt"=d$SocietalThreat_7,
                    "diseases"=d$SocietalThreat_8,
                    "refugees"=d$SocietalThreat_9,
                    "illegalimmigration"=d$SocietalThreat_10,
                    "foreignattack"=d$SocietalThreat_11,
                    "identityloss"=d$SocietalThreat_12)

ev <- eigen(cor(threats)) # get eigenvalues
print(ev)
ap <- parallel(subject=nrow(threats),var=ncol(threats),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qpea)
plotnScree(nS)

# Maximum Likelihood Factor Analysis, Varimax rotation
fit <- kaiser(fa(threats, nfactors=2, rotate="varimax", fm="ml"))
print(fit, digits=2, cut=.39, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(threats),cex=.7) # add variable names

#Creating separate indices
Conflict<-data.frame("terrorist"=d$SocietalThreat_4,
                            "refugees"=d$SocietalThreat_9,
                            "illegalimmigration"=d$SocietalThreat_10,
                            "foreignattack"=d$SocietalThreat_11,
                            "identityloss"=d$SocietalThreat_12)

NegEvents<-data.frame("naturaldisaster"=d$SocietalThreat_1,
                         "climatechange"=d$SocietalThreat_2,
                         "pollution"=d$SocietalThreat_3,
                         "discrimination"=d$SocietalThreat_6,
                         "debt"=d$SocietalThreat_7,
                         "diseases"=d$SocietalThreat_8,
                         "crimesurge"=d$SocietalThreat_5)

psych::alpha(Conflict)
d$Conflict<-rowMeans(Conflict,na.rm = T)
psych::alpha(NegEvents)
d$NegEvents<-rowMeans(NegEvents,na.rm = T)

##########################################
#######         God Indices        #######
##########################################

VOG<-data.frame("helping"=d$VOG_1,
                "compassionate"=d$VOG_2,
                "merciful"=d$VOG_3,
                "strict"=d$VOG_4,
                "wrathful"=d$VOG_5,
                "punishing"=d$VOG_6)

ev2 <- eigen(cor(VOG)) # get eigenvalues
print(ev2)
ap2 <- parallel(subject=nrow(VOG),var=ncol(VOG),
               rep=100,cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qpea)
plotnScree(nS2)

# Maximum Likelihood Factor Analysis, varimax rotation 
fit2 <- kaiser(fa(VOG, nfactors=2, rotate="varimax", fm="ml"))
print(fit2, digits=2, cut=.39, sort=TRUE)
# plot factor 1 by factor 2 
load2 <- fit2$loadings[,1:2] 
plot(load2,type="n") # set up plot 
text(load2,labels=names(VOG),cex=.7) # add variable names

Loving<-data.frame("helping"=d$VOG_1,
                 "compassionate"=d$VOG_2,
                 "merciful"=d$VOG_3)

Punitive<-data.frame("strict"=d$VOG_4,
                 "wrathful"=d$VOG_5,
                 "punishing"=d$VOG_6)

psych::alpha(Loving)
d$Loving<-rowMeans(Loving,na.rm=T)
psych::alpha(Punitive)
d$Punitive<-rowMeans(Punitive,na.rm=T)

#creating composite score of general religiosity
SBS<-data.frame(d$SBS_1, d$SBS_2, d$SBS_3, d$SBS_4, d$SBS_5, d$SBS_6)
psych::alpha(SBS)
d$Religiosity<-rowMeans(SBS,na.rm=T)

##########################################
#######   Multiple Regressions     #######
##########################################

model1<-(lm(d$Punitive~d$Conflict+d$NegEvents))
summary(model1) 
lm.beta(model1)

model2<-(lm(d$Punitive~d$Conflict+d$NegEvents+d$Conservative+d$Religiosity))
summary(model2) 
lm.beta(model2)

model3<-(lm(d$Punitive~d$Conflict*d$NationTL+d$NegEvents+d$Conservative+d$Religiosity))
summary(model3) 
lm.beta(model3)

model4<-(lm(d$Loving~d$Conflict+d$NegEvents))
summary(model4)
lm.beta(model4)

