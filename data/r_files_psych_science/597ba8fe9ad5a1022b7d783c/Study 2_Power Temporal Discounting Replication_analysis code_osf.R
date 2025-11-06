library("TOSTER")
library(PSYC201)
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)
library(psych)
library(MBESS)
library(lsr)
library(ez)
library(effsize)

# Data set ----------------------------------------------------------------
## The following commented-out code are used to read in data
## For student privacy purposes, we remove student ID information from the dataset we share on osf

# header <- read.csv("Sep_2016_Study_3.csv", header = TRUE, nrows = 1, skip = 0)
# header <- names(header)
# air <- read.csv("Sep_2016_Study_3.csv", header = FALSE, col.names = header, skip = 2)
# 
# air <- subset(air,V7 == 0 & V10 == 1) # remove survey previews and incomplete responses
# nrow(air) # 465
# names(air)
# air = air[,-c(11,12)] # embedded text in qualtrics added in week 2 & 3 for screening out week 1 participants
# names(air)[12]='SID'
# air <- air[air$SID %in% c('test','test-jyoti','999') == F,] # take out test trials by experimenters
# 
# air$SID= NA
# write.csv(air, 'Power Temporal Discounting Study 2.csv',row.names=F)

## osf code starts here ####
air = read.csv('Power Temporal Discounting Study 2.csv')
nrow(air)
# Demongraphics ##########################
table(air$Gender)
mean(air$Age,na.rm=T)
sd(air$Age,na.rm=T)

# Power conditions #############################
for (i in 1:nrow(air)){
  if(air$HP_write[i] != ""){air$PowerCond[i] = "HP"}
  else if(air$LP_write[i] != ""){air$PowerCond[i] = "LP"}
  else if(air$Ctr_write[i] != ""){air$PowerCond[i] = "Ctr"}
  else{air$PowerCond[i] = NA}
}

table(air$PowerCond)
air$PowerCond = factor(air$PowerCond, c('HP','Ctr','LP'))
air$PowerCondT = ordered(air$PowerCond,levels = c('HP','LP','Ctr')) 
# because we want the coefficients to reflect the effect of HP - LP, so use this order for t tests 

# PowerCode for regression
for (i in 1:nrow(air)){
  if(air$PowerCond[i] =='HP'){air$PowerCode[i] = 3}
  else if(air$PowerCond[i] =='Ctr'){air$PowerCode[i] = 2}
  else if(air$PowerCond[i] =='LP'){air$PowerCode[i] = 1}
  else{air$PowerCode[i] = NA}
}

table(air$PowerCode)

# Connection with the future self -----------------------------------------
alpha(data.frame(air$Connected, air$Similar))
air$Connection = (air$Connected + air$Similar)/2
hist(air$Connection)

## Counterbalance 
names(air)[46]= 'CounterBalance'


# Indifference points -----------------------------------------------------

# calculate Indifference Point
# take out participants who have missing responses 
missing = c()
for(i in 1: nrow(air)){
  if(sum(is.na(air[i,c(20:27)])) != 0 ){missing = append(missing,i)}
}

length(missing) # 6 participants incomplete response in titration task
# e.g. 30346 - Did not make it forced response. 

table(air$PowerCond[missing])
AllTrue<- function(vec){
  for (i in 1: length(vec)){
    if (vec[i] == FALSE){
      return(FALSE)
    }
  }
  return(TRUE)
}
## record participants who max out 
nodiscount<-c()                                             ## record participants who do not discount
inconsistent <-c()                                          ## recorde inconsistent participants

# choosing 1 is 21 days now, choosing 2 is the future amount in a year

for(i in 1: nrow(air)){
  if(i %in% missing){air$Indiff[i] = NA}
  else if (AllTrue(air[i,20:27] == 1)){air$Indiff[i] = 37}
  else if(AllTrue(air[i,20:27] == 2)){                       ## No discount
    nodiscount = append(nodiscount,i)
    air$Indiff[i] = NA
  }
  else if (AllTrue(air[i,27] == 1) && AllTrue(air[i, 20:26] == 2)){   ## From here on, regular titration 
    air$Indiff[i] = 23                               
  }
  else if (AllTrue(air[i,26:27] == 1) && AllTrue(air[i, 20:25] == 2)){
    air$Indiff[i] = 25
  }
  else if (AllTrue(air[i,25:27] == 1) && AllTrue(air[i, 20:24] == 2)){
    air$Indiff[i] = 27
  }
  else if (AllTrue(air[i,24:27] == 1) && AllTrue(air[i, 20:23]== 2)){
    air$Indiff[i] =29
  }
  else if (AllTrue(air[i,23:27] == 1) && AllTrue(air[i, 20:22] == 2)){
    air$Indiff[i] = 31
  }
  else if (AllTrue(air[i,22:27] == 1) && AllTrue(air[i, 20:21] == 2)){
    air$Indiff[i] = 33
  }
  else if (AllTrue(air[i,21:27] == 1) && AllTrue(air[i, 20] == 2)){
    air$Indiff[i] = 35
  }
  else {
    air$Indiff[i] = NA
    inconsistent = append(inconsistent,i)
  }
}
sum(air$Indiff == 37,na.rm=T) #211

length(inconsistent)
table(air$PowerCond[inconsistent]) 
chisq.test(rbind(table(air$PowerCond[inconsistent]), table(air$PowerCond)))

length(nodiscount)
table(air$PowerCond[nodiscount]) 
chisq.test(rbind(table(air$PowerCond[nodiscount]), table(air$PowerCond)))
table(air$PowerCond[nodiscount]) / table(air$PowerCond) 

# titration only discount rate ####
air$KT = air$Indiff/21 - 1
hist(air$KT)

airkt = subset(air, !is.na(air$KT))

save(air, file = 'air.rdata')
save(airkt,file = 'airkt.rdata')

load('airkt.rdata')
nrow(airkt)
table(airkt$Gender)
round(mean(airkt$Age),digits = 2)
round(sd(airkt$Age),digits = 2)

# Manipulation check ------------------------------------------------------
summary(aov(SRPower ~ PowerCond,airkt))
etaSquared(aov(SRPower ~ PowerCond,airkt))
ci.pvaf(F.value=194.8, df.1=2, df.2=396, N=399, conf.level=.95)
# HP and LP#
independentSamplesTTest(SRPower ~ PowerCond, subset(airkt, PowerCond != "Ctr"))
with(subset(airkt, PowerCond != "Ctr"),cohen.d(SRPower,PowerCond,na.rm=T))
# Ctr and LP#
independentSamplesTTest(SRPower ~ PowerCond, subset(airkt, PowerCond != "HP"))
with(subset(airkt, PowerCond != "HP"),cohen.d(SRPower,PowerCond,na.rm=T))
# HP and Ctr# 
independentSamplesTTest(SRPower ~ PowerCond, subset(airkt, PowerCond != "LP"))
with(subset(airkt, PowerCond != "LP"),cohen.d(SRPower,PowerCond,na.rm=T))
## counterbalance effect ? ##

summary(aov(KT ~ as.factor(CounterBalance)*PowerCond, airkt))
summary(aov(Connection ~ as.factor(CounterBalance)*PowerCond, airkt))

## Discount rate contrast analysis ####
levels(airkt$PowerCond)
airkt$PowerCond = factor(airkt$PowerCond, c('HP','Ctr','LP'))
KT.lm = lm(KT ~ PowerCond, airkt)
registered.Inv = contrast(KT.lm,c(1,-.5,-.5))
registered.Inv # n.s.
ci.pvaf(F.value=(0.7584573)^2, df.1=1, df.2=397, N=399, conf.level=.95)

# for joshi & Fast (2013) study 3
ci.pvaf(F.value=(2.32)^2, df.1=1, df.2=76, N=78, conf.level=.95)
sum(!is.na(airkt$KT))


## ANOVA ####

anova(lm(KT ~ PowerCond, airkt))#*

airkt%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m = round(mean(KT, na.rm=T),digits =2), 
            sd = round(sd(KT, na.rm=T), digits =2))
s2 = airkt%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m = round(mean(KT, na.rm=T),digits =3), 
            sd = round(sd(KT, na.rm=T), digits =3))

write.csv(s2, file = 'KT descriptives.csv')
sum(!is.na(airkt$KT))
independentSamplesTTest(KT ~ PowerCond, subset(airkt, PowerCond != 'Ctr')) # n.s.
with(subset(airkt, PowerCond != "Ctr"),cohen.d(KT,PowerCond,na.rm=T))
independentSamplesTTest(KT ~ PowerCond, subset(airkt, PowerCond != 'HP'))# * LP had higher discount rate than Ctr
with(subset(airkt, PowerCond != "HP"),cohen.d(KT,PowerCond,na.rm=T))
independentSamplesTTest(KT ~ PowerCond, subset(airkt, PowerCond != 'LP'))# marginal
with(subset(airkt, PowerCond != "LP"),cohen.d(KT,PowerCond,na.rm=T))

anova(lm(KT ~ PowerCond*Connection, airkt))

## Equivalence Test ####
TOSTtwo(m1=0.567,sd1 = 0.26, n1=138, m2= 0.551,sd2=0.267,n2=120,
        low_eqbound_d = -0.4309130, high_eqbound_d = 0.4309130, alpha = 0.05 )
# higly sig, can reject
#################################
## Number of Immediate choices ##
#################################

air$NNow = NA

for (i in 1:nrow(air)){
  if(i %in% c(inconsistent, nodiscount)){}
  else{air$NNow[i] = sum(air[i,20:27] == 1)}
}

hist(air$NNow)

##  inconsistent and no discount excluded  ## 
sum(!is.na(air$NNow)) #399
hist(air$NNow)

air %>%
  group_by(PowerCond)%>%
  summarize(m.now = mean(NNow,na.rm=T)/8, sd.now = sd(NNow/8, na.rm=T))


m1 = glm(cbind(NNow, 8-NNow) ~ PowerCond, air, family = binomial())
anova(m1, test= "Chisq") # model testing
summary(m1)

m2 = glm(cbind(NNow, 8-NNow) ~ PowerCond, 
         subset(air,PowerCond != 'Ctr'), family = binomial())
anova(m2, test= "Chisq") # model testing
summary(m2)
# odds ratio
1/exp(coef(m2)[2])# effect HP
1/exp(confint(m2))



m3 = glm(cbind(NNow, 8-NNow) ~ PowerCond, 
         subset(air,PowerCond != 'LP'), family = binomial())
anova(m3, test= "Chisq") # model testing
summary(m3)
# odds ratio
exp(coef(m3)[2])# effect of control condition 
exp(confint(m3))


m4 = glm(cbind(NNow, 8-NNow) ~ PowerCond, 
         subset(air,PowerCond != 'HP'), family = binomial())
anova(m4, test= "Chisq") # model testing
summary(m4)
# odds ratio
1/exp(coef(m4)[2]) # effect of control condition 
1/exp(confint(m4))

# Connection with the future self ---------------------------------------
alpha(data.frame(airkt$Connected, airkt$Similar))
hist(airkt$Connected)
connection.descriptives=airkt%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m.Conn = mean(Connection),sd.conn= sd(Connection))
write.csv(connection.descriptives, file = "connection.descriptives.csv")
# direction: HP is higher than LP 
# replicating original regression analyses 

# contrast analysis for connectionr#
levels(airkt$PowerCond)
Connection.lm = lm(Connection ~ PowerCond,airkt)
connect = contrast(Connection.lm,c(1,-.5,-.5))
connect 


summary(aov(Connection ~ PowerCond,airkt)) # 
summary(lm(Connection ~ PowerCond,airkt)) # 

# HP and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airkt, PowerCond != "Ctr")) #* 
with(subset(airkt, PowerCond != "Ctr"), cohen.d(Connection, PowerCond))
# Ctr and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airkt, PowerCond != "HP"))
with(subset(airkt, PowerCond != "HP"), cohen.d(Connection, PowerCond))
# HP and Ctr# 
independentSamplesTTest(Connection ~ PowerCond, subset(airkt, PowerCond != "LP"))
with(subset(airkt, PowerCond != "LP"), cohen.d(Connection, PowerCond))

cor.test(airkt$KT, airkt$Connection)
## regressions 
## Coding power as 1,2,3 
summary(lm(KT ~ PowerCode, airkt))

summary(lm(KT ~ Connection, airkt))

summary(lm(KT ~ Connection+PowerCode, airkt))


summary(lm(Connection ~ PowerCode,airkt)) #*
table(airkt$PowerCode, airkt$PowerCond)

## mediation ##
library(mediation)

b = lm(KT ~ PowerCode, airkt)
c = lm(KT ~ PowerCode + Connection, airkt)
model = mediate(b,c, treat ='PowerCode',mediator='Connection',sims= 5000)
summary(model)
model$ConfInt

## Moderators ####
# Experience with tasks ---------------------------------------------------
# scale(NRecall)
hist(airkt$NRecall)
sort(airkt$NRecall)

hist(log10(airkt$NRecall))

mean(airkt$NRecall,na.rm=T)
sd(airkt$NRecall,na.rm=T)

sum(is.na(airkt$NRecall)) # 2 
sum(is.na(airkt$NDiscount)) # 2
summary(lm(KT ~ PowerCode*scale(NRecall),airkt))
confint(lm(KT ~ PowerCode*scale(NRecall),airkt))

## Dichotomized 
table(airkt$NRecall == 0)
table(airkt$NRecall == 0)/nrow(subset(airkt, !is.na(scale(NRecall))))

airkt$RecallY = airkt$NRecall != 0
summary(aov(KT ~ PowerCond*RecallY,airkt)) 
etaSquared(aov(KT ~ PowerCond*RecallY,airkt))

airkt %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KT), sd.K = sd(KT))


## scale(NDiscount) 
hist(airkt$NDiscount)
mean(airkt$NDiscount,na.rm=T)
sd(airkt$NDiscount,na.rm=T)

sort(airkt$NDiscount)
table(airkt$NDiscount== 0)
table(airkt$NDiscount== 0)/nrow(subset(airkt, !is.na(NDiscount)))

summary(lm(KT ~ PowerCode*scale(NDiscount),airkt))
confint(lm(KT ~ PowerCode*scale(NDiscount),airkt))

## dichotomized
airkt$DiscountY = airkt$NDiscount != 0
summary(aov(KT ~ PowerCond*DiscountY,airkt)) 
etaSquared(aov(KT ~ PowerCond*DiscountY,airkt))

## Gender ##
summary(aov(KT ~ PowerCond*as.factor(Gender),airkt)) 
etaSquared(aov(KT ~ PowerCond*Gender,airkt))

## Titration + Matching  ############################################
# Titration+Matching calculations ------------------------------------------------------

maxout<- c()                                                ## record participants who max out 
nodiscount<-c()                                             ## record participants who do not discount
inconsistent <-c()                                          ## recorde inconsistent participants

# choosing 1 is 21 days now, choosing 2 is the future amount in a year


for(i in 1: nrow(air)){
  if(i %in% missing){air$Indiff[i] = NA}
  else if (AllTrue(air[i,20:27] == 1)){                           ##max out the titration scale
    maxout = append(maxout,i)
  }
  else if(AllTrue(air[i,20:27] == 2)){                       ## No discount
    nodiscount = append(nodiscount,i)
    air$Indiff[i] = NA
  }
  else if (AllTrue(air[i,27] == 1) && AllTrue(air[i, 20:26] == 2)){   ## From here on, regular titration 
    air$Indiff[i] = 23                              
  }
  else if (AllTrue(air[i,26:27] == 1) && AllTrue(air[i, 20:25] == 2)){
    air$Indiff[i] = 25
  }
  else if (AllTrue(air[i,25:27] == 1) && AllTrue(air[i, 20:24] == 2)){
    air$Indiff[i] = 27
  }
  else if (AllTrue(air[i,24:27] == 1) && AllTrue(air[i, 20:23]== 2)){
    air$Indiff[i] = 29
  }
  else if (AllTrue(air[i,23:27] == 1) && AllTrue(air[i, 20:22] == 2)){
    air$Indiff[i] = 31
  }
  else if (AllTrue(air[i,22:27] == 1) && AllTrue(air[i, 20:21] == 2)){
    air$Indiff[i] = 33
  }
  else if (AllTrue(air[i,21:27] == 1) && AllTrue(air[i, 20] == 2)){
    air$Indiff[i] = 35
  }
  else {
    air$Indiff[i] = NA
    inconsistent = append(inconsistent,i)
  }
}

length(inconsistent) #20
table(air$PowerCond[inconsistent])
rbind(table(air$PowerCond[inconsistent]), table(air$PowerCond[-inconsistent]))
chisq.test(rbind(table(air$PowerCond[inconsistent]), table(air$PowerCond[-inconsistent])))

length(nodiscount)#38
table(air$PowerCond[nodiscount]) 
chisq.test(rbind(table(air$PowerCond[nodiscount]), table(air$PowerCond[-nodiscount])))

length(maxout)#211
table(air$PowerCond[maxout])


## Matching indifference point
names(air)
air$Match_1_1_TEXT
air$Match_1_1_TEXT = as.character(air$Match_1_1_TEXT)

# some participants wrote infinite, exclude them
# exclude infinite 
infinite = c()
for (i in 1:nrow(air)){
  if(air$Match_1_1_TEXT[i] %in% c('infinite',"infinite ")){
    air$Match[i] = NA
    infinite = append(infinite, i)}
  else {air$Match[i] = as.numeric(air$Match_1_1_TEXT[i])}}
sort(air$Match)
# because of the big magnitude difference, R displaces the numbers in exponential format

sum(is.na(air$Match)) #4


# titration + matching discount rate------------------------------------------------
# Use match for max out participants
# Check if matching is consistent with titration
sum(!is.na(air$Match[maxout]) & air$Match[maxout] <= 35) # 60 ppts 

air$IndiffKTM = NA
air$IndiffKTM[-maxout] = air$Indiff[-maxout]
for(i in maxout){
  if(!is.na(air$Match[i]) & air$Match[i] >35){air$IndiffKTM[i] = air$Match[i]} 
  # take out ppts with indifference point < 21 in matching
  else{air$IndiffKTM[i] = NA}
}

sum(!is.na(air$IndiffKTM)) # 336

air$KTM <- with(air, (as.numeric(IndiffKTM) / 21) - 1 )
airKTM = subset(air, !is.na(air$KTM))
sum(!is.na(air$Match[maxout]) & air$Match[maxout] <= 35) # 60 ppts 
table(airKTM$Gender)
round(mean(airKTM$Age),digits=2)
round(sd(airKTM$Age),digits=2)
save(airKTM, file = 'airKTM.rdata')

## Titration + Matching Discount rate #############################################################
##
## contrast analyses ## 
##
# compare HP to LP and Ctr
hist(airKTM$KTM)
skewness(airKTM$KTM)
kurtosis(airKTM$KTM)
levels(airKTM$PowerCond)

##
## transformations ##
##
min(airKTM$KTM, na.rm = T) # 0.05 

# sqrt
hist(sqrt(airKTM$KTM+1- min(airKTM$KTM,na.rm=T))) # not enough
skewness(sqrt(airKTM$KTM+1- min(airKTM$KTM,na.rm=T)))
kurtosis(sqrt(airKTM$KTM+1- min(airKTM$KTM,na.rm=T)))

# log 
hist(log10(airKTM$KTM+1- min(airKTM$KTM,na.rm=T))) # not enough
skewness(log10(airKTM$KTM+1- min(airKTM$KTM,na.rm=T)))
kurtosis(log10(airKTM$KTM+1- min(airKTM$KTM,na.rm=T)))

# Inverse 
hist(2-(1/(airKTM$KTM + 1- min(airKTM$KTM,na.rm=T)))) # best
skewness(2-(1/(airKTM$KTM + 1- min(airKTM$KTM,na.rm=T))))
kurtosis(2-(1/(airKTM$KTM + 1- min(airKTM$KTM,na.rm=T))))

airKTM$InvKTM = 2-(1/(airKTM$KTM + 1- min(airKTM$KTM,na.rm=T)))

##
## Kruskal-Wallis rank sum ## 
##
kruskal.test(KTM ~ PowerCond, airKTM) # *

airKTM %>%
  group_by(PowerCond) %>%
  summarize(m.InvKTM = median(KTM,na.rm=T))


anova(lm(KTM ~ PowerCond*Connection, airKTM))
## InvKTM contrast ####
levels(airKTM$PowerCond)
InvKTM.lm = lm(InvKTM ~ PowerCond, airKTM)
registered.Inv = contrast(InvKTM.lm,c(1,-.5,-.5))
registered.Inv # *

airKTM %>%
  group_by(PowerCond) %>%
  summarize(n = n(),m.InvKTM = mean(InvKTM,na.rm=T), sd.InvKTM = sd(InvKTM,na.rm=T))

##
## InvKTM ANOVA ##
##
summary(aov(InvKTM ~ PowerCond, airKTM)) 
diagnostic.plot(lm(InvKTM ~ PowerCond, airKTM))
etaSquared(aov(InvKTM ~ PowerCond, airKTM))

independentSamplesTTest(InvKTM ~ PowerCond, subset(airKTM, PowerCond != "LP"),var.equal = T) # *
with(subset(airKTM, PowerCond != "LP"), cohen.d(InvKTM, PowerCond))
independentSamplesTTest(InvKTM ~ PowerCond, subset(airKTM, PowerCond != "HP")) #*
with(subset(airKTM, PowerCond != "HP"), cohen.d(InvKTM, PowerCond))
independentSamplesTTest(InvKTM ~ PowerCond, subset(airKTM, PowerCond != "Ctr")) # n.s.


# Connection with the future self ---------------------------------------
alpha(data.frame(airKTM$Connected, airKTM$Similar))
hist(airKTM$Connected)
airKTM%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m.Conn = mean(Connection),sd.conn= sd(Connection))
# direction: HP is higher than LP 
#replicating original regression analyses 

# contrast analysis reported in the original paper #
levels(airKTM$PowerCond)
Connection.lm = lm(Connection ~ PowerCond,airKTM)
connect = contrast(Connection.lm,c(1,-.5,-.5))
connect # p = .06


summary(aov(Connection ~ PowerCond,airKTM)) # p = .1
summary(lm(Connection ~ PowerCond,airKTM)) # HP/LP *
LSD.test(lm(Connection ~ PowerCond,airKTM),trt= 'PowerCond')$groups # no difference 

# HP and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airKTM, PowerCond != "Ctr")) #* 
# Ctr and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airKTM, PowerCond != "HP"))
# HP and Ctr# 
independentSamplesTTest(Connection ~ PowerCond, subset(airKTM, PowerCond != "LP"))


connect.alt = contrast(Connection.lm,c( .5, .5, -1))
connect.alt # .08


## regressions 
## Coding power as 1,2,3 
summary(lm(InvKTM ~ PowerCode, airKTM))


summary(lm(InvKTM ~ Connection, airKTM))

summary(lm(InvKTM ~ Connection+PowerCode, airKTM))


summary(lm(Connection ~ PowerCode,airKTM)) #*
table(airKTM$PowerCode, airKTM$PowerCond)

## mediation ##
library(mediation)

b = lm(InvKTM ~ PowerCode, airKTM)
c = lm(InvKTM ~ PowerCode + Connection, airKTM)
model = mediate(b,c, treat ='PowerCode',mediator='Connection',sims= 5000)
summary(model)
model$ConfInt

## InvInvKTMM Moderators ####

# Experience with tasks ---------------------------------------------------
# scale(NRecall)
hist(airKTM$NRecall)
sort(airKTM$NRecall)

hist(log10(airKTM$NRecall))

mean(airKTM$NRecall,na.rm=T)
sd(airKTM$NRecall,na.rm=T)

sum(is.na(airKTM$NRecall)) # 2 
sum(is.na(airKTM$NDiscount)) # 2
summary(lm(InvKTM ~ PowerCode*scale(NRecall),airKTM))
confint(lm(InvKTM ~ PowerCode*scale(NRecall),airKTM))

## Dichotomized 
table(airKTM$NRecall == 0)
table(airKTM$NRecall == 0)/nrow(subset(airKTM, !is.na(scale(NRecall))))

airKTM$RecallY = airKTM$NRecall != 0
summary(aov(InvKTM ~ PowerCond*RecallY,airKTM)) 
etaSquared(aov(InvKTM ~ PowerCond*RecallY,airKTM))

airKTM %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(InvKTM), sd.K = sd(InvKTM))


## scale(NDiscount) 
hist(airKTM$NDiscount)
mean(airKTM$NDiscount,na.rm=T)
sd(airKTM$NDiscount,na.rm=T)

sort(airKTM$NDiscount)
table(airKTM$NDiscount== 0)
table(airKTM$NDiscount== 0)/nrow(subset(airKTM, !is.na(scale(NDiscount))))

summary(lm(InvKTM ~ PowerCode*scale(NDiscount),airKTM))
confint(lm(InvKTM ~ PowerCode*scale(NDiscount),airKTM))

## dichotomized
airKTM$DiscountY = airKTM$NDiscount != 0
summary(aov(InvKTM ~ PowerCond*DiscountY,airKTM)) 
etaSquared(aov(InvKTM ~ PowerCond*DiscountY,airKTM))

## Gender ##
summary(aov(InvKTM ~ PowerCond*as.factor(Gender),airKTM)) 
etaSquared(aov(InvKTM ~ PowerCond*Gender,airKTM))


## KTM Exclude by 3 IQR ####
IQRupper = median(airKTM$KTM,na.rm=T)+3*IQR(airKTM$KTM,na.rm=T) # 5.47619
IQRlower = median(airKTM$KTM,na.rm=T)-3*IQR(airKTM$KTM,na.rm=T) # negative

sum(airKTM$KTM > IQRupper) # 42 

for (i in 1:nrow(airKTM)){
  if(!is.na(airKTM$KTM[i]) & (airKTM$KTM[i] > IQRupper)){airKTM$KTMIQR[i] = NA}
  else {airKTM$KTMIQR[i] = airKTM$KTM[i]}
}

sum(!is.na(airKTM$KTMIQR))# 

table(airKTM$PowerCond[airKTM$KTM > IQRupper]) 
chisq.test(table(airKTM$PowerCond,airKTM$KTM > IQRupper))

hist(airKTM$KTMIQR)

## Final sample with 3IQR ####

r2 = function(x){format(round(x, 2), nsmall = 2)}
airKTMIQR = subset(airKTM, !is.na(airKTM$KTMIQR))
nrow(airKTMIQR)
r2(table(airKTMIQR$Gender))
r2(mean(airKTMIQR$Age))
r2(sd(airKTMIQR$Age))

save(airKTMIQR,file = 'airKTMIQR.rdata')

summary(aov(KTMIQR ~ PowerCond, airKTMIQR))
etaSquared(aov(KTMIQR ~ PowerCond, airKTMIQR))
# Connection with the future self ---------------------------------------
alpha(data.frame(airKTM$Connected, airKTM$Similar))
hist(airKTM$Connected)
airKTM%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m.Conn = mean(Connection),sd.conn= sd(Connection))
# direction: HP is higher than LP 
#replicating original regression analyses 

# contrast analysis reported in the original paper #
levels(airKTM$PowerCond)
Connection.lm = lm(Connection ~ PowerCond,airKTM)
connect = contrast(Connection.lm,c(1,-.5,-.5))
connect # p = .06


summary(aov(Connection ~ PowerCond,airKTM)) # p = .1
summary(lm(Connection ~ PowerCond,airKTM)) # HP/LP *
LSD.test(lm(Connection ~ PowerCond,airKTM),trt= 'PowerCond')$groups # no difference 

# HP and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airKTM, PowerCond != "Ctr")) #* 
# Ctr and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airKTM, PowerCond != "HP"))
# HP and Ctr# 
independentSamplesTTest(Connection ~ PowerCond, subset(airKTM, PowerCond != "LP"))


connect.alt = contrast(Connection.lm,c( .5, .5, -1))
connect.alt # .08


## regressions 
## Coding power as 1,2,3 
summary(lm(KTMIQR ~ PowerCode, airKTM))


summary(lm(KTMIQR ~ Connection, airKTM))

summary(lm(KTMIQR ~ Connection+PowerCode, airKTM))


summary(lm(Connection ~ PowerCode,airKTM)) #*
table(airKTM$PowerCode, airKTM$PowerCond)

## mediation ##
library(mediation)

b = lm(KTMIQR ~ PowerCode, airKTM)
c = lm(KTMIQR ~ PowerCode + Connection, airKTM)
model = mediate(b,c, treat ='PowerCode',mediator='Connection',sims= 5000)
summary(model)
model$ConfInt

## InvKTMIQRM Moderators ####

# Experience with tasks ---------------------------------------------------
# scale(NRecall)
hist(airKTM$NRecall)
sort(airKTM$NRecall)

hist(log10(airKTM$NRecall))

mean(airKTM$NRecall,na.rm=T)
sd(airKTM$NRecall,na.rm=T)

sum(is.na(airKTM$NRecall)) # 2 
sum(is.na(airKTM$NDiscount)) # 2
summary(lm(KTMIQR ~ PowerCode*scale(NRecall),airKTM))
confint(lm(KTMIQR ~ PowerCode*scale(NRecall),airKTM))

## Dichotomized 
table(airKTM$NRecall == 0)
table(airKTM$NRecall == 0)/nrow(subset(airKTM, !is.na(scale(NRecall))))

airKTM$RecallY = airKTM$NRecall != 0
summary(aov(KTMIQR ~ PowerCond*RecallY,airKTM)) 
etaSquared(aov(KTMIQR ~ PowerCond*RecallY,airKTM))

airKTM %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KTMIQR), sd.K = sd(KTMIQR))


## scale(NDiscount) 
hist(airKTM$NDiscount)
mean(airKTM$NDiscount,na.rm=T)
sd(airKTM$NDiscount,na.rm=T)

sort(airKTM$NDiscount)
table(airKTM$NDiscount== 0)
table(airKTM$NDiscount== 0)/nrow(subset(airKTM, !is.na(scale(NDiscount))))

summary(lm(KTMIQR ~ PowerCode*scale(NDiscount),airKTM))
confint(lm(KTMIQR ~ PowerCode*scale(NDiscount),airKTM))

## dichotomized
airKTM$DiscountY = airKTM$NDiscount != 0
summary(aov(KTMIQR ~ PowerCond*DiscountY,airKTM)) 
etaSquared(aov(KTMIQR ~ PowerCond*DiscountY,airKTM))

## Gender ##
summary(aov(KTMIQR ~ PowerCond*as.factor(Gender),airKTM)) 
etaSquared(aov(KTMIQR ~ PowerCond*Gender,airKTM))

## Matching only ####

min(air$Match,na.rm=T) # 0
max(air$Match,na.rm=T) # 10^12
sum(!is.na(air$Match)) # 459, only excluding 4 that wrote infinity. 
table(subset(air, is.na(Match))$PowerCond)
chisq.test(table(air$PowerCond, air$Match > 21))
air$KM = with(air, (as.numeric(Match) / 21) - 1 )

## Using the Indiff > 21 rule 
sum(air$Match <=21,na.rm=T)
table(subset(air, Match <=21)$PowerCond)
chisq.test(table(air$PowerCond, air$Match > 21))

airm = subset(air, Match > 21)
nrow(airm) 

table(airm$Gender)
round(mean(airm$Age),digits=2)
round(sd(airm$Age),digits=2)

save(airm, file = 'airm.rdata')

# Matching Only -----------------------------------------------------------
load('airm.rdata')
names(airm)
hist(airm$KM)
skewness(airm$KM)       
kurtosis(airm$KM)
nrow(airm)
## Kruskal-Wallis rank sum ## 
kruskal.test(KM ~ PowerCond, airm)

airm %>%
  group_by(PowerCond) %>%
  summarize(m.InvKM = median(KTM,na.rm=T))

wilcox.test(KM ~ PowerCond, subset(airm, PowerCond != "Ctr"))
wilcox.test(KM ~ PowerCond, subset(airm, PowerCond != "HP"))
wilcox.test(KM ~ PowerCond, subset(airm, PowerCond != "LP"))



## transformations for matching based ## 


# sqrt
hist(sqrt(airm$KM+1- min(airm$KM,na.rm=T))) # not enough
skewness(sqrt(airm$KM+1- min(airm$KM,na.rm=T)))
kurtosis(sqrt(airm$KM+1- min(airm$KM,na.rm=T)))

# log 
hist(log10(airm$KM+1- min(airm$KM,na.rm=T))) # not enough
skewness(log10(airm$KM+1- min(airm$KM,na.rm=T)))
kurtosis(log10(airm$KM+1- min(airm$KM,na.rm=T)))

# Inverse 
hist(2-(1/(airm$KM + 1- min(airm$KM,na.rm=T)))) # best
skewness(2-(1/(airm$KM + 1- min(airm$KM,na.rm=T))))
kurtosis(2-(1/(airm$KM + 1- min(airm$KM,na.rm=T))))

airm$InvKM = 2-(1/(airm$KM + 1- min(airm$KM,na.rm=T)))


##  InvKM ANOVA ##
summary(aov(InvKM ~ PowerCond, airm)) # *
etaSquared(aov(InvKM ~ PowerCond, airm))
diagnostic.plot(lm(InvKM ~ PowerCond, airm))
etaSquared(aov(InvKM ~ PowerCond, airm))


LSD.test(lm(InvKM ~ PowerCond,airm),trt= 'PowerCond')$means
LSD.test(lm(InvKM ~ PowerCond,airm),trt= 'PowerCond')$parameters # critical t value
LSD.test(lm(InvKM ~ PowerCond,airm),trt= 'PowerCond')$groups # calculated with pooled variance 

# HP and LP#
independentSamplesTTest(InvKM ~ PowerCond, subset(airm, PowerCond != "Ctr"))
with(subset(airm, PowerCond != "Ctr"), cohen.d(InvKM, PowerCond))
# Ctr and LP#
independentSamplesTTest(InvKM ~ PowerCond, subset(airm, PowerCond != "HP"))
with(subset(airm, PowerCond != "HP"), cohen.d(InvKM, PowerCond))
# HP and Ctr# 
independentSamplesTTest(InvKM ~ PowerCond, subset(airm, PowerCond != "LP"))
with(subset(airm, PowerCond != "LP"), cohen.d(InvKM, PowerCond))

## contrast ##
levels(airm$PowerCond)
InvKM.lm = lm(InvKM ~ PowerCond, airm)
registered.KM = contrast(InvKM.lm,c(1,-.5,-.5))
registered.KM # p = .03

# Connection with the future self ---------------------------------------
alpha(data.frame(airm$Connected, airm$Similar))
hist(airm$Connected)
airm%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m.Conn = mean(Connection),sd.conn= sd(Connection))
# direction: HP is higher than LP 
#replicating original regression analyses 

# contrast analysis reported in the original paper #
levels(airm$PowerCond)
Connection.lm = lm(Connection ~ PowerCond,airm)
connect = contrast(Connection.lm,c(1,-.5,-.5))
connect # p = .06


summary(aov(Connection ~ PowerCond,airm)) # p = .1
summary(lm(Connection ~ PowerCond,airm)) # HP/LP *
LSD.test(lm(Connection ~ PowerCond,airm),trt= 'PowerCond')$groups # no difference 

# HP and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airm, PowerCond != "Ctr")) #* 
# Ctr and LP#
independentSamplesTTest(Connection ~ PowerCond, subset(airm, PowerCond != "HP"))
# HP and Ctr# 
independentSamplesTTest(Connection ~ PowerCond, subset(airm, PowerCond != "LP"))


connect.alt = contrast(Connection.lm,c( .5, .5, -1))
connect.alt # .08


## regressions 
## Coding power as 1,2,3 
summary(lm(KM ~ PowerCode, airm))


summary(lm(KM ~ Connection, airm))

summary(lm(KM ~ Connection+PowerCode, airm))


summary(lm(Connection ~ PowerCode,airm)) #*
table(airm$PowerCode, airm$PowerCond)

## mediation ##
library(mediation)

b = lm(InvKM ~ PowerCode, airm)
c = lm(InvKM ~ PowerCode + Connection, airm)
model = mediate(b,c, treat ='PowerCode',mediator='Connection',sims= 5000)
summary(model)
model$ConfInt

## InvKM Moderators ####

# Experience with tasks ---------------------------------------------------
# scale(NRecall)
hist(airm$NRecall)
sort(airm$NRecall)

hist(log10(airm$NRecall))

mean(airm$NRecall,na.rm=T)
sd(airm$NRecall,na.rm=T)

sum(is.na(airm$NRecall)) # 2 
sum(is.na(airm$NDiscount)) # 2
summary(lm(InvKM ~ PowerCode*scale(NRecall),airm))
confint(lm(InvKM ~ PowerCode*scale(NRecall),airm))

library(pequod)

recallslope<-lmres(InvKM~PowerCode*NRecall, centered=c('NRecall'),data = airm)
NRecall.slope<-simpleSlope(recallslope,pred="PowerCode",mod1="NRecall")
summary(NRecall.slope) 



## Dichotomized 
table(airm$NRecall == 0)
table(airm$NRecall == 0)/nrow(subset(airm, !is.na(scale(NRecall))))

airm$RecallY = airm$NRecall != 0
summary(aov(InvKM ~ PowerCond*RecallY,airm)) 
etaSquared(aov(InvKM ~ PowerCond*RecallY,airm))

airm %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(InvKM), sd.K = sd(InvKM))


## scale(NDiscount) 
hist(airm$NDiscount)
mean(airm$NDiscount,na.rm=T)
sd(airm$NDiscount,na.rm=T)

sort(airm$NDiscount)
table(airm$NDiscount== 0)
table(airm$NDiscount== 0)/nrow(subset(airm, !is.na(scale(NDiscount))))

summary(lm(InvKM ~ PowerCode*scale(NDiscount),airm))
confint(lm(InvKM ~ PowerCode*scale(NDiscount),airm))

## dichotomized
airm$DiscountY = airm$NDiscount != 0
summary(aov(InvKM ~ PowerCond*DiscountY,airm)) 
etaSquared(aov(InvKM ~ PowerCond*DiscountY,airm))

## Gender ##
summary(aov(InvKM ~ PowerCond*as.factor(Gender),airm)) 
etaSquared(aov(InvKM ~ PowerCond*Gender,airm))
