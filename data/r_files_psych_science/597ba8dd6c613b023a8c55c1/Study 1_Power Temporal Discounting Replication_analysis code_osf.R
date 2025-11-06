library(PSYC201)
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)
library(ez)
library(lsr)
library(pequod)
library(effsize)
library("TOSTER")
# Data set ----------------------------------------------------------------
## The following commented-out code are used to combining the two parts of the studies together 
## The two files are combined by student ID
## For student privacy purposes, we remove student ID information from the dataset we share on osf

# part 1 is the main study
# header <- read.csv("Jan_2017_Study_1__Part_1.csv", header = TRUE, nrows = 1, skip = 0)
# header <- names(header)
# moneypart1 <- read.csv("Jan_2017_Study_1__Part_1.csv", header = FALSE, col.names = header, skip = 2)
# 
# moneypart1 <- subset(moneypart1,V7 == 0 & V10 == 1) # remove survey previews and incomplete responses
# nrow(moneypart1) 
# names(moneypart1)
# moneypart1 = moneypart1[,-c(11,12)] 
# # remove variable for screening participants who did previous replication study
# names(moneypart1)[12]='SID'
# moneypart1$SID = factor(moneypart1$SID) 
# 
# # part 2 included a few additional measures
# header <- read.csv("Jan_2017_Study_1___Part_2.csv", header = TRUE, nrows = 1, skip = 0)
# header <- names(header)
# moneypart2 <- read.csv("Jan_2017_Study_1___Part_2.csv", header = FALSE, col.names = header, skip = 2)
# moneypart2 <- subset(moneypart2,V7 == 0 & V10 == 1) 
# # remove survey previews and incomplete responses
# nrow(moneypart2) 
# names(moneypart2)
# moneypart2 = moneypart2[,-c(11,12)] 
# # remove variable for screening participants who did previous replication study
# names(moneypart2)[12]='SID'
# moneypart2$SID = factor(moneypart2$SID) 
# 
# # combine by SID
# money = inner_join(moneypart1,moneypart2,by = "SID")
# nrow(money) # 420 finished both parts of the study and can be indentified


# # Anonymize by Taking out student ID information. 
# money$SID = NA
# write.csv(money,file = 'Power Temporal Discounting Study 1.csv',row.names = F)

## OSF codes start ####
money = read.csv('Power Temporal Discounting Study 1.csv')
names(money)

nrow(money) #420 
table(money$Gender)
mean(money$Age)
sd(money$Age)

## Power manipualation conditions ## 
table(money$DO.BR.FL_8)
money$PowerCond = ifelse(money$DO.BR.FL_8 == 'Hi_power','HP','LP')

money$PowerCond = factor(money$PowerCond, c('LP','HP') )
## LP as control group, HP as treatment group
money$PowerCondT = ordered(money$PowerCond, levels = c('HP','LP'))
# for t tests, we want the the coefficients to reflect HP effect relative to LP, thus reorder it
table(money$PowerCond)

## sense of power ##

money$SOP <- (money$SOP1+ money$SOP3 + money$SOP5 + money$SOP8+
                   32 - money$SOP2 - money$SOP4 - money$SOP6 - money$SOP7)/8


## connection with the future self ##
money$Connection = (money$Connected+ money$Similar)/2

# Attention & Manipulation Check ------------------------------------------
## Attention/Role Check ##
money$RoleCK = NA
table(money$Role)
for (i in 1:nrow(money)){
  if(money$PowerCond[i] == 'HP' & money$Role[i] == 1){money$RoleCK[i] = 1}
  else if(money$PowerCond[i] == 'LP' & money$Role[i] == 2){money$RoleCK[i] = 1}
  else{money$RoleCK[i] = 0}
}

table(money$PowerCond, money$RoleCK)
chisq.test(table(money$PowerCond, money$RoleCK)) #35 failed

# exclude participants who did not pass attention check
money = subset(money, RoleCK == 1)
nrow(money) # n= 385


# Extract Indifference point -------------------------------------------------

## Titration ##
## Indifference Point ##
AllTrue<- function(vec){
  for (i in 1: length(vec)){
    if (vec[i] == FALSE){
      return(FALSE)
    }
  }
  return(TRUE)
}

names(money[i,28:36]) # titration questions
maxout<- c()                                                ## record participants who max out 
nodiscount<-c()                                             ## record participants who prefer $120 in a year to $120 today
inconsistent <-c()                                          ## record inconsistent participants

for(i in 1: nrow(money)){
  if (AllTrue(money[i,28:36] == 1)){                           ##max out the titration scale
    maxout = append(maxout,i)
    money$Indiff[i] = NA
  }
  else if(AllTrue(money[i,29:36] == 2)){                       ##No discount
    nodiscount = append(nodiscount,i)
    money$Indiff[i] = NA
  }
  else if (AllTrue(money[i,28:29] == 1) && AllTrue(money[i, 30:36] == 2)){   ## From here on, regular titration 
    money$Indiff[i] = 137                               ## Indiff estimate = half way at flip
  }
  else if (AllTrue(money[i,28:30] == 1) && AllTrue(money[i, 31:36] == 2)){
    money$Indiff[i] = 154
  }
  else if (AllTrue(money[i,28:31] == 1) && AllTrue(money[i, 32:36] == 2)){
    money$Indiff[i] = 171
  }
  else if (AllTrue(money[i,28:32] == 1) && AllTrue(money[i, 33:36]== 2)){
    money$Indiff[i] = 189
  }
  else if (AllTrue(money[i,28:33] == 1) && AllTrue(money[i, 34:36] == 2)){
    money$Indiff[i] = 206
  }
  else if (AllTrue(money[i,28:34] == 1) && AllTrue(money[i, 35:36] == 2)){
    money$Indiff[i] = 223
  }
  else if (AllTrue(money[i,28:35] == 1) && AllTrue(money[i, 36] == 2)){
    money$Indiff[i] = 240
  }
  else {
    money$Indiff[i] = NA
    inconsistent = append(inconsistent,i)
  }
}


length(inconsistent) # 5
table(money[inconsistent,]$PowerCond)

length(nodiscount) #12
table(money[c(nodiscount),]$PowerCond)

length(maxout) #34
table(money[maxout,]$PowerCond)  

chisq.test(cbind (table(money[maxout,]$PowerCond) , table(money[-maxout,]$PowerCond) ))
nrow(money)#385

#################################
## Number of Immediate choices ##
#################################

money$NNow = NA

for (i in 1:nrow(money)){
  if(i %in% c(inconsistent, nodiscount)){}
  else{money$NNow[i] = sum(money[i,28:36] == 1)}
}

hist(money$NNow)

##  inconsistent and no discount excluded  ## 
sum(!is.na(money$NNow)) #368
hist(money$NNow)

money %>%
  group_by(PowerCond)%>%
  summarize(m.now = mean(NNow,na.rm=T)/9, sd.now = sd(NNow/9, na.rm=T))


m1 = glm(cbind(NNow, 9-NNow) ~ PowerCond, money, family = binomial())
anova(m1, test= "Chisq") # model testing
summary(m1)

# odds ratio
exp(coef(m1)[2])
exp(confint(m1))


## Matching Indifference point ####
money$Match_1_1_TEXT = as.character(money$Match_1_1_TEXT)
money$Match_1_1_TEXT = gsub(",", "", money$Match_1_1_TEXT)
#take out comma participants types
money$Matching = as.numeric(money$Match_1_1_TEXT)

max(money$Matching)
min(money$Matching)
sort(money$Matching)


## titration + matching ##
money$IndiffKTM = NA
money$IndiffKTM[-maxout] = money$Indiff[-maxout]
sum(money$Matching[maxout]<= 240) # 6 participants inconsistent with their titration choice 
for(i in maxout){
  if(!is.na(money$Matching[i]) & money$Matching[i] > 240){money$IndiffKTM[i] = money$Matching[i]}
  else{money$IndiffKTM[i]=NA}
}

sum(!is.na(money$IndiffKTM)) # 362

## demo for the KTM dataset
table(subset(money, !is.na(IndiffKTM))$Gender)
mean(subset(money, !is.na(IndiffKTM))$Age)
sd(subset(money, !is.na(IndiffKTM))$Age)

# KTM & KM calculations ####
money$KTM = with(money, IndiffKTM/120 - 1)
money$KM = ifelse(money$Matching > 120, money$Matching/120 - 1, NA)


# KTM Discount rate analyses --------------------------------------------------
hist(money$KTM)
skewness(money$KTM)
kurtosis(money$KTM)

# Trim - 3SD ##
mKTM = mean(money$KTM, na.rm=T)
sdKTM = sd(money$KTM,na.rm=T)
mKTM + 3*sdKTM
mKTM - 3*sdKTM
for (i in 1:nrow(money)){
  if(!is.na(money$KTM[i]) & (money$KTM[i] > (mKTM + 3*sdKTM)|money$KTM[i] < (mKTM - 3*sdKTM))){money$KTMTrim[i] = NA}
  else {money$KTMTrim[i] = money$KTM[i]}
}

sum(!is.na(money$KTMTrim))# 361
hist(money$KTMTrim)# still highly skewed 

skewness(money$KTMTrim)
kurtosis(money$KTMTrim)


## KTM Exclude by 3 IQR ####

IQRupper = median(money$KTM,na.rm=T)+3*IQR(money$KTM,na.rm=T) # 2.15
IQRlower = median(money$KTM,na.rm=T)-3*IQR(money$KTM,na.rm=T) # negative


for (i in 1:nrow(money)){
  if(!is.na(money$KTM[i]) & (money$KTM[i] > IQRupper)){money$KTM3IQR[i] = NA}
  else {money$KTM3IQR[i] = money$KTM[i]}
}

sum(!is.na(money$KTM3IQR))# 342
sum(!is.na(money$KTM))-sum(!is.na(money$KTM3IQR)) # 20 were excluded
table(money$Power[money$KTM > IQRupper]) # 10 in LP, 10 in HP

table(!is.na(money$KTM3IQR), money$PowerCond)
table(!is.na(money$KTM), money$PowerCond)
hist(money$KTM3IQR)

skewness(money$KTM3IQR)
kurtosis(money$KTM3IQR)

mean(money$KTM3IQR,na.rm=T)
sd(money$KTM3IQR, na.rm=T)


## Final sample with 3IQR ####
moneyIQR = subset(money, !is.na(money$KTM3IQR))
nrow(moneyIQR)
table(moneyIQR$Gender)
mean(moneyIQR$Age)
sd(moneyIQR$Age)


## Power Manipulation Check ####
# power over group member
independentSamplesTTest(Manipulation.Check_2 ~ PowerCondT,moneyIQR,var.equal = T)
with(moneyIQR,cohen.d(Manipulation.Check_2,PowerCond))

table(moneyIQR$PowerCond)

## KTM Discount rate analysis #####
independentSamplesTTest(KTM3IQR ~ PowerCondT,moneyIQR,var.equal = T)
with(moneyIQR,cohen.d(KTM3IQR,PowerCond))

summary(aov(KTM3IQR ~ PowerCond*Connection, moneyIQR))


## equivalence test ####
TOSTtwo(m1=0.432,sd1 = 0.32, n1=172, m2= 0.43,sd2=0.287,n2=170,
        low_eqbound_d = -0.3768422, high_eqbound_d = 0.3768422, alpha = 0.05 )


# Connection with the future self ---------------------------------------
library(psych)
alpha(data.frame(moneyIQR$Connected, moneyIQR$Similar)) # 0.53

hist(moneyIQR$Connection)
connection.descriptive = moneyIQR%>%
  group_by(PowerCond)%>%
  summarize(n = n(),m.Conn = mean(Connection),sd.conn= sd(Connection))
write.csv(connection.descriptive, 'connection.descriptive.csv')

independentSamplesTTest(Connection ~ PowerCondT, moneyIQR,var.equal = T)#n.s
with(money,cohen.d(Connection,PowerCond,na.rm=T))
# exploratory
independentSamplesTTest(Connected ~ PowerCond, moneyIQR)#n.s
independentSamplesTTest(Similar~ PowerCond, moneyIQR)#n.s

## regressions 
summary(lm(KTM3IQR ~ PowerCond, moneyIQR))
confint(lm(KTM3IQR ~ PowerCond, moneyIQR))

summary(lm(Connection ~ PowerCond,moneyIQR)) 
confint(lm(Connection ~ PowerCond,moneyIQR)) 

summary(lm(KTM3IQR ~ scale(Connection), moneyIQR))
confint(lm(KTM3IQR ~ scale(Connection), moneyIQR))

summary(lm(KTM3IQR ~ scale(Connection)+PowerCond, moneyIQR))
confint(lm(KTM3IQR ~ scale(Connection)+PowerCond, moneyIQR))

## mediation ##
library(mediation)

b = lm(KTM3IQR ~ PowerCond, moneyIQR)
c = lm(KTM3IQR ~ PowerCond + Connection, moneyIQR)
model = mediate(b,c, treat ='PowerCond',mediator='Connection',sims= 5000)
summary(model)

# Moderators --------------------------------------------------------------

## Legitimacy ####
hist(moneyIQR$Legitimate)
mean(moneyIQR$Legitimate)

independentSamplesTTest(Legitimate ~ PowerCond, moneyIQR, var.equal=T) # n.s 
with(moneyIQR,cohen.d(Legitimate,PowerCond,na.rm=T))
summary(lm(KTM3IQR ~ PowerCond*scale(Legitimate),moneyIQR)) # *
confint(lm(KTM3IQR ~ PowerCond*scale(Legitimate),moneyIQR))

legit<-lmres(KTM3IQR~PowerCond*Legitimate, centered=c('Legitimate'),data = moneyIQR)
legit.slope<-simpleSlope(legit,pred="PowerCondHP",mod1="Legitimate")
summary(legit.slope) 
# high legitimate positive
# low legitimate negative 

# Experience with tasks ---------------------------------------------------
# NVirtual
names(moneyIQR)[57] = 'NVirtual'
names(moneyIQR)[58] = 'NDiscount'
hist(moneyIQR$NVirtual)
sort(moneyIQR$NVirtual)

round(mean(moneyIQR$NVirtual,na.rm=T),digits=2)
round(sd(moneyIQR$NVirtual,na.rm=T),digits = 2)

summary(lm(KTM3IQR ~ PowerCond*scale(NVirtual),moneyIQR))
confint(lm(KTM3IQR ~ PowerCond*scale(NVirtual),moneyIQR))

## Dichotomized 
table(moneyIQR$NVirtual == 0)
table(moneyIQR$NVirtual == 0)/sum(!is.na(moneyIQR$NVirtual))

moneyIQR$Virtual0 = moneyIQR$NVirtual != 0
summary(aov(KTM3IQR ~ PowerCond*Virtual0,moneyIQR)) 
etaSquared(aov(KTM3IQR ~ PowerCond*RecallY,moneyIQR))

moneyIQR %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KTM3IQR), sd.K = sd(KTM3IQR))


## NDiscount 
hist(moneyIQR$NDiscount)
mean(moneyIQR$NDiscount,na.rm=T)
sd(moneyIQR$NDiscount,na.rm=T)

summary(lm(KTM3IQR ~ PowerCond*scale(NDiscount),moneyIQR))
confint(lm(KTM3IQR ~ PowerCond*scale(NDiscount),moneyIQR))

table(moneyIQR$NDiscount== 0)
table(moneyIQR$NDiscount== 0)/sum(!is.na(moneyIQR$NDiscount))


## Dichotomized 
table(moneyIQR$NDiscount== 0)
table(moneyIQR$NDiscount== 0)/sum(!is.na(moneyIQR$NDiscount))

moneyIQR$DiscountY = moneyIQR$NDiscount != 0
summary(aov(KTM3IQR ~ PowerCond*DiscountY,moneyIQR)) 
etaSquared(aov(KTM3IQR ~ PowerCond*DiscountY,moneyIQR))

moneyIQR %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KTM3IQR), sd.K = sd(KTM3IQR))


summary(lm(KTM3IQR ~ PowerCond*NDiscount,moneyIQR))
confint(lm(KTM3IQR ~ PowerCond*NDiscount,moneyIQR))


# SES ---------------------------------------------------------------------
## objective SES

hist(moneyIQR$Education_2)
hist(moneyIQR$Education_3)
moneyIQR$Paternal = scale(moneyIQR$Education_2)
moneyIQR$Maternal = scale(moneyIQR$Education_3)
mean(moneyIQR$Education_2)
sd(moneyIQR$Education_2)
mean(moneyIQR$Education_3)
sd(moneyIQR$Education_3)

hist(moneyIQR$Income_2)
mean(moneyIQR$Income_2)
sd(moneyIQR$Income_2)
moneyIQR$Income = scale(moneyIQR$Income_2)

moneyIQR$ObjectiveSES = with(moneyIQR, Paternal+Maternal+Income)
hist(moneyIQR$ObjectiveSES)

summary(lm(KTM3IQR ~ scale(ObjectiveSES)*PowerCond, moneyIQR))
confint(lm(KTM3IQR ~ scale(ObjectiveSES)*PowerCond, moneyIQR))

# Subjective
hist(moneyIQR$Ladder)
summary(lm(KTM3IQR ~ scale(Ladder)*PowerCond, moneyIQR)) # n.s.
confint(lm(KTM3IQR ~ scale(Ladder)*PowerCond, moneyIQR))

# Goals related to money --------------------------------------------------
# earn moneyIQR now
hist(moneyIQR$MoneyGoalNow)
independentSamplesTTest(MoneyGoalNow ~ PowerCond, moneyIQR)
summary(lm(KTM3IQR ~ scale(MoneyGoalNow)*PowerCond, moneyIQR))
confint(lm(KTM3IQR ~ scale(MoneyGoalNow)*PowerCond, moneyIQR))
with(moneyIQR,cohen.d(MoneyGoalNow,PowerCond,na.rm=T))
# Save for future
hist(moneyIQR$MoneyGoalFuture)
independentSamplesTTest(MoneyGoalFuture ~ PowerCond, moneyIQR) 
summary(lm(KTM3IQR ~ scale(MoneyGoalFuture)*PowerCond, moneyIQR)) # main effect of goal,marginal  interaction
confint(lm(KTM3IQR ~ scale(MoneyGoalFuture)*PowerCond, moneyIQR))
with(moneyIQR,cohen.d(MoneyGoalFuture,PowerCond,na.rm=T))
# Difference in goals
moneyIQR$MoneyGoalDiff = with(moneyIQR, MoneyGoalFuture - MoneyGoalNow)
hist(moneyIQR$MoneyGoalDiff)
independentSamplesTTest(MoneyGoalDiff ~ PowerCond, moneyIQR)
summary(lm(KTM3IQR ~ scale(MoneyGoalDiff)*PowerCond, moneyIQR)) # main effect of moneyIQR goal diff
confint(lm(KTM3IQR ~ scale(MoneyGoalDiff)*PowerCond, moneyIQR))
with(moneyIQR,cohen.d(MoneyGoalDiff,PowerCond,na.rm=T))

# SOP ---------------------------------------------------------------------
independentSamplesTTest(SOP ~ PowerCond, moneyIQR, var.equal = T) 
summary(lm(KTM3IQR ~ scale(SOP)*PowerCond, moneyIQR)) # positive effect of SOP
confint(lm(KTM3IQR ~ scale(SOP)*PowerCond, moneyIQR))
with(moneyIQR,cohen.d(SOP,PowerCond,na.rm=T))
## Gender ####
summary(aov(KTM3IQR ~ Gender*PowerCond, moneyIQR))
etaSquared(aov(KTM3IQR ~ Gender*PowerCond, moneyIQR))
independentSamplesTTest(KTM3IQR ~ Gender, moneyIQR,var.equal = T) 


## Transformations ####
max(money$KTM,na.rm=T)
min(money$KTM,na.rm=T)

# sqrt
hist(sqrt(money$KTM+1- min(money$KTM,na.rm=T))) # not enough
skewness(sqrt(money$KTM+1- min(money$KTM,na.rm=T)))
kurtosis(sqrt(money$KTM+1- min(money$KTM,na.rm=T)))

# log 
hist(log10(money$KTM+1- min(money$KTM,na.rm=T))) # not enough
skewness(log10(money$KTM+1- min(money$KTM,na.rm=T)))
kurtosis(log10(money$KTM+1- min(money$KTM,na.rm=T)))

# Inverse 
hist(2-(1/(money$KTM + 1- min(money$KTM,na.rm=T)))) # best
skewness(2-(1/(money$KTM + 1- min(money$KTM,na.rm=T))))
kurtosis(2-(1/(money$KTM + 1- min(money$KTM,na.rm=T))))

money$InvKTM = 2-(1/(money$KTM + 1- min(money$KTM,na.rm=T)))

# InvKTM t test #####
independentSamplesTTest(InvKTM ~ PowerCond, money,var.equal = T) # nothing 
with(money,cohen.d(InvKTM,PowerCond,na.rm=T))

summary(aov(InvKTM ~ PowerCond*Connection, money))
## Non parametric ##
wilcox.test(KTM ~ PowerCond,money) # n.s.

money%>%
  group_by(PowerCond)%>%
  summarize(n = n(), m.kTM = round(mean(KTM, na.rm=T),digits=2), 
            s.KTM = round(sd(KTM, na.rm=T),digits=2), mdn = round(median(KTM, na.rm=T),digits=2))

# InvKTM Moderators --------------------------------------------------------------

## Legitimacy ####
hist(money$Legitimate)
mean(money$Legitimate)

independentSamplesTTest(Legitimate ~ PowerCond, money, var.equal=T) #n.s. 

summary(lm(InvKTM ~ PowerCond*scale(Legitimate),money))#**
confint(lm(InvKTM ~ PowerCond*scale(Legitimate),money))
## simple slope ## exploratory
legit<-lmres(InvKTM~PowerCond*Legitimate, centered=c('Legitimate'),data = money)
legit.slope<-simpleSlope(legit,pred="PowerCond",mod1="Legitimate")
summary(legit.slope) 
# low legitimacy, high power discount less
# high legitimacy, low power discount less


# graph #
ggplot(data = money, aes(x=Legitimate, y = InvKTM, color = PowerCond))+geom_smooth(method = lm, se=F)


# Experience with tasks ---------------------------------------------------
# NVirtual
names(money)[57] = 'NVirtual'
names(money)[58] = 'NDiscount'
hist(money$NVirtual)
sort(money$NVirtual)

sum(is.na(money$NVirtual))# 1
sum(is.na(money$NDiscount))# 1

mean(money$NVirtual,na.rm=T)
sd(money$NVirtual,na.rm=T)


summary(lm(InvKTM ~ PowerCond*scale(NVirtual),money))
confint(lm(InvKTM ~ PowerCond*scale(NVirtual),money))

## Dichotomized 
table(money$NVirtual == 0)
table(money$NVirtual == 0)/sum(!is.na(money$NVirtual))

money$RecallY = money$NVirtual != 0
summary(aov(InvKTM ~ PowerCond*RecallY,money)) 
etaSquared(aov(InvKTM ~ PowerCond*RecallY,money))

money %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(InvKTM), sd.K = sd(InvKTM))


## NDiscount 
hist(money$NDiscount)
mean(money$NDiscount,na.rm=T)
sd(money$NDiscount,na.rm=T)

summary(lm(InvKTM ~ PowerCond*scale(NDiscount),money))
confint(lm(InvKTM ~ PowerCond*scale(NDiscount),money))

table(money$NDiscount== 0)
table(money$NDiscount== 0)/sum(!is.na(money$NDiscount))



## Dichotomized 
table(money$NDiscount== 0)
table(money$NDiscount== 0)/sum(!is.na(money$NDiscount))

money$DiscountY = money$NDiscount != 0
summary(aov(InvKTM ~ PowerCond*DiscountY,money)) 
etaSquared(aov(InvKTM ~ PowerCond*DiscountY,money))

money %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(InvKTM), sd.K = sd(InvKTM))


summary(lm(InvKTM ~ PowerCond*NDiscount,money))
confint(lm(InvKTM ~ PowerCond*NDiscount,money))


# SES ---------------------------------------------------------------------
## objective SES
summary(lm(InvKTM ~ scale(ObjectiveSES)*PowerCond, money))
confint(lm(InvKTM ~ scale(ObjectiveSES)*PowerCond, money))

# Subjective
hist(money$Ladder)
summary(lm(InvKTM ~ scale(Ladder)*PowerCond, money)) #n.s.
confint(lm(InvKTM ~ scale(Ladder)*PowerCond, money))
# Goals related to Money --------------------------------------------------

# earn money now
hist(money$MoneyGoalNow)
independentSamplesTTest(MoneyGoalNow ~ PowerCond, money)
summary(lm(InvKTM ~ scale(MoneyGoalNow)*PowerCond, money))
confint(lm(InvKTM ~ scale(MoneyGoalNow)*PowerCond, money))

# Save for future
hist(money$MoneyGoalFuture)
independentSamplesTTest(MoneyGoalFuture ~ PowerCond, money)
summary(lm(InvKTM ~ scale(MoneyGoalFuture)*PowerCond, money))
confint(lm(InvKTM ~ scale(MoneyGoalFuture)*PowerCond, money))

# Difference in money goals
independentSamplesTTest(MoneyGoalDiff ~ PowerCond, money)
summary(lm(InvKTM ~ scale(MoneyGoalDiff)*PowerCond, money))
confint(lm(InvKTM ~ scale(MoneyGoalDiff)*PowerCond, money))


# SOP ---------------------------------------------------------------------
summary(lm(InvKTM ~ scale(SOP)*PowerCond, money))
confint(lm(InvKTM ~ scale(SOP)*PowerCond, money))

## Gender ####
summary(aov(InvKTM ~ Gender*PowerCond, money))
etaSquared(aov(InvKTM ~ Gender*PowerCond, money))
independentSamplesTTest(InvKTM ~ Gender, money,var.equal = T) 




# Matching based Discount rate analyses --------------------------------------------------
moneyMatch = subset(money, Matching > 120)
nrow(moneyMatch)# 313
sum(money$Matching <=120) #72

table(moneyMatch$Gender)
mean(moneyMatch$Age)
sd(moneyMatch$Age)

skewness(moneyMatch$KM)
kurtosis(moneyMatch$KM)


## Non parametric ####
wilcox.test(KM ~ PowerCond,moneyMatch) # n.s.

moneyMatch%>%
  group_by(PowerCond)%>%
  summarize(n = n(), m.KM = round(mean(KM, na.rm=T),digits=2), 
            s.KM = round(sd(KM, na.rm=T),digits=2), mdn = round(median(KM, na.rm=T),digits=2))

## Transformations ####
max(moneyMatch$KM,na.rm=T)
min(moneyMatch$KM,na.rm=T) 

# sqrt
hist(sqrt(moneyMatch$KM+1- min(moneyMatch$KM,na.rm=T))) # not enough
skewness(sqrt(moneyMatch$KM+1- min(moneyMatch$KM,na.rm=T)))
kurtosis(sqrt(moneyMatch$KM+1- min(moneyMatch$KM,na.rm=T)))

# log 
hist(log10(moneyMatch$KM+1- min(moneyMatch$KM,na.rm=T))) # not enough
skewness(log10(moneyMatch$KM+1- min(moneyMatch$KM,na.rm=T)))
kurtosis(log10(moneyMatch$KM+1- min(moneyMatch$KM,na.rm=T)))

# Inverse 
hist(2-(1/(moneyMatch$KM + 1- min(moneyMatch$KM,na.rm=T)))) # best
skewness(2-(1/(moneyMatch$KM + 1- min(moneyMatch$KM,na.rm=T))))
kurtosis(2-(1/(moneyMatch$KM + 1- min(moneyMatch$KM,na.rm=T))))

moneyMatch$InvKM = 2-(1/(moneyMatch$KM + 1- min(moneyMatch$KM,na.rm=T)))

# InvKM t test #
independentSamplesTTest(InvKM ~ PowerCond, moneyMatch,var.equal = T) # nothing 
with(moneyMatch,cohen.d(InvKM,PowerCond,na.rm=T))
summary(lm(InvKM ~ SOP,moneyMatch)) # marginal

## InvKM Moderator ####
## Legitimacy ####
hist(moneyMatch$Legitimate)
mean(moneyMatch$Legitimate)

independentSamplesTTest(Legitimate ~ PowerCond, moneyMatch, var.equal=T) #n.s. 

summary(lm(InvKM ~ PowerCond*scale(Legitimate),moneyMatch))#**
confint(lm(InvKM ~ PowerCond*scale(Legitimate),moneyMatch))
## simple slope ## exploratory
legit<-lmres(InvKM~PowerCond*Legitimate, centered=c('Legitimate'),data = moneyMatch)
legit.slope<-simpleSlope(legit,pred="PowerCond",mod1="Legitimate")
summary(legit.slope) 
# low legitimacy, high power discount less
# high legitimacy, low power discount less


# graph #
ggplot(data = moneyMatch, aes(x=Legitimate, y = InvKM, color = PowerCond))+geom_smooth(method = lm, se=F)


# Experience with tasks ---------------------------------------------------
# NVirtual
names(moneyMatch)[57] = 'NVirtual'
names(moneyMatch)[58] = 'NDiscount'
hist(moneyMatch$NVirtual)
sort(moneyMatch$NVirtual)

sum(is.na(moneyMatch$NVirtual))# 1
sum(is.na(moneyMatch$NDiscount))# 1

mean(moneyMatch$NVirtual,na.rm=T)
sd(moneyMatch$NVirtual,na.rm=T)


summary(lm(InvKM ~ PowerCond*scale(NVirtual),moneyMatch))
confint(lm(InvKM ~ PowerCond*scale(NVirtual),moneyMatch))

## Dichotomized 
table(moneyMatch$NVirtual == 0)
table(moneyMatch$NVirtual == 0)/sum(!is.na(moneyMatch$NVirtual))

moneyMatch$RecallY = moneyMatch$NVirtual != 0
summary(aov(InvKM ~ PowerCond*RecallY,moneyMatch)) 
etaSquared(aov(InvKM ~ PowerCond*RecallY,moneyMatch))

moneyMatch %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(InvKM), sd.K = sd(InvKM))


## NDiscount 
hist(moneyMatch$NDiscount)
mean(moneyMatch$NDiscount,na.rm=T)
sd(moneyMatch$NDiscount,na.rm=T)

summary(lm(InvKM ~ PowerCond*scale(NDiscount),moneyMatch))
confint(lm(InvKM ~ PowerCond*scale(NDiscount),moneyMatch))

table(moneyMatch$NDiscount== 0)
table(moneyMatch$NDiscount== 0)/sum(!is.na(moneyMatch$NDiscount))



## Dichotomized 
table(moneyMatch$NDiscount== 0)
table(moneyMatch$NDiscount== 0)/sum(!is.na(moneyMatch$NDiscount))

moneyMatch$DiscountY = moneyMatch$NDiscount != 0
summary(aov(InvKM ~ PowerCond*DiscountY,moneyMatch)) 
etaSquared(aov(InvKM ~ PowerCond*DiscountY,moneyMatch))

moneyMatch %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(InvKM), sd.K = sd(InvKM))


summary(lm(InvKM ~ PowerCond*NDiscount,moneyMatch))
confint(lm(InvKM ~ PowerCond*NDiscount,moneyMatch))


# SES ---------------------------------------------------------------------
## objective SES
summary(lm(InvKM ~ scale(ObjectiveSES)*PowerCond, moneyMatch))
confint(lm(InvKM ~ scale(ObjectiveSES)*PowerCond, moneyMatch))

# Subjective
hist(moneyMatch$Ladder)
summary(lm(InvKM ~ scale(Ladder)*PowerCond, moneyMatch)) #n.s.
confint(lm(InvKM ~ scale(Ladder)*PowerCond, moneyMatch))
# Goals related to moneyMatch --------------------------------------------------

# earn moneyMatch now
hist(moneyMatch$moneyMatchGoalNow)
independentSamplesTTest(moneyMatchGoalNow ~ PowerCond, moneyMatch)
summary(lm(InvKM ~ scale(moneyMatchGoalNow)*PowerCond, moneyMatch))
confint(lm(InvKM ~ scale(moneyMatchGoalNow)*PowerCond, moneyMatch))

# Save for future
hist(moneyMatch$moneyMatchGoalFuture)
independentSamplesTTest(moneyMatchGoalFuture ~ PowerCond, moneyMatch)
summary(lm(InvKM ~ scale(moneyMatchGoalFuture)*PowerCond, moneyMatch))
confint(lm(InvKM ~ scale(moneyMatchGoalFuture)*PowerCond, moneyMatch))

# Difference in moneyMatch goals
moneyMatch$moneyMatchGoalDiff = with(moneyMatch, moneyMatchGoalFuture - moneyMatchGoalNow)
hist(moneyMatch$moneyMatchGoalDiff)
independentSamplesTTest(moneyMatchGoalDiff ~ PowerCond, moneyMatch)
summary(lm(InvKM ~ scale(moneyMatchGoalDiff)*PowerCond, moneyMatch))
confint(lm(InvKM ~ scale(moneyMatchGoalDiff)*PowerCond, moneyMatch))


# SOP ---------------------------------------------------------------------

summary(lm(InvKM ~ scale(SOP)*PowerCond, moneyMatch))
confint(lm(InvKM ~ scale(SOP)*PowerCond, moneyMatch))

## Gender ####
summary(aov(InvKM ~ Gender*PowerCond, moneyMatch))
etaSquared(aov(InvKM ~ Gender*PowerCond, moneyMatch))
independentSamplesTTest(InvKM ~ Gender, moneyMatch,var.equal = T) 


## Trimming by 3IQR ####

IQRupper = median(moneyMatch$KM,na.rm=T)+3*IQR(moneyMatch$KM,na.rm=T)
IQRlower = median(moneyMatch$KM,na.rm=T)-3*IQR(moneyMatch$KM,na.rm=T)

for (i in 1:nrow(moneyMatch)){
  if(!is.na(moneyMatch$KM[i]) & (moneyMatch$KM[i] > IQRupper|moneyMatch$KM[i] < IQRlower)){moneyMatch$KM3IQR[i] = NA}
  else {moneyMatch$KM3IQR[i] = moneyMatch$KM[i]}
}

sum(!is.na(moneyMatch$KM3IQR))# 281
sum(!is.na(moneyMatch$KM))-sum(!is.na(moneyMatch$KM3IQR)) 
hist(moneyMatch$KM3IQR)

skewness(moneyMatch$KM3IQR)
kurtosis(moneyMatch$KM3IQR)


independentSamplesTTest(KM3IQR ~ PowerCond, moneyMatch,var.equal = T) # nothing 
summary(lm(KM3IQR ~ SOP,moneyMatch)) # ***

## KM3IQR Moderator ####
## Legitimacy ####
hist(moneyMatch$Legitimate)
mean(moneyMatch$Legitimate)

independentSamplesTTest(Legitimate ~ PowerCond, moneyMatch, var.equal=T) #n.s. 

summary(lm(KM3IQR ~ PowerCond*scale(Legitimate),moneyMatch))#**
confint(lm(KM3IQR ~ PowerCond*scale(Legitimate),moneyMatch))
## simple slope ## exploratory
legit<-lmres(KM3IQR~PowerCond*Legitimate, centered=c('Legitimate'),data = moneyMatch)
legit.slope<-simpleSlope(legit,pred="PowerCond",mod1="Legitimate")
summary(legit.slope) 
# low legitimacy, high power discount less
# high legitimacy, low power discount less


# graph #
ggplot(data = moneyMatch, aes(x=Legitimate, y = KM3IQR, color = PowerCond))+geom_smooth(method = lm, se=F)


# Why? Exploratory
summary(lm(Connection ~ PowerCond*Legitimate,moneyMatch))#n.s.
summary(lm(Manipulation.Check_2 ~ PowerCond*Legitimate,moneyMatch))#*


manick<-lmres(Manipulation.Check_2~PowerCond*Legitimate, centered=c('Legitimate'),data = moneyMatch)
manick.slope<-simpleSlope(manick,pred="PowerCond",mod1="Legitimate")
summary(manick.slope) 
# the effect of power is stronger for high legitimacy condition
# but manipulation check does not predict discount rate...
summary(lm(KM3IQR~ Manipulation.Check_2,moneyMatch))
summary(lm(KM3IQR~ Manipulation.Check_2*Legitimate,moneyMatch))


# Experience with tasks ---------------------------------------------------
# NVirtual
names(moneyMatch)[57] = 'NVirtual'
names(moneyMatch)[58] = 'NDiscount'
hist(moneyMatch$NVirtual)
sort(moneyMatch$NVirtual)

sum(is.na(moneyMatch$NVirtual))# 1
sum(is.na(moneyMatch$NDiscount))# 1

mean(moneyMatch$NVirtual,na.rm=T)
sd(moneyMatch$NVirtual,na.rm=T)


summary(lm(KM3IQR ~ PowerCond*scale(NVirtual),moneyMatch))
confint(lm(KM3IQR ~ PowerCond*scale(NVirtual),moneyMatch))

## Dichotomized 
table(moneyMatch$NVirtual == 0)
table(moneyMatch$NVirtual == 0)/sum(!is.na(moneyMatch$NVirtual))

moneyMatch$RecallY = moneyMatch$NVirtual != 0
summary(aov(KM3IQR ~ PowerCond*RecallY,moneyMatch)) 
etaSquared(aov(KM3IQR ~ PowerCond*RecallY,moneyMatch))

moneyMatch %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KM3IQR), sd.K = sd(KM3IQR))


## NDiscount 
hist(moneyMatch$NDiscount)
mean(moneyMatch$NDiscount,na.rm=T)
sd(moneyMatch$NDiscount,na.rm=T)

summary(lm(KM3IQR ~ PowerCond*scale(NDiscount),moneyMatch))
confint(lm(KM3IQR ~ PowerCond*scale(NDiscount),moneyMatch))

table(moneyMatch$NDiscount== 0)
table(moneyMatch$NDiscount== 0)/sum(!is.na(moneyMatch$NDiscount))



## Dichotomized 
table(moneyMatch$NDiscount== 0)
table(moneyMatch$NDiscount== 0)/sum(!is.na(moneyMatch$NDiscount))

moneyMatch$DiscountY = moneyMatch$NDiscount != 0
summary(aov(KM3IQR ~ PowerCond*DiscountY,moneyMatch)) 
etaSquared(aov(KM3IQR ~ PowerCond*DiscountY,moneyMatch))

moneyMatch %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KM3IQR), sd.K = sd(KM3IQR))


summary(lm(KM3IQR ~ PowerCond*NDiscount,moneyMatch))
confint(lm(KM3IQR ~ PowerCond*NDiscount,moneyMatch))


# SES ---------------------------------------------------------------------
## objective SES
summary(lm(KM3IQR ~ scale(ObjectiveSES)*PowerCond, moneyMatch))
confint(lm(KM3IQR ~ scale(ObjectiveSES)*PowerCond, moneyMatch))

# Subjective
hist(moneyMatch$Ladder)
summary(lm(KM3IQR ~ scale(Ladder)*PowerCond, moneyMatch)) #n.s.
confint(lm(KM3IQR ~ scale(Ladder)*PowerCond, moneyMatch))
# Goals related to moneyMatch --------------------------------------------------

# earn moneyMatch now
hist(moneyMatch$moneyMatchGoalNow)
independentSamplesTTest(moneyMatchGoalNow ~ PowerCond, moneyMatch)
summary(lm(KM3IQR ~ scale(moneyMatchGoalNow)*PowerCond, moneyMatch))
confint(lm(KM3IQR ~ scale(moneyMatchGoalNow)*PowerCond, moneyMatch))

# Save for future
hist(moneyMatch$moneyMatchGoalFuture)
independentSamplesTTest(moneyMatchGoalFuture ~ PowerCond, moneyMatch)
summary(lm(KM3IQR ~ scale(moneyMatchGoalFuture)*PowerCond, moneyMatch))
confint(lm(KM3IQR ~ scale(moneyMatchGoalFuture)*PowerCond, moneyMatch))

# Difference in moneyMatch goals
moneyMatch$moneyMatchGoalDiff = with(moneyMatch, moneyMatchGoalFuture - moneyMatchGoalNow)
hist(moneyMatch$moneyMatchGoalDiff)
independentSamplesTTest(moneyMatchGoalDiff ~ PowerCond, moneyMatch)
summary(lm(KM3IQR ~ scale(moneyMatchGoalDiff)*PowerCond, moneyMatch))
confint(lm(KM3IQR ~ scale(moneyMatchGoalDiff)*PowerCond, moneyMatch))


# SOP ---------------------------------------------------------------------

summary(lm(KM3IQR ~ scale(SOP)*PowerCond, moneyMatch))
confint(lm(KM3IQR ~ scale(SOP)*PowerCond, moneyMatch))

## Gender ####
summary(aov(KM3IQR ~ Gender*PowerCond, moneyMatch))
etaSquared(aov(KM3IQR ~ Gender*PowerCond, moneyMatch))
independentSamplesTTest(KM3IQR ~ Gender, moneyMatch,var.equal = T) 



## Titration only discount rate analyses ####
moneytitration = money


# Calculate discount rate titration only -------------------------------------------------

## Titration ##
## Indifference Point ##
AllTrue<- function(vec){
  for (i in 1: length(vec)){
    if (vec[i] == FALSE){
      return(FALSE)
    }
  }
  return(TRUE)
}

names(moneytitration[i,28:36])

maxout<- c()                                                ## record participants who max out 
nodiscount<-c()                                             ## record participants who prefer $120 in a year to $120 today
inconsistent <-c()                                          ## record inconsistent participants


for(i in 1: nrow(moneytitration)){
  if (AllTrue(moneytitration[i,28:36] == 1)){                           ##max out the titration scale
    maxout = append(maxout,i)
    moneytitration$Indiff[i] = 257
  }
  else if(AllTrue(moneytitration[i,29:36] == 2)){                       ##No discount
    nodiscount = append(nodiscount,i)
    moneytitration$Indiff[i] = NA
  }
  else if (moneytitration[i,28] == 2){
    moneytitration$Indiff[i] = NA
    negativediscount = append(negativediscount, i)
  }
  else if (AllTrue(moneytitration[i,28:29] == 1) && AllTrue(moneytitration[i, 30:36] == 2)){   ## From here on, regular titration 
    moneytitration$Indiff[i] = 137                               ## Indiff estimate = half way at flip
  }
  else if (AllTrue(moneytitration[i,28:30] == 1) && AllTrue(moneytitration[i, 31:36] == 2)){
    moneytitration$Indiff[i] = 154
  }
  else if (AllTrue(moneytitration[i,28:31] == 1) && AllTrue(moneytitration[i, 32:36] == 2)){
    moneytitration$Indiff[i] = 171
  }
  else if (AllTrue(moneytitration[i,28:32] == 1) && AllTrue(moneytitration[i, 33:36]== 2)){
    moneytitration$Indiff[i] = 189
  }
  else if (AllTrue(moneytitration[i,28:33] == 1) && AllTrue(moneytitration[i, 34:36] == 2)){
    moneytitration$Indiff[i] = 206
  }
  else if (AllTrue(moneytitration[i,28:34] == 1) && AllTrue(moneytitration[i, 35:36] == 2)){
    moneytitration$Indiff[i] = 223
  }
  else if (AllTrue(moneytitration[i,28:35] == 1) && AllTrue(moneytitration[i, 36] == 2)){
    moneytitration$Indiff[i] = 240
  }
  else {
    moneytitration$Indiff[i] = NA
    inconsistent = append(inconsistent,i)
  }
}


length(inconsistent) # 5
table(moneytitration[inconsistent,]$PowerCond)

length(nodiscount) #12
table(moneytitration[c(nodiscount),]$PowerCond)

table(moneytitration$PowerCond)
chisq.test(cbind(table(moneytitration[c(nodiscount),]$PowerCond),
                 table(moneytitration[c(-nodiscount),]$PowerCond)))

length(maxout) #34
table(moneytitration[maxout,]$PowerCond)  

chisq.test(cbind (table(moneytitration[maxout,]$PowerCond) , table(moneytitration[-maxout,]$PowerCond) ))
nrow(moneytitration)#385


sum(!is.na(moneytitration$Indiff)) # 368

## Titration only discount rate calculation ####
moneytitration$KT = with(moneytitration, Indiff/120 - 1)
save(moneytitration,file = 'moneytitration.rdata')

## Titration only Discount rate anlaysis ##
independentSamplesTTest(KT ~ PowerCond, moneytitration, var.equal =T)
with(moneytitration,cohen.d(KT,PowerCond,na.rm=T))
# KT Moderators --------------------------------------------------------------

## Legitimacy ####
hist(moneytitration$Legitimate)
mean(moneytitration$Legitimate)


independentSamplesTTest(Legitimate ~ PowerCond, moneytitration, var.equal=T) #n.s. 

summary(lm(KT ~ PowerCond*scale(Legitimate),moneytitration))
confint(lm(KT ~ PowerCond*scale(Legitimate),moneytitration))#**

## simple slope ## exploratory
legit<-lmres(KT~PowerCond*Legitimate, centered=c('Legitimate'),data = moneytitration)
legit.slope<-simpleSlope(legit,pred="PowerCond",mod1="Legitimate")
summary(legit.slope) 
# low legitimacy, high power discount less
# high legitimacy, low power discount less


# Experience with tasks ---------------------------------------------------
# NVirtual
names(moneytitration)[57] = 'NVirtual'
names(moneytitration)[58] = 'NDiscount'
hist(moneytitration$NVirtual)
sort(moneytitration$NVirtual)

sum(is.na(moneytitration$NVirtual))# 1
sum(is.na(moneytitration$NDiscount))# 1

mean(moneytitration$NVirtual,na.rm=T)
sd(moneytitration$NVirtual,na.rm=T)


summary(lm(KT ~ PowerCond*scale(NVirtual),moneytitration))
confint(lm(KT ~ PowerCond*scale(NVirtual),moneytitration))

## Dichotomized 
table(moneytitration$NVirtual == 0)
table(moneytitration$NVirtual == 0)/sum(!is.na(moneytitration$NVirtual))

moneytitration$RecallY = moneytitration$NVirtual != 0
summary(aov(KT ~ PowerCond*RecallY,moneytitration)) 
etaSquared(aov(KT ~ PowerCond*RecallY,moneytitration))

moneytitration %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KT), sd.K = sd(KT))


## NDiscount 
hist(moneytitration$NDiscount)
mean(moneytitration$NDiscount,na.rm=T)
sd(moneytitration$NDiscount,na.rm=T)

summary(lm(KT ~ PowerCond*scale(NDiscount),moneytitration))
confint(lm(KT ~ PowerCond*scale(NDiscount),moneytitration))

table(moneytitration$NDiscount== 0)
table(moneytitration$NDiscount== 0)/sum(!is.na(moneytitration$NDiscount))



## Dichotomized 
table(moneytitration$NDiscount== 0)
table(moneytitration$NDiscount== 0)/sum(!is.na(moneytitration$NDiscount))

moneytitration$DiscountY = moneytitration$NDiscount != 0
summary(aov(KT ~ PowerCond*DiscountY,moneytitration)) 
etaSquared(aov(KT ~ PowerCond*DiscountY,moneytitration))

moneytitration %>% 
  group_by(RecallY)%>%
  summarize(m.K = mean(KT), sd.K = sd(KT))


summary(lm(KT ~ PowerCond*NDiscount,moneytitration))
confint(lm(KT ~ PowerCond*NDiscount,moneytitration))


# SES ---------------------------------------------------------------------
## objective SES

hist(moneytitration$Education_2)
hist(moneytitration$Education_3)
moneytitration$Paternal = scale(moneytitration$Education_2)
moneytitration$Maternal = scale(moneytitration$Education_3)
mean(moneytitration$Education_2)
sd(moneytitration$Education_2)
mean(moneytitration$Education_3)
sd(moneytitration$Education_3)

hist(moneytitration$Income_2)
mean(moneytitration$Income_2)
sd(moneytitration$Income_2)
moneytitration$Income = scale(moneytitration$Income_2)

moneytitration$ObjectiveSES = with(moneytitration, Paternal+Maternal+Income)
hist(moneytitration$ObjectiveSES)

summary(lm(KT ~ scale(ObjectiveSES)*PowerCond, moneytitration))
confint(lm(KT ~ scale(ObjectiveSES)*PowerCond, moneytitration))

# Subjective
hist(moneytitration$Ladder)
summary(lm(KT ~ scale(Ladder)*PowerCond, moneytitration))
confint(lm(KT ~ scale(Ladder)*PowerCond, moneytitration))
# Goals related to moneytitration --------------------------------------------------

# earn moneytitration now
hist(moneytitration$MoneyGoalNow)
independentSamplesTTest(MoneyGoalNow ~ PowerCond, moneytitration)
summary(lm(KT ~ scale(MoneyGoalNow)*PowerCond, moneytitration))
confint(lm(KT ~ scale(MoneyGoalNow)*PowerCond, moneytitration))

# Save for future
hist(moneytitration$MoneyGoalFuture)
independentSamplesTTest(MoneyGoalFuture ~ PowerCond, moneytitration)
summary(lm(KT ~ scale(MoneyGoalFuture)*PowerCond, moneytitration))
confint(lm(KT ~ scale(MoneyGoalFuture)*PowerCond, moneytitration))

# no interactoin, not even main effects
moneytitration$MoneyGoalDiff = with(moneytitration, MoneyGoalFuture - MoneyGoalNow)
hist(moneytitration$MoneyGoalDiff)
independentSamplesTTest(MoneyGoalDiff ~ PowerCond, moneytitration)
summary(lm(KT ~ scale(MoneyGoalDiff)*PowerCond, moneytitration))
confint(lm(KT ~ scale(MoneyGoalDiff)*PowerCond, moneytitration))


# SOP ---------------------------------------------------------------------

summary(lm(KT ~ scale(SOP)*PowerCond, moneytitration))
confint(lm(KT ~ scale(SOP)*PowerCond, moneytitration))

summary(lm(KT ~ scale(SOP), moneytitration))

## Gender ####
summary(aov(KT ~ Gender*PowerCond, moneytitration))
etaSquared(aov(KT ~ Gender*PowerCond, moneytitration))
independentSamplesTTest(KT ~ Gender, moneytitration,var.equal = T) 







