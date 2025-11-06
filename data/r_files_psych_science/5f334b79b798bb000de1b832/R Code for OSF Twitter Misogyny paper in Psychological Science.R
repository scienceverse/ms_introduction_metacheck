setwd(choose.dir())

data<-read.csv("tom1.csv", header = T)

data2013<-subset(data,YEAR=='2013')
data2014<-subset(data,YEAR=='2014')
write.csv(data2013, file="data2013.csv")
write.csv(data2014, file="data2014.csv")

d1<-aggregate.data.frame(data2013, by=list(data2013$cbsa), FUN = mean) #averaged values for cbsas with more than 1 value
state<-read.csv("cbsa_state_region.csv", header = T)
state<-data.frame(state)
d1$State_Name <- state$State_Name[match(d1$cbsa,state$cbsa)] #replace name and region as they were deleted during aggregation
d1$region <- state$region[match(d1$cbsa,state$cbsa)]

d2<-aggregate.data.frame(data2014, by=list(data2014$cbsa), FUN = mean) #averaged values for cbsas with more than 1 value
state<-read.csv("cbsa_state_region.csv", header = T)
state<-data.frame(state)
d2$State_Name <- state$State_Name[match(d2$cbsa,state$cbsa)]
d2$region <- state$region[match(d2$cbsa,state$cbsa)]

data<-rbind(d1,d2)

crosslag<-read.csv("crosslag.csv", header = T)

#Descriptive statistics for Table 3
install.packages("psych")
library(psych)
describe(crosslag$OCCUR.x)
describe(crosslag$OCCUR.y)
describe(crosslag$cbsaTweets2.x)
describe(crosslag$cbsaTweets2.y)
describe(crosslag$pop.x)
describe(crosslag$pop.y)
describe(crosslag$alcohol.x)
describe(crosslag$alcohol.y)
describe(crosslag$white.x)
describe(crosslag$white.y)
describe(crosslag$GI.edu.x)
describe(crosslag$GI.edu.y)
describe(crosslag$GI.empl.x)
describe(crosslag$GI.empl.y)
describe(crosslag$GI.health.x)
describe(crosslag$GI.health.y)
describe(crosslag$GI.income.x)
describe(crosslag$GI.income.y)
describe(crosslag$gini.x)
describe(crosslag$gini.y)
describe(crosslag$eduCollege.x)
describe(crosslag$eduCollege.y)
describe(crosslag$medianAge.x)
describe(crosslag$medianAge.y)
describe(crosslag$medianIncome.x)
describe(crosslag$medianIncome.y)

#Log and Z-score transform
data$logalc<-log(data$alcohol)
data$logpop<-log(data$pop)
data$zGI.edu<-scale(data$GI.edu, center = TRUE, scale = TRUE)
data$zGI.empl<-scale(data$GI.empl, center = TRUE, scale = TRUE)
data$zGI.income<-scale(data$GI.income, center = TRUE, scale = TRUE)
data$zGI.health<-scale(data$GI.health, center = TRUE, scale = TRUE)
data$zeduCollege<-scale(data$eduCollege, center = TRUE, scale = TRUE)
data$zwhite<-scale(data$white, center = TRUE, scale = TRUE)
data$zlogalc<-scale(data$logalc, center = TRUE, scale = TRUE)
data$zcbsaTweets2<-scale(data$cbsaTweets2, center = TRUE, scale = TRUE)
data$zlogpop<-scale(data$logpop, center = TRUE, scale = TRUE)
data$zgini<-scale(data$gini, center = TRUE, scale = TRUE)
data$zmedianAge<-scale(data$medianAge, center = TRUE, scale = TRUE)
data$zmedianIncome<-scale(data$medianIncome, center = TRUE, scale = TRUE)

data$zOCCUR<-scale(data$OCCUR, center = TRUE, scale = TRUE)

write.csv(data,file="zdata.csv") #for use in cross-lagged model below


####Correlations###########
cor.test(data$OCCUR,data$cbsaTweets2)
cor.test(data$OCCUR,data$logalc)
cor.test(data$OCCUR,data$logpop)
cor.test(data$OCCUR,data$eduCollege)
cor.test(data$OCCUR,data$white)
cor.test(data$OCCUR,data$GI.edu)
cor.test(data$OCCUR,data$medianIncome)

cor.test(data$cbsaTweets2,data$logalc)
cor.test(data$cbsaTweets2,data$white)

install.packages("apaTables")
library(apaTables)
df<-cbind(data$OCCUR, data$cbsaTweets2, data$logpop, data$logalc, data$GI.edu, data$GI.empl, 
          data$GI.health, data$GI.income, data$gini,data$eduCollege, data$white, data$medianAge,data$medianIncome)
apa.cor.table(df,show.conf.interval = TRUE, filename = "cortable.doc")

hist(data$pop)
hist(data$logpop)
hist(data$medianAge)
hist(data$medianIncome)
hist(data$alcohol)
hist(data$logalc)
hist(data$gini)
hist(data$white)
hist(data$eduCollege)
hist(data$GI.edu)
hist(data$GI.income)
hist(data$GI.empl)
hist(data$GI.health)


#find number of cbsas with 0 DV cases
#not zero-inflated
zeroes2013<-dplyr::arrange(data2013, OCCUR)
print(zeroes2013) #33 zeroes
33/499
zeroes2014<-dplyr::arrange(data2014, OCCUR)
print(zeroes2014) #35 zeroes
35/512

#regression model for reviewer who wanted a standardized domestic violence variable
m3<-lm(zOCCUR~zcbsaTweets2 + region + zlogpop + zlogalc + zwhite + zGI.edu + zGI.empl + zGI.health + zGI.income
       + zgini + zeduCollege + zmedianAge + zmedianIncome, data=data)
summary(m3)
library(car)
vif(m3) # get VIFs

#Full model
install.packages("MASS")
library(MASS)

#check for overdispersion
var(data$OCCUR, na.rm = T)
mean(data$OCCUR, na.rm = T)

#FINAL MODEL
m1<-glm.nb(OCCUR~zcbsaTweets2 + region + zlogpop + zlogalc + zwhite + zGI.edu + zGI.empl + zGI.health + zGI.income
           + zgini + zeduCollege + zmedianAge + zmedianIncome, data=data, maxit=100, init.theta = 1.032713156)
summary(m1)
#Confidence intervals for table
(est <- cbind(Estimate = coef(m1), confint(m1)))

#FOR ZERO-INFLATED: Khandis
#predictors behind the line are the variables thought to cause the zeroes in the data (not RFX).
m2<-pscl::zeroinfl(OCCUR~zcbsaTweets2 + region + zlogpop + zlogalc + zwhite + zGI.edu + zGI.empl + zGI.health + 
                     +              zGI.income + zgini + zeduCollege + zmedianAge + zmedianIncome | 1 + region + zlogpop, data=data)
summary(m2)

AIC(m2)

#Vuong's test shows us that the nb distribution (model m1)
#is a superior fit to zero-inflated model.
pscl::vuong(m1, m2) 

#POISSON
m3<-glm(OCCUR~zcbsaTweets2 + region + zlogpop + zlogalc + zwhite + zGI.edu + zGI.empl + zGI.health + zGI.income +
           zgini + zeduCollege + zmedianAge + zmedianIncome, data=data, family="poisson")

#Overdispersion test
install.packages("AER")
library(AER)
AER::dispersiontest(m3)
#Poisson is over-dispersed, so inappropriate. ALSO:
AIC(m1, m3)
#Poisson model demonstrates an inferior fit to m1. anova() test wont work because the models are different classes.


#==============================================================
######cross lagged analyses################
zdata<-read.csv("zdata.csv", header = T)
data2013<-subset(zdata,YEAR=="2013")
data2014<-subset(zdata,YEAR=="2014")

crosslag<-merge(data2013,data2014, by="cbsa")
write.csv(crosslag, file="crosslag.csv")
crosslag<-read.csv("crosslag.csv", header = T)

install.packages("lavaan")
library(lavaan)

#create dummy variables for region
install.packages("plyr")
library(plyr)
crosslag$region.x<-as.factor(crosslag$region.x)
crosslag$south <- revalue(crosslag$region.x, c("midwest"="0", "south" = "1", "northeast" = "0", "west" = "0"))
crosslag$northeast <- revalue(crosslag$region.x, c("midwest"="0", "south" = "0", "northeast" = "1", "west" = "0"))
crosslag$west <- revalue(crosslag$region.x, c("midwest"="0", "south" = "0", "northeast" = "0", "west" = "1"))

#standardize zOCCUR because we cannot do negative binomial regression with lavaan package
crosslag$zOCCUR.y <- scale(crosslag$OCCUR.y)
crosslag$zOCCUR.x <- scale(crosslag$OCCUR.x)


#PATH MODEL

#specify regressions
m0 <- 'zOCCUR.y ~ zOCCUR.x + zcbsaTweets2.x + zlogpop.x + zlogalc.x + zwhite.x 
          + zGI.edu.x + zGI.empl.x + zGI.health.x + zGI.income.x
           + zgini.x + zeduCollege.x + zmedianAge.x + zmedianIncome.x
           + south + northeast + west
      zcbsaTweets2.y ~ zOCCUR.x + zcbsaTweets2.x + zlogpop.x + zlogalc.x + zwhite.x 
+ zGI.edu.x + zGI.empl.x + zGI.health.x + zGI.income.x
+ zgini.x + zeduCollege.x + zmedianAge.x + zmedianIncome.x
+ south + northeast + west'


fit<-sem(m0, data=crosslag, estimator="MLR")
varTable(fit)               
hist(crosslag$zOCCUR.y)
hist(crosslag$zOCCUR.x)
summary(fit)
summary(fit, standardized=F, fit.measures=T, rsq=T)


###Editor asked for test of unique variance for tweets
#model without tweets
m1<-glm.nb(OCCUR.y ~ zOCCUR.x + zlogpop.x + zlogalc.x + zwhite.x 
           + zGI.edu.x + zGI.empl.x + zGI.health.x + zGI.income.x
           + zgini.x + zeduCollege.x + zmedianAge.x + zmedianIncome.x
           + south + northeast + west, data=crosslag, maxit=100, init.theta = 1.032713156)
summary(m1)
#Confidence intervals for table
(est <- cbind(Estimate = coef(m1), confint(m1)))

m2<-glm.nb(OCCUR.y ~ zOCCUR.x + zcbsaTweets2.x + zlogpop.x + zlogalc.x + zwhite.x 
           + zGI.edu.x + zGI.empl.x + zGI.health.x + zGI.income.x
           + zgini.x + zeduCollege.x + zmedianAge.x + zmedianIncome.x
           + south + northeast + west, data=crosslag, maxit=100, init.theta = 1.032713156)
summary(m2)
#Confidence intervals for table
(est <- cbind(Estimate = coef(m2), confint(m2)))

anova(m1,m2) #model with Tweets is a better fit.



