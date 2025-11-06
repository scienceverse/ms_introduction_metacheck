#Code: DoG Experiment 2 (Doebel & Munakata, in press, Psychological Science)

install.packages("survival")
library(survival)
install.packages("psych")
library(psych)
install.packages("pastecs")
library(pastecs)
install.packages("reshape2")
library(reshape2)
install.packages("lme4")
library(lme4)

getwd()
setwd()
setwd("/Users/doebe002/Desktop/DoGV2/DoG2_DataAndAnalyses")
dogdf<-read.table(file="dogdata2",sep=",", header=T)
d<-dogdf 

d$wait.nowait<- -1/2*(d$cond=="Waited")+1/2*(d$cond=="Didn'tWait")

#descriptives

tapply(d$mintrunc,d$cond,median, na.rm=T)
tapply(d$mintrunc,d$cond,mean, na.rm=T)

table(d$cond, d$status)

mean(d$age, na.rm=T)
sum((d$gender=="m")*1)

#ingroup behavior (did children remember what their group did, did this vary by condition and time)
mean(d$ingroupbehmem1, na.rm=T)
mean(d$ingroupbehmem2, na.rm=T)
sd(d$ingroupbehmem1, na.rm=T)
sd(d$ingroupbehmem2, na.rm=T)

#frequency data

tapply(d$ingroupbehmem1==1, d$cond, sum, na.rm=TRUE)
tapply(d$ingroupbehmem1==0, d$cond, sum, na.rm=TRUE)
tapply(d$ingroupbehmem2==1, d$cond, sum, na.rm=TRUE)
tapply(d$ingroupbehmem2==0, d$cond, sum, na.rm=TRUE)

#compare to chance

d$ingroupbehmem1.chance<-d$ingroupbehmem1-.5
d$ingroupbehmem2.chance<-d$ingroupbehmem2-.5
summary(lm(ingroupbehmem1.chance ~ 1, d))
summary(lm(ingroupbehmem2.chance ~ 1, d))

#Survival analysis

#d<-subset(dogdf2, condition !="") #to get rid of empty cells in csv file
#to get medians in presence of censoring, need to estimate KM survival curves and obtain est survival rate at 50th percentile
kmmod<-survfit(Surv(mintrunc, status) ~ cond, type="kaplan-meier", data=d)
summary(kmmod)
survdiff(Surv(mintrunc, status) ~ cond, data=d)#tests diff btw the two km survival fxs
quantile(kmmod, quantiles=c(.25, .5, .75)) #can't obtain median for delay bc at no point did surv fall below .50 (d/t censoring)

barplot(table(d$cond, d$status), beside=T,
        legend=c("waited", "didn't wait"), ylim=c(0,15))

#To use cox regression instead of KM (preferable) need to check if PH assumption is met (first estimate model then check):
coxmod<-coxph(Surv(d$mintrunc, d$status) ~ d$wait.nowait)
summary(coxmod) #CONFIRMATORY TEST REPORTED IN ANALYSIS

#exploratory analysis controlling for age
coxmod<-coxph(Surv(d$mintrunc, d$status) ~ d$wait.nowait + d$age)
summary(coxmod)
  
#test for violation of PH assumption:
cox.zph(coxmod) 

#exploratory analysis that includes responses to pre-test group identity question as a covariate (as requested by
#a reviewer)

coxmod<-coxph(Surv(d$mintrunc, d$status) ~ d$wait.nowait*d$groupident1)
summary(coxmod)


# To test whether condition influenced children’s evaluation of delaying gratification, we planned to use 
# linear regression to compare a model with condition (in-group-waited vs. in-group-didn't-wait) as the only
# predictor to a model without any predictors. 

#first create a score for children's evaluations
str(d)
d$eval.score<-rowMeans(subset(d, select=c(eval1,eval2,eval3,eval4,eval5,eval6,eval7,eval8,eval9,eval10,eval11,eval12)), na.rm=T)

modelc<-lm(eval.score ~ 1, d)
modela<-lm(eval.score ~ wait.nowait, d)
anova(modelc, modela)
summary(modela)

t.test(eval.score ~ cond,d)

#means by group

tapply(d$eval.score,d$cond,mean, na.rm=T)
tapply(d$eval.score,d$cond,sd, na.rm=T)

#effect of order? no
t.test(eval.score ~ evalorder,d)

#bar graph

df<-subset(d, select=c(cond, eval.score));df
stat.desc(subset(df, cond =="Waited"))
stat.desc(subset(df, cond =="Didn'tWait"))

Condition<-c("In-group waited","In-group didn't wait")
condmean<-c(.73,.53) #obtained from above
SEmean<-c(.029, 0.034) #same
condlowerCIs<--1.96*SEmean+condmean;condlowerCIs
condupperCIs<-1.96*SEmean+condmean;condupperCIs
df<-data.frame(Condition,condmean,SEmean, condlowerCIs, condupperCIs);df

ggplot(df, aes(x=Condition, y=condmean, fill=Condition)) +
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("blue", "red"))+
  geom_errorbar(aes(ymax=condupperCIs,
                    ymin=condlowerCIs),
                width=.0,                    # Width of the error bars
                position=position_dodge(.9))+
  ylab("Preference For Those Who Delay Gratification \n") +
  xlab("")+
  theme(legend.position="none")+
  theme(axis.title.x = element_text( size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y  = element_text(size=16),
        axis.text.y  = element_text(size=16),
        axis.ticks.x = element_line(size = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(color="black"),
        axis.line.x = element_line(color="black")) 


#To test whether in-group bias varied depending on condition, 
# we planned to use multiple linear regression to compare a model including condition (in-group-waited vs. 
# in-group-didn't-wait), time of group identity measurement (pre- vs. post marshmallow test) and their interaction, to a model excluding 
# the interaction term, with the expectation that the model including the interaction term would be a better 
# fit to the data.

d$ident.score.pre<-rowMeans(subset(d, select=c(groupident1,groupident2,groupident3,groupident4)), na.rm=T)
d$ident.score.post<-rowMeans(subset(d, select=c(groupident5,groupident6,groupident7,groupident8)), na.rm=T)

mean(d$ident.score.pre, na.rm=T)
mean(d$ident.score.post, na.rm=T)
sd(d$ident.score.pre, na.rm=T)
sd(d$ident.score.post, na.rm=T)

#compare to chance
d$ident.score.pre.chance<-d$ident.score.pre-.5
d$ident.score.post.chance<-d$ident.score.post-.5
summary(lm(ident.score.pre.chance ~ 1, d))
summary(lm(ident.score.post.chance ~ 1, d))

#partially melt data
d2<-(subset(d, select=c(id, ident.score.pre, ident.score.post,cond)));str(d2)

d2<-melt(d2, id=c("id","cond"))

d2$wait.nowait<- -1/2*(d$cond=="Waited")+1/2*(d$cond=="Didn'tWait")
d2

#testing interaction
modelc<-lmer(value ~ wait.nowait + variable + (1|id), d2, REML = F)
modela<-lmer(value ~ wait.nowait*variable + (1|id), d2, REML = F)
anova(modelc, modela)
summary(modela)

#testing condition
modelc<-lmer(value ~  variable + variable:wait.nowait + (1|id), d2, REML = F)
modela<-lmer(value ~ wait.nowait*variable + (1|id), d2, REML = F)
anova(modelc, modela)
summary(modela)
summary(modelc)

#testing time
modelc<-lmer(value ~ wait.nowait + variable:wait.nowait + (1|id), d2, REML = F)
modela<-lmer(value ~ wait.nowait*variable + (1|id), d2, REML = F)
anova(modelc, modela)
summary(modela)

tapply(d2$value,d2$cond,mean, na.rm=T)
tapply(d2$value,d2$cond,sd, na.rm=T)

d.wait<-subset(d2, d2$wait.nowait==-1/2);d.wait
d.wait$value.chance<-d.wait$value-.5
summary(lm(value.chance ~ 1, d.wait))

d.nwait<-subset(d2, d2$wait.nowait==1/2);d.nwait
d.nwait$value.chance<-d.nwait$value-.5
summary(lm(value.chance ~ 1, d.nwait))




