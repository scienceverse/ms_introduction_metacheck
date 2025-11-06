library(car)
library(MASS)

datap = read.csv('data_prepped.csv')

#code gender and conditions as factors
datap$sex = as.factor(datap$sex)
datap$condition = as.factor(datap$condition)

#males and females count
gentable = table(datap$sex)
gentable

#ethnicity count
sum(datap$ethnicity_1, na.rm = TRUE) #33
sum(datap$ethnicity_2, na.rm = TRUE) #11
sum(datap$ethnicity_3, na.rm = TRUE) #12
sum(datap$ethnicity_4, na.rm = TRUE) #2
sum(datap$ethnicity_5, na.rm = TRUE) #82
sum(datap$ethnicity_6, na.rm = TRUE) #1
sum(datap$ethnicity_7, na.rm = TRUE) #2

#age
datap$age = as.numeric(as.character(datap$age))
mean(datap$age); sd(datap$age)

#contrast code in order to get correct ANOVA results (this is something we learned in psych 252)
contrasts(datap$sex) = c(-1,1)
contrasts(datap$condition) = c(-1,1)
#contrasts(datap$condition) = c(0,1)

#checking out vengefulness by condition and gender data
table = aggregate(datap$vengefulness, by=list(datap$condition,datap$sex),FUN=mean)
table

#just checking male vs. females
table2 = aggregate(datap$vengefulness, by=list(datap$sex),FUN=mean)
table2
table2sd = aggregate(datap$vengefulness, by=list(datap$sex),FUN=sd)
table2sd
table3 = aggregate(datap$TRIMscore, by=list(datap$condition), FUN=mean)
table3
table3sd = aggregate(datap$TRIMscore, by=list(datap$condition), FUN=sd)
table3sd

newTable = matrix(table$x,nrow=2,ncol=2)
newTable

x = barplot(newTable,beside = TRUE, names.arg = c("male","female"))

resA = lm(datap$vengefulness~datap$condition)
summary(resA)

#Two ways to test the ANOVA of condition x gender on vengefulness
res1 = lm(datap$vengefulness~datap$condition*datap$sex)
summary(res1)

res1.1 = Anova(lm(datap$vengefulness~datap$condition*datap$sex),type="III")
(res1.1)

##exploratory

#testing whether overall effect worked without gender
res1.1 = lm(datap$TRIMscore~datap$condition)
summary(res1.1)

#testing TRIM score interaction of gender x condition
Anova(lm(datap$TRIMscore~datap$condition*datap$sex), type="III")
res1.2 = lm(datap$TRIMscore~datap$condition*datap$sex)
summary(res1.2)

#testing gender effects on capability and empathy
t.test(datap$cap~datap$sex)
t.test(datap$empathy~datap$sex)

#making subsets of capability asked before or after
capBefore = subset(datap, condition == "1", select = c(vengefulness,cap,empathy,TRIMscore))
capAfter = subset(datap, condition == "2", select = c(vengefulness,cap,empathy,TRIMscore))

#testing correlation of capability and vengefulness both before and after
cor.test(capBefore$vengefulness,capBefore$cap)
cor.test(capAfter$vengefulness,capAfter$cap)

#testing the interaction
res3 = lm(datap$vengefulness~datap$condition*datap$cap)
summary(res3)

#correlation of overall vengefulness and personal capability
cor.test(datap$vengefulness,datap$cap)
  
#correlations of TRIM and personal cap
cor.test(datap$TRIMscore,datap$cap)
cor.test(capBefore$TRIMscore,capBefore$cap)
cor.test(capAfter$TRIMscore,capAfter$cap)
res4 = lm(datap$TRIMscore~datap$condition*datap$cap)
summary(res4)

#exploring positive correlation between vengefulness and personal capability...
library(lattice)
xyplot(datap$vengefulness~datap$cap|datap$condition, auto.key=TRUE)

#correlations of vengefulness and empathy
cor.test(capBefore$vengefulness,capBefore$empathy)
cor.test(capAfter$vengefulness,capAfter$empathy)
cor.test(datap$vengefulness,datap$empathy)
res5 = lm(datap$vengefulness~datap$condition*datap$empathy) #testing the interaction
summary(res5)

cor.test(datap$TRIMscore,datap$empathy)
cor.test(capBefore$TRIMscore,capBefore$empathy)
cor.test(capAfter$TRIMscore,capAfter$empathy)
res6 = lm(datap$TRIMscore~datap$condition*datap$empathy)
summary(res6)
##ethnicity chi-square

obs = c(33, 84, 18)
exp = c(.79, .16, .05) #original 

chisq.test(obs, p=exp, rescale.p=T, simulate.p.value=F)