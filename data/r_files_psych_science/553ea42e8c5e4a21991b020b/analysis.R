### analaysis.R 

rm(list=ls())

# setworking directory
setwd("~/Dropbox/Psych 254 Wrapup/Lee & Kyung/")

# read in Final.Rdata
load("Final.Rdata")

## explore data
## correlation table
final$condi <- as.numeric(as.factor(final$condition))

imD <- subset(final, select=c("totalScore", "edu", "qual.duration", "vTotal", "mTotal", "gender", "condi", "enjoy","age"))
imD$edu <- as.numeric(imD$edu)

## correlatio table without vTotal, mTotal
imD2 <- subset(imD, select=c("totalScore", "qual.duration", "age", "condi", "gender", "edu", "enjoy"))
imD2$gender <- as.numeric(imD2$gender)
cor(imD2)

cor.test(final$enjoy, final$qual.duration, method = "pearson")
cor.test(final$edu, final$qual.duration, method = "pearson")
cor.test(final$enjoy, final$edu , method = "pearson")
cor.test(final$edu, final$totalScore , method = "pearson")
cor.test(final$enjoy, final$totalScore , method = "pearson")
cor.test(final$totalScore, final$qual.duration , method = "pearson")

cor.test(final$gender, final$enjoy , method = "pearson")
final$gender<-as.numeric(final$gender)

mode(final$edu)
## pivot table
aggregate(totalScore ~ gender + condition, data = final, mean)

## simple anova 
res1 = lm(final$totalScore ~ final$condition)
summary(res1)
anova(res1)
etasq(res1, anova=TRUE)

## get condition means
aggregate(totalScore ~ condition, data = final, mean)
## get condition SDs
aggregate(totalScore ~ condition, data = final, sd)

## drawing boxplot w. error bar
library(plotrix)

sem <- function (x) {sd(x) / sqrt(length(x))}

ms <- aggregate(totalScore ~ condition, data = final, mean)
es <- aggregate(totalScore ~ condition, data = final, sem )
ys  <- matrix(ms$totalScore, nrow=1, ncol=2, byrow=T)

yes <- matrix(es$totalScore, nrow=1, ncol=2, byrow=T)

xs <- barplot(ys, width = 0.4, beside=T, space = c(0, 0.2), names=c("Action","Inaction"),
              col = c("light grey", "dark grey"),               
              ylab="Number of Correctly Solved Problems", xlab="Condition", 
              xlim= c(0,1), yaxp=c(0,15,3), ylim=c(0,15))
plotCI(xs, ys, yes, add=T, pch=" ",xpd="n")

## boxplot
boxplot(totalScore ~ condition, data = final)

## USING LATTICE 
## 5. Explore the lattice package's xyplot command to see all the data at once
## plot total score by subjects, with condition as a group variable
library(lattice)
xyplot(totalScore ~ subid, group= condition, data=final, auto.key = T)

## ANOVA w. vTotal and mTotal as a seperate DV

summary(lm(final$vTotal ~ final$condition))
summary(lm(final$mTotal ~ final$condition))

anova(lm(final$vTotal ~ final$condition))
anova(lm(final$mTotal ~ final$condition))

aggregate(vTotal ~ condition, data = final, mean)
aggregate(mTotal ~ condition, data = final, mean)

### extra analysis
## ANOVA w. gender & condition as IVs 

res1= lm (totalScore ~ condition + gender + as.numeric(edu) + age + enjoy, data=final)
summary(res1)

anova(lm (totalScore ~ gender + condition, data=final))

anova(lm (totalScore ~ age , data=final))
aggregate(totalScore ~ age, data = final, mean)

anova(lm (totalScore ~ gender , data=final))
aggregate(totalScore ~ gender, data = final, mean)
aggregate(totalScore ~ condition, data = final, mean)

anova(lm (totalScore ~ enjoy , data=final))
aggregate(totalScore ~ enjoy + condition, data = final, mean)

# education
anova(lm (totalScore ~ edu, data=final))
aggregate(totalScore ~ edu, data = final, mean)

# regression with edu, enjoy as predictors
res3 = lm(totalScore ~ edu, data=final)
summary(res3)