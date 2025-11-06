#import data file;
#setwd("C:/Users/")
R4g <- read.csv("160229 ROS4g data.csv",header=TRUE)
attach(R4g)

t.test(Objective ~ SubjCond)

mylogit <- glm(BFF ~ SubjCond, family = binomial(logit), data=R4g)
summary(mylogit)

mylogit <- glm(formula=BFF ~ SubjCond + FC, family = binomial, data=R4g)
summary(mylogit)