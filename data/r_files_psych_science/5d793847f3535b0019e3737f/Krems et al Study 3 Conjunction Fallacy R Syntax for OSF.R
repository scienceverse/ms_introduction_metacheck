Data1=read.csv("[INSERT FILE]", header = T)
names(Data1)
attach(Data1)
Data1$TargsexD = factor(Data1$Targsex, labels = c("F", "M"))
Data1$SEsteemD = factor(Data1$SEsteem, labels = c("Low", "High"))

detach(Data1)

##Effects of Target Sex and Self-Esteem conjunction option
GenderSe=glm(Conjunction~TargsexD*SEsteemD, data=Data1, family=binomial)
summary(GenderSe)

##To get odds ratios
GenderSe$coef
confint(GenderSe)
exp(coef(GenderSe))
exp(confint(GenderSe))

##Does participant SOI moderate these effects?
conjunction=glm(Conjunction~TargsexD*SEsteemD*SOI, data=Data1, family=binomial)
summary(conjunction)
conjunction$coef
confint(conjunction) #Log odds ratio confidence intervals
exp(coef(conjunction)) #exponentiated coefficients, the odds ratios
exp(confint(conjunction)) #CIs for the ORs, the exponentiated coefficients

##Testing whether participant sex is a moderator--it is not (ps > .40); Q16=participant sex
conjunctionWsex=glm(Conjunction~TargsexD*SOI*SEsteemD*Q16, data=Data1, family=binomial)
summary(conjunctionWsex)##Target sex doesn't interact
conjunctionWsex$coef
confint(conjunctionWsex)
exp(coef(conjunctionWsex))
exp(confint(conjunctionWsex))

##Are these effects robust with hostile sexism as a covariate? 
HostileModel=glm(Conjunction~(TargsexD*SEsteemD) + hostile, data=Data1, family=binomial)
summary(HostileModel)
HostileModel$coef
confint(HostileModel)
exp(coef(HostileModel))
exp(confint(HostileModel))

##Are these effects robust with benevolent sexism as a covariate? 
BenevModel=glm(Conjunction~(TargsexD*SEsteemD) + benevolent, data=Data1, family=binomial)
summary(BenevModel)
BenevModel$coef
confint(BenevModel)
exp(coef(BenevModel))
exp(confint(BenevModel))
