
library("lme4")
fulldata = read.csv("keyvarscentred.csv")

#SyllPos	0=diff, 1 = same		
#Group	-.5 = wake, 0.5 = sleep		
#Block	-1 = train, 0 = test1, 1 = test2		
#Session	-0.5 = train, .5  = test		
#Restriction	-.5 = unrestricted, .5 = restricted		
#List	Fas-sif = -.5, Saf-fis = .5		

fullmodel = glmer(SyllPos ~ Group*Session*Restriction + (1+Session*Restriction|SubID), data = fulldata, family = binomial)
summary(fullmodel)

#all sleep participants
sleepdata = subset(fulldata, Group == "0.5")

#sleep data - training
sleeptraindata = subset(sleepdata, Session == "-0.5")
sleeptrainmodel = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = sleeptraindata, family = binomial)
summary(sleeptrainmodel)

#sleep data - test
sleeptestdata = subset(sleepdata, Session == "0.5")
sleeptestmodel = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = sleeptestdata, family = binomial)
summary(sleeptestmodel)

#sleep data - test block 1
sleeptest1data = subset(sleepdata, Block == "0")
sleeptest1model = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = sleeptest1data, family = binomial)
summary(sleeptest1model)

#sleep data - test block 2
sleeptest2data = subset(sleepdata, Block == "1")
sleeptest2model = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = sleeptest2data, family = binomial)
summary(sleeptest2model)

#all wake participants
wakedata = subset(fulldata, Group == "-0.5")

#wake data - training
waketraindata = subset(wakedata, Session == "-0.5")
waketrainmodel = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = waketraindata, family = binomial)
summary(waketrainmodel)

#wake data - test
waketestdata = subset(wakedata, Session == "0.5")
waketestmodel = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = waketestdata, family = binomial)
summary(waketestmodel)

#now go through everything again with just the matched items - for convenience this reuses the same variable names
matcheddata = subset(fulldata, Matched == "yes")
matchedmodel = glmer(SyllPos ~ Group*Session*Restriction + (1+Session*Restriction|SubID), data = matcheddata, family = binomial)
summary(matchedmodel)

#first check that there is no difference between the groups at training
matchedtraindata = subset(matcheddata, Session == "-0.5")
matchedtrainmodel = glmer(SyllPos ~ Group*Restriction + (1+Restriction|SubID), data = matchedtraindata, family = binomial)
summary(matchedtrainmodel)

#all sleep participants
sleepdata = subset(matcheddata, Group == "0.5")
sleepmodel = glmer(SyllPos ~ Session*Restriction + (1+Session*Restriction|SubID), data = sleepdata, family = binomial)
summary(sleepmodel)

#sleep data - test
sleeptestdata = subset(sleepdata, Session == "0.5")
sleeptestmodel = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = sleeptestdata, family = binomial)
summary(sleeptestmodel)

#all wake participants
wakedata = subset(matcheddata, Group == "-0.5")
wakemodel = glmer(SyllPos ~ Session*Restriction + (1+Session*Restriction|SubID), data = wakedata, family = binomial)
summary(wakemodel)

#wake data - test
waketestdata = subset(wakedata, Session == "0.5")
waketestmodel = glmer(SyllPos ~ Restriction + (1+Restriction|SubID), data = waketestdata, family = binomial)
summary(waketestmodel)

#now that the matched analyses are complete - back to the full set
sleepdata = subset(fulldata, Group == "0.5")
wakedata = subset(fulldata, Group == "-0.5")

#regression - focusing on the key interaction
N1model = glm(SyllPos ~ Session:Restriction:N1Mins, data = sleepdata, family = "binomial")  
N2model = glm(SyllPos ~ Session:Restriction:N2Mins, data = sleepdata, family = "binomial")
N3model = glm(SyllPos ~ Session:Restriction:N3Mins, data = sleepdata, family = "binomial")
Rmodel = glm(SyllPos ~ Session:Restriction:RMins, data = sleepdata, family = "binomial")
SleepPeriodmodel = glm(SyllPos ~ Session:Restriction:SleepPeriod, data = sleepdata, family = "binomial")
Deltamodel = glm(SyllPos ~ Session:Restriction:Delta, data = sleepdata, family = "binomial")

summary(N1model)
summary(N2model)
summary(N3model)
summary(Rmodel)
summary(SleepPeriodmodel)
summary(Deltamodel)