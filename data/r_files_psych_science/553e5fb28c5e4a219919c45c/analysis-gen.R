

library("lme4")
recgendata = read.csv("recgendata.csv")

#result	1 = correct, 0 = incorrect		
#Group	-.5 = wake, 0.5 = sleep
#Type: OK = both unfamiliar, bad-x = target unfamiliar, bad-m = target familiar		
	
#all gen data
gendata = subset(recgendata, test == "B-Gen")

#set up sum contrasts 
contrasts(gendata$Type) = contr.sum(levels(gendata$Type))

genallmodel = glmer(result ~ Group*Type + (1+Type|SubID), data = gendata, family = binomial)
summary(genallmodel)

#look just at the "OK" items  - the ones in which both items have an unfamiliar phoneme
genokdata = subset(gendata, Type == "OK")
genoksleepdata = subset(genokdata, Group == "0.5")
genokwakedata = subset(genokdata, Group == "-0.5")

#these are to test for above chance performance just the OK items - sig of intercept...
genoksleepmodel = glmer(result ~ 1 + (1|SubID), data = genoksleepdata, family = binomial)
summary(genoksleepmodel)
genokwakemodel = glmer(result ~ 1 + (1|SubID), data = genokwakedata, family = binomial)
summary(genokwakemodel)
