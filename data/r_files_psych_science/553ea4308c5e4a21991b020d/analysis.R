d=read.csv("~/Dropbox/Psych 254 Wrapup/Holubar/data_prepped.csv")
d.obed=d[d$Condition=="Obedient",]
d.affirm=d[d$Condition=="Rebel Affirmed",]
d.reb=d[d$Condition=="Rebel Control",]

#self-affirmation check:
mean(d.affirm$ValueImportance,na.rm=T)
sd(d.affirm$ValueImportance,na.rm=T)

#other attraction analysis
#get alpha
otherattract.items=d[,18:20]
score.items(c(1,1,1),otherattract.items)

#create other attraction composite measure:
d$Other.Attraction=rowMeans(d[,18:20],na.rm=T)

#summarize, compare other attraction means by condition
aggregate(d$Other.Attraction,by=list(d$Condition),mean)
aggregate(d$Other.Attraction,by=list(d$Condition),sd)

library(heplots)
attraction <- aov(d$Other.Attraction~d$Condition)
summary(attraction)
etasq(attraction)

TukeyHSD(aov(d$Other.Attraction~d$Condition))

#compare groups on morality
aggregate(d$Moral,by=list(d$Condition),function(x) mean(x,na.rm=T))
aggregate(d$Moral,by=list(d$Condition),function(x) sd(x,na.rm=T))

oneway.test(d$Moral~d$Condition)

#agency and communion factor analysis
traits=na.omit(d[,c(21:34)])
rs1=factanal(traits,2,rotation="promax")
print(rs1,cutoff=.4,sort=T)

#agency composite
agency.items=d[,c(21,22,34,31,33,27)]
score.items(c(1,1,1,1,1,1),agency.items)
d$agency=rowMeans(d[,c(21,22,34,31,33,27)],na.rm=T)

#communion composite
communion.items=d[,c(28:30,23,26,32)]
score.items(c(1,1,1,1,1,1),communion.items)
d$communion=rowMeans(d[,c(28:30,23,26,32)],na.rm=T)
cor(d$agency,d$communion)

#compare on agency
aggregate(d$agency,by=list(d$Condition),mean)
oneway.test(d$agency~d$Condition)
#compare on communion
aggregate(d$communion,by=list(d$Condition),mean)
oneway.test(d$communion~d$Condition)


#self attraction analysis
#get alpha
selfattract.items=d[,65:67]
score.items(c(1,1,1),selfattract.items)

#create self attraction composite measure:
d$Self.Attraction=rowMeans(d[,65:67],na.rm=T)

#summarize, compare self attraction means by condition
aggregate(d$Self.Attraction,by=list(d$Condition),mean)
summary(aov(d$Self.Attraction~d$Condition))
TukeyHSD(aov(d$Self.Attraction~d$Condition))

#affect factor analysis
affect.items=na.omit(d[,36:59])
rs2=factanal(affect.items,3,rotation="promax")
print(rs2,cutoff=.45,sort=T)

#negaffect composite
negaffect.items=d[,c(47,51,54,36,41,55,43,52,59,56)]
d$negaffect=rowMeans(d[,c(47,51,54,36,41,55,43,52,59,56)],na.rm=T)
score.items(rep(1,10),negaffect.items)
aggregate(d$negaffect,by=list(d$Condition),mean)
summary(aov(d$negaffect~d$Condition))
TukeyHSD(aov(d$negaffect~d$Condition))

#hap composite
hapos.items=d[,c(50,48,42,49,40,37,39,57)]
d$hapos=rowMeans(d[,c(50,48,42,49,40,37,39,57)],na.rm=T)
score.items(rep(1,8), hapos.items)
aggregate(d$hapos,by=list(d$Condition),mean)
summary(aov(d$hapos ~d$Condition))
TukeyHSD(aov(d$hapos ~d$Condition))

#lap composite
lapos.items=d[,c(58,45,44,46)]
score.items(c(rep(1,3),-1), lapos.items)
d[,46]=8-d[,46]#reverse score uneasy
d$lapos=rowMeans(d[,c(58,45,44,46)],na.rm=T)
aggregate(d$lapos,by=list(d$Condition),mean)
summary(aov(d$lapos ~d$Condition))
TukeyHSD(aov(d$lapos ~d$Condition))

#affect composite
affect.items=cbind(negaffect.items,hapos.items,lapos.items)
print(score.items(c(rep(1,10),rep(-1,12)),affect.items),short=F)
d$affect=rowMeans(d[,36:59],na.rm=T)
aggregate(d$affect,by=list(d$Condition),mean)
oneway.test(d$affect~d$Condition)

#own moral standing
aggregate(d$Self.Moral,by=list(d$Condition),function(x) mean(x,na.rm=T))
oneway.test(d$Self.Moral~d$Condition)

#happiness with choice
aggregate(d$Choice.happy,by=list(d$Condition),function(x) mean(x,na.rm=T))
oneway.test(d$Choice.happy~d$Condition)
t.test(d[d$Condition=="Rebel Control",]$Choice.happy,d[d$Condition=="Rebel Affirmed",]$Choice.happy,var.equal=T)



#situational blame
d$sitblame=rowMeans(d[,c(62:64)],na.rm=T)
aggregate(d$sitblame,by=list(d$Condition),function(x) mean(x,na.rm=T))
oneway.test(d$sitblame~d$Condition)
t.test(d[d$Condition=="Rebel Control",]$sitblame,d[d$Condition=="Obedient",]$sitblame,var.equal=T)
t.test(d[d$Condition=="Rebel Affirmed",]$sitblame,d[d$Condition=="Obedient",]$sitblame,var.equal=T)