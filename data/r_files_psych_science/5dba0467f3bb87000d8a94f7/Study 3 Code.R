library(nFactors)
library(GPArotation)
library(gmodels)

data<-read.csv("~/Desktop/Study 3 Data.csv")
d<-subset(data, data$AttChk == 1 & data$Agnostic != 1 & data$Atheist != 1)

##########################################
###   Support for Cultural Tightness   ###
##########################################

SCT<-data.frame(d$SCT_1, d$SCT_2, d$SCT_3, d$SCT_4, d$SCT_5, d$SCT_6, d$SCT_7)

psych::alpha(SCT) #.86
d$SCTTotal<-rowMeans(SCT,na.rm=T)


##########################################
###             God Indices            ###
##########################################

VOG<-data.frame("helping"=d$GodTrait_1,
                "generous"=d$GodTrait_2,
                "compassionate"=d$GodTrait_3,
                "gracious"=d$GodTrait_4,
                "tolerant"=d$GodTrait_5,
                "caring"=d$GodTrait_6,
                "accepting"=d$GodTrait_7,
                "merciful"=d$GodTrait_8,
                "forgiving"=d$GodTrait_9,
                "controlling"=d$GodTrait_10,
                "restricting"=d$GodTrait_11,
                "stern"=d$GodTrait_12,
                "commanding"=d$GodTrait_13,
                "strict"=d$GodTrait_14,
                "angry"=d$GodTrait_15,
                "judging"=d$GodTrait_16,
                "punishing"=d$GodTrait_17,
                "wrathful"=d$GodTrait_18)

ev2 <- eigen(cor(VOG)) # get eigenvalues
print(ev2)
ap2 <- parallel(subject=nrow(VOG),var=ncol(VOG),
               rep=100,cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qpea)
plotnScree(nS2)

#Maximum Likelihood FA, Oblimin Rotation
fit2<- kaiser(fa(VOG,nfactors = 2,rotate = "varimax",fm="ml"))
print(fit2, digits=2, cut=.20, sort=TRUE)
# plot factor 1 by factor 2 
load2 <- fit2$loadings[,1:2] 
plot(load2,type="n") # set up plot 
text(load2,labels=names(VOG),cex=.7) # add variable names

Loving<-data.frame("helping"=d$GodTrait_1,
                   "generous"=d$GodTrait_2,
                   "compassionate"=d$GodTrait_3,
                   "gracious"=d$GodTrait_4,
                   "caring"=d$GodTrait_6,
                   "accepting"=d$GodTrait_7,
                   "merciful"=d$GodTrait_8,
                   "forgiving"=d$GodTrait_9)

Punitive<-data.frame("controlling"=d$GodTrait_10,
                     "restricting"=d$GodTrait_11,
                     "stern"=d$GodTrait_12,
                     "commanding"=d$GodTrait_13,
                     "strict"=d$GodTrait_14,
                     "angry"=d$GodTrait_15,
                     "judging"=d$GodTrait_16,
                     "punishing"=d$GodTrait_17,
                     "wrathful"=d$GodTrait_18)

d$Loving<-rowMeans(Loving,na.rm=T)
psych::alpha(Loving) #.93
d$Punitive<-rowMeans(Punitive,na.rm=T)
psych::alpha(Punitive) #.94

##########################################
###               ANOVAS               ###
##########################################
library(car)
library(psych)
library(sjstats)
library(phia)
library(emmeans)
library(agricolae)

table(d$Condtn)
d$Condtn.f <- factor(d$Condtn, labels = c("Peace", "Poverty", "War"))

model1 <- aov(Punitive ~ Condtn.f, data = d)
Anova(model1, type = 3) 
eta_sq(model1)
TukeyHSD(model1)
LSD.test(model1, trt = "Condtn.f", console = TRUE, group = FALSE)
emmeans(model1, ~ Condtn.f)
describeBy(d$Punitive, group = d$Condtn.f)

model2<-(aov(Loving~Condtn.f, data=d))
Anova(model2, type = 3) 
eta_sq(model2)
TukeyHSD(model2)
LSD.test(model2, trt = "Condtn.f", console = TRUE, group = FALSE)
emmeans(model2, ~ Condtn.f)
describeBy(d$Loving, group = d$Condtn.f)

model3 <- aov(Punitive ~ Condtn.f + Religious + Conservative, data = d)
Anova(model3, type = 3) 
eta_sq(model3)
TukeyHSD(model3, which = "Condtn.f")
LSD.test(model1, trt = "Condtn.f", console = TRUE, group = FALSE)
emmeans(model3, ~ Condtn.f + Religious + Conservative)
describeBy(d$Punitive, group = d$Condtn.f)

##########################################
###              Mediation             ###
##########################################
library(mediation)
library(lm.beta)

d$CondDummy<- ifelse(d$Condtn=="War", 1, 0)

path_c<-lm(d$Punitive~d$CondDummy)
summary(path_c)
lm.beta(path_c)
path_a<-lm(SCTTotal~CondDummy, data = d)
summary(path_a)
lm.beta(path_a)
path_b<-lm(d$Punitive~d$SCTTotal)
summary(path_b)
lm.beta(path_b)
path_cprime<-lm(Punitive~SCTTotal+CondDummy, data = d)
summary(path_cprime)
lm.beta(path_cprime)

mediate <- mediation::mediate
mediation<-mediate(path_a, path_cprime, treat='CondDummy', mediator='SCTTotal',
                   boot=TRUE, sims=5000)

summary(mediation)

##################
##   Plotting   ##
############# #####

library("yarrr")

d$CondtnPlotting<-ifelse(d$Condtn=="War",1,
                         ifelse(d$Condtn=="Poverty",2,
                                ifelse(d$Condtn=="Peace",3,
                                       NA)))
d$CondPlot<- factor(d$CondtnPlotting, labels = c("War", "Poverty", "Peace"))
colors<- c("coral3", "lightskyblue3", "dimgray")

#Punitive
pirateplot(formula = Punitive ~ CondPlot, 
           data = d, 
           xlab = "", 
           ylab = "",
           #xaxt = "n",
           ylim = range(2:5),
           cex.lab = 2.5,
           cex.axis = 2.5,
           main = "", 
           theme = 4,
           # Choose your color palette, or give common color vector
           pal = colors,
           
           # Set transparency of the elements:
           avg.line.o = .6,
           avg.line.lwd = 1,
           avg.line.col = "black",
           bar.f.o = .7,
           bar.b.o = 0,
           bar.f.col = colors,
           bean.f.o = .1,
           #bean.f.col = "white",
           point.o = .3,
           inf.f.o = .9,
           inf.f.c = ,
           inf.b.o = .9,
           inf.method = "ci",
           bean.b.o = 1,
           # Shape of point
           point.pch = 16,
           #point.col = decision,
           
           
           #Background color
           back.col = "white",
           gl.col = "white")

pirateplot(formula = Loving ~ CondPlot, 
           data = d, 
           xlab = "", 
           ylab = "", 
           ylim = range(4:7),
           cex.axis = 2.5,
           main = "", 
           theme = 4,
           
           # Choose your color palette, or give common color vector
           pal = colors,
           
           # Set transparency of the elements:
           avg.line.o = .6,
           avg.line.lwd = 1,
           avg.line.col = "black",
           bar.f.o = .6,
           bar.b.o = 0,
           bar.f.col = colors,
           bean.f.o = .2,
           point.o = .3,
           inf.f.o = .9,
           inf.f.c = ,
           inf.b.o = .2,
           inf.method = "ci",
           bean.b.o = 1,
           # Shape of point
           point.pch = 16,
           
           #Background color
           back.col = "white",
           gl.col = "white")


##########################################
#######    Measure Correlations    #######
##########################################
library(Hmisc)
correlations<-data.frame(d$SCTTotal, d$Loving, d$Punitive, d$Religious, d$Conservative)
rcorr(as.matrix(correlations))

      