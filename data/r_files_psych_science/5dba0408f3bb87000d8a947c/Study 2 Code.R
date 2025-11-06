d<-read.csv("~/Desktop/Study 2 Data.csv")

# Excluding nonreligious/polytheist
d<-d[d$Religion!=10,] # None
d<-d[d$Religion!=0,] # Atheist
d<-d[d$Religion!=8,] # Agnostic
d<-d[d$Religion!=1,] # Buddhist
d<-d[d$Religion!=4,] # Hindu

##Age, Gender, & Religion
mean(d$Age,na.rm=T)
sd(d$Age,na.rm=T)
table(d$Gender)
table(d$Religion) # 2 = Catholic, 3 = Protestant, 5 = Jewish, 6 = Muslim, 7 = Sikh, 11 = Other
table(d$Nation)

d$Singapore<-ifelse(d$Nation==1,1,0)
d$Brazil<-ifelse(d$Nation==2,1,0)
d$Germany<-ifelse(d$Nation==3,1,0)
d$USA<-ifelse(d$Nation==4,1,0)
d$NationTL<-ifelse(d$Nation==1,1,
                   ifelse(d$Nation==2,0,
                          ifelse(d$Nation==3,1,
                                 ifelse(d$Nation==4,0,
                                        NA))))

d_Brazil<-d[d$Nation==2,]
d_Germany<-d[d$Nation==3,]
d_USA<-d[d$Nation==4,]
d_Singapore<-d[d$Nation==1,]

##########################################
## Conflict and Negative Events Indices ##
##########################################
# Creating separate indices
Conflict<-data.frame("terrorist"=d$SocietalThreat_4,
                     "refugees"=d$SocietalThreat_22,
                     "illegalimmigration"=d$SocietalThreat_23,
                     "foreignattack"=d$SocietalThreat_26)

NegEvents<-data.frame("climatechange"=d$SocietalThreat_2,
                      "naturaldisaster"=d$SocietalThreat_1,
                      "pollution"=d$SocietalThreat_3,
                      "discrimination"=d$SocietalThreat_10,
                      "debt"=d$SocietalThreat_12,
                      "diseases"=d$SocietalThreat_17,
                      "crimesurge"=d$SocietalThreat_6)

psych::alpha(Conflict)
d$Conflict<-rowMeans(Conflict,na.rm = T)
psych::alpha(NegEvents)
d$NegEvents<-rowMeans(NegEvents,na.rm = T)

##########################################
#######         God Indices        #######
##########################################
library(nFactors)
library(GPArotation)

VOG<-data.frame("helping"=d$VOG_1,
                "compassionate"=d$VOG_2,
                "merciful"=d$VOG_3,
                "strict"=d$VOG_4,
                "wrathful"=d$VOG_5,
                "punishing"=d$VOG_6)

ev2 <- eigen(cor(VOG)) # get eigenvalues
print(ev2)
ap2 <- parallel(subject=nrow(VOG),var=ncol(VOG),
               rep=100,cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qpea)
plotnScree(nS2)

# Maximum Likelihood Factor Analysis, varimax rotation 
fit2 <- kaiser(fa(VOG, nfactors=2, rotate="varimax", fm="ml"))
print(fit2, digits=2, cut=.39, sort=TRUE)
# plot factor 1 by factor 2 
load2 <- fit2$loadings[,1:2] 
plot(load2,type="n") # set up plot 
text(load2,labels=names(VOG),cex=.7) # add variable names

Loving<-data.frame("helping"=d$VOG_1,
                 "compassionate"=d$VOG_2,
                 "merciful"=d$VOG_3)

Punitive<-data.frame("strict"=d$VOG_4,
                 "wrathful"=d$VOG_5,
                 "punishing"=d$VOG_6)

psych::alpha(Loving)
d$Loving<-rowMeans(Loving,na.rm=T)
psych::alpha(Punitive)
d$Punitive<-rowMeans(Punitive,na.rm=T)

#########################################
#######        Religiosity       ########
#########################################
SBS<-data.frame(d$SBS_1, d$SBS_2, d$SBS_3, d$SBS_4, d$SBS_5, d$SBS_6)
psych::alpha(SBS)
d$Religiosity<-rowMeans(SBS,na.rm=T)

##########################################
#######   Multiple Regressions     #######
##########################################
library(lm.beta)

model1<-(lm(d$Punitive~d$Conflict+d$NegEvents))
summary(model1) 
lm.beta(model1)

model2<-(lm(d$Punitive~d$Conflict+d$NegEvents+d$Conservative+d$Religiosity))
summary(model2) 
lm.beta(model2)

d$NationTL<-d$NationTL-.50
d$Conflict_c<-d$Conflict-mean(d$Conflict,na.rm=T)

model3b<-(lm(d$Punitive~d$Conflict_c*d$NationTL+d$NegEvents+d$Conservative+d$Religiosity))
summary(model3b) 
lm.beta(model3b)

model4<-(lm(d$Loving~d$Conflict+d$NegEvents+d$Conservative+d$Religiosity))
summary(model4)
lm.beta(model4)

############################################
#########Country-specific analyses##########
############################################
d_Brazil<-d[d$Nation==2,]
d_Germany<-d[d$Nation==3,]
d_USA<-d[d$Nation==4,]
d_Singapore<-d[d$Nation==1,]

##Just in Brazil
model1<-(lm(d_Brazil$Punitive~d_Brazil$Conflict+d_Brazil$NegEvents))
summary(model1) 
lm.beta(model1)

##Just in Germany
model2<-(lm(d_Germany$Punitive~d_Germany$Conflict+d_Germany$NegEvents))
summary(model2)
lm.beta(model2)

##Just in the USA
model3<-(lm(d_USA$Punitive~d_USA$Conflict+d_USA$NegEvents))
summary(model3) 
lm.beta(model3)

##Just in Singapore
model4<-(lm(d_Singapore$Punitive~d_Singapore$Conflict+d_Singapore$NegEvents))
summary(model4) 
lm.beta(model4)

##########################################
#######   Reliability By Nation    #######
##########################################
## Brazil ##
B_Conflict<-data.frame("terrorist"=d_Brazil$SocietalThreat_4,
                       "refugees"=d_Brazil$SocietalThreat_22,
                       "illegalimmigration"=d_Brazil$SocietalThreat_23,
                       "foreignattack"=d_Brazil$SocietalThreat_26)
psych::alpha(B_Conflict)

B_NegEvents<-data.frame("naturaldisaster"=d_Brazil$SocietalThreat_1,
                        "climatechange"=d_Brazil$SocietalThreat_2,
                        "pollution"=d_Brazil$SocietalThreat_3,
                        "discrimination"=d_Brazil$SocietalThreat_10,
                        "debt"=d_Brazil$SocietalThreat_12,
                        "diseases"=d_Brazil$SocietalThreat_17,
                        "crimesurge"=d_Brazil$SocietalThreat_6)
psych::alpha(B_NegEvents)

B_Loving<-data.frame("helping"=d_Brazil$VOG_1,
                     "compassionate"=d_Brazil$VOG_2,
                     "merciful"=d_Brazil$VOG_3)
psych::alpha(B_Loving)

B_Punitive<-data.frame("strict"=d_Brazil$VOG_4,
                       "wrathful"=d_Brazil$VOG_5,
                       "punishing"=d_Brazil$VOG_6)
psych::alpha(B_Punitive)

B_SBS<-data.frame(d_Brazil$SBS_1, d_Brazil$SBS_2, d_Brazil$SBS_3, d_Brazil$SBS_4, 
                  d_Brazil$SBS_5, d_Brazil$SBS_6)
psych::alpha(B_SBS)

## USA ##
USA_Conflict<-data.frame("terrorist"=d_USA$SocietalThreat_4,
                         "refugees"=d_USA$SocietalThreat_22,
                         "illegalimmigration"=d_USA$SocietalThreat_23,
                         "foreignattack"=d_USA$SocietalThreat_26)
psych::alpha(USA_Conflict)

USA_NegEvents<-data.frame("naturaldisaster"=d_USA$SocietalThreat_1,
                          "climatechange"=d_USA$SocietalThreat_2,
                          "pollution"=d_USA$SocietalThreat_3,
                          "discrimination"=d_USA$SocietalThreat_10,
                          "debt"=d_USA$SocietalThreat_12,
                          "diseases"=d_USA$SocietalThreat_17,
                          "crimesurge"=d_USA$SocietalThreat_6)
psych::alpha(USA_NegEvents)

USA_Loving<-data.frame("helping"=d_USA$VOG_1,
                       "compassionate"=d_USA$VOG_2,
                       "merciful"=d_USA$VOG_3)
psych::alpha(USA_Loving)

USA_Punitive<-data.frame("strict"=d_USA$VOG_4,
                         "wrathful"=d_USA$VOG_5,
                         "punishing"=d_USA$VOG_6)
psych::alpha(USA_Punitive)

USA_SBS<-data.frame(d_USA$SBS_1, d_USA$SBS_2, d_USA$SBS_3, d_USA$SBS_4, 
                    d_USA$SBS_5, d_USA$SBS_6)
psych::alpha(USA_SBS)

## Germany ##
G_Conflict<-data.frame("terrorist"=d_Germany$SocietalThreat_4,
                       "refugees"=d_Germany$SocietalThreat_22,
                       "illegalimmigration"=d_Germany$SocietalThreat_23,
                       "foreignattack"=d_Germany$SocietalThreat_26)
psych::alpha(G_Conflict)

G_NegEvents<-data.frame("naturaldisaster"=d_Germany$SocietalThreat_1,
                        "climatechange"=d_Germany$SocietalThreat_2,
                        "pollution"=d_Germany$SocietalThreat_3,
                        "discrimination"=d_Germany$SocietalThreat_10,
                        "debt"=d_Germany$SocietalThreat_12,
                        "diseases"=d_Germany$SocietalThreat_17,
                        "crimesurge"=d_Germany$SocietalThreat_6)
psych::alpha(G_NegEvents)

G_Loving<-data.frame("helping"=d_Germany$VOG_1,
                     "compassionate"=d_Germany$VOG_2,
                     "merciful"=d_Germany$VOG_3)
psych::alpha(G_Loving)

G_Punitive<-data.frame("strict"=d_Germany$VOG_4,
                       "wrathful"=d_Germany$VOG_5,
                       "punishing"=d_Germany$VOG_6)
psych::alpha(G_Punitive)

G_SBS<-data.frame(d_Germany$SBS_1, d_Germany$SBS_2, d_Germany$SBS_3, d_Germany$SBS_4, 
                  d_Germany$SBS_5, d_Germany$SBS_6)
psych::alpha(G_SBS)

## Singapore ##

S_Conflict<-data.frame("terrorist"=d_Singapore$SocietalThreat_4,
                       "refugees"=d_Singapore$SocietalThreat_22,
                       "illegalimmigration"=d_Singapore$SocietalThreat_23,
                       "foreignattack"=d_Singapore$SocietalThreat_26)
psych::alpha(S_Conflict)

S_NegEvents<-data.frame("naturaldisaster"=d_Singapore$SocietalThreat_1,
                        "climatechange"=d_Singapore$SocietalThreat_2,
                        "pollution"=d_Singapore$SocietalThreat_3,
                        "discrimination"=d_Singapore$SocietalThreat_10,
                        "debt"=d_Singapore$SocietalThreat_12,
                        "diseases"=d_Singapore$SocietalThreat_17,
                        "crimesurge"=d_Singapore$SocietalThreat_6)
psych::alpha(S_NegEvents)

S_Loving<-data.frame("helping"=d_Singapore$VOG_1,
                     "compassionate"=d_Singapore$VOG_2,
                     "merciful"=d_Singapore$VOG_3)
psych::alpha(S_Loving)

S_Punitive<-data.frame("strict"=d_Singapore$VOG_4,
                       "wrathful"=d_Singapore$VOG_5,
                       "punishing"=d_Singapore$VOG_6)
psych::alpha(S_Punitive)

S_SBS<-data.frame(d_Singapore$SBS_1, d_Singapore$SBS_2, d_Singapore$SBS_3, d_Singapore$SBS_4, 
                  d_Singapore$SBS_5, d_Singapore$SBS_6)
psych::alpha(S_SBS)

####################################
####Meta-analysis of effect sizes###
####################################

library(rmeta)
library(metafor)

meta<-read.csv("~/Desktop/Study 2 Data - Meta.csv")

analysis1<-rma(fr,fvar,method="REML",data=meta)
summary(analysis1)

library(scatterplot3d)

forest(analysis1, slab=paste(meta$Analysis))
##########################################
#######    Measure Correlations    #######
##########################################
library(Hmisc)
correlations<-data.frame(d$Conflict, d$NegEvents, d$Loving, d$Punitive, 
                         d$Religiosity, d$Conservative)

rcorr(as.matrix(correlations))

#############
#### CFA ####
#############
threats<-data.frame("naturaldisaster"=d$SocietalThreat_1,
                    "climatechange"=d$SocietalThreat_2,
                    "pollution"=d$SocietalThreat_3,
                    "terrorist"=d$SocietalThreat_4,
                    "crimesurge"=d$SocietalThreat_6,
                    "discrimination"=d$SocietalThreat_10,
                    "debt"=d$SocietalThreat_12,
                    "diseases"=d$SocietalThreat_17,
                    "refugees"=d$SocietalThreat_22,
                    "illegalimmigration"=d$SocietalThreat_23,
                    "foreignattack"=d$SocietalThreat_26)
threats<-na.omit(threats)

threats_Brazil<-data.frame("naturaldisaster"=d_Brazil$SocietalThreat_1,
                           "climatechange"=d_Brazil$SocietalThreat_2,
                           "pollution"=d_Brazil$SocietalThreat_3,
                           "terrorist"=d_Brazil$SocietalThreat_4,
                           "crimesurge"=d_Brazil$SocietalThreat_6,
                           "discrimination"=d_Brazil$SocietalThreat_10,
                           "debt"=d_Brazil$SocietalThreat_12,
                           "diseases"=d_Brazil$SocietalThreat_17,
                           "refugees"=d_Brazil$SocietalThreat_22,
                           "illegalimmigration"=d_Brazil$SocietalThreat_23,
                           "foreignattack"=d_Brazil$SocietalThreat_26)


threats_Germany<-data.frame("naturaldisaster"=d_Germany$SocietalThreat_1,
                            "climatechange"=d_Germany$SocietalThreat_2,
                            "pollution"=d_Germany$SocietalThreat_3,
                            "terrorist"=d_Germany$SocietalThreat_4,
                            "crimesurge"=d_Germany$SocietalThreat_6,
                            "discrimination"=d_Germany$SocietalThreat_10,
                            "debt"=d_Germany$SocietalThreat_12,
                            "diseases"=d_Germany$SocietalThreat_17,
                            "refugees"=d_Germany$SocietalThreat_22,
                            "illegalimmigration"=d_Germany$SocietalThreat_23,
                            "foreignattack"=d_Germany$SocietalThreat_26)


threats_USA<-data.frame("naturaldisaster"=d_USA$SocietalThreat_1,
                        "climatechange"=d_USA$SocietalThreat_2,
                        "pollution"=d_USA$SocietalThreat_3,
                        "terrorist"=d_USA$SocietalThreat_4,
                        "crimesurge"=d_USA$SocietalThreat_6,
                        "discrimination"=d_USA$SocietalThreat_10,
                        "debt"=d_USA$SocietalThreat_12,
                        "diseases"=d_USA$SocietalThreat_17,
                        "refugees"=d_USA$SocietalThreat_22,
                        "illegalimmigration"=d_USA$SocietalThreat_23,
                        "foreignattack"=d_USA$SocietalThreat_26)

threats_Singapore<-data.frame("naturaldisaster"=d_Singapore$SocietalThreat_1,
                              "climatechange"=d_Singapore$SocietalThreat_2,
                              "pollution"=d_Singapore$SocietalThreat_3,
                              "terrorist"=d_Singapore$SocietalThreat_4,
                              "crimesurge"=d_Singapore$SocietalThreat_6,
                              "discrimination"=d_Singapore$SocietalThreat_10,
                              "debt"=d_Singapore$SocietalThreat_12,
                              "diseases"=d_Singapore$SocietalThreat_17,
                              "refugees"=d_Singapore$SocietalThreat_22,
                              "illegalimmigration"=d_Singapore$SocietalThreat_23,
                              "foreignattack"=d_Singapore$SocietalThreat_26)

library(lavaan)
brazil_mod  = '    conflict =~ terrorist + illegalimmigration + foreignattack + refugees
negevents =~ pollution + discrimination + debt + diseases + crimesurge

refugees~~illegalimmigration

refugees~~diseases
illegalimmigration~~diseases

pollution~~terrorist

crimesurge~~foreignattack
discrimination~~debt'

brazil = cfa(model = brazil_mod, data = threats_Brazil, meanstructure = T, std.lv = F, mimic='mplus')
summary(brazil, fit.measures=T, rsquare = T, standardized = T)

modindices(brazil)

usa_mod  = '    conflict =~ terrorist + illegalimmigration + foreignattack + refugees +crimesurge
negevents =~ pollution + discrimination + debt + diseases + crimesurge

refugees~~illegalimmigration
refugees~~terrorist

refugees~~discrimination
illegalimmigration~~discrimination

terrorist~~diseases
foreignattack~~diseases'

usa = cfa(model = usa_mod, data = threats_USA, meanstructure = T, std.lv = F, mimic='mplus')
summary(usa, fit.measures=T, rsquare = T, standardized = T)

modindices(usa)

germany_mod  = ' conflict =~ illegalimmigration + foreignattack + refugees + crimesurge + terrorist
negevents =~ pollution + discrimination + debt + diseases + crimesurge

refugees~~illegalimmigration

foreignattack~~discrimination

foreignattack~~diseases
terrorist~~diseases

foreignattack~~terrorist

terrorist~~debt
crimesurge~~debt

crimesurge~~diseases'

germany = cfa(model = germany_mod, data = threats_Germany, meanstructure = T, std.lv = F, mimic='mplus')
summary(germany, fit.measures=T, rsquare = T, standardized = T)

modindices(germany)

singapore_mod  = ' conflict =~ illegalimmigration + foreignattack + refugees + terrorist
negevents =~ pollution + discrimination + debt + diseases + crimesurge + terrorist

refugees~~illegalimmigration
terrorist~~foreignattack

terrorist~~pollution

debt~~illegalimmigration
debt~~refugees
debt~~discrimination

'

singapore = cfa(model = singapore_mod, data = threats_Singapore, meanstructure = T, std.lv = F, mimic='mplus')
summary(singapore, fit.measures=T, rsquare = T, standardized = T)

modindices(singapore)