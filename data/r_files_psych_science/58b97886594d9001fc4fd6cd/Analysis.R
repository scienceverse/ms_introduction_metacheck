#########################################################
# Rscript for the analysis  of The Validity of Trolleys #
# D.H. Bostyn, S. Sevenhant, A. Roets                   #
#                                                       #
# R Script by D.H. Bostyn                               #
# Contact dries.bostyn at ugent.be                      #
# for any questions or remarks                          #
#                                                       #
#########################################################
# Last updated:                                         #
# 08/12/2017                                            #
#########################################################

#Loading libraries
library(psych)
library(Hmisc)
library(sjPlot)
library(car)

#Cleaning up the workspace
rm(list=ls(all=TRUE))

#Setting options
options(contrasts=c("contr.sum","contr.poly"))

#Please make sure the correct data file is in your working directory 
data <- read.table("Data.txt", sep = "\t", header = TRUE)

#Factorizing the relevant variables
data$STUDY <- factor(data$STUDY, levels = c("1","2"), labels = c("Real", "Hypothetical"))
data$GENDER <- factor(data$GENDER, levels = c("0","1"), labels = c("Female","Male")) 

data$DECISION <- factor(data$DECISION, levels = c("0","1"), labels = c("DEO","UTI"))

#Splitting the two datafiles into seperate dataframes per Study
#Do note that study1 includes some subjects that did not complete the 'real life' experiment and thus contains some NA values
study1 <- data[data$STUDY == "Real",]
study2 <- data[data$STUDY == "Hypothetical",]

#Making centered versions of the continuous variables in study1 for moderation analyses
study1$CONc <- scale(study1$CON, scale = F)
study1$DEOc <- scale(study1$DEO, scale = F)

study1$NOSHOCKc <- scale(study1$NOSHOCK, scale = F)
study1$AEc <- scale(study1$AE, scale = F)
study1$MOUSEc <- scale(study1$MOUSE, scale = F)

###############
#Reliabilities#
###############
deoscale <- data[,5:14]
conscale <- data[,16:25]
nfc <- data[,27:44]
ec <- data[,46:52]
pt <- data[,54:60]
pp <- data[,62:77]
mii <- data[,89:93]
mis <- data[,95:99]
ae <- data[,101:122]
mouse <- data[,124:126]

psych::alpha(deoscale)
psych::alpha(conscale)
psych::alpha(nfc)
psych::alpha(ec)
psych::alpha(pt)
psych::alpha(pp)
psych::alpha(mii)
psych::alpha(mis)
psych::alpha(ae)
psych::alpha(mouse)

##########
#Analysis#
##########

#Exploratory: demographics
summary(study1$GENDER)
summary(study2$GENDER)

summary(study1$AGE)
summary(study2$AGE)

#Preliminary Analysis: Correlation table (Table 1)
correlation <- as.matrix(data.frame(study1$CON, study1$DEO, study1$NFC, study1$EC, study1$PT, study1$PP, 
                                    study1$I, study1$S, study1$AE, study1$MOUSE))
rcorr(correlation)

#Preliminary Analysis: Were subjects skeptic?
summary(study1$NOSHOCK)
table(study1$NOSHOCK)
hist(study1$NOSHOCK)

  #Creating two new datafiles for subsample analysis
  skeptics1 <- na.omit(study1[study1$NOSHOCK >= 100,])  
  skeptics2 <- na.omit(study1[study1$NOSHOCK >= 90,])
  
  study1.noskeptics <- study1[study1$NOSHOCK < 90,]

  #Were skeptics uncomfortable?
  table(skeptics1$UNCOMFORTABLE)
  table(skeptics2$UNCOMFORTABLE)


#Preliminary Analysis: Were subjects uncomfortable?
summary(study1$UNCOMFORTABLE)
table(study1$UNCOMFORTABLE)


#Main Analysis: More consequentialists on the 'real life' dilemma?
  #Proportion test
  prop.test(c(summary(study1$DECISION)[1],summary(study2$DECISION)[1]),
            c(length(na.omit(study1$DECISION)),length(na.omit(study2$DECISION))))

  #Controlled for Gender & Age differences 
  fit.main1 <- glm(DECISION ~ STUDY + GENDER + AGE, family = binomial("logit"), data = data) 
  summary(fit.main1)

#Main Analysis: Do moral preferences predict judgment on the hypothetical dilemma? 
  #No Demographic controls
  fit.uc2 <- glm(DECISION ~ CON + DEO, family = binomial("logit"), data = study2) 
  summary(fit.uc2)

  #Controlled for Gender & Age differences(reported in the manuscript)
  fit.main2 <- glm(DECISION ~ CON + DEO + GENDER + AGE, family = binomial("logit"), data = study2) 
  summary(fit.main2)


#Main Analysis: Do moral preferences predict judgment on the 'real life' dilemma?
  #No Demographic controls
  fit.uc3 <- glm(DECISION ~ CON + DEO, family = binomial("logit"), data = study1) 
  summary(fit.uc3)
  
  #Controlled for Gender & Age differences (reported in the manuscript)
  fit.main3 <- glm(DECISION ~ CON + DEO + GENDER + AGE, family = binomial("logit"), data = study1) 
  summary(fit.main3)
  
  #No skeptics
  fit.skeptic3 <- glm(DECISION ~ CON + DEO + GENDER + AGE, family = binomial("logit"), data = study1.noskeptics) 
  summary(fit.skeptic3)
  
  #Moderated by skepticism? (Supplementary Table 1)
  fit.skepmod3 <- glm(DECISION ~ (CONc + DEOc) * NOSHOCKc, family = binomial("logit"), data = study1) 
  summary(fit.skepmod3)
  vif(fit.skepmod3) #Checking for multicollinearity
  
  #Moderated by animal empathy? (Supplementary Table 2)
  fit.aemod3 <- glm(DECISION ~ (CONc + DEOc) * AEc, family = binomial("logit"), data = study1) 
  summary(fit.aemod3)
  vif(fit.aemod3) #Checking for multicollinearity
  
  #Moderated by empathy for mice? (Supplementary Table 3)
  fit.mousemod3 <- glm(DECISION ~ (CONc + DEOc) * MOUSEc, family = binomial("logit"), data = study1) 
  summary(fit.mousemod3)
  vif(fit.mousemod3) #Checking for multicollinearity
  
#Main Analysis: Do the individual difference measures predict decisions on the real life' dilemma?
  #No Demographic controls
  fit.uc4 <- glm(DECISION ~ NFC + EC + PT + PP + I + S + AE  + MOUSE, family = binomial("logit"), data = study1) 
  summary(fit.uc4)

  #Controlled for Gender & Age differences (reported in the manuscript)  
  fit.main4 <- glm(DECISION ~ NFC + EC + PT + PP + I + S + AE + MOUSE + GENDER + AGE, family = binomial("logit"), data = study1) 
  summary(fit.main4)

  #No skeptics
  fit.skeptic4 <- glm(DECISION ~ NFC + EC + PT + PP + I + S + AE + MOUSE + GENDER + AGE, family = binomial("logit"), data = study1.noskeptics) 
  summary(fit.skeptic4)


#Main Analysis: Do moral preferences predict self-reported doubt on the real life' dilemma?
  #No Demographic controls
  fit.uc5 <- lm(DOUBT ~ CON + DEO, data = study1) 
  summary(fit.uc5)

  #Controlled for Gender & Age differences (reported in the manuscript)  
  fit.main5 <- lm(DOUBT ~ CON + DEO + GENDER + AGE, data = study1) 
  summary (fit.main5)

  #No skeptics 
  fit.skeptic5 <- lm(DOUBT ~ CON + DEO + GENDER + AGE, data = study1.noskeptics) 
  summary(fit.skeptic5)
  
  #Moderated by skepticism? (Supplementary Table 4)
  fit.skepmod5 <- lm(DOUBT ~ (CONc + DEOc) * NOSHOCKc, data = study1) 
  summary(fit.skepmod5)
  
  #Moderated by animal empathy? (Supplementary Table 5)
  fit.aemod5 <- lm(DOUBT ~ (CONc + DEOc) * AEc, data = study1) 
  summary(fit.aemod5)
  
  #Moderated by empathy for mice? (Supplementary Table 6)
  fit.mousemod5 <- lm(DOUBT ~ (CONc + DEOc) * MOUSEc, data = study1) 
  summary(fit.mousemod5)


#Main Analysis: Do moral preferences predict self-reported uncomfortabless on the real life' dilemma? 
  #No Demographic controls
  fit.uc6 <- lm(UNCOMFORTABLE ~ CON + DEO, data = study1) 
  summary (fit.uc6)
  
  #Controlled for Gender & Age differences (reported in the manuscript)  
  fit.main6 <- lm(UNCOMFORTABLE ~ CON + DEO + GENDER + AGE, data = study1) 
  summary(fit.main6)
  
  #No skeptics 
  fit.skeptic6 <- lm(UNCOMFORTABLE ~ CON + DEO + GENDER + AGE, data = study1.noskeptics) 
  summary(fit.skeptic6)
  
  #Moderated by skepticism? (Supplementary Table 7)
  fit.skepmod6 <- lm(UNCOMFORTABLE ~ (CONc + DEOc) * NOSHOCKc, data = study1) 
  summary(fit.skepmod6)
  
  #Moderated by animal empathy? (Supplementary Table 8)
  fit.aemod6 <- lm(UNCOMFORTABLE ~ (CONc + DEOc) * AEc, data = study1) 
  summary(fit.aemod6)
  
  #Moderated by empathy for mice? (Supplementary Table 9)
  fit.mousemod6 <- lm(UNCOMFORTABLE ~ (CONc + DEOc) * MOUSEc, data = study1) 
  summary(fit.mousemod6)
  
  cor.test(study1$UNCOMFORTABLE, study1$AE)
  cor.test(study1$UNCOMFORTABLE, study1$MOUSE)
  
  #Main Analysis: Do moral preferences predict predict reaction times on the real life' dilemma? (Sup. Table 8)
  #No Demographic controls
  fit.uc7 <- lm(RT ~ CON + DEO, data = study1) 
  summary (fit.uc7)
  
  #Controlled for Gender & Age differences (reported in the manuscript)  
  fit.main7 <- lm(RT ~ CON + DEO + GENDER + AGE, data = study1) 
  summary(fit.main7)
  
  #No skeptics 
  fit.skeptic7 <- lm(RT ~ CON + DEO + GENDER + AGE, data = study1.noskeptics) 
  summary(fit.skeptic7)
  
  #Moderated by skepticism? (Supplementary Table 10)
  fit.skepmod7 <- lm(RT ~ (CONc + DEOc) * NOSHOCKc, data = study1) 
  summary(fit.skepmod7)
  
  #Moderated by animal empathy? (Supplementary Table 11)
  fit.aemod7 <- lm(RT ~ (CONc + DEOc) * AEc, data = study1) 
  summary(fit.aemod7)
  
  #Moderated by empathy for mice? (Supplementary Table 12)
  fit.mousemod7 <- lm(RT ~ (CONc + DEOc) * MOUSEc, data = study1) 
  summary(fit.mousemod7)
  

########################
#Supplementary Analyses#
########################

#Reanalyzing the data using a personal / impersonal distinction
  #Creating new moral preference measures: 4vs6 measure
  study1$PERSONAL_4v6_CON <- (study1$CON2 + study1$CON3 + study1$CON4 + study1$CON8 + study1$CON9 + study1$CON10)/6
  study1$PERSONAL_4v6_DEO <- (study1$DEO2 + study1$DEO3 + study1$DEO4 + study1$DEO8 + study1$DEO9 + study1$DEO10)/6
  
  study1$IMPERSONAL_4v6_CON <- (study1$CON1 + study1$CON5 + study1$CON6 + study1$CON7)/4
  study1$IMPERSONAL_4v6_DEO <- (study1$DEO1 + study1$DEO5 + study1$DEO6 + study1$DEO7)/4
  
  study2$PERSONAL_4v6_CON <- (study2$CON2 + study2$CON3 + study2$CON4 + study2$CON8 + study2$CON9 + study2$CON10)/6
  study2$PERSONAL_4v6_DEO <- (study2$DEO2 + study2$DEO3 + study2$DEO4 + study2$DEO8 + study2$DEO9 + study2$DEO10)/6
  
  study2$IMPERSONAL_4v6_CON <- (study2$CON1 + study2$CON5 + study2$CON6 + study2$CON7)/4
  study2$IMPERSONAL_4v6_DEO <- (study2$DEO1 + study2$DEO5 + study2$DEO6 + study2$DEO7)/4
  
  #Creating new moral preference measures: 1vs9 measure
  study1$PERSONAL_1v9_CON <- (study1$CON2 + study1$CON3 + study1$CON4 + study1$CON8 + study1$CON9 + study1$CON10 + study1$CON1 + study1$CON5 + study1$CON6)/9
  study1$PERSONAL_1v9_DEO <- (study1$DEO2 + study1$DEO3 + study1$DEO4 + study1$DEO8 + study1$DEO9 + study1$DEO10 + study1$DEO1 + study1$DEO5 + study1$DEO6)/9
  
  study2$PERSONAL_1v9_CON <- (study2$CON2 + study2$CON3 + study2$CON4 + study2$CON8 + study2$CON9 + study2$CON10 + study2$CON1 + study2$CON5 + study2$CON6)/6
  study2$PERSONAL_1v9_DEO <- (study2$DEO2 + study2$DEO3 + study2$DEO4 + study2$DEO8 + study2$DEO9 + study2$DEO10 + study2$DEO1 + study2$DEO5 + study2$DEO6)/6
  

  #Analysis of 4v6 and the Real-life dilemma
  fit.impers1 <- glm(DECISION ~ IMPERSONAL_4v6_CON, family = binomial("logit"), data = study1) 
  summary(fit.impers1)
  fit.pers1 <- glm(DECISION ~ PERSONAL_4v6_CON, family = binomial("logit"), data = study1) 
  summary(fit.pers1)
  
  #Analysis of 4v6 and the Hypothetical mouse dilemma
  fit.impers2 <- glm(DECISION ~ IMPERSONAL_4v6_CON, family = binomial("logit"), data = study2) 
  summary(fit.impers2)
  fit.pers2 <- glm(DECISION ~ PERSONAL_4v6_CON, family = binomial("logit"), data = study2) 
  summary(fit.pers2)

  #Analysis of 1v9 and the Real-life dilemma
  fit.impers1 <- glm(DECISION ~ CON7, family = binomial("logit"), data = study1) 
  summary(fit.impers1)
  fit.pers1 <- glm(DECISION ~ PERSONAL_1v9_CON, family = binomial("logit"), data = study1) 
  summary(fit.pers1)
  
  #Analysis of 1v9 and the Hypothetical mouse dilemma
  fit.impers2 <- glm(DECISION ~ CON7, family = binomial("logit"), data = study2) 
  summary(fit.impers2)
  fit.pers2 <- glm(DECISION ~ PERSONAL_1v9_CON, family = binomial("logit"), data = study2) 
  summary(fit.pers2)
  
  #Item level correlations of participants' responses to the consequentialist option for the moral dilemma battery
  rcorr(as.matrix(conscale))

  #Principal component analysis of participants' responses to the consequentialist option for the moral dilemma battery
  pc.conscale <- princomp(conscale)
  
  plot(pc.conscale, main = NA, family="serif")
  pc.conscale$loadings

  #Additional analysis to test for potential demographic differences
  prop.test(table(data$GENDER, data$STUDY))
  fit.age <- lm(AGE ~ STUDY, data = data)
  summary(fit.age)
  
  fit.deo <- lm(DEO ~ STUDY, data = data)
  summary(fit.deo)
  
  fit.con <- lm(CON ~ STUDY, data = data)
  summary(fit.con)
  
  #Bayes Factor
  library(brms)  #Do note that to replicate these analyses some additional programs might need to be installed
                 #Please check the online documentation of the brms package for assistance.
  
  set.seed(0)    #Setting seed for replicability
  
  prior <- set_prior("student_t(3,0,2.5)", class = "b")   #prior was chose as per the current recommendations
                                                          #of the Stan team
  
  bayes.fit1 <- brm(DECISION ~ CON + DEO + GENDER + AGE, family="bernoulli", data= study1, prior = prior, sample_prior = TRUE)
  bayes.fit1
  plot(bayes.fit1)
  
  hyp1 <- hypothesis(bayes.fit1, "CON = 0")
  hyp2 <- hypothesis(bayes.fit1, "DEO = 0")
  hyp1
  hyp2
  
  bayes.fit2 <- brm(DECISION ~ NFC + EC + PT + PP + I + S + AE + MOUSE + GENDER + AGE, family="bernoulli", data= study1, prior = prior, sample_prior = TRUE)
  bayes.fit2
  plot(bayes.fit2)
  hyp3 <- hypothesis(bayes.fit2, "NFC = 0")
  hyp4 <- hypothesis(bayes.fit2, "EC = 0")
  hyp5 <- hypothesis(bayes.fit2, "PT = 0")
  hyp6 <- hypothesis(bayes.fit2, "PP = 0")
  hyp7 <- hypothesis(bayes.fit2, "I = 0")
  hyp8 <- hypothesis(bayes.fit2, "S = 0")
  hyp9 <- hypothesis(bayes.fit2, "AE = 0")
  hyp10 <- hypothesis(bayes.fit2, "MOUSE = 0")
  
  hyp3
  hyp4
  hyp5
  hyp6
  hyp7
  hyp8
  hyp9
  hyp10
  
  