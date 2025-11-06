#########################################################
# Rscript for Should trolleys be scared of mice?        #
# D.H. Bostyn, A. Roets                                 #
#                                                       #
# R Script by D.H. Bostyn                               #
# Contact dries.bostyn at ugent.be                      #
# for any questions or remarks                          #
#                                                       #
#########################################################
# Last updated:                                         #
# 29/05/2019                                            #
#########################################################

#Loading libraries
library(brms) 

#Setting options
options(contrasts=c("contr.sum","contr.poly"))

#Please make sure the correct data file is in your working directory 
data <- read.table("Data.txt", sep = "\t", header = TRUE)

#Factorizing the relevant variables
data$STUDY.dummy <- data$STUDY-1.5 #Dummy variable for Evans & Brandy

data$STUDY <- factor(data$STUDY, levels = c("1","2"), labels = c("Real", "Hypothetical"))
data$GENDER <- factor(data$GENDER, levels = c("0","1"), labels = c("Female","Male")) 

data$DECISION <- factor(data$DECISION, levels = c("0","1"), labels = c("DEO","UTI"))

#Making centered versions of the continuous variables
data$CONc <- scale(data$CON, scale = F)
data$DEOc <- scale(data$DEO, scale = F)

data$CONs <- scale(data$CON)
data$DEOs <- scale(data$DEO)

#Making Difference measure Plunkett & Greene
data$DIF <- data$CON - data$DEO

#Splitting the two datafiles into seperate dataframes per Study
#Do note that study1 includes some subjects that did not complete the 'real life' experiment and thus contains some NA values
study1 <- data[data$STUDY == "Real",]
study2 <- data[data$STUDY == "Hypothetical",]

###
#Evans & Brandt
set.seed(456)    #Setting seed for replicability

prior <- set_prior("student_t(3,0,2.5)", class = "b")   #prior was chosen as per the current recommendations #of the Stan team

bayes.fit1 <- brm(DECISION ~ CONc*STUDY.dummy + DEOc*STUDY.dummy + GENDER + AGE, family="bernoulli", 
                  data= data, prior = prior, sample_prior = TRUE,
                  iter= 5000 )
bayes.fit1

hyp1 <- hypothesis(bayes.fit1, "CONc:STUDY.dummy = 0")
hyp1    #BF reported by Evans & Brandt = 3.47

hyp2 <- hypothesis(bayes.fit1, "CONc:STUDY.dummy > 0")
hyp2    #Alternative BF = 5.11


###
#Plunkett & Greene
fit.PG.main <- glm(DECISION ~ DIF + GENDER + AGE, family = binomial("logit"), data = study1) 
summary(fit.PG.main) #Residual Deviance = 147.15

fit.BSR.main <- glm(DECISION ~ CON + DEO + GENDER + AGE, family = binomial("logit"), data = study1) 
summary(fit.BSR.main) #Residual Deviance = 146.63

fit.PG.study2 <- glm(DECISION ~ DIF + GENDER + AGE, family = binomial("logit"), data = study2) 
summary(fit.PG.study2) #Residual Deviance = 86.961

fit.BSR.study2 <- glm(DECISION ~ CON + DEO + GENDER + AGE, family = binomial("logit"), data = study2) 
summary(fit.BSR.study2) #Residual Deviance = 85.702



fit.PG.rt <- lm(RT ~ DIF, data = study1) 
summary(fit.PG.rt) #R² = 0.051

fit.BSR.rt <- lm(RT ~ CON + DEO, data = study1) 
summary(fit.BSR.rt) #R² = 0.055

fit.PG.uncomfort <- lm(UNCOMFORTABLE ~ DIF, data = study1) 
summary(fit.PG.uncomfort) #R² = 0.135

fit.BSR.uncomfort <- lm(UNCOMFORTABLE ~ CON + DEO, data = study1) 
summary(fit.BSR.uncomfort) #R² = 0.151

fit.PG.doubt <- lm(DOUBT ~ DIF, data = study1) 
summary(fit.PG.doubt) #R² = 0.079

fit.BSR.doubt <- lm(DOUBT ~ CON + DEO, data = study1) 
summary(fit.BSR.doubt) #R² = 0.084


var.test(data$DEO, data$CON)      #Variances not equal for all data

PairedData::levene.Var.test(data$DEO, data$CON)      #Variances not equal for all data
PairedData::levene.Var.test(study1$DEO, study1$CON)  #Variances not equal for study 1
PairedData::levene.Var.test(study2$DEO, study2$CON)  #Variances not equal for study 2


