###Children’s Cognitive Reflection Predicts Conceptual Understanding in Science and Mathematics 
###Frequentist regression analyses (using multiple imputation) reported in the supplemental text
##Note: exact values will not reproduce because variability in multiple imputation


################
###Load packages
################

library(mice)
library(reshape2)
library(lme4)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggstance)


#############################
###Load data & make variables
#############################

#Data is available at https://osf.io/e72ka/?view_only=57c8b2f8cf7b4193af2df40af14c71d7
data=read.csv("CRTD_DATA.csv")

#CRTD
CRTD<- data[,c("crt.cows","crt.emily","crt.butterfly",
               "crt.pound","crt.sheep","crt.santa",
               "crt.race","crt.apples","crt.mo")]
data$CRTD <- rowSums(CRTD)

#Denominator Neglect
DN <- data[,c("T.1.9.vs.9.91","T.1.9.vs.8.92","T.2.8.vs.18.82","T.2.8.vs.19.81",
              "T.3.7.vs.29.71","T.3.7.vs.28.72")]
data$DN <- rowSums(DN)

#Base Rate Sensitivity
BRS <- data[,c("brs1","brs2","brs3","brs4","brs5")]
data$BRS <- rowSums(BRS)

#NFC
NFC <- data[,c("NFC_1","NFC_2","NFC_3","NFC_4","NFC_5","NFC_6","NFC_7",                 
               "NFC_8","NFC_9","NFC_10","NFC_11","NFC_12", "NFC_13","NFC_14")]
data$NFC <- rowMeans(NFC)

#Mathematical Equivalence: Number Correct and Any Correct
ME <- data[,c("ME1_correct","ME2_correct","ME3_correct","ME4_correct")]
data$ME_Sum <- rowSums(ME,na.rm=FALSE)
data$ME_Any<- ifelse(data$ME_Sum >0,1,0)

#BIO Composite
BIO <- data[,c("BodyParts","ANIMISM_Sum")]
BIO <- scale(BIO)
data$BIOcomp <- rowMeans(scale(BIO),na.rm=FALSE)


###################################
###Make imputed data sets with mice
###################################

###Scaled-Predictor Model Data
mod_data <- data[,c("ResponseId",
                    "ANIMALS","PLANTS","NATURAL","ARTIFACTS",
                    "BodyParts","ANIMISM_Sum","BIOcomp","ME_Any",
                    "MOS", 
                    "CRTD",
                    "NFC",
                    "VF","SPAN",
                    "DCCS","FLANKER",
                    "DN","BRS")]     

#Unit scale predictor variables & Bio/BPAS
pvars <-c("MOS","CRTD","NFC","VF","SPAN","BodyParts","BIOcomp","DCCS","FLANKER","DN","BRS")
scaled_model_data <- mod_data
scaled_model_data[pvars] <- lapply(mod_data[pvars],scale)

####Make Multiple Imputation Data 
mi_data <- scaled_model_data
###if mice isn't happy with lapply step above, reload data
#write.csv(mi_data,"mi_data.csv")
#mi_data<-read.csv("mi_data.csv")

#set up imputation model
#all predictors using default pmm. do not impute or use outcome variables for imputation.
ini <- mice(mi_data, maxit=0, pri=F)
pred <- ini$predictorMatrix
pred[,c("ResponseId","BIOcomp","BodyParts","ME_Any", "ANIMALS","PLANTS","NATURAL","ARTIFACTS","ANIMISM_Sum")] <- 0
meth <- ini$meth
meth[c("BIOcomp","BodyParts","ME_Any", "ANIMALS","PLANTS","NATURAL","ARTIFACTS","ANIMISM_Sum")] <- ""

#run imputation
imp <- mice(mi_data, meth=meth, pred=pred,print=TRUE, m=100,maxit=100,seed=2018)


######################################################################
###Fit models on mi data and combine for inference using Rubin's rules
######################################################################

###Biology Composite Models
mi_bio_CRTD <- with(data=imp, exp=lm(BIOcomp ~ MOS + DCCS + FLANKER + VF + SPAN + DN + BRS  + NFC + CRTD))
mi_bio_noCRTD <- with(data=imp, exp=lm(BIOcomp ~ MOS + DCCS + FLANKER + VF + SPAN + DN + BRS + NFC))
pool.r.squared(mi_bio_CRTD)       
pool.r.squared(mi_bio_noCRTD)
mi_sum_bio <- summary(pool(mi_bio_CRTD), conf.int=TRUE)
pool.compare(mi_bio_CRTD, mi_bio_noCRTD, method="wald")

###Body Parts Knowledge Models
mi_BodyParts_CRTD <- with(data=imp,exp=lm(BodyParts ~ MOS + DCCS + FLANKER + VF + SPAN + DN + BRS + NFC + CRTD))
mi_BodyParts_noCRTD <- with(data=imp, exp=lm(BodyParts ~ MOS + DCCS + FLANKER + VF + SPAN + DN + BRS + NFC))
pool.r.squared(mi_BodyParts_CRTD)       
pool.r.squared(mi_BodyParts_noCRTD) 
mi_sum_BodyParts<- summary(pool(mi_BodyParts_CRTD), conf.int=TRUE)
pool.compare(mi_BodyParts_CRTD, mi_BodyParts_noCRTD, method="wald")

###Math Equivalence Models
mi_ME_CRTD <- with(data=imp, exp=glm(ME_Any ~ MOS + DCCS + FLANKER + VF + SPAN + DN + BRS + NFC + CRTD, family = binomial))
mi_ME_noCRTD <- with(data=imp, exp=glm(ME_Any~ MOS + DCCS + FLANKER + VF + SPAN + DN + BRS + NFC, family = binomial))
mi_sum_ME <- summary(pool(mi_ME_CRTD),conf.int=TRUE)
pool.compare(mi_ME_CRTD, mi_ME_noCRTD, method="wald")

####Living Things (multilevel) Models

#Make data for glmer (i.e., long by category)
implong <- complete(imp,"long")
implong <- melt(implong, variable.name = "category",value.name = "ANIMISM",
                                    id=c( ".imp",".id","X","ResponseId",
                                          "BodyParts","ANIMISM_Sum","BIOcomp","ME_Any",
                                          "MOS","CRTD","NFC","VF","SPAN","DCCS",        
                                          "FLANKER","DN","BRS"))
l <- split(implong, implong$.imp)
l <- mitml::as.mitml.list(l)

#Fit models
fit0<- with(l , glmer(ANIMISM ~ MOS + DCCS + FLANKER+ VF + SPAN + DN + BRS + NFC + (1|category) + (1|ResponseId),
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family=binomial))
fit1<- with(l , glmer(ANIMISM ~ MOS + DCCS + FLANKER + VF+ SPAN + DN + BRS + NFC + CRTD + (1|category) + (1|ResponseId),
                      control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                      family=binomial))
#Results
mi_sum_ANIMISM <- summary(pool(fit1),conf.int=TRUE)
pool.compare(fit1, fit0,method="wald")


#####################################################
####Summary model coefficient plots (i.e., Fig. S1)
##################################################

###Bio Comp Summary
mi_sum_bio$term <- row.names(mi_sum_bio)
mi_sum_bio$term <- revalue(mi_sum_bio$term, c("CRTD"="CRT-D", 
                                                          "MOS"="AGE",
                                                          "SPAN"="WM"))
mi_sum_bio$term  <- factor(mi_sum_bio$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))
mi_sum_bio$upper <- mi_sum_bio[,7]
mi_sum_bio$lower <- mi_sum_bio[,6]
S_BIO_Coeff_fig <- ggplot(mi_sum_bio[-1,], aes(x = estimate, y = term, 
                                              xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (SDs)") + ylab("") +
  ggtitle("A. Biology Composite") +
  xlim(-.5, .5) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)
S_BIO_Coeff_fig
#ggsave("S_BIO_coef.pdf", width = 3.25, height = 3)


###Body Parts Summary
mi_sum_BodyParts$term <- row.names(mi_sum_BodyParts)
mi_sum_BodyParts$term <- revalue(mi_sum_BodyParts$term, c("CRTD"="CRT-D", 
                                              "MOS"="AGE",
                                              "SPAN"="WM"))
mi_sum_BodyParts$term  <- factor(mi_sum_BodyParts$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))
mi_sum_BodyParts$upper <- mi_sum_BodyParts[,7]
mi_sum_BodyParts$lower <- mi_sum_BodyParts[,6]
S_BodyParts_Coeff_fig <- ggplot(mi_sum_BodyParts[-1,], aes(x = estimate, y = term, 
                                               xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (SDs)") + ylab("") +
  ggtitle("B. Body Parts Knowledge") +
  xlim(-.5, .5) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)
S_BodyParts_Coeff_fig
#ggsave("S_BPAS_coef.pdf", width = 3.25, height = 3)


###Living Things Summary
mi_sum_ANIMISM$term <- row.names(mi_sum_ANIMISM)
mi_sum_ANIMISM$term <- revalue(mi_sum_ANIMISM$term, c("CRTD"="CRT-D", 
                                                "MOS"="AGE",
                                                "SPAN"="WM"))
mi_sum_ANIMISM$term  <- factor(mi_sum_ANIMISM$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))
mi_sum_ANIMISM$upper <- mi_sum_ANIMISM[,7]
mi_sum_ANIMISM$lower <- mi_sum_ANIMISM[,6]
S_ANIMISM_Coeff_fig <- ggplot(mi_sum_ANIMISM[-1,], aes(x = estimate, y = term, 
                                                 xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(position = position_dodgev(height = .6),size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (log odds)") + ylab("") +
  ggtitle("C. Living Things Knowledge") +
  xlim(-1, 1) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)
S_ANIMISM_Coeff_fig
#ggsave("S_ANIMISM_coef.pdf", width = 3.25, height = 3)


###Math Equivalence Summary
mi_sum_ME$term <- row.names(mi_sum_ME)
mi_sum_ME$term <- revalue(mi_sum_ME$term, c("CRTD"="CRT-D", 
                                                "MOS"="AGE",
                                                "SPAN"="WM"))
mi_sum_ME$term  <- factor(mi_sum_ME$term, levels = c("NFC","BRS","DN","WM","VF","FLANKER","DCCS","AGE","CRT-D"))
mi_sum_ME$upper <- mi_sum_ME[,7]
mi_sum_ME$lower <- mi_sum_ME[,6]
S_ME_Coeff_fig <- ggplot(mi_sum_ME[-1,], aes(x = estimate, y = term, 
                                                 xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype =2) +
  geom_pointrangeh(position = position_dodgev(height = .6),size=.3,stroke = .75)+
  theme_bw() + xlab("Coefficient Estimate (log odds)") + ylab("") +
  ggtitle("D. Mathematical Equivalence") +
  xlim(-4, 4) +
  scale_colour_grey(start = .1, end = .1) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        text = element_text(family = "Helvetica"),
        axis.text = element_text(size=9,colour="#000000"),
        plot.title = element_text(face="bold",size=12),
        axis.title.x = element_text(colour="#000000", size=10),
        legend.position = c(.6, 0.01),
        #legend.position = "top",
        legend.text = element_text(colour="#000000", size=9),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.title.align = .5) +
  guides(shape = guide_legend(reverse=TRUE,title=NULL),
         colour=FALSE,fill=FALSE)
S_ME_Coeff_fig
ggsave("S_ME_coef.pdf", width = 3.25, height = 3)

