#######################
#CCT Supplementary Code

#Zickfeld et al. (2019).

#This script contains code for the following analyses:

#1. Meta-Analyses.

#2. Pilot Studies.

#2.1 Pilot Study - Situation Ratings: https://osf.io/gmp8b/
#2.2 Pilot Study - Situation Ratings Neutral: https://osf.io/r4wfz/
#2.3 Pilot Study - Retention of Warmth and Competence Items
#2.4 Pilot Study - Main Pilot Study 1: https://osf.io/vwdj3/.
#2.5 Pilot Stimulus Rating: https://osf.io/ej4a3/.
#2.6 Pilot Study - Main Pilot Study 2: https://osf.io/s7b2y/.
#2.7 Pilot Study - Main Pilot Study 3: https://osf.io/bmsku/.
#2.8 Pilot Study - Main Pilot Study 4: https://osf.io/cjfvq/.
#2.9 Pilot Study - Arousal Rating: https://osf.io/vmpuj/.



#######################
#LOAD LIBRARIES.

library(dplyr) #-> version 0.8.1
library(psych) #-> version 1.8.12
library(lme4) #-> version 1.1-21
library(lmerTest) #-> version 3.0-1
library(emmeans) #-> version 1.2.3 
library(ggpubr) #-> version 0.1.8.999
library(tidyr) # -> version 0.8.3
library(reshape2) #-> version 1.4.3
library(apaTables) #-> version 2.0.5
library(foreign) #-> version 0.8-71
library(lm.beta) #-> version 1.5-1
library(gmodels) #-> version 2.18.1
library(simr) #-> version 1.0.4
library(sjPlot) #-> version 2.6.2
library(OpenMx) #-> version 2.12.2
library(metaSEM) #-> version 1.2.0
library(metafor) #-> version 2.0-0
library(car) #-> version 3.0-2
library(httr) #-> version 1.4.1

source("http://sachaepskamp.com/files/OSF/getOSFfile.R")
source("CCT_Functions.R") #you will need to download the CCT_Functions file and put it in the same directory as your working directory. 



######################################################################################### 

#########################################################################################


######################################################################################### 

#########################################################################################

###########1. Meta-Analyses.################
#IMPORTANT. See Supplementary Material for an overview of Literature and Effect Sizes.


#set working directory.
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Main Manuscript/Supplement")


###Additional Meta-Analysis (non-systematic)

###Willingness to help######


#Willingness to help effect size.
#get information on n, m, and sd for each study. 
Study <- c("Hendriks & Vingerhoets (2006)", "Hendriks et al. (2008)", "Balsters et al. (2013) - Study 2a", "Balsters et al. (2013) - Study 2b",
           "Vingerhoets et al. (2016) - Study 1", "Vingerhoets et al. (2016) - Study 2", "van de Ven et al. (2017) - Study 3",
           "Zickfeld & Schubert (2018) - Study 1", "Zickfeld & Schubert (2018) - Study 2","Küster (2018)",
           "Gracanin et al. (2018)", "Aragón & Clark (2018) - Study 6", "Zickfeld et al. (2019) - Pilot Study 1", "Zickfeld et al. (2019) - Pilot Study 2", "Zickfeld et al. (2019) - Pilot Study 3",
           "Bobowik et al. (2019) - Study 1", "Bobowik et al. (2019) - Study 2","Bobowik et al. (2019) - Study 3","Stadel et al. (2019)", "Vanman et al. (2019) - Study 2", "Vanman et al. (2019) - Study 3", "Zickfeld et al. (2019) - Pilot Study 4")
n1 <- c(312,1590,960,1664,1920,98,103,1045,637,300,77,64,616,427,140,29,41,100,140,1452,1776,149) #number of participants tear condition
m1 <- c(2.54,.38787,4.1359,1.875,5.90,4.10,3.72,4.83,5.20,6.32,658.604,5.11,2.93,3.57,3.49,5.21,4.12,4.78,4.71,4.29,5.3773,3.74) #mean tear condition
s1 <- c(.44,.899965,.39038,.71845,1.51,1.40,1.51,1.89,1.74,1.39,101.94,.80,1.89,1.83,1.76,1.42,1.59,1.55,.84,2.107,1.08439,1.49) #sd tear condition
n2 <- c(312,1590,960,1664,1920,98,114,1057,605,300,77,87,311,212,139,37,40,102,140,1452,1776,150) #number of participants non tear condition
m2 <- c(1.53,-.3878727,4.0072,1.7158,4.38,2.75,2.84,3.93,4.13,5.77,606.604,5.10,2.72,1.93,1.77,3.54,3.16,3.94,3.79,3.78,4.0434,3.07) #mean non tear condition
s2 <- c(.40,1.12,.4515,.73182,1.30,1.76,1.44,1.86,1.80,1.74,81.7578,.84,1.58,1.85,1.70,1.60,1.28,1.73,.96,2.105,1.33561,1.39) #sd non tear condition


help_es <- data.frame(Study = Study, n1 = n1, m1 = m1, s1 = s1, n2 = n2, m2 = m2, s2 = s2)


help_d <- escalc(measure="SMD", m1i=m1, sd1i=s1, n1i=n1,
                 m2i=m2, sd2i=s2, n2i=n2, data=help_es)

##add main effects.

##get data from main study. 
intent_d <- intent_d %>% 
  rename(Study = CountryName,
         m1 = mean_No.Tears,
         m2 = mean_Tears,
         s1 = sd_No.Tears,
         s2 = sd_Tears,
         n1 = n_No.Tears,
         n2 = n_Tears)

final_d <- bind_rows(help_d, intent_d)

#####

help_rma <- rma(yi, vi, slab = paste(Study), data=help_d)

help_rma

forest(help_rma) # -> Figure 1.2.1
confint(help_rma)


help_final_rma <- rma(yi, vi, slab = paste(Study), data=final_d)
help_final_rma

forest(help_final_rma) # -> Figure 4.6.1
confint(help_rma)


#trim and fill.

### carry out trim-and-fill analysis
taf <- trimfill(help_rma)
taf
### draw funnel plot with missing studies filled in
funnel(taf)
#no indication of publication bias.

##moderation.

##ADDING MODERATORS.

#2. population. 1 = general population, 0 = undergraduates
help_d$population <- c(0,1,0,0,0,1,0,1,0,0,0,1,1,1,1,0,0,1,0,1,1,1)

#2. design. 1 = within, 0 = between. 
help_d$design <- c(1,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0,0,0,1,1,1,1)

#3. type of tear manipulation. 0 = added; 1 = removed.
help_d$tearm <- c(0,NA,0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,1,1,1,0)

#population type.
help_rma <- rma(yi, vi, slab = paste(Study),mods=population, data=help_d)
summary(help_rma) #-> 0% accounted for. 
forest(help_rma)

#design type.
help_rma <- rma(yi, vi, slab = paste(Study),mods=design, data=help_d)
summary(help_rma) #-> 2.62% accounted for. 
forest(help_rma)


#tear type.
help_rma <- rma(yi, vi, slab = paste(Study),mods=tearm, data=help_d)
summary(help_rma) #-> 0% accounted for. 
forest(help_rma)
#################################

#Warmth Effect.####

#Warmth effect size.
#get information on n, m, and sd for each study. 
Study <- c("Vingerhoets et al. (2016) - Study 1", "Vingerhoets et al. (2016) - Study 2", "Van de ven et al. (2017) - Study 1", "Van de ven et al. (2017) - Study 2", "Van de ven et al. (2017) - Study 3", 
           "Zickfeld & Schubert (2018) - Study 1", "Zickfeld & Schubert (2018) - Study 2", "Zickfeld & Schubert (2018) - Unpublished Study",
           "Zickfed et al. (2018) - Study 1", "Zickfeld et al. (2018) - Study 2", "Küster (2018)", "Zickfeld et al. (2019) - Pilot Study 1",
           "Zickfeld et al. (2019) - Pilot Study 2", "Zickfeld et al. (2019) - Pilot Study 3","Bobowik et al. (2019) - Study 1", "Bobowik et al. (2019) - Study 2", "Bobowik et al. (2019) - Study 3",
           "Stadel et al. (2019)", "Zickfeld et al. (2019) - Pilot Study 4")
n1 <- c(1920,98, 1719,336,103,1042,593,444,291,1421,300,661,427,140,29,40,100,140,149) #number of participants tear condition
m1 <- c(6.11, 3.96, 3.8575,3.98,3.72,3.63,3.55,3.78,3.71,3.65,4.88,2.987,3.639,3.02,5.76,3.93,4.21,5.06,3.19) #mean tear condition
s1 <- c(.91, .98, 1.17711,1.23,.86,1.42,1.34,1.50,1.32,1.38,1.42,1.469,1.522,1.44,1.88,2.21,1.22,.60,1.25) #sd tear condition
n2 <- c(1920, 98, 1713,317,114,1057,634,222,240,1343,300,312,212,139,37,40,102,140,150) #number of participants non tear condition
m2 <- c(5.66, 3.46, 3.278,3.19,2.97,3.05,3.15,3.03,3.21,3.26,5.07,2.996,2.967,2.26,3.11,3.50,4.03,4.59,2.47) #mean non tear condition
s2 <- c(1.09, 1.24, 1.30359, 1.19, 1.01, 1.44, 1.34,1.37,1.28,1.38,1.71,1.229,1.3669,1.43,2.22,2,1.22,.71,1.35) #sd non tear condition

warm_es <- data.frame(Study = Study, n1 = n1, m1 = m1, s1 = s1, n2 = n2, m2 = m2, s2 = s2)


warm_d <- escalc(measure="SMD", m1i=m1, sd1i=s1, n1i=n1,
                 m2i=m2, sd2i=s2, n2i=n2, data=warm_es)

warm_rma <- rma(yi, vi, slab = paste(Study), data=warm_d)

warm_rma

forest(warm_rma) # -> Figure 1.2.2
confint(warm_rma)

#trim and fill.

### carry out trim-and-fill analysis
taf <- trimfill(warm_rma)
taf
### draw funnel plot with missing studies filled in
funnel(taf)
#indication of publication bias.

#################################

#Connectedness Effect.####

#Connectedness effect size.
#get information on n, m, and sd for each study. 
Study <- c("Vingerhoets et al. (2016) - Study 1", "Vingerhoets et al. (2016) - Study 2", "Zickfeld et al. (2019) - Pilot Study 1",
           "Zickfeld et al. (2019) - Pilot Study 2", "Zickfeld et al. (2019) - Pilot Study 3", "Stadel et al. (2019)", "Zickfeld et al. (2019) - Pilot Study 4")
n1 <- c(1920,98,661,427,140,140,149) #number of participants tear condition
m1 <- c(4.55,2.24,2.37,2.84,2.43,4.08,2.47) #mean tear condition
s1 <- c(1.30,1.36,1.488,1.77,1.71,.82,1.41) #sd tear condition
n2 <- c(1920,98,312,212,139,140,150) #number of participants non tear condition
m2 <- c(3.75,1.59,2.12,2.24,1.81,3.41,2.03) #mean non tear condition
s2 <- c(1.11,1.32,1.319,1.69,1.27,.91,1.24) #sd non tear condition

connect_es <- data.frame(Study = Study, n1 = n1, m1 = m1, s1 = s1, n2 = n2, m2 = m2, s2 = s2)


connect_d <- escalc(measure="SMD", m1i=m1, sd1i=s1, n1i=n1,
                    m2i=m2, sd2i=s2, n2i=n2, data=connect_es)

connect_rma <- rma(yi, vi, slab = paste(Study), data=connect_d)

connect_rma

forest(connect_rma) # -> Figure 1.2.3
confint(connect_rma)

### carry out trim-and-fill analysis
taf <- trimfill(connect_rma)
taf
### draw funnel plot with missing studies filled in
funnel(taf)

######################################
#Sadness Effect.

#Provine: T: M=5.29, SD=.64; NT: M=4.05, SD=.63, N=each 80*50
#Balster: T: M=549.686, SD = 143.008, N = 30, NT: M=608, SD = 158.748, N = 30.
#Reed: T: M=4.66, SD=1.90, NT: M=1.64, SD = 0.98, N=each 108
#Küster: T: M = 6.52 ,SD=.96; NT: M = 5.30, SD =1.72, N=each 
#Aragon (Study 2): T: M= 1.19, SD = .7348; NT: M=1.16, SD = .85732. Neach = 150.
#Aragon (Study 3): T: M= 1.35, SD = 1.5231; NT: M=1.10, SD = .761577, Neach = 232. 
#Aragon (Study 4): T: M= 1.16, SD= .8632; NT: M = 1.12, SD = .71937, Neach = 207.
#Aragon (Study 5): T: M = 1.28, SD = ; NT: M = 1.08, N?
#Aragon (Study 6): T: M = 1.44, SD = 1.624, N = 64; NT: M = 1.08, SD = .59059, N = 87.
#de Melo & Gratch: T: M = 7.93, SD = 1.707; NT: M = 6.18, SD = 1.707; Neach = 40.
#VVV (Study 1): T: M = 6.93, SD = 1.08; NT: M = 5.21, SD = 1.21; Neach = 1920
#VVV (Study 2): T: M = 4.07, SD = 1.01; NT: M = 2.94, SD = 1.49; Neach = 98.
#VMV (Study 2): T: M = 4.42, SD = 1.63, N = 336; NT: M = 3.13, SD = 1.72, N = 317
#ZS (Study 1): T: M = 4.76, SD = 1.773, N = 1052; NT: M = 3.45, SD = 1.887, N = 1066.
#ZS (Study 2): T: M = 4.43, SD = 1.739, N = 640; NT: M = 3.74, SD = 1.708, N = 619.
#ZS (Unpublished): T: M = 4.6239, SD = 1.70293, N = 444; NT: M = 3.5923, SD = 1.70753, N = 222.
#ZVSV (Study 1): T: M = 4.4948, SD = 1.61538, N = 291; NT: M = 3.0292, SD = 1.78182, N = 240.
#ZVSV (Study 2): T: M = 4.50, SD = 1.776, N = 1402; NT: M = 3.55, SD = 1.869, N = 1336.
#Pilot 1: T: M = 4.11, SD = 1.729, N = 616; NT: M = 3.32, SD = 1.470, N = 312.
#Pilot 2: T: M = 4.98, SD = 1.815, N = 427; NT: M = 2.66, SD = 1.657, N = 212.
#Pilot 3: T: M = 3.55, SD = 1.52, N = 140; NT: M = 1.65, SD = 1.25, N = 139.
#Ito et al. (2019): T: M = 46.3267, SD = 26.43826, N = 50; NT: M = 22.2917, SD = 21.67301, N = 48.
#Stadel et al. (2019): T: M = 5, SD = .72 N = 140; NT: M = 3.98, SD = .84, N = 140.
#Vanman et al. (2019, Study 1): T: M = 4.93, SD = 1.370, N = 783 ; NT: M = 3.99, SD = 1.61, N = 772.
#Scambler (2013): T: M = 5.16, SD = .94, N = 12*101; NT: M = 3.59, SD = .85, N = 12*101.

Study <- c("de Melo & Gratch (2009)",
           "Provine et al. (2009)",
           "Balsters et al. (2013) - Study 1",
           "Scambler (2013)",
           "Reed et al. (2015)",
           "Vingerhoets et al. (2016) - Study 1",
           "Vingerhoets et al. (2016) - Study 2",
           "Van de Ven et al. (2017) - Study 2",
           "Aragón & Clark (2018) - Study 2",
           "Aragón & Clark (2018) - Study 3",
           "Aragón & Clark (2018) - Study 4",
           "Aragón & Clark (2018) - Study 6",
           "Zickfeld & Schubert (2018) - Study 1",
           "Zickfeld & Schubert (2018) - Study 2",
           "Zickfeld & Schubert (2018) - Unpublished",
           "Zickfeld et al. (2018) - Study 1",
           "Zickfeld et al. (2018) - Study 2",
           "Küster (2018)",
           "Zickfeld et al. (2019) - Pilot 1",
           "Zickfeld et al. (2019) - Pilot 2",
           "Zickfeld et al. (2019) - Pilot 3",
           "Bobowik et al. (2019) - Study 1",
           "Bobowik et al. (2019) - Study 2",
           "Bobowik et al. (2019) - Study 3",
           "Ito et al. (2019)",
           "Stadel et al. (2019)",
           "Vanman et al. (2019) - Study 1",
           "Vanman et al. (2019) - Study 2",
           "Vanman et al. (2019) - Study 3",
           "Reed et al. (2019) - Study 1",
           "Reed et al. (2019) - Study 2",
           "Zickfeld et al. (2019) - Pilot 4")

n1 <- c(40, 80*50, 32*30, 12*101, 108,1920, 98, 336, 150, 232, 207, 64, 1052, 640, 444, 291, 1402, 300, 616, 427, 140,29,41,100, 50, 140, 783, 1452, 1776,37,41,149)
m1 <- c(7.93, 5.29, 608.1927, 5.16, 4.66, 6.93, 4.07, 4.42, 1.19, 1.35, 1.16, 1.44, 4.76, 4.43, 4.6239, 4.4948, 4.50, 6.52, 4.11, 4.98, 3.55, 6.62,5.76,6.30,46.3267, 5, 4.93, 5.76, 4.57,5.29,5.57,3.44)
s1 <- c(1.707, .64, 158.74817, .94, 1.90,1.08, 1.01, 1.63, .7348, 1.5231, .8632, 1.624, 1.773, 1.739, 1.70293, 1.61538, 1.776, .96, 1.729, 1.815, 1.52, .62,1.48,1.16,26.43826, .72, 1.370, 1.346, 2.074,1.53,1.54,1.49)
n2 <- c(40, 80*50, 32*30, 12*101, 108,1920, 98, 317, 150, 232, 207, 87, 1066, 619, 222, 240, 1336, 300, 312, 212, 139,37,40,102, 48, 140,772, 1452,1776,38,40,150)
m2 <- c(6.18, 4.05, 549.686, 3.59, 1.64,5.21, 2.94, 3.13, 1.16, 1.10, 1.12, 1.08, 3.45, 3.74, 3.5923, 3.0292, 3.55, 5.30, 3.32, 2.66, 1.65,4.05,2.68,4.71,22.2917, 3.98, 3.99, 5.08, 3.91,3.47,4.00,1.43)
s2 <- c(1.707, .63, 143.008, .85, .98,1.21, 1.49, 1.72, .85732, .7615, .71937, .59059, 1.887, 1.708, 1.70753, 1.78182, 1.869, 1.72, 1.47, 1.657, 1.25, 1.73,1.56,1.75,21.67301, .84, 1.61, 1.594, 2.074,1.71,1.79,1.26)

sad_es <- data.frame(Study = Study, n1 = n1, m1 = m1, s1 = s1, n2 = n2, m2 = m2, s2 = s2)


sad_d <- escalc(measure="SMD", m1i=m1, sd1i=s1, n1i=n1,
                m2i=m2, sd2i=s2, n2i=n2, data=sad_es)

sad_rma <- rma(yi, vi, slab = paste(Study), data=sad_d)

sad_rma

forest(sad_rma) # -> Figure 1.2.4
confint(sad_rma)

### carry out trim-and-fill analysis
taf <- trimfill(sad_rma)
taf
### draw funnel plot with missing studies filled in
funnel(taf)

####################################.
#Indirect effect via warmth.


#Indirect Effect Meta for Warmth (m) on Tears (x) and Competence (y).

#calculate indirect effects.

#Study 1 (VVV).
x1 <- matrix(c(1, 0.50, 0.55, 0.50, 1, 0.2740, 0.55, 0.2740, 1), ncol=3)
dimnames(x1) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Study 2 (VVV).
x2 <- matrix(c(1, .55, 0.3911, 0.55, 1, 0.2195, 0.3911, 0.2195, 1), ncol=3)
dimnames(x2) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Study 3 (VMV).
x3 <- matrix(c(1, .372, 0.286, .372, 1, 0.369, 0.286, 0.369, 1), ncol=3)
dimnames(x3) <- list( c("y", "m", "x"), c("y", "m", "x") )


#(ZS) Study 1.
x4 <- matrix(c(1, .509, .233, .509, 1, 0.198, .233, .198, 1), ncol=3)
dimnames(x4) <- list( c("y", "m", "x"), c("y", "m", "x") )
#(ZS) Study 2.
x5 <- matrix(c(1, .362, .291, .362, 1, 0.147, .291, 0.147, 1), ncol=3)
dimnames(x5) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 1.
x6 <- matrix(c(1, .432, .054, .432, 1, -0.003, .054, -0.003, 1), ncol=3)
dimnames(x6) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 2.
x7 <- matrix(c(1, .269, .386, .269, 1, 0.211, .386, 0.211, 1), ncol=3)
dimnames(x7) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 3.
x8 <- matrix(c(1, .330, .446, .330, 1, 0.257, .446, 0.257, 1), ncol=3)
dimnames(x8) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 4.
x9 <- matrix(c(1, .499, .2259, .499, 1, 0.2666, .2259, 0.2666, 1), ncol=3)
dimnames(x9) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Stadel et al. (2019). x-m: .3387, m-y: .530; x-y: .454
x10 <- matrix(c(1, .530, .454, .530, 1, 0.3387, .454, 0.3387, 1), ncol=3)
dimnames(x10) <- list( c("y", "m", "x"), c("y", "m", "x") )

Study <- c("VVV Study 1", "VVV Study 2", "VMV, Study 3", "ZS Study 1", "ZS Study 2", "Pilot Study 1", "Pilot Study 2","Pilot Study 3","Pilot Study 4","Stadel et al. (2019)")
#calculate indirect and direct effect.
ind <-indirectEffect(list(x1, x2, x3, x4, x5, x6, x7, x8,x9,x10), n=c(1920*2,98*2,208,2122,1248,927,639,279,299,16*140), standardized=TRUE)
ind

#meta analysis on indirect and direct effect.
indirect2 <- meta(ind[, c("ind_eff", "dir_eff")],
                  ind[, c("ind_var", "ind_dir_cov", "dir_var")], intervals.type="z")

summary(indirect2)
plot.meta(indirect2)
forest(rma(yi=ind_eff,vi=ind_var, slab=Study, data=ind))
rma(yi=ind_eff,vi=ind_var, slab=Study,data=ind)
forest(rma(yi=dir_eff,vi=dir_var, slab=Study, data=ind))


plot(indirect2, diag.panel=TRUE, main="Mediation meta-analysis", axis.label=c("Indirect Effect", "Direct Effect"))
## Forest plot for PD
forest(rma(yi=ind_eff,vi=ind_var, slab=Study, data=ind))
title("Forest plot of Indirect Effect")
## Forest plot for AL
forest(rma(yi=dir_eff,vi=dir_var, slab=Study, data=ind))
title("Forest plot of Direct Effect")

#get path a and b.
a <- c(x1[3,2],x2[3,2],x3[3,2],x4[3,2],x5[3,2],x6[3,2],x7[3,2],x8[3,2],x9[3,2])
n <- c(1920*2,98*2,208,2122,1248,927,639,279,16*140)

path_a <- data.frame(a,n)
path_a_rma <- rma(ri=a, ni=n, method="REML", measure="COR", data=path_a)
forest(path_a_rma)
path_a_rma
#0.2210  0.0371

b <- c(x1[1,2],x2[1,2],x3[1,2],x4[1,2],x5[1,2],x6[1,2],x7[1,2],x8[1,2],x9[1,2])
path_b <- data.frame(b,n)
path_b_rma <- rma(ri=b, ni=n, method="REML", measure="COR", data=path_b)
forest(path_b_rma)
path_b_rma
#0.4325  0.0328
sum(path_b$n)
.0328 * sqrt(11699)
.0371 * sqrt(11699)

#path c.
c <- c(x1[1,3],x2[1,3],x3[1,3],x4[1,3],x5[1,3],x6[1,3],x7[1,3],x8[1,3],x9[1,3])
n <- c(1920*2,98*2,208,2122,1248,927,639,279,16*140)

path_c <- data.frame(c,n)
path_c_rma <- rma(ri=c, ni=n, method="REML", measure="COR", data=path_c)
forest(path_c_rma)
path_c_rma
##################

#Indirect effect via connectedness.


#Indirect Effect Meta for Connectedness (m) on Tears (x) and Help (y).

#calculate indirect effects.
#library(OpenMx)
#install.packages("metaSEM")
#library(metaSEM)

#Study 1 (VVV).
x1 <- matrix(c(1, 0.77, 0.5623, 0.77, 1, 0.4472, 0.5623, 0.4472, 1), ncol=3)
dimnames(x1) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Study 2 (VVV).
x2 <- matrix(c(1, .49, 0.3911, 0.49, 1, 0.238, 0.3911, 0.238, 1), ncol=3)
dimnames(x2) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 1.
x3 <- matrix(c(1, .475, .054, .475, 1, 0.083, .054, 0.083, 1), ncol=3)
dimnames(x3) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 2.
x4 <- matrix(c(1, .431, .386, .431, 1, 0.162, .386, 0.162, 1), ncol=3)
dimnames(x4) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 3.
x5 <- matrix(c(1, .457, .446, .457, 1, 0.201, .446, 0.201, 1), ncol=3)
dimnames(x5) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 4.
x6 <- matrix(c(1, .302, .2259, .302, 1, 0.1629, .2259, 0.1629, 1), ncol=3)
dimnames(x6) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Stadel et al. (2019), x-m: .3593; m-y: .686, x-y: .454
x7 <- matrix(c(1, .686, .454, .686, 1, 0.3593, .454, 0.3593, 1), ncol=3)
dimnames(x7) <- list( c("y", "m", "x"), c("y", "m", "x") )

Study <- c("VVV Study 1", "VVV Study 2","Pilot Study 1", "Pilot Study 2", "Pilot Study 3","Pilot Study 4", "Stadel et al. (2019)")
#calculate indirect and direct effect.
ind <-indirectEffect(list(x1, x2, x3, x4,x5,x6,x7), n=c(1920*2,98*2,927,639,279,299,16*140), standardized=TRUE)
ind

#meta analysis on indirect and direct effect.
indirect2 <- meta(ind[, c("ind_eff", "dir_eff")],
                  ind[, c("ind_var", "ind_dir_cov", "dir_var")], intervals.type="z")

summary(indirect2)
plot.meta(indirect2)
forest(rma(yi=ind_eff,vi=ind_var, slab=Study, data=ind))
rma(yi=ind_eff,vi=ind_var, slab=Study,data=ind)
forest(rma(yi=dir_eff,vi=dir_var, slab=Study, data=ind))

plot(indirect2, diag.panel=TRUE, main="Mediation meta-analysis", axis.label=c("Indirect Effect", "Direct Effect"))
## Forest plot for PD
forest(rma(yi=ind_eff,vi=ind_var, slab=Study, data=ind))
title("Forest plot of Indirect Effect")
## Forest plot for AL
forest(rma(yi=dir_eff,vi=dir_var, slab=Study, data=ind))
title("Forest plot of Direct Effect")

###########################


#Indirect effect via sadness.


#Indirect Effect Meta for Sadness (m) on Tears (x) and Help (y).

#calculate indirect effects.
#library(OpenMx)
#install.packages("metaSEM")
#library(metaSEM)

#Study 1 (VVV).
x1 <- matrix(c(1, 0.72, 0.5508, 0.72, 1, 0.5623, 0.5508, 0.5623, 1), ncol=3)
dimnames(x1) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Study 2 (VVV).
x2 <- matrix(c(1, .52, 0.3911, 0.52, 1, 0.4027, 0.3911, 0.4027, 1), ncol=3)
dimnames(x2) <- list( c("y", "m", "x"), c("y", "m", "x") )
#ZS Study 1.
x3 <- matrix(c(1, .291, .233, .291, 1, 0.337, .233, 0.337, 1), ncol=3)
dimnames(x3) <- list( c("y", "m", "x"), c("y", "m", "x") )
#ZS Study 2.
x4 <- matrix(c(1, .401, .291, .401, 1, 0.198, .291, 0.198, 1), ncol=3)
dimnames(x4) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 1.
x5 <- matrix(c(1, .344, .054, .344, 1, 0.221, .054, 0.221, 1), ncol=3)
dimnames(x5) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 2.
x6 <- matrix(c(1, .510, .386, .510, 1, 0.527, .386, 0.527, 1), ncol=3)
dimnames(x6) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 3.
x7 <- matrix(c(1, .661, .446, .661, 1, 0.566, .446, 0.566, 1), ncol=3)
dimnames(x7) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Pilot Study 4.
x8 <- matrix(c(1, .4174, .2259, .4174, 1, 0.5895, .2259, 0.5895, 1), ncol=3)
dimnames(x8) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Stadel et al. (2019). x-m: .545, m-y: .605; x-y: .454
x9 <- matrix(c(1, .605, .454, .605, 1, 0.545, .454, 0.545, 1), ncol=3)
dimnames(x9) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Vanman et al. (2019) - Study 2. m-y: .445, x-m; .2241, x-y; .119
x10 <- matrix(c(1, 0.445, 0.119, 0.445, 1, 0.2241, 0.119, 0.2241, 1), ncol=3)
dimnames(x10) <- list( c("y", "m", "x"), c("y", "m", "x") )
#Vanman et al. (2019) - Study 3. m-y: .315, x-m; .158, x-y; .4819
x11 <- matrix(c(1, 0.315, 0.4819, 0.315, 1, 0.158, 0.4819, 0.158, 1), ncol=3)
dimnames(x11) <- list( c("y", "m", "x"), c("y", "m", "x") )

Study <- c("VVV Study 1", "VVV Study 2","ZS Study 1","ZS Study 2","Pilot Study 1", "Pilot Study 2", "Pilot Study 3","Pilot Study 4","Stadel et al. (2019)","Vanman et al. (2019) - Study 2", "Vanman et al. (2019) - Study 3")
#calculate indirect and direct effect.
ind <-indirectEffect(list(x1, x2, x3, x4,x5,x6,x7,x8,x9,x10,x11), n=c(1920*2,98*2,2118,1258,927,639,279,299,16*140,2904,3552), standardized=TRUE)
ind

#meta analysis on indirect and direct effect.
indirect2 <- meta(ind[, c("ind_eff", "dir_eff")],
                  ind[, c("ind_var", "ind_dir_cov", "dir_var")], intervals.type="z")


plot(indirect2, diag.panel=TRUE, main="Mediation meta-analysis", axis.label=c("Indirect Effect", "Direct Effect"))
## Forest plot for PD
forest(rma(yi=ind_eff,vi=ind_var, slab=Study, data=ind))
title("Forest plot of Indirect Effect")
## Forest plot for AL
forest(rma(yi=dir_eff,vi=dir_var, slab=Study, data=ind))
title("Forest plot of Direct Effect")

summary(indirect2)
plot.meta(indirect2)
forest(rma(yi=ind_eff,vi=ind_var, slab=Study, data=ind))
rma(yi=ind_eff,vi=ind_var, slab=Study,data=ind)
forest(rma(yi=dir_eff,vi=dir_var, slab=Study, data=ind))



######################################################################################### 
#2. Pilot Studies
#########################################################################################


###########2.1 Pilot Study - Situation Ratings##############

#1.2. Set working directory. !! Please adjust this path to fit your system. 
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/")


#2. Load Dataset.
#load dataset.
#if not working download manually from https://osf.io/gmp8b/
cct_pilot <- getOSFfile("gmp8b")
cct_pilot <- read.csv(cct_pilot)


#3. Rename and Recode Variables.

#3.1 Rename variables.
cct_pilot <- cct_pilot %>% 
              rename(participantID = participantID, Valence_Rating = Valence, Context_Rating = Context,
                     Context_Episode = Setting, Type_Episode = Type, Cry_Rating = Cry) %>% 
              mutate(Version_Episode = case_when(Episode <= 46 ~ 1,
                                                  Episode > 46 ~ 2))

#3.2 Recode variables to factors and add levels. 
cct_pilot$Valence_Episode <- as.factor(cct_pilot$Valence_Episode)
levels(cct_pilot$Valence_Episode)[1] <- "Negative"
levels(cct_pilot$Valence_Episode)[2] <- "Positive"
cct_pilot$Version_Episode <- as.factor(cct_pilot$Version_Episode)
levels(cct_pilot$Version_Episode)[1] <- "Version1"
levels(cct_pilot$Version_Episode)[2] <- "Version2"
cct_pilot$Type_Episode <- as.factor(cct_pilot$Type_Episode)
levels(cct_pilot$Type_Episode)[1] <- "Reunion"
levels(cct_pilot$Type_Episode)[2] <- "Kindness"
levels(cct_pilot$Type_Episode)[3] <- "Wedding"
levels(cct_pilot$Type_Episode)[4] <- "Patriotism"
levels(cct_pilot$Type_Episode)[5] <- "Religion"
levels(cct_pilot$Type_Episode)[6] <- "Success"
levels(cct_pilot$Type_Episode)[7] <- "Admired person"
levels(cct_pilot$Type_Episode)[8] <- "Award ceremony"
levels(cct_pilot$Type_Episode)[9] <- "Overcoming obstacle"
levels(cct_pilot$Type_Episode)[10] <- "Nature"
levels(cct_pilot$Type_Episode)[11] <- "Music"
levels(cct_pilot$Type_Episode)[12] <- "Arts"
levels(cct_pilot$Type_Episode)[13] <- "Cute"
levels(cct_pilot$Type_Episode)[14] <- "Relationship Loss"
levels(cct_pilot$Type_Episode)[15] <- "Funeral"
levels(cct_pilot$Type_Episode)[16] <- "Saying Goodbye"
levels(cct_pilot$Type_Episode)[17] <- "Other's suffering"
levels(cct_pilot$Type_Episode)[18] <- "Tragedy"
levels(cct_pilot$Type_Episode)[19] <- "Failure"
levels(cct_pilot$Type_Episode)[20] <- "Guilt"
levels(cct_pilot$Type_Episode)[21] <- "Rejection"
levels(cct_pilot$Type_Episode)[22] <- "Anger"
levels(cct_pilot$Type_Episode)[23] <- "Humiliation"
cct_pilot$Context_Episode <- as.factor(cct_pilot$Context_Episode)
levels(cct_pilot$Context_Episode)[1] <- "Public"
levels(cct_pilot$Context_Episode)[2] <- "Private"
cct_pilot$Episode <- as.factor(cct_pilot$Episode)
levels(cct_pilot$Episode)[1] <- "Reunion 1 Pb"
levels(cct_pilot$Episode)[2] <- "Reunion 1 Pr"
levels(cct_pilot$Episode)[3] <- "Kindness 1 Pb"
levels(cct_pilot$Episode)[4] <- "Kindness 1 Pr"
levels(cct_pilot$Episode)[5] <- "Wedding 1 Pb"
levels(cct_pilot$Episode)[6] <- "Wedding 1 Pr"
levels(cct_pilot$Episode)[7] <- "Patriotism 1 Pb"
levels(cct_pilot$Episode)[8] <- "Patriotism 1 Pr"
levels(cct_pilot$Episode)[9] <- "Religion 1 Pb"
levels(cct_pilot$Episode)[10] <- "Religion 1 Pr"
levels(cct_pilot$Episode)[11] <- "Success 1 Pb"
levels(cct_pilot$Episode)[12] <- "Success 1 Pr"
levels(cct_pilot$Episode)[13] <- "Admired Person 1 Pb"
levels(cct_pilot$Episode)[14] <- "Admired Person 1 Pr"
levels(cct_pilot$Episode)[15] <- "Award Ceremony 1 Pb"
levels(cct_pilot$Episode)[16] <- "Award Ceremony 1 Pr"
levels(cct_pilot$Episode)[17] <- "Overcoming Obstacle 1 Pb"
levels(cct_pilot$Episode)[18] <- "Overcoming Obstacle 1 Pr"
levels(cct_pilot$Episode)[19] <- "Nature 1 Pb"
levels(cct_pilot$Episode)[20] <- "Nature 1 Pr"
levels(cct_pilot$Episode)[21] <- "Music 1 Pb"
levels(cct_pilot$Episode)[22] <- "Music 1 Pr"
levels(cct_pilot$Episode)[23] <- "Arts 1 Pb"
levels(cct_pilot$Episode)[24] <- "Arts 1 Pr"
levels(cct_pilot$Episode)[25] <- "Cute 1 Pb"
levels(cct_pilot$Episode)[26] <- "Cute 1 Pr"
levels(cct_pilot$Episode)[27] <- "Relationship Loss 1 Pb"
levels(cct_pilot$Episode)[28] <- "Relationship Loss 1 Pr"
levels(cct_pilot$Episode)[29] <- "Funeral 1 Pb"
levels(cct_pilot$Episode)[30] <- "Funeral 1 Pr"
levels(cct_pilot$Episode)[31] <- "Saying Goodbye 1 Pb"
levels(cct_pilot$Episode)[32] <- "Saying Goodbye 1 Pr"
levels(cct_pilot$Episode)[33] <- "Other's Suffering 1 Pb"
levels(cct_pilot$Episode)[34] <- "Other's Suffering 1 Pr"
levels(cct_pilot$Episode)[35] <- "Tragedy 1 Pb"
levels(cct_pilot$Episode)[36] <- "Tragedy 1 Pr"
levels(cct_pilot$Episode)[37] <- "Failure 1 Pb"
levels(cct_pilot$Episode)[38] <- "Failure 1 Pr"
levels(cct_pilot$Episode)[39] <- "Guilt 1 Pb"
levels(cct_pilot$Episode)[40] <- "Guilt 1 Pr"
levels(cct_pilot$Episode)[41] <- "Rejection 1 Pb"
levels(cct_pilot$Episode)[42] <- "Rejection 1 Pr"
levels(cct_pilot$Episode)[43] <- "Anger 1 Pb"
levels(cct_pilot$Episode)[44] <- "Anger 1 Pr"
levels(cct_pilot$Episode)[45] <- "Humilation 1 Pb"
levels(cct_pilot$Episode)[46] <- "Humilation 1 Pr"
levels(cct_pilot$Episode)[47] <- "Reunion 2 Pb"
levels(cct_pilot$Episode)[48] <- "Reunion 2 Pr"
levels(cct_pilot$Episode)[49] <- "Kindness 2 Pb"
levels(cct_pilot$Episode)[50] <- "Kindness 2 Pr"
levels(cct_pilot$Episode)[51] <- "Wedding 2 Pb"
levels(cct_pilot$Episode)[52] <- "Wedding 2 Pr"
levels(cct_pilot$Episode)[53] <- "Patriotism 2 Pb"
levels(cct_pilot$Episode)[54] <- "Patriotism 2 Pr"
levels(cct_pilot$Episode)[55] <- "Religion 2 Pb"
levels(cct_pilot$Episode)[56] <- "Religion 2 Pr"
levels(cct_pilot$Episode)[57] <- "Success 2 Pb"
levels(cct_pilot$Episode)[58] <- "Success 2 Pr"
levels(cct_pilot$Episode)[59] <- "Admired Person 2 Pb"
levels(cct_pilot$Episode)[60] <- "Admired Person 2 Pr"
levels(cct_pilot$Episode)[61] <- "Award Ceremony 2 Pb"
levels(cct_pilot$Episode)[62] <- "Award Ceremony 2 Pr"
levels(cct_pilot$Episode)[63] <- "Overcoming Obstacle 2 Pb"
levels(cct_pilot$Episode)[64] <- "Overcoming Obstacle 2 Pr"
levels(cct_pilot$Episode)[65] <- "Nature 2 Pb"
levels(cct_pilot$Episode)[66] <- "Nature 2 Pr"
levels(cct_pilot$Episode)[67] <- "Music 2 Pb"
levels(cct_pilot$Episode)[68] <- "Music 2 Pr"
levels(cct_pilot$Episode)[69] <- "Arts 2 Pb"
levels(cct_pilot$Episode)[70] <- "Arts 2 Pr"
levels(cct_pilot$Episode)[71] <- "Cute 2 Pb"
levels(cct_pilot$Episode)[72] <- "Cute 2 Pr"
levels(cct_pilot$Episode)[73] <- "Relationship Loss 2 Pb"
levels(cct_pilot$Episode)[74] <- "Relationship Loss 2 Pr"
levels(cct_pilot$Episode)[75] <- "Funeral 2 Pb"
levels(cct_pilot$Episode)[76] <- "Funeral 2 Pr"
levels(cct_pilot$Episode)[77] <- "Saying Goodbye 2 Pb"
levels(cct_pilot$Episode)[78] <- "Saying Goodbye 2 Pr"
levels(cct_pilot$Episode)[79] <- "Other's Suffering 2 Pb"
levels(cct_pilot$Episode)[80] <- "Other's Suffering 2 Pr"
levels(cct_pilot$Episode)[81] <- "Tragedy 2 Pb"
levels(cct_pilot$Episode)[82] <- "Tragedy 2 Pr"
levels(cct_pilot$Episode)[83] <- "Failure 2 Pb"
levels(cct_pilot$Episode)[84] <- "Failure 2 Pr"
levels(cct_pilot$Episode)[85] <- "Guilt 2 Pb"
levels(cct_pilot$Episode)[86] <- "Guilt 2 Pr"
levels(cct_pilot$Episode)[87] <- "Rejection 2 Pb"
levels(cct_pilot$Episode)[88] <- "Rejection 2 Pr"
levels(cct_pilot$Episode)[89] <- "Anger 2 Pb"
levels(cct_pilot$Episode)[90] <- "Anger 2 Pr"
levels(cct_pilot$Episode)[91] <- "Humilation 2 Pb"
levels(cct_pilot$Episode)[92] <- "Humilation 2 Pr"

cct_pilot$Gender <- as.factor(cct_pilot$Gender)
levels(cct_pilot$Gender)[1] <- "Female"
levels(cct_pilot$Gender)[2] <- "Male"
levels(cct_pilot$Gender)[3] <- "Other/Prefer not to say"

#4. Explore Sample

#4.1 Check descriptives.
describe(cct_pilot)

#4.2 Summarise number of participants by gender and nation.
cct_pilot %>% 
  group_by(Gender) %>%
  summarise(no_rows = n()/92)
cct_pilot %>% 
  group_by(Nation) %>%
  summarise(no_rows = n()/92)

#4.3 Compute correlation between -> Table 2.1.2
cct_cor <- cct_pilot %>% 
            select(Valence_Rating, Context_Rating, Cry_Rating)
#save correlation table.
apa.cor.table(cct_cor, filename="cct_cor_table.doc")

#5. Main Analyses.

#5.1 Test for Valence. 
mod_valence <- lmer(Valence_Rating ~ Valence_Episode + (1|participantID), data=cct_pilot)
summary(mod_valence)
anova(mod_valence)
confint(mod_valence)
emmeans(mod_valence, "Valence_Episode")

#Plot Valence. 
ggbarplot(cct_pilot, x = "Valence_Episode", y ="Valence_Rating",
          fill = "Valence_Episode", color = "Valence_Episode", palette = c("red4", "#E7B800"),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Valence Rating", xlab = "Valence of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7)


#5.2 Test for Context.
mod_context <- lmer(Context_Rating ~ Context_Episode + (1|participantID), data=cct_pilot)
summary(mod_context)
anova(mod_context)
confint(mod_context)
emmeans(mod_context, "Context_Episode")

#Plot context.
ggbarplot(cct_pilot, x = "Context_Episode", y ="Context_Rating",
          fill = "Context_Episode", color = "Context_Episode", palette = c("red4", "#E7B800"),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Context Rating", xlab = "Context of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7)

#5.3 Test for Crying.
#first model with Context and Valence.
mod_cry <- lmer(Cry_Rating ~ Context_Episode + Valence_Episode + Context_Episode*Valence_Episode + (1|participantID), data=cct_pilot)
summary(mod_cry)
anova(mod_cry)
confint(mod_cry)
emmeans(mod_cry, "Context_Episode")
emmeans(mod_cry, "Valence_Episode")

#now model with Type and Context.
mod_cry2 <- lmer(Cry_Rating ~ Type_Episode + Context_Episode + Type_Episode*Context_Episode + (1|participantID), data=cct_pilot)
summary(mod_cry2)
emmeans(mod_cry2, "Type_Episode")

#Plot interactions with the Cry Rating.
ggbarplot(cct_pilot, x = "Context_Episode", y ="Cry_Rating",
          fill = "Context_Episode", color = "Context_Episode", palette = c("red4", "#E7B800"),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Context Rating", xlab = "Context of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =4)

ggbarplot(cct_pilot, x = "Valence_Episode", y ="Cry_Rating",
          fill = "Valence_Episode", color = "Valence_Episode", palette = c("red4", "#E7B800"),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Valence Rating", xlab = "Valence of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =4.5)

ggbarplot(cct_pilot, x = "Type_Episode", y ="Cry_Rating",
          fill = "Type_Episode", color = "Type_Episode",
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Cry Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggbarplot(cct_pilot, x = "Type_Episode", y ="Cry_Rating",
          fill = "Context_Episode", color = "Context_Episode", position = position_dodge(.8),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Cry Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggbarplot(cct_pilot, x = "Episode", y ="Cry_Rating",
          fill = "Episode", color = "Episode",
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Cry Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#5.4 Standardize Ratings.

###standardized on group mean.


#Standardize Valence Rating.
cct_pilot <- cct_pilot %>%
  mutate(Valence_z = (Valence_Rating - mean(Valence_Rating))/sd(Valence_Rating))

#Plot Standardized Valence Rating -> Figure 2.1.1
ggbarplot(cct_pilot, x = "Type_Episode", y ="Valence_z",
          fill = "Context_Episode", color = "Version_Episode", position = position_dodge(.8),
          add = c("mean_ci"), 
          ylab = "Standardized Valence Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#first get mean and sd.
cct_pilot %>% 
  group_by(Valence_Episode) %>% 
  summarise(mean_cry = mean(Cry_Rating, na.rm=TRUE), sd_cry = sd(Cry_Rating, na.rm=TRUE))

#now standardize on group mean. 
cct_pilot <- cct_pilot %>%
                mutate(Cry_z = case_when(
                                Valence_Episode == "Negative" ~ (Cry_Rating - 4.05)/2.25,
                                Valence_Episode == "Positive" ~ (Cry_Rating - 2.94)/2.11))

#Plot Standardized Crying Score. -> Figure 2.1.2
ggbarplot(cct_pilot, x = "Type_Episode", y ="Cry_z",
          fill = "Context_Episode", color = "Version_Episode", position = position_dodge(.8),
          add = c("mean_ci"), 
          ylab = "Standardized Cry Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#now standardize Context rating. 
cct_pilot <- cct_pilot %>%
  mutate(Context_z = (Context_Rating - mean(Context_Rating))/sd(Context_Rating))

#Plot Context differences -> Figure 2.1.3
ggbarplot(cct_pilot, x = "Type_Episode", y ="Context_diff",
          fill = "Version_Episode", color = "Version_Episode", position = position_dodge(.8),
          add = c("mean_ci"), error.plot="errorbar", 
          ylab = "Standardized Context Difference", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#5.5 Create Overall Index.
#create overall score. 
#this scores is based on total z-score for crying rating + relative z-score valence rating, 
#and difference between public ratings between both version.

#5.5.1 Get mean scores and sds for context ratings.
context_measures <- cct_pilot %>% 
                      group_by(Type_Episode, Version_Episode, Context_Episode) %>% 
                      summarise(mean_context = mean(Context_Rating, na.rm=TRUE), sd_context = sd(Context_Rating, na.rm=TRUE))

#Now reshape. 
split <- dcast(context_measures, Type_Episode + Version_Episode ~ Context_Episode, value.var = "mean_context")
split1 <- dcast(context_measures, Type_Episode + Version_Episode ~ Context_Episode, value.var = "sd_context")

split <- merge(split, split1, by=c("Type_Episode", "Version_Episode"))

split <- split %>% 
          mutate(Context_diff = (Private.x - Public.x)/sqrt((Public.y^2 + Private.y^2)/2))

cct_pilot <- merge(cct_pilot, split, by=c("Type_Episode", "Version_Episode"))

#Compute Index.
cct_pilot <- cct_pilot %>% 
              select(-c("Public.x", "Private.x", "Public.y", "Private.y")) %>% 
              mutate(Index = Cry_z + abs(Valence_z) + Context_diff)

#Plot Index -> Figure 2.1.4
ggbarplot(cct_pilot, x = "Type_Episode", y ="Index",
          fill = "Version_Episode", color = "Version_Episode", 
          add = c("mean_ci"), error.plot="errorbar", position = position_dodge(.8),
          ylab = "Standardized Index Sum", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


######################################################################################### 

#########################################################################################


###############################################################
#2.2 Pilot Study - Situation Ratings Neutral
###############################################################

#1.Load dataset.
#if not working download manually from https://osf.io/r4wfz/
cct_pilot_neu <- getOSFfile("r4wfz")
cct_pilot_neu <- read.csv(cct_pilot_neu)


#2 Renaming & Recoding.

#2.1 First do renaming.
cct_pilot_neu <- cct_pilot_neu %>% 
  rename(Valence_Rating = Valence, Context_Rating = Context,
         Context_Episode = Setting, Type_Episode = Type, Cry_Rating = Cry) 

###FOR WINDOWS MACHINES.
cct_pilot_neu <- cct_pilot_neu %>% 
  rename(participantID = ?..participantID)


#2.2 Now recode to factors and create levels.
cct_pilot_neu$Type_Episode <- as.factor(cct_pilot_neu$Type_Episode)
levels(cct_pilot_neu$Type_Episode)[1] <- "Sweater"
levels(cct_pilot_neu$Type_Episode)[2] <- "Payment"
levels(cct_pilot_neu$Type_Episode)[3] <- "Grocery Shopping"
levels(cct_pilot_neu$Type_Episode)[4] <- "Water"
levels(cct_pilot_neu$Type_Episode)[5] <- "Newspaper"
levels(cct_pilot_neu$Type_Episode)[6] <- "Phone Call"
levels(cct_pilot_neu$Type_Episode)[7] <- "Waiting"
levels(cct_pilot_neu$Type_Episode)[8] <- "Airport"
levels(cct_pilot_neu$Type_Episode)[9] <- "Package"
levels(cct_pilot_neu$Type_Episode)[10] <- "Salad"
levels(cct_pilot_neu$Type_Episode)[11] <- "Weekend"
levels(cct_pilot_neu$Type_Episode)[12] <- "Wikipedia"
levels(cct_pilot_neu$Type_Episode)[13] <- "Present"
levels(cct_pilot_neu$Type_Episode)[14] <- "Mandala"
levels(cct_pilot_neu$Type_Episode)[15] <- "Coffee"
levels(cct_pilot_neu$Type_Episode)[16] <- "Pine Tree"
levels(cct_pilot_neu$Type_Episode)[17] <- "Consumer Survey"
levels(cct_pilot_neu$Type_Episode)[18] <- "Birds"
levels(cct_pilot_neu$Type_Episode)[19] <- "Print"
levels(cct_pilot_neu$Type_Episode)[20] <- "Wash"
cct_pilot_neu$Context_Episode <- as.factor(cct_pilot_neu$Context_Episode)
levels(cct_pilot_neu$Context_Episode)[1] <- "Public"
levels(cct_pilot_neu$Context_Episode)[2] <- "Private"
cct_pilot_neu$Episode <- as.factor(cct_pilot_neu$Episode)
levels(cct_pilot_neu$Episode)[1] <- "Sweater Pb"
levels(cct_pilot_neu$Episode)[2] <- "Payment Pb"
levels(cct_pilot_neu$Episode)[3] <- "Grocery Shopping Pb"
levels(cct_pilot_neu$Episode)[4] <- "Water Pb"
levels(cct_pilot_neu$Episode)[5] <- "Newspaper Pb"
levels(cct_pilot_neu$Episode)[6] <- "Phone Call Pb"
levels(cct_pilot_neu$Episode)[7] <- "Waiting Pb"
levels(cct_pilot_neu$Episode)[8] <- "Airport Pb"
levels(cct_pilot_neu$Episode)[9] <- "Package Pb"
levels(cct_pilot_neu$Episode)[10] <- "Salad Pb"
levels(cct_pilot_neu$Episode)[11] <- "Weekend Pb"
levels(cct_pilot_neu$Episode)[12] <- "Wikipedia Pb"
levels(cct_pilot_neu$Episode)[13] <- "Present Pb"
levels(cct_pilot_neu$Episode)[14] <- "Mandala Pb"
levels(cct_pilot_neu$Episode)[15] <- "Coffee Pb"
levels(cct_pilot_neu$Episode)[16] <- "Pine Tree Pb"
levels(cct_pilot_neu$Episode)[17] <- "Consumer Survey Pb"
levels(cct_pilot_neu$Episode)[18] <- "Birds Pb"
levels(cct_pilot_neu$Episode)[19] <- "Print Pb"
levels(cct_pilot_neu$Episode)[20] <- "Wash Pb"
levels(cct_pilot_neu$Episode)[21] <- "Sweater Pr"
levels(cct_pilot_neu$Episode)[22] <- "Payment Pr"
levels(cct_pilot_neu$Episode)[23] <- "Grocery Shopping Pr"
levels(cct_pilot_neu$Episode)[24] <- "Water Pr"
levels(cct_pilot_neu$Episode)[25] <- "Newspaper Pr"
levels(cct_pilot_neu$Episode)[26] <- "Phone Call Pr"
levels(cct_pilot_neu$Episode)[27] <- "Waiting Pr"
levels(cct_pilot_neu$Episode)[28] <- "Airport Pr"
levels(cct_pilot_neu$Episode)[29] <- "Package Pr"
levels(cct_pilot_neu$Episode)[30] <- "Salad Pr"
levels(cct_pilot_neu$Episode)[31] <- "Weekend Pr" 
levels(cct_pilot_neu$Episode)[32] <- "Wikipedia Pr"
levels(cct_pilot_neu$Episode)[33] <- "Present Pr"
levels(cct_pilot_neu$Episode)[34] <- "Mandala Pr"
levels(cct_pilot_neu$Episode)[35] <- "Coffee Pr"
levels(cct_pilot_neu$Episode)[36] <- "Pine Tree Pr"
levels(cct_pilot_neu$Episode)[37] <- "Consumer Survey Pr"
levels(cct_pilot_neu$Episode)[38] <- "Birds Pr"
levels(cct_pilot_neu$Episode)[39] <- "Print Pr"
levels(cct_pilot_neu$Episode)[40] <- "Wash Pr"

cct_pilot_neu$Gender <- as.factor(cct_pilot_neu$Gender)
levels(cct_pilot_neu$Gender)[1] <- "Female"
levels(cct_pilot_neu$Gender)[2] <- "Male"
levels(cct_pilot_neu$Gender)[3] <- "Other/Prefer not to say"


#3. Explore Sample. 

#3.1 Get descriptives.
describe(cct_pilot_neu)

#3.2 Check number of pps per gender and nation. 
cct_pilot_neu %>% 
  group_by(Gender) %>%
  summarise(no_rows = n()/40)
cct_pilot_neu %>% 
  group_by(Nation) %>%
  summarise(no_rows = n()/40)

#3.3 Check mean,min,max age of pps.
cct_pilot_neu %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age), max = max(Age))

#3.4 Create correlation table for main variables. 
cct_cor_neu <- cct_pilot_neu %>% 
  select(Valence_Rating, Context_Rating, Cry_Rating)

#correlation table. -> Table 2.2.2
apa.cor.table(cct_cor_neu, filename="cct_cor_neu_table.doc")


#4. Main Analyses.

#4.1 Test for Valence.

#Main model.
mod_valence_neu <- lmerTest::lmer(Valence_Rating ~ Type_Episode + (1|participantID), data=cct_pilot_neu)
summary(mod_valence_neu)
anova(mod_valence_neu)
confint(mod_valence_neu)
emmeans(mod_valence_neu, "Type_Episode")

#Plot Valence Rating. -> Figure 2.2.1
ggbarplot(cct_pilot_neu, x = "Type_Episode", y ="Valence_Rating",
          fill = "Type_Episode", color = "Type_Episode", 
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Valence Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#4.2 Test for Context.
#Main Model
mod_context_neu <- lmer(Context_Rating ~ Context_Episode + (1|participantID), data=cct_pilot_neu)
summary(mod_context_neu)
confint(mod_context_neu)
emmeans(mod_context_neu, "Context_Episode")

#Plot Context Rating. -> Figure 2.2.2
ggbarplot(cct_pilot_neu, x = "Episode", y ="Context_Rating",
          fill = "Episode", color = "Episode", 
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Context Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#4.3 Test for Crying.
#Main Model
mod_cry_neu <- lmerTest::lmer(Cry_Rating ~ Type_Episode + (1|participantID), data=cct_pilot_neu)
summary(mod_cry_neu)
anova(mod_cry_neu)
confint(mod_cry_neu)
emmeans(mod_cry_neu, "Type_Episode")

#Plot for Crying Rating. -> Figure 2.2.3
ggbarplot(cct_pilot_neu, x = "Type_Episode", y ="Cry_Rating",
          fill = "Type_Episode", color = "Type_Episode", 
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Cry Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##########.
#5. Standardize Ratings.

#5.1 First Valence Rating. Midpoint of scale is 4. 
cct_pilot_neu <- cct_pilot_neu %>%
  mutate(Valence_z = Valence_Rating - 4)


#5.2 Get mean scores and sds for context ratings.
context_measures <- cct_pilot_neu %>% 
  group_by(Type_Episode, Context_Episode) %>% 
  summarise(mean_context = mean(Context_Rating, na.rm=TRUE), sd_context = sd(Context_Rating, na.rm=TRUE))

#now reshape. 
split <- dcast(context_measures, Type_Episode ~ Context_Episode, value.var = "mean_context")
split1 <- dcast(context_measures, Type_Episode ~ Context_Episode, value.var = "sd_context")

split <- merge(split, split1, by=c("Type_Episode"))

split <- split %>% 
  mutate(Context_diff = (Private.x - Public.x)/sqrt((Public.y^2 + Private.y^2)/2))

cct_pilot_neu <- merge(cct_pilot_neu, split, by=c("Type_Episode"))

#Compute Index Rating. 
cct_pilot_neu <- cct_pilot_neu %>% 
  select(-c("Public.x", "Private.x", "Public.y", "Private.y")) %>% 
  mutate(Index = (Cry_Rating * -1) + (Valence_z * -1) + Context_diff)

#Plot Index. -> Figure 2.2.4
ggbarplot(cct_pilot_neu, x = "Type_Episode", y ="Index",
          fill = "Type_Episode", color = "Type_Episode", 
          add = c("mean_ci"), error.plot="errorbar", position = position_dodge(.8),
          ylab = "Standardized Index Sum", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Create Final List to check the general performance of each situation.
final_list <- cct_pilot_neu %>% 
  group_by(Type_Episode) %>% 
  summarise(mean = mean(Index, na.rm=TRUE))

######################################################################################### 

#########################################################################################

#####2.3 Pilot Study - Retention of Warmth & Competence Items. ########

#STEP 1. Get All Datasets.

#studies from Van de Ven et al., (2017) should be downloaded manually from: http://hdl.handle.net/10411/20818.
#then put them in your main folder and load them as specified below. 

#set working directory. adapt this to your own path. 
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Additional Data/What emotional tears convey BJSP")

#Van de Ven et al. (2017) Study1.

VMV_Study1 <- read.spss("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Additional Data/What emotional tears convey BJSP/Study 1 explicit within subject/data/How_Do_People_Perceive_Cryers workfile.sav")
VMV_Study1 <- as.data.frame(VMV_Study1)

#select variables.
VMV_Study1 <- VMV_Study1 %>% 
  mutate(Study = 1) %>% 
  select(SCM_1:SCM_8, Study)

#Van de Ven et al. (2017) Study 2.

VMV_Study2 <- read.spss("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Additional Data/What emotional tears convey BJSP/Study 2 tears and sadness/data/Study 3 workfile.sav")
VMV_Study2 <- as.data.frame(VMV_Study2)

#select and rename variables.
VMV_Study2 <- VMV_Study2 %>% 
  rename(SCM_1 = SCM_5, SCM_2 = SCM_6, SCM_3 = SCM_7, SCM_4 = SCM_8, SCM_5 = SCM_9, SCM_6 = SCM_10, SCM_7 = SCM_11, SCM_8 = SCM_12) %>% 
  mutate(Study = 2) %>%
  select(SCM_1:SCM_8, Study)

#Van de Ven et al. (2017) Study 3.

VMV_Study3 <- read.spss("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Additional Data/What emotional tears convey BJSP/Study 3 tears and intentions/data/workfile SCM and intentions.sav")
VMV_Study3 <- as.data.frame(VMV_Study3)

#select and rename variables.
VMV_Study3 <- VMV_Study3 %>% 
  rename(SCM_1 = Q4_1, SCM_2 = Q4_2, SCM_3 = Q4_3, SCM_4 = Q4_4, SCM_5 = Q4_5, SCM_6 = Q4_6, SCM_7 = Q4_7, SCM_8 = Q4_8) %>% 
  mutate(Study = 3) %>% 
  select(SCM_1:SCM_8, Study)

#Zickfeld and Schubert (2018) Study 1.

ZS_Study1 <- getOSFfile("fwvzj")
ZS_Study1 <- read.table(ZS_Study1, header=TRUE)
is.na(ZS_Study1) = ZS_Study1 == -99
#filter out excluded cases.
ZS_Study1 <- subset(ZS_Study1, filter==1)

#select variables.
ZS_Study1 <- ZS_Study1 %>% 
              mutate(Study = 4) %>% 
              select(SCM_1:SCM_8, Study)

#Zickfeld and Schubert (2018) Study 2.

ZS_Study2 <- getOSFfile("42qtb")
ZS_Study2 <- read.table(ZS_Study2, header=TRUE)
is.na(ZS_Study2) = ZS_Study2 == -99
#filter out excluded cases.
ZS_Study2 <- subset(ZS_Study2, filter==1)

#select variables.
ZS_Study2 <- ZS_Study2 %>% 
  mutate(Study = 5) %>% 
  select(SCM_1:SCM_8, Study)


#Zickfeld, van de Ven Schubert Vingehoets (2018) Study 1.

#load dataset.
#if not working download manually from https://osf.io/zq7vk/
ZVSV_Study1 <- getOSFfile("zq7vk")
ZVSV_Study1 <- read.csv(ZVSV_Study1)
#filter out excluded cases.
ZVSV_Study1 <- subset(ZVSV_Study1, ZVSV_Study1$filter==1)

#rename.
ZVSV_Study1 <- ZVSV_Study1 %>% 
                rename(SCM_1 = Warmth_1, SCM_2 = Warmth_2, SCM_3 = Warmth_3, SCM_4 = Warmth_4, SCM_5 = Competence_1,
                       SCM_6 = Competence_2, SCM_7 = Competence_3, SCM_8 = Competence_4) %>% 
                mutate(Study = 6) %>% 
                select(SCM_1, SCM_2, SCM_3, SCM_4, SCM_5, SCM_6, SCM_7, SCM_8, Study)



#Zickfeld, van de Ven Schubert Vingehoets (2018) Study 2.

#get study 2 data from https://osf.io/83xug/.
ZVSV_Study2 <- getOSFfile("83xug")
ZVSV_Study2 <- read.csv(ZVSV_Study2)
#filter out excluded cases.
ZVSV_Study2 <- subset(ZVSV_Study2, ZVSV_Study2$filter==1)

ZVSV_Study2 <- ZVSV_Study2 %>% 
  rename(SCM_1 = Warmth_1, SCM_2 = Warmth_2, SCM_3 = Warmth_3, SCM_4 = Warmth_4, SCM_5 = Competence_1,
         SCM_6 = Competence_2, SCM_7 = Competence_3, SCM_8 = Competence_4) %>% 
  mutate(Study = 7) %>% 
  select(SCM_1, SCM_2, SCM_3, SCM_4, SCM_5, SCM_6, SCM_7, SCM_8, Study)

#Zickfeld and Schubert Unpublished Study.

#or download manually from https://osf.io/s2tvb/.

ZS_Unpublished <- getOSFfile("s2tvb")
ZS_Unpublished <- read.csv(ZS_Unpublished)

ZS_Unpublished <- ZS_Unpublished %>% 
  rename(SCM_1 = Warmth_1, SCM_2 = Warmth_2, SCM_3 = Warmth_3, SCM_4 = Warmth_4, SCM_5 = Competent_1,
         SCM_6 = Competent_2, SCM_7 = Competent_3, SCM_8 = Competent_4) %>% 
  mutate(Study = 8) %>% 
  select(SCM_1, SCM_2, SCM_3, SCM_4, SCM_5, SCM_6, SCM_7, SCM_8, Study)


##STEP 2: combine into one dataset.

All_Studies <- rbind(VMV_Study1, VMV_Study2)
All_Studies <- rbind(All_Studies, VMV_Study3)
All_Studies <- rbind(All_Studies, ZS_Study1)
All_Studies <- rbind(All_Studies, ZS_Study2)
All_Studies <- rbind(All_Studies, ZVSV_Study1)
All_Studies <- rbind(All_Studies, ZVSV_Study2)
All_Studies <- rbind(All_Studies, ZS_Unpublished)

#save dataset to path. 
write.csv(All_Studies, "/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Main Manuscript/Supplement/WarmthCompetence_AllStudies.csv")


#convert all variables to numeric.
All_Studies <- All_Studies %>% 
                select(SCM_1:SCM_8)
dat <- sapply(All_Studies, as.numeric)
dat <- as.data.frame(dat)

#conduct parallel analysis. suggests retention of 2 factors.
parallel <- fa.parallel(dat, fm="minres", fa="fa")

#conduct factor analysis. 
factor <- fa(dat, nfactors=2, rotate="oblimin", fm="minres", n.iter=100)
print(factor$loadings,cutoff=0.3)
factor
#-> Table 2.3.1


######################################################################################### 

#########################################################################################


###########2.4 Pilot Study - Main Pilot Study 1.###############

#set working directory. (adapt to own path).
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/Pilot Study 1")
########

###############
#Step 1. Load Dataset.

###############

#load dataset.
#if not working download manually from https://osf.io/vwdj3/
dat <- getOSFfile("vwdj3")
dat <- read.csv(dat)

#alternatively load from disk.
#dat <- read.csv("CCT_PilotStudy1.csv")

########

################
#Step 2. Recode & Rename.

################

#FOR WINDOWS MACHINES.
dat <- dat %>% 
  rename(SVO_1 = ?..SVO_1)

#create interactions.
dat$ConditionxTarget_Gender <- dat$Condition * dat$Target_Gender

dat$ConditionxEthnicity_Fit <- dat$Condition * dat$Ethnicity_Fit
dat$Target_GenderxEthnicity_Fit <- dat$Target_Gender * dat$Ethnicity_Fit
dat$ConditionxTarget_GenderxEthnicity_Fit <- dat$Condition * dat$Target_Gender * dat$Ethnicity_Fit

dat$TypexTarget_Gender <- dat$Type * dat$Target_Gender
dat$Social_ContextxTarget_Gender <- dat$Social_Context * dat$Target_Gender
dat$TypexSocial_Context <- dat$Type * dat$Social_Context
dat$TypexTarget_GenderxSocial_Context <- dat$Type * dat$Target_Gender * dat$Social_Context

dat$TypexEthnicity_Fit <- dat$Type * dat$Ethnicity_Fit
dat$Social_ContextxEthnicity_Fit <- dat$Social_Context * dat$Ethnicity_Fit
dat$TypexTarget_GenderxSocial_ContextxEthnicity_Fit <- dat$Type * dat$Target_Gender * dat$Social_Context * dat$Ethnicity_Fit

#####################

##create factors and add levels.

dat$Condition <- as.factor(dat$Condition)
levels(dat$Condition)[1] <- "No Tears"
levels(dat$Condition)[2] <- "Tears"

dat$Target_Gender <- as.factor(dat$Target_Gender)
levels(dat$Target_Gender)[1] <- "Male"
levels(dat$Target_Gender)[2] <- "Female"

dat$Type <- as.factor(dat$Type)
levels(dat$Type)[1] <- "Negative"
levels(dat$Type)[2] <- "Neutral"
levels(dat$Type)[3] <- "Positive"

dat$Ethnicity <- as.factor(dat$Ethnicity)
levels(dat$Ethnicity)[1] <- "Black"
levels(dat$Ethnicity)[2] <- "East Asian"
levels(dat$Ethnicity)[3] <- "Latin"
levels(dat$Ethnicity)[4] <- "Middle East"
levels(dat$Ethnicity)[5] <- "White"

dat$Social_Context <- as.factor(dat$Social_Context)
levels(dat$Social_Context)[1] <- "Public"
levels(dat$Social_Context)[2] <- "Private"

dat$Gender <- as.factor(dat$Gender)
levels(dat$Gender)[1] <- "Female"
levels(dat$Gender)[2] <- "Male"
levels(dat$Gender)[3] <- "Other"


######################################
#Step 3. Internal Reliabilities.

######################################

##calculate internal reliabilities of scales.

#reliability for competence items.
comp_alpha <- dat %>% 
                select(Competent, Capable) %>% 
                do(alpha = alpha(.))

comp_alpha$alpha[1]
#alpha of .90.

#reliability for warmth items.
warmth_alpha <- dat %>% 
  select(Warm, Friendly) %>% 
  do(alpha = alpha(.))

warmth_alpha$alpha[1]
#alpha of .91.

#reliability for honesty items.
hon_alpha <- dat %>% 
  select(Honest, Reliable) %>% 
  do(alpha = alpha(.))

hon_alpha$alpha[1]
#alpha of .86.


######################################
#Step 4. Demographics.

######################################

#check total number of participants.
dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#total of 231 participants.

#check distribution by gender.
dat %>%
  group_by(MTurkCode) %>%
  slice(which.min(Gender)) %>%
  ungroup(MTurkCode) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 157 Females, 71 Males, 2 Other

#check distribution of age.
dat %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 37.92, sd = 12.92.

#check distribution of nationality.
dat %>% 
  group_by(MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Nation) %>% 
  summarise(n = n(), per = n()/231)
#most (227) are from the UK. 

#check for distribution of ehtnicity and targets.

#Cross Table of Type and Target Gender -> Table 1.4.1
CrossTable(dat$Type, dat$Target_Gender, chisq=TRUE, format="SPSS")

#Cross Table of Type and Ethnicity -> Table 1.4.2
CrossTable(dat$Type, dat$Ethnicity, chisq=TRUE, format="SPSS")

#Cross Table Target Gender and Ethnicity.
CrossTable(dat$Target_Gender, dat$Ethnicity, chisq=TRUE, format="SPSS")
#rather similar distributions.

#Cross Table Target Gender and Scoial Context.
CrossTable(dat$Target_Gender, dat$Social_Context, chisq=TRUE, format="SPSS")


#####################
#4.2 Preparation of Scores.

#need to reajust scales from 0 to 6, not 1 to 7.
dat$Competence <- dat$Competence - 1
dat$Warmth <- dat$Warmth - 1
dat$Honesty <- dat$Honesty - 1
dat$Dominant <- dat$Dominant - 1
dat$Help <- dat$Help - 1
dat$Pos <- dat$Pos - 1
dat$Neg <- dat$Neg - 1
dat$Sad <- dat$Sad - 1
dat$Touched <- dat$Touched - 1
dat$Appropriate <- dat$Appropriate - 1 


#general overview of data.
describe(dat)


#correlation table among main variables. 
dat_cor <- dat %>% 
            select(Competence, Warmth, Honesty, Dominant, Sad, Touched, Help, IOS, Pos, Neg, Appropriate)
#save correlation table. 
apa.cor.table(dat_cor, filename="Table3_APA.doc", table.number=3)

#get descriptives by Type and Target Gender -> Table 2.4.3
Descriptives <- dat %>% 
  group_by(Type) %>% 
  select(Competence, Warmth, Honesty, Dominant, Sad, Touched, Help, IOS, Pos, Neg, Appropriate) %>% 
  do(descr = describe(.))

Descriptives$descr[1]
Descriptives$descr[2]
Descriptives$descr[3]

dat %>% 
  group_by(Type, Target_Gender) %>% 
  summarise(mean = mean(Warmth, na.rm=TRUE), sd = sd(Warmth, na.rm=TRUE))

###################################


################################################################
#Step 5. MAIN ANALYSES.

#Main Analyses are based on the File: CCT_Functions.R
#There are two main models: main_model_1 and main_model_2
#as well as the mediation model. 

#For more information check CCT_Functions.R file directly. 
################################################################

######################
#Main Models -> Table 2.4.4

#H1. 
main_model_1("Warmth")


#H2.
main_model_1("Competence")

#H3.
main_model_1("Honesty")

#H4.
main_model_1("Dominant")

#H5. 
main_model_1("Help")
main_model_1("SVO")

#H6.
main_model_1("IOS")

#######################################

#H7.

main_model_1("Sad")
main_model_1("Touched")

mediation("Condition","Touched", "Warmth")
mediation("Condition", "Sad", "Warmth")
mediation("Condition", "Sad", "Competence")

#######################

#Mani Check.
main_model_2("Pos")
main_model_2("Neg")

#H8. 
main_model_2("Touched")
dat %>% 
  group_by(Type, Target_Gender) %>% 
  summarise(mean = mean(Touched))

main_model_2("Sad")

#H9. 
main_model_2("Appropriate")

#H10.
main_model_2("Warmth")



######################################################################################### 

#########################################################################################

######2.5 Pilot Study - Stimulus Rating#######



#1. Set Working Directory.


#1.2 set working directory. Change working directory to your own path.
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/PIlot Stimuli Ratings")


#2. Load dataset.

#if not working download manually from https://osf.io/ej4a3/.
dat <- getOSFfile("ej4a3")
dat <- read.csv(dat)


#3. Recode & Rename.

#3.1 Convert to factors and add levels.
dat$Target_Gender <- as.factor(dat$Target_Gender)
levels(dat$Target_Gender)[1] <- "Male"
levels(dat$Target_Gender)[2] <- "Female"

dat$Response <- as.factor(dat$Response)
levels(dat$Response)[1] <- "Crying"
levels(dat$Response)[2] <- "No Crying"

dat$Tear <- as.factor(dat$Tear)
levels(dat$Tear)[1] <- "No Tear"
levels(dat$Tear)[2] <- "Tear"

dat$Gender <- as.factor(dat$Gender)
levels(dat$Gender)[1] <- "Female"
levels(dat$Gender)[2] <- "Male"
levels(dat$Gender)[3] <- "Other/Prefer not to say"
##################

#4. Explore Sample.

#4.1 Check number of participants.
dat %>% 
  group_by(participantID) %>% 
  summarise(n = n())
#in total 40 participants. all participants completed all 200 trials.

#4.2 Check age distribution.
dat %>% 
  summarise(mean = mean(Age), sd = sd(Age), min = min(Age), max = max(Age))
# mean age = 32.55, sd = 10.45.

#4.3 Check distribution by gender.
dat %>% 
  group_by(Gender) %>% 
  summarise(n = n()/200)
#15 females, 24 males, 1 missing.

#4.4 Check disutribution for nation. 
dat %>% 
  group_by(Nation) %>% 
  summarise(n = n()/200)
#38 from US, 1 Armenia, 1 German.

#4.5 Check distribution of duration of the survey. 
#histogram shows that some participants took rather long to finish the survey. 
hist(dat$Duration__in_seconds_)

####################
#5. Main Analyses.

#5.1 Main Model with RT and Tear factor (Tear vs. No Tear) and Response (Crying vs. not crying.)
rt_cnd <- lmerTest::lmer(RT ~ Tear + Response + Tear*Response + (1|participantID), data=dat)
anova(rt_cnd)
summary(rt_cnd)
emmeans(rt_cnd, "Response")
emmeans(rt_cnd, "Tear")
emmeans(rt_cnd, pairwise ~ Tear : Response)
emmip(rt_cnd, Tear ~ Response)
#faster reaction times for tearful faces. 

#5.2 repeat with transformed RT.
rtlog_cnd <- lmer(RTlog ~ Tear + Response + Tear*Response (1|participantID), data=dat)
summary(rtlog_cnd)
emmeans(rtlog_cnd, "Tear")
#same effect.

#5.3 Main models for Errors. 
rt_error <- lmerTest::lmer(RT ~ as.factor(Error) + (1|participantID), data=dat)
anova(rt_error)
summary(rt_error)
emmeans(rt_error, "Error")
#errors had faster reaction times. not surprising. 

#5.4. Check differences in RT for gender. 
rt_gender <- lmer(RT ~ Target_Gender + (1|participantID), data=dat)
summary(rt_gender)
emmeans(rt_gender, "Target_Gender")
#no differences in speed for the different gender types.

##############.

#5.5 let's check how errors are distributed. -> Table 2.5.1
CrossTable(dat$Response, dat$Tear, chisq=TRUE, format="SPSS")
#in general the majority of cases (>90%) were identified correctly. 
#participants rated 6.39% of non tearful stimuli as showing a tear and 9.62% of tearful stimuli as non tearful.

#Check the amount of errors.
dat %>% 
  group_by(Error) %>% 
  summarise(n = n()/8000)
#in total 7.4% of errors. 

#Check amount of errors for the different stimuli types.
dat %>% 
  group_by(Tear,Error) %>% 
  summarise(n = n()/4000)
#9.1% of errors for tearful stimuli. 

##
#5.6 Check error distribution across gender. 
CrossTable(dat$Error, dat$Target_Gender, chisq=TRUE, format="SPSS")
#no differences between gender for amount of errors chi2(1) = .805, p = .369


#6. Compute Accuracy Index. 
#create new dataset including only Tearful stimuli and number of errors, gender and RT.
#also add Index of accuracy including stand errors and RT.
dat_tear <- dat %>% 
             filter(Tear == "Tear") %>% 
             group_by(Number) %>% 
             summarise(Error = sum(Error), Gender = mean(as.numeric(Target_Gender)), RT = mean(RT)) %>% 
             mutate(Index = scale(Error) + scale(RT)) %>% 
             mutate(Error_z = scale(Error)) %>% 
             mutate(Accuracy = Index * -1)
#convert Index to numeric.
dat_tear$Index <- as.numeric(dat_tear$Index)
dat_tear$Error_z <- as.numeric(dat_tear$Error_z)
dat_tear$Accuracy <- as.numeric(dat_tear$Accuracy)

#Check amount of errors.
dat_tear %>% 
  filter(Error > 5) %>% 
  group_by(Number) %>% 
  summarise(Error = sum(Error), Percentage = sum(Error)/40)
#20 targets produced more than 5 errors.

dat_tear %>% 
  filter(Error < 3) %>% 
  group_by(Number) %>% 
  summarise(Error = sum(Error))
#37 targets produced less than 3 errors.

dat_tear %>% 
  filter(Error == 0) %>% 
  group_by(Number) %>% 
  summarise(Error = sum(Error))
#4 targets produced no error.

dat_tear %>% 
  group_by(Number) %>% 
  mutate(Percentage = sum(Error)/40) %>% 
  filter(Percentage > .10) %>% 
  summarise(Error = sum(Error), Perc = mean(Percentage))
#there are 27 targets that were misperceived more than 10% of the time (i.e. by more than 4 participants). 

#add types of stimuli.
Ethnicity <- c(rep(c("Black"),10), rep(c("East Asian"),10), rep(c("Latin"),10), rep(c("Middle East"),10), 
         rep(c("White"),10), rep(c("Black"),10), rep(c("East Asian"),10), rep(c("Latin"),10), rep(c("Middle East"),10), 
          rep(c("White"),10))

#bind ethnicity variable to dataset. 
dat_tear <- cbind(dat_tear, Ethnicity)

#6.1 check distribution of errors.
dat_tear %>% 
  summarise(mean = mean(Error), median = median(Error), min = min(Error), max = max(Error))
hist(dat_tear$Error)


#plot errors.
ggbarplot(dat_tear, x = "Number", y ="Error",
          color = "Ethnicity", fill = "Ethnicity",
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Total Errors", xlab = "Stimulus #") +
          geom_hline(yintercept = 4)

##plot standardized errors. -> Figure 2.5.1
ggbarplot(dat_tear, x = "Number", y ="Error_z",
          color = "Ethnicity", fill = "Ethnicity",
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "z-Errors", xlab = "Stimulus #")


#plot RT.
ggbarplot(dat_tear, x = "Number", y ="RT",
          color = "Ethnicity", fill = "Ethnicity",
          position = position_dodge(), ylim = c(1,1.8),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Mean RT", xlab = "Stimulus #")


#plot index.
ggbarplot(dat_tear, x = "Number", y ="Accuracy",
          color = "Ethnicity", fill = "Ethnicity",
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar", ylim = c(-4.2, 3),
          ylab = "Accuracy Index", xlab = "Stimulus #") +
  guides(fill=FALSE, color=FALSE) 



######################################################################################### 

#########################################################################################

#######2.6 Pilot Study - Main Pilot Study 2#################

#set working directory.
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/Pilot Study 2")

#load dataset.
#if not working download manually from https://osf.io/s7b2y/
dat <- getOSFfile("s7b2y")
dat <- read.csv(dat)

#alternatively load from disk.
#dat <- read.csv("CCT_PilotStudy2.csv")

###################################
#fix SVO score.
#dat <- dat %>%
#mutate(Stimulus_SVO = if_else(is.na(Stimulus_SVO), .5, Stimulus_SVO))

###########
#Step 2. Recoding & Renaming

###########


#2.1 create interactions.

dat$ConditionxTarget_Gender <- dat$Condition * dat$Target_Gender

dat$ConditionxEthnicity_Fit <- dat$Condition * dat$Ethnicity_Fit
dat$Target_GenderxEthnicity_Fit <- dat$Target_Gender * dat$Ethnicity_Fit
dat$ConditionxTarget_GenderxEthnicity_Fit <- dat$Condition * dat$Target_Gender * dat$Ethnicity_Fit

dat$TypexTarget_Gender <- dat$Type * dat$Target_Gender
dat$Social_ContextxTarget_Gender <- dat$Social_Context * dat$Target_Gender
dat$TypexSocial_Context <- dat$Type * dat$Social_Context
dat$TypexTarget_GenderxSocial_Context <- dat$Type * dat$Target_Gender * dat$Social_Context

dat$TypexEthnicity_Fit <- dat$Type * dat$Ethnicity_Fit
dat$Social_ContextxEthnicity_Fit <- dat$Social_Context * dat$Ethnicity_Fit
dat$TypexTarget_GenderxSocial_ContextxEthnicity_Fit <- dat$Type * dat$Target_Gender * dat$Social_Context * dat$Ethnicity_Fit

dat$Stimulus_SVOxTarget_Gender <- dat$Stimulus_SVO * dat$Target_Gender

dat$ConditionxEmpathy <- dat$Condition * dat$Empathy
dat$Target_GenderxEmpathy <- dat$Target_Gender * dat$Empathy
dat$ConditionxTarget_GenderxEmpathy <- dat$Condition * dat$Target_Gender * dat$Empathy

dat$TypexEmpathy <- dat$Type * dat$Empathy

##2.2 create factors and add levels.

dat$Condition <- as.factor(dat$Condition)
levels(dat$Condition)[1] <- "No Tears"
levels(dat$Condition)[2] <- "Tears"

dat$Target_Gender <- as.factor(dat$Target_Gender)
levels(dat$Target_Gender)[1] <- "Male"
levels(dat$Target_Gender)[2] <- "Female"

dat$Type <- as.factor(dat$Type)
levels(dat$Type)[1] <- "Negative"
levels(dat$Type)[2] <- "Neutral"
levels(dat$Type)[3] <- "Positive"

dat$Ethnicity <- as.factor(dat$Ethnicity)
levels(dat$Ethnicity)[1] <- "Black"
levels(dat$Ethnicity)[2] <- "East Asian"
levels(dat$Ethnicity)[3] <- "Latin"
levels(dat$Ethnicity)[4] <- "Middle East"
levels(dat$Ethnicity)[5] <- "White"

dat$Social_Context <- as.factor(dat$Social_Context)
levels(dat$Social_Context)[1] <- "Public"
levels(dat$Social_Context)[2] <- "Private"

dat$Gender <- as.factor(dat$Gender)
levels(dat$Gender)[1] <- "Female"
levels(dat$Gender)[2] <- "Male"
levels(dat$Gender)[3] <- "Other"

dat$Stimulus_SVO <- as.factor(dat$Stimulus_SVO)
levels(dat$Stimulus_SVO)[1] <- "No Tears"
levels(dat$Stimulus_SVO)[2] <- "Tears"

######################################
#Step 3. Internal Reliabilities.

######################################

##calculate internal reliabilities of scales.

#3.1 reliability of competence items.
comp_alpha <- dat %>% 
                select(SCM_1, SCM_2) %>% 
                do(alpha = alpha(.))

comp_alpha$alpha[1]
#alpha of .87.

#3.2 reliability of warmth items.
warmth_alpha <- dat %>% 
  select(SCM_3, SCM_4) %>% 
  do(alpha = alpha(.))

warmth_alpha$alpha[1]
#alpha of .89.

#3.3 reliability of honesty items.
hon_alpha <- dat %>% 
  select(SCM_5, SCM_6) %>% 
  do(alpha = alpha(.))

hon_alpha$alpha[1]
#alpha of .80.

######################################
#Step 4. Demographics.

######################################

#4.1 check total number of participants.

dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#total of 224 participants.

#4.2 now FILTER out excluded cases.
#ALWAYS USE THIS FILTERED DATASET FOR THE FINAL ANALYSES.
dat <- dat %>% 
        filter(filter==1)

#Check final amount of participants.
dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#remaining 218 participants. 6 excluded.

#4.3 Check distribution by gender. 
dat %>%
  group_by(MTurkCode) %>%
  slice(which.min(Gender)) %>%
  ungroup(MTurkCode) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 103 Females, 114 Males, 1 Other

#4.4 Check age distribution.
dat %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 32.41, sd = 10.24.

#4.5 Check nationality distribution
dat %>% 
  group_by(MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Nation) %>% 
  summarise(n = n(), per = n()/218)
#most (207) are from the US. 

#check for distribution of ehtnicity and targets.

#Cross Table of Type and Target Gender -> Table 1.6.1
CrossTable(dat$Type, dat$Target_Gender, chisq=TRUE, format="SPSS")
#we have in total 325 male targets and 314 female.

#Cross Table of Ethnicity and Type -> Table 1.6.2
CrossTable(dat$Type, dat$Ethnicity, chisq=TRUE, format="SPSS")
#we have 125 black, 157 east asian, 114 latin, 115 middle east and 128 white targets in total.

#Cross Table Target Gender & Ethnicity.
CrossTable(dat$Target_Gender, dat$Ethnicity, chisq=TRUE, format="SPSS")
#rather similar distributions.

#Cross Table Target Gender & Social Context.
CrossTable(dat$Target_Gender, dat$Social_Context, chisq=TRUE, format="SPSS")
#312 public contexts, 327 private. 

######
#Readjust Main Variables. 

#need to reajust scales from 0 to 6 not 1 to 7.
dat$Competence <- dat$Competence - 1
dat$Warmth <- dat$Warmth - 1
dat$Honesty <- dat$Honesty - 1
dat$Dominant <- dat$Dominant - 1
dat$Intent <- dat$Intent - 1
dat$Pos <- dat$Pos - 1
dat$Neg <- dat$Neg - 1
dat$Sad <- dat$Sad - 1
dat$Touched <- dat$Touched - 1
dat$Appropriate <- dat$Appropriate - 1 


#4.6 general overview of data.
describe(dat)

#4.7 correlation table among main variables.
dat_cor <- dat %>% 
            select(Competence, Warmth, Honesty, Dominant, Sad, Touched, Intent, IOS, Pos, Neg, Appropriate)
#save table.
apa.cor.table(dat_cor, filename="Table3_APA.doc", table.number=3)

#4.8 get descriptives per type. -> Table 2.6.3
Descriptives <- dat %>% 
  group_by(Type) %>% 
  select(Competence, Warmth, Honesty, Dominant, Sad, Touched, Intent, IOS, Pos, Neg, Appropriate) %>% 
  do(descr = describe(.))

Descriptives$descr[1]
Descriptives$descr[2]
Descriptives$descr[3]

dat %>% 
  group_by(Type, Target_Gender) %>% 
  summarise(mean = mean(Appropriate), sd = sd(Appropriate))


################################################################
#Step 5. MAIN ANALYSES.

#Main Analyses are based on the File: CCT_Functions.R
#There are two main models: main_model_1 and main_model_2
#as well as the mediation model. 

#For more information check CCT_Functions.R file directly. 
################################################################

######################

#H1. 
main_model_1("Warmth")


###Additional Exploratory Analysis with approriateness.

warmth_approp <- lmerTest::lmer(Intent ~ Condition + Appropriate + Ethnicity_Fit + Condition*Appropriate + Ethnicity_Fit*Condition*Appropriate + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(warmth_approp)
emmeans(warmth_approp, "Condition")


##############################

#H2.
main_model_1("Competence")

#H3.
main_model_1("Honesty")

#H4.
main_model_1("Dominant")

#H5. 
main_model_1("Intent")


#Check SVO variable.
dat_svo <- dat %>% 
  group_by(MTurkCode) %>%
  slice(which.min(SVO)) %>% 
  ungroup(MTurkCode)

 dat_svo %>% 
  group_by(Stimulus_SVO) %>% 
  summarise(n = n())
#73 responses missing due to some technical error.  

 
mean(dat_svo$SVO, na.rm=TRUE) 
sd(dat_svo$SVO, na.rm=TRUE) 

#Model with SVO score.
lm_svo <- lm(SVO ~ Stimulus_SVO + Target_Gender + Stimulus_SVOxTarget_Gender, data = dat_svo)
anova(lm_svo)
lm.beta(lm_svo)
summary(lm_svo)
confint(lm_svo)
emmeans(lm_svo, "Stimulus_SVO")
emmeans(lm_svo, "Target_Gender")
apa.d.table(iv = Stimulus_SVO, dv = SVO, data = dat_svo, 
            filename = "Table_SVO.doc", 
            table.number = 1)

apa.d.table(iv = Target_Gender, dv = SVO, data = dat_svo, 
            filename = "Table_SVO.doc", 
            table.number = 1)

#H6.
main_model_1("IOS")


#######################################

#H7.

main_model_1("Sad")
main_model_1("Touched")
mediation("Condition","Touched", "Warmth")
mediation("Condition", "Sad", "Warmth")
mediation("Condition", "Sad", "Competence")

##################################

#Mani Check.
main_model_2("Pos")
main_model_2("Neg")

#H8. 
main_model_2("Touched")
dat %>% 
  group_by(Type, Target_Gender) %>% 
  summarise(mean = mean(Touched))

main_model_2("Sad")

#H9. 
main_model_2("Appropriate")

#H10.
main_model_2("Warmth")

##################

#H11. Ethnic Indentification.

dat_eth <- dat %>% 
  select(African, EastAsian, Latin, Middle_East, Caucasian) %>% 
  do(descr = describe(.))

dat_eth$descr

main_model_1_eth("Warmth")

dat %>% 
  group_by(Condition, Ethnicity_split) %>% 
  summarise(mean = mean(Warmth))

main_model_1_eth("Competence")

dat %>% 
  group_by(Condition, Ethnicity_split) %>% 
  summarise(mean = mean(Competence))

main_model_1_eth("Honesty")

dat %>% 
  group_by(Condition, Ethnicity_split) %>% 
  summarise(mean = mean(Honesty))

main_model_1_eth("Dominant")
main_model_1_eth("Intent")
dat %>% 
  group_by(Condition, Ethnicity_split, Target_Gender) %>% 
  summarise(mean = mean(Intent))

main_model_1_eth("IOS")
dat %>% 
  group_by(Condition, Ethnicity_split) %>% 
  summarise(mean = mean(IOS))

dat %>% 
  group_by(Condition, Ethnicity_split, Target_Gender) %>% 
  summarise(mean = mean(IOS))

main_model_1_eth("Touched")
main_model_1_eth("Sad")

########################

#H12. Main models with Empathy as a Covariate.

dat <- dat %>% 
      mutate(Empathy_split = ifelse(Empathy > 1, 1, 0))

main_model_1_emp("Warmth")
main_model_1_emp("Competence")
dat %>% 
  group_by(Condition, Empathy_split) %>% 
  summarise(mean = mean(Competence))

dat %>% 
  group_by(Condition, Empathy_split, Target_Gender) %>% 
  summarise(mean = mean(Competence))

main_model_1_emp("Honesty")
main_model_1_emp("Dominant")
main_model_1_emp("Intent")
main_model_1_emp("IOS")
main_model_1_emp("Touched")
dat %>% 
  group_by(Condition, Empathy_split) %>% 
  summarise(mean = mean(Touched))
main_model_1_emp("Sad")
dat %>% 
  group_by(Target_Gender, Empathy_split) %>% 
  summarise(mean = mean(Sad))

#########################

#H13. 

dat <- dat %>% 
          mutate(Type_1 = ifelse(Type == "Neutral", -.5, ifelse(Type == "Negative", 0, .5))) 
dat$Type_1 <- as.factor(dat$Type_1)


mediation2("Type_1","Sad", "Competence")

##########################

################################
#ADDITIONAL EXPLORATIONS.
################################
#Intention to help full function.

Intent_full <- lmerTest::lmer(Intent ~ Type + Target_Gender + Social_Context + Ethnicity_Fit + Empathy + Appropriate + Gender +
                                TypexTarget_Gender + TypexSocial_Context + TypexEthnicity_Fit + TypexEmpathy + Type*Appropriate + Type*Gender +
                                Type*Gender*Target_Gender + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)

summary(Intent_full)


library(ggplot2)
# Add regression lines
ggplot(dat, aes(x=Appropriate, y=Intent, color=Type, shape=Type)) +
  geom_smooth(method=lm)

ggplot(dat, aes(x=Appropriate, y=Intent, color=Target_Gender, shape=Target_Gender)) +
  geom_smooth(method=lm)

ggplot(dat, aes(x=Appropriate, y=Intent, color=Social_Context, shape=Social_Context)) +
  geom_smooth(method=lm)

# Remove confidence intervals
ggplot(dat, aes(x=Appropriate, y=Intent, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)
ggplot(dat, aes(x=Ethnicity_Fit, y=Intent, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)
ggplot(dat, aes(x=log(Empathy), y=Intent, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

ggplot(dat, aes(y=Intent, x=Type, fill=Target_Gender)) + 
    geom_bar(position = "dodge", stat="identity") +    
    facet_wrap(~Gender)

hist(log(dat$Empathy))

ggplot(dat, aes(x=Appropriate, y=Intent, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)
ggplot(dat, aes(x=Ethnicity_Fit, y=Intent, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)
ggplot(dat, aes(x=Empathy, y=Intent, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

ggbarplot(dat, x = "Social_Context", y ="Intent", color = "Social_Context", fill = "Social_Context",
          palette = c("green4","#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Help", xlab = "Condition") +
  guides(fill=FALSE, color=FALSE) 

ggbarplot(dat, x = "Target_Gender", y ="Intent", color = "Target_Gender", fill = "Target_Gender",
          palette = c("green4","#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Help", xlab = "Condition") +
  guides(fill=FALSE, color=FALSE) 

ggbarplot(dat, x = "Gender", y ="Intent", color = "Gender", fill = "Gender",
          palette = c("green4","#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Help", xlab = "Condition") +
  guides(fill=FALSE, color=FALSE) 




ggbarplot(dat, x = "Type", y ="Intent",
          fill = "Target_Gender", color = "Target_Gender", palette = c("green4", "#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Help", xlab = "Condition") +
  guides(fill=FALSE, color=FALSE) 

ggbarplot(dat, x = "Type", y ="Intent",
          fill = "Social_Context", color = "Social_Context", palette = c("green4", "#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Help", xlab = "Condition") 
  guides(fill = FALSE, color=FALSE) 

  ggbarplot(dat, x = "Type", y ="Intent",
          fill = "Ethnicity", color = "Ethnicity", palette = c("green4", "#E7B800", "#FC4E07", "red2", "blue1"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Help", xlab = "Condition") 
  guides(fill = FALSE, color=FALSE) 
  

#check mediation tears -> sadness -> helping.
  
#create two datasets.
  
dat_neg <- dat %>% 
    filter(Type %in% c("Negative","Neutral"))
dat_pos <- dat %>% 
    filter(Type %in% c("Positive","Neutral"))

mediation("Condition","Sad", "Intent", dat_neg)
mediation("Condition","Sad", "Intent", dat_pos)
mediation("Condition","IOS", "Intent", dat_neg)
mediation("Condition","IOS", "Intent", dat_pos)
mediation("Condition","Warmth", "Intent", dat_neg)
mediation("Condition","Warmth", "Intent", dat_pos)

summary(lmer(Intent ~ Warmth + Touched + Condition + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat_neg))

mediation_full("Condition", "Intent", "IOS", "Sad", "Warmth", dat_neg)
mediation_full("Condition", "Intent", "IOS", "Sad", "Warmth", dat_pos)

mediation("Condition", "Sad", "Warmth")
mediation("Condition", "Sad", "Competence")

#with appropriateness.

a_model <- glmer(Condition ~ IOS*Appropriate + Sad*Appropriate + Warmth*Appropriate +
                   (1 | MTurkCode) + (1 | Ethnicity/Stimulus), data = dat, family = binomial, control = glmerControl(optimizer = "bobyqa"))

summary(a_model)

dat %>% 
  group_by(Type) %>% 
  summarise(cor = cor(Intent, Appropriate))

cor(dat$IOS, dat$Appropriate)

######################################################################################### 

#########################################################################################

#######2.7 Pilot Study - Main Pilot Study 3#################

#set working directory.
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/Pilot Study 3")

#load dataset.
#if not working download manually from https://osf.io/bmsku/
dat <- getOSFfile("bmsku")
dat <- read.csv(dat)


#alternatively load from disk.
#dat <- read.csv("CCT_PilotStudy3.csv")

###

#########
#STEP 2. 
#RECODING 
#########


dat <- dat %>% 
  mutate(Type_A = ifelse(Type == 0, 1, -.5)) %>% 
  mutate(Type_B = ifelse(Type == -.5, .5, ifelse(Type == .5, -.5, 0)))


##compute interactions.
dat$ConditionxTarget_Gender <- dat$Condition * dat$Target_Gender

dat$ConditionxEthnicity_Fit <- dat$Condition * dat$Ethnicity_Fit
dat$Target_GenderxEthnicity_Fit <- dat$Target_Gender * dat$Ethnicity_Fit
dat$ConditionxTarget_GenderxEthnicity_Fit <- dat$Condition * dat$Target_Gender * dat$Ethnicity_Fit

dat$ConditionxTarget_Gender <- dat$Condition * dat$Target_Gender
dat$ConditionxGender <- dat$Condition * dat$Gender
dat$Social_ContextxTarget_Gender <- dat$Social_Context * dat$Target_Gender
dat$ConditionxSocial_Context <- dat$Condition * dat$Social_Context
dat$TypexTarget_GenderxSocial_Context <- dat$Type * dat$Target_Gender * dat$Social_Context

dat$TypexEthnicity_Fit <- dat$Type * dat$Ethnicity_Fit
dat$Social_ContextxEthnicity_Fit <- dat$Social_Context * dat$Ethnicity_Fit
dat$TypexTarget_GenderxSocial_ContextxEthnicity_Fit <- dat$Type * dat$Target_Gender * dat$Social_Context * dat$Ethnicity_Fit


dat$ConditionxEmpathy <- dat$Condition * dat$Empathic_Concern
dat$Target_GenderxEmpathy <- dat$Target_Gender * dat$Empathic_Concern
dat$ConditionxTarget_GenderxEmpathy <- dat$Condition * dat$Target_Gender * dat$Empathic_Concern

dat$ConditionxEmpathy <- dat$Condition * dat$Empathic_Concern

##create factors.
dat$Condition <- as.factor(dat$Condition)
levels(dat$Condition)[1] <- "No Tears"
levels(dat$Condition)[2] <- "Tears"

dat$Target_Gender <- as.factor(dat$Target_Gender)
levels(dat$Target_Gender)[1] <- "Male"
levels(dat$Target_Gender)[2] <- "Female"

dat$Type <- as.factor(dat$Type)
levels(dat$Type)[1] <- "Negative"
levels(dat$Type)[2] <- "Neutral"
levels(dat$Type)[3] <- "Positive"

dat$Ethnicity <- as.factor(dat$Ethnicity)
levels(dat$Ethnicity)[1] <- "Black"
levels(dat$Ethnicity)[2] <- "East Asian"
levels(dat$Ethnicity)[3] <- "Latin"
levels(dat$Ethnicity)[4] <- "Middle East"
levels(dat$Ethnicity)[5] <- "White"

dat$Social_Context <- as.factor(dat$Social_Context)
levels(dat$Social_Context)[1] <- "Public"
levels(dat$Social_Context)[2] <- "Private"

dat$Gender <- as.factor(dat$Gender)
levels(dat$Gender)[1] <- "Female"
levels(dat$Gender)[2] <- "Male"
levels(dat$Gender)[3] <- "Other/Prefer not to Say"


#set contrasts.
contrasts(dat$Type)
contrasts(dat$Type) <- cbind(c(.33,-.66,.33),c(.50,0,-.50))


######################################
#Step 3. Internal Reliabilities.

######################################

##calculate internal reliabilities of scales.
##compute with filter == 1 !!!!


#3.1 reliability of competence items.
comp_alpha <- dat %>% 
                select(SCM_1, SCM_2) %>% 
                do(alpha = psych::alpha(.))

comp_alpha$alpha[1]
#alpha of .86.

#compute competence score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Competence=mean(c(SCM_1,SCM_2)))
dat <- as.data.frame(dat)


#3.2 reliability of warmth items.
warmth_alpha <- dat %>% 
  select(SCM_3, SCM_4) %>% 
  do(alpha = psych::alpha(.))

warmth_alpha$alpha[1]
#alpha of .90.

#3.3 reliability of honesty items.
hon_alpha <- dat %>% 
  select(SCM_5, SCM_6) %>% 
  do(alpha = psych::alpha(.))

hon_alpha$alpha[1]
#alpha of .80.

#compute honesty score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Honesty=mean(c(SCM_5,SCM_6)))
dat <- as.data.frame(dat)


#3.4 reliability of support items.
sup_alpha <- dat %>% 
  select(Support, Comfort) %>% 
  do(alpha = psych::alpha(.))

sup_alpha$alpha[1]
#alpha of .97.

#3.5 reliability of helplessness items.
hel_alpha <- dat %>% 
  select(Helpless, Overwhelmed, Sad) %>% 
  do(alpha = psych::alpha(.))

hel_alpha$alpha[1]
#alpha of .85.

#3.6 reliability of empathic concern items.
ec_alpha <- dat %>% 
  select(EC_1, EC_2_r, EC_3, EC_4_r, EC_5_r, EC_6, EC_7) %>% 
  do(alpha = psych::alpha(.))

ec_alpha$alpha[1]
#alpha of .85.

######################################
#Step 4. Demographics.

######################################

#4.1 check total number of participants.

dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#total of 86 participants.

#4.2 now FILTER out excluded cases.
#ALWAYS USE THIS FILTERED DATASET FOR THE FINAL ANALYSES.
dat <- dat %>% 
        filter(filter==1)

#Check final amount of participants.
dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#remaining 71 participants. 15 excluded.

#4.3 Check distribution by gender. 
dat %>%
  group_by(MTurkCode) %>%
  slice(which.min(Gender)) %>%
  ungroup(MTurkCode) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 48 Males, 23 Females

#4.4 Check age distribution.
dat %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 35.20789, sd = 11.3790.

#4.5 Check nationality distribution
dat %>% 
  group_by(MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Nation) %>% 
  summarise(n = n(), per = n()/71)
#most (207) are from the US. 

#check for distribution of ehtnicity and targets.

#Cross Table of Type and Target Gender -> Table 2.7.1
CrossTable(dat$Type, dat$Target_Gender, chisq=TRUE, format="SPSS")
#we have in total 138 male targets and 141 female.

#Cross Table of Ethnicity and Type -> Table 2.7.2
CrossTable(dat$Type, dat$Ethnicity, chisq=TRUE, format="SPSS")
#we have 51 black, 55 east asian, 50 latin, 60 middle east and 63 white targets in total.

#Cross Table Target Gender & Ethnicity.
CrossTable(dat$Target_Gender, dat$Ethnicity, chisq=TRUE, format="SPSS")
#rather similar distributions.

#Cross Table Target Gender & Social Context.
CrossTable(dat$Target_Gender, dat$Social_Context, chisq=TRUE, format="SPSS")
#140 public contexts, 139 private. 

######
#Readjust Main Variables. 

#need to reajust scales from 0 to 6 not 1 to 7.
dat$Warmth <- dat$Warmth - 1
dat$Support_Intention <- dat$Support_Intention - 1
dat$Pos <- dat$Pos - 1
dat$Neg <- dat$Neg - 1
dat$Sad <- dat$Sad - 1
dat$Touched <- dat$Touched - 1
dat$Appropriate <- dat$Appropriate - 1 
dat$Helplessness <- dat$Helplessness - 1
dat$Competence <- dat$Competence - 1
dat$Honesty <- dat$Honesty - 1
dat$Dominant <- dat$SCM_7 - 1


#4.6 general overview of data.
describe(dat)

#4.7 correlation table among main variables.
dat_cor <- dat %>% 
            select(Competence, Warmth, Honesty, Dominant, Helplessness, Touched, Support_Intention, IOS, Pos, Neg, Appropriate, Empathic_Concern)
#save table.
apa.cor.table(dat_cor, filename="Table3_APA.doc", table.number=3)

#4.8 get descriptives per type. -> Table 2.7.3
Descriptives <- dat %>% 
  group_by(Type) %>% 
  select(Competence, Warmth, Honesty, Dominant, Helplessness, Touched, Support_Intention, IOS, Pos, Neg, Appropriate, Empathic_Concern) %>% 
  do(descr = describe(.))

Descriptives$descr[1] #-> negative
Descriptives$descr[2] #-> neutral
Descriptives$descr[3] #-> positive

dat %>% 
  group_by(Type, Target_Gender) %>% 
  summarise(mean = mean(Warmth), sd = sd(Warmth))


################################################################
#Step 5. MAIN ANALYSES.

#Main Analyses are based on the File: CCT_Functions.R
#There are two main models: main_model_1 and main_model_2
#as well as the mediation model. 

#For more information check CCT_Functions.R file directly. 
################################################################

#center continuous predictors.

center_scale <- function(x) {
    scale(x, scale = FALSE)
}

dat$Appropriate_c <- center_scale(dat$Appropriate)
dat$Appropriate_c <- as.numeric(dat$Appropriate_c)

dat$Empathic_Concern_c <- center_scale(dat$Empathic_Concern)
dat$Empathic_Concern_c <- as.numeric(dat$Empathic_Concern_c)

dat$Ethnicity_Fit_c <- center_scale(dat$Ethnicity_Fit)
dat$Ethnicity_Fit_c <- as.numeric(dat$Ethnicity_Fit_c)

#######

#MAIN FUNCTIONS.
#create main function to run most models. 

#function to convert r to d based on Lakens (2013). 
r2d2 <- function(r, n1, n2) {
  d <- r/sqrt((1-r^2)*(n1/(n1+n2)*(n2/(n1+n2))))
  ds <- d*sqrt((n1+n2-2)/(n1+n2))
  print(ds)
}

main_model <- function(dat,y,x,rea,reb,rec) {
  dat$y <- dat[[y]]
  dat$x <- dat[[x]]
  dat$rea <- dat[[rea]]
  dat$reb <- dat[[reb]]
  dat$rec <- dat[[rec]]
  
  #first fit main model. 
  model_final <- lmerTest::lmer(y ~ x + (1|rea) + (1|reb/rec), data=dat)
  
  #compute estimated means for the condition and video type.
  mean_cond <- emmeans(model_final, "x")
  #get the main anova table for the model.
  anova <- anova(model_final, type=3)
  #get the unstandardized effect sizes.
  effects <- summary(model_final)
  #compute the 95% confidence intervals.
  CI <- confint(model_final)
  
  #get the unstandardized effect sizes for the main effects.
  es <- fixef(lmer(y ~ x + (1|rea) + (1|reb/rec), data=dat))
  
  #standardize estimates according to Bowman (2012) by the formula B * SDx / SDy. 
  r_cond <- es[2] * sd(dat$x, na.rm=TRUE) / sd(dat$y, na.rm=TRUE)
  #get cell sizes.
  n <- dat %>% 
  group_by(x) %>% 
  summarise(n = n())
  #transform rs to ds by using the 'r2d2' function. 
  d <- r2d2(r_cond, n$n[1], n$n[2])
  
  #### Effect Sizes R2####
  # Obtain F-values for calculating partial R^2
  anovaOutput = Anova(model_final, type=3, test.statistic="F")
  # Use R to calculate partial R^2 automatically
  R2 <- as_tibble(with(anovaOutput, ((Df/Df.res)*F)/(1+((Df/Df.res)*F)))) %>% 
  mutate(effects=names(fixef(model_final))) %>% 
  filter(effects!="(Intercept)") %>% select(effects, value)
  
  #compile everything in a list and return it.
  mainList <- list("d" = d, "Cond" = mean_cond,"Anova" = anova, "Effects" = effects, "CI" = CI, "R2" = R2)
  
  return(mainList)
}
##########################

#5.1: H1. Model with Intention to Help.

main_model(dat,"Support_Intention", "Condition", "MTurkCode", "Ethnicity", "Stimulus")

#plot. #-> Figure 2.7.1
library(ggpubr)
ggbarplot(dat, x = "Condition", y ="Support_Intention",
          fill = "Condition", palette = c("green4", "#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Support", xlab = "Condition") +
  guides(fill=FALSE, color=FALSE) 

#5.2: H1b. Model with Type of Tears.

model_final <- lmerTest::lmer(Support_Intention ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(model_final) #main effect of oc and tt.
emmeans(model_final, "Condition")
emmeans(model_final, "Type")
confint(model_final)

#Plot. #-> Figure 2.7.2
ggline(dat, x = "Condition", y = "Support_Intention", color = "Type",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

###############

#5.3: H2. Main Models. 

#5.3.1 Model with Perceived Warmth.

main_model(dat,"Warmth","Condition","MTurkCode","Ethnicity","Stimulus")
#d: .53
#R2: .12

#5.2.3 Model with Perceived Helplessness.

main_model(dat,"Helplessness","Condition","MTurkCode","Ethnicity","Stimulus")
#R2: .41
#d: 1.3611

#5.3.3 Model with IOS.

main_model(dat,"IOS","Condition","MTurkCode","Ethnicity","Stimulus")
#d: .41
#R2: .074
##########

#5.4 H2. Mediation Analyses.
##IV: Occurence of Tears; M: Perceived Warmth, IOS, Perceived Sadness. DV: Intention to Help.

#5.4.1 model path a M -> IV.

fm_med1 <- glmer(Condition ~ Warmth + IOS + Helplessness +
                   (1 | MTurkCode) + (1 | Ethnicity/Stimulus), data = dat, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(fm_med1)
#compute CIs.
se <- sqrt(diag(vcov(fm_med1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(fm_med1), LL = fixef(fm_med1) - 1.96 * se, UL = fixef(fm_med1) + 1.96 *
                se))

#effect by warmth and helplessness.

##################.

#5.4.2 model Path B + C' DV -> M + IV.

fm_med2 <- lmerTest::lmer(Support_Intention ~ Condition + Warmth + IOS + Helplessness +
                  (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(fm_med2)
confint(fm_med2)
#full mediation by warmh, IOS and helplessness

##################.

#5.4.3 model Path C.

fm_med3 <- lmer(Support_Intention ~ Condition +
                   (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)

summary(fm_med3)
confint(fm_med3)

############

#5.5 H3: Mediation Model Appropriateness.

#Plot Interaction Effect on Appropriateness #-> Figure 2.7.3
ggline(dat, x = "Condition", y = "Appropriate_c", color = "Type",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))


#5.5.1. Warmth Model.
#5.5.1.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_a)
confint(path_a)

#5.5.1.2 Path b.
path_b <- lmerTest::lmer(Warmth ~ Condition*Type + Appropriate_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_b)
confint(path_b)

#5.5.1.3 Path c. 

path_c <- lmerTest::lmer(Warmth ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_c)
confint(path_c)

#5.5.2 IOS Model.
#5.5.2.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_a)
confint(path_a)

#5.5.2.2 Path b.
path_b <- lmerTest::lmer(IOS ~ Condition*Type + Appropriate_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_b)
confint(path_b)

#5.5.2.3 Path c. 

path_c <- lmerTest::lmer(IOS ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_c)
confint(path_c)

#5.5.3 Helplessness Model.
#5.5.3.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_a)
confint(path_a)

#5.5.3.1 Path b.
path_b <- lmerTest::lmer(Helplessness ~ Condition*Type + Appropriate_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_b)
confint(path_b)

#5.5.3.1 Path c. 

path_c <- lmerTest::lmer(Helplessness ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_c)
confint(path_c)

#############

#5.6 H4-7: Moderation Model.

#5.6. Full Model with all variables. 

Intent_full <- lmerTest::lmer(Support_Intention ~ Condition + Target_Gender + Social_Context + Ethnicity_Fit_c + Empathic_Concern_c + 
                                Gender + ConditionxTarget_Gender + ConditionxSocial_Context + ConditionxGender + ConditionxEthnicity_Fit + ConditionxEmpathy +
                                 (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)


summary(Intent_full)
confint(Intent_full)
anova(Intent_full)
sjp.lmer(Intent_full)
ranef(Intent_full)
plot_model(Intent_full, type="std")
tab_model(Intent_full)

#Plot Interactions.

#social context (H4). #-> Figure 2.7.4
ggline(dat, x = "Condition", y = "Support_Intention", color = "Social_Context",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

#target gender (H5). #-> Figure 2.7.5
ggline(dat, x = "Condition", y = "Support_Intention", color = "Target_Gender",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

#empathy (h6). #-> Figure 2.7.6
ggplot(dat, aes(x=Empathic_Concern_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

#ethnicity fit. (h7) #-> Figure 2.7.7
ggplot(dat, aes(x=Ethnicity_Fit_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

####

#EXPLORATORY. WITH AROUSAL.

#create arousal score from Pilot_Arousal
dat <- dat %>% 
  mutate(
         Arousal = case_when(
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 1 ~ -.90,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 2 ~ -1.64,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 3 ~ -.86,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 4 ~ -.66,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 5 ~ -1.10,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 6 ~ -1.32,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 1 ~ -1.32,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 2 ~ -2.64,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 3 ~ -1.68,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 4 ~ -1.38,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 5 ~ -1.58,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 6 ~ -1.86,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 1 ~ 3.72,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 2 ~ 1.88,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 3 ~ 4.12,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 4 ~ 4.24,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 5 ~ 4.68,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 6 ~ 1.36,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 1 ~ 3.72,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 2 ~ 1.08,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 3 ~ 3.92,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 4 ~ 3.40,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 5 ~ 2.44,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 6 ~ 1.44,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 1 ~ 3.76,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 2 ~ 3.64,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 3 ~ 3.48,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 4 ~ -1.24,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 5 ~ 2.92,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 6 ~ 2.24,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 1 ~ 2.72,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 2 ~ 1.64,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 3 ~ 3.28,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 4 ~ -3.40,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 5 ~ 3.08,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 6 ~ 1.92   
         )
  )

dat$Arousal_c <- center_scale(dat$Arousal)
dat$Arousal_c <- as.numeric(dat$Arousal_c)

#main model with arousal.
model_arousal <- lmerTest::lmer(Support_Intention ~ Condition*Arousal_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(model_arousal) #main effect of oc and arousal.
emmeans(model_arousal, "Condition")
confint(model_final)

ggplot(dat, aes(x=Arousal_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)



######################################################################################### 

#########################################################################################

#######2.8 Pilot Study - Main Pilot Study 4#################

#set working directory.
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/Pilot Study 4")

#load dataset.
#if not working download manually from https://osf.io/cjfvq/
dat <- getOSFfile("cjfvq")
dat <- read.csv(dat)


#alternatively load from disk.
#dat <- read.csv("CCT_PilotStudy4.csv")

###

#########
#STEP 2. 
#RECODING 
#########


dat <- dat %>% 
  mutate(Type_A = ifelse(Type == 0, 1, -.5)) %>% 
  mutate(Type_B = ifelse(Type == -.5, .5, ifelse(Type == .5, -.5, 0)))


##compute interactions.
dat$ConditionxTarget_Gender <- dat$Condition * dat$Target_Gender

dat$ConditionxEthnicity_Fit <- dat$Condition * dat$Ethnicity_Fit
dat$Target_GenderxEthnicity_Fit <- dat$Target_Gender * dat$Ethnicity_Fit
dat$ConditionxTarget_GenderxEthnicity_Fit <- dat$Condition * dat$Target_Gender * dat$Ethnicity_Fit

dat$ConditionxTarget_Gender <- dat$Condition * dat$Target_Gender
dat$ConditionxGender <- dat$Condition * dat$Gender
dat$Social_ContextxTarget_Gender <- dat$Social_Context * dat$Target_Gender
dat$ConditionxSocial_Context <- dat$Condition * dat$Social_Context
dat$TypexTarget_GenderxSocial_Context <- dat$Type * dat$Target_Gender * dat$Social_Context

dat$TypexEthnicity_Fit <- dat$Type * dat$Ethnicity_Fit
dat$Social_ContextxEthnicity_Fit <- dat$Social_Context * dat$Ethnicity_Fit
dat$TypexTarget_GenderxSocial_ContextxEthnicity_Fit <- dat$Type * dat$Target_Gender * dat$Social_Context * dat$Ethnicity_Fit

dat$Target_GenderxGender <- dat$Target_Gender * dat$Gender


#FOR WINDOWS MACHINES.
dat <- dat %>% 
  rename(EC_1 = ?..EC_1)

######################################
#Step 3. Internal Reliabilities.

######################################

##calculate internal reliabilities of scales.
##compute with filter == 1 !!!!
dat <- dat %>% 
  filter(filter==1)



#3.1 reliability of competence items.
comp_alpha <- dat %>% 
                select(SCM_1, SCM_2) %>% 
                do(alpha = psych::alpha(.))

comp_alpha$alpha[1]
#alpha of .87.

#compute competence score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Competence=mean(c(SCM_1,SCM_2)))
dat <- as.data.frame(dat)


#3.2 reliability of warmth items.
warmth_alpha <- dat %>% 
  select(SCM_3, SCM_4) %>% 
  do(alpha = psych::alpha(.))

warmth_alpha$alpha[1]
#alpha of .93.

#compute warmth score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Warmth=mean(c(SCM_3,SCM_4)))
dat <- as.data.frame(dat)

#3.3 reliability of honesty items.
hon_alpha <- dat %>% 
  select(SCM_5, SCM_6) %>% 
  do(alpha = psych::alpha(.))

hon_alpha$alpha[1]
#alpha of .73.

#compute honesty score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Honesty=mean(c(SCM_5,SCM_6)))
dat <- as.data.frame(dat)


#3.4 reliability of support items.
sup_alpha <- dat %>% 
  select(Support, Needed, Accept) %>% 
  do(alpha = psych::alpha(.))

sup_alpha$alpha[1]
#alpha of .91.

dat <- dat %>% 
    rowwise() %>% 
    mutate(Support_Intention=mean(c(Support, Needed, Accept)))
dat <- as.data.frame(dat)

#3.5 reliability of helplessness items.
hel_alpha <- dat %>% 
  select(Helpless, Overwhelmed, Sad) %>% 
  do(alpha = psych::alpha(.))

hel_alpha$alpha[1]
#alpha of .88.

dat <- dat %>% 
    rowwise() %>% 
    mutate(Helplessness=mean(c(Sad, Helpless, Overwhelmed)))
dat <- as.data.frame(dat)

#3.6 reliability of empathic concern (state) items.
sec_alpha <- dat %>% 
  select(ECPD_1, ECPD_2) %>% 
  do(alpha = psych::alpha(.))

sec_alpha$alpha[1]
#alpha of .96.

dat <- dat %>% 
    rowwise() %>% 
    mutate(EC=mean(c(ECPD_1, ECPD_2)))
dat <- as.data.frame(dat)

#3.7 reliability of personal distress (state) items.
spd_alpha <- dat %>% 
  select(ECPD_3, ECPD_4) %>% 
  do(alpha = psych::alpha(.))

spd_alpha$alpha[1]
#alpha of .73.

dat <- dat %>% 
    rowwise() %>% 
    mutate(PD=mean(c(ECPD_3, ECPD_4)))
dat <- as.data.frame(dat)

#3.8 reliability of empathic concern items.

dat$EC_2_r <- (dat$EC_2 - 6) * -1
dat$EC_4_r <- (dat$EC_4 - 6) * -1
dat$EC_5_r <- (dat$EC_5 - 6) * -1

ec_alpha <- dat %>% 
  select(EC_1, EC_2_r, EC_3, EC_4_r, EC_5_r, EC_6, EC_7) %>% 
  do(alpha = psych::alpha(.))

ec_alpha$alpha[1]
#alpha of .82.

dat <- dat %>% 
    rowwise() %>% 
    mutate(Empathic_Concern=mean(c(EC_1, EC_2_r, EC_3, EC_4_r, EC_5_r, EC_6, EC_7)))
dat <- as.data.frame(dat)
#######

dat$ConditionxEmpathy <- dat$Condition * dat$Empathic_Concern
dat$Target_GenderxEmpathy <- dat$Target_Gender * dat$Empathic_Concern
dat$ConditionxTarget_GenderxEmpathy <- dat$Condition * dat$Target_Gender * dat$Empathic_Concern

dat$ConditionxEmpathy <- dat$Condition * dat$Empathic_Concern


##create factors.
dat$Condition <- as.factor(dat$Condition)
levels(dat$Condition)[1] <- "No Tears"
levels(dat$Condition)[2] <- "Tears"

dat$Target_Gender <- as.factor(dat$Target_Gender)
levels(dat$Target_Gender)[1] <- "Male"
levels(dat$Target_Gender)[2] <- "Female"

dat$Type <- as.factor(dat$Type)
levels(dat$Type)[1] <- "Negative"
levels(dat$Type)[2] <- "Neutral"
levels(dat$Type)[3] <- "Positive"

dat$Ethnicity <- as.factor(dat$Ethnicity)
levels(dat$Ethnicity)[1] <- "Black"
levels(dat$Ethnicity)[2] <- "East Asian"
levels(dat$Ethnicity)[3] <- "Latin"
levels(dat$Ethnicity)[4] <- "Middle East"
levels(dat$Ethnicity)[5] <- "White"

dat$Social_Context <- as.factor(dat$Social_Context)
levels(dat$Social_Context)[1] <- "Public"
levels(dat$Social_Context)[2] <- "Private"

dat$Gender <- as.factor(dat$Gender)
levels(dat$Gender)[1] <- "Female"
levels(dat$Gender)[2] <- "Male"
levels(dat$Gender)[3] <- "Other"


#set contrasts.
contrasts(dat$Type)
contrasts(dat$Type) <- cbind(c(.33,-.66,.33),c(.50,0,-.50))


######################################
#Step 4. Demographics.

######################################

#4.1 check total number of participants.

dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#total of 75 participants.

#4.2 now FILTER out excluded cases.
#ALWAYS USE THIS FILTERED DATASET FOR THE FINAL ANALYSES.
dat <- dat %>% 
        filter(filter==1)

#Check final amount of participants.
dat %>% 
  group_by(MTurkCode) %>% 
  summarise(n = n())
#remaining 75 participants. 14 excluded.

#4.3 Check distribution by gender. 
dat %>%
  group_by(MTurkCode) %>%
  slice(which.min(Gender)) %>%
  ungroup(MTurkCode) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 52 Males, 23 Females

#4.4 Check age distribution.
dat %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 35.92308, sd = 11.26991. min 20 max 74

#4.5 Check nationality distribution
dat %>% 
  group_by(MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Nation) %>% 
  summarise(n = n(), per = n()/71)
#most (74) are from the UK. 

#check for distribution of ehtnicity and targets.

#Cross Table of Type and Target Gender -> Table 2.8.1
CrossTable(dat$Type, dat$Target_Gender, chisq=TRUE, format="SPSS")
#we have in total 137 male targets and 162 female.

#Cross Table of Ethnicity and Type -> Table 2.8.2
CrossTable(dat$Type, dat$Ethnicity, chisq=TRUE, format="SPSS")
#we have 75 black, 50 east asian, 57 latin, 49 middle east and 68 white targets in total.

#Cross Table Target Gender & Ethnicity.
CrossTable(dat$Target_Gender, dat$Ethnicity, chisq=TRUE, format="SPSS")
#rather similar distributions.

#Cross Table Target Gender & Social Context.
CrossTable(dat$Target_Gender, dat$Social_Context, chisq=TRUE, format="SPSS")
#146 public contexts, 153 private. 

######
#Readjust Main Variables. 

#need to reajust scales from 0 to 6 not 1 to 7.
dat$Warmth <- dat$Warmth - 1
dat$Support_Intention <- dat$Support_Intention - 1
dat$Pos <- dat$Pos - 1
dat$Neg <- dat$Neg - 1
dat$Sad <- dat$Sad - 1
dat$Touched <- dat$Touched - 1
dat$Appropriate <- dat$Appropriate - 1 
dat$Helplessness <- dat$Helplessness - 1
dat$Competence <- dat$Competence - 1
dat$Honesty <- dat$Honesty - 1
dat$Dominant <- dat$SCM_7 - 1
dat$Attractive <- dat$SCM_8 - 1
dat$EC <- dat$EC - 1
dat$PD <- dat$PD - 1

#4.6 general overview of data.
describe(dat)

#4.7 correlation table among main variables.
dat_cor <- dat %>% 
            select(Competence, Warmth, Honesty, Dominant, Helplessness, Touched, Support_Intention, IOS, Pos, Neg, Appropriate, EC, PD, Empathic_Concern)
#save table.
apa.cor.table(dat_cor, filename="Table3_APA.doc", table.number=3)

#4.8 get descriptives per type. -> Table 2.7.3
Descriptives <- dat %>% 
  group_by(Condition) %>% 
  select(Competence, Warmth, Honesty, Dominant, Attractive, Helplessness, Touched, Support_Intention, IOS, Pos, Neg, Appropriate, EC, PD, Empathic_Concern) %>% 
  do(descr = describe(.))

Descriptives$descr[1] #-> no tears
Descriptives$descr[2] #-> tears


dat %>% 
  group_by(Condition, Target_Gender) %>% 
  summarise(mean = mean(PD), sd = sd(PD))



################################################################
#Step 5. MAIN ANALYSES.

#Main Analyses are based on the File: CCT_Functions.R
#There are two main models: main_model_1 and main_model_2
#as well as the mediation model. 

#For more information check CCT_Functions.R file directly. 
################################################################

#center continuous predictors.

center_scale <- function(x) {
    scale(x, scale = FALSE)
}

dat$Appropriate_c <- center_scale(dat$Appropriate)
dat$Appropriate_c <- as.numeric(dat$Appropriate_c)

dat$Empathic_Concern_c <- center_scale(dat$Empathic_Concern)
dat$Empathic_Concern_c <- as.numeric(dat$Empathic_Concern_c)

dat$Ethnicity_Fit_c <- center_scale(dat$Ethnicity_Fit)
dat$Ethnicity_Fit_c <- as.numeric(dat$Ethnicity_Fit_c)

#######

#MAIN FUNCTIONS.
#create main function to run most models. 

#function to convert r to d based on Lakens (2013). 
r2d2 <- function(r, n1, n2) {
  d <- r/sqrt((1-r^2)*(n1/(n1+n2)*(n2/(n1+n2))))
  ds <- d*sqrt((n1+n2-2)/(n1+n2))
  print(ds)
}

main_model <- function(dat,y,x,rea,reb,rec) {
  dat$y <- dat[[y]]
  dat$x <- dat[[x]]
  dat$rea <- dat[[rea]]
  dat$reb <- dat[[reb]]
  dat$rec <- dat[[rec]]
  
  #first fit main model. 
  model_final <- lmerTest::lmer(y ~ x + (1|rea) + (1|reb/rec), data=dat)
  
  #compute estimated means for the condition and video type.
  mean_cond <- emmeans(model_final, "x")
  #get the main anova table for the model.
  anova <- anova(model_final, type=3)
  #get the unstandardized effect sizes.
  effects <- summary(model_final)
  #compute the 95% confidence intervals.
  CI <- confint(model_final)
  
  #get the unstandardized effect sizes for the main effects.
  es <- fixef(lmer(y ~ x + (1|rea) + (1|reb/rec), data=dat))
  

  #standardize estimates according to Bowman (2012) by the formula B * SDx / SDy. 
  r_cond <- es[2] * sd(as.numeric(dat$x), na.rm=TRUE) / sd(dat$y, na.rm=TRUE)

  #get cell sizes.
  n <- dat %>% 
  group_by(x) %>% 
  summarise(n = n())
  #transform rs to ds by using the 'r2d2' function. 
  d <- r2d2(r_cond, n$n[1], n$n[2])
  
  #### Effect Sizes R2####
  # Obtain F-values for calculating partial R^2
  anovaOutput = Anova(model_final, type=3, test.statistic="F")
  # Use R to calculate partial R^2 automatically
  R2 <- as_tibble(with(anovaOutput, ((Df/Df.res)*F)/(1+((Df/Df.res)*F)))) %>% 
  mutate(effects=names(fixef(model_final))) %>% 
  filter(effects!="(Intercept)") %>% select(effects, value)
  
  #compile everything in a list and return it.
  mainList <- list("d" = d, "Cond" = mean_cond,"Anova" = anova, "Effects" = effects, "CI" = CI, "R2" = R2)
  
  return(mainList)
}
##########################

#5.1: H1. Model with Intention to Help.

main_model(dat,"Support_Intention", "Condition", "MTurkCode", "Ethnicity", "Stimulus")
#d = .465.
#R2 = .111.

#plot. #-> Figure 1.8.1
library(ggpubr)
ggbarplot(dat, x = "Condition", y ="Support_Intention",
          fill = "Condition", palette = c("green4", "#E7B800", "#FC4E07"),
          position = position_dodge(),
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Intention to Support", xlab = "Condition") +
  guides(fill=FALSE, color=FALSE) 

#5.2: H1b. Model with Type of Tears.

model_final <- lmerTest::lmer(Support_Intention ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(model_final) #main effect of oc and tt.
emmeans(model_final, "Condition")
emmeans(model_final, "Type")
confint(model_final)

#Plot. #-> Figure 1.8.2
ggline(dat, x = "Condition", y = "Support_Intention", color = "Type",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

###############

#5.3: H2. Main Models. 

#5.3.1 Model with Perceived Warmth.

main_model(dat,"Warmth","Condition","MTurkCode","Ethnicity","Stimulus")
#d: .539
#R2: .117

#5.2.3 Model with Perceived Helplessness.

main_model(dat,"Helplessness","Condition","MTurkCode","Ethnicity","Stimulus")
#R2: .45
#d: 1.45286

#5.3.3 Model with IOS.

main_model(dat,"IOS","Condition","MTurkCode","Ethnicity","Stimulus")
#d: .33523
#R2: .0739
##########

#5.4 H2. Mediation Analyses.
##IV: Occurence of Tears; M: Perceived Warmth, IOS, Perceived Sadness. DV: Intention to Help.

#5.4.1 model path a M -> IV.

fm_med1 <- glmer(Condition ~ Warmth + IOS + Helplessness +
                   (1 | MTurkCode) + (1 | Ethnicity/Stimulus), data = dat, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(fm_med1)
#compute CIs.
se <- sqrt(diag(vcov(fm_med1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(fm_med1), LL = fixef(fm_med1) - 1.96 * se, UL = fixef(fm_med1) + 1.96 *
                se))

#effect by warmth and helplessness.

##################.

#5.4.2 model Path B + C' DV -> M + IV.

fm_med2 <- lmerTest::lmer(Support_Intention ~ Condition + Warmth + IOS + Helplessness +
                  (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(fm_med2)
confint(fm_med2)
#full mediation by warmth, IOS and helplessness

##################.

#5.4.3 model Path C.

fm_med3 <- lmer(Support_Intention ~ Condition +
                   (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)

summary(fm_med3)
confint(fm_med3)

############

#5.4b H2b. Mediation Analyses.
##IV: Occurence of Tears; M: Empathic Concern, Personal Distress. DV: Intention to Help.

#5.4.1 model path a M -> IV.

fm_med1 <- glmer(Condition ~ EC + PD +
                   (1 | MTurkCode) + (1 | Ethnicity/Stimulus), data = dat, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(fm_med1)
#compute CIs.
se <- sqrt(diag(vcov(fm_med1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(fm_med1), LL = fixef(fm_med1) - 1.96 * se, UL = fixef(fm_med1) + 1.96 *
                se))

#effect by empathic concern.

##################.

#5.4.2 model Path B + C' DV -> M + IV.

fm_med2 <- lmerTest::lmer(Support_Intention ~ Condition + EC + PD +
                  (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(fm_med2)
confint(fm_med2)
#full mediation by warmth, IOS and helplessness
.55385 * .49513 #Ind. EC
.12221 * .04612 #Ind PD
##################.

#5.4.3 model Path C.

fm_med3 <- lmer(Support_Intention ~ Condition +
                   (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)

summary(fm_med3)
confint(fm_med3)

################################

#5.5 H3: Mediation Model Appropriateness.

#Plot Interaction Effect on Appropriateness #-> Figure 2.8.3
ggline(dat, x = "Condition", y = "Appropriate_c", color = "Type",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))


#5.5.1. Warmth Model.
#5.5.1.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_a)
confint(path_a)

#5.5.1.2 Path b.
path_b <- lmerTest::lmer(Warmth ~ Condition*Type + Appropriate_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_b)
confint(path_b)

#5.5.1.3 Path c. 

path_c <- lmerTest::lmer(Warmth ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_c)
confint(path_c)

3.1481 * .3519
.217 * .3519
#5.5.2 IOS Model.
#5.5.2.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_a)
confint(path_a)

#5.5.2.2 Path b.
path_b <- lmerTest::lmer(IOS ~ Condition*Type + Appropriate_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_b)
confint(path_b)

#5.5.2.3 Path c. 

path_c <- lmerTest::lmer(IOS ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_c)
confint(path_c)
.217 * .188
3.1481 * .188
#5.5.3 Helplessness Model.
#5.5.3.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_a)
confint(path_a)

#5.5.3.1 Path b.
path_b <- lmerTest::lmer(Helplessness ~ Condition*Type + Appropriate_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_b)
confint(path_b)

#5.5.3.1 Path c. 

path_c <- lmerTest::lmer(Helplessness ~ Condition*Type + (1|MTurkCode) + (1|Ethnicity/Stimulus), dat=dat)
summary(path_c)
confint(path_c)
.14976 * .217
.14976 * 3.1481
#############

#5.6 H4-7: Moderation Model.

#5.6. Full Model with all variables. 

Intent_full <- lmerTest::lmer(Support_Intention ~ Condition + Target_Gender + Social_Context + Ethnicity_Fit_c + Empathic_Concern_c + 
                                Gender + ConditionxTarget_Gender + ConditionxSocial_Context + ConditionxGender + ConditionxEthnicity_Fit + ConditionxEmpathy + Target_GenderxGender +
                                 (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)


summary(Intent_full)
confint(Intent_full)
anova(Intent_full)
sjp.lmer(Intent_full)
ranef(Intent_full)
plot_model(Intent_full, type="std")
tab_model(Intent_full)

#Plot Interactions.

#social context (H4). #-> Figure 2.8.4
ggline(dat, x = "Condition", y = "Support_Intention", color = "Social_Context",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

#target gender (H5). #-> Figure 2.8.5
ggline(dat, x = "Condition", y = "Support_Intention", color = "Target_Gender",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

#target gender x gender (H5b). #-> Figure 2.8.6
ggline(dat, x = "Condition", y = "Support_Intention", color = "Target_Gender", facet.by = "Gender",
 add = "mean_se", palette = c("green4", "#E7B800", "#FC4E07"))

#empathy (h6). #-> Figure 2.8.7
ggplot(dat, aes(x=Empathic_Concern_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

#ethnicity fit. (h7) #-> Figure 2.8.8
ggplot(dat, aes(x=Ethnicity_Fit_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

####

#EXPLORATORY. WITH AROUSAL.

#create arousal score from Pilot_Arousal
dat <- dat %>% 
  mutate(
         Arousal = case_when(
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 1 ~ -.90,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 2 ~ -1.64,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 3 ~ -.86,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 4 ~ -.66,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 5 ~ -1.10,
           Type == "Neutral" | Social_Context == "Public" | ContextNumber == 6 ~ -1.32,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 1 ~ -1.32,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 2 ~ -2.64,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 3 ~ -1.68,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 4 ~ -1.38,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 5 ~ -1.58,
           Type == "Neutral" | Social_Context == "Private" | ContextNumber == 6 ~ -1.86,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 1 ~ 3.72,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 2 ~ 1.88,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 3 ~ 4.12,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 4 ~ 4.24,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 5 ~ 4.68,
           Type == "Negative" | Social_Context == "Public" | ContextNumber == 6 ~ 1.36,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 1 ~ 3.72,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 2 ~ 1.08,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 3 ~ 3.92,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 4 ~ 3.40,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 5 ~ 2.44,
           Type == "Negative" | Social_Context == "Private" | ContextNumber == 6 ~ 1.44,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 1 ~ 3.76,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 2 ~ 3.64,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 3 ~ 3.48,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 4 ~ -1.24,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 5 ~ 2.92,
           Type == "Positive" | Social_Context == "Public" | ContextNumber == 6 ~ 2.24,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 1 ~ 2.72,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 2 ~ 1.64,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 3 ~ 3.28,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 4 ~ -3.40,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 5 ~ 3.08,
           Type == "Positive" | Social_Context == "Private" | ContextNumber == 6 ~ 1.92   
         )
  )

dat$Arousal_c <- center_scale(dat$Arousal)
dat$Arousal_c <- as.numeric(dat$Arousal_c)

#main model with arousal.
model_arousal <- lmerTest::lmer(Support_Intention ~ Condition*Arousal_c + (1|MTurkCode) + (1|Ethnicity/Stimulus), data=dat)
summary(model_arousal) #main effect of oc and arousal.
emmeans(model_arousal, "Condition")
anova(model_arousal)
confint(model_final)

ggplot(dat, aes(x=Arousal_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm)

###################################################

####1.9 CCT AROUSAL RATING############

#CCT Pilot Arousal.

###########Pilot: Arousal##############

#1.2. Set working directory. Please adjust this path to fit your system. 
setwd("/Users/janiszickfeld/Dropbox/Cross-Cultural Tears/Pilot Test/Pilot Study Arousal")


#2. Load Dataset.
#load dataset.
#if not working download manually from https://osf.io/vmpuj/
cct_arousal <- getOSFfile("vmpuj")
cct_arousal <- read.csv(cct_arousal)

#3. Convert Variables.

#3.1 Rename variables.
cct_arousal <- cct_arousal %>% 
              rename(participantID = PID, Situation = Sit_ID_1)

#FOR WINDOWS MACHINES.
cct_arousal <- cct_arousal %>% 
              rename(Gender = ?..Gender)


cct_arousal$Type <- as.factor(cct_arousal$Type)
levels(cct_arousal$Type)[1] <- "Negative"
levels(cct_arousal$Type)[2] <- "Neutral"
levels(cct_arousal$Type)[3] <- "Positive"

cct_arousal$Social_Context <- as.factor(cct_arousal$Social_Context)
levels(cct_arousal$Social_Context)[1] <- "Public"
levels(cct_arousal$Social_Context)[2] <- "Private"

cct_arousal$Situation <- as.factor(cct_arousal$Situation)
levels(cct_arousal$Situation)[1] <- "Shopping Pb"
levels(cct_arousal$Situation)[2] <- "Water Pb"
levels(cct_arousal$Situation)[3] <- "Salad Pb"
levels(cct_arousal$Situation)[4] <- "Survey Pb"
levels(cct_arousal$Situation)[5] <- "Print Pb"
levels(cct_arousal$Situation)[6] <- "Wash Pb"
levels(cct_arousal$Situation)[7] <- "Shopping Pr"
levels(cct_arousal$Situation)[8] <- "Water Pr"
levels(cct_arousal$Situation)[9] <- "Salad Pr"
levels(cct_arousal$Situation)[10] <- "Survey Pr"
levels(cct_arousal$Situation)[11] <- "Print Pr"
levels(cct_arousal$Situation)[12] <- "Wash Pr"
levels(cct_arousal$Situation)[13] <- "Reunion Pb"
levels(cct_arousal$Situation)[14] <- "Proposal Pb"
levels(cct_arousal$Situation)[15] <- "Award Pb"
levels(cct_arousal$Situation)[16] <- "Poetry Pb"
levels(cct_arousal$Situation)[17] <- "Cancer Pb"
levels(cct_arousal$Situation)[18] <- "Gift Pb"
levels(cct_arousal$Situation)[19] <- "Reunion Pr"
levels(cct_arousal$Situation)[20] <- "Proposal Pr"
levels(cct_arousal$Situation)[21] <- "Award Pr"
levels(cct_arousal$Situation)[22] <- "Poetry Pr"
levels(cct_arousal$Situation)[23] <- "Cancer Pr"
levels(cct_arousal$Situation)[24] <- "Gift Pr"
levels(cct_arousal$Situation)[25] <- "Break Up Pb"
levels(cct_arousal$Situation)[26] <- "Funeral Pb"
levels(cct_arousal$Situation)[27] <- "Infidelity Pb"
levels(cct_arousal$Situation)[28] <- "Humilation Pb"
levels(cct_arousal$Situation)[29] <- "Accident Pb"
levels(cct_arousal$Situation)[30] <- "Goodbye Pb"
levels(cct_arousal$Situation)[31] <- "Break Up Pr"
levels(cct_arousal$Situation)[32] <- "Funeral Pr"
levels(cct_arousal$Situation)[33] <- "Infidelity Pr"
levels(cct_arousal$Situation)[34] <- "Humilation Pr"
levels(cct_arousal$Situation)[35] <- "Accident Pr"
levels(cct_arousal$Situation)[36] <- "Goodbye Pr"

###

#3.2 Correlation Among Variables.
cct_cor <- cct_arousal %>% 
  select(Arousal_1, Arousal_2, Arousal_3, Arousal_4)
cor(cct_cor)
#low correlations among items. treat each on its own. 


#3.3 check total number of participants.

cct_arousal %>% 
  group_by(participantID) %>% 
  summarise(n = n())
#total of 25 participants.


#4.3 Check distribution by gender. 
cct_arousal %>%
  group_by(participantID) %>%
  slice(which.min(Gender)) %>%
  ungroup(participantID) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 20 Females, 5 Females

#4.4 Check age distribution.
cct_arousal %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 33.64, sd = 12.60. min 19 max 71

#4.5 Check nationality distribution
cct_arousal %>% 
  group_by(participantID) %>%
  slice(which.min(Nation)) %>% 
  ungroup(participantID) %>% 
  group_by(Nation) %>% 
  summarise(n = n(), per = n()/25)
#all (25) are from the UK. 

#################

#4. MAIN ANALYSES.

#4.1 Test for High (Negative) Arousal. 
mod_distress <- lmerTest::lmer(Arousal_1 ~ Type*Social_Context + (1|participantID), data=cct_arousal)
summary(mod_distress)
anova(mod_distress)
confint(mod_distress)
emmeans(mod_distress, "Type")
emmeans(mod_distress, "Social_Context")

#Plot Valence. 
ggbarplot(cct_arousal, x = "Situation", y ="Arousal_1",
          fill = "Situation", color = "Situation",
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Distress Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4.2 Test for High (Positive) Arousal. 
mod_excited <- lmerTest::lmer(Arousal_2 ~ Type*Social_Context + (1|participantID), data=cct_arousal)
summary(mod_excited)
anova(mod_excited)
confint(mod_excited)
emmeans(mod_excited, "Type")
emmeans(mod_excited, "Social_Context")

#Plot Valence. 
ggbarplot(cct_arousal, x = "Situation", y ="Arousal_2",
          fill = "Situation", color = "Situation",
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Excited Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#4.3 Test for Low (Negative) Arousal. 
mod_tired <- lmerTest::lmer(Arousal_3 ~ Type*Social_Context + (1|participantID), data=cct_arousal)
summary(mod_tired)
confint(mod_tired)
anova(mod_tired)
emmeans(mod_tired, "Type")
emmeans(mod_tired, "Social_Context")

#Plot Valence. 
ggbarplot(cct_arousal, x = "Situation", y ="Arousal_3",
          fill = "Situation", color = "Situation",
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Tired Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4.4 Test for Low (Positive) Arousal. 
mod_relaxed <- lmerTest::lmer(Arousal_4 ~ Type*Social_Context + (1|participantID), data=cct_arousal)
summary(mod_relaxed)
anova(mod_relaxed)
confint(mod_relaxed)
emmeans(mod_relaxed, "Type")
emmeans(mod_relaxed, "Social_Context")

#Plot Valence. 
ggbarplot(cct_arousal, x = "Situation", y ="Arousal_4",
          fill = "Situation", color = "Situation",
          add = c("mean_ci"), error.plot = "upper_errorbar",
          ylab = "Relaxed Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  stat_compare_means(label.y =7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###construct arousal rating.


cct_arousal$Arousal_1 <- as.numeric(cct_arousal$Arousal_1)
cct_arousal$Arousal_2 <- as.numeric(cct_arousal$Arousal_2)
cct_arousal$Arousal_3 <- as.numeric(cct_arousal$Arousal_3)
cct_arousal$Arousal_4 <- as.numeric(cct_arousal$Arousal_4)

cct_arousal <- cct_arousal %>%
                mutate(Arousal = case_when(
                                Type == "Negative" ~ Arousal_1 - Arousal_3,
                                Type == "Positive" ~ Arousal_2 - Arousal_4,
                                Type == "Neutral" ~ (Arousal_1 + Arousal_2)/2 - (Arousal_3 + Arousal_4)/2))
      
        

#Plot Arousal. 
ggbarplot(cct_arousal, x = "Situation", y ="Arousal",
          fill = "Situation", color = "Situation",
          add = c("mean_ci"),
          ylab = "Arousal Rating", xlab = "Type of Episode") +
  guides(fill=FALSE, color=FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#output final arousal rating. 

cct_arousal_mean <- cct_arousal %>% 
  group_by(Situation) %>% 
  summarise(mean = mean(Arousal))

########################################################################
