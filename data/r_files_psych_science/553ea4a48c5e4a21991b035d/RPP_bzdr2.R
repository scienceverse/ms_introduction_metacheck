################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: Sahakyan: Intentional forgetting is easier after two shots than one.
#@ Coder name: John Hodsoll
#@ Coder e-mail: john.hodsoll@kcl.ac.uk
#@ Type of statistic: F
#@ Type of effect-size: partial eta squared
#@ OSF link project: https://osf.io/bzdr2/
#@ OSF link replication report: https://osf.io/kcwfa/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
library("foreign")
library("reshape2")

source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.

#Data available in dta format

file <- getOSFfile("https://osf.io/wg4ib/")
saha.data <- read.dta(file, convert.factors=TRUE) 

##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

#Gen variable for Smallgroup setting one2two = 1 meaning 1 experimenter & 2 participants
saha.data$one2two = 0
saha.data$one2two[saha.data$subject < 31] <- 1 

#exclude all participants that expected a memory test ( Memory == 1) or indicated that they did not try anything to forgett the first list (Forgetti == 0 & Treat == 1)
#drop if strategy == 6
saha.data2 <- saha.data[saha.data$Memory==0,]

names(saha.data2)[grep("List_1_Si", names(saha.data2))] <- "lista1"
names(saha.data2)[grep("List_1_Mi", names(saha.data2) )] <- "lista2"

saha.data2$proportionList1 <- 100/16* saha.data2$List_1i
saha.data2$proportionList2 <- 100/16* saha.data2$List_2i

#Reshape to long format for ANOVA
saha.data.long <- melt(saha.data2, 
                    id.vars=c("subject", "Treat", "Choice"),
                    measure.vars=c("lista1", "lista2"),
                    variable.name="a.name",
                    value.name = "lista")

#Create and code variables as in Sahakyan

saha.data.long$proportionListA = 100/8* saha.data.long$lista
saha.data.long$spaced = 1 
saha.data.long$spaced[which(saha.data.long$a.name=="lista2")] <- 0 

saha.data.long <- transform(saha.data.long, subject=factor(subject))

##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object

saha.aov = aov(proportionListA ~ (Treat*spaced) + Error(subject / spaced), data=saha.data.long)
summ.aov1 <- summary(saha.aov)


#Type 3 and type 1 SS for interaction term are the same. 

##@@ STATISTIC @@##
#F from interaction term
summ.aov1[["Error: subject:spaced"]][[1]][["F value"]][[2]]

##@@ P-VALUE @@##
summ.aov1[["Error: subject:spaced"]][[1]][["Pr(>F)"]][[2]]

##@@ SAMPLE SIZE @@##
nrow(saha.data2)

##@@ EFFECT SIZE @@##
sumsq.error <- summ.aov1[["Error: subject:spaced"]][[1]][["Sum Sq"]][[3]]
sumsq.effect <- summ.aov1[["Error: subject:spaced"]][[1]][["Sum Sq"]][[2]]

effect.size <- sumsq.effect / (sumsq.effect + sumsq.error)
print(effect.size)  
##@@ AGREEMENT WITH AUTHORS @@##

TRUE

#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---