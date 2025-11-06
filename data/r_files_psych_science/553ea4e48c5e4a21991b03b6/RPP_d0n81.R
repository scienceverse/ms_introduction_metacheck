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
#@ Study Title: Action dynamics reveal parallel competition in decision making
#@ Coder name: Sacha Epskamp
#@ Coder e-mail: sacha.epskamp@gmail.com
#@ Type of statistic: F
#@ Type of effect-size: ---ENTER HERE THE TYPE OF STATISTIC---
#@ OSF link project: https://osf.io/d0n81
#@ OSF link replication report: https://osf.io/pu9nb/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("xlsx")
library("ez")
library("plyr")
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.

File <- getOSFfile('https://osf.io/yjk4s/')
Data <- read.xlsx(File, sheetIndex=2, header=FALSE, stringsAsFactors=FALSE)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

Data <- Data[,-1]
Data <- as.matrix(Data)
Data[Data=="[]"] <- NA
mode(Data) <- "numeric"
data2 <- list()
for (i in 1:ncol(Data)){
  data2[[i]] <- cbind(Data[1,i],Data[-1,i])
}
Data3 <- do.call(rbind,data2)
Data3 <- as.data.frame(Data3)
names(Data3) <- c("truthvalue","entrophy")

Data.p <- ddply(Data3, .(truthvalue), summarise, 
                difference = abs(unique(truthvalue) - 0.5),
                entropy = mean(entrophy,na.rm=TRUE))

##@@ DATA ANLAYSIS @@##
fit <- lm(entropy ~ difference, Data.p)
summary(fit)

##@@ STATISTIC @@##

summary(fit)$fstatistic[1]

##@@ P-VALUE @@##

summary(fit)$coefficients[2,4]

##@@ SAMPLE SIZE @@##

nrow(Data.p)

##@@ EFFECT SIZE @@##
sqrt(summary(fit)$r.squared)
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE

#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---