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
#@ Study Title: Extremal edges: a powerful cue to depth perception and figure-ground organization.
#@ Coder name: Onur Sahin
#@ Coder e-mail: 92onursahin@gmail.com
#@ Type of statistic: t-test
#@ Type of effect-size: d
#@ OSF link project: https://osf.io/0aifq/
#@ OSF link replication report: https://osf.io/4ynbx/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library('xlsx')
library("reshape2")
library("compute.es")

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
File <- getOSFfile('https://osf.io/yk63j/')
Data <- read.xlsx(File, 1)
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL difference between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
## I subset the data, so I only have the columns that are informative for me
## I also removed the subjects that the authors removed, subj 5 and subj 6
newData <- subset(Data, SubjName == "SAX_ee_1" | SubjName == "SAX_ee_2" |
                    SubjName == "SAX_ee_3" | SubjName == "SAX_ee_4" | 
                    SubjName == "SAX_ee_7" | SubjName == "SAX_ee_8" |
                    SubjName == "SAX_ee_9" |SubjName == "SAX_ee_10", 
                    select = c(SubjName, Response, Texture.cond., Textures, Color.cond.))

## Here I made sure I only see the scores on the textures I need to compare:
newerData <- subset(newData, Textures == "Convex2 vs EE1")
## Here I transformed the "left" or "right" strings to numeric scores:
newerData$Response <- as.numeric(newerData$Response)

## I saw that the authors flipped the scores in the 4_3 condition
## This is because in this condition the textures switch places, so the left right
## responses need to be flipped to correspond to the corresponding texture
## The first step is to prevent that in the second step all the numbers become 2:
newerData$Response[newerData$Texture.cond. == "4_3" & newerData$Response == 2] <- 3
newerData$Response[newerData$Texture.cond. == "4_3" & newerData$Response == 1] <- 2
newerData$Response[newerData$Texture.cond. == "4_3" & newerData$Response == 3] <- 1
## I reshape the data so it becomes much more clear:
reshapedData <- dcast(newerData, SubjName ~ Texture.cond. + Color.cond., value.var = "Response")
## I sum the total times a subject presses left or right, and divide by 8 to achieve percentages
timesRight <- cbind(rowSums(reshapedData == 2))
timesLeft  <- cbind(rowSums(reshapedData == 1))
percentageLeft <- timesLeft/8
percentageRight <- timesRight/8

  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or sumarry call on the resulting object
## I am guessing the authors compared to chance level, so I set mu to 0.50
Result <- t.test(percentageRight, mu = 0.50) 
print(Result)

##@@ STATISTIC @@##
Result$statistic

##@@ P-VALUE @@##
Result$p.value
  
##@@ SAMPLE SIZE @@##
nrow(reshapedData)

##@@ EFFECT SIZE @@##
t <- Result$statistic     #To calculate effect size I use t statistic
n <- nrow(reshapedData)   # And n
efd <- t/sqrt(n)          
eSize <- des(efd, n.1 = n, n.2 = n, level = 95, dig = 2, id = NULL, data = NULL)
eSize$d      #Using the package compute.es I calculate and substract d
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---