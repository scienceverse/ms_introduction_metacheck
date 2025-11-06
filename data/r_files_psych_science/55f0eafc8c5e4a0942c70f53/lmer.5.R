rm(list=ls(all=TRUE)) 
library(lme4)
sessionInfo()
setwd("[DIRECTORY]")

###################################################
#read in data, convert #'s to factor, construct contrast codes for fixed effects
###################################################

data <- as.data.frame(read.delim(file="data.txt",sep="\t"))


#redefine numeric data as factors 
data$Version <- as.factor(data$Version)

#scale the numeric data within the database
data$expCode <- (data$Exp == "Faces/Letter") - .5
data$expCode <- data$expCode - mean(data$expCode)


data$Fsib     <- (data$fricative == "s") + (data$fricative == "sh") + (data$fricative == "z") + (data$fricative=="zh") - .5
data$Ffront   <- (data$fricative == "s") + (data$fricative == "z")  + (data$fricative == "f") + (data$fricative== "v") - .5
data$Fvoice   <- (data$fricative == "z") + (data$fricative == "zh") + (data$fricative == "v") + (data$fricative=="dh") - .5

data$vLow     <- (data$vowel == "ae") + (data$vowel == "ah") - .5
data$vFront   <- (data$vowel == "ae") + (data$vowel == "i") - .5

data$Male	  <- (data$Gender == "M") - .5

#eliminate multilinguals from dataset

data <- data[ data$MultiLingual == "N", ]
summary(data)


#now create a reduced data set for the No Expectations Condition
dataN <- data[ data$expCode < 0, ]


#### model structure
ctrl=glmerControl(optCtrl=list(maxfun=50000), optimizer="bobyqa")

##############################################
#first conduct the models with just the no expectations condition
##############################################

resultsFMl.n <- glmer(Correct ~ Fvoice + Fsib + Ffront + Male + (Male + Fvoice + Fsib + Ffront |SubCode), data=dataN, family=binomial, control=ctrl)
resultsVl.n  <- glmer(Correct ~ vLow + vFront + (vLow + vFront|SubCode), data=dataN, family=binomial, control=ctrl)



##############################################
#models for complete dataset (both face+letter and no-exp condition
##############################################

#model with fricative properties and gender
resultsFMl <- glmer(Correct ~ expCode*Fvoice + expCode*Fsib + expCode*Ffront  + expCode*Male + (Male + Fvoice + Fsib + Ffront |SubCode), data=data, family=binomial, control=ctrl)

#model with vowel properties
resultsVl  <- glmer(Correct ~ expCode*vLow + expCode*vFront + (vLow + vFront|SubCode), data=data, family=binomial, control=ctrl)




##############################################
##### Now sepate for the interactions: voicing, gender, front
##############################################



## gender ##
dataM <- data[ data$Gender == "M", ]
dataF <- data[ data$Gender== "F", ]

resultsFm <- glmer(Correct ~ expCode*Fvoice + expCode*Fsib + expCode*Ffront + (Fvoice + Fsib + Ffront |SubCode), data=dataM, family=binomial, control=ctrl)
resultsFf <- glmer(Correct ~ expCode*Fvoice + expCode*Fsib + expCode*Ffront + (Fvoice + Fsib + Ffront |SubCode), data=dataF, family=binomial, control=ctrl)

## Voicing ##
dataVCD <- data[ data$Voiced  == "voiced", ]
dataVL  <- data[ data$Voiced == "Voiceless", ]

resultsFvcd <- glmer(Correct ~ expCode*Male + expCode*Fsib + expCode*Ffront + (Male + Fsib + Ffront |SubCode), data=dataVCD, family=binomial, control=ctrl)
resultsFvl  <- glmer(Correct ~ expCode*Male + expCode*Fsib + expCode*Ffront + (Male + Fsib + Ffront |SubCode), data=dataVL , family=binomial, control=ctrl)


#place
dataC  <- data[ data$Place == "coronal", ]
dataID <- data[ data$Place == "interdental", ]
dataLD <- data[ data$Place == "labiodental", ]
dataPA <- data[ data$Place == "post-alveolar", ]

resultsFc  <- glmer(Correct ~ expCode*Fvoice + expCode*Male + (Fvoice + Male |SubCode), data=dataC, family=binomial, control=ctrl)
resultsFid <- glmer(Correct ~ expCode*Fvoice + expCode*Male + (Fvoice + Male |SubCode), data=dataID, family=binomial, control=ctrl)
resultsFld <- glmer(Correct ~ expCode*Fvoice + expCode*Male + (Fvoice + Male |SubCode), data=dataLD, family=binomial, control=ctrl)
resultsFpa <- glmer(Correct ~ expCode*Fvoice + expCode*Male + (Fvoice + Male |SubCode), data=dataPA, family=binomial, control=ctrl)


