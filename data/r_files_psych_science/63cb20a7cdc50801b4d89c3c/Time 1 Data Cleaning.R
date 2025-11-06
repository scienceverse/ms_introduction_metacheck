##### TIME 1 DATA CLEANING #####

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
# NOTE THAT THIS SCRIPT WILL NOT RUN BECAUSE DEMOGRAPHIC VARIABLES HAVE BEEN REMOVED FROM THE PUBLIC VERSION OF THE DATASET TO PRESERVE PARTICIPANT ANONYMITY #
# PRE-CLEANED DATA ARE AVAILABLE IN THE OSF REPOSITORY #
# IF YOU NEED THE UNREDACTED VERSION OF THE DATASET, SEE THE README FILE IN THE OSF REPOSITORY #
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####

###### SET UP ENVIRONMENT #####
# Load in packages #
library(tidyverse) #Version 1.3.0
library(psych) #Version 2.1.9

# Load in the data #
data <- read.delim("Time 1 Anonymized Raw Data.csv", sep = ",", stringsAsFactors = FALSE)
data <- data[-c(1:2),-c(1:5, 7, 9:14)]

# Create function to score key variables #
recodeTime <- function(x) {
  ifelse(x == "0Never", 0, 
         ifelse(x == "1", 1,
                ifelse(x == "2", 2,
                       ifelse(x == "3", 3,
                              ifelse(x == "4", 4, 
                                     ifelse(x == "5", 5, 
                                            ifelse(x == "6All the time", 6, x)))))))}

recodeOut <- function(x) {
  ifelse(x == "-3This person definitely does not know that you are ${q://QID10/ChoiceGroup/SelectedChoicesTextEntry}", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3This person definitely knows that you are ${q://QID10/ChoiceGroup/SelectedChoicesTextEntry} and it is openly talked about", 3, 
                                                   ifelse(x == "N/A", NA, x))))))))}

recodeExtreme <- function(x) {
  ifelse(x == "0Not at all", 0, 
         ifelse(x == "1", 1,
                ifelse(x == "2", 2,
                       ifelse(x == "3", 3,
                              ifelse(x == "4", 4, 
                                     ifelse(x == "5", 5, 
                                            ifelse(x == "6Extremely", 6, x)))))))}

recodeAgree <- function(x) {
  ifelse(x == "-3Strongly disagree", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Strongly agree", 3, x)))))))}

recodeAgreeCap <- function(x) {
  ifelse(x == "-3Strongly Disagree", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Strongly Agree", 3, x)))))))}

recodeExcellent <- function(x) {
  ifelse(x == "-3Poor", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Excellent", 3, x)))))))}

recodeWarm <- function(x) {
  ifelse(x == "-3Very cold", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Very warm", 3, x)))))))}

recodePos <- function(x) {
  ifelse(x == "-3Very negative", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Very positive", 3, x)))))))}

recodeQuant <- function(x) {
  ifelse(x == "-3Not at all", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3A great deal", 3, x)))))))}

recodeBest <- function(x) {
  ifelse(x == "1Worst off", 1, 
         ifelse(x == "2", 2,
                ifelse(x == "3", 3,
                       ifelse(x == "4", 4,
                              ifelse(x == "5", 5, 
                                     ifelse(x == "6", 6, 
                                            ifelse(x == "7", 7,
                                                   ifelse(x == "8", 8,
                                                          ifelse(x == "9", 9,
                                                                 ifelse(x == "10Best off", 10, x))))))))))}

recodePolitic <- function(x) {
  ifelse(x == "-3Very Liberal", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Very Conservative", 3, x)))))))}

###### Pre-Registered Exclusions From Dataset ######
#Remove non-participants
data <- data[data$consent == "I consent to participate in this study",]
data <- data[data$Finished == "TRUE",] #1000 did the study

#Remove participants who do not meet pre-screening criteria
data <- data[data$dCountry != "Another country",] #1 removed
data <- data[data$dGender == "Man",] #33 removed
data <- data[data$dSexuality != "Straight" & 
               data$dSexuality != "Asexual",] #86 removed
table(data$dSexuality_7_TEXT) #check for responses not indicating same-sex sexual attraction or that won't make sense when piped into future questions
data <- data[data$ResponseId != "R_2QVWE3h8Q9zrXjH",] #I consider myself straight, but I am sexually attracted to transgender women. The reason I put this in other is that I do not usually consider transgender people to be the gender they have transitioned to. However, I still have sex with transgender women. I am not sure what this makes me.
data <- data[data$ResponseId != "R_3G7Bv66QY3nEk7v",] #Other sexual orientation: I don't have lavels
data <- data[data$ResponseId != "R_2RXrq9LEXX24C2P",] #Other sexual orientation: No answer
data <- data[data$ResponseId != "R_27qUDtjc6Tit215",] #Other sexual orientation: Super straight
data <- data[data$ResponseId != "R_27eXLCKKBzaUdGb",] #Other sexual orientation: left blank
data <- data[data$ResponseId != "R_1nSLNS9m5jSItYS",] #Other sexual orientation: demisexual

#Remove participants who failed the attention checks
data <- data[data$attn1 == "3Strongly Agree",] #2 removed
data <- data[data$attn2 == "YES: I paid attention and answered carefully.",] #2 removed

#Remove participants who indicated not understanding
#none

#Deal with missing data through listwise deletion
data[data == ""] <- NA

data$VaxQNAFlag <- NA
data$VaxQNAFlag[!is.na(data$planMpoxVax) | !is.na(data$planMpoxVaxRetro)] <- 1

data$OutQNAFlag <- NA
data$OutQNAFlag[data$out_1 != "N/A" |
                  data$out_2 != "N/A" |
                  data$out_3 != "N/A" |
                  data$out_4 != "N/A" |
                  data$out_5 != "N/A" |
                  data$out_6 != "N/A" |
                  data$out_7 != "N/A" |
                  data$out_8 != "N/A" |
                  data$out_9 != "N/A"] <- 1

analysisvars <- c("ResponseId", "dSexuality",
                  names(data)[startsWith(names(data), "ec")],
                  names(data)[startsWith(names(data), "out")],
                  "OutQNAFlag",
                  names(data)[startsWith(names(data), "sic")],
                  names(data)[startsWith(names(data), "ic")],
                  names(data)[startsWith(names(data), "sc")],
                  names(data)[startsWith(names(data), "chThreat")],
                  names(data)[startsWith(names(data), "contextThreat")],
                  "localMSMMpoxIssue",
                  names(data)[startsWith(names(data), "socMpox")],
                  names(data)[startsWith(names(data), "persMpox")],
                  "receivedMpoxVax", "VaxQNAFlag",
                  names(data)[startsWith(names(data), "mpox")],
                  "healthSeekGeneral",
                  names(data)[startsWith(names(data), "vax")],
                  "healthCoverage",
                  names(data)[startsWith(names(data), "risk")],
                  names(data)[startsWith(names(data), "disclose")],
                  "physHealth", "stigma", 
                  names(data)[startsWith(names(data), "therm")],
                  names(data)[startsWith(names(data), "cc")]
                  )

analysisdata <- data[analysisvars]
analysisdata <- na.omit(analysisdata) #6 removed
analysisdata$VaxQNAFlag <- NULL

analysisvars <- analysisvars[-1]
othervars <- names(data)
othervars <- othervars[!othervars %in% c(analysisvars)]
other <- data[othervars]

analysisdata <- merge(analysisdata, other, by="ResponseId")
remove(other, analysisvars, othervars, data)

###### Score Scales #####
#Sexual orientation concealment scale
ecvars <- names(analysisdata)[startsWith(names(analysisdata), "ec")]
analysisdata <- mutate_at(analysisdata, all_of(ecvars), recodeTime)
analysisdata <- mutate_at(analysisdata, all_of(ecvars), as.numeric)
ec <- analysisdata[ecvars]
psych::alpha(ec) #a = .83
analysisdata$t1_ec <- (analysisdata$ec1 + analysisdata$ec2 + analysisdata$ec3 + analysisdata$ec4 + 
                       analysisdata$ec5 + analysisdata$ec6) / 6
psych::describe(analysisdata$t1_ec) #M = 1.75, SD = 1.30
hist(analysisdata$t1_ec)
analysisdata$ec1 <- NULL
analysisdata$ec2 <- NULL
analysisdata$ec3 <- NULL
analysisdata$ec4 <- NULL
analysisdata$ec5 <- NULL
analysisdata$ec6 <- NULL
remove(ec, ecvars)

#Outness inventory
outvars <- names(analysisdata)[startsWith(names(analysisdata), "out")]
analysisdata <- mutate_at(analysisdata, all_of(outvars), recodeOut)
analysisdata <- mutate_at(analysisdata, all_of(outvars), as.numeric)
out <- analysisdata[outvars]
psych::alpha(out) #a = .93
analysisdata$t1_out <- rowMeans(analysisdata[,grep("out", names(analysisdata), value=TRUE)], na.rm = T)
psych::describe(analysisdata$t1_out) #M = 0.14, SD = 1.87
hist(analysisdata$t1_out)
analysisdata[startsWith(names(analysisdata), "out_")] <- NULL
remove(out, outvars)

#Sexual identity concealment scale
sicvars <- names(analysisdata)[startsWith(names(analysisdata), "sic")]
analysisdata <- mutate_at(analysisdata, all_of(sicvars), recodeExtreme)
analysisdata <- mutate_at(analysisdata, all_of(sicvars), as.numeric)
analysisdata$sic1 <- analysisdata$sic1 * -1 + 6
analysisdata$sic2 <- analysisdata$sic2 * -1 + 6
analysisdata$sic3 <- analysisdata$sic3 * -1 + 6
analysisdata$sic4 <- analysisdata$sic4 * -1 + 6
analysisdata$sic6 <- analysisdata$sic6 * -1 + 6
analysisdata$sic7 <- analysisdata$sic7 * -1 + 6
analysisdata$sic8 <- analysisdata$sic8 * -1 + 6
sic <- analysisdata[sicvars]
psych::alpha(sic) #a = .93
analysisdata$t1_sic <- (analysisdata$sic1 + analysisdata$sic2 + analysisdata$sic3 + analysisdata$sic4 + 
                       analysisdata$sic5 + analysisdata$sic6 + analysisdata$sic7 + analysisdata$sic8) / 8
psych::describe(analysisdata$t1_sic) #M = 3.82, SD = 1.42
hist(analysisdata$t1_sic)
analysisdata$sic1 <- NULL
analysisdata$sic2 <- NULL
analysisdata$sic3 <- NULL
analysisdata$sic4 <- NULL
analysisdata$sic5 <- NULL
analysisdata$sic6 <- NULL
analysisdata$sic7 <- NULL
analysisdata$sic8 <- NULL
remove(sic, sicvars)

#Identity centrality
icvars <- names(analysisdata)[startsWith(names(analysisdata), "ic")]
analysisdata <- mutate_at(analysisdata, all_of(icvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(icvars), as.numeric)
analysisdata$ic1 <- analysisdata$ic1 * -1
analysisdata$ic4 <- analysisdata$ic4 * -1
analysisdata$ic8 <- analysisdata$ic8 * -1
ic <- analysisdata[icvars]
psych::alpha(ic) #a = .89
analysisdata$t1_ic <- (analysisdata$ic1 + analysisdata$ic2 + analysisdata$ic3 + analysisdata$ic4 + 
                      analysisdata$ic5 + analysisdata$ic6 + analysisdata$ic7 + analysisdata$ic8) / 8
psych::describe(analysisdata$t1_ic) #M = -0.42, SD = 1.41
hist(analysisdata$t1_ic)
analysisdata$ic1 <- NULL
analysisdata$ic2 <- NULL
analysisdata$ic3 <- NULL
analysisdata$ic4 <- NULL
analysisdata$ic5 <- NULL
analysisdata$ic6 <- NULL
analysisdata$ic7 <- NULL
analysisdata$ic8 <- NULL
remove(ic, icvars)

#Stigma consciousness
scvars <- names(analysisdata)[startsWith(names(analysisdata), "sc")]
analysisdata <- mutate_at(analysisdata, all_of(scvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(scvars), as.numeric)
analysisdata$sc1 <- analysisdata$sc1 * -1
analysisdata$sc2 <- analysisdata$sc2 * -1
analysisdata$sc4 <- analysisdata$sc4 * -1
analysisdata$sc5 <- analysisdata$sc5 * -1
analysisdata$sc6 <- analysisdata$sc6 * -1
analysisdata$sc7 <- analysisdata$sc7 * -1
analysisdata$sc9 <- analysisdata$sc9 * -1
sc <- analysisdata[scvars]
psych::alpha(sc) #a = .88
analysisdata$t1_sc <- (analysisdata$sc1 + analysisdata$sc2 + analysisdata$sc3 + analysisdata$sc4 + 
                      analysisdata$sc5 + analysisdata$sc6 + analysisdata$sc7 + analysisdata$sc8 + 
                      analysisdata$sc9 + analysisdata$sc10) / 10
psych::describe(analysisdata$t1_sc) #M = -0.26, SD = 1.24
hist(analysisdata$t1_sc)
analysisdata$sc1 <- NULL
analysisdata$sc2 <- NULL
analysisdata$sc3 <- NULL
analysisdata$sc4 <- NULL
analysisdata$sc5 <- NULL
analysisdata$sc6 <- NULL
analysisdata$sc7 <- NULL
analysisdata$sc8 <- NULL
analysisdata$sc9 <- NULL
analysisdata$sc10 <- NULL
remove(sc, scvars)

#Chronic identity threat
chThreatvars <- names(analysisdata)[startsWith(names(analysisdata), "chThreat")]
analysisdata <- mutate_at(analysisdata, all_of(chThreatvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(chThreatvars), as.numeric)
analysisdata$chThreat3 <- analysisdata$chThreat3 * -1
chThreat <- analysisdata[chThreatvars]
psych::alpha(chThreat) #a = .86
analysisdata$t1_chThreat <- (analysisdata$chThreat1 + analysisdata$chThreat2 + analysisdata$chThreat3) / 3
psych::describe(analysisdata$t1_chThreat) #M = -0.21, SD = 1.72
hist(analysisdata$t1_chThreat)
analysisdata$chThreat1 <- NULL
analysisdata$chThreat2 <- NULL
analysisdata$chThreat3 <- NULL
remove(chThreat, chThreatvars)

#Contextual identity threat
contextThreatvars <- names(analysisdata)[startsWith(names(analysisdata), "contextThreat")]
analysisdata <- mutate_at(analysisdata, all_of(contextThreatvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(contextThreatvars), as.numeric)
analysisdata$contextThreat3 <- analysisdata$contextThreat3 * -1
contextThreat <- analysisdata[contextThreatvars]
psych::alpha(contextThreat) #a = .76
analysisdata$t1_contextThreat <- (analysisdata$contextThreat1 + analysisdata$contextThreat2 + analysisdata$contextThreat3) / 3
psych::describe(analysisdata$t1_contextThreat) #M = -0.93, SD = 1.60
hist(analysisdata$t1_contextThreat)
analysisdata$contextThreat1 <- NULL
analysisdata$contextThreat2 <- NULL
analysisdata$contextThreat3 <- NULL
remove(contextThreat, contextThreatvars)

#Perception of local-MSM Monkeypox Issue
analysisdata <- mutate_at(analysisdata, "localMSMMpoxIssue", recodeAgree)
analysisdata <- mutate_at(analysisdata, "localMSMMpoxIssue", as.numeric)
names(analysisdata)[names(analysisdata) == "localMSMMpoxIssue"] <- "t1_localMSMMpoxIssue"
psych::describe(analysisdata$t1_localMSMMpoxIssue) #M = -0.64, SD = 1.85
hist(analysisdata$t1_localMSMMpoxIssue)

#Societal Monkeypox-MSM Association
socMpoxMSMAssocvars <- names(analysisdata)[startsWith(names(analysisdata), "socMpoxMSMAssoc")]
analysisdata <- mutate_at(analysisdata, all_of(socMpoxMSMAssocvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(socMpoxMSMAssocvars), as.numeric)
socMpoxMSMAssoc <- analysisdata[socMpoxMSMAssocvars]
psych::alpha(socMpoxMSMAssoc) #a = .95
analysisdata$t1_socMpoxMSMAssoc <- (analysisdata$socMpoxMSMAssoc1 + analysisdata$socMpoxMSMAssoc2 + analysisdata$socMpoxMSMAssoc3) / 3
psych::describe(analysisdata$t1_socMpoxMSMAssoc) #M = 0.62, SD = 1.77
hist(analysisdata$t1_socMpoxMSMAssoc)
analysisdata$socMpoxMSMAssoc1 <- NULL
analysisdata$socMpoxMSMAssoc2 <- NULL
analysisdata$socMpoxMSMAssoc3 <- NULL
remove(socMpoxMSMAssoc, socMpoxMSMAssocvars)

#Personal Monkeypox-MSM Association
persMpoxMSMAssocvars <- names(analysisdata)[startsWith(names(analysisdata), "persMpoxMSMAssoc")]
analysisdata <- mutate_at(analysisdata, all_of(persMpoxMSMAssocvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(persMpoxMSMAssocvars), as.numeric)
persMpoxMSMAssoc <- analysisdata[persMpoxMSMAssocvars]
psych::alpha(persMpoxMSMAssoc) #a = .94
analysisdata$t1_persMpoxMSMAssoc <- (analysisdata$persMpoxMSMAssoc1 + analysisdata$persMpoxMSMAssoc2 + analysisdata$persMpoxMSMAssoc3) / 3
psych::describe(analysisdata$t1_persMpoxMSMAssoc) #M = -0.54, SD = 1.74
hist(analysisdata$t1_persMpoxMSMAssoc)
analysisdata$persMpoxMSMAssoc1 <- NULL
analysisdata$persMpoxMSMAssoc2 <- NULL
analysisdata$persMpoxMSMAssoc3 <- NULL
remove(persMpoxMSMAssoc, persMpoxMSMAssocvars)

#Monkeypox Vaccine Intentions
analysisdata$intentMpoxVax <- NA
analysisdata$intentMpoxVax[analysisdata$receivedMpoxVax == "Yes"] <- analysisdata$planMpoxVaxRetro[analysisdata$receivedMpoxVax == "Yes"]
analysisdata$intentMpoxVax[analysisdata$receivedMpoxVax == "No"] <- analysisdata$planMpoxVax[analysisdata$receivedMpoxVax == "No"]
analysisdata <- mutate_at(analysisdata, "intentMpoxVax", recodeAgree)
analysisdata <- mutate_at(analysisdata, "intentMpoxVax", as.numeric)
names(analysisdata)[names(analysisdata) == "intentMpoxVax"] <- "t1_intentMpoxVax"
psych::describe(analysisdata$t1_intentMpoxVax) #M = 0.03, SD = 2.05
hist(analysisdata$t1_intentMpoxVax)
analysisdata$planMpoxVaxRetro <- NULL
analysisdata$planMpoxVax <- NULL

#Monkeypox Vaccine Receipt
names(analysisdata)[names(analysisdata) == "receivedMpoxVax"] <- "t1_receivedMpoxVax"
table(analysisdata$t1_receivedMpoxVax) #YES = 30; NO = 835

#Monkeypox Test Intentions
analysisdata <- mutate_at(analysisdata, "mpoxTestFuture", recodeAgree)
analysisdata <- mutate_at(analysisdata, "mpoxTestFuture", as.numeric)
psych::describe(analysisdata$mpoxTestFuture) #M = 2.12, SD = 1.42, skew = -2.08
analysisdata$mpoxTestFuture_cbd <- (analysisdata$mpoxTestFuture + 4)**3
names(analysisdata)[names(analysisdata) == "mpoxTestFuture"] <- "t1_intentMpoxTest"
names(analysisdata)[names(analysisdata) == "mpoxTestFuture_cbd"] <- "t1_intentMpoxTest_cbd"
psych::describe(analysisdata$t1_intentMpoxTest_cbd) #skew = -1.00
hist(analysisdata$t1_intentMpoxTest_cbd)

#General Health-Seeking Intentions
analysisdata <- mutate_at(analysisdata, "healthSeekGeneral", recodeAgree)
analysisdata <- mutate_at(analysisdata, "healthSeekGeneral", as.numeric)
psych::describe(analysisdata$healthSeekGeneral) #M = 2.25, SD = 1.13, skew = -2.05
analysisdata$healthSeekGeneral_cbd <- (analysisdata$healthSeekGeneral + 4)**3
names(analysisdata)[names(analysisdata) == "healthSeekGeneral"] <- "t1_intentHealthGeneral"
names(analysisdata)[names(analysisdata) == "healthSeekGeneral_cbd"] <- "t1_intentHealthGeneral_cbd"
psych::describe(analysisdata$t1_intentHealthGeneral_cbd) #skew = -0.96
hist(analysisdata$t1_intentHealthGeneral_cbd)

#Vaccine Safety
analysisdata <- mutate_at(analysisdata, all_of("vaxSafe"), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of("vaxSafe"), as.numeric)
names(analysisdata)[names(analysisdata) == "vaxSafe"] <- "t1_vaxSafe"
psych::describe(analysisdata$t1_vaxSafe) #M = 2.35, SD = 1.15, skew = -2.46
analysisdata$t1_vaxSafe_qnt <- (analysisdata$t1_vaxSafe + 4)**5
psych::describe(analysisdata$t1_vaxSafe_qnt) #skew = -0.88
hist(analysisdata$t1_vaxSafe_qnt)

#Vaccine Efficacy
analysisdata <- mutate_at(analysisdata, all_of("vaxEffective"), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of("vaxEffective"), as.numeric)
names(analysisdata)[names(analysisdata) == "vaxEffective"] <- "t1_vaxEffective"
psych::describe(analysisdata$t1_vaxEffective) #M = 2.39, SD = 1.06, skew = -2.48
analysisdata$t1_vaxEffective_qnt <- (analysisdata$t1_vaxEffective + 4)**5
psych::describe(analysisdata$t1_vaxEffective_qnt) #skew = -0.87
hist(analysisdata$t1_vaxEffective_qnt)

#Vaccine Availability
table(analysisdata$mpoxVaxAvail) #YES = 157; NO = 141; DON'T KNOW = 566
names(analysisdata)[names(analysisdata) == "mpoxVaxAvail"] <- "t1_mpoxVaxAvail"

#Test Availability
table(analysisdata$mpoxTestAvail) #YES = 153; NO = 107; DON'T KNOW = 604
names(analysisdata)[names(analysisdata) == "mpoxTestAvail"] <- "t1_mpoxTestAvail"

#Health Coverage
analysisdata <- mutate_at(analysisdata, "healthCoverage", recodeAgree)
analysisdata <- mutate_at(analysisdata, "healthCoverage", as.numeric)
psych::describe(analysisdata$healthCoverage) #M = 1.38, SD = 1.92, skew = -1.13
analysisdata$healthCoverage_sq <- (analysisdata$healthCoverage + 4)**2
names(analysisdata)[names(analysisdata) == "healthCoverage"] <- "t1_healthCoverage"
names(analysisdata)[names(analysisdata) == "healthCoverage_sq"] <- "t1_healthCoverage_sq"
psych::describe(analysisdata$t1_healthCoverage_sq) #skew = -0.63
hist(analysisdata$t1_healthCoverage_sq)

#Monkeypox during outbreak
table(analysisdata$mpox2022) #YES = 3; NO = 861
names(analysisdata)[names(analysisdata) == "mpox2022"] <- "t1_mpoxDuringOutbreak"

#Monkeypox before outbreak
table(analysisdata$mpoxPre2022) #YES = 1; NO = 863
names(analysisdata)[names(analysisdata) == "mpoxPre2022"] <- "t1_mpoxBeforeOutbreak"

#Monkeypox test
table(analysisdata$mpoxTest2022) #YES = 8; NO = 856
names(analysisdata)[names(analysisdata) == "mpoxTest2022"] <- "t1_receivedMpoxTest"

#High Risk Flag
analysisdata$t1_highRisk <- NA
analysisdata$t1_highRisk[analysisdata$riskStatusSTI == "Yes" | 
                        analysisdata$riskStatusPartners == "Yes" | 
                        analysisdata$riskStatusAnon == "Yes" | 
                        analysisdata$riskStatusMonog == "Yes" | 
                        analysisdata$riskStatusGroup == "Yes" | 
                        analysisdata$riskStatusPublic == "Yes" | 
                        analysisdata$riskStatusSexWork == "Yes"] <- "High risk"
analysisdata$t1_highRisk[analysisdata$riskStatusSTI == "No" &
                        analysisdata$riskStatusPartners == "No" &
                        analysisdata$riskStatusAnon == "No" &
                        analysisdata$riskStatusMonog == "No" &
                        analysisdata$riskStatusGroup == "No" &
                        analysisdata$riskStatusPublic == "No" &
                        analysisdata$riskStatusSexWork == "No"] <- "Not high risk"
table(analysisdata$t1_highRisk) #High Risk = 235; Not High Risk = 629
analysisdata$riskStatusSTI <- NULL
analysisdata$riskStatusPartners <- NULL
analysisdata$riskStatusAnon <- NULL
analysisdata$riskStatusMonog <- NULL
analysisdata$riskStatusGroup <- NULL
analysisdata$riskStatusPublic <- NULL
analysisdata$riskStatusSexWork <- NULL

#Disclosure Risk: Vaccination
analysisdata <- mutate_at(analysisdata, "discloseRiskVax", recodeAgree)
analysisdata <- mutate_at(analysisdata, "discloseRiskVax", as.numeric)
names(analysisdata)[names(analysisdata) == "discloseRiskVax"] <- "t1_discloseRiskVax_v1"
psych::describe(analysisdata$t1_discloseRiskVax_v1) #M = -1.98, SD = 1.62, skew = 1.59
analysisdata$t1_discloseRiskVax_v1_log <- log(analysisdata$t1_discloseRiskVax_v1 + 4)
psych::describe(analysisdata$t1_discloseRiskVax_v1_log) #skew = 0.97
hist(analysisdata$t1_discloseRiskVax_v1_log)

#Disclosure Risk: Test
analysisdata <- mutate_at(analysisdata, "discloseRiskTest", recodeAgree)
analysisdata <- mutate_at(analysisdata, "discloseRiskTest", as.numeric)
names(analysisdata)[names(analysisdata) == "discloseRiskTest"] <- "t1_discloseRiskTest_v1"
psych::describe(analysisdata$t1_discloseRiskTest_v1) #M = -1.98, SD = 1.60, skew = 1.54
analysisdata$t1_discloseRiskTest_v1_log <- log(analysisdata$t1_discloseRiskTest_v1 + 4)
psych::describe(analysisdata$t1_discloseRiskTest_v1_log) #skew = 0.96
hist(analysisdata$t1_discloseRiskTest_v1_log)

#Disclosure Risk: General Medical
analysisdata <- mutate_at(analysisdata, "discloseRiskGeneral", recodeAgree)
analysisdata <- mutate_at(analysisdata, "discloseRiskGeneral", as.numeric)
names(analysisdata)[names(analysisdata) == "discloseRiskGeneral"] <- "t1_discloseRiskGeneral_v1"
psych::describe(analysisdata$t1_discloseRiskGeneral_v1) #M = -2.08, SD = 1.43, skew = 1.61
analysisdata$t1_discloseRiskGeneral_v1_log <- log(analysisdata$t1_discloseRiskGeneral_v1 + 4)
psych::describe(analysisdata$t1_discloseRiskGeneral_v1_log) #skew = 0.95
hist(analysisdata$t1_discloseRiskGeneral_v1_log)

#Physical Health
analysisdata <- mutate_at(analysisdata, "physHealth", recodeExcellent)
analysisdata <- mutate_at(analysisdata, "physHealth", as.numeric)
names(analysisdata)[names(analysisdata) == "physHealth"] <- "t1_physHealth"
psych::describe(analysisdata$t1_physHealth) #M = 0.63, SD = 1.55
hist(analysisdata$t1_physHealth)

#Subjective Stigma
analysisdata <- mutate_at(analysisdata, "stigma", recodeAgreeCap)
analysisdata <- mutate_at(analysisdata, "stigma", as.numeric)
names(analysisdata)[names(analysisdata) == "stigma"] <- "t1_stigma"
psych::describe(analysisdata$t1_stigma) #M = 1.43, SD = 1.43, skew = -1.14
analysisdata$t1_stigma_sq <- (analysisdata$t1_stigma + 4)**2
psych::describe(analysisdata$t1_stigma_sq) #skew = -0.35
hist(analysisdata$t1_stigma_sq)

#Feeling Thermometer
thermvars <- names(analysisdata)[startsWith(names(analysisdata), "therm")]
analysisdata <- mutate_at(analysisdata, "therm1", recodeWarm)
analysisdata <- mutate_at(analysisdata, "therm2", recodePos)
analysisdata <- mutate_at(analysisdata, "therm3", recodeQuant)
analysisdata <- mutate_at(analysisdata, thermvars, as.numeric)
therm <- analysisdata[thermvars]
psych::alpha(therm) #a = .93
analysisdata$t1_therm <- (analysisdata$therm1 + analysisdata$therm2 + analysisdata$therm3) / 3
psych::describe(analysisdata$t1_therm) #M = 1.80, SD = 1.20
hist(analysisdata$t1_therm)
analysisdata$therm1 <- NULL
analysisdata$therm2 <- NULL
analysisdata$therm3 <- NULL
remove(therm, thermvars)

#Community Connectedness
ccvars <- names(analysisdata)[startsWith(names(analysisdata), "cc")]
analysisdata <- mutate_at(analysisdata, all_of(ccvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(ccvars), as.numeric)
cc <- analysisdata[ccvars]
psych::alpha(cc) #a = .93
analysisdata$t1_cc <- (analysisdata$cc1 + analysisdata$cc2 + analysisdata$cc3 + analysisdata$cc4 + 
                      analysisdata$cc5 + analysisdata$cc6 + analysisdata$cc7 + analysisdata$cc8) / 8
psych::describe(analysisdata$t1_cc) #M = 0.31, SD = 1.46
hist(analysisdata$t1_cc)
analysisdata$cc1 <- NULL
analysisdata$cc2 <- NULL
analysisdata$cc3 <- NULL
analysisdata$cc4 <- NULL
analysisdata$cc5 <- NULL
analysisdata$cc6 <- NULL
analysisdata$cc7 <- NULL
analysisdata$cc8 <- NULL
remove(cc, ccvars)

#Open-ended experiences
names(analysisdata)[names(analysisdata) == "oeExperiences"] <- "t1_oeExperiences"

#Demographics
names(analysisdata)[names(analysisdata) == "dCountry"] <- "t1_dCountry"
names(analysisdata)[names(analysisdata) == "dAge"] <- "t1_dAge"
names(analysisdata)[names(analysisdata) == "dGender"] <- "t1_dGender"
names(analysisdata)[names(analysisdata) == "dTrans"] <- "t1_dTrans"
names(analysisdata)[names(analysisdata) == "dSexuality"] <- "t1_dSexuality"
names(analysisdata)[names(analysisdata) == "dSexuality_7_TEXT"] <- "t1_dSexuality_7_TEXT"
names(analysisdata)[names(analysisdata) == "dRace"] <- "t1_dRace"
names(analysisdata)[names(analysisdata) == "dEthnicity"] <- "t1_dEthnicity"
names(analysisdata)[names(analysisdata) == "dReligion"] <- "t1_dReligion"
names(analysisdata)[names(analysisdata) == "dRelStatus"] <- "t1_dRelStatus"
names(analysisdata)[names(analysisdata) == "dSubnationalAus"] <- "t1_dSubnationalAus"
names(analysisdata)[names(analysisdata) == "dSubnationalCan"] <- "t1_dSubnationalCan"
names(analysisdata)[names(analysisdata) == "dSubnationalUK"] <- "t1_dSubnationalUK"
names(analysisdata)[names(analysisdata) == "dSubnationalUSA"] <- "t1_dSubnationalUSA"

analysisdata <- mutate_at(analysisdata, "dPolitical", recodePolitic)
analysisdata <- mutate_at(analysisdata, "dPolitical", as.numeric)
names(analysisdata)[names(analysisdata) == "dPolitical"] <- "t1_dPolitical"
psych::describe(analysisdata$t1_dPolitical)
hist(analysisdata$t1_dPolitical)

analysisdata <- mutate_at(analysisdata, "dSES", recodeBest)
analysisdata <- mutate_at(analysisdata, "dSES", as.numeric)
names(analysisdata)[names(analysisdata) == "dSES"] <- "t1_dSES"
psych::describe(analysisdata$t1_dSES) #M = 5.07, SD = 1.79
hist(analysisdata$t1_dSES)

#Feedback
names(analysisdata)[names(analysisdata) == "feedback"] <- "t1_feedback"

#ResponseId
names(analysisdata)[names(analysisdata) == "ResponseId"] <- "t1_ResponseId"

#Remove admin vars
analysisdata$consent <- NULL
analysisdata$Finished <- NULL
analysisdata$OutQNAFlag <- NULL
analysisdata$attn1 <- NULL
analysisdata$attn2 <- NULL

write.csv(analysisdata, "Time 1 Final Data.csv", row.names = FALSE)






