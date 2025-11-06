##### TIME 2 DATA CLEANING #####

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
data <- read.delim("Time 2 Anonymized Raw Data.csv", sep = ",", stringsAsFactors = FALSE)
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
  ifelse(x == "-3This person definitely does not know that you are ${q://QID251/ChoiceTextEntryValue}", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3This person definitely knows that you are ${q://QID251/ChoiceTextEntryValue} and it is openly talked about", 3, 
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

recodeQuant2 <- function(x) {
  ifelse(x == "-3Much less than usual", -3, 
         ifelse(x == "-2", -2,
                ifelse(x == "-1", -1,
                       ifelse(x == "0", 0,
                              ifelse(x == "1", 1, 
                                     ifelse(x == "2", 2, 
                                            ifelse(x == "3Much more than usual", 3, x)))))))}

recodeQuant3 <- function(x) {
  ifelse(x == "0Not at all", 0, 
         ifelse(x == "1", 1,
                ifelse(x == "2", 2,
                       ifelse(x == "3", 3,
                              ifelse(x == "4", 4, 
                                     ifelse(x == "5", 5, 
                                            ifelse(x == "6A great deal", 6, x)))))))}

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

###### Pre-Registered Exclusions From Dataset ######
#Remove non-participants
data <- data[data$Finished == "TRUE",] #686 participants completed T2

#Remove participants who do not meet pre-screening criteria
table(data$sidForPipe) #no responses indicated lack of same-sex sexual attraction or wouldn't make sense when piped into future questions; note this demographic variable was removed from the anonymous dataset

#Remove participants who failed the attention checks
data <- data[data$attn1 == "3Strongly Agree",] #0 failed
data <- data[data$attn2 == "YES: I paid attention and answered carefully.",] #0 failed

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
analysisvars <- c("ResponseId",
                  names(data)[startsWith(names(data), "ec")],
                  names(data)[startsWith(names(data), "out")],
                  "OutQNAFlag",
                  names(data)[startsWith(names(data), "sic")],
                  "concealChange",
                  names(data)[startsWith(names(data), "disclose")],
                  names(data)[startsWith(names(data), "cc")],
                  names(data)[startsWith(names(data), "belong")],
                  names(data)[startsWith(names(data), "socsupp")],
                  names(data)[startsWith(names(data), "se_")],
                  "stigma", "mediaPubHealthStig", "backlashWorry", "stigChange",
                  names(data)[startsWith(names(data), "chThreat")],
                  names(data)[startsWith(names(data), "contextThreat")],
                  "receivedMpoxVax", "VaxQNAFlag",
                  "mpoxTest2022", "mpoxTestFuture",
                  "healthSeekGeneral",
                  names(data)[startsWith(names(data), "risk")],
                  "sexHealthBehChange", "closeContacBehChange",
                  "mpox2022", "mpoxSymptoms", 
                  "healthCoverage",
                  "physHealth", "mentHealth",
                  names(data)[startsWith(names(data), "swb")],
                  "localMSMMpoxIssue",
                  names(data)[startsWith(names(data), "socMpox")],
                  names(data)[startsWith(names(data), "persMpox")],
                  "mpoxVaxAvail", "mpoxTestAvail", "resourceKnowledge")
analysisdata <- data[analysisvars]
analysisdata <- na.omit(analysisdata) #1 removed
analysisdata$VaxQNAFlag <- NULL
analysisdata$OutQNAFlag <- NULL

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
psych::alpha(ec) #a = .86
analysisdata$t2_ec <- (analysisdata$ec1 + analysisdata$ec2 + analysisdata$ec3 + analysisdata$ec4 + 
                      analysisdata$ec5 + analysisdata$ec6) / 6
psych::describe(analysisdata$t2_ec) #M = 1.81, SD = 1.38
hist(analysisdata$t2_ec)
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
analysisdata$t2_out <- rowMeans(analysisdata[,grep("out", names(analysisdata), value=TRUE)], na.rm = T)
psych::describe(analysisdata$t2_out) #M = 0.23, SD = 1.87
hist(analysisdata$t2_out)
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
psych::alpha(sic) #a = .94
analysisdata$t2_sic <- (analysisdata$sic1 + analysisdata$sic2 + analysisdata$sic3 + analysisdata$sic4 + 
                       analysisdata$sic5 + analysisdata$sic6 + analysisdata$sic7 + analysisdata$sic8) / 8
psych::describe(analysisdata$t2_sic) #M = 3.79, SD = 1.42
hist(analysisdata$t2_sic)
analysisdata$sic1 <- NULL
analysisdata$sic2 <- NULL
analysisdata$sic3 <- NULL
analysisdata$sic4 <- NULL
analysisdata$sic5 <- NULL
analysisdata$sic6 <- NULL
analysisdata$sic7 <- NULL
analysisdata$sic8 <- NULL
remove(sic, sicvars)

#Chronic identity threat
chThreatvars <- names(analysisdata)[startsWith(names(analysisdata), "chThreat")]
analysisdata <- mutate_at(analysisdata, all_of(chThreatvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(chThreatvars), as.numeric)
analysisdata$chThreat3 <- analysisdata$chThreat3 * -1
chThreat <- analysisdata[chThreatvars]
psych::alpha(chThreat) #a = .83
analysisdata$t2_chThreat <- (analysisdata$chThreat1 + analysisdata$chThreat2 + analysisdata$chThreat3) / 3
psych::describe(analysisdata$t2_chThreat) #M = -0.28, SD = 1.69
hist(analysisdata$t2_chThreat)
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
psych::alpha(contextThreat) #a = .70
analysisdata$t2_contextThreat <- (analysisdata$contextThreat1 + analysisdata$contextThreat2 + analysisdata$contextThreat3) / 3
psych::describe(analysisdata$t2_contextThreat) #M = -0.64, SD = 1.55
hist(analysisdata$t2_contextThreat)
analysisdata$contextThreat1 <- NULL
analysisdata$contextThreat2 <- NULL
analysisdata$contextThreat3 <- NULL
remove(contextThreat, contextThreatvars)

#Perception of local-MSM Monkeypox Issue
analysisdata <- mutate_at(analysisdata, "localMSMMpoxIssue", recodeAgree)
analysisdata <- mutate_at(analysisdata, "localMSMMpoxIssue", as.numeric)
names(analysisdata)[names(analysisdata) == "localMSMMpoxIssue"] <- "t2_localMSMMpoxIssue"
psych::describe(analysisdata$t2_localMSMMpoxIssue) #M = -0.86, SD = 1.71
hist(analysisdata$t2_localMSMMpoxIssue)

#Societal Monkeypox-MSM Association
socMpoxMSMAssocvars <- names(analysisdata)[startsWith(names(analysisdata), "socMpoxMSMAssoc")]
analysisdata <- mutate_at(analysisdata, all_of(socMpoxMSMAssocvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(socMpoxMSMAssocvars), as.numeric)
socMpoxMSMAssoc <- analysisdata[socMpoxMSMAssocvars]
psych::alpha(socMpoxMSMAssoc) #a = .96
analysisdata$t2_socMpoxMSMAssoc <- (analysisdata$socMpoxMSMAssoc1 + analysisdata$socMpoxMSMAssoc2 + analysisdata$socMpoxMSMAssoc3) / 3
psych::describe(analysisdata$t2_socMpoxMSMAssoc) #M = 0.92, SD = 1.63
hist(analysisdata$t2_socMpoxMSMAssoc)
analysisdata$socMpoxMSMAssoc1 <- NULL
analysisdata$socMpoxMSMAssoc2 <- NULL
analysisdata$socMpoxMSMAssoc3 <- NULL
remove(socMpoxMSMAssoc, socMpoxMSMAssocvars)

#Personal Monkeypox-MSM Association
persMpoxMSMAssocvars <- names(analysisdata)[startsWith(names(analysisdata), "persMpoxMSMAssoc")]
analysisdata <- mutate_at(analysisdata, all_of(persMpoxMSMAssocvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(persMpoxMSMAssocvars), as.numeric)
persMpoxMSMAssoc <- analysisdata[persMpoxMSMAssocvars]
psych::alpha(persMpoxMSMAssoc)
analysisdata$t2_persMpoxMSMAssoc <- (analysisdata$persMpoxMSMAssoc1 + analysisdata$persMpoxMSMAssoc2 + analysisdata$persMpoxMSMAssoc3) / 3
psych::describe(analysisdata$t2_persMpoxMSMAssoc) #M = -0.21, SD = 1.74
hist(analysisdata$t2_persMpoxMSMAssoc)
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
names(analysisdata)[names(analysisdata) == "intentMpoxVax"] <- "t2_intentMpoxVax"
psych::describe(analysisdata$t2_intentMpoxVax) #M = -0.56, SD = 2.00
hist(analysisdata$t2_intentMpoxVax)
analysisdata$planMpoxVaxRetro <- NULL
analysisdata$planMpoxVax <- NULL

#Monkeypox Vaccine Receipt
names(analysisdata)[names(analysisdata) == "receivedMpoxVax"] <- "t2_receivedMpoxVax"
table(analysisdata$t2_receivedMpoxVax) # Yes = 43, No = 642

#Monkeypox Vaccine date
table(analysisdata$mpoxVaxDate)
names(analysisdata)[names(analysisdata) == "mpoxVaxDate"] <- "t2_mpoxVaxDate"

#Monkeypox Test Intentions
analysisdata <- mutate_at(analysisdata, "mpoxTestFuture", recodeAgree)
analysisdata <- mutate_at(analysisdata, "mpoxTestFuture", as.numeric)
names(analysisdata)[names(analysisdata) == "mpoxTestFuture"] <- "t2_intentMpoxTest"
psych::describe(analysisdata$t2_intentMpoxTest) #M = 2.11, SD = 1.40, skew = -2.00
analysisdata$t2_intentMpoxTest_cbd <- (analysisdata$t2_intentMpoxTest + 4)**3
psych::describe(analysisdata$t2_intentMpoxTest_cbd) #skew = -0.91
hist(analysisdata$t2_intentMpoxTest_cbd)

#General Health-Seeking Intentions
analysisdata <- mutate_at(analysisdata, "healthSeekGeneral", recodeAgree)
analysisdata <- mutate_at(analysisdata, "healthSeekGeneral", as.numeric)
names(analysisdata)[names(analysisdata) == "healthSeekGeneral"] <- "t2_intentHealthGeneral"
psych::describe(analysisdata$t2_intentHealthGeneral) #M = 2.15, SD = 1.23, skew = -1.94
analysisdata$t2_intentHealthGeneral_cbd <- (analysisdata$t2_intentHealthGeneral + 4)**3
psych::describe(analysisdata$t2_intentHealthGeneral_cbd) #skew = -0.84
hist(analysisdata$t2_intentHealthGeneral_cbd)

#Vaccine Availability
table(analysisdata$mpoxVaxAvail) #YES = 207; NO = 62; DON'T KNOW = 416
names(analysisdata)[names(analysisdata) == "mpoxVaxAvail"] <- "t2_mpoxVaxAvail"

#Test Availability
table(analysisdata$mpoxTestAvail) #Don't know = 431, Yes 213, No = 41
names(analysisdata)[names(analysisdata) == "mpoxTestAvail"] <- "t2_mpoxTestAvail"

#Health Coverage
analysisdata <- mutate_at(analysisdata, "healthCoverage", recodeAgree)
analysisdata <- mutate_at(analysisdata, "healthCoverage", as.numeric)
names(analysisdata)[names(analysisdata) == "healthCoverage"] <- "t2_healthCoverage"
psych::describe(analysisdata$t2_healthCoverage) #M = 1.51, SD = 1.85, skew = -1.19
analysisdata$t2_healthCoverage_sq <- (analysisdata$t2_healthCoverage + 4)**2
psych::describe(analysisdata$t2_healthCoverage_sq) #skew = -0.70
hist(analysisdata$t2_healthCoverage_sq)

#Monkeypox
analysisdata$mpox2022[analysisdata$ResponseId == "R_3EnGFbVtzt8D0gk"] <- "No" #correct data for one participant who verbally told us they mis-clicked their response
analysisdata$mpox2022Date[analysisdata$ResponseId == "R_3EnGFbVtzt8D0gk"] <- NA #continued from above
names(analysisdata)[names(analysisdata) == "mpox2022"] <- "t2_mpoxDuringOutbreak"
table(analysisdata$t2_mpoxDuringOutbreak) #YES = 2, NO = 683

#Monkeypox Speculation
table(analysisdata$mpoxSymptoms)
names(analysisdata)[names(analysisdata) == "mpoxSymptoms"] <- "t2_mpoxSpeculation"

#Monkeypox date
table(analysisdata$mpox2022Date)
names(analysisdata)[names(analysisdata) == "mpox2022Date"] <- "t2_mpox2022Date"

#Monkeypox test
names(analysisdata)[names(analysisdata) == "mpoxTest2022"] <- "t2_receivedMpoxTest"
table(analysisdata$t2_receivedMpoxTest) #YES = 15, NO = 670

#Monkeypox test date
table(analysisdata$mpoxTestDate)
names(analysisdata)[names(analysisdata) == "mpoxTestDate"] <- "t2_mpoxTestDate"

#High Risk Flag
analysisdata$t2_highRisk <- NA
analysisdata$t2_highRisk[analysisdata$riskStatusSTI == "Yes" | 
                        analysisdata$riskStatusPartners == "Yes" | 
                        analysisdata$riskStatusAnon == "Yes" | 
                        analysisdata$riskStatusMonog == "Yes" | 
                        analysisdata$riskStatusGroup == "Yes" | 
                        analysisdata$riskStatusPublic == "Yes" | 
                        analysisdata$riskStatusSexWork == "Yes"] <- "High risk"
analysisdata$t2_highRisk[analysisdata$riskStatusSTI == "No" &
                        analysisdata$riskStatusPartners == "No" &
                        analysisdata$riskStatusAnon == "No" &
                        analysisdata$riskStatusMonog == "No" &
                        analysisdata$riskStatusGroup == "No" &
                        analysisdata$riskStatusPublic == "No" &
                        analysisdata$riskStatusSexWork == "No"] <- "Not high risk"
table(analysisdata$t2_highRisk) #High risk = 174, not high risk = 511
analysisdata$riskStatusSTI <- NULL
analysisdata$riskStatusPartners <- NULL
analysisdata$riskStatusAnon <- NULL
analysisdata$riskStatusMonog <- NULL
analysisdata$riskStatusGroup <- NULL
analysisdata$riskStatusPublic <- NULL
analysisdata$riskStatusSexWork <- NULL

#Disclosure Risk: Vaccination
disRiskVaxvars <- names(analysisdata)[startsWith(names(analysisdata), "discloseRiskVax")]
analysisdata <- mutate_at(analysisdata, all_of(disRiskVaxvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(disRiskVaxvars), as.numeric)
names(analysisdata)[names(analysisdata) == "discloseRiskVax1"] <- "t2_discloseRiskVax_v1"
psych::describe(analysisdata$t2_discloseRiskVax_v1) #M = -1.73, SD = 1.74, skew = 1.24
analysisdata$t2_discloseRiskVax_v1_log <- log(analysisdata$t2_discloseRiskVax_v1 + 4)
psych::describe(analysisdata$t2_discloseRiskVax_v1_log) #skew = 0.66
hist(analysisdata$t2_discloseRiskVax_v1_log)

disRiskVaxvars <- names(analysisdata)[startsWith(names(analysisdata), "discloseRiskVax")]
disRiskVax <- analysisdata[disRiskVaxvars]
psych::alpha(disRiskVax) #a = .77
analysisdata$t2_discloseRiskVax_v2 <- (analysisdata$t2_discloseRiskVax_v1 + analysisdata$discloseRiskVax2 + analysisdata$discloseRiskVax3) / 3
psych::describe(analysisdata$t2_discloseRiskVax_v2) #M = -1.57, SD = 1.59, skew = 1.05
analysisdata$t2_discloseRiskVax_v2_log <- log(analysisdata$t2_discloseRiskVax_v2 + 4)
psych::describe(analysisdata$t2_discloseRiskVax_v2_log) #skew = 0.30
hist(analysisdata$t2_discloseRiskVax_v2_log)
analysisdata$discloseRiskVax2 <- NULL
analysisdata$discloseRiskVax3 <- NULL
remove(disRiskVax, disRiskVaxvars)

#Disclosure Risk: Test
disRiskTestvars <- names(analysisdata)[startsWith(names(analysisdata), "discloseRiskTest")]
analysisdata <- mutate_at(analysisdata, all_of(disRiskTestvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(disRiskTestvars), as.numeric)
names(analysisdata)[names(analysisdata) == "discloseRiskTest1"] <- "t2_discloseRiskTest_v1"
psych::describe(analysisdata$t2_discloseRiskTest_v1) #M = -1.71, SD = 1.76, skew = 1.20
analysisdata$t2_discloseRiskTest_v1_log <- log(analysisdata$t2_discloseRiskTest_v1 + 4)
psych::describe(analysisdata$t2_discloseRiskTest_v1_log) #skew = 0.64
hist(analysisdata$t2_discloseRiskTest_v1_log)

disRiskTestvars <- names(analysisdata)[startsWith(names(analysisdata), "discloseRiskTest")]
disRiskTest <- analysisdata[disRiskTestvars]
psych::alpha(disRiskTest) #a = .78
analysisdata$t2_discloseRiskTest <- (analysisdata$t2_discloseRiskTest_v1 + analysisdata$discloseRiskTest2 + analysisdata$discloseRiskTest3) / 3
psych::describe(analysisdata$t2_discloseRiskTest) #M = -1.56, SD = 1.61, skew = 1.04
analysisdata$t2_discloseRiskTest_log <- log(analysisdata$t2_discloseRiskTest + 4)
psych::describe(analysisdata$t2_discloseRiskTest_log) #skew = 0.29
hist(analysisdata$t2_discloseRiskTest_log)
analysisdata$discloseRiskTest2 <- NULL
analysisdata$discloseRiskTest3 <- NULL
remove(disRiskTest, disRiskTestvars)

#Disclosure Risk: General Medical
disRiskGenvars <- names(analysisdata)[startsWith(names(analysisdata), "discloseRiskGeneral")]
analysisdata <- mutate_at(analysisdata, all_of(disRiskGenvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(disRiskGenvars), as.numeric)
names(analysisdata)[names(analysisdata) == "discloseRiskGeneral1"] <- "t2_discloseRiskGeneral_v1"
psych::describe(analysisdata$t2_discloseRiskGeneral_v1) #M = -2.08, SD = 1.49, skew = 1.71 -- skew could not be fixed through transformation

disRiskGenvars <- names(analysisdata)[startsWith(names(analysisdata), "discloseRiskGeneral")]
disRiskGen <- analysisdata[disRiskGenvars]
psych::alpha(disRiskGen) #a = .82
analysisdata$t2_discloseRiskGeneral_v2 <- (analysisdata$t2_discloseRiskGeneral_v1 + analysisdata$discloseRiskGeneral2 + analysisdata$discloseRiskGeneral3) / 3
psych::describe(analysisdata$t2_discloseRiskGeneral_v2) #M = -2.09, SD = 1.32, skew = 1.64
analysisdata$t2_discloseRiskGeneral_v2_log <- log(analysisdata$t2_discloseRiskGeneral_v2 + 4)
psych::describe(analysisdata$t2_discloseRiskGeneral_v2_log) #skew = 0.85
hist(analysisdata$t2_discloseRiskGeneral_v2_log)
analysisdata$discloseRiskGeneral2 <- NULL
analysisdata$discloseRiskGeneral3 <- NULL
remove(disRiskGen, disRiskGenvars)

#Physical Health
analysisdata <- mutate_at(analysisdata, "physHealth", recodeExcellent)
analysisdata <- mutate_at(analysisdata, "physHealth", as.numeric)
names(analysisdata)[names(analysisdata) == "physHealth"] <- "t2_physHealth"
psych::describe(analysisdata$t2_physHealth) #M = 0.50, SD = 1.63
hist(analysisdata$t2_physHealth)

#Mental Health
analysisdata <- mutate_at(analysisdata, "mentHealth", recodeExcellent)
analysisdata <- mutate_at(analysisdata, "mentHealth", as.numeric)
names(analysisdata)[names(analysisdata) == "mentHealth"] <- "t2_mentHealth"
psych::describe(analysisdata$t2_mentHealth) #M = 0.19, SD = 1.89
hist(analysisdata$t2_mentHealth)

#Subjective Stigma
analysisdata <- mutate_at(analysisdata, "stigma", recodeAgreeCap)
analysisdata <- mutate_at(analysisdata, "stigma", as.numeric)
names(analysisdata)[names(analysisdata) == "stigma"] <- "t2_stigma"
psych::describe(analysisdata$t2_stigma) #M = 1.45, SD = 1.45, skew = -1.22
analysisdata$t2_stigma_sq <- (analysisdata$t2_stigma + 4)**2
psych::describe(analysisdata$t2_stigma_sq) #skew = -0.40
hist(analysisdata$t2_stigma_sq)

#Community Connectedness
ccvars <- names(analysisdata)[startsWith(names(analysisdata), "cc")]
analysisdata <- mutate_at(analysisdata, all_of(ccvars), recodeAgree)
analysisdata <- mutate_at(analysisdata, all_of(ccvars), as.numeric)
cc <- analysisdata[ccvars]
psych::alpha(cc) #a = 0.94
analysisdata$t2_cc <- (analysisdata$cc1 + analysisdata$cc2 + analysisdata$cc3 + analysisdata$cc4 + 
                      analysisdata$cc5 + analysisdata$cc6 + analysisdata$cc7 + analysisdata$cc8) / 8
psych::describe(analysisdata$t2_cc) #M = 0.23, SD = 1.49
hist(analysisdata$t2_cc)
analysisdata$cc1 <- NULL
analysisdata$cc2 <- NULL
analysisdata$cc3 <- NULL
analysisdata$cc4 <- NULL
analysisdata$cc5 <- NULL
analysisdata$cc6 <- NULL
analysisdata$cc7 <- NULL
analysisdata$cc8 <- NULL
remove(cc, ccvars)

# Belonging
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "belong")]), recodeAgreeCap)
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "belong")]), as.numeric)
psych::alpha(analysisdata[names(analysisdata)[startsWith(names(analysisdata), "belong")]]) #α = .96
analysisdata$t2_belong <- (analysisdata$belong_1 + analysisdata$belong_2 + analysisdata$belong_3 + 
                                 analysisdata$belong_4 + analysisdata$belong_5 + analysisdata$belong_6) / 6
psych::describe(analysisdata$t2_belong) #M = 0.99, SD = 1.50
hist(analysisdata$t2_belong)
analysisdata[names(analysisdata)[startsWith(names(analysisdata), "belong_")]] <- NULL

# Self-Esteem
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "se_")]), recodeAgreeCap)
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "se_")]), as.numeric)
analysisdata$se_2 <- analysisdata$se_2 * -1
analysisdata$se_5 <- analysisdata$se_5 * -1
analysisdata$se_6 <- analysisdata$se_6 * -1
analysisdata$se_8 <- analysisdata$se_8 * -1
analysisdata$se_9 <- analysisdata$se_9 * -1
psych::alpha(analysisdata[names(analysisdata)[startsWith(names(analysisdata), "se_")]]) #α = 0.94
analysisdata$t2_se <- (analysisdata$se_1 + analysisdata$se_2 + analysisdata$se_3 + analysisdata$se_4 + analysisdata$se_5 + 
                      analysisdata$se_6 + analysisdata$se_7 + analysisdata$se_8 + analysisdata$se_9 + analysisdata$se_10) / 10
psych::describe(analysisdata$t2_se) #M = 0.44, SD = 1.51
hist(analysisdata$t2_se)
analysisdata[names(analysisdata)[startsWith(names(analysisdata), "se_")]] <- NULL

# Social Support
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "socsupp")]), recodeAgreeCap)
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "socsupp")]), as.numeric)
psych::alpha(analysisdata[names(analysisdata)[startsWith(names(analysisdata), "socsupp")]]) #α = 0.93
analysisdata$t2_support <- (analysisdata$socsupp1 + analysisdata$socsupp2 + analysisdata$socsupp3 + analysisdata$socsupp4 + analysisdata$socsupp5 + analysisdata$socsupp6 +
                           analysisdata$socsupp7 + analysisdata$socsupp8 + analysisdata$socsupp9 + analysisdata$socsupp10 + analysisdata$socsupp11 + analysisdata$socsupp12) / 12
psych::describe(analysisdata$t2_support) #M = 1.21, SD = 1.36
hist(analysisdata$t2_support)
analysisdata[names(analysisdata)[startsWith(names(analysisdata), "socsupp")]] <- NULL

#concealChange
analysisdata <- mutate_at(analysisdata, "concealChange", recodeQuant2)
analysisdata <- mutate_at(analysisdata, "concealChange", as.numeric)
names(analysisdata)[names(analysisdata) == "concealChange"] <- "t2_concealChange"
psych::describe(analysisdata$t2_concealChange) #M = -0.24, SD = 1.14
hist(analysisdata$t2_concealChange)

#sexHealthBehChange
analysisdata <- mutate_at(analysisdata, "sexHealthBehChange", recodeQuant3)
analysisdata <- mutate_at(analysisdata, "sexHealthBehChange", as.numeric)
names(analysisdata)[names(analysisdata) == "sexHealthBehChange"] <- "t2_sexHealthBehChange"
psych::describe(analysisdata$t2_sexHealthBehChange) #M = 1.05, SD = 1.75, skew = 1.50 -- skew can't be fixed through transformation
hist(analysisdata$t2_sexHealthBehChange)

#mediaPubHealthStig
analysisdata <- mutate_at(analysisdata, "mediaPubHealthStig", recodeAgree)
analysisdata <- mutate_at(analysisdata, "mediaPubHealthStig", as.numeric)
names(analysisdata)[names(analysisdata) == "mediaPubHealthStig"] <- "t2_mediaPubHealthStig"
psych::describe(analysisdata$t2_mediaPubHealthStig) #M = 0.49, SD = 1.73
hist(analysisdata$t2_mediaPubHealthStig)

#backlashWorry
analysisdata <- mutate_at(analysisdata, "backlashWorry", recodeAgree)
analysisdata <- mutate_at(analysisdata, "backlashWorry", as.numeric)
names(analysisdata)[names(analysisdata) == "backlashWorry"] <- "t2_backlashWorry"
psych::describe(analysisdata$t2_backlashWorry) #M = 0.25, SD = 1.99
hist(analysisdata$t2_backlashWorry)

#stigChange
analysisdata <- mutate_at(analysisdata, "stigChange", recodeAgree)
analysisdata <- mutate_at(analysisdata, "stigChange", as.numeric)
names(analysisdata)[names(analysisdata) == "stigChange"] <- "t2_stigChange"
psych::describe(analysisdata$t2_stigChange) #M = 0.56, SD = 1.62
hist(analysisdata$t2_stigChange)

#closeContacBehChange
analysisdata <- mutate_at(analysisdata, "closeContacBehChange", recodeQuant3)
analysisdata <- mutate_at(analysisdata, "closeContacBehChange", as.numeric)
names(analysisdata)[names(analysisdata) == "closeContacBehChange"] <- "t2_closeContacBehChange"
psych::describe(analysisdata$t2_closeContacBehChange) #M = 1.29, SD = 1.80, skew = 1.11
analysisdata$t2_closeContacBehChange_log <- log(analysisdata$t2_closeContacBehChange + 1)
psych::describe(analysisdata$t2_closeContacBehChange_log) #skew = 0.67
hist(analysisdata$t2_closeContacBehChange_log)

#Subjective Wellbeing
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "swb_")]), recodeAgreeCap)
analysisdata <- mutate_at(analysisdata, all_of(names(analysisdata)[startsWith(names(analysisdata), "swb_")]), as.numeric)
psych::alpha(analysisdata[names(analysisdata)[startsWith(names(analysisdata), "swb_")]]) #α = .94
analysisdata$t2_swb <- (analysisdata$swb_1 + analysisdata$swb_2 + analysisdata$swb_3 + analysisdata$swb_4 + analysisdata$swb_5) / 5
psych::describe(analysisdata$t2_swb) #M = -0.07, SD = 1.69
hist(analysisdata$t2_swb)
analysisdata[names(analysisdata)[startsWith(names(analysisdata), "swb_")]] <- NULL

#resourceKnowledge
analysisdata <- mutate_at(analysisdata, "resourceKnowledge", recodeAgree)
analysisdata <- mutate_at(analysisdata, "resourceKnowledge", as.numeric)
names(analysisdata)[names(analysisdata) == "resourceKnowledge"] <- "t2_resourceKnowledge"
psych::describe(analysisdata$t2_resourceKnowledge) #M = -0.50, SD = 1.91
hist(analysisdata$t2_resourceKnowledge)

#Open-ended experiences
names(analysisdata)[names(analysisdata) == "oeExperiences"] <- "t2_oeExperiences"

#Demographics
names(analysisdata)[names(analysisdata) == "dSexuality"] <- "t2_dSexuality"

#Feedback
names(analysisdata)[names(analysisdata) == "feedback"] <- "t2_feedback"

#ResponseId
names(analysisdata)[names(analysisdata) == "ResponseId"] <- "t2_ResponseId"

#Remove admin vars
analysisdata$sidForPipe <- NULL
analysisdata$Finished <- NULL
analysisdata$attn1 <- NULL
analysisdata$attn2 <- NULL

write.csv(analysisdata, "Time 2 Final Data.csv", row.names = FALSE)


