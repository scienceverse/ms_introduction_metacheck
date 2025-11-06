##### DATA ANALYSIS #####

###### SET UP ENVIRONMENT #####
###Load in packages
library(lme4) #Version 1.1-27.1
library(lavaan) #Version 0.6-13
library(rockchalk) #Version 1.8.144
library(ggplot2) #Version 3.4.0
library(papaja) #Version 0.1.0.9997
library(emmeans) #Version 1.4.6
library(effsize) #Version 0.8.0
library(lubridate) #Version 1.7.8

###Load in and merge the data
t1 <- read.delim("Time 1 Final Data.csv", sep = ",")
t2 <- read.delim("Time 2 Final Data.csv", sep = ",")
leg <- read.delim("Legend for Matching.csv", sep = ",")
leg$X <- NULL

data <- merge(t1, leg, by = "t1_ResponseId")
data <- merge(data, t2, by = "t2_ResponseId")
remove(leg)

who <- read.delim("WHO Epicurve by Date of Onset.csv", sep = ",")

###Dummy code, effect code, and center variables
t1$t1_highRisk_dummy <- as.numeric(dplyr::recode(t1$t1_highRisk, 
                                              "Not high risk" = "0", 
                                              "High risk" = "1"))
data$t2_highRisk_dummy <- as.numeric(dplyr::recode(data$t2_highRisk, 
                                                 "Not high risk" = "0", 
                                                 "High risk" = "1"))

data$t2_receivedMpoxVax_dummy <- as.numeric(dplyr::recode(data$t2_receivedMpoxVax, 
                                                          "No" = "0", 
                                                          "Yes" = "1"))

data$t1_receivedMpoxVax_dummy <- as.numeric(dplyr::recode(data$t1_receivedMpoxVax,
                                                          "No" = "0",
                                                          "Yes" = "1"))

t1$t1_ec_c <- scale(t1$t1_ec, scale = F)
t1$t1_out_c <- scale(t1$t1_out, scale = F)
t1$t1_dSES_c <- scale(t1$t1_dSES, scale = F) #will not run, see note on lines 253-260

data$t1_ec_c <- scale(data$t1_ec, scale = F)
data$t1_out_c <- scale(data$t1_out, scale = F)
data$t1_discloseRiskVax_v1_log_c <- scale(data$t1_discloseRiskVax_v1_log, scale = F)
data$t1_intentMpoxVax_c <- scale(data$t1_intentMpoxVax, scale = F)
data$t1_cc_c <- scale(data$t1_cc, scale = F)
data$t2_resourceKnowledge_c <- scale(data$t2_resourceKnowledge, scale = F)
data$t1_dSES_c <- scale(data$t1_dSES, scale = F) #will not run, see note on lines 253-260

data$t1_intentMpoxTest_cbd_c <- scale(data$t1_intentMpoxTest_cbd, scale = F)

##### Participant Demographics #####
### Time 1 Sample ###
psych::describe(t1$t1_dAge) #will not run, see note on lines 253-260
table(t1$t1_dCountry) #will not run, see note on lines 253-260
table(t1$t1_dEthnicity) #will not run, see note on lines 253-260
table(t1$t1_dSexuality) #will not run, see note on lines 253-260
table(t1$t1_dTrans) #will not run, see note on lines 253-260
table(t1$t1_dGender) #will not run, see note on lines 253-260
table(t1$t1_dRace) #will not run, see note on lines 253-260
table(t1$t1_dReligion) #will not run, see note on lines 253-260
psych::describe(t1$t1_dPolitical) #will not run, see note on lines 253-260
table(t1$t1_dRelStatus) #will not run, see note on lines 253-260
psych::describe(t1$t1_dSES) #will not run, see note on lines 253-260
table(t1$t1_dSubnationalAus) #will not run, see note on lines 253-260
table(t1$t1_dSubnationalCan) #will not run, see note on lines 253-260
table(t1$t1_dSubnationalUK) #will not run, see note on lines 253-260
table(t1$t1_dSubnationalUSA) #will not run, see note on lines 253-260

### Combined Samples ###
psych::describe(data$t1_dAge) #will not run, see note on lines 253-260
table(data$t1_dCountry) #will not run, see note on lines 253-260
table(data$t1_dEthnicity) #will not run, see note on lines 253-260
table(data$t1_dSexuality) #will not run, see note on lines 253-260
table(data$t1_dTrans) #will not run, see note on lines 253-260
table(data$t1_dGender) #will not run, see note on lines 253-260
table(data$t1_dRace) #will not run, see note on lines 253-260
table(data$t1_dReligion) #will not run, see note on lines 253-260
psych::describe(data$t1_dPolitical) #will not run, see note on lines 253-260
table(data$t1_dRelStatus) #will not run, see note on lines 253-260
psych::describe(data$t1_dSES) #will not run, see note on lines 253-260
table(data$t1_dSubnationalAus) #will not run, see note on lines 253-260
table(data$t1_dSubnationalCan) #will not run, see note on lines 253-260
table(data$t1_dSubnationalUK) #will not run, see note on lines 253-260
table(data$t1_dSubnationalUSA) #will not run, see note on lines 253-260

##### Visualize Data Collection Timeline #####
who$reference_date <- as_date(who$reference_date)

whoAll <- aggregate(cases ~ reference_date, data = who, sum)

whoSub <- who[who$country == "Australia" | 
                who$country == "Canada" | 
                who$country == "The United Kingdom" |
                who$country == "United States of America",]
whoSub <- whoSub[whoSub$reference_date < as_date("2022_12_01"),]
whoSub$country[whoSub$country == "The United Kingdom"] <- "U.K."
whoSub$country[whoSub$country == "United States of America"] <- "U.S.A"

ggplot(whoSub, aes(x = reference_date, y = cases, fill = country)) + 
  annotate("rect", xmin = as_date("2022-08-10"), xmax = as_date("2022-08-15"), ymin = 0,ymax = 700, fill = "light grey") +
  annotate("rect", xmin = as_date("2022-10-06"), xmax = as_date("2022-11-06"), ymin = 0,ymax = 700, fill = "light grey") +
  geom_segment(aes(x=as_date("2022-08-10"), xend=as_date("2022-08-15"), y=700, yend = 700), colour="black") +
  geom_segment(aes(x=as_date("2022-10-06"), xend=as_date("2022-11-06"), y=700, yend = 700), colour="black") +
  annotate("text", x = as_date("2022-08-12") + 0.5, y = 725, label = "Time 1", family = "Times", size = 5) +
  annotate("text", x = as_date("2022-10-21") + 0.5, y = 725, label = "Time 2", family = "Times", size = 5) +
  geom_col(width = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = c("%B %e, %Y"), expand = c(0, 0), name = "Date") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 750), breaks = seq(0, 700, by = 100), name = "Cases") +
  scale_fill_manual(values = c("#000000", "#424242", "#7a7a7a", "#bcbcbc"), name = "Country:") +
  theme_apa() +
  theme(text = element_text(family = "Times", size = 17.5),
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "bottom")
ggsave("outbreakPlot.png", width = 8, height = 7, units = "in", dpi = 320)

##### Calculate ICCs #####
#Pre-registered dependent variable
summary(lmer(t1_intentMpoxVax ~ (1 | t1_dCountry), data = t1)) #ICC = .04 #will not run, see note on lines 253-260

#Additional, exploratory dependent variables
summary(lmer(t1_intentMpoxTest ~ (1 | t1_dCountry), data = t1)) #ICC = .00 #will not run, see note on lines 253-260
summary(glmer(t1_highRisk_dummy ~ (1 | t1_dCountry), data = t1, family = "binomial")) #ICC = .00 (0/(0+(3.14159^2/3))) #will not run, see note on lines 253-260
summary(lmer(t1_localMSMMpoxIssue ~ (1 | t1_dCountry), data = t1)) #ICC = .00 #will not run, see note on lines 253-260

##### Time 1 Analyses #####
#High Risk Behavior
summary(glm(t1_highRisk_dummy ~ t1_ec_c, data = t1, family = "binomial")) #b = .02, SE = 0.06, Z = 0.32, p = .749
exp(0.01869) #OR = 1.02

#Local Mpox issue
summary(lm(t1_localMSMMpoxIssue ~ t1_ec_c, data = t1)) #b = 0.05, SE = 0.05, t(862) = 1.03, p = .305, R2 = .00

# Vaccine Intentions
summary(lm(t1_intentMpoxVax ~ t1_ec_c, data = t1)) #b = -0.17, SE = 0.05, t(862) = -3.26, p = .001, R2 = .01

# Testing Intentions
summary(lm(t1_intentMpoxTest_cbd ~ t1_ec_c, data = t1)) #b = -12.41, SE = 2.82, t(862) = -4.41, p < .001, R2 = .02

##### Longitudinal Analyses #####
#High Risk Behavior
summary(glm(t2_highRisk_dummy ~ t1_ec_c, data = data, family = "binomial")) #b = -.01, SE = 0.07, Z = -0.16, p = .871
exp(-0.01077) #OR = 0.99

#Local Mpox issue
summary(lm(t2_localMSMMpoxIssue ~ t1_ec_c, data = data)) #b = 0.03, SE = 0.05, t(683) = 0.69, p = .493, R2 = .00

# Vaccine Intentions
summary(lm(t2_intentMpoxVax ~ t1_ec_c, data = data)) #b = -0.17, SE = 0.06, t(683) = -2.95, p = .003, R2 = .01

# Testing Intentions
summary(lm(t2_intentMpoxTest_cbd ~ t1_ec_c, data = data)) #b = -15.25, SE = 3.12, t(683) = -4.89, p < .001, R2 = .03

#Variance on Health Beahvior Outcomes
table(data$t2_receivedMpoxVax) #6.28% of sample vaccinated
table(data$t2_receivedMpoxTest) #2.19% of sample tested
table(data$t2_mpoxDuringOutbreak) #0.29% of sample infected

#Vaccination
summary(glm(t2_receivedMpoxVax_dummy ~ t1_ec_c, data = data, family = "binomial")) #b = -0.42, SE = 0.15, Z = -2.82, p = .005
exp(-0.4243) #OR = 0.65
vaxModelData <- data.frame(emmeans(glm(t2_receivedMpoxVax_dummy ~ t1_ec_c, data = data, family = "binomial"), ~ t1_ec_c,
                                   at = list(
                                     t1_ec_c = c(min(data$t1_ec_c),
                                                 max(data$t1_ec_c)))))
vaxModelData$prob <- exp(vaxModelData$emmean)/(1 + exp(vaxModelData$emmean))

##### Exploratory Longitudinal Analyses #####
cor.test(data$t1_intentMpoxVax_c, data$t2_intentMpoxVax) #r = .67
cor.test(data$t1_intentMpoxTest_cbd, data$t2_intentMpoxTest_cbd) #r = .59
cor.test(data$t1_receivedMpoxVax_dummy, data$t2_receivedMpoxVax_dummy) #r = .57

summary(lm(t2_intentMpoxVax ~ t1_ec_c + t1_intentMpoxVax_c, data = data)) #no longer sig
  getDeltaRsquare(lm(t2_intentMpoxVax ~ t1_ec_c + t1_intentMpoxVax_c, data = data))
summary(lm(t2_intentMpoxTest_cbd ~ t1_ec_c + t1_intentMpoxTest_cbd_c, data = data)) #still sig
  getDeltaRsquare(lm(t2_intentMpoxTest_cbd ~ t1_ec_c + t1_intentMpoxTest_cbd_c, data = data))
summary(glm(t2_receivedMpoxVax_dummy ~ t1_ec_c + t1_receivedMpoxVax_dummy, data = data, family = "binomial")) #no longer sig
  exp(-0.2926)

##### Mediation Analysis #####
model <- "
  #first serial mediation path
  t1_cc_c ~ a11*t1_ec_c
  t2_resourceKnowledge_c ~ a12*t1_ec_c + d1*t1_cc_c

  #second serial mediation path
  t1_discloseRiskVax_v1_log_c ~ a21*t1_ec_c
  t1_intentMpoxVax_c ~ a22*t1_ec_c + d2*t1_discloseRiskVax_v1_log_c

  #paths to DV
  t2_receivedMpoxVax_dummy ~ c*t1_ec_c + b11*t1_cc_c + b12*t2_resourceKnowledge_c + b21*t1_discloseRiskVax_v1_log_c + b22*t1_intentMpoxVax_c
  
  #covariances
  t1_discloseRiskVax_v1_log_c ~~ t1_cc_c
  t1_discloseRiskVax_v1_log_c ~~ t2_resourceKnowledge_c
  t1_intentMpoxVax_c ~~ t1_cc_c
  t1_intentMpoxVax_c ~~ t2_resourceKnowledge_c
  
  #indirect and total effects
  simple_ind_1 := a11 * b11       #conceal  > cc      > vax
  simple_ind_2 := a12 * b12       #conceal  > know    > vax
  serial_ind_1 := a11 * d1 * b12  #conceal  > cc      > know    > vax
  simple_ind_3 := a21 * b21       #conceal  > risk    > vax
  simple_ind_4 := a22 * b22       #conceal  > intent  > vax
  serial_ind_2 := a21 * d2 * b22  #conceal  > risk    > intent  > vax
  tot := c + simple_ind_1 + simple_ind_2 + serial_ind_1 + simple_ind_3 + simple_ind_4 + serial_ind_2
"
set.seed(1)
fit <- sem(model, data = data, ordered = "t2_receivedMpoxVax_dummy", se = "boot", bootstrap = 5000, estimator = "DWLS")
lavaan::parameterEstimates(fit, boot.ci.type = "bca.simple", standardized = TRUE)
  
##### Supplementary Robustness Analyses #####
### Outness predictor
summary(glm(t1_highRisk_dummy ~ t1_out_c, data = t1, family = "binomial"))
exp(0.05177)

summary(lm(t1_localMSMMpoxIssue ~ t1_out_c, data = t1))

summary(lm(t1_intentMpoxVax ~ t1_out_c, data = t1))

summary(lm(t1_intentMpoxTest_cbd ~ t1_out_c, data = t1))

summary(glm(t2_highRisk_dummy ~ t1_out_c, data = data, family = "binomial"))
exp(0.10250)

summary(lm(t2_localMSMMpoxIssue ~ t1_out_c, data = data))

summary(lm(t2_intentMpoxVax ~ t1_out_c, data = data))

summary(lm(t2_intentMpoxTest_cbd ~ t1_out_c, data = data))

summary(glm(t2_receivedMpoxVax_dummy ~ t1_out_c, data = data, family = "binomial"))
exp(0.3541)

summary(lm(t2_intentMpoxVax ~ t1_out_c + t1_intentMpoxVax_c, data = data))

summary(lm(t2_intentMpoxTest_cbd ~ t1_out_c + t1_intentMpoxTest_cbd_c, data = data))

summary(glm(t2_receivedMpoxVax_dummy ~ t1_out_c + t1_receivedMpoxVax_dummy, data = data, family = "binomial"))

summary(lm(t2_intentMpoxVax ~ t1_ec_c + t1_intentMpoxVax_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data))

summary(lm(t2_intentMpoxTest_cbd ~ t1_ec_c + t1_intentMpoxTest_cbd_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data))

summary(glm(t2_receivedMpoxVax_dummy ~ t1_ec_c + t1_receivedMpoxVax_dummy + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data, family = "binomial"))

modelOut <- "
  #first serial mediation path
  t1_cc_c ~ a11*t1_out_c
  t2_resourceKnowledge_c ~ a12*t1_out_c + d1*t1_cc_c
  
  #second serial mediation path
  t1_discloseRiskVax_v1_log_c ~ a21*t1_out_c
  t1_intentMpoxVax_c ~ a22*t1_out_c + d2*t1_discloseRiskVax_v1_log_c
  
  #paths to DV
  t2_receivedMpoxVax_dummy ~ c*t1_out_c + b11*t1_cc_c + b12*t2_resourceKnowledge_c + b21*t1_discloseRiskVax_v1_log_c + b22*t1_intentMpoxVax_c
  
  #covariances
  t1_discloseRiskVax_v1_log_c ~~ t1_cc_c
  t1_discloseRiskVax_v1_log_c ~~ t2_resourceKnowledge_c
  t1_intentMpoxVax_c ~~ t1_cc_c
  t1_intentMpoxVax_c ~~ t2_resourceKnowledge_c
  
  #indirect and total effects
  simple_ind_1 := a11 * b11       #conceal  > cc      > vax
  simple_ind_2 := a12 * b12       #conceal  > know    > vax
  serial_ind_1 := a11 * d1 * b12  #conceal  > cc      > know    > vax
  simple_ind_3 := a21 * b21       #conceal  > risk    > vax
  simple_ind_4 := a22 * b22       #conceal  > intent  > vax
  serial_ind_2 := a21 * d2 * b22  #conceal  > risk    > intent  > vax
  tot := c + simple_ind_1 + simple_ind_2 + serial_ind_1 + simple_ind_3 + simple_ind_4 + serial_ind_2
"
set.seed(2)
fitOut <- sem(modelOut, data = data, ordered = "t2_receivedMpoxVax_dummy", se = "boot", bootstrap = 5000, estimator = "DWLS")
lavaan::parameterEstimates(fitOut, boot.ci.type = "bca.simple", standardized = TRUE)

### Demographic covariates

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
# NOTE THAT THE ROBUSTNESS ANALYSES WITH DEMOGRAPHIC COVARIATES WILL NOT RUN BECAUSE DEMOGRAPHIC VARIABLES HAVE BEEN REMOVED FROM THE DATASET FOR PARTICIPANT ANONYMITY #
# IF YOU NEED THE UNREDACTED VERSION OF THE DATASET, SEE THE README FILE IN THE OSF REPOSITORY #
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ########## ##### ##### ##### ##### ##### #####

#Run models
summary(glm(t1_highRisk_dummy ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = t1, family = "binomial"))
exp(0.024004)

summary(lm(t1_localMSMMpoxIssue ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = t1))

summary(lm(t1_intentMpoxVax ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = t1))

summary(lm(t1_intentMpoxTest_cbd ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = t1))

summary(glm(t2_highRisk_dummy ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data, family = "binomial"))
exp(-0.002490)

summary(lm(t2_localMSMMpoxIssue ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data))

summary(lm(t2_intentMpoxVax ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data))

summary(lm(t2_intentMpoxTest_cbd ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data))

summary(glm(t2_receivedMpoxVax_dummy ~ t1_ec_c + t1_dAge + t1_dSES_c + t1_dRace + t1_dSexuality + t1_dTrans + t1_dRelStatus + t1_dCountry, data = data, family = "binomial"))
exp(-0.508685)


