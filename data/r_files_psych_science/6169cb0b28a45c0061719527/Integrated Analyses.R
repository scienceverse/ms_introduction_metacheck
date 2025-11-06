##########This Script Includes Data Merging, Preparation, and Analyses Reported to Accompany Manuscript and Supporting Materials

####Load Packages
library(pacman)
p_load(tidyverse, tidyr, psych, lme4, sjPlot, sjlabelled, lmerTest, car, gtools, optimx, nlme, dfoptim, optimx, parallel, minqa, sjstats, glmmTMB, glmmADMB, reshape2,BayesFactor)

#####################DATA PREPARATION

#################Fiji (Studies 1, 2, and 3)
####Load Data
EC_Fiji <- read.csv("Studies1-2-3FIJI.csv")
colnames(EC_Fiji) <- tolower(colnames(EC_Fiji)) #convert all column names to lowercase
EC_Fiji$id <- sub("^", "fj", EC_Fiji$id )
EC_Fiji$religion <-recode(EC_Fiji$rel_community,"1='Christian';2='Hindu';3='Muslim'")
EC_Fiji$context <- "Fiji"
colnames(EC_Fiji)[colnames(EC_Fiji)=="egself_percentage"] <- "self_percentage"
colnames(EC_Fiji)[colnames(EC_Fiji)=="eggod_percentage"] <- "god_percentage"
EC_Fiji$gender_text <-recode(EC_Fiji$gender,"1='Male';2='Female'")
EC_Fiji$recipient_location <- "Fiji"

EC_Fiji$recipient_religion[EC_Fiji$religion=="Christian" & EC_Fiji$condition==0] <- "Muslim"
EC_Fiji$recipient_religion[EC_Fiji$religion=="Christian" & EC_Fiji$condition==1] <- "Christian"
EC_Fiji$recipient_religion[EC_Fiji$religion=="Hindu" & EC_Fiji$condition==0] <- "Christian"
EC_Fiji$recipient_religion[EC_Fiji$religion=="Hindu" & EC_Fiji$condition==1] <- "Hindu"
EC_Fiji$recipient_religion[EC_Fiji$religion=="Muslim" & EC_Fiji$condition==0] <- "Christian"
EC_Fiji$recipient_religion[EC_Fiji$religion=="Muslim" & EC_Fiji$condition==1] <- "Muslim"
EC_Fiji$condition <- NULL
colnames(EC_Fiji)[colnames(EC_Fiji)=="recipient_religion"] <- "condition"
table(EC_Fiji$religion)
EC_Fiji$design <- "EC"
EC_Fiji$study[EC_Fiji$rel_community==1] <- "Fiji_Christian"
EC_Fiji$study[EC_Fiji$rel_community==2] <- "Fiji_Hindu"
EC_Fiji$study[EC_Fiji$rel_community==3] <- "Fiji_Muslim"

table(EC_Fiji$condition, EC_Fiji$religion)

#Subset participants who have economic game data
EC_Fiji_EG <- EC_Fiji[which(EC_Fiji$egdata==1),]
table(EC_Fiji_EG$religion)
table(EC_Fiji_EG$ethnicity, EC_Fiji_EG$study)
describeBy(EC_Fiji_EG$age, EC_Fiji_EG$study)
table(EC_Fiji_EG$gender, EC_Fiji_EG$study)

#Punish reward god knows and cares
EC_Fiji_EG$knows_01 <- (EC_Fiji_EG$god_know - 1)/2
EC_Fiji_EG$cares_01 <- (EC_Fiji_EG$god_care - 1)/2
EC_Fiji_EG$punish_01 <- (EC_Fiji_EG$god_punish_score - 1)/2
EC_Fiji_EG$reward_01 <- (EC_Fiji_EG$god_reward_score - 1)/2

describeBy(EC_Fiji_EG$knows_01, EC_Fiji_EG$rel_community)
describeBy(EC_Fiji_EG$cares_01, EC_Fiji_EG$rel_community)
describeBy(EC_Fiji_EG$punish_01, EC_Fiji_EG$rel_community)
describeBy(EC_Fiji_EG$reward_01, EC_Fiji_EG$rel_community)

EC_Fiji_EG_C <- EC_Fiji_EG[which(EC_Fiji_EG$rel_community == 1),]
t.test(EC_Fiji_EG_C$knows_01)
t.test(EC_Fiji_EG_C$cares_01)
t.test(EC_Fiji_EG_C$punish_01)
t.test(EC_Fiji_EG_C$reward_01)

EC_Fiji_EG_H <- EC_Fiji_EG[which(EC_Fiji_EG$rel_community == 2),]
t.test(EC_Fiji_EG_H$knows_01)
t.test(EC_Fiji_EG_H$cares_01)
t.test(EC_Fiji_EG_H$punish_01)
t.test(EC_Fiji_EG_H$reward_01)

EC_Fiji_EG_M <- EC_Fiji_EG[which(EC_Fiji_EG$rel_community == 3),]
t.test(EC_Fiji_EG_M$knows_01)
t.test(EC_Fiji_EG_M$cares_01)
t.test(EC_Fiji_EG_M$punish_01)
t.test(EC_Fiji_EG_M$reward_01)

#Create Small Data for Fiji
EC_Fiji_small <- EC_Fiji_EG[c("id", "religion", "condition", "self_percentage", "god_percentage", "design", "context", "recipient_location", "study", "knows_01", "cares_01", "reward_01", "punish_01")]
EC_Fiji_small$threat_composite <- NA
EC_Fiji_small$commonality_composite <- NA

table(EC_Fiji$egdata)

################STUDY 4: USA
########Load Data
Study4 <- read_csv("Study4USA.csv")
colnames(Study4) <- tolower(colnames(Study4)) #convert all column names to lowercase
colnames(Study4)[colnames(Study4)=="responseid"] <- "id"
Study4$religion <-recode(Study4$religion_participant,"1='Christian';6='Atheist'")
Study4$context <- "USA"
colnames(Study4)[colnames(Study4)=="dgpre"] <- "self_cond"
colnames(Study4)[colnames(Study4)=="dgpost"] <- "god_cond"
Study4$self_percentage <- Study4$self_cond/.4
Study4$god_percentage <- Study4$god_cond/.4
Study4$gender_text <-recode(Study4$gender,"1='Male';2='Female'; 3='Other'")
Study4$recipient_location <- "USA"
Study4_christians <- Study4[which(Study4$religion=="Christian"),]
Study4_christians$age <- Study4_christians$q226

##Big God beliefs descriptives
table(Study4_christians$condition)

Study4_christians_noneutral <- Study4_christians[which(Study4_christians$condition != "Neutral"),]
table(Study4_christians_noneutral$condition)
table(Study4_christians_noneutral$reward_punish_perspective)
Study4_christians_noneutral_godyes <- Study4_christians_noneutral[which(Study4_christians_noneutral$reward_punish_perspective == 1),]
table(Study4_christians_noneutral_godyes$reward_punish_perspective)

Study4_christians_noneutral_godyes$punishvars <- select(Study4_christians_noneutral_godyes, "punish_1", "punish_2", "punish_3")
alpha(Study4_christians_noneutral_godyes$punishvars) #alpha = .94
Study4_christians_noneutral_godyes$punish_composite <- rowMeans(Study4_christians_noneutral_godyes[, c("punish_1", "punish_2", "punish_3")])
describe(Study4_christians_noneutral_godyes$punish_composite)

Study4_christians_noneutral_godyes$punish_01 <- (Study4_christians_noneutral_godyes$punish_composite -1)/6
describe(Study4_christians_noneutral_godyes$punish_01)
t.test(Study4_christians_noneutral_godyes$punish_01)


Study4_christians_noneutral_godyes$rewardvars <- select(Study4_christians_noneutral_godyes, "reward_4", "reward_5", "reward_6")
alpha(Study4_christians_noneutral_godyes$rewardvars) #alpha = .97
Study4_christians_noneutral_godyes$reward_composite <- rowMeans(Study4_christians_noneutral_godyes[, c( "reward_4", "reward_5", "reward_6")])
describe(Study4_christians_noneutral_godyes$reward_composite)

Study4_christians_noneutral_godyes$reward_01 <- (Study4_christians_noneutral_godyes$reward_composite -1)/6
describe(Study4_christians_noneutral_godyes$reward_01)
t.test(Study4_christians_noneutral_godyes$reward_01)


#####Create big god items for christian data ignoring neutral (this is for merging purposes)

#first create small dataset continaing just ID and punish and reward scores
Study4_christians_noneutral_godyes_vars <- select(Study4_christians_noneutral_godyes, "punish_01", "reward_01", "id")

#Subset variables to make small dataset
Study4_small <- Study4_christians[c("id", "religion", "condition", "self_percentage", "god_percentage", "context", "gender_text", "recipient_location")]
length(unique(Study4_small$id))
Study4_small$threat_composite <- NA
Study4_small$commonality_composite <- NA
Study4_small$knows_01 <- NA
Study4_small$cares_01 <- NA
Study4_small <- merge(Study4_small, Study4_christians_noneutral_godyes_vars, by = "id", all.x = TRUE)

table(Study4_small$condition)

###Reshape data wide to long
Study4_long <- gather(Study4_small, perspective, percentage, self_percentage:god_percentage, factor_key = TRUE)

#Code perspective
Study4_long$perspective_contrastcode <- as.numeric(recode(Study4_long$perspective,"'self_percentage'=.5;'god_percentage'=-.5"))
Study4_long$self0 <- as.numeric(recode(Study4_long$perspective,"'self_percentage'=0;'god_percentage'=1"))
Study4_long$god0 <- as.numeric(recode(Study4_long$perspective,"'self_percentage'=1;'god_percentage'=0"))

#Code condition
Study4_long$condition_contrast1 <- recode(Study4_long$condition,"'Christian'=1;'Atheist'=-.5; 'Neutral'=-.5")
Study4_long$condition_contrast2 <- recode(Study4_long$condition,"'Christian'=0;'Atheist'=-.5; 'Neutral'=.5")
Study4_long$condition_c0_d1 <- recode(Study4_long$condition,"'Christian'=0;'Atheist'=0; 'Neutral'=1")
Study4_long$condition_c0_d2 <- recode(Study4_long$condition,"'Christian'=0;'Atheist'=1; 'Neutral'=0")
Study4_long$condition_a0_d1 <- recode(Study4_long$condition,"'Christian'=0;'Atheist'=0; 'Neutral'=1")
Study4_long$condition_a0_d2 <- recode(Study4_long$condition,"'Christian'=1;'Atheist'=0; 'Neutral'=0")
Study4_long$condition_n0_d1 <- recode(Study4_long$condition,"'Christian'=1;'Atheist'=0; 'Neutral'=0")
Study4_long$condition_n0_d2 <- recode(Study4_long$condition,"'Christian'=0;'Atheist'=1; 'Neutral'=0")

##Add Study Descriptor Variables
Study4_long$design <- "WC"
Study4_long$study <- "USA1"

###Select only participants in ingroup or outgroup condition, exclude neutral
Study4_small <- Study4_small[which(Study4_small$condition != "Neutral"),]
length(unique(Study4_small$id))
Study4_long<- Study4_long[which(Study4_long$condition != "Neutral"),]


##For descriptives
Study4_christians_noneutral <- Study4_christians[which(Study4_christians$condition != "Neutral"),]
describe(Study4_christians_noneutral$age)
table(Study4_christians_noneutral$gender_text)
table(Study4_christians_noneutral$ethnicity)

################STUDY 5: USA (with UAE Outgroup)
#########Load data
Study5 <- read_csv("Study5USA.csv")
colnames(Study5) <- tolower(colnames(Study5)) #convert all column names to lowercase
colnames(Study5)[colnames(Study5)=="responseid"] <- "id"
Study5$religion <- "Christian"
Study5$context <- "USA"
colnames(Study5)[colnames(Study5)=="dgpre"] <- "self_cond"
colnames(Study5)[colnames(Study5)=="dgpost"] <- "god_cond"
Study5$self_percentage <- Study5$self_cond/.4
Study5$god_percentage <- Study5$god_cond/.4
Study5$gender_text <-recode(Study5$gender,"1='Male';2='Female'; 3='Other'")
Study5$recipient_location <- recode(Study5$condition,"'UAE'='UAE';'American'='USA'")
length(unique(Study5$id))

#Descriptives
describe(Study5$age)
table(Study5$gender_text)
table(Study5$ethnicity)

#Reward and Punish

Study5$punishvars <- select(Study5, "punish_1", "punish_2", "punish_3")
alpha(Study5$punishvars) #alpha = .94
Study5$punish_composite <- rowMeans(Study5[, c("punish_1", "punish_2", "punish_3")])
describe(Study5$punish_composite)

Study5$punish_01 <- (Study5$punish_composite -1)/6
describe(Study5$punish_01)
t.test(Study5$punish_01)

Study5$rewardvars <- select(Study5, "reward_4", "reward_5", "reward_6")
alpha(Study5$rewardvars) #alpha = .95
Study5$reward_composite <- rowMeans(Study5[, c( "reward_4", "reward_5", "reward_6")])
describe(Study5$reward_composite)

Study5$reward_01 <- (Study5$reward_composite -1)/6
describe(Study5$reward_01)
t.test(Study5$reward_01)

#Subset variables to make small dataset
Study5_small <- Study5[c("id", "religion", "condition", "self_percentage", "god_percentage", "context", "gender_text", "recipient_location", "punish_01", "reward_01")]
Study5_small$threat_composite <- NA
Study5_small$commonality_composite <- NA
Study5_small$knows_01 <- NA
Study5_small$cares_01 <- NA


###Reshape data wide to long
Study5_long <- gather(Study5_small, perspective, percentage, self_percentage:god_percentage, factor_key = TRUE)

#Code Condition
Study5_long$condition <- recode(Study5_long$condition,"'UAE'='Muslim';'American'='Christian'")
Study5_long$condition_contrast <- as.numeric(recode(Study5_long$condition,"'Muslim'=-.5;'Christian'=.5"))
Study5_long$condition_c0 <- as.numeric(recode(Study5_long$condition,"'Muslim'=1;'Christian'=0"))
Study5_long$condition_m0 <- as.numeric(recode(Study5_long$condition,"'Muslim'=0;'Christian'=1"))

#Code perspective
Study5_long$perspective_contrastcode <- as.numeric(recode(Study5_long$perspective,"'self_percentage'=.5;'god_percentage'=-.5"))
Study5_long$self0 <- as.numeric(recode(Study5_long$perspective,"'self_percentage'=0;'god_percentage'=1"))
Study5_long$god0 <- as.numeric(recode(Study5_long$perspective,"'self_percentage'=1;'god_percentage'=0"))

##Add Study Descriptor Variables
Study5_long$design <- "WC"
Study5_long$study <- "USA/UAE"


##############STUDY 6: USA Evangelical
####Load Data
Study6 <- read_csv("Evangelical\ Study.csv")
Study6 <- read_csv("Study6USA.csv")

colnames(Study6) <- tolower(colnames(Study6)) #convert all column names to lowercase
colnames(Study6)[colnames(Study6)=="responseid"] <- "id"
Study6$religion <- "Christian"
Study6$context <- "USA"
colnames(Study6)[colnames(Study6)=="pre_dg"] <- "self_cond"
colnames(Study6)[colnames(Study6)=="post_dg"] <- "god_cond"
Study6$self_percentage <- Study6$self_cond/3
Study6$god_percentage <- Study6$god_cond/3
Study6$gender_text <-recode(Study6$gender,"1='Male';2='Female'; 3='Other'")
Study6$recipient_location <- "USA"
length(unique(Study6$id))

table(Study6$condition)
head(Study6)

Study6$threat_economic[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$econ_muslim
Study6$threat_security[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$secure_muslim
Study6$threat_symbolic[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$culture_muslim
Study6$threat_existential[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$kill_muslim
Study6$threat_economic[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$econ_atheist
Study6$threat_security[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$secure_atheist
Study6$threat_symbolic[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$culture_atheist
Study6$threat_existential[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$kill_atheist
Study6$commonality_samegod[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$pray_muslim
Study6$commonality_relvals[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$relig_muslim
Study6$commonality_groupvalues[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$value_muslims
Study6$commonality_groupvalues_ath[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$value_atheist
Study6$commonality_sameid[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$identity_muslims
Study6$commonality_sameid_ath[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$identity_atheist
Study6$conflict[Study6$condition == "Muslim East" | Study6$condition == "Muslim West"] <- Study6$relate_muslim
Study6$conflict[Study6$condition == "Atheist East" | Study6$condition == "Atheist West"] <- Study6$relate_atheist

#Punish reward god knows and cares
Study6$knows_01 <- (Study6$know - 1)/4
Study6$cares_01 <- (Study6$care - 1)/4
Study6$punish_01 <- (Study6$punish - 1)/4
Study6$reward_01 <- (Study6$reward - 1)/4

describe(Study6$knows_01)
describe(Study6$cares_01)
describe(Study6$punish_01)
describe(Study6$reward_01)

t.test(Study6$knows_01)
t.test(Study6$cares_01)
t.test(Study6$punish_01)
t.test(Study6$reward_01)


#Subset variables to make small dataset
Study6_small <- Study6[c("id", "religion", "condition", "self_percentage", "god_percentage", "context", "gender_text", "recipient_location", "threat_economic",
                               "threat_security", "threat_symbolic", "threat_existential", "commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid",
                               "conflict", "knows_01", "cares_01", "punish_01", "reward_01")]

threat_vars_US <- select(Study6_small, "threat_economic", "threat_security", "threat_symbolic", "threat_existential") #threat vector
alpha(threat_vars_US) #reliability for threat items

Study6_small$threat_composite <- rowMeans(Study6_small[, c("threat_economic", "threat_security", "threat_symbolic", "threat_existential")]) #make threat scale
describe(Study6_small$threat_composite) #compute mean threat

commonality_vars_US <- select(Study6_small, "commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid") #commonality vector
alpha(commonality_vars_US) #reliability for commonality items

Study6_small$commonality_composite <- rowMeans(Study6_small[, c("commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid")]) #make commonality scale
describe(Study6_small$commonality_composite) #compute mean commonality

describe.by(Study6_small$threat_composite, Study6_small$condition) #means by condition

###Correlation for commonality measures assessed for atheists
cor.test(Study6$commonality_sameid_ath, Study6$commonality_groupvalues_ath)
Study6$commonality_atheists <- rowMeans(Study6[, c("commonality_sameid_ath", "commonality_groupvalues_ath")])
describe(Study6$commonality_atheists)

###Describe sample
describe(Study6$age_num)
table(Study6$gender_text)
table(Study6$ethnicity)

###Reshape data wide to long
Study6_long <- gather(Study6_small, perspective, percentage, self_percentage:god_percentage, factor_key = TRUE)

#Code Condition & design, add descriptor for study
Study6_long$design <- recode(Study6_long$condition,"'Muslim East'='EC';'Christian East'='EC'; 'Muslim West'= 'WC'; 'Christian West' = 'WC'; 'Atheist East' = 'EC'; 'Atheist West' = 'WC'")
Study6_long$condition <- recode(Study6_long$condition,"'Muslim East'='Muslim';'Christian East'='Christian'; 'Muslim West'= 'Muslim'; 'Christian West' = 'Christian'; 'Atheist East'='Atheist'; 'Atheist West'='Atheist'")

Study6_long$condition_contrast1 <- recode(Study6_long$condition,"'Christian'=1;'Atheist'=-.5; 'Muslim'=-.5")
Study6_long$condition_contrast2 <- recode(Study6_long$condition,"'Christian'=0;'Atheist'=-.5; 'Muslim'=.5")
Study6_long$condition_c0_d1 <- recode(Study6_long$condition,"'Christian'=0;'Atheist'=0; 'Muslim'=1")
Study6_long$condition_c0_d2 <- recode(Study6_long$condition,"'Christian'=0;'Atheist'=1; 'Muslim'=0")
Study6_long$condition_a0_d1 <- recode(Study6_long$condition,"'Christian'=0;'Atheist'=0; 'Muslim'=1")
Study6_long$condition_a0_d2 <- recode(Study6_long$condition,"'Christian'=1;'Atheist'=0; 'Muslim'=0")
Study6_long$condition_m0_d1 <- recode(Study6_long$condition,"'Christian'=1;'Atheist'=0; 'Muslim'=0")
Study6_long$condition_m0_d2 <- recode(Study6_long$condition,"'Christian'=0;'Atheist'=1; 'Muslim'=0")
Study6_long$study <- "USA Evangelical"
Study6_long$design_contrast <- recode(Study6_long$design,"'WC'=-.5;'EC'=.5")
Study6_long$design_wc0 <- recode(Study6_long$design,"'WC'=0;'EC'=1")
Study6_long$design_ec0 <- recode(Study6_long$design,"'WC'=1;'EC'=0")

#Code perspective
Study6_long$perspective_contrastcode <- as.numeric(recode(Study6_long$perspective,"'self_percentage'=.5;'god_percentage'=-.5"))
Study6_long$self0 <- as.numeric(recode(Study6_long$perspective,"'self_percentage'=0;'god_percentage'=1"))
Study6_long$god0 <- as.numeric(recode(Study6_long$perspective,"'self_percentage'=1;'god_percentage'=0"))


###############################Creating small long-form data for USA data 

#####Create Small Versions of Long Datasets for Each Study
Study4_long_small <- Study4_long[c("id", "religion", "condition", "perspective", "percentage", "design", "context", "recipient_location", "study", "threat_composite", "commonality_composite", "knows_01", "cares_01", "punish_01", "reward_01")]
Study5_long_small <- Study5_long[c("id", "religion", "condition", "perspective", "percentage", "design", "context", "recipient_location", "study", "threat_composite", "commonality_composite", "knows_01", "cares_01", "punish_01", "reward_01")]
Study6_long_small <- Study6_long[c("id", "religion", "condition", "perspective", "percentage", "design", "context", "recipient_location", "study", "threat_composite", "commonality_composite", "knows_01", "cares_01", "punish_01", "reward_01")]

#Merge USA Data
USAData_long <- rbind (Study4_long_small, Study5_long_small, Study6_long_small)

#Code conditions for USA merged data
table(USAData_long$condition)
#USAData_long$condition_contrast1 <- recode(USAData_long$condition,"'Christian'=3;'Atheist'=-1; 'Muslim'=-1;'Neutral'=-1")
#USAData_long$condition_contrast2 <- recode(USAData_long$condition,"'Christian'=0;'Atheist'=-1; 'Muslim'=-1;'Neutral'=2")
#USAData_long$condition_contrast3 <- recode(USAData_long$condition,"'Christian'=0;'Atheist'=1; 'Muslim'=-1;'Neutral'=0")
USAData_long$condition_contrast1 <- recode(USAData_long$condition, "'Atheist' = -.5; 'Christian' = 1; 'Muslim' = -.5")
USAData_long$condition_contrast2 <- recode(USAData_long$condition, "'Atheist' = .5; 'Christian' = 0; 'Muslim' = -.5")
table(USAData_long$condition_contrast1)
#Code perspective
USAData_long$perspective_contrastcode <- as.numeric(recode(USAData_long$perspective,"'self_percentage'=.5;'god_percentage'=-.5"))
USAData_long$self0 <- as.numeric(recode(USAData_long$perspective,"'self_percentage'=0;'god_percentage'=1"))
USAData_long$god0 <- as.numeric(recode(USAData_long$perspective,"'self_percentage'=1;'god_percentage'=0"))

#Design Contrast
USAData_long$design_contrast <- recode(USAData_long$design,"'WC'=-.5;'EC'=.5")
USAData_long$design_wc0 <- recode(USAData_long$design,"'WC'=0;'EC'=1")
USAData_long$design_ec0 <- recode(USAData_long$design,"'WC'=1;'EC'=0")

#Study Contrast
USAData_long$study_contrast1 <- recode(USAData_long$study,"'USA1'=-1;'USA/UAE'=-1; 'USA Evangelical' = 2")
USAData_long$study_contrast2 <- recode(USAData_long$study,"'USA1'=-1;'USA/UAE'=1; 'USA Evangelical' = 0")

######BRINGING IN DATA FROM Fiji, West Bank, & Israel

###########################ISRAEL
##Load Data
Study7_raw1 <- read.csv("Study7Israel.csv")
colnames(Study7_raw1) <- tolower(colnames(Study7_raw1)) #convert all column names to lowercase
length(unique(Study7_raw1$i.user1))

length(unique(Study7_raw1$i.user1)) #pre exclusions
Study7_raw2 <- Study7_raw1[which((Study7_raw1$ipexclude_base !=1 | is.na(Study7_raw1$ipexclude_base))),] #exclude ip repeats
length(unique(Study7_raw2$i.user1)) #exclude 37 from ip
Study7_raw3 <- Study7_raw2[which(Study7_raw2$basesurveydata ==1),]
length(unique(Study7_raw3$i.user1))
Study7_raw4 <- Study7_raw3[which(Study7_raw3$rel_id >2),]
length(unique(Study7_raw4$i.user1)) #exclude 12 more from religious id

##Cleaner data #removing duplicate ids, non-religious, and incomplete
Study7 <- Study7_raw1[which((Study7_raw1$ipexclude_base !=1 | is.na(Study7_raw1$ipexclude_base)) & (Study7_raw1$basesurveydata == 1) & (Study7_raw1$rel_id >2)),]
length(unique(Study7$i.user1))


##Set up data for consistency
Study7$id <- sub("^", "isr", Study7$i.user1 )
Study7$religion <- "Jewish"
Study7$context <- "Middle East"
Study7$self_percentage <- Study7$controlother/12
Study7$god_percentage <- Study7$godother/12
Study7$gender_text <-recode(Study7$sex,"1='Male';2='Female'")
Study7$condition<- recode(Study7$condition,"1='Jewish';2='Muslim'")
Study7$recipient_location<- recode(Study7$condition,"'Jewish'='Israel';'Muslim'='West Bank'")
Study7$design <- "EC"
Study7$study<- "Israel"

#Descriptives
describe(Study7$age)
table(Study7$gender_text)
table(Study7$ethnicity)

#threat/commonality
head(Study7)
colnames(Study7)[colnames(Study7)=="pal_economic_threat"] <- "threat_economic"
colnames(Study7)[colnames(Study7)=="pal_security_threat"] <- "threat_security"
colnames(Study7)[colnames(Study7)=="pal_symbolic_threat"] <- "threat_symbolic"
colnames(Study7)[colnames(Study7)=="pal_existential_thre"] <- "threat_existential"
colnames(Study7)[colnames(Study7)=="rel_com_1"] <- "commonality_samegod"
colnames(Study7)[colnames(Study7)=="rel_com_2"] <- "commonality_relvals"
colnames(Study7)[colnames(Study7)=="com_val_pal"] <- "commonality_groupvalues"
colnames(Study7)[colnames(Study7)=="com_id_pal"] <- "commonality_sameid"
colnames(Study7)[colnames(Study7)=="pal_conflict"] <- "conflict"

threat_vars_Israel <- select(Study7, "threat_economic", "threat_security", "threat_symbolic", "threat_existential")
alpha(threat_vars_Israel)

Study7$threat_composite <- rowMeans(Study7[, c("threat_economic", "threat_security", "threat_symbolic", "threat_existential")], na.rm=TRUE)
describe(Study7$threat_composite)

commonality_vars_Israel <- select(Study7, "commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid")
alpha(commonality_vars_Israel)

Study7$commonality_composite <- rowMeans(Study7[, c("commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid")])
describe(Study7$commonality_composite)

#Punish reward god knows and cares
Study7$knows_01 <- (Study7$know - 1)/4
Study7$cares_01 <- (Study7$care - 1)/4
Study7$punish_01 <- (Study7$punish - 1)/4
Study7$reward_01 <- (Study7$reward - 1)/4

describe(Study7$knows_01)
describe(Study7$cares_01)
describe(Study7$punish_01)
describe(Study7$reward_01)
t.test(Study7$knows_01)
t.test(Study7$cares_01)
t.test(Study7$punish_01)
t.test(Study7$reward_01)

#Small dataset
Study7_small <- Study7[c("id", "religion", "condition", "self_percentage", "god_percentage", "design", "context", "recipient_location", "study", "threat_composite", "commonality_composite", "knows_01", "cares_01", "punish_01", "reward_01")]

#######################WEST BANK
####Load Data
Study8 <- read.csv("Study8WestBank.csv")
colnames(Study8) <- tolower(colnames(Study8)) #convert all column names to lowercase
Study8$egmissing <- NA
Study8$egmissing[is.na(Study8$self_100)] <-1
describe(Study8$egmissing)
Study8<- Study8[which(is.na(Study8$egmissing)),]

Study8$id <- sub("^", "wb", Study8$participant_id )
Study8$religion <-"Muslim"
Study8$context <- "Middle East"
Study8$self_percentage <- Study8$self_100/100
Study8$god_percentage <- Study8$god_100/100
Study8$gender_text <-recode(Study8$sex,"1='Male';2='Female'")
Study8$condition<- recode(Study8$recpient,"'Palestinian'='Muslim';'Israeli'='Jewish'")
Study8$recipient_location<- recode(Study8$condition,"'Muslim'='West Bank';'Jewish'='Israel'")
Study8$design <- "EC"
Study8$study <- "West Bank"
length(unique(Study8$id))
table(Study8$recpient, Study8$condition)

#Descriptives
describe(as.numeric(Study8$age))
table(Study8$gender_text)

head(Study8)
colnames(Study8)[colnames(Study8)=="isr_econ_threat"] <- "threat_economic"
colnames(Study8)[colnames(Study8)=="isr_security_threat"] <- "threat_security"
colnames(Study8)[colnames(Study8)=="isr_symbolic_threat"] <- "threat_symbolic"
colnames(Study8)[colnames(Study8)=="isr_exis_threat"] <- "threat_existential"
colnames(Study8)[colnames(Study8)=="rel_com_1"] <- "commonality_samegod"
colnames(Study8)[colnames(Study8)=="rel_com_2"] <- "commonality_relvals"
colnames(Study8)[colnames(Study8)=="com_val_isr"] <- "commonality_groupvalues"
colnames(Study8)[colnames(Study8)=="com_id_isr"] <- "commonality_sameid"
colnames(Study8)[colnames(Study8)=="isr_conflict"] <- "conflict"

threat_vars_WB <- select(Study8, "threat_economic", "threat_security", "threat_symbolic", "threat_existential")
alpha(threat_vars_WB)

Study8$threat_composite <- rowMeans(Study8[, c("threat_economic", "threat_security", "threat_symbolic", "threat_existential")], na.rm=TRUE)
describe(Study8$threat_composite)

commonality_vars_WB <- select(Study8, "commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid")
alpha(commonality_vars_WB)

Study8$commonality_composite <- rowMeans(Study8[, c("commonality_samegod", "commonality_relvals", "commonality_groupvalues", "commonality_sameid")])
describe(Study8$commonality_composite)

#Punish reward god knows and cares
Study8$knows_01 <- (Study8$know - 1)/4
Study8$cares_01 <- (Study8$care - 1)/4
Study8$punish_01 <- (Study8$punish - 1)/4
Study8$reward_01 <- (Study8$reward - 1)/4

describe(Study8$knows_01)
describe(Study8$cares_01)
describe(Study8$punish_01)
describe(Study8$reward_01)
t.test(Study8$knows_01)
t.test(Study8$cares_01)
t.test(Study8$punish_01)
t.test(Study8$reward_01)

#Small dataset
Study8_small <- Study8[c("id", "religion", "condition", "self_percentage", "god_percentage", "design", "context", "recipient_location", "study", "threat_composite", "commonality_composite", "knows_01", "cares_01", "punish_01", "reward_01")]


####Merge non-USA Wide Small Data files
EC_wide_merged <- rbind(EC_Fiji_small, Study7_small, Study8_small)

#Make EC Merged Data Long
EC_long <- gather(EC_wide_merged, perspective, percentage, self_percentage:god_percentage, factor_key = TRUE)

######Merge ALL LONG FILES TOGETHER
##First make small WC merged file
USAData_long_small <- USAData_long[c("id", "religion", "condition", "perspective", "percentage", "design", "context", "recipient_location", "study", "threat_composite", "commonality_composite", "knows_01", "cares_01", "punish_01", "reward_01")]

#then Merge Data
Merged_long <- rbind(USAData_long_small, EC_long)

#Make Outcome Easier to interperet by *100
Merged_long$percentage100 <- Merged_long$percentage*100

#Code Perspective
Merged_long$perspective_contrastcode <- as.numeric(recode(Merged_long$perspective,"'self_percentage'=.5;'god_percentage'=-.5"))
Merged_long$self0[Merged_long$perspective == "self_percentage"] <- 0
Merged_long$self0[Merged_long$perspective == "god_percentage"] <- 1
Merged_long$god0[Merged_long$perspective == "self_percentage"] <- 1
Merged_long$god0[Merged_long$perspective == "god_percentage"] <- 0

table(Merged_long$self0)

########Code Ingroup and Outgroup and Related Variables
Merged_long$ingrp_v_outgroup[Merged_long$religion==Merged_long$condition]<-"Ingroup"
Merged_long$ingrp_v_outgroup[Merged_long$religion!=Merged_long$condition]<-"Outgroup"
Merged_long$ingrp_v_outgroup_contrast <- recode(Merged_long$ingrp_v_outgroup,"'Ingroup'=.5;'Outgroup'=-.5")
Merged_long$ingrp0 <- recode(Merged_long$ingrp_v_outgroup,"'Ingroup'=0;'Outgroup'=1")
Merged_long$outgrp0 <- recode(Merged_long$ingrp_v_outgroup,"'Ingroup'=1;'Outgroup'=0")

table(Merged_long$ingrp_v_outgroup, Merged_long$study)
table(Merged_long$ingrp_v_outgroup, Merged_long$study)/2 #total sample per ingroup and outgroup condition

####Create Design Contrast (comparing primes)
Merged_long$design_contrast <-  recode(Merged_long$design,"'EC'=.5;'WC'=-.5")
Merged_long$wc0 <-  recode(Merged_long$design,"'EC'=1;'WC'=0")
Merged_long$ec0 <-  recode(Merged_long$design,"'EC'=0;'WC'=1")

####Code context
Merged_long$context_contrast1 <-  recode(Merged_long$context,"'USA'=-.5;'Fiji'=-.5; 'Middle East' = 1")
Merged_long$context_contrast2 <-  recode(Merged_long$context,"'USA'=.5;'Fiji'=-.5; 'Middle East' = 0")
Merged_long$context_USA1 <-  recode(Merged_long$context,"'USA'=0;'Fiji'=0; 'Middle East' =1")
Merged_long$context_USA2 <-  recode(Merged_long$context,"'USA'=0;'Fiji'=1; 'Middle East' =0")
Merged_long$context_Fiji1 <-  recode(Merged_long$context,"'USA'=0;'Fiji'=0; 'Middle East' =1")
Merged_long$context_Fiji2 <-  recode(Merged_long$context,"'USA'=1;'Fiji'=0; 'Middle East' =0")
Merged_long$context_ME1 <-  recode(Merged_long$context,"'USA'=0;'Fiji'=1; 'Middle East' =0")
Merged_long$context_ME2 <-  recode(Merged_long$context,"'USA'=1;'Fiji'=0; 'Middle East' =0")

###Code Religion Proselytizing
Merged_long$rel_pros_contrast <- recode(Merged_long$religion, "'Jewish'=-.5; 'Hindu' = -.5; 'Muslim' = 0.5; 'Christian' = 0.5")
Merged_long$rel_pros0 <- recode(Merged_long$religion, "'Jewish'=1; 'Hindu' = 1; 'Muslim' = 0; 'Christian' = 0")
Merged_long$rel_notpros0 <- recode(Merged_long$religion, "'Jewish'=0; 'Hindu' = 0; 'Muslim' = 1; 'Christian' = 1")

#Merged_long$rel_pros_contrast <- recode(Merged_long$religion, "'Jewish'=-.5; 'Hindu' = -.5; 'Muslim' = 0.5; 'Christian' = 0.5; 'Atheist' = -.5")
#Merged_long$rel_pros0 <- recode(Merged_long$religion, "'Jewish'=1; 'Hindu' = 1; 'Muslim' = 0; 'Christian' = 0")
#Merged_long$rel_notpros0 <- recode(Merged_long$religion, "'Jewish'=0; 'Hindu' = 0; 'Muslim' = 1; 'Christian' = 1")

##Code outgroup religion as atheist or religious
Merged_long$outgroup_religious_contrast <- recode(Merged_long$condition, "'Atheist' = -0.5; 'Christian' = 0.5; 'Hindu' = 0.5; 'Jewish' = 0.5; 'Muslim' = 0.5")
Merged_long$outgroup_atheist0 <- recode(Merged_long$condition, "'Atheist' = 0; 'Christian' = 1; 'Hindu' = 1; 'Jewish' = 1; 'Muslim' = 1")
Merged_long$outgroup_religious0 <- recode(Merged_long$condition, "'Atheist' = 1; 'Christian' = 0; 'Hindu' = 0; 'Jewish' = 0; 'Muslim' = 0")

####Export Data
#write.csv(Merged_long, "Merged_long.csv") #saves clean long format data

###########Create Orthogonal Constrasts for Study & Context

##########Contrast 1: Middle East vs USA & Fiji
Merged_long$C1_ME_vs_FJUSA <- recode(Merged_long$study, "'Fiji_Hindu' = -1; 'Fiji_Christian' = -1; 'Fiji_Muslim' = -1; 'USA1' = -1;
                         'USA/UAE' = -1; 'USA Evangelical' = -1; 'Israel' = 3; 'West Bank' = 3")

##########Contrast 2: USA vs Fiji
Merged_long$C2_USA_vs_FJ <- recode(Merged_long$study, "'Fiji_Hindu' = -1; 'Fiji_Christian' = -1; 'Fiji_Muslim' = -1; 'USA1' = 1;
                         'USA/UAE' = 1; 'USA Evangelical' = 1; 'Israel' = 0; 'West Bank' = 0")

##########Contrast 3: Fiji Hindus vs Fiji Christians and Muslims
Merged_long$C3_FJH_vs_FJMFMC <- recode(Merged_long$study, "'Fiji_Hindu' = 2; 'Fiji_Christian' = -1; 'Fiji_Muslim' = -1; 'USA1' = 0;
                         'USA/UAE' = 0; 'USA Evangelical' = 0; 'Israel' = 0; 'West Bank' = 0")

##########Contrast 4: Fiji Muslims vs Fiji Christians
Merged_long$C4_FJM_vs_FJC <- recode(Merged_long$study, "'Fiji_Hindu' = 0; 'Fiji_Christian' = -1; 'Fiji_Muslim' = 1; 'USA1' = 0;
                         'USA/UAE' = 0; 'USA Evangelical' = 0; 'Israel' = 0; 'West Bank' = 0")

##########Contrast 5: USA Evangelicals vs USA 1 & USA 2
Merged_long$C5_USAE_vs_USA1_2 <- recode(Merged_long$study, "'Fiji_Hindu' = 0; 'Fiji_Christian' = 0; 'Fiji_Muslim' = 0; 'USA1' = -1;
                         'USA/UAE' = -1; 'USA Evangelical' = 2; 'Israel' = 0; 'West Bank' = 0")

##########Contrast 6: USA 1 vs USA 2 (USA/UAE)
Merged_long$C6_USA1_vs_USA2 <- recode(Merged_long$study, "'Fiji_Hindu' = 0; 'Fiji_Christian' = 0; 'Fiji_Muslim' = 0; 'USA1' = 1;
                         'USA/UAE' = -1; 'USA Evangelical' = 0; 'Israel' = 0; 'West Bank' = 0")

##########Contrast 7: West Bank vs Israel
Merged_long$C7_WB_vs_Isr <- recode(Merged_long$study, "'Fiji_Hindu' = 0; 'Fiji_Christian' = 0; 'Fiji_Muslim' = 0; 'USA1' = 0;
                         'USA/UAE' = 0; 'USA Evangelical' = 0; 'Israel' = -1; 'West Bank' = 1")


##################################
#######################################Integrated Data Analyses
##################################


######RESEARCH QUESTION 1: WHAT IS OVERAL EFFECT OF PERSPECTIVE, AND PERSPECTIVE X CONDITION INTERACTION, CONTROLLING FOR STUDY (at random level)?

Q1 <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast + (1 |study/id), data = Merged_long)
summary(Q1)
confint(Q1, method = "Wald")

Q1_random <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast + (1 |study/id) + (0 + self0|id), data = Merged_long)
summary(Q1_random)
confint(Q1_random, method = "Wald") #Adding random slopes by subject, but not by study

anova(Q1, Q1_random) #Suggests adding random slopes improves fit

Q1_table <- tab_model(Q1_random, show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                      pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Intergroup Condition (Ingroup = 0.5, Outgroup = -0.5)",
                                      "Perspective x Intergroup Condition"), dv.labels = "Percentage of Total Stakes Given",
                      title = "Results for Research Question 1", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;' ),
                      col.order = c("est", "stat", "ci"), file = "Q1table.html")
Q1_table #save table

Q1_ingroup <- lmer(percentage100 ~ self0*ingrp0 + (1 |study/id) + (0 + self0 | id), data = Merged_long)
summary(Q1_ingroup)
confint(Q1_ingroup, method = "Wald")

Q1_outgroup <- lmer(percentage100 ~ self0*outgrp0 + (1 |study/id) + (0 + self0 | id), data = Merged_long)
summary(Q1_outgroup)
confint(Q1_outgroup, method = "Wald")

Q1_god <- lmer(percentage100 ~ god0*ingrp_v_outgroup_contrast + (1 |study/id) + (0 + self0 | id), data = Merged_long)
summary(Q1_god)


######Bayes Factor for Interaction

Merged_long_nona <- Merged_long[which(!is.na(Merged_long$percentage100)),] #requires no missing data on DV

full_BF = lmBF(percentage100 ~ self0 + ingrp_v_outgroup_contrast + self0:ingrp_v_outgroup_contrast, data = Merged_long_nona, whichRandom = c("study", "id", "self0:id"))
null_BF = lmBF(percentage100 ~ self0 + ingrp_v_outgroup_contrast, data = Merged_long_nona, whichRandom = c("study", "id", "self0:id"))
full_BF / null_BF

######RESEARCH QUESTION 2: ARE EFFECTS MODERATED BY CONTEXT?

#Q2 <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*C1_ME_vs_FJUSA + self0*ingrp_v_outgroup_contrast*C2_USA_vs_FJ + self0*ingrp_v_outgroup_contrast*C3_FJH_vs_FJMFMC + self0*ingrp_v_outgroup_contrast*C4_FJM_vs_FJC +
  #               self0*ingrp_v_outgroup_contrast*C5_USAE_vs_USA1_2 + self0*ingrp_v_outgroup_contrast*C6_USA1_vs_USA2 + self0*ingrp_v_outgroup_contrast*C7_WB_vs_Isr + (1 |id), data = Merged_long)
#summary(Q2)
#confint(Q2, method = "Wald")

Q2_random <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*C1_ME_vs_FJUSA + self0*ingrp_v_outgroup_contrast*C2_USA_vs_FJ + self0*ingrp_v_outgroup_contrast*C3_FJH_vs_FJMFMC + self0*ingrp_v_outgroup_contrast*C4_FJM_vs_FJC +
             self0*ingrp_v_outgroup_contrast*C5_USAE_vs_USA1_2 + self0*ingrp_v_outgroup_contrast*C6_USA1_vs_USA2 + self0*ingrp_v_outgroup_contrast*C7_WB_vs_Isr + (1 |id) + (0 + self0|id), data = Merged_long)
summary(Q2_random) #random slope for self included, no covariance between slopes and intercepts.
confint(Q2_random, method = "Wald")

#anova(Q2, Q2_random)

Q2_table <- tab_model(Q2_random, show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                      pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Intergroup Condition (Ingroup = 0.5, Outgroup = -0.5)",
                                      "C1: Middle East = 3, US & Fiji = -1", "C2: USA = 1, Fiji = -1", "C3: Fiji Hindus = 2, Fiji Muslims & Christians = -1, Others = 0",
                                      "C4: Fiji Muslims = 1, Fiji Christians = -1, Others = 0", "C5: USA Evangelicals = 2, USA Studies 1 & 2 = -1, Others = 0",
                                      "C6: USA Study 1 = 1, USA Study 2 = -1, Others = 0", "C7: West Bank = 1, Israel = -1, Others = 0",
                                      "Perspective x Intergroup Condition",
                                      "Perspective x C1", "Intergroup Condition x C1",
                                      "Perspective x C2", "Intergroup Condition x C2",
                                      "Perspective x C3", "Intergroup Condition x C3",
                                      "Perspective x C4", "Intergroup Condition x C4",
                                      "Perspective x C5", "Intergroup Condition x C5",
                                      "Perspective x C6", "Intergroup Condition x C6",
                                      "Perspective x C7", "Intergroup Condition x C7",
                                      "Perspective x Intergroup Condition x C1",
                                      "Perspective x Intergroup Condition x C2",
                                      "Perspective x Intergroup Condition x C3",
                                      "Perspective x Intergroup Condition x C4",
                                      "Perspective x Intergroup Condition x C5",
                                      "Perspective x Intergroup Condition x C6",
                                      "Perspective x Intergroup Condition x C7"),
                      dv.labels = "Percentage of Total Stakes Given",
                      title = "Results for Research Question 2", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;' ),
                      col.order = c("est", "stat", "ci"), file = "Q2table.html")
Q2_table #save table


#Bayes Factor for Context Moderation

full_BF = lmBF(percentage100 ~ self0 + ingrp_v_outgroup_contrast + self0:ingrp_v_outgroup_contrast + C1_ME_vs_FJUSA + C2_USA_vs_FJ + C3_FJH_vs_FJMFMC + C4_FJM_vs_FJC +
                 C5_USAE_vs_USA1_2 + C6_USA1_vs_USA2 + C7_WB_vs_Isr + self0:C1_ME_vs_FJUSA + self0:C2_USA_vs_FJ + self0:C3_FJH_vs_FJMFMC + self0:C4_FJM_vs_FJC +
                 self0:C5_USAE_vs_USA1_2 + self0:C6_USA1_vs_USA2 + self0:C7_WB_vs_Isr + ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA + ingrp_v_outgroup_contrast:C2_USA_vs_FJ +
                 ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + ingrp_v_outgroup_contrast:C4_FJM_vs_FJC + ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 +
                 ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + ingrp_v_outgroup_contrast:C7_WB_vs_Isr + self0:ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA +
                 self0:ingrp_v_outgroup_contrast:C2_USA_vs_FJ + self0:ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + self0:ingrp_v_outgroup_contrast:C4_FJM_vs_FJC +
                 self0:ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 + self0:ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + self0:ingrp_v_outgroup_contrast:C7_WB_vs_Isr,
               data = Merged_long_nona, whichRandom = c("id", "self0:id"))

C1Int_BF = lmBF(percentage100 ~ self0 + ingrp_v_outgroup_contrast + self0:ingrp_v_outgroup_contrast + C1_ME_vs_FJUSA + C2_USA_vs_FJ + C3_FJH_vs_FJMFMC + C4_FJM_vs_FJC +
                  C5_USAE_vs_USA1_2 + C6_USA1_vs_USA2 + C7_WB_vs_Isr + self0:C1_ME_vs_FJUSA + self0:C2_USA_vs_FJ + self0:C3_FJH_vs_FJMFMC + self0:C4_FJM_vs_FJC +
                  self0:C5_USAE_vs_USA1_2 + self0:C6_USA1_vs_USA2 + self0:C7_WB_vs_Isr + ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA + ingrp_v_outgroup_contrast:C2_USA_vs_FJ +
                  ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + ingrp_v_outgroup_contrast:C4_FJM_vs_FJC + ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 +
                  ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + ingrp_v_outgroup_contrast:C7_WB_vs_Isr +
                  self0:ingrp_v_outgroup_contrast:C2_USA_vs_FJ + self0:ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + self0:ingrp_v_outgroup_contrast:C4_FJM_vs_FJC +
                  self0:ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 + self0:ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + self0:ingrp_v_outgroup_contrast:C7_WB_vs_Isr,
                data = Merged_long_nona, whichRandom = c("id", "self0:id"))

C2Int_BF = lmBF(percentage100 ~ self0 + ingrp_v_outgroup_contrast + self0:ingrp_v_outgroup_contrast + C1_ME_vs_FJUSA + C2_USA_vs_FJ + C3_FJH_vs_FJMFMC + C4_FJM_vs_FJC +
                  C5_USAE_vs_USA1_2 + C6_USA1_vs_USA2 + C7_WB_vs_Isr + self0:C1_ME_vs_FJUSA + self0:C2_USA_vs_FJ + self0:C3_FJH_vs_FJMFMC + self0:C4_FJM_vs_FJC +
                  self0:C5_USAE_vs_USA1_2 + self0:C6_USA1_vs_USA2 + self0:C7_WB_vs_Isr + ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA + ingrp_v_outgroup_contrast:C2_USA_vs_FJ +
                  ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + ingrp_v_outgroup_contrast:C4_FJM_vs_FJC + ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 +
                  ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + ingrp_v_outgroup_contrast:C7_WB_vs_Isr + self0:ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA +
                  self0:ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + self0:ingrp_v_outgroup_contrast:C4_FJM_vs_FJC +
                  self0:ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 + self0:ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + self0:ingrp_v_outgroup_contrast:C7_WB_vs_Isr,
                data = Merged_long_nona, whichRandom = c("id", "self0:id"))

C6Int_BF = lmBF(percentage100 ~ self0 + ingrp_v_outgroup_contrast + self0:ingrp_v_outgroup_contrast + C1_ME_vs_FJUSA + C2_USA_vs_FJ + C3_FJH_vs_FJMFMC + C4_FJM_vs_FJC +
                  C5_USAE_vs_USA1_2 + C6_USA1_vs_USA2 + C7_WB_vs_Isr + self0:C1_ME_vs_FJUSA + self0:C2_USA_vs_FJ + self0:C3_FJH_vs_FJMFMC + self0:C4_FJM_vs_FJC +
                  self0:C5_USAE_vs_USA1_2 + self0:C6_USA1_vs_USA2 + self0:C7_WB_vs_Isr + ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA + ingrp_v_outgroup_contrast:C2_USA_vs_FJ +
                  ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + ingrp_v_outgroup_contrast:C4_FJM_vs_FJC + ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 +
                  ingrp_v_outgroup_contrast:C6_USA1_vs_USA2 + ingrp_v_outgroup_contrast:C7_WB_vs_Isr + self0:ingrp_v_outgroup_contrast:C1_ME_vs_FJUSA +
                  self0:ingrp_v_outgroup_contrast:C2_USA_vs_FJ + self0:ingrp_v_outgroup_contrast:C3_FJH_vs_FJMFMC + self0:ingrp_v_outgroup_contrast:C4_FJM_vs_FJC +
                  self0:ingrp_v_outgroup_contrast:C5_USAE_vs_USA1_2 + self0:ingrp_v_outgroup_contrast:C7_WB_vs_Isr,
                data = Merged_long_nona, whichRandom = c("id", "self0:id"))

full_BF/C1Int_BF #Calculates BF for full model vs model without C1*self0*ingrp_v_outgroup_contrast

full_BF/C2Int_BF #Calculates BF for full model vs model without C2*self0*ingrp_v_outgroup_contrast

full_BF/C6Int_BF #Calculates BF for full model vs model without C3*self0*ingrp_v_outgroup_contrast


######RESEARCH QUESTION 3: ARE EFFECTS MODERATED BY PERCEIVED THREAT (STUDY FIXED LEVEL)?

#Subset samples with same threat measures
threat_data <- Merged_long[which((Merged_long$study == "USA Evangelical" | Merged_long$study == "Israel" | Merged_long$study == "West Bank") & Merged_long$ingrp_v_outgroup == "Outgroup"),]

describe(threat_data$threat_composite)
threat_data_wide <- threat_data[!duplicated(threat_data$id),]
describe(threat_data_wide$threat_composite)
threat_data$threat_composite.c <- threat_data$threat_composite - 2.67
threat_data$threat_composite.h <- threat_data$threat_composite.c - 1.34
threat_data$threat_composite.l <- threat_data$threat_composite.c + 1.34
describeBy(threat_data_wide$threat_composite, threat_data_wide$study)
table(threat_data_wide$study, threat_data_wide$condition)
Study6_small2 <- Study6_small

Study6_small2$outgroup[Study6_small2$condition == "Muslim East" | Study6_small2$condition == "Muslim West"] <- "Muslim"
Study6_small2$outgroup[Study6_small2$condition == "Atheist East" | Study6_small2$condition == "Atheist West"] <- "Atheist"
describeBy(Study6_small2$threat_composite, Study6_small2$outgroup)

describeBy(Study6_small$threat_composite, Study6_small$condition)


threat_data$study_contrast1[threat_data$study == "USA Evangelical"] <- -1
threat_data$study_contrast1[threat_data$study == "Israel"] <- .5
threat_data$study_contrast1[threat_data$study == "West Bank"] <- .5
threat_data$study_contrast2[threat_data$study == "USA Evangelical"] <- 0
threat_data$study_contrast2[threat_data$study == "Israel"] <- -.5
threat_data$study_contrast2[threat_data$study == "West Bank"] <- .5

Q3 <- lmer(percentage100 ~ self0*threat_composite.c*study_contrast1 + self0*threat_composite.c*study_contrast2 + (1|id), data = threat_data)
summary(Q3)
confint(Q3, method="Wald")

Q3_random <- lmer(percentage100 ~ self0*threat_composite.c*study_contrast1 + self0*threat_composite.c*study_contrast2 + (1 |id) + (0 + self0|id), data = threat_data)
summary(Q3_random)
confint(Q3_random, method="Wald") #random slopes for id

wide_threat <-  threat_data[!duplicated(threat_data$id),]
threat_compare <- lm(wide_threat$threat_composite.c ~ wide_threat$study_contrast1 + wide_threat$study_contrast2)
summary(threat_compare)
describeBy(wide_threat$threat_composite, wide_threat$condition)
describeBy(wide_threat$threat_composite, wide_threat$study)
describeBy(Study6_small$threat_composite, Study6_small$condition)

Q3_table <- tab_model(c(Q3_random), show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                      pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Threat (Centered)", "Study Contrast 1: USA Evangelicals = 1, Middle East = -0.5", "Study Contrast 2: Israel = -0.5, West Bank = 0.5", "Perspective x Threat",
                                      "Perspective x Study Contrast 1", "Threat x Study Contrast 1", "Perspective x Study Contrast 2", "Threat x Study Contrast 2", "Perspective x Threat x Study Contrast 1",
                                      "Perspective x Threat x Study Contrast 2"),
                      dv.labels = c("Percentage of Total Stakes Given"),                
                      title = "Table 6. Results for threat moderation model", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;'),
                      col.order = c("est", "stat", "ci"), file = "q3_table.html")
Q3_table

#####Bayes for Threat

threat_data_nona <- threat_data[which(!is.na(threat_data$percentage100) & !is.na(threat_data$threat_composite.c)),] #requires no missing data on DV

threat_full_BF = lmBF(percentage100 ~ self0 + study_contrast1 + study_contrast2 + self0:study_contrast1 + self0:study_contrast1 +
                 threat_composite.c + self0:threat_composite.c + study_contrast1:threat_composite.c +
                   study_contrast2:threat_composite.c + self0:study_contrast1:threat_composite.h +
                   self0:study_contrast2:threat_composite.c, data = threat_data_nona, whichRandom = c("id", "self0:id"))
Threat_null_BF = lmBF(percentage100 ~ self0 + study_contrast1 + study_contrast2 + self0:study_contrast1 + self0:study_contrast1 +
                        threat_composite.c + study_contrast1:threat_composite.c +
                        study_contrast2:threat_composite.c, data = threat_data_nona, whichRandom = c("id", "self0:id"))
threat_full_BF / Threat_null_BF

###To do with random slopes
threat_full_BF = lmBF(percentage100 ~ self0 + study_contrast1 + study_contrast2 + self0:study_contrast1 + self0:study_contrast1 +
                        threat_composite.c + self0:threat_composite.c + study_contrast1:threat_composite.c +
                        study_contrast2:threat_composite.c + self0:study_contrast1:threat_composite.h +
                        self0:study_contrast2:threat_composite.c, data = threat_data_nona, whichRandom = c('id', 'self0:id'))
Threat_null_BF = lmBF(percentage100 ~ self0 + study_contrast1 + study_contrast2 + self0:study_contrast1 + self0:study_contrast1 +
                        threat_composite.c + study_contrast1:threat_composite.c +
                        study_contrast2:threat_composite.c, data = threat_data_nona, whichRandom = c('id', 'self0:id'))


######RESEARCH QUESTION 4: ARE EFFECTS MODERATED BY PERCEIVED COMMONALITY (STUDY FIXED LEVEL)?

commonality_data <- Merged_long[which((Merged_long$study == "USA Evangelical" | Merged_long$study == "Israel" | Merged_long$study == "West Bank") & Merged_long$ingrp_v_outgroup == "Outgroup"),]
commonality_data <- commonality_data[which(commonality_data$condition == "Jewish" | commonality_data$condition == "Muslim"),]
table(commonality_data$condition)

describe(commonality_data$commonality_composite)
commonality_data_wide <- commonality_data[!duplicated(commonality_data$id),]
describe(commonality_data_wide$commonality_composite)

commonality_data$commonality_composite.c <- commonality_data$commonality_composite - 2.18
commonality_data$threat_composite.h <- commonality_data$commonality_composite.c - 1.03
commonality_data$threat_composite.l <- commonality_data$commonality_composite.c + 1.03

describeBy(commonality_data$commonality_composite, commonality_data$religion)

commonality_data$study_contrast1[commonality_data$study == "USA Evangelical"] <- -1
commonality_data$study_contrast1[commonality_data$study == "Israel"] <- .5
commonality_data$study_contrast1[commonality_data$study == "West Bank"] <- .5
commonality_data$study_contrast2[commonality_data$study == "USA Evangelical"] <- 0
commonality_data$study_contrast2[commonality_data$study == "Israel"] <- -.5
commonality_data$study_contrast2[commonality_data$study == "West Bank"] <- .5


Q4 <- lmer(percentage100 ~ self0*commonality_composite.c*study_contrast1 + self0*commonality_composite.c*study_contrast2 + (1|id), data = commonality_data)
summary(Q4)
confint(Q4, method = "Wald")

Q4_random <- lmer(percentage100 ~ self0*commonality_composite.c*study_contrast1 + self0*commonality_composite.c*study_contrast2 + (1|id) + (0+self0|id), data = commonality_data)
summary(Q4_random)
confint(Q4_random, method = "Wald")

wide_commonality <-  commonality_data[!duplicated(commonality_data$id),]
comonality_compare <- lm(wide_commonality$commonality_composite ~ wide_commonality$study_contrast1 + wide_commonality$study_contrast2)
summary(comonality_compare)
describeBy(wide_commonality$commonality_composite, wide_commonality$study)

Q4_table <- tab_model(c(Q4_random), show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                      pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Commonality (Centered)", "Study Contrast 1: USA Evangelicals = 1, Middle East = -0.5", "Study Contrast 2: Israel = -0.5, West Bank = 0.5", "Perspective x Commonality",
                                      "Perspective x Study Contrast 1", "Commonality x Study Contrast 1", "Perspective x Study Contrast 2", "Commonality x Study Contrast 2", "Perspective x Commonality x Study Contrast 1",
                                      "Perspective x Commonality x Study Contrast 2"),
                      dv.labels = c("Percentage of Total Stakes Given"),                
                      title = "Table 5. Results for commonality moderation model", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;'),
                      col.order = c("est", "stat", "ci"), file = "q4_table.html")
Q4_table


commonality_data_nona <- commonality_data[which(!is.na(commonality_data$percentage100) & !is.na(commonality_data$commonality_composite.c)),] #requires no missing data on DV

commonality_full_BF = lmBF(percentage100 ~ self0 + study_contrast1 + study_contrast2 + self0:study_contrast1 + self0:study_contrast1 +
                             commonality_composite.c + self0:commonality_composite.c + study_contrast1:commonality_composite.c +
                        study_contrast2:commonality_composite.c + self0:study_contrast1:commonality_composite.c +
                        self0:study_contrast2:commonality_composite.c, data = commonality_data_nona, whichRandom = c("id", "self0:id"))
commonality_null_BF = lmBF(percentage100 ~ self0 + study_contrast1 + study_contrast2 + self0:study_contrast1 + self0:study_contrast1 +
                             commonality_composite.c + study_contrast1:commonality_composite.c +
                        study_contrast2:commonality_composite.c, data = commonality_data_nona, whichRandom = c("id", "self0:id"))
commonality_full_BF / commonality_null_BF


###########RESEARCH QUESTION 5: Do Results differ if outgroup members are atheists vs religious?

usa_evan_data <- Merged_long[Merged_long$study == "USA Evangelical",]

usa_evan_data$cond_contrast1[usa_evan_data$condition == "Christian"] <- 1
usa_evan_data$cond_contrast1[usa_evan_data$condition == "Atheist" | usa_evan_data$condition == "Muslim"] <- -0.5
usa_evan_data$cond_contrast2[usa_evan_data$condition == "Christian"] <- 0
usa_evan_data$cond_contrast2[usa_evan_data$condition == "Atheist"] <- -0.5
usa_evan_data$cond_contrast2[usa_evan_data$condition == "Muslim"] <- 0.5

Q5 <- lmer(percentage100 ~ self0*cond_contrast1 + self0*cond_contrast2 + (1 |id) + (0+self0|id), data = usa_evan_data)
summary(Q5)
confint(Q5, method = "Wald") #using random slopes

Q5_table <- tab_model(c(Q5), show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                      pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Condition Contrast 1 (Christians = 1, Muslims and Atheists = -0.5)", "Condition Contrast 2 (Muslims = 0.5, Atheists = -0.5, Christians = 0)", "Perspective x Condition Contrast 1", "Perspective x Condition Contrast 2"),
                      dv.labels = c("Percentage of Total Stakes Given"),                
                      title = "Table 7. Results for model comparing atheist and religious outgroups", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;'),
                      col.order = c("est", "stat", "ci"), file = "q5_table.html")
Q5_table

#Dummy coding for simple effects

usa_evan_data$cond_mus_d1[usa_evan_data$condition == "Christian"] <- 0
usa_evan_data$cond_mus_d1[usa_evan_data$condition == "Atheist"] <- 1
usa_evan_data$cond_mus_d1[usa_evan_data$condition == "Muslim"] <- 0
usa_evan_data$cond_mus_d2[usa_evan_data$condition == "Christian"] <- 1
usa_evan_data$cond_mus_d2[usa_evan_data$condition == "Atheist"] <- 0
usa_evan_data$cond_mus_d2[usa_evan_data$condition == "Muslim"] <- 0

usa_evan_data$cond_ath_d1[usa_evan_data$condition == "Christian"] <- 0
usa_evan_data$cond_ath_d1[usa_evan_data$condition == "Atheist"] <- 0
usa_evan_data$cond_ath_d1[usa_evan_data$condition == "Muslim"] <- 1
usa_evan_data$cond_ath_d2[usa_evan_data$condition == "Christian"] <- 1
usa_evan_data$cond_ath_d2[usa_evan_data$condition == "Atheist"] <- 0
usa_evan_data$cond_ath_d2[usa_evan_data$condition == "Muslim"] <- 0



#Atheists
Q5_ath <- lmer(percentage100 ~ self0*cond_ath_d1 + self0*cond_ath_d2 + (1 |id) + (0+self0|id), data = usa_evan_data)
summary(Q5_ath)
confint(Q5_ath, method = "Wald") #using random slopes

#Muslims
Q5_mus <- lmer(percentage100 ~ self0*cond_mus_d1 + self0*cond_mus_d2 + (1 |id) + (0+self0|id), data = usa_evan_data)
summary(Q5_mus)
confint(Q5_mus, method = "Wald") #using random slopes


###########RESEARCH QUESTION 6: Do Results differ based on manipulation wording? SUPPLEMENTAL

Q6_random <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*design_contrast + (1|id) + (0 + self0|id), data = usa_evan_data)
summary(Q6_random)
confint(Q6_random, method="Wald")

Q6.wc0 <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*wc0 + (1|id) + (0+self0|id), data = usa_evan_data)
summary(Q6.wc0)
confint(Q6.wc0, method="Wald")

Q6.ec0 <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*ec0 + (1|id) + (0 + self0|id), data = usa_evan_data)
summary(Q6.ec0)
confint(Q6.ec0, method = "Wald")

Q6_table <- tab_model(c(Q6_random), show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                      pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Intergroup Condition (Ingroup = 0.5, Outgroup = -0.5)",
                                      "Manipulation  (Think about God's preferences = 0.5, Think about God = -0.5)", "Perspective x Intergroup Condition",
                                      "Perspective x Manipulation Contrast", "Intergroup Condition x Manipulation Check", "Perspective x Intergroup Condition x Manipulation Check"),
                      dv.labels = c("Percentage of Total Stakes Given"),                
                      title = "Supplemental Table 1. Results for model comparing manipulations", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;'),
                      col.order = c("est", "stat", "ci"), file = "q6_table.html")
Q6_table


############Alternative Model: Due to lack of normality and zero/1 inflation, regress difference score on context and intergroup condition
########Make long data wide
library(reshape2)
Merged_wide_small <- reshape2::dcast(Merged_long, id + study + C1_ME_vs_FJUSA + C2_USA_vs_FJ + C3_FJH_vs_FJMFMC +
                                       C4_FJM_vs_FJC + C5_USAE_vs_USA1_2 + C6_USA1_vs_USA2 + C7_WB_vs_Isr + ingrp_v_outgroup_contrast + knows_01 + cares_01 + reward_01 + punish_01 ~ self0, value.var = "percentage100")
names(Merged_wide_small)[names(Merged_wide_small)=="0"] <- "self"
names(Merged_wide_small)[names(Merged_wide_small)=="1"] <- "god"

#Make difference score
Merged_wide_small$perspective_dif <- Merged_wide_small$god - Merged_wide_small$self

hist(Merged_wide_small$perspective_dif)

#Run model

DScore_Model <- lm(perspective_dif ~ ingrp_v_outgroup_contrast*C1_ME_vs_FJUSA + C2_USA_vs_FJ*ingrp_v_outgroup_contrast +
                     C3_FJH_vs_FJMFMC*ingrp_v_outgroup_contrast + C4_FJM_vs_FJC*ingrp_v_outgroup_contrast + C5_USAE_vs_USA1_2*ingrp_v_outgroup_contrast +
                     C6_USA1_vs_USA2*ingrp_v_outgroup_contrast + C7_WB_vs_Isr*ingrp_v_outgroup_contrast, data = Merged_wide_small)
summary(DScore_Model)     
describe(Merged_wide_small$perspective_dif)
confint(DScore_Model)

DScore_Model_table <- tab_model(DScore_Model, show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                                pred.labels = c("Intercept (Change in Giving)", "Intergroup Condition (Ingroup = 0.5, Outgroup = -0.5)",
                                                "C1: Middle East = 3, USA & Fiji = -1", "C2: US = 1, Fiji = -1", "C3: Fiji Hindus = 2, Fiji Muslims & Christians = -1, Others = 0",
                                                "C4: Fiji Muslims = 1, Fiji Christians = -1, Others = 0", "C5: USA Evangelicals = 2, USA Studies 1 & 2 = -1, Others = 0",
                                                "C6: USA Study 1 = 1, USA Study 2 = -1, Others = 0", "C7: West Bank = 1, Israel = -1, Others = 0",
                                                "Intergroup Condition x C1", "Intergroup Condition x C2","Intergroup Condition x C3", "Intergroup Condition x C4",
                                                "Intergroup Condition x C5","Intergroup Condition x C6","Intergroup Condition x C7"),
                                dv.labels = c("Change in Percentage Given"),                
                                title = "Supplemental Table 2: Change score model", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;' ),
                                col.order = c("est", "stat", "ci"), file = "changescore.html")


#####How do Big God beliefs moderate results?

#Center vars
describe(Merged_wide_small$punish_01)
describe(Merged_wide_small$reward_01)
describe(Merged_wide_small$knows_01) #Not enough variation to test moderation
describe(Merged_wide_small$cares_01) #Not enough variation to test moderation

Merged_long$punish_01.c <- Merged_long$punish_01 - .56
Merged_long$punish_01.h <- Merged_long$punish_01.c - .3
Merged_long$punish_01.l <- Merged_long$punish_01.c + .3

Merged_long$reward_01.c <- Merged_long$reward_01 - .77
Merged_long$reward_01.h <- Merged_long$reward_01.c - .26
Merged_long$reward_01.l <- Merged_long$reward_01.c + .26

#Are general results moderated by beleif in a punishing God?
Punish_m1 <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*punish_01.c + (1 |study/id) + (0 + self0|id), data = Merged_long)
summary(Punish_m1)
confint(Punish_m1, method = "Wald")

Punish_m1.h <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*punish_01.h + (1 |study/id)+ (0 + self0|id), data = Merged_long)
summary(Punish_m1.h)
confint(Punish_m1.h, method = "Wald")

Punish_m1.l <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*punish_01.l + (1 |study/id)+ (0 + self0|id), data = Merged_long)
summary(Punish_m1.l)
confint(Punish_m1.l, method = "Wald")

Punish_m1_ingroup <- lmer(percentage100 ~ self0*ingrp0*punish_01.c + (1 |study/id)+ (0 + self0|id), data = Merged_long)
summary(Punish_m1_ingroup)
confint(Punish_m1_ingroup, method = "Wald")

Punish_m1_outgroup <- lmer(percentage100 ~ self0*outgrp0*punish_01.c + (1 |study/id)+ (0 + self0|id), data = Merged_long)
summary(Punish_m1_outgroup)
confint(Punish_m1_outgroup, method = "Wald")

punish_table <- tab_model(Punish_m1, show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                          pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Intergroup Condition (Ingroup = 0.5, Outgroup = -0.5)", "Belief in a Punishing God (Centered)",
                                          "Perspective x Intergroup Condition", "Perspective x Belief in a Punishing God", "Intergroup Condition x Belief in a Punishing God", "Perspective x Intergroup Condition x Belief in a Punishing God"), dv.labels = "Percentage of Total Stakes Given",
                          title = "Table 8. Results for belief in a punishing God moderation model", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;' ),
                          col.order = c("est", "stat", "ci"), file = "Punishtable.html")
punish_table #save table

#Reward
Reward_m1 <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*reward_01.c + (1 |study/id) + (0+self0|id), data = Merged_long)
summary(Reward_m1)
confint(Reward_m1, method = "Wald")

Reward_m1.h <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*reward_01.h + (1 |study/id) + (0+self0|id), data = Merged_long)
summary(Reward_m1.h)
confint(Reward_m1.h, method = "Wald")

Reward_m1.l <- lmer(percentage100 ~ self0*ingrp_v_outgroup_contrast*reward_01.l + (1 |study/id) + (0+self0|id), data = Merged_long)
summary(Reward_m1.l)
confint(Reward_m1.l, method = "Wald")

Reward_m1_ingroup <- lmer(percentage100 ~ self0*ingrp0*reward_01.c + (1 |study/id), data = Merged_long)
summary(Reward_m1_ingroup)
confint(Reward_m1_ingroup, method = "Wald")

Reward_m1_outgroup <- lmer(percentage100 ~ self0*outgrp0*reward_01.c + (1 |study/id), data = Merged_long)
summary(Reward_m1_outgroup)
confint(Reward_m1_outgroup, method = "Wald")

reward_table <- tab_model(Reward_m1, show.stat = TRUE, show.p = TRUE, p.style = "stars", show.df = TRUE, auto.label = FALSE,
                          pred.labels = c("Intercept (Baseline)", "Perspective (Self = 0, God = 1)", "Intergroup Condition (Ingroup = 0.5, Outgroup = -0.5)", "Belief in a Rewarding God (Centered)",
                                          "Perspective x Intergroup Condition", "Perspective x Belief in a Rewarding God", "Intergroup Condition x Belief in a Rewarding God", "Perspective x Intergroup Condition x Belief in a Rewarding God"), dv.labels = "Percentage of Total Stakes Given",
                          title = "Table 9. Results for belief in a rewarding God moderation model", CSS = list(css.thead = 'padding:0.00;', css.tdata = 'padding:0.00;', css.zeroparts = 'padding:0.00;', css.summary = 'padding:0.00;' ),
                          col.order = c("est", "stat", "ci"), file = "Rewardtable.html")
reward_table #save table

