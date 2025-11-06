######################################################################################### 
#Tears Trigger the Intention to Offer Social Support: A Systematic Investigation 
#of the Interpersonal Effects of Emotional Crying Across 41 Countries. Zickfeld et al. (2020).
#Main Syntax.

#Overview.

#1. Load Libraries, Data, Set WD.
#2. Internal Reliabilities & Score Computation
#3. Recoding
#4. Demographics
#5. MAIN ANALYSES
#6. EXPLORATORY ANALYSES
#########################################################################################

#######CCT MAIN SYNTAX#################

################################
#STEP 1. 
#LOAD LIBRARIES., DATA. SET WD.
################################
#Check if required packages are installed
if ("tidyr" %in% installed.packages() == FALSE) {install.packages("tidyr")}
if ("simr" %in% installed.packages() == FALSE) {install.packages("simr")}
if ("metafor" %in% installed.packages() == FALSE) {install.packages("metafor")}
if ("dplyr" %in% installed.packages() == FALSE) {install.packages("dplyr")}
if ("reshape2" %in% installed.packages() == FALSE) {install.packages("reshape2")}
if ("sjPlot" %in% installed.packages() == FALSE) {install.packages("sjPlot")}
if ("sjmisc" %in% installed.packages() == FALSE) {install.packages("sjmisc")}
if ("devtools" %in% installed.packages() == FALSE) {install.packages("devtools")}
if ("lme4" %in% installed.packages() == FALSE) {install.packages("lme4")}
if ("emmeans" %in% installed.packages() == FALSE) {install.packages("emmeans")}
if ("ggpubr" %in% installed.packages() == FALSE) {install.packages("ggpubr")}
if ("ggplot2" %in% installed.packages() == FALSE) {install.packages("ggplot2")}
if ("car" %in% installed.packages() == FALSE) {install.packages("car")}
if ("gmodels" %in% installed.packages() == FALSE) {install.packages("gmodels")}
if ("metaforest" %in% installed.packages() == FALSE) {install.packages("metaforest")}
if ("apaTables" %in% installed.packages() == FALSE) {install.packages("apaTables")}
if ("psych" %in% installed.packages() == FALSE) {install.packages("psych")}
if ("countrycode" %in% installed.packages() == FALSE) {install.packages("countrycode")}
if ("semTools" %in% installed.packages() == FALSE) {install.packages("semTools")}
if ("rworldmap" %in% installed.packages() == FALSE) {install.packages("rworldmap")}
if ("readxl" %in% installed.packages() == FALSE) {install.packages("readxl")}
if ("Routliers" %in% installed.packages() == FALSE) {install.packages("Routliers")}
if ("caret" %in% installed.packages() == FALSE) {install.packages("caret")}
if ("interactions" %in% installed.packages() == FALSE) {install.packages("interactions")}
if ("lavaanPlot" %in% installed.packages() == FALSE) {install.packages("lavaanPlot")}
if ("janitor" %in% installed.packages() == FALSE) {install.packages("janitor")}

if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')

library(raincloudplots)

#load libraries.

library(tidyr) #-> version 0.8.3
library(simr) #-> version 1.0.4
library(metafor) #-> version 2.0-0
library(dplyr) #-> version 0.8.1
library(reshape2) #-> version 1.4.3
library(sjPlot) #-> version 2.6.2
library(sjmisc) #-> version 2.7.7
library(devtools) #-> version 2.0.2
library(lme4) #-> version 1.1-21
library(emmeans) #-> version 1.2.3
library(car) #-> version 3.0-2
library(ggpubr) #-> version 0.1.8.999
library(gmodels) #-> version 2.18.1
library(metaforest) #-> 0.1.3
library(apaTables) #-> 2.0.5
library(psych) #-> 1.9.12
library(countrycode) #-> 1.1.2
library(semTools) #-> 0.5-2
library(rworldmap) #-> 1.3-6
library(readxl) #-> 1.3.1
library(Routliers) #-> 0.0.0.3
library(caret) #-> 6.0-86
library(interactions) #-> 1.1.3 
library(lavaanPlot) #-> 0.5.1
source("http://sachaepskamp.com/files/OSF/getOSFfile.R")

#set working directory.
setwd("C:\\Users\\jhzic\\Dropbox\\Cross-Cultural Tears\\Data") #adapt to own working directory. 

#load dataset.
#if not working download manually from https://osf.io/yb6w8/
dat <- getOSFfile("yb6w8")
dat <- read.csv(dat)


#alternatively load from disk.
#dat <- read.csv("Dat_merged.csv")

###


######################################
#Step 2. 
#Internal Reliabilities & Score Computation.
######################################

##NOTE. Set main exclusion filter if you want to calculate correct reliabilities! 
##calculate internal reliabilities of scales.
##compute with filter == 1 !!!!
##exclusion criteria are detailed in the manuscript. they exclude participants < 18 years, participants that indicate a different nationality than the
##lab location AND that the lab country has not influenced them culturally, cases failing an attention check question (i.e., selecting the right situation), 
##cases finishing less than 50% of all items. 
#dat <- dat %>% 
#  filter(filter==1)


#2.1 reliability of competence items.
cor(dat$SCM_1, dat$SCM_2, use="complete.obs")
#r of .7599.

#compute competence score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Competence=mean(c(SCM_1,SCM_2)))
dat <- as.data.frame(dat)


#2.2 reliability of warmth items.
cor(dat$SCM_3, dat$SCM_4, use="complete.obs")
#r of .7522.


#compute warmth score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Warmth=mean(c(SCM_3,SCM_4)))
dat <- as.data.frame(dat)

#2.3 reliability of honesty items.
cor(dat$SCM_5, dat$SCM_6, use="complete.obs")
#r of .6827.


#compute honesty score.
dat <- dat %>% 
    rowwise() %>% 
    mutate(Honesty=mean(c(SCM_5,SCM_6)))
dat <- as.data.frame(dat)


#2.4 reliability of support items.
sup_alpha <- dat %>% 
  select(Support, Needed, Accept) %>% 
  do(alpha = psych::alpha(.)) %>% 
  unlist()
sup_alpha$alpha.total.std.alpha
#alpha of .87.

dat <- dat %>% 
    rowwise() %>% 
    mutate(Support_Intention=mean(c(Support, Needed, Accept)))
dat <- as.data.frame(dat)

#2.5 reliability of helplessness items.


hel_alpha <- dat %>% 
  select(Helpless, Overwhelmed, Sad) %>% 
  do(alpha = psych::alpha(.)) %>% 
  unlist()

hel_alpha$alpha.total.std.alpha
#alpha of .86.

#compute helplessness score. 
dat <- dat %>% 
    rowwise() %>% 
    mutate(Helplessness=mean(c(Sad, Helpless, Overwhelmed)))
dat <- as.data.frame(dat)

#2.6 reliability of empathic concern (state) items.
cor(dat$ECPD_1, dat$ECPD_2, use="complete.obs")
#r of .8229.

dat <- dat %>% 
    rowwise() %>% 
    mutate(EC=mean(c(ECPD_1, ECPD_2)))
dat <- as.data.frame(dat)

#2.7 reliability of personal distress (state) items.
cor(dat$ECPD_3, dat$ECPD_4, use="complete.obs")
#r of .588.


dat <- dat %>% 
    rowwise() %>% 
    mutate(PD=mean(c(ECPD_3, ECPD_4)))
dat <- as.data.frame(dat)

#2.8 reliability of empathic concern items.

dat$EC_2_r <- (dat$EC_2 - 6) * -1
dat$EC_4_r <- (dat$EC_4 - 6) * -1
dat$EC_5_r <- (dat$EC_5 - 6) * -1

dat <- dat %>% 
        rename(EC_1 = ?..EC_1)

ec_alpha <- dat %>% 
  select(EC_1, EC_2_r, EC_3, EC_4_r, EC_5_r, EC_6, EC_7) %>% 
  do(alpha = psych::alpha(.)) %>% 
  unlist()

ec_alpha$alpha.total.std.alpha
#alpha of .74.

#compute trait empathic concern score
dat <- dat %>% 
    rowwise() %>% 
    mutate(Empathic_Concern=mean(c(EC_1, EC_2_r, EC_3, EC_4_r, EC_5_r, EC_6, EC_7)))
dat <- as.data.frame(dat)


################################
#STEP 3. 
#RECODING 
################################

##3.1 convert factors.

dat$Condition <- factor(dat$Condition, levels=c(-.5,.5), #occurrence of tears.
                        labels=c("No Tears", "Tears"))

dat$Target_Gender <- factor(dat$Target_Gender, levels = c(-.5,.5), #target gender.
                            labels = c("Male", "Female")) 

dat$Type <- factor(dat$Type, levels = c(-.5,0,.5), #situational valence
                   labels = c("Negative", "Neutral", "Positive"))

dat$Ethnicity[dat$Ethnicity == 0] <- NA #target ethnicity
dat$Ethnicity <- factor(dat$Ethnicity, levels = c(1,2,3,4,5),
                        labels = c("Black", "East Asian", "Latin", "Middle East", "White"))

dat$Social_Context <- factor(dat$Social_Context, levels = c(-.5,.5), #social context.
                             labels = c("Public", "Private"))

dat$Gender <- factor(dat$Gender, levels = c(1,2,3), #participant's gemder.
                     labels = c("Female", "Male", "Other"))


dat %>% 
  group_by(Gender) %>% 
  summarise(n = n(), perc = n()/nrow(.))
#check amount of cases and percentages in dataset. 
#less than 5% other cases. thus excluded for main analyses. 

#create new gender variable without other category. 
dat <- dat %>% 
  mutate(Gender_c = ifelse(Gender == "Male", -.5, ifelse(Gender == "Female", .5, NA)))
dat$Gender_c <- as.factor(dat$Gender_c)
levels(dat$Gender_c)[1] <- "Male"
levels(dat$Gender_c)[2] <- "Female"

#3.2 Set contrasts for main variables..
contrasts(dat$Type)
contrasts(dat$Type) <- cbind(c(.33,-.66,.33),c(.50,0,-.50)) #contrast for situational valence

contrasts(dat$Condition) <- c(-.5, .5) #contrast for occurrence of tears.
contrasts(dat$Condition)

contrasts(dat$Social_Context) <- c(-.5, .5) #contrast for social context.
contrasts(dat$Target_Gender) <- c(-.5, .5) #contrast for target gender.

contrasts(dat$Gender_c) <- c(-.5, .5) #contrast for participant's gender. 

#3.3 Compute/Transform Variables.

#Readjust Main Variables. 

#need to reajust scales from 1 to 7 to 0 to 6.
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

#compute ID number that does not contain duplicates per country. 
dat$ID <- group_indices(dat, MTurkCode, Lab_ID, Country)

#Recode Country Codes to country names. 
dat$CountryName <- countrycode(dat$Country, origin = "iso3n", destination = "country.name")


#center continuous predictors.

#set up center function.
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

#center appropriateness.
dat$Appropriate_c <- center_scale(dat$Appropriate)
dat$Appropriate_c <- as.numeric(dat$Appropriate_c)

#center empathic concern. 
dat$Empathic_Concern_c <- center_scale(dat$Empathic_Concern)
dat$Empathic_Concern_c <- as.numeric(dat$Empathic_Concern_c)

#center group indentification. 
dat$Ethnicity_Fit_c <- center_scale(dat$Group)
dat$Ethnicity_Fit_c <- as.numeric(dat$Ethnicity_Fit_c)


######################################
#STEP 4. 
#Demographics.

######################################

#4.1 check total number of participants.
dat %>% 
  group_by(Lab_ID, MTurkCode) %>% 
  summarise(n = n())
#total of 7745 participants.

##filter nation filter for Supplementary Material Analyses 4.5.
dat_n <- dat %>% 
          filter(filter_nation==1)

#4.2 now FILTER out excluded cases. MAIN EXCLUSION FILTER.
#ALWAYS USE THIS FILTERED DATASET FOR THE FINAL ANALYSES!!!
##exclusion criteria are detailed in the manuscript. they exclude participants < 18 years, participants that indicate a different nationality than the
##lab location AND that the lab country has not influenced them culturally, cases failing an attention check question (i.e., selecting the right situation), 
##cases finishing less than 50% of all items. 
dat <- dat %>% 
        filter(filter==1)

#Check final amount of participants.
dat %>% 
  group_by(Lab_ID, MTurkCode) %>% 
  summarise(n = n())
#remaining 7007 participants.

#4.3 Check distribution by gender. 
dat %>%
  group_by(Lab_ID, MTurkCode) %>%
  slice(which.min(Gender)) %>%
  ungroup(MTurkCode) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 1975 Males, 4474 Females 45 other

labdistr <- dat %>% 
  group_by(Lab_ID,MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Lab_ID,Gender) %>% 
  summarise(n = n(), per = n()/7007)
#check gender per Lab -> Table 1. 


#4.4 Check age distribution.

#fix age. one participant indicated an impossible age, set to NA.
dat$Age[dat$Age==663] <- NA

#summarise age. 
dat %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 28.0845, sd = 10.886 min 18 max 79

#summarise age by lab -> Table 1. 
age_dist <- dat %>% 
  group_by(Lab_ID) %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))

#4.5 Check nationality distribution
dat %>% 
  group_by(Lab_ID,MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Nation) %>% 
  summarise(n = n(), per = n()/7007)

#4.6 Check lab distribution 

labdistr <- dat %>% 
  group_by(Lab_ID,MTurkCode) %>%
  slice(which.min(Nation)) %>% 
  ungroup(MTurkCode) %>% 
  group_by(Lab_ID) %>% 
  summarise(n = n(), per = n()/7007)

#4.7 check for distribution of ehtnicity and targets.

#Cross Table of Type and Target Gender
CrossTable(dat$Type, dat$Target_Gender, chisq=TRUE, format="SPSS")

#Cross Table of Ethnicity and Type
CrossTable(dat$Type, dat$Ethnicity, chisq=TRUE, format="SPSS")

#Cross Table Target Gender & Ethnicity.
CrossTable(dat$Target_Gender, dat$Ethnicity, chisq=TRUE, format="SPSS")

#Cross Table Target Gender & Social Context.
CrossTable(dat$Target_Gender, dat$Social_Context, chisq=TRUE, format="SPSS")

#4.8 general overview of data.
describe(dat)

#4.9 get descriptives per condition & type. -> Table 2
Descriptives <- dat %>% #compute descriptives by occurrence of tears.
  group_by(Condition) %>% 
  select(Support_Intention, Warmth, IOS, Helplessness, Pos, Neg, Appropriate, EC, PD, Empathic_Concern) %>% 
  do(descr = describe(.))

Descriptives$descr[1] #-> no tears
Descriptives$descr[2] #-> tears

Descriptives <- dat %>% #compute descriptives by occurrence and situational valence
  group_by(Condition, Type) %>% 
  select(Support_Intention, Warmth, IOS, Helplessness, Pos, Neg, Appropriate, EC, PD, Empathic_Concern) %>% 
  do(descr = describe(.))

Descriptives$descr[1] #-> nt, negative
Descriptives$descr[2] #-> nt, neutral
Descriptives$descr[3] #-> nt, positive

Descriptives$descr[4] #-> t, negative
Descriptives$descr[5] #-> t, neutral
Descriptives$descr[6] #-> t, positive

################################################################
#Step 5. MAIN ANALYSES.

################################################################
hist(dat$PD)
mean(dat$PD, na.rm=TRUE)
#5.0 Measurement Invariance

support.model <- 'support =~ Support + Needed + Accept' #set up main model.

measurementInvariance(model = support.model, data = dat,
                      group = "Country", fit.measures = c("cfi","aic", "rmsea")) #test for measurement invariance across countries.


#############

#5.1: H1. Model with Intention to Help.

model_h1 <- lmerTest::lmer(Support_Intention ~ Condition + (1|Country/ID) + (1|Ethnicity/StimNum), data=dat) #run main model H1. 

h1_sum <- summary(model_h1) #get model overview. 
emmeans(model_h1, "Condition") #retrieve estimated means per condition (OT). 

pval <- h1_sum$coefficients[,5] #store p-values. 

tab_model(model_h1, show.df=TRUE, 
          show.std = TRUE, 
          show.fstat = TRUE, 
          auto.label = TRUE) #get overview, effect sizes -> Table 3. 

intent_d
#5.1.1 Meta Analysis Across Countries. -> Figure 2. 
#get m/sd and n for each condition and country seperately. 
intent_d <- dat %>% 
  group_by(CountryName, Condition) %>% 
  summarise(mean = mean(Support_Intention, na.rm=TRUE), sd = sd(Support_Intention, na.rm=TRUE), n = n()) #get means per country and condition.
#restructure to wide.
intent_d <- dcast(melt(intent_d, id.vars=c("CountryName","Condition")), CountryName~variable+Condition)
#calculate effect size.
intent_d <- escalc(measure="SMD", m1i=mean_Tears, sd1i=sd_Tears, n1i=n_Tears,
                   m2i=`mean_No Tears`, sd2i=`sd_No Tears`, n2i=`n_No Tears`,
                   data=intent_d)
#run random effects model.
intent_rma <- rma(yi, vi, slab = paste(CountryName), data=intent_d)
intent_rma

#plot forest plot. -> Figure 2. 
png("Figure2.png", width = 3000, height = 3000, res=300) #save figure. 
forest(intent_rma)
dev.off()
#confindence intervals for heterogeneity measures.
confint(intent_rma)


#5.1.1.1 World map.

intent_d <- intent_d %>% 
  filter(CountryName != "United Arab Emirates")

intent_d$Country <- countrycode(intent_d$CountryName, origin = "country.name", destination = "iso3n")

map <- intent_d %>% 
  select(Country, yi)
map <- as.data.frame(map)

map$Country <- countrycode(map$Country, origin = "iso3n", destination = "iso3c")
map_support <- joinCountryData2Map(map, 
                                   joinCode = "ISO3",
                                   nameJoinColumn = "Country")

png("WorldMap.png", width = 5000, height = 5000, res=300)
mapCountryData(map_support, nameColumnToPlot = "yi", mapTitle = "Intention to Support (d)", colourPalette = viridis::viridis(40), numCats=nrow(map),
               catMethod = "fixedWidth", missingCountryCol = "grey", borderCol = "black", addLegend = TRUE)
dev.off()

#####################################

#5.1.2: H1b. Model with Type of Tears.

model_final <- lmerTest::lmer(Support_Intention ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), data=dat) #main model H1b. 
H1b_sum <- summary(model_final) #main effect of oc and tt.
emmeans(model_final, "Condition") #estimated means OT.
emmeans(model_final, "Type") #estimated means SV.
confint(model_final) #confidence intervals.

pval <- c(pval,H1b_sum$coefficients[,5]) #record pvalue

em_interaction <- emmeans(model_final, pairwise ~ Condition | Type) #compare interaction.
eff_size(em_interaction$emmeans, sigma=sigma(model_final), edf=24867) #get effect sizes.

tab_model(model_final, show.df=TRUE, 
          show.std = TRUE, 
          show.fstat = TRUE, 
          auto.label = TRUE) #get overview, effect sizes -> Table 3. 

#Plot. #-> Figure 4
H1b_plot <- ggbarplot(dat, x = "Condition", y = "Support_Intention", fill = "Type",
            add = "mean_ci", position = position_dodge(.9),
            ylab = "Intention to Support", xlab = "Occurrence of Tears") +
            scale_fill_viridis_d(name = "Situational Valence")

##

################################

################################

#5.3: H2. Main Mediation Models. 

#5.3.1 Model with Perceived Warmth.
model_h2_warmth <- lmer(Warmth ~ Condition + (1|Country/ID) + (1|Ethnicity/StimNum), data=dat,
                        control = lmerControl("Nelder_Mead")) 
summary(model_h2_warmth)

#5.3.2 Model with Perceived Helplessness.
model_h2_helpless <- lmer(Helplessness ~ Condition + (1|Country/ID) + (1|Ethnicity/StimNum), data=dat,
                          control = lmerControl("Nelder_Mead")) 
summary(model_h2_helpless)


#5.3.3 Model with IOS.
model_h2_ios <- lmer(IOS ~ Condition + (1|Country/ID) + (1|Ethnicity/StimNum), data=dat,
                               control = lmerControl("Nelder_Mead")) 
summary(model_h2_ios)

tab_model(model_h2_warmth, 
          model_h2_helpless, 
          model_h2_ios, 
          show.df=TRUE, 
          show.std = TRUE, 
          show.fstat = TRUE, 
          auto.label = TRUE) #get overview, effect sizes -> Supplementary Table 4.4.5. 

##

#5.3.4 Meta Analysis Across Countries. 

#Perceived Warmth
warmth_d <- dat %>% 
  group_by(Country, Condition) %>% 
  summarise(mean = mean(Warmth, na.rm=TRUE), sd = sd(Warmth, na.rm=TRUE), n = n())
#restructure to wide.
warmth_d <- dcast(melt(warmth_d, id.vars=c("Country","Condition")), Country~variable+Condition)
#calculate effect size.
warmth_d <- escalc(measure="SMD", m1i=mean_Tears, sd1i=sd_Tears, n1i=n_Tears,
                   m2i=`mean_No Tears`, sd2i=`sd_No Tears`, n2i=`n_No Tears`,
                   data=warmth_d)
#run random effects model
warmth_rma <- rma(yi, vi, slab = paste(Country), data=warmth_d)
warmth_rma
#plot forest plot.
jpeg("ForestWarmth.jpeg", width=1000, height=800)
forest(warmth_rma)
dev.off()
#confindence intervals for heterogeneity measures.
confint(warmth_rma)

##

#Perceived Helplessness
Helplessness_d <- dat %>% 
  group_by(Country, Condition) %>% 
  summarise(mean = mean(Helplessness, na.rm=TRUE), sd = sd(Helplessness, na.rm=TRUE), n = n())
#restructure to wide.
Helplessness_d <- dcast(melt(HELPLESSNESS_d, id.vars=c("Country","Condition")), Country~variable+Condition)
#calculate effect size.
Helplessness_d <- escalc(measure="SMD", m1i=mean_Tears, sd1i=sd_Tears, n1i=n_Tears,
                         m2i=`mean_No Tears`, sd2i=`sd_No Tears`, n2i=`n_No Tears`,
                         data=Helplessness_d)
#run random effects model.
Helplessness_rma <- rma(yi, vi, slab = paste(Country), data=Helplessness_d)
Helplessness_rma
#plot forest plot.
jpeg("ForestHELPLESSNESS.jpeg", width=1000, height=800)
forest(Helplessness_rma)
dev.off()
#confindence intervals for heterogeneity measures.
confint(Helplessness_rma)

##

#Perceived Connectedness.
IOS_d <- dat %>% 
  group_by(Country, Condition) %>% 
  summarise(mean = mean(IOS, na.rm=TRUE), sd = sd(IOS, na.rm=TRUE), n = n())
#restructure to wide.
IOS_d <- dcast(melt(IOS_d, id.vars=c("Country","Condition")), Country~variable+Condition)
#calculate effect size.
IOS_d <- escalc(measure="SMD", m1i=mean_Tears, sd1i=sd_Tears, n1i=n_Tears,
                m2i=`mean_No Tears`, sd2i=`sd_No Tears`, n2i=`n_No Tears`,
                data=IOS_d)
#run random effects model.
IOS_rma <- rma(yi, vi, slab = paste(Country), data=IOS_d)
IOS_rma
#plot forest plot.
jpeg("ForestIOS.jpeg", width=1000, height=800)
forest(IOS_rma)
dev.off()
#confindence intervals for heterogeneity measures.
confint(IOS_rma)


#5.4 H2. Mediation Analyses.
##IV: Occurence of Tears; M: Perceived Warmth, IOS, Perceived Sadness. DV: Intention to Help.

#5.4.1 model path a M -> IV.

fm_med1_w <- lmerTest::lmer(Warmth ~ Condition +
                            (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)

fm_med1_h <- lmerTest::lmer(Helplessness ~ Condition +
                               (1|Country/ID) + (1|Ethnicity/StimNum), data=dat, control=lmerControl("Nelder_Mead"))

fm_med1_c <- lmerTest::lmer(IOS ~ Condition +
                              (1|Country/ID) + (1|Ethnicity/StimNum), data=dat, control=lmerControl("Nelder_Mead"))

H2_a_w <- summary(fm_med1_w)
confint(fm_med1_w, method="Wald")
pval <- c(pval, H2_a_w$coefficients[,5]) #store p-values. 


H2_a_h <- summary(fm_med1_h)
confint(fm_med1_h, method="Wald")
pval <- c(pval,H2_a_w$coefficients[,5]) #store p-values. 


H2_a_c <- summary(fm_med1_c)
confint(fm_med1_c, method="Wald")
pval <- c(pval,H2_a_c$coefficients[,5]) #store p-values. 

#effect by warmth and helplessness.

##################.

#5.4.2 model Path B + C' DV -> M + IV.

fm_med2 <- lmerTest::lmer(Support_Intention ~ Condition + Warmth + IOS + Helplessness +
                  (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)
H2_b <- summary(fm_med2)
confint(fm_med2, method="Wald")
pval <- c(pval,H2_b$coefficients[,5]) #store p-values. 

#full mediation by warmth, IOS and helplessness
.7030 * .2518 #Warmth .177 [.1665, .1877]
1.624 * .2647 #Helpless .4298 [.4121, .4478]
.5342 * .2333 #IOS .1246 [.1159, .1336]
##################.

#5.4.3 model Path C.

fm_med3 <- lmerTest::lmer(Support_Intention ~ Condition +
                   (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)

H2_c <- summary(fm_med3)
confint(fm_med3, method="Wald")
pval <- c(pval,H2_c$coefficients[,5]) #store p-values. 

#############################

##############################
#5.4b H2b. Mediation Analyses.
##IV: Occurence of Tears; M: Empathic Concern, Personal Distress. DV: Intention to Help.

#5.4.1 model path a M -> IV.

fm_med1_ec <- lmerTest::lmer(EC ~ Condition +
                               (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)

fm_med1_pd <- lmerTest::lmer(PD ~ Condition +
                               (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)

H2b_e <- summary(fm_med1_ec)
confint(fm_med1_ec, method="Wald")
pval <- c(pval,H2b_e$coefficients[,5]) #store p-values. 


H2b_d <- summary(fm_med1_pd)
confint(fm_med1_pd, method="Wald")
pval <- c(pval,H2b_d$coefficients[,5]) #store p-values. 

#effect by empathic concern & personal distress.

##################.

#5.4.2 model Path B + C' DV -> M + IV.

fm_med2 <- lmerTest::lmer(Support_Intention ~ Condition + EC + PD +
                            (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)
H2b_b <- summary(fm_med2)
confint(fm_med2, method="Wald")
pval <- c(pval,H2b_b$coefficients[,5]) #store p-values. 

#full mediation by warmth, IOS and helplessness
1.409 * .4903 #Ind. EC .69 [.6695, .7124]
.5359 * .03111 #Ind PD .01667 [.0113, .0221]
##################.

#5.4.3 model Path C.

fm_med3 <- lmer(Support_Intention ~ Condition +
                  (1|Country/ID) + (1|Ethnicity/StimNum), data=dat)

summary(fm_med3)
confint(fm_med3, method="Wald")


################################

################################
#5.5 H3: Mediation Model Appropriateness.

#Plot Interaction Effect on Appropriateness #-> Figure 1.8.3
plot_app <- ggbarplot(dat, x = "Condition", y = "Appropriate", fill = "Type",
 add = "mean_ci", position = position_dodge(.9),
 ylab = "Perceived Appropriateness", xlab = "Occurrence of Tears") +
  scale_fill_viridis_d(name = "Situational Valence")


#compile Figure 3. 
png("Figure3.png", width = 3000, height = 2000, res=300)
ggarrange(H1b_plot, plot_app, labels = c("A","B"), common.legend = TRUE)
dev.off()


##CREATE PLOTS FOR FIGURE 5. 
#add factor coding for Contrast. 
dat <- dat %>% 
  mutate(ContrastA = ifelse(Type == "Neutral", -.66, .33),
         ContrastB = ifelse(Type == "Positive", -.5, ifelse(Type == "Negative", .5,NA)))
dat$ContrastA <- as.factor(dat$ContrastA)
levels(dat$ContrastA)[1] <- "Neutral"
levels(dat$ContrastA)[2] <- "Negative/Positive"

dat$ContrastB <- as.factor(dat$ContrastB)
levels(dat$ContrastB)[1] <- "Positive"
levels(dat$ContrastB)[2] <- "Negative"

png("Figure51.png", width = 2000, height = 2000, res=300)
ggline(dat, x = "Condition", y = "Appropriate", color = "ContrastA",
          add = "mean_ci",
          ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()

png("Figure52.png", width = 2000, height = 2000, res=300)
dat %>% 
  filter(ContrastB %in% c("Positive", "Negative")) %>% 
ggline(., x = "Condition", y = "Appropriate", color = "ContrastB",
          add = "mean_ci", 
          ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()

#######

png("Figure51_W.png", width = 2000, height = 2000, res=300)
ggline(dat, x = "Condition", y = "Warmth", color = "ContrastA",
       add = "mean_ci",
       ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()

png("Figure52_W.png", width = 2000, height = 2000, res=300)
dat %>% 
  filter(ContrastB %in% c("Positive", "Negative")) %>% 
  ggline(., x = "Condition", y = "Warmth", color = "ContrastB",
         add = "mean_ci", 
         ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()
###

png("Figure51_H.png", width = 2000, height = 2000, res=300)
ggline(dat, x = "Condition", y = "Helplessness", color = "ContrastA",
       add = "mean_ci",
       ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()

png("Figure52_H.png", width = 2000, height = 2000, res=300)
dat %>%
  filter(ContrastB %in% c("Positive", "Negative")) %>% 
  ggline(., x = "Condition", y = "Helplessness", color = "ContrastB",
         add = "mean_ci", 
         ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()


###

png("Figure51_C.png", width = 2000, height = 2000, res=300)
ggline(dat, x = "Condition", y = "IOS", color = "ContrastA",
       add = "mean_ci",
       ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()

png("Figure52_C.png", width = 2000, height = 2000, res=300)
dat %>% 
  filter(ContrastB %in% c("Positive", "Negative")) %>% 
  ggline(., x = "Condition", y = "IOS", color = "ContrastB",
         add = "mean_ci", 
         ylab = "", xlab = "", size=2) +
  scale_color_viridis_d(name = "", end=.7) +
  theme(legend.position = "none")
dev.off()

#5.5.1. Warmth Model.
#5.5.1.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat,
                         control=lmerControl("Nelder_Mead"))
H3_a <- summary(path_a)
confint(path_a, method="Wald")
pval <- c(pval,H3_a$coefficients[,5]) #store p-values. 

#5.5.1.2 Path b.
path_b <- lmerTest::lmer(Warmth ~ Condition*Type + Appropriate_c + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat)
H3_w_b <- summary(path_b)
confint(path_b, method="Wald")
pval <- c(pval,H3_w_b$coefficients[,5]) #store p-values. 

#5.5.1.3 Path c. 

path_c <- lmerTest::lmer(Warmth ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat,
                         control=lmerControl("Nelder_Mead"))
H3_w_c <- summary(path_c)
confint(path_c, method="Wald")
pval <- c(pval,H3_w_c$coefficients[,5]) #store p-values. 

3.204 * .3040 #.9359, 1.0127
.2232 * .3040 #.0387, .0971
#5.5.2 IOS Model.
#5.5.2.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat, control=lmerControl("Nelder_Mead"))
summary(path_a)
confint(path_a, method="Wald")
 

#5.5.2.2 Path b.
path_b <- lmerTest::lmer(IOS ~ Condition*Type + Appropriate_c + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat,
                         control=lmerControl("Nelder_Mead"))
H3_c_b <- summary(path_b)
confint(path_b, method="Wald")
pval <- c(pval,H3_c_b$coefficients[,5]) #store p-values. 

#5.5.2.3 Path c. 

path_c <- lmerTest::lmer(IOS ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat,
                         control = lmerControl("Nelder_Mead"))
H3_c_c <- summary(path_c)
confint(path_c, method="Wald")
pval <- c(pval,H3_c_c$coefficients[,5]) #store p-values. 

3.204 * .2392 #.7293, .8040
.2232 * .2392 #.0304, .0765'
#5.5.3 Helplessness Model.
#5.5.3.1 Path a. 

path_a <- lmerTest::lmer(Appropriate ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat, 
                         control=lmerControl("Nelder_Mead"))
summary(path_a)
confint(path_a, method="Wald")

#5.5.3.1 Path b.
path_b <- lmerTest::lmer(Helplessness ~ Condition*Type + Appropriate_c + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat, 
                         control=lmerControl("Nelder_Mead"))
H3_h_b <- summary(path_b)
confint(path_b, method="Wald")
pval <- c(pval,H3_h_b$coefficients[,5]) #store p-values. 

#5.5.3.1 Path c. 

path_c <- lmerTest::lmer(Helplessness ~ Condition*Type + (1|Country/ID) + (1|Ethnicity/StimNum), dat=dat)
H3_h_c <- summary(path_c)
confint(path_c, method="Wald")
pval <- c(pval,H3_h_c$coefficients[,5]) #store p-values. 

.063 * 3.20 #.1689, .2350
.063 * .22 #.0078, .0208
#############

#5.6 H4-7: Moderation Model.

#5.6. Full Model with all variables. 

Intent_full <- lmerTest::lmer(Support_Intention ~ Condition + Target_Gender + Social_Context + Ethnicity_Fit_c + Empathic_Concern_c + 
                                Gender_c + Condition*Target_Gender + Condition*Social_Context + Condition*Gender_c + Condition*Ethnicity_Fit_c + 
                                Condition*Empathic_Concern_c + Target_Gender*Gender_c +
                                 (1|Country/ID) + (1|Ethnicity/StimNum), control=lmerControl("Nelder_Mead"), dat=dat)

Intent_sum <- summary(Intent_full)
emmeans(Intent_full, "Condition")
emmeans(Intent_full, "Target_Gender")
emmeans(Intent_full, "Social_Context")
emmeans(Intent_full, "Gender_c")

pval <- c(pval, Intent_sum$coefficients[,5])
p.adjust(pval, method="holm") #adjust pvalues. 

TOSTER::TOSTmeta(ES = 0.09, se = 0.0153, low_eqbound_d=-0.2, high_eqbound_d=0.2, alpha=0.05) #equivalence test. values are taken from tab_model function
TOSTER::TOSTmeta(ES = 0.07, se = 0.0153, low_eqbound_d=-0.2, high_eqbound_d=0.2, alpha=0.05) #equivalence test. values are taken from tab_model function

#model table. 
tab_model(Intent_full, show.df=TRUE, show.std = TRUE, show.fstat = TRUE, auto.label = TRUE) #-> Table 7. 

#Plot Interactions.

#social context (H4). 
sc_plot <- ggbarplot(dat, x = "Condition", y = "Support_Intention", fill = "Social_Context",
 add = "mean_ci", position = position_dodge(.8),
 ylab = "Intention to Support", xlab = "Occurrence of Tears") +
  scale_fill_viridis_d(name = "Social Context")

#target gender (H5). 
tg_plot <- ggbarplot(dat, x = "Condition", y = "Support_Intention", fill = "Target_Gender",
 add = "mean_ci", position = position_dodge(.8),
 ylab = "Intention to Support", xlab = "Occurrence of Tears") +
  scale_fill_viridis_d(name = "Target Gender")

#target gender x gender (H5b). 

tgge_plot <- dat %>% 
  filter(Gender_c %in% c("Female", "Male")) %>% 
ggbarplot(., x = "Condition", y = "Support_Intention", fill = "Target_Gender", facet.by = "Gender_c",
 add = "mean_ci", position = position_dodge(.8),
 ylab = "Intention to Support", xlab = "Occurrence of Tears") +
  scale_fill_viridis_d(name = "Target Gender")

#empathy (h6)
ec_plot <- ggplot(dat, aes(x=Empathic_Concern_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm) +
  labs(x = "Trait Empathic Concern", y = "Intention to Support") +
  scale_color_viridis_d() +
  jtools::theme_apa() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

#group identification. (h7) #
gi_plot <- ggplot(dat, aes(x=Ethnicity_Fit_c, y=Support_Intention, color=Condition, shape=Condition)) +
  geom_smooth(method=lm) +
  labs(x = "Group Identification", y = "Intention to Support") +
  scale_color_viridis_d() +
  jtools::theme_apa() +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))


png("Figure4.png", width = 6000, height = 3000, res=600)
ggarrange(sc_plot, tg_plot, tgge_plot,ec_plot,gi_plot, labels = c("A","B","C","D","E"))
dev.off()

interact_plot(Intent_full, pred = Empathic_Concern_c, modx = Condition, interval = TRUE)
interact_plot(Intent_full, pred = Ethnicity_Fit_c, modx = Condition, interval = TRUE)

#Supplement 4.4.9 Simple slopes. 
dat$Condition_d <- as.numeric(dat$Condition)

Intent_full_d <- lmerTest::lmer(Support_Intention ~ Condition_d + Target_Gender + Social_Context + Ethnicity_Fit_c + Empathic_Concern_c + 
                                Gender_c + Condition_d*Target_Gender + Condition_d*Social_Context + Condition_d*Gender_c + Condition_d*Ethnicity_Fit_c + 
                                Condition_d*Empathic_Concern_c + Target_Gender*Gender_c +
                                (1|Country/ID) + (1|Ethnicity/StimNum), control=lmerControl("Nelder_Mead"), dat=dat)

interact_plot(Intent_full_d, pred = Condition_d, modx = Empathic_Concern_c, modx.values = c(-2.5,-2,-1,0,1))
interact_plot(Intent_full_d, pred = Condition_d, modx = Ethnicity_Fit_c, modx.values = c(-2,-1,0,1,2,3))

sim_slopes(Intent_full_d, pred = Condition_d, modx = Empathic_Concern_c, modx.values = c(-2.5,-2,-1,0,1))
sim_slopes(Intent_full_d, pred = Condition_d, modx = Ethnicity_Fit_c, modx.values = c(-2,-1,0,1,2,3))


##########################
#STEP 6.
#EXPLORATORY ANALYSES.
##########################

#6.1 GET CULTURAL INDICES.

#Split dataset into two halves TRAIN AND TEST.
set.seed(420) #set seed for reproducibility.

# Split Data into Training and Testing in R 
sample_size = floor(0.5*nrow(dat))

# randomly split data in r
picked <- sample(seq_len(nrow(dat)),size = sample_size)
train <- dat[picked,] #train dataset
test <- dat[-picked,] #test dataset.

#save datasets.
write.csv(train, "Train_Dat.csv")
write.csv(test, "Test_Dat.csv")

#########

#6.2 Train Dataset - Add Cultural Indices.

##Calculate effect sizes for train dataset.
intent_train <- train %>% 
  group_by(Lab_ID, Condition) %>% 
  summarise(mean = mean(Support_Intention, na.rm=TRUE), sd = sd(Support_Intention, na.rm=TRUE), n = n())
#restructure to wide.
intent_train <- dcast(melt(intent_train, id.vars=c("Lab_ID","Condition")), Lab_ID~variable+Condition)
#calculate effect size.
intent_train <- escalc(measure="SMD", m1i=mean_Tears, sd1i=sd_Tears, n1i=n_Tears,
                   m2i=`mean_No Tears`, sd2i=`sd_No Tears`, n2i=`n_No Tears`,
                   data=intent_train)

#Import Datasheet with Cultural Indices.
#if not working download manually from https://osf.io/t6kj5/
culture <- getOSFfile("t6kj5")
culture <- read_xlsx(culture)
#culture <- readxl::read_xlsx("Cultural Indices.xlsx") #alternatively read by hand. 
culture$CountryName <- countrycode(culture$ISO_3_Code, origin = "iso3n", destination = "country.name") #add countryname.

#dat_merged <- merge(x = dat, y = culture, by = "CountryName", all.x=TRUE) #add cultural indices to main data.

#get lablist
lab_list <- dat %>% 
  group_by(Lab_ID, Country) %>% 
  summarise(n = n())
intent_train <- merge(x = lab_list, y = intent_train, by = "Lab_ID", all.x=TRUE)

intent_train$CountryName <- countrycode(intent_train$Country, origin = "iso3n", destination = "country.name") #add country name variable.

intent_train <- intent_train %>% 
        select(CountryName, yi, vi, Lab_ID) #keep only important variables.

intent_train <- merge(x = intent_train, y = culture, by = "CountryName", all.x=TRUE) #merge with cultural indices.

#convert character variables to numeric.
intent_train$Pop <- as.numeric(intent_train$Pop)
intent_train$HDI <- as.numeric(intent_train$HDI)
intent_train$GDP <- as.numeric(intent_train$GDP)

intent_train <- intent_train %>% 
  select(-Country, -CountryName, -ISO_3_Code) #keep only important variables.

####

#6.3 Original Registered Model.


model <- rma(yi = yi, 
              vi = vi, 
              data = intent_train, 
              method = "REML", 
              mods = ~ HHoM + Pop + UP2018 + RelationalMobility + LifeExpectancy + EmploymentAgriculture + Education + 
                GII + HDI + SWB + SocialSupport + PathogenPrevalence + ClimateDemandingness + GINI + GDP + MPI + Religiosity + CTL + 
               Individualism + Masculinity + LongTermOrientation + UncertaintyAvoidance + Indulgence +
               Extraversion + Agreeableness + Conscientiousness + Neuroticism + Openness + HumaneOrientation...31 + InGroupCollectivism +
               PowerDistance + HumaneOrientation...34 + Trust)
##too many variables not possible to estimate, use metaforest approach (6.4).
##DOES NOT WORK. 

#########################

####6.4 METAFOREST MODEL.


#6.4.1 Check for outliers in data. 
outliers_mad(intent_train$yi) #check for outliers.
#ARE detected as outlier - remove.

intent_train <- intent_train %>% 
  filter(Lab_ID != "ARE001") #exclude ARE effect size. 

##6.4.2 METAFOREST. (based on: https://cjvanlissa.github.io/metaforest/articles/Introduction_to_metaforest.html)

# Run model with many trees to check convergence
check_conv <- MetaForest(yi~HHoM+Pop+UP2018+LifeExpectancy+EmploymentAgriculture+Education+HDI+SWB+SocialSupport+ClimateDemandingness+GDP+Religiosity+Individualism+Masculinity+UncertaintyAvoidance+LongTermOrientation
                         +Extraversion+Agreeableness+Conscientiousness+Neuroticism+Openness,
                         data = intent_train,
                         study = "Lab_ID",
                         whichweights = "random",
                         num.trees = 20000)
# Plot convergence trajectory
plot(check_conv) #converges at about 5000 trees. 

# Model with 5000 trees for replication.
mf_rep <- MetaForest(yi~HHoM+Pop+UP2018+LifeExpectancy+EmploymentAgriculture+Education+HDI+SWB+SocialSupport+ClimateDemandingness+GDP+Religiosity+Individualism+Masculinity+UncertaintyAvoidance+LongTermOrientation+
                       +Extraversion+Agreeableness+Conscientiousness+Neuroticism+Openness,
                     data = intent_train,
                     study = "Lab_ID",
                     whichweights = "random",
                     num.trees = 5000)

# Run recursive preselection, store results in object 'preselect'
preselected <- preselect(mf_rep,
                         replications = 100,
                         algorithm = "recursive")
# Plot the results
png("Figure5.png", width = 6000, height = 6000, res=600) #-> Supplementary Figure 4.4.8
plot(preselected)
dev.off()
# Retain only moderators with positive variable importance in more than
# 50% of replications
retain_mods <- preselect_vars(preselected, cutoff = .5)


##6.4.2.1 TUNE PARAMETERS.

# Set up 10-fold grouped (=clustered) CV
grouped_cv <- trainControl(method = "cv",
                           index = groupKFold(intent_train$Lab_ID, k = 10))

# Set up a tuning grid for the three tuning parameters of MetaForest
tuning_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                           mtry = 2:6,
                           min.node.size = 2:6)

# X should contain only retained moderators, clustering variable, and vi
X <- intent_train[, c("Lab_ID", "vi", retain_mods)]

#6.4.2.2 Train the model
mf_cv <- train(y = intent_train$yi,
               x = X,
               study = "Lab_ID", # Name of the clustering variable
               method = ModelInfo_mf(),
               trControl = grouped_cv,
               tuneGrid = tuning_grid,
               num.trees = 5000)
# Examine optimal tuning parameters
mf_cv$results[which.min(mf_cv$results$RMSE), ]


# For convenience, extract final model
final <- mf_cv$finalModel
# Extract R^2_{oob} from the final model
r2_oob <- final$forest$r.squared
# Plot convergence
plot(final)

png("Figure5.1.png", width = 6000, height = 6000, res=600) #-> Supplementary Material 4.4.8
VarImpPlot(final)
dev.off()

#Partial Dependence Plot
# Sort the variable names by importance, so that the
# partial dependence plots will be ranked by importance
ordered_vars <- names(final$forest$variable.importance)[
  order(final$forest$variable.importance, decreasing = TRUE)]
# Plot partial dependence
png("Figure5.2.png", width = 6000, height = 6000, res=600) #-> Supplementary Material 4.4.8
PartialDependence(final, vars = ordered_vars,
                  rawdata = TRUE, pi = .95)
dev.off()

#######################
#6.5 TEST DATASET#


##Calculate effect sizes for test dataset.
intent_test <- test %>% 
  group_by(Lab_ID, Condition) %>% 
  summarise(mean = mean(Support_Intention, na.rm=TRUE), sd = sd(Support_Intention, na.rm=TRUE), n = n())
#restructure to wide.
intent_test <- dcast(melt(intent_test, id.vars=c("Lab_ID","Condition")), Lab_ID~variable+Condition)
#calculate effect size.
intent_test <- escalc(measure="SMD", m1i=mean_Tears, sd1i=sd_Tears, n1i=n_Tears,
                       m2i=`mean_No Tears`, sd2i=`sd_No Tears`, n2i=`n_No Tears`,
                       data=intent_test)

intent_test <- merge(x = lab_list, y = intent_test, by = "Lab_ID", all.x=TRUE)

intent_test$CountryName <- countrycode(intent_test$Country, origin = "iso3n", destination = "country.name") #add country name variable.

intent_test <- intent_test %>% 
  select(CountryName, yi, vi, Lab_ID) #keep only important variables.

intent_test <- merge(x = intent_test, y = culture, by = "CountryName", all.x=TRUE) #merge with cultural indices.

intent_test <- intent_test %>% 
  filter(Lab_ID != "ARE001") #exclude ARE effect size. 

#convert character variables to numeric.
intent_test$Pop <- as.numeric(intent_test$Pop)
intent_test$HDI <- as.numeric(intent_test$HDI)
intent_test$GDP <- as.numeric(intent_test$GDP)

intent_test <- intent_test %>% 
  select(yi, vi, HDI, SocialSupport, GDP, Extraversion, SWB, UncertaintyAvoidance, Masculinity,
         Neuroticism, Religiosity, Education, Individualism, Conscientiousness, Pop,
         Agreeableness, EmploymentAgriculture, LongTermOrientation, HHoM,
         UP2018, LifeExpectancy, ClimateDemandingness) #keep only important variables.

#standardize variables to ease comparison. 
intent_test <- intent_test %>% mutate_at(c("HDI", "SocialSupport", "GDP", "Extraversion", "SWB", "UncertaintyAvoidance", "Masculinity",
                            "Neuroticism", "Religiosity", "Education", "Individualism", "Conscientiousness", "Pop", "Agreeableness",
                            "EmploymentAgriculture"), ~(scale(.) %>% as.vector)) #standardize variables

#6.5.1 Run MetaRegression Models. -> Table 8.

#model with HDI
model_hdi <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ HDI)
model_hdi #model summary.

#model with social support
model_ss <- rma(yi = yi, 
                vi = vi, 
                data = intent_test, 
                method = "REML", 
                mods = ~ SocialSupport)
model_ss

#model with gdp.
model_gdp <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ GDP)
model_gdp

#model with extraversion.
model_ext <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Extraversion)
model_ext


#model with subjective well being.
model_swb <- rma(yi = yi, 
              vi = vi, 
              data = intent_test, 
              method = "REML", 
              mods = ~ SWB)
model_swb

#model with uncertainty avoidance.
model_uca <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                mods = ~ UncertaintyAvoidance)
model_uca

#model with masculinty.
model_male <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Masculinity)
model_male

#model with neuroticism.
model_neuro <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Neuroticism)
model_neuro

#model with religiosity.
model_rel <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Religiosity)
model_rel

#model with education.
model_edu <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Education)
model_edu

#model with individualism.
model_ind <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Individualism)
model_ind


#model with conscientiousness.
model_con <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Conscientiousness)
model_con

#model with population density.
model_pop <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Pop)
model_pop

#model with agreeableness.
model_agr <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ Agreeableness)
model_agr

#model with employment in agriculture.
model_agri <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ EmploymentAgriculture)
model_agri

#model with urban population. 
model_up <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ UP2018)
model_up

#model with historical heterogeneity of migration. 
model_hhom <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ HHoM)
model_hhom

#model with climate demandingness. 
model_cli <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ ClimateDemandingness)
model_cli

#model with life expectancy at birth. 
model_life <- rma(yi = yi, 
                 vi = vi, 
                 data = intent_test, 
                 method = "REML", 
                 mods = ~ LifeExpectancy)
model_life

#############################################
#FIN :) 
 
