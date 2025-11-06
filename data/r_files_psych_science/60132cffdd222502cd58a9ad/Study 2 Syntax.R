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
        rename(EC_1 = ï..EC_1)

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

#check total number of participants.
dat %>% 
  group_by(Lab_ID, MTurkCode) %>% 
  summarise(n = n())
#total of 7745 participants.

##filter nation filter for Supplementary Material Analyses 4.5.
dat_n <- dat %>% 
  filter(filter_nation==1)

#now FILTER out excluded cases. MAIN EXCLUSION FILTER.
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

table(dat$ID)

######################################
#STEP 4. 
#Additional Cleaning and Replication Analyses.

######################################

#we preregistered analyses using each of the 8 individual trait ratings
#rescaling appears to be linear, so using SCM_1 through SCM_8 for tests
#rename these variables for easier interpretation

names(dat)[19:26] <- c("Competent_test", "Capable_test", "Warm_test", "Friendly_test", "Honest_test", "Reliable_test", "Dominant_test", "Attractive_test")

#calculate Region

#AFRICA	Kenya, Nigeria, South Africa
#EAST AND SOUTHEAST ASIA	China, India, Malaysia, Taiwan, Thailand
#AUSTRALIA AND NEW ZEALAND	Australia, New Zealand
#CENTRAL AMERICA AND MEXICO	El Salvador, Mexico
#EASTERN EUROPE	Hungary, Lithuania, Poland, Russia, Serbia, Slovakia
#MIDDLE EAST	Iran, Israel, Turkey
# AND CANADA	Canada, USA
#SCANDINAVIA	Denmark, Finland, Norway, Sweden
#SOUTH AMERICA	Argentina, Brazil, Chile, Colombia, Ecuador


dat$Region <- ifelse(dat$CountryName %in% c("Argentina", "Brazil", "Chile", "Peru"), "South America",
                     ifelse(dat$CountryName %in% c("Australia", "New Zealand"), "Australia & New Zealand",
                            ifelse(dat$CountryName %in% c("Bosnia & Herzegovina", "Croatia", "Hungary", "Poland", "Serbia", "Slovakia"), "Eastern Europe",
              ifelse(dat$CountryName %in% c("Canada", "United States"), "USA and Canada",
                     ifelse(dat$CountryName %in% c("China", "India", "Japan", "Malaysia", "Philippines", "Singapore", "South Korea", "Thailand"), "South and Southeast Asia",
                            ifelse(dat$CountryName %in% c("Colombia", "Mexico"), "Central America and Mexico",
              ifelse(dat$CountryName %in% c("Finland", "Norway"), "Scandinavia",
                     ifelse(dat$CountryName %in% c("Austria", "France", "Germany", "Greece", "Netherlands", "Portugal", "Spain"), "Western Europe",
                            ifelse(dat$CountryName %in% c("Ireland", "United Kingdom"), "United Kingdom",
              ifelse(dat$CountryName %in% c("Israel", "Pakistan", "Turkey", "United Arab Emirates"), "Middle East",
                     ifelse(dat$CountryName %in% c("Nigeria", "South Africa"), "Africa", NA)))))))))))

#check
table(dat$Region)

#check N per region
dat_region_sum <- dat %>% 
  group_by(ID) %>%
  filter(row_number()==1)


dat_region_sum %>% 
  group_by(Region) %>%
  summarise(n = n(), .groups="keep")

#fit main models
##NOTE: We use "ID" rather than "MTurkCode", which was specified in the preregistration. This change was made at the recommendation
#of the original study authors, who noted that "MTurkCode" is only unique within lab, not across labs as intended

competent_big <- lmer(Competent_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
capable_big <- lmer(Capable_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
warm_big <- lmer(Warm_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
friendly_big <- lmer(Friendly_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
honest_big <- lmer(Honest_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
reliable_big <- lmer(Reliable_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
dominant_big <- lmer(Dominant_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)
attractive_big <- lmer(Attractive_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)

options(scipen=99)

summary(competent_big)
summary(capable_big)
summary(warm_big)
summary(friendly_big)
summary(honest_big)
summary(reliable_big)
summary(dominant_big)
summary(attractive_big)

###tables

dir.create('tables')

library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(competent_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/competent.html")

tab_model(capable_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/capable.html")

tab_model(warm_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/warm.html")

tab_model(friendly_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/friendly.html")

tab_model(honest_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/honest.html")

tab_model(reliable_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/reliable.html")

tab_model(dominant_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/dominant.html")

tab_model(attractive_big, show.p = FALSE, show.se = TRUE, string.est = "Est.",
          string.se = "SE", ci.hyphen = ",&nbsp;", digits=3, p.style="numeric",
          CSS = list(css.centeralign = 'text-align: left'), file="tables/attractive.html")

#checking convergence issues for reliable and warm with brms
#CODE BELOW PRODUCED ERROR FOR ME

#library(brms)
#lmer <- function(...){brm(...,cores=4,iter=2000)}

#honest_brms <- lmer(Honest_test ~ 1 + (1|ID) + (1|StimNum) + (1|CountryName) + (1|Region) + (1|CountryName:StimNum) + (1|Region:StimNum), data=dat)

#rm(lmer)

#


###graph results
#NOTE: need to bootstrap CIs

library(readr)
library(readxl)
library(stats)
library(forcats)
library(patchwork)
library(viridis)

iccs <- read_xlsx("All ICCS, no CIs.xlsx", sheet=1)
iccs_l <- pivot_longer(iccs, cols=c(ends_with("_icc")))#, ends_with("_l"), ends_with("_u")))
#write_csv(ICCs_l, "ICCs_l.csv") 
#has to be edited some manually in Excel to rearrange

#iccs_l <- read_csv("ICCs_l.csv")

#iccs_l$icc_l_orig <- iccs_l$icc_l

#iccs_l$icc_l <- ifelse(iccs_l$icc_l < 0, 0, iccs_l$icc_l_orig)
#summary(iccs_l$icc_l)

names(iccs_l)[6:7] <- c("icc_type", "icc")

iccs_l$icc_type <- fct_recode(as.factor(iccs_l$icc_type), Perceiver = "p_icc",
                              Target = "t_icc",
                              Country = "c_icc", `Country by Target` = "ct_icc",
                              Region = "r_icc", `Region by Target` = "rt_icc")
iccs_l$icc_type <- fct_inorder(iccs_l$icc_type, ordered=TRUE)




dir.create("figures")

###FIGURE 2

dpi=300   #pixels per square inch
tiff("figures/iccs_viridis_rep.tif", width=9*dpi, height=11*dpi, res=dpi, compression="jpeg")

ggplot(data = iccs_l, mapping = aes(x = icc_type, y = icc, fill=trait)) +
  geom_col() +
  #geom_errorbar(aes(ymin=icc_l, ymax=icc_u),width=0,cex=1.5) + 
  theme_minimal() +
  facet_wrap(~trait, ncol=2) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE)

dev.off()

###BOOTSTRAP CIs###


# Bootstrapping Confidence Intervals around ICCs [osf.io/anwx2/]
# Updated 17/01/2020 for lme4 version 1.1-21 and boot version 1.3-24
# updated 12/02/2020 to be able to accommodate any number of clusters and their interactions

# library
library(boot)     # bootstrapping
library(psych)    # descriptives
library(lme4)     # fitting mixed-effects models
library(lmeresampler) # resampling mixed-effects models
library(tidyr)    # data hygiene

## uncomment to install the lmeresampler package from github
## if(!require(devtools)) install.packages("devtools",dependencies=TRUE, repos='http://cran.rstudio.com/')
## devtools::install_github("aloy/lmeresampler")

# import sample dataset 

#df <- df[df$WhatWasRating=="agg",] # look at ratings of aggressiveness

# This script will estimate intraclass correlation coefficients (ICC) for each cluster in a model with k clusters. 
# It can handle models that are cross-classified, as well as interactions between clusters.
# All ICC formulas based on Raudenbush & Bryk (2002).

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Step 1. Fit your lmer model ####

# In this example the DV is cross-classified across Participants (ParticipantID) and Targets (StimName).
# DV = Rating
# cluster 1 variable     = ParticipantID
# cluster 2 variable     = StimName
# interaction component  = ParticipantID:StimName

# model <- lmer(Rating ~ 1 + (1 | ParticipantID), data=df)                  # sample model with 1 cluster
# model <- lmer(Rating ~ 1 + (1 | ParticipantID) + (1 | StimName), data=df) # sample model with 2 clusters (but no interaction)
# model <- lmer(Rating ~ 1 + (1 | ParticipantID) + (1 | StimName) + (1 | ParticipantID:StimName), data=df) # sample model with 2 clusters & an interaction

# For this demonstration, we will build a model with 2 clusters (but no interaction)
# model <- lmer(Rating ~ 1 + (1 | ParticipantID) + (1 | StimName), data=df)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Step 2. Load the bootICC function (requires lmeresampler package)

bootstrapICC <- function(model,             # Lmer model object
                         iterations = 5000, # Number of iterations or bootstrap resamples
                         boot.type,         # A character string indicating the type of bootstrap that is being requested,
                         ## e.g., "parametric", "residual", "case", "cgr", "reb" (see ?lmeresampler::bootstrap for more info)
                         boot.ci.type,      # Enter a character string indicating the type of interval that is being requested,
                         ## e.g., "norm", "basic", "stud", "perc", "bca" (see ?boot.ci for more info)
                         ...){             
  m <- model
  ranefs = names(getME(m,"cnms")) # get cluster names
  ranef_k = length(ranefs)   # number of clusters
  require(lmeresampler)
  print(paste("Starting lmer resampling at",iterations,"iterations using",boot.type,"bootstrap and requesting",boot.ci.type,"intervals",sep=" "))
  
  mysumm   <- function(m){  #:::::::::::::# function returning the outputs/statistics of interest
    sig    <- getME(m, "sigma")           # sigma
    R_ij   <- sig^2; names(R_ij) <- "Residual" 
    taus   <- ((getME(m,"theta")*sig)^2)  # variance components for k clusters
    names(taus) <- gsub("\\s*\\.\\([^\\)]+\\)\\s*$","",names(taus)) # clean up column names
    taus   <- taus[c(ranefs)]             # re-order according to original cluster names
    vars   <- c(taus,R_ij)
    totvar <- sum(vars)                   # Total Variance = Sum of all variance components including residual variance (i.e., sigma^2)
    ICCs   <- sapply(c(vars),
                     function(x){x/totvar},   # Calculate ICC for each cluster, including the residual ICC
                     simplify=T, USE.NAMES=T) # (Raudenbush & Bryk, 2002)
    names(sig) <- "(Sigma)"
    names(vars)<- paste0(names(vars), " Variance")
    names(ICCs)<- paste0(names(ICCs), " ICC")
    c(sig,vars,ICCs)
  }
  # store summary object to call later on
  pointsummary = mysumm(m)
  
  # bootstrap your lmer object
  bootie <- lmeresampler::bootstrap(model = m, fn=mysumm, type=boot.type, B=iterations)
  
  # store bootstrapped samples in dataframe
  dfbootie <- as.data.frame(bootie[['t']])
  names(dfbootie) <- names(pointsummary)
  
  # what to extract from boot.ci (don't touch this unless boot.ci got updated and broke things)
  boot.ci.index <- as.numeric(c(1:(2*ranef_k+3)))
  
  # store 95% CIs
  CIs     <- lapply(boot.ci.index, function(i){boot.ci(bootie, index=i, type=boot.ci.type)})
  CIs.out <- sapply(CIs, function(j){tail(as.numeric(j[[4]]),2)}) 
  CIs.out <- as.data.frame(t(CIs.out))
  names(CIs.out) <- c("l-95% CI", "u-95% CI")
  row.names(CIs.out) <- c(names(pointsummary[1:length(pointsummary)]))
  pointsummary <- as.data.frame(pointsummary); names(pointsummary) = "Estimate"
  CIs.out <- cbind(CIs.out, pointsummary)
  
  # split variances and ICCs into diff columns
  CIs.ICC <- CIs.out[grepl("ICC", rownames(CIs.out))==TRUE,]
  CIs.var <- CIs.out[grepl("ICC", rownames(CIs.out))==FALSE,]
  
  # print CI output
  print(CIs.var,digits=5); print(CIs.ICC,digits=5)
  
  # return list containing boot object, boot.ci output, and the table of 95% CIs with their Point Estimates
  merp <- list(bootie, dfbootie, CIs, CIs.var, CIs.ICC)
  names(merp) <- c("boot_object", "df_bootsamples", "bootCI_object", "CI_table_Variance", "CI_table_ICC")
  print("Done! Have a great day.")
  return(merp)
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::#

# Step 3. Adjust parameters below and run the bootstrapICC function

# model         = Lmer model object
# iterations    = Number of iterations or bootstrap resamples (default:5000)
# boot.type     = A character string indicating the type of bootstrap that is being requested (e.g., "parametric", "residual", "case", "cgr", "reb")
#                 (see ?lmeresampler::bootstrap for more info)
# boot.ci.type  = A character string indicating the type of interval that is being requested (e.g., "norm", "basic", "stud", "perc", "bca")
#                 (see ?boot.ci for more info)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!dir.exists("bootstrap iccs")) { dir.create("bootstrap iccs") }

att_booty <- bootstrapICC(attractive_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(att_booty, file="att_booty.Rda")
cap_booty <- bootstrapICC(capable_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(cap_booty, file="cap_booty.Rda")
com_booty <- bootstrapICC(competent_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(com_booty, file="com_booty.Rda")
dom_booty <- bootstrapICC(dominant_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(dom_booty, file="dom_booty.Rda")
fri_booty <- bootstrapICC(friendly_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(fri_booty, file="fri_booty.Rda")
hon_booty <- bootstrapICC(honest_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(hon_booty, file="hon_booty.Rda")
rel_booty <- bootstrapICC(reliable_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(rel_booty, file="rel_booty.Rda")
war_booty <- bootstrapICC(warm_big, iterations=100, boot.type="parametric", boot.ci.type="norm"); save(war_booty, file="war_booty.Rda")

# The output is a list containing:
booty[["boot_object"]]       # the original boot object from lmeresampler::bootstrap()
booty[["df_bootsamples"]]    # a dataframe containing bootstrapped samples
booty[["bootCI_object"]]     # the boot.ci object
booty[["CI_table_Variance"]] # a dataframe of 95% Interval Estimates of Variance Components 
booty[["CI_table_ICC"]]      # a dataframe of 95% Interval Estimates of intraclass correlation coefficients



#if needed, load _booty objects into the environment for use

library(tidyverse)
library(viridis)
library(colorspace)

load("att_booty.rda")
load("cap_booty.rda")
load("com_booty.rda")
load("dom_booty.rda")
load("fri_booty.rda")
load("hon_booty.rda")
load("rel_booty.rda")
load("war_booty.rda")

#look at values

att_booty$CI_table_ICC
cap_booty$CI_table_ICC
com_booty$CI_table_ICC
dom_booty$CI_table_ICC
fri_booty$CI_table_ICC
hon_booty$CI_table_ICC
rel_booty$CI_table_ICC
war_booty$CI_table_ICC

#add type and trait, then rbind

att_booty$CI_table_ICC$trait <- "Attractive"
cap_booty$CI_table_ICC$trait <- "Capable"
com_booty$CI_table_ICC$trait <- "Competent"
dom_booty$CI_table_ICC$trait <- "Dominant"
fri_booty$CI_table_ICC$trait <- "Friendly"
hon_booty$CI_table_ICC$trait <- "Honest"
rel_booty$CI_table_ICC$trait <- "Reliable"
war_booty$CI_table_ICC$trait <- "Warm"

icc_l <- rbind(att_booty$CI_table_ICC, cap_booty$CI_table_ICC, com_booty$CI_table_ICC, dom_booty$CI_table_ICC,
               fri_booty$CI_table_ICC, hon_booty$CI_table_ICC, rel_booty$CI_table_ICC, war_booty$CI_table_ICC)

#add icc type to the merged file
for (i in 1:8){
  icc_l$icc_type[i*7-6] <- 'Perceiver'
  icc_l$icc_type[i*7-5] <- 'Country by Target'
  icc_l$icc_type[i*7-4] <- 'Region by Target'
  icc_l$icc_type[i*7-3] <- 'Target'
  icc_l$icc_type[i*7-2] <- 'Country'
  icc_l$icc_type[i*7-1] <- 'Region'
  icc_l$icc_type[i*7] <- 'Error'
}

icc_l$icc_type <- as.factor(icc_l$icc_type)
names(icc_l)[1:3] <- c('icc_l', 'icc_u', 'icc')

#order icc types

icc_l$icc_type <- fct_relevel(icc_l$icc_type, 'Perceiver', 'Target', 'Country', 'Country by Target', 'Region', 'Region by Target')

#figure

dpi=300   #pixels per square inch
tiff("figures/iccs_viridis_rep.tif", width=9*dpi, height=9.5*dpi, res=dpi)

ggplot(data = subset(icc_l, icc_type!="Error"), mapping = aes(x = icc_type, y = icc, fill=trait)) +
  geom_col() +
  geom_errorbar(aes(ymin=icc_l, ymax=icc_u),width=0,cex=1.5) + 
  theme_minimal() +
  facet_wrap(~trait, ncol=2) +
  xlab("ICC Type") + ylab ("Proportion of Variance Explained") +
  theme(legend.position="none",
        strip.text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text.x = element_text(size=12, angle=90, hjust=1),
        axis.text.y = element_text(size=14)) +
  scale_fill_viridis(option="viridis", discrete=TRUE)

dev.off()

#aggregate statistics

icc_l %>% 
  group_by(icc_type) %>%
  summarise(mean(icc), min(icc), max(icc), .groups="keep")

#save aggregate file as .csv for supplemental materials

write_csv(icc_l, "ICCs with CIs, Study 2.csv")

#summary statistics for reporting

#4.3 Check distribution by gender. 
dat %>%
  group_by(Lab_ID, MTurkCode) %>%
  slice(which.min(Gender)) %>%
  ungroup(MTurkCode) %>% 
  group_by(Gender) %>% 
  summarise(n=n())
#in total 1975 Males, 4474 Females 45 other

#summarise age. 
dat %>% 
  summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), min = min(Age, na.rm=TRUE), max = max(Age, na.rm=TRUE))
#mean age 28.0845, sd = 10.886 min 18 max 79