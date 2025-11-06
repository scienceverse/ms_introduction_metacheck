---
title: "Everyday Empathy clean and format"
author: "Greg"
date: "June 26th, 2019"
output: html_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)
library(hausekeep)
library(stats)
library(lubridate)
library(interactions)
require(GGally)
require(reshape2)
require(compiler)
require(parallel)
require(boot)
require(lattice)
require(sjPlot)



#if needed:
#install.packages("devtools")
#devtools::install_github("hauselin/hausekeep")

```
  
## R Markdown

This is a script for cleaning and formatting data for the everyday empathy project

```{r Read in data, include = FALSE}

#Due to the design, data came from multiple sources and had to be read in and matched to participant ID
Baseline <- fread("Baseline w1 to 19 (no duplicates, only ptcpts that completed ES).csv")
Descriptive <- fread("ES w1-W19 descriptive.csv") 
ExpSamp <- fread("ES w1 to w19 (includes desc data with qual answers removed).csv")
Ptcpts_All <- fread("ES Ptcpts w 1 to 19 no duplicates (pretrimmed).csv")

Ptcpts_All = Ptcpts_All[ , c(1,2, 4)]
colnames(Ptcpts_All) <- c("email", "User_Id", "SurveysAnswered")
Ptcpts <- subset(Ptcpts_All, Ptcpts_All$SurveysAnswered >=7)
# We preregistered removing particiapnts who answered less than 7 surveys

rm(Ptcpts_All)

Demo_trim <- fread("Demo survey w1 to w19 completes .csv")
Demo_FULL <- fread("Demo Survey N = 841 (complete).csv")

EPost <-  fread("ES w1 to w19 post emp.csv", header = TRUE)
```

```{r Format raw data}



#Changes metricwire formatting for when people aren't displayed the question or don't anawer a questions
ExpSamp[ExpSamp == "CONDITION_SKIPPED"] <- NA
ExpSamp[ExpSamp == "NO_ANSWER"] <- NA

#Makes all emails in lowercase so they match, remove spaces, remove unneccesary columns, rename columns
Baseline$email <- tolower(as.character(Baseline$email))
Baseline$email <- gsub(" ", "", Baseline$email)
 
ExpSamp$email <- tolower(as.character(ExpSamp$email))

Demo_trim$email <- tolower(as.character(Demo_trim$email))


```

```{r Convert columns to numeric and transform}

#This is some code written by Hause to convert columns to numeric using data table syntax

#first, take out spaces in names in Exp samp data
names(ExpSamp) <- gsub(pattern = " ", replacement = "_", x = names(ExpSamp))
names(ExpSamp) <- gsub(pattern = "[.]", replacement = "_", x = names(ExpSamp))
glimpse(ExpSamp)


names(Demo_trim) <- gsub(pattern = "[.]", replacement = "_", x = names(Demo_trim))

#Change duplicate column name
names(ExpSamp)[which(duplicated(names(ExpSamp)))]

names(ExpSamp)[which(duplicated(names(ExpSamp)))] <- 'other_emo_self'


#Remove my test data from EXpSamp dataframe

ExpSamp = ExpSamp[User_Id != "5c7dee175bdb231d42b622b1"]

#Also need to change the duplicate email column in the baseline survey (moved the metricwire instructions earlier now they both say email)




#Convert EPost column names

names(EPost) <- gsub(pattern = "[.]", replacement = "_", x = names(EPost))
names(EPost) <- gsub(pattern = "[(-)]", replacement = "_neg", x = names(EPost))
names(EPost) <- gsub(pattern = "-_neg", replacement = "", x = names(EPost))
names(EPost) <- gsub(pattern = " ", replacement = "_", x = names(EPost))





cols2Convert <- names(ExpSamp)[c(13:14, 17, 20:22, 24:25, 29:31, 33:35, 37:39, 41, 43, 45, 47:50, 51, 54, 55, 66:69)] # select which scale cols to convert to numeric
names(ExpSamp)[c(13:14, 17, 20:22, 24:25, 29:31, 33:35, 37:39, 41, 43, 45, 47:50, 51, 54, 55, 66:69)]
                               
# old string using numeric column indexing for cols2convert that became obsolete: 17, 18, 24:26, 28:29, 33:35, 37:39, 41:43, 45, 47, 49, 51:55, 58, 59, 69:73
cols2Convert
ExpSamp[,cols2Convert, with = FALSE]
ExpSamp[, (cols2Convert) := lapply(X = .SD, FUN = function(x) as.numeric(x)), .SDcols = cols2Convert] 
ExpSamp[, cols2Convert, with = FALSE]

#here we are applying function x which we define as adding 1 to each of the columns we want to change. 
# We do this because responses to our 1-7 scale are coded as 0-6 in the app
ExpSamp[, (cols2Convert) := lapply(X = .SD, FUN = function(x) x + 1), .SDcols = cols2Convert]

choiceCols2convert <- names(ExpSamp)[c(15, 16, 23, 28, 32, 36, 40, 42, 44, 46, 52, 58, 62:64)] #Select choice cols to convert to numeric

ExpSamp[, choiceCols2convert, with = FALSE]

ExpSamp[, (choiceCols2convert) := lapply(X = .SD, FUN = function(x) as.numeric(x)), .SDcols = choiceCols2convert]
#This next line converts our Yes's to 1 and No's to O

ExpSamp[, (choiceCols2convert) := lapply(X = .SD, FUN = function(x) 1 - x), .SDcols = choiceCols2convert]

# Convert post empathy responses to numeric and add 1 (0-6 -> 1-7)
PostScaleCols <- names(EPost)[c(17:45)]
EPost[, PostScaleCols, with = FALSE]
EPost[, (PostScaleCols) := lapply(X = .SD, FUN = function(x) as.numeric(x)), .SDcols = PostScaleCols]
EPost[, (PostScaleCols) := lapply(X = .SD, FUN = function(x) x + 1), .SDcols = PostScaleCols]

#Code below rescales the SITES which is out of 5 on the post test and out of 7 normally
SITEScol <- names(EPost)[c(45)]
EPost[, (SITEScol) := lapply(X = .SD, FUN = function(x) x - 1), .SDcols = SITEScol]
EPost[, (SITEScol) := lapply(X = .SD, FUN = function(x) x * 1.5), .SDcols = SITEScol]
EPost[, (SITEScol) := lapply(X = .SD, FUN = function(x) x + 1), .SDcols = SITEScol]

```



```{r Transform data, score scales}



####Code to score scales and subscales####  

#Once again fixing column names, this time on the baseline
names(Baseline) <- gsub(pattern = " ", replacement = "_", x = names(Baseline))
names(Baseline) <- gsub(pattern = "I-", replacement = "I_", x = names(Baseline))
names(Baseline) <- gsub(pattern = "[.]", replacement = "_", x = names(Baseline))

#Now we are using select to calculate scores on our scales and subscales

Mall_of_E_score <-  Baseline %>% select(Mall_of_E_1, Mall_of_E_2, Mall_of_E_3, Mall_of_E_4, Mall_of_E_5, Mall_of_E_6) %>% reverse.code(c(-1, -1, -1, 1, 1, 1), .) %>% psych::alpha(., check.keys = T)
Baseline$Mall_of_E_Score <- Mall_of_E_score$scores

Imp_Will_score <-  Baseline %>% select(Imp_Will_1, Imp_Will_2, Imp_Will_3, Imp_Will_4, Imp_Will_5, Imp_Will_6) %>% reverse.code(c(-1, -1, 1, 1, -1, 1), .) %>% psych::alpha(., check.keys = T)
Baseline$Imp_Will_score <- Imp_Will_score$scores

BFI_Openness <- Baseline %>% select(BFI_2a_5, BFI_2a_10, BFI_2a_15, BFI_2a_20, BFI_2a_25, BFI_2a_30, BFI_2b_5, BFI_2b_10, BFI_2b_15, BFI_2b_20, BFI_2b_25, BFI_2b_30) %>% reverse.code(c(-1, 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1), .) %>% psych::alpha(., check.keys = T)
Baseline$BFI_O_score <- BFI_Openness$scores 

BFI_Conscientousness <- Baseline %>% select(BFI_2a_3, BFI_2a_8, BFI_2a_13, BFI_2a_18, BFI_2a_23, BFI_2a_28, BFI_2b_3, BFI_2b_8, BFI_2b_13, BFI_2b_18, BFI_2b_23, BFI_2b_28) %>% reverse.code(c(-1, -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1), .) %>% psych::alpha(., check.keys = T)
Baseline$BFI_C_score <- BFI_Conscientousness$scores





BFI_Extraversion <- Baseline %>% select(BFI_2a_1, BFI_2a_6, BFI_2a_11, BFI_2a_16, BFI_2a_21, BFI_2a_26, BFI_2b_1, BFI_2b_6, BFI_2b_11, BFI_2b_16, BFI_2b_21, BFI_2b_26) %>% reverse.code(c(1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1), .) %>% psych::alpha(., check.keys = T)
Baseline$BFI_E_score <- BFI_Extraversion$scores



BFI_Agreeableness <- Baseline %>% select(BFI_2a_2, BFI_2a_7, BFI_2a_12, BFI_2a_17, BFI_2a_22, BFI_2a_27, BFI_2b_2, BFI_2b_7, BFI_2b_12, BFI_2b_17, BFI_2b_22, BFI_2b_27) %>% reverse.code(c(1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1), .) %>% psych::alpha(., check.keys = T)
Baseline$BFI_A_score <- BFI_Agreeableness$scores 

BFI_NegEmo <-  Baseline %>% select(BFI_2a_4, BFI_2a_9, BFI_2a_14, BFI_2a_19, BFI_2a_24, BFI_2a_29, BFI_2b_4, BFI_2b_9, BFI_2b_14, BFI_2b_19, BFI_2b_24, BFI_2b_29) %>% reverse.code(c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1), .) %>% psych::alpha(., check.keys = T)
Baseline$BFI_N_score <- BFI_NegEmo$scores

IRI_EC <- Baseline %>% select(IRI_2, IRI_4, IRI_9, IRI_14, IRI_18, IRI_20, IRI_22) %>% reverse.code(c(1, -1, 1, -1, -1, 1, 1) ,.) %>% psych::alpha(., check.keys = T) 
Baseline$IRI_EC_score <- IRI_EC$scores 

IRI_PT <- Baseline %>% select(IRI_3, IRI_8, IRI_11, IRI_15, IRI_21, IRI_25, IRI_28) %>% reverse.code(c(-1, 1, 1, -1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = T) 
Baseline$IRI_PT_score <- IRI_PT$scores

IRI_Fantasy <- Baseline %>% select(IRI_1, IRI_5, IRI_7, IRI_12, IRI_16, IRI_23, IRI_26) %>% reverse.code(c(1, 1, -1, -1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = T) 
Baseline$IRI_F_score <- IRI_Fantasy$scores

IRI_PD <-  Baseline %>% select(IRI_6, IRI_10, IRI_13, IRI_17, IRI_19, IRI_24, IRI_27) %>% reverse.code(c(1, 1, -1, 1, -1, 1, 1) ,.) %>% psych::alpha(., check.keys = T) 
Baseline$IRI_PD_score <- IRI_PD$scores 

IRI_Total <- Baseline %>% select(IRI_EC_score, IRI_PT_score, IRI_F_score, IRI_PD_score) %>% psych::alpha(., check.keys = T)
Baseline$IRI_Total_score <- IRI_Total$scores


EI_Emp <- Baseline %>% select(EI_1, EI_2, EI_3, EI_4, EI_5, EI_6, EI_7) %>% reverse.code(c(1, 1, 1, 1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = T) 
Baseline$EI_Emp_score <- EI_Emp$scores

EI_BC <- Baseline %>% select(EI_8, EI_9, EI_10, EI_11, EI_12, EI_13, EI_14) %>% reverse.code(c(1, 1, 1, 1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = T) 
Baseline$EI_BC_score <- EI_BC$scores

EI_Total <- Baseline %>% select(EI_1, EI_2, EI_3, EI_4, EI_5, EI_6, EI_7, EI_8, EI_9, EI_10, EI_11, EI_12, EI_13, EI_14) %>% psych::alpha(., check.keys = T)
Baseline$EI_Total_score <- EI_Total$scores


Chronotype <- Baseline %>% select(rMEQ_1, rMEQ_2, rMEQ_3, rMEQ_4, rMEQ_5) %>% psych::alpha(., check.keys = T)
Baseline$Chronotype_score <- Chronotype$scores

Baseline$ChoicesOther <-Baseline %>% dplyr::select(contains("Q1_1")) %>% rowSums(., na.rm=T)


SWB <- ExpSamp %>% select(happy, purpose) %>% psych::alpha(., check.keys = T)
ExpSamp$SWB <- SWB$scores


#note that HSI is simply computed with rowSums() as we scored it so they get 1 for correct answers 0 otherwise
Baseline$HSI_Total <- Baseline %>% select(HS1, HS2, HS3, HS4,HS5, HS6, HS7, HS8, HS9, HS10, HS11, HS12, HS13, HS14, HS15, HS16, HS17, HS18, HS19, HS20, HS21, HS22, HS23, HS24, HS25, HS26, HS27, HS28, HS29, HS30, HS31, HS32, HS33, HS34, HS35, HS36, HS37, HS38, HS39, HS40) %>% rowSums()



Post_IRI_EC <- EPost %>% select(IRI_2_EC, IRI_4_EC_neg, IRI_9_EC, IRI_14_EC_neg, IRI_18_EC_neg, IRI_20_EC, IRI_22_EC) %>% reverse.code(c(1, -1, 1, -1, -1, 1, 1) ,.) %>% psych::alpha(., check.keys = TRUE)
EPost$PostIRI_EC_score <- Post_IRI_EC$scores

Post_IRI_F <- EPost %>% select(IRI_1_FS, IRI_5_FS, IRI_7_FS_neg, IRI_12_FS_neg, IRI_16_FS, IRI_23_FS, IRI_26_FS) %>% reverse.code(c(1, 1, -1, -1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = TRUE)
EPost$PostIRI_F_score <- Post_IRI_F$scores

#Note, you didn't flag IRI_3_PT as neg but it is! reverse code is correct
Post_IRI_PT <- EPost %>% select(IRI_3_PT, IRI_8_PT, IRI_11_PT, IRI_15_PT_neg, IRI_21_PT, IRI_25_PT, IRI_28_PT) %>% reverse.code(c(-1, 1, 1, -1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = TRUE)
EPost$PostIRI_PT_score <- Post_IRI_PT$scores  

Post_EI_Emp <- EPost %>% select(EI_Emp_1, EI_Emp_2, EI_Emp_3, EI_Emp_4, EI_Emp_5, EI_Emp_6, EI_Emp_7) %>% reverse.code(c(1, 1, 1, 1, 1, 1, 1) ,.) %>% psych::alpha(., check.keys = TRUE)
EPost$PostEI_Emp_score <- Post_EI_Emp$scores  




#####Remove lists used to create scale scores from workspace to keep it tidy####

rm(BFI_Openness, BFI_Conscientousness, BFI_Extraversion, BFI_Agreeableness, BFI_NegEmo, IRI_EC, IRI_PT, IRI_Fantasy, IRI_PD, EI_Emp, EI_BC, Mall_of_E_score, Imp_Will_score, EI_Total, IRI_Total, SWB, Chronotype, Post_EI_Emp, Post_IRI_EC, Post_IRI_F, Post_IRI_PT, cols2Convert, choiceCols2convert, PostScaleCols)




```






```{r Merging dataframes}
#Now we merge Baseline and Ptcpts by User.ID, and then merge Baseline and Exp sampling by User.ID

#Merge Baseline data with ptcpt data containing Mwire User_Id


temp <- left_join(Ptcpts, Baseline, by = 'email', header = TRUE)

temp$email
#merge User_Id tagged baseline data with demo survey data

temp2 <- left_join(temp, Demo_trim, by = 'email')

#temp3 <- subset(temp2, temp2$SurveysAnswered >=7)
#temp3 not needed as I pretrimmed ptcpts with less than 7 surveys

#merge demo, baseline, User_Id's with Post empathy data

#We should also remove all identifying columns at this point

df_All <- left_join(ExpSamp, temp2, by = 'User_Id')

##############################################################

#Remove participants who have answered less than 7 surveys
# df <- subset(df_All, df_All$SurveysAnswered >=7)
df <- df_All

EPostnorep <- EPost[match(unique(EPost$User_Id), EPost$User_Id),]
df <- merge(df, EPostnorep, by = "User_Id", all.x = TRUE)


#rm(temp, temp2, EPostnorep, ExpSamp, Ptcpts, Demo_trim, df_All, EPost)

```



```{r Calculating component proportions and participant avgs for repeated measures also create dfnorep}

#To calculate participant proportion means for each combination of components we can use the for loop below

props_EP <- c()
props_EPC <- c()
props_EC <- c()
props_PC <- c()
props_E <- c()
props_P <- c()
props_C <- c()
props_0 <- c()


for (i in unique(df$User_Id)){
  prop_EP <- with(df, length(ES[ES==1 & PT == 1 & comp == 0 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_EPC <- with(df, length(ES[ES==1 & PT == 1 & comp == 1 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_EC <- with(df, length(ES[ES==1 & PT == 0 & comp == 1 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_PC <- with(df, length(ES[ES==0 & PT == 1 & comp == 1 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_E <- with(df, length(ES[ES==1 & PT == 0 & comp == 0 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_P <- with(df, length(ES[ES==0 & PT == 1 & comp == 0 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_C <- with(df, length(ES[ES==0 & PT == 0 & comp == 1 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  prop_0 <- with(df, length(ES[ES==0 & PT == 0 & comp == 0 & User_Id == i & !is.na(ES)])/ length(SurveysAnswered[User_Id ==i & E == 1 & !is.na(ES)]))
  

  
  props_EP <<- c(props_EP, prop_EP)
  props_EPC <<- c(props_EPC, prop_EPC)
  props_EC <<- c(props_EC, prop_EC)
  props_PC <<- c(props_PC, prop_PC)
  props_E <<- c(props_E, prop_E)
  props_C <<- c(props_C, prop_C)
  props_P <<- c(props_P, prop_P)
  props_0 <<- c(props_0, prop_P)
  
  
}
na.omit(props_P)%>% mean()
na.omit(props_C) %>% mean()
na.omit(props_E) %>% mean()
na.omit(props_EC) %>% mean()
na.omit(props_PC) %>% mean()
na.omit(props_EP) %>% mean()
na.omit(props_EPC) %>% mean()

na.omit(props_P)%>% sd()
na.omit(props_C) %>% sd()
na.omit(props_E) %>% sd()
na.omit(props_EC) %>% sd()
na.omit(props_PC) %>% sd()
na.omit(props_EP) %>% sd()
na.omit(props_EPC) %>% sd()

hist(props_EPC)
#This histogram, then, plots the mean participant proportion of empathy components co-occuring at one time


```

```{r Calculating means for repeated measures, adding participant centered scores to df}



#Calc means (or any statistic you want) for each participants repeated measures
#Some scores are averages on 1-7 or 1-5 likert scales, others are proportions of times yes is selected (out of one)
#Create a data frame with the means for each participant on all the listed variables


PartMeans2 <- data.frame(happymeans = as.numeric(tapply(df$happy, df$User_Id, mean, na.rm=T)), 
                         purposemeans = as.numeric(tapply(df$purpose, df$User_Id, mean, na.rm=T)),
                         SWBmeans = as.numeric(tapply(df$SWB, df$User_Id, mean, na.rm=T)),
                         bathroom_means = as.numeric(tapply(df$bathroom, df$User_Id, mean, na.rm=T)),
                         E_Oppmeans = as.numeric(tapply(df$E_Opp, df$User_Id, mean, na.rm=T)),
                         Emeans = as.numeric(tapply(df$E, df$User_Id, mean, na.rm=T)),
                         ESmeans = as.numeric(tapply(df$ES, df$User_Id, mean, na.rm=T)),
                         ES_extentmeans = as.numeric(tapply(df$ES_extent, df$User_Id, mean, na.rm=T)),
                         PTmeans = as.numeric(tapply(df$ES, df$User_Id, mean, na.rm=T)),
                         PT_extentmeans = as.numeric(tapply(df$PT_extent, df$User_Id, mean, na.rm=T)),
                         compmeans = as.numeric(tapply(df$comp, df$User_Id, mean, na.rm=T)),
                         comp_extentmeans = as.numeric(tapply(df$comp_extent, df$User_Id, mean, na.rm=T)),
                         E_how_closemeans  = as.numeric(tapply(df$E_how_close, df$User_Id, mean, na.rm=T)),
                         valencemeans = as.numeric(tapply(df$valence, df$User_Id, mean, na.rm=T)),
                         PDmeans = as.numeric(tapply(df$PD, df$User_Id, mean, na.rm=T)),
                         E_Volmeans = as.numeric(tapply(df$E_Vol, df$User_Id, mean, na.rm=T)),
                         No_E_volmeans = as.numeric(tapply(df$No_E_vol, df$User_Id, mean, na.rm=T)),
                         ES_confmeans = as.numeric(tapply(df$ES_conf, df$User_Id, mean, na.rm=T)),
                         ES_diffmeans = as.numeric(tapply(df$ES_diff, df$User_Id, mean, na.rm=T)),
                         PT_confmeans = as.numeric(tapply(df$PT_conf, df$User_Id, mean, na.rm=T)),
                         PT_diffmeans = as.numeric(tapply(df$ES_diff, df$User_Id, mean, na.rm=T)),
                         comp_confmeans = as.numeric(tapply(df$comp_conf, df$User_Id, mean, na.rm=T)),
                         comp_diffmeans = as.numeric(tapply(df$comp_diff, df$User_Id, mean, na.rm=T)),
                         T_ES_extentmeans = as.numeric(tapply(df$T_ES_extent, df$User_Id, mean, na.rm=T)),
                         T_PT_extentmeans = as.numeric(tapply(df$T_PT_extent, df$User_Id, mean, na.rm=T)),
                         T_comp_extentmeans = as.numeric(tapply(df$T_comp_extent, df$User_Id, mean, na.rm=T)),
                         connectedmeans = as.numeric(tapply(df$connected, df$User_Id, mean, na.rm=T)),
                         anxiousmeans = as.numeric(tapply(df$anxious, df$User_Id, mean, na.rm=T)),
                         contentmeans = as.numeric(tapply(df$content, df$User_Id, mean, na.rm=T)),
                         lovedmeans = as.numeric(tapply(df$loved, df$User_Id, mean, na.rm=T)),
                         rathermeans = as.numeric(tapply(df$would_you_rather, df$User_Id, mean, na.rm=T)),
                         lonelymeans = as.numeric(tapply(df$lonely, df$User_Id, mean, na.rm=T)),
                         controlmeans = as.numeric(tapply(df$control, df$User_Id, mean, na.rm=T)),
                         isolatedmeans = as.numeric(tapply(df$isolated, df$User_Id, mean, na.rm=T)),
                         hungrymeans = as.numeric(tapply(df$hungry, df$User_Id, mean, na.rm=T)),
                         painmeans = as.numeric(tapply(df$pain, df$User_Id, mean, na.rm=T)),
                         User_Id = names(tapply(df$happy, df$User_Id, mean, na.rm=T)))


#Merge with df, create new columns within df with participant centered scores. Note - Doesn't makes sense to participant center binomial variables, though we can use the means for btwn subject tests.
# We can use our PC scores for within person analysis

df <- merge(df, PartMeans2)

df <- within(df, {
  E_PC <- E - Emeans
  E_OppPC <- E_Oppmeans
  happyPC <- happy - happymeans
  purposePC <- purpose - purposemeans
  SWBPC <- SWB - SWBmeans
  ES_extentPC <- ES_extent - ES_extentmeans
  PT_extentPC <- PT_extent - PT_extentmeans
  comp_extentPC <- comp_extent - comp_extentmeans
  valencePC <- valence - valencemeans
  E_how_closePC <- E_how_close - E_how_closemeans
  PDPC <- PD - PDmeans
  E_VolPC <- E_Vol - E_Volmeans
  No_E_volPC <- No_E_vol - No_E_volmeans
  ES_confPC <- ES_conf - ES_confmeans
  ES_diffPC <- ES_diff - ES_diffmeans
  PT_confPC <- PT_conf - PT_confmeans
  PT_diffPC <- PT_diff - PT_diffmeans
  comp_confPC <- comp_conf - comp_confmeans
  comp_diffPC <- comp_diff - comp_diffmeans
  T_ES_extentPC <- T_ES_extent - T_ES_extentmeans   
  T_PT_extentPC <- T_PT_extent - T_PT_extentmeans
  T_comp_extentPC <- T_comp_extent - T_comp_extentmeans
  connectedPC <- connected - connectedmeans
  anxiousPC <- anxious - anxiousmeans
  contentPC <- content - contentmeans
  lovedPC <- loved - lovedmeans
  ratherPC <- would_you_rather - rathermeans
  lonelyPC <- lonely - lonelymeans
  controlPC <- control - controlmeans
  hungryPC <- hungry - hungrymeans
  painPC <- pain - painmeans
})


df$happyPC
unique(df$happymeans)



#rm(PartMeans2)

```


```{r Format date and time data}
#This is code written by Zoe to format our date and time variables 

#Code isn't working because our column names got messed up

names(df) <- gsub(pattern = ".x", replacement = "", x = names(df))
names(df) <- gsub(pattern = "tent", replacement = "extent", x = names(df))


df$time <- sapply(strsplit(df$Survey_Submitted_Time,":"),
                    function(x) {
                    x <- as.numeric(x)
                             x[1]+x[2]/60
                           })



df$time %>% str()

df$dayofYear <- yday(dmy(df$Survey_Submitted_Date))
str(df$dayofYear)

PartMeans <- data.frame(firstdays = as.numeric(tapply(df$dayofYear, df$User_Id, min, na.rm=T)), User_Id = names(tapply(df$dayofYear, df$User_Id, min, na.rm=T)))
df$Mall
df <- merge(df, PartMeans)
df$Survey_Day <- df$dayofYear - df$firstdays 
head(df$Survey_Day)
summary(df$Survey_Day)
table(df$Survey_Day)

df$dayofYear

#Remove any participants that completed part 2 twice
df <- subset(df, Survey_Day < 7) 
df$Survey_Day = df$Survey_Day + 1
```



```{r Centering}

# Grand mean centering all of our trait scores. 
df$Mall_of_E_Score_C <- df$Mall_of_E_Score - mean(Baseline$Mall_of_E_Score, na.rm = T)
str(df$Mall_of_E_Score_C)
hist(df$Mall_of_E_Score_C)
df$Mall_of_E_Score_Low <- df$Mall_of_E_Score_C + sd(Baseline$Mall_of_E_Score, na.rm = T)
df$Mall_of_E_Score_High <- df$Mall_of_E_Score_C - sd(Baseline$Mall_of_E_Score, na.rm = T)


df$Imp_Will_score_C <-  df$Imp_Will_score - mean(Baseline$Imp_Will_score, na.rm = T)
df$Imp_Will_score_High <-  df$Imp_Will_score_C - sd(Baseline$Imp_Will_score, na.rm = T)
df$Imp_Will_score_Low <-  df$Imp_Will_score_C + sd(Baseline$Imp_Will_score, na.rm = T)

df$Chronotype_score_C <-  df$Chronotype_score - mean(Baseline$Chronotype_score, na.rm = T)
df$Chronotype_score_High <-  df$Chronotype_score_C - sd(Baseline$Chronotype_score, na.rm = T)
df$Chronotype_score_Low <-  df$Chronotype_score_C + sd(Baseline$Chronotype_score, na.rm = T)


df$IRI_EC_score_C <- df$IRI_EC_score - mean(Baseline$IRI_EC_score, na.rm = T)
df$IRI_EC_score_High <- df$IRI_EC_score_C - sd(Baseline$IRI_EC_score, na.rm = T)
df$IRI_EC_score_Low <- df$IRI_EC_score_C + sd(Baseline$IRI_EC_score, na.rm = T)

df$IRI_F_score_C <-  df$IRI_F_score - mean(Baseline$IRI_F_score, na.rm = T)
df$IRI_F_score_High <-  df$IRI_F_score_C - sd(Baseline$IRI_F_score, na.rm = T)
df$IRI_F_score_Low <-  df$IRI_F_score_C + sd(Baseline$IRI_F_score, na.rm = T)


df$IRI_PT_score_C <-  df$IRI_PT_score - mean(Baseline$IRI_PT_score, na.rm = T)
df$IRI_PT_score_High <-  df$IRI_PT_score_C - sd(Baseline$IRI_PT_score, na.rm = T)
df$IRI_PT_score_Low <-  df$IRI_PT_score_C + sd(Baseline$IRI_PT_score, na.rm = T)

df$IRI_PD_score_C <-  df$IRI_PD_score - mean(Baseline$IRI_PD_score, na.rm = T)
df$IRI_PD_score_High <-  df$IRI_PD_score_C - sd(Baseline$IRI_PD_score, na.rm = T)
df$IRI_PD_score_Low <-  df$IRI_PD_score_C + sd(Baseline$IRI_PD_score, na.rm = T)

df$EI_Emp_score_C <-  df$EI_Emp_score - mean(Baseline$EI_Emp_score, na.rm = T)
df$EI_Emp_score_High <-  df$EI_Emp_score_C - sd(Baseline$EI_Emp_score, na.rm = T)
df$EI_Emp_score_Low <-  df$EI_Emp_score_C + sd(Baseline$EI_Emp_score, na.rm = T)


df$EI_BC_score_C <-  df$EI_BC_score - mean(Baseline$EI_BC_score, na.rm = T)
df$EI_BC_score_High <-  df$EI_BC_score_C - sd(Baseline$EI_BC_score, na.rm = T)
df$EI_BC_score_Low <-  df$EI_BC_score_C + sd(Baseline$EI_BC_score, na.rm = T)

df$BFI_E_score_C <-  df$BFI_E_score - mean(Baseline$BFI_E_score, na.rm = T)
df$BFI_E_score_High <-  df$BFI_E_score_C - sd(Baseline$BFI_E_score, na.rm = T)
df$BFI_E_score_Low <-  df$BFI_E_score_C + sd(Baseline$BFI_E_score, na.rm = T)

df$BFI_A_score_C <-  df$BFI_A_score - mean(Baseline$BFI_A_score, na.rm = T)
df$BFI_A_score_High <-  df$BFI_A_score_C - sd(Baseline$BFI_A_score, na.rm = T)
df$BFI_A_score_Low <-  df$BFI_A_score_C + sd(Baseline$BFI_A_score, na.rm = T)

df$BFI_C_score_C <-  df$BFI_C_score - mean(Baseline$BFI_C_score, na.rm = T)
df$BFI_C_score_High <-  df$BFI_C_score_C - sd(Baseline$BFI_C_score, na.rm = T)
df$BFI_C_score_Low <-  df$BFI_C_score_C + sd(Baseline$BFI_C_score, na.rm = T)

df$BFI_N_score_C <-  df$BFI_N_score - mean(Baseline$BFI_N_score, na.rm = T)
df$BFI_N_score_High <-  df$BFI_N_score_C - sd(Baseline$BFI_N_score, na.rm = T)
df$BFI_N_score_Low <-  df$BFI_N_score_C + sd(Baseline$BFI_N_score, na.rm = T)

df$BFI_O_score_C <-  df$BFI_O_score - mean(Baseline$BFI_O_score, na.rm = T)
df$BFI_O_score_High <-  df$BFI_O_score_C - sd(Baseline$BFI_O_score, na.rm = T)
df$BFI_O_score_Low <-  df$BFI_O_score_C + sd(Baseline$BFI_O_score, na.rm = T)

df$SITES_C <-  df$SITES - mean(Baseline$SITES, na.rm = T)
df$SITES_High <-  df$SITES_C - sd(Baseline$SITES, na.rm = T)
df$SITES_Low <-  df$SITES_C + sd(Baseline$SITES, na.rm = T)

df$valence_C <-  df$valence - mean(df$valence, na.rm = T)
df$valence_High <-  df$valence_C - sd(df$valence, na.rm = T)
df$valence_Low <-  df$valence_C + sd(df$valence, na.rm = T)




df$HSI_Total_C <-  df$HSI_Total - mean(Baseline$HSI_Total, na.rm = T)
df$HSI_Total_High <-  df$HSI_Total_C - sd(Baseline$HSI_Total, na.rm = T)
df$HSI_Total_Low <-  df$HSI_Total_C + sd(Baseline$HSI_Total, na.rm = T)

df$ChoicesOther_C <-  df$ChoicesOther - mean(Baseline$ChoicesOther, na.rm = T)
df$ChoicesOther_High <-  df$ChoicesOther_C - sd(Baseline$ChoicesOther, na.rm = T)
df$ChoicesOther_Low <-  df$ChoicesOther_C + sd(Baseline$ChoicesOther, na.rm = T)

df$ChoicesOther_C <-  df$ChoicesOther - mean(Baseline$ChoicesOther, na.rm = T)
df$ChoicesOther_High <-  df$ChoicesOther_C - sd(Baseline$ChoicesOther, na.rm = T)
df$ChoicesOther_Low <-  df$ChoicesOther_C + sd(Baseline$ChoicesOther, na.rm = T)




#df$SurveysAnswered_C <-  df$SurveysAnswered - mean(dfnorep$SurveysAnswered, na.rm = T)

```


```{r Extent, diff, and conf avg scores, as well as target extent avgs}


# Get average empathy difficulty, confidence, and extent ratings by averaging ratings of each in terms of ES, PT, and compassion. 

Conf_Avg <- df %>% select(ES_conf, PT_conf, comp_conf) %>% reverse.code(c(1, 1, 1),.) %>%  psych::alpha(., check.keys = TRUE)
df$ConfAvg_score <- Conf_Avg$scores

Diff_Avg <- df %>% select(ES_diff, PT_diff, comp_diff) %>% reverse.code(c(1, 1, 1),.) %>% psych::alpha(., check.keys = TRUE)
df$DiffAvg_score <- Diff_Avg$scores


Extent_Avg <- df %>% select(ES_extent, PT_extent, comp_extent) %>% reverse.code(c(1, 1, 1),.) %>% psych::alpha(., check.keys = TRUE)
df$ExtentAvg_score <- Extent_Avg$scores 


T_Emp_extent <- df %>% select(T_ES_extent, T_PT_extent, T_comp_extent) %>% reverse.code(c(1, 1, 1),.) %>% psych::alpha(., check.keys = TRUE)
df$T_extentAvg <- T_Emp_extent$scores


```

```{r get shifty (shift some columns to look at time 2)}

# shift function for time 1 time 2 analyses

# Want to know impact of empathy at time 1 on empathy at time 2
#create a copy of df with all rows, selected columns

dfcopy <- df[ , c("User_Id", "E_Opp", "E", "T_opp", "Gen_Prosocial", "SWB", "ExtentAvg_score", "comp_extent", "PD", "T_ES", "T_ES_extent", "happy", "purpose")]

dfcopy

#Now shift each of these forward one time, so that it creates a column of those variables at time 2 in each 'time 1' row
dfcopyshifted <- dfcopy[, c("E", "E_Opp", "T_opp", "Gen_Prosocial", "SWB", "ExtentAvg_score", "comp_extent", "PD", "T_ES", "T_ES_extent", "happy", "purpose") := shift(.SD, 1L), by= User_Id]

dfcopyshifted

#Now we rename those variables so we know they are at time 2

names(dfcopyshifted) <- c("User_Id", "E_time2", "E_Opp_time2", "T_opp_time2", "Gen_Prosocial_time2",  "SWB_time2", "ExtentAvg_score_time2", "comp_extent_time2", "PD_time2", "T_ES_time2", "T_ES_extent_time2", "happy_time2", "purpose_time2")

df <- cbind(df, dfcopyshifted, by = "User_Id")

rm(dfcopy, dfcopyshifted)


names(df)[which(duplicated(names(df)))] 
names(df)[which(duplicated(names(df)))] <- "User_Id2"



```



```{r more participant centered variables}

#Copying formula above Zoe demo'd, create data frame calculating means of peoples extent, confidence and difficulty ratings averaged across the 3 empathy components. 
PartMeans3 <- data.frame(ExtentAvgmeans = as.numeric(tapply(df$ExtentAvg_score, df$User_Id, mean, na.rm=T)),
                         ConfAvgmeans = as.numeric(tapply(df$ConfAvg_score, df$User_Id, mean, na.rm=T)),
                         DiffAvgmeans = as.numeric(tapply(df$DiffAvg_score, df$User_Id, mean, na.rm=T)),
                         T_extentAvgmeans = as.numeric(tapply(df$T_extentAvg, df$User_Id, mean, na.rm=T)),
                         missed_E_oppmeans = as.numeric(tapply(df$missed_E_opp, df$User_Id, mean, na.rm=T)),
                         Gen_Prosocialmeans = as.numeric(tapply(df$Gen_Prosocial, df$User_Id, mean, na.rm=T)),
                         T_oppmeans = as.numeric(tapply(df$T_opp, df$User_Id, mean, na.rm=T)),
                         User_Id = names(tapply(df$happy, df$User_Id, mean, na.rm=T)))


df <- merge(df, PartMeans3)

df <- within(df, {
  ExtentAvg_PC <- ExtentAvg_score - ExtentAvgmeans
  ConfAvg_PC <- ConfAvg_score - ConfAvgmeans
  DiffAvg_PC <- DiffAvg_score - DiffAvgmeans
  T_extentAvg_PC <- T_extentAvg - T_extentAvgmeans
})




```



```{r Calculating sums of binomial occurrences per participant}

#Calculates participant sum of yes responses to binomial questions
sumGen_Prosocial <- df[,sum(Gen_Prosocial, na.rm = T), by = User_Id] 
names(sumGen_Prosocial) <- c("User_Id", "Gen_Prosocialsum")

sumE_Opp <- df[,sum(E_Opp, na.rm = T), by = User_Id] 
names(sumE_Opp) <- c("User_Id", "E_Oppsum")

sumE <- df[,sum(E, na.rm = T), by = User_Id] 
names(sumE) <- c("User_Id", "Esum")

sumES <- df[,sum(ES, na.rm = T), by = User_Id] 
names(sumES) <- c("User_Id", "ESsum")

sumPT <- df[,sum(PT, na.rm = T), by = User_Id] 
names(sumPT) <- c("User_Id", "PTsum")

sumcomp <- df[,sum(comp, na.rm = T), by = User_Id] 
names(sumcomp) <- c("User_Id", "compsum")

sumT_opp <- df[,sum(T_opp, na.rm = T), by = User_Id] 
names(sumT_opp) <- c("User_Id", "T_oppsum")

sumMissedOpp <- df[,sum(missed_E_opp, na.rm = T), by = User_Id] 
names(sumMissedOpp) <- c("User_Id", "missed_E_oppsum")


#Attach them together then to our data set
dfsums <- left_join(sumE_Opp, sumE, by = "User_Id")
dfsums <- left_join(dfsums, sumES, by = "User_Id")
dfsums <- left_join(dfsums, sumPT, by = "User_Id")
dfsums <- left_join(dfsums, sumT_opp, by = "User_Id")
dfsums <- left_join(dfsums, sumcomp, by = "User_Id")
dfsums <- left_join(dfsums, sumGen_Prosocial, by = "User_Id")
dfsums <- left_join(dfsums, sumMissedOpp, by = "User_Id")


df <- left_join(df, dfsums, by = "User_Id")


```



```{r Grand mean center mean participant scores from repeated measures in exp sampling}


## These scores can be used for btwn ptcpt analysis, how are average differences in everyday empathy associated with different outcomes? 


df$E_Oppmeans_C <-  df$E_Oppmeans - mean(dfnorep$E_Oppmeans, na.rm = T)
df$E_Oppmeans_High <-  df$E_Oppmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$E_Oppmeans_Low <-  df$E_Oppmeans_C + sd(dfnorep$E_Oppmeans, na.rm = T)

df$Emeans_C <-  df$Emeans - mean(dfnorep$Emeans, na.rm = T)
df$Emeans_High <-  df$Emeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$Emeans_Low <-  df$Emeans_C + sd(dfnorep$Emeans, na.rm = T)

df$ESmeans_C <-  df$ESmeans - mean(dfnorep$ESmeans, na.rm = T)
df$ESmeans_High <-  df$ESmeans_C - sd(dfnorep$ESmeans, na.rm = T)
df$ESmeans_Low <-  df$ESmeans_C + sd(dfnorep$ESmeans, na.rm = T)

df$PTmeans_C <-  df$PTmeans - mean(dfnorep$PTmeans, na.rm = T)
df$PTmeans_High <-  df$PTmeans_C - sd(dfnorep$PTmeans, na.rm = T)
df$PTmeans_Low <-  df$PTmeans_C + sd(dfnorep$PTmeans, na.rm = T)

df$compmeans_C <-  df$compmeans - mean(dfnorep$compmeans, na.rm = T)
df$compmeans_High <-  df$compmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$compmeans_Low <-  df$compmeans_C + sd(dfnorep$compmeans, na.rm = T)

df$ES_extentmeans_C <-  df$ES_extentmeans - mean(dfnorep$ES_extentmeans, na.rm = T)
df$ES_extentmeans_High <-  df$ES_extentmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$ES_extentmeans_Low <-  df$ES_extentmeans_C + sd(dfnorep$ES_extentmeans, na.rm = T)

df$PT_extentmeans_C <-  df$PT_extentmeans - mean(dfnorep$PT_extentmeans, na.rm = T)
df$PT_extentmeans_High <-  df$PT_extentmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$PT_extentmeans_Low <-  df$PT_extentmeans_C + sd(dfnorep$PT_extentmeans, na.rm = T)

df$comp_extentmeans_C <-  df$comp_extentmeans - mean(dfnorep$comp_extentmeans, na.rm = T)
df$comp_extentmeans_High <-  df$comp_extentmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$comp_extentmeans_Low <-  df$comp_extentmeans_C + sd(dfnorep$comp_extentmeans, na.rm = T)

df$ExtentAvgmeans_C <-  df$ExtentAvgmeans - mean(dfnorep$ExtentAvgmeans, na.rm = T)
df$ExtentAvgmeans_High <-  df$ExtentAvgmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$ExtentAvgmeans_Low <-  df$ExtentAvgmeans_C + sd(dfnorep$ExtentAvgmeans, na.rm = T)

df$DiffAvgmeans_C <-  df$DiffAvgmeans - mean(dfnorep$DiffAvgmeans, na.rm = T)
df$DiffAvgmeans_High <-  df$DiffAvgmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$DiffAvgmeans_Low <-  df$DiffAvgmeans_C + sd(dfnorep$DiffAvgmeans, na.rm = T)

df$ConfAvgmeans_C <-  df$ConfAvgmeans - mean(dfnorep$ConfAvgmeans, na.rm = T)
df$ConfAvgmeans_High <-  df$ConfAvgmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$ConfAvgmeans_Low <-  df$ConfAvgmeans_C + sd(dfnorep$ConfAvgmeans, na.rm = T)

df$T_extentAvgmeans_C <-  df$T_extentAvgmeans - mean(dfnorep$T_extentAvgmeans, na.rm = T)
df$T_extentAvgmeans_High <-  df$T_extentAvgmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$T_extentAvgmeans_Low <-  df$T_extentAvgmeans_C + sd(dfnorep$T_extentAvgmeans, na.rm = T)

df$T_oppmeans_C <-  df$T_oppmeans - mean(dfnorep$T_oppmeans, na.rm = T)
df$T_oppmeans_High <-  df$T_oppmeans_C - sd(dfnorep$ChoicesOther, na.rm = T)
df$T_oppmeans_Low <-  df$T_oppmeans_C + sd(dfnorep$T_oppmeans, na.rm = T)

# Create a no repeating version of df by taking the first row of each participant. Can only look at averages no single item repeated measure scores in this dataframe
# T.f we can use dfnorep for between persons analysis, each row is one participant, while in df, each row is an experience sampling survey


dfnorep <- df[match(unique(df$User_Id), df$User_Id),]

### Let's split our data into positive, negative and mixed opportunities 
dfNegative <- subset(df, valence < 4)
dfPositive <- subset(df, valence > 4)
dfMid <- subset(df, valence == 4)


na.omit(dfNegative$Gen_Prosocial %>% mean())


dfNeg_norep <- dfNegative[match(unique(dfPositive$User_Id), dfNegative$User_Id),]
dfPos_norep <- dfPositive[match(unique(dfPositive$User_Id), dfPositive$User_Id),]
dfMid_norep <- dfMid[match(unique(dfMid$User_Id), dfMid$User_Id),]

# Need to transfrom Empathy Opp sums from 450 minute span to 720 (12hour) span so we can calc CI's

dfnorep$E_Oppsum12hour <- dfnorep$E_Oppsum * 1.58
na.omit(dfnorep$E_Oppsum12hour) %>% length() 


# Let's look at the difference between pre and post sites scores and see if that is predicted by Mall of e beliefs

dfnorep$SITES_diff <- dfnorep$SITES_post - dfnorep$SITES

# Make data confidential

df2 = df[ , -c("email_backup", "email", "email.y", "RecipientEmail", "RecipientFirstName", "RecipientLastName", "Contact_1", "Contact_8", "RecipientEmail.y", "RecipientFirstName.y", "RecipientLastName.y")] 

dfnorep2 <- df2[match(unique(df$User_Id), df$User_Id),]


# Write data to CSV file

write.csv(x = df2, file = c("AnonCleaned_Data_EachRowisASurvey_All_ES_Participants.csv"))
write.csv(x = dfnorep2, file = c("AnonCleaned_Data_EachRowisAPtcpt_All_ES_Participants.csv"))

#Clean workspace
rm(Baseline, Conf_Avg, Demo_trim, Demo_FULL, Descriptive, df2, dfnorep2, dfMid_norep, dfNeg_norep, dfPos_norep, dfsums, Diff_Avg, Extent_Avg, PartMeans, PartMeans3, PartMeans3, i, sumcomp, sumE, sumE_Opp, sumES, sumGen_Prosocial, sumMissedOpp, sumPT, sumT_opp, T_Emp_extent, props_0, props_C, props_E, props_EC, props_EP, props_EPC, props_P, props_PC, SITEScol)

```

