
#########################################################################
#################### Load packages and data files #######################
#########################################################################

##  Install packages
libs <- c("Hmisc", "rmarkdown", "car", "lme4", "psych", "reshape2", "tidyverse", "dplyr")
lapply(libs, require, character.only=T)

##  Set the working directory and pull the data files containing the candidate variables
setwd("C:/Desktop/")

## Load datasets 
d1=read.csv("da21940-0011_ADS-COUT54.csv", header = T)
d2=read.csv("da21940-0011_ADS-COUTG1.csv", header = T)
d3=read.csv("da21942-0029_ADS-COUTG5.csv", header = T)
d4=read.csv("da22361-0012_ADS-COUTX5.csv", header = T)
d5=read.csv("da21941-0023_ADS-TCHRG1.csv", header=T)
d6=read.csv("da21942-0031_ADS-FAMG5.csv", header=T)
d7=read.csv("da21942-0034_ADS-TCHRG5.csv", header=T)
d8=read.csv("da22361-0014_ADS-FAMX5.csv", header=T)
d9=read.csv("da22361-0010_ADS-SCHG9.csv", header=T)

## Merge the files by subject ID into one master dataset 
dataset<-merge(d1, d2, by='ID')
dataset<-merge(dataset, d3, by='ID')
dataset<-merge(dataset, d4, by='ID')
dataset<-merge(dataset, d5, by='ID')
dataset<-merge(dataset, d6, by='ID')
dataset<-merge(dataset, d7, by='ID')
dataset<-merge(dataset, d8, by='ID')
dataset<-merge(dataset, d9, by='ID')

#########################################################################
#################### Select and scale analysis variables ################
#########################################################################

## Select list of candidate measures for composites 
d <- subset(dataset, select = c(TPRELM54,	BWT_TM54,	OSESIO54,	SLFRSM54,	
                                IMPLSC54,	BTO_TM54,	PRBSSM54,	WJAPPC54,	WJIWPC54,	
                                WJLWPC54,	WJMSPC54,	WJPVPC54,	SSTSSM54,	SOCASC54,	
                                RELASC54,	NATRBC54,	BIN_TM54,	INTRSM54,	CBQANM54,	
                                CBQFEM54,	CBQSDM54,	AGGAFO54,	CBQAPM54,	CBQALM54,	
                                CBQSHM54,	DOGTWO54,	TPRE_M1S,	BWT_TM1S,	BWT_TT1S,	
                                TPRE_T1S,	TCRPST1S,	LONLYC1F,	SLFRSM1S,	SLFRST1S,	
                                BTO_TM1S,	TPRE_MG5,	SRSMACG5,	SSFPRCG5,	BWT_TMG5,
                                HH_ARMG5, SRSTRCG5,	TPRE_TG5,	BWT_TTG5,	FSQTOCG5,	
                                SLFRSMG5,	SLFRSTG5,	BTO_TMG5,	BTO_TTG5,	ANYRTCG5,	
                                TPRE_MX5,	POSPGMX5,	BWT_TMX5,	HHAARMX5,	FSQT_CX5,	
                                LONL_CX5,	PR1WSCX5,	BONDTCX5,	SECATCX5,	SLFRSMX5,	
                                MPLSCCX5,	SLFRSCX5,	BTO_TMX5,	PRBSSMX5,	ANYR_CX5,	
                                MTHCLSG9,	SCICLSG9,	HAPENSG9,	SSTSSMX5,	SSTSSCX5,	
                                BIN_TMX5,	INTRSMX5,	PSMITCX5,	REPPRCX5, BTO_TT1S,
                                TOTRTMG5))

## Scale all continuous variables
d <- apply(d, 2, function(x) scale(x))
d <- as.data.frame(d)

## Add unscaled CBCL measures for multilevel models
d$bp1 <- dataset$BTO_TM54
d$bp2 <- dataset$BTO_TM1S
d$bp3 <- dataset$BTO_TMG5
d$bp4 <- dataset$BTO_TMX5

## Add ID and delay of gratification pass/fail index
d$ID <- dataset$ID
d$dog <- dataset$DOGPFO54

## Write this to a new .csv for later use
write.csv(d, "d.csv")

#########################################################################
#################### Create composites ##################################
#########################################################################

## Check pairwise correlations among candidate variables for preschool and adolescent levels of five outcomes
## Drop measures with at least one non-significant association, maximizing the number of measures retained
## Create final composites by taking the average, reverse-scoring as needed

##################################
############# Academic Achievement
##################################
  ## Preschool
d$aa_1 <- (d$WJAPPC54 + d$WJIWPC54 + d$WJLWPC54 + d$WJMSPC54 + d$WJPVPC54)/5

  ## Adolescence
d$aa_4 <- (d$MTHCLSG9 + d$SCICLSG9 + d$HAPENSG9)/3

##################################
############# Emotion Regulation
##################################

  ## Preschool 
d$t1cbqna <- (d$CBQANM54 + d$CBQFEM54 + d$CBQSDM54)/3 # First create negative affectivity score from CBQ subscales
d$t1cbqna <- scale(d$t1cbqna*-1) # Re-scale and reverse-score it
d$er_1 <- ((d$BIN_TM54*-1) + (d$INTRSM54*-1) + d$t1cbqna)/3 # Combine with other ER measures
# Dropped: AGGAFO54

  ## Adolescence 
d$er_4 <- ((d$BIN_TMX5*-1) + (d$INTRSMX5*-1))/2

##################################
############# Personality
##################################

  ## Preschool 
d$p_1 <- (d$CBQAPM54 + d$CBQALM54 + (d$CBQSHM54*-1))/3

  ## Adolescence 
d$p_4 <- (d$PSMITCX5 + d$REPPRCX5)/2

##################################
############# Problem Behavior
##################################

  ## Preschool 
d$pbcomp_1 <- (d$BTO_TM54 + d$PRBSSM54)/2

  ## Adolescence 
d$pbcomp_4 <- (d$BTO_TMX5 + d$PRBSSMX5 + d$ANYR_CX5)/3

##################################
############# Social Skills
##################################

  ## Preschool 
d$ss_1 <- (d$SSTSSM54 + d$SOCASC54)/2
# Dropped: NATRBC54

  ## Adolescence
d$ss_4 <- (d$SSTSSMX5 + d$SSTSSCX5)/2

#########################################################################
#################### Models to Produce Table 2 Results ##################
#########################################################################

aa <- summary(lm(d$aa_4 ~ d$dog))

er <- summary(lm(d$er_4 ~ d$dog))

p <- summary(lm(d$p_4 ~ d$dog))

pb <- summary(lm(d$pbcomp_4 ~ d$dog))

ss <- summary(lm(d$ss_4 ~ d$dog))

#########################################################################
#################### Models to Produce Table 3 Results ##################
#########################################################################

aa <- summary(lm(d$aa_4 ~ d$dog + d$aa_1))

er <- summary(lm(d$er_4 ~ d$dog + d$er_1))

p <- summary(lm(d$p_4 ~ d$dog + d$p_1))

pb <- summary(lm(d$pbcomp_4 ~ d$dog + d$pbcomp_1))

ss <- summary(lm(d$ss_4 ~ d$dog + d$ss_1))