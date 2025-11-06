
#########################################################################
#################### Load packages and data files #######################
#########################################################################

##  Install packages
libs <- c("Hmisc", "rmarkdown", "car", "lme4", "psych", "reshape2", "tidyverse", "dplyr")
lapply(libs, require, character.only=T)

##  Set the working directory 
setwd("C:/Desktop/")

##  Read in the analysis dataset
d <- read.csv("d.csv")

#########################################################################
#################### Create composites ##################################
#########################################################################

## Check pairwise correlations among candidate variables for self-control and social support at each timepoint
## Drop measures with at least one non-significant association, maximizing the number of measures retained
## Create final composites by taking the average, reverse-scoring as needed

##################################
############# Social Support
##################################

  ##  Time 1
d$ssup1 = (d$TPRELM54 + (d$BWT_TM54*-1))/2
# Dropped: OSESIO54

  ## Time 2
d$t2tpre <- scale((d$TPRE_M1S + d$TPRE_T1S)/2) # Combine and rescale parent- and teacher-reported positive relationship scores
d$t2bwt_r <- scale((d$BWT_TM1S*-1) + (d$BWT_TT1S*-1))/2) # Same for CBCL Withdrawn
d$ssup2 <- (d$t2tpre + d$t2bwt + d$TCRPST1S + (d$LONLYC1F*-1))/4

  ## Time 3
d$t3tpre <- scale((d$TPRE_MG5 + d$TPRE_TG5)/2) # Combine and rescale parent- and teacher-reported positive relationship scores
d$t3bwt <- scale((d$BWT_TMG5*-1) + (d$BWT_TTG5*-1)/2) # Same for CBCL Withdrawn
d$t3relsec <- scale((d$SRSMACG5 + d$SRSTRCG5)/2) # Same for relationship security
d$ssup3 <- (d$t3tpre + d$t3bwt + d$t3relsec + d$SSFPRCG5 + d$HH_ARMG5 + d$FSQTOCG5)/6

  ## Time 4
d$ssup4 <- (d$TPRE_MX5 + (d$BWT_TMX5*-1) + d$HHAARMX5 + d$FSQT_CX5 + (d$LONL_CX5*-1) + d$PR1WSCX5 + d$SECATCX5)/7
# Dropped: BONDTCX5 

  ## Take the average across the four timepoints
d$meanssup <- rowMeans(d[,c("ssup1", "ssup2", "ssup3", "ssup4")], na.rm = TRUE)

##################################
############# Self-Control
##################################

  ## Time 2
d$sc1 = (d$SLFRSM54+(d$IMPLSC54*-1))/2

  ## Time 2
d$sc2 <- (d$SLFRSM1S + d$SLFRST1S)/2

  ## Time 3
d$sc3 <- (d$SLFRSTG5 + d$SLFRSMG5)/2

  ## Time 4
d$sc4 <- (d$SLFRSMX5 + d$SLFRSCX5 + (d$MPLSCCX5*-1))/2

  ## Take the average across the four timepoints
d$meansc <- rowMeans(d[,c("sc1", "sc2", "sc3", "sc4")], na.rm = TRUE)

#########################################################################
#################### Create analysis dataset for Stata ##################
#########################################################################

#Select variables for stata
statad<-subset(d, select=c(ssup1, ssup2, ssup3, ssup4, meanssup, sc1, sc2, sc3, sc4, meansc, bp1, bp2, bp3, bp4, id, dog))

#Output for melting from wide to long format in stata
write.csv(statad, "C:/Desktop/widedata.csv")
