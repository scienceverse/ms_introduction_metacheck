## ============================================================
## This file contains R code that was used to analyze the data for
##
## Salzar Kämpf, M., Liebermann, H., Kerschreiter, R., Krause, S., 
## Nestler, S. & Schmukle, S.C.(2017).
## Disentangling the Sources of Mimicry: Social Relations Analyses 
## of the Link Between Mimicry and Liking.
## 
## ============================================================

##############################################
#Analyses of Gender Differences (for SOM-R)
##############################################

# Load neccessary packages
library (foreign)
library (TripleR)

# Load data set. 
RRSM <- read.spss ("Raw_data_Ratings_Social_Mimicry.sav", to.data.frame=TRUE, use.value.labels = FALSE)

###############################################################
# Gender Differences in SRM Variance Components and Covariances
###############################################################

#1.Univariate SRM analysis of social mimicry with TripleR
SMcompounds<- RR(social_mimicry_global_rater1_rater2_rater3 ~ subject * target | group, data=RRSM)
SMcompounds

#2.Compute group-specific variance components and covariances
groups<-SMcompounds$varComp.group

#2a.groups of women (sex == 1)
groups$sex<-1
#2b.the following are groups of men (sex ==2):
groups$sex[groups$group.id==c("8")] <- 2
groups$sex[groups$group.id==c("9")] <- 2
groups$sex[groups$group.id==c("10")] <- 2
groups$sex[groups$group.id==c("11")] <- 2
groups$sex[groups$group.id==c("16")] <- 2
groups$sex[groups$group.id==c("19")] <- 2
groups$sex[groups$group.id==c("20")] <- 2
groups$sex[groups$group.id==c("21")] <- 2
groups$sex[groups$group.id==c("22")] <- 2
groups$sex[groups$group.id==c("23")] <- 2
groups$sex[groups$group.id==c("24")] <- 2
groups$sex[groups$group.id==c("25")] <- 2
groups$sex[groups$group.id==c("26")] <- 2

#3.Compute t-tests (weighted for group size) for comparing variances and covariances between groups of men and women
summary(lm(estimate~sex, data=groups[groups$type=="actor variance",], weights=group.size))
summary(lm(estimate~sex, data=groups[groups$type=="partner variance",], weights=group.size))
summary(lm(estimate~sex, data=groups[groups$type=="relationship variance",], weights=group.size))
summary(lm(estimate~sex, data=groups[groups$type=="actor-partner covariance",], weights=group.size))
summary(lm(estimate~sex, data=groups[groups$type=="relationship covariance",], weights=group.size))


###############################################################
# Gender Differences in the Mean of Social Mimicry
###############################################################

#4.Compute mean of social mimicry group per group
meangroup <- aggregate (RRSM[,c("social_mimicry_global_rater1_rater2_rater3", "sex")], by = list(RRSM$group), mean)

#5.Define group size (by using results of TripleR)
weightsgroup <- groups$group.size[groups$type=="actor variance"]

#6.Compute weighted means and standard deviations
library(SDMTools)

#6a.Weighten means for groups of women (sex == 1) and men (sex ==2)
wt.mean(meangroup$social_mimicry_global_rater1_rater2_rater3[meangroup$sex==1], weightsgroup[meangroup$sex==1])
wt.mean(meangroup$social_mimicry_global_rater1_rater2_rater3[meangroup$sex==2], weightsgroup[meangroup$sex==2])

#6b.Weighted sd's for groups of women (sex == 1) and men (sex ==2)
wt.sd(meangroup$social_mimicry_global_rater1_rater2_rater3[meangroup$sex==1], weightsgroup[meangroup$sex==1])
wt.sd(meangroup$social_mimicry_global_rater1_rater2_rater3[meangroup$sex==2], weightsgroup[meangroup$sex==2])

#7.Compute t-tests (weighted for group size) for comparing mean of social mimicry between groups of men and women
summary(lm(social_mimicry_global_rater1_rater2_rater3~sex, data=meangroup,weights=weightsgroup))


################################################################
# Gender Effects in the Social Relations Prediction Model
################################################################

#These analyses can be found in the script "3.Data_analysis_LikingT2_and_MetaLikingT2.r",
#because they employ the REML estimator by Nestler (in press), Psychometrika


