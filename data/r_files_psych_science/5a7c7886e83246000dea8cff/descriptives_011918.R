## Charlesworth, & Banaji Patterns of Change I.
## Script version date: January 19th, 2018

####################################################################################################################################

## This file provides the basic descriptives for each of the attitudes

## We first get the means and cohen's ds for each of the attitudes (implicit and explicit)
# We then plot these values, including bootstrapped confidence intervals
## We then compute the correlations of implicit and explicit attitudes
## We then compute the demographics of the whole sample, followed by the demographics for each attitude, as well as how the sample demographics
# have changed over time. 
## In this section, we also compute the monthly frequencies by attitude, as well as by the whole sample, and plot these frequencies.

## starting with the full.demogs.NOZIP_012518.RData data object, which includes the US only participants with cleaned data without zipcodes


####################################################################################################################################

## Load the data object
load("full.demogs.NOZIP_012518.RData")


## Install packages
install.packages("pastecs", dependencies = TRUE) 
install.packages("lsr", dependencies = TRUE)  
install.packages("lm.beta", dependencies = TRUE) 
install.packages("bootES", dependencies = TRUE) 
install.packages("ggplot2", dependencies = TRUE)   
install.packages("corrplot", dependencies = TRUE) 
install.packages("car", dependencies = TRUE) 

## Load packages
require(pastecs) # for stat.desc
require(lsr) # for cohensD and EtaSquared
require(lm.beta) # for beta effect sizes
require(bootES) # for effect size bootstrapping, CI for cohens ds
require(ggplot2) # for cohens ds barplots
require(corrplot) # for visualizing correlations with time as well
require(car) # for subsetting weight data

set.seed(000) # be sure to set the seed because of the random selection in the bootstrapping for confidence intervals



## FREQUENCIES  ---------------------------------------------------------------------------------------
## with imputated months removed, 2004 - 2015
median(table(age.full.demogs$month.cent)); mean(table(age.full.demogs$month.cent)); sd(table(age.full.demogs$month.cent)); min(table(age.full.demogs$month.cent)); max(table(age.full.demogs$month.cent))
median(table(dis.full.demogs$month.cent)); mean(table(dis.full.demogs$month.cent)); sd(table(dis.full.demogs$month.cent)); min(table(dis.full.demogs$month.cent)); max(table(dis.full.demogs$month.cent))
median(table(race.full.demogs$month.cent)); mean(table(race.full.demogs$month.cent)); sd(table(race.full.demogs$month.cent)); min(table(race.full.demogs$month.cent)); max(table(race.full.demogs$month.cent))
median(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8]));mean(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])); sd(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])); min(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])); max(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8]))
median(table(sex.full.demogs$month.cent)); mean(table(sex.full.demogs$month.cent)); sd(table(sex.full.demogs$month.cent)); min(table(sex.full.demogs$month.cent)); max(table(sex.full.demogs$month.cent))
median(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); mean(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); sd(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); min(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); max(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))

# Median frequency of medians across samples = 3659
median(c(median(table(age.full.demogs$month.cent)), median(table(dis.full.demogs$month.cent)),
         median(table(race.full.demogs$month.cent)), median(table(sex.full.demogs$month.cent)),
         median(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])),
         median(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

# Median of means across samples = 3984
median(c(mean(table(age.full.demogs$month.cent)), mean(table(dis.full.demogs$month.cent)),
         mean(table(race.full.demogs$month.cent)), mean(table(sex.full.demogs$month.cent)),
         mean(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])),
         mean(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

# Median of min across samples = 408
median(c(min(table(age.full.demogs$month.cent)), min(table(dis.full.demogs$month.cent)),
         min(table(race.full.demogs$month.cent)), min(table(sex.full.demogs$month.cent)),
         min(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])),
         min(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

# Median of max across samples = 11,104
median(c(max(table(age.full.demogs$month.cent)), max(table(dis.full.demogs$month.cent)),
         max(table(race.full.demogs$month.cent)), max(table(sex.full.demogs$month.cent)),
         max(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent!=6 & skin.full.demogs$month.cent!=8])),
         max(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

## Same frequency computations, but only for data used in time series, 2007 - 2015
## with imputation months removed
median(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])); mean(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])); sd(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])); min(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])); max(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36]))
median(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])); mean(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])); sd(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])); min(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])); max(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36]))
median(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])); mean(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])); sd(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])); min(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])); max(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36]))
median(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36]));mean(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])); sd(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])); min(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])); max(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36]))
median(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])); mean(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])); sd(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])); min(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])); max(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36]))
median(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); mean(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); sd(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); min(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131])); max(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))

# Median frequency of medians across samples = 3760
median(c(median(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])),
         median(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])),
         median(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])), 
         median(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])),
         median(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])),
         median(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 & weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

# Median of means across samples = 4018
median(c(mean(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])),
         mean(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])),
         mean(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])),
         mean(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])),
         mean(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])),
         mean(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 & weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

# Median of min across samples = 717
median(c(min(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])),
         min(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])),
         min(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])),
         min(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])),
         min(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])),
         min(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))

# Median of max across samples = 11,104
median(c(max(table(age.full.demogs$month.cent[age.full.demogs$month.cent>36])),
         max(table(dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])),
         max(table(race.full.demogs$month.cent[race.full.demogs$month.cent>36])),
         max(table(sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])),
         max(table(skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])),
         max(table(weight.full.demogs$month.cent[weight.full.demogs$month.cent>36 &weight.full.demogs$month.cent<125 | weight.full.demogs$month.cent>131]))))



## DEMOGRAPHICS ------------------------------------------------------------------------------------------------------------------
# ** make sure that the table values actually correspond to the variable names (i.e., that you are actually looking at White participants)
## FULL SAMPLE -------------------------------------------------------------------------------------------------------------------
## Number of respondents =  4393362
dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] + dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1]

# Age --------
## Full sample age = 27.46; 11.91
(mean(c(age.full.demogs$age.new, dis.full.demogs$age.new, race.full.demogs$age.new, sex.full.demogs$age.new, skin.full.demogs$age.new, weight.full.demogs$age.new)))
(sd(c(age.full.demogs$age.new, dis.full.demogs$age.new, race.full.demogs$age.new, sex.full.demogs$age.new, skin.full.demogs$age.new, weight.full.demogs$age.new)))

# Gender ---------
## Full sample female = 65.88%
(table(age.full.demogs$sex.new)[1] + table(dis.full.demogs$sex.new)[1] + table(race.full.demogs$sex.new)[1] + table(sex.full.demogs$sex.new)[1] + table(skin.full.demogs$sex.new)[1] +
    table(weight.full.demogs$sex.new)[1])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                            dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])
# Race ----------
## Full sample White = 72.24%
(table(age.full.demogs$ethnic.race.new)[1] + table(dis.full.demogs$ethnic.race.new)[1] + table(race.full.demogs$ethnic.race.new)[1] +
    table(sex.full.demogs$ethnic.race.new)[1] + table(skin.full.demogs$ethnic.race.new)[1] +
    table(weight.full.demogs$ethnic.race.new)[1])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])

## Full sample Black = 10.53%
(table(age.full.demogs$ethnic.race.new)[4] + table(dis.full.demogs$ethnic.race.new)[4] + table(race.full.demogs$ethnic.race.new)[4] +
    table(sex.full.demogs$ethnic.race.new)[4] + table(skin.full.demogs$ethnic.race.new)[4] +
    table(weight.full.demogs$ethnic.race.new)[4])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])
## Full sample American Indian = 0.74%
(table(age.full.demogs$ethnic.race.new)[2] + table(dis.full.demogs$ethnic.race.new)[2] + table(race.full.demogs$ethnic.race.new)[2] +
    table(sex.full.demogs$ethnic.race.new)[2] + table(skin.full.demogs$ethnic.race.new)[2] +
    table(weight.full.demogs$ethnic.race.new)[2])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])

## Full sample Asian = 5.10%
(table(age.full.demogs$ethnic.race.new)[3] + table(dis.full.demogs$ethnic.race.new)[3] + table(race.full.demogs$ethnic.race.new)[3] +
    table(sex.full.demogs$ethnic.race.new)[3] + table(skin.full.demogs$ethnic.race.new)[3] +
    table(weight.full.demogs$ethnic.race.new)[3])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])

## Full sample BW biracial = 1.88%
(table(age.full.demogs$ethnic.race.new)[5] + table(dis.full.demogs$ethnic.race.new)[5] + table(race.full.demogs$ethnic.race.new)[5] +
    table(sex.full.demogs$ethnic.race.new)[5] + table(skin.full.demogs$ethnic.race.new)[5] +
    table(weight.full.demogs$ethnic.race.new)[5])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])
## Full sample Other biracial = 4.85%
(table(age.full.demogs$ethnic.race.new)[6] + table(dis.full.demogs$ethnic.race.new)[6] + table(race.full.demogs$ethnic.race.new)[6] +
    table(sex.full.demogs$ethnic.race.new)[6] + table(skin.full.demogs$ethnic.race.new)[6] +
    table(weight.full.demogs$ethnic.race.new)[6])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])
## Full sample Other race = 4.66%
(table(age.full.demogs$ethnic.race.new)[7] + table(dis.full.demogs$ethnic.race.new)[7] + table(race.full.demogs$ethnic.race.new)[7] +
    table(sex.full.demogs$ethnic.race.new)[7] + table(skin.full.demogs$ethnic.race.new)[7] +
    table(weight.full.demogs$ethnic.race.new)[7])/(dim(age.full.demogs)[1] + dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] + dim(sex.full.demogs)[1] +
                                                   dim(skin.full.demogs)[1] + dim(weight.full.demogs)[1])

# Education ----------
# first, create a releveled variable for edu
age.full.demogs$edu.releveled <- as.numeric(age.full.demogs$edu.new) - 5
dis.full.demogs$edu.releveled <- as.numeric(dis.full.demogs$edu.new) - 5
race.full.demogs$edu.releveled <- as.numeric(race.full.demogs$edu.new) - 5
sex.full.demogs$edu.releveled <- as.numeric(sex.full.demogs$edu.new) - 5
skin.full.demogs$edu.releveled <- as.numeric(skin.full.demogs$edu.new) - 5
weight.full.demogs$edu.releveled <- as.numeric(weight.full.demogs$edu.new) - 5

## Full sample HS + = 91.26%
(sum(table(age.full.demogs$edu.releveled[age.full.demogs$edu.releveled>=-1])) + 
    sum(table(dis.full.demogs$edu.releveled[dis.full.demogs$edu.releveled>=-1])) +
    sum(table(race.full.demogs$edu.releveled[race.full.demogs$edu.releveled>=-1])) +
    sum(table(sex.full.demogs$edu.releveled[sex.full.demogs$edu.releveled>=-1])) +
    sum(table(skin.full.demogs$edu.releveled[skin.full.demogs$edu.releveled>=-1])) +
    sum(table(weight.full.demogs$edu.releveled[weight.full.demogs$edu.releveled>=-1])))/(dim(age.full.demogs)[1] +
                                                                                       dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] +
                                                                                       dim(sex.full.demogs)[1] + dim(skin.full.demogs)[1] +
                                                                                       dim(weight.full.demogs)[1])
## Full sample college + = 86.26%
(sum(table(age.full.demogs$edu.releveled[age.full.demogs$edu.releveled>=0])) + 
    sum(table(dis.full.demogs$edu.releveled[dis.full.demogs$edu.releveled>=0])) +
    sum(table(race.full.demogs$edu.releveled[race.full.demogs$edu.releveled>=0])) +
    sum(table(sex.full.demogs$edu.releveled[sex.full.demogs$edu.releveled>=0])) +
    sum(table(skin.full.demogs$edu.releveled[skin.full.demogs$edu.releveled>=0])) +
    sum(table(weight.full.demogs$edu.releveled[weight.full.demogs$edu.releveled>=0])))/(dim(age.full.demogs)[1] +
                                                                                       dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] +
                                                                                       dim(sex.full.demogs)[1] + dim(skin.full.demogs)[1] +
                                                                                       dim(weight.full.demogs)[1])


# Political Ideology --------
# create a releveled political id variable
age.full.demogs$politicalid.releveled <- as.numeric(age.full.demogs$politicalid) - 4
dis.full.demogs$politicalid.releveled <- as.numeric(dis.full.demogs$politicalid) - 4
race.full.demogs$politicalid.releveled <- as.numeric(race.full.demogs$politicalid) - 4
sex.full.demogs$politicalid.releveled <- as.numeric(sex.full.demogs$politicalid) - 4
skin.full.demogs$politicalid.releveled <- as.numeric(skin.full.demogs$politicalid) - 4
weight.full.demogs$politicalid.releveled <- as.numeric(weight.full.demogs$politicalid) - 4

## Full sample Liberal = 45.98%
(sum(table(age.full.demogs$politicalid.releveled[age.full.demogs$politicalid.releveled>0])) + 
    sum(table(dis.full.demogs$politicalid.releveled[dis.full.demogs$politicalid.releveled>0])) +
    sum(table(race.full.demogs$politicalid.releveled[race.full.demogs$politicalid.releveled>0])) +
    sum(table(sex.full.demogs$politicalid.releveled[sex.full.demogs$politicalid.releveled>0])) +
    sum(table(skin.full.demogs$politicalid.releveled[skin.full.demogs$politicalid.releveled>0])) +
    sum(table(weight.full.demogs$politicalid.releveled[weight.full.demogs$politicalid.releveled>0])))/(dim(age.full.demogs)[1] +
                                                                                                     dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] +
                                                                                                     dim(sex.full.demogs)[1] + dim(skin.full.demogs)[1] +
                                                                                                     dim(weight.full.demogs)[1])

## Full sample Conservative = 25.77%
(sum(table(age.full.demogs$politicalid.releveled[age.full.demogs$politicalid.releveled<0])) + 
    sum(table(dis.full.demogs$politicalid.releveled[dis.full.demogs$politicalid.releveled<0])) +
    sum(table(race.full.demogs$politicalid.releveled[race.full.demogs$politicalid.releveled<0])) +
    sum(table(sex.full.demogs$politicalid.releveled[sex.full.demogs$politicalid.releveled<0])) +
    sum(table(skin.full.demogs$politicalid.releveled[skin.full.demogs$politicalid.releveled<0])) +
    sum(table(weight.full.demogs$politicalid.releveled[weight.full.demogs$politicalid.releveled<0])))/(dim(age.full.demogs)[1] +
                                                                                                     dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] +
                                                                                                     dim(sex.full.demogs)[1] + dim(skin.full.demogs)[1] +
                                                                                                     dim(weight.full.demogs)[1])
## Full sample Neutral = 28.25%
(sum(table(age.full.demogs$politicalid.releveled[age.full.demogs$politicalid.releveled==0])) + 
    sum(table(dis.full.demogs$politicalid.releveled[dis.full.demogs$politicalid.releveled==0])) +
    sum(table(race.full.demogs$politicalid.releveled[race.full.demogs$politicalid.releveled==0])) +
    sum(table(sex.full.demogs$politicalid.releveled[sex.full.demogs$politicalid.releveled==0])) +
    sum(table(skin.full.demogs$politicalid.releveled[skin.full.demogs$politicalid.releveled==0])) +
    sum(table(weight.full.demogs$politicalid.releveled[weight.full.demogs$politicalid.releveled==0])))/(dim(age.full.demogs)[1] +
                                                                                                     dim(dis.full.demogs)[1] + dim(race.full.demogs)[1] +
                                                                                                     dim(sex.full.demogs)[1] + dim(skin.full.demogs)[1] +
                                                                                                     dim(weight.full.demogs)[1])



### AGE demographics --------------------------------------------------------------------------------------------------
# age
stat.desc(age.full.demogs$age.new) 
# gender
table(age.full.demogs$sex.new)/sum(table(age.full.demogs$sex.new)) 
# race
(table(age.full.demogs$ethnic.race.new)/sum(table(age.full.demogs$ethnic.race.new)))*100 

# education, with 0 = some college, -1  = high school grad
sum(table(age.full.demogs$edu.releveled[age.full.demogs$edu.releveled>=-1]))/sum(table(age.full.demogs$edu.releveled)) 
sum(table(age.full.demogs$edu.releveled[age.full.demogs$edu.releveled>=0]))/sum(table(age.full.demogs$edu.releveled))
# politicalid
sum(table(age.full.demogs$politicalid.releveled[age.full.demogs$politicalid.releveled>0]))/sum(table(age.full.demogs$politicalid.releveled)) 
sum(table(age.full.demogs$politicalid.releveled[age.full.demogs$politicalid.releveled<0]))/sum(table(age.full.demogs$politicalid.releveled)) 
sum(table(age.full.demogs$politicalid.releveled[age.full.demogs$politicalid.releveled==0]))/sum(table(age.full.demogs$politicalid.releveled)) 


### DISABILITY demographics --------------------------------------------------------------------------------------------------
# age
mean(dis.full.demogs$age.new); sd(dis.full.demogs$age.new)
# gender
table(dis.full.demogs$sex.new)/sum(table(dis.full.demogs$sex.new))
# race
(table(dis.full.demogs$ethnic.race.new)/sum(table(dis.full.demogs$ethnic.race.new)))*100
# education, with 0 = some college
sum(table(dis.full.demogs$edu.releveled[dis.full.demogs$edu.releveled>=-1]))/sum(table(dis.full.demogs$edu.releveled))
sum(table(dis.full.demogs$edu.releveled[dis.full.demogs$edu.releveled>=0]))/sum(table(dis.full.demogs$edu.releveled))
# politicalid
table(dis.full.demogs$politicalid)/sum(table(dis.full.demogs$politicalid))
sum(table(dis.full.demogs$politicalid.releveled[dis.full.demogs$politicalid.releveled>0]))/sum(table(dis.full.demogs$politicalid.releveled))
sum(table(dis.full.demogs$politicalid.releveled[dis.full.demogs$politicalid.releveled<0]))/sum(table(dis.full.demogs$politicalid.releveled))
sum(table(dis.full.demogs$politicalid.releveled[dis.full.demogs$politicalid.releveled==0]))/sum(table(dis.full.demogs$politicalid.releveled))

# disability
table(dis.full.demogs$dis.new)/sum(table(dis.full.demogs$dis.new))

### RACE demographics --------------------------------------------------------------------------------------------------
# age
mean(race.full.demogs$age.new); sd(race.full.demogs$age.new)
# gender
table(race.full.demogs$sex.new)/sum(table(race.full.demogs$sex.new))
# race
(table(race.full.demogs$ethnic.race.new)/sum(table(race.full.demogs$ethnic.race.new)))*100
# education, with 0 = some college
sum(table(race.full.demogs$edu.releveled[race.full.demogs$edu.releveled>=-1]))/sum(table(race.full.demogs$edu.releveled)) 
sum(table(race.full.demogs$edu.releveled[race.full.demogs$edu.releveled>=0]))/sum(table(race.full.demogs$edu.releveled)) 
# politicalid
table(race.full.demogs$politicalid)/sum(table(race.full.demogs$politicalid))
sum(table(race.full.demogs$politicalid.releveled[race.full.demogs$politicalid.releveled>0]))/sum(table(race.full.demogs$politicalid.releveled)) 
sum(table(race.full.demogs$politicalid.releveled[race.full.demogs$politicalid.releveled<0]))/sum(table(race.full.demogs$politicalid.releveled)) 
sum(table(race.full.demogs$politicalid.releveled[race.full.demogs$politicalid.releveled==0]))/sum(table(race.full.demogs$politicalid.releveled)) 


### SEXUALITY demographics --------------------------------------------------------------------------------------------------
# age
mean(sex.full.demogs$age.new) ; sd(sex.full.demogs$age.new)
# gender
table(sex.full.demogs$sex.new)/sum(table(sex.full.demogs$sex.new)) 
# race
(table(sex.full.demogs$ethnic.race.new)/sum(table(sex.full.demogs$ethnic.race.new)))*100
# education, with 0 = some college
table(sex.full.demogs$edu.new)/sum(table(sex.full.demogs$edu.new))
sum(table(sex.full.demogs$edu.releveled[sex.full.demogs$edu.releveled>=-1]))/sum(table(sex.full.demogs$edu.releveled)) 
sum(table(sex.full.demogs$edu.releveled[sex.full.demogs$edu.releveled>=0]))/sum(table(sex.full.demogs$edu.releveled)) 

# politicalid
table(sex.full.demogs$politicalid)/sum(table(sex.full.demogs$politicalid))
sum(table(sex.full.demogs$politicalid.releveled[sex.full.demogs$politicalid.releveled>0]))/sum(table(sex.full.demogs$politicalid.releveled)) 
sum(table(sex.full.demogs$politicalid.releveled[sex.full.demogs$politicalid.releveled<0]))/sum(table(sex.full.demogs$politicalid.releveled)) 
sum(table(sex.full.demogs$politicalid.releveled[sex.full.demogs$politicalid.releveled==0]))/sum(table(sex.full.demogs$politicalid.releveled)) 

# sexuality
table(sex.full.demogs$sexuality.new)/sum(table(sex.full.demogs$sexuality.new))

### SKINTONE demographics --------------------------------------------------------------------------------------------------
# age
mean(skin.full.demogs$age.new); sd(skin.full.demogs$age.new)
# gender
table(skin.full.demogs$sex.new)/sum(table(skin.full.demogs$sex.new))
# race
(table(skin.full.demogs$ethnic.race.new)/sum(table(skin.full.demogs$ethnic.race.new)))*100
# education, with 0 = some college
sum(table(skin.full.demogs$edu.releveled[skin.full.demogs$edu.releveled>=-1]))/sum(table(skin.full.demogs$edu.releveled)) 
sum(table(skin.full.demogs$edu.releveled[skin.full.demogs$edu.releveled>=0]))/sum(table(skin.full.demogs$edu.releveled)) 

# politicalid
table(skin.full.demogs$politicalid)/sum(table(skin.full.demogs$politicalid))
sum(table(skin.full.demogs$politicalid.releveled[skin.full.demogs$politicalid.releveled>0]))/sum(table(skin.full.demogs$politicalid.releveled)) 
sum(table(skin.full.demogs$politicalid.releveled[skin.full.demogs$politicalid.releveled<0]))/sum(table(skin.full.demogs$politicalid.releveled)) 
sum(table(skin.full.demogs$politicalid.releveled[skin.full.demogs$politicalid.releveled==0]))/sum(table(skin.full.demogs$politicalid.releveled)) 


### WEIGHT demographics --------------------------------------------------------------------------------------------------
weight.full.demogs$stim.collapse <- recode(weight.full.demogs$Stimuli.new, "'body figure v1' = 'body'; 'body figure v2' = 'body'")
weightbody <- weight.full.demogs[weight.full.demogs$stim.collapse=="body",]
table(weightbody$month)
weightface <- weight.full.demogs[weight.full.demogs$stim.collapse=="face",]
table(weightface$month)

# age
mean(weight.full.demogs$age.new) 
sd(weight.full.demogs$age.new)
# gender
table(weight.full.demogs$sex.new)/sum(table(weight.full.demogs$sex.new))
# race
(table(weight.full.demogs$ethnic.race.new)/sum(table(weight.full.demogs$ethnic.race.new)))*100
# education, with 0 = some college
sum(table(weight.full.demogs$edu.releveled[weight.full.demogs$edu.releveled>=-1]))/sum(table(weight.full.demogs$edu.releveled)) 
sum(table(weight.full.demogs$edu.releveled[weight.full.demogs$edu.releveled>=0]))/sum(table(weight.full.demogs$edu.releveled)) 

# politicalid
table(weight.full.demogs$politicalid)/sum(table(weight.full.demogs$politicalid))
sum(table(weight.full.demogs$politicalid.releveled[weight.full.demogs$politicalid.releveled>0]))/sum(table(weight.full.demogs$politicalid.releveled)) 
sum(table(weight.full.demogs$politicalid.releveled[weight.full.demogs$politicalid.releveled<0]))/sum(table(weight.full.demogs$politicalid.releveled)) 
sum(table(weight.full.demogs$politicalid.releveled[weight.full.demogs$politicalid.releveled==0]))/sum(table(weight.full.demogs$politicalid.releveled)) 

# weight
table(weight.full.demogs$weight.COMB)/sum(table(weight.full.demogs$weight.COMB)) 
weight.full.demogs$weight.relevel <- as.numeric(weight.full.demogs$weight.COMB)
weight.full.demogs$weight.relevel <- weight.full.demogs$weight.relevel - 4
# Overweight
sum(table(weight.full.demogs$weight.relevel[weight.full.demogs$weight.relevel>0]))/sum(table(weight.full.demogs$weight.relevel))
# Underweight
sum(table(weight.full.demogs$weight.relevel[weight.full.demogs$weight.relevel<0]))/sum(table(weight.full.demogs$weight.relevel)) 
# Neither over nor under
sum(table(weight.full.demogs$weight.relevel[weight.full.demogs$weight.relevel==0]))/sum(table(weight.full.demogs$weight.relevel))



## DEMOGRAPHICS (OVER TIME) ---------------------------------------------------------------------------------------------------------
## first, make remaining factor-level demographic variables numeric 
# race
age.full.demogs$ethnicrace.num <- as.numeric(age.full.demogs$ethnic.race.new)
dis.full.demogs$ethnicrace.num <- as.numeric(dis.full.demogs$ethnic.race.new)
race.full.demogs$ethnicrace.num <- as.numeric(race.full.demogs$ethnic.race.new)
sex.full.demogs$ethnicrace.num <- as.numeric(sex.full.demogs$ethnic.race.new)
skin.full.demogs$ethnicrace.num <- as.numeric(skin.full.demogs$ethnic.race.new)
weight.full.demogs$ethnicrace.num <- as.numeric(weight.full.demogs$ethnic.race.new)

# sex
age.full.demogs$sex.num <- as.numeric(age.full.demogs$sex.new)
dis.full.demogs$sex.num <- as.numeric(dis.full.demogs$sex.new)
race.full.demogs$sex.num <- as.numeric(race.full.demogs$sex.new)
sex.full.demogs$sex.num <- as.numeric(sex.full.demogs$sex.new)
skin.full.demogs$sex.num <- as.numeric(skin.full.demogs$sex.new)
weight.full.demogs$sex.num <- as.numeric(weight.full.demogs$sex.new)

# and attitude-specific variables
# disability
table(dis.full.demogs$dis.new)
dis.full.demogs$disabled.num <- as.numeric(dis.full.demogs$dis.new)

# sexuality
table(sex.full.demogs$sexuality.new)
sex.full.demogs$sexuality.num <- as.numeric(sex.full.demogs$sexuality.new)

# weight
table(weight.full.demogs$weight.COMB)/sum(table(weight.full.demogs$weight.COMB)) 
weight.full.demogs$weight.relevel <- as.numeric(weight.full.demogs$weight.COMB)
weight.full.demogs$weight.relevel <- weight.full.demogs$weight.relevel - 4
table(weight.full.demogs$weight.relevel)


# AGE ---------------------------------------------------------------------------------------------------------
# demographic correlations
dem.cor.age <- age.full.demogs[c("month.cent", "dscore", "att.center1",
                               "age.new", "sex.num","ethnicrace.num",  "edu.releveled", "politicalid.releveled")]
names(dem.cor.age) <- c("Month", "Implicit", "Explicit", "Age", "Sex", "Race", "Education", "Political")
round(cor(dem.cor.age), 3)
corrplot(cor(dem.cor.age), insig = "blank")

dem.cor.age07 <- dem.cor.age[dem.cor.age$Month>36,]
round(cor(dem.cor.age07), 3)
corrplot(cor(dem.cor.age07), insig = "blank")

cor.test(age.full.demogs$dscore, age.full.demogs$month.cent)
cor.test(age.full.demogs$att.center1, age.full.demogs$month.cent)
cor.test(age.full.demogs$dscore[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])
cor.test(age.full.demogs$att.center1[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])

# How have demographics changed over time? -------------
# age of sample, 2004 - 2015
cor.test(age.full.demogs$age.new, age.full.demogs$month.cent)
# age of sample, 2007 - 2015
cor.test(age.full.demogs$age.new[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])

# gender, 2004 - 2015
cor.test(age.full.demogs$sex.num, age.full.demogs$month.cent)
# gender, 2007 - 2015
cor.test(age.full.demogs$sex.num[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])

# race, 2004 - 2015
cor.test(age.full.demogs$ethnicrace.num, age.full.demogs$month.cent)
# race, 2004 - 2015
cor.test(age.full.demogs$ethnicrace.num[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])

# politicalid, 2004 - 2015
cor.test(age.full.demogs$politicalid.releveled, age.full.demogs$month.cent)
# politicalid, 2007 - 2015
cor.test(age.full.demogs$politicalid.releveled[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])

# education, 2004 - 2015
cor.test(age.full.demogs$edu.releveled, age.full.demogs$month.cent)
# education, 2007 - 2015
cor.test(age.full.demogs$edu.releveled[age.full.demogs$month.cent>36], 
         age.full.demogs$month.cent[age.full.demogs$month.cent>36])


# DISABILITY ------------------------------------------------------------------------------------------
# demographic correlations
dem.cor.dis <- dis.full.demogs[c("month.cent", "dscore", "att.center1",
                               "age.new", "sex.num","ethnicrace.num",  "edu.releveled", "politicalid.releveled", "disabled.num")]
names(dem.cor.dis) <- c("Month", "Implicit", "Explicit", "Age", "Sex", "Race", "Education", "Political", "Disability")
round(cor(dem.cor.dis), 3)
corrplot(cor(dem.cor.dis), insig = "blank")

dem.cor.dis07 <- dem.cor.dis[dem.cor.dis$Month>36,]
round(cor(dem.cor.dis07), 3)
corrplot(cor(dem.cor.dis07), insig = "blank")

cor.test(dis.full.demogs$dscore, dis.full.demogs$month.cent)
cor.test(dis.full.demogs$att.center1, dis.full.demogs$month.cent)
cor.test(dis.full.demogs$dscore[dis.full.demogs$month.cent>36], 
         dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])
cor.test(dis.full.demogs$att.center1[dis.full.demogs$month.cent>36], 
         dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])


# How have demographics changed over time? -------------
# Age of sample, 2004 - 2015
cor.test(dis.full.demogs$age.new, dis.full.demogs$month.cent)
# Dis of sample, 2007 - 2015
cor.test(dis.full.demogs$age.new[dis.full.demogs$month.cent>36], 
         dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])

# gender, 2004 - 2015
cor.test(dis.full.demogs$sex.num, dis.full.demogs$month.cent)
# gender, 2007 - 2015
cor.test(dis.full.demogs$sex.num[dis.full.demogs$month.cent>36], dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])

# race, 2004 - 2015
cor.test(dis.full.demogs$ethnicrace.num, dis.full.demogs$month.cent)
# race, 2004 - 2015
cor.test(dis.full.demogs$ethnicrace.num[dis.full.demogs$month.cent>36], dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])

# politicalid, 2004 - 2015
cor.test(dis.full.demogs$politicalid.releveled, dis.full.demogs$month.cent)
# politicalid, 2007 - 2015
cor.test(dis.full.demogs$politicalid.releveled[dis.full.demogs$month.cent>36], dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])

# education, 2004 - 2015
cor.test(dis.full.demogs$edu.releveled, dis.full.demogs$month.cent)
# education, 2007 - 2015
cor.test(dis.full.demogs$edu.releveled[dis.full.demogs$month.cent>36], dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])

# disability, 2004 - 2015
cor.test(dis.full.demogs$disabled.num, dis.full.demogs$month.cent)
# disability, 2007 - 2015
cor.test(dis.full.demogs$disabled.num[dis.full.demogs$month.cent>36], dis.full.demogs$month.cent[dis.full.demogs$month.cent>36])


# RACE  -----------
# demographic correlations
dem.cor.race <- race.full.demogs[c("month.cent", "dscore", "att.center1",
                                 "age.new", "sex.num","ethnicrace.num",  "edu.releveled", "politicalid.releveled")]
names(dem.cor.race) <- c("Month", "Implicit", "Explicit", "Age", "Sex", "Race", "Education", "Political")
round(cor(dem.cor.race), 3)
corrplot(cor(dem.cor.race), insig = "blank")

dem.cor.race07 <- dem.cor.race[dem.cor.race$Month>36,]
round(cor(dem.cor.race07), 3)
corrplot(cor(dem.cor.race07), insig = "blank")

cor.test(race.full.demogs$dscore, race.full.demogs$month.cent)
cor.test(race.full.demogs$att.center1, race.full.demogs$month.cent)
cor.test(race.full.demogs$dscore[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])
cor.test(race.full.demogs$att.center1[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])


# How have demographics changed over time? -------------
# Age of sample, 2004 - 2015
cor.test(race.full.demogs$age.new, race.full.demogs$month.cent)
# Age of sample, 2007 - 2015
cor.test(race.full.demogs$age.new[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])

# gender, 2004 - 2015
cor.test(race.full.demogs$sex.num, race.full.demogs$month.cent)
# gender, 2007 - 2015
cor.test(race.full.demogs$sex.num[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])

# race, 2004 - 2015
cor.test(race.full.demogs$ethnicrace.num, race.full.demogs$month.cent)
# race, 2004 - 2015
cor.test(race.full.demogs$ethnicrace.num[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])

# politicalid, 2004 - 2015
cor.test(race.full.demogs$politicalid.releveled, race.full.demogs$month.cent)
# politicalid, 2007 - 2015
cor.test(race.full.demogs$politicalid.releveled[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])

# education, 2004 - 2015
cor.test(race.full.demogs$edu.releveled, race.full.demogs$month.cent)
# education, 2007 - 2015
cor.test(race.full.demogs$edu.releveled[race.full.demogs$month.cent>36], race.full.demogs$month.cent[race.full.demogs$month.cent>36])


# SEXUALITY  ------------
# demographic correlations
dem.cor.sex <- sex.full.demogs[c("month.cent", "dscore", "att.center1",
                               "age.new", "sex.num","ethnicrace.num",  "edu.releveled", "politicalid.releveled", "sexuality.num")]
names(dem.cor.sex) <- c("Month", "Implicit", "Explicit", "Age", "Sex", "Race", "Education", "Political", "Sexuality")
round(cor(dem.cor.sex), 3)
corrplot(cor(dem.cor.sex), insig = "blank")

dem.cor.sex07 <- dem.cor.sex[dem.cor.sex$Month>36,]
round(cor(dem.cor.sex07), 3)
corrplot(cor(dem.cor.sex07), insig = "blank")

cor.test(sex.full.demogs$dscore, sex.full.demogs$month.cent)
cor.test(sex.full.demogs$att.center1, sex.full.demogs$month.cent)
cor.test(sex.full.demogs$dscore[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])
cor.test(sex.full.demogs$att.center1[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])


# How have demographics changed over time? -------------
# Age of sample, 2004 - 2015
cor.test(sex.full.demogs$age.new, sex.full.demogs$month.cent)
# Age of sample, 2007 - 2015
cor.test(sex.full.demogs$age.new[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])

# gender, 2004 - 2015
cor.test(sex.full.demogs$sex.num, sex.full.demogs$month.cent)
# gender, 2007 - 2015
cor.test(sex.full.demogs$sex.num[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])

# race, 2004 - 2015
cor.test(sex.full.demogs$ethnicrace.num, sex.full.demogs$month.cent)
# race, 2004 - 2015
cor.test(sex.full.demogs$ethnicrace.num[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])

# politicalid, 2004 - 2015
cor.test(sex.full.demogs$politicalid.releveled, sex.full.demogs$month.cent)
# politicalid, 2007 - 2015
cor.test(sex.full.demogs$politicalid.releveled[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])

# education, 2004 - 2015
cor.test(sex.full.demogs$edu.releveled, sex.full.demogs$month.cent)
# education, 2007 - 2015
cor.test(sex.full.demogs$edu.releveled[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])

# Sexuality, 2004 - 2015
cor.test(sex.full.demogs$sexuality.num, sex.full.demogs$month.cent)
# Sexuality, 2007 - 2015
cor.test(sex.full.demogs$sexuality.num[sex.full.demogs$month.cent>36], sex.full.demogs$month.cent[sex.full.demogs$month.cent>36])


# SKINTONE  -----------
# demographic correlations
dem.cor.skin <- skin.full.demogs[c("month.cent", "dscore", "att.center1",
                                 "age.new", "sex.num","ethnicrace.num",  "edu.releveled", "politicalid.releveled")]
names(dem.cor.skin) <- c("Month", "Implicit", "Explicit", "Age", "Sex", "Race", "Education", "Political")
round(cor(dem.cor.skin), 3)
corrplot(cor(dem.cor.skin), insig = "blank")

dem.cor.skin07 <- dem.cor.skin[dem.cor.skin$Month>36,]
round(cor(dem.cor.skin07), 3)
corrplot(cor(dem.cor.skin07), insig = "blank")

cor.test(skin.full.demogs$dscore, skin.full.demogs$month.cent)
cor.test(skin.full.demogs$att.center1, skin.full.demogs$month.cent)
cor.test(skin.full.demogs$dscore[skin.full.demogs$month.cent>36], skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])
cor.test(skin.full.demogs$att.center1[skin.full.demogs$month.cent>36], skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])


# How have demographics changed over time? -------------
# Age of sample, 2004 - 2015
cor.test(skin.full.demogs$age.new, skin.full.demogs$month.cent)
# Skin of sample, 2007 - 2015
cor.test(skin.full.demogs$age.new[skin.full.demogs$month.cent>36], 
         skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])

# gender, 2004 - 2015
cor.test(skin.full.demogs$sex.num, skin.full.demogs$month.cent)
# gender, 2007 - 2015
cor.test(skin.full.demogs$sex.num[skin.full.demogs$month.cent>36], skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])

# race, 2004 - 2015
cor.test(skin.full.demogs$ethnicrace.num, skin.full.demogs$month.cent)
# race, 2004 - 2015
cor.test(skin.full.demogs$ethnicrace.num[skin.full.demogs$month.cent>36], skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])

# politicalid, 2004 - 2015
cor.test(skin.full.demogs$politicalid.releveled, skin.full.demogs$month.cent)
# politicalid, 2007 - 2015
cor.test(skin.full.demogs$politicalid.releveled[skin.full.demogs$month.cent>36], skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])

# education, 2004 - 2015
cor.test(skin.full.demogs$edu.releveled, skin.full.demogs$month.cent)
# education, 2007 - 2015
cor.test(skin.full.demogs$edu.releveled[skin.full.demogs$month.cent>36], skin.full.demogs$month.cent[skin.full.demogs$month.cent>36])



# WEIGHT ------------
# demographic correlations (in both face and body tests)
# because of the nas occuring in the weight.releveled measure, 
# we will compute the correlation table without that measure
# we will then separately do the cor.test for weight.releveled with month using na.omit.

dem.cor.weight <- weight.full.demogs[c("month.cent", "dscore", "att.center1",
                                     "age.new", "sex.num","ethnicrace.num",  "edu.releveled", "politicalid.releveled")]
names(dem.cor.weight) <- c("Month", "Implicit", "Explicit", "Age", "Sex", "Race", "Education", "Political")

round(cor(dem.cor.weight), 3)
corrplot(cor(dem.cor.weight), insig = "blank")

dem.cor.weight07 <- dem.cor.weight[dem.cor.weight$Month>36,]
round(cor(dem.cor.weight07), 3)
corrplot(cor(dem.cor.weight07), insig = "blank")

cor.test(weight.full.demogs$dscore, weight.full.demogs$month.cent)
cor.test(weight.full.demogs$att.center1, weight.full.demogs$month.cent)
cor.test(weight.full.demogs$dscore[weight.full.demogs$month.cent>36], weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])
cor.test(weight.full.demogs$att.center1[weight.full.demogs$month.cent>36], weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])


# How have demographics changed over time? -------------
# Age of sample, 2004 - 2015
cor.test(weight.full.demogs$age.new, weight.full.demogs$month.cent)
# Age of sample, 2007 - 2015
cor.test(weight.full.demogs$age.new[weight.full.demogs$month.cent>36], weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])

# gender, 2004 - 2015
cor.test(weight.full.demogs$sex.num, weight.full.demogs$month.cent)
# gender, 2007 - 2015
cor.test(weight.full.demogs$sex.num[weight.full.demogs$month.cent>36], weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])

# race, 2004 - 2015
cor.test(weight.full.demogs$ethnicrace.num, weight.full.demogs$month.cent)
# race, 2004 - 2015
cor.test(weight.full.demogs$ethnicrace.num[weight.full.demogs$month.cent>36], weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])

# politicalid, 2004 - 2015
cor.test(weight.full.demogs$politicalid.releveled, weight.full.demogs$month.cent)
# politicalid, 2007 - 2015
cor.test(weight.full.demogs$politicalid.releveled[weight.full.demogs$month.cent>36], 
         weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])

# education, 2004 - 2015
cor.test(weight.full.demogs$edu.releveled, weight.full.demogs$month.cent)
# education, 2007 - 2015
cor.test(weight.full.demogs$edu.releveled[weight.full.demogs$month.cent>36], 
         weight.full.demogs$month.cent[weight.full.demogs$month.cent>36])

# weight, 2004 - 2015
cor.test(weight.full.demogs$weight.relevel, weight.full.demogs$month.cent, na.action("na.omit"))
# weight, 2007 - 2015
cor.test(weight.full.demogs$weight.relevel[weight.full.demogs$month.cent>36], 
         weight.full.demogs$month.cent[weight.full.demogs$month.cent>36], na.action("na.omit"))


# FULL SAMPLE: How have demographics changed over time? -------------
## Create a full sample of just the key demographics to look at all changes over time...
# remove the attitude-specific variables
dem.cor.dis <- dem.cor.dis[,-9]
dem.cor.sex <- dem.cor.sex[,-9]
alldata <- rbind(dem.cor.age, dem.cor.dis, dem.cor.race, dem.cor.sex, dem.cor.skin, dem.cor.weight)

# 2004 - 2015
round(cor(alldata),4)
corrplot(cor(alldata), insig = "blank")

cor.test(alldata$Month, alldata$Age)
cor.test(alldata$Month, alldata$Sex)
cor.test(alldata$Month, alldata$Race)
cor.test(alldata$Month, alldata$Education)
cor.test(alldata$Month, alldata$Political)

# 2007 - 2015
alldata07 <- alldata[alldata$Month > 36,]
round(cor(alldata07),4)
corrplot(cor(alldata07), insig = "blank")

cor.test(alldata07$Month, alldata07$Age)
cor.test(alldata07$Month, alldata07$Sex)
cor.test(alldata07$Month, alldata07$Race)
cor.test(alldata07$Month, alldata07$Education)
cor.test(alldata07$Month, alldata07$Political)


## Plotting all monthly frequencies -----------------
op <- par(mfrow = c(3,2))
hist(age.full.demogs$month.cent, main = "Frequencies of Age IAT by Month", xaxt = "n", xlab = "", breaks = 20, ylim = c(0, 75000))
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133), labels=c(2004:2015), cex.axis = 1.3)
box()
hist(dis.full.demogs$month.cent, main = "Frequencies of Disability IAT by Month", xaxt = "n", xlab = "", breaks = 15, ylim = c(0, 50000))
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133), labels=c(2004:2015), cex.axis = 1.3)
box()
hist(race.full.demogs$month.cent, main = "Frequencies of Race IAT by Month", xaxt = "n", xlab = "", breaks = 20, ylim = c(0, 180000))
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133), labels=c(2004:2015), cex.axis = 1.3)
box()
hist(skin.full.demogs$month.cent, main = "Frequencies of Skin-Tone IAT by Month", xaxt = "n", xlab = "", breaks = 15, ylim = c(0, 50000))
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133), labels=c(2004:2015), cex.axis = 1.3)
box()
hist(sex.full.demogs$month.cent, main = "Frequencies of Sexuality IAT by Month", xaxt = "n", xlab = "", breaks = 15, ylim = c(0, 75000))
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133), labels=c(2004:2015), cex.axis = 1.3)
box()
hist(weight.full.demogs$month.cent, main = "Frequencies of Weight IAT by Month", xaxt = "n", xlab = "", breaks = 15, ylim = c(0, 75000))
axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133), labels=c(2004:2015), cex.axis = 1.3)
box()


## IMPLICIT ATTITUDES ---------------------------------------------------------------------------------------------------------

## Means -----------
## get measures of central tendency of the implicit attitudes 
stat.desc(age.full.demogs$dscore)
stat.desc(dis.full.demogs$dscore)
stat.desc(race.full.demogs$dscore)
stat.desc(sex.full.demogs$dscore)
stat.desc(skin.full.demogs$dscore)
stat.desc(weight.full.demogs$dscore)
stat.desc(weightbody$dscore)
stat.desc(weightface$dscore)

# significance, one-sample t-tests
t.test(age.full.demogs$dscore)
t.test(dis.full.demogs$dscore) 
t.test(race.full.demogs$dscore)
t.test(sex.full.demogs$dscore) 
t.test(skin.full.demogs$dscore) 
t.test(weight.full.demogs$dscore) 
t.test(weightbody$dscore)
t.test(weightface$dscore)

# Cohens ds effect sizes ---------
cohensD(age.full.demogs$dscore) # 
cohensD(dis.full.demogs$dscore) # 
cohensD(race.full.demogs$dscore) # 
cohensD(sex.full.demogs$dscore) # 
cohensD(skin.full.demogs$dscore) # 
cohensD(weight.full.demogs$dscore) # 
cohensD(weightbody$dscore)
cohensD(weightface$dscore)

# compile cohens for plotting later...
cohens.implicit <- c(cohensD(age.full.demogs$dscore), cohensD(skin.full.demogs$dscore), cohensD(race.full.demogs$dscore), 
                     cohensD(dis.full.demogs$dscore), cohensD(weight.full.demogs$dscore), cohensD(sex.full.demogs$dscore))

# cohens ds confidence intervals for plotting...
## note that these simulations will take a while...
set.seed(000)
age.full.demogs.sample1 <- age.full.demogs[sample(1:nrow(age.full.demogs), 1000),]
dis.full.demogs.sample1 <- dis.full.demogs[sample(1:nrow(dis.full.demogs), 1000),]
race.full.demogs.sample1 <- race.full.demogs[sample(1:nrow(race.full.demogs), 1000),]
sex.full.demogs.sample1 <- sex.full.demogs[sample(1:nrow(sex.full.demogs), 1000),]
skin.full.demogs.sample1 <- skin.full.demogs[sample(1:nrow(skin.full.demogs), 1000),]
weight.full.demogs.sample1 <- weight.full.demogs[sample(1:nrow(weight.full.demogs), 1000),]

age.cohens.CI <- bootES(age.full.demogs.sample1, R=10000, data.col="dscore",
                        effect.type = "cohens.d")
age.cohens.CI$bounds 
age.cohens.CI$t0 - age.cohens.CI$bounds[1] # the simulated mean - lower boundary = the standard error of the cohens d (used in plotting).

dis.cohens.CI <- bootES(dis.full.demogs.sample1, R=10000, data.col="dscore",
                        effect.type = "cohens.d")
dis.cohens.CI$bounds 
dis.cohens.CI$t0 - dis.cohens.CI$bounds[1] 

race.cohens.CI <- bootES(race.full.demogs.sample1, R=10000, data.col="dscore",
                         effect.type = "cohens.d")
race.cohens.CI$bounds 
race.cohens.CI$t0 - race.cohens.CI$bounds[1] 

sex.cohens.CI <- bootES(sex.full.demogs.sample1, R=10000, data.col="dscore",
                        effect.type = "cohens.d")
sex.cohens.CI$bounds 
sex.cohens.CI$t0 - sex.cohens.CI$bounds[1] 

skin.cohens.CI <- bootES(skin.full.demogs.sample1, R=10000, data.col="dscore",
                         effect.type = "cohens.d")
skin.cohens.CI$bounds 
skin.cohens.CI$t0 - skin.cohens.CI$bounds[1] 

weight.cohens.CI <- bootES(weight.full.demogs.sample1, R=10000, data.col="dscore",
                           effect.type = "cohens.d")
weight.cohens.CI$bounds
weight.cohens.CI$t0 - weight.cohens.CI$bounds[1] 


cohens.SE.implicit <- as.vector(c(age.cohens.CI$t0 - age.cohens.CI$bounds[1],
                                  skin.cohens.CI$t0 - skin.cohens.CI$bounds[1],
                                  race.cohens.CI$t0 - race.cohens.CI$bounds[1],
                                  dis.cohens.CI$t0 - dis.cohens.CI$bounds[1],
                                  weight.cohens.CI$t0 - weight.cohens.CI$bounds[1],
                                  sex.cohens.CI$t0 - sex.cohens.CI$bounds[1]))


## Visualize distributions of implicit attitudes ------
op <- par(mfrow = c(3,2))
hist(age.full.demogs$dscore, main = "Distribution of Implicit Age Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(age.full.demogs$dscore), col = "red", lwd = 3)
abline(v = mean(age.full.demogs$dscore)+sd(age.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(age.full.demogs$dscore)-sd(age.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
hist(skin.full.demogs$dscore, main = "Distribution of Implicit Skin-Tone Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(skin.full.demogs$dscore), col = "red", lwd = 3)
abline(v = mean(skin.full.demogs$dscore)+sd(skin.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(skin.full.demogs$dscore)-sd(skin.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
hist(race.full.demogs$dscore, main = "Distribution of Implicit Race Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(race.full.demogs$dscore), col = "red", lwd = 3)
abline(v = mean(race.full.demogs$dscore)+sd(race.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(race.full.demogs$dscore)-sd(race.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
hist(sex.full.demogs$dscore, main = "Distribution of Implicit Sexuality Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(sex.full.demogs$dscore), col = "red", lwd = 3)
abline(v = mean(sex.full.demogs$dscore)+sd(sex.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(sex.full.demogs$dscore)-sd(sex.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
hist(dis.full.demogs$dscore, main = "Distribution of Implicit Disability Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(dis.full.demogs$dscore), col = "red", lwd = 3)
abline(v = mean(dis.full.demogs$dscore)+sd(dis.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(dis.full.demogs$dscore)-sd(dis.full.demogs$dscore), col = "blue", lwd = 2, lty = 2)
hist(weightbody$dscore, main = "Distribution of Implicit Weight Attitudes \n (Body-figure)", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(weightbody$dscore), col = "red", lwd = 3)
abline(v = mean(weightbody$dscore)+sd(weightbody$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(weightbody$dscore)-sd(weightbody$dscore), col = "blue", lwd = 2, lty = 2)
par(op)


## plot all in one graph using boxplots
x1 <- age.full.demogs$dscore
x2 <- skin.full.demogs$dscore
x3 <- race.full.demogs$dscore
x4 <- sex.full.demogs$dscore
x5 <- dis.full.demogs$dscore
x6 <- weight.full.demogs$dscore
boxplot(x1, x2, x3, x4, x5, x6, names=c("Age", "Skin-Tone", "Race", "Sexuality", "Disability", "Weight"), 
        outline = FALSE,
        main = "Median Implicit Attitudes",
        ylab = "Raw IAT D-scores")



## EXPLICIT ATTITUDES ---------------------------------------------------------------------------------------------------------
## att.center1 is mean-centered but not standardized
## att.center is standardized

## Means ----------
## get measures of central tendency of the explicit attitudes
stat.desc(age.full.demogs$att.center1) 
stat.desc(dis.full.demogs$att.center1) 
stat.desc(race.full.demogs$att.center1) 
stat.desc(sex.full.demogs$att.center1) 
stat.desc(skin.full.demogs$att.center1) 
stat.desc(weight.full.demogs$att.center1) 
stat.desc(weightbody$att.center1)
stat.desc(weightface$att.center1)

# t-tests
t.test(age.full.demogs$att.center1) 
t.test(dis.full.demogs$att.center1) 
t.test(race.full.demogs$att.center1) 
t.test(sex.full.demogs$att.center1) 
t.test(skin.full.demogs$att.center1) 
t.test(weight.full.demogs$att.center1) 
t.test(weightbody$att.center1)
t.test(weightface$att.center1)


# Cohens ds ---------
cohensD(age.full.demogs$att.center1) # 
cohensD(dis.full.demogs$att.center1) # 
cohensD(race.full.demogs$att.center1) # 
cohensD(sex.full.demogs$att.center1) # 
cohensD(skin.full.demogs$att.center1) # 
cohensD(weight.full.demogs$att.center1) # 
cohensD(weightbody$att.center1)
cohensD(weightface$att.center1)


cohens.explicit <- c(cohensD(age.full.demogs$att.center1), cohensD(skin.full.demogs$att.center1), cohensD(race.full.demogs$att.center1), 
                     cohensD(dis.full.demogs$att.center1), cohensD(weight.full.demogs$att.center1), cohensD(sex.full.demogs$att.center1))

# cohens ds confidence intervals
age.cohens.CI.exp <- bootES(age.full.demogs.sample1, R=10000, data.col="att.center1",
                            effect.type = "cohens.d")
age.cohens.CI.exp$bounds 
age.cohens.CI.exp$t0 - age.cohens.CI.exp$bounds[1] 


dis.cohens.CI.exp <- bootES(dis.full.demogs.sample1, R=10000, data.col="att.center1",
                            effect.type = "cohens.d")
dis.cohens.CI.exp$bounds  
dis.cohens.CI.exp$t0 - dis.cohens.CI.exp$bounds[1]

race.cohens.CI.exp <- bootES(race.full.demogs.sample1, R=10000, data.col="att.center1",
                             effect.type = "cohens.d")
race.cohens.CI.exp$bounds 
race.cohens.CI.exp$t0 - race.cohens.CI.exp$bounds[1]

sex.cohens.CI.exp <- bootES(sex.full.demogs.sample1, R=10000, data.col="att.center1",
                            effect.type = "cohens.d")
sex.cohens.CI.exp$bounds 
sex.cohens.CI.exp$t0 - sex.cohens.CI.exp$bounds[1]

skin.cohens.CI.exp <- bootES(skin.full.demogs.sample1, R=10000, data.col="att.center1",
                             effect.type = "cohens.d")
skin.cohens.CI.exp$bounds 
skin.cohens.CI.exp$t0 - skin.cohens.CI.exp$bounds[1]

weight.cohens.CI.exp <- bootES(weight.full.demogs.sample1, R=10000, data.col="att.center1",
                               effect.type = "cohens.d")
weight.cohens.CI.exp$bounds 
weight.cohens.CI.exp$t0 - weight.cohens.CI.exp$bounds[1]


cohens.SE.explicit <- as.vector(c(age.cohens.CI.exp$t0 - age.cohens.CI.exp$bounds[1],
                                  skin.cohens.CI.exp$t0 - skin.cohens.CI.exp$bounds[1],
                                  race.cohens.CI.exp$t0 - race.cohens.CI.exp$bounds[1],
                                  dis.cohens.CI.exp$t0 - dis.cohens.CI.exp$bounds[1],
                                  weight.cohens.CI.exp$t0 - weight.cohens.CI.exp$bounds[1],
                                  sex.cohens.CI.exp$t0 - sex.cohens.CI.exp$bounds[1]))

## Visualize distributions of explicit attitudes ----------
op <- par(mfrow = c(3,2))
hist(age.full.demogs$att.center1, main = "Distribution of Explicit Age Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(age.full.demogs$att.center1), col = "red", lwd = 3)
abline(v = mean(age.full.demogs$att.center1)+sd(age.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(age.full.demogs$att.center1)-sd(age.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
hist(skin.full.demogs$att.center1, main = "Distribution of Explicit Skin-Tone Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(skin.full.demogs$att.center1), col = "red", lwd = 3)
abline(v = mean(skin.full.demogs$att.center1)+sd(skin.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(skin.full.demogs$att.center1)-sd(skin.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
hist(race.full.demogs$att.center1, main = "Distribution of Explicit Race Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(race.full.demogs$att.center1), col = "red", lwd = 3)
abline(v = mean(race.full.demogs$att.center1)+sd(race.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(race.full.demogs$att.center1)-sd(race.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
hist(sex.full.demogs$att.center1, main = "Distribution of Explicit Sexuality Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(sex.full.demogs$att.center1), col = "red", lwd = 3)
abline(v = mean(sex.full.demogs$att.center1)+sd(sex.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(sex.full.demogs$att.center1)-sd(sex.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
hist(dis.full.demogs$att.center1, main = "Distribution of Explicit Disability Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(dis.full.demogs$att.center1), col = "red", lwd = 3)
abline(v = mean(dis.full.demogs$att.center1)+sd(dis.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(dis.full.demogs$att.center1)-sd(dis.full.demogs$att.center1), col = "blue", lwd = 2, lty = 2)
hist(weight.full.demogs$att.center1, main = "Distribution of Explicit Weight Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(weight.full.demogs$att.center1, na.rm = TRUE), col = "red", lwd = 3)
abline(v = mean(weight.full.demogs$att.center1, na.rm = TRUE)+sd(weight.full.demogs$att.center1, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
abline(v = mean(weight.full.demogs$att.center1, na.rm = TRUE)-sd(weight.full.demogs$att.center1, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
par(op)


## All in one boxplot 
x1.e <- age.full.demogs$att.center1
x2.e <- skin.full.demogs$att.center1
x3.e <- race.full.demogs$att.center1
x4.e <- sex.full.demogs$att.center1
x5.e <- dis.full.demogs$att.center1
x6.e <- weight.full.demogs$att.center1
boxplot(x1.e, x2.e, x3.e, x4.e, x5.e, x6.e, names=c("Age", "Skin-Tone", "Race", "Sexuality", "Disability", "Weight"), 
        outline = FALSE,
        main = "Median Explicit Attitudes",
        ylab = "Raw Preference Scores")

## Plots of implicit and explicit cohens ----------------
## first, create the dataset
explicit.implicit <- as.data.frame(c("Age", "Skin-Tone", "Race", "Disability", "Weight","Sexuality",
                                     "Age", "Skin-Tone", "Race", "Disability", "Weight", "Sexuality"))
explicit.implicit$impexp <- c(rep("Implicit", 6), rep("Explicit", 6))
explicit.implicit$cohens <- c(cohens.implicit, cohens.explicit)
explicit.implicit$se <- c(cohens.SE.implicit, cohens.SE.explicit)
colnames(explicit.implicit)[1] <- "attitude"
explicit.implicit$attitude <- factor(explicit.implicit$attitude, levels = c("Age", "Disability", "Weight", "Skin-Tone", "Race", "Sexuality"))

# now plot (with bars)
dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = explicit.implicit$cohens + explicit.implicit$se,
              ymin = explicit.implicit$cohens - explicit.implicit$se)

p <- ggplot(data = explicit.implicit, aes(x = factor(attitude), y = cohens,
                                          fill = factor(impexp)))
p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "", y = "Cohen's d") +
  scale_fill_discrete(name = "") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold", angle = 180),
        legend.position = "bottom",
        legend.text = element_text(size = 30))


# plot (with points)
limits <- aes(ymax = explicit.implicit$cohens + explicit.implicit$se,
              ymin = explicit.implicit$cohens - explicit.implicit$se)
p <- ggplot(data = explicit.implicit, 
            aes(x = factor(attitude), y = cohens, 
                colour = factor(impexp)))
p + geom_point(stat = "identity",
               position = position_dodge(0.2),
               size = 10) +
  geom_errorbar(limits, position = position_dodge(0.2),
                width = 0.50) +
  labs(x = "", y = "Cohen's d") +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30, angle = 180))+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 30),
        legend.title = element_blank())+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major.x = element_line(colour = "black", linetype = 3))+
  ylim(0, 1.3)

# Significance of difference between Implicit versus Explicit given by overlap of the 95% confidence intervals of the
# cohen's d effect sizes, from 10000 bootstrap replications



## CORRELATIONS of implicit and explicit attitudes ---------------------
cor.test(age.full.demogs$dscore, age.full.demogs$att.center1) 
cor.test(dis.full.demogs$dscore, dis.full.demogs$att.center1) 
cor.test(race.full.demogs$dscore, race.full.demogs$att.center1) 
cor.test(sex.full.demogs$dscore, sex.full.demogs$att.center1) 
cor.test(skin.full.demogs$dscore, skin.full.demogs$att.center1) 
cor.test(weight.full.demogs$dscore, weight.full.demogs$att.center1) 
cor.test(weightbody$dscore, weightbody$att.center1)
cor.test(weightface$dscore, weightface$att.center1)


