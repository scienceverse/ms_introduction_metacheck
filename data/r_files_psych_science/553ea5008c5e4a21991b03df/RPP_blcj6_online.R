################################################################################
##################          REPLICATION R CODES               ##################
################################################################################
#
##@@ INSTRUCTIONS @@##
# Please do not remove any comments from this file, but feel free to add as many
# comments to R code as you like!
#
# Replace all ---USER INPUT--- sections with relevant input (including the - 
# symbols).
#
# When you are done, save this file as RPP_YYYY.R, where YYYY is the OSF code of 
# the project.
# e.g., The first study, Tracing attention and the activation...., should be
# called RPP_qwkum.R (since https://osf.io/qwkum/ is the project site)
#
# For any questions, please contact me at sacha.epskamp@gmail.com
#
##@@ GENERAL INFORMATION @@##
#@ Study Title: The best men are (not always) already taken: Female preference for single versus attached males depends on conception risk.
#@ Coder name: Felix Henninger
#@ Coder e-mail: felix.henninger@web.de
#@ Type of statistic: F
#@ Type of effect-size: None reported, using f which was used in the replication proposal
#@ OSF link project: https://osf.io/blcj6/
#@ OSF link replication report: https://osf.io/g358q/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library("car")
library('foreign')
library('dplyr')
library('reshape2')
library('afex')
library('heplots')
  
##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.

# Load data from the OSF
# Cleaned Lab Data + Demographic.sav 
file <- getOSFfile('https://osf.io/him5e/')
data <- data.frame(read.spss(file))
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.

# Rename a pesky column whose name is imported incorrectly
# ('Participant#' in the original data, 'Participant.' after import)
colnames(data)[1] <- 'Participant'
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object

# To run the analysis, the data needs to be converted into a long form
# (in SPSS, repeated measures are in seperate columns, while R expects
# them to be in distinct rows)
data.glm <- data %>%
  # Select only the variables necessary for the analysis to keep
  # the dataset manageable
  dplyr::select(one_of('Participant', 'conceptionrisk', 'haspartner', 'singleavg', 'attachedavg')) %>%
  # Use melt from reshape2 to split the ratings for single and 'attached'
  # targets into seperate rows. The variable 'target_relationship_status'
  # indicates the relationship status of the stimulus men, and 
  # 'attractiveness_rating' the assigned ratings, averaged across multiple
  # stimuli.
  melt(
    measure.vars=c('singleavg', 'attachedavg'), 
    value.name='attractiveness_rating',
    variable.name='target_relationship_status'
    )

# Recode the targets' relationship status in a slightly more legible way
data.glm$target_relationship_status <- car::recode(
  data.glm$target_relationship_status,
  "'singleavg'='Single'; 'attachedavg'='Attached'"
  )
  
# Run a GLM on the thusly preprated data
test <- ez.glm(
  data=data.glm,
  dv='attractiveness_rating',
  # Design factors:
  # - relationship status of the (male) target
  #   (single or attached)
  # - relationship status of the (female) participant
  # - participant conception risk
  #   (based on menstrual cycle)
  within='target_relationship_status',
  between=c('conceptionrisk', 'haspartner'),
  observed=c('conceptionrisk', 'haspartner'),
  # Connected observations are indicated by the participant id
  id='Participant',
  return='Anova'
  )

# Output the raw ANOVA summary available via the car package
test.summary.car <- summary(test)

print(test.summary.car)

# # Format the GLM constructed above into a nice ANOVA table
# test.summary.nice <- nice.anova(test, 
#   # Include partial eta squared as effect size to be comparable
#   # to the SPSS output
#   es=c('pes', 'ges'), 
#   # Indentify observed variables as such, so that a generalized
#   # eta squared can be computed
#   observed=c('conceptionrisk', 'haspartner')
#   )

# print(test.summary.nice)

# Find the row of the test output that corresponds to the hypothesis of interest
# ... for the car package output
# test.hypothesis_row.car <- which.names(
#   'conceptionrisk:haspartner:target_relationship_status', 
#   rownames(test.summary.car$univariate.tests)
#   )
# # ... for afex' nice output
# test.hypothesis_row.nice <- which.names(
#   'conceptionrisk:haspartner:target_relationship_status',
#   test.summary.nice$Effect
#   )

##@@ STATISTIC @@##
test.summary.car$univariate.tests[test.hypothesis_row.car, 'F']

##@@ P-VALUE @@##
test.summary.car$univariate.tests[test.hypothesis_row.car, 'Pr(>F)']
  
##@@ SAMPLE SIZE @@##
length(unique(data.glm$Participant))

##@@ EFFECT SIZE @@##
# Finally, output the effect size as partial eta squared
# (Generalized eta squared, which has many advantages,
# is available in test.summary.nice, but we use pes here
# to compute the f effect size in the next step)
test.pes = etasq(test)[test.hypothesis_row.car,]

# Compute f from eta squared, using the transformation formular provided
# in the G*Power manual (downloaded from http://gpower.hhu.de/, 
# p.27 at the time of writing)
sqrt(test.pes / (1-test.pes))
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: None