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
#@ Study Title: Pacton (Attention-based associative account)
#@ Coder name: Daan van Renswoude
#@ Coder e-mail: d.r.vanrenswoude@uva.nl
#@ Type of statistic: F
#@ Type of effect-size: partial eta squared
#@ OSF link project: https://osf.io/vmz2e/
#@ OSF link replication report: https://osf.io/asn7w/
#
##@@ REQUIRED PACKAGES @@##
library("httr")
library("RCurl")
source("http://sachaepskamp.com/files/OSF/getOSFfile.R") # the getOSFfile function
library('afex')

##@@ DATA LOADING @@##
#@ NOTE: Data must be loaded from OSF directly and NOT rely on any local files.
file <- getOSFfile('https://osf.io/sbdg6/')
PP_data <- read.table(file, header = T, sep="\t")
  
##@@ DATA MANIPULATION @@##
#@ NOTE: Include here ALL differences between OSF data and data used in analysis
#@ TIP: You will want to learn all about dplyr for manipulating data.
PP_data$targets <- ifelse(PP_data$targets == 29, c("029"), c("158"))

# Prepare aggregated dataset for confirmatory analysis
attach(PP_data)
PP_data_aggr_dependency <- aggregate(PP_data, by=list(Subject, digit_combination, targets, dependency), FUN=mean)
detach(PP_data)
PP_data_aggr_dependency <- PP_data_aggr_dependency[c(1:4,20)]
names(PP_data_aggr_dependency) <- c("Subject","digit_combination", "targets", "dependency", "Recognition.ACC" )
PP_data_aggr_dependency$Recognition.ACC <- PP_data_aggr_dependency$Recognition.ACC * 100
  
##@@ DATA ANLAYSIS @@##
#@ NOTE: Include a print or summary call on the resulting object
model.aov_dep <- aov_car(Recognition.ACC ~ digit_combination*targets + Error(Subject/dependency), data = PP_data_aggr_dependency, return = "nice",anova_table = list(es = "pes"))
model.aov_dep

##@@ STATISTIC @@##
# result <- nice.anova(model.aov_dep, es = "pes")
result <- model.aov_dep
# remove the significant stars (***) from the string
as.numeric(unlist(strsplit(result$F[4], ' '))[1])

##@@ P-VALUE @@##
as.numeric(result$p.value[4])
  
##@@ SAMPLE SIZE @@##
length(unique(PP_data$Subject))

##@@ EFFECT SIZE @@##
as.numeric(result$pes[4])
  
##@@ AGREEMENT WITH AUTHORS @@##
TRUE
  
#@ Reason disagreement: ---ENTER WHY YOU BELIEVE RESULTS DISAGREE (ONLY IF RELEVANT)---