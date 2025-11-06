###################################################################
#  Reason defaults 
#  Code to reproduce results of Study 1b created by Shweta Desiraju
###################################################################

####load any required packages####
if(!require(confintr)){install.packages("confintr")}
library(confintr)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(MASS)){install.packages("MASS")}
library(MASS)
if(!require(readxl)){install.packages("readxl")}
library(readxl)

####Variable descriptions####
#StartDate: Date and time participant began study        
#EndDate: Date and time participant ended study 
#ResponseID: Random ID assigned to response
#FailedAC: Participant failed attention check prior to beginning study
#DupID: Did participant's worker ID appear more than once in the data (0: no, 1: yes)
#Default: Did ps see a default (0: no, 1: yes)
#Message: Did ps see a reason message (0: no, 1: yes)
#ContrastDefault: Did ps see a default (contrast coded. -1: no, 1: yes)
#ContrastMessage: Did ps see a reason message (contrast coded. -1: no, 1: yes)
#first: Plan shown first for choice (T: traditional; R: Roth)
#NoDefaultFirst: Plan described first for reason message (T: traditional; R: Roth)
#Consistent: Did ps choose the Traditional IRA, the best plan for a likely tax decrease
#Comp2: Did p pass comprehension check question 1 (0, no: 1: yes)
#Comp1: Did p pass comprehension check question 2 (0, no: 1: yes)
#PassedCC: Did participant pass both comprehension check questions (0, no: 1: yes)
#Sexm1F2o3: Participant reported sex (1: Male, 2: Female, 3: non binary or other)
#Age: Participant age
#EnglishNative: Is English the participant's native language English

####Study 1b analyses in paper####
completes <- read_excel("Study 1b data.xls") #867 prior to exclusions 

FileCC <- completes[which(completes$DupID==0 & completes$Consistent!=" " & completes$PassedCC),] #611, excluding participants who failed either one of two comprehension check questions
File <- completes[which(completes$DupID==0 & completes$Consistent!=" "),] #766, including participants who failed either one of two comprehension check questions

#The logistic regression revealed that participants assigned to a bad default selected the savings plan consistent with their tax regime (i.e., the Traditional IRA) significantly less often (46%) than those assigned to no default at all (55%), ß = -0.20, z = -2.41 p = .016
#participants assigned to receive the reason message selected the advantageous Traditional IRA significantly more often (63%) than those assigned not to receive this message (38%), ß = 0.51, z = 6.05, p < .001
#these two main effects were not qualified by a significant interaction, ß = -0.09, z = -1.19, p = .236
summary(filemodCC <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = FileCC, family = "binomial"))
confint(filemodCC <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = FileCC, family = "binomial"))
#Call:
#  glm(formula = Consistent ~ ContrastDefault * ContrastMessage, 
#      family = "binomial", data = FileCC)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5488  -1.0211   0.8469   1.0795   1.4341  

#Coefficients:
#                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      0.02760    0.08426   0.328    0.743    
# ContrastDefault                 -0.20305    0.08426  -2.410    0.016 *  
# ContrastMessage                  0.51021    0.08426   6.055  1.4e-09 ***
# ContrastDefault:ContrastMessage -0.09993    0.08426  -1.186    0.236    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 847.01  on 610  degrees of freedom
#Residual deviance: 803.15  on 607  degrees of freedom
#AIC: 811.15

#Number of Fisher Scoring iterations: 4

table(FileCC$ContrastDefault,FileCC$Consistent) #less likely to be consistent if a default is present (46.0%) vs not (54.5%) 

table(FileCC$ContrastMessage,FileCC$Consistent) #more likely to be consistent if a reason message is present (62.6%) vs not (38.2%)


#Participants who received the bad default were significantly more likely to select the advantageous savings plan when they received the reason message (56%) than when they did not (36%), χ2(1, N = 305) = 12.38, p < .001
table(FileCC$ContrastMessage[FileCC$Default==1],FileCC$Consistent[FileCC$Default==1]) #More likely to be consistent if message present (55.8%) than not present (35.7%)
chisq.test(FileCC$ContrastMessage[FileCC$Default==1],FileCC$Consistent[FileCC$Default==1], correct = F) #X-squared = 12.383, df = 1, p-value = 0.0004333
cramersv(chisq.test(FileCC$ContrastMessage[FileCC$Default==1],FileCC$Consistent[FileCC$Default==1], correct = F)) #0.201494

####Study 1b analyses in supplement####
#Fig S1b
mean_se(FileCC$Consistent[FileCC$Message==0&FileCC$Default==0]) #m 0.40625 min 0.3673007 max 0.4451993
mean_se(FileCC$Consistent[FileCC$Message==0&FileCC$Default==1]) #m 0.3576159 min 0.3184814 max 0.3967504
mean_se(FileCC$Consistent[FileCC$Message==1&FileCC$Default==0]) #m 0.6986301 min 0.6605245 max 0.7367358
mean_se(FileCC$Consistent[FileCC$Message==1&FileCC$Default==1]) #m 0.5584416 min 0.518296 max 0.5985871

#The logistic regression revealed that participants assigned to a bad default selected the savings plan consistent with their tax regime (i.e., the Traditional IRA) significantly less often (41%) than those assigned to no default at all (52%), ß = -0.23, z = -3.04, p = .002 
#participants assigned to receive the reason message selected the advantageous Traditional IRA significantly more often (57%) than those assigned not to receive this message (37%), ß = 0.40, z = 5.39, p < .001
#These two main effects were not qualified by a significant interaction, ß = -0.07, z = -0.979, p = .327
summary(filemod <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = File, family = "binomial"))
confint(filemod <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = File, family = "binomial"))
#Call:
#  glm(formula = Consistent ~ ContrastDefault * ContrastMessage, 
#      family = "binomial", data = File)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4299  -1.0242  -0.9063   1.1865   1.4753  

#Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                     -0.12355    0.07440  -1.661  0.09679 .  
#ContrastDefault                 -0.22591    0.07440  -3.036  0.00239 ** 
#ContrastMessage                  0.40105    0.07440   5.390 7.03e-08 ***
#ContrastDefault:ContrastMessage -0.07287    0.07440  -0.979  0.32737    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 1058.9  on 765  degrees of freedom
#Residual deviance: 1019.2  on 762  degrees of freedom
#(25 observations deleted due to missingness)
#AIC: 1027.2

#Number of Fisher Scoring iterations: 4

table(File$ContrastDefault,File$Consistent) #less likely to be consistent if a default is present (41.4%) vs not (52.2%)

table(File$ContrastMessage,File$Consistent) #more likely to be consistent if a reason message is present (56.8%) vs not (37.3%)


#Participants who received the bad default were significantly more likely to select the advantageous savings plan when they received the reason message (49%) than when they did not (33%), χ2(1, N = 381) = 9.78, p = .002. 
table(File$ContrastMessage[File$Default==1],File$Consistent[File$Default==1]) #More likely to be consistent if message present (49.4%) than not present (33.6%)
chisq.test(File$ContrastMessage[File$Default==1],File$Consistent[File$Default==1], correct = F) #X-squared = 9.7815, df = 1, p-value = 0.001763
cramersv(chisq.test(File$ContrastMessage[File$Default==1],File$Consistent[File$Default==1], correct = F)) #0.1602287

####notes####
#We preregistered standard chi square tests and the Yate's continuity correction is not appropriate for our sample sizes. 
#https://www.uvm.edu/~statdhtx/StatPages/R/Chi-Square-Folder/chi-square-alternatives.html
