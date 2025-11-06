###################################################################
#  Reason defaults 
#  Code to reproduce results of Study 1c created by Shweta Desiraju
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
#Actext: text entered in attention check (correct answer, not case sensitive: book)
#Actime: time taken to respond to attention check
#DupID: Did participant's worker ID appear more than once in the data (0: no, 1: yes)
#Default: Did ps see a default (0: no, 1: yes)
#Message: Did ps see a reason message (0: no, 1: yes)
#ContrastDefault: Did ps see a default (contrast coded. -1: no, 1: yes)
#ContrastMessage: Did ps see a reason message (contrast coded. -1: no, 1: yes)
#first: Plan shown first for choice (T: traditional; R: Roth)
#Consistent: Did ps choose the Traditional IRA, the best plan for a likely tax decrease
#PassedCC: Did participant pass both comprehension check questions (0, no: 1: yes)
#Comp2: Did p pass comprehension check question 1 (0, no: 1: yes)
#Comp1: Did p pass comprehension check question 2 (0, no: 1: yes)
#NoDefaultFirst: Plan described first for reason message (T: traditional; R: Roth)
#Change: random actual tax change at end of simulation (increased, decreased)
#Sexm1F2o3: Participant reported sex (1: Male, 2: Female, 3: non binary or other)
#Age: Participant age
#EnglishNL: Is English the participant's native language English

####Study 1c analyses in paper####
completes <- read_excel("Study 1c data.xls") #863 prior to exclusions 

FileCC <- completes[which(completes$DupID==0 & completes$Consistent!=" " & completes$PassedCC),] #679, excluding participants who failed either one of two comprehension check questions
File <- completes[which(completes$DupID==0 & completes$Consistent!=" "),] #783, including participants who failed either one of two comprehension check questions

#The logistic regression revealed that participants assigned to a good default selected the savings plan consistent with their tax regime (i.e., the Roth IRA) significantly more often (93%) than those assigned to no default at all (88%), ß = 0.28, z = 2.03, p = .042
#participants who received the reason message selected the Roth IRA at a similar rate (91%) as those who did not receive this message (90%), ß = 0.01, z = 0.05, p = .961
#These two main effects were not qualified by a significant interaction, ß = -0.12, z = -0.91, p = .365
summary(filemodCC <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = FileCC, family = "binomial"))
confint(filemodCC <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = FileCC, family = "binomial"))
#Call:
#glm(formula = Consistent ~ ContrastDefault * ContrastMessage, 
#    family = "binomial", data = FileCC)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.3426   0.3647   0.4077   0.4717   0.5325  

#Coefficients:
#                                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                      2.286204   0.135310  16.896   <2e-16 ***
#ContrastDefault                  0.275278   0.135310   2.034   0.0419 *  
#ContrastMessage                  0.006672   0.135310   0.049   0.9607    
#ContrastDefault:ContrastMessage -0.122468   0.135310  -0.905   0.3654    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 428.58  on 678  degrees of freedom
#Residual deviance: 423.30  on 675  degrees of freedom
#AIC: 431.3

#Number of Fisher Scoring iterations: 5

table(FileCC$ContrastDefault,FileCC$Consistent) #directionally more likely to be consistent if a default is present (92.8%) vs not (88.1%)

table(FileCC$ContrastMessage,FileCC$Consistent) #similarly likely to be consistent if a reason message is present (90.7%) vs not (90.1%)


#participants in the standard default condition chose the advantageous Roth IRA more often (94%) than participants in the forced choice condition (87%), χ2(1, N = 345) = 4.47, p = .034
table(FileCC$ContrastDefault[FileCC$Message==0],FileCC$Consistent[FileCC$Message==0]) ##more consistent if default is present (93.56%) vs  not (86.78%)
chisq.test(FileCC$ContrastDefault[FileCC$Message==0],FileCC$Consistent[FileCC$Message==0], correct = F) #X-squared = 4.47, df = 1, p-value = 0.0345
cramersv(chisq.test(FileCC$ContrastDefault[FileCC$Message==0],FileCC$Consistent[FileCC$Message==0], correct = F)) #0.1138267

#participants in the standard default (94%) and reason default (92%) conditions selected the more advantageous Roth IRA at similar rates, χ2(1, N = 334) = 0.30, p = .585
table(FileCC$ContrastMessage[FileCC$Default==1],FileCC$Consistent[FileCC$Default==1]) #Similarly likely to be consistent if message present (92.0%) or not present (93.5%)
chisq.test(FileCC$ContrastMessage[FileCC$Default==1],FileCC$Consistent[FileCC$Default==1], correct = F) #X-squared = 0.2978, df = 1, p-value = 0.5853
cramersv(chisq.test(FileCC$ContrastMessage[FileCC$Default==1],FileCC$Consistent[FileCC$Default==1], correct = F)) #0.02986002

####Study 1c analyses in supplement####
#Fig S1c
mean_se(FileCC$Consistent[FileCC$Message==0&FileCC$Default==0]) #m 0.8678161 min 0.8420659 max 0.8935663
mean_se(FileCC$Consistent[FileCC$Message==0&FileCC$Default==1]) #m 0.9356725 min 0.9168561 max 0.9544889
mean_se(FileCC$Consistent[FileCC$Message==1&FileCC$Default==0]) #m 0.8947368 min 0.8711993 max 0.9182744
mean_se(FileCC$Consistent[FileCC$Message==1&FileCC$Default==1]) #m 0.9202454 min 0.8989605 max 0.9415303

#The logistic regression revealed that participants assigned to a good default selected the savings plan consistent with their tax regime (i.e., the Roth IRA) significantly more often (91%) than those assigned to no default at all (87%), ß = 0.24, z = 1.99, p = .046 
#participants who received the reason message selected the Roth IRA at a similar rate (89%) as those who did not receive this message (90%), ß = -0.07, z = -0.58, p = .565
#These two main effects were not qualified by a significant interaction, ß = -0.13, z = -1.12, p = .261
summary(filemod <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = File, family = "binomial"))
confint(filemod <- glm(Consistent ~ ContrastDefault * ContrastMessage, data = File, family = "binomial"))
#Call:
#glm(formula = Consistent ~ ContrastDefault * ContrastMessage, 
#    family = "binomial", data = File)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.2996   0.3840   0.4652   0.5125   0.5445  

#Coefficients:
#                                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                      2.13429    0.11808  18.075   <2e-16 ***
#ContrastDefault                  0.23545    0.11808   1.994   0.0461 *  
#ContrastMessage                 -0.06796    0.11808  -0.576   0.5649    
#ContrastDefault:ContrastMessage -0.13273    0.11808  -1.124   0.2610    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 537.9  on 782  degrees of freedom
#Residual deviance: 532.6  on 779  degrees of freedom
#(38 observations deleted due to missingness)
#AIC: 540.6

#Number of Fisher Scoring iterations: 5

table(File$ContrastDefault,File$Consistent) #marginally more likely to be consistent if a default is present (91.3%) vs not (86.9%)

table(File$ContrastMessage,File$Consistent) #similarly likely to be consistent with (88.7%) and without (89.5%) reason message

#participants in the standard default condition chose the advantageous Roth IRA more often (93%) than participants in the forced choice condition (86%), χ2(1, N = 393) = 4.68, p = .031
table(File$ContrastDefault[File$Message==0],File$Consistent[File$Message==0]) ##more consistent if default is present (92.89%) vs  not (86.22%)
chisq.test(File$ContrastDefault[File$Message==0],File$Consistent[File$Message==0], correct = F) #X-squared = 4.6763, df = 1, p-value = 0.03058
cramersv(chisq.test(File$ContrastDefault[File$Message==0],File$Consistent[File$Message==0], correct = F)) #0.1090819

#participants in the standard default (93%) and reason default (90%) conditions selected the more advantageous Roth IRA at similar rates, χ2(1, N = 334) = 1.23, p = .268
table(File$ContrastMessage[File$Default==1],File$Consistent[File$Default==1]) #Similarly likely to be consistent if message present (89.7%) or not present (92.8%)
chisq.test(File$ContrastMessage[File$Default==1],File$Consistent[File$Default==1], correct = F) #X-squared = 1.2274, df = 1, p-value = 0.2679
cramersv(chisq.test(File$ContrastMessage[File$Default==1],File$Consistent[File$Default==1], correct = F)) #0.05595694

####notes####
#We preregistered standard chi square tests and the Yate's continuity correction is not appropriate for our sample sizes. 
#https://www.uvm.edu/~statdhtx/StatPages/R/Chi-Square-Folder/chi-square-alternatives.html
