###################################################################
#  Reason defaults 
#  Code to reproduce results of Study 1a created by Shweta Desiraju
###################################################################

####load any required packages####
if(!require(confintr)){install.packages("confintr")}
library(confintr)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(lsr)){install.packages("lsr")}
library(lsr)
if(!require(readxl)){install.packages("readxl")}
library(readxl)

####Variable descriptions####
#StartDate: Date and time participant began study        
#EndDate: Date and time participant ended study 
#ResponseID: Random ID assigned to response
#FailedAC: Participant failed attention check prior to beginning study
#DupID: Did participant's worker ID appear more than once in the data (0: no, 1: yes)
#Information: Default condition (-1: forced choice, 0: standard default, 1: reason default) 
#AssignedPlan:IRA assigned to be the default (T: traditional, R: Roth)
#Change: Tax condition. Did tax rate increase or decrease at end of 20 year scenario (I: increase, D: decrease)
#Chosen: Which plan did participant choose (text: Traditional IRA, Roth IRA)
#ChoseRoth: Did participant choose the Roth IRA (0: no, 1: yes)
#ExpectedDecrease: Did participant expect their tax rate to decrease (0: no, 1: yes)
#Comp1change: Did participant correctly identify direction of simulation tax change (0: no, 1: yes)
#Comp2rate: Did participant correctly identify simulation tax rate change (0: no, 1: yes)
#PassedCC: Did participant pass both comprehension check questions (0, no: 1: yes)
#ConsistWTruth: Did participant choose the plan best for the true tax outcome at end of 20 year scenario (0: no, 1: yes)
#ConsistWExpec: Did participant choose the plan best for their expected tax outcome at end of 20 year scenario (0: no, 1: yes)
#GoodDefault: Did the default suggest the correct tax plan for the tax change one was assigned (0: no, 1: yes)
#SureChoseBestPlan: How sure was participant that they chose the best plan for them (Scale from 1, low, to 5, high)
#UnderstandWhen: How sure was participant that they understood when to choose each plan (Scale from 1, low, to 5, high)
#CouldExplain: How sure was participant that they could explain when one should choose each plan (Scale from 1, low, to 5, high)
#GoodDefaultBeliefs: Did the default suggest the correct tax plan for the tax change one believed would occur (0: no, 1: yes)
#Sexm1F2o3: Participant reported sex (1: Male, 2: Female, 3: non binary or other)
#Age: Participant age
#EnglishNL: Is English the participant's native language English

####Study 1a analyses in paper####
completes <- read_excel("Study 1a data.xls") #1153 prior to exclusions

File <- completes[which(completes$DupID==0 & completes$ConsistWTruth!=" "),] #938 

#Choice consistent with likely future tax rate for those correctly answering comprehension check questions
#74% of participants chose an IRA consistent with their assigned future tax rates (e.g., choosing a traditional IRA when they taxes were likely to decrease) in the reason default condition compared to 
#62% of participants in the standard default condition, χ2(1, N = 556) = 10.22, p = .001, and 
#63% of participants in the forced choice condition, χ2(1, N = 408) = 5.38, p = .020.
table(File$Information[File$PassedCC==1],File$ConsistWTruth[File$PassedCC==1]) #1:0.7434944 ; 0: 0.6167247 ;-1:0.6330935
chisq.test(File$Information[File$Information!=-1&File$PassedCC==1],File$ConsistWTruth[File$Information!=-1&File$PassedCC==1], correct = F) #X-squared = 10.222, df = 1, p-value = 0.001388
cramersv(chisq.test(File$Information[File$Information!=-1&File$PassedCC==1],File$ConsistWTruth[File$Information!=-1&File$PassedCC==1], correct = F)) #0.1355924

chisq.test(File$Information[File$Information!=0&File$PassedCC==1],File$ConsistWTruth[File$Information!=0&File$PassedCC==1], correct = F) #X-squared = 5.3802, df = 1, p-value = 0.02037
cramersv(chisq.test(File$Information[File$Information!=0&File$PassedCC==1],File$ConsistWTruth[File$Information!=0&File$PassedCC==1], correct = F)) #0.1148337

#Participants who were assigned to a “bad” default were significantly more likely to override this default in the reason default condition (68%) 
#than the standard default condition (48%), χ2(1, N = 277) = 11.55, p = .001,
table(File$Information[File$GoodDefault==0&File$PassedCC==1],File$ConsistWTruth[File$GoodDefault==0&File$PassedCC==1]) #0: 0.4822695; 1:0.6838235
chisq.test(File$Information[File$GoodDefault==0&File$PassedCC==1],File$ConsistWTruth[File$GoodDefault==0&File$PassedCC==1], correct = F) #X-squared = 11.554, df = 1, p-value = 0.000676
cramersv(chisq.test(File$Information[File$GoodDefault==0&File$PassedCC==1],File$ConsistWTruth[File$GoodDefault==0&File$PassedCC==1], correct = F)) #0.2042342

#while participants in the reason (80%) and standard (75%) default conditions were similarly likely to accept a “good” default, χ2(1, N = 279) =1.34, p = .248
table(File$Information[File$GoodDefault==1&File$PassedCC==1],File$ConsistWTruth[File$GoodDefault==1&File$PassedCC==1]) #0:  0.7465753, 1:0.8045113
chisq.test(File$Information[File$GoodDefault==1&File$PassedCC==1],File$ConsistWTruth[File$GoodDefault==1&File$PassedCC==1], correct = F) #1.3363, df = 1, p-value = 0.2477
cramersv(chisq.test(File$Information[File$GoodDefault==1&File$PassedCC==1],File$ConsistWTruth[File$GoodDefault==1&File$PassedCC==1], correct = F))#0.06920749

####Study 1a analyses in supplement####
#Choice consistent with beliefs for those correctly answering comprehension check questions
#76% of participants chose an IRA consistent with their beliefs about future tax rates (e.g., choosing a traditional IRA when they expected taxes to decrease) in the reason default condition compared to 
#65% of participants in the standard default condition, χ2(1, N = 556) = 7.59, p = 0.006 and
#63% of participants in the forced choice condition, χ2(1, N = 408) = 7.07, p = 0.008
table(File$Information[File$PassedCC==1],File$ConsistWExpec[File$PassedCC==1]) #1:0.7583643 ; 0:0.6515679 ;-1:0.6330935
chisq.test(File$Information[File$Information!=-1&File$PassedCC==1],File$ConsistWExpec[File$Information!=-1&File$PassedCC==1], correct = F) #X-squared = 7.5886, df = 1, p-value = 0.005874
cramersv(chisq.test(File$Information[File$Information!=-1&File$PassedCC==1],File$ConsistWExpec[File$Information!=-1&File$PassedCC==1], correct = F)) #0.116827

chisq.test(File$Information[File$Information!=0&File$PassedCC==1],File$ConsistWExpec[File$Information!=0&File$PassedCC==1], correct = F) #X-squared = 7.0678, df = 1, p-value = 0.007848
cramersv(chisq.test(File$Information[File$Information!=0&File$PassedCC==1],File$ConsistWExpec[File$Information!=0&File$PassedCC==1], correct = F)) #0.1316173

#Participants who were assigned to a “bad” default were significantly more likely to override this default in the reason default condition (70%) 
#than the standard default condition (52%), χ2(1, N = 283) = 9.41, p = .002,
table(File$Information[File$GoodDefaultBeliefs==0&File$PassedCC==1],File$ConsistWExpec[File$GoodDefaultBeliefs==0&File$PassedCC==1]) #0:0.5172414; 1:0.6956522
chisq.test(File$Information[File$GoodDefaultBeliefs==0&File$PassedCC==1],File$ConsistWExpec[File$GoodDefaultBeliefs==0&File$PassedCC==1], correct = F) #X-squared = 9.4116, df = 1, p-value = 0.002156
cramersv(chisq.test(File$Information[File$GoodDefaultBeliefs==0&File$PassedCC==1],File$ConsistWExpec[File$GoodDefaultBeliefs==0&File$PassedCC==1], correct = F)) #0.1823634

#while participants in the reason (78%) and standard (72%) default conditions were similarly likely to accept a “good” default, χ2(1, N = 273) = 0.55, p = 0.456
table(File$Information[File$GoodDefaultBeliefs==1&File$PassedCC==1],File$ConsistWExpec[File$GoodDefaultBeliefs==1&File$PassedCC==1]) #0:0.7887324, 1:0.8244275
chisq.test(File$Information[File$GoodDefaultBeliefs==1&File$PassedCC==1],File$ConsistWExpec[File$GoodDefaultBeliefs==1&File$PassedCC==1], correct = F) #X-squared = 0.55493, df = 1, p-value = 0.4563
cramersv(chisq.test(File$Information[File$GoodDefaultBeliefs==1&File$PassedCC==1],File$ConsistWExpec[File$GoodDefaultBeliefs==1&File$PassedCC==1], correct = F)) #0.04508572

#Choice consistent with likely future tax rate including those who did not pass comprehension check
#66% of participants chose an IRA consistent with their likely future tax rates (e.g., choosing a traditional IRA when it was likely for taxes to decrease) in the reason default condition compared to 
#58% of participants in the standard default condition, χ2(1, N = 751) = 5.94, p = .015, and 
#60% of participants in the forced choice condition, χ2(1, N = 563) = 2.37, p = .124
table(File$Information,File$ConsistWTruth) #1:0.6648936; 0:0.5786667; -1:0.5989305
chisq.test(File$Information[File$Information!=-1],File$ConsistWTruth[File$Information!=-1], correct = F) #X-squared = 5.9362, df = 1, p-value = 0.01483
cramersv(chisq.test(File$Information[File$Information!=-1],File$ConsistWTruth[File$Information!=-1], correct = F)) #0.0889068

chisq.test(File$Information[File$Information!=0],File$ConsistWTruth[File$Information!=0], correct = F) #X-squared = 2.3672, df = 1, p-value = 0.1239
cramersv(chisq.test(File$Information[File$Information!=0],File$ConsistWTruth[File$Information!=0], correct = F)) #0.06484309

#Participants who were assigned to a “bad” default were significantly more likely to override this default in the reason default condition (57%) 
#than the standard default condition (42%), χ2(1, N = 369) = 8.20, p = .004,
table(File$Information[File$GoodDefault==0],File$ConsistWTruth[File$GoodDefault==0]) #0:0.4216216; 1:0.5706522
chisq.test(File$Information[File$GoodDefault==0],File$ConsistWTruth[File$GoodDefault==0], correct = F) #X-squared = 8.196, df = 1, p-value = 0.004198
cramersv(chisq.test(File$Information[File$GoodDefault==0],File$ConsistWTruth[File$GoodDefault==0], correct = F)) #0.1490349

#while participants in the reason (76%) and standard (73%) default conditions were similarly likely to accept a “good” default, χ2(1, N = 382) = 0.28, p = .597
table(File$Information[File$GoodDefault==1],File$ConsistWTruth[File$GoodDefault==1]) #0:0.7315789, 1:0.7552083
chisq.test(File$Information[File$GoodDefault==1],File$ConsistWTruth[File$GoodDefault==1], correct = F) #X-squared = 0.27956, df = 1, p-value = 0.597
cramersv(chisq.test(File$Information[File$GoodDefault==1],File$ConsistWTruth[File$GoodDefault==1], correct = F)) #0.02705251

#Choice consistent with beliefs including those who did not pass comprehension check
#71% of participants chose an IRA consistent with their beliefs about future tax rates (e.g., choosing a traditional IRA when they expected taxes to decrease) in the reason default condition compared to 
#62% of participants in the standard default condition, χ2(1, N = 751) = 6.68, p = .010, and 
#60% of participants in the forced choice condition, χ2(1, N = 563) = 7.38, p = .007.
table(File$Information,File$ConsistWExpec) #1:0.712766 ; 0: 0.624 ;-1:0.5989305
chisq.test(File$Information[File$Information!=-1],File$ConsistWExpec[File$Information!=-1], correct = F) #X-squared = 6.675, df = 1, p-value = 0.009778
cramersv(chisq.test(File$Information[File$Information!=-1],File$ConsistWExpec[File$Information!=-1], correct = F)) #0.09427672

chisq.test(File$Information[File$Information!=0],File$ConsistWExpec[File$Information!=0], correct = F) #X-squared = 7.3766, df = 1, p-value = 0.006608
cramersv(chisq.test(File$Information[File$Information!=0],File$ConsistWExpec[File$Information!=0], correct = F)) #0.1144656

#Participants who were assigned to a “bad” default were significantly more likely to override this default in the reason default condition (61%) 
#than the standard default condition (47%), χ2(1, N = 380) = 8.21, p = .004,
table(File$Information[File$GoodDefaultBeliefs==0],File$ConsistWExpec[File$GoodDefaultBeliefs==0]) #0: 0.4680851; 1:0.6145833
chisq.test(File$Information[File$GoodDefaultBeliefs==0],File$ConsistWExpec[File$GoodDefaultBeliefs==0], correct = F) #X-squared = 8.2128, df = 1, p-value = 0.00416
cramersv(chisq.test(File$Information[File$GoodDefaultBeliefs==0],File$ConsistWExpec[File$GoodDefaultBeliefs==0], correct = F)) #0.1470123

#while participants in the reason (70%) and standard (67%) default conditions were similarly likely to accept a “good” default, χ2(1, N = 371) = 0.68, p = .409
table(File$Information[File$GoodDefaultBeliefs==1],File$ConsistWExpec[File$GoodDefaultBeliefs==1]) #0: 0.7807487, 1:0.8152174
chisq.test(File$Information[File$GoodDefaultBeliefs==1],File$ConsistWExpec[File$GoodDefaultBeliefs==1], correct = F) #X-squared = 0.68317, df = 1, p-value = 0.4085
cramersv(chisq.test(File$Information[File$GoodDefaultBeliefs==1],File$ConsistWExpec[File$GoodDefaultBeliefs==1], correct = F)) #0.04291197

#Three exploratory questions
#The first question asked participants to what extent they agreed with the statement “I am sure that I chose the best savings plan for me”,
#and we did not observe significant differences on this measure across conditions (Mreason = 3.92, Mstandard = 3.91, Mforcedchoice = 4.02).
File$Information <- factor(File$Information, levels = c(1,0,-1))
summary(lm(File$SureChoseBestPlan~as.factor(File$Information))) #Adjusted R-squared: -9.247e-05, F-statistic: 0.9567 on 2 and 935 DF,  p-value: 0.3845
etaSquared(lm(File$SureChoseBestPlan~as.factor(File$Information))) #0.002042198 partial eta sq

mean_se(File$SureChoseBestPlan[File$Information==1]) #3.925532
se_mean(File$SureChoseBestPlan[File$Information==1]) #0.05038257

mean_se(File$SureChoseBestPlan[File$Information==0]) #3.906667
se_mean(File$SureChoseBestPlan[File$Information==0]) #0.04848752

mean_se(File$SureChoseBestPlan[File$Information==-1]) #4.02139
se_mean(File$SureChoseBestPlan[File$Information==-1]) #0.06695236

t.test(File$SureChoseBestPlan[File$Information==1],File$SureChoseBestPlan[File$Information==-1], var.equal = TRUE) #t = -1.1193, df = 561, p-value = 0.2635
cohensD(File$SureChoseBestPlan[File$Information==1],File$SureChoseBestPlan[File$Information==-1]) #0.1001619

t.test(File$SureChoseBestPlan[File$Information==1],File$SureChoseBestPlan[File$Information==0], var.equal = TRUE) #t = 0.26979, df = 749, p-value = 0.7874 
cohensD(File$SureChoseBestPlan[File$Information==1],File$SureChoseBestPlan[File$Information==0]) #0.01968885

#The second question asked participants to what extent they agreed with the statement "I understand when someone should choose a Roth or Traditional IRA”, 
#and we observe significant differences on this measure across conditions (Mreason = 4.06, Mstandard = 3.79, Mforcedchoice = 4.03) 
summary(lm(File$UnderstandWhen~as.factor(File$Information))) #Adjusted R-squared:  0.01411, F-statistic: 7.704 on 2 and 935 DF,  p-value: 0.00048
#etaSquared(lm(File$UnderstandWhen~as.factor(File$Information))) #0.0162129 partial eta sq

mean_se(File$UnderstandWhen[File$Information==1]) #4.06383
se_mean(File$UnderstandWhen[File$Information==1]) #0.05111995

mean_se(File$UnderstandWhen[File$Information==0]) #3.792 
se_mean(File$UnderstandWhen[File$Information==0]) #0.05444754

mean_se(File$UnderstandWhen[File$Information==-1]) #4.032086
se_mean(File$UnderstandWhen[File$Information==-1]) #0.06692939

t.test(File$UnderstandWhen[File$Information==1],File$UnderstandWhen[File$Information==-1], var.equal = TRUE) #t = 0.36697, df = 561, p-value = 0.7138
cohensD(File$UnderstandWhen[File$Information==1],File$UnderstandWhen[File$Information==-1]) #0.03283722

t.test(File$UnderstandWhen[File$Information==1],File$UnderstandWhen[File$Information==0], var.equal = TRUE) #t = 3.64, df = 749, p-value = 0.0002914
cohensD(File$UnderstandWhen[File$Information==1],File$UnderstandWhen[File$Information==0]) #0.2656515

#The third question asked participants to what extent they agreed with the statement “I could explain why I chose the savings plan that I chose”,
#and we observe marginally significant differences on this measure across conditions (Mreason = 4.08, Mstandard = 3.96, Mforcedchoice = 4.10).
summary(lm(File$CouldExplain~as.factor(File$Information)))#Adjusted R-squared:  0.001759, F-statistic: 1.825 on 2 and 935 DF,  p-value: 0.1617
#etaSquared(lm(File$CouldExplain~as.factor(File$Information))) #0.003889411 partial eta sq

mean_se(File$CouldExplain[File$Information==1]) #4.082447
se_mean(File$CouldExplain[File$Information==1]) #0.05208042

mean_se(File$CouldExplain[File$Information==0]) #3.962667 
se_mean(File$CouldExplain[File$Information==0]) #0.05215344

mean_se(File$CouldExplain[File$Information==-1]) #4.101604
se_mean(File$CouldExplain[File$Information==-1]) #0.06889

t.test(File$CouldExplain[File$Information==1],File$CouldExplain[File$Information==-1], var.equal = TRUE) #t = -0.21671, df = 561, p-value = 0.8285
cohensD(File$CouldExplain[File$Information==1],File$CouldExplain[File$Information==-1]) #0.01939198

t.test(File$CouldExplain[File$Information==1],File$CouldExplain[File$Information==0], var.equal = TRUE) #t = 1.6251, df = 749, p-value = 0.1046
cohensD(File$CouldExplain[File$Information==1],File$CouldExplain[File$Information==0]) #0.1186046

####notes####
#We preregistered standard chi square tests and the Yate's continuity correction is not appropriate for our sample sizes. 
#https://www.uvm.edu/~statdhtx/StatPages/R/Chi-Square-Folder/chi-square-alternatives.html
