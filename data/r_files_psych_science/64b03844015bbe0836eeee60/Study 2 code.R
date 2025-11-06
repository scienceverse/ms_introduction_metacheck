###################################################################
#  Reason defaults 
#  Code to reproduce results of Study 2 created by Shweta Desiraju
###################################################################

####load any required packages####
if(!require(confintr)){install.packages("confintr")}
library(confintr)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(lsr)){install.packages("lsr")}
library(lsr)
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
#ScoreEng: Number of correct responses to English quiz, out of 10
#ScorePR: Number of correct responses to Parks and Rec quiz, out of 10
#Score: Number of correct responses to quiz, out of 10, either quiz
#BestTest: Did participants feel they had selected the best quiz for them (0: no, 1: yes)
#ConfidenceEng: How confident was participant in English grammar and vocabulary (1: not at all, 2: somewhat, 3: very)
#ConfidencePR: How confident was participant in Parks and Rec trivia (1: not at all, 2: somewhat, 3: very)
#SexM1F2: Participant reported sex (1: Male, 2: Female, 3: non binary)
#Age: Participant age
#EnglishFirstLangY1N2: Is English participant's first language (1: Yes, 2: No)
#DupID: Did participant's worker ID appear more than once in the data (0: no, 1: yes)
#Information: Default condition (-1: forced choice, 0: standard default, 1: reason default) 
#English: Was the English (the default) quiz labeled "a" or "b" (a: "a", b: "b")
#Chosen: Did participant choose quiz labeled "a" or "b" (a: "a", b: "b")
#ChoseEng: Did participant choose to take the English (the default) quiz (0: no, 1: yes)
#Switched: Did participant choose the quiz that was not the default (0: no, 1: yes)
#InfoFactorCoded:  Default condition (1: forced choice, 2: standard default, 3: reason default) 

####Study 2 analyses in paper####
completes <- read_excel("Study 2 data.xls") #1151 prior to exclusions
File <- completes[which(completes$DupID==0 & completes$Score!=" "),] #834

#Participants assigned to a reason default answered more questions correctly (M=5.17) and earned larger bonuses than 
#participants assigned to a forced choice (M=4.35), t(549) = 3.93, p < .001, or 
#standard default (M=4.45), t(558) = 4.01, p <. 001. 
mean_se(File$Score[File$Information==1]) #5.173285
se_mean(File$Score[File$Information==1]) #0.1409879

mean_se(File$Score[File$Information==-1]) #4.354015
se_mean(File$Score[File$Information==-1]) #0.1536809

mean_se(File$Score[File$Information==0]) #4.448763
se_mean(File$Score[File$Information==0]) #0.1136063

t.test(File$Score[File$Information==1],File$Score[File$Information==-1], var.equal = TRUE) #t = 3.93, df = 549, p-value = 9.578e-05
cohensD(File$Score[File$Information==1],File$Score[File$Information==-1]) #0.3348552

t.test(File$Score[File$Information==1],File$Score[File$Information==0], var.equal = TRUE) #t = 4.0102, df = 558, p-value = 6.893e-05
cohensD(File$Score[File$Information==1],File$Score[File$Information==0]) #0.3389417

t.test(File$Score[File$Information==0],File$Score[File$Information==-1], var.equal = TRUE) #t = 0.49801, df = 555, p-value = 0.6187
cohensD(File$Score[File$Information==0],File$Score[File$Information==-1]) #0.04220842

#Among participants who chose the alternative test, (53% in the forced choice condition, 15% in the standard default condition, and 20% in the reason default condition, χ2(2, N = 834) = 116.95, p < .001)
table(File$ChoseEng,File$Information) #choosing alternate: fc:53%, sd:15%, rd:20%
chisq.test(File$ChoseEng,File$Information) #X-squared = 116.95, df = 2, p-value < 2.2e-16
cramersv(chisq.test(File$ChoseEng,File$Information)) #0.3744726

#participants assigned to a reason default scored higher (M=7.62) than
#those assigned to a forced choice (M=4.03), t(199) = 7.38, p < .001, or 
#standard default (M=3.71) , t(95) = 6.42, p < .001. 
mean_se(File$Score[File$Information==1&File$ChoseEng==0]) #7.618182
se_mean(File$Score[File$Information==1&File$ChoseEng==0]) #0.4252081

mean_se(File$Score[File$Information==-1&File$ChoseEng==0]) #4.034247
se_mean(File$Score[File$Information==-1&File$ChoseEng==0]) #0.2514792

mean_se(File$Score[File$Information==0&File$ChoseEng==0]) #3.714286
se_mean(File$Score[File$Information==0&File$ChoseEng==0]) #0.4177893

t.test(File$Score[File$Information==1&File$ChoseEng==0],File$Score[File$Information==-1&File$ChoseEng==0], var.equal = TRUE) #t = 7.3782, df = 199, p-value = 4.256e-12
cohensD(File$Score[File$Information==1&File$ChoseEng==0],File$Score[File$Information==-1&File$ChoseEng==0]) #1.167326

t.test(File$Score[File$Information==1&File$ChoseEng==0],File$Score[File$Information==0&File$ChoseEng==0], var.equal = TRUE) #t = 6.4161, df = 95, p-value = 5.437e-09
cohensD(File$Score[File$Information==1&File$ChoseEng==0],File$Score[File$Information==0&File$ChoseEng==0]) #1.314782

#Participants who chose the default test performed similarly across all conditions, Mforced=4.72, Mstandard=4.58, Mreason=4.57, F(2, 588) = 0.38, p = .684. 
mean_se(File$Score[File$Information==-1&File$ChoseEng==1]) #4.71875
se_mean(File$Score[File$Information==-1&File$ChoseEng==1]) #0.156053

mean_se(File$Score[File$Information==0&File$ChoseEng==1]) #4.576763
se_mean(File$Score[File$Information==0&File$ChoseEng==1]) #0.1102328

mean_se(File$Score[File$Information==1&File$ChoseEng==1]) #4.567568
se_mean(File$Score[File$Information==1&File$ChoseEng==1]) #0.1080214

summary(lm(File$Score[File$ChoseEng==1]~(as.factor(File$Information[File$ChoseEng==1])))) #F-statistic: 0.3798 on 2 and 588 DF,  p-value: 0.6842
etaSquared(lm(File$Score[File$ChoseEng==1]~(as.factor(File$Information[File$ChoseEng==1])))) #partial eta sq: 0.001290161
           
#Cumulative distributions of participants’ scores across conditions
cdfrd <- ecdf(File$Score[File$Information==1])
cdfsd <- ecdf(File$Score[File$Information==0])
cdffc <- ecdf(File$Score[File$Information==-1])

plot(cdfrd, verticals=TRUE, do.points=FALSE,col='green',main="CDF of score on selected test by condition",
     xlab="Score")
plot(cdfsd, verticals=TRUE, do.points=FALSE, add=TRUE, col='red')
plot(cdffc, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')

chisq.test(File$BestTest,File$Information, correct = F) #X-squared = 73.999, df = 2, p-value < 2.2e-16
cramersv(chisq.test(File$BestTest,File$Information, correct = F)) #0.2978714
table(File$BestTest,File$Information) #fc: 47%, sd: 53%, rd: 81%
#More participants thought they had taken the best test for them in the reason default condition (81%), than those in the standard default condition (53%, χ2(1, N=560) = 48.70, p<0.001) 
#and forced choice condition (47%, χ2(1, N=551) = 66.70, p<0.001
#There was no significant difference in thinking one had taken the best test between the forced choice and standard default conditions (χ2(1, N=557) =1.73, p=0.188)
chisq.test(File$BestTest[File$Information!=-1],File$Information[File$Information!=-1], correct = F) #X-squared = 48.704, df = 1, p-value = 2.976e-12
cramersv(chisq.test(File$BestTest[File$Information!=-1],File$Information[File$Information!=-1], correct = F)) #0.2949101

chisq.test(File$BestTest[File$Information!=0],File$Information[File$Information!=0], correct = F) #X-squared = 66.701, df = 1, p-value = 3.16e-16
cramersv(chisq.test(File$BestTest[File$Information!=0],File$Information[File$Information!=0], correct = F)) #0.3479287

chisq.test(File$BestTest[File$Information!=1],File$Information[File$Information!=1], correct = F) #1.7276, df = 1, p-value = 0.1887
cramersv(chisq.test(File$BestTest[File$Information!=1],File$Information[File$Information!=1], correct = F)) #0.05569167

####Study 2 analyses in supplement####
table(File$ConfidenceEng) #very:10.79%, somewhat:67.02%, not:22.18%
table(File$ConfidencePR) #very:56.83%, somewhat:23.50%, not:19.66%

#For chart:
mean_se(File$Score[File$Information==-1&File$ChoseEng==1])
mean_se(File$Score[File$Information==-1&File$ChoseEng==0])

mean_se(File$Score[File$Information==0&File$ChoseEng==1])
mean_se(File$Score[File$Information==0&File$ChoseEng==0])

mean_se(File$Score[File$Information==1&File$ChoseEng==1])
mean_se(File$Score[File$Information==1&File$ChoseEng==0])

  
####notes####
#We preregistered standard chi square tests and the Yate's continuity correction is not appropriate for our sample sizes. 
#https://www.uvm.edu/~statdhtx/StatPages/R/Chi-Square-Folder/chi-square-alternatives.html
