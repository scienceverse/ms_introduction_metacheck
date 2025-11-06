###################################################################
#  Reason defaults 
#  Code to reproduce results of Study 3 created by Shweta Desiraju
###################################################################

####load any required packages####
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(lsr)){install.packages("lsr")}
library(lsr)
if(!require(psych)){install.packages("psych")}
library(psych)
if(!require(readxl)){install.packages("readxl")}
library(readxl)

####Variable descriptions####
#StartDate: Date and time participant began study        
#EndDate: Date and time participant ended study
#RecordedDate: Date and time response was recorded by Qualtrics
#ResponseID: Random ID assigned to response
#FailedAC: Participant failed attention check prior to beginning study
#Actext: text entered in attention check (correct answer, not case sensitive: book)
#Actime: time taken to respond to attention check
#Info1, Info2, Info3: Default condition (-1: forced choice, 0: standard default, 1: reason default), for 1st, 2nd, or 3rd choice set up 
#UndClear1, UndClear2, UndClear3: It is clear why someone should choose each of the options (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up     
#UndMatch1, UndMatch2, UndMatch3: It is clear how the options match up with different preferences (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#EffEasy1, EffEasy2, EffEasy3: Decision makers will have an easier time making a decision when the options are presented this way (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#EffEffort1, EffEffort2, EffEffort3: The way the options are presented reduces the effort needed to make a good decision (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#RecRec1, RecRec2, RecRec3: The way the decision is presented makes a strong recommendation (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#RecFavor1, RecFavor2, RecFavor3: The people who set up this decision favor one option over another (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#StatDef1, StatDef2, StatDef3: One of the options is the status quo or default (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#StatNorm1, StatNorm2, StatNorm3: Choosing one of the options is against the norm (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#AltFrame1, AltFrame2, AltFrame3: The way the choice is presented will lead everyone to make the same decision (reverse coded) (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#AltMinority1, AltMinority2, AltMinority3: The way the choice is presented will help people who have a minority preference (i.e. a preference opposite of the majority) to make the better choice for themselves (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#AutThemself1, AutThemself2, AutThemself3: Decision makers will feel like they are making this decision for themselves (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#AutMadeFor1, AutMadeFor2, AutMadeFor3: This decision is being made for decision makers to some extent (reverse coded) (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#TransClear1, TransClear2, TransClear3: It is clear why the people who set up the decision presented the options in the way they did (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#TransThinking1, TransThinking2, TransThinking3: The thinking of the people who set up the decision is transparent (1: strongly disagree to 5: strongly agree), for 1st, 2nd, or 3rd choice set up       
#SexM1F2O3: Participant reported sex (1: Male, 2: Female, 3: non binary)            
#Age: Participant age   
#EnglishFLY1N2: Is English participant's first language (1: Yes, 2: No)        
#DupID: Did participant's worker ID appear more than once in the data (0: no, 1: yes)
#Context: Context in which individuals judged choice set up (Airb: vehicle feature, CFL: bulb type, Fin: savings plan, Food: side dish, Med: organ donation status, Privacy: cookie setting)        
#context_text: text description of context        
#AssignedDefault: text description of default option 
#AssignedDefaultFirst: Was the default option displayed first (0: no, 1: yes)
#first: text of option displayed first                 
#second: text of option displayed second                 
#reason_text1: recommendation text, specific to assigned context       
#reason_text2: why to choose alternative option, specific to assigned context          
#FCUndClear: Response to UndClear for the forced choice choice set up          
#FCUndMatch: Response to UndMatch for the forced choice choice set up              
#FCEffEasy: Response to EffEasy for the forced choice choice set up              
#FCEffEffort: Response to EffEffort for the forced choice choice set up           
#FCRecRec: Response to RecRec for the forced choice choice set up              
#FCRecFavor: Response to RecFavor for the forced choice choice set up           
#FCStatDef: Response to StatDef for the forced choice choice set up             
#FCStatNorm: Response to StatNorm for the forced choice choice set up            
#FCAltFrameRC: Response to AltFrameRC for the forced choice choice set up          
#FCAltMinority: Response to AltMinority for the forced choice choice set up         
#FCAutThemself: Response to AutThemself for the forced choice choice set up        
#FCAutMadeForRC: Response to AutMadeForRC for the forced choice choice set up        
#FCTransClear: Response to TransClear for the forced choice choice set up          
#FCTransThinking: Response to TransThinking for the forced choice choice set up       
#SDUndClear: Response to UndClear for the standard default choice set up            
#SDUndMatch: Response to UndClear for the standard default choice set up            
#SDEffEasy: Response to EffEasy for the standard default choice set up              
#SDEffEffort: Response to EffEffort for the standard default choice set up            
#SDRecRec: Response to RecRec for the standard default choice set up               
#SDRecFavor: Response to RecFavor for the standard default choice set up             
#SDStatDef: Response to StatDef for the standard default choice set up             
#SDStatNorm: Response to StatNorm for the standard default choice set up             
#SDAltFrameRC: Response to AltFrameRC for the standard default choice set up           
#SDAltMinority: Response to AltMinority for the standard default choice set up          
#SDAutThemself: Response to AutThemself for the standard default choice set up          
#SDAutMadeForRC: Response to AutMadeForRC for the standard default choice set up        
#SDTransClear: Response to TransClear for the standard default choice set up           
#SDTransThinking: Response to TransThinking for the standard default choice set up        
#RDUndClear: Response to UndClear for the reason default choice set up             
#RDUndMatch: Response to UndClear for the reason default choice set up             
#RDEffEasy: Response to EffEasy for the reason default choice set up             
#RDEffEffort: Response to EffEffort for the reason default choice set up            
#RDRecRec: Response to RecRec for the reason default choice set up               
#RDRecFavor: Response to RecFavor for the reason default choice set up             
#RDStatDef: Response to StatDef for the reason default choice set up              
#RDStatNorm: Response to StatNorm for the reason default choice set up            
#RDAltFrameRC: Response to AltFrameRC for the reason default choice set up           
#RDAltMinority: Response to AltMinority for the reason default choice set up          
#RDAutThemself: Response to AutThemself for the reason default choice set up          
#RDAutMadeForRC: Response to AutMadeForRC for the reason default choice set up         
#RDTransClear: Response to TransClear for the reason default choice set up          
#RDTransThinking: Response to TransThinking for the reason default choice set up        
#FCUnd: Averaged response to Und measures for the forced choice condition                
#FCEff: Averaged response to Eff measures for the forced choice condition                  
#FCRec: Averaged response to Rec measures for the forced choice condition                  
#FCStat: Averaged response to Stat measures for the forced choice condition                
#FCAlt: Averaged response to Alt measures for the forced choice condition                  
#FCAut: Averaged response to Aut measures for the forced choice condition                  
#FCTrans: Averaged response to Und measures for the forced choice condition                
#SDUnd: Averaged response to Trans measures for the standard default condition                  
#SDEff: Averaged response to Eff measures for the standard default condition                 
#SDRec: Averaged response to Rec measures for the standard default condition                  
#SDStat: Averaged response to Stat measures for the standard default condition                 
#SDAlt: Averaged response to Alt measures for the standard default condition                  
#SDAut: Averaged response to Aut measures for the standard default condition                  
#SDTrans: Averaged response to Trans measures for the standard default condition               
#RDUnd: Averaged response to Und measures for the reason default condition                  
#RDEff: Averaged response to Eff measures for the reason default condition                  
#RDRec: Averaged response to Rec measures for the reason default condition                  
#RDStat: Averaged response to Stat measures for the reason default condition                 
#RDAlt: Averaged response to Alt measures for the reason default condition                 
#RDAut: Averaged response to Aut measures for the reason default condition                  
#RDTrans: Averaged response to Trans measures for the reason default condition      

####Study 3 analyses in paper####
completes <- read_excel("Study 3 data.xls") #1059 prior to exclusions

File <- completes[which(completes$DupID==0),] #992 #(N=889 complete responses to all main dvs, Ns for main dvs also listed below)

#Participants reported that reason defaults promote greater
#understanding of the options, transparency, and decision ease regarding the thinking of the choice architect than standard defaults and forced choices (all ps <.001). 
mean_se(File$FCUnd) #2.788654 
mean_se(File$SDUnd) #2.888639
mean_se(File$RDUnd) #4.020694

sd(File$FCUnd, na.rm = TRUE)  
sd(File$SDUnd, na.rm = TRUE) 
sd(File$RDUnd, na.rm = TRUE)

nrow(filter(File,FCUnd != " ")) #899 #counts
nrow(filter(File,SDUnd != " ")) #889
nrow(filter(File,RDUnd != " ")) #894

t.test(File$RDUnd,File$SDUnd, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 24.306, df = 879, p-value < 2.2e-16
t.test(File$RDUnd,File$FCUnd, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 23.833, df = 883, p-value < 2.2e-16
t.test(File$SDUnd,File$FCUnd, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 2.4196, df = 882, p-value = 0.01574

cohensD(File$RDUnd,File$SDUnd) #1.096942
cohensD(File$RDUnd,File$FCUnd) #1.155476
cohensD(File$SDUnd,File$FCUnd) #0.08380307


mean_se(File$FCTrans) #2.871524
mean_se(File$SDTrans) #3.24072
mean_se(File$RDTrans) #3.86689

sd(File$FCTrans, na.rm = TRUE)
sd(File$SDTrans, na.rm = TRUE)
sd(File$RDTrans, na.rm = TRUE)

nrow(filter(File,FCTrans != " ")) #899
nrow(filter(File,SDTrans != " ")) #889
nrow(filter(File,RDTrans != " ")) #894

t.test(File$RDTrans,File$SDTrans, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 14.013, df = 879, p-value < 2.2e-16
t.test(File$RDTrans,File$FCTrans, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 20.503, df = 883, p-value < 2.2e-16
t.test(File$SDTrans,File$FCTrans, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 7.8485, df = 882, p-value = 1.217e-14

cohensD(File$RDTrans,File$SDTrans) #0.6008692
cohensD(File$RDTrans,File$FCTrans) #0.9828061
cohensD(File$SDTrans,File$FCTrans) #0.3242187


mean_se(File$FCEff) #2.698554
mean_se(File$SDEff) #3.239033
mean_se(File$RDEff) #3.572707

sd(File$FCEff, na.rm = TRUE)
sd(File$SDEff, na.rm = TRUE)
sd(File$RDEff, na.rm = TRUE)

nrow(filter(File,FCEff != " ")) #899
nrow(filter(File,SDEff != " ")) #889
nrow(filter(File,RDEff != " ")) #894

t.test(File$RDEff,File$SDEff, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 8.5798, df = 879, p-value < 2.2e-16
t.test(File$RDEff,File$FCEff, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 17.012, df = 883, p-value < 2.2e-16
t.test(File$SDEff,File$FCEff, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 11.625, df = 882, p-value < 2.2e-16

cohensD(File$RDEff,File$SDEff) #0.3317492
cohensD(File$RDEff,File$FCEff) #0.8174507
cohensD(File$SDEff,File$FCEff) #0.5009155


#participants reported that reason defaults fell between forced choices and standard defaults regarding the 
#strength of the recommendation, the autonomy of the decision maker, the extent to which one option was the status quo, and the extent to which choosing the alternative was encouraged (all ps <.001)
mean_se(File$FCRec) #2.088988
mean_se(File$SDRec) #4.282902
mean_se(File$RDRec) #3.889821

sd(File$FCRec, na.rm = TRUE)
sd(File$SDRec, na.rm = TRUE)
sd(File$RDRec, na.rm = TRUE)

nrow(filter(File,FCRec != " ")) #899
nrow(filter(File,SDRec != " ")) #889
nrow(filter(File,RDRec != " ")) #894

t.test(File$RDRec,File$SDRec, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = -10.116, df = 879, p-value < 2.2e-16
t.test(File$RDRec,File$FCRec, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 34.572, df = 883, p-value < 2.2e-16
t.test(File$SDRec,File$FCRec, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 41.649, df = 882, p-value < 2.2e-16

cohensD(File$RDRec,File$SDRec) #0.4258436
cohensD(File$RDRec,File$FCRec) #1.684558
cohensD(File$SDRec,File$FCRec) #2.1313


mean_se(File$FCAut) #4.00723
mean_se(File$SDAut) #2.590551
mean_se(File$RDAut) #3.042506

sd(File$FCAut, na.rm = TRUE)
sd(File$SDAut, na.rm = TRUE)
sd(File$RDAut, na.rm = TRUE)

nrow(filter(File,FCAut != " ")) #899
nrow(filter(File,SDAut != " ")) #889
nrow(filter(File,RDAut != " ")) #894

t.test(File$RDAut,File$SDAut, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 11.921, df = 879, p-value < 2.2e-16
t.test(File$RDAut,File$FCAut, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = -22.382, df = 883, p-value < 2.2e-16
t.test(File$SDAut,File$FCAut, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = -30.338, df = 882, p-value < 2.2e-16

cohensD(File$RDAut,File$SDAut) #0.4910257
cohensD(File$RDAut,File$FCAut) #1.01622
cohensD(File$SDAut,File$FCAut) #1.526054


mean_se(File$FCStat) #2.228031
mean_se(File$SDStat) #3.574241
mean_se(File$RDStat) #3.29698

sd(File$FCStat, na.rm = TRUE)
sd(File$SDStat, na.rm = TRUE)
sd(File$RDStat, na.rm = TRUE)

nrow(filter(File,FCStat != " ")) #899
nrow(filter(File,SDStat != " ")) #889
nrow(filter(File,RDStat != " ")) #894

t.test(File$RDStat,File$SDStat, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = -8.476, df = 879, p-value < 2.2e-16
t.test(File$RDStat,File$FCStat, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 23.96, df = 883, p-value < 2.2e-16
t.test(File$SDStat,File$FCStat, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 29.324, df = 882, p-value < 2.2e-16

cohensD(File$RDStat,File$SDStat) #0.2820115
cohensD(File$RDStat,File$FCStat) #1.045663
cohensD(File$SDStat,File$FCStat) #1.319033


mean_se(File$FCAlt) #3.548943
mean_se(File$SDAlt) #2.885827
mean_se(File$RDAlt) #3.420582

sd(File$FCAlt, na.rm = TRUE)
sd(File$SDAlt, na.rm = TRUE)
sd(File$RDAlt, na.rm = TRUE)

nrow(filter(File,FCAlt != " ")) #899
nrow(filter(File,SDAlt != " ")) #889
nrow(filter(File,RDAlt != " ")) #894

t.test(File$RDAlt,File$SDAlt, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = 14.851, df = 879, p-value < 2.2e-16
t.test(File$RDAlt,File$FCAlt, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = -3.5351, df = 883, p-value = 0.0004288
t.test(File$SDAlt,File$FCAlt, paired = TRUE, alternative = "two.sided", var.equal = TRUE) #t = -17.423, df = 882, p-value < 2.2e-16

cohensD(File$RDAlt,File$SDAlt) #0.6337576
cohensD(File$RDAlt,File$FCAlt) #0.159209
cohensD(File$SDAlt,File$FCAlt) #0.845915


####Study 3 analyses in supplement####
#Reliability, alphas
File$Und1Avg <- (File$UndClear1+File$UndClear2+File$UndClear3)/3
File$Und2Avg <- (File$UndMatch1+File$UndMatch2+File$UndMatch3)/3
  
File$Eff1Avg <- (File$EffEasy1+File$EffEasy2+File$EffEasy3)/3
File$Eff2Avg <- (File$EffEffort1+File$EffEffort2+File$EffEffort3)/3

File$Rec1Avg <- (File$RecRec1+File$RecRec2+File$RecRec3)/3
File$Rec2Avg <- (File$RecFavor1+File$RecFavor2+File$RecFavor3)/3

File$Norm1Avg <- (File$StatDef1+File$StatDef2+File$StatDef3)/3
File$Norm2Avg <- (File$StatNorm1+File$StatNorm2+File$StatNorm3)/3

File$Alt1Avg <- ((6-File$AltFrame1)+(6-File$AltFrame2)+(6-File$AltFrame3))/3 ##alt 1 is rev coded
File$Alt2Avg <- (File$AltMinority1+File$AltMinority2+File$AltMinority3)/3

File$Aut1Avg <- (File$AutThemself1+File$AutThemself2+File$AutThemself3)/3
File$Aut2Avg <- ((6-File$AutMadeFor1)+(6-File$AutMadeFor2)+(6-File$AutMadeFor3))/3 #aut 2 is rev coded

File$Tran1Avg <- (File$TransClear1+File$TransClear2+File$TransClear3)/3
File$Tran2Avg <- (File$TransThinking1+File$TransThinking2+File$TransThinking3)/3

#The reliability of the aggregate measure for understanding the reasons for choosing each option was high 
F1 <- data.frame(File$Und1Avg,File$Und2Avg) 
alpha(F1) #0.8

#The reliability of the aggregate measure for making the decision easier was high
F2 <- data.frame(File$Eff1Avg,File$Eff2Avg)
alpha(F2) #0.74

#The reliability of the aggregate measure for how strongly an option was recommended was high 
F3 <- data.frame(File$Rec1Avg,File$Rec2Avg)
alpha(F3) #0.7

#The reliability of the aggregate measure for whether one option was the norm was sufficient
F4 <- data.frame(File$Norm1Avg,File$Norm2Avg)
alpha(F4) #0.63

#The reliability of the aggregate measure for whether choosing the alternative is encouraged was low
F5 <- data.frame(File$Alt1Avg,File$Alt2Avg) #alt 1 is rev coded
alpha(F5) #-0.56

#The reliability of the aggregate measure for whether decision makers have autonomy was low
F6 <- data.frame(File$Aut1Avg,File$Aut2Avg) #aut 2 is rev coded
alpha(F6) #0.25

#The reliability of the aggregate measure for whether the choice architect intention was transparent was high
F7 <- data.frame(File$Tran1Avg,File$Tran2Avg)
alpha(F7) #0.74


#The Holm–Bonferroni method was used to correct for multiple comparisons
ps<- c(2.2e-16, 2.2e-16, 2.2e-16, 2.2e-16,
       2.2e-16, 2.2e-16, 2.2e-16, 2.2e-16,
       2.2e-16, 2.2e-16, 2.2e-16, 2.2e-16,
       2.2e-16, 2.2e-16, 2.2e-16, 2.2e-16,
       2.2e-16, 2.2e-16,
       0.01574, 0.0004288, 1.218e-14)

#Order the p-values from smallest to greatest and compare to Holm–Bonferroni critical value
2.2e-16 < .05/(21 - 1 + 1) = 0.002380952
2.2e-16 < .05/(21 - 2 + 1) = 0.0025
2.2e-16 < .05/(21 - 3 + 1) =  0.002631579
2.2e-16 < .05/(21 - 4 + 1) = 0.002777778
2.2e-16 < .05/(21 - 5 + 1) = 0.002941176
2.2e-16 < .05/(21 - 6 + 1) = 0.003125
2.2e-16 < .05/(21 - 7 + 1) = 0.003333333
2.2e-16 < .05/(21 - 8 + 1) = 0.003571429
2.2e-16 < .05/(21 - 9 + 1) = 0.003846154
2.2e-16 < .05/(21 - 10 + 1) = 0.004166667
2.2e-16 < .05/(21 - 11 + 1) = 0.004545455
2.2e-16 < .05/(21 - 12 + 1) = 0.005
2.2e-16 < .05/(21 - 13 + 1) = 0.005555556
2.2e-16 < .05/(21 - 14 + 1) = 0.00625
2.2e-16 < .05/(21 - 15 + 1) = 0.007142857
2.2e-16 < .05/(21 - 16 + 1) = 0.008333333
2.2e-16 < .05/(21 - 17 + 1) = 0.01
2.2e-16 < .05/(21 - 18 + 1) = 0.0125
1.218e-14 < .05/(21 - 19 + 1) = 0.01666667
0.0004288 < .05/(21 - 20 + 1) = 0.025
0.01574 < .05/(21 - 21 + 1) = 0.05
#(N=21)

