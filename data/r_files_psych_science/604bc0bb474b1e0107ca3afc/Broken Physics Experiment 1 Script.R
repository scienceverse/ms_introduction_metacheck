
# Broken Physics: A Conjunction-Fallacy Effect in Intuitive Physical Reasoning
# Ethan Ludwin-Peery, Neil R. Bramley, Ernest Davis, and Todd M. Gureckis

# Script for Experiment 1

if (!require(BayesianFirstAid)) {install.packages("BayesianFirstAid"); require(BayesianFirstAid)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}

column.greppr = function(DATA, GREP){
  return(DATA[, grep(GREP, names(DATA))])
}

physicalCF1 = read.csv(file.choose()) # Import the data for Experiment 1

physicalCF1.1 = physicalCF1 # Makes a copy of the original that won't be affected by exclusions



##### Exclusions ------------------------------------------------------------------------

pcf.pv.1 = pcf.pv.1[pcf.pv.1$Finished == 1,] # Gotta have finished it! 

#In all studies, participants will be asked to answer a series of comprehension questions about the task 
# once they have read the instructions. If they fail any of these comprehension questions, they will be 
# shown the instructions again and given the opportunity to change their answers to the comprehension questions. 
# If they fail to answer all the comprehension questions correctly three times in a row, they will be allowed to 
# continue with the experiment but excluded from the analysis.

physicalCF1.1 = physicalCF1.1[physicalCF1.1$numFAIL != 3,]

# For the CONJUNCTION FALLACY study, there will be one question where the true outcome participants are asked 
# about appears near certain, and another where it appears near impossible. 

# Participants that report that the near certain outcome is less than 90% likely...

physicalCF1.1 = physicalCF1.1[physicalCF1.1$check.hundred_5 >= 90,]

# ...or that the near impossible outcome is more than 10% likely will be excluded from the analysis.

physicalCF1.1 = physicalCF1.1[physicalCF1.1$check.zero_5 <= 10,]

# Only the first 60 participants

if(nrow(physicalCF1.1) > 60){
  physicalCF1.1 = physicalCF1.1[1:60,]
}



##### Analysis ------------------------------------------------------------------------

# Our main analysis will be to average the rating difference scores 
# (% conjunction probability  - % sole probability) for each participant for each of the eight problems. 
# We will then use a two-tailed one-sample t-test and the one-sample BEST to determine if participants tend 
# to commit the conjunction fallacy, on average. If the scores are greater than and distinguishable from zero, 
# this is evidence that people tend to commit the conjunction fallacy for physical reasoning problems of this 
# type.

physicalCF1.1$DEL_T0 = physicalCF1.1$CM.T0.1_5 - physicalCF1.1$CM.T0M.2_5
physicalCF1.1$DEL_T1 = physicalCF1.1$CS.T1.1_5 - physicalCF1.1$CS.T1.2_5
physicalCF1.1$DEL_T2 = physicalCF1.1$CM.T2.1_5 - physicalCF1.1$CM.T2M.2_5
physicalCF1.1$DEL_T3 = physicalCF1.1$CS.T3.1_5 - physicalCF1.1$CS.T3.2_5

physicalCF1.1$DEL_T4 = physicalCF1.1$OM.T4M.2_5 - physicalCF1.1$OM.T4.1_5
physicalCF1.1$DEL_T5 = physicalCF1.1$OS.T5.2_5 - physicalCF1.1$OS.T5.1_5
physicalCF1.1$DEL_T6 = physicalCF1.1$OM.T6M.2_5 - physicalCF1.1$OM.T6.1_5
physicalCF1.1$DEL_T7 = physicalCF1.1$OS.T7.2_5 - physicalCF1.1$OS.T7.1_5

conj.scoresCF1 = column.greppr(physicalCF1.1, "DEL_")

average.differenceCF1 = apply(conj.scoresCF1, 1, mean)

t.test(average.differenceCF1, mu = 0)

bayes.t.test(average.differenceCF1, mu = 0)



##### Figure 2 ------------------------------------------------------------------------

ad = data.frame("average.differenceCF1" = average.differenceCF1)

pcf1.correct = ggplot(ad, aes(average.differenceCF1)) + 
  geom_histogram(binwidth = 5, aes(y=..count../sum(..count..))) +
  xlab("δ (Rating Difference Scores)") + 
  ylab("Proportion") + 
  #xlim(c(-40, 40)) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 7.29, color = "black") +
  geom_vline(xintercept = 3.92, color = "black", linetype="dotted") +
  geom_vline(xintercept = 10.67, color = "black", linetype="dotted")

ggsave("PCF1_NoCorrect.pdf", pcf1.correct, device = cairo_pdf,  width = 5, height = 3)



