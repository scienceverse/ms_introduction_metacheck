
# Broken Physics: A Conjunction-Fallacy Effect in Intuitive Physical Reasoning
# Ethan Ludwin-Peery, Neil R. Bramley, Ernest Davis, and Todd M. Gureckis

# Script for Experiment 2

if (!require(BayesianFirstAid)) {install.packages("BayesianFirstAid"); require(BayesianFirstAid)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(plyr)) {install.packages("plyr"); require(plyr)}

column.greppr = function(DATA, GREP){
  return(DATA[, grep(GREP, names(DATA))])
}

physicalCF2 = read.csv(file.choose()) # Import the data for Experiment 2

physicalCF2.1 = physicalCF2 # Makes a copy of the original that won't be affected by exclusions



##### Exclusions ------------------------------------------------------------------------

physicalCF2.1 = physicalCF2.1[physicalCF2.1$Finished == 1,] # Gotta have finished it! 

# In all studies, participants will be asked to answer a series of comprehension questions about the task 
# once they have read the instructions. If they fail any of these comprehension questions, they will be 
# shown the instructions again and given the opportunity to change their answers to the comprehension questions. 
# If they fail to answer all the comprehension questions correctly three times in a row, they will be allowed to 
# continue with the experiment but excluded from the analysis.

physicalCF2.1 = physicalCF2.1[physicalCF2.1$numFAIL != 3,]

# For the CONJUNCTION FALLACY study, there will be one question where the true outcome participants are asked 
# about appears near certain, and another where it appears near impossible. 

#Participants that report that the near certain outcome is less than 90% likely 

physicalCF2.1 = physicalCF2.1[physicalCF2.1$check.hundred_5 >= 90,]

# or that the near impossible outcome is more than 10% likely will be excluded from the analysis.

physicalCF2.1 = physicalCF2.1[physicalCF2.1$check.zero_5 <= 10,]

# First 180 only (60 * 3 conditions)
if(nrow(physicalCF2.1) > 180){
  physicalCF2.1 = physicalCF2.1[1:180,]
}

physicalCF2.1 = droplevels(physicalCF2.1)



##### Analysis ------------------------------------------------------------------------

#  Our main analysis will be to average the rating difference scores 
# (% conjunction probability  - % sole probability) for each participant for each of the eight problems. 
# We will then use a two-tailed one-sample t-test and the one-sample BEST to determine if participants tend 
# to commit the conjunction fallacy, on average. If the scores are greater than and distinguishable from zero, 
# this is evidence that people tend to commit the conjunction fallacy for physical reasoning problems of this 
# type.

physicalCF2.1$DEL_T0 = physicalCF2.1$CM.T0.1_5 - physicalCF2.1$CM.T0M.2_5
physicalCF2.1$DEL_T1 = physicalCF2.1$CS.T1.1_5 - physicalCF2.1$CS.T1.2_5
physicalCF2.1$DEL_T2 = physicalCF2.1$CM.T2.1_5 - physicalCF2.1$CM.T2M.2_5
physicalCF2.1$DEL_T3 = physicalCF2.1$CS.T3.1_5 - physicalCF2.1$CS.T3.2_5

physicalCF2.1$DEL_T4 = physicalCF2.1$OM.T4M.2_5 - physicalCF2.1$OM.T4.1_5
physicalCF2.1$DEL_T5 = physicalCF2.1$OS.T5.2_5 - physicalCF2.1$OS.T5.1_5
physicalCF2.1$DEL_T6 = physicalCF2.1$OM.T6M.2_5 - physicalCF2.1$OM.T6.1_5
physicalCF2.1$DEL_T7 = physicalCF2.1$OS.T7.2_5 - physicalCF2.1$OS.T7.1_5

conj.scoresCF2 = column.greppr(physicalCF2.1, "DEL_")

average.differenceCF2 = apply(conj.scoresCF2, 1, mean)

t.test(average.differenceCF2, mu = 0)
bayes.t.test(average.differenceCF2, mu = 0)



### Figure 3  ------------------------------------------------------------------------

physicalCF2.1$short_condition = revalue(physicalCF2.1$conjunction_wording, c(
  "How likely is it that both will happen: the pink sphere will end up on the GRASS and the cannonball will hit the pink sphere?" = "Disordered",
  "How likely is it that the cannonball will hit the pink sphere and the pink sphere will end up on the GRASS?" = "No Adverb",
  "How likely is it that the cannonball will hit the pink sphere, and then the pink sphere will end up on the GRASS?" = "Original"
))
physicalCF2.1$short_condition = relevel(physicalCF2.1$short_condition, ref = "No Adverb")
physicalCF2.1$short_condition = relevel(physicalCF2.1$short_condition, ref = "Original")


# This will only work if you have just run the Experiment 1 script in the same environment; you need the average.differenceCF1 object
both_pcf = data.frame("short.condition" = as.factor(c(as.character(physicalCF2.1$short_condition), rep("Study 1", 60))),
                      "avg.diff" = c(average.differenceCF2, average.differenceCF1))


with_pcfstudy1_jitt_skip = ggplot(both_pcf, aes(short.condition, avg.diff)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + geom_boxplot(fill = "#999999", outlier.shape = NA) + ylim(c(-50,50)) +
  theme_bw() + geom_hline(yintercept = 0, linetype="dashed") +
  xlab("Condition") + ylab("δ (Rating Difference Scores)") +
  coord_flip() + scale_x_discrete(breaks = levels(both_pcf$short.condition), limits=c("Disordered", "No Adverb", "Original", "skip", "Study 1")) + 
  geom_jitter(alpha = .2, width = .15)

ggsave("PCF_altphrasings_withs1_andjitter_andalsoskip.pdf", with_pcfstudy1_jitt_skip, width = 6, height = 2.5, device = cairo_pdf)





