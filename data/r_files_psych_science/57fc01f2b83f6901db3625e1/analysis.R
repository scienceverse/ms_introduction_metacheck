########## RISEN & GILOVICH REPLICATION - Statistics & Analysis ##########

##### Load Data #####
#Dataframe d4 is the cleaned data frame created in the data-prep.R file.
setwd("~/Dropbox/Personal computer/Stanford/Junior year Stanford/Winter Quarter/Psych 254/Risen & Gilovich replication/2015-02-13 For OSF")
d4 = read.csv("data_prepped_2.csv")
dim(d4)  #226 X 8
  
##### Explore #####
hist(d4$Likelihood)
hist(d4$Effort, 0:6)
aggregate(d4$Likelihood ~ d4$Had.read + d4$Load, d4, mean)
aggregate(d4$Likelihood ~ d4$Load, d4, mean)

##### Interaction Plot #####
library(car)
d4$tempt.fate = ifelse(d4$Had.read==0, "Yes", "No")  # recode DV for clarity

library(plyr)
agg = ddply(d4, .(Load, tempt.fate), summarize, val = mean(Likelihood) )  # aggregate data for plotting happiness

library(ggplot2)
ggplot( d4, aes(x = as.factor(Load), y = Likelihood, color=tempt.fate) ) + geom_boxplot(width=0.5) +
  geom_point(data = agg, aes(y = val), size=4 ) +
  geom_line(data = agg, aes(y = val, group = tempt.fate), lwd=2 ) +
  theme_bw()  + xlab("Cognitive load?") + ylab("Perceived likelihood of being called on") +
  guides(color=guide_legend(title="Tempted fate?"))


##### Recompute Original Analyses from R&G (2008) #####
#two-way ANOVA
summary(aov(Likelihood ~ Load * Had.read, data = d4))

#summary(aov(Likelihood ~ Load * Had.read - Load - Had.read, data = d4))
#effect size eta squared = SSeffect/SStotal

#likelihood is really skewed, so try transforming it
hist(d4$Likelihood^0.5) #much better!
summary(aov((d4$Likelihood^0.5) ~ Load * Had.read, data = d4))
