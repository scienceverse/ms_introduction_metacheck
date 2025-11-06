
#
#
# STUDY 1e
#
#


##########################
#
# IMPORT PACKAGES
#
##########################
library(groundhog)

groundhog_day_lmer = "2020-01-01"
groundhog.library('nlme',groundhog_day_lmer)
groundhog.library('MASS',groundhog_day_lmer)
groundhog.library('lattice',groundhog_day_lmer)
groundhog.library('Matrix',groundhog_day_lmer)
groundhog.library(lmerTest, groundhog_day_lmer)
groundhog.library(lme4, groundhog_day_lmer)


groundhog_day = "2020-01-01"

groundhog.library(tidyverse, groundhog_day)
groundhog.library(ggplot2, groundhog_day)
groundhog.library(ggrepel, groundhog_day)
groundhog.library(formattable, groundhog_day)
groundhog.library(ggeffects, groundhog_day)
groundhog.library(sjPlot, groundhog_day)


##########################
#
# CLEAN DATA
#
##########################
path_to_data = "INSTER HERE"
raw <- read.csv(path_to_data, comment.char="#", stringsAsFactors = FALSE)

test_id_ct = 22 #up to 22 should be cut

raw = raw[test_id_ct:nrow(raw),]

#create the lockup table
lookup_vals = raw[1,166:211]
lookup_vals = unlist((lookup_vals))
lookup_names = names(raw[1,166:211])
lookup_table = data.frame("wtp_question" = lookup_names, "start_prob" =  lookup_vals)
names(lookup_table) = c("wtp_question", "start_prob")
lookup_table = lookup_table %>% filter(grepl("start", wtp_question, fixed=T))
lookup_table$wtp_question = as.numeric(gsub("[^0-9.-]", "", lookup_table$wtp_question))

#add ID var
raw$id = 1:nrow(raw)

#go wide to long
long = raw %>% 
  pivot_longer(cols=starts_with('X'), names_to="wtp_question", names_prefix="X", values_to="wtp")
keeps = c("wtp_question", "wtp", "comp_1", "comp_2", "attentionCheck", "wtp60to60_1", "WTP30to30_1", "id")
long = long[, names(long) %in% keeps]

#remove trailing 1's from each column
long$wtp_question = substr(long$wtp_question, 1, nchar(long$wtp_question)-1)
long$wtp_question = as.numeric(gsub("[^0-9.-]", "", long$wtp_question))


#Add the starting probability from the lookup table
long = merge(lookup_table, long, by='wtp_question')

#make it numeric
long = data.frame(apply(long, 2, as.numeric))

#add the ending probabilities, percent decreases
long$end_prob = long$start_prob + 10
long$start_prob = long$start_prob/100
long$end_prob = long$end_prob/100
long$perc_reduction = 1 - (1.00001-long$end_prob)/(1.000001-long$start_prob)


#
# Create attention checks dataframes and lists
#
#comp_1 should = 1
#comp_2 should = 2
#attentionCheck should = 1
long$pass_comp1 = ifelse(long$comp_1==1,1,0)
long$pass_comp2 = ifelse(long$comp_2==2,1,0)
long$pass_attn = ifelse(long$attentionCheck==1,1,0)
long$wtp_control_60to60 = long$wtp60to60_1
long$wtp_control_30to30 = long$WTP30to30_1

long$pass_all_checks = ifelse(long$pass_comp1==1 & long$pass_comp2==1 & long$wtp_control_30to30<.5 & long$wtp_control_60to60<.5 & long$pass_attn==1,1,0)

long$pass_both_comp = ifelse(long$pass_comp1==1 & long$pass_comp2==1,1,0)

long$pass_all_wtp_controls = ifelse(long$wtp_control_30to30<.5 & long$wtp_control_60to60<.5, 1, 0)

#check pass %
prop.table(table(long$pass_all_checks))
table(long$pass_all_checks[!duplicated(long$id)])


long$pass_all_checks
#Demographics
with_exclusions = long[long$pass_all_checks==1,]
remaining_ids = unique(with_exclusions$id)

remaining = raw[raw$id %in% remaining_ids,]

mean(as.numeric(remaining$ageQ), na.rm=T)
sd(as.numeric(remaining$ageQ), na.rm=T)

table(remaining$genderQ)





##########################
#
# ANALYSIS
#
##########################




mean_wtp_by_start = long %>% filter(pass_all_checks==1) %>% group_by(start_prob) %>% summarise(mean_wtp = mean(wtp), se_wtp = sd(wtp)/sqrt(length(unique(long$id))))
#Show change from previous point
mean_wtp_by_start$wtp_diff = c(0,diff(mean_wtp_by_start$mean_wtp))

mean_wtp_by_start$starting_prob_diff = c(0,diff(mean_wtp_by_start$start_prob))


mean_wtp_by_start$normalized_diff =  mean_wtp_by_start$wtp_diff/mean_wtp_by_start$starting_prob_diff

#OVERALL FIGURE
mean_wtp_by_start %>% ggplot(aes(x=start_prob, y=mean_wtp)) + geom_point() + geom_line() + geom_errorbar(aes(ymax = mean_wtp + se_wtp, ymin=mean_wtp - se_wtp)) + ggtitle("WTP by Starting Probability") + ggtitle("WTP for a 10 Percentage Point Increase in Winning Probability by Starting Probability") + ylab("Mean WTP") + xlab("Starting probability of winning lottery")  + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,by=.2))  + 
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,1.50)) +  
  geom_errorbar(aes(ymin = mean_wtp - se_wtp, ymax = mean_wtp + se_wtp), alpha=.8) + 
  theme_light(base_size = 15)

#FIGURE INSET
#Zoom in on just the certainty effect
mean_wtp_by_start %>% ggplot(aes(x=start_prob, y=mean_wtp)) + geom_point() + geom_line() + geom_errorbar(aes(ymax = mean_wtp + se_wtp, ymin=mean_wtp - se_wtp)) + ggtitle("WTP by Starting Probability, Certainty Effect Focus") + scale_x_continuous(labels = scales::percent, limits = c(.85,.94), breaks = seq(0,1,by=.01))  + 
  scale_y_continuous(labels = scales::dollar_format(), limits = c(.5,1.50)) + 
  geom_errorbar(aes(x=.905, ymax=mean_wtp[23], ymin = mean_wtp[22]), color="darkgreen", width = .005) + 
  geom_text(aes(x = .92, label="Certainty premium", y = (mean_wtp[22] + mean_wtp[23])/2)) + 
  #geom_text_repel(label = scales::percent(mean_wtp_by_start$start_prob), force=10) + 
  theme_light(base_size = 15) + theme(panel.grid.minor = element_blank()) + ylab("Mean WTP") + xlab("Starting probability of winning lottery")





mean_wtp_by_start %>% ggplot(aes(x=start_prob, y=wtp_diff)) + geom_point() + geom_line()  + ggtitle("Change in WTP from previous lottery")

mean_wtp_by_start %>% ggplot(aes(x=start_prob, y=normalized_diff)) + geom_point() + geom_line()  + ggtitle("Change in WTP from previous lottery, divided by percentage point difference in starting prob") + coord_cartesian(ylim=c(-10,20))

#mean_wtp_by_start$start_prob = mean_wtp_by_start$start_prob*100
formattable(mean_wtp_by_start[,1:4], list(mean_wtp = color_bar("lightblue"), start_prob = color_bar("lightgreen")))

#long %>% ggplot(aes(x=start_prob, y=wtp, color=as.factor(pass_all_checks))) + geom_point() + geom_line() + facet_wrap(long$id) + ggtitle("WTP by Starting Prob, All Subjects")


mean_wtp_by_start_indiv = long %>% group_by(id, start_prob) %>% summarise(mean_wtp = mean(wtp), se_wtp = sd(wtp)/sqrt(length(unique(long$id))))

#base
null_mod = lmer(wtp ~ 1 + (1 | id), data=long)
fr_mod = lmer(wtp ~ perc_reduction + (1 | id), data=long)
ob_mod = lmer(wtp ~ end_prob + (1 | id), data=long)

anova(null_mod, fr_mod)
anova(ob_mod, fr_mod)

#with exclusions
null_mod_x = lmer(wtp ~ 1 + (1 | id), data=long[long$pass_all_checks==1,])
fr_mod_x = lmer(wtp ~ perc_reduction + (1 | id), data=long[long$pass_all_checks==1,])
ob_mod_x = lmer(wtp ~ end_prob + (1 | id), data=long[long$pass_all_checks==1,])

anova(null_mod_x, fr_mod_x)

anova(ob_mod_x, fr_mod_x)



#
# Certainty predictions
#

#Get our model, estimate the params on everything until start prob = .9 -- then based on that estimate the parameters

#fit model
#with exclusions and no .9 start prob
null_mod_x2 = lmer(wtp ~ 1 + (1 | id), data=long[long$pass_all_checks==1 & long$start_prob<.9,])
fr_mod_x2 = lmer(wtp ~ perc_reduction + (1 | id), data=long[long$pass_all_checks==1 & long$start_prob<.9,])
ob_mod_x2 = lmer(wtp ~ end_prob + (1 | id), data=long[long$pass_all_checks==1 & long$start_prob<.9,])

anova(null_mod_x2, fr_mod_x2)

anova(ob_mod_x2, fr_mod_x2)

#get predictions
v = seq(0,1,by=.001)
pr_fr = ggpredict(fr_mod_x2,  terms = c("perc_reduction [v]"))
predictions = data.frame('perc_reduction' = pr_fr$x, 'y_hat' = pr_fr$predicted, 'std_error' = pr_fr$std.error)
#algebra!
#https://www.wolframalpha.com/input/?i=solve+p+%3D+1+-+%281+-+x+-+.1%29%2F%281-x%29+for+x
predictions$start_prob = 1 - (1/(10*predictions$perc_reduction))

predictions = predictions[predictions$start_prob>0,]

#plot predictions alongside the actual values

mean_wtp_by_start$pointValue = ">89.99% Starting Probability"
mean_wtp_by_start$pointValue = ifelse(mean_wtp_by_start$start_prob == .9, "90% Starting Probability", mean_wtp_by_start$pointValue)
mean_wtp_by_start$pointValue = ifelse(mean_wtp_by_start$start_prob < .9 & mean_wtp_by_start$start_prob > .8990, "89.99% Starting Probability", mean_wtp_by_start$pointValue)


pointColors = c(">89.99% Starting Probability" = "black", "90% Starting Probability" = "firebrick3", "89.99% Starting Probability" ="darkgoldenrod2")


ggplot() + 
  geom_line(data=predictions, aes(x=start_prob, y=y_hat), color='blue', size=1.2) +
  geom_point(data=mean_wtp_by_start, aes(x=start_prob, y=mean_wtp, color=pointValue)) + theme_light()  + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,by=.2))  + 
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,1.50)) + ylab("Mean WTP") + xlab("Starting probability of winning lottery") +  
  geom_errorbar(data = mean_wtp_by_start, aes(x=start_prob, ymin = mean_wtp - se_wtp, ymax = mean_wtp + se_wtp, color=pointValue), alpha=.8, width=.01) + ggtitle("Mean WTP vs. Predicted WTP by Starting Probability") + scale_color_discrete(drop=TRUE, limits = levels (mean_wtp_by_start$pointValue)) + scale_color_manual(values  = pointColors, name = "Starting Probabilities")



#Try to create a shared legend... somehow

merge = merge(predictions, mean_wtp_by_start, by = 'start_prob', all = T)

merge %>% ggplot() + 
  geom_line(aes(x=start_prob, y=y_hat), color='blue', size=1.2) +
  geom_point(aes(x=start_prob, y=mean_wtp, color=pointValue)) + theme_light()  + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1), breaks = seq(0,1,by=.2))  + 
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,1.50)) + ylab("Mean WTP") + xlab("Starting probability of winning lottery") +  
  geom_errorbar(aes(x=start_prob, ymin = mean_wtp - se_wtp, ymax = mean_wtp + se_wtp, color=pointValue), alpha=.8, width=.01) + ggtitle("Mean WTP vs. Predicted WTP by Starting Probability") + scale_color_discrete(drop=TRUE, limits = levels (mean_wtp_by_start$pointValue)) + scale_color_manual(values  = pointColors, name = "Starting Probabilities")


