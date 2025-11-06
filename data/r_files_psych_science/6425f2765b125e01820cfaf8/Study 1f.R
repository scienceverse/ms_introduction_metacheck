#Study 1f

library(Hmisc)
library(ggplot2)
library(lme4)
library(lmerTest)
library(data.table)
library(readr)
library(here)
library(tidyverse)
library(AICcmodavg)

rm(list = ls())

raw <- read.csv(here("01 - Data/213 real.csv"), comment.char="#")

#raw = read.csv("https://www.dropbox.com/s/3cruxstedio1cpp/213%20real.csv?dl=0")

#raw <- read_excel("~/Desktop/213 real.xlsx")
#raw = read.csv("~/Desktop/213 real.csv")


#subset to actual participants from pilot
#pilot_raw = raw[23:32,]

pilot_raw = raw[3:nrow(raw),]

# Set up the lookup table for starting/ending probability
end_start_cols = names(pilot_raw)[names(pilot_raw) %like% ('q')][1:46]

lookup_temp = pilot_raw[1, names(pilot_raw) %in% end_start_cols]
lookup_probs_temp = data.frame(t(lookup_temp))
strip1 = gsub('[0-9.]', '', rownames(lookup_probs_temp))
strip2 = gsub('q', '', strip1)
lookup = data.frame('q_number' = parse_number(rownames(lookup_probs_temp)), 'probability' = as.numeric(lookup_probs_temp$X3)/100, 'start_or_end' = strip2)
lookup$end = Hmisc::Lag(lookup$probability, -1)
lookup$start = lookup$probability
lookup$change = lookup$end - lookup$start
lookup = lookup[lookup$start_or_end=="start",]
keeps_lookup = c('q_number', 'end', 'start')
lookup = lookup[names(lookup) %in% keeps_lookup]
lookup = rbind(lookup, c(24, 30, 30))
lookup = rbind(lookup, c(25, 60, 60))


#Pull out the WTP questions and useful Qs
wtp_question_cols = names(pilot_raw)[names(pilot_raw) %like% ('wtp')]
keeps = c(wtp_question_cols, "genderQ", "income", "ageQ", "ID", "effortQuestion", "whatSurveyAsking", "whatWasWeird", "attentionCheck", "comp_1", "comp_2")
wide = pilot_raw[names(pilot_raw) %in% keeps]

#Add in ID and all the other demos
wide$id = 1:nrow(wide)

#go wide to long
long = wide %>% pivot_longer(
  cols = contains('wtp'),
  names_to = "wtp_q",
  values_to = "wtp_val"
)

#Add percent reduction in
long$q_number = parse_number(substr(long$wtp_q,1,3))
long = merge(long, lookup, by = 'q_number')
long$perc_reduction = 1 - (1.000000000001-long$end)/(1.00000000000001-long$start)
long$perc_reduction = round(long$perc_reduction, 5)


long = long[long$perc_reduction!=0,]



long = type.convert(long)

#Add in the attention checks
table(long$attentionCheck)
prop.table(table(long$comp_1))
prop.table(table(long$comp_2))


pass_1 = ifelse(long$comp_1 == "You complete 20 Captchas, and your chances of winning the lottery increase.", TRUE, FALSE)


pass_2 = ifelse(long$comp_2 == "You don't complete any Captchas, and your chances of winning the lottery do not increase.", TRUE,FALSE)

pass_ac = ifelse(long$attentionCheck == "Indicate your willingness to complete Captchas to change your chances of winning a lottery",TRUE,FALSE)


long$wtp_val[long$wtp_q == "X25_1wtp_1"]

long$keep = ifelse(pass_1 & pass_2 & pass_ac, 1, 0)
longx = long[long$keep == 1,]

length(unique(longx$id))
#117
length(unique(long$id))
#214

#write.csv(long, "effortic.csv")
#write.csv(longx, "efforticx.csv")

#
# Do analysis
#

wtp_plot = long %>% group_by(start) %>% summarize(mean_w = mean(wtp_val, na.rm = T))

ggplot(aes(x = start, y = mean_w), data = wtp_plot) + geom_point() + geom_line() + xlab("Start probability") + ylab("Mean # Captchas") + scale_y_continuous(limits = c(0,20))

wtp_plotx = longx %>% group_by(start) %>% summarize(mean_w = mean(wtp_val, na.rm = T))

ggplot(aes(x = start, y = mean_w), data = wtp_plotx) + geom_point() + geom_line() + xlab("Start probability") + ylab("Mean # Captchas") + scale_y_continuous(limits = c(0,30))



#7/10 get it all right 
table(long$attentionCheck)
prop.table(table(long$comp_1))
prop.table(table(long$comp_2))

raw$is_chosen


#Check for ceiling effects
hist(as.numeric(long$wtp_val), breaks = 0:40)

prop.table(table(long$start, long$wtp_val==40),1)



fr_mod = lmer(wtp_val ~ perc_reduction + (1 | id), data = long[long$start !=.9,])
ob_mod = lmer(wtp_val ~ end + (1 | id), data = long[long$start !=.9,])
base_mod = lmer(wtp_val ~ 1 + (1 | id), data = long[long$start !=.9,])


anova(fr_mod, ob_mod)
anova(fr_mod, base_mod)

#Exclusions
fr_mod = lmer(wtp_val ~ perc_reduction + (1 | id), data = longx[longx$start !=.9,], REML = F)
ob_mod = lmer(wtp_val ~ end + (1 | id), data = longx[longx$start !=.9,], REML = F)
base_mod = lmer(wtp_val ~ 1 + (1 | id), data = longx[longx$start !=.9,], REML = F)


anova(fr_mod, ob_mod, type = "Kenward-Roger")
anova(fr_mod, base_mod)



summary(fr_mod)
#AICC mod avg

models = list(fr_mod, ob_mod)
aic_comp = aictab(cand.set = models)
evidence(aic_comp)

models = list(fr_mod, base_mod)
aic_comp = aictab(cand.set = models)
evidence(aic_comp)

#Write data without exclusions
write.csv(long, "s1b.csv")

