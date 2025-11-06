# Study 1g


# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load( ggplot2 ,  tidyverse ,  lme4 ,  here ,  sjPlot,  lmerTest ,  AICcmodavg)

# ===============================================================
# FUNCTIONS
# ===============================================================
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}


# ===============================================================
# LOAD DATA
# ===============================================================
raw = read.csv(file = "data/fr rpp ic.csv")
temp = raw
names(temp)

# ===============================================================
# CLEAN DATA
# ===============================================================

#Remove previews and cruft rows
temp = temp[temp$Status == "IP Address",]

#Make numerics numeric
temp = type.convert(temp)

#Subset only to people who completed the survey
temp = temp[temp$Progress > 98,]

#Add a variable for ID
temp$id = 1:nrow(temp)

#
# Do exclusions
#
correct_c1 = "You pay $1, and your chances of winning the lottery increase. Your bonus will be the remaining $1 after paying the price and whatever you win from the lottery"
temp$pass_comp1 = ifelse(temp$comp_1 == correct_c1,1,0)
table(temp$pass_comp1)

correct_c2 = "You don't pay the price, and your chances of winning the lottery do not increase. Your bonus will be the $2 you were given and whatever you win from the lottery"
temp$pass_comp2 = ifelse(temp$comp_2 == correct_c2,1,0)
table(temp$pass_comp2)

correct_ac = "Indicate your willingness to pay to change your chances of winning a lottery"
temp$pass_ac = ifelse(temp$attentionCheck == correct_ac, 1, 0)
table(temp$pass_ac)

temp$pass_6060 = ifelse(temp$wtp60to60_1 <= .5, 1, 0)
table(temp$pass_6060)
temp$pass_3030 = ifelse(temp$WTP30to30_1 <= .5, 1, 0)
table(temp$pass_3030)

temp$pass_ct = temp$pass_3030 + temp$pass_6060 + temp$pass_comp1 + temp$pass_comp2 + temp$pass_ac

table(temp$pass_ct)

temp$keep_pr = ifelse(temp$pass_ct == 5, 1, 0) #prereg
temp$keep_2 = ifelse(temp$pass_ct > 3, 1, 0) #less strict

table(temp$keep_2)

#
# Get demographics
#

#With strict prereg exclusions
mean(temp$ageQ[temp$keep_pr==1], na.rm = T)
sd(temp$ageQ[temp$keep_pr==1], na.rm = T)
table(temp$genderQ[temp$keep_pr==1])

#with looser exclusions
mean(temp$ageQ[temp$keep_2==1], na.rm = T)
sd(temp$ageQ[temp$keep_2==1], na.rm = T)
table(temp$genderQ[temp$keep_2==1])

#with no exclusions
mean(temp$ageQ, na.rm = T)
sd(temp$ageQ, na.rm = T)
table(temp$genderQ)


#
# Create lookup table for start percentages
#
lookup = temp[1,] %>% select(contains("start"))
lookup = t(lookup[,2:31])
lookup = lookup/100
lookup = data.frame(lookup)
lookup$q_name = unlist(row.names(lookup))
lookup$q_name = parse_number(lookup$q_name)
names(lookup) = c("start", "q_num")

#
# Go wide to long
#

long = temp %>% pivot_longer(cols = contains("wtp"), names_to = "col_name", values_to = "wtp")

#remove control questions
long = long[long$col_name != "WTP30to30_1",]
long = long[long$col_name != "wtp60to60_1",]


#Make a question number column
long$q_num = parse_number(long$col_name)


#Merge the lookup onto the long dataframe
long = merge(long, lookup, by = "q_num")

#Add in change questions
long$change = .1
long$end = long$start + long$change
long$perc_reduction = 1 - (1.000000000001-long$end)/(1.00000000000001-long$start)




#Save cleaned data
#write.csv(long, "rppic.csv")
#write.csv(long[long$keep_2 == 1,], "rppicx2.csv")
#write.csv(long[long$keep_pr==1,], "rppicx.csv")

# ===============================================================
# ANALYSIS
# ===============================================================

#
# Preliminary plots
#

#without exclusions
wtp_table = long %>% group_by(start) %>% summarize(m_wtp = mean(wtp, na.rm=T), se_wtp = sd(wtp, na.rm = T)/sqrt(n()))
ggplot(data = wtp_table, aes(x = start, y = m_wtp)) + geom_line()

#with full exclusions
wtp_tablex = long[long$keep_pr==1,] %>% group_by(start) %>% summarize(m_wtp = mean(wtp, na.rm=T), se_wtp = sd(wtp, na.rm = T)/sqrt(n()))
ggplot(data = wtp_tablex, aes(x = start, y = m_wtp)) + geom_line()


#
# Prereg analysis
#

#With exclusions
fr_mod = lmer(wtp ~ perc_reduction + (1 | id), data = long[long$start !=.9 & long$keep_pr==1,], REML = F)
ob_mod = lmer(wtp ~ end + (1 | id), data = long[long$start !=.9 & long$keep_pr==1,], REML = F)
base_mod = lmer(wtp ~ 1 + (1 | id), data = long[long$start !=.9 & long$keep_pr==1,], REML = F)


anova(fr_mod, ob_mod)
anova(fr_mod, base_mod)

#AIC analysis
models = list(fr_mod, ob_mod)
aic_comp = aictab(cand.set = models)
evidence(aic_comp)


#Run it with mild exclusions
#With exclusions
fr_mod2 = lmer(wtp ~ perc_reduction + (1 | id), data = long[long$start !=.9 & long$keep_2==1,], REML = F)
ob_mod2 = lmer(wtp ~ end + (1 | id), data = long[long$start !=.9 & long$keep_2==1,], REML = F)
base_mod2 = lmer(wtp ~ 1 + (1 | id), data = long[long$start !=.9 & long$keep_2==1,], REML = F)


anova(fr_mod2, ob_mod2)
anova(fr_mod2, base_mod2)




# Without exclusions
fr_modnox = lmer(wtp ~ perc_reduction + (1 | id), data = long[long$start !=.9,], REML = F)
ob_modnox = lmer(wtp ~ end + (1 | id), data = long[long$start !=.9,], REML = F)
base_modnox = lmer(wtp ~ 1 + (1 | id), data = long[long$start !=.9,], REML = F)


anova(fr_modnox, ob_modnox)
anova(fr_modnox, base_modnox)

#AIC analysis
modelsnox = list(fr_modnox, ob_modnox)
aic_compnox = aictab(cand.set = modelsnox)
evidence(aic_compnox)

