#
# WTP for probability increases
#


##########################
#
# IMPORT PACKAGES
#
##########################

library(groundhog)

groundhog_day = "2020-01-01"

groundhog.library(reshape2, groundhog_day) #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
groundhog.library(lme4, groundhog_day) 
groundhog.library(lmerTest, groundhog_day)

##########################
#
# CLEAN DATA
#
##########################

path_to_csv = ""
raw <- read.csv(path_to_csv, comment.char="#")


column_names = colnames(raw)
wtp_columns = column_names[grepl('_1$', column_names)]
wtp = raw[,wtp_columns]
wtp = wtp[3:nrow(wtp),]

wtp = sapply(wtp, function(x) as.numeric(as.character(x)))

# checking for exclusions
wtp = as.data.frame(wtp)
plot(wtp$X30to30_1[order(wtp$X30to30_1)], xlab="Subjects", ylab = "WTP for 0 change")
prop.table(table(wtp$X30to30_1 > 0))
prop.table(table(wtp$X30to30_1 > 0.1))
prop.table(table(wtp$X30to30_1 > 0.5))

#
# Reshape wide -> long
#

#create id column
wtp$id = 1:nrow(wtp)

#reshaping from wide -> long
wtp_long = melt(wtp, id.vars=c('id'))

colnames(wtp_long) = c('id', 'change', 'wtp')

wtp_long$initial = rep(99, nrow(wtp_long)) 
wtp_long$final =  rep(99, nrow(wtp_long)) 

unique(wtp_long$change)

#This is correct - several trials were mislabeled but are actually 60% to 70% trials
wtp_long$initial = ifelse(wtp_long$change == 'X60to70_1', 60, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X60to70_1', 70, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X20to25_1', 20, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X20to25_1', 25, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X10to20_1', 10, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X10to20_1', 20, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X85to95_1', 85, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X85to95_1', 95, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X85to90_1', 85, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X85to90_1', 90, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X90to100_1', 90, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X90to100_1', 100, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X25to100_1', 60, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X25to100_1', 70, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X75to80_1', 60, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X75to80_1', 70, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X50to60_1', 50, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X50to60_1', 60, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X75to85_1', 75, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X75to85_1', 85, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X65to70_1', 65, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X65to70_1', 70, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X60to60_1', 60, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X60to60_1', 80, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X30to30_1', 30, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X30to30_1', 30, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X90to95_1', 90, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X90to95_1', 95, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X80to90_1', 80, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X80to90_1', 90, wtp_long$final)

wtp_long$initial = ifelse(wtp_long$change == 'X80to85_1', 80, wtp_long$initial)
wtp_long$final = ifelse(wtp_long$change == 'X80to85_1', 85, wtp_long$final)

#create the change variable
wtp_long$change = wtp_long$final - wtp_long$initial
wtp_long$change = as.factor(wtp_long$change)
wtp_long$id = as.factor(wtp_long$id)

#scale initial and final to 0 to 1 instead of 0 to 100
wtp_long$initial = wtp_long$initial/100
wtp_long$final = wtp_long$final/100

# Add additional column for % decrease
wtp_long$perc_reduction = 1 - (1.1-wtp_long$final)/(1.1-wtp_long$initial)

#add additional column for percent increase
wtp_long$perc_increase = (wtp_long$final - wtp_long$initial)/wtp_long$initial



##########################
#
# ANALYSIS
#
##########################

#No exclusion criteria
nullLMER = lmer(wtp ~ change+ (1 | id), data=wtp_long, REML=FALSE)
fancyRegretLMER = lmer(wtp ~ change + perc_reduction + (1 | id), data=wtp_long, REML=FALSE)
outcomeBiasLMER = lmer(wtp ~ change + final + (1 | id), data=wtp_long, REML=FALSE)

anova(nullLMER, outcomeBiasLMER, fancyRegretLMER) #Result holds

#Payment of $.50 and above exclusion criteria

#Do exclusions
wtp_long$exclude = as.numeric(as.character(wtp_long$change)) == 0 & wtp_long$wtp > .5
ids_to_exclude = as.numeric(unique(wtp_long$id[wtp_long$exclude==TRUE]))
post_exclusions = wtp_long[!wtp_long$id %in% ids_to_exclude,]
length(ids_to_exclude)

#compare models
nullLMER = lmer(wtp ~ change+ (1 | id), data=post_exclusions, REML=FALSE)
fancyRegretLMER = lmer(wtp ~ change + perc_reduction + (1 | id), data=post_exclusions, REML=FALSE)
outcomeBiasLMER = lmer(wtp ~ change + final + (1 | id), data=post_exclusions, REML=FALSE)

anova(nullLMER, outcomeBiasLMER, fancyRegretLMER) #Result holds




#Payment of $0 and above exclusion criteria
#Do exclusions
wtp_long$exclude = as.numeric(as.character(wtp_long$change)) == 0 & wtp_long$wtp > 0
ids_to_exclude = as.numeric(unique(wtp_long$id[wtp_long$exclude==TRUE]))
post_exclusions = wtp_long[!wtp_long$id %in% ids_to_exclude,]
length(ids_to_exclude)
#Run and compare models
fancyRegretLMER = lmer(wtp ~ change + perc_reduction + (1 | id), data=post_exclusions, REML=FALSE)
outcomeBiasLMER = lmer(wtp ~ change + final + (1 | id), data=post_exclusions, REML=FALSE)
nullLMER = lmer(wtp ~ change+ (1 | id), data=post_exclusions, REML=FALSE)

anova(nullLMER, outcomeBiasLMER, fancyRegretLMER) #Result holds

#Excluding repeated trials
#Do exclusions
wtp_long_noreps = wtp_long[wtp_long$initial!=60,]
nullLMER = lmer(wtp ~ change+ (1 | id), data=wtp_long_noreps, REML=FALSE)
fancyRegretLMER = lmer(wtp ~ change + perc_reduction + (1 | id), data=wtp_long_noreps, REML=FALSE)
outcomeBiasLMER = lmer(wtp ~ change + final + (1 | id), data=wtp_long_noreps, REML=FALSE)
combinedLMER = lmer(wtp ~ change + final + perc_reduction + (1 | id), data=wtp_long_noreps, REML=FALSE)

#compare models
anova(outcomeBiasLMER, fancyRegretLMER) 
anova(nullLMER, fancyRegretLMER)


#Demographics

mean(as.numeric(raw$ageQ), na.rm=T)
sd(as.numeric(raw$ageQ), na.rm=T)

table(raw$genderQ)