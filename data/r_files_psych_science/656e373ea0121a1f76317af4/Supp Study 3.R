#Load packages
pacman::p_load("here", "tidyverse", "ggplot2", "lme4", "lmerTest", "qualtRics", "effectsize")
#Create a project containing this R file and the data, then run this
here::i_am("Supp Study 3.R")


#Load data from qualtrics
raw = read.csv("Supp Study 3.csv")
raw = raw[3:nrow(raw),]
raw = type.convert(raw)

#Get demographics
mean(raw$age, na.rm=T)
prop.table(table(raw$gender))

#Clean data
names(raw)

temp = raw

temp[,20:27] = lapply(temp[,20:27], function(x) as.numeric(x))


#attention check exclusions
temp$pass_simple = ifelse(temp$simple_ac==1,1,0)
temp$pass_starting = ifelse(temp$starting_chances==1,1,0)
temp$pass_imp_ac = ifelse((temp$q3hi==1 & temp$improvement_size == 2)|(temp$q3hi==0 & temp$improvement_size==1),1,0)

temp$pass_all_acs = ifelse((temp$pass_imp_ac + temp$pass_starting + temp$pass_simple)==3,1,0)

temp = temp[temp$pass_all_acs == 1,]

#Make it so 1 always is the one with a bigger change
cols_to_reverse = c("Q64", "Q63", "control_bbig_med", "medical_a_hi")
temp[,names(temp) %in% cols_to_reverse] = abs(temp[,names(temp) %in% cols_to_reverse] - 3)

#Create single variables from counterbalanced stuff
temp$biz_t_dv = temp[,20:21] %>% data.table::fcoalesce()
temp$biz_c_dv = temp[,22:23] %>% data.table::fcoalesce()
temp$med_c_dv = temp[,24:25] %>% data.table::fcoalesce()
temp$med_t_dv = temp[,26:27] %>% data.table::fcoalesce()

temp$id = 1:nrow(temp)
#Go wide to long
long = temp %>% pivot_longer(cols = ends_with("dv"), values_to = "choice", names_to = "question")

long$scenario = substr(long$question,1,3)
long$condition = substr(long$question,5,5)

long$choice = abs(long$choice - 1)
#1 = bigger change, 0 = smaller change in long$choice


long$condition_cc = ifelse(long$condition == "t", 1, -1)


#Analyze data
aggregate(choice==1 ~ scenario + condition, data = long, mean)
aggregate(choice==1 ~ condition, data = long, mean)

#Run regression
#Main test
main_mod = lmer(choice ~ condition_cc + (1 | id), data = long)
summary(main_mod)

main_mod = lm(choice ~ condition, data = long)
summary(main_mod)

binom_mod = glm(choice ~ condition, data = long, family = "binomial")
summary(binom_mod)

#Individual for each
summary(lmer(choice ~ condition + (1 | id), data = long[long$scenario=="med",]))

summary(lmer(choice ~ condition + (1 | id), data = long[long$scenario=="biz",]))

#Get cohen's D for individual comparisons

effectsize::cohens_d(choice ~ condition, data = long)


summary(lm(choice ~ condition, data = long[long$scenario=="med",]))
effectsize::cohens_d(choice ~ condition, data = long[long$scenario=="med",])

summary(lm(choice ~ condition, data = long[long$scenario=="biz",]))
effectsize::cohens_d(choice ~ condition, data = long[long$scenario=="biz",])


