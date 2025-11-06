

                        #####EXPERIMENT 1b###### 

# Packages ----------------------------------------------------------------
for(pack in c("tidyverse","psych","car","lme4","lmerTest")) {
  if(!(pack %in% installed.packages()[,1])) {
    install.packages(pack)  
  }
  library(pack, character.only = T)
}

# Set your working directory 
                            
# Data import and wrangle -------------------------------------------------
# d <- read.csv(file.choose(), header = T, stringsAsFactors = F)

# Or throught the working directory
d <- read.csv("Data/1b - time.csv",header = T, stringsAsFactors = F)

d <- d[-(c(1,2)),]# Delete first two rows
d <- subset(d, d$Check == "7")# Pass check 101 fail
d <- subset(d, d$Look_up == "2")# Didn't look up answers 10 fail
names(d)
d <- d[,-c(1:19,140:142,144,146:147)]# Exclude columns we don't need 

# Age and gender statistics
describe(as.numeric(d$Age))
IQR(d$Age, na.rm = T)
prop.table(table(d$Sex))

d$id <- 1:nrow(d)
# Creating a between condition variable 
names(d)[names(d) == 'C'] <- "condition"

# Combining the data ------------------------------------------------------
 
# This just combines  data from the conditions into the same variables
# You can mark everything from the first and second set and run it 
names(d)
# For the time measure, we are interested in the "Page.Submit" part 

d <- d %>% select(-contains("First.Click"))
d <- d %>% select(-contains("Last.Click"))
d <- d %>% select(-contains("Click.Count"))

names(d)

# First set
d$Airport_T1_Self_4 <- ifelse(d$Airport_T1_Self_4 == "", d$Airport_T1_Dis_4, d$Airport_T1_Self_4)
d$Population_T1_Self_4 <- ifelse(d$Population_T1_Self_4 == "", d$Population_T1_Dis_4, d$Population_T1_Self_4)
d$Fertility_T1_Self_4 <- ifelse(d$Fertility_T1_Self_4 == "", d$Fertility_T1_Dis_4, d$Fertility_T1_Self_4)
d$Phone_T1_Self_4 <- ifelse(d$Phone_T1_Self_4 == "", d$Phone_T1_Dis_4, d$Phone_T1_Self_4)
d$Oil_T1_Self_4 <- ifelse(d$Oil_T1_Self_4 == "", d$Oil_T1_Dis_4, d$Oil_T1_Self_4)
d$Life_T1_Self_4 <- ifelse(d$Life_T1_Self_4 == "", d$Life_T1_Dis_4, d$Life_T1_Self_4)

#Second set
d$Airport_T2_Self_4 <- ifelse(d$Airport_T2_Self_4 == "", d$Airport_T2_Dis_4, d$Airport_T2_Self_4)
d$Population_T2_Self_4 <- ifelse(d$Population_T2_Self_4 == "", d$Population_T2_Dis_4, d$Population_T2_Self_4)
d$Fertility_T2_Self_4 <- ifelse(d$Fertility_T2_Self_4 == "", d$Fertility_T2_Dis_4, d$Fertility_T2_Self_4)
d$Phone_T2_Self_4 <- ifelse(d$Phone_T2_Self_4 == "", d$Phone_T2_Dis_4, d$Phone_T2_Self_4)
d$Oil_T2_Self_4 <- ifelse(d$Oil_T2_Self_4 == "", d$Oil_T2_Dis_4, d$Oil_T2_Self_4)
d$Life_T2_Self_4 <- ifelse(d$Life_T2_Self_4 == "", d$Life_T2_Dis_4, d$Life_T2_Self_4)

#Time question 1
d$Time_Airport_T1_Self_Page.Submit <- ifelse(d$Time_Airport_T1_Self_Page.Submit == "", d$Time_Airport_T1_Dis_Page.Submit, d$Time_Airport_T1_Self_Page.Submit)
d$Time_Pop_T1_Self_Page.Submit <- ifelse(d$Time_Pop_T1_Self_Page.Submit == "", d$Time_Pop_T1_Dis_Page.Submit, d$Time_Pop_T1_Self_Page.Submit)
d$Time_Fertil_T1_Self_Page.Submit <- ifelse(d$Time_Fertil_T1_Self_Page.Submit == "", d$Time_Fertil_T1_Dis_Page.Submit, d$Time_Fertil_T1_Self_Page.Submit)
d$Time_Phone_T1_Self_Page.Submit <- ifelse(d$Time_Phone_T1_Self_Page.Submit == "", d$Time_Phone_T1_Dis_Page.Submit, d$Time_Phone_T1_Self_Page.Submit)
d$Time_Oil_T1_Self_Page.Submit <- ifelse(d$Time_Oil_T1_Self_Page.Submit == "", d$Time_Oil_T1_Dis_Page.Submit, d$Time_Oil_T1_Self_Page.Submit)
d$Time_Life_T1_Self_Page.Submit <- ifelse(d$Time_Life_T1_Self_Page.Submit == "", d$Time_Life_T1_Dis_Page.Submit, d$Time_Life_T1_Self_Page.Submit)

#Time question 2
d$Time_Airport_T2_self_Page.Submit <- ifelse(d$Time_Airport_T2_self_Page.Submit == "", d$Time_Airport_T2_Dis_Page.Submit, d$Time_Airport_T2_self_Page.Submit)
d$Time_Pop_T2_Self_Page.Submit <- ifelse(d$Time_Pop_T2_Self_Page.Submit == "", d$Time_Pop_T2_Dis_Page.Submit, d$Time_Pop_T2_Self_Page.Submit)
d$Time_Fertil_T2_Self_Page.Submit <- ifelse(d$Time_Fertil_T2_Self_Page.Submit == "", d$Time_Fertil_T2_Dis_Page.Submit, d$Time_Fertil_T2_Self_Page.Submit)
d$Time_Phone_T2_Self_Page.Submit <- ifelse(d$Time_Phone_T2_Self_Page.Submit == "", d$Time_Phone_T2_Dis_Page.Submit, d$Time_Phone_T2_Self_Page.Submit)
d$Time_Oil_T2_Self_Page.Submit <- ifelse(d$Time_Oil_T2_Self_Page.Submit == "", d$Time_Oil_T2_Dis_Page.Submit, d$Time_Oil_T2_Self_Page.Submit)
d$Time_Life_T2_Self_Page.Submit <- ifelse(d$Time_Life_T2_Self_Page.Submit == "", d$Time_Life_T2_Dis_Page.Submit, d$Time_Life_T2_Self_Page.Submit)

names(d)
d <- d[,-c(25:48)]# We don't need these columns any more

#Condition variable needs to be made a factor with more usable labels
levels(as.factor(d$condition))
d$condition <- factor(d$condition,
                      levels = c("Friend_disagree","Self"),
                      labels = c("Disagree","Self"))
levels(d$condition)

# These lines below will make the new variable names
#---------
#q1 = airport
#q2 = population
#q3 = fertility 
#q4 = phone 
#q5 = oil
#q6 = life 

names(d)
oldnames = c("Airport_T1_Self_4","Population_T1_Self_4",
             "Fertility_T1_Self_4","Phone_T1_Self_4","Oil_T1_Self_4","Life_T1_Self_4",
             "Airport_T2_Self_4","Population_T2_Self_4",
             "Fertility_T2_Self_4","Phone_T2_Self_4","Oil_T2_Self_4","Life_T2_Self_4",
             "Time_Airport_T1_Self_Page.Submit","Time_Pop_T1_Self_Page.Submit",
             "Time_Fertil_T1_Self_Page.Submit","Time_Phone_T1_Self_Page.Submit",
             "Time_Oil_T1_Self_Page.Submit","Time_Life_T1_Self_Page.Submit",
             "Time_Airport_T2_self_Page.Submit","Time_Pop_T2_Self_Page.Submit",
             "Time_Fertil_T2_Self_Page.Submit","Time_Phone_T2_Self_Page.Submit",
             "Time_Oil_T2_Self_Page.Submit","Time_Life_T2_Self_Page.Submit")

newnames = c("guess1.q1","guess1.q2","guess1.q3","guess1.q4",
             "guess1.q5","guess1.q6",
             "guess2.q1","guess2.q2","guess2.q3","guess2.q4",
             "guess2.q5","guess2.q6",
             "Time1.q1","Time1.q2","Time1.q3","Time1.q4","Time1.q5","Time1.q6",
             "Time2.q1","Time2.q2","Time2.q3","Time2.q4","Time2.q5","Time2.q6")

for(i in 1:24) names(d)[names(d) == oldnames[i]] = newnames[i]

# Making the variabels into proper types 
str(d)
names(d)
d[,c(1:26)] <- sapply(
  d[,c(1:26)], as.numeric)
str(d)#Check if it worked 


# Re-shaping the data 
dat1 <- d %>% dplyr::select(id,condition,contains("guess1")) %>% 
  gather(guess_info, guess_1, guess1.q1:guess1.q6)

dat2 <- d %>% dplyr::select(id,condition,contains("guess2")) %>% 
  gather(guess_info, guess_2, guess2.q1:guess2.q6)

dat3 <- d %>% dplyr::select(id,condition,contains("Time1")) %>% 
  gather(time_info, time1, Time1.q1:Time1.q6)

dat4 <- d %>% dplyr::select(id,condition,contains("Time2")) %>% 
  gather(time_info, time2, Time2.q1:Time2.q6)

a <- bind_cols(dat1, dat2["guess_2"])
b <- bind_cols(a, dat3["time1"])
c <- bind_cols(b, dat4["time2"])
dlong <- c

rm("dat1","dat2","dat3","dat4","a","b","c")

# Create the question variable
dlong$question <- substr(dlong$guess_info,8,10)
dlong$question <- substr(dlong$question,2,2)
dlong$question <- as.numeric(dlong$question)

# Create the answer variable and make sure it corresponds to the question 
dlong <- dlong %>%
  mutate(answer = case_when(
    question == "1" ~ 32.3,
    question == "2" ~ 43.2,
    question == "3" ~ 57.7,
    question == "4" ~ 53.3,
    question == "5" ~ 32.5,
    question == "6" ~ 19.6
  ))

# Mean squared error ------------------------------------------------------
MSEfun <- function(guess, answer) {
  se <- (guess - answer)^2
  return(mean(se))
}

dlong <- dlong %>% rowwise() %>% 
  dplyr::mutate(mse.guess1 = MSEfun(guess_1, answer)) %>% 
  dplyr::mutate(mse.guess2 = MSEfun(guess_2,answer)) %>% 
  dplyr::mutate(mse.mean = MSEfun((guess_1 + guess_2)/2, answer))

glimpse(dlong)

# Mean absolute error -----------------------------------------------------
# Create a function to calculute the MAE
MAEfun <- function(guess, answer) {
  se <- (guess - answer)
  return(mean(abs(se)))
}

dlong <- dlong %>% rowwise() %>% 
  dplyr::mutate(mae.guess1 = MAEfun(guess_1, answer)) %>% 
  dplyr::mutate(mae.guess2 = MAEfun(guess_2,answer)) %>% 
  dplyr::mutate(mae.mean = MAEfun((guess_1 + guess_2)/2, answer))

# Convert the data to a tibble for easier use with tidyverse package
dlong <- as_tibble(dlong)
glimpse(dlong)

# Analysis ----------------------------------------------------------------
# Creating the datasets for comparisons
dlong_mse_1vm <- dlong %>% 
  select(id,condition,question,mse.guess1,mse.mean) %>%
  gather(mse,estimate,mse.guess1:mse.mean) %>% 
  mutate(c = ifelse(mse == "mse.guess1",-1,1))
dlong_mse_2vm <- dlong %>% 
  select(id,condition,question,mse.guess2,mse.mean) %>%
  gather(mse,estimate,mse.guess2:mse.mean) %>% 
  mutate(c = ifelse(mse == "mse.guess2",-1,1))
dlong_mae_1vm <- dlong %>% 
  select(id,condition,question,mae.guess1,mae.mean) %>%
  gather(mae,estimate,mae.guess1:mae.mean) %>% 
  mutate(c = ifelse(mae == "mae.guess1",-1,1))
dlong_mae_2vm <- dlong %>% 
  select(id,condition,question,mae.guess2,mae.mean) %>%
  gather(mae,estimate,mae.guess2:mae.mean) %>% 
  mutate(c = ifelse(mae == "mae.guess2",-1,1))

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_1vm)
summary(ic1)
6.45/sqrt(894)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_2vm)
summary(ic2)
15.71/sqrt(894)

options(pillar.sigfig = 6)
dlong %>% dplyr::summarise_at(c("mse.guess1",
                                "mse.guess2",
                                "mse.mean",
                                "mae.guess1",
                                "mae.guess2",
                                "mae.mean"),
                              list(mean))#  Get the means

dlong %>% dplyr::summarise_at(c("mse.guess1",
                                "mse.guess2",
                                "mse.mean",
                                "mae.guess1",
                                "mae.guess2",
                                "mae.mean"),
                              list(sd))#  Get the sd's

# WITHIN CROWD SELF
ic3 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Self"))
summary(ic3)
3.36/sqrt(449)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self"))
summary(ic4)
5.12/sqrt(449)

# WITHIN CROWD DISAGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree"))
summary(ic5)
5.84/sqrt(445)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree"))
summary(ic6)
16.56/sqrt(445)

dlong %>% group_by(condition) %>% dplyr::summarise_at(c("mse.guess1",
                                                        "mse.guess2",
                                                        "mse.mean",
                                                        "mae.guess1",
                                                        "mae.guess2",
                                                        "mae.mean"),
                                                      list(mean))#  Get the means
dlong %>% group_by(condition) %>% dplyr::summarise_at(c("mse.guess1",
                                                        "mse.guess2",
                                                        "mse.mean",
                                                        "mae.guess1",
                                                        "mae.guess2",
                                                        "mae.mean"),
                                                      list(sd))#  Get the sd's

# LOOKING AT THE DIFFERENCE SCORES (Benefit of averaging)
dlong$diffmse <- dlong$mse.guess1 - dlong$mse.mean
dlong$diffmae <- dlong$mae.guess1 - dlong$mae.mean
# Create conditions datasets for comparisons
dlong <- dlong %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))
  
table(d$condition)
ic7 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = dlong)
summary(ic7)
2.63*sqrt((1/445)+(1/449))


dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               mean)

dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               sd)

# TIME QUESTION ANALYSIS 
dlong %>% group_by(condition) %>% 
  summarise_at(c("time1","time2"),
               mean)

dlong %>% group_by(condition) %>% 
  summarise_at(c("time1","time2"),
               sd)

ic8 <- lmer(time1 ~ condition_c + (1|id) + (1|question), 
            data = dlong)
summary(ic8)
0.79*sqrt((1/445)+(1/449))#no difference

ic9 <- lmer(time2 ~ condition_c + (1|id) + (1|question), 
             data = dlong)
summary(ic9)
1.25*sqrt((1/445)+(1/449))#difference

# Write the time CSV
# Bayes t-test performed in JASP 
# Link to JASP: https://jasp-stats.org/download/ 
  
dlongBA <- dlong %>% 
  select(condition,time1,time2)
write.csv(dlongBA, "bayestest.csv")

