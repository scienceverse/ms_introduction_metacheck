

                          #####EXPERIMENT 3###### 

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
d <- read.csv("Data/3 - mechanism.csv",header = T, stringsAsFactors = F)

d <- d[-(c(1,2)),]# Delete first two rows
d <- subset(d, d$Check == "7")# Pass check 13 fail
1500-1487
d <- subset(d, d$Look_up == "2")# Didn't look up answers 61 fail
1487-1426
names(d)
d <- d[,-c(1:26,81:83,85,87,88)]# Exclude columns we don't need 

# Age and gender statistics
describe(as.numeric(d$Age))
IQR(d$Age, na.rm = T)
prop.table(table(d$Gender))

d$id <- 1:nrow(d)
# Creating a between condition variable 
names(d)[names(d) == 'C'] <- "condition"

# Combining the data ------------------------------------------------------
 
# This just combines  data from the conditions into the same variables
# You can mark everything from the first and second set and run it 
names(d)
# Extreme question has _4 appended, Second guess has _5 appended

# First set
d$Airport_T1_Self_4 <- ifelse(d$Airport_T1_Self_4 == "", d$Airport_T1_Friend_4, d$Airport_T1_Self_4)
d$Population_T1_Self_4 <- ifelse(d$Population_T1_Self_4 == "", d$Population_T1_Friend_4, d$Population_T1_Self_4)
d$Fertility_T1_Self_4 <- ifelse(d$Fertility_T1_Self_4 == "", d$Fertility_T1_Friend_4, d$Fertility_T1_Self_4)
d$Phone_T1_Self_4 <- ifelse(d$Phone_T1_Self_4 == "", d$Phone_T1_Friend_4, d$Phone_T1_Self_4)
d$Oil_T1_Self_4 <- ifelse(d$Oil_T1_Self_4 == "", d$Oil_T1_Friend_4, d$Oil_T1_Self_4)
d$Life_T1_Self_4 <- ifelse(d$Life_T1_Self_4 == "", d$Life_T1_Friend_4, d$Life_T1_Self_4)

d$Airport_T1_Self_4 <- ifelse(d$Airport_T1_Self_4 == "", d$Airport_T1_Dis_4, d$Airport_T1_Self_4)
d$Population_T1_Self_4 <- ifelse(d$Population_T1_Self_4 == "", d$Population_T1_Dis_4, d$Population_T1_Self_4)
d$Fertility_T1_Self_4 <- ifelse(d$Fertility_T1_Self_4 == "", d$Fertility_T1_Dis_4, d$Fertility_T1_Self_4)
d$Phone_T1_Self_4 <- ifelse(d$Phone_T1_Self_4 == "", d$Phone_T1_Dis_4, d$Phone_T1_Self_4)
d$Oil_T1_Self_4 <- ifelse(d$Oil_T1_Self_4 == "", d$Oil_T1_Dis_4, d$Oil_T1_Self_4)
d$Life_T1_Self_4 <- ifelse(d$Life_T1_Self_4 == "", d$Life_T1_Dis_4, d$Life_T1_Self_4)

#Second set
#Extreme questions
d$Airport_T2_Self_4 <- ifelse(d$Airport_T2_Self_4 == "", d$Airport_T2_Friend_4, d$Airport_T2_Self_4)
d$Population_T2_Self_4 <- ifelse(d$Population_T2_Self_4 == "", d$Population_T2_Friend_4, d$Population_T2_Self_4)
d$Fertility_T2_Self_4 <- ifelse(d$Fertility_T2_Self_4 == "", d$Fertility_T2_Friend_4, d$Fertility_T2_Self_4)
d$Phone_T2_Self_4 <- ifelse(d$Phone_T2_Self_4 == "", d$Phone_T2_Friend_4, d$Phone_T2_Self_4)
d$Oil_T2_Self_4 <- ifelse(d$Oil_T2_Self_4 == "", d$Oil_T2_Friend_4, d$Oil_T2_Self_4)
d$Life_T2_Self_4 <- ifelse(d$Life_T2_Self_4 == "", d$Life_T2_Friend_4, d$Life_T2_Self_4)

d$Airport_T2_Self_4 <- ifelse(d$Airport_T2_Self_4 == "", d$Airport_T2_Dis_4, d$Airport_T2_Self_4)
d$Population_T2_Self_4 <- ifelse(d$Population_T2_Self_4 == "", d$Population_T2_Dis_4, d$Population_T2_Self_4)
d$Fertility_T2_Self_4 <- ifelse(d$Fertility_T2_Self_4 == "", d$Fertility_T2_Dis_4, d$Fertility_T2_Self_4)
d$Phone_T2_Self_4 <- ifelse(d$Phone_T2_Self_4 == "", d$Phone_T2_Dis_4, d$Phone_T2_Self_4)
d$Oil_T2_Self_4 <- ifelse(d$Oil_T2_Self_4 == "", d$Oil_T2_Dis_4, d$Oil_T2_Self_4)
d$Life_T2_Self_4 <- ifelse(d$Life_T2_Self_4 == "", d$Life_T2_Dis_4, d$Life_T2_Self_4)

#Second guess
d$Airport_T2_Self_5 <- ifelse(d$Airport_T2_Self_5 == "", d$Airport_T2_Friend_5, d$Airport_T2_Self_5)
d$Population_T2_Self_5 <- ifelse(d$Population_T2_Self_5 == "", d$Population_T2_Friend_5, d$Population_T2_Self_5)
d$Fertility_T2_Self_5 <- ifelse(d$Fertility_T2_Self_5 == "", d$Fertility_T2_Friend_5, d$Fertility_T2_Self_5)
d$Phone_T2_Self_5 <- ifelse(d$Phone_T2_Self_5 == "", d$Phone_T2_Friend_5, d$Phone_T2_Self_5)
d$Oil_T2_Self_5 <- ifelse(d$Oil_T2_Self_5 == "", d$Oil_T2_Friend_5, d$Oil_T2_Self_5)
d$Life_T2_Self_5 <- ifelse(d$Life_T2_Self_5 == "", d$Life_T2_Friend_5, d$Life_T2_Self_5)

d$Airport_T2_Self_5 <- ifelse(d$Airport_T2_Self_5 == "", d$Airport_T2_Dis_5, d$Airport_T2_Self_5)
d$Population_T2_Self_5 <- ifelse(d$Population_T2_Self_5 == "", d$Population_T2_Dis_5, d$Population_T2_Self_5)
d$Fertility_T2_Self_5 <- ifelse(d$Fertility_T2_Self_5 == "", d$Fertility_T2_Dis_5, d$Fertility_T2_Self_5)
d$Phone_T2_Self_5 <- ifelse(d$Phone_T2_Self_5 == "", d$Phone_T2_Dis_5, d$Phone_T2_Self_5)
d$Oil_T2_Self_5 <- ifelse(d$Oil_T2_Self_5 == "", d$Oil_T2_Dis_5, d$Oil_T2_Self_5)
d$Life_T2_Self_5 <- ifelse(d$Life_T2_Self_5 == "", d$Life_T2_Dis_5, d$Life_T2_Self_5)

names(d)
d <- d[,-c(19:54)]# We don't need these columns any more

#Condition variable needs to be made a factor with more usable labels
levels(as.factor(d$condition))
d$condition <- factor(d$condition,
                      levels = c("Friend_agree","Friend_disagree","Self"),
                      labels = c("Agree","Disagree","Self"))
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
             "Airport_T2_Self_5","Population_T2_Self_5",
             "Fertility_T2_Self_5","Phone_T2_Self_5","Oil_T2_Self_5","Life_T2_Self_5")

newnames = c("guess1.q1","guess1.q2","guess1.q3","guess1.q4",
             "guess1.q5","guess1.q6",
             "x.q1","x.q2","x.q3","x.q4","x.q5","x.q6",
             "guess2.q1","guess2.q2","guess2.q3","guess2.q4",
             "guess2.q5","guess2.q6")

for(i in 1:18) names(d)[names(d) == oldnames[i]] = newnames[i]

# Making the variabels into proper types 
str(d)
d[,c(1:20)] <- sapply(
  d[,c(1:20)], as.numeric)
str(d)#Check if it worked 


# Re-shaping the data 
dat1 <- d %>% dplyr::select(id,condition,contains("guess1")) %>% 
  gather(guess_info, guess_1, guess1.q1:guess1.q6)

dat2 <- d %>% dplyr::select(id,condition,contains("guess2")) %>% 
  gather(guess_info, guess_2, guess2.q1:guess2.q6)

dat3 <- d %>% dplyr::select(id,condition,contains("x")) %>% 
  gather(guess_info, extreme, x.q1:x.q6)

a <- bind_cols(dat1, dat2["guess_2"])
b <- bind_cols(a, dat3["extreme"])
dlong <- b

rm("dat1","dat2","dat3","a","b")

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

# Calculate the difference scores from extremes
dlong$diffexfirst <- abs(dlong$extreme - dlong$guess_1)
dlong$diffexsecond <- abs(dlong$extreme - dlong$guess_2)

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_1vm)
summary(ic1)
8.08/sqrt(1426)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_2vm)
summary(ic2)
12.61/sqrt(1426)

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
3.62/sqrt(468)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self"))
summary(ic4)
4.86/sqrt(468)

# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree"))
summary(ic5)
4.95/sqrt(477)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree"))
summary(ic6)
4.12/sqrt(477)


# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree"))
summary(ic7)
5.40/sqrt(481)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree"))
summary(ic8)
12.32/sqrt(481)

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
d_a_s <- dlong %>% 
  filter(condition == "Agree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_s <- dlong %>% 
  filter(condition == "Disagree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_a <- dlong %>% 
  filter(condition == "Disagree" | condition == "Agree") %>% 
  mutate(condition_c = ifelse(condition == "Agree",0.5,-0.5))

ic9 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic9)
2.03*sqrt((1/477)+(1/468))

ic10 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_d_s)
summary(ic10)
2.30*sqrt((1/481)+(1/468))#disagree better

ic11 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic11)
0.60*sqrt((1/477)+(1/481))#no difference disagree vs. agree


dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               mean)

dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               sd)

# EXTREME QUESTION ANALYSIS 
dlong %>% group_by(condition) %>% 
  summarise_at(c("diffexfirst","diffexsecond"),
               mean)

dlong %>% group_by(condition) %>% 
  summarise_at(c("diffexfirst","diffexsecond"),
               sd)

ic12 <- lmer(diffexfirst ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic12)
0.66*sqrt((1/477)+(1/468))

ic13 <- lmer(diffexfirst ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic13)
6.27*sqrt((1/481)+(1/468))

ic14 <- lmer(diffexfirst ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic14)
7.18*sqrt((1/477)+(1/481))

# Now the closness 
ic15 <- lmer(diffexsecond ~ condition_c + (1|id) + (1|question), 
             data = d_a_s)
summary(ic15)
1.911*sqrt((1/477)+(1/468))

ic16 <- lmer(diffexsecond ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
tt <- getME(ic16,"theta")
ll <- getME(ic16,"lower")
derivs1 <- ic16@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic16,c("theta","fixef"))
ic16_update <- update(ic16,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic16_update)
3.83*sqrt((1/481)+(1/468))

ic17 <- lmer(diffexsecond ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
tt <- getME(ic17,"theta")
ll <- getME(ic17,"lower")
derivs1 <- ic17@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic17,c("theta","fixef"))
ic17_update <- update(ic17,
                      start=ss,
                      control=
                        lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
summary(ic17_update)
2.10*sqrt((1/477)+(1/481)) 
