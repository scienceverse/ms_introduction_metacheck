

                            #####EXPERIMENT 2###### 

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
d <- read.csv("Data/2 - percentages.csv",header = T, stringsAsFactors = F)

d <- d[-(c(1,2)),]# Delete first two rows
d <- subset(d, d$Check == "7")# Pass check 13 fail
d <- subset(d, d$Look_up == "2")# Didn't look up answers 23 fail
names(d)
d <- d[,-c(1:25,62:65,69,70,72:78)]# Exclude columns we don't need 

# Age and gender statistics
describe(as.numeric(d$Age))
IQR(d$Age)
prop.table(table(d$Gender))

d$id <- 1:nrow(d)
# Creating a between condition variable 
names(d)[names(d) == 'C'] <- "condition"

# Combining the data ------------------------------------------------------
 
# This just combines  data from the conditions into the same variables
# You can mark everything from the first and second set and run it 
names(d)
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

names(d)
d <- d[,-c(13:36)]# We don't need these columns any more

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
             "Fertility_T2_Self_4","Phone_T2_Self_4","Oil_T2_Self_4","Life_T2_Self_4")

newnames = c("guess1.q1","guess1.q2","guess1.q3","guess1.q4",
             "guess1.q5","guess1.q6",
             "guess2.q1","guess2.q2","guess2.q3","guess2.q4",
             "guess2.q5","guess2.q6")

for(i in 1:12) names(d)[names(d) == oldnames[i]] = newnames[i]

# Making the variabels into proper types 
str(d)
d[,c(1:12)] <- sapply(
  d[,c(1:12)], as.numeric)
str(d)#Check if it worked 

# Re-shaping the data 
dat1 <- d %>% dplyr::select(id,condition,contains("guess1")) %>% 
  gather(guess_info,guess_1,guess1.q1:guess1.q6)

dat2 <- d %>% dplyr::select(id,condition,contains("guess2")) %>% 
  gather(guess_info,guess_2,guess2.q1:guess2.q6)

dlong <- bind_cols(dat1, dat2["guess_2"])

rm("dat1","dat2")

# Create the question variable
dlong$question <- substr(dlong$guess_info,8,10)

# Create the answer variable and make sure it corresponds to the question 
dlong <- dlong %>%
  mutate(answer = case_when(
    question == "q1" ~ 32.3,
    question == "q2" ~ 43.2,
    question == "q3" ~ 57.7,
    question == "q4" ~ 53.3,
    question == "q5" ~ 32.5,
    question == "q6" ~ 19.6
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

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_1vm)
summary(ic1)
6.96/sqrt(1389)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_2vm)
summary(ic2)
16.44/sqrt(1389)

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
3.31/sqrt(462)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self"))
summary(ic4)
5.32/sqrt(462)

# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree"))
tt <- getME(ic5,"theta")
ll <- getME(ic5,"lower")
derivs1 <- ic5@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic5,c("theta","fixef"))
ic5_update <- update(ic5,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic5_update)
2.92/sqrt(463)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree"))
tt <- getME(ic6,"theta")
ll <- getME(ic6,"lower")
derivs1 <- ic6@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic6,c("theta","fixef"))
ic6_update <- update(ic6,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic6_update)
5.04/sqrt(463)


# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree"))
summary(ic7)
5.95/sqrt(464)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree"))
summary(ic8)
17.13/sqrt(464)

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
0.51*sqrt((1/462)+(1/463))

ic10 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_d_s)
summary(ic10)
2.71*sqrt((1/462)+(1/464))

ic11 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic11)
3.23*sqrt((1/463)+(1/464))

dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               mean)

dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               sd)
