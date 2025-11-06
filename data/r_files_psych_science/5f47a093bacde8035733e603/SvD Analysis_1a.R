

                            #####EXPERIMENT 1a######

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
d <- read.csv("Data/1a - weights.csv",header = T, stringsAsFactors = F)

d <- d[-(c(1,2)),]# Delete first two rows
d <- subset(d, d$Check == "7")# Pass check 4 fail
d <- subset(d, d$Look_up == "2")# Didn't look up answers 19 fail
names(d)

d <- d[,-c(1:25,67,69,73,74,76:80)]#  Exclude columns we don't need

# Age and gender statistics
describe(as.numeric(d$Age))
IQR(d$Age)
prop.table(table(d$Gender))
d$id <- 1:nrow(d)
#Creating a between condition variable 
names(d)[names(d) == 'C'] <- "condition"

# Combining and organizing the data ---------------------------------------

# This  combines  data from the conditions into the same variables
# You can mark everything from the first and second set and run it 
names(d)
#First set
d$Sumo_T1_Self_4 <- ifelse(d$Sumo_T1_Self_4 == "", d$Sumo_T1_Dis_4, d$Sumo_T1_Self_4)
d$Bull_T1_Self_4 <- ifelse(d$Bull_T1_Self_4 == "", d$Bull_T1_Dis_4, d$Bull_T1_Self_4)
d$Mini_T1_Self_4 <- ifelse(d$Mini_T1_Self_4 == "", d$Mini_T1_Dis_4, d$Mini_T1_Self_4)
d$Polar_T1_Self_4 <- ifelse(d$Polar_T1_Self_4 == "", d$Polar_T1_Dis_4, d$Polar_T1_Self_4)
d$Piano_T1_Self_4 <- ifelse(d$Piano_T1_Self_4 == "", d$Piano_T1_Dis_4, d$Piano_T1_Self_4)
d$Honda_T1_Self_4 <- ifelse(d$Honda_T1_Self_4 == "", d$Honda_T1_Dis_4, d$Honda_T1_Self_4)
d$Bell_T1_Self_4 <- ifelse(d$Bell_T1_Self_4 == "", d$Bell_T1_Dis_4, d$Bell_T1_Self_4)
d$Pole_T1_Self_4 <- ifelse(d$Pole_T1_Self_4 == "", d$Pole_T1_Dis_4, d$Pole_T1_Self_4)
d$Machine_T1_Self_4 <- ifelse(d$Machine_T1_Self_4 == "", d$Machine_T1_Dis_4, d$Machine_T1_Self_4)
d$Elephant_T1_Self_4 <- ifelse(d$Elephant_T1_Self_4 == "", d$Elephant_T1_Dis_4, d$Elephant_T1_Self_4)

d$Sumo_T2_Self_4 <- ifelse(d$Sumo_T2_Self_4 == "", d$Sumo_T2_Dis_4, d$Sumo_T2_Self_4)
d$Bull_T2_Self_4 <- ifelse(d$Bull_T2_Self_4 == "", d$Bull_T2_Dis_4, d$Bull_T2_Self_4)
d$Mini_T2_Self_4 <- ifelse(d$Mini_T2_Self_4 == "", d$Mini_T2_Dis_4, d$Mini_T2_Self_4)
d$Polar_T2_Self_4 <- ifelse(d$Polar_T2_Self_4 == "", d$Polar_T2_Dis_4, d$Polar_T2_Self_4)
d$Piano_T2_Self_4 <- ifelse(d$Piano_T2_Self_4 == "", d$Piano_T2_Dis_4, d$Piano_T2_Self_4)
d$Honda_T2_Self_4 <- ifelse(d$Honda_T2_Self_4 == "", d$Honda_T2_Dis_4, d$Honda_T2_Self_4)
d$Bell_T2_Self_4 <- ifelse(d$Bell_T2_Self_4 == "", d$Bell_T2_Dis_4, d$Bell_T2_Self_4)
d$Pole_T2_Self_4 <- ifelse(d$Pole_T2_Self_4 == "", d$Pole_T2_Dis_4, d$Pole_T2_Self_4)
d$Machine_T2_Self_4 <- ifelse(d$Machine_T2_Self_4 == "", d$Machine_T2_Dis_4, d$Machine_T2_Self_4)
d$Elephant_T2_Self_4 <- ifelse(d$Elephant_T2_Self_4 == "", d$Elephant_T2_Dis_4, d$Elephant_T2_Self_4)

d <- d[,-c(21:40)]# We don't need these columns any more

# Condition variable needs to be made a factor with more usable labels
levels(as.factor(d$condition))
d$condition <- as.factor(d$condition)

#These lines below will make the new variable names
#---------
#q1 = sumo
#q2 = bull
#q3 = mini
#q4 = polar
#q5 = piano
#q6 = honda 
#q7 = bell
#q8 = pole 
#q9 = machine
#q9 = elephant

names(d)
oldnames = c("Sumo_T1_Self_4","Bull_T1_Self_4","Mini_T1_Self_4","Polar_T1_Self_4",
             "Piano_T1_Self_4","Honda_T1_Self_4","Bell_T1_Self_4","Pole_T1_Self_4",
             "Machine_T1_Self_4","Elephant_T1_Self_4",
             "Sumo_T2_Self_4","Bull_T2_Self_4","Mini_T2_Self_4","Polar_T2_Self_4",
             "Piano_T2_Self_4","Honda_T2_Self_4","Bell_T2_Self_4","Pole_T2_Self_4",
             "Machine_T2_Self_4","Elephant_T2_Self_4")

newnames = c("guess1.q1","guess1.q2","guess1.q3","guess1.q4",
             "guess1.q5","guess1.q6","guess1.q7","guess1.q8","guess1.q9","guess1.q10",
             "guess2.q1","guess2.q2","guess2.q3","guess2.q4",
             "guess2.q5","guess2.q6","guess2.q7","guess2.q8","guess2.q9","guess2.q10")

for(i in 1:20) names(d)[names(d) == oldnames[i]] = newnames[i]

# Making the variabels into proper types 
str(d)
d[,c(1:20)] <- sapply(
  d[,c(1:20)], as.numeric)
str(d)#Check if it worked 

# Re-shaping the data 
dat1 <- d %>% dplyr::select(id,condition,contains("guess1")) %>% 
  gather(guess_info,guess_1,guess1.q1:guess1.q10)
  
dat2 <- d %>% dplyr::select(id,condition,contains("guess2")) %>% 
  gather(guess_info,guess_2,guess2.q1:guess2.q10)

dlong <- bind_cols(dat1, dat2["guess_2"])

# Create the question variable
dlong$question <- substr(dlong$guess_info,8,10)

# Create the answer variable and make sure it corresponds to the question 
dlong <- dlong %>%
  mutate(answer = case_when(
    question == "q1" ~ 633,
    question == "q2" ~ 1900,
    question == "q3" ~ 2625,
    question == "q4" ~ 1157.5,
    question == "q5" ~ 990,
    question == "q6" ~ 787,
    question == "q7" ~ 2080,
    question == "q8" ~ 175,
    question == "q9" ~ 175,
    question == "q10" ~ 234
  ))

rm("dat1","dat2")

# Mean squared error ------------------------------------------------------
# Create a function to calculute the MSE
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
# WITHIN CROWD OVERALL
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

ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_1vm)
summary(ic1)
5.56/sqrt(880)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_2vm)
summary(ic2)
8.40/sqrt(880)

glimpse(dlong)
options(pillar.sigfig = 8)
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
# Use a different optimizer
tt <- getME(ic3,"theta")
ll <- getME(ic3,"lower")
derivs1 <- ic3@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic3,c("theta","fixef"))
ic3_update <- update(ic3,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic3_update)
3.09/sqrt(441)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self"))
summary(ic4)
3.31/sqrt(441)


# WITHIN CROWD DISAGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree"))
summary(ic5)
4.80/sqrt(439)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree"))
summary(ic6)
8.44/sqrt(439)

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

# BENEFIT OF AVERAGING
dlong$diffmse <- dlong$mse.guess1 - dlong$mse.mean
dlong$diffmae <- dlong$mae.guess1 - dlong$mae.mean
# Create a condition varible 
dlong$condition_c <- ifelse(dlong$condition == "Self",0.5,-0.5)

ic7 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = dlong)
summary(ic7)
2.30*sqrt((1/439)+(1/441))


dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               mean)

dlong %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               sd)
