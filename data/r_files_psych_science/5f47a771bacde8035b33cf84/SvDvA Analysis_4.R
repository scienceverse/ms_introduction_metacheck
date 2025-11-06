

                          #####EXPERIMENT 4###### 

# Packages ----------------------------------------------------------------
for(pack in c("tidyverse","lme4","psych","lme4","lmerTest")) {
  if(!(pack %in% installed.packages()[,1])) {
    install.packages(pack)  
  }
  library(pack, character.only = T)
}

# Choose the working directory

# Importing data ----------------------------------------------------------
# Wave 1
d1a <- read.csv("Data/4a - Large test.csv",header = T, stringsAsFactors = F)
# Wave 2
d1b <- read.csv("Data/4b - Prereg test.csv",header = T, stringsAsFactors = F)

# Data cleaning  ----------------------------------------------------------
d1a$wave <- "1"
d1b$wave <- "2"

# Excluding attention failures and unnecessary variables 
# Eliminate the first two rows (it's qualtrics meta data repeated)
d1a <- d1a[-(c(1,2)),]
d1b <- d1b[-(c(1,2)),]

# Only keep people who correctly answered the check with a 7
d1a <- subset(d1a, d1a$Check == "7")# 6 failed
d1b <- subset(d1b, d1b$Check == "7")# 8 failed

# Only keep people who said that they didn't look up the answers
d1a <- subset(d1a, d1a$Look_up == "2")
983-964
d1b <- subset(d1b, d1b$Look_up == "2")
891-872

# Exclude unecessary columns
names(d1a)# Names are the same in both data sets 
d1a <- d1a[,-c(1:25,99,101,105,106,108:114)]
d1b <- d1b[,-c(1:25,99,101,105,106,108:114)]

# Combining the data per data set
# First set D1A
d1a$Airport_T1_Self_4 <- ifelse(d1a$Airport_T1_Self_4 == "", d1a$Airport_T1_Friend_4, d1a$Airport_T1_Self_4)
d1a$Population_T1_Self_4 <- ifelse(d1a$Population_T1_Self_4 == "", d1a$Population_T1_Friend_4, d1a$Population_T1_Self_4)
d1a$Ocean_T1_Self_4 <- ifelse(d1a$Ocean_T1_Self_4 == "", d1a$Ocean_T1_Friend_4, d1a$Ocean_T1_Self_4)
d1a$Roads_T1_Self_4 <- ifelse(d1a$Roads_T1_Self_4 == "", d1a$Roads_T1_Friend_4, d1a$Roads_T1_Self_4)
d1a$Fertility_T1_Self_4 <- ifelse(d1a$Fertility_T1_Self_4 == "", d1a$Fertility_T1_Friend_4, d1a$Fertility_T1_Self_4)
d1a$Phone_T1_Self_4 <- ifelse(d1a$Phone_T1_Self_4 == "", d1a$Phone_T1_Friend_4, d1a$Phone_T1_Self_4)
d1a$Oil_T1_Self_4 <- ifelse(d1a$Oil_T1_Self_4 == "", d1a$Oil_T1_Friend_4, d1a$Oil_T1_Self_4)
d1a$Life_T1_Self_4 <- ifelse(d1a$Life_T1_Self_4 == "", d1a$Life_T1_Friend_4, d1a$Life_T1_Self_4)
d1a$Age_T1_Self_4 <- ifelse(d1a$Age_T1_Self_4 == "", d1a$Age_T1_Friend_4, d1a$Age_T1_Self_4)
d1a$Internet_T1_Self_4 <- ifelse(d1a$Internet_T1_Self_4 == "", d1a$Internet_T1_Friend_4, d1a$Internet_T1_Self_4)
d1a$Cellphone_T1_Self_4 <- ifelse(d1a$Cellphone_T1_Self_4 == "", d1a$Cellphone_T1_Friend_4, d1a$Cellphone_T1_Self_4)
d1a$China_T1_Self_4 <- ifelse(d1a$China_T1_Self_4 == "", d1a$China_T1_Friend_4, d1a$China_T1_Self_4)

d1a$Airport_T1_Self_4 <- ifelse(d1a$Airport_T1_Self_4 == "", d1a$Airport_T1_Dis_4, d1a$Airport_T1_Self_4)
d1a$Population_T1_Self_4 <- ifelse(d1a$Population_T1_Self_4 == "", d1a$Population_T1_Dis_4, d1a$Population_T1_Self_4)
d1a$Ocean_T1_Self_4 <- ifelse(d1a$Ocean_T1_Self_4 == "", d1a$Ocean_T1_Dis_4, d1a$Ocean_T1_Self_4)
d1a$Roads_T1_Self_4 <- ifelse(d1a$Roads_T1_Self_4 == "", d1a$Roads_T1_Dis_4, d1a$Roads_T1_Self_4)
d1a$Fertility_T1_Self_4 <- ifelse(d1a$Fertility_T1_Self_4 == "", d1a$Fertility_T1_Dis_4, d1a$Fertility_T1_Self_4)
d1a$Phone_T1_Self_4 <- ifelse(d1a$Phone_T1_Self_4 == "", d1a$Phone_T1_Dis_4, d1a$Phone_T1_Self_4)
d1a$Oil_T1_Self_4 <- ifelse(d1a$Oil_T1_Self_4 == "", d1a$Oil_T1_Dis_4, d1a$Oil_T1_Self_4)
d1a$Life_T1_Self_4 <- ifelse(d1a$Life_T1_Self_4 == "", d1a$Life_T1_Dis_4, d1a$Life_T1_Self_4)
d1a$Age_T1_Self_4 <- ifelse(d1a$Age_T1_Self_4 == "", d1a$Age_T1_Dis_4, d1a$Age_T1_Self_4)
d1a$Internet_T1_Self_4 <- ifelse(d1a$Internet_T1_Self_4 == "", d1a$Internet_T1_Dis_4, d1a$Internet_T1_Self_4)
d1a$Cellphone_T1_Self_4 <- ifelse(d1a$Cellphone_T1_Self_4 == "", d1a$Cellphone_T1_Dis_4, d1a$Cellphone_T1_Self_4)
d1a$China_T1_Self_4 <- ifelse(d1a$China_T1_Self_4 == "", d1a$China_T1_Dis_4, d1a$China_T1_Self_4)
#--
d1a$Airport_T2_Self_4 <- ifelse(d1a$Airport_T2_Self_4 == "", d1a$Airport_T2_Friend_4, d1a$Airport_T2_Self_4)
d1a$Population_T2_Self_4 <- ifelse(d1a$Population_T2_Self_4 == "", d1a$Population_T2_Friend_4, d1a$Population_T2_Self_4)
d1a$Ocean_T2_Self_4 <- ifelse(d1a$Ocean_T2_Self_4 == "", d1a$Ocean_T2_Friend_4, d1a$Ocean_T2_Self_4)
d1a$Roads_T2_Self_4 <- ifelse(d1a$Roads_T2_Self_4 == "", d1a$Roads_T2_Friend_4, d1a$Roads_T2_Self_4)
d1a$Fertility_T2_Self_4 <- ifelse(d1a$Fertility_T2_Self_4 == "", d1a$Fertility_T2_Friend_4, d1a$Fertility_T2_Self_4)
d1a$Phone_T2_Self_4 <- ifelse(d1a$Phone_T2_Self_4 == "", d1a$Phone_T2_Friend_4, d1a$Phone_T2_Self_4)
d1a$Oil_T2_Self_4 <- ifelse(d1a$Oil_T2_Self_4 == "", d1a$Oil_T2_Friend_4, d1a$Oil_T2_Self_4)
d1a$Life_T2_Self_4 <- ifelse(d1a$Life_T2_Self_4 == "", d1a$Life_T2_Friend_4, d1a$Life_T2_Self_4)
d1a$Age_T2_Self_4 <- ifelse(d1a$Age_T2_Self_4 == "", d1a$Age_T2_Friend_4, d1a$Age_T2_Self_4)
d1a$Internet_T2_Self_4 <- ifelse(d1a$Internet_T2_Self_4 == "", d1a$Internet_T2_Friend_4, d1a$Internet_T2_Self_4)
d1a$Cellphone_T2_Self_4 <- ifelse(d1a$Cellphone_T2_Self_4 == "", d1a$Cellphone_T2_Friend_4, d1a$Cellphone_T2_Self_4)
d1a$China_T2_Self_4 <- ifelse(d1a$China_T2_Self_4 == "", d1a$China_T2_Friend_4, d1a$China_T2_Self_4)

d1a$Airport_T2_Self_4 <- ifelse(d1a$Airport_T2_Self_4 == "", d1a$Airport_T2_Dis_4, d1a$Airport_T2_Self_4)
d1a$Population_T2_Self_4 <- ifelse(d1a$Population_T2_Self_4 == "", d1a$Population_T2_Dis_4, d1a$Population_T2_Self_4)
d1a$Ocean_T2_Self_4 <- ifelse(d1a$Ocean_T2_Self_4 == "", d1a$Ocean_T2_Dis_4, d1a$Ocean_T2_Self_4)
d1a$Roads_T2_Self_4 <- ifelse(d1a$Roads_T2_Self_4 == "", d1a$Roads_T2_Dis_4, d1a$Roads_T2_Self_4)
d1a$Fertility_T2_Self_4 <- ifelse(d1a$Fertility_T2_Self_4 == "", d1a$Fertility_T2_Dis_4, d1a$Fertility_T2_Self_4)
d1a$Phone_T2_Self_4 <- ifelse(d1a$Phone_T2_Self_4 == "", d1a$Phone_T2_Dis_4, d1a$Phone_T2_Self_4)
d1a$Oil_T2_Self_4 <- ifelse(d1a$Oil_T2_Self_4 == "", d1a$Oil_T2_Dis_4, d1a$Oil_T2_Self_4)
d1a$Life_T2_Self_4 <- ifelse(d1a$Life_T2_Self_4 == "", d1a$Life_T2_Dis_4, d1a$Life_T2_Self_4)
d1a$Age_T2_Self_4 <- ifelse(d1a$Age_T2_Self_4 == "", d1a$Age_T2_Dis_4, d1a$Age_T2_Self_4)
d1a$Internet_T2_Self_4 <- ifelse(d1a$Internet_T2_Self_4 == "", d1a$Internet_T2_Dis_4, d1a$Internet_T2_Self_4)
d1a$Cellphone_T2_Self_4 <- ifelse(d1a$Cellphone_T2_Self_4 == "", d1a$Cellphone_T2_Dis_4, d1a$Cellphone_T2_Self_4)
d1a$China_T2_Self_4 <- ifelse(d1a$China_T2_Self_4 == "", d1a$China_T2_Dis_4, d1a$China_T2_Self_4)
#--
# Second set D1B 
d1b$Airport_T1_Self_4 <- ifelse(d1b$Airport_T1_Self_4 == "", d1b$Airport_T1_Friend_4, d1b$Airport_T1_Self_4)
d1b$Population_T1_Self_4 <- ifelse(d1b$Population_T1_Self_4 == "", d1b$Population_T1_Friend_4, d1b$Population_T1_Self_4)
d1b$Ocean_T1_Self_4 <- ifelse(d1b$Ocean_T1_Self_4 == "", d1b$Ocean_T1_Friend_4, d1b$Ocean_T1_Self_4)
d1b$Roads_T1_Self_4 <- ifelse(d1b$Roads_T1_Self_4 == "", d1b$Roads_T1_Friend_4, d1b$Roads_T1_Self_4)
d1b$Fertility_T1_Self_4 <- ifelse(d1b$Fertility_T1_Self_4 == "", d1b$Fertility_T1_Friend_4, d1b$Fertility_T1_Self_4)
d1b$Phone_T1_Self_4 <- ifelse(d1b$Phone_T1_Self_4 == "", d1b$Phone_T1_Friend_4, d1b$Phone_T1_Self_4)
d1b$Oil_T1_Self_4 <- ifelse(d1b$Oil_T1_Self_4 == "", d1b$Oil_T1_Friend_4, d1b$Oil_T1_Self_4)
d1b$Life_T1_Self_4 <- ifelse(d1b$Life_T1_Self_4 == "", d1b$Life_T1_Friend_4, d1b$Life_T1_Self_4)
d1b$Age_T1_Self_4 <- ifelse(d1b$Age_T1_Self_4 == "", d1b$Age_T1_Friend_4, d1b$Age_T1_Self_4)
d1b$Internet_T1_Self_4 <- ifelse(d1b$Internet_T1_Self_4 == "", d1b$Internet_T1_Friend_4, d1b$Internet_T1_Self_4)
d1b$Cellphone_T1_Self_4 <- ifelse(d1b$Cellphone_T1_Self_4 == "", d1b$Cellphone_T1_Friend_4, d1b$Cellphone_T1_Self_4)
d1b$China_T1_Self_4 <- ifelse(d1b$China_T1_Self_4 == "", d1b$China_T1_Friend_4, d1b$China_T1_Self_4)

d1b$Airport_T1_Self_4 <- ifelse(d1b$Airport_T1_Self_4 == "", d1b$Airport_T1_Dis_4, d1b$Airport_T1_Self_4)
d1b$Population_T1_Self_4 <- ifelse(d1b$Population_T1_Self_4 == "", d1b$Population_T1_Dis_4, d1b$Population_T1_Self_4)
d1b$Ocean_T1_Self_4 <- ifelse(d1b$Ocean_T1_Self_4 == "", d1b$Ocean_T1_Dis_4, d1b$Ocean_T1_Self_4)
d1b$Roads_T1_Self_4 <- ifelse(d1b$Roads_T1_Self_4 == "", d1b$Roads_T1_Dis_4, d1b$Roads_T1_Self_4)
d1b$Fertility_T1_Self_4 <- ifelse(d1b$Fertility_T1_Self_4 == "", d1b$Fertility_T1_Dis_4, d1b$Fertility_T1_Self_4)
d1b$Phone_T1_Self_4 <- ifelse(d1b$Phone_T1_Self_4 == "", d1b$Phone_T1_Dis_4, d1b$Phone_T1_Self_4)
d1b$Oil_T1_Self_4 <- ifelse(d1b$Oil_T1_Self_4 == "", d1b$Oil_T1_Dis_4, d1b$Oil_T1_Self_4)
d1b$Life_T1_Self_4 <- ifelse(d1b$Life_T1_Self_4 == "", d1b$Life_T1_Dis_4, d1b$Life_T1_Self_4)
d1b$Age_T1_Self_4 <- ifelse(d1b$Age_T1_Self_4 == "", d1b$Age_T1_Dis_4, d1b$Age_T1_Self_4)
d1b$Internet_T1_Self_4 <- ifelse(d1b$Internet_T1_Self_4 == "", d1b$Internet_T1_Dis_4, d1b$Internet_T1_Self_4)
d1b$Cellphone_T1_Self_4 <- ifelse(d1b$Cellphone_T1_Self_4 == "", d1b$Cellphone_T1_Dis_4, d1b$Cellphone_T1_Self_4)
d1b$China_T1_Self_4 <- ifelse(d1b$China_T1_Self_4 == "", d1b$China_T1_Dis_4, d1b$China_T1_Self_4)
#--
d1b$Airport_T2_Self_4 <- ifelse(d1b$Airport_T2_Self_4 == "", d1b$Airport_T2_Friend_4, d1b$Airport_T2_Self_4)
d1b$Population_T2_Self_4 <- ifelse(d1b$Population_T2_Self_4 == "", d1b$Population_T2_Friend_4, d1b$Population_T2_Self_4)
d1b$Ocean_T2_Self_4 <- ifelse(d1b$Ocean_T2_Self_4 == "", d1b$Ocean_T2_Friend_4, d1b$Ocean_T2_Self_4)
d1b$Roads_T2_Self_4 <- ifelse(d1b$Roads_T2_Self_4 == "", d1b$Roads_T2_Friend_4, d1b$Roads_T2_Self_4)
d1b$Fertility_T2_Self_4 <- ifelse(d1b$Fertility_T2_Self_4 == "", d1b$Fertility_T2_Friend_4, d1b$Fertility_T2_Self_4)
d1b$Phone_T2_Self_4 <- ifelse(d1b$Phone_T2_Self_4 == "", d1b$Phone_T2_Friend_4, d1b$Phone_T2_Self_4)
d1b$Oil_T2_Self_4 <- ifelse(d1b$Oil_T2_Self_4 == "", d1b$Oil_T2_Friend_4, d1b$Oil_T2_Self_4)
d1b$Life_T2_Self_4 <- ifelse(d1b$Life_T2_Self_4 == "", d1b$Life_T2_Friend_4, d1b$Life_T2_Self_4)
d1b$Age_T2_Self_4 <- ifelse(d1b$Age_T2_Self_4 == "", d1b$Age_T2_Friend_4, d1b$Age_T2_Self_4)
d1b$Internet_T2_Self_4 <- ifelse(d1b$Internet_T2_Self_4 == "", d1b$Internet_T2_Friend_4, d1b$Internet_T2_Self_4)
d1b$Cellphone_T2_Self_4 <- ifelse(d1b$Cellphone_T2_Self_4 == "", d1b$Cellphone_T2_Friend_4, d1b$Cellphone_T2_Self_4)
d1b$China_T2_Self_4 <- ifelse(d1b$China_T2_Self_4 == "", d1b$China_T2_Friend_4, d1b$China_T2_Self_4)

d1b$Airport_T2_Self_4 <- ifelse(d1b$Airport_T2_Self_4 == "", d1b$Airport_T2_Dis_4, d1b$Airport_T2_Self_4)
d1b$Population_T2_Self_4 <- ifelse(d1b$Population_T2_Self_4 == "", d1b$Population_T2_Dis_4, d1b$Population_T2_Self_4)
d1b$Ocean_T2_Self_4 <- ifelse(d1b$Ocean_T2_Self_4 == "", d1b$Ocean_T2_Dis_4, d1b$Ocean_T2_Self_4)
d1b$Roads_T2_Self_4 <- ifelse(d1b$Roads_T2_Self_4 == "", d1b$Roads_T2_Dis_4, d1b$Roads_T2_Self_4)
d1b$Fertility_T2_Self_4 <- ifelse(d1b$Fertility_T2_Self_4 == "", d1b$Fertility_T2_Dis_4, d1b$Fertility_T2_Self_4)
d1b$Phone_T2_Self_4 <- ifelse(d1b$Phone_T2_Self_4 == "", d1b$Phone_T2_Dis_4, d1b$Phone_T2_Self_4)
d1b$Oil_T2_Self_4 <- ifelse(d1b$Oil_T2_Self_4 == "", d1b$Oil_T2_Dis_4, d1b$Oil_T2_Self_4)
d1b$Life_T2_Self_4 <- ifelse(d1b$Life_T2_Self_4 == "", d1b$Life_T2_Dis_4, d1b$Life_T2_Self_4)
d1b$Age_T2_Self_4 <- ifelse(d1b$Age_T2_Self_4 == "", d1b$Age_T2_Dis_4, d1b$Age_T2_Self_4)
d1b$Internet_T2_Self_4 <- ifelse(d1b$Internet_T2_Self_4 == "", d1b$Internet_T2_Dis_4, d1b$Internet_T2_Self_4)
d1b$Cellphone_T2_Self_4 <- ifelse(d1b$Cellphone_T2_Self_4 == "", d1b$Cellphone_T2_Dis_4, d1b$Cellphone_T2_Self_4)
d1b$China_T2_Self_4 <- ifelse(d1b$China_T2_Self_4 == "", d1b$China_T2_Dis_4, d1b$China_T2_Self_4)

# Excluding the unecessary variables 
d1a <- d1a[,-c(25:72)]#We don't need these columns any more
d1b <- d1b[,-c(25:72)]#We don't need these columns any more

# Combine the two datasets 

dat <- rbind(d1a, d1b)
# Age and gender statistics
psych::describe(as.numeric(dat$Age))
IQR(dat$Age)
prop.table(table(dat$Gender))

# Tidying up --------------------------------------------------------------
dat$id <- 1:nrow(dat)#  Making an ID column
# Creating a between condition variable 
names(dat)[names(dat) == 'C'] <- "condition"

# Condition variable needs to be made a factor with more usable labels
levels(as.factor(dat$condition))
dat$condition <- factor(dat$condition,
                      levels = c("Friend_agree","Friend_disagree","Self"),
                      labels = c("Agree","Disagree","Self"))
levels(dat$condition)

# These lines below will make the new variable names
#q1 = airport; q2 = population; q3 = ocean; q4 = roads; q5 = fertility 
#q6 = phone; q7 = oil; q8 = life; q9 = age; q10 = internet; q11 = cellphone
#q12 = china

names(dat)
oldnames = c("Airport_T1_Self_4","Population_T1_Self_4","Ocean_T1_Self_4","Roads_T1_Self_4",
             "Fertility_T1_Self_4","Phone_T1_Self_4","Oil_T1_Self_4","Life_T1_Self_4",
             "Age_T1_Self_4","Internet_T1_Self_4","Cellphone_T1_Self_4","China_T1_Self_4",
             "Airport_T2_Self_4","Population_T2_Self_4","Ocean_T2_Self_4","Roads_T2_Self_4",
             "Fertility_T2_Self_4","Phone_T2_Self_4","Oil_T2_Self_4","Life_T2_Self_4",
             "Age_T2_Self_4","Internet_T2_Self_4","Cellphone_T2_Self_4","China_T2_Self_4")

newnames = c("guess1.q1","guess1.q2","guess1.q3","guess1.q4",
             "guess1.q5","guess1.q6","guess1.q7","guess1.q8",
             "guess1.q9","guess1.q10","guess1.q11","guess1.q12",
             "guess2.q1","guess2.q2","guess2.q3","guess2.q4",
             "guess2.q5","guess2.q6","guess2.q7","guess2.q8",
             "guess2.q9","guess2.q10","guess2.q11","guess2.q12")

for(i in 1:24) names(dat)[names(dat) == oldnames[i]] = newnames[i]

# Making the variabels into proper types 
dat[,c(1:24)] <- sapply(
  dat[,c(1:24)], as.numeric)
str(dat)#Check if it worked 

# Get data into long format
dat1 <- dat %>% dplyr::select(id,condition,wave,contains("guess1")) %>% 
  gather(guess_info,guess_1,guess1.q1:guess1.q12)

dat2 <- dat %>% dplyr::select(id,condition,wave,contains("guess2")) %>% 
  gather(guess_info,guess_2,guess2.q1:guess2.q12)

dlong <- bind_cols(dat1, dat2["guess_2"])

rm("dat1","dat2")

# Create the question variable
dlong$question <- substr(dlong$guess_info,8,10)
table(dlong$question)

# Create the answer variable and make sure it corresponds to the question 
dlong <- dlong %>%
  mutate(answer = case_when(
    question == "q1" ~ 32.3,
    question == "q2" ~ 43.2,
    question == "q3" ~ 6.3,
    question == "q4" ~ 7.3,
    question == "q5" ~ 57.7,
    question == "q6" ~ 53.3,
    question == "q7" ~ 32.5,
    question == "q8" ~ 19.6,
    question == "q9" ~ 96.0,
    question == "q10" ~ 94.8,
    question == "q11" ~ 95.0,
    question == "q12" ~ 5.1
  ))

# Create the question range variable  
dlong <- dlong %>%
  mutate(range = case_when(
    question == "q1" ~ "mid",
    question == "q2" ~ "mid",
    question == "q3" ~ "ext",
    question == "q4" ~ "ext",
    question == "q5" ~ "mid",
    question == "q6" ~ "mid",
    question == "q7" ~ "mid",
    question == "q8" ~ "mid",
    question == "q9" ~ "ext",
    question == "q10" ~ "ext",
    question == "q11" ~ "ext",
    question == "q12" ~ "ext"
  ))

glimpse(dlong)
dlong$range <- as.factor(dlong$range)
glimpse(dlong)


## Mean squared error ------------------------------------------------------
MSEfun <- function(guess, answer) {
  se <- (guess - answer)^2
  return(mean(se))
}

dlong <- dlong %>% rowwise() %>% 
  dplyr::mutate(mse.guess1 = MSEfun(guess_1, answer)) %>% 
  dplyr::mutate(mse.guess2 = MSEfun(guess_2,answer)) %>% 
  dplyr::mutate(mse.mean = MSEfun((guess_1 + guess_2)/2, answer))

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


# Analysis section --------------------------------------------------------
dlong_mse_1vm <- dlong %>% 
  select(id,condition,question,range,mse.guess1,mse.mean) %>%
  gather(mse,estimate,mse.guess1:mse.mean) %>% 
  mutate(c = ifelse(mse == "mse.guess1",-1,1))
dlong_mse_2vm <- dlong %>% 
  select(id,condition,question,range,mse.guess2,mse.mean) %>%
  gather(mse,estimate,mse.guess2:mse.mean) %>% 
  mutate(c = ifelse(mse == "mse.guess2",-1,1))
dlong_mae_1vm <- dlong %>% 
  select(id,condition,question,range,mae.guess1,mae.mean) %>%
  gather(mae,estimate,mae.guess1:mae.mean) %>% 
  mutate(c = ifelse(mae == "mae.guess1",-1,1))
dlong_mae_2vm <- dlong %>% 
  select(id,condition,question,range,mae.guess2,mae.mean) %>%
  gather(mae,estimate,mae.guess2:mae.mean) %>% 
  mutate(c = ifelse(mae == "mae.guess2",-1,1))


# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_1vm)
summary(ic1)
1.40/sqrt(1836)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = dlong_mse_2vm)
summary(ic2)
23.02/sqrt(1836)


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
1.98/sqrt(620)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self"))
summary(ic4)
8.92/sqrt(620)


# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree"))
summary(ic5)
1.90/sqrt(611)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree"))
summary(ic6)
7.26/sqrt(611)


# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree"))
summary(ic7)
1.27/sqrt(605)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree"))
tt <- getME(ic8,"theta")
ll <- getME(ic8,"lower")
derivs1 <- ic8@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic8,c("theta","fixef"))
ic8_update <- update(ic8,
                      start=ss,
                      control=
                        lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
summary(ic8_update)
21.59/sqrt(605)


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



                          # LOOKING AT QUESTIONS SEPARATELY #
# Mid-Scale questions -----------------------------------------------------
dlongmid <- dlong %>% 
  dplyr::filter(range == "mid")

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_1vm,dlong_mse_1vm$range == "mid"))
summary(ic1)
8.16/sqrt(1836)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_2vm, dlong_mse_2vm$range == "mid"))
summary(ic2)
18.93/sqrt(1836)


dlongmid %>% dplyr::summarise_at(c("mse.guess1",
                                "mse.guess2",
                                "mse.mean",
                                "mae.guess1",
                                "mae.guess2",
                                "mae.mean"),
                              list(mean))#  Get the means

dlongmid %>% dplyr::summarise_at(c("mse.guess1",
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
                          dlong_mse_1vm$condition == "Self" & dlong_mse_1vm$range=="mid"))
summary(ic3)
3.38/sqrt(620)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self" & dlong_mse_2vm$range == "mid"))
summary(ic4)
8.56/sqrt(620)

# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree" & dlong_mse_1vm$range == "mid"))
summary(ic5)
3.76/sqrt(611)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree" & dlong_mse_2vm$range == "mid"))
summary(ic6)
6.32/sqrt(611)

# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree" & dlong_mse_1vm$range == "mid"))
summary(ic7)
6.90/sqrt(605)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree" & dlong_mse_2vm$range == "mid"))

summary(ic8)
16.98/sqrt(605)

dlongmid %>% group_by(condition) %>% dplyr::summarise_at(c("mse.guess1",
                                                        "mse.guess2",
                                                        "mse.mean",
                                                        "mae.guess1",
                                                        "mae.guess2",
                                                        "mae.mean"),
                                                      list(mean))#  Get the means
dlongmid %>% group_by(condition) %>% dplyr::summarise_at(c("mse.guess1",
                                                        "mse.guess2",
                                                        "mse.mean",
                                                        "mae.guess1",
                                                        "mae.guess2",
                                                        "mae.mean"),
                                                      list(sd))#  Get the sd's

# LOOKING AT THE DIFFERENCE SCORES (Benefit of averaging)
dlongmid$diffmse <- dlongmid$mse.guess1 - dlongmid$mse.mean
dlongmid$diffmae <- dlongmid$mae.guess1 - dlongmid$mae.mean

# Create conditions datasets for comparisons
d_a_s <- dlongmid %>% 
  filter(condition == "Agree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_s <- dlongmid %>% 
  filter(condition == "Disagree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_a <- dlongmid %>% 
  filter(condition == "Disagree" | condition == "Agree") %>% 
  mutate(condition_c = ifelse(condition == "Agree",0.5,-0.5))


ic9 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic9)
0.87*sqrt((1/611)+(1/620))

ic10 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic10)
4.91*sqrt((1/605)+(1/620))

ic11 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic11)
4.23*sqrt((1/605)+(1/611))

dlongmid %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               mean)

dlongmid %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               sd)

# Extreme-scale questions -------------------------------------------------
dlongext <- dlong %>% 
  dplyr::filter(range == "ext")

# WITHIN CROWD OVERALL
ic1 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_1vm,dlong_mse_1vm$range == "ext"))
summary(ic1)
4.04/sqrt(1836)

ic2 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), data = subset(dlong_mse_2vm, dlong_mse_2vm$range == "ext"))
tt <- getME(ic2,"theta")
ll <- getME(ic2,"lower")
derivs1 <- ic2@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic2,c("theta","fixef"))
ic2_update <- update(ic2,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic2_update)
16.46/sqrt(1836)


dlongext %>% dplyr::summarise_at(c("mse.guess1",
                                   "mse.guess2",
                                   "mse.mean",
                                   "mae.guess1",
                                   "mae.guess2",
                                   "mae.mean"),
                                 list(mean))#  Get the means

dlongext %>% dplyr::summarise_at(c("mse.guess1",
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
                          dlong_mse_1vm$condition == "Self" & dlong_mse_1vm$range=="ext"))
summary(ic3)
0.13/sqrt(620)

ic4 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Self" & dlong_mse_2vm$range == "ext"))
summary(ic4)
5.29/sqrt(620)


# WITHIN CROWD AGREE
ic5 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Agree" & dlong_mse_1vm$range == "ext"))
summary(ic5)
0.37/sqrt(611)

ic6 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Agree" & dlong_mse_2vm$range == "ext"))
summary(ic6)
4.79/sqrt(611)


# WITHIN CROWD DISAGREE
ic7 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_1vm,
                          dlong_mse_1vm$condition == "Disagree" & dlong_mse_1vm$range == "ext"))
summary(ic7)
6.31/sqrt(605)

ic8 <- lmer(estimate ~ c +
              (1|id) +
              (1|question), 
            data = subset(dlong_mse_2vm, 
                          dlong_mse_2vm$condition == "Disagree" & dlong_mse_2vm$range == "ext"))

summary(ic8)
16.31/sqrt(605)

dlongext %>% group_by(condition) %>% dplyr::summarise_at(c("mse.guess1",
                                                           "mse.guess2",
                                                           "mse.mean",
                                                           "mae.guess1",
                                                           "mae.guess2",
                                                           "mae.mean"),
                                                         list(mean))#  Get the means
dlongext %>% group_by(condition) %>% dplyr::summarise_at(c("mse.guess1",
                                                           "mse.guess2",
                                                           "mse.mean",
                                                           "mae.guess1",
                                                           "mae.guess2",
                                                           "mae.mean"),
                                                         list(sd))#  Get the sd's

# LOOKING AT THE DIFFERENCE SCORES (Benefit of averaging)
dlongext$diffmse <- dlongext$mse.guess1 - dlongext$mse.mean
dlongext$diffmae <- dlongext$mae.guess1 - dlongext$mae.mean
# Create conditions datasets for comparisons
d_a_s <- dlongext %>% 
  filter(condition == "Agree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_s <- dlongext %>% 
  filter(condition == "Disagree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_a <- dlongext %>% 
  filter(condition == "Disagree" | condition == "Agree") %>% 
  mutate(condition_c = ifelse(condition == "Agree",0.5,-0.5))


ic9 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
summary(ic9)
0.85*sqrt((1/611)+(1/620))

ic10 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic10)
7.22*sqrt((1/605)+(1/620))

ic11 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic11)
6.91*sqrt((1/605)+(1/611))

dlongext %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               mean)

dlongext %>% group_by(condition) %>% 
  summarise_at(c("diffmse","diffmae"),
               sd)
