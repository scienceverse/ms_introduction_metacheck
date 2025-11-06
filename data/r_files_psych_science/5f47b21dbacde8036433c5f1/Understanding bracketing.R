# UNDERSTANDING BRACKETING  

# Load the data from all experiments together 
alldata <- read.csv("alldata.csv", header = T)

# Packages
library(tidyverse)

glimpse(alldata)
names(alldata)[names(alldata) == "condition"] <- "Perspective"

# Bracketing overall ------------------------------------------------------

# Create a function to find bracketing 
b.find <- function(t,g1,g2) {
  ifelse(g1<t & g2>t | g1>t & g2<t,1,0)
}

alldata <- alldata %>% 
  select(X,id,Perspective,guess_1,guess_2,question,mse.guess1,mse.guess2,
         mse.mean,answer,Ex) %>% 
  dplyr::mutate(bracket = b.find(answer, guess_1, guess_2))

# Second est. moving towards true -----------------------------------------
# When first overestimates ----
alldata.over <- alldata %>% 
  filter(guess_1 > answer)

b.towards.1 <- function(t,g1,g2)
{
  ifelse(g2<g1,1,0)
}
alldata.over <- alldata.over %>% 
  mutate(towards = b.towards.1(answer,guess_1,guess_2))

# Select those that do go towards
alldata.over.towards <- alldata.over %>% 
  filter(towards == 1)

good.towards.1 <- function(t,g1,g2)
{
  ifelse(g2 > g1-4*abs(g1-t),1,0)
}

alldata.over.towards <- alldata.over.towards %>% 
  mutate(towards.good = good.towards.1(answer,guess_1,guess_2))

# When first underestimates ----
alldata.under <- alldata %>% 
  filter(guess_1 < answer)

b.towards.2 <- function(t,g1,g2)
{
  ifelse(g2>g1,1,0)
}
alldata.under <- alldata.under %>% 
  mutate(towards1 = b.towards.2(answer,guess_1,guess_2))

# Select only those that go towards
alldata.under.towards <- alldata.under %>% 
  filter(towards1 == 1)

good.towards.2 <- function(t,g1,g2)
{
  ifelse(g2 < g1+4*abs(g1-t),1,0)
}

alldata.under.towards <- alldata.under.towards %>% 
  mutate(towards.good1 = good.towards.2(answer,guess_1,guess_2))

# JOINING THIS DATA WHERE SECOND EST IN RIGHT DIRECTION

alldata <- left_join(alldata,alldata.over[,c("X","towards")], by = "X") 
alldata <- left_join(alldata,alldata.under[,c("X","towards1")], by = "X") 
alldata <- left_join(alldata,alldata.over.towards[,c("X","towards.good")], by = "X") 
alldata <- left_join(alldata,alldata.under.towards[,c("X","towards.good1")], by = "X") 

alldata$towards <- ifelse(is.na(alldata$towards),alldata$towards1,alldata$towards)
alldata$towards.good <- ifelse(is.na(alldata$towards.good),alldata$towards.good1,alldata$towards.good)
alldata <- alldata %>% 
  select(-towards1, -towards.good1)

rm("alldata.over","alldata.over.towards","alldata.under","alldata.under.towards")

# When second estimate is going in the wrong direction --------------------
b.away <- function(t,g1,g2)
{
  ifelse(g2>g1 & g1>t | g2<g1 & g1<t,1,0)
}

alldata <- alldata %>% 
  mutate(away = b.away(answer, guess_1, guess_2))

# Bad bracketing  ---------------------------------------------------------
alldata.bracket.over <- alldata %>% 
  filter(bracket == 1) %>% 
  filter(guess_1 > answer)

bad.brack.over <- function(t,g1,g2)
{
  ifelse(g2 < g1 - 4*abs(g1-t),1,0)
}

alldata.bracket.over <- alldata.bracket.over %>% 
  mutate(bad.bracket = bad.brack.over(answer,guess_1,guess_2))

alldata.bracket.under <- alldata %>% 
  filter(bracket == 1) %>% 
  filter(guess_1 < answer)

bad.brack.under <- function(t,g1,g2)
{
  ifelse(g2 > g1 + 4*abs(g1-t),1,0)
}

alldata.bracket.under <- alldata.bracket.under %>% 
  mutate(bad.bracket1 = bad.brack.under(answer, guess_1,guess_2))

#JOINING THE BAD BRACKET COLUMN

alldata <- left_join(alldata,alldata.bracket.over[,c("X","bad.bracket")], by = "X") 
alldata <- left_join(alldata,alldata.bracket.under[,c("X","bad.bracket1")], by = "X") 

alldata$bad.bracket <- ifelse(is.na(alldata$bad.bracket),alldata$bad.bracket1,alldata$bad.bracket)
rm("alldata.bracket.over","alldata.bracket.under")

alldata <- alldata %>% 
  select(-bad.bracket1)

# Second identical to first estimate --------------------------------------

alldata <- alldata %>% 
  mutate(f.s.identical = ifelse(guess_1 == guess_2,1,0))

# Got it on the first guess -----------------------------------------------

alldata <- alldata %>% 
  mutate(exact = ifelse(guess_1 == answer & guess_1 != guess_2,1,0))

# Descriptives
# Percentage of basic bracketing across conditions 
alldata %>% 
  group_by(Perspective,bracket) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# Percentages of going towards the true answer across conditions
alldata %>% 
  group_by(Perspective,towards) %>%
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# Percentages of towards good
alldata %>% 
  group_by(Perspective,towards.good) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# Percentages where the second estimate moves away (i.e., in the wrong direction)
alldata %>% 
  group_by(Perspective,away) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# Bad bracketing 
alldata %>% 
  group_by(Perspective,bad.bracket) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# First and second estimate are identical 
alldata %>% 
  group_by(Perspective,f.s.identical) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# First and true answer are identical 
alldata %>% 
  group_by(Perspective,exact) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# TAKING A LOOK AT END SCALE QUESTIONS IN EX4
# Selecting experiment 4 and the end-scale questions 
escale <- alldata %>% 
  filter(Ex == "Experiment 4") %>% 
  filter(question == 3 | question == 4| question == 9| question == 10
         | question == 11| question == 12)
# N1 column 
options(pillar.sigfig = 10)
escale %>% 
  filter(towards.good == 1) %>% 
  group_by(Perspective) %>% 
  summarise(sum(mse.guess1-mse.mean))

# N2 column
escale %>% 
  filter(away == 1) %>% 
  group_by(Perspective) %>% 
  summarise(sum(mse.guess1-mse.mean))

# N3 column
escale %>% 
  filter(bad.bracket == 1) %>% 
  group_by(Perspective) %>% 
  summarise(sum(mse.guess1-mse.mean))

# N4 column
escale %>% 
  filter(exact == 1) %>% 
  group_by(Perspective) %>% 
  summarise(sum(mse.guess1-mse.mean))

# Verify if it adds up 
escale %>%
  group_by(Perspective) %>%
  summarise(mean(mse.guess1-mse.mean))

(521313.95 - 928113.5 - 64406.35 - 8696.25)/(605*6)#disagree
(414840.65 - 393072.45 - 11220.10 - 1330.00)/(620*6)#self
(333995.70-348384.15-9958.70-239.25)/(611*6)#agree

# Bad bracketing percentage in Ex4 end scale questions-----
escale %>% 
  group_by(Perspective,bad.bracket) %>% 
  summarise(n = n()) %>% 
  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# Across all estimates
escale %>% 
  group_by(Perspective,bad.bracket) %>% 
  summarise(n = n()) %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

