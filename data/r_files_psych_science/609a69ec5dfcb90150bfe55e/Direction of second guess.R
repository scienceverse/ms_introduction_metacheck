# DIRECTION OF THE SECOND GUESS

# Don't forget to set the working directory 

# Load the data from all experiments together 
alldata <- read.csv("alldata.csv", header = T)

# Packages ----
library(tidyverse)

# Check data ----
glimpse(alldata)
names(alldata)[names(alldata) == "condition"] <- "Perspective"
table(alldata$Ex)

# Select and separate end-scale questions ----
escale <- alldata %>% 
  filter(Ex == "Experiment 4") %>% 
  filter(question == 3 | question == 4 | question == 9 | question == 10
         | question == 11 | question == 12)
escale <- escale %>% 
  filter(guess_1 != 50)


#end.left <- alldata %>% 
#  filter(Ex == "Experiment 4") %>% 
#  filter(question == 3|question == 4|question == 12)

#end.right <- alldata %>% 
#  filter(Ex == "Experiment 4") %>% 
#  filter(question == 9|question == 10
#         |question == 11)

end.left <- escale %>% 
  filter(guess_1 < 50)
end.right <- escale %>% 
  filter(guess_1 > 50)

# Function for second guess moving away from end-scale 
# Left side 
away.left <- function(g1,g2)
{
  ifelse(g2>g1,1,0)
}
end.left <- end.left %>% 
  mutate(move.away = away.left(guess_1,guess_2))

# right side 
away.right <- function(g1,g2)
{
  ifelse(g2<g1,1,0)
}
end.right <- end.right %>% 
  mutate(move.away1 = away.right(guess_1,guess_2))

# Join these values 
escale <- left_join(escale,end.left[,c("X","move.away")], by = "X") 
escale <- left_join(escale,end.right[,c("X","move.away1")], by = "X") 

escale$move.away <- ifelse(is.na(escale$move.away),escale$move.away1,escale$move.away)
escale$move.away <- as.factor(escale$move.away)

# Calculate direction for mid-scale questions ----
mscale <- alldata %>% 
  filter(Ex == "Experiment 4") %>% 
  filter(question == 1 | question == 2| question == 5| question == 6
         | question == 7| question == 8) 
mscale <- mscale %>% 
  filter(guess_1 != "50")

mid.left <- mscale %>% 
  filter(guess_1 < 50)
mid.right <- mscale %>% 
  filter(guess_1 > 50)

# Function for second guess moving away from end-scale 
# Left side 
mid.left <- mid.left %>% 
  mutate(move.away = away.left(guess_1,guess_2))

# right side 
mid.right <- mid.right %>% 
  mutate(move.away1 = away.right(guess_1,guess_2))

# Join these values 
mscale <- left_join(mscale,mid.left[,c("X","move.away")], by = "X") 
mscale <- left_join(mscale,mid.right[,c("X","move.away1")], by = "X") 

mscale$move.away <- ifelse(is.na(mscale$move.away),mscale$move.away1,mscale$move.away)
mscale$move.away <- as.factor(mscale$move.away)

# Percentages 
escale %>% 
  group_by(Perspective,move.away) %>% 
  summarise(n = n()) %>% 
#  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

mscale %>% 
  group_by(Perspective,move.away) %>% 
  summarise(n = n()) %>% 
#  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

# Direction of the second guess across all experiments ----
glimpse(alldata)

# A function to categorize left or right from first guess 
left.or.right <- function(g1,g2)
{
  ifelse(g2>g1,"right","left")
}

alldata %>% 
  filter(guess_1 != guess_2) %>% 
  mutate(lr = left.or.right(guess_1,guess_2)) %>% 
  group_by(Perspective,lr) %>% 
  summarise(n = n()) %>% 
  #  drop_na() %>% 
  mutate(rel.freq = paste0(round(100*n/sum(n),2), "%"))

