
#####COMBINING ALL EXPERIMENTS TOGETHER######

# Packages ----------------------------------------------------------------
for(pack in c("tidyverse","psych","car","lme4","lmerTest")) {
  if(!(pack %in% installed.packages()[,1])) {
    install.packages(pack)  
  }
  library(pack, character.only = T)
}

# Experiment 1a -----------------------------------------------------------
source("Analysis/SvD Analysis_1a.R", echo  = TRUE)
ex1a <- dlong %>% 
  select(id,condition,guess_1,guess_2,question,answer,mse.guess1:mae.mean) %>% 
  mutate(experiment = "ex1a")

# Experiment 1b -----------------------------------------------------------
source("Analysis/SvD Analysis_1b.R", echo  = TRUE)
ex1b <- dlong %>% 
  select(id,condition,guess_1,guess_2,question,answer,mse.guess1:mae.mean) %>% 
  mutate(experiment = "ex1b")

# Experiment 2 ------------------------------------------------------------
source("Analysis/SvDvA Analysis_2.R", echo  = TRUE)
ex2 <- dlong %>% 
  select(id,condition,guess_1,guess_2,question,answer,mse.guess1:mae.mean) %>% 
  mutate(experiment = "ex2")

# Experiment 3 ------------------------------------------------------------
source("Analysis/SvDvA Analysis_3.R", echo  = TRUE)
ex3 <- dlong %>% 
  select(id,condition,guess_1,guess_2,question,answer,mse.guess1:mae.mean) %>% 
  mutate(experiment = "ex3")

# Experiment 4 ------------------------------------------------------------
source("Analysis/SvDvA Analysis_4.R", echo  = TRUE)
ex4 <- dlong %>% 
  select(id,condition,guess_1,guess_2,question,answer,mse.guess1:mae.mean) %>% 
  mutate(experiment = "ex4")

# Creating an id variable
# Ex1a = 880 
# Ex1b = 894
# Ex2 = 1389
# Ex3 = 1426
# Ex4 = 1836
ex1b$id <- ex1b$id + 880
ex2$id <- ex2$id + (880+894)
ex3$id <- ex3$id + (880+894+1389)
ex4$id <- ex4$id + (880+894+1389+1426)

# Combining the datasets
adat <- do.call("rbind", list(ex1a, ex1b, ex2, ex3, ex4))
adat1 <- adat
adat$Ex <- factor(adat$experiment,
                  levels = c("ex1a","ex1b","ex2","ex3","ex4"),
                  labels = c("Experiment 1a", "Experiment 1b", "Experiment 2",
                             "Experiemnt 3","Experiment 4"))

adat$question <- gsub("[q]","",adat$question)#this deletes "q" from the question variables

glimpse(adat)

write.csv(adat, "alldata.csv")


