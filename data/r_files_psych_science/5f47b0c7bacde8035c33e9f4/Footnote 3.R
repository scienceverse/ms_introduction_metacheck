# Footnote 3
# The code below allows you to replicate the calculations reported in footnote 3 in
# the main manuscript. For this, you will need to use the alldata.csv data file. 

library(tidyverse)
library(lme4)
library(lmerTest)

all <- read.csv("alldata.csv", header = T)
glimpse(alldata)

all$Ex <- factor(all$Ex,
                     levels = c("Experiment 1a","Experiment 1b","Experiment 2", 
                                "Experiemnt 3", "Experiment 4"),
                     labels = c("Experiment 1a","Experiment 1b","Experiment 2", 
                                "Experiment 3", "Experiment 4"))

# DIFFERENCE SCORES (Benefit of averaging)
all$diffmse <- all$mse.guess1 - all$mse.mean
all$diffmae <- all$mae.guess1 - all$mae.mean

# Filtering those cases that I need 
# Ex 2,3, and 4
# From Ex 4 
# q1 is 1; q2 is 2, 34 we don't need, 5 is 3, 6 is 4, 7 is 5, 8 is 6, 9101112 we don't need 

allfil <- all %>% 
  filter(Ex %in% c("Experiment 2","Experiment 3"))

ex4 <- all %>% 
  filter(Ex == "Experiment 4") %>% 
  filter(question %in% c("1","2","5","6","7","8")) %>% 
  mutate(question = case_when(question == 5 ~ 3,
                              question == 6 ~ 4,
                              question == 7 ~ 5,
                              question == 8 ~ 6,
                              question == 1 ~ 1,
                              question == 2 ~ 2))
#add ex4 to allfil
allfil <- rbind(allfil,ex4)
table(allfil$question)
table(allfil$Ex)

# ANALYSIS 
# Create conditions datasets for comparisons
d_a_s <- allfil %>% 
  filter(condition == "Agree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_s <- allfil %>% 
  filter(condition == "Disagree" | condition == "Self") %>% 
  mutate(condition_c = ifelse(condition == "Self",0.5,-0.5))

d_d_a <- allfil %>% 
  filter(condition == "Disagree" | condition == "Agree") %>% 
  mutate(condition_c = ifelse(condition == "Agree",0.5,-0.5))

# Ex 4 self 620, agree 611, disagree 605
# Ex 2 462, 463, 464
# Ex 3 468, 477, 481

ic1 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
            data = d_a_s)
tt <- getME(ic1,"theta")
ll <- getME(ic1,"lower")
derivs1 <- ic1@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
ss <- getME(ic1,c("theta","fixef"))
ic1_update <- update(ic1,
                     start=ss,
                     control=
                       lmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5)))
summary(ic1_update)
1.39*sqrt((1/(611+463+477))+(1/(620+462+468)))


ic2 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_s)
summary(ic2)
5.83*sqrt((1/(605+464+481))+(1/(620+462+468)))

ic3 <- lmer(diffmse ~ condition_c + (1|id) + (1|question), 
             data = d_d_a)
summary(ic3)
4.90*sqrt((1/(605+464+481))+(1/(611+463+477)))






