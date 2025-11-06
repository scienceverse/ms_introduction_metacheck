

library(tidyverse)
library(modelr)
library(ggplot2)
library(dplyr)
library(forcats)
library(lme4)
library(devtools)
library(MASS)
library(nnet)
library(survival)
library(survminer)
library(car)
library(brms)


#Bayesian Analyses

Scientist1_3<-read_csv("Study1_3.csv")
Scientist1_3$Condition <- factor(Scientist1_3$Condition, levels = c("Be Scientist", "Do Science"))
Scientist1_3$Gender <- factor(Scientist1_3$Gender, levels = c("Female", "Male"))
Scientist1_3$TP1<-Scientist1_3$TaskPersistence + 1
n<-mean(Scientist1_3$ExactAge)
Scientist1_3$ExactAgeM<-(Scientist1_3$ExactAge - n)


Study4<- read_csv("Study4.csv")


Study4$Condition <- factor(Study4$Condition, levels = c("Be", "Do"))
Study4$Gender <- factor(Study4$Gender, levels = c("Female", "Male"))
Study4$ExactAge<-as.numeric(Study4$ExactAge)
summary(Study4$ExactAge)
Study4$ExactAgeM4<-Study4$ExactAge-4.829
Study4$ExactAgeM4
Study4$TP1<-Study4$TaskPersistence + 1

modelbayes<-brm(TP1 ~ Condition*Gender*ExactAgeM, 
                data = Scientist1_3, 
                family = "cumulative",
                save_all_pars = TRUE,
                chains = 4,
                iter=5000,
                warmup=2500,
                control=list(adapt_delta = .99),
                set_prior("normal(0,1)", class = "b"))

nullmodelbayes<-brm(TP1 ~ Condition*ExactAgeM + Gender*ExactAgeM,
                    data = Scientist1_3, 
                    family = "cumulative",
                    save_all_pars = TRUE,
                    chains = 4,
                    iter=5000,
                    warmup=2500,
                    control=list(adapt_delta = .99),
                    set_prior("normal(0,1)", class = "b"))

modelbayes
nullmodelbayes
test = bayes_factor(modelbayes, nullmodelbayes)
test



modelbayes4<-brm(TP1 ~ Condition*Gender*ExactAgeM4, 
                 data = Study4, 
                 family = "cumulative",
                 save_all_pars = TRUE,
                 chains = 4,
                 iter=5000,
                 warmup=2500,
                 control=list(adapt_delta = .99),
                 set_prior("normal(0,1)", class = "b"))

nullmodelbayes4<-brm(TP1 ~ Condition*ExactAgeM4 + Gender*ExactAgeM4, 
                     data = Study4, 
                     family = "cumulative",
                     save_all_pars = TRUE,
                     chains = 4,
                     iter=5000,
                     warmup=2500,
                     control=list(adapt_delta = .99),
                     set_prior("normal(0,1)", class = "b"))
modelbayes4
nullmodelbayes4

test4 = bayes_factor(modelbayes4, nullmodelbayes4)
test4






