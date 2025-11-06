# load packages -----------------------------------------------------------

library(tidyverse)
library(broom.mixed)
library(sjPlot)
library(sjmisc)
library(ggeffects)
library(RColorBrewer)
library(ppcor)
library(tidystats)
library(cowplot)
library(patchwork)
library(plyr)
library(lm.beta)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


# load data for all studies -----------------------------------------------

#Study 1
#load data
data1 <- read_csv("Data/workingdata_study1.csv")

data1 <- data1 %>%
  filter(!is.na(support_t1))
data1 <- data1 %>%
  filter(!is.na(support_t2))

#change to factor
data1$condition <- as.factor(data1$condition)
#relabel conditions
data1$condition <- mapvalues(data1$condition, from = c("Control", "Harm", "Liberty", "Non-moral"), 
                             to = c("Control message", "Harm-based moral message", "Liberty-based moral message",
                                    "Non-moral message"))
#change ref group to control
data1$condition <- relevel(data1$condition, ref = "Control message") 

#Study 2
#load data
data2 <- read_csv("Data/workingdata_study2.csv")

data2 <- data2 %>%
  filter(!is.na(support_t1))
data2 <- data2 %>%
  filter(!is.na(support_t2))

#change to factor
data2$condition <- as.factor(data2$condition)
#relabel conditions
data2$condition <- mapvalues(data2$condition, from = c("Control", "Harm/Fairness", "Non-moral"), 
                             to = c("Control message", "Harm/Fairness-based moral message",
                                    "Non-moral message"))
#change ref group to control
data2$condition <- relevel(data2$condition, ref = "Control message")

#Study 3
#load data
data3 <- read_csv("Data/workingdata_study3.csv")

data3 <- data3 %>%
  filter(!is.na(support))

#change to factor
data3$condition <- as.factor(data3$condition)
#relabel conditions
data3$condition <- mapvalues(data3$condition, from = c("Control", "Moral", "Non-moral"), 
                             to = c("Control message", "Harm/Fairness-based moral message",
                                    "Non-moral message"))
#change ref group to control
data3$condition <- relevel(data3$condition, ref = "Control message")


data_1 <- data1 %>% 
  dplyr::select(condition, MC_t1, MC_t2, support_t1, support_t2)%>%
  group_by(condition) %>%
  summarise_all("sd")

data_2 <- data2 %>% 
  dplyr::select(condition, MC_t1, MC_t2, support_t1, support_t2)%>%
  group_by(condition) %>%
  summarise_all("sd")

# Analyses ----------------------------------------------------------------


# effect of condition on support ------------------------------------------
#study 1
output1 <- lm(support_t2 ~ support_t1 + condition, data1)
summary(output1)

# calculate terms controlling for support_t1
dummy1 <- ggpredict(output1, terms = "condition", show.summary = TRUE)

#standardize variables
data1$support_t2_std <- data1$support_t2/sd(data1$support_t2)
data1$support_t1_std <- data1$support_t1/sd(data1$support_t1)

#analysis with standardized variables
output1_std <- lm(support_t2_std ~ support_t1_std + condition, data1)#output coefficient is d
summary(output1_std)


#Study 2
#effect of condition on support at Time 2
output3 <- lm(support_t2 ~ support_t1 + condition, data2)
summary(output3)

# calculate terms controlling for support_t1
dummy3 <- ggpredict(output3, terms = "condition", show.summary = TRUE)

#standardize variables
data2$support_t2_std <- data2$support_t2/sd(data2$support_t2)
data2$support_t1_std <- data2$support_t1/sd(data2$support_t1)

#analysis with standardized variables
output3_std <- lm(support_t2_std ~ support_t1_std + condition, data2) #output coefficient is d
summary(output3_std)


#Study 3
#effect of condition on support (only one time point)
output7 <- lm(support ~ condition, data = data3)
summary(output7)
dummy7 <- ggpredict(output7, term = "condition", show.summary = TRUE)

#standardize variables
data3$support_std <- data3$support/sd(data3$support)

#analysis with standardized variables
output7_std <- lm(support_std ~ condition, data3) #output coefficient is d
summary(output7_std)


# effect of condition on moral conviction ---------------------------------

#Study 1
#effect of condition on moral conviction at Time 2
output2 <- lm(MC_t2 ~ MC_t1 + condition, data1)
summary(output2)

#calculate term after controlling for MC_t1
dummy2 <- ggpredict(output2, terms = "condition", show.summary = TRUE)

#standardize variables
data1$MC_t2_std <- data1$MC_t2/sd(data1$MC_t2)
data1$MC_t1_std <- data1$MC_t1/sd(data1$MC_t1)

output2_std <- lm(MC_t2_std ~ MC_t1_std + condition, data1) #output coefficient is d
summary(output2_std)


#Study 2
#effect of condition on moral conviction at time 2
output4 <- lm(MC_t2 ~ MC_t1 + condition, data2)
summary(output4)

#calculate terms after controlling for MC_t1
dummy4 <- ggpredict(output4, terms = "condition", show.summary = TRUE)

#standardize variables
data2$MC_t2_std <- data2$MC_t2/sd(data2$MC_t2)
data2$MC_t1_std <- data2$MC_t1/sd(data2$MC_t1)

output4_std <- lm(MC_t2_std ~ MC_t1_std + condition, data2) #output coefficient is d
summary(output4_std)


#Study 3
#effect of condition on moral conviction (one time point)
output8 <- lm(MC ~ condition, data = data3)
summary(output8)
dummy8 <- ggpredict(output8, term = "condition", show.summary = TRUE)

#standardize variable
data3$MC_std <- data3$MC/sd(data3$MC)

#analysis with standardized variables
output8_std <- lm(MC_std ~ condition, data = data3) #output coefficient is d
summary(output8_std)


# effect of condition on willingness to compromise ------------------------
#(only study 3)

# support for candidate ---------------------------------------------------

# condition and uncompromising candidate ----------------------------------

#change ref to moral condition
data3$condition <- relevel(data3$condition, ref = "Harm/Fairness-based moral message")

#support for uncompromiising candidate
output9a <- lm(dv_candidate_uncomp ~ condition, data = data3)
summary(output9a)
dummy9a <- ggpredict(output9a, term = "condition", show.summary = TRUE)

#standardize variables
data3$dv_candidate_uncomp_std <- data3$dv_candidate_uncomp/sd(data3$dv_candidate_uncomp, na.rm = TRUE)

#analysis with standardized variables
output9a_std <- lm(dv_candidate_uncomp_std ~ condition, data = data3)#coefficient is d
summary(output9a_std)


# condition and compromisng candidate -------------------------------------

#change ref group to non-moral
data3$condition <- relevel(data3$condition, ref = "Non-moral message")

#support for compromising candidate
output9b <- lm(dv_candidate_comp ~ condition, data = data3)
summary(output9b)
dummy9b <- ggpredict(output9b, term = "condition", show.summary = TRUE)

#standardize
data3$dv_candidate_comp_std <- data3$dv_candidate_comp/sd(data3$dv_candidate_comp, na.rm = TRUE)

#analysis with standardized variables
output9b_std <- lm(dv_candidate_comp_std ~ condition, data = data3) #output coeff is d
summary(output9b_std)


# willingness to work with manager ----------------------------------------

# condition and uncompromising manager ------------------------------------

#change ref group to moral condition
data3$condition <- relevel(data3$condition, ref = "Harm/Fairness-based moral message")

#willingness to work with uncompromising manager
output11a <- lm(dv_manager_uncomp ~ condition, data = data3)
summary(output11a)
dummy11a <- ggpredict(output11a, term = "condition", show.summary = TRUE)

#standardize
data3$dv_manager_uncomp_std <- data3$dv_manager_uncomp/sd(data3$dv_manager_uncomp, na.rm = TRUE)

#analysis with standardized variables
output11a_std <- lm(dv_manager_uncomp_std ~ condition, data = data3) #output coeff is d
summary(output11a_std)

# condition and compromising manager --------------------------------------

#change ref group to non-moral
data3$condition <- relevel(data3$condition, ref = "Non-moral message")

#willingness to work with compromising manager
output11b <- lm(dv_manager_comp ~ condition, data = data3)
summary(output11b)
dummy11b <- ggpredict(output11b, term = "condition", show.summary = TRUE)

#standardize
data3$dv_manager_comp_std <- data3$dv_manager_comp/sd(data3$dv_manager_comp, na.rm = TRUE)

#analysis with standardized variables
output11b_std <- lm(dv_manager_comp_std ~ condition, data = data3)#output coeff is d
summary(output11b_std)


# compromise game ---------------------------------------------------------

#change ref to control condition
data3$condition <- relevel(data3$condition, ref = "Control message")

#willingness to compromise in compromise game
#compromise DV is the payoff for opponent /higher payoff, higher compromise
output13 <- lm(compromise ~ condition, data = data3)
summary(output13)
dummy13 <- ggpredict(output13, term = "condition", show.summary = TRUE)

#standardize
data3$compromise_std <- data3$compromise/sd(data3$compromise, na.rm = TRUE)

#analysis with standardized variables
output13_std <- lm(compromise_std ~ condition, data = data3) #output coeff is d
summary(output13_std)


# potential mechanisms of moralization and demoralization (study 2 and 3) -----------------

#study 2

#risks, benefits, emotions as DVs.
#label dvs 
dvs <- c("risks", "benefits", "fear", "anger", "disgust", "creepedout", "gratefulness")

#create empty lists
outputs <- list() #for output
ci <- list() #for confidence intervals
dummys <- list() #for means 
outs <- list() #for standardized coeff output (beta)

for(p in dvs){
  
  print(p)
  #label dvs
  name <- paste("dvs", p, sep = "_")
  #run analysis
  model <- tidy(lm(get(p) ~ condition, data = data2)) %>% mutate(model = name) %>% slice(2:3)
  #calculate cis
  cis <- as.data.frame(confint(lm(get(p) ~ condition, data = data2))) %>% slice(2:3)
  #calculate standardized reg coefficients
  out <- lm.beta(lm(get(p) ~ condition, data = data2))
  #get means used in the graph
  dummy <- ggpredict(lm(get(p) ~ condition, data = data2), 
                     terms = "condition", show.summary = TRUE)%>% mutate(dv = name)
  
  outputs[[name]] <- model
  ci[[name]] <- cis
  dummys[[name]] <- dummy
  outs[[name]] <- out
}

#create dataframes with outputs
output5 <- bind_rows(outputs)
ci5 <- bind_rows(ci)
dummy5 <- bind_rows(dummys)
output5 <- bind_cols(output5, ci5)

#analyses to obtain cohens d
#standardize all variables
data2$risks_std <- data2$risks/sd(data2$risks, na.rm = TRUE)
data2$benefits_std <- data2$benefits/sd(data2$benefits, na.rm = TRUE)
data2$fear_std <- data2$fear/sd(data2$fear, na.rm = TRUE)
data2$anger_std <- data2$anger/sd(data2$anger, na.rm = TRUE)
data2$disgust_std <- data2$disgust/sd(data2$disgust, na.rm = TRUE)
data2$creepedout_std <- data2$creepedout/sd(data2$creepedout, na.rm = TRUE)
data2$gratefulness_std <- data2$gratefulness/sd(data2$gratefulness, na.rm = TRUE)

#label dvs
dvs_std <- c("risks_std", "benefits_std", "fear_std", "anger_std", 
             "disgust_std", "creepedout_std", "gratefulness_std")

#create empty lists
outputs <- list() #for output
ci <- list() #for confidence intervals
dummys <- list() #for mean terms
outs <- list() #for standardized reg coeff

for(p in dvs_std){
  
  print(p)
  #labels
  name <- paste("dvs_std", p, sep = "_")
  #run analysis
  model <- tidy(lm(get(p) ~ condition, data = data2)) %>% mutate(model = name) %>% slice(2:3)
  #calculate cis
  cis <- as.data.frame(confint(lm(get(p) ~ condition, data = data2))) %>% slice(2:3)
  #calculate std reg coefficients
  out <- lm.beta(lm(get(p) ~ condition, data = data2))
  #means used in graph
  dummy <- ggpredict(lm(get(p) ~ condition, data = data2), 
                     terms = "condition", show.summary = TRUE)%>% mutate(dv = name)
  
  outputs[[name]] <- model
  ci[[name]] <- cis
  dummys[[name]] <- dummy
  outs[[name]] <- out
}

#store outputs in dataframes
output5_std <- bind_rows(outputs)
ci5_std <- bind_rows(ci)
dummy5_std <- bind_rows(dummys)
out5_std <- bind_rows(outs)
output5_std <- bind_cols(output5_std, ci5_std)

#study 3
#refgroup is control condition 
data3$condition <- relevel(data3$condition, ref = "Control message")

#label dvs 
dvs <- c("risks", "benefits", "fear", "anger", "disgust", "creeped", "grateful", "tradeoff", "finance")

#create empty lists
outputs <- list() #for output
ci <- list() #for confidence intervals
dummys <- list() # mean terms for graph
outs <- list() #for std reg coefficients

for(p in dvs){
  
  print(p)
  #labels
  name <- paste("dvs", p, sep = "_")
  #run analysis
  model <- tidy(lm(get(p) ~ condition, data = data3)) %>% mutate(model = name) %>% slice(2:3)
  #calculate confidence intervals
  cis <- as.data.frame(confint(lm(get(p) ~ condition, data = data3))) %>% slice(2:3)
  #calculate std reg coefficients
  out <- lm.beta(lm(get(p) ~ condition, data = data3))
  #mean terms for graph
  dummy <- ggpredict(lm(get(p) ~ condition, data = data3), 
                     terms = "condition", show.summary = TRUE)%>% mutate(dv = name)
  
  outputs[[name]] <- model
  ci[[name]] <- cis
  dummys[[name]]<- dummy
  outs[[name]] <- out
  
}

#store output in dataframes
output15 <- bind_rows(outputs)
ci15 <- bind_rows(ci)
dummy15 <- bind_rows(dummys)
output15 <- bind_cols(output15, ci15)

#standardized variables to be used to calculate cohens d

data3$risks_std <- data3$risks/sd(data3$risks, na.rm = TRUE)
data3$benefits_std <- data3$benefits/sd(data3$benefits, na.rm = TRUE)
data3$fear_std <- data3$fear/sd(data3$fear, na.rm = TRUE)
data3$anger_std <- data3$anger/sd(data3$anger, na.rm = TRUE)
data3$disgust_std <- data3$disgust/sd(data3$disgust, na.rm = TRUE)
data3$creeped_std <- data3$creeped/sd(data3$creeped, na.rm = TRUE)
data3$grateful_std <- data3$grateful/sd(data3$grateful, na.rm = TRUE)
data3$tradeoff_std <- data3$tradeoff/sd(data3$tradeoff, na.rm = TRUE)
data3$finance_std <- data3$finance/sd(data3$finance, na.rm = TRUE)

#labels
dvs_std <- c("risks_std", "benefits_std", "fear_std", "anger_std", "disgust_std",
             "creeped_std", "grateful_std", "tradeoff_std", "finance_std")

#create empty lists
outputs <- list() #for output
ci <- list() # for confidence intervals
dummys <- list() # mean terms for graphs
outs <- list() # standardized reg coeff

for(p in dvs_std){
  
  print(p)
  #labels
  name <- paste("dvs_std", p, sep = "_")
  #run analysis
  model <- tidy(lm(get(p) ~ condition, data = data3)) %>% mutate(model = name) %>% slice(2:3)
  #calculate ci
  cis <- as.data.frame(confint(lm(get(p) ~ condition, data = data3))) %>% slice(2:3)
  #calculate std reg coeff
  out <- lm.beta(lm(get(p) ~ condition, data = data3))
  #mean terms for graph
  dummy <- ggpredict(lm(get(p) ~ condition, data = data3), 
                     terms = "condition", show.summary = TRUE)%>% mutate(dv = name)
  
  outputs[[name]] <- model
  ci[[name]] <- cis
  dummys[[name]]<- dummy
  outs[[name]] <- out
  
}

#store outputs in dataframes
output15_std <- bind_rows(outputs)
ci15_std <- bind_rows(ci)
dummy15_std <- bind_rows(dummys)
output15_std <- bind_cols(output15_std, ci15_std)


# Graphs ------------------------------------------------------------------

palette1 <- c('#1b9e77', '#d95f02', '#e7298a', '#7570b3')

# effect of condition on support ------------------------------------------
#study 1
#Raincloud plot with mean and confidence interval
p1 <- ggplot()+
  geom_flat_violin(data = data1, aes(x=condition,y=support_t2,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data1,aes(x=condition,y=support_t2, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy1, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy1, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+ xlab("Condition")+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_manual(values = palette1)+
  scale_fill_manual(values = palette1)+
  ggtitle("Study1 - Crime Surveillance Technology")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm-based moral message" = "Harm-based\nmoral\nmessage",
                            "Liberty-based moral message" = "Liberty-based\nmoral\nmessage",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"))+
  ylab("Support")
p1

#study 2
p3 <- ggplot()+
  geom_flat_violin(data = data2, aes(x=condition,y=support_t2,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=support_t2, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy3, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy3, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 2 - Hiring Algorithm") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
p3

#study 3

p7 <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=support,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=support, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy7, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy7, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 3 - Hiring Algorithm") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
p7

#combine graphs using patchwork

p1+p3+p7 + plot_annotation(
  title = 'Effect of Condition on Attitude Support',
  theme = theme(plot.title = element_text(size = 18, face = "bold")))

# effect of condition on moral conviction ---------------------------------
#study 1
p2 <- ggplot()+
  geom_flat_violin(data = data1, aes(x=condition,y=MC_t2,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data1,aes(x=condition,y=MC_t2, fill = condition,colour = condition),
             position = position_jitter(width = .15), size = .25)+
  geom_point(data = dummy2, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy2, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_manual(values = palette1)+
  scale_fill_manual(values = palette1)+
  ggtitle("Study 1 - Crime Surveillance Technology")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm-based moral message" = "Harm-based\nmoral\nmessage",
                            "Liberty-based moral message" = "Liberty-based\nmoral\nmessage",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15, face = "bold"))+
  ylab("Moral Conviction")

p2

#study 2

p4 <- ggplot()+
  geom_flat_violin(data = data2, aes(x=condition,y=MC_t2,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=MC_t2, fill = condition,colour = condition),
             position = position_jitter(width = .15), size = .25)+
  geom_point(data = dummy4, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy4, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 2 - Hiring Algorithm")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
p4

#study 3

p8 <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=MC,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=MC, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy8, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy8, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 3 - Hiring Algorithm") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", 
                                   size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
p8

#combine graphs

p2+p4+p8 + plot_annotation(
  title = 'Effect of condition on Moral Conviction',
  theme = theme(plot.title = element_text(size = 18, face = "bold")))

# effect of condition on willingness to compromise ------------------------

#condition and uncompromising candidate

data3$condition <- relevel(data3$condition, ref = "Control message")
p9a <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=dv_candidate_uncomp,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=dv_candidate_uncomp, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy9a, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy9a, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Effect of condition on support for candidate\nUncompromising candidate")+
  ylab("Support for candidate")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold", size=15))
p9a

#condition and compromising candidate

data3$condition <- relevel(data3$condition, ref = "Harm/Fairness-based moral message")
data3$condition <- relevel(data3$condition, ref = "Control message")
p9b <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=dv_candidate_comp,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=dv_candidate_comp, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy9b, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy9b, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("\nCompromising candidate")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

p9b

#condition and willigness to work with uncompromising manager

data3$condition <- relevel(data3$condition, ref = "Control message")
p11a <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=dv_manager_uncomp,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=dv_manager_uncomp, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy11a, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy11a, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Effect of condition on willingness to work\nUncompromising manager")+
  ylab("Willingness to work")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold", size=15))
p11a

#condition and willingness to work with compromising manager

data3$condition <- relevel(data3$condition, ref = "Harm/Fairness-based moral message")
data3$condition <- relevel(data3$condition, ref = "Control message")
p11b <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=dv_manager_comp,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=dv_manager_comp, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy11b, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy11b, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("\nCompromising manager")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

p11b

#condition and compromise in compromise game

p13 <- ggplot()+
  geom_flat_violin(data = data3, aes(x=condition,y=compromise,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=compromise, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = dummy13, aes(x = x, y = predicted), position = position_nudge(.25), colour = "BLACK")+
  geom_errorbar(data = dummy13, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+
  ylab("Willingness to compromise")+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Effect of condition on willingness to compromise \nin Compromise Game")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.text.x = element_text(face="bold", size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold", size=15))
p13

#combine graphs
p9a+p9b+p11a + p11b +p13 + plot_layout(ncol = 2)


# potential mechanisms ----------------------------------------------------
#study 2

plotdummy <- dummy5 %>%
  dplyr::mutate(
    dv = replace(dv, dv == 'dvs_risks', 'risks'),
    dv = replace(dv, dv == 'dvs_benefits', 'benefits'),
    dv = replace(dv, dv == 'dvs_fear', 'fear'),
    dv = replace(dv, dv == 'dvs_anger', 'anger'),
    dv = replace(dv, dv == 'dvs_disgust', 'disgust'),
    dv = replace(dv, dv == 'dvs_creepedout', 'creeped out'),
    dv = replace(dv, dv == 'dvs_gratefulness', 'gratefulness'))%>%
  dplyr::rename(condition = x)%>%
  mutate(facet = ifelse(dv == "risks", "Perceived risks and benefits",
                        ifelse(dv == "benefits", "Perceived risks and benefits",
                               ifelse(dv == "fear", "Emotions",
                                      ifelse(dv == "anger", "Emotions",
                                             ifelse(dv == "disgust", "Emotions",
                                                    ifelse(dv == "creeped out", "Emotions",
                                                           ifelse(dv == "gratefulness", "Emotions", "no"))))))))



#study 3

plotdummy <- dummy15 %>%
  dplyr::mutate(
    dv = replace(dv, dv == 'dvs_risks', 'risks'),
    dv = replace(dv, dv == 'dvs_benefits', 'benefits'),
    dv = replace(dv, dv == 'dvs_fear', 'fear'),
    dv = replace(dv, dv == 'dvs_anger', 'anger'),
    dv = replace(dv, dv == 'dvs_disgust', 'disgust'),
    dv = replace(dv, dv == 'dvs_creeped', 'creeped out'),
    dv = replace(dv, dv == 'dvs_grateful', 'gratefulness'),
    dv = replace(dv, dv == 'dvs_tradeoff', 'tradeoff'),
    dv = replace(dv, dv == 'dvs_finance', 'finance'))%>%
  dplyr::rename(condition = x)%>%
  mutate(facet = ifelse(dv == "risks", "Perceived risks and benefits",
                        ifelse(dv == "benefits", "Perceived risks and benefits",
                               ifelse(dv == "fear", "Emotions",
                                      ifelse(dv == "anger", "Emotions",
                                             ifelse(dv == "disgust", "Emotions",
                                                    ifelse(dv == "creeped out", "Emotions",
                                                           ifelse(dv == "gratefulness", "Emotions",
                                                                  ifelse(dv == "tradeoff", "weigh costs",
                                                                         ifelse(dv == "finance", "weigh costs", "no"))))))))))



#study 2


#risks/benefits


p5_risks <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=risks,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=risks, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "risks"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "risks"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  ylab("Perceived risks")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 2") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p5_risks

p5_ben <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=benefits,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=benefits, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "benefits"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "benefits"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+ ylab("Perceived benefits")+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p5_ben

#study 3

p15_risks <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=risks,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=risks, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "risks"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "risks"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 3")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15)) +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) 

p15_risks

p15_ben <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=benefits,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=benefits, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "benefits"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "benefits"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15)) +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) 

p15_ben

#combine graphs for risks/benefits

p5_risks + p15_risks + p5_ben + p15_ben +plot_layout(ncol = 2) +
  plot_annotation(title = "Effect of condition on perceived risks/benefits",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))


#emotions
#study 2

p5_anger <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=anger,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=anger, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "anger"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "anger"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 2\nAnger")+
  ylab("Anger") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 15))

p5_anger

p5_disgust <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=disgust,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=disgust, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "disgust"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "disgust"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Disgust")+
  ylab("Disgust") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 15))
p5_disgust

p5_fear <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=fear,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=fear, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "fear"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "fear"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Fear")+
  ylab("Fear") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 15))
p5_fear

p5_creeped <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=creepedout,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=creepedout, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "creeped out"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "creeped out"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Creeped out")+
  ylab("Feeling creeped out") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 15))

p5_creeped

p5_grateful <- ggplot() +
  geom_flat_violin(data = data2, aes(x=condition,y=gratefulness,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data2,aes(x=condition,y=gratefulness, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "gratefulness"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "gratefulness"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Gratefulness") +
  ylab("Gratefulness") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 15))
p5_grateful

#study 3

p15_anger <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=anger,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=anger, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "anger"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "anger"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  xlab("Condition")+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Study 3")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15))

p15_anger

p15_disgust <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=disgust,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=disgust, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "disgust"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "disgust"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15))

p15_disgust

p15_fear <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=fear,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=fear, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "fear"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "fear"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15))

p15_fear

p15_creeped <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=creeped,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=creeped, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "creeped out"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "creeped out"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  xlab("Condition")+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15))

p15_creeped

p15_grateful <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=grateful,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=grateful, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "gratefulness"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "gratefulness"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15))

p15_grateful

#combine graphs for emotions

p5_anger + p15_anger + 
  p5_disgust + p15_disgust +
  p5_fear + p15_fear + 
  p5_creeped +  p15_creeped +
  p5_grateful + p15_grateful +
  plot_layout(ncol = 2)+
  plot_annotation(title = "Effect of condition on emotions",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))

#weighing costs benefits (only study 3)

p15_trade <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=tradeoff,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=tradeoff, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "tradeoff"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "tradeoff"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+ ylab("Weigh costs and benefits")+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Effect of condition on making tradeoffs\nWeighing costs and benefits")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p15_trade

p15_fin <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=finance,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=finance, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "finance"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "finance"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+ ylab("Perceived financial cost")+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("\nPerceived financial cost") +
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p15_fin

p15_trade + p15_fin
