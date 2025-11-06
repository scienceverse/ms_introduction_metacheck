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


# load data materials test ------------------------------------------------

data <- read_csv("Data/workingdata_materialstest")

#mean center data
data4 <- data%>%
  mutate(harm = harm - mean(harm, na.rm = TRUE),
         fairness = fairness - mean(fairness, na.rm = TRUE),
         purity = purity - mean(purity, na.rm = TRUE),
         loyalty = loyalty - mean(loyalty, na.rm = TRUE),
         authority = authority - mean(authority, na.rm = TRUE),
         liberty = liberty - mean(liberty, na.rm = TRUE),
         moral = moral - mean(moral, na.rm = TRUE),
         practical = practical - mean(practical, na.rm = TRUE),
         emotional = emotional - mean(emotional, na.rm = TRUE),
         rational = rational - mean(rational, na.rm = TRUE))

#change to long form

data4 <- data4 %>%
  gather("foundation", "response_foundation", harm, fairness, purity, loyalty, authority, liberty)%>%
  gather("concern", "response_concern", moral, practical) %>%
  gather("emotion", "response_emotion", emotional, rational)%>%
  mutate(
    condition = replace(condition, condition == "1", "Harm"))%>%
  mutate(condition = replace(condition, condition == "2", "Liberty"))%>%
  mutate(condition = replace(condition, condition == "3", "Non-moral"))%>%
  mutate( condition = replace(condition, condition == "4", "Control"))



#change responses to factor

data4$condition <- as.factor(data4$condition)
data4$foundation <- as.factor(data4$foundation)
data4$condition <- relevel(data4$condition, ref = 1)
data4$foundation <- relevel(data4$foundation, ref = "harm")
data4 <- na.omit(data4)

# analyses ----------------------------------------------------------------

# test materials ----------------------------------------------------------

#foundations

output1 <- lmer(response_foundation ~ condition*foundation + (1|ID), data4)
summary(output1)

#concerns

output2 <- lmer(response_concern ~ condition*concern + (1|ID), data4)
summary(output2)
#emotion

output3 <- lmer(response_emotion ~ condition*emotion + (1|ID), data4)
summary(output3)

# moral reframing hypothesis (study 1 and 2) ----------------------------------------------

#study 1
#mean center data
data1 <- data1 %>%
  mutate(Harm = Harm - mean(Harm, na.rm = TRUE),
         Fairness = Fairness - mean(Fairness, na.rm = TRUE),
         Loyalty = Loyalty - mean(Loyalty, na.rm = TRUE),
         Purity = Purity - mean(Purity, na.rm = TRUE),
         Authority = Authority - mean(Authority, na.rm = TRUE),
         Liberty = Liberty - mean(Liberty, na.rm = TRUE),
         age = age - mean(age, na.rm = TRUE),
         PI_1 = PI_1 - mean(PI_1, na.rm = TRUE),
         PI_2 = PI_2 - mean(PI_2, na.rm = TRUE),
         PI_3 = PI_3 - mean(PI_3, na.rm = TRUE),
         PI_4 = PI_4 - mean(PI_4, na.rm = TRUE))

#analysis with interaction between condition and moral foundations
output4 <- lm(support_t2 ~ support_t1 + condition + 
                (Harm + Liberty + Fairness + Authority + Loyalty + Purity)*condition, data1) 
summary(output4)

#study 2
#mean center data
data2 <- data2 %>%
  mutate(Harm = Harm - mean(Harm, na.rm = TRUE),
         Fairness = Fairness - mean(Fairness, na.rm = TRUE),
         Loyalty = Loyalty - mean(Loyalty, na.rm = TRUE),
         Purity = Purity - mean(Purity, na.rm = TRUE),
         Authority = Authority - mean(Authority, na.rm = TRUE),
         Liberty = Liberty - mean(Liberty, na.rm = TRUE),
         age = age - mean(age, na.rm = TRUE),
         PI_1 = PI_1 - mean(PI_1, na.rm = TRUE),
         PI_2 = PI_2 - mean(PI_2, na.rm = TRUE),
         PI_3 = PI_3 - mean(PI_3, na.rm = TRUE),
         PI_4 = PI_4 - mean(PI_4, na.rm = TRUE),
         support_t1 = support_t1 - mean(support_t1, na.rm = TRUE))

#analysis with interaction between condition and foundations
output5 <- lm(support_t2 ~ support_t1 + condition + 
                (Harm + Liberty + Fairness + Authority + Loyalty + Purity)*condition, data2) 
summary(output5)

# effect of condition on attitude strength dimensions ---------------------

#label dvs 
dvs <- c("certainty", "importance", "centrality", "extremity")

#create empty lists
outputs <- list() #for output
ci <- list() # for confidence intervals
outs <- list() #for standardized reg coeff
dummys <- list() #for mean terms in graph

for(p in dvs){
  
  print(p)
  #labels
  name <- paste("dvs", p, sep = "_")
  #run analysis
  model <- tidy(lm(get(p) ~ condition, data = data3)) %>% mutate(model = name) %>% slice(2:3)
  #calculate confidence intervals
  cis <- as.data.frame(confint(lm(get(p) ~ condition, data = data3))) %>% slice(2:3)
  #calculate std reg coeff
  out <- lm.beta(lm(get(p) ~ condition, data = data3))
  #calculate mean terms
  dummy <- ggpredict(lm(get(p) ~ condition, data = data3), 
                     terms = "condition", show.summary = TRUE)%>% mutate(dv = name)
  
  outputs[[name]] <- model
  ci[[name]] <- cis
  dummys[[name]] <- dummy
  outs[[name]] <- out
  
}

#store outputs in dataframes
output17 <- bind_rows(outputs)
ci17 <- bind_rows(ci)
dummy17 <- bind_rows(dummys)
output17 <- bind_cols(output17, ci17)

#for cohens d standardize
#labels
dvs_std <- c("certainty_std", "importance_std", "centrality_std", "extremity_std")

#standardize variables
data3$certainty_std <- data3$certainty/sd(data3$certainty, na.rm = TRUE)
data3$importance_std <- data3$importance/sd(data3$importance, na.rm = TRUE)
data3$centrality_std <- data3$centrality/sd(data3$centrality, na.rm = TRUE)
data3$extremity_std <- data3$extremity/sd(data3$extremity, na.rm = TRUE)

#create empty lists
outputs <- list() #for output
ci <- list() # for CIS
outs <- list() #for std reg coeff
dummys <- list() # for mean terms in graph

for(p in dvs_std){
  
  print(p)
  #labels
  name <- paste("dvs_std", p, sep = "_")
  #run analysis
  model <- tidy(lm(get(p) ~ condition, data = data3)) %>% mutate(model = name) %>% slice(2:3)
#calculate CIs
  cis <- as.data.frame(confint(lm(get(p) ~ condition, data = data3))) %>% slice(2:3)
  #std reg coeff
  out <- lm.beta(lm(get(p) ~ condition, data = data3))
  #mean  terms
  dummy <- ggpredict(lm(get(p) ~ condition, data = data3), 
                     terms = "condition", show.summary = TRUE)%>% mutate(dv = name)
  
  outputs[[name]] <- model
  ci[[name]] <- cis
  dummys[[name]] <- dummy
  outs[[name]] <- out
  
}

output17_std <- bind_rows(outputs)
ci17_std <- bind_rows(ci)

dummy17_std <- bind_rows(dummys)

output17_std <- bind_cols(output17, ci17) # output coeffs are d's

# association of willingness to compromise and moral conviction -----------

#support for uncompromising candidate

output10a <- lm(dv_candidate_uncomp ~ MC, data=data3)
summary(output10a)

#support for compromising candidate

output10b <- lm(dv_candidate_comp ~ MC, data = data3)
summary(output10b)

#working with uncompromising manager

output12a <- lm(dv_manager_uncomp ~ MC, data = data3)
summary(output12a)

#working with compromising manager

output12b <- lm(dv_manager_comp ~ MC, data = data3)
summary(output12b)

#compromise game

output14 <- lm(compromise ~ MC, data = data3)
summary(output14)

# association of mechanism factors with moral conviction -------------------

#study 2

output6 <- lm(MC_t2 ~ MC_t1 + risks + benefits + fear + anger + disgust + creepedout + gratefulness, data2)
summary(output6)

cis <- as.data.frame(confint(output6))

output6 <- tidy(output6)

output6 <- output6 %>%
  bind_cols(cis)%>%
  slice(3:9)%>%
  dplyr::rename(min = `2.5 %`, max = `97.5 %`)

output6 <- output6 %>%
  arrange(term) %>%
  mutate(term = factor(term, levels=c("gratefulness", "creepedout", "fear", "disgust", "anger", "benefits", "risks")))


#study 3

output16 <- lm(MC ~ risks + benefits + fear + anger + disgust + creeped + grateful + tradeoff +
                 finance, data3)
summary(output16)

cis <- as.data.frame(confint(output16))

output16 <- tidy(output16)

output16 <- output16 %>%
  bind_cols(cis)%>%
  slice(2:10)%>%
  dplyr::rename(min = `2.5 %`, max = `97.5 %`)

output16 <- output16 %>%
  arrange(term) %>%
  mutate(term = factor(term, levels=c("finance", "tradeoff", "grateful", "creeped", "fear", "disgust", "anger",  "benefits", "risks")))%>%
  mutate(facet = c(1, 1, 1, 1, 1, 2, 1, 1, 2))


# Graphs ------------------------------------------------------------------

# test materials ----------------------------------------------------------

#change to long form
data5 <- data %>%
  gather("foundation", "response_foundation", harm, fairness, purity, loyalty, authority, liberty)%>%
  gather("concern", "response_concern", moral, practical) %>%
  gather("emotion", "response_emotion", emotional, rational)%>%
  mutate(
    condition = replace(condition, condition == "1", "Harm"))%>%
  mutate(condition = replace(condition, condition == "2", "Liberty"))%>%
  mutate(condition = replace(condition, condition == "3", "Non-moral"))%>%
  mutate( condition = replace(condition, condition == "4", "Control"))

#change responses to factor

data5$condition <- as.factor(data5$condition)
data5$foundation <- as.factor(data5$foundation)
data5$condition <- relevel(data5$condition, ref = 1)
data5$foundation <- relevel(data5$foundation, ref = "harm")
data5 <- na.omit(data5)

#prep data for foundations graph
data5 <- data5 %>%
  group_by(condition, foundation)
data_summary <- data5 %>%
  dplyr::summarize(mean_PL = mean(response_foundation),  # calculates the mean of each group
                   sd_PL = sd(response_foundation), # calculates the standard deviation of each group
                   n_PL = n(),  # calculates the sample size per group
                   ci_PL = 1.96*sd(response_foundation)/sqrt(n())) # calculates the standard error of each group

ggplot(data_summary, aes(condition, mean_PL, fill = foundation)) + 
  geom_col(position = "dodge", width = 0.75) +  
  geom_errorbar(aes(ymin = mean_PL - ci_PL, ymax = mean_PL + ci_PL), width=0.2, position = position_dodge(0.75))+
  theme_classic(base_size = 26) +
  # ylim(1,7) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Effect of condition on relevance to foundation",
       x = "Condition",
       y = "Relevance to foundation",
       fill = "foundations") 

#graph for concerns

ungroup(data5)
data5 <- data5 %>%
  group_by(condition, concern)
data_summary <- data5 %>%
  dplyr::summarize(mean_PL = mean(response_concern),  # calculates the mean of each group
                   sd_PL = sd(response_concern), # calculates the standard deviation of each group
                   n_PL = n(),  # calculates the sample size per group
                   ci_PL = 1.96*sd(response_concern)/sqrt(n())) # calculates the standard error of each group

ggplot(data_summary, aes(condition, mean_PL, fill = concern)) + 
  geom_col(position = "dodge", width = 0.75) +  
  geom_errorbar(aes(ymin = mean_PL - ci_PL, ymax = mean_PL + ci_PL), width=0.2, position = position_dodge(0.75))+
  theme_classic(base_size = 26) +
  # ylim(1,7) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Effect of condition on moral and practical concerns",
       x = "Condition",
       y = "Level of Concern",
       fill = "Concern") 

#graph for emotinality/rationality

ungroup(data5)
data5 <- data5 %>%
  group_by(condition, emotion)
data_summary <- data5 %>%
  dplyr::summarize(mean_PL = mean(response_emotion),  # calculates the mean of each group
                   sd_PL = sd(response_emotion), # calculates the standard deviation of each group
                   n_PL = n(),  # calculates the sample size per group
                   ci_PL = 1.96*sd(response_emotion)/sqrt(n())) # calculates the standard error of each group

ggplot(data_summary, aes(condition, mean_PL, fill = emotion)) + 
  geom_col(position = "dodge", width = 0.75) +  
  geom_errorbar(aes(ymin = mean_PL - ci_PL, ymax = mean_PL + ci_PL), width=0.2, position = position_dodge(0.75))+
  theme_classic(base_size = 26) +
  # ylim(1,7) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Effect of condition on emotionality/rationality",
       x = "Condition",
       y = "Level of emotionality/rationality",
       fill = "Emotion")

# moral reframing hypothesis ----------------------------------------------

#study 1
plotdata <- ggpredict(output4, terms = c("Harm", "condition"))

s4a <- ggplot(plotdata, aes(x, predicted, colour = group)) + 
  geom_line(size = 1.1) + 
  ylim(1,7) +
  labs(title = "Study 1",
       x = "Relevance to Harm foundation",
       y = "Support at Time 2",
       color = "Condition") +
  theme_classic(base_size = 15) +
  xlim(-4,2)+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "none")
s4a

plotdata <- ggpredict(output4, terms = c("Liberty", "condition"))

s4b <- ggplot(plotdata, aes(x, predicted, colour = group)) + 
  geom_line(size  = 1.1) + 
  ylim(1,7) +
  labs(title = "",
       x = "Relevance to Liberty foundation",
       color = "Condition") +
  theme_classic(base_size = 15) +
  scale_color_brewer(palette = "Dark2")+
  theme(axis.title.y = element_blank())
s4b

#study 2

plotdata <- ggpredict(output5, terms = c("Harm", "condition"))

s5a <- ggplot(plotdata, aes(x, predicted, colour = group)) + 
  geom_line(size = 1.1) + 
  ylim(1,7) +
  labs(title = "Study 2",
       x = "Relevance to Harm foundation",
       y = "Support at Time 2",
       color = "Condition") +
  xlim(-4,2)+
  theme_classic(base_size = 15) +
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "none")
s5a

plotdata <- ggpredict(output5, terms = c("Fairness", "condition"))

s5b <- ggplot(plotdata, aes(x, predicted, colour = group)) + 
  geom_line(size  = 1.1) + 
  ylim(1,7) +
  labs(title = " ",
       x = "Relevance to Fairness foundation",
       y = "Support at Time 2",
       color = "Condition") +
  theme_classic(base_size = 15) +
  scale_color_brewer(palette = "Dark2")+
  theme(axis.title.y = element_blank())
s5b

#combine graphs

s4a + s4b +s5a + s5b + 
  plot_layout(ncol = 2)


# effect of condition on attitude strength dimensions ---------------------

plotdummy <- dummy17 %>%
  dplyr::mutate(
    dv = replace(dv, dv == 'dvs_certainty', 'certainty'),
    dv = replace(dv, dv == 'dvs_importance', 'importance'),
    dv = replace(dv, dv == 'dvs_centrality', 'centrality'),
    dv = replace(dv, dv == 'dvs_extremity', 'extremity'))%>%
  dplyr::rename(condition = x)

#certainty

p17_certainty <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=certainty,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=certainty, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "certainty"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "certainty"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Effect of condition on attitude strength dimensions\nCertainty")+
  ylab("Certainty")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p17_certainty

#importance

p17_importance <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=importance,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=importance, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "importance"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "importance"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("\nImportance")+
  ylab("Importance")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p17_importance

#centrality

p17_centrality <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=centrality,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=centrality, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "centrality"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "centrality"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Centrality")+
  ylab("Centrality")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))
p17_centrality

#extremity

p17_extremity <- ggplot() +
  geom_flat_violin(data = data3, aes(x=condition,y=extremity,fill = condition,colour = condition),
                   position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(data = data3,aes(x=condition,y=extremity, fill = condition,colour = condition),
             position = position_jitter(width = .30), size = .25)+
  geom_point(data = filter(plotdummy, dv == "extremity"), aes(x = condition, y = predicted), color = "BLACK", position = position_nudge(.25))+
  geom_errorbar(data = filter(plotdummy, dv == "extremity"), aes(x = condition, y = predicted, ymin = conf.low, ymax = conf.high),
                position = position_nudge(.25), color = "BLACK", width = 0.1, size = 0.8)+
  theme_cowplot(font_size = 14)+
  guides(fill = FALSE, colour = FALSE) +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Extremity")+
  ylab("Extremity")+
  scale_x_discrete(labels=c("Control message" = "Control\nmessage",
                            "Harm/Fairness-based moral message" = "Harm/Fairness-based\nmoral message",
                            "Non-moral message" = "Non-moral\nmessage")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

p17_extremity

#combine

(p17_certainty + p17_importance)/(p17_extremity + p17_centrality)

# association of willingness to compromise and moral conviction -----------

cand1 <- tidy(output10a) %>% mutate(var = "uncompromising candidate") %>% slice(2:2)
cand2 <- tidy(output10b) %>% mutate(var = "compromising candidate") %>% slice(2:2)

man1 <- tidy(output12a) %>% mutate(var = "uncompromising manager") %>% slice(2:2)
man2 <- tidy(output12b) %>% mutate(var = "compromising manager") %>% slice(2:2)

game <- tidy(output14) %>% mutate(var = "compromise game") %>% slice(2:2)

compromise <- bind_rows(cand1, cand2, man1, man2, game)

ci_cand1 <- as.data.frame(confint(output10a))%>% 
  mutate(v = "uncompromising candidate") %>% slice(2:2)
ci_cand2 <- as.data.frame(confint(output10b))%>%
  mutate(v = "compromising candidate") %>% slice(2:2)
ci_man1 <- as.data.frame(confint(output12a))%>%
  mutate(v = "uncompromising manager") %>% slice(2:2)
ci_man2 <- as.data.frame(confint(output12b)) %>%
  mutate(v = "compromising manager") %>% slice(2:2)
ci_game <- as.data.frame(confint(output14)) %>%
  mutate(v = "compromise game") %>% slice(2:2)
cis <- bind_rows(ci_cand1, ci_cand2, ci_man1, ci_man2, ci_game)

compromise <- compromise %>%
  bind_cols(cis)%>%
  dplyr::rename(min = `2.5 %`, max = `97.5 %`)

compromise <- compromise %>%
  arrange(var) %>%
  mutate(var = factor(var, levels=c("compromise game", "compromising manager",
                                    "uncompromising manager", "compromising candidate",
                                    "uncompromising candidate")))%>%
  mutate(facet = c("Compromise Game", "Willingness to Work with Manager",
                   "Willingness to Work with Manager", "Support for Candidate", "Support for Candidate"))%>%
  mutate(facet = factor(facet, levels = c("Support for Candidate",
                                          "Willingness to Work with Manager", "Compromise Game")))

compromisemc <- ggplot(compromise, aes(x=var, y = estimate)) + 
  geom_pointrange(size = 1.2, aes(ymin = min, ymax = max)) + 
  facet_wrap(~facet, ncol = 1, scales  = "free_y") +
  labs(title = "Association between moral conviction and compromise",
       y = "Estimate") +
  theme_cowplot(font_size = 14) +
  scale_color_brewer(palette = "Dark2") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 5.5)+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_blank())
compromisemc

# association of mechanism factors with moral conviction -------------------

#study 2

p6_factors <- ggplot(output6, aes(x=term, y = estimate)) + 
  geom_pointrange(size = 1.2, aes(ymin = min, ymax = max)) + 
  labs(title = "Study 2",
       x = "Factors",
       y = "Estimate") +
  ylim(-0.2, 0.35)+
  theme_cowplot(font_size = 14) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(labels=c("risks" = "Risks",
                            "benefits" = "Benefits",
                            "anger" = "Anger",
                            "disgust" = "Disgust",
                            "fear" = "Fear",
                            "creepedout" = "Feeling creeped out",
                            "gratefulness" = "Gratefulness"))+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "longdash")+
  geom_vline(xintercept = 5.5)+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))

p6_factors


#study 3

p16_factors_1 <- ggplot(filter(output16, facet == 1), aes(x=term, y = estimate)) + 
  geom_pointrange(size = 1.2, aes(ymin = min, ymax = max)) + 
  labs(title = "Study 3",
       y = "Estimate") +
  ylim(-0.2, 0.35)+
  theme_cowplot(font_size = 14) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(labels=c("risks" = "Risks",
                            "benefits" = "Benefits",
                            "anger" = "Anger",
                            "disgust" = "Disgust",
                            "fear" = "Fear",
                            "creeped" = "Feeling creeped out",
                            "grateful" = "Gratefulness",
                            "tradeoff" ="Weighing\ncosts/benefits",
                            "finance" = "Perceived\nfinancial cost"))+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 5.5)+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.y = element_blank(),
        aspect.ratio = 1.5)
p16_factors_1


p16_factors_2 <- ggplot(filter(output16, facet == 2), aes(x=term, y = estimate)) + 
  geom_pointrange(size = 1.2, aes(ymin = min, ymax = max)) + 
  labs(title = "Study 3",
       y = "Estimate",
       x = "Factors") +
  ylim(-0.2,0.35)+
  theme_cowplot(font_size = 14) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(labels=c(
    "tradeoff" ="Weighing\ncosts/benefits",
    "finance" = "Perceived\nfinancial cost"))+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "longdash") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        aspect.ratio = 1)
p16_factors_2

p6_factors+p16_factors_1 + p16_factors_2 + plot_layout(ncol = 2)+
  plot_annotation(
    title = "Association between moral conviction perceived risks/benefits,\nemotions, and making tradeoffs",
    theme = theme(plot.title = element_text(size = 18, face = "bold")))
