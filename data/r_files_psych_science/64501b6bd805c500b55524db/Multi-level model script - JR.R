library(lme4)
library(dplyr)
library(tidyr)
library(stringr)
library(sjPlot)
library(jtools)
library(rockchalk)
library(ggpubr)
library(wesanderson)
install.packages("easystats")
library(easystats)

# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Set working directory
setwd("/Users/steverathje/Desktop/AccuracyNudgeReAnalysis")

# Read in Data
data_pol <- read.csv("AdCollabData3.csv")
xtabs(~data_pol$study)

# Select by political news sharers, where relevant (not in every study):
data_pol <- data[which(data$ever_share_political == 1 | data$ever_share_political %in% NA),]

xtabs(~data$ever_share_political)

data_pol$trueShare <- ifelse(data_pol$headline_true == 1, data_pol$rating, NA)
data_pol$falseShare <- ifelse(data_pol$headline_true == 0, data_pol$rating, NA)

data_pol$evaluationRecode <- ifelse(data_pol$condition == 1, 1, NA)
data_pol$evaluationRecode <- ifelse(data_pol$condition == 0, 0, data_pol$evaluationRecode)
data_pol$evaluationRecode <- ifelse(data_pol$condition == 2, 1, data_pol$evaluationRecode)

# Recode dataset
data_polRecode <- data_pol %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

data_polRecode$discernment <- data_polRecode$trueShare - data_polRecode$falseShare

#### Subselect datasets

# By individual study:
# Just study A
MetaA <- data_pol[which(data_pol$study == "A"),]
MetaARecode <- MetaA %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaARecode$discernment <- MetaARecode$trueShare - MetaARecode$falseShare

t.test(discernment ~ condition, data = MetaARecode)

# Just study B
MetaB <- data_pol[which(data_pol$study == "B"),]
MetaBRecode <- MetaB %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaBRecode$discernment <- MetaBRecode$trueShare - MetaBRecode$falseShare

# Just study C
MetaC <- data_pol[which(data_pol$study == "C"),]
MetaCRecode <- MetaC %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaCRecode$discernment <- MetaCRecode$trueShare - MetaCRecode$falseShare

# Just study D
MetaD <- data_pol[which(data_pol$study == "D"),]
MetaDRecode <- MetaD %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaDRecode$discernment <- MetaDRecode$trueShare - MetaDRecode$falseShare

# Just study E
MetaE <- data_pol[which(data_pol$study == "E"),]
MetaERecode <- MetaE %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaERecode$discernment <- MetaERecode$trueShare - MetaERecode$falseShare

# Just study F
MetaF <- data_pol[which(data_pol$study == "F"),]
MetaFRecode <- MetaF %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaFRecode$discernment <- MetaFRecode$trueShare - MetaFRecode$falseShare

# Just study G
MetaG <- data_pol[which(data_pol$study == "G"),]
MetaGRecode <- MetaG %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaGRecode$discernment <- MetaGRecode$trueShare - MetaGRecode$falseShare

# Just study H
MetaH <- data_pol[which(data_pol$study == "H"),]
MetaHRecode <- MetaH %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaHRecode$discernment <- MetaHRecode$trueShare - MetaHRecode$falseShare

# Just study I
MetaI <- data_pol[which(data_pol$study == "I"),]
MetaIRecode <- MetaI %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaIRecode$discernment <- MetaIRecode$trueShare - MetaIRecode$falseShare

# Just study J
MetaJ <- data_pol[which(data_pol$study == "J"),]
MetaJRecode <- MetaJ %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaJRecode$discernment <- MetaJRecode$trueShare - MetaJRecode$falseShare

# Just study K
MetaK <- data_pol[which(data_pol$study == "K"),]
MetaKRecode <- MetaK %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaKRecode$discernment <- MetaKRecode$trueShare - MetaKRecode$falseShare

# Just study L
MetaL <- data_pol[which(data_pol$study == "L"),]
MetaLRecode <- MetaL %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaLRecode$discernment <- MetaLRecode$trueShare - MetaLRecode$falseShare

# Just study M
MetaM <- data_pol[which(data_pol$study == "M"),]
MetaMRecode <- MetaM %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaMRecode$discernment <- MetaMRecode$trueShare - MetaMRecode$falseShare

# Just study N
MetaN <- data_pol[which(data_pol$study == "N"),]
MetaNRecode <- MetaN %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaNRecode$discernment <- MetaNRecode$trueShare - MetaNRecode$falseShare

# Just study O
MetaO <- data_pol[which(data_pol$study == "O"),]
MetaORecode <- MetaO %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaORecode$discernment <- MetaORecode$trueShare - MetaORecode$falseShare


# Just study P
MetaP <- data_pol[which(data_pol$study == "P"),]
MetaPRecode <- MetaP %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaPRecode$discernment <- MetaPRecode$trueShare - MetaPRecode$falseShare

# Just study Q
MetaQ <- data_pol[which(data_pol$study == "Q"),]
MetaQRecode <- MetaQ %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaQRecode$discernment <- MetaQRecode$trueShare - MetaQRecode$falseShare

# Just study R
View(MetaR)
MetaR <- data_pol[which(data_pol$study == "R"),]
MetaR$evaluationRecode
MetaRRecode <- MetaR %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )
MetaRRecode$discernment <- MetaRRecode$trueShare - MetaRRecode$falseShare
lm <- lm(discernment ~ as.factor(condition), data = MetaRRecode)

# Just study S
MetaS <- data_pol[which(data_pol$study == "S"),]
MetaSRecode <- MetaS %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaSRecode$discernment <- MetaSRecode$trueShare - MetaSRecode$falseShare

# Just study T
MetaT <- data_pol[which(data_pol$study == "T"),]
MetaTRecode <- MetaT %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaTRecode$discernment <- MetaTRecode$trueShare - MetaTRecode$falseShare

# Just Roozenbeek et al., 2021:
MetaRooz <- data_pol[which(data_pol$study == "X"),]

MetaRoozRecode <- MetaRooz %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

MetaRoozRecode$discernment <- MetaRoozRecode$trueShare - MetaRoozRecode$falseShare

# Just evaluation treatment (1x):
data_pol_eval <- data_pol[which(data$study == "A" | data$study == "B" | 
                                  data$study == "C" | data$study == "G" |
                                  data$study == "H" | data$study == "I" |
                                  data$study == "J" | data$study == "K" |
                                  data$study == "L" | data$study == "M" |
                                  data$study == "P" | data$study == "Q" |
                                  data$study == "R" | data$study == "S" |
                                  data$study == "X"),]

data_pol_evalRecode <- data_pol_eval %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   discernment2 = mean(trueShare - falseShare),
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

data_pol_evalRecode$discernment <- data_pol_evalRecode$trueShare - data_pol_evalRecode$falseShare

eval_ttest <- t.test(discernment ~ condition, data = data_pol_evalRecode) %>%
  report_table(summary = TRUE) 

install.packages("pprint")
library(pubprint)
print(eval_ttest)

# Just Pennycook et al. (2020, Psych Science) and Pennycook et al. (2021, Nature):
data_pol_P20202021 <- data_pol[which(data_pol$study == "B" | data_pol$study == "C" | 
                                       data_pol$study == "G" | data_pol$study == "I"),]

data_pol_P20202021Recode <- data_pol_P20202021 %>% group_by(id) %>%
  dplyr::summarise(n = n(),
                   condition = mean(evaluationRecode),
                   trueShare = sum(trueShare, na.rm = TRUE),
                   falseShare = sum(falseShare, na.rm = TRUE), 
                   conservative = mean(conservative, na.rm = TRUE), 
                   demrepParty = mean(demrepParty, na.rm = TRUE), 
                   demrep_c = mean(demrep_c, na.rm = TRUE), 
                   politicalheadline = mean(headline_type_0C_1P),
                   MTurk = mean(mturk), 
                   Age = mean(age), 
                   College = mean(college)
  )

data_pol_P20202021Recode$discernment <- data_pol_P20202021Recode$trueShare - data_pol_P20202021Recode$falseShare

##### PLOTS

# Simple model with all studies

# Discernment:
lm1 <- lm(discernment ~ condition*conservative, data = data_polRecode)
summary(lm1)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(0,3),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Discernment") 

# True headlines:
lm2 <- lm(trueShare ~ condition*conservative, data = data_polRecode)
summary(lm2)
plotSlopes(lm2, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(3,6),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Sharing true headlines") 

# False headlines:
lm3 <- lm(falseShare ~ condition*conservative, data = data_polRecode)
summary(lm3)
plotSlopes(lm3, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(3,6),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Sharing false headlines") 

####

# With only single evaluation treatment studies

# Discernment:
lm4 <- lm(discernment ~ condition*conservative, data = data_pol_evalRecode)
summary(lm4)
plotSlopes(lm4, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(0,3),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Discernment") 

# True headlines:
lm5 <- lm(trueShare ~ condition*conservative, data = data_pol_evalRecode)
plotSlopes(lm5, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(3,6),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Sharing true headlines") 

# False headlines:
lm6 <- lm(falseShare ~ condition*conservative, data = data_pol_evalRecode)
plotSlopes(lm6, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(3,6),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Sharing false headlines") 

# With only 4 studies from Pennycook et al. (2021, Nature) and Pennycook et al. (2020, Psych Science)
# Discernment:
lm7 <- lm(discernment ~ condition*conservative, data = data_pol_P20202021Recode)
summary(lm7)
plotSlopes(lm7, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(0,3),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Discernment") 

# True headlines:
lm8 <- lm(trueShare ~ condition*conservative, data = data_pol_P20202021Recode)
plotSlopes(lm8, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(3,6),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Sharing true headlines") 

# False headlines:
lm9 <- lm(falseShare ~ condition*conservative, data = data_pol_P20202021Recode)
plotSlopes(lm9, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(3,6),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Sharing false headlines") 


# Only Roozenbeek et al., 2021 Psych Science
lm <- lm(Discernment ~ evaluation_treatment*conservative, data = R2021)
plotSlopes(lm, plotLegend = FALSE, plotx="conservative", 
           modx="evaluation_treatment", modxVals="std.dev.", 
           ylim = c(0, 3),
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Discernment") 


##############
### PLOTS PER STUDY
##############

# Study A
lmA <- lm(discernment ~ condition*conservative, data = MetaARecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.",
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study A") 

# Study B
lmB <- lm(discernment ~ condition*conservative, data = MetaBRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study B") 

# Study C
lmC <- lm(discernment ~ condition*conservative, data = MetaCRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study C") 

# Study G
lmG <- lm(discernment ~ condition*conservative, data = MetaGRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study G") 

# Study H
lmH <- lm(discernment ~ condition*conservative, data = MetaHRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study H") 

# Study I
lmI <- lm(discernment ~ condition*conservative, data = MetaIRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study I") 


# Study J
lmJ <- lm(discernment ~ condition*conservative, data = MetaJRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study J") 

# Study K
lmK <- lm(discernment ~ condition*conservative, data = MetaKRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study K") 

# Study L
lmL <- lm(discernment ~ condition*conservative, data = MetaLRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study L") 

# Study M
lmM <- lm(discernment ~ condition*conservative, data = MetaMRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study M") 

# Study P
lmP <- lm(discernment ~ condition*conservative, data = MetaPRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(1,5), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study P") 

# Study Q
lm1 <- lm(discernment ~ condition*conservative, data = MetaQRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study Q") 

# Study R
lm1 <- lm(discernment ~ condition*conservative, data = MetaRRecode)
summary(lm1)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study R") 

# Study S
lm1 <- lm(discernment ~ condition*conservative, data = MetaSRecode)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,5), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Study S") 

# Roozenbeek et al. replication
lm1 <- lm(discernment ~ condition*conservative, data = MetaRoozRecode)
summary(lm1)
plotSlopes(lm1, plotLegend = FALSE, plotx="conservative", 
           modx="condition", modxVals="std.dev.", 
           ylim = c(-1,3), xlab="Conservatism",
           gridArgs = "none", legendPct = FALSE, 
           interval = "conf", plotPoints = FALSE, ylab = "Roozenbeek replication") 

## Seperate Effects for Dems and Repubs

ARepub <- subset(MetaARecode, MetaARecode$demrepParty == 1)
BRepub <- subset(MetaBRecode, MetaBRecode$demrepParty == 1)
CRepub <- subset(MetaCRecode, MetaCRecode$demrepParty == 1)
DRepub <- subset(MetaDRecode, MetaDRecode$demrepParty == 1)
ERepub <- subset(MetaERecode, MetaERecode$demrepParty == 1)
FRepub <- subset(MetaFRecode, MetaFRecode$demrepParty == 1)
GRepub <- subset(MetaGRecode, MetaGRecode$demrepParty == 1)
HRepub <- subset(MetaHRecode, MetaHRecode$demrepParty == 1)
IRepub <- subset(MetaIRecode, MetaIRecode$demrepParty == 1)
JRepub <- subset(MetaJRecode, MetaJRecode$demrepParty == 1)
KRepub <- subset(MetaKRecode, MetaKRecode$demrepParty == 1)
LRepub <- subset(MetaLRecode, MetaLRecode$demrepParty == 1)
MRepub <- subset(MetaMRecode, MetaMRecode$demrepParty == 1)
NRepub <- subset(MetaNRecode, MetaNRecode$demrepParty == 1)
ORepub <- subset(MetaORecode, MetaORecode$demrepParty == 1)
PRepub <- subset(MetaPRecode, MetaPRecode$demrepParty == 1)
QRepub <- subset(MetaQRecode, MetaQRecode$demrepParty == 1)
RRepub <- subset(MetaRRecode, MetaRRecode$demrepParty == 1)
SRepub <- subset(MetaSRecode, MetaSRecode$demrepParty == 1)
RoozRepub <- subset(MetaRoozRecode, MetaRoozRecode$demrepParty == 1)

ADem <- subset(MetaARecode, MetaARecode$demrepParty == 0)
BDem <- subset(MetaBRecode, MetaBRecode$demrepParty == 0)
CDem <- subset(MetaCRecode, MetaCRecode$demrepParty == 0)
DDem <- subset(MetaDRecode, MetaDRecode$demrepParty == 0)
EDem <- subset(MetaERecode, MetaERecode$demrepParty == 0)
FDem <- subset(MetaFRecode, MetaFRecode$demrepParty == 0)
GDem <- subset(MetaGRecode, MetaGRecode$demrepParty == 0)
HDem <- subset(MetaHRecode, MetaHRecode$demrepParty == 0)
IDem <- subset(MetaIRecode, MetaIRecode$demrepParty == 0)
JDem <- subset(MetaJRecode, MetaJRecode$demrepParty == 0)
KDem <- subset(MetaKRecode, MetaKRecode$demrepParty == 0)
LDem <- subset(MetaLRecode, MetaLRecode$demrepParty == 0)
MDem <- subset(MetaMRecode, MetaMRecode$demrepParty == 0)
NDem <- subset(MetaNRecode, MetaNRecode$demrepParty == 0)
ODem <- subset(MetaORecode, MetaORecode$demrepParty == 0)
PDem <- subset(MetaPRecode, MetaPRecode$demrepParty == 0)
QDem <- subset(MetaQRecode, MetaQRecode$demrepParty == 0)
RDem <- subset(MetaRRecode, MetaRRecode$demrepParty == 0)
SDem <- subset(MetaSRecode, MetaSRecode$demrepParty == 0)
RoozDem <- subset(MetaRoozRecode, MetaRoozRecode$demrepParty == 0)

t1Repub <- t.test(discernment ~ condition, data = ARepub) %>%
  report_table(summary = TRUE) 
t2Repub <- t.test(discernment ~ condition, data = BRepub) %>%
  report_table(summary = TRUE) 
t3Repub <- t.test(discernment ~ condition, data = CRepub) %>%
  report_table(summary = TRUE) 
t4Repub <- t.test(discernment ~ condition, data = GRepub) %>%
    report_table(summary = TRUE)
t5Repub <- t.test(discernment ~ condition, data = HRepub) %>%
  report_table(summary = TRUE)
t6Repub <- t.test(discernment ~ condition, data = IRepub) %>%
  report_table(summary = TRUE)
t7Repub <- t.test(discernment ~ condition, data = JRepub) %>%
  report_table(summary = TRUE)
t8Repub <- t.test(discernment ~ condition, data = KRepub) %>%
  report_table(summary = TRUE)
t9Repub <- t.test(discernment ~ condition, data = LRepub) %>%
  report_table(summary = TRUE)
t10Repub <- t.test(discernment ~ condition, data = MRepub) %>%
  report_table(summary = TRUE)
t11Repub <- t.test(discernment ~ condition, data = PRepub) %>%
  report_table(summary = TRUE)
t12Repub <- t.test(discernment ~ condition, data = QRepub) %>%
  report_table(summary = TRUE)
t13Repub <- t.test(discernment ~ condition, data = RRepub) %>%
  report_table(summary = TRUE)
t14Repub <- t.test(discernment ~ condition, data = SRepub) %>%
  report_table(summary = TRUE)
t15Repub <- t.test(discernment ~ condition, data = RoozRepub) %>%
  report_table(summary = TRUE)

RRepub$falseShare

t1Dem <- t.test(discernment ~ condition, data = ADem) %>%
  report_table(summary = TRUE) 
t2Dem <- t.test(discernment ~ condition, data = BDem) %>%
  report_table(summary = TRUE) 
t3Dem <- t.test(discernment ~ condition, data = CDem) %>%
  report_table(summary = TRUE) 
t4Dem <- t.test(discernment ~ condition, data = GDem) %>%
  report_table(summary = TRUE)
t5Dem <- t.test(discernment ~ condition, data = HDem) %>%
  report_table(summary = TRUE)
t6Dem <- t.test(discernment ~ condition, data = IDem) %>%
  report_table(summary = TRUE)
t7Dem <- t.test(discernment ~ condition, data = JDem) %>%
  report_table(summary = TRUE)
t8Dem <- t.test(discernment ~ condition, data = KDem) %>%
  report_table(summary = TRUE)
t9Dem <- t.test(discernment ~ condition, data = LDem) %>%
  report_table(summary = TRUE)
t10Dem <- t.test(discernment ~ condition, data = MDem) %>%
  report_table(summary = TRUE)
t11Dem <- t.test(discernment ~ condition, data = PDem) %>%
  report_table(summary = TRUE)
t12Dem <- t.test(discernment ~ condition, data = QDem) %>%
  report_table(summary = TRUE)
t13Dem <- t.test(discernment ~ condition, data = RDem) %>%
  report_table(summary = TRUE)
t14Dem <- t.test(discernment ~ condition, data = SDem) %>%
  report_table(summary = TRUE)
t15Dem <- t.test(discernment ~ condition, data = RoozDem) %>%
  report_table(summary = TRUE)

get_summary_stats()

RepubTable <- rbind.fill(t1Repub, 
                         t2Repub, 
                         t3Repub,
                         t4Repub, 
                         t5Repub,
                         t6Repub,
                         t7Repub,
                         t8Repub,
                         t9Repub,
                         t10Repub, 
                         t11Repub,
                         t12Repub,
                         t13Repub,
                         t14Repub,
                         t15Repub)
View(RepubTable)

DemTable <- rbind.fill(t1Dem, 
                         t2Dem, 
                         t3Dem,
                         t4Dem, 
                         t5Dem,
                         t6Dem,
                         t7Dem,
                         t8Dem,
                         t9Dem,
                         t10Dem, 
                         t11Dem,
                         t12Dem,
                         t13Dem,
                         t14Dem,
                         t15Dem
                       )
View(DemTable)

A <- ARepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd") 
A <- A %>%
  mutate(study = "A")
B <- BRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
B <- B %>%
  mutate(study = "B")
C <- CRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
C <- C %>%
  mutate(study = "C")
G <- GRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
G <- G %>%
  mutate(study = "G")
H <- HRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
H <- H %>%
  mutate(study = "H")
I <- IRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
I <- I %>%
  mutate(study = "I")
J <- JRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
J <- J %>%
  mutate(study = "J")
K <- KRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
K <- K %>%
  mutate(study = "K")
L <- LRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
L <- L %>%
  mutate(study = "L")
M <- MRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
M <- M %>%
  mutate(study = "M")
P <- PRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
P <- P %>%
  mutate(study = "P")
Q <- QRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
Q <- Q %>%
  mutate(study = "Q")
R <- RRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
R <- R %>%
  mutate(study = "R")
S <- SRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
S <- S %>%
  mutate(study = "S")
Rooz <- RoozRepub %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
Rooz <- Rooz %>%
  mutate(study = "Rooz")

RepubTable <- rbind(A, B, C, G, H, I, J, K, L, M, P, Q, R, S, Rooz)

A <- ADem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd") 
A <- A %>%
  mutate(study = "A")
B <- BDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
B <- B %>%
  mutate(study = "B")
C <- CDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
C <- C %>%
  mutate(study = "C")
G <- GDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
G <- G %>%
  mutate(study = "G")
H <- HDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
H <- H %>%
  mutate(study = "H")
I <- IDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
I <- I %>%
  mutate(study = "I")
J <- JDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
I <- I %>%
  mutate(study = "J")
K <- KDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
K <- K %>%
  mutate(study = "K")
L <- LDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
L <- L %>%
  mutate(study = "L")
M <- MDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
M <- M %>%
  mutate(study = "M")
P <- PDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
P <- P %>%
  mutate(study = "P")
Q <- QDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
Q <- Q %>%
  mutate(study = "Q")
R <- RDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
R <- R %>%
  mutate(study = "R")
S <- SDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
S <- S %>%
  mutate(study = "S")
Rooz <- RoozDem %>%
  group_by(condition) %>%
  get_summary_stats(discernment, type = "mean_sd")
Rooz <- Rooz %>%
  mutate(study = "Rooz")

DemTable <- rbind(A, B, C, G, H, I, J, K, L, M, P, Q, R, S, Rooz)
View(DemTable)

metaRepub <- read_csv("MetaAnalysisRepub.csv")
metaDem <- read_csv("MetaAnalysisDem.csv")

metaRepub2 <- metaRepub[-c(10:11),]
metaDem2 <- metaDem[-c(10:11),]

metacont(Nc,
         Mc,
         Sc,
         Ne,
         Me,
         Se,
         #comb.fixed = TRUE,
         #comb.random = TRUE,
         #prediction=TRUE,
         sm="SMD",
         method.smd = "Cohen",
         method.tau = "DL",
         data = metaRepub)

metacont(Nc,
         Mc,
         Sc,
         Ne,
         Me,
         Se,
         #comb.fixed = TRUE,
         #comb.random = TRUE,
         #prediction=TRUE,
         sm="SMD",
         method.smd = "Cohen",
         method.tau = "DL",
         data = metaRepub2)

metacont(Nc,
         Mc,
         Sc,
         Ne,
         Me,
         Se,
         #comb.fixed = TRUE,
         #comb.random = TRUE,
         #prediction=TRUE,
         sm="SMD",
         method.smd = "Cohen",
         method.tau = "DL",
         data = metaDem)

metacont(Nc,
         Mc,
         Sc,
         Ne,
         Me,
         Se,
         #comb.fixed = TRUE,
         #comb.random = TRUE,
         #prediction=TRUE,
         sm="SMD",
         method.smd = "Cohen",
         method.tau = "DL",
         data = metaDem2)


nrow(ARepub)
nrow(BRepub)
nrow(CRepub)
nrow(GRepub)
nrow(HRepub)
nrow(IRepub)
nrow(JRepub)
nrow(KRepub)
nrow(LRepub)
nrow(MRepub)
nrow(PRepub)
nrow(QRepub)
nrow(RRepub)
nrow(SRepub)
nrow(RoozRepub)

nrow(ADem)
nrow(BDem)
nrow(CDem)
nrow(GDem)
nrow(HDem)
nrow(IDem)
nrow(JDem)
nrow(KDem)
nrow(LDem)
nrow(MDem)
nrow(PDem)
nrow(QDem)
nrow(RDem)
nrow(SDem)
nrow(RoozDem)

