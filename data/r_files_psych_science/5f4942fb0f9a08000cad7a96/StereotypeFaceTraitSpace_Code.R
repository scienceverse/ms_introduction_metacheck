# libraries ####
library(psych)
library(dplyr)
library(reshape2)
library(lavaan)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2);library(RColorBrewer) # for plotting only

# import ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("study2data_StereotypeFaceTraitSpace.csv")

# center TraitSimilarity variable ####
data <- data %>%
  mutate(TraitSimilarity_gmc = TraitSimilarity-mean(TraitSimilarity)) %>% # grand mean centering
  group_by(ParticipantID) %>% 
  mutate(TraitSimilarity_cm = mean(TraitSimilarity), # cluster mean
         TraitSimilarity_cwc = TraitSimilarity-TraitSimilarity_cm) %>% # centered within cluster
  ungroup %>%
  mutate(TraitSimilarity_cmc = TraitSimilarity_cm-mean(TraitSimilarity_cm))

# function to obtain standardized beta coefficients from fitted lme4 models
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# Analysis ####

# > full model (Equation 1 in manuscript) ####
fullmlm <- lmer(faceCor ~ TraitSimilarity_cwc + TraitSimilarity_cm + (TraitSimilarity_cwc | ParticipantID) + (1 | Traitpair), data=data, REML=TRUE)
summary(fullmlm)
stdCoef.merMod(fullmlm)

# confidence intervals (might take a while)
confint(fullmlm, "beta_", level = 0.95,
        method = c("profile", "Wald", "boot"), 
        nsim = 500,
        boot.type = c("perc","basic","norm"),
        FUN = NULL, quiet = FALSE,
        oldNames = TRUE)


# > full model with RacexGen moderators (Equation in Supplementary Materials) ####
data <- mutate(data, TarRaceblack = ifelse(TarRace=="black", 1, 0)) %>%
  mutate(TarRaceasian = ifelse(TarRace=="asian", 1, 0)) %>%
  mutate(TarRace_whiteref = ifelse(TarRace == "black", 1,
                                   ifelse(TarRace == "asian",2,0)))
data$TarRace_whiteref = as.factor(data$TarRace_whiteref)

suppmlm <- lmer(faceCor ~ TraitSimilarity_cwc*TarGen*TarRaceblack*TarRaceasian + TraitSimilarity_cm + (TraitSimilarity_cwc | ParticipantID) + (1 | Traitpair), data=data, REML=TRUE)
print(summary(suppmlm),digits=3)
stdCoef.merMod(suppmlm)


# Plot ####
p <- ggplot(data=data, aes(x = TraitSimilarity, y = faceCor)) +
  scale_color_distiller(palette="YlGnBu") +
  geom_line(aes(group=ParticipantID,colour=ParticipantID),size=0.8,stat="smooth",method = "lm",alpha = 0.5) + 
  stat_smooth(method = "lm", formula = y ~ x, colour="coral", size = 2, se=F) +
  theme_minimal() + 
  theme(legend.position="none") +
  coord_cartesian(ylim=c(-1, 1), xlim=c(1,7)) +
  theme(text=element_text(size=30, family="Corbel")) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"))+
  xlab("\nStereotype Trait Space") +
  ylab("\nFace-Trait Space")
print(p)
