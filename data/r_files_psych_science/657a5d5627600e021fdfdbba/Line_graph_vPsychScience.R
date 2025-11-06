#Libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(dplyr)
library(egg)

###Creating a backup for raw data
backUpData <- pbpEjectReason

###Create function to add tag to panels without loosing strip labels
tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                       hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}

################################################## First figure ###############################################
pbpEjectReason <- backUpData

####Rename factors for Ejection
levels(pbpEjectReason$Ejection) <- c("Before", "After")
###Reorder for better color fitting 
pbpEjectReason$Ejection <- factor(pbpEjectReason$Ejection , c("After", "Before"))
####Rename factors for EjectedTeam
levels(pbpEjectReason$EjectedTeam) <- c("Not ejected Team", "Ejected Team")

####Fit Model 1 with combined factor (EjectedTeam*Ejection)
model1 <- glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                  (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName) +
                  cs_prob + about.halfInning + Ejection*EjectedTeam, 
                data = pbpEjectReason, family = binomial, 
                control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 

#####Plot model2
fig1 <- plot_model(model1, type = "eff", terms = c("cs_prob [all]", "Ejection", "EjectedTeam"),
                   axis.title = c("Baseline called strike probability","Called strike probability in games with ejection"), title="",
                   legend.title = "Ejection")+ theme_classic()

tag_facet2(fig1, tag_pool = c("a", "b"))

ggsave("Fig1.pdf", width = 24, height = 10, units = "cm", dpi = 900)


################################################## Second figure ###############################################
pbpEjectReason <- backUpData

####Rename factors for Ejection
levels(pbpEjectReason$Ejection) <- c("Before", "After")
###Reorder for better color fitting 
pbpEjectReason$Ejection <- factor(pbpEjectReason$Ejection , c("After", "Before"))

####Create factors for combination of Moment of ejection and EjectionReason
pbpEjectReason$EjectReasonByTeam <- as.factor(paste(pbpEjectReason$EjectedTeam, pbpEjectReason$EjectionReason))
levels(pbpEjectReason$EjectReasonByTeam) <- c("Not ejected Team - SZ criticism", "Not ejected Team - Non-SZ criticism", "Not ejected Team - Physical aggression",
                                                "Ejected Team - SZ criticism", "Ejected Team - Non-SZ criticism", "Ejected Team - Physical aggression")

####Fit Model 2 with combined factor (EjectedTeam*EjectionReason)
model2 <- glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                           (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName) +
                           cs_prob + about.halfInning + Ejection*EjectReasonByTeam, 
                         data = pbpEjectReason, family = binomial, 
                         control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 

#####Plot model2
fig2 <- plot_model(model2, type = "eff", terms = c("cs_prob [all]", "Ejection", "EjectReasonByTeam"),
           axis.title = c("Baseline called strike probability","Called strike probability in games with ejection"), title="",
           legend.title = "Ejection")+ theme_classic()

tag_facet2(fig2, tag_pool = c("a", "c", "e", "b", "d", "f"))

ggsave("Fig2.pdf", width = 32, height = 18, units = "cm", dpi = 900)

################################################## Third figure ###############################################
pbpEjectReason <- backUpData
pbpEjectReason$JOB <- recode(pbpEjectReason$JOB, C = "C", M = "C", P = "P")

pbpRole <- subset(pbpEjectReason, pbpEjectReason$EjectionReason == 0 & pbpEjectReason$EJECTEENUMBER == 1)

####Rename factors for Ejection
levels(pbpRole$Ejection) <- c("Before", "After")

###Reorder for better color fitting 
pbpRole$Ejection <- factor(pbpRole$Ejection , c("After", "Before"))

####Create factors for combination of Moment of ejection and Role of ejectee
pbpRole$RoleByTeam <- as.factor(paste(pbpRole$EjectedTeam, pbpRole$JOB))
levels(pbpRole$RoleByTeam) <- c("Not ejected Team - Coach", "Not ejected Team - Players",
                                              "Ejected Team - Coach", "Ejected Team - Players")

####Fit Model 3 with combined factor (EjectedTeam*JOB)
model3 <- glmer(calledStrike ~ (1|UMPIRENAME) + (1|fielder_2) + 
                  (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName) +
                  cs_prob + about.halfInning + Ejection*RoleByTeam, 
                data = pbpRole, family = binomial, 
                control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 

#####Plot model3
fig3 <- plot_model(model3, type = "eff", terms = c("cs_prob [all]", "Ejection", "RoleByTeam"),
                   axis.title = c("Baseline called strike probability","Called strike probability in games with ejection"), title="",
                   legend.title = "Ejection") + theme_classic() 

tag_facet2(fig3, tag_pool = c("a", "c", "b", "d"))

ggsave("Fig3.pdf", width = 24, height = 18, units = "cm", dpi = 900)


