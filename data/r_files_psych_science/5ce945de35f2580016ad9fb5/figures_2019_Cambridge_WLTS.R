####################################
# Version 1
####################################
# WD
setwd("U:/Publications/talks/talk_2019_Cambridge_WLTS/images")

# Libaries
library(ggplot2)
library(brms)
library(cowplot)
theme_set(theme_grey())
library(plyr)
library(assortedRFunctions)

############################################################################################################
# Plot settings
cbPalette     <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
kitchenColour <- c(cbPalette[2], cbPalette[3])
expColour     <- cbPalette[6:8]
mrcColour     <- '#77685C' # MRC colour 77685C
axisText      <- 20
legendText    <- 15
annotateSize  <- 5

############################################################################################################
# Loading and preparing data
####################################
# schemaVR1
####################################
load("U:/Projects/schemaVR/report_firstYear/report/data/exp1Data.RData")
dataSchemaVR1 <- combData
rm(combData)

# Notes and decision on participants:
# Participant #1: Sligthly different instructions for no-memory trial -> exlcude.
# Participant #1 to #6: Had no familiarisation phase
# Participant #5 and # 15: Second exposure with stimulus -> include.
# Participant #9: No memory trials on trial 1 and 2 because I pressed those accidently -> changed.
dataSchemaVR1[which(dataSchemaVR1$subNum == 9 & (dataSchemaVR1$recallTrial == 1 | dataSchemaVR1$recallTrial == 2)), 'recallNoMemory'] <- 1
# Participant #10: Problems with data and from towel -> no anormalities.
# Participant #13: Had a longer delay (1.5 min)
dataSchemaVR1 <- subset(dataSchemaVR1, subNum != 1)

#Add (non-)kitchen object factor to data
dataSchemaVR1$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR1[which(dataSchemaVR1$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Scaling data for analysis
dataSchemaVR1_Scaled                    <- dataSchemaVR1
dataSchemaVR1_Scaled$objLocTargetRating <- scale(dataSchemaVR1_Scaled$objLocTargetRating)
dataSchemaVR1_Scaled$generalRatingPost  <- scale(dataSchemaVR1_Scaled$generalRatingPost)

# Subsetting data
dataSchemaVR1_AFC    <- subset(dataSchemaVR1_Scaled, dataSchemaVR1_Scaled$resCon != 1)
dataSchemaVR1_recall <- subset(dataSchemaVR1_Scaled, dataSchemaVR1_Scaled$recallNoMemory == 0)

####################################
# schemaVR2
####################################
load("U:/Projects/schemaVR/report_firstYear/report/data/exp2Data.RData")
dataSchemaVR2 <- combData
rm(combData)

#Add (non-)kitchen object factor to data
dataSchemaVR2$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR2[which(dataSchemaVR2$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Notes and decision on participants:
# Participant #20 to #24: Exclude participants because they did the  wrong objLocTargetRating
dataSchemaVR2 <- subset(dataSchemaVR2, subNum >= 25)
# Participant #20 to #27: Foil2 for umbrella was not saved.
# Participant #25 to #27: Rated object 16 at location 1 instead of 14. This value is therefore missing.
# Participant #22: Seen objects twice but is excluded anyway. 
# Participant #26: Check recall microwave because it was correct (it is). The 2nd rating was 100 not 0.
dataSchemaVR2[which(dataSchemaVR2$subNum == 26 & dataSchemaVR2$objNum == 3), 'generalRatingPost'] <- 100

# Response given for judgement (0 = no memory, 1 = remember, 2 = familiar, 3 = guess)
dataSchemaVR2$recallMemory[which(dataSchemaVR2$recallMemory == -1)] <- NA # Code missing value

# Scaling data for analysis
dataSchemaVR2_Scaled                    <- dataSchemaVR2
dataSchemaVR2_Scaled$objLocTargetRating <- scale(dataSchemaVR2_Scaled$objLocTargetRating)
dataSchemaVR2_Scaled$generalRatingPost  <- scale(dataSchemaVR2_Scaled$generalRatingPost)

# Subsetting data
dataSchemaVR2_AFC    <- subset(dataSchemaVR2_Scaled, dataSchemaVR2_Scaled$resCon != 0)
dataSchemaVR2_recall <- subset(dataSchemaVR2_Scaled, dataSchemaVR2_Scaled$recallMemory != 0)

####################################
# schemaVR3
####################################
load("U:/Projects/schemaVR/schemaVR3/data/dataSchemaVR3.RData")

# 3 said no memory for fruitbowl instead of guess
dataSchemaVR3[dataSchemaVR3$subNum == 3 & dataSchemaVR3$objNam == 'bowl of fruits', 'recallMemory'] <- 0
dataSchemaVR3[dataSchemaVR3$subNum == 3 & dataSchemaVR3$objNam == 'mixer', 'recallMemory']          <- 0
# Toy that was presented before the helmet should become NA
# 6 very poor recall performance
# 7 meant to press 2 instead of 1 for toy
dataSchemaVR3[dataSchemaVR3$subNum == 7 & dataSchemaVR3$objNam == 'toy', 'resAFC'] <- 2
# 13 said remember to towels
dataSchemaVR3[dataSchemaVR3$subNum == 13 & dataSchemaVR3$objNam == 'towels', 'recallMemory'] <- 1

# Decided to exclude # 5
dataSchemaVR3        <- dataSchemaVR3[dataSchemaVR3$subNum != 5, ]
dataSchemaVR3$setNum <- as.factor(dataSchemaVR3$setNum )

#Add (non-)kitchen object factor to data
dataSchemaVR3$expectedInKitchen                                      <- 'non-kitchen'
dataSchemaVR3[which(dataSchemaVR2$objNum < 13), 'expectedInKitchen'] <- 'kitchen'

# Scaling data for analysis
dataSchemaVR3_Scaled                    <- dataSchemaVR3
dataSchemaVR3_Scaled$objLocTargetRating <- scale(dataSchemaVR3_Scaled$objLocTargetRating)
dataSchemaVR3_Scaled$targetRankPre      <- scale(dataSchemaVR3_Scaled$targetRankPre)
dataSchemaVR3_Scaled$generalRatingPost  <- scale(dataSchemaVR3_Scaled$generalRatingPost)

# Subsetting data
dataSchemaVR3_AFC    <- subset(dataSchemaVR3_Scaled, dataSchemaVR3_Scaled$resCon != '0')
dataSchemaVR3_recall <- subset(dataSchemaVR3_Scaled, dataSchemaVR3_Scaled$recallMemory != '0' & !is.na(dataSchemaVR3_Scaled$recallMemory))

####################################
# Bayesian results
####################################
load('U:/Projects/schemaVR/schemaVR3/analysis/bayesianMM_recall_20190402_121352.RData')
load('U:/Projects/schemaVR/schemaVR3/analysis/bayesianMM_euclid_20190402_114634.RData')
load('U:/Projects/schemaVR/schemaVR3/analysis/bayesianMM_AFC_20190402_120010.RData')

load('U:/Projects/schemaVR/schemaVR3/analysis/bayesianMM_euclid_twolines_20190401_145823.RData')
load('U:/Projects/schemaVR/schemaVR3/analysis/bayesianMM_recall_twolines_20190401_152413.RData')
load('U:/Projects/schemaVR/schemaVR3/analysis/bayesianMM_AFC_twolines_20190401_150832.RData')

load('U:/Projects/schemaVR/schemaVR3/analysis/bayesian_RK_20190403_072709.RData')

####################################
# Main results
####################################
####################################
# Exp 1
####################################
# Limits
# Limits
xlims    <- c(-1.63, 1.43)
ylimsAcc <- c(-0.039, 1.039)
yLimsEuc <- c(-0.02, 6)
x        <- seq(xlims[1], xlims[2], 0.01)

# Scatterplots
# Plot 1
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR1_recall_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR1_recall_effM_postDen[, 1] + schemaVR1_recall_effM_postDen[, 2]*xMatrix + schemaVR1_recall_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Get mean fit
meanFit        <- fixef(schemaVR1_recall_effM)[1, 1] + fixef(schemaVR1_recall_effM)[2,1]*x + fixef(schemaVR1_recall_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,logitFit = meanFit)
meanFit_DF$fit <- logit2prob(meanFit_DF$logitFit)

# Plot
schemaVR1_plot1 <- ggplot(dataSchemaVR1_recall, aes(x = objLocTargetRating, y = accRecall)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5,
             aes(colour = expectedInKitchen))  +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Pr (recall accuracy)',
       x = "Expectancy") +
  coord_cartesian(ylim = c(ylimsAcc[1],
                           ylimsAcc[2]),
                  xlim = c(xlims[1],
                           xlims[2]),
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) + 
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText)) 

####################################
# Plot 2
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR1_euclid_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR1_euclid_effM_postDen[, 1] + schemaVR1_euclid_effM_postDen[, 2]*xMatrix + schemaVR1_euclid_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit = c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR1_euclid_effM)[1, 1] + fixef(schemaVR1_euclid_effM)[2,1]*x + fixef(schemaVR1_euclid_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,fit = meanFit)

#Plot
schemaVR1_plot2 <- ggplot(dataSchemaVR1_recall, aes(x = objLocTargetRating, y = euclideanDist)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5, 
             aes(colour = expectedInKitchen), 
             show.legend = FALSE)  +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Euclidean distance in vm', 
       x = "Expectancy") +
  coord_cartesian(ylim = c(yLimsEuc[1], 
                           yLimsEuc[2]),  
                  xlim = c(xlims[1], 
                           xlims[2]), 
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Plot1
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR1_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR1_AFC_effM_postDen[, 1] + schemaVR1_AFC_effM_postDen[, 2]*xMatrix + schemaVR1_AFC_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Get mean fit
meanFit        <- fixef(schemaVR1_AFC_effM)[1, 1] + fixef(schemaVR1_AFC_effM)[2,1]*x + fixef(schemaVR1_AFC_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,logitFit = meanFit)
meanFit_DF$fit <- logit2prob(meanFit_DF$logitFit)

# Plot 3
schemaVR1_plot3 <- ggplot(dataSchemaVR1_AFC, aes(x = objLocTargetRating, y = accAFC)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5, 
             aes(colour = expectedInKitchen),
             show.legend = FALSE) +  
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Pr (3AFC accuracy)',  
       x = "Expectancy") +
  coord_cartesian(ylim = c(ylimsAcc[1], 
                           ylimsAcc[2]),
                  xlim = c(xlims[1], 
                           xlims[2]), 
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Plot posterior distributions
# Recall accuracy
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_recall_quad_eff <- data.frame(Experiment = Experiment,
                                      values = Values)

# Get values for lines
yMax                     <- max(density(schemaVR3_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR1_recall_effM_CI <- as.numeric(fixef(schemaVR1_recall_effM)[3,3:4])

# Plot
postDen_recall_quad_plot <- ggplot(postDen_recall_quad_eff, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR1_recall_effM_CI[1],
               y = 0,
               xend = schemaVR1_recall_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR1_recall_effM_CI[2],
               y = 0,
               xend = schemaVR1_recall_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR1_recall_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR1_recall_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR1_recall_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Euclidean distance
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_euclid_quad_eff <- data.frame(Experiment = Experiment,
                                      values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR1_euclid_effM_CI <- as.numeric(fixef(schemaVR1_euclid_effM)[3,3:4])

# Plot
postDen_euclid_quad_plot <- ggplot(postDen_euclid_quad_eff, aes(x = values, fill = Experiment)) +
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR1_euclid_effM_CI[1],
               y = 0,
               xend = schemaVR1_euclid_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR1_euclid_effM_CI[2],
               y = 0,
               xend = schemaVR1_euclid_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR1_euclid_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR1_euclid_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR1_euclid_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 8), expand = FALSE) +
  scale_fill_manual(values = expColour) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# 3 AFC accuracy
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_AFC_quad_eff <- data.frame(Experiment = Experiment,
                                   values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR1_AFC_effM_CI <- as.numeric(fixef(schemaVR1_AFC_effM)[3,3:4])

# Plot
postDen_AFC_quad_plot <- ggplot(postDen_AFC_quad_eff, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR1_AFC_effM_CI[1],
               y = 0,
               xend = schemaVR1_AFC_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR1_AFC_effM_CI[2],
               y = 0,
               xend = schemaVR1_AFC_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR1_AFC_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR1_AFC_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR1_AFC_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  scale_fill_manual(values = expColour) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Save figure
combinedPlots <- plot_grid(schemaVR1_plot1,
                          schemaVR1_plot2,
                          schemaVR1_plot3,
                          postDen_recall_quad_plot,
                          postDen_euclid_quad_plot,
                          postDen_AFC_quad_plot,
                          ncol = 3,
                          nrow = 2)

save_plot("schemaVR1.png", combinedPlots,
          ncol = 3, 
          nrow = 2,
          base_height = 12/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1)

# Delete all plots to save memory
rm(combinedPlots)
rm(schemaVR1_plot1)
rm(schemaVR1_plot2)
rm(schemaVR1_plot3)
rm(postDen_recall_quad_plot)
rm(postDen_euclid_quad_plot)
rm(postDen_AFC_quad_plot)

####################################
# Exp 2
####################################
# Scatterplots
# Plot 1
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR2_recall_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR2_recall_effM_postDen[, 1] + schemaVR2_recall_effM_postDen[, 2]*xMatrix + schemaVR2_recall_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Get mean fit
meanFit        <- fixef(schemaVR2_recall_effM)[1, 1] + fixef(schemaVR2_recall_effM)[2,1]*x + fixef(schemaVR2_recall_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,logitFit = meanFit)
meanFit_DF$fit <- logit2prob(meanFit_DF$logitFit)

# Plot
schemaVR2_plot1 <- ggplot(dataSchemaVR2_recall, aes(x = objLocTargetRating, y = accRecall)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5,
             aes(colour = expectedInKitchen))  +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Pr (recall accuracy)',
       x = "Expectancy") +
  coord_cartesian(ylim = c(ylimsAcc[1],
                           ylimsAcc[2]),
                  xlim = c(xlims[1],
                           xlims[2]),
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) + 
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText)) 

####################################
# Plot 2
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR2_euclid_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR2_euclid_effM_postDen[, 1] + schemaVR2_euclid_effM_postDen[, 2]*xMatrix + schemaVR2_euclid_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit = c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR2_euclid_effM)[1, 1] + fixef(schemaVR2_euclid_effM)[2,1]*x + fixef(schemaVR2_euclid_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,fit = meanFit)

#Plot
schemaVR2_plot2 <- ggplot(dataSchemaVR2_recall, aes(x = objLocTargetRating, y = euclideanDist)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5, 
             aes(colour = expectedInKitchen), 
             show.legend = FALSE)  +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Euclidean distance in vm', 
       x = "Expectancy") +
  coord_cartesian(ylim = c(yLimsEuc[1], 
                           yLimsEuc[2]),  
                  xlim = c(xlims[1], 
                           xlims[2]), 
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Plot1
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR2_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR2_AFC_effM_postDen[, 1] + schemaVR2_AFC_effM_postDen[, 2]*xMatrix + schemaVR2_AFC_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Get mean fit
meanFit        <- fixef(schemaVR2_AFC_effM)[1, 1] + fixef(schemaVR2_AFC_effM)[2,1]*x + fixef(schemaVR2_AFC_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,logitFit = meanFit)
meanFit_DF$fit <- logit2prob(meanFit_DF$logitFit)

# Plot 3
schemaVR2_plot3 <- ggplot(dataSchemaVR2_AFC, aes(x = objLocTargetRating, y = accAFC)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5, 
             aes(colour = expectedInKitchen),
             show.legend = FALSE) +  
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Pr (3AFC accuracy)',  
       x = "Expectancy") +
  coord_cartesian(ylim = c(ylimsAcc[1], 
                           ylimsAcc[2]),
                  xlim = c(xlims[1], 
                           xlims[2]), 
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Plot posterior distributions
# Recall accuracy
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 2',length(schemaVR2_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR2_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_recall_quad_eff <- data.frame(Experiment = Experiment,
                                      values = Values)

# Get values for lines
yMax                     <- max(density(schemaVR3_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR2_recall_effM_CI <- as.numeric(fixef(schemaVR2_recall_effM)[3,3:4])

# Plot
postDen_recall_quad_plot <- ggplot(postDen_recall_quad_eff, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR2_recall_effM_CI[1],
               y = 0,
               xend = schemaVR2_recall_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR2_recall_effM_CI[2],
               y = 0,
               xend = schemaVR2_recall_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR2_recall_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR2_recall_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR2_recall_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Euclidean distance
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 2',length(schemaVR2_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR2_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_euclid_quad_eff <- data.frame(Experiment = Experiment,
                                      values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR2_euclid_effM_CI <- as.numeric(fixef(schemaVR2_euclid_effM)[3,3:4])

# Plot
postDen_euclid_quad_plot <- ggplot(postDen_euclid_quad_eff, aes(x = values, fill = Experiment)) +
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR2_euclid_effM_CI[1],
               y = 0,
               xend = schemaVR2_euclid_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR2_euclid_effM_CI[2],
               y = 0,
               xend = schemaVR2_euclid_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR2_euclid_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR2_euclid_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR2_euclid_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 8), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# 3 AFC accuracy
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 2',length(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR2_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_AFC_quad_eff <- data.frame(Experiment = Experiment,
                                   values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR2_AFC_effM_CI <- as.numeric(fixef(schemaVR2_AFC_effM)[3,3:4])

# Plot
postDen_AFC_quad_plot <- ggplot(postDen_AFC_quad_eff, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR2_AFC_effM_CI[1],
               y = 0,
               xend = schemaVR2_AFC_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR2_AFC_effM_CI[2],
               y = 0,
               xend = schemaVR2_AFC_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR2_AFC_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR2_AFC_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR2_AFC_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Save figure
combinedPlots <- plot_grid(schemaVR2_plot1,
                           schemaVR2_plot2,
                           schemaVR2_plot3,
                           postDen_recall_quad_plot,
                           postDen_euclid_quad_plot,
                           postDen_AFC_quad_plot,
                           ncol = 3,
                           nrow = 2)

save_plot("schemaVR2.png", combinedPlots,
          ncol = 3, 
          nrow = 2,
          base_height = 12/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1)

# Delete all plots to save memory
rm(combinedPlots)
rm(schemaVR2_plot1)
rm(schemaVR2_plot2)
rm(schemaVR2_plot3)
rm(postDen_recall_quad_plot)
rm(postDen_euclid_quad_plot)
rm(postDen_AFC_quad_plot)

####################################
# Exp 3
####################################
xlims    <- c(-1.53, 1.43)
ylimsAcc <- c(-0.039, 1.039)
yLimsEuc <- c(-0.02, 6)
x        <- seq(xlims[1], xlims[2], 0.01)

# Scatterplots
# Plot 1
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR3_recall_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR3_recall_effM_postDen[, 1] + schemaVR3_recall_effM_postDen[, 2]*xMatrix + schemaVR3_recall_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Get mean fit
meanFit        <- fixef(schemaVR3_recall_effM)[1, 1] + fixef(schemaVR3_recall_effM)[2,1]*x + fixef(schemaVR3_recall_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,logitFit = meanFit)
meanFit_DF$fit <- logit2prob(meanFit_DF$logitFit)

# Plot
schemaVR3_plot1 <- ggplot(dataSchemaVR3_recall, aes(x = objLocTargetRating, y = accRecall)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5,
             aes(colour = kitchen))  +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Pr (recall accuracy)',
       x = "Expectancy") +
  coord_cartesian(ylim = c(ylimsAcc[1],
                           ylimsAcc[2]),
                  xlim = c(xlims[1],
                           xlims[2]),
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) + 
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText)) 

####################################
# Plot 2
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR3_euclid_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR3_euclid_effM_postDen[, 1] + schemaVR3_euclid_effM_postDen[, 2]*xMatrix + schemaVR3_euclid_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit = c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR3_euclid_effM)[1, 1] + fixef(schemaVR3_euclid_effM)[2,1]*x + fixef(schemaVR3_euclid_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,fit = meanFit)

#Plot
schemaVR3_plot2 <- ggplot(dataSchemaVR3_recall, aes(x = objLocTargetRating, y = euclideanDist)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5, 
             aes(colour = kitchen), 
             show.legend = FALSE)  +
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Euclidean distance in vm', 
       x = "Expectancy") +
  coord_cartesian(ylim = c(yLimsEuc[1], 
                           yLimsEuc[2]),  
                  xlim = c(xlims[1], 
                           xlims[2]), 
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Plot1
# Extract line fits from Bayesian analysis
nDraws      <- dim(schemaVR3_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR3_AFC_effM_postDen[, 1] + schemaVR3_AFC_effM_postDen[, 2]*xMatrix + schemaVR3_AFC_effM_postDen[, 3]*xMatrix*xMatrix
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), logitFit= c(drawnFit))
drawnFit_DF$fit <- logit2prob(drawnFit_DF$logitFit)

# Get mean fit
meanFit        <- fixef(schemaVR3_AFC_effM)[1, 1] + fixef(schemaVR3_AFC_effM)[2,1]*x + fixef(schemaVR3_AFC_effM)[3,1]*x*x
meanFit_DF     <- data.frame(x = x,logitFit = meanFit)
meanFit_DF$fit <- logit2prob(meanFit_DF$logitFit)

# Plot 3
schemaVR3_plot3 <- ggplot(dataSchemaVR3_AFC, aes(x = objLocTargetRating, y = accAFC)) + 
  geom_point(position = position_jitter(height = 0.03, 
                                        width = 0), 
             alpha = 0.5, 
             aes(colour = kitchen),
             show.legend = FALSE) +  
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Pr (3AFC accuracy)',  
       x = "Expectancy") +
  coord_cartesian(ylim = c(ylimsAcc[1], 
                           ylimsAcc[2]),
                  xlim = c(xlims[1], 
                           xlims[2]), 
                  expand = FALSE) + 
  scale_color_manual(values = kitchenColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0.7, 0.75),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 0.7)),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Plot posterior distributions
# Recall accuracy
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 2',length(schemaVR2_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 3',length(schemaVR3_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR2_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR3_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_recall_quad_eff <- data.frame(Experiment = Experiment,
                                      values = Values)

# Get values for lines
yMax                     <- max(density(schemaVR3_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR3_recall_effM_CI <- as.numeric(fixef(schemaVR3_recall_effM)[3,3:4])

# Plot
postDen_recall_quad_plot <- ggplot(postDen_recall_quad_eff, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_recall_effM_CI[1],
               y = 0,
               xend = schemaVR3_recall_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_recall_effM_CI[2],
               y = 0,
               xend = schemaVR3_recall_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_recall_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_recall_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_recall_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Euclidean distance
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 2',length(schemaVR2_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 3',length(schemaVR3_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR2_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR3_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_euclid_quad_eff <- data.frame(Experiment = Experiment,
                                      values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_euclid_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR3_euclid_effM_CI <- as.numeric(fixef(schemaVR3_euclid_effM)[3,3:4])

# Plot
postDen_euclid_quad_plot <- ggplot(postDen_euclid_quad_eff, aes(x = values, fill = Experiment)) +
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_euclid_effM_CI[1],
               y = 0,
               xend = schemaVR3_euclid_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_euclid_effM_CI[2],
               y = 0,
               xend = schemaVR3_euclid_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_euclid_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_euclid_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_euclid_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 8), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# 3 AFC accuracy
# Extracting values
Experiment <- c(rep(' Exp 1',length(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 2',length(schemaVR2_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)),
                rep(' Exp 3',length(schemaVR3_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)))
Values     <- c(schemaVR1_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR2_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating,
                schemaVR3_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)
postDen_AFC_quad_eff <- data.frame(Experiment = Experiment,
                                   values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_AFC_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating)$y)
schemaVR3_AFC_effM_CI <- as.numeric(fixef(schemaVR3_AFC_effM)[3,3:4])

# Plot
postDen_AFC_quad_plot <- ggplot(postDen_AFC_quad_eff, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_AFC_effM_CI[1],
               y = 0,
               xend = schemaVR3_AFC_effM_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_AFC_effM_CI[2],
               y = 0,
               xend = schemaVR3_AFC_effM_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_AFC_effM_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_AFC_effM_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_AFC_effM_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Quadratic term') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Save figure
combinedPlots <- plot_grid(schemaVR3_plot1,
                           schemaVR3_plot2,
                           schemaVR3_plot3,
                           postDen_recall_quad_plot,
                           postDen_euclid_quad_plot,
                           postDen_AFC_quad_plot,
                           ncol = 3,
                           nrow = 2)

save_plot("schemaVR3.png", combinedPlots,
          ncol = 3, 
          nrow = 2,
          base_height = 12/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1)

# Delete all plots to save memory
rm(combinedPlots)
rm(schemaVR3_plot1)
rm(schemaVR3_plot2)
rm(schemaVR3_plot3)
rm(postDen_recall_quad_plot)
rm(postDen_euclid_quad_plot)
rm(postDen_AFC_quad_plot)

####################################
# Recollection familiarity
####################################
# Exp 2
####################################
# Scatter plots
####################################
# Aggregating
schemaVR2_table4 <- ddply(dataSchemaVR2, 
                          c('objNum', 'objName'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          RecallNoMemory     = table(recallMemory)[1],
                          RecallRecollection = table(recallMemory)[2],
                          RecallFamiliarity  = table(recallMemory)[3],
                          RecallGuess        = table(recallMemory)[4],
                          RecallN            = sum(table(recallMemory)),
                          RecallRec_per      = (RecallRecollection/RecallN),
                          RecallFam_per      = (RecallFamiliarity/RecallN),
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN),
                          AFCFam_per         = (AFCFamiliarity/AFCN),
                          AFCGuess_per       = (AFCGuess/AFCN),
                          AFCFam_indepen     = AFCFam_per/(1-AFCRec_per))

# Recollection
# Extracting lines
x           <- range(schemaVR2_rec_AFC_effM$data$objLocTargetRating)
nDraws      <- dim(schemaVR2_rec_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR2_rec_AFC_effM_postDen[, 1] + schemaVR2_rec_AFC_effM_postDen[, 2]*xMatrix 
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit= c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR2_rec_AFC_effM)[1, 1] + fixef(schemaVR2_rec_AFC_effM)[2,1]*x 
meanFit_DF     <- data.frame(x = x,fit = meanFit)

# Plot
corrPlot1 <- ggplot(schemaVR2_rec_AFC_effM$data, aes(x = objLocTargetRating, y = AFCRec_per)) +
  geom_point() + 
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Recollection',  
       x = "Expectancy") +
  coord_cartesian(xlim =c(-1.5, 1.7), expand = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title = element_text(size = axisText))

# Familarity
x           <- range(schemaVR2_famInd_AFC_effM$data$objLocTargetRating)
nDraws      <- dim(schemaVR2_famInd_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR2_famInd_AFC_effM_postDen[, 1] + schemaVR2_famInd_AFC_effM_postDen[, 2]*xMatrix 
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit= c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR2_famInd_AFC_effM)[1, 1] + fixef(schemaVR2_famInd_AFC_effM)[2,1]*x 
meanFit_DF     <- data.frame(x = x,fit = meanFit)

# Plot
corrPlot2 <- ggplot(schemaVR2_famInd_AFC_effM$data, aes(x = objLocTargetRating, y = AFCFam_indepen)) +
  geom_point() + 
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Familiarity',  
       x = "Expectancy") +
  coord_cartesian(xlim =c(-1.5, 1.7), expand = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title = element_text(size = axisText))

####################################
# Bayesian results
####################################
# Recollection
# Extracting values
Experiment      <- c(rep(' Exp 2',length(schemaVR2_rec_AFC_effM_postDen$b_objLocTargetRating)))
Values          <- c(schemaVR2_rec_AFC_effM_postDen$b_objLocTargetRating)
postDen_rec_AFC <- data.frame(Experiment = Experiment,
                                   values = Values)

# Get values for lines
yMax <- max(density(schemaVR2_rec_AFC_effM_postDen$b_objLocTargetRating)$y)
rec_AFC_CI <- as.numeric(fixef(schemaVR2_rec_AFC_effM)[2,3:4])

# Plot
postDen_rec_AFC_plot <- ggplot(postDen_rec_AFC, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = rec_AFC_CI[1],
               y = 0,
               xend = rec_AFC_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = rec_AFC_CI[2],
               y = 0,
               xend = rec_AFC_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = rec_AFC_CI[1],
               y = yMax*1.065,
               xend = rec_AFC_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(rec_AFC_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 20), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour[2:3]) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

# Familiarity
# Extracting values
Experiment <- c(rep('Exp 2', length(schemaVR2_famInd_AFC_effM_postDen$b_objLocTargetRating)))
Values     <- c(schemaVR2_famInd_AFC_effM_postDen$b_objLocTargetRating)
postDen_famInd_AFC <- data.frame(Experiment = Experiment,
                                 values = Values)

# Get values for lines
yMax <- max(density(schemaVR2_famInd_AFC_effM_postDen$b_objLocTargetRating)$y)
famInd_AFC_CI <- as.numeric(fixef(schemaVR2_famInd_AFC_effM)[2,3:4])

# Plot
postDen_famInd_AFC_plot <- ggplot(postDen_famInd_AFC, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = famInd_AFC_CI[1],
               y = 0,
               xend = famInd_AFC_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = famInd_AFC_CI[2],
               y = 0,
               xend = famInd_AFC_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = famInd_AFC_CI[1],
               y = yMax*1.065,
               xend = famInd_AFC_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(famInd_AFC_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 18), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour[2:3]) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Save figure
combinedPlots <- plot_grid(corrPlot1, 
                           corrPlot2, 
                           postDen_rec_AFC_plot, 
                           postDen_famInd_AFC_plot,
                           ncol = 2,
                           nrow = 2)

save_plot("schemaVR2_correlation.png", combinedPlots,
          ncol = 2, 
          base_height = 24/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1.1)


####################################
# Exp 3
####################################
# Scatter plots
####################################
# Aggregating
schemaVR3_table4 <- ddply(dataSchemaVR3, 
                          c('objNum', 'objNam', 'targetLocation'), 
                          summarise,
                          objLocTargetRating = mean(objLocTargetRating, na.rm = TRUE),
                          RecallNoMemory     = table(recallMemory)[1],
                          RecallRecollection = table(recallMemory)[2],
                          RecallFamiliarity  = table(recallMemory)[3],
                          RecallGuess        = table(recallMemory)[4],
                          RecallN            = sum(table(recallMemory)),
                          RecallRec_per      = (RecallRecollection/RecallN),
                          RecallFam_per      = (RecallFamiliarity/RecallN),
                          AFCNoMemory        = table(resCon)[1], 
                          AFCRecollection    = table(resCon)[2],
                          AFCFamiliarity     = table(resCon)[3],
                          AFCGuess           = table(resCon)[4],
                          AFCN               = sum(table(resCon)),
                          AFCRec_per         = (AFCRecollection/AFCN),
                          AFCFam_per         = (AFCFamiliarity/AFCN),
                          AFCGuess_per       = (AFCGuess/AFCN),
                          AFCFam_indepen     = AFCFam_per/(1-AFCRec_per))

# Recollection
# Extracting lines
x           <- range(schemaVR3_rec_AFC_effM$data$objLocTargetRating)
nDraws      <- dim(schemaVR3_rec_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR3_rec_AFC_effM_postDen[, 1] + schemaVR3_rec_AFC_effM_postDen[, 2]*xMatrix 
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit= c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR3_rec_AFC_effM)[1, 1] + fixef(schemaVR3_rec_AFC_effM)[2,1]*x 
meanFit_DF     <- data.frame(x = x,fit = meanFit)

# Plot
corrPlot1 <- ggplot(schemaVR3_rec_AFC_effM$data, aes(x = objLocTargetRating, y = AFCRec_per)) +
  geom_point() + 
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Recollection',  
       x = "Expectancy") +
  coord_cartesian(xlim =c(-1.75, 1.6), ylim = c(-0.05, 1.05), expand = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title = element_text(size = axisText))

# Familarity
x           <- range(schemaVR3_famInd_AFC_effM$data$objLocTargetRating)
nDraws      <- dim(schemaVR3_famInd_AFC_effM_postDen)[1]
xMatrix     <- matrix(rep(x, each = nDraws), nrow = nDraws) 
drawnFit    <- schemaVR3_famInd_AFC_effM_postDen[, 1] + schemaVR3_famInd_AFC_effM_postDen[, 2]*xMatrix 
drawnFit_DF <- data.frame(id = rep(1:nDraws), x  = rep(x, each = nDraws), fit= c(drawnFit))

# Get mean fit
meanFit        <- fixef(schemaVR3_famInd_AFC_effM)[1, 1] + fixef(schemaVR3_famInd_AFC_effM)[2,1]*x 
meanFit_DF     <- data.frame(x = x,fit = meanFit)

# Plot
corrPlot2 <- ggplot(schemaVR3_famInd_AFC_effM$data, aes(x = objLocTargetRating, y = AFCFam_indepen)) +
  geom_point() + 
  geom_line(data = drawnFit_DF, aes(x = x, y = fit, group = id), alpha = .01) +
  geom_line(data = meanFit_DF, aes(x = x, y = fit)) +
  labs(y = 'Familiarity',  
       x = "Expectancy") +
  coord_cartesian(xlim =c(-1.75, 1.6), ylim = c(-0.05, 1.05), expand = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        axis.title = element_text(size = axisText))

####################################
# Bayesian results
####################################
# Recollection
# Extracting values
Experiment      <- c(rep(' Exp 2',length(schemaVR2_rec_AFC_effM_postDen$b_objLocTargetRating)),
                     rep(' Exp 3',length(schemaVR3_rec_AFC_effM_postDen$b_objLocTargetRating)))
Values          <- c(schemaVR2_rec_AFC_effM_postDen$b_objLocTargetRating,
                     schemaVR3_rec_AFC_effM_postDen$b_objLocTargetRating)
postDen_rec_AFC <- data.frame(Experiment = Experiment,
                              values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_rec_AFC_effM_postDen$b_objLocTargetRating)$y)
rec_AFC_CI <- as.numeric(fixef(schemaVR3_rec_AFC_effM)[2,3:4])

# Plot
postDen_rec_AFC_plot <- ggplot(postDen_rec_AFC, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = rec_AFC_CI[1],
               y = 0,
               xend = rec_AFC_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = rec_AFC_CI[2],
               y = 0,
               xend = rec_AFC_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = rec_AFC_CI[1],
               y = yMax*1.065,
               xend = rec_AFC_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(rec_AFC_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 20), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour[2:3]) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

# Familiarity
# Extracting values
Experiment <- c(rep('Exp 2', length(schemaVR2_famInd_AFC_effM_postDen$b_objLocTargetRating)),
                rep('Exp 3', length(schemaVR3_famInd_AFC_effM_postDen$b_objLocTargetRating)))
Values     <- c(schemaVR2_famInd_AFC_effM_postDen$b_objLocTargetRating,
                schemaVR3_famInd_AFC_effM_postDen$b_objLocTargetRating)
postDen_famInd_AFC <- data.frame(Experiment = Experiment,
                                 values = Values)

# Get values for lines
yMax <- max(density(schemaVR3_famInd_AFC_effM_postDen$b_objLocTargetRating)$y)
famInd_AFC_CI <- as.numeric(fixef(schemaVR3_famInd_AFC_effM)[2,3:4])

# Plot
postDen_famInd_AFC_plot <- ggplot(postDen_famInd_AFC, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = famInd_AFC_CI[1],
               y = 0,
               xend = famInd_AFC_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = famInd_AFC_CI[2],
               y = 0,
               xend = famInd_AFC_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = famInd_AFC_CI[1],
               y = yMax*1.065,
               xend = famInd_AFC_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(famInd_AFC_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 18), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour[2:3]) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Save figure
combinedPlots <- plot_grid(corrPlot1, 
                           corrPlot2, 
                           postDen_rec_AFC_plot, 
                           postDen_famInd_AFC_plot,
                           ncol = 2,
                           nrow = 2)

save_plot("schemaVR3_correlation.png", combinedPlots,
          ncol = 2, 
          base_height = 24/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1.1)

# Delete all plots to clear memory
rm(corrPlot1)
rm(corrPlot2)
rm(combinedPlots)
rm(postDen_rec_AFC_plot)
rm(postDen_famInd_AFC_plot)

####################################
# caculting no memory rate
####################################
table(dataSchemaVR1$recallNoMemory)[2]/sum(table(dataSchemaVR1$recallNoMemory))
table(dataSchemaVR1$resCon)[1]/sum(table(dataSchemaVR1$resCon))

table(dataSchemaVR2$recallMemory)[1]/sum(table(dataSchemaVR2$recallMemory))
table(dataSchemaVR2$resCon)[1]/sum(table(dataSchemaVR2$resCon))
# No memory rate ranges from 11 to 17 %

####################################
# Bayesian plots
# Unexpected effect
####################################
# Recall accuracy
# Extracting values
Experiment <- c(rep(' Exp 1', length(schemaVR1_recall_fullM_postDen$b_xlow)),
                rep(' Exp 2', length(schemaVR2_recall_fullM_postDen$b_xlow)),
                rep(' Exp 3', length(schemaVR3_recall_fullM_postDen$b_xlow)))
Values     <- c(schemaVR1_recall_fullM_postDen$b_xlow,
                schemaVR2_recall_fullM_postDen$b_xlow,
                schemaVR3_recall_fullM_postDen$b_xlow)
postDen_recall_line1 <- data.frame(Experiment = Experiment,
                                   values = Values)
# Get values for lines
yMax <- max(density(schemaVR3_recall_fullM_postDen$b_xlow)$y)
schemaVR3_recall_line1_CI <- as.numeric(fixef(schemaVR3_recall_fullM)[2,3:4])

# Plot
postDen_recall_line1_plot <- ggplot(postDen_recall_line1, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_recall_line1_CI[1],
               y = 0,
               xend = schemaVR3_recall_line1_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_recall_line1_CI[2],
               y = 0,
               xend = schemaVR3_recall_line1_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_recall_line1_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_recall_line1_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_recall_line1_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 2.5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Euclidean distance
# Extracting values
Experiment <- c(rep('Exp 1', length(schemaVR1_euclid_fullM_postDen$b_xlow)),
                rep('Exp 2', length(schemaVR2_euclid_fullM_postDen$b_xlow)),
                rep('Exp 3', length(schemaVR3_euclid_fullM_postDen$b_xlow)))
Values     <- c(schemaVR1_euclid_fullM_postDen$b_xlow,
                schemaVR2_euclid_fullM_postDen$b_xlow,
                schemaVR3_euclid_fullM_postDen$b_xlow)
postDen_euclid_line1 <- data.frame(Experiment = Experiment,
                                   values = Values)
# Get values for lines
yMax <- max(density(schemaVR3_euclid_fullM_postDen$b_xlow)$y)
schemaVR3_euclid_line1_CI <- as.numeric(fixef(schemaVR3_euclid_fullM)[2,3:4])

# Plot
postDen_euclid_line1_plot <- ggplot(postDen_euclid_line1, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_euclid_line1_CI[1],
               y = 0,
               xend = schemaVR3_euclid_line1_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_euclid_line1_CI[2],
               y = 0,
               xend = schemaVR3_euclid_line1_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_euclid_line1_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_euclid_line1_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_euclid_line1_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 3.5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# 3 AFC accuracy
#  Extracting values
Experiment <- c(rep('Exp 1', length(schemaVR1_AFC_fullM_postDen$b_xlow)),
                rep('Exp 2', length(schemaVR2_AFC_fullM_postDen$b_xlow)),
                rep('Exp 3', length(schemaVR3_AFC_fullM_postDen$b_xlow)))
Values     <- c(schemaVR1_AFC_fullM_postDen$b_xlow,
                schemaVR2_AFC_fullM_postDen$b_xlow,
                schemaVR3_AFC_fullM_postDen$b_xlow)
postDen_AFC_line1 <- data.frame(Experiment = Experiment,
                                   values = Values)
# Get values for lines
yMax <- max(density(schemaVR3_AFC_fullM_postDen$b_xlow)$y)
schemaVR3_AFC_line1_CI <- as.numeric(fixef(schemaVR3_AFC_fullM)[2,3:4])

# Plot
postDen_AFC_line1_plot <- ggplot(postDen_AFC_line1, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_AFC_line1_CI[1],
               y = 0,
               xend = schemaVR3_AFC_line1_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_AFC_line1_CI[2],
               y = 0,
               xend = schemaVR3_AFC_line1_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_AFC_line1_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_AFC_line1_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_AFC_line1_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 2.5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Putting plots together
combinedPlots <- plot_grid(postDen_recall_line1_plot,
                           postDen_euclid_line1_plot,
                           postDen_AFC_line1_plot, 
                           ncol = 3)

save_plot("bayesianLine1.png", combinedPlots,
          ncol = 3, 
          base_height = 12/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1.3)

####################################
# Expected effect
####################################
# Recall accuracy
# Extracting values
Experiment <- c(rep(' Exp 1', length(schemaVR1_recall_fullM_postDen$b_xhigh)),
                rep(' Exp 2', length(schemaVR2_recall_fullM_postDen$b_xhigh)),
                rep(' Exp 3', length(schemaVR3_recall_fullM_postDen$b_xhigh)))
Values     <- c(schemaVR1_recall_fullM_postDen$b_xhigh,
                schemaVR2_recall_fullM_postDen$b_xhigh,
                schemaVR3_recall_fullM_postDen$b_xhigh)
postDen_recall_line2 <- data.frame(Experiment = Experiment,
                                   values = Values)
# Get values for lines
yMax <- max(density(schemaVR3_recall_fullM_postDen$b_xhigh)$y)
schemaVR3_recall_line2_CI <- as.numeric(fixef(schemaVR3_recall_fullM)[3,3:4])

# Plot
postDen_recall_line2_plot <- ggplot(postDen_recall_line2, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_recall_line2_CI[1],
               y = 0,
               xend = schemaVR3_recall_line2_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_recall_line2_CI[2],
               y = 0,
               xend = schemaVR3_recall_line2_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_recall_line2_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_recall_line2_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_recall_line2_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 3), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.title = element_text(size = axisText),
        legend.text = element_text(size = legendText))

####################################
# Euclidean distance
# Extracting values
Experiment <- c(rep('Exp 1', length(schemaVR1_euclid_fullM_postDen$b_xhigh)),
                rep('Exp 2', length(schemaVR2_euclid_fullM_postDen$b_xhigh)),
                rep('Exp 3', length(schemaVR3_euclid_fullM_postDen$b_xhigh)))
Values     <- c(schemaVR1_euclid_fullM_postDen$b_xhigh,
                schemaVR2_euclid_fullM_postDen$b_xhigh,
                schemaVR3_euclid_fullM_postDen$b_xhigh)
postDen_euclid_line2 <- data.frame(Experiment = Experiment,
                                   values = Values)
# Get values for lines
yMax <- max(density(schemaVR3_euclid_fullM_postDen$b_xhigh)$y)
schemaVR3_euclid_line2_CI <- as.numeric(fixef(schemaVR3_euclid_fullM)[3,3:4])

# Plot
postDen_euclid_line2_plot <- ggplot(postDen_euclid_line2, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_euclid_line2_CI[1],
               y = 0,
               xend = schemaVR3_euclid_line2_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_euclid_line2_CI[2],
               y = 0,
               xend = schemaVR3_euclid_line2_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_euclid_line2_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_euclid_line2_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_euclid_line2_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 5), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# 3 AFC accuracy
# Extracting values
Experiment <- c(rep('Exp 1', length(schemaVR1_AFC_fullM_postDen$b_xhigh)),
                rep('Exp 2', length(schemaVR2_AFC_fullM_postDen$b_xhigh)),
                rep('Exp 3', length(schemaVR3_AFC_fullM_postDen$b_xhigh)))
Values     <- c(schemaVR1_AFC_fullM_postDen$b_xhigh,
                schemaVR2_AFC_fullM_postDen$b_xhigh,
                schemaVR3_AFC_fullM_postDen$b_xhigh)
postDen_AFC_line2 <- data.frame(Experiment = Experiment,
                                values = Values)
# Get values for lines
yMax <- max(density(schemaVR3_AFC_fullM_postDen$b_xhigh)$y)
schemaVR3_AFC_line2_CI <- as.numeric(fixef(schemaVR3_AFC_fullM)[3,3:4])

# Plot
postDen_AFC_line2_plot <- ggplot(postDen_AFC_line2, aes(x = values, fill = Experiment)) + 
  geom_vline(xintercept = c(0), linetype ="dotted", size = 1) +
  geom_density(alpha = 0.5) + 
  geom_segment(x = schemaVR3_AFC_line2_CI[1],
               y = 0,
               xend = schemaVR3_AFC_line2_CI[1],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_AFC_line2_CI[2],
               y = 0,
               xend = schemaVR3_AFC_line2_CI[2],
               yend = yMax,
               colour = 'black') +
  geom_segment(x = schemaVR3_AFC_line2_CI[1],
               y = yMax*1.065,
               xend = schemaVR3_AFC_line2_CI[2],
               yend = yMax*1.065,
               colour = 'black',
               arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = mean(schemaVR3_AFC_line2_CI), y = yMax*1.1, label = "95 % CI") +
  coord_cartesian(ylim = c(0, 3), expand = FALSE) +
  labs(y = 'Density',
       x = 'Slope') +
  scale_fill_manual(values = expColour) +
  theme(plot.margin = unit(c(1,1,1,1), "lines"),
        legend.position = "none",
        axis.title = element_text(size = axisText))

####################################
# Putting plots together
combinedPlots <- plot_grid(postDen_recall_line2_plot,
                           postDen_euclid_line2_plot,
                           postDen_AFC_line2_plot, 
                           ncol = 3)

save_plot("bayesianLine2.png", combinedPlots,
          ncol = 3, 
          base_height = 12/cm(1),
          base_width = 12/cm(1),
          base_aspect_ratio = 1.3)