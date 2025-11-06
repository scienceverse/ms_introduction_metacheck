library(dplyr)
library(reshape)
library(ggplot2)
library(scales)
library(gridExtra)
library(psych)
library(grid)
library(bruceR)
library(lsr)

# Lines 18 to 30: read data
# Lines 33 to 147: pre-processing
# Lines 151 to 263: statistical analyses
# Lines 267 to the end: figures



################ Set the current directory to the folder of Experiment 3.
YOUR_DIRECTORY <- ""
setwd(YOUR_DIRECTORY)

################ Load raw data
mydata <- read.delim("expt2.txt", stringsAsFactors = F)

################ Load participant info
ParticipantInfo <- read.csv("ParticipantInfo_expt2.csv", stringsAsFactors = F)
ParticipantInfo$Participant <- sub(".edf", "", ParticipantInfo$Participant)

describe(ParticipantInfo)
describeBy(ParticipantInfo, ParticipantInfo$Gender)


################ Pre-processing starts

mydata$HTARGET_X <- as.numeric(mydata$HTARGET_X)
mydata$HTARGET_Y <- as.numeric(mydata$HTARGET_Y)

mydata$LEFT_GAZE_X <- as.numeric(mydata$LEFT_GAZE_X)
mydata$LEFT_GAZE_Y <- as.numeric(mydata$LEFT_GAZE_Y)

mydata$RIGHT_GAZE_X <- as.numeric(mydata$RIGHT_GAZE_X)
mydata$RIGHT_GAZE_Y <- as.numeric(mydata$RIGHT_GAZE_Y)

mydata$LEFT_ACCELERATION_X <- as.numeric(mydata$LEFT_ACCELERATION_X)
mydata$LEFT_ACCELERATION_Y <- as.numeric(mydata$LEFT_ACCELERATION_Y)

mydata$LEFT_VELOCITY_X <- as.numeric(mydata$LEFT_VELOCITY_X)
mydata$LEFT_VELOCITY_Y <- as.numeric(mydata$LEFT_VELOCITY_Y)

mydata$RIGHT_ACCELERATION_X <- as.numeric(mydata$RIGHT_ACCELERATION_X)
mydata$RIGHT_ACCELERATION_Y <- as.numeric(mydata$RIGHT_ACCELERATION_Y)

mydata$RIGHT_VELOCITY_X <- as.numeric(mydata$RIGHT_VELOCITY_X)
mydata$RIGHT_VELOCITY_Y <- as.numeric(mydata$RIGHT_VELOCITY_Y)

################################
mydata <- select(mydata, RECORDING_SESSION_LABEL, LEFT_GAZE_X, LEFT_GAZE_Y, RIGHT_GAZE_X, RIGHT_GAZE_Y, 
                 LEFT_VELOCITY_X, LEFT_VELOCITY_Y, RIGHT_VELOCITY_X, RIGHT_VELOCITY_Y,
                 SAMPLE_MESSAGE, TIMESTAMP, TRIAL_INDEX, TRIAL_LABEL, MotionDirection, Coherence, LearningTest)

mydata$TIMESTAMP <- 1:nrow(mydata)

SoundTime <- cast(mydata, SAMPLE_MESSAGE + TIMESTAMP ~., length, value = "RIGHT_GAZE_X", subset = SAMPLE_MESSAGE == "SoundPlay")

SoundTime$TIMESTAMP <- SoundTime$TIMESTAMP - 200 # trial starts at least 400 ms before music play.

names(SoundTime)[2:3] <- c("SoundsTime", "TRIAL_INDEX_new")

SoundTime$TRIAL_INDEX_new <- 1:nrow(SoundTime)

SoundTime <- select(SoundTime, -SAMPLE_MESSAGE)

k <- findInterval(mydata$TIMESTAMP, SoundTime$SoundsTime)

mydata$TRIAL_INDEX_new <- k


# re-index the samples accroding to each TRIAL_INDEX_new
SoundTime[nrow(SoundTime) + 1, ] <- c(0, 0)

mydata <- merge(mydata, SoundTime, all.x = T)

mydata$TIMESTAMP <- mydata$TIMESTAMP - mydata$SoundsTime - 200


mydata <- group_by(mydata, mydata$RECORDING_SESSION_LABEL)

mydata <- mutate(mydata, TRIAL_INDEX = TRIAL_INDEX_new - min(TRIAL_INDEX_new))

mydata <- ungroup(mydata)


mydata$LearningTest <- "Learning"
mydata[mydata$TRIAL_INDEX > 24,]$LearningTest <- "Testing"

mydata$TrialDuration <- 2

mydata <- filter(mydata, TIMESTAMP < TrialDuration * 500 + 300)

# Mark the recorded eye

EyeUse <- cast(mydata, RECORDING_SESSION_LABEL ~., function(x){all(is.na(x))}, value = "LEFT_GAZE_X")
names(EyeUse)[2] <- "RightEyeUsed"

mydata <- merge(mydata, EyeUse, all.x = T)

# Move gaze data from the left or right eye to one column

mydata[mydata$RightEyeUsed,]$LEFT_GAZE_X <- mydata[mydata$RightEyeUsed,]$RIGHT_GAZE_X
mydata[mydata$RightEyeUsed,]$LEFT_GAZE_Y <- mydata[mydata$RightEyeUsed,]$RIGHT_GAZE_Y

mydata <- select(mydata, -RIGHT_GAZE_X, -RIGHT_GAZE_Y)

names(mydata)[3:4] <- c("GAZE_X", "GAZE_Y")

names(mydata)[c(1,2,10,11)] <- c("Participant", "Trial_new", "Timestamp","Trial")

# Move velocity data from the left or right eye to one column

mydata[mydata$RightEyeUsed,]$LEFT_VELOCITY_X <- mydata[mydata$RightEyeUsed,]$RIGHT_VELOCITY_X
mydata[mydata$RightEyeUsed,]$LEFT_VELOCITY_Y <- mydata[mydata$RightEyeUsed,]$RIGHT_VELOCITY_Y

mydata <- select(mydata, -RIGHT_VELOCITY_X, -RIGHT_VELOCITY_Y)

names(mydata)[5:6] <- c("VELOCITY_X", "VELOCITY_Y")

# Velocity filter (7 to 9 ded/s), to filter out the data with abrupt eye movement, such as saccade

mydata$VelocityFilter <- 0

k <- abs(mydata$VELOCITY_X) >= 7 & abs(mydata$VELOCITY_X) <= 9

k[is.na(k)] <- FALSE

mydata[k == TRUE, ]$VelocityFilter <- 1


######## mark participant information
mydata <- merge(mydata, ParticipantInfo, all.x = T)

# Participant 81_yWa & 81_6vZ were the same infant. The program crashed in the middle of the study. By resuming the experiment, the program assigned a different Participant ID to this participant.
# So, we used one ID to represented this participant.
mydata$Participant <- sub("81_yWa", "81_6vZ", mydata$Participant)

write.csv(mydata, "Processed_expt2.csv", row.names = F)

################ Pre-processing ends



################ Analyses start

######### Calculate averaged horizontal smooth pursuit velocity across each trial.
# The motion presentation starts at 309th samples after music playing and ends at 1308th sample with a duration of 1000 samples (2 seconds).
# To represent motion perception, the calculation focuses on the eye movement at the speed close to the speed of motion stimulus (8 deg/s).
# The eye movement in the leftward and rightward motion trials were combined.
mydata <- read.csv("processed_expt2.csv", stringsAsFactors = F)

Agg <- cast(mydata, Participant + Coherence + Trial + MotionDirection ~., function(x){mean(x, na.rm = T)}, value = "VELOCITY_X",
            subset = Timestamp >= 309 & Timestamp <= 1308 & VelocityFilter == 1 &
              Trial > 0 & Trial <= 42)

Agg[Agg$MotionDirection == 180 & Agg$Coherence > 0,]$`(all)` <- Agg[Agg$MotionDirection == 180 & Agg$Coherence > 0,]$`(all)` * -1

Agg <- cast(Agg, Participant + Coherence ~., function(x){mean(x, na.rm = T)}, value = "(all)")

names(Agg)[3] <- "y"
Agg$Coherence <- as.factor(Agg$Coherence)


######### Descriptive stats
# Means and SDs
describeBy(data = Agg, y ~ Coherence + MotionDirection)
# CIs
library("Rmisc")
by(Agg, list(Agg$Coherence, Agg$MotionDirection), function(x){CI(x$y)})


######### Statistical analyses
# Repeated-measures ANOVA to examine the horizontal eye movement as the function of directional motion availability
MANOVA(data = Agg, subID = "Participant", dv = "y", within = c("Coherence"))

# One-sample t-tests to examine whether the horizontal eye movement induced by different level of motion signals was different from 0 deg/s.
by(Agg, Agg$Coherence, function(x){t.test(x$y, mu = 0, var.equal = T)})

# Cohen's d
by(Agg, Agg$Coherence, function(x){cohensD(x$y, mu = 0)})


# Compare Experiment 2 with Experiment 1 with Mixed ANOVA
E <- read.csv("Expt_combined.csv")

E12 <- filter(E, Expt %in% c("Expt1", "Expt2"))

E12$Coherence <- as.factor(E12$Coherence)

summary(aov(y ~ Coherence * Expt + Error(Participant/Coherence), E12))

MANOVA(data = E12, subID = "Participant", dv = "y", within = c("Coherence"), between = "Expt")

by(E12, E12$Coherence, function(x){t.test(x$y ~ x$Expt, var.equal = T)})

p.adjust(c(0.678, 0.01003), "bonferroni")

# Cohen's d
by(E12, E12$Coherence, function(x){cohensD(x[x$Expt == "Expt1", ]$y,x[x$Expt == "Expt2", ]$y)})

# Independent t-tests to compare the averaged eye movement velocity induced by top-down signal in Experiment 1 with those induced by the perception of motion signals at different availability levels in Experiment 2.
E1 <- filter(E, Expt == "Expt1")
Agg$Expt <- "Expt2"

Agg.test <- rbind(E1[E1$Coherence == 0,], Agg)

# 0% (Expt1) vs 0% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 0, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 0, ]$y)

# 0% (Expt1) vs 5% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 5, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 5, ]$y)

# 0% (Expt1) vs 10% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 10, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 10, ]$y)

# 0% (Expt1) vs 20% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 20, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 20, ]$y)

# 0% (Expt1) vs 30% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 30, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 30, ]$y)

# 0% (Expt1) vs 40% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 40, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 40, ]$y)

# 0% (Expt1) vs 60% (Expt2)
t.test(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
       Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 60, ]$y, var.equal = T)

cohensD(Agg.test[Agg.test$Expt == "Expt1" & Agg.test$Coherence == 0, ]$y,
        Agg.test[Agg.test$Expt == "Expt2" & Agg.test$Coherence == 60, ]$y)

################ Analyses end



######### Figure
theme_set(theme_bw() + theme(text = element_text(family = 'Helvetica')))

Agg <- cast(mydata, Participant + Coherence + Trial + MotionDirection ~., function(x){mean(x, na.rm = T)}, value = "VELOCITY_X",
            subset = Timestamp >= 309 & Timestamp <= 1308 & VelocityFilter == 1 &
              Trial > 0 & Trial <= 42)

Agg[Agg$MotionDirection == 180 & Agg$Coherence > 0,]$`(all)` <- Agg[Agg$MotionDirection == 180 & Agg$Coherence > 0,]$`(all)` * -1

Agg <- cast(Agg, Participant + Coherence ~., function(x){mean(x, na.rm = T)}, value = "(all)")

names(Agg)[3] <- "y"
Agg$Coherence <- as.factor(Agg$Coherence)
Agg$Expt <- "Expt3"

Agg.a <- cast(Agg, Coherence + Expt ~., mean_se, value = "y")


E <- read.csv("Expt_combined.csv")

E1 <- filter(E, Expt == "Expt1")
E1$Coherence <- E1$Coherence * 100
Agg.a1 <- cast(E1, Coherence + Expt ~., mean_se, value = "y")
Agg.a1 <- rbind(Agg.a, Agg.a1)
Agg.a1$Coherence <- paste(Agg.a1$Coherence, "%", sep = "")
Agg.a1$Coherence <- factor(Agg.a1$Coherence, levels = c("0%", "5%", "10%", "20%", "30%", "40%", "60%"))

Agg.a1$Sig <- ""
Agg.a1[8, ]$Sig <- "*"
  
Agg.a1$FC <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594", "#ed462f", "#ed462f")

bracket <- cast(Agg.a1, Coherence + Expt + FC ~., max, value = "ymax")
bracket <- bracket[c(1), ]

bracket <- rbind(bracket, bracket, bracket, bracket)
# bracket <- arrange(bracket, Age, Congruency, Face)

bracket$X <- c(1 - .7/4, 1 - .7/4, 1 + .7/4, 1 + .7/4)

bracket$Y <- rep(c(.25, .25 + .15, .25 + .15, .25), 1)
bracket$Y <- bracket$`(all)` + bracket$Y

bracket1 <- bracket[1:4,]

bracket2 <- bracket1
bracket2$X <- c(1 - .7/4, 1 - .7/4, 2, 2)
bracket2$Y <- bracket2$Y + 1


bracket <- cast(Agg.a1, Coherence + Expt + FC ~., max, value = "ymax")
bracket3 <- bracket[c(1), ]

bracket3 <- rbind(bracket3, bracket3, bracket3, bracket3)
bracket3$X <- c(1 - .7/4, 1 - .7/4, 3, 3)
bracket3$Y <- rep(c(.25, .25 + .15, .25 + .15, .25), 1)
bracket3$Y <- bracket3$`(all)` + bracket3$Y

bracket4 <- bracket[c(5), ]
bracket4 <- rbind(bracket4, bracket4, bracket4, bracket4)
bracket4$X <- c(1 - .7/4, 1 - .7/4, 4, 4)
bracket4$Y <- rep(c(.25, .25 + .15, .25 + .15, .25), 1)
bracket4$Y <- bracket4$`(all)` + bracket4$Y
bracket4$Y <- bracket4$Y + .6

bracket5 <- bracket[c(9), ]
bracket5 <- rbind(bracket5, bracket5, bracket5, bracket5)
bracket5$X <- c(7 - .7/4, 7 - .7/4, 7 + .7/4, 7 + .7/4)
bracket5$Y <- rep(c(.25, .25 + .15, .25 + .15, .25), 1)
bracket5$Y <- bracket5$`(all)` + bracket5$Y

quartz("", 5, 3)
ggplot(Agg.a1[1:9, ], aes(x = Coherence, y, fill = Expt)) + 
  geom_bar(stat = "identity", width = .7, position = position_dodge2(padding = 0, preserve = "single")) + #aes(alpha = Coherence), 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .7, size = 0.6,
                position = position_dodge2(width = 0.7, padding = .7, preserve = "single")) +
  scale_x_discrete("Directional motion availability") + 
  scale_y_continuous("Eye movement velocity (deg/s)", limits = c(-.5, 6)) +
  # scale_fill_manual("", values = c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#ed462f", "#eff3ff")) +
  scale_fill_manual("", values = c("#ed462f", "#40B0A6"),
                    labels = c("Audio-visual predictive cue with motion display (Expt 1)",
                               "No predictive cue with motion display (Expt 2)")) +
  scale_alpha_manual(values = c(0, .05, .1, .2, .3, .4, .6) / 6 * 5 + .5, guide = 'none') + 
  geom_path(data = bracket1, aes(x = X, y = Y), size = .6) +
  geom_path(data = bracket2, aes(x = X, y = Y, group = 1), size = .6) +
  annotate("text", label = "*", x = 1, y = max(bracket1$Y) + 0.2, size = 8) +
  annotate("text", label = "*", x = 1.5 - .7/8, y = max(bracket2$Y) + .2, size = 8) +
  # geom_path(data = bracket3, aes(x = X, y = Y), size = .6) +
  # geom_path(data = bracket4, aes(x = X, y = Y, group = 1), size = .6) +
  # annotate("text", label = "ns", x = 2 - .7/8, y = max(bracket3$Y) + 0.4, size = 4) +
  # annotate("text", label = "ns", x = 2.5 - .7/8, y = max(bracket4$Y) + .4, size = 4) +
  geom_path(data = bracket5, aes(x = X, y = Y), size = .6) +
  annotate("text", label = "ns", x = 7, y = max(bracket5$Y) + .3, size = 4) +
  geom_abline(slope = 0, intercept = 0, linetype = 2, size = 1) +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12, face = "bold")) + 
  theme(panel.grid.minor = element_line(color = "white"),
        panel.grid.major = element_line(color = "white")) +
  theme(legend.position=c(.34, 0.93), 
        # legend.position = "none",
        legend.title = element_blank(),
        # legend.justification = c("left", "top"),
        # legend.box.just = "left",
        # legend.direction="vertical",
        legend.text = element_text(color = "black", size = 8),
        legend.key.size = unit(0.35, "cm"),
        legend.background = element_rect(fill = "transparent", colour = "transparent"))


