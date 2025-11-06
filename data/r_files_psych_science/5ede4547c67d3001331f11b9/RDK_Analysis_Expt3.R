library(dplyr)
library(reshape)
library(ggplot2)
library(scales)
library(gridExtra)
library(psych)
library(grid)
library(bruceR)
library(lsr)

# Lines 18 to 34: read data
# Lines 37 to 150: pre-processing (identical across 4 experiments)
# Lines 154 to 220: statistical analyses
# Lines 224 to the end: figures



################ Set the current directory to the folder of Experiment 3.
YOUR_DIRECTORY <- ""
setwd(YOUR_DIRECTORY)

################ Load raw data
mydata <- read.delim("expt3.txt", stringsAsFactors = F)

################ Load participant info
ParticipantInfo <- read.csv("ParticipantInfo_expt3.csv", stringsAsFactors = F)
ParticipantInfo$Participant <- sub(".edf", "", ParticipantInfo$Participant)

describe(ParticipantInfo)
describeBy(ParticipantInfo, ParticipantInfo$Gender)

################ Load Trial information

TrialInfo <- read.csv("TrialInfo_expt3.csv", stringsAsFactors = F); TrialInfo <- TrialInfo[1:3]


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

######## mark the correct coherence label & participant information
mydata <- select(mydata, -Coherence)
mydata <- filter(mydata, Trial > 0)

mydata <- merge(mydata, TrialInfo, all.x = T)
mydata <- merge(mydata, ParticipantInfo, all.x = T)

write.csv(mydata, "Processed_expt3.csv", row.names = F)

################ Pre-processing ends



################ Analyses start

######### Calculate averaged horizontal smooth pursuit velocity across each trial.
# The motion presentation starts at 309th samples after music playing and ends at 1308th sample with a duration of 1000 samples (2 seconds).
# To represent motion perception, the calculation focuses on the eye movement at the speed close to the speed of motion stimulus (8 deg/s).
mydata <- read.csv("Processed_expt3.csv")

Agg <- cast(mydata, Participant + Coherence + Trial + MotionDirection ~., function(x){mean(x, na.rm = T)}, value = "VELOCITY_X",
            subset = Timestamp >= 309 & Timestamp <= 1308 & VelocityFilter == 1)

# Average the averaged eye movement velocity across the first 24 test trials. The test trials start at the 25th trials (the fist 24 trials are learning trials).
Agg <- cast(Agg, Participant + Coherence + MotionDirection ~., function(x){mean(x, na.rm = T)}, value = "(all)",
            subset = Trial <= 48 &  Trial > 24)

names(Agg)[4] <- "y"
Agg$Coherence <- as.factor(Agg$Coherence)
Agg$MotionDirection <- as.factor(Agg$MotionDirection)


######### Descriptive stats
# Means and SDs
describeBy(data = Agg, y ~ Coherence + MotionDirection)
# CIs
library("Rmisc")
by(Agg, list(Agg$Coherence, Agg$MotionDirection), function(x){CI(x$y)})


######### Statistical analyses
# paired-sample t-tests, examining whether participants' smooth pursuit eye movement modulated by left and right cues and by leftward and rightward motion signals. 
Agg.test <- cast(Agg, Participant + Coherence ~ MotionDirection, mean, value = "y")

by(Agg.test, Agg.test$Coherence, function(x){t.test(x$`0`, x$`180`, paired = TRUE)})

# Cohen's d
by(Agg.test, Agg.test$Coherence, function(x){cohensD(x$`0`, x$`180`, method = "paired")})


# One-sample t-tests, examining whether participants' average eye movement velocity is different from 0.
Agg.test <- cast(Agg, Participant + Coherence + MotionDirection ~., mean, value = "y")

names(Agg.test)[4] <- "y"

by(Agg.test, list(Agg.test$Coherence, Agg.test$MotionDirection), function(x){t.test(x$y, mu = 0)})

# Cohen's d
by(Agg.test, list(Agg.test$Coherence, Agg.test$MotionDirection), function(x){cohensD(x$y, mu = 0)})


# Compare Experiment 3 with Experiment 1 with Mixed ANOVA
E <- read.csv("Expt_combined.csv")

E13 <- filter(E, Expt %in% c("Expt1", "Expt3"))

E13$Coherence <- as.factor(E13$Coherence)

summary(aov(y ~ Coherence * Expt + Error(Participant/Coherence), E13))

MANOVA(data = E13, subID = "Participant", dv = "y", between="Expt", within = c("Coherence"))

by(E13, E12$Coherence, function(x){t.test(x$y ~ x$Expt, var.equal = T)})

p.adjust(c(0.8397, 0.004501), "bonferroni")

# Cohen's d
by(E13, E13$Coherence, function(x){cohensD(x[x$Expt == "Expt1", ]$y,x[x$Expt == "Expt3", ]$y)})

################ Analyses end



######### Figures
theme_set(theme_bw() + theme(text = element_text(family = 'Helvetica')))

Agg.a <- cast(Agg, Coherence + MotionDirection ~., mean_se, value = "y")

Sig <- cast(Agg, Coherence + MotionDirection ~., mean_se, value = "y",
            subset = MotionDirection == 0 & Coherence == .6)
names(Sig)[5] <- "YLoc"
Sig$X <- 1

bracket <- data.frame(X = c(1 - .9/4, 1 - .9/4, 1 + .9/4, 1 + .9/4),
                      Y = c(5.6, 6.2, 6.2, 5.6) + .9,
                      MotionDirection = "0", Coherence = "0")

Agg.a$Coherence <- factor(Agg.a$Coherence, levels = c("0.6", "0"))
Agg.a$MotionDirection <- factor(Agg.a$MotionDirection, levels = c(180, 0))

quartz("", 3.5, 3.5)
TwoDirection <- ggplot(Agg.a, aes(Coherence, y, fill = MotionDirection)) + 
  geom_bar(stat = "identity", position = "dodge", aes(alpha = Coherence)) + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.3, size = 1, position = position_dodge(width = 0.9)) +
  geom_text(data = Sig, aes(x = X, y = YLoc + 1.4), label = "*", size = 10, check_overlap = T) +
  geom_path(data = bracket, aes(x = X, y = Y), size = 1) +
  scale_x_discrete("Directional motion availability", labels = c("60%", "0%")) +
  scale_y_continuous("Eye movement velocity (deg/s)", limits = c(-8, 8)) +
  scale_fill_manual("Cued direction", values = c("#FFB000", "#648FFF"), labels = c("Left", "Right")) +
  scale_alpha_manual("", values = c(1,.7), guide = "none") +
  theme(legend.position=c(.75, 0.15), legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(colour = "transparent", fill = "white")) + 
  theme(axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12, face = "bold")) + 
  theme(panel.grid.minor = element_line(color = "white"), panel.grid.major = element_line(color = "white"))

TwoDirection


#################
# aggregate two directions
Agg <- cast(mydata, Participant + Coherence + Trial + MotionDirection ~., function(x){mean(x, na.rm = T)}, value = "VELOCITY_X",
            subset = Timestamp >= 309 & Timestamp <= 1308 & VelocityFilter == 1)

Agg[Agg$MotionDirection == 180,]$`(all)` <- Agg[Agg$MotionDirection == 180,]$`(all)` * -1

Agg <- cast(Agg, Participant + Coherence ~., function(x){mean(x, na.rm = T)}, value = "(all)",
            subset = Trial <= 48 & Trial > 24)

names(Agg)[3] <- "y"
Agg$Coherence <- as.factor(Agg$Coherence)

Agg.a <- cast(Agg, Coherence ~., mean_se, value = "y")

Sig <- cast(Agg, Coherence ~., mean_se, value = "y",
            subset = Coherence == 0.6)
names(Sig)[4] <- "YLoc"
Sig$X <- 1

Agg.a$Coherence <- factor(Agg.a$Coherence, levels = c("0.6", "0"))

quartz("", 3.5, 3.5)
Combined <- ggplot(Agg.a, aes(x = Coherence, y)) + 
  geom_bar(stat = "identity", width = .5, fill = "grey60", aes(alpha = Coherence)) + 
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, size = 1) +
  geom_text(data = Sig, aes(x = X, y = YLoc + .6), label = "*", size = 10, check_overlap = T) +
  scale_x_discrete("Directional motion availability", labels = c("60%", "0%")) +
  scale_y_continuous("Eye movement velocity (deg/s)", limits = c(-.5, 7)) +
  scale_alpha_manual("", values = c(1,.7), guide = "none") +
  geom_abline(slope = 0, intercept = 0, linetype = 2, size = 1) +
  theme(legend.position=c(.75, 0.15), legend.text = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"), legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(colour = "transparent", fill = "white")) + 
  theme(axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12, face = "bold")) + 
  theme(panel.grid.minor = element_line(color = "white"), panel.grid.major = element_line(color = "white"))

Combined

# combine two figures
quartz("", 7, 3.5)

Ml <- grid.arrange(TwoDirection, Combined, widths = c(1, 1), ncol = 2)

