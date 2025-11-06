
## Patterns of Change IV.
## Script version date: January 18th, 2022

####################################################################################################################################

## Runs supplemental analyses using segmented regressions to zero in on rises/falls in change in implicit and explicit attitudes

####################################################################################################################################

# Set wd
# setwd("**")

# Load data 
load("timeseries_NEWDATA.RData")
load("timeseries_OLDDATA.RData")
load("TSvectors_NEWDATA.RData")
load("TSvectors_OLDDATA.RData")

# Load packages
if (!require(segmented)) {install.packages("segmented", dependencies = TRUE); require(segmented)}
if (!require(forecast)) {install.packages("forecast", dependencies = TRUE); require(forecast)}


## IMPLICIT ATTITUDES --------------------------------------------------------------------------------------------------------
## Combine all data
skin.WtsALL <- ts(c(skin.Wts, skin.WtsNEW), start = c(2007, 1), frequency = 12)
race.WtsALL <- ts(c(race.Wts, race.WtsNEW), start = c(2007, 1), frequency = 12)
dis.WtsALL <- ts(c(dis.Wts, dis.WtsNEW), start = c(2007, 1), frequency = 12)
weight.WtsALL <- ts(c(weight.Wts, weight.WtsNEW), start = c(2010, 4), frequency = 12)
sex.WtsALL <- ts(c(sex.Wts, sex.WtsNEW), start = c(2007, 1), frequency = 12)
age.WtsALL <- ts(c(age.Wts, age.WtsNEW), start = c(2007, 1), frequency = 12)

## SEXUALITY ----
## Create decomposed trend lines
plot(sex.WtsALL) # visually inspect
decomp.sexALL <- as.vector(decompose(sex.WtsALL)$trend)
plot(decomp.sexALL, type = "l")

monthpreds <- 1:168

## Fit base model
sexdecomp.lm <- lm(decomp.sexALL ~ monthpreds)
davies.test(sexdecomp.lm, seg.Z = ~ monthpreds, alternative = "two.sided")
# says that there is indeed a need for a segmented regression, if anything it picks the first breakpoint (~ month 76)

## Completely bottom-up, only specifying that the model can have three or five breaks.
sexdecomp.seglm5 <- segmented(sexdecomp.lm, 
                               seg.Z = ~ monthpreds,
                               npsi = list(monthpreds = 5))
sexdecomp.seglm3 <- segmented(sexdecomp.lm, 
                               seg.Z = ~ monthpreds,
                               npsi = list(monthpreds = 3))
anova(sexdecomp.seglm3, sexdecomp.seglm5) # 5 breaks adds significant explanatory value.
summary(sexdecomp.seglm3) # R2 = 98%
summary(sexdecomp.seglm5) # R2 = 99%
slope(sexdecomp.seglm3)
slope(sexdecomp.seglm5)
# all slopes are negative; particularly rapid decrease in chunk 3

confint.segmented(sexdecomp.seglm3)
confint.segmented(sexdecomp.seglm5)

plot(decomp.sexALL, type = "l", main = "5 breakpoint model")
abline(v = c(25, 65, 71, 112, 124), lty = 2, col = "coral")
plot(decomp.sexALL, type = "l", main = "3 breakpoint model")
abline(v = c(62, 113, 124), lty = 2, col = "cadetblue")

# inspection of the actual data shows the 5 breakpoint model is likely an overfit, so go with 3 point model
decomp.sexALL <- ts(decomp.sexALL, start = c(2007,1), frequency = 12)
decomp.sexALL[113] - decomp.sexALL[124] # raw change
(decomp.sexALL[113] - decomp.sexALL[124])/decomp.sexALL[113] # p change

## Visualize with slopes overlayed on raw data
pdf("sex_segmented3.pdf", height = 5, width = 7)
plot(y = sexdecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "SEXUALITY")
lines(y = decomp.sexALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.5, by = 0.05), labels = seq(0.1, 0.5, by = 0.05))
abline(v = c(62, 113, 124), lty = 2, col = "cadetblue") 
box()
par(new = TRUE)
plot(sex.WtsALL, ylim = c(0.1, 0.5), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)
legend("bottomleft", c("Raw data", "Trend", "Segmented Regression"), 
       lty = c(2,1,1), col = c("lightgrey", "darkgrey", "coral"), lwd = 2, bty = "n")
par(op)
dev.off()


## AGE ----
## Create decomposed trend lines
plot(age.WtsALL) # visually inspect
decomp.ageALL <- as.vector(decompose(age.WtsALL)$trend)
plot(decomp.ageALL, type = "l")

monthpreds <- 1:168

## Fit base model
agedecomp.lm <- lm(decomp.ageALL ~ monthpreds)
davies.test(agedecomp.lm, seg.Z = ~ monthpreds, alternative = "two.sided")
# says that there is indeed a need for a segmented regression, if anything it picks the first breakpoint (~ month 127)

## Completely bottom-up, only specifying that the model can have three or five breaks.
agedecomp.seglm5 <- segmented(agedecomp.lm, 
                              seg.Z = ~ monthpreds,
                              npsi = list(monthpreds = 5))
agedecomp.seglm3 <- segmented(agedecomp.lm, 
                              seg.Z = ~ monthpreds,
                              npsi = list(monthpreds = 3))
anova(agedecomp.seglm3, agedecomp.seglm5) # 5 breaks adds significant explanatory value.
summary(agedecomp.seglm3) # R2 = 96%
summary(agedecomp.seglm5) # R2 = 98%
slope(agedecomp.seglm3)
slope(agedecomp.seglm5)
# third (and fifth) slope is small but the only one that is positive

# both identify breakpoints around 69, then again at 102-106 and 154
confint.segmented(agedecomp.seglm3)
confint.segmented(agedecomp.seglm5)

plot(decomp.ageALL, type = "l", main = "5 breakpoint model")
abline(v = c(69, 105, 115, 128, 155), lty = 2, col = "coral")
plot(decomp.ageALL, type = "l", main = "3 breakpoint model")
abline(v = c(69, 100, 157), lty = 2, col = "cadetblue")

# inspection of the actual data shows the 5 breakpoint model is likely an overfit, so go with 3 point model

decomp.ageALL <- ts(decomp.ageALL, start = c(2007,1), frequency = 12)
decomp.ageALL[100] - decomp.ageALL[157] # raw change
(decomp.ageALL[100] - decomp.ageALL[157])/decomp.ageALL[100] # p change


## Visualize with slopes overlayed on raw data
pdf("age_segmented3.pdf", height = 5, width = 7)
plot(y = agedecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "AGE")
lines(y = decomp.ageALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.5, by = 0.05), labels = seq(0.1, 0.5, by = 0.05))
abline(v = c(69, 100, 157), lty = 2, col = "cadetblue") 
box()
par(new = TRUE)
plot(age.WtsALL, ylim = c(0.1, 0.5), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)
legend("bottomleft", c("Raw data", "Trend", "Segmented Regression"), 
       lty = c(2,1,1), col = c("lightgrey", "darkgrey", "coral"), lwd = 2, bty = "n")
par(op)
dev.off()


## SKIN TONE----
## Create decomposed trend lines
plot(skin.WtsALL) # visually inspect
decomp.skinALL <- as.vector(decompose(skin.WtsALL)$trend)
plot(decomp.skinALL, type = "l")

monthpreds <- 1:168

## Fit base model
skindecomp.lm <- lm(decomp.skinALL ~ monthpreds)
davies.test(skindecomp.lm, seg.Z = ~ monthpreds, alternative = "two.sided")
# says that there is indeed a need for a segmented regression, if anything it picks the first breakpoint (~ month 59)

## Completely bottom-up, only specifying that the model can have three or five breaks.
skindecomp.seglm5 <- segmented(skindecomp.lm, 
                            seg.Z = ~ monthpreds,
                            npsi = list(monthpreds = 5))
skindecomp.seglm3 <- segmented(skindecomp.lm, 
                              seg.Z = ~ monthpreds,
                              npsi = list(monthpreds = 3))
anova(skindecomp.seglm3, skindecomp.seglm5) # 5 breaks adds significant explanatory value.
summary(skindecomp.seglm3) # R2 = 98%
summary(skindecomp.seglm5) # R2 = 99%
slope(skindecomp.seglm3)
slope(skindecomp.seglm5)

# both identify breakpoints around 55-62, then again at 111-115
confint.segmented(skindecomp.seglm3)
confint.segmented(skindecomp.seglm5)

plot(decomp.skinALL, type = "l", main = "5 breakpoint model")
abline(v = c(32, 55, 103, 111, 122), lty = 2, col = "coral")
plot(decomp.skinALL, type = "l", main = "3 breakpoint model")
abline(v = c(62, 115, 122), lty = 2, col = "cadetblue")

# inspection of the actual data mins shows that 111 is the true minimum; 123 is the true maximum for the final slope
decomp.skinALL <- ts(decomp.skinALL, start = c(2007,1), frequency = 12)
decomp.skinALL[111] - decomp.skinALL[123]
(decomp.skinALL[111] - decomp.skinALL[123])/decomp.skinALL[111]



## Visualize with slopes overlayed on raw data
pdf("skin_segmented3.pdf", height = 5, width = 7)
plot(y = skindecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "SKIN-TONE")
lines(y = decomp.skinALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.5, by = 0.05), labels = seq(0.1, 0.5, by = 0.05))
abline(v = c(62, 115, 122), lty = 2, col = "cadetblue") 
box()
par(new = TRUE)
plot(skin.WtsALL, ylim = c(0.1, 0.5), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)
legend("bottomleft", c("Raw data", "Trend", "Segmented Regression"), 
       lty = c(2,1,1), col = c("lightgrey", "darkgrey", "coral"), lwd = 2, bty = "n")
par(op)
dev.off()


## RACE ----
## Create decomposed trend lines
plot(race.WtsALL) # visually inspect
decomp.raceALL <- as.vector(decompose(race.WtsALL)$trend)
plot(decomp.raceALL, type = "l")

monthpreds <- 1:168

## Fit base model
racedecomp.lm <- lm(decomp.raceALL ~ monthpreds)
davies.test(racedecomp.lm, seg.Z = ~ monthpreds, alternative = "two.sided")
# says that there is indeed a need for a segmented regression, if anything it picks the first breakpoint (~ month 59)

## Completely bottom-up, only specifying that the model can have three or five breaks.
racedecomp.seglm5 <- segmented(racedecomp.lm, 
                               seg.Z = ~ monthpreds,
                               npsi = list(monthpreds = 5))
racedecomp.seglm3 <- segmented(racedecomp.lm, 
                               seg.Z = ~ monthpreds,
                               npsi = list(monthpreds = 3))
anova(racedecomp.seglm3, racedecomp.seglm5) # 5 breaks adds significant explanatory value.
summary(racedecomp.seglm3) # R2 = 98%
summary(racedecomp.seglm5) # R2 = 99%
slope(racedecomp.seglm3)
slope(racedecomp.seglm5)

# both identify breakpoints around 69-73, then again at 112 and 123
confint.segmented(racedecomp.seglm3)
confint.segmented(racedecomp.seglm5)

plot(decomp.raceALL, type = "l", main = "5 breakpoint model")
abline(v = c(23, 69, 113, 123, 155), lty = 2, col = "coral")
plot(decomp.raceALL, type = "l", main = "3 breakpoint model")
abline(v = c(73, 112, 124), lty = 2, col = "cadetblue")

# inspection of the actual data mins shows that 112 is the true minimum; 124 is the true maximum for the final slope
decomp.raceALL <- ts(decomp.raceALL, start = c(2007,1), frequency = 12)
decomp.raceALL[112] - decomp.raceALL[124]
(decomp.raceALL[112] - decomp.raceALL[124])/decomp.raceALL[112] # 10% increase

## Visualize with slopes overlayed on raw data
pdf("race_segmented3.pdf", height = 5, width = 7)
plot(y = racedecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "RACE")
lines(y = decomp.raceALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.2, 0.4, by = 0.05), labels = seq(0.2, 0.4, by = 0.05))
abline(v = c(73, 112, 124), lty = 2, col = "cadetblue") 
box()
par(new = TRUE)
plot(race.WtsALL, ylim = c(0.2, 0.4), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)
legend("bottomleft", c("Raw data", "Trend", "Segmented Regression"), 
       lty = c(2,1,1), col = c("lightgrey", "darkgrey", "coral"), lwd = 2, bty = "n")
par(op)
dev.off()



## DISABILITY ----
## Create decomposed trend lines
plot(dis.WtsALL) # visually inspect
decomp.disALL <- as.vector(decompose(dis.WtsALL)$trend)
plot(decomp.disALL, type = "l")
max(decomp.disALL, na.rm = T)

monthpreds <- 1:168

## Fit base model
disdecomp.lm <- lm(decomp.disALL ~ monthpreds)
davies.test(disdecomp.lm, seg.Z = ~ monthpreds, alternative = "two.sided")
# says that there is indeed a need for a segmented regression, if anything it picks the first breakpoint (~ month 93)

## Completely bottom-up, only specifying that the model can have three or five breaks.
disdecomp.seglm5 <- segmented(disdecomp.lm, 
                               seg.Z = ~ monthpreds,
                               npsi = list(monthpreds = 5))
disdecomp.seglm3 <- segmented(disdecomp.lm, 
                               seg.Z = ~ monthpreds,
                               npsi = list(monthpreds = 3))
anova(disdecomp.seglm3, disdecomp.seglm5) # 5 breaks adds significant explanatory value.
summary(disdecomp.seglm3) # R2 = 91%
summary(disdecomp.seglm5) # R2 = 92%
slope(disdecomp.seglm3)
slope(disdecomp.seglm5)

# both identify breakpoints around 68-70, then again at 110-111
confint.segmented(disdecomp.seglm3)
confint.segmented(disdecomp.seglm5)

plot(decomp.disALL, type = "l", main = "5 breakpoint model")
abline(v = c(71, 80, 111, 122, 137), lty = 2, col = "coral")
plot(decomp.disALL, type = "l", main = "3 breakpoint model")
abline(v = c(68, 110, 123), lty = 2, col = "cadetblue")

decomp.disALL <- ts(decomp.disALL, start = c(2007, 1), frequency = 12)
(decomp.disALL[110] - decomp.disALL[123])/decomp.disALL[110]

## Visualize with slopes overlayed on raw data
pdf("dis_segmented3.pdf", height = 5, width = 7)
plot(y = disdecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.53), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "DISABILITY")
lines(y = decomp.disALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.53, by = 0.05), labels = seq(0.1, 0.53, by = 0.05))
abline(v = c(68, 110, 123), lty = 2, col = "cadetblue") 
box()
par(new = TRUE)
plot(dis.WtsALL, ylim = c(0.1, 0.53), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)
legend("bottomleft", c("Raw data", "Trend", "Segmented Regression"), 
       lty = c(2,1,1), col = c("lightgrey", "darkgrey", "coral"), lwd = 2, bty = "n")
par(op)
dev.off()



## BODY WEIGHT ----
## Create decomposed trend lines
plot(weight.WtsALL) # visually inspect
decomp.weightALL <- as.vector(decompose(weight.WtsALL)$trend)
plot(decomp.weightALL, type = "l")
decomp.weightALL

# for predictor months, 1 corresponded to Jan 2007
# So 40 will correspond to April 2010
monthpreds2 <- 40:168

## Fit base model
weightdecomp.lm <- lm(decomp.weightALL ~ monthpreds2)
davies.test(weightdecomp.lm, seg.Z = ~ monthpreds2, alternative = "two.sided")
# says that there is indeed a need for a segmented regression, if anything it picks the first breakpoint (~ month 134)

## Completely bottom-up, only specifying that the model can have three or five breaks.
weightdecomp.seglm5 <- segmented(weightdecomp.lm, 
                              seg.Z = ~ monthpreds2,
                              npsi = list(monthpreds2 = 5))
weightdecomp.seglm3 <- segmented(weightdecomp.lm, 
                              seg.Z = ~ monthpreds2,
                              npsi = list(monthpreds2 = 3))
anova(weightdecomp.seglm3, weightdecomp.seglm5) # 5 breaks adds significant explanatory value.
summary(weightdecomp.seglm3) # R2 = 88%
summary(weightdecomp.seglm5) # R2 = 92%
slope(weightdecomp.seglm3)
slope(weightdecomp.seglm5)

# both identify breakpoints around 68-75, then again at 104-105 and 115-116
confint.segmented(weightdecomp.seglm3)
confint.segmented(weightdecomp.seglm5)

plot(decomp.weightALL, type = "l", main = "5 breakpoint model")
abline(v = c(50, 68, 105, 116, 155)-40, lty = 2, col = "coral") # note -40 to match breakpoints to indices
plot(decomp.weightALL, type = "l", main = "3 breakpoint model")
abline(v = c(75, 104, 116)-40, lty = 2, col = "cadetblue")

decomp.weightALL <- ts(decomp.weightALL, start = c(2010, 4), frequency = 12)
decomp.weightALL

## Visualize with slopes overlayed on raw data
pdf("weight_segmented3.pdf", height = 5, width = 7)
plot(y = weightdecomp.seglm3$fitted.values, x = monthpreds2[7:(length(monthpreds2)-6)], type = "l",
     axes = FALSE, ylim = c(0.1, 0.53), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "BODY WEIGHT", xlim = c(1,168))
lines(y = decomp.weightALL, x = monthpreds2, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.53, by = 0.05), labels = seq(0.1, 0.53, by = 0.05))
abline(v = c(75, 104, 116), lty = 2, col = "cadetblue") 
box()
par(new = TRUE)
plot(weight.WtsALL, ylim = c(0.1, 0.53), xlim = c(2007, 2021), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)
legend("bottomleft", c("Raw data", "Trend", "Segmented Regression"), 
       lty = c(2,1,1), col = c("lightgrey", "darkgrey", "coral"), lwd = 2, bty = "n")
par(op)
dev.off()




## VISUALIZE ALL TOGETHER ----

pdf("all_segmented_lines.pdf", height = 8, width = 12)
op <- par(mfrow = c(2,3))
plot(y = sexdecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "SEXUALITY")
lines(y = decomp.sexALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.5, by = 0.05), labels = seq(0.1, 0.5, by = 0.05))
abline(v = c(62, 113, 124), lty = 2, col = "cadetblue") 
text(x = 116, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("slope", round(slope(sexdecomp.seglm3)$monthpreds[3,1], 4), sep = "= "))
text(x = 122, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("months", 
           paste(round(confint.segmented(sexdecomp.seglm3)[2:3,1], 0), collapse = ","), 
           sep = "= "))
box()
par(new = TRUE)
plot(sex.WtsALL, ylim = c(0.1, 0.5), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)

plot(y = racedecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "RACE")
lines(y = decomp.raceALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.2, 0.4, by = 0.05), labels = seq(0.2, 0.4, by = 0.05))
abline(v = c(73, 112, 124), lty = 2, col = "cadetblue") 
text(x = 116, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("slope", round(slope(racedecomp.seglm3)$monthpreds[3,1], 4), sep = "= "))
text(x = 122, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("months", 
           paste(round(confint.segmented(racedecomp.seglm3)[2:3,1], 0), collapse = ","), 
           sep = "= "))
box()
par(new = TRUE)
plot(race.WtsALL, ylim = c(0.2, 0.4), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)


plot(y = skindecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "SKIN-TONE")
lines(y = decomp.skinALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.5, by = 0.05), labels = seq(0.1, 0.5, by = 0.05))
abline(v = c(62, 115, 122), lty = 2, col = "cadetblue") 
text(x = 116, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("slope", round(slope(skindecomp.seglm3)$monthpreds[3,1], 4), sep = "= "))
text(x = 122, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("months", 
           paste(round(confint.segmented(skindecomp.seglm3)[2:3,1], 0), collapse = ","), 
           sep = "= "))
box()
par(new = TRUE)
plot(skin.WtsALL, ylim = c(0.2, 0.4), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)

plot(y = agedecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.5), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "AGE")
lines(y = decomp.ageALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.5, by = 0.05), labels = seq(0.1, 0.5, by = 0.05))
abline(v = c(69, 100, 157), lty = 2, col = "cadetblue") 
text(x = 116, y = 0.3, srt = 90, col = "cadetblue", cex = 0.8,
     paste("slope", round(slope(agedecomp.seglm3)$monthpreds[3,1], 4), sep = "= "))
text(x = 122, y = 0.3, srt = 90, col = "cadetblue", cex = 0.8,
     paste("months", 
           paste(round(confint.segmented(agedecomp.seglm3)[2:3,1], 0), collapse = ","), 
           sep = "= "))
box()
par(new = TRUE)
plot(age.WtsALL, ylim = c(0.1, 0.5), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)

plot(y = disdecomp.seglm3$fitted.values, x = monthpreds[7:162], type = "l",
     axes = FALSE, ylim = c(0.1, 0.53), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "DISABILITY")
lines(y = decomp.disALL, x = monthpreds, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.53, by = 0.05), labels = seq(0.1, 0.53, by = 0.05))
abline(v = c(68, 110, 123), lty = 2, col = "cadetblue") 
text(x = 116, y = 0.3, srt = 90, col = "cadetblue", cex = 0.8,
     paste("slope", round(slope(disdecomp.seglm3)$monthpreds[3,1], 4), sep = "= "))
text(x = 122, y = 0.3, srt = 90, col = "cadetblue", cex = 0.8,
     paste("months", 
           paste(round(confint.segmented(disdecomp.seglm3)[2:3,1], 0), collapse = ","), 
           sep = "= "))
box()
par(new = TRUE)
plot(dis.WtsALL, ylim = c(0.1, 0.53), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)

plot(y = weightdecomp.seglm3$fitted.values, x = monthpreds2[7:(length(monthpreds2)-6)], type = "l",
     axes = FALSE, ylim = c(0.1, 0.53), ylab = "", xlab = "", 
     col = "coral", lwd = 2, main = "BODY WEIGHT", xlim = c(1,168))
lines(y = decomp.weightALL, x = monthpreds2, col = "darkgrey", lwd = 2)
axis(1, at = seq(1, 168, by = 12), labels = 2007:2020)
axis(2, at = seq(0.1, 0.53, by = 0.05), labels = seq(0.1, 0.53, by = 0.05))
abline(v = c(75, 104, 116), lty = 2, col = "cadetblue") 
text(x = 107, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("slope", round(slope(weightdecomp.seglm3)$monthpreds[3,1], 4), sep = "= "))
text(x = 113, y = 0.4, srt = 90, col = "cadetblue", cex = 0.8,
     paste("months", 
           paste(round(confint.segmented(weightdecomp.seglm3)[2:3,1], 0), collapse = ","), 
           sep = "= "))
box()
par(new = TRUE)
plot(weight.WtsALL, ylim = c(0.1, 0.53), xlim = c(2007, 2021), ylab = "", xlab = "",
     axes = FALSE, col = "lightgrey", lty = 2)

par(op)
dev.off()



## Visualize with all plots in one ----
sexsegTS <- window(ts(sexdecomp.seglm3$fitted.values, start = c(2007, 7), frequency = 12),
                    start = c(2014, 1))
racesegTS <- window(ts(racedecomp.seglm3$fitted.values, start = c(2007, 7), frequency = 12),
                    start = c(2014, 1))
skinsegTS <- window(ts(skindecomp.seglm3$fitted.values, start = c(2007, 7), frequency = 12),
                    start = c(2014, 1))
agesegTS <- window(ts(agedecomp.seglm3$fitted.values, start = c(2007, 7), frequency = 12),
                    start = c(2014, 1))
dissegTS <- window(ts(disdecomp.seglm3$fitted.values, start = c(2007, 7), frequency = 12),
                    start = c(2014, 1))
weightsegTS <- window(ts(weightdecomp.seglm3$fitted.values, start = c(2010, 10), frequency = 12),
                    start = c(2014, 1))

pdf("allbumps_inspect_combine_wlines.pdf", height = 6, width = 8)
plot(racesegTS,
     axes = FALSE, ylim = c(0.1, 0.6), ylab = "", xlab = "", 
     col = "cadetblue", lwd = 3)
text(x = 2020, y = 0.3, labels = "Race", col = "cadetblue", cex = 1.5, adj = 1)
axis(1, at = 2007:2020, labels = 2007:2020)
axis(2, at = seq(0.1, 0.6, by = 0.1), labels = seq(0.1, 0.6, by = 0.1))
rect(xleft = 2016+(3/12), ybottom = 0.25, # month 112 = Apr 2016 2016+3/12
     xright = 2017+(3/12), ytop = 0.35,  # month 124 = Apr 2017 2017+3/12
     col = adjustcolor("cadetblue", alpha = 0.2), border = adjustcolor("cadetblue", alpha = 0.2))
box()
par(new = TRUE)
plot(window(race.WtsALL, start = c(2014, 1)), ylim = c(0.1, 0.6), ylab = "", xlab = "",
     axes = FALSE, col = "cadetblue", lty = 2)

par(new = TRUE)
plot(skinsegTS,
     axes = FALSE, ylim = c(0.1, 0.6), ylab = "", xlab = "", 
     col = "coral", lwd = 3)
text(x = 2020, y = 0.22, labels = "Skin-tone", col = "coral", cex = 1.5, adj = 1)
rect(xleft = 2016+(6/12), ybottom = 0.25, # month 115 = July 2016 2016+6/12
     xright = 2017+(1/12), ytop = 0.35,  # month 122 = Feb 2017 2017+1/12
     col = adjustcolor("coral", alpha = 0.2), border = adjustcolor("coral", alpha = 0.2))
par(new = TRUE)
plot(window(skin.WtsALL, start = c(2014, 1)), ylim = c(0.1, 0.6), ylab = "", xlab = "",
     axes = FALSE, col = "coral", lty = 2)

par(new = TRUE)
plot(dissegTS,
     axes = FALSE, ylim = c(0.1, 0.6), ylab = "", xlab = "", 
     col = "darkgreen", lwd = 3)
text(x = 2020, y = 0.55, labels = "Disability", col = "darkgreen", cex = 1.5, adj = 1)
rect(xleft = 2016+(1/12), ybottom = 0.45, # month 110 = Feb 2016 2016+1/12
     xright = 2017+(2/12), ytop = 0.55,  # month 123 = Mar 2017 2017+2/12
     col = adjustcolor("darkgreen", alpha = 0.2), border = adjustcolor("darkgreen", alpha = 0.2))
par(new = TRUE)
plot(window(dis.WtsALL, start = c(2014, 1)), ylim = c(0.1, 0.6), ylab = "", xlab = "",
     axes = FALSE, col = "darkgreen", lty = 2)

par(new = TRUE)
plot(weightsegTS, 
     axes = FALSE, ylim = c(0.1, 0.6), ylab = "", xlab = "", 
     col = "purple", lwd = 3)
text(x = 2020, y = 0.45, labels = "Body weight", col = "purple", cex = 1.5, adj = 1)
rect(xleft = 2015+(7/12), ybottom = 0.45, # month 104 = August 2015 2016+7/12
     xright = 2016+(7/12), ytop = 0.55,  # month 116 = August 2016 2016+7/12
     col = adjustcolor("purple", alpha = 0.2), border = adjustcolor("purple", alpha = 0.2))
par(new = TRUE)
plot(window(weight.WtsALL, start = c(2014, 1)), ylim = c(0.1, 0.6), ylab = "", xlab = "",
     axes = FALSE, col = "purple", lty = 2)

par(new = TRUE)
plot(sexsegTS,
     axes = FALSE, ylim = c(0.1, 0.6), ylab = "", xlab = "", 
     col = "darkblue", lwd = 3)
text(x = 2020, y = 0.12, labels = "Sexuality", col = "darkblue", cex = 1.5, adj = 1)
# rect(xleft = 2016+(4/12), ybottom = 0.15, # month 113 = May 2016 2016+4/12
#      xright = 2017+(3/12), ytop = 0.25,  # month 124 = April 2017 2017+3/12
#      col = adjustcolor("darkblue", alpha = 0.2), border = adjustcolor("darkblue", alpha = 0.2))
par(new = TRUE)
plot(window(sex.WtsALL, start = c(2014, 1)), ylim = c(0.1, 0.6), ylab = "", xlab = "",
     axes = FALSE, col = "darkblue", lty = 2)

par(new = TRUE)
plot(agesegTS,
     axes = FALSE, ylim = c(0.1, 0.6), ylab = "", xlab = "", 
     col = "goldenrod", lwd = 3)
text(x = 2020, y = 0.39, labels = "Age", col = "goldenrod", cex = 1.5, adj = 1)
par(new = TRUE)
plot(window(age.WtsALL, start = c(2014, 1)), ylim = c(0.1, 0.6), ylab = "", xlab = "",
     axes = FALSE, col = "goldenrod", lty = 2)

dev.off()

