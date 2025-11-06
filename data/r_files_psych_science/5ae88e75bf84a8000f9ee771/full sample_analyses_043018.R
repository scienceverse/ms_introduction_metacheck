## Charlesworth & Banaji. Patterns of Implicit and Explicit Attitudes I. Long-term trends from 2007 to 2016.
## Script version date: April 12th, 2018

####################################################################################################################################

## This file performs all primary analyses 
# (means, correlations, main arima models) 
# without any data exclusions

# The files used are those ending .iat, which includes a variety of relevant variables, and is limited to those with complete IAT scores.
# We also compare to .iat, which includes a variety of relevant variables, but is limited to those with complete IAT and explicit scores, 
# and only US participants


####################################################################################################################################
## Installing required packages
install.packages("car", dependencies = TRUE) 
install.packages("pastecs", dependencies = TRUE) 

install.packages("lsr", dependencies = TRUE)  
install.packages("lm.beta", dependencies = TRUE) 
install.packages("bootES", dependencies = TRUE) 
install.packages("ggplot2", dependencies = TRUE)   
install.packages("corrplot", dependencies = TRUE) 

install.packages("forecast", dependencies = TRUE) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
install.packages("tseries", dependencies = TRUE)
install.packages("lmtest", dependencies = TRUE)
install.packages("strucchange", dependencies = TRUE)

## Loading required packages
require(car)
require(pastecs)

require(lsr)
require(lm.beta) 
require(bootES) 
require(ggplot2) 
require(corrplot) 

require(forecast) ## ensure this is forecast package v8.2 (see arima_011918.R or instructions for details)
require(tseries)
require(lmtest)
require(strucchange)

set.seed(000) # be sure to set the seed because of the random selection in the bootstrapping for confidence intervals


## Load the data objects 
load("fullsample_alldata_043018.RData")
load("fullsample_timeseries_043018.RData")
load("fullsample_TSvectors_043018.RData")

## MEANS OVERALL -----------------------------
## IMPLICIT ------------------------------------------------------------------------------------------------------

## get measures of central tendency of the implicit attitudes 
stat.desc(age.iat$dscore)[c("mean", "std.dev")]
stat.desc(dis.iat$dscore)[c("mean", "std.dev")]
stat.desc(race.iat$dscore)[c("mean", "std.dev")]
stat.desc(s.iat$dscore)[c("mean", "std.dev")]
stat.desc(skin.iat$dscore)[c("mean", "std.dev")]
stat.desc(weight.iat$dscore)[c("mean", "std.dev")]
stat.desc(weightbody$dscore)[c("mean", "std.dev")]
stat.desc(weightface$dscore)[c("mean", "std.dev")]

# significance, one-sample t-tests
t.test(age.iat$dscore)
t.test(dis.iat$dscore) 
t.test(race.iat$dscore)
t.test(s.iat$dscore) 
t.test(skin.iat$dscore) 
t.test(weight.iat$dscore) 
t.test(weightbody$dscore)
t.test(weightface$dscore)

# Cohens ds effect sizes
cohensD(age.iat$dscore) # 
cohensD(dis.iat$dscore) # 
cohensD(race.iat$dscore) # 
cohensD(s.iat$dscore) # 
cohensD(skin.iat$dscore) # 
cohensD(weight.iat$dscore) # 
cohensD(weightbody$dscore)
cohensD(weightface$dscore)

# compile cohens for plotting later...
cohens.implicit <- c(cohensD(age.iat$dscore), cohensD(skin.iat$dscore), cohensD(race.iat$dscore), 
                     cohensD(dis.iat$dscore), cohensD(weight.iat$dscore), cohensD(s.iat$dscore))

# cohens ds confidence intervals for plotting...
## note that these simulations will take a while...
set.seed(000)
age.iat.sample1 <- age.iat[sample(1:nrow(age.iat), 1000),]
dis.iat.sample1 <- dis.iat[sample(1:nrow(dis.iat), 1000),]
race.iat.sample1 <- race.iat[sample(1:nrow(race.iat), 1000),]
s.iat.sample1 <- s.iat[sample(1:nrow(s.iat), 1000),]
skin.iat.sample1 <- skin.iat[sample(1:nrow(skin.iat), 1000),]
weight.iat.sample1 <- weight.iat[sample(1:nrow(weight.iat), 1000),]

age.cohens.CI <- bootES(age.iat.sample1, R=10000, data.col="dscore",
                        effect.type = "cohens.d")
age.cohens.CI$bounds 
age.cohens.CI$t0 - age.cohens.CI$bounds[1] # the simulated mean - lower boundary = the standard error of the cohens d (used in plotting).

dis.cohens.CI <- bootES(dis.iat.sample1, R=10000, data.col="dscore",
                        effect.type = "cohens.d")
dis.cohens.CI$bounds 
dis.cohens.CI$t0 - dis.cohens.CI$bounds[1] 

race.cohens.CI <- bootES(race.iat.sample1, R=10000, data.col="dscore",
                         effect.type = "cohens.d")
race.cohens.CI$bounds 
race.cohens.CI$t0 - race.cohens.CI$bounds[1] 

sex.cohens.CI <- bootES(s.iat.sample1, R=10000, data.col="dscore",
                        effect.type = "cohens.d")
sex.cohens.CI$bounds 
sex.cohens.CI$t0 - sex.cohens.CI$bounds[1] 

skin.cohens.CI <- bootES(skin.iat.sample1, R=10000, data.col="dscore",
                         effect.type = "cohens.d")
skin.cohens.CI$bounds 
skin.cohens.CI$t0 - skin.cohens.CI$bounds[1] 

weight.cohens.CI <- bootES(weight.iat.sample1, R=10000, data.col="dscore",
                           effect.type = "cohens.d")
weight.cohens.CI$bounds
weight.cohens.CI$t0 - weight.cohens.CI$bounds[1] 


cohens.SE.implicit <- as.vector(c(age.cohens.CI$t0 - age.cohens.CI$bounds[1],
                                  skin.cohens.CI$t0 - skin.cohens.CI$bounds[1],
                                  race.cohens.CI$t0 - race.cohens.CI$bounds[1],
                                  dis.cohens.CI$t0 - dis.cohens.CI$bounds[1],
                                  weight.cohens.CI$t0 - weight.cohens.CI$bounds[1],
                                  sex.cohens.CI$t0 - sex.cohens.CI$bounds[1]))


## Visualize distributions of implicit attitudes
op <- par(mfrow = c(3,2))
hist(age.iat$dscore, main = "Distribution of Implicit Age Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(age.iat$dscore), col = "red", lwd = 3)
abline(v = mean(age.iat$dscore)+sd(age.iat$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(age.iat$dscore)-sd(age.iat$dscore), col = "blue", lwd = 2, lty = 2)
hist(skin.iat$dscore, main = "Distribution of Implicit Skin-Tone Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(skin.iat$dscore), col = "red", lwd = 3)
abline(v = mean(skin.iat$dscore)+sd(skin.iat$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(skin.iat$dscore)-sd(skin.iat$dscore), col = "blue", lwd = 2, lty = 2)
hist(race.iat$dscore, main = "Distribution of Implicit Race Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(race.iat$dscore), col = "red", lwd = 3)
abline(v = mean(race.iat$dscore)+sd(race.iat$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(race.iat$dscore)-sd(race.iat$dscore), col = "blue", lwd = 2, lty = 2)
hist(s.iat$dscore, main = "Distribution of Implicit Sexuality Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(s.iat$dscore), col = "red", lwd = 3)
abline(v = mean(s.iat$dscore)+sd(s.iat$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(s.iat$dscore)-sd(s.iat$dscore), col = "blue", lwd = 2, lty = 2)
hist(dis.iat$dscore, main = "Distribution of Implicit Disability Attitudes", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(dis.iat$dscore), col = "red", lwd = 3)
abline(v = mean(dis.iat$dscore)+sd(dis.iat$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(dis.iat$dscore)-sd(dis.iat$dscore), col = "blue", lwd = 2, lty = 2)
hist(weightbody$dscore, main = "Distribution of Implicit Weight Attitudes \n (Body-figure)", xlab = "Raw D-scores", ylim = c(0, 1), freq = FALSE, breaks = 15)
abline(v = mean(weightbody$dscore), col = "red", lwd = 3)
abline(v = mean(weightbody$dscore)+sd(weightbody$dscore), col = "blue", lwd = 2, lty = 2)
abline(v = mean(weightbody$dscore)-sd(weightbody$dscore), col = "blue", lwd = 2, lty = 2)
par(op)


## plot all in one graph using boxplots
x1 <- age.iat$dscore
x2 <- skin.iat$dscore
x3 <- race.iat$dscore
x4 <- s.iat$dscore
x5 <- dis.iat$dscore
x6 <- weight.iat$dscore
boxplot(x1, x2, x3, x4, x5, x6, names=c("Age", "Skin-Tone", "Race", "Sexuality", "Disability", "Weight"), 
        outline = FALSE,
        main = "Median Implicit Attitudes",
        ylab = "Raw IAT D-scores")



## EXPLICIT ---------------------------------------------------------------------------------------------------------

## Means
## get measures of central tendency of the explicit attitudes
stat.desc(age.iat$att.center1)[c("mean", "std.dev")]
stat.desc(dis.iat$att.center1)[c("mean", "std.dev")]
stat.desc(race.iat$att.center1)[c("mean", "std.dev")]
stat.desc(s.iat$att.center1)[c("mean", "std.dev")]
stat.desc(skin.iat$att.center1)[c("mean", "std.dev")]
stat.desc(weight.iat$att.center1)[c("mean", "std.dev")]
stat.desc(weightbody$att.center1)[c("mean", "std.dev")]
stat.desc(weightface$att.center1)[c("mean", "std.dev")]

# t-tests
t.test(age.iat$att.center1) 
t.test(dis.iat$att.center1) 
t.test(race.iat$att.center1) 
t.test(s.iat$att.center1) 
t.test(skin.iat$att.center1) 
t.test(weight.iat$att.center1) 
t.test(weightbody$att.center1)
t.test(weightface$att.center1)


# Cohens ds 
cohensD(age.iat$att.center1) # 
cohensD(dis.iat$att.center1) # 
cohensD(race.iat$att.center1) # 
cohensD(s.iat$att.center1) # 
cohensD(skin.iat$att.center1) # 
cohensD(weight.iat$att.center1) # 
cohensD(weightbody$att.center1)
cohensD(weightface$att.center1)


cohens.explicit <- c(cohensD(age.iat$att.center1), cohensD(skin.iat$att.center1), cohensD(race.iat$att.center1), 
                     cohensD(dis.iat$att.center1), cohensD(weight.iat$att.center1), cohensD(s.iat$att.center1))

# cohens ds confidence intervals
age.cohens.CI.exp <- bootES(age.iat.sample1, R=10000, data.col="att.center1",
                            effect.type = "cohens.d")
age.cohens.CI.exp$bounds 
age.cohens.CI.exp$t0 - age.cohens.CI.exp$bounds[1] 


dis.cohens.CI.exp <- bootES(dis.iat.sample1, R=10000, data.col="att.center1",
                            effect.type = "cohens.d")
dis.cohens.CI.exp$bounds  
dis.cohens.CI.exp$t0 - dis.cohens.CI.exp$bounds[1]

race.cohens.CI.exp <- bootES(race.iat.sample1, R=10000, data.col="att.center1",
                             effect.type = "cohens.d")
race.cohens.CI.exp$bounds 
race.cohens.CI.exp$t0 - race.cohens.CI.exp$bounds[1]

sex.cohens.CI.exp <- bootES(s.iat.sample1, R=10000, data.col="att.center1",
                            effect.type = "cohens.d")
sex.cohens.CI.exp$bounds 
sex.cohens.CI.exp$t0 - sex.cohens.CI.exp$bounds[1]

skin.cohens.CI.exp <- bootES(skin.iat.sample1, R=10000, data.col="att.center1",
                             effect.type = "cohens.d")
skin.cohens.CI.exp$bounds 
skin.cohens.CI.exp$t0 - skin.cohens.CI.exp$bounds[1]

weight.cohens.CI.exp <- bootES(weight.iat.sample1, R=10000, data.col="att.center1",
                               effect.type = "cohens.d")
weight.cohens.CI.exp$bounds 
weight.cohens.CI.exp$t0 - weight.cohens.CI.exp$bounds[1]


cohens.SE.explicit <- as.vector(c(age.cohens.CI.exp$t0 - age.cohens.CI.exp$bounds[1],
                                  skin.cohens.CI.exp$t0 - skin.cohens.CI.exp$bounds[1],
                                  race.cohens.CI.exp$t0 - race.cohens.CI.exp$bounds[1],
                                  dis.cohens.CI.exp$t0 - dis.cohens.CI.exp$bounds[1],
                                  weight.cohens.CI.exp$t0 - weight.cohens.CI.exp$bounds[1],
                                  sex.cohens.CI.exp$t0 - sex.cohens.CI.exp$bounds[1]))

## Visualize distributions of explicit attitudes 
op <- par(mfrow = c(3,2))
hist(age.iat$att.center1, main = "Distribution of Explicit Age Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(age.iat$att.center1), col = "red", lwd = 3)
abline(v = mean(age.iat$att.center1)+sd(age.iat$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(age.iat$att.center1)-sd(age.iat$att.center1), col = "blue", lwd = 2, lty = 2)
hist(skin.iat$att.center1, main = "Distribution of Explicit Skin-Tone Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(skin.iat$att.center1), col = "red", lwd = 3)
abline(v = mean(skin.iat$att.center1)+sd(skin.iat$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(skin.iat$att.center1)-sd(skin.iat$att.center1), col = "blue", lwd = 2, lty = 2)
hist(race.iat$att.center1, main = "Distribution of Explicit Race Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(race.iat$att.center1), col = "red", lwd = 3)
abline(v = mean(race.iat$att.center1)+sd(race.iat$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(race.iat$att.center1)-sd(race.iat$att.center1), col = "blue", lwd = 2, lty = 2)
hist(s.iat$att.center1, main = "Distribution of Explicit Sexuality Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(s.iat$att.center1), col = "red", lwd = 3)
abline(v = mean(s.iat$att.center1)+sd(s.iat$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(s.iat$att.center1)-sd(s.iat$att.center1), col = "blue", lwd = 2, lty = 2)
hist(dis.iat$att.center1, main = "Distribution of Explicit Disability Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(dis.iat$att.center1), col = "red", lwd = 3)
abline(v = mean(dis.iat$att.center1)+sd(dis.iat$att.center1), col = "blue", lwd = 2, lty = 2)
abline(v = mean(dis.iat$att.center1)-sd(dis.iat$att.center1), col = "blue", lwd = 2, lty = 2)
hist(weight.iat$att.center1, main = "Distribution of Explicit Weight Attitudes", xlab = "Raw Preference Scores", ylim = c(0, 1), freq = FALSE, breaks = 7)
abline(v = mean(weight.iat$att.center1, na.rm = TRUE), col = "red", lwd = 3)
abline(v = mean(weight.iat$att.center1, na.rm = TRUE)+sd(weight.iat$att.center1, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
abline(v = mean(weight.iat$att.center1, na.rm = TRUE)-sd(weight.iat$att.center1, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
par(op)


## All in one boxplot 
x1.e <- age.iat$att.center1
x2.e <- skin.iat$att.center1
x3.e <- race.iat$att.center1
x4.e <- s.iat$att.center1
x5.e <- dis.iat$att.center1
x6.e <- weight.iat$att.center1
boxplot(x1.e, x2.e, x3.e, x4.e, x5.e, x6.e, names=c("Age", "Skin-Tone", "Race", "Sexuality", "Disability", "Weight"), 
        outline = FALSE,
        main = "Median Explicit Attitudes",
        ylab = "Raw Preference Scores")

## Plots of implicit and explicit cohens 
## first, create the dataset
explicit.implicit <- as.data.frame(c("Age", "Skin-Tone", "Race", "Disability", "Weight","Sexuality",
                                     "Age", "Skin-Tone", "Race", "Disability", "Weight", "Sexuality"))
explicit.implicit$impexp <- c(rep("Implicit", 6), rep("Explicit", 6))
explicit.implicit$cohens <- c(cohens.implicit, cohens.explicit)
explicit.implicit$se <- c(cohens.SE.implicit, cohens.SE.explicit)
colnames(explicit.implicit)[1] <- "attitude"
explicit.implicit$attitude <- factor(explicit.implicit$attitude, levels = c("Age", "Disability", "Weight", "Skin-Tone", "Race", "Sexuality"))

# now plot (with bars)
dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = explicit.implicit$cohens + explicit.implicit$se,
              ymin = explicit.implicit$cohens - explicit.implicit$se)

p <- ggplot(data = explicit.implicit, aes(x = factor(attitude), y = cohens,
                                          fill = factor(impexp)))
p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "", y = "Cohen's d") +
  scale_fill_discrete(name = "") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold", angle = 180),
        legend.position = "bottom",
        legend.text = element_text(size = 30))


# plot (with points)
limits <- aes(ymax = explicit.implicit$cohens + explicit.implicit$se,
              ymin = explicit.implicit$cohens - explicit.implicit$se)
p <- ggplot(data = explicit.implicit, 
            aes(x = factor(attitude), y = cohens, 
                colour = factor(impexp)))
p + geom_point(stat = "identity",
               position = position_dodge(0.2),
               size = 10) +
  geom_errorbar(limits, position = position_dodge(0.2),
                width = 0.50) +
  labs(x = "", y = "Cohen's d") +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30, angle = 180))+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 30),
        legend.title = element_blank())+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major.x = element_line(colour = "black", linetype = 3))+
  ylim(0, 1.3)

# Significance of difference between Implicit versus Explicit given by overlap of the 95% confidence intervals of the
# cohen's d effect sizes, from 10000 bootstrap replications



## CORRELATIONS of implicit and explicit attitudes ---------------------
cor.test(age.iat$dscore, age.iat$att.center1) 
cor.test(dis.iat$dscore, dis.iat$att.center1) 
cor.test(race.iat$dscore, race.iat$att.center1) 
cor.test(s.iat$dscore, s.iat$att.center1) 
cor.test(skin.iat$dscore, skin.iat$att.center1) 
cor.test(weight.iat$dscore, weight.iat$att.center1) 
cor.test(weightbody$dscore, weightbody$att.center1)
cor.test(weightface$dscore, weightface$att.center1)


### TIME SERIES ---------------------------------------------------------
## be sure to use the TS vectors from the full sample
## IMPLICIT -------------------------------------------------------------------------------------------------------------
## Age -------------------------------------------------------------------------------------------------------------------------
age.arima <- auto.arima(age.Wts, stepwise = FALSE, approximation = FALSE)
forecast.age <- forecast(age.arima, h = 60) # 5 years
forecast.ageLONG <- forecast(age.arima, h = 1800) # 150 years
decomp.age <- decompose(age.Wts)

## Disability -----------
dis.arima <- auto.arima(dis.Wts, stepwise = FALSE, approximation = FALSE)
forecast.dis <- forecast(dis.arima, h = 60) # 2 years
forecast.disLONG <- forecast(dis.arima, h = 1800) # 150 years
decomp.dis <- decompose(dis.Wts)

## Race -----------
race.arima <- auto.arima(race.Wts, stepwise = FALSE, approximation = FALSE)
forecast.race <- forecast(race.arima, h = 60) # 2 years
forecast.raceLONG <- forecast(race.arima, h = 1800) # 150 years
decomp.race <- decompose(race.Wts)

## Sexuality -----------
sex.arima <- auto.arima(sex.Wts, stepwise = FALSE, approximation = FALSE)
forecast.sex <- forecast(sex.arima, h = 60) # 2 years
forecast.sexLONG <- forecast(sex.arima, h = 1800) # 150 years
decomp.sex <- decompose(sex.Wts)

## Skin -----------
skin.arima <- auto.arima(skin.Wts, stepwise = FALSE, approximation = FALSE)
forecast.skin <- forecast(skin.arima, h = 60) # 2 years
forecast.skinLONG <- forecast(skin.arima, h = 1800) # 150 years
decomp.skin <- decompose(skin.Wts)

## Weight -----------
## (1) Body ---------
weight.arimaBody <- auto.arima(weight.Wts, stepwise = FALSE, approximation = FALSE)
forecast.weightBody <- forecast(weight.arimaBody, h = 60) 
forecast.weightBodyLONG <- forecast(weight.arimaBody, h = 1800) # 200 years
plot(forecast.weightBodyLONG)
decomp.weight <- decompose(weight.Wts)

## (1) Face ---------
weight.arimaFace <- auto.arima(weight.WtsFace, stepwise = FALSE, approximation = FALSE)
forecast.weightFace <- forecast(weight.arimaFace, h = 98) # up to 2020
forecast.weightFaceLONG <- forecast(weight.arimaFace, h = 1800) # 200 years
decomp.weightFace <- decompose(weight.WtsFace)


##############################################################################################################################
##############################################################################################################################

### EXPLICIT  ------------------------------------------------------------------------------------------------------------
## Age ----------------
age.arimaE <- auto.arima(age.WEts, stepwise = FALSE, approximation = FALSE)
forecast.ageE <- forecast(age.arimaE, h = 60) # 5 years
forecast.ageLONGE <- forecast(age.arimaE, h = 1800) # 150 years
decomp.ageE <- decompose(age.WEts)

## Disability ----------------
dis.arimaE <- auto.arima(dis.WEts, stepwise = FALSE, approximation = FALSE)
forecast.disE <- forecast(dis.arimaE, h = 60) # 5 years
forecast.disLONGE <- forecast(dis.arimaE, h = 1800) # 150 years
decomp.disE <- decompose(dis.WEts)

## Race ----------------
race.arimaE <- auto.arima(race.WEts, stepwise = FALSE, approximation = FALSE)
forecast.raceE <- forecast(race.arimaE, h = 60) # 5 years
forecast.raceLONGE <- forecast(race.arimaE, h = 1800) # 150 years
decomp.raceE <- decompose(race.WEts)

## Sexuality ----------------
sex.arimaE <- auto.arima(sex.WEts, stepwise = FALSE, approximation = FALSE)
forecast.sexE <- forecast(sex.arimaE, h = 60) # 5 years
forecast.sexLONGE <- forecast(sex.arimaE, h = 1800) # 150 years
decomp.sexE <- decompose(sex.WEts)

## Skin Tone ----------------
skin.arimaE <- auto.arima(skin.WEts, stepwise = FALSE, approximation = FALSE)
forecast.skinE <- forecast(skin.arimaE, h = 60) # 5 years
forecast.skinLONGE <- forecast(skin.arimaE, h = 1800) # 150 years
decomp.skinE <- decompose(skin.WEts)

## Weight ----------------
weight.arimaE <- auto.arima(weight.WEts, stepwise = FALSE, approximation = FALSE)
forecast.weightE <- forecast(weight.arimaE, h = 60) # 5 years
forecast.weightLONGE <- forecast(weight.arimaE, h = 1800) # 150 years
decomp.weightE <- decompose(weight.WEts)

##############################################################################################################################
##############################################################################################################################

## SUMMARIES -----------------
## (1) Model parameters ---------
forecast.age$model # Age Implicit (0,1,2)(2,0,0)
forecast.ageE$model # Age Explicit (1,0,1)(0,1,2) + drift
forecast.dis$model # Disability Implicit (0,1,2) 
forecast.disE$model # Disability Explicit (1,1,1)(2,0,0) + drift
forecast.race$model # Race Implicit (1,1,1)(2,0,0)
forecast.raceE$model # Race Explicit (0,1,1)(2,0,2) + drift 
forecast.sex$model # Sexuality Implicit (0,1,2)(2,0,0) + drift 
forecast.sexE$model # Sexuality Explicit (0,1,2)(2,0,0)
forecast.skin$model # Skin-Tone Implicit (0,1,1) 
forecast.skinE$model # Skin-Tone Explicit (0,1,3)(1,0,1)
forecast.weightBody$model # Weight Implicit (Body) (1,0,0)
forecast.weightFace$model # Weight Implicit (Face) (0,1,1)(1,0,0)
forecast.weightE$model # Weight Explicit (0,1,2)(2,0,0)

## (2) Forecasts to neutrality ----------
## EXPLICIT --------
## AGE
min(which(forecast.ageLONGE$lower[,"95%"]<=0))/12 # 7.5833
min(which(forecast.ageLONGE$mean<=0))/12 # 12.5833
min(which(forecast.ageLONGE$upper[,"95%"]<=0))/12 # 19.5833
min(which(forecast.ageLONGE$upper[,"95%"]>=2*age.WEts[1]))/12 # NA

## DISABILITY
min(which(forecast.disLONGE$lower[,"95%"]<=0))/12 # 17.667 (17 yrs, 8 mos)
min(which(forecast.disLONGE$mean<=0))/12 # 23.91667 (23 yrs, 11 mos)
min(which(forecast.disLONGE$upper[,"95%"]<=0))/12 # 30.91667 (30 yrs, 11 mos)
min(which(forecast.disLONGE$upper[,"95%"]>=2*dis.WEts[1]))/12 # NA

## RACE
min(which(forecast.raceLONGE$lower[,"95%"]<=0))/12 # 1.91667 (1 yr, 11 mos)
min(which(forecast.raceLONGE$mean<=0))/12 # 7 yrs
min(which(forecast.raceLONGE$upper[,"95%"]<=0))/12 # 21.5 (21 yrs, 6 mos)

## SEXUALITY
min(which(forecast.sexLONGE$lower[,"95%"]<=0))/12 # 5.5 (5 yrs, 6 mos)
min(which(forecast.sexLONGE$mean<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.sexLONGE$upper[,"95%"]>=2*sex.WEts[1]))/12 # 42.75 (42 yrs, 9 mos)

## SKIN
min(which(forecast.skinLONGE$lower[,"95%"]<=0))/12 # 2.58333 (2 yrs, 7 mos)
min(which(forecast.skinLONGE$mean<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONGE$upper[,"95%"]>=2*skin.WEts[1]))/12 # 23.667 (23 yrs, 8 mos)

## BODY WEIGHT
min(which(forecast.weightLONGE$lower[,"95%"]<=0))/12 # 34.25 (34 yrs, 3 mos)
min(which(forecast.weightLONGE$mean<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightLONGE$upper[,"95%"]>=2*weight.WEts[1]))/12 # 67.5 (67 yrs, 6 mos)

## IMPLICIT --------
## AGE
min(which(forecast.ageLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$mean<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.ageLONG$upper[,"95%"]>=2*age.Wts[1]))/12 # NA

## DISABILITY
min(which(forecast.disLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.disLONG$mean<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.disLONG$upper[,"95%"]>=2*dis.Wts[1]))/12 # NA

## RACE
min(which(forecast.raceLONG$lower[,"95%"]<=0))/12 # 57.667 (57 yrs, 8 mos)
min(which(forecast.raceLONG$mean<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.raceLONG$upper[,"95%"]>=2*race.Wts[1]))/12 # 107.0833 (107 yrs, 1 mos)

## SEXUALITY
min(which(forecast.sexLONG$lower[,"95%"]<=0))/12 # 9 yrs
min(which(forecast.sexLONG$mean<=0))/12 # 16.5 (16 yrs, 6 mos)
min(which(forecast.sexLONG$upper[,"95%"]<=0))/12 # 29.75 (29 yrs, 9 mos)
min(which(forecast.sexLONG$upper[,"95%"]>=2*sex.Wts[1]))/12 # NA

## SKIN
min(which(forecast.skinLONG$lower[,"95%"]<=0))/12 # 138.667 (138 yrs, 8 mos) **
min(which(forecast.skinLONG$mean<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.skinLONG$upper[,"95%"]>=2*skin.Wts[1]))/12 # NA

## BODY WEIGHT (BODY)
min(which(forecast.weightBodyLONG$lower[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyLONG$mean<=0))/12 # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]<=0))/12 # NA
min(which(forecast.weightBodyLONG$upper[,"95%"]>=2*weight.Wts[1]))/12 #NA

## BODY WEIGHT (FACE) ** from Oct 2011
min(which(forecast.weightFaceLONG$lower[,"95%"]<=0))/12 # 31.75 (31 yrs, 9 mos) = July, 2043
min(which(forecast.weightFaceLONG$mean<=0))/12 # NA
min(which(forecast.weightFaceLONG$upper[,"95%"]>=2*weight.WtsFace[1]))/12 # 6.667 (6 yrs, 8 mos) = June, 2018

## (3) Percentage changes from trend values ----------
## EXPLICIT -------
## AGE
(decomp.ageE$trend[7] - decomp.ageE$trend[114])/decomp.ageE$trend[7] # 31.67% decrease
## DIS
(decomp.disE$trend[7] - decomp.disE$trend[114])/decomp.disE$trend[7] # 23.97% decrease
## RACE
(decomp.raceE$trend[7] - decomp.raceE$trend[114])/decomp.raceE$trend[7] # 36.74% decrease
## SEX
(decomp.sexE$trend[7] - decomp.sexE$trend[114])/decomp.sexE$trend[7] # 48.59% decrease
## SKIN
(decomp.skinE$trend[7] - decomp.skinE$trend[114])/decomp.skinE$trend[7] # 21.04% decrease
## WEIGHT
(decomp.weightE$trend[7] - decomp.weightE$trend[114])/decomp.weightE$trend[7] # 14.81% decrease

## IMPLICIT -----
## AGE
(decomp.age$trend[7] - decomp.age$trend[114])/decomp.age$trend[7] # 4.02% decrease
## DIS
(decomp.dis$trend[7] - decomp.dis$trend[114])/decomp.dis$trend[7] # 1.66% decrease
## RACE
(decomp.race$trend[7] - decomp.race$trend[114])/decomp.race$trend[7] # 16.81% decrease
## SEX
(decomp.sex$trend[7] - decomp.sex$trend[114])/decomp.sex$trend[7] # 33.46% decrease
## SKIN
(decomp.skin$trend[7] - decomp.skin$trend[114])/decomp.skin$trend[7] # 14.59% decrease
## WEIGHT
# Body
(decomp.weight$trend[7] - decomp.weight$trend[75])/decomp.weight$trend[7] # increased by 4.67%
# Face
(decomp.weightFace$trend[7] - decomp.weightFace$trend[84])/decomp.weightFace$trend[7] # increased by 40.10%

## (4) First and Last raw values ----------
## EXPLICIT -------
## AGE
age.WEts[1]; age.WEts[120] # 0.52, 0.40
## DIS
dis.WEts[1]; dis.WEts[120] # 0.64, 0.44
## RACE
race.WEts[1]; race.WEts[120] # 0.30, 0.17
## SEX
sex.WEts[1]; sex.WEts[120] # 0.67, 0.35
## SKIN
skin.WEts[1]; skin.WEts[120] # 0.30, 0.15
## WEIGHT
weight.WEts[1]; weight.WEts[120] # 0.90, 0.80

## IMPLICIT -----
## AGE
age.Wts[1]; age.Wts[120] # 0.44, 0.42
## DIS
dis.Wts[1]; dis.Wts[120] # 0.51, 0.52
## RACE
race.Wts[1]; race.Wts[120] # 0.33, 0.30
## SEX
sex.Wts[1]; sex.Wts[120] # 0.33, 0.17
## SKIN
skin.Wts[1]; skin.Wts[120] # 0.33, 0.29
## WEIGHT
# Body
weight.Wts[1]; weight.Wts[81] # 0.48, 0.48
# Face
weight.WtsFace[1]; weight.WtsFace[90] # 0.30, 0.41

# (2) Plots ---------
## EXPLICIT --------
## plotting all weighted EXPLICIT forecasts with confidence intervals and no model parameters, together
# with decomposed trend lines as well
op <- par(mfrow = c(2,3))
plot(forecast.sexE, main = "Explicit Sexuality Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.sexE$trend, col = "black", lwd = 3)
plot(forecast.raceE, main = "Explicit Race Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.raceE$trend, col = "black", lwd = 3)
plot(forecast.skinE, main = "Explicit Skin-Tone Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.skinE$trend, col = "black", lwd = 3)
plot(forecast.ageE, main = "Explicit Age Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8), cex.main = 1.5,
     col = "darkgrey", xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.ageE$trend, col = "black", lwd = 3)
plot(forecast.disE, main = "Explicit Disability Attitudes", ylab = "Explicit preference score", ylim = c(-0.1, 0.8), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
abline(h = 0, lty = 2)
lines(decomp.disE$trend, col = "black", lwd = 3)
plot(forecast.weightE, main = "Explicit Body-Weight Attitudes", ylab = "Explicit preference score", ylim = c(0.5, 1.3), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.weightE$trend, col = "black", lwd = 3)
par(op)



## IMPLICIT ----------
## plotting all weighted implicit forecasts with confidence intervals and no model parameters, together
# with decomposed trend lines as well
op <- par(mfrow = c(2,3))
plot(forecast.sex, main = "Implicit Sexuality Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.4), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.sex$trend, col = "black", lwd = 3)

plot(forecast.race, main = "Implicit Race Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.4),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.race$trend, col = "black", lwd = 3)

plot(forecast.skin, main = "Implicit Skin-Tone Attitudes", ylab = "IAT D-score", ylim = c(0.1, 0.4),cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.skin$trend, col = "black", lwd = 3)

plot(forecast.age, main = "Implicit Age Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "darkgrey", xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.age$trend, col = "black", lwd = 3)

plot(forecast.dis, main = "Implicit Disability Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2007, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"))
lines(decomp.dis$trend, col = "black", lwd = 3)

plot(forecast.weightFace, main = "Implicit Body-Weight Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "lightgrey", 
     xlim = c(2004, 2020), fcol = "darkgrey", flty = 3,
     shadecols = c("gray90", "gray96"))
lines(decomp.weightFace$trend, col = "darkgrey", lwd = 3, lty = 2)
par(new = TRUE)
plot(forecast.weightBody, main = "Implicit Body-Weight Attitudes", ylab = "IAT D-score", ylim = c(0.3, 0.6), cex.main = 1.5,
     col = "darkgrey", 
     xlim = c(2004, 2020), fcol = "black", flty = 3,
     shadecols = c("gray50", "gray76"),
     axes = FALSE)
lines(decomp.weight$trend, col = "black", lwd = 3)
text(2009, 0.35, "Face stimuli", cex = 1, adj = 0)
text(2009, 0.32, "Figure stimuli", cex = 1, adj = 0)
lines(x = c(2007, 2008.5), y = c(0.35, 0.35), lty = 2, lwd = 3, col = "darkgrey")
lines(x = c(2007, 2008.5), y = c(0.32, 0.32), lwd = 3, col = "black")
par(op)




## (3) Model fit ----------
## IMPLICIT
## Age
plot(forecast.age$residuals)
Box.test(forecast.age$residuals)
accuracy(forecast.age)

## Disability
plot(forecast.dis$residuals)
Box.test(forecast.dis$residuals)
accuracy(forecast.dis)

## Race
plot(forecast.race$residuals)
Box.test(forecast.race$residuals)
accuracy(forecast.race)

## Sexuality
plot(forecast.sex$residuals)
Box.test(forecast.sex$residuals)
accuracy(forecast.sex)

## Skin
plot(forecast.skin$residuals)
Box.test(forecast.skin$residuals)
accuracy(forecast.skin)

## Weight
plot(forecast.weightBody$residuals)
Box.test(forecast.weightBody$residuals)
accuracy(forecast.weightBody)


## EXPLICIT
## Age
plot(forecast.ageE$residuals)
Box.test(forecast.ageE$residuals)
accuracy(forecast.ageE)

## Disability
plot(forecast.disE$residuals)
Box.test(forecast.disE$residuals)
accuracy(forecast.disE)

## Race
plot(forecast.raceE$residuals)
Box.test(forecast.raceE$residuals)
accuracy(forecast.raceE)

## Sexuality
plot(forecast.sexE$residuals)
Box.test(forecast.sexE$residuals)
accuracy(forecast.sexE)

## Skin
plot(forecast.skinE$residuals)
Box.test(forecast.skinE$residuals)
accuracy(forecast.skinE)

## Weight
plot(forecast.weightE$residuals)
Box.test(forecast.weightE$residuals)
accuracy(forecast.weightE)
