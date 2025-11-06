

data <- read.csv("Study1_OriginalData_ForR_Final.csv") 

#Instructions for computing correlation + p value = http://www.statmethods.net/stats/correlations.html
library(Hmisc)

#computes correlation between VII and ESVI, using only the 24 days for which we have data
# on both ESVI and VII. This Replicates the correlation and P-value on Page 4. 
rcorr(data$VoterIntentionIndex, data$EbolaSearchVolumeIndex)

cor.test(data$VoterIntentionIndex, data$EbolaSearchVolumeIndex)

#computes correlation between VII and DailyEbolaSearchVolume - the BHS index inflates
#the correlation (p value of 0.43, correlation of 0.17)

rcorr(data$VoterIntentionIndex, data$DailyEbolaSearchVolume)

#Imports libraries necessary to perform next operations
library(plyr)
library(DataCombine)

#Calculates the Autocorrelation in ESVI

data_lagged <- slide(data, Var = "EbolaSearchVolumeIndex", slideBy = -1)

data_lagged <- rename(data_lagged, replace = c("EbolaSearchVolumeIndex-1" = "EbolaSearchVolumeIndex_Minus1"))

cor(data_lagged$EbolaSearchVolumeIndex, data_lagged$EbolaSearchVolumeIndex_Minus1, use = "complete.obs")

#generates p value
cor.test(data_lagged$EbolaSearchVolumeIndex, data_lagged$EbolaSearchVolumeIndex_Minus1)

#Calculates the Autocorrelation in Raw Ebola Searches

data_lagged_2 <- slide(data, Var = "DailyEbolaSearchVolume", slideBy = -1)

data_lagged_2 <- rename(data_lagged_2, replace = c("DailyEbolaSearchVolume-1" = "DailyEbolaSearchVolume_Minus1"))

cor(data_lagged_2$DailyEbolaSearchVolume, data_lagged_2$DailyEbolaSearchVolume_Minus1, use = "complete.obs")

#generates the p value
cor.test(data_lagged_2$DailyEbolaSearchVolume, data_lagged_2$DailyEbolaSearchVolume_Minus1, use = "complete.obs")


#Calculates the Autocorrelation in VII
#Note - here the data cannot be just lagged - there end up being too many times that a 
# Datapoint has to be correlated with "NA", which reduces the sample size

## Below code creates a new object, that just has the VII data - then removes all 
##NA values in that (2nd object), then lags it (3rd object), then correlates the 2nd and
## third objects

VII_autocorr <- data$VoterIntentionIndex

VII_autocorr_NAremoved <- VII_autocorr[!is.na(VII_autocorr)]

VII_autocorr_NAremoved_lagged <- Lag(VII_autocorr_NAremoved, shift = 1)

cor(VII_autocorr_NAremoved, VII_autocorr_NAremoved_lagged, use = "complete.obs")

#generates p value

cor.test(VII_autocorr_NAremoved, VII_autocorr_NAremoved_lagged, use = "complete.obs")

#Now accounting for 1st order autocorrelation by calculating the correlation between
#changes in daily ESVI and changes in daily VII

#looks in the dataset, applies complete.cases to the VII column, and saves all of the data in a new object

data_CompleteVII <- data[complete.cases(data$VoterIntentionIndex),]

#now this dataset has complete VII values -  calculates the daily changes in VII and daily 
#changes in ESVI and see how they predict each other

VII_DailyChanges <- diff(data_CompleteVII$VoterIntentionIndex)

ESVI_DailyChanges <- diff(data_CompleteVII$EbolaSearchVolumeIndex)

rcorr(VII_DailyChanges, ESVI_DailyChanges) #p value is 0.1591, correlation is 0.3035243

cor.test(VII_DailyChanges, ESVI_DailyChanges)

#calculating daily changes in the RAW ebola search volumes

Raw_Ebola_Changes <- diff(data_CompleteVII$DailyEbolaSearchVolume)

#Calculating 1st order autocorrelation in the RAW ebola search volume, only for the same subset
# of data points that corresponds to the VII

data_lagged_3 <- slide(data_CompleteVII, Var = "DailyEbolaSearchVolume", slideBy = -1)

data_lagged_3 <- rename(data_lagged_3, replace = c("DailyEbolaSearchVolume-1" = "DailyEbolaSearchVolume_Minus1"))

cor(data_lagged_3$DailyEbolaSearchVolume, data_lagged_3$DailyEbolaSearchVolume_Minus1, use = "complete.obs")

cor.test(data_lagged_3$DailyEbolaSearchVolume, data_lagged_3$DailyEbolaSearchVolume_Minus1, use = "complete.obs")

#calculating the correlation between the Raw Ebola Changes and the VII Changes for 
# a 2-week period

cor.test(Raw_Ebola_Changes[5:12], VII_DailyChanges[5:12])

plot(Raw_Ebola_Changes[5:12], VII_DailyChanges[5:12])

#for the raw data, calculates the correlations for the two-week period that captures the outbreak, one week before and one after
#gets basically a correlation of 1 when you correlate VII with ESVI.
# BHS get this correlation by using RAW ebola search volumes, not the ESVI used elsewhere.

cor.test(data_CompleteVII$VoterIntentionIndex[6:13], data_CompleteVII$EbolaSearchVolumeIndex[6:13])

cor.test(data_CompleteVII$VoterIntentionIndex[6:13], data_CompleteVII$DailyEbolaSearchVolume[6:13])

#this test gets the correlation of 0.61 

plot(data_CompleteVII$VoterIntentionIndex[6:13], data_CompleteVII$EbolaSearchVolumeIndex[6:13])
reg2 <- lm(data_CompleteVII$EbolaSearchVolumeIndex[6:13] ~ data_CompleteVII$VoterIntentionIndex[6:13])
abline(reg2)

#actually

##calculates the correlations for the two-week period that captures the outbreak, one week before and one after
#this is the changes data

rcorr(VII_DailyChanges[5:12], ESVI_DailyChanges[5:12])

cor.test(VII_DailyChanges[5:12], ESVI_DailyChanges[5:12])

# plots the above data with a regression line

plot(ESVI_DailyChanges[5:12], VII_DailyChanges[5:12])
reg1 <- lm(VII_DailyChanges[5:12] ~ ESVI_DailyChanges[5:12])
abline(reg1)

#sensitivity analysis - what if we expand or make the data-points sampled smaller or bigger by 1?

# it's robust to this analysis, but that's only because the entire correlation is driven by 
# the ESVI change of 0.6, which is in the 10th day, if you do it without that data point
# correlation drops a lot
rcorr(VII_DailyChanges[6:12], ESVI_DailyChanges[6:12]) # correlation goes up to 0.83, p value of 0.02

rcorr(VII_DailyChanges[4:12], ESVI_DailyChanges[4:12])# correlation is 0.65, p value of 0.06

rcorr(VII_DailyChanges[5:13], ESVI_DailyChanges[5:13]) # correlation of 0.59, p = 0.10

rcorr(VII_DailyChanges[4:13], ESVI_DailyChanges[4:13]) # correlation is 0.61, p  = 0.06

rcorr(VII_DailyChanges[6:11], ESVI_DailyChanges[6:11]) # correlation is 0.86, p = 0.0271

rcorr(VII_DailyChanges[6:13], ESVI_DailyChanges[6:13]) # correlation is 0.83, p = 0.01

rcorr(VII_DailyChanges[5:11], ESVI_DailyChanges[5:11]) # correlation of 0.67, p = 0.10

rcorr(VII_DailyChanges[c(5,6,7,8,9,11,12)], ESVI_DailyChanges[c(5,6,7,8,9,11,12)]) 

#correlation of 0.39, p value of 0.39, once removing the datapoint on day 10
#below code plots this 

plot(ESVI_DailyChanges[c(5,6,7,8,9,11,12)], VII_DailyChanges[c(5,6,7,8,9,11,12)])
reg1 <- lm(VII_DailyChanges[c(5,6,7,8,9,11,12)] ~ ESVI_DailyChanges[c(5,6,7,8,9,11,12)])
abline(reg1)

##calculates the autocorrelation for these variables during this specific time period

VII_DailyChanges_twoweeks_lagged <- Lag(VII_DailyChanges[5:12], shift = 1)

cor(VII_DailyChanges[5:12], VII_DailyChanges_twoweeks_lagged, use = "complete.obs") 

ESVI_DailyChanges_twoweeks_lagged <- Lag(ESVI_DailyChanges[5:12], shift = 1)

cor(ESVI_DailyChanges[5:12], ESVI_DailyChanges_twoweeks_lagged, use = "complete.obs") 

#Calculating the autocorrelation in the daily changes (ie. second order autocorrelation) for VII
#2nd order autocorrelation is 0.4290828

VII_DailyChanges_lagged <- Lag(VII_DailyChanges, shift = 1)

cor(VII_DailyChanges, VII_DailyChanges_lagged, use = "complete.obs") 

#generates p value

cor.test(VII_DailyChanges, VII_DailyChanges_lagged, use = "complete.obs") 

#Calculating the autocorrelation in the daily changes (ie. second order autocorrelation) for ESVI
#2nd order autocorrelation is 0.5964

ESVI_DailyChanges_lagged <- Lag(ESVI_DailyChanges, shift = 1)

cor(ESVI_DailyChanges, ESVI_DailyChanges_lagged, use = "complete.obs") 

#generates p value

cor.test(ESVI_DailyChanges, ESVI_DailyChanges_lagged, use = "complete.obs") 

##calculating correlation between VII and ESVI after controlling for second order correlation by taking the differences
## of the changes - controls for 2nd order autocorrelation

VII_DailyChanges_2ndorder <- diff(VII_DailyChanges)

ESVI_DailyChanges_2ndorder <- diff(ESVI_DailyChanges)

rcorr(ESVI_DailyChanges_2ndorder, VII_DailyChanges_2ndorder ) #correlation of r = 0.24, p value of 0.27

#calculates correlation for the same 2 week period that BHS analyzed 

rcorr(VII_DailyChanges_2ndorder[4:11], ESVI_DailyChanges_2ndorder[4:11]) 

# above correlation is r = 0.44, p value is 0.2805

#plots the above data with a regression line

plot(ESVI_DailyChanges_2ndorder[4:11], VII_DailyChanges_2ndorder[4:11])
reg1 <- lm(VII_DailyChanges_2ndorder[4:11] ~ ESVI_DailyChanges_2ndorder[4:11])
abline(reg1)

#Calculates the autocorrelation in the daily changes (ie. second order autocorrelation) for Raw Ebola Searches

RawEbola_DailyChanges <- diff(data_CompleteVII$DailyEbolaSearchVolume)

RawEbola_DailyChanges_lagged <- Lag(RawEbola_DailyChanges, shift = 1)

cor.test(RawEbola_DailyChanges, RawEbola_DailyChanges_lagged, use = "complete.obs") #output is r = -0.37

#visualization of correlation in ESVI daily changes
plot(ESVI_DailyChanges, ESVI_DailyChanges_lagged)

#visualization of correlation in VII daily changes
plot(VII_DailyChanges, VII_DailyChanges_lagged)

#plots VII and ESVI as function of time, both on the same graph
plot(data_CompleteVII$EbolaSearchVolumeIndex, col="blue")
par(new=true)
plot(data_CompleteVII$VoterIntentionIndex, col="red", lwd=3)

#visualization of how the ESVI and VII daily changes are autocorrelated - positive change followed by 
# positive change, negative change followed by negative change - also looks like there is negative autocorrelation

plot(ESVI_DailyChanges, col="red", lwd=3)

plot(VII_DailyChanges, col="blue", lwd=3)

#Pre and Post differences in voter intentions 

data_CompleteVII_September <- subset(data_CompleteVII, month=="September")

data_CompleteVII_October <- subset(data_CompleteVII, month=="October")

#calculates the means in the VII for the months of September (0.4) and October (1.4857)
#the mean for October that they get is slightly different (1.55), probably because BHS include data from November - unspecified

mean(data_CompleteVII_September$VoterIntentionIndex, na.rm = TRUE)

mean(data_CompleteVII_October$VoterIntentionIndex, na.rm = TRUE)

#calculates the mean VII, including October and November (now we get 1.55, replicating the reported value)

mean(data_CompleteVII$VoterIntentionIndex[10:24], na.rm = TRUE)

#calculates the means for VII for the last week of september and the first week of octobe - replicates BHS analysis on pg 4

mean(data_CompleteVII_October$VoterIntentionIndex[1:4])

mean(data_CompleteVII_September$VoterIntentionIndex[6:9])

#Calculates the differences in the changes, before and after the outbreak

mean(VII_DailyChanges[1:8]) #mean changes in September are -0.1875

mean(VII_DailyChanges[9:23]) #mean changes in October are 0.16667

##### plotting Raw Ebola Search Volumes against the VII

data_forplots <- read.csv("Study1_OriginalData_ForR_Final.csv") 

data_forplots$fulldate <- as.Date(data_forplots$fulldate, format="%m/%d/%y") 

library(ggplot2)
library(gridExtra)


#this will make a nice plot of just the Ebola search volume data as a function of time 

p <- ggplot(data_forplots, aes(x = fulldate, y = DailyEbolaSearchVolume))
p <- p + ggtitle('Google Searches for "Ebola" Across Time')
p <- p + geom_line(color = "grey70", size = 0.8)
p <- p + labs(x="Date", 
              y = "Google Trends Search Volume") 
p <- p + theme(plot.title = element_text(size=20, face="bold"),
               axis.title.x = element_text(face="bold", size=12), 
               axis.title.y = element_text(face="bold", size=12))
p

#this will make a plot of both ESVI and Raw Ebola Search Data

a <- data.frame(data_forplots$fulldate, data_forplots$DailyEbolaSearchVolume)
b <- data.frame(data_forplots$fulldate, data_forplots$EbolaSearchVolumeIndex)

  
p <- ggplot() + 
  geom_line(data = a, aes(x = data_forplots.fulldate, 
                 y = data_forplots.DailyEbolaSearchVolume, color="Google Trends Data"),
                 size =1, linetype=3) +
  
  geom_line(data = b, aes(x = data_forplots.fulldate, 
                 y = data_forplots.EbolaSearchVolumeIndex, color="ESVI"), 
                size =1, linetype=1)  +
  
  theme(plot.title = element_text(size=18, face="bold"),
        legend.position=c(0.19,0.74), 
        legend.title=element_blank(),
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12)) +
  
  labs(x="Date", y = "Google Trends Search Volume") +
  ggtitle('Google Searches for "Ebola" Across Time')  + 
  
  scale_color_manual(values = c("Google Trends Data"="grey1", "ESVI"="grey70")) +
    
  guides(colour=guide_legend(override.aes=list(linetype=c(1,3))))
  
p

#now plotting VII across Time####

data_forplots$VoterIntentionIndex

data_forplots_complete <- data_forplots[complete.cases(data_forplots$VoterIntentionIndex),]

v <- ggplot(data_forplots_complete, aes(x = fulldate, y = VoterIntentionIndex))
v <- v + ggtitle('Voter Intention Index (VII) Across Time')
v <- v + geom_point(color = "grey70", size = 2)
v <- v + geom_line(color = "grey70", size = 0.8)
v <- v + labs(x="Date", 
              y = "Voter Intention Index (VII)") 
v <- v + theme(plot.title = element_text(size=18, face="bold"),
               axis.title.x = element_text(face="bold", size=12), 
               axis.title.y = element_text(face="bold", size=12))

v <- v + scale_x_date(limits = as.Date(c('2014-09-01','2014-11-01')))
v

