

data <- read.csv("EbolaElections_Study3_Data.csv") 

library(Hmisc)

#computes correlation between VII and ESVI, using only the 9 days for which we have data
# on both ESVI and VII. This Replicates the correlation and P-value for study 3. 
rcorr(data$CanadaSpecificVoterIntentionIndex, data$CanadaSpecificEbolaSearchVolumeIndex)

#computes correlation between Canada VII and Canada DailyEbolaSearchVolume - shows that their index inflates
#the correlation (p value of 0.24, correlation of 0.44)
rcorr(data$CanadaSpecificVoterIntentionIndex, data$DailyEbolaSearchVolume.Canada)

#Imports libraries necessary to perform next operations
library(plyr)
library(DataCombine)

#Calculates the Autocorrelation in ESVI 

data_lagged <- slide(data, Var = "CanadaSpecificEbolaSearchVolumeIndex", slideBy = -1)

data_lagged <- rename(data_lagged, replace = c("CanadaSpecificEbolaSearchVolumeIndex-1" = "CanadaSpecificEbolaSearchVolumeIndex_Minus1"))

#generates p value and autocorrelation
#autocorrelation in the Canada Specific Ebola Search Volume Index is 0.99, p value of < 0.001
cor.test(data_lagged$CanadaSpecificEbolaSearchVolumeIndex, data_lagged$CanadaSpecificEbolaSearchVolumeIndex_Minus1)

#Calculates the Autocorrelation in Raw Ebola Searches (High: 0.899)

data_lagged_2 <- slide(data, Var = "DailyEbolaSearchVolume.Canada", slideBy = -1)

data_lagged_2 <- rename(data_lagged_2, replace = c("DailyEbolaSearchVolume.Canada-1" = "DailyEbolaSearchVolume.Canada_Minus1"))

#generates the p value and autocorrelation in the Canada Daily Ebola Search volume
# autocorrelation of 0.94 and p value of < 0.001
cor.test(data_lagged_2$DailyEbolaSearchVolume.Canada, data_lagged_2$DailyEbolaSearchVolume.Canada_Minus1, use = "complete.obs")

#Calculates the Autocorrelation in Canadian Specific VII

#Note - here the data cannot be just lagged - there end up being too many times that a 
#Datapoint has to be correlated with "NA", which reduces the sample size

Canada_VII_autocorr <- data$CanadaSpecificVoterIntentionIndex

Canada_VII_autocorr_NAremoved <- Canada_VII_autocorr[!is.na(Canada_VII_autocorr)]

Canada_VII_autocorr_NAremoved_lagged <- Lag(Canada_VII_autocorr_NAremoved, shift = 1)

#generates p value and autocorrelation
# n = 8, autocorrelation is 0.68, p value is 0.06071
cor.test(Canada_VII_autocorr_NAremoved, Canada_VII_autocorr_NAremoved_lagged, use = "complete.obs")

#Now accounting for 1st order autocorrelation by calculating the correlation between
#changes in daily Canada ESVI and changes in daily Canada VII

#looks in the dataset, applies complete.cases to the Canada VII column, and saves all of the data in a new object

data_CompleteVII <- data[complete.cases(data$CanadaSpecificVoterIntentionIndex),]

#now this dataset has complete VII values - now can calculate the daily changes in VII and daily 
#changes in ESVI and see how they predict each other
# correlation of 0.6451, p value of 0.084

Canada_VII_DailyChanges <- diff(data_CompleteVII$CanadaSpecificVoterIntentionIndex)

Canada_ESVI_DailyChanges <- diff(data_CompleteVII$CanadaSpecificEbolaSearchVolumeIndex)

cor.test(Canada_VII_DailyChanges, Canada_ESVI_DailyChanges)

plot(Canada_VII_DailyChanges, Canada_ESVI_DailyChanges)

##This is supposed to be a correlation of 1, but the BHS CanadaSpecificVoterIntentionChangeIndex
##has an extra data point on September 5 - unclear where this data point comes from
# given that that it is the 1st data point in the series - there is no other number
# from which this difference value can be calculated. 

cor.test(Canada_VII_DailyChanges, data_CompleteVII$CanadaSpecificVoterIntentionChangeIndex)


#calculating daily changes in the RAW ebola search volumes

Canada_Raw_Ebola_Changes <- diff(data_CompleteVII$DailyEbolaSearchVolume.Canada)

#calculating the correlation between the Canada Raw Ebola Changes and the Canada VII Changes
# correlation of 0.391, p value of 0.338
cor.test(Canada_Raw_Ebola_Changes, Canada_VII_DailyChanges)

#Calculating the autocorrelation in the daily changes (ie. second order autocorrelation) for VII
#2nd order autocorrelation is 0.58, p value is 0.1722

Canada_VII_DailyChanges_lagged <- Lag(Canada_VII_DailyChanges, shift = 1)

cor.test(Canada_VII_DailyChanges, Canada_VII_DailyChanges_lagged, use = "complete.obs") 

#Calculating the autocorrelation in the daily changes (ie. second order autocorrelation) for ESVI
#2nd order autocorrelation is 0.107, p value is 0.819

Canada_ESVI_DailyChanges_lagged <- Lag(Canada_ESVI_DailyChanges, shift = 1)

cor.test(Canada_ESVI_DailyChanges, Canada_ESVI_DailyChanges_lagged, use = "complete.obs") 

##calculating correlation between Canada VII and ESVI after controlling for second order correlation by taking the differences
## of the changes - controls for 2nd order autocorrelation

Canada_VII_DailyChanges_2ndorder <- diff(Canada_VII_DailyChanges)

Canada_ESVI_DailyChanges_2ndorder <- diff(Canada_ESVI_DailyChanges)

rcorr(Canada_ESVI_DailyChanges_2ndorder, Canada_VII_DailyChanges_2ndorder ) #correlation of 0.73, p = 0.0619

plot(Canada_ESVI_DailyChanges_2ndorder, Canada_VII_DailyChanges_2ndorder ) 
# plot shows that this correlation is driven entirely by 1 data point.......

#Calculating the autocorrelation in the Canada daily changes (ie. second order autocorrelation) for Canada Raw Ebola Searches

Canada_RawEbola_DailyChanges <- diff(data_CompleteVII$DailyEbolaSearchVolume.Canada)

Canada_RawEbola_DailyChanges_lagged <- Lag(Canada_RawEbola_DailyChanges, shift = 1)

cor(Canada_RawEbola_DailyChanges, Canada_RawEbola_DailyChanges_lagged, use = "complete.obs") #output is r = -0.318

#plots Canada VII and ESVI as function of time, both on the same graph
plot(data_CompleteVII$CanadaSpecificEbolaSearchVolumeIndex, col="blue")
par(new=true)
plot(data_CompleteVII$CanadaSpecificVoterIntentionIndex, col="red", lwd=3)

#visualization of how the Canada ESVI and VII daily changes are autocorrelated 

plot(Canada_ESVI_DailyChanges, col="red", lwd=3)

plot(Canada_VII_DailyChanges, col="blue", lwd=3)

##### plotting Raw Ebola Search Volumes against the VII

data_forplots <- read.csv("EbolaElections_Study3_Data.csv") 

data_forplots$fulldate <- as.Date(data_forplots$fulldate, format="%m/%d/%y") 

library(ggplot2)
library(gridExtra)


#this will make a nice plot of just the Canada Ebola search volume data as a function of time 

p <- ggplot(data_forplots, aes(x = fulldate, y = DailyEbolaSearchVolume.Canada))
p <- p + ggtitle('Canadian Google Searches for "Ebola" Across Time')
p <- p + geom_line(color = "grey70", size = 0.8)
p <- p + labs(x="Date", 
              y = "Canadian Google Trends Search Volume") 
p <- p + theme(plot.title = element_text(size=20, face="bold"),
               axis.title.x = element_text(face="bold", size=12), 
               axis.title.y = element_text(face="bold", size=12))
p

#this will make a plot of both Canadian ESVI and Raw Ebola Search Data

a <- data.frame(data_forplots$fulldate, data_forplots$DailyEbolaSearchVolume.Canada)

b <- data.frame(data_forplots$fulldate, data_forplots$CanadaSpecificEbolaSearchVolumeIndex)


p <- ggplot() + 
  geom_line(data = a, aes(x = data_forplots.fulldate, 
                          y = data_forplots.DailyEbolaSearchVolume.Canada, color="Canadian Google Trends Data"),
            size =1, linetype=3) +
  
  geom_line(data = b, aes(x = data_forplots.fulldate, 
                          y = data_forplots.CanadaSpecificEbolaSearchVolumeIndex, color="Canadian ESVI"), 
            size =1, linetype=1)  +
  
  theme(plot.title = element_text(size=18, face="bold"),
        legend.position=c(0.19,0.74), 
        legend.title=element_blank(),
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12)) +
  
  labs(x="Date", y = "Canadian Google Trends Search Volume") +
  ggtitle('Canadian Google Searches for "Ebola" Across Time')  + 
  
  scale_color_manual(values = c("Canadian Google Trends Data"="grey1", "Canadian ESVI"="grey70")) +
  
  guides(colour=guide_legend(override.aes=list(linetype=c(1,3))))

p

#now plotting Canadian VII across Time####

data_forplots$CanadaSpecificVoterIntentionIndex

data_forplots_complete <- data_forplots[complete.cases(data_forplots$CanadaSpecificVoterIntentionIndex),]

v <- ggplot(data_forplots_complete, aes(x = fulldate, y = CanadaSpecificVoterIntentionIndex))
v <- v + ggtitle('Canadian Voter Intention Index (VII) Across Time')
v <- v + geom_point(color = "grey70", size = 2)
v <- v + geom_line(color = "grey70", size = 0.8)
v <- v + labs(x="Date", 
              y = "Canadian Voter Intention Index (VII)") 
v <- v + theme(plot.title = element_text(size=18, face="bold"),
               axis.title.x = element_text(face="bold", size=12), 
               axis.title.y = element_text(face="bold", size=12))

v <- v + scale_x_date(limits = as.Date(c('2020-09-01','2020-11-01')))
v
