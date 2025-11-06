#reads the study 2 stacked data into R
data <- read.table("Study2_Stacked_Unlagged_ForRawIntraStateCorrelations.txt", header=TRUE)


#Correlation between Ebola-search-volume-index and state-specific voter intention index, 
#excluding rhode island and hawaii

###this means we can only calculate this for each state on the days that we have the 
###state specific VII

str(data)

#splits the data by state into 70 data frames, 1 for each state.
data_split <- split(data, f = data$State)

sapply(data_split, function(x) cor.test(x$ESVI, x$VII))

#Finds the correlation between ESVI and VII_StateSpecific for each of the 70 data frames. 
#These correlations were then copied and pasted into a new document (Study2_States_SingleMeasures.csv)

#Now to find the correlations between daily changes in ESVI and Daily Changes in VII

##In cleaning this data, I did the following things:

#1) I deleted all of the rows that had ".R" in them, representing the 
# republican primaries -  the .R and .D things have duplicate info - 
#the voter intention index already captures the difference between republican and 
# democrat votes - So there is no need to have a VII for both republican and democratic
# States - Each state should just have 1 VII that changes over time and 1 ESVI

#2) In Excel, I lagged each VII value by 1 and made a new column, VII_Lagged. in 
# Study2_Stacked_Unlagged_ForRawIntraStateCorrelations.txt.

#3) Replaced each of the first VII values with NA, because it doesn't make sense
# to have a VII index value from the previous state

# 4) VII_DailyChanges calculated by subtracting each VII value from the VII_Lagged value

# 5) In the VII_DailyChanges column, the first value for each state is replaced by NA, 
# because it doesn't make sense to have a daily change between states - this ensures
# all daily changes are calculated within states only

# 6) ESVI_DailyChanges calculated by Subtracting the value of ESVI on day X from the
# value of ESVI on day X+1

#The below code creates a new dataset, with only those states for which we have at least 3 VII values

data_CompleteVII_Changes <- data[!is.na(data["VII_DailyChanges_No_SmallStates_No_HI_KA_RI"]),]

data_CompleteVII_Changes <- droplevels(data_CompleteVII_Changes)

#splits the cleaned data by state into 26 data frames, 1 for each state.
data_CompleteVII_Changes_split <- 
  split(data_CompleteVII_Changes, f = data_CompleteVII_Changes$State)

#Calculates correlations between changes in VII and Changes in ESVI
#Outputs pasted into Study2_States_SingleMeasures, column ESVI_Changes_VII_Changes_No_HI_RI
sapply(data_CompleteVII_Changes_split, function(x) 
  cor.test(x$ESVI_DailyChanges_No_SmallStates_No_HI_KA_RI,
x$VII_DailyChanges_No_SmallStates_No_HI_KA_RI))

#calculates the VII autocorrelation for the main 26 states in study 2, excluding outliers
#these are then copied and pasted into "study2_States_SingleMeasures.xls" in the column
# "VII_autocorrelations"

sapply(data_CompleteVII_Changes_split, function(x) 
  cor(x$VII, x$VII_Lagged, use = "complete.obs"))

#calculating the VII-changes autocorrelation for main 26 states (below)

#First, I creared a "VII_DailyChanges_Lagged_NoSmall_No_HI_KA_RI" column, which is a lagged 
# version of the "VII_DailyChanges_No_SMallStates_No_HI_KA_RI" column

#Then the first value for each state was turned into "NA", since it represented the last value
# of the previous state

#Finally the below function is run to calculate the autocorrelations for each state
#These outputs are pasted into Study2_States_SingleMeasures, column VII_Changes_autocorrelations
#Excluding Montana, SouthCarolina1, SouthCarolina2, because they each have less than 3 data points
#when calculating the autocorrelation of the changes

sapply(data_CompleteVII_Changes_split, function(x) 
  cor(x$VII_DailyChanges_No_SmallStates_No_HI_KA_RI, x$VII_DailyChanges_Lagged_NoSmall_No_HI_KA_RI, use = "complete.obs")) 

#Pre and Post Outbreak Differences in Voter Intentions
#BHS Claim: Across 32 elections in the primary analyses
#(excluding Hawaii, RHode Island, and Kansas) - Mean 
#Voter Intention Difference score was 1.02% (difference between each state's October and September VII)

data_34 <- data[!is.na(data["VII_No_KA"]),]

data_34 <- droplevels(data_34)


data_32 <- data[!is.na(data["VII_No_HI_KA_RI"]),]

data_32 <- droplevels(data_32)

#Calculating the Voter Intention Difference Score
data_32_september <- subset(data_32, Month=="September")

data_32__september_split <- split(data_32_september, f = data_32_september$States)

data_32_october <- subset(data_32, Month=="October")

data_32__october_split <- split(data_32_october, f = data_32_october$States)

data_34_september <- subset(data_34, Month=="September")

data_34__september_split <- split(data_34_september, f = data_34_september$States)

data_34_october <- subset(data_34, Month=="October")

data_34__october_split <- split(data_34_october, f = data_34_october$States)

#The below functions calculate the mean VII for each state in the months of september and October
#I then Copy and Paste these Into "Study2_States_SingleMeasure", columns VII_September_32 and
# VII_October_32, respectively

sapply(data_32__september_split, function(x) 
  mean(x$VII_No_HI_KA_RI))

sapply(data_32__october_split, function(x) 
  mean(x$VII_No_HI_KA_RI))

##does the same thing but includes the outlier states
##I then Copy and Paste these Into "Study2_States_SingleMeasure", columns VII_September_34 and
# VII_October_34, respectively

sapply(data_34__september_split, function(x) 
  mean(x$VII_No_KA))

sapply(data_34__october_split, function(x) 
  mean(x$VII_No_KA))


data_meansincluded <- read.csv("Study2_States_SingleMeasures.csv", header=TRUE)

#autocorrelations of VII and VII Changes

t.test(data_meansincluded$VII_autocorrelations) # average correlation of 0.7637, n =26

t.test(data_meansincluded$VII_autocorrelations_No_Mon_Sc1_Sc2) 
#average correlation of 0.7512, n of 23

t.test(data_meansincluded$VII_Changes_autocorrelations_No_Mon_Sc1_Sc2)
#average correlation of 0.0336, n of 23


##Calculating the mean for september for october VII for 32 states - this outputs a difference of 1.02 - 
# replicating the mean on page 7. 

mean(data_meansincluded$VII_September_32, na.rm = TRUE)

mean(data_meansincluded$VII_October_32, na.rm = TRUE)

##Added new column to data, by hand, indicating who was leading the polls on September 30th (or earliest election
##before then)

#below code subsets the data into states that had R and D leading when the ebola outbreak hits

data_meansincluded_Rlead <- subset(data_meansincluded, PollLead_Sept30_No_HI_RI_KA=="R")

data_meansincluded_Dlead <- subset(data_meansincluded, PollLead_Sept30_No_HI_RI_KA=="D")

#Now to see what the VII-difference-score is for states with D lead (-0.3395) and states with R lead (1.7298)
#This perfectly replicates the calculations on page 7

mean(data_meansincluded_Rlead$VII_October_32, na.rm = TRUE) - mean(data_meansincluded_Rlead$VII_September_32, na.rm = TRUE)

mean(data_meansincluded_Dlead$VII_October_32, na.rm = TRUE) - mean(data_meansincluded_Dlead$VII_September_32, na.rm = TRUE)

########## below code does the same thing, including outliers Hawaii and Rhode Island

data_meansincluded_Rlead_34 <- subset(data_meansincluded, PollLead_Sept30_No_KA=="R")

data_meansincluded_Dlead_34 <- subset(data_meansincluded, PollLead_Sept30_No_KA=="D")

#18.63129 for October and 16.90139 for September - Difference is 1.73, replicating BHS
mean(data_meansincluded_Rlead_34$VII_October_34, na.rm = TRUE)

mean(data_meansincluded_Rlead_34$VII_September_34, na.rm = TRUE)


#-17.545 for October and -14.91346 for September - Difference is -2.63155, replciating BHS
mean(data_meansincluded_Dlead_34$VII_October_34, na.rm = TRUE)

mean(data_meansincluded_Dlead_34$VII_September_34, na.rm = TRUE)

#The below functions calculate the mean VII-Changes for each state in the months of september and October
#I then Copy and Paste these Into "Study2_States_SingleMeasure", columns 
# VII_Difference_September_34 and VII_Difference_October34 - - -the "32" Versions of these
#columns are the same, with Hawaii and RHode Island Removed

sapply(data_34__september_split, function(x) 
  mean(x$VII_DailyChanges, na.rm = TRUE))

sapply(data_34__october_split, function(x) 
  mean(x$VII_DailyChanges, na.rm = TRUE))

#now generating p values for the differences in VII in R leading states and D leading States
#- below code shows that, once you look at the changes in the VII in these different states,  there is 
# no longer a bandwagon effect  

R_States_VII_Differences <- data_meansincluded_Rlead$VII_Difference_October_32 - data_meansincluded_Rlead$VII_Difference_September_32

R_States_VII_Differences <- R_States_VII_Differences[!is.na(R_States_VII_Differences)]

t.test(R_States_VII_Differences) # p value of 0.7034, confidence interval includes 0

D_States_VII_Differences <- data_meansincluded_Dlead$VII_Difference_October_32 - data_meansincluded_Dlead$VII_Difference_September_32

D_States_VII_Differences <- D_States_VII_Differences[!is.na(D_States_VII_Differences)]

t.test(D_States_VII_Differences) # p value of 0.1426, confidence interval includes 0

##replicating the above code with outliers included

R_States_VII_Differences_34 <- data_meansincluded_Rlead_34$VII_Difference_October_34 - data_meansincluded_Rlead_34$VII_Difference_September_34

R_States_VII_Differences_34 <- R_States_VII_Differences_34[!is.na(R_States_VII_Differences_34)]

t.test(R_States_VII_Differences_34) # p value of 0.7034, confidence interval includes 0

D_States_VII_Differences_34 <- data_meansincluded_Dlead_34$VII_Difference_October_34 - data_meansincluded_Dlead_34$VII_Difference_September_34

D_States_VII_Differences_34 <- D_States_VII_Differences_34[!is.na(D_States_VII_Differences_34)]

t.test(D_States_VII_Differences_34) # p value of 0.07721, confidence interval includes 0, but barely - once you include the outliers
#then you're getting something in the direction opposite to that predicted by a bandwagon effect - the Democratic states are
#becoming more republican after the election.

#Now comparing the means of the changes to each other (outliers exlcuded and included)

t.test(R_States_VII_Differences, D_States_VII_Differences)

t.test(R_States_VII_Differences_34, D_States_VII_Differences_34)


##Next step is to replicate the above analysis, but for states that have Positive and Negative PVI scores

#generates new objects that subset the data based on whether the states had D or R PVI score
##Note that all of these objects Exlcude virginia, as it had a PVI score of 0, coded as NA in the data

data_meansincluded_R_PVI <- subset(data_meansincluded, PVI_Rep_Dem_WIKI=="R")

data_meansincluded_D_PVI <- subset(data_meansincluded, PVI_Rep_Dem_WIKI=="D")

#Now to see what the VII-difference-score is for states with D PVI (-0.5839) and states with R PVI (1.842533)
#This perfectly replicates BHS  calculations on page 7

mean(data_meansincluded_R_PVI$VII_October_32, na.rm = TRUE) - mean(data_meansincluded_R_PVI$VII_September_32, na.rm = TRUE)

mean(data_meansincluded_D_PVI$VII_October_32, na.rm = TRUE) - mean(data_meansincluded_D_PVI$VII_September_32, na.rm = TRUE)

########## below code does the same thing, including outliers Hawaii and Rhode Island

#18.76128 for October and 16.91965 for September - Difference is 1.84, Just like they Report (R states)

mean(data_meansincluded_R_PVI$VII_October_34, na.rm = TRUE) - mean(data_meansincluded_R_PVI$VII_September_34, na.rm = TRUE)

#-13.05544 for October and -10.37815 for September, Difference is -2.67729, just like they report (D States)

mean(data_meansincluded_D_PVI$VII_October_34, na.rm = TRUE) - mean(data_meansincluded_D_PVI$VII_September_34, na.rm = TRUE)

##The below functions calculate the mean VII-Changes for each state in the months of september and October, for states
# With R PVI and Dem PVI

########## Calculates the changes in VII in Sept and subtracts them from the VII changes in OCtober - then removes
### the NA values and does a t test on the difference in the mean changes between October and September. Does this for both
## Repub and Dem PVI states / and both including and excluding outliers

R_PVI_VII_DIfferences_32 <- data_meansincluded_R_PVI$VII_Difference_October_32 - data_meansincluded_R_PVI$VII_Difference_September_32

R_PVI_VII_DIfferences_32 <- R_PVI_VII_DIfferences_32[!is.na(R_PVI_VII_DIfferences_32)]

t.test(R_PVI_VII_DIfferences_32) #mean estimate is -0.06093254, p value of 0.9233

D_PVI_VII_DIfferences_32 <- data_meansincluded_D_PVI$VII_Difference_October_32 - data_meansincluded_D_PVI$VII_Difference_September_32

D_PVI_VII_DIfferences_32 <- D_PVI_VII_DIfferences_32[!is.na(D_PVI_VII_DIfferences_32)]

t.test(D_PVI_VII_DIfferences_32) # mean estimate is 0.1981304, p value of 0.4951

#now replicating the above with outliers included

R_PVI_VII_DIfferences_34 <- data_meansincluded_R_PVI$VII_Difference_October_34 - data_meansincluded_R_PVI$VII_Difference_September_34

R_PVI_VII_DIfferences_34 <- R_PVI_VII_DIfferences_34[!is.na(R_PVI_VII_DIfferences_34)]

t.test(R_PVI_VII_DIfferences_34) #same estimate as above - mean of -0.6093, p value of 0.9233

D_PVI_VII_DIfferences_34 <- data_meansincluded_D_PVI$VII_Difference_October_34 - data_meansincluded_D_PVI$VII_Difference_September_34

D_PVI_VII_DIfferences_34 <- D_PVI_VII_DIfferences_34[!is.na(D_PVI_VII_DIfferences_34)]

t.test(D_PVI_VII_DIfferences_34) # mean of 0.5644689, p value of 0.1877

#comparing the R PVI differences and D PVI differences against each other, instead of against 0. 

t.test(R_PVI_VII_DIfferences_32, D_PVI_VII_DIfferences_32) # p value of 0.7081

t.test(R_PVI_VII_DIfferences_34, D_PVI_VII_DIfferences_34) # p value of 0.4081

##############
#calculates the mean correlation between ESVI and VII across 32 states
t.test(data_meansincluded$ESVI_VII_Cor_No_HI_RI_KA)
#output is a mean of 0.305044 and a p value of 0.16, just like that they got

#calculates the mean correlation between ESVI and VII across 34 states (kansas excluded)
t.test(data_meansincluded$ESVI_VII_Cor_No_KA)
#output is a mean of 0.2395 and a p value of 0.05747 - just like what they got

#below code calculates the mean correlation between changes in ESVI and changes in VII across all the states, both including
# and excluding Hawaii and Rhode Island 

t.test(data_meansincluded$ESVI_Changes_VII_Changes_No_HI_RI)
#output is a mean of 0.03662394 and a p value of 0.7028

t.test(data_meansincluded$ESVI_Changes_VII_Changes_HI_RI_Included)
#output is a mean of 0.03141983 and a p value of 0.7242

###correlation between ESVI and VII for states where R vs D led the polls at time of outbreak (replicates BHS analysis)

#Raw correlation between ESVI and VII in Repub states, outliers excluded - corr is 0.5072, replicating
#BHS result on page 8
mean(data_meansincluded_Rlead$ESVI_VII_Cor_No_HI_RI_KA)

t.test(data_meansincluded_Rlead$ESVI_VII_Cor_No_HI_RI_KA)

#Raw correlation between ESVI and VII in Dem states, outliers excluded - corr is -0.07985, replicating
#BHS result on page 8
mean(data_meansincluded_Dlead$ESVI_VII_Cor_No_HI_RI_KA)

t.test(data_meansincluded_Dlead$ESVI_VII_Cor_No_HI_RI_KA)
       
#now testing the differences between Democratic and Republican leading states in original data
library(lsr)

cohensD(data_meansincluded_Rlead$ESVI_VII_Cor_No_HI_RI_KA, data_meansincluded_Dlead$ESVI_VII_Cor_No_HI_RI_KA )
#results in effect size of 0.94 - close to the 0.92 reported by BHS but doesn't exactly replicate

#replicating above, outliers included

t.test(data_meansincluded_Rlead_34$ESVI_VII_Cor_No_KA)

t.test(data_meansincluded_Dlead_34$ESVI_VII_Cor_No_KA)

# now considering only those 28 / 26 states with enough data for a detrended analysis

t.test(data_meansincluded_Rlead$ESVI_VII_Cor_26)

t.test(data_meansincluded_Dlead$ESVI_VII_Cor_26)

cohensD(data_meansincluded_Rlead$ESVI_VII_Cor_26, data_meansincluded_Dlead$ESVI_VII_Cor_26)

t.test(data_meansincluded_Rlead$ESVI_VII_Cor_26, data_meansincluded_Dlead$ESVI_VII_Cor_26)

t.test(data_meansincluded_Rlead_34$ESVI_VII_Cor_28)

t.test(data_meansincluded_Dlead_34$ESVI_VII_Cor_28)

##correlation between changes in ESVI and VII in Repub states, outliers exluded 
ESVI_VII_changes_correlation_R_32 <- data_meansincluded_Rlead$ESVI_Changes_VII_Changes_No_HI_RI

ESVI_VII_changes_correlation_R_32 <- ESVI_VII_changes_correlation_R_32[!is.na(ESVI_VII_changes_correlation_R_32)]

t.test(ESVI_VII_changes_correlation_R_32) #mean of 0.08614 and p value of 0.4919

##correlation between changes in ESVI and VII in Repub states, outliers included

ESVI_VII_changes_correlation_R_34 <- data_meansincluded_Rlead_34$ESVI_Changes_VII_Changes_HI_RI_Included

ESVI_VII_changes_correlation_R_34 <- ESVI_VII_changes_correlation_R_34[!is.na(ESVI_VII_changes_correlation_R_34)]

t.test(ESVI_VII_changes_correlation_R_34) # mean of 0.08614 and p value of 0.4919

##correlation between changes in ESVI and VII in Dem states, outliers exluded 
ESVI_VII_changes_correlation_D_32 <- data_meansincluded_Dlead$ESVI_Changes_VII_Changes_No_HI_RI

ESVI_VII_changes_correlation_D_32 <- ESVI_VII_changes_correlation_D_32[!is.na(ESVI_VII_changes_correlation_D_32)]

t.test(ESVI_VII_changes_correlation_D_32) #mean of -0.030899 and p value of 0.8459

##correlation between changes in ESVI and VII in Dem states, outliers included

ESVI_VII_changes_correlation_D_34 <- data_meansincluded_Dlead_34$ESVI_Changes_VII_Changes_HI_RI_Included

ESVI_VII_changes_correlation_D_34 <- ESVI_VII_changes_correlation_D_34[!is.na(ESVI_VII_changes_correlation_D_34)]

t.test(ESVI_VII_changes_correlation_D_34) # mean of -0.3171974 and p value of 0.8117

##differences

t.test(ESVI_VII_changes_correlation_R_32, ESVI_VII_changes_correlation_D_32)
cohensD(ESVI_VII_changes_correlation_R_32, ESVI_VII_changes_correlation_D_32)

###correlation between ESVI and VII for states where PVI was positive or negative (replicates their analysis)

mean(data_meansincluded_R_PVI$ESVI_VII_Cor_No_HI_RI_KA, na.rm= TRUE) #mean correlation for Repub PVI
#states btwn ESVI and VII is 0.551885, replicating BHS finding on page 8 - outliers excluded

t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_No_HI_RI_KA, na.rm= TRUE)

mean(data_meansincluded_D_PVI$ESVI_VII_Cor_No_HI_RI_KA, na.rm= TRUE) # mean correlation for Dem PVI
#states btwn ESVI and VII is -0.1205, replicating BHS finding on page 8 - outliers excluded

t.test(data_meansincluded_D_PVI$ESVI_VII_Cor_No_HI_RI_KA, na.rm= TRUE)

#differences - replicates BHS findings
t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_No_HI_RI_KA, data_meansincluded_D_PVI$ESVI_VII_Cor_No_HI_RI_KA, na.rm= TRUE)
cohensD(data_meansincluded_R_PVI$ESVI_VII_Cor_No_HI_RI_KA, data_meansincluded_D_PVI$ESVI_VII_Cor_No_HI_RI_KA)

mean(data_meansincluded_D_PVI$ESVI_VII_Cor_No_KA, na.rm= TRUE) # mean correlation for Dem PVI
#states btwn ESVI and VII is -0.21652, replicating BHS finding on page 8 - outliers included

t.test(data_meansincluded_D_PVI$ESVI_VII_Cor_No_KA, na.rm= TRUE)

#now doing the same thing, but for the 27 / 25 states that have enough data for use to calculate
# the changes

t.test(data_meansincluded_D_PVI$ESVI_VII_Cor_25, na.rm= TRUE)

t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_25, na.rm= TRUE)

t.test(data_meansincluded_D_PVI$ESVI_VII_Cor_28, na.rm= TRUE)

t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_25, data_meansincluded_D_PVI$ESVI_VII_Cor_25, na.rm= TRUE)

cohensD(data_meansincluded_R_PVI$ESVI_VII_Cor_25, data_meansincluded_D_PVI$ESVI_VII_Cor_25)

#For states with republican PVI's, what is the correlation between changes in ESVI and changes in VII?
#outliers excluded

ESVI_VII_changes_correlation_R_PVI_32 <- data_meansincluded_R_PVI$ESVI_Changes_VII_Changes_No_HI_RI

ESVI_VII_changes_correlation_R_PVI_32 <- ESVI_VII_changes_correlation_R_PVI_32[!is.na(ESVI_VII_changes_correlation_R_PVI_32)]

t.test(ESVI_VII_changes_correlation_R_PVI_32) #mean of 0.1295957 and p value of 0.3911

#now outliers included

ESVI_VII_changes_correlation_R_PVI_34 <- data_meansincluded_R_PVI$ESVI_Changes_VII_Changes_HI_RI_Included

ESVI_VII_changes_correlation_R_PVI_34 <- ESVI_VII_changes_correlation_R_PVI_34[!is.na(ESVI_VII_changes_correlation_R_PVI_34)]

t.test(ESVI_VII_changes_correlation_R_PVI_34) #mean of 0.1295957 and p value of 0.3911

#For states with DEM PVI's, what is the correlation between changes in ESVI and changes in VII?

ESVI_VII_changes_correlation_D_PVI_32 <- data_meansincluded_D_PVI$ESVI_Changes_VII_Changes_No_HI_RI

ESVI_VII_changes_correlation_D_PVI_32 <- ESVI_VII_changes_correlation_D_PVI_32[!is.na(ESVI_VII_changes_correlation_D_PVI_32)]

t.test(ESVI_VII_changes_correlation_D_PVI_32) #mean of -0.05181387 and p value of 0.7032

#now outliers included

ESVI_VII_changes_correlation_D_PVI_34 <- data_meansincluded_D_PVI$ESVI_Changes_VII_Changes_HI_RI_Included

ESVI_VII_changes_correlation_D_PVI_34 <- ESVI_VII_changes_correlation_D_PVI_34[!is.na(ESVI_VII_changes_correlation_D_PVI_34)]

t.test(ESVI_VII_changes_correlation_D_PVI_34) #mean of -0.05948725 and p value of 0.6679

#now comparing these two means(the prior analyses compared each of the means (R or R) to 0
#the below analyses compare the means to each other, just to make sure
#First Replicating the BHS t tests

# for PVI - repub vs dem states - outliers excluded - p value of 0.01168(means of .55 and -.12)
t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_No_HI_RI_KA, data_meansincluded_D_PVI$ESVI_VII_Cor_No_HI_RI_KA)

# for PVI - repub vs dem states - outliers included - p value of 0.002264 (means of .55 and -.22)
t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_No_KA, data_meansincluded_D_PVI$ESVI_VII_Cor_No_KA)

#now comparing the means of the changes 
#p value of 0.3665, means of .1295 and -0.05181, as found above
t.test(data_meansincluded_R_PVI$ESVI_Changes_VII_Changes_No_HI_RI, data_meansincluded_D_PVI$ESVI_Changes_VII_Changes_No_HI_RI)

cohensD(data_meansincluded_R_PVI$ESVI_Changes_VII_Changes_No_HI_RI, data_meansincluded_D_PVI$ESVI_Changes_VII_Changes_No_HI_RI)

#outliers included
##p value of 0.3411, means of .1295 and -0.04958725, as found above
t.test(data_meansincluded_R_PVI$ESVI_Changes_VII_Changes_HI_RI_Included, data_meansincluded_D_PVI$ESVI_Changes_VII_Changes_HI_RI_Included)


##now comparing the means of the changes - for states with R or D lead
#p value of 0.5593, means of 0.0861 and -0.030899 - outliers excluded
t.test(data_meansincluded_Rlead$ESVI_Changes_VII_Changes_No_HI_RI, data_meansincluded_Dlead$ESVI_Changes_VII_Changes_No_HI_RI)

#outliers included
# p value of 0.5149, means of 0.0861 and -0.03171974
t.test(data_meansincluded_Rlead_34$ESVI_Changes_VII_Changes_HI_RI_Included, data_meansincluded_Dlead_34$ESVI_Changes_VII_Changes_HI_RI_Included)

######supplemental calculations######

#calculates the mean correlation between ESVI and VII across 26 states (same states as used in our analysis of changes)
t.test(data_meansincluded$ESVI_VII_Cor_26)

#same thing, but including outliers

t.test(data_meansincluded$ESVI_VII_Cor_28)

#compare mean correlations for R and D lead states, 26 states only

t.test(data_meansincluded_Rlead$ESVI_VII_Cor_26, na.rm = TRUE) #mean correlation is 0.39

t.test(data_meansincluded_Dlead$ESVI_VII_Cor_26, na.rm = TRUE) # mean correlation is -0.08

#compare mean correlations for R and D lead states, 25 states only

t.test(data_meansincluded_R_PVI$ESVI_VII_Cor_25, na.rm = TRUE) #mean correlation is 0.43

t.test(data_meansincluded_D_PVI$ESVI_VII_Cor_25, na.rm = TRUE) # mean correlation is -0.12

###########code for the histogram of correlations###############

data_plots <- read.csv("Study2_States_SingleMeasures.csv", header=TRUE)

library(ggplot2)

 # for 32 vs 26 elections
p1 <- ggplot(data=data_plots, aes(data_plots$ESVI_VII_Cor_No_HI_RI_KA)) + 
 geom_histogram(breaks=seq(-1, 1, by = 0.05), 
 col="grey70", 
  fill="grey70", 
  alpha = .6) + 
  theme(plot.title = element_text(size=18, face="bold"),
   axis.title.x = element_text(face="bold", size=12), 
   axis.title.y = element_text(face="bold", size=12)) +
  labs(title="Correlations between ESVI and VII in original BHS data") +
labs(x="Correlation", y="Count") + 
xlim(c(-1,1)) +
  geom_density()

p2 <- ggplot(data=data_plots, aes(data_plots$ESVI_Changes_VII_Changes_No_HI_RI)) + 
  geom_histogram(breaks=seq(-1, 1, by = 0.05), 
                 col="grey70", 
                 fill="grey70", 
                 alpha = .6) + 
  theme(plot.title = element_text(size=18, face="bold"),
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12)) +
  labs(title="Correlations between ESVI and VII after removing 1st-order autocorrelation") +
  labs(x="Correlation", y="Count") + 
  xlim(c(-1,1)) +
  geom_density()

library (gridExtra)
grid.arrange(p1, p2)

# for 26 vs 26 elections
z1 <- ggplot(data=data_plots, aes(data_plots$ESVI_VII_Cor_26)) + 
  geom_histogram(breaks=seq(-1, 1, by = 0.05), 
                 col="grey70", 
                 fill="grey70", 
                 alpha = .6) + 
  theme(plot.title = element_text(size=18, face="bold"),
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12)) +
  labs(title="Correlations between ESVI and VII in original BHS data") +
  labs(x="Correlation", y="Count") + 
  xlim(c(-1,1)) +
  geom_density()

z2 <- ggplot(data=data_plots, aes(data_plots$ESVI_Changes_VII_Changes_No_HI_RI)) + 
  geom_histogram(breaks=seq(-1, 1, by = 0.05), 
                 col="grey70", 
                 fill="grey70", 
                 alpha = .6) + 
  theme(plot.title = element_text(size=18, face="bold"),
        axis.title.x = element_text(face="bold", size=12), 
        axis.title.y = element_text(face="bold", size=12)) +
  labs(title="Correlations between ESVI and VII after removing 1st-order autocorrelation") +
  labs(x="Correlation", y="Count") + 
  xlim(c(-1,1)) +
  geom_density()

library (gridExtra)
grid.arrange(z1, z2)
