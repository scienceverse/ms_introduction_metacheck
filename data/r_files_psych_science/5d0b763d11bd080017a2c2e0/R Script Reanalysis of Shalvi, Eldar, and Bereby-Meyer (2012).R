###################################
###        Loading data         ###
###################################

#Read in data from csv file and print first 6 rows
data <- read.csv2("DataOriginalStudies.csv", header = TRUE, sep=";", dec=".")
data$Dice_report <- as.numeric(as.character(data$Dice_report))
head(data)
install.packages("effsize")
library(effsize)
install.packages("ltm")
library(ltm)
install.packages("psych")
library(psych)

#exclude participants of time pressure condition who failed to report within the time limit
data <- data[data$Within_time == "1",]


###################################
###        Choose study         ###
###################################

#remove the # from the Experiment you want to reproduce
#data <- data[data$Experiment == "1",]
#data <- data[data$Experiment == "2",]

###################################
###     Manipulation check      ###
###################################

n <- nrow(data)
n

#make vector containing times spent on task of every participant
rtSP = data[data$Time_pressure == "0",]$Report_time
rtTP = data[data$Time_pressure == "1",]$Report_time

#ckeck mean and standard error of times spent on task 
mean(rtSP)
sd(rtSP)
mean(rtTP)
sd(rtTP)

#check for significant difference in time spent on task between time-pressure and self-paced
t.test(rtTP,rtSP, var.equal = TRUE)
effsize::cohen.d(rtSP,rtTP)

###################################
###        Main analysis        ###
###################################

TP <- data[data$Time_pressure == "1",]$Dice_report
mean(TP)
sd(TP)
SP <- data[data$Time_pressure == "0",]$Dice_report
mean(SP)
sd(SP)

#examining whether the reported dice roll outcomes are higher in the time-pressure condition than in the no time-pressure condition
W <- wilcox.test(TP,SP)
W
-qnorm(W$p.value/2)
effsize::cohen.d(TP,SP)
rbc <- biserial.cor(data$Dice_report, data$Time_pressure)
rbc
r.test(n=n, r12=rbc, twotailed=FALSE)

#comparing distribution of the reported dice roll outcomes with the expected distribution of dice roll outcomes by chance
nrow(data[data$Time_pressure == "1",])
chisq.test(table(TP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
nrow(data[data$Time_pressure == "0",])
chisq.test(table(SP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

