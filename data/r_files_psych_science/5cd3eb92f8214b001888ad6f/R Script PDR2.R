###################################
###        Loading data         ###
###################################

#Read in data from csv file 
data <- read.csv2("PDR2_Final_Data.csv", header = TRUE)
data$Time <- as.character(data$Time)
data$Time <- as.numeric(data$Time)
data$Age <- as.character(data$Age)
data$Age <- as.numeric(data$Age)
install.packages("effsize")
library(effsize)
install.packages("ltm")
library(ltm)
install.packages("psych")
library(psych)
install.packages("dplyr")
library(dplyr)


####################################
###     Exploratory analyses     ###
####################################

#Exploratory1 without exlusions
#omit lines 39-42

#Exploratory2 only native language
#data <- data[data$Native_Nonnative == 1,]

#Exploratory3 only nonnative language
#data <- data[data$Native_Nonnative == 0,]

################################
###       Demographics       ###
################################

#Exclude participants in time pressure condition who failed to report within time limit, omit these lines for exploratory1 without exclusions
DataSP <- data[data$Condition == 0,]
DataTP <- data[data$Condition == 1 & data$Time < 8.01,]
data <- rbind(DataSP, DataTP)
n <- nrow(data)

#Demographics total sample
n <- nrow(data)
n
prop.table(table(data$SexQ))
mean(data$Age, na.rm = TRUE)
sd(data$Age, na.rm = TRUE)
prop.table(table(data$Nationality))
prop.table(table(data$MotherTongue))

#Demographics Time Pressure Condition
sum(data$Condition == "1")
prop.table(table(data$SexQ[data$Condition == "1"]))
mean(data$Age[data$Condition == "1"], na.rm = TRUE)
sd(data$Age[data$Condition == "1"], na.rm = TRUE)

#Demographics Self Paced Condition
sum(data$Condition == "0")
prop.table(table(data$SexQ[data$Condition == "0"]))
mean(data$Age[data$Condition == "0"], na.rm = TRUE)
sd(data$Age[data$Condition == "0"], na.rm = TRUE)


######################################
###     Preregistered analyses     ###
######################################

#Time pressure manipulation check
timeSP = data[data$Condition == "0",]$Time
timeTP = data[data$Condition == "1",]$Time
mean(timeSP)
sd(timeSP)
mean(timeTP)
sd(timeTP)
t.test(timeSP,timeTP, var.equal = TRUE)
effsize::cohen.d(timeSP,timeTP)

#Effect of time pressure on reported die roll outcome
SP <- data[data$Condition == "0",]$reported.roll
mean(SP)
sd(SP)
TP <- data[data$Condition == "1",]$reported.roll
mean(TP)
sd(TP)
W <- wilcox.test(SP,TP,"less")
W
-qnorm(W$p.value)
rbc <- biserial.cor(data$reported.roll, data$Condition)
rbc
r.test(n=n, r12=rbc, twotailed=FALSE)

#Was there cheating?
nrow(data[data$Condition == "1",])
chisq.test(table(TP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
chi2 <- unname(chisq.test(table(TP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))$statistic)
nTP <- sum(data$Condition == "1")
Vtp <- sqrt(chi2/nTP*1)
Vtp
nrow(data[data$Condition == "0",])
chisq.test(table(SP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
chi2 <- unname(chisq.test(table(SP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))$statistic)
nSP <- sum(data$Condition == "0")
Vsp <- sqrt(chi2/nSP*1)
Vsp

###################################
###   Exploratory self-report   ###
###################################

data$ChanceReward <- as.character(data$ChanceReward)
data$ChanceReward <- as.numeric(data$ChanceReward)

prop.table(table(data$The.ratio.between.the.dice.roll.and.the.possible.extra.reward.is...))
prop.table(table(data$My.dice.role.was.fully.anonymous...only.I.could.know.what.I.rolled.))
mean(data$ChanceReward, na.rm=TRUE)
sd(data$ChanceReward, na.rm=TRUE)
prop.table(table(data$Several.students.will.receive.an.extra.monetary.reward.for.the.dice.under.cup.task.))
data$What.was.your.perceived.time.pressure.during.the.die.roll. <- recode(data$What.was.your.perceived.time.pressure.during.the.die.roll., "Very low" = 1, "Low" = 2, "Neutral" = 3, "High" = 4, "Very high" = 5)
perceivedtimeSP <- data[data$Condition == "0",]$What.was.your.perceived.time.pressure.during.the.die.roll.
perceivedtimeTP <- data[data$Condition == "1",]$What.was.your.perceived.time.pressure.during.the.die.roll.
mean(perceivedtimeTP)
sd(perceivedtimeTP)
mean(perceivedtimeSP)
sd(perceivedtimeSP)
t.test(perceivedtimeTP,perceivedtimeSP, var.equal = TRUE)
effsize::cohen.d(perceivedtimeTP,perceivedtimeSP)
kruskal.test(reported.roll ~ What.was.your.perceived.time.pressure.during.the.die.roll., data = DataTP)

###################################
###      PiratePlot code        ###
###################################

#Install Yarrr Package

install.packages("yarrr")
library(yarrr)

#Reorder factor

data$What.was.your.perceived.time.pressure.during.the.die.roll. <- factor(data$What.was.your.perceived.time.pressure.during.the.die.roll., levels = c("Very low", "Low", "Neutral", "High", "Very high")) 

#Compute pirateplot

pirateplot(formula = reported.roll ~ What.was.your.perceived.time.pressure.during.the.die.roll., 
            data = data, 
            main = "", 
            gl.col = "white", 
            point.o = 0.5, 
            inf.method = 'ci',
            xlab = "Perceived Time Pressure", 
            ylab = "Reported Die Roll Outcome", 
            yaxt = "n", 
            pal = "gray") 


 #Add a custom axis 
 axis(2, at = seq(from = 1, to = 6, by = 1)) 



