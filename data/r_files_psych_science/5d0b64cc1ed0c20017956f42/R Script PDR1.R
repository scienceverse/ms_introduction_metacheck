###################################
###        Loading data         ###
###################################

#Read in data from csv file and download needed packages
data <- read.csv2("Final_Data_PDR1.csv", header = TRUE)
data$Response.time <- as.numeric(as.character(data$Response.time))
names(data)[1] <- "Reported.outcome"
install.packages("effsize")
library(effsize)
install.packages("ltm")
library(ltm)
install.packages("psych")
library(psych)


################################
###   Exploratory analyses   ###
################################

#without any exclusions
#omit lines 36-40 and 96-97 and include 99-100

#The subsample that expressed (strong) belief in the payment scheme
#for this exploratory analysis, we only removed the hastag from line 26, the rest of the script remained the same
#data <- data[data$Several.students.will.receive.a.monetary.reward.for.the.dice.under.cup.game. == "Believe" | data$Several.students.will.receive.a.monetary.reward.for.the.dice.under.cup.game. == "Strongly believe",]


##################################
###        Demographics        ###
##################################

#the total sample
nrow(data)

#exclude participants of time pressure condition who failed to report within the time limit, omit these lines for Exploratory analysis without exclusions
nrow(data) - nrow(data[data$Exclusion == "0",]) 
data <- data[data$Exclusion == "0",]
n <- nrow(data)
n

#demographics after exclusions
prop.table(table(data$Gender))
mean(data$Age, na.rm = TRUE)
sd(data$Age, na.rm = TRUE)
sum(data$TP == "1")
sum(data$TP == "0")


###################################
###   Preregistered analyses    ###
###################################

#Effect of time pressure on reported die roll outcome
SP <- data[data$TP == "0",]$Reported.outcome
mean(SP)
sd(SP)
TP <- data[data$TP == "1",]$Reported.outcome
mean(TP)
sd(TP)
W <- wilcox.test(SP,TP,"less")
W
-qnorm(W$p.value)
rbc <- biserial.cor(data$Reported.outcome, data$TP)
rbc
r.test(n=n, r12=rbc, twotailed = FALSE)

#Was there cheating?
nrow(data[data$TP == "1",])
chisq.test(table(TP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
chi2 <- unname(chisq.test(table(TP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))$statistic)
nTP <- sum(data$TP == "1")
Vtp <- sqrt(chi2/nTP*1)
Vtp
nrow(data[data$TP == "0",])
chisq.test(table(SP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
chi2 <- unname(chisq.test(table(SP), p = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))$statistic)
nSP <- sum(data$TP == "0")
Vsp <- sqrt(chi2/nSP*1)
Vsp


###################################
###    Exploratory analyses     ###
###################################

#time pressure manipulation check
rtSP = data[data$TP == "0",]$Response.time
rtTP = data[data$TP == "1",]$Response.time
mSP <- mean(rtSP)
sdSP <- sd(rtSP)
maxSP <- mSP+5*sdSP
mTP <- mean(rtTP)
sdTP <- sd(rtTP)
maxTP <- mTP+5*sdTP
rtSP = data[data$TP == "0" & data$Response.time<maxSP,]$Response.time
rtTP = data[data$TP == "1" & data$Response.time<maxTP,]$Response.time
#Exporatory analysis without exclusions: use lines 99-100 instead of 96-97
#rtSP = data[data$TP == "0",]$Response.time
#rtTP = data[data$TP == "1",]$Response.time
mean(rtSP)
sd(rtSP)
mean(rtTP)
sd(rtTP)
t.test(rtSP,rtTP, var.equal = TRUE)
effsize::cohen.d(rtSP,rtTP)

#Self-report ratings
prop.table(table(data$The.ratio.between.the.dice.roll.and.the.possible.reward.is...))
prop.table(table(data$My.dice.role.was.fully.anonymous.only.I.could.know.what.I.rolled.))
mean(data$What.is.the.chance.that.you.will.get.the.reward.)
sd(data$What.is.the.chance.that.you.will.get.the.reward.)
prop.table(table(data$Several.students.will.receive.a.monetary.reward.for.the.dice.under.cup.game.))
