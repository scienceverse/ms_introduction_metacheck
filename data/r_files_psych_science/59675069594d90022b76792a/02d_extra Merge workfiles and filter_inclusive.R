library(plyr)
library(reshape)
library(ggplot2)
library(Hmisc)

input.1 = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data"
setwd (input.1)

soep_fit_2005 <- read.delim("soep_fit_2005.csv")
soep_fit_2009 <- read.delim("soep_fit_2009.csv")
soep_fit_2013 <- read.delim("soep_fit_2013.csv")

soep_fit_2005$year <- 2005
soep_fit_2009$year <- 2009
soep_fit_2013$year <- 2013

merged050913 <- rbind.fill(soep_fit_2005, soep_fit_2009, soep_fit_2013)

workfile <- merged050913[!duplicated(merged050913$persnr),]
table(workfile$year)

# Export as csv file
setwd(input.1)
write.table(workfile, "soep_fit_050913.csv", sep="\t") 

#reverse-code N

workfile$per_s <- 8 - workfile$per_n

#switch order of N and S
workfile <- workfile[,c(1:6, 37, 8:36, 7)]

############### PREPARATIONS ########################

# Center age and education
workfile$age.c <- (scale(workfile$age))
workfile$educat.c <- (scale(workfile$educat))

# Why not work hours and experience as well?
workfile$experience.c <- scale(workfile$experience)
workfile$wrkhrs.c <- scale(workfile$wrkhrs)

# Create quadratic variables
workfile$age2 <- workfile$age.c*workfile$age.c
workfile$educat2 <- workfile$educat.c*workfile$educat.c

##"year" is included to correct for inflation (not necessary, however, because of the 2008 crisis)
workfile$year <- factor(workfile$year)

# Transform income variable 
hist(workfile[which(workfile$income<10000), "income"])
workfile$loginc = log(workfile$income+.01)

#create separate category for married people (because they earn a bit more)
workfile$unmarried <- 1
workfile[which(workfile$marstat=="married"), "unmarried"] <- 0

#create yearly income variable
workfile$income <- workfile$income
workfile$income_year <- 12*workfile$income

#make numeric value out of east factor (to make West the reference category)
workfile$east <- as.numeric(workfile$east)
workfile$east <- 1 - (workfile$east - 1)
table(workfile$east)

############### ESTABLISH COVARIATES ########################

#extra covariates = wrkstatus + work.full

fit <- lm(loginc ~ wrkstatus + work.full + year + east + female + age.c + age2 + educat.c + educat2 + foreign + unmarried + experience + wrkhrs, data=workfile)
summary(fit)

#conclusion: keep east + female + age.c + age2 + educat.c + educat2 + married + experience + wrkhrs (drop year, foreign)

############### FILTER SOME PEOPLE OUT ########################

# work.full = months worked (necessary in order to interpret yearly income)
# note: this eliminates a LOT of people!

table(workfile$wrkstatus)
nrow(workfile[which(workfile$wrkstatus=="nicht erwerbstaetig"),])
valid1 <- workfile[-which(workfile$wrkstatus=="nicht erwerbstaetig"),]

# Drop temporary workers and other work forms
table(valid1$wrkstatus)
nrow(valid1[-which(valid1$wrkstatus=="fulltime"),])
valid2 <- valid1[which(valid1$wrkstatus=="fulltime"),]
#toggle off by removing comment:
valid2 <- valid1

#Only select people who worked for 12 months
table(valid2$work.full)
nrow(valid2[-which(valid2$work.full=="12"),])
valid3 <- valid2[which(valid2$work.full=="12"),]
#toggle off by removing comment:
valid3 <- valid1

#Only select people with a coded job
nrow(valid3[is.na(valid3$ba2_e), ])
valid4 = valid3[complete.cases(valid3$ba2_e), ]

### Only individuals with valid data on big five, covariates, or outcome assessed in SOEP
summary(valid4[,c("wrkstatus", "work.full", "year", "ISCO", "income", "east", "loginc", "prestige", "female", "age", "age.c", "age2", "educat", "educat.c", "educat2", "unmarried", "experience", "experience.c", "wrkhrs", "wrkhrs.c", "per_e", "per_a", "per_c", "per_s", "per_o", "ba2_e", "ba2_a", "ba2_c", "ba2_s", "ba2_o")])

valid5 = na.omit(valid4[c("persnr", "wrkstatus", "work.full", "year", "ISCO", "income", "east", "loginc", "prestige", "female", "age", "age.c", "age2", "educat", "educat.c", "educat2", "unmarried", "experience", "experience.c", "wrkhrs", "wrkhrs.c", "per_e", "per_a", "per_c", "per_s", "per_o", "ba2_e", "ba2_a", "ba2_c", "ba2_s", "ba2_o")])
nrow(valid5)- nrow(valid4)

table(valid5$year)

################## CREATE DEPENDENT VARIABLE(S) ############################

#run a regression to obtain residuals
fit <- lm(loginc ~ wrkstatus + work.full + east + female + age.c + age2 + educat.c + educat2 + unmarried + experience.c + wrkhrs.c, data=valid5)
summary(fit)

# Compute standardized residuals 

resids <- as.data.frame(rstandard(fit))
names(resids) <- c("loginc.res")
valid5 <- cbind(valid5, resids)

# Examine distribution of residuals

hist(resids$loginc.res)
boxplot(resids$loginc.res)

plot(density(valid5$loginc.res), lwd = 2)
lines(density(valid5$loginc.res), col = 2)

nrow(valid5[-which(valid5$loginc.res>-5 & valid5$loginc.res<5),])
valid6 <- valid5[which(valid5$loginc.res>-5 & valid5$loginc.res<5),]
#hist(valid[which(valid$incbru<20000),"incbru"])

#write.table(valid6, "valid.csv", sep="\t")

##Added this for PsychScience revision
##Toggle the valid2 and valid3 exclusion criteria off
#toggle off by removing comment:
write.table(valid6, "valid_inclusive.csv", sep="\t")
