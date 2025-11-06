#rm(list=ls())

library(psych)
library(haven)
library(dplyr)
library(rje)
library(plyr)
library(ggplot2)

setwd("C:/Users/Ted/Desktop")
      
data <- read_sav("nzavs.sav", col_select = c("Questionnaire.Num",
               "Gender", "Cohort",
               "T01","Age.at.T01","NZREG.T01","NZSEI06.T01","Pol.Orient.T01",
               "Pol.SupGreens.T01","Env.Eff01.T01","Env.Eff02.T01","Env.EnvironmentValues.T01","Env.ClimateChgReal.T01","Env.ClimateChgCause.T01","Env.SacWilling.T01","Env.SacMade.T01","Env.RoutineWilling.T01","Env.RoutineMade.T01",
               "T1.EXTRAVERSION","T1.AGREEABLENESS","T1.CONSCIENTIOUSNESS","T1.NEUROTICISM","T1.OPENNESS","T1.HONESTY_HUMILITY",
               
               "T02","Age.at.T02","NZSEI06.T02","Pol.Orient.T02",
               "Pol.SupGreens.T02","Env.Eff01.T02","Env.Eff02.T02","Env.EnvironmentValues.T02","Env.ClimateChgReal.T02","Env.ClimateChgCause.T02","Env.SacWilling.T02","Env.SacMade.T02","Env.RoutineWilling.T02","Env.RoutineMade.T02",
               "T2.EXTRAVERSION","T2.AGREEABLENESS","T2.CONSCIENTIOUSNESS","T2.NEUROTICISM","T2.OPENNESS","T2.HONESTY_HUMILITY",
               
               "T03","Age.at.T03","NZSEI06.T03","Pol.Orient.T03",
               "Pol.SupGreens.T03","Env.Eff01.T03","Env.Eff02.T03","Env.EnvironmentValues.T03","Env.ClimateChgReal.T03","Env.ClimateChgCause.T03","Env.SacWilling.T03","Env.SacMade.T03","Env.RoutineWilling.T03","Env.RoutineMade.T03",
               "T3.EXTRAVERSION","T3.AGREEABLENESS","T3.CONSCIENTIOUSNESS","T3.NEUROTICISM","T3.OPENNESS","T3.HONESTY_HUMILITY",
               
               "T04","Age.at.T04","NZREG.T04","NZSEI06.T04","Pol.Orient.T04",
               "Pol.SupGreens.T04","Env.Eff01.T04","Env.Eff02.T04","Env.EnvironmentValues.T04","Env.ClimateChgReal.T04","Env.ClimateChgCause.T04","Env.SacWilling.T04","Env.SacMade.T04","Env.RoutineWilling.T04","Env.RoutineMade.T04",
               "T4.EXTRAVERSION","T4.AGREEABLENESS","T4.CONSCIENTIOUSNESS","T4.NEUROTICISM","T4.OPENNESS","T4.HONESTY_HUMILITY",
               
               "T05","Age.at.T05","NZREG.T05","NZSEI06.T05","Pol.Orient.T05",
               "Pol.SupGreens.T05","Env.Eff01.T05","Env.Eff02.T05","Env.ClimateChgReal.T05","Env.ClimateChgCause.T05","Env.ClimateChgConcern.T05","Env.SacWilling.T05","Env.SacMade.T05","Env.RoutineWilling.T05","Env.RoutineMade.T05",
               "T5.EXTRAVERSION","T5.AGREEABLENESS","T5.CONSCIENTIOUSNESS","T5.NEUROTICISM","T5.OPENNESS","T5.HONESTY_HUMILITY",
               
               "T06","Age.at.T06","NZREG.T06","NZSEI06.T06","Pol.Orient.T06",
               "Pol.SupGreens.T06","Env.Eff01.T06","Env.Eff02.T06","Env.ClimateChgReal.T06","Env.ClimateChgConcern.T06","Env.ClimateChgCause.T06",
               "T6.EXTRAVERSION","T6.AGREEABLENESS","T6.CONSCIENTIOUSNESS","T6.NEUROTICISM","T6.OPENNESS","T6.HONESTY_HUMILITY",
               
               "T07","Age.at.T07","NZREG.T07","NZSEI06.T07","Pol.Orient.T07",
               "Pol.SupGreens.T07","Env.EnvironmentValues.T07","Env.ClimateChgReal.T07","Env.ClimateChgConcern.T07","Env.ClimateChgCause.T07",
               "T7.EXTRAVERSION","T7.AGREEABLENESS","T7.CONSCIENTIOUSNESS","T7.NEUROTICISM","T7.OPENNESS","T7.HONESTY_HUMILITY",
               
               "T08","Age.at.T08","NZREG.T08","NZSEI06.T08","Pol.Orient.T08",
               "Pol.SupGreens.T08","Env.Eff01.T08","Env.Eff02.T08","Env.EnvironmentValues.T08","Env.ClimateChgReal.T08","Env.ClimateChgConcern.T08","Env.ClimateChgCause.T08",
               "T8.EXTRAVERSION","T8.AGREEABLENESS","T8.CONSCIENTIOUSNESS","T8.NEUROTICISM","T8.OPENNESS","T8.HONESTY_HUMILITY",
               
               "T09","Age.at.T09","NZREG.T09","NZSEI06.T09","Pol.Orient.T09",
               "Pol.SupGreens.T09","Env.Eff01.T09","Env.Eff02.T09","Env.ClimateChgReal.T09","Env.ClimateChgConcern.T09","Env.ClimateChgCause.T09",
               "T9.EXTRAVERSION","T9.AGREEABLENESS","T9.CONSCIENTIOUSNESS","T9.NEUROTICISM","T9.OPENNESS","T9.HONESTY_HUMILITY",
               
               "T10","Age.at.T10","NZREG.T10","NZSEI06.T10","Pol.Orient.T10",
               "Pol.SupGreens.T10","Env.ClimateChgReal.T10","Env.ClimateChgConcern.T10","Env.ClimateChgCause.T10",
               "T10.EXTRAVERSION","T10.AGREEABLENESS","T10.CONSCIENTIOUSNESS","T10.NEUROTICISM","T10.OPENNESS","T10.HONESTY_HUMILITY"
               ))

data$ID <- data$Questionnaire.Num

#calculate each person's TICs
data$minage <- rowMins(select(data, contains("Age.at")))
data$sminage <- scale(data$minage)
data$maxedu <- do.call(pmax, c(select(data, contains("NZREG")), list(na.rm=TRUE)))
data$avgses <- rowMeans(select(data, contains("NZSEI")), na.rm = TRUE)
data$maxses <- do.call(pmax, c(select(data, contains("NZSEI")), list(na.rm=TRUE)))
data$avgpol <- rowMeans(select(data, contains("Pol.Orient")), na.rm = TRUE)

#sort into groups
data$yearborn <- 2009 - data$Age.at.T01
data$generation <- NA
data$generation <- as.numeric(data$generation)
data[which(data$yearborn < 1965),]$generation <- 0
data[which(data$yearborn >= 1965 & data$yearborn < 1980),]$generation <- 1
data[which(data$yearborn > 1979),]$generation <- 2

data$decadeborn <- NA
data$decadeborn <- as.character(data$decadeborn)
data[which(data$yearborn <= 1929),]$decadeborn <- "1929 or earlier"
data[which(data$yearborn >= 1930 & data$yearborn < 1940),]$decadeborn <- "1930s"
data[which(data$yearborn >= 1940 & data$yearborn < 1950),]$decadeborn <- "1940s"
data[which(data$yearborn >= 1950 & data$yearborn < 1960),]$decadeborn <- "1950s"
data[which(data$yearborn >= 1960 & data$yearborn < 1970),]$decadeborn <- "1960s"
data[which(data$yearborn >= 1970 & data$yearborn < 1980),]$decadeborn <- "1970s"
data[which(data$yearborn >= 1980 & data$yearborn < 1990),]$decadeborn <- "1980s"
data[which(data$yearborn >= 1990),]$decadeborn <- "1990 or later"

data$conservative <- NA
data[which(data$avgpol < 4),]$conservative <- 0
data[which(data$avgpol > 4),]$conservative <- 1

data$college <- NA
data[which(data$maxedu >= 7),]$college <- 1
data[which(data$maxedu < 7),]$college <- 0

data$highses <- NA
data[which(data$maxses > mean(data$maxses, na.rm = TRUE)),]$highses <- 1
data[which(data$maxses < mean(data$maxses, na.rm = TRUE)),]$highses <- 0
ftable(data$highses)

tics <- select(data, ID, Cohort, Gender, minage, avgses, maxedu, generation, avgpol, highses, conservative, yearborn, decadeborn)

names(tics) <- c("ID","cohort","male","minage","avgses","maxedu","generation","avgpol","highses","conservative","yearborn","decadeborn")

#make data long
wave1 <- select(data, ID,"Age.at.T01",
                "Pol.SupGreens.T01","Env.Eff01.T01","Env.Eff02.T01","Env.EnvironmentValues.T01","Env.ClimateChgReal.T01","Env.ClimateChgCause.T01","Env.SacWilling.T01","Env.SacMade.T01","Env.RoutineWilling.T01","Env.RoutineMade.T01",
                "T1.EXTRAVERSION","T1.AGREEABLENESS","T1.CONSCIENTIOUSNESS","T1.NEUROTICISM","T1.OPENNESS","T1.HONESTY_HUMILITY")
names(wave1) <- c("ID","age","Egreens","Eeff1","Eeff2","Evalue","Echgreal","Echgcause","Esacwilling","Esacmade","Eroutwilling","Eroutmade","ext","agr","con","neu","ope","hon")

wave2 <- select(data, ID,"Age.at.T02",
                "Pol.SupGreens.T02","Env.Eff01.T02","Env.Eff02.T02","Env.EnvironmentValues.T02","Env.ClimateChgReal.T02","Env.ClimateChgCause.T02","Env.SacWilling.T02","Env.SacMade.T02","Env.RoutineWilling.T02","Env.RoutineMade.T02",
                "T2.EXTRAVERSION","T2.AGREEABLENESS","T2.CONSCIENTIOUSNESS","T2.NEUROTICISM","T2.OPENNESS","T2.HONESTY_HUMILITY")
names(wave2) <- c("ID","age","Egreens","Eeff1","Eeff2","Evalue","Echgreal","Echgcause","Esacwilling","Esacmade","Eroutwilling","Eroutmade","ext","agr","con","neu","ope","hon")

wave3 <- select(data, ID,"Age.at.T03",
                "Pol.SupGreens.T03","Env.Eff01.T03","Env.Eff02.T03","Env.EnvironmentValues.T03","Env.ClimateChgReal.T03","Env.ClimateChgCause.T03","Env.SacWilling.T03","Env.SacMade.T03","Env.RoutineWilling.T03","Env.RoutineMade.T03",
                "T3.EXTRAVERSION","T3.AGREEABLENESS","T3.CONSCIENTIOUSNESS","T3.NEUROTICISM","T3.OPENNESS","T3.HONESTY_HUMILITY")
names(wave3) <- c("ID","age","Egreens","Eeff1","Eeff2","Evalue","Echgreal","Echgcause","Esacwilling","Esacmade","Eroutwilling","Eroutmade","ext","agr","con","neu","ope","hon")

wave4 <- select(data, ID,"Age.at.T04",
                "Pol.SupGreens.T04","Env.Eff01.T04","Env.Eff02.T04","Env.EnvironmentValues.T04","Env.ClimateChgReal.T04","Env.ClimateChgCause.T04","Env.SacWilling.T04","Env.SacMade.T04","Env.RoutineWilling.T04","Env.RoutineMade.T04",
                "T4.EXTRAVERSION","T4.AGREEABLENESS","T4.CONSCIENTIOUSNESS","T4.NEUROTICISM","T4.OPENNESS","T4.HONESTY_HUMILITY")
names(wave4) <- c("ID","age","Egreens","Eeff1","Eeff2","Evalue","Echgreal","Echgcause","Esacwilling","Esacmade","Eroutwilling","Eroutmade","ext","agr","con","neu","ope","hon")

wave5 <- select(data, ID,"Age.at.T05",
                "Pol.SupGreens.T05","Env.Eff01.T05","Env.Eff02.T05","Env.ClimateChgReal.T05","Env.ClimateChgCause.T05","Env.ClimateChgConcern.T05","Env.SacWilling.T05","Env.SacMade.T05","Env.RoutineWilling.T05","Env.RoutineMade.T05",
                "T5.EXTRAVERSION","T5.AGREEABLENESS","T5.CONSCIENTIOUSNESS","T5.NEUROTICISM","T5.OPENNESS","T5.HONESTY_HUMILITY")
names(wave5) <- c("ID","age","Egreens","Eeff1","Eeff2","Echgreal","Echgcause","Echgconcern","Esacwilling","Esacmade","Eroutwilling","Eroutmade","ext","agr","con","neu","ope","hon")

wave6 <- select(data, ID,"Age.at.T06",
                "Pol.SupGreens.T06","Env.Eff01.T06","Env.Eff02.T06","Env.ClimateChgReal.T06","Env.ClimateChgConcern.T06","Env.ClimateChgCause.T06",
                "T6.EXTRAVERSION","T6.AGREEABLENESS","T6.CONSCIENTIOUSNESS","T6.NEUROTICISM","T6.OPENNESS","T6.HONESTY_HUMILITY")
names(wave6) <- c("ID","age","Egreens","Eeff1","Eeff2","Echgreal","Echgconcern","Echgcause","ext","agr","con","neu","ope","hon")

wave7 <- select(data, ID,"Age.at.T07",
                "Pol.SupGreens.T07","Env.EnvironmentValues.T07","Env.ClimateChgReal.T07","Env.ClimateChgConcern.T07","Env.ClimateChgCause.T07",
                "T7.EXTRAVERSION","T7.AGREEABLENESS","T7.CONSCIENTIOUSNESS","T7.NEUROTICISM","T7.OPENNESS","T7.HONESTY_HUMILITY")
names(wave7) <- c("ID","age","Egreens","Evalue","Echgreal","Echgconcern","Echgcause","ext","agr","con","neu","ope","hon")

wave8 <- select(data, ID,"Age.at.T08",
                "Pol.SupGreens.T08","Env.Eff01.T08","Env.Eff02.T08","Env.EnvironmentValues.T08","Env.ClimateChgReal.T08","Env.ClimateChgConcern.T08","Env.ClimateChgCause.T08",
                "T8.EXTRAVERSION","T8.AGREEABLENESS","T8.CONSCIENTIOUSNESS","T8.NEUROTICISM","T8.OPENNESS","T8.HONESTY_HUMILITY")
names(wave8) <- c("ID","age","Egreens","Eeff1","Eeff2","Evalue","Echgreal","Echgconcern","Echgcause","ext","agr","con","neu","ope","hon")

wave9 <- select(data, ID,"Age.at.T09",
                "Pol.SupGreens.T09","Env.Eff01.T09","Env.Eff02.T09","Env.ClimateChgReal.T09","Env.ClimateChgConcern.T09","Env.ClimateChgCause.T09",
                "T9.EXTRAVERSION","T9.AGREEABLENESS","T9.CONSCIENTIOUSNESS","T9.NEUROTICISM","T9.OPENNESS","T9.HONESTY_HUMILITY")
names(wave9) <- c("ID","age","Egreens","Eeff1","Eeff2","Echgreal","Echgconcern","Echgcause","ext","agr","con","neu","ope","hon")

wave10 <- select(data, ID,"Age.at.T10",
                "Pol.SupGreens.T10","Env.ClimateChgReal.T10","Env.ClimateChgConcern.T10","Env.ClimateChgCause.T10",
                "T10.EXTRAVERSION","T10.AGREEABLENESS","T10.CONSCIENTIOUSNESS","T10.NEUROTICISM","T10.OPENNESS","T10.HONESTY_HUMILITY")
names(wave10) <- c("ID","age","Egreens","Echgreal","Echgconcern","Echgcause","ext","agr","con","neu","ope","hon")

wave1$year <- 2009
wave2$year <- 2010
wave3$year <- 2011
wave4$year <- 2012
wave5$year <- 2013
wave6$year <- 2014
wave7$year <- 2015
wave8$year <- 2016
wave9$year <- 2017
wave10$year <- 2018

wave1$yearcode  <- "X09"
wave2$yearcode  <- "X10"
wave3$yearcode  <- "X11"
wave4$yearcode  <- "X12"
wave5$yearcode  <- "X13"
wave6$yearcode  <- "X14"
wave7$yearcode  <- "X15"
wave8$yearcode  <- "X16"
wave9$yearcode  <- "X17"
wave10$yearcode <- "X18"

long <- rbind.fill(wave1,wave2,wave3,wave4,wave5,wave6,wave7,wave8,wave9,wave10)

#correct weird values scale
ftable(!is.na(long$Evalue))
ftable(long$Evalue)
long[which(long$Evalue == -1),]$Evalue <- NA
long[which(long$Evalue == .1),]$Evalue <- NA

long$Eeff <- rowMeans(select(long, Eeff1, Eeff2), na.rm = TRUE)
long$Esac <- rowMeans(select(long, Esacwilling, Esacmade, Eroutwilling, Eroutmade), na.rm = TRUE)
long$Ebelief <- rowMeans(select(long, Echgreal, Echgcause), na.rm = TRUE)

#standardize variables
long$sEeff <- scale(long$Eeff)
long$sEsac <- scale(long$Esac)
long$sEbelief <- scale(long$Ebelief)
long$sEgreens <- scale(long$Egreens)
long$sEvalue <- scale(long$Evalue)
long$sEchgconcern <- scale(long$Echgconcern)
long$sext <- scale(long$ext)
long$sagr <- scale(long$agr)
long$scon <- scale(long$con)
long$sneu <- scale(long$neu)
long$sope <- scale(long$ope)
long$shon <- scale(long$hon)

#mean-center environmentalism variables
long$mEeff <- (long$Eeff - mean(filter(long, year == 2009 )$Eeff, na.rm = TRUE)) / sd(long$Eeff, na.rm = TRUE)
long$mEsac <- (long$Esac - mean(filter(long, year == 2009 )$Esac, na.rm = TRUE)) / sd(long$Esac, na.rm = TRUE)
long$mEbelief <- (long$Ebelief - mean(filter(long, year == 2009 )$Ebelief, na.rm = TRUE)) / sd(long$Ebelief, na.rm = TRUE)
long$mEgreens <- (long$Egreens - mean(filter(long, year == 2009 )$Egreens, na.rm = TRUE)) / sd(long$Egreens, na.rm = TRUE)
long$mEvalue <- (long$Evalue - mean(filter(long, year == 2009 )$Evalue, na.rm = TRUE)) / sd(long$Evalue, na.rm = TRUE)
long$mEchgconcern <- (long$Echgconcern - mean(filter(long, year == 2013 )$Echgconcern, na.rm = TRUE)) / sd(long$Echgconcern, na.rm = TRUE)

#readd time invariant covariates to long data
longfull <- left_join(long, tics, by = "ID")
  
#make wide data
library(reshape2)
mean2 <- function(x) {mean(x, na.rm = TRUE)}

data_w <- melt(long, id = c("ID","yearcode"))
data_x <- dcast(data_w, formula = ID ~ yearcode + ..., fun = mean2)
names(data_x)
widefull <- left_join(data_x, tics, by = "ID")

#write.csv(longfull, "nzavs_long2.csv")
#write.csv(widefull, "nzavs_wide2.csv")

