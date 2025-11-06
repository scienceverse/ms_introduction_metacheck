#### The Golden Age Is Behind Us: How the Status Quo Impacts the Evaluation of Technology ####
### Analytic Syntax ###
### Working Directories - *Make sure to set working directory before running code*

### Install (if not already installed) and load all packages
if(!require("psych")) {install.packages("psych"); require("psych")}
if(!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}
if(!require("effsize")) {install.packages("effsize"); require("effsize")}
if(!require("stats")) {install.packages("stats"); require("stats")}
if(!require("ggpubr")) {install.packages("ggpubr"); require("ggpubr")}
if(!require("lmerTest")) {install.packages("lmerTest"); require("lmerTest")}
if(!require("inflection")) {install.packages("inflection"); require("inflection")}
if(!require("sjPlot")) {install.packages("sjPlot"); require("sjPlot")}
if(!require("svglite")) {install.packages("svglite"); require("svglite")}

#### Pilot Study #### -----------------------------------------------------------

### DATA WRANGLING & CLEANING ###

## Read in data and fix first column name ("Subject")
pilot.wide.df <- read.csv(file="TechAgePilot.csv")
names(pilot.wide.df)[1] <- "Subject"

table(pilot.wide.df$Gender)
mean(pilot.wide.df$Age)
sd(pilot.wide.df$Age)

## Change data from wide to Long
## Each row now represents a single evaluation
pilot.df <- pilot.wide.df %>%  gather(key = "Item", value = "Evaluation", Item.1:Item.10) %>%
  gather(key = "Date", value = "Estimated_Date", Date.1:Date.10) %>%
  filter(substring(Item,6) == substring(Date,6))
  
## Age at Invention based on independent research
pilot.df$AgeAtInvention <- ifelse(pilot.df$Item == "Item.1", pilot.df$Age - 30,
                          ifelse(pilot.df$Item == "Item.2", pilot.df$Age - 47, 
                          ifelse(pilot.df$Item == "Item.3", pilot.df$Age - 19,
                          ifelse(pilot.df$Item == "Item.4", pilot.df$Age - 18,
                          ifelse(pilot.df$Item == "Item.5", pilot.df$Age - 21,
                          ifelse(pilot.df$Item == "Item.6", pilot.df$Age - 49,
                          ifelse(pilot.df$Item == "Item.7", pilot.df$Age - 113,
                          ifelse(pilot.df$Item == "Item.8", pilot.df$Age - 67,
                          ifelse(pilot.df$Item == "Item.9", pilot.df$Age - 75,
                          ifelse(pilot.df$Item == "Item.10", pilot.df$Age - 24,       
                            NA))))))))))

## Age at Invention based on participants' estimates
pilot.df$SelfRatedAgeAtInvention <- pilot.df$Age - (2021 - pilot.df$Estimated_Date)


#Relabel
pilot.df$Item[pilot.df$Item=="Item.1"] <- "Internet"
pilot.df$Item[pilot.df$Item=="Item.2"] <- "Computer"
pilot.df$Item[pilot.df$Item=="Item.3"] <- "Smartphone"
pilot.df$Item[pilot.df$Item=="Item.4"] <- "Social Media"
pilot.df$Item[pilot.df$Item=="Item.5"] <- "GPS"
pilot.df$Item[pilot.df$Item=="Item.6"] <- "Microwave"
pilot.df$Item[pilot.df$Item=="Item.7"] <- "Car"
pilot.df$Item[pilot.df$Item=="Item.8"] <- "Solar Panels"
pilot.df$Item[pilot.df$Item=="Item.9"] <- "Televisions"
pilot.df$Item[pilot.df$Item=="Item.10"] <- "CDs"

drop1 <- "Date"
pilot.df<-pilot.df[,!names(pilot.df) %in% drop1]
headTail(pilot.df)
pilot.df<-pilot.df[!is.na(pilot.df$Evaluation), ] ## Remove NAs
### END DATA WRANGLING & CLEANING

## Plots
## Independent Dates
# All results
all1 <- ggplot(aes(x = AgeAtInvention, y = Evaluation), data = pilot.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 80, by = 10)) +
  geom_smooth()
all1

#By Item
items1 <- ggplot(aes(x = AgeAtInvention, y = Evaluation, colour = Item), data = pilot.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 80, by = 10)) +
  geom_smooth(se=F)
items1

## Participants' Estimated Dates

pilot.plot <- ggplot(aes(x = SelfRatedAgeAtInvention, y = Evaluation), data = pilot.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 60, by = 10), limits = c(-100, 60)) +
  geom_smooth(method = "gam") +
  geom_vline(xintercept = 2.28, color = "red") +
  xlab("Participants' perceived age at invention") +
  ylab("Evaluation of technology")
pilot.plot

ggsave(file="pilot.svg", plot= pilot.plot, width=8, height=5)


## CALCULATE INFLECTION POINT (ESE)
y.vals <- ggplot_build(pilot.plot)$data[[1]]$y
x.vals <- ggplot_build(pilot.plot)$data[[1]]$x

## x vals where there is a change of direction
change.dir <- c(FALSE, diff(diff(y.vals)>0)!=0)
x.vals[change.dir]

## Find inflection point between second and third direction change
cc <- check_curve(x.vals[44:67], y.vals[44:67]);cc
ipbese <- bese(x.vals[44:67], y.vals[44:67], index = 1); ipbese
## ESE is 2.28

## Plot for paper
pilot.plot2 <- ggplot(aes(x = SelfRatedAgeAtInvention, y = Evaluation), data = pilot.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 60, by = 10), limits = c(-100, 60)) +
  coord_cartesian(xlim = c(-30,50)) +
  geom_smooth(method = "gam") +
  geom_vline(xintercept = 2.28, color = "red") +
  xlab("Participants' perceived age at invention") +
  ylab("Evaluation of technology")
ggsave(file="pilot2.svg", plot= pilot.plot2, width=8, height=5)

#### END Pilot Study

#### Experiment 1 - Aerogel (S1) #### ------------------------------------------------------
s1.df <- read.csv(file = "TechAgeS1.csv")

### DATA WRANGLING & CLEANING ###

## Participant Info
s1.df$Gender <- factor(s1.df$Gender,labels=c("Male","Female","Other"))
names(s1.df)[1] <- "Subject"

### END DATA WRANGLING & CLEANING ###

## Demographic information
table(s1.df$Gender)
mean(s1.df$Age)
sd(s1.df$Age)

### Analysis WITHOUT exclusions ###
## Descriptive statistics
s1.cond.difs <- s1.df %>% group_by(Condition) %>% summarise(EVAL = mean(Eval), EVAL_SD = sd(Eval))
s1.cond.difs

## T-test and Cohen's d
## Condition refers to born after/before invention (experimentally manipulated)
t.test(Eval ~ Condition, var.equal=TRUE, data=s1.df)
effsize::cohen.d(Eval ~ Condition, data = s1.df)

### END Analysis WITHOUT exclusions

### Analysis WITH exclusions ###
## Exclusion
table(s1.df$Check) # n = 39
table(s1.df$Knowledge) # n = 33
# Exclude participants who failed manipulation check or knew what aerogel was
s1.df.f <- s1.df %>% filter(Check == "Pass" & Knowledge > 1)

## Descriptive statistics
s1.cond.difs <- s1.df.f %>% group_by(Condition) %>% summarise(EVAL = mean(Eval), EVAL_SD = sd(Eval))
s1.cond.difs

## T-test and Cohen's d 
t.test(Eval ~ Condition, var.equal=TRUE, data=s1.df.f)
effsize::cohen.d(Eval ~ Condition, data = s1.df.f)

## Age and Evaluation correlation
cor.test(s1.df.f$Age, s1.df.f$Eval)

## Make Condition.Num numeric so it can be scaled in lm
s1.df.f$Condition.Num <- rep(NA, length(s1.df.f$Condition))
s1.df.f$Condition.Num[s1.df.f$Condition=="Born After"] <- 0
s1.df.f$Condition.Num[s1.df.f$Condition=="Born Before"] <- 1

## Age and Condition multilevel model
s1.lm.age <- lm(scale(Eval) ~ scale(Age) + scale(Condition.Num), data = s1.df.f)
summary(s1.lm.age)

#### END Experiment 1 - Aerogel (S1)

#### Study 2 (Pre-registered) #### ---------------------------------------------------
s2.wide.df <- read.csv(file="TechAgeS2.csv")

## Demographic information
table(s2.wide.df$Gender)
mean(s2.wide.df$Age)
sd(s2.wide.df$Age)

### DATA WRANGLING & CLEANING ###
## Name first column "Subject"
names(s2.wide.df)[1] <- "Subject"

## Transform data from wide to long format
## Each row now represents a single eval
s2.df <- s2.wide.df %>%  gather(key = "Item", value = "Eval", Item.1:Item.10) %>%
  gather(key = "Exposure", value = "Age_Of_Exposure", Exposure.1:Exposure.10) %>%
  gather(key = "Date", value = "Estimated_Date", Date.1:Date.10) %>%
  gather(key = "Freq", value = "Frequency", Freq.1:Freq.10)%>%
  filter(substring(Item,6) == substring(Exposure,10), substring(Date,6) == substring(Freq,6), substring(Item,6) == substring(Date,6))

## Age at Invention based on independent research (Method 1)
s2.df$AgeAtInvention <- ifelse(s2.df$Item == "Item.1",s2.df$Age - 38,
                          ifelse(s2.df$Item == "Item.2",s2.df$Age - 13, 
                          ifelse(s2.df$Item == "Item.3",s2.df$Age - 46,
                          ifelse(s2.df$Item == "Item.4",s2.df$Age - 22,
                          ifelse(s2.df$Item == "Item.5",s2.df$Age - 22,
                          ifelse(s2.df$Item == "Item.6",s2.df$Age - 50,
                          ifelse(s2.df$Item == "Item.7",s2.df$Age - 54,
                          ifelse(s2.df$Item == "Item.8",s2.df$Age - 6,
                          ifelse(s2.df$Item == "Item.9",s2.df$Age - 12,
                          ifelse(s2.df$Item == "Item.10",s2.df$Age - 50,       
                                 NA))))))))))



#Relabel
s2.df$Item[s2.df$Item=="Item.1"] <- "Cell Phone"
s2.df$Item[s2.df$Item=="Item.2"] <- "Electric Car"
s2.df$Item[s2.df$Item=="Item.3"] <- "Laptop"
s2.df$Item[s2.df$Item=="Item.4"] <- "Bluetooth"
s2.df$Item[s2.df$Item=="Item.5"] <- "WiFi"
s2.df$Item[s2.df$Item=="Item.6"] <- "Email"
s2.df$Item[s2.df$Item=="Item.7"] <- "Drone"
s2.df$Item[s2.df$Item=="Item.8"] <- "Self-driving Car"
s2.df$Item[s2.df$Item=="Item.9"] <- "Blockchain"
s2.df$Item[s2.df$Item=="Item.10"] <- "Videogames"

## Remove estimated dates pre 1900 or post 2021 (for Method 2)
s2.df$Estimated_Date1900 <- s2.df$Estimated_Date
s2.df$Estimated_Date1000 <- s2.df$Estimated_Date
s2.df$Estimated_Date1900[s2.df$Estimated_Date<1900 | s2.df$Estimated_Date>2021] <- NA
s2.df$Estimated_Date1000[s2.df$Estimated_Date<1000 | s2.df$Estimated_Date>2021] <- NA

## Age at Invention based on individuals' estimates of invention date (Method 2)
s2.df$AgeAtInvention2_1900 <- s2.df$Age - (2021 - s2.df$Estimated_Date1900)
s2.df$AgeAtInvention2_1000 <- s2.df$Age - (2021 - s2.df$Estimated_Date1000)

drop <- c("Exposure","Date","Freq")
s2.df<-s2.df[,!names(s2.df) %in% drop]
headTail(s2.df)

## Create variable AgeOver2 (Method 1)
s2.df$AgeOver2 <- rep(NA, length(s2.df$AgeAtInvention))
s2.df$AgeOver2[s2.df$AgeAtInvention <= 2] <- 0
s2.df$AgeOver2[s2.df$AgeAtInvention > 2] <- 1
## Create variable AgeOver0 (Method 1)
s2.df$AgeOver0 <- rep(NA, length(s2.df$AgeAtInvention))
s2.df$AgeOver0[s2.df$AgeAtInvention <= 0] <- 0
s2.df$AgeOver0[s2.df$AgeAtInvention > 0] <- 1

## Create variable AgeOver2 (Method 2) - exclude pre 1900 (pr)
s2.df$AgeOver2M2_1900 <- rep(NA, length(s2.df$AgeAtInvention2_1900))
s2.df$AgeOver2M2_1900[s2.df$AgeAtInvention2_1900 <= 2] <- 0
s2.df$AgeOver2M2_1900[s2.df$AgeAtInvention2_1900 > 2] <- 1

## Create variable AgeOver2 (Method 2) - exclude pre 1000
s2.df$AgeOver2M2_1000 <- rep(NA, length(s2.df$AgeAtInvention2_1000))
s2.df$AgeOver2M2_1000[s2.df$AgeAtInvention2_1000 <= 2] <- 0
s2.df$AgeOver2M2_1000[s2.df$AgeAtInvention2_1000 > 2] <- 1

## Create variable AgeOver0 (Method 2)
s2.df$AgeOver0M2 <- rep(NA, length(s2.df$AgeAtInvention2_1900))
s2.df$AgeOver0M2[s2.df$AgeAtInvention2_1900 <= 0] <- 0
s2.df$AgeOver0M2[s2.df$AgeAtInvention2_1900 > 0] <- 1

### END DATA WRANGLING & CLEANING

### Pre-registered analyses ###
## Mixed effects model age over 2 binary variable (Method 1)
s2.AgeOver2.M1 <- lmer(scale(Eval) ~ scale(AgeOver2) + (1|Subject), data = s2.df)
summary(s2.AgeOver2.M1)
## Bootstrap Confidence Intervals
BS.s2.AgeOver2.M1 <- confint.merMod(s2.AgeOver2.M1, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.AgeOver2.M1


## Mixed effects model age over 2 binary variable (Method 2) - 1900 cutoff (pr)
s2.AgeOver2.M2_1900 <- lmer(scale(Eval) ~ scale(AgeOver2M2_1900) + (1|Subject), data = s2.df)
summary(s2.AgeOver2.M2_1900)
## Bootstrap Confidence Intervals
BS.s2.AgeOver2.M2_1900 <- confint.merMod(s2.AgeOver2.M2_1900, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.AgeOver2.M2_1900

## Mixed effects model age over 2 binary variable (Method 2) - 1000 cutoff
s2.AgeOver2.M2_1000 <- lmer(scale(Eval) ~ scale(AgeOver2M2_1000) + (1|Subject), data = s2.df)
summary(s2.AgeOver2.M2_1000)
## Bootstrap Confidence Intervals
BS.s2.AgeOver2.M2_1000 <- confint.merMod(s2.AgeOver2.M2_1000, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.AgeOver2.M2_1000

## Mixed effects model continuous age variable (Method 1)
s2.Age.M1 <- lmer(scale(Eval) ~ scale(AgeAtInvention) + (1|Subject), data = s2.df)
summary(s2.Age.M1)
## Bootstrap Confidence Intervals
BS.s2.Age.M1 <- confint.merMod(s2.Age.M1, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.Age.M1

## Mixed effects model continuous age variable (Method 2)
s2.Age.M2_1900 <-  lmer(scale(Eval) ~ scale(AgeAtInvention2_1900) + (1|Subject), data = s2.df)
summary(s2.Age.M2_1900)
## Bootstrap Confidence Intervals
BS.s2.Age.M2_1900 <- confint.merMod(s2.Age.M2_1900, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.Age.M2_1900

## Mixed effects model continuous age variable (Method 2)
s2.Age.M2_1000 <-  lmer(scale(Eval) ~ scale(AgeAtInvention2_1000) + (1|Subject), data = s2.df)
summary(s2.Age.M2_1000)
## Bootstrap Confidence Intervals
BS.s2.Age.M2_1000 <- confint.merMod(s2.Age.M2_1000, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.Age.M2_1000



### End pre-registered analyses

### Exploratory analyses
## Exploratory Mixed effects model using Age of First Exposure
s2.Expo <-  lmer(scale(Eval) ~ scale(Age_Of_Exposure) + (1|Subject), data = s2.df)
summary(s2.Expo)
## Bootstrap Confidence Intervals
BS.s2.Expo <- confint.merMod(s2.Expo, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.Expo

## Exploratory Mixed effects model using Frequency of Use
s2.Eval <-  lmer(scale(Eval) ~ scale(Frequency) + (1|Subject), data = s2.df)
summary(s2.model2)
## Bootstrap Confidence Intervals
BS.s2.Eval <- confint.merMod(s2.Eval, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.Eval

## Only 2yo and under (M1)
s2.df.M1.sub2 <- filter(s2.df, AgeAtInvention < 3)
s2.Age.M1.sub2 <- lmer(scale(Eval) ~ scale(AgeAtInvention) + (1|Subject), data = s2.df.M1.sub2)
summary(s2.Age.M1.sub2)
BS.s2.sub2 <- confint.merMod(s2.Age.M1.sub2, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.sub2

## Only 2yo and under (M2)
s2.df.M2.sub2 <- filter(s2.df, AgeAtInvention2 < 3)
s2.Age.M2.sub2 <- lmer(scale(Eval) ~ scale(AgeAtInvention2) + (1|Subject), data = s2.df.M2.sub2)
summary(s2.Age.M2.sub2)
BS.s2.M2.sub2 <- confint.merMod(s2.Age.M2.sub2, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s2.M2.sub2


### Plots ###

## Using Method 1
## Unscaled (for lm on plot)
s2.Age.Unscaled.M1 <- lmer(Eval ~ AgeAtInvention + (1|Subject), data = s2.df)
s2.out.M1 <- summary(s2.Age.Unscaled.M1)

## Individual inventions
s2.plot.ind.M1 <- ggplot(aes(x = AgeAtInvention, y = Eval, colour = Item), data = s2.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -50, to = 80, by = 10)) +
  geom_smooth(se=F)
## All inventions combined
s2.plot.M1 <- ggplot(data = s2.df, aes(x = AgeAtInvention, y = Eval)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(from = -75, to = 75, by = 25), expand = c(0,0)) +
  geom_smooth(fill = "gray", color = "black", alpha = .5) +
  geom_abline(intercept = s2.out.M1$coefficients[1], slope = s2.out.M1$coefficients[2], color = "red", size = 1) +
  xlab("Participants' age at invention") +
  ylab("Evaluation of technology") +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(xlim = c(-50,75), ylim = c(-1,3))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Study 2 (Method 1)")

ggsave(file="s2M1.svg", plot=s2.plot.M1, width=8, height=5)


## Using Method 2
## Unscaled (for lm on plot)
s2.Age.Unscaled.M2_1900 <- lmer(Eval ~ AgeAtInvention2_1900 + (1|Subject), data = s2.df)
s2.out.M2_1900 <- summary(s2.Age.Unscaled.M2_1900)

## Unscaled (for lm on plot)
s2.Age.Unscaled.M2_1000 <- lmer(Eval ~ AgeAtInvention2_1000 + (1|Subject), data = s2.df)
s2.out.M2_1000 <- summary(s2.Age.Unscaled.M2_1000)

## Individual inventions
s2.plot.ind.M2 <- ggplot(aes(x = AgeAtInvention2_1900, y = Eval, colour = Item), data = s2.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -50, to = 80, by = 10)) +
  geom_smooth(se=F)

## All inventions combined
s2.plot.M2 <- ggplot(data = s2.df, aes(x = AgeAtInvention2_1900, y = Eval)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(from = -75, to = 75, by = 25)) +
  geom_smooth(fill = "gray", color = "black", alpha = .5) +
  geom_abline(intercept = s2.out.M2_1900$coefficients[1], slope = s2.out.M2_1900$coefficients[2], color = "red", size = 1) +
  xlab("Participants' perceived age at invention") +
  ylab("Evaluation of technology") +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(xlim = c(-50,75), ylim = c(-1,3)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Study 2 (Method 2)")

ggsave(file="s2.svg", plot=s2.plot.M2, width=8, height=5)

## 1000 as cutoff
s2.plot.M2_1000 <- ggplot(data = s2.df, aes(x = AgeAtInvention2_1000, y = Eval)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(from = -75, to = 75, by = 25)) +
  geom_smooth(fill = "gray", color = "black", alpha = .5) +
  geom_abline(intercept = s2.out.M2_1000$coefficients[1], slope = s2.out.M2_1000$coefficients[2], color = "red", size = 1) +
  xlab("Participants' perceived age at invention") +
  ylab("Evaluation of technology") +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(xlim = c(-50,75), ylim = c(-1,3)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Study 2 (Method 2)")

ggsave(file="s2-1000.svg", plot=s2.plot.M2_1000, width=8, height=5)


## Inflection Point
s2.y.vals <- ggplot_build(s2.plot.M2)$data[[1]]$y
s2.x.vals <- ggplot_build(s2.plot.M2)$data[[1]]$x
infl <- c(FALSE, diff(diff(s2.y.vals)>0)!=0)
infl
s2.x.vals[infl]

cc <- check_curve(s2.x.vals, s2.y.vals);cc
ipbese <- bese(s2.x.vals, s2.y.vals, index = 1); ipbese;

### END Study 2

#### Study 3 (Insider tech WITHOUT moderator, pre-reg'd) #### -------------------------------------------------

s3.wide.df <- read.csv("TechAgeS3.csv")

## Descriptive Statistics

mean(s3.wide.df$Age)
sd(s3.wide.df$Age)
table(s3.wide.df$Gender)

### DATA WRANGLING & CLEANING ###

## Calculate age at invention
## Name Age at Invention Vars
inv.names.age <- paste0(names(s3.wide.df[1:52]),"Age")
s3.wide.df[57:108] <- rep(NA,length(s3.wide.df$NintendoDS))
names(dat)[57:108] <- inv.names.age


## Vector of when each tech was released (not all years had a tech)
invention.years <- c(1950,1951,1954:1960,1962:1965,1967:2005)

## Calculage age at invention for each invention
for(i in 1:52){
  s3.wide.df[i+56] <- s3.wide.df$Age - (2021 - invention.years[i])
}


## Wide to long data so that each row is a single eval (s3.df)
## Add subject numbers
s3.wide.df$Subject <- seq(1:length(s3.wide.df$RemoteControls))
Subject <- rep(s3.wide.df$Subject, 52)
Age <- rep(s3.wide.df$Age, 52)

AgeAtInvention <- unname(as_vector(s3.wide.df[57:108]))
InventionYear <- rep(invention.years, each = length(s3.wide.df$RemoteControls))
Eval <- as.numeric(unname(as_vector(s3.wide.df[1:52])))
Invention <- rep(names(s3.wide.df)[1:52], each = length(s3.wide.df$RemoteControls))


AgeAtInvention_Clean <- AgeAtInvention[!is.na(Eval)]
Eval_Clean <- Eval[!is.na(Eval)]
Invention_Clean <- Invention[!is.na(Eval)]
Subject_Clean <- Subject[!is.na(Eval)]
InventionYear_Clean <- InventionYear[!is.na(Eval)]
Age_Clean <- Age[!is.na(Eval)]

s3.df <- as.data.frame(cbind(Subject_Clean, Eval_Clean, AgeAtInvention_Clean, Invention_Clean, InventionYear_Clean, Age_Clean))
names(s3.df) <- c("Subject", "Eval", "AgeAtInvention", "Invention", "InventionYear", "Age")
s3.df$Eval <- as.numeric(as.character(s3.df$Eval))
s3.df$AgeAtInvention <- as.numeric(as.character(s3.df$AgeAtInvention))
s3.df$InventionYear <- as.numeric(as.character(s3.df$InventionYear))
s3.df$Age <- as.numeric(as.character(s3.df$Age))

## Create AgeOver2 (if over 2 yo at time of invention)

s3.df$AgeOver2 <- rep(NA, length(s3.df$AgeAtInvention))
s3.df$AgeOver2[s3.df$AgeAtInvention<3] <- 0
s3.df$AgeOver2[s3.df$AgeAtInvention>2] <- 1



### END DATA WRANGLING & CLEANING

### Pre-registered Analyses
## Multilevel Model - Age as a continuous variable

s3.Age <- lmer(scale(Eval) ~ scale(AgeAtInvention) + (1|Subject), data = s3.df)
summary(s3.Age)

## Bootstrap CIs

BS.s3.Age <- confint.merMod(s3.Age, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s3.Age

## Multilevel Model - AgeOver2 as a binary variable
s3.AgeOver2 <- lmer(scale(Eval) ~ scale(AgeOver2) + (1|Subject), data = s3.df)
summary(s3.AgeOver2)

BS.s3.AgeOver2 <- confint.merMod(s3.AgeOver2, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s3.AgeOver2

### Plots

## Unscaled (for lm on plot)
s3.Age.Unscaled <- lmer(Eval ~ AgeAtInvention+ (1|Subject), data = s3.df)
s3.out <- summary(s3.Age.Unscaled)


## Main plot (for manuscript)
s3.age.plot <- ggplot(data = s3.df, aes(x = AgeAtInvention, y = Eval)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(from = -75, to = 75, by = 25)) +
  geom_smooth(fill = "gray", color = "black", alpha = .5) +
  geom_abline(intercept = s3.out$coefficients[1], slope = s3.out$coefficients[2], color = "red", size = 1) +
  xlab("Participants' age at invention") +
  ylab("Evaluation of technology") +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(xlim = c(-50,75), ylim = c(-1,3)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Study 3")
ggsave(file="s3ageplot2.svg", plot=s3.age.plot, width=8, height=5)

## Plot by invention
s3.ind.plot <- ggplot(aes(x = AgeAtInvention, y = Eval, colour = Invention), data = s3.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 80, by = 10)) +
  geom_smooth(se=F) +
  theme(legend.position = "none")


## Legend for above plot
s3.ind.plot.leg <- ggplot(aes(x = AgeAtInvention, y = Eval, colour = Invention), data = s3.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 80, by = 10)) +
  geom_smooth(se=F)

### Exploratory analysis

## Only 2yo and under
s3.df.sub2 <- filter(s3.df, AgeAtInvention < 3)
s3.Age.sub2 <- lmer(scale(Eval) ~ scale(AgeAtInvention) + (1|Subject), data = s3.df.sub2)
summary(s3.Age.sub2)
BS.s3.sub2 <- confint.merMod(s3.Age.sub2, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s3.sub2


### Inflection Point

i.y.vals <- ggplot_build(s3.age.plot)$data[[1]]$y
i.x.vals <- ggplot_build(s3.age.plot)$data[[1]]$x
infl <- c(FALSE, diff(diff(i.y.vals)>0)!=0)

cc <- check_curve(i.x.vals, i.y.vals);cc
ipbese <- bese(i.x.vals, i.y.vals, index = 1); ipbese;


#### End Study 3

#### Study 4 (Insider Tech with Status Quo Moderator) #### --------------------------------------

s4.wide.df <- read.csv("TechAgeS4.csv")
s4.wide.df <- mutate_all(s4.wide.df, function(x) as.numeric(as.character(x)))
s4.wide.df <- filter(s4.wide.df, !is.na(s4.wide.df$Age))

## Descriptive Statistics

mean(s4.wide.df$Age)
sd(s4.wide.df$Age)
table(s4.wide.df$Gender)

### DATA WRANGLING & CLEANING ###

## Remove those who failed attention check (11 removed)
s4.wide.df <- filter(s4.wide.df, AttentionCheck==3)

## Calculate age at invention
## Name Age at Invention Vars
inv.names.age <- paste0(names(s4.wide.df[1:52]),"Age")
s4.wide.df[63:114] <- rep(NA,length(s4.wide.df$NintendoDS))
names(s4.wide.df)[63:114] <- inv.names.age

## Vector of when each tech was released
invention.years <- c(1950,1951,1954:1960,1962:1965,1967:2005)

## Calculage age at invention for each invention
for(i in 1:52){
  s4.wide.df[i+62] <- s4.wide.df$Age - (2021 - invention.years[i])
}


## Calculate Status Quo Score

s4.wide.df$SQ <- rep(NA, length(s4.wide.df$SQ1))
for(i in 1:length(s4.wide.df$SQ)){
  s4.wide.df$SQ[i] <- 0
  if(s4.wide.df$SQ1[i]==1) s4.wide.df$SQ[i] <- s4.wide.df$SQ[i] + 1
  if(s4.wide.df$SQ2[i]==1) s4.wide.df$SQ[i] <- s4.wide.df$SQ[i] + 1
  if(s4.wide.df$SQ3[i]==1) s4.wide.df$SQ[i] <- s4.wide.df$SQ[i] + 1
  if(s4.wide.df$SQ4[i]==1) s4.wide.df$SQ[i] <- s4.wide.df$SQ[i] + 1
  if(s4.wide.df$SQ5[i]==1) s4.wide.df$SQ[i] <- s4.wide.df$SQ[i] + 1
}
## Spread of SQ scores
table(s4.wide.df$SQ)



## Wide to long format data
## Add subject numbers
s4.wide.df$Subject <- seq(1:length(s4.wide.df$RemoteControls))
Subject <- rep(s4.wide.df$Subject, 52)
Age <- rep(s4.wide.df$Age, 52)
SQ <- rep(s4.wide.df$SQ, 52)

AgeAtInvention <- unname(as_vector(s4.wide.df[63:114]))
InventionYear <- rep(invention.years, each = length(s4.wide.df$RemoteControls))
Eval <- as.numeric(unname(as_vector(s4.wide.df[1:52])))
Invention <- rep(names(s4.wide.df)[1:52], each = length(s4.wide.df$RemoteControls))


AgeAtInvention_Clean <- AgeAtInvention[!is.na(Eval)]
Eval_Clean <- Eval[!is.na(Eval)]
Invention_Clean <- Invention[!is.na(Eval)]
Subject_Clean <- Subject[!is.na(Eval)]
InventionYear_Clean <- InventionYear[!is.na(Eval)]
Age_Clean <- Age[!is.na(Eval)]
SQ_Clean <- SQ[!is.na(Eval)]

s4.df <- as.data.frame(cbind(Subject_Clean, Eval_Clean, AgeAtInvention_Clean, Invention_Clean, InventionYear_Clean, Age_Clean, SQ_Clean))
names(s4.df) <- c("Subject", "Eval", "AgeAtInvention", "Invention", "InventionYear", "Age", "SQ")
s4.df$Eval <- as.numeric(as.character(s4.df$Eval))
s4.df$AgeAtInvention <- as.numeric(as.character(s4.df$AgeAtInvention))
s4.df$InventionYear <- as.numeric(as.character(s4.df$InventionYear))
s4.df$Age <- as.numeric(as.character(s4.df$Age))
s4.df$SQ <- as.numeric(as.character(s4.df$SQ))

## Create AgeOver2 (if over 2 yo at time of invention)
s4.df$AgeOver2 <- rep(NA, length(s4.df$AgeAtInvention))
s4.df$AgeOver2[s4.df$AgeAtInvention<3] <- 0
s4.df$AgeOver2[s4.df$AgeAtInvention>2] <- 1

### END DATA WRANGLING & CLEANING


### Pre-Registered Analyses

## Multilevel Model Age as continuous variable (no interaction)
s4.Age <- lmer(scale(Eval) ~ scale(AgeAtInvention) + scale(SQ) + (1|Subject), data = s4.df)
summary(s4.Age)
## Bootstrap CIs
BS.s4.Age <- confint.merMod(s4.Age, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s4.Age

## With Interaction
s4.Age.Int <- lmer(scale(Eval) ~ scale(AgeAtInvention) * scale(SQ) + (1|Subject), data = s4.df)
summary(s4.Age.Int)

BS.s4.Age.Int <- confint.merMod(s4.Age.Int, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s4.Age.Int

## Compare models (for moderation)
anova(s4.Age, s4.Age.Int)



## Multilevel model AgeOver2 as binary variable (no interaction)
s4.AgeOver2 <- lmer(scale(Eval) ~ scale(AgeOver2) + scale(SQ) + (1|Subject), data = s4.df)
summary(s4.AgeOver2)
## Bootstrap CIs
BS.s4.AgeOver2 <- confint.merMod(s4.AgeOver2, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s4.AgeOver2

## With interaction
s4.AgeOver2.Int <- lmer(scale(Eval) ~ scale(AgeOver2) * scale(SQ) + (1|Subject), data = s4.df)
summary(s4.AgeOver2.Int)
## Bootstrap CIs
BS.s4.AgeOver2.Int <- confint.merMod(s4.AgeOver2.Int, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s4.AgeOver2.Int

## Compare models (for moderation)
anova(s4.AgeOver2, s4.AgeOver2.Int)


### Plots

## Unscaled (for lm on plot)
s4.Age.Unscaled <- lmer(Eval ~ AgeAtInvention + SQ + (1|Subject), data = s4.df)
s4.out <- summary(s4.Age.Unscaled)

## AgeAtInvention and Evaluation plot

s4.age.plot <- ggplot(data = s4.df, aes(x = AgeAtInvention, y = Eval)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(from = -75, to = 75, by = 25)) +
  geom_smooth(fill = "gray", color = "black", alpha = .5) +
  geom_abline(intercept = s4.out$coefficients[1], slope = s4.out$coefficients[2], color = "red", size = 1) +
  xlab("Participants' age at invention") +
  ylab("Evaluation of technology") +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(xlim = c(-50,75), ylim = c(-1,3)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Study 4")
ggsave(file="s4ageplot.svg", plot=s4.age.plot, width=8, height=5)




## Individual level data

ggplot(aes(x = AgeAtInvention, y = Eval, colour = Invention), data = s4.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 80, by = 10)) +
  geom_smooth(se=F) +
  theme(legend.position = "none")


## Legend for above plot

ggplot(aes(x = AgeAtInvention, y = Eval, colour = Invention), data = s4.df) +
  theme_classic() +
  scale_x_continuous(breaks = seq(from = -100, to = 80, by = 10)) +
  geom_smooth(se=F)


## Have to remove scale to plot interaction
s4.Age.Int.Unscaled <- lmer(Eval ~ AgeAtInvention * SQ + (1|Subject), data = s4.df)
summary(s4.Age.Int.Unscaled)
plot_model(s4.Age.Int.Unscaled, type = "int", legend.title="Status\nQuo\nBias", title = element_blank(), mdrt.values = "quart") +
  theme_classic(base_size = 14) +
  xlab("Age At Invention") +
  ylab("Technology Evaluation (-3 to 3)") +
  scale_y_continuous(expand = c(0, 0))



## Named variable for AgeOver2
s4.df$AgeOver2N <- as.character(s4.df$AgeOver2)
s4.df$AgeOver2N[s4.df$AgeOver2==0] <- "2 or younger"
s4.df$AgeOver2N[s4.df$AgeOver2==1] <- "Older than 2"
s4.AgeOver2.Named <- lmer(Eval ~ SQ * AgeOver2N + (1|Subject), data = s4.df)
s4.int.out <- summary(s4.AgeOver2.Named)
s4.int.out$coefficients

s4.AgeOver2.plot <- plot_model(s4.AgeOver2.Named, type = "int", legend.title="Age when\ninvented", title = element_blank(), ci.style = "whisker", dodge = .8, mdrt.values = "all", colors = c("#04C4D9","#0B2631")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 14) +
  ylab("Evaluation of technology") +
  xlab("Status Quo Bias") +
  geom_bar(position=position_dodge(), stat = "identity", width=0.8, alpha = 0.6, show.legend = FALSE)
ggsave(file="s4AgeOver2plot.svg", plot=s4.AgeOver2.plot, width=8, height=5)

s4.AgeOver2.plot2 <- plot_model(s4.AgeOver2.Named, type = "int", title = element_blank(), ci.style = "whisker", mdrt.values = "all", colors = "bw", line.size = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 14) +
  ylab("Evaluation of technology") +
  xlab("Status quo bias") +
  scale_linetype_manual(values = c("solid", "dotted"), name = "Age at time\nof invention")
ggsave(file="s4AgeOver2LINEplot.svg", plot=s4.AgeOver2.plot2, width=8, height=5)


### Exploratory analysis


## Only 2yo and under
s4.df.sub2 <- filter(s4.df, AgeAtInvention < 3)
s4.Age.sub2 <- lmer(scale(Eval) ~ scale(AgeAtInvention) + (1|Subject), data = s4.df.sub2)
summary(s4.Age.sub2)
BS.s4.sub2 <- confint.merMod(s4.Age.sub2, method="boot", nsim=200, oldNames=FALSE, parallel="multicore", .progress="txt")
BS.s4.sub2


### Inflection Point
sq.y.vals <- ggplot_build(s4.Age.plot)$data[[1]]$y
sq.x.vals <- ggplot_build(s4.Age.plot)$data[[1]]$x
infl <- c(FALSE, diff(diff(sq.y.vals)>0)!=0)
sq.x.vals[infl]

cc <- check_curve(sq.x.vals, sq.y.vals);cc
ipbese <- bese(sq.x.vals, sq.y.vals, index = 1) 
ipbese

#### End Study 4