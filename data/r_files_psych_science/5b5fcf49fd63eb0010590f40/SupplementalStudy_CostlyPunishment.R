## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Supplemental Study on Costly Punishment: data preparation and analyses

## Set working directory
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
currentPath <- getActiveDocumentContext()$path
setwd(dir = dirname(path = currentPath))

## Packages needed
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("corrplot")) {install.packages("corrplot"); require("corrplot")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("GGally")) {install.packages("GGally"); require("GGally")}
if (!require("smacof")) {install.packages("smacof"); require("smacof")}
if (!require("vegan")) {install.packages("vegan"); require("vegan")}
if (!require("ggfortify")) install.packages("ggfortify"); require("ggfortify")
if (!require("ggrepel")) {install.packages("ggrepel"); require("ggrepel")}
if (!require("grid")) {install.packages("grid"); require("grid")}
if (!require("smacof")) {install.packages("smacof"); require("smacof")}
if (!require("ggdendro")) {install.packages("ggdendro"); require("ggdendro")}
if (!require("quanteda")) {install.packages("quanteda"); require("quanteda")}
if (!require("lsr")) {install.packages("lsr"); require("lsr")}
if (!require("boot")) {install.packages("boot"); require("boot")}
if (!require("compute.es")) {install.packages("compute.es"); require("compute.es")}

## Data preparation
## ----------------------------------------------------------------------------------------------------------- ##

## Load data
load("SupplementalStudy_CostlyPunishment.Rda")

## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Turn ResponseId and workerID into character format
dat$ResponseId <- as.character(dat$ResponseId)
dat$workerID <-as.character(dat$workerID)
dat$workerID_bonus <-as.character(dat$workerID_bonus)

## Change ResponseID to responseID
colnames(dat)[3] <- "responseID"

## Drop unused levels
dat <- droplevels(dat)

## Break up condition
levels(dat$cond)

dat$personX <- ifelse(test = dat$cond == "1" | dat$cond == "2" |
                        dat$cond == "3" | dat$cond == "4", 
                      yes = "Man more likely", 
                      no = "Equally likely")

dat$personX <- factor(dat$personX, 
                      levels = c("Man more likely", "Equally likely"))

dat$order <- ifelse(test = dat$cond == "1" | dat$cond == "3" | 
                      dat$cond == "5" | dat$cond == "7",
                    yes = "Man-Woman", 
                    no = "Woman-Man")

dat$order <- factor(dat$order, 
                    levels = c("Man-Woman", "Woman-Man"))

dat$punishment_order <-ifelse(test = dat$cond == "1" | dat$cond == "2" | 
                                dat$cond == "5" | dat$cond == "6",
                              yes = "lowHigh", 
                              no = "highLow")

dat$punishment_order <- factor(dat$punishment_order, 
                               levels = c("lowHigh", "highLow"))

## Demographics
  ## Turn gender into factor
dat$gender <- factor(x = dat$gender, 
                     levels = c("1", "2"), 
                     labels = c("Male", "Female"))

## Turn education into factor
levels(dat$educ)

dat$educ <- factor(x = dat$educ, 
                   levels = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12","13", "14"), 
                   labels = c("Some HS", "HS Grad", "Some College", 
                              "Associate's", "Bachelor's", "Some Grad School", 
                              "Master's", "MBA", "JD", "MD", "PhD", "Other Advanced Degree"))

## Turn bmonth and byear into numeric
dat$bmonth <- as.numeric(levels(dat$bmonth))[dat$bmonth]
dat$byear <- as.numeric(levels(dat$byear))[dat$byear]

## Turn politicalid into factor
levels(dat$politicalid)
dat$politicalid <- factor(x = dat$politicalid, 
                          levels = c("1", "2", "3", "4", "5", "6", "7"), 
                          labels = c("Strongly Conservative", 
                                     "Moderately Conservative", 
                                     "Slightly Conservative", 
                                     "Neutral", 
                                     "Slightly Liberal", 
                                     "Moderately Liberal", 
                                     "Strongly Liberal"))

## Create birthday and calculate age
dat$birthday <- paste(dat$bmonth, 
                      "1",          
                      dat$byear, 
                      sep = "/")

dat$birthday <- as.POSIXct(dat$birthday, 
                           format = "%m/%d/%Y")

dat$Date.of.Completion <- sapply(X = strsplit(x = as.character(dat$StartDate), split = " "), 
                                 "[", 
                                 1)

dat$Date.of.Completion <- as.POSIXct(dat$Date.of.Completion, 
                                     format = "%Y-%m-%d")

dat$age <- with(dat, (Date.of.Completion - birthday)/365)

dat$age <- as.numeric(dat$age)

hist(dat$age)

## Turn main DVs into numeric
colnames(dat)

dat$mw.query <- as.numeric(levels(dat$mw.query))[dat$mw.query]
dat$wm.query <- as.numeric(levels(dat$wm.query))[dat$wm.query]

dat$mw.uneq.intelligent <- as.numeric(levels(dat$mw.uneq.intelligent))[dat$mw.uneq.intelligent]
dat$wm.uneq.intelligent <- as.numeric(levels(dat$wm.uneq.intelligent))[dat$wm.uneq.intelligent]
dat$mw.eq.intelligent <- as.numeric(levels(dat$mw.eq.intelligent))[dat$mw.eq.intelligent]
dat$wm.eq.intelligent <- as.numeric(levels(dat$wm.eq.intelligent))[dat$wm.eq.intelligent]

dat$mw.uneq.accurate <- as.numeric(levels(dat$mw.uneq.accurate))[dat$mw.uneq.accurate]
dat$wm.uneq.accurate <- as.numeric(levels(dat$wm.uneq.accurate))[dat$wm.uneq.accurate]
dat$mw.eq.accurate <- as.numeric(levels(dat$mw.eq.accurate))[dat$mw.eq.accurate]
dat$wm.eq.accurate <- as.numeric(levels(dat$wm.eq.accurate))[dat$wm.eq.accurate]

dat$mw.uneq.fair <- as.numeric(levels(dat$mw.uneq.fair))[dat$mw.uneq.fair]
dat$wm.uneq.fair <- as.numeric(levels(dat$wm.uneq.fair))[dat$wm.uneq.fair]
dat$mw.eq.fair <- as.numeric(levels(dat$mw.eq.fair))[dat$mw.eq.fair]
dat$wm.eq.fair <- as.numeric(levels(dat$wm.eq.fair))[dat$wm.eq.fair]

dat$mw.uneq.just <- as.numeric(levels(dat$mw.uneq.just))[dat$mw.uneq.just]
dat$wm.uneq.just <- as.numeric(levels(dat$wm.uneq.just))[dat$wm.uneq.just]
dat$mw.eq.just <- as.numeric(levels(dat$mw.eq.just))[dat$mw.eq.just]
dat$wm.eq.just <- as.numeric(levels(dat$wm.eq.just))[dat$wm.eq.just]

dat$punishment_lowHigh <- as.numeric(levels(dat$punishment_lowHigh))[dat$punishment_lowHigh]
dat$punishment_highLow <- as.numeric(levels(dat$punishment_highLow))[dat$punishment_highLow]

## Collapse responses

## Intelligent
dat$intelligent <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                          yes = dat$mw.uneq.intelligent, 
                          no = ifelse(test = dat$cond == "2" | dat$cond == "4", 
                                      yes = dat$wm.uneq.intelligent, 
                                      no = ifelse(test = dat$cond == "5" | dat$cond == "7", 
                                                  yes = dat$mw.eq.intelligent, 
                                                  no = dat$wm.eq.intelligent)))

## Accurate
dat$accurate <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                       yes = dat$mw.uneq.accurate, 
                       no = ifelse(test = dat$cond == "2" | dat$cond == "4", 
                                   yes = dat$wm.uneq.accurate, 
                                   no = ifelse(test = dat$cond == "5" | dat$cond == "7", 
                                               yes = dat$mw.eq.accurate, 
                                               no = dat$wm.eq.accurate)))


## Fair
dat$fair <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                   yes = dat$mw.uneq.fair, 
                   no = ifelse(test = dat$cond == "2" | dat$cond == "4", 
                               yes = dat$wm.uneq.fair, 
                               no = ifelse(test = dat$cond == "5" | dat$cond == "7", 
                                           yes = dat$mw.eq.fair, 
                                           no = dat$wm.eq.fair)))

## Just
dat$just <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                   yes = dat$mw.uneq.just, 
                   no = ifelse(test = dat$cond == "2" | dat$cond == "4", 
                               yes = dat$wm.uneq.just, 
                               no = ifelse(test = dat$cond == "5" | dat$cond == "7", 
                                           yes = dat$mw.eq.just, 
                                           no = dat$wm.eq.just)))

## Query
dat$query <- ifelse(test = dat$cond == "1" | dat$cond == "3" | 
                      dat$cond == "5" | dat$cond == "7", 
                    yes = dat$mw.query, 
                    no = dat$wm.query)

## Comments
dat$mw.uneq.free <- as.character(dat$mw.uneq.free)
dat$wm.uneq.free <- as.character(dat$wm.uneq.free)
dat$mw.eq.free <- as.character(dat$mw.eq.free)
dat$wm.eq.free <- as.character(dat$wm.eq.free)

dat$comment <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                      yes = dat$mw.uneq.free, 
                      no = ifelse(test = dat$cond == "2" | dat$cond == "4", 
                                  yes = dat$wm.uneq.free, 
                                  no = ifelse(test = dat$cond == "5" | dat$cond == "7", 
                                              yes = dat$mw.eq.free, 
                                              no = dat$wm.eq.free)))

## =============================================================================================================== ##
## Participants who didn't complete study
dat$completed <- ifelse(test = is.na(dat$punishment_lowHigh) == TRUE & 
                          is.na(dat$punishment_highLow) == TRUE, 
                        yes = "Didn't finish", 
                        no = "Finished")

count(dat$completed)

dat <- subset(dat, completed == "Finished")


## =============================================================================================================== ##
## Sample characteristics
  ## N = 401
count(dat$gender)
  ## 166 males, 231 females, 4 NAs
round(x = mean(dat$age, na.rm = TRUE), digits = 2)
round(x = sd(dat$age, na.rm = TRUE), digits = 2)
  ## Mean age = 33.87 years, SD = 10.52 years

## =============================================================================================================== ##
## query: which statement do participants agree with?
count(dat$query)
356/nrow(dat) ## 89% agree that both man and woman are equally likely to be the doctor
23/nrow(dat)  ## 6% agree that man is more likely to be the doctor
22/nrow(dat)   ## 5% agree that woman is more likely to be the doctor

## =============================================================================================================== ##
## Four composite items
## Four main items of interest: intelligent, accurate, fair, and just

## Create dataset with just composite items
dat.composite <- dat[,c("intelligent", "accurate", "fair", "just", "personX")]

dat.composite <- melt(dat.composite, measure.vars = c("intelligent", "accurate", "fair", "just"))

levels(dat.composite$variable)
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("fair", "accurate", "just", "intelligent"), 
                                 labels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Inaccurate ... 7 = Accurate",
                                            "1 = Unjust ... 7 = Just",
                                            "1 = Unintelligent ... 7 = Intelligent"))
                                    


## Relevel
dat.composite$personX <- relevel(x = dat.composite$personX, 
                                 ref = "Equally likely")

## Joy Plot
if (!require("ggjoy")) {install.packages("ggjoy"); require("ggjoy")}

## Fig. S2
ggplot(data = dat.composite, 
       mapping = aes(x = value, y = personX)) + 
  geom_joy() + 
  facet_wrap(~variable) + 
  coord_cartesian(xlim = c(0, 8)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  theme(axis.title = element_blank()) + 
  theme(axis.ticks.y = element_blank()) + 
  theme(axis.text.y = element_text(vjust = -0.5, size = 14, face = "bold", color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 14, face = "bold")) + 
  theme(panel.spacing = unit(2.5, "lines")) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  theme(aspect.ratio = 1)

psych::alpha(x = var(subset(dat, personX == "Man more likely")[,c("intelligent",
                                                                  "accurate", 
                                                                  "fair", 
                                                                  "just")]))

psych::alpha(x = var(subset(dat, personX == "Equally likely")[,c("intelligent",
                                                                 "accurate", 
                                                                 "fair", 
                                                                 "just")]))

## Composite average
dat$composite.avg <- with(dat, (fair + just + accurate + intelligent)/4)

table(dat$personX)

## Man more likely
round(x = mean(dat$composite.avg[dat$personX == "Man more likely"]), digits = 2)
round(x = std.error(dat$composite.avg[dat$personX == "Man more likely"]), digits = 2)
t.test(dat$composite.avg[dat$personX == "Man more likely"], mu = 4)
cohensD(dat$composite.avg[dat$personX == "Man more likely"], mu = 4)

## 95% CI around Cohen's d
set.seed(123)
one.sample.t.cohensD <- function(data, indices) {
  d <- data[indices,]
  return(cohensD(x = d$composite.avg, mu = 4))
}

manMoreLikely.boot.cohensD.results <- boot(data = subset(dat, personX == "Man more likely"), 
                                           statistic = one.sample.t.cohensD, 
                                           R = 10000)

hist(manMoreLikely.boot.cohensD.results$t)

quantile(x = manMoreLikely.boot.cohensD.results$t, 
         probs = c(0.025, 0.975))

## Equally likely
round(x = mean(dat$composite.avg[dat$personX == "Equally likely"]), digits = 2)
round(x = std.error(dat$composite.avg[dat$personX == "Equally likely"]), digits = 2)
t.test(dat$composite.avg[dat$personX == "Equally likely"], mu = 4)
cohensD(dat$composite.avg[dat$personX == "Equally likely"], mu = 4)

## 95% CI around Cohen's d
eqLikely.boot.cohensD.results <- boot(data = subset(dat, personX == "Equally likely"), 
                                      statistic = one.sample.t.cohensD, 
                                      R = 10000)

hist(eqLikely.boot.cohensD.results$t)

quantile(x = eqLikely.boot.cohensD.results$t, 
         probs = c(0.025, 0.975))

## Individual item means and std. errors
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Unjust ... 7 = Just", 
                                            "1 = Inaccurate ... 7 = Accurate", 
                                            "1 = Unintelligent ... 7 = Intelligent"))

round(tapply(X = dat.composite$value, 
             INDEX = list(dat.composite$variable, dat.composite$personX), 
             FUN = mean), 
      digits = 2)

round(tapply(X = dat.composite$value, 
             INDEX = list(dat.composite$variable, dat.composite$personX), 
             FUN = std.error), 
      digits = 2)


## =============================================================================================================== ##
## Punishment Data

levels(dat$punishment_order)
dat$punishment <- ifelse(test = dat$punishment_order == "lowHigh", 
                         yes = dat$punishment_lowHigh, 
                         no = dat$punishment_highLow)

count(dat$personX)
  ## Man more likely: 198
  ## Equally likely: 203

dat$personX <- factor(dat$personX, 
                      levels = c("Equally likely", "Man more likely"))

## Means and SEs
tapply(X = dat$punishment, 
       INDEX = dat$personX, 
       FUN = mean)/100

tapply(X = dat$punishment, 
       INDEX = dat$personX, 
       FUN = std.error)/100

## Inferential statistics
t.test(punishment ~ personX, data = dat)
cohensD(punishment ~ personX, data = dat)
tes(t = t.test(punishment ~ personX, data = dat)$statistic, 
    n.1 = 203, 
    n.2 = 198)

## Distributions
count(dat$punishment[dat$personX == "Equally likely"])
190/203

count(dat$punishment[dat$personX == "Man more likely"])
147/203



## Find bootstrap confidence intervals
dat$punishment.cents <- dat$punishment/100

sample.mean <- function(x, ind) {
  return(mean(x[ind]))
}

punishment.manMoreLikely <- boot(data = dat$punishment.cents[dat$personX == "Man more likely"], 
                                 statistic = sample.mean, 
                                 R = 10000)

## CIs for man more likely condition
hist(punishment.manMoreLikely$t)
unname(quantile(punishment.manMoreLikely$t, probs = .025))
unname(quantile(punishment.manMoreLikely$t, probs = .975))

punishment.equallyLikely <- boot(data = dat$punishment.cents[dat$personX == "Equally likely"], 
                                 statistic = sample.mean, 
                                 R = 10000)

## CIs for equally likely condition
hist(punishment.equallyLikely$t)
unname(quantile(punishment.equallyLikely$t, probs = .025))
unname(quantile(punishment.equallyLikely$t, probs = .975))

## Means
tapply(X = dat$punishment.cents, 
       INDEX = dat$personX, 
       FUN = mean)


punishment.dat <- data.frame(personX = c("Man more likely", "Equally likely"), 
                             punishment.cents = c(mean(dat$punishment.cents[dat$personX == "Man more likely"]),
                                                  mean(dat$punishment.cents[dat$personX == "Equally likely"])), 
                             lower = c(unname(quantile(punishment.manMoreLikely$t, probs = .025)),
                                       unname(quantile(punishment.equallyLikely$t, probs = .025))),
                             upper = c(unname(quantile(punishment.manMoreLikely$t, probs = .975)),
                                       unname(quantile(punishment.equallyLikely$t, probs = .975)))
                            )

dat.manMoreLikely.only <- dat
dat.manMoreLikely.only$punishment.cents <- ifelse(test = dat.manMoreLikely.only$personX == "Equally likely", 
                                                  yes = -1, 
                                                  no = dat.manMoreLikely.only$punishment.cents)

dat.equallyLikely.only <- dat
dat.equallyLikely.only$punishment.cents <- ifelse(test = dat.equallyLikely.only$personX == "Man more likely", 
                                                  yes = -1, 
                                                  no = dat.equallyLikely.only$punishment.cents)


## Fig. S3
ggplot(data = dat, 
       mapping = aes(x = personX, y = punishment.cents)) + 
  geom_violin(data = dat.manMoreLikely.only, 
              mapping = aes(x = personX, y = punishment.cents), 
              fill = "lightgrey"
              ,
              bw = 0.0025
              ) + 
  geom_violin(data = dat.equallyLikely.only, 
              mapping = aes(x = personX, y = punishment.cents), 
              fill = "lightgrey"
              , 
              bw = 0.005
             ) + 
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.1, lwd = 2) + 
  labs(x = "\nPerson X's answer", y = "Money\ngiven up\nto punish\nPerson X") + 
  coord_cartesian(ylim = c(0, 0.10)) + 
  scale_x_discrete(breaks = c("Equally likely", "Man more likely"), 
                   labels = c("Equally likely\nn = 203", 
                              "Man more likely\nn = 198")) + 
  scale_y_continuous(breaks = seq(0, 0.10, 0.01), 
                     labels = c("(No punishment) $0.00",
                                "$0.01",
                                "$0.02",
                                "$0.03",
                                "$0.04",
                                "$0.05",
                                "$0.06",
                                "$0.07",
                                "$0.08",
                                "$0.09",
                                "(Max. punishment) $0.10")) + 
  theme_bw(base_size = 20) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.text = element_text(color = "black")) +
  theme(axis.ticks.x = element_blank()) + 
  theme(aspect.ratio = 1) + 
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) 

