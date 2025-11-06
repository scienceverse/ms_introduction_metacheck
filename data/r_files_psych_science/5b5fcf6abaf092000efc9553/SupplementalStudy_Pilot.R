## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Supplemental Study: Pilot

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

## Data preparation
## ----------------------------------------------------------------------------------------------------------- ##

## Load data
load("SupplementalStudy_Pilot.Rda")

## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Turn ResponseId and workerID into character format
dat$ResponseId <- as.character(dat$ResponseId)
dat$workerID <-as.character(dat$workerID)

## Change ResponseID to responseID
colnames(dat)[3] <- "responseID"

## Drop unused levels
dat <- droplevels(dat)

## Turn gender into factor
dat$gender <- factor(x = dat$gender, 
                     levels = c("1", "2"), 
                     labels = c("Male", "Female"))

## Turn education into factor
levels(dat$educ)

dat$educ <- factor(x = dat$educ, 
                   levels = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), 
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

## Rearrange columns
colnames(dat)

dat <- dat[,c("responseID", "workerID", "StartDate", "EndDate", "Date.of.Completion", 
              "birthday", "bmonth", "byear", "age", "gender", "educ", "politicalid", 
              "cond",
              
              ## conds 1 and 3
              "mw.query", 
              
              ## cond 2 and 4
              "wm.query", 
              
              ## cond 1
              "mw.uneq.intelligent", "mw.uneq.accurate", "mw.uneq.fair", "mw.uneq.just", 
              "mw.uneq.free", 
              
              ## cond 2
              "wm.uneq.intelligent", "wm.uneq.accurate", "wm.uneq.fair", "wm.uneq.just", 
              "wm.uneq.free", 
              
              ## cond 3
              "mw.eq.intelligent", "mw.eq.accurate", "mw.eq.fair", "mw.eq.just", 
              "mw.eq.free", 
              
              ## cond 4
              "wm.eq.intelligent", "wm.eq.accurate", "wm.eq.fair", "wm.eq.just", 
              "wm.eq.free")]

## Turn main DVs into numeric
dat$mw.query <- as.numeric(levels(dat$mw.query))[dat$mw.query]
dat$wm.query <- as.numeric(levels(dat$wm.query))[dat$wm.query]

## cond 1
dat$mw.uneq.intelligent <- as.numeric(levels(dat$mw.uneq.intelligent))[dat$mw.uneq.intelligent]
dat$mw.uneq.accurate <- as.numeric(levels(dat$mw.uneq.accurate))[dat$mw.uneq.accurate]
dat$mw.uneq.fair <- as.numeric(levels(dat$mw.uneq.fair))[dat$mw.uneq.fair]
dat$mw.uneq.just <- as.numeric(levels(dat$mw.uneq.just))[dat$mw.uneq.just]

## cond 2
dat$wm.uneq.intelligent <- as.numeric(levels(dat$wm.uneq.intelligent))[dat$wm.uneq.intelligent]
dat$wm.uneq.accurate <- as.numeric(levels(dat$wm.uneq.accurate))[dat$wm.uneq.accurate]
dat$wm.uneq.fair <- as.numeric(levels(dat$wm.uneq.fair))[dat$wm.uneq.fair]
dat$wm.uneq.just <- as.numeric(levels(dat$wm.uneq.just))[dat$wm.uneq.just]

## cond 3
dat$mw.eq.intelligent <- as.numeric(levels(dat$mw.eq.intelligent))[dat$mw.eq.intelligent]
dat$mw.eq.accurate <- as.numeric(levels(dat$mw.eq.accurate))[dat$mw.eq.accurate]
dat$mw.eq.fair <- as.numeric(levels(dat$mw.eq.fair))[dat$mw.eq.fair]
dat$mw.eq.just <- as.numeric(levels(dat$mw.eq.just))[dat$mw.eq.just]

## cond 4
dat$wm.eq.intelligent <- as.numeric(levels(dat$wm.eq.intelligent))[dat$wm.eq.intelligent]
dat$wm.eq.accurate <- as.numeric(levels(dat$wm.eq.accurate))[dat$wm.eq.accurate]
dat$wm.eq.fair <- as.numeric(levels(dat$wm.eq.fair))[dat$wm.eq.fair]
dat$wm.eq.just <- as.numeric(levels(dat$wm.eq.just))[dat$wm.eq.just]

## Collapse responses
levels(dat$cond)

dat$query <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                    yes = dat$mw.query, 
                    no = dat$wm.query)

dat$intelligent <- ifelse(test = dat$cond == "1", 
                          yes = dat$mw.uneq.intelligent, 
                          no = ifelse(test = dat$cond == "2", 
                                      yes = dat$wm.uneq.intelligent, 
                                      no = ifelse(test = dat$cond == "3", 
                                                  yes = dat$mw.eq.intelligent, 
                                                  no = dat$wm.eq.intelligent)))

dat$accurate <- ifelse(test = dat$cond == "1", 
                       yes = dat$mw.uneq.accurate, 
                       no = ifelse(test = dat$cond == "2", 
                                   yes = dat$wm.uneq.accurate, 
                                   no = ifelse(test = dat$cond == "3", 
                                               yes = dat$mw.eq.accurate, 
                                               no = dat$wm.eq.accurate)))

dat$fair <- ifelse(test = dat$cond == "1", 
                   yes = dat$mw.uneq.fair, 
                   no = ifelse(test = dat$cond == "2", 
                               yes = dat$wm.uneq.fair, 
                               no = ifelse(test = dat$cond == "3", 
                                           yes = dat$mw.eq.fair, 
                                           no = dat$wm.eq.fair)))

dat$just <- ifelse(test = dat$cond == "1", 
                   yes = dat$mw.uneq.just, 
                   no = ifelse(test = dat$cond == "2", 
                               yes = dat$wm.uneq.just, 
                               no = ifelse(test = dat$cond == "3", 
                                           yes = dat$mw.eq.just, 
                                           no = dat$wm.eq.just)))



## Collapse comments
dat$mw.uneq.free <- as.character(dat$mw.uneq.free)
dat$wm.uneq.free <- as.character(dat$wm.uneq.free)
dat$mw.eq.free <- as.character(dat$mw.eq.free)
dat$wm.eq.free <- as.character(dat$wm.eq.free)


dat$comment <- ifelse(test = dat$cond == "1", 
                      yes = dat$mw.uneq.free, 
                      no = ifelse(test = dat$cond == "2", 
                                  yes = dat$wm.uneq.free, 
                                  no = ifelse(test = dat$cond == "3", 
                                              yes = dat$mw.eq.free, 
                                              no = dat$wm.eq.free)))

## Rename rows
rownames(dat) <- c(1:nrow(dat))

## Create variables, order and personX
dat$order <- ifelse(test = dat$cond == "1" | dat$cond == "3", 
                    yes = "MW", 
                    no = "WM")

dat$order <- factor(dat$order, 
                    levels = c("MW", "WM"))


dat$personX <- ifelse(test = dat$cond == "1" | dat$cond == "2", 
                      yes = "Man more likely", 
                      no = "Equally likely")

table(dat$personX)

dat$personX <- factor(dat$personX, 
                      levels = c("Equally likely", "Man more likely"), 
                      labels = c("Equally likely\nn = 201", "Man more likely\nn = 199"))

## =============================================================================================================== ##
## Sample characteristics
  ## N = 400
count(dat$gender)
  ## 150 males, 250 females
round(x = mean(dat$age, na.rm = TRUE), digits = 2)
round(x = sd(dat$age, na.rm = TRUE), digits = 2)
  ## Mean age = 34.68 years, SD = 10.89 years

## =============================================================================================================== ##
## query: which statement do participants agree with?
count(dat$query)
  325/400 ## 81% agree that both man and woman are equally likely to be the doctor
  60/400  ## 15% agree that man is more likely to be the doctor
  15/400   ## 4% agree that woman is more likely to be the doctor

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


levels(dat.composite$personX)  

dat.composite$personX <- factor(x = dat.composite$personX, 
                                levels = c("Equally likely\nn = 201", 
                                           "Man more likely\nn = 199"), 
                                labels = c("Equally likely", 
                                           "Man more likely"))


## Joy Plot
if (!require("ggjoy")) {install.packages("ggjoy"); require("ggjoy")}

## Fig. S4
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

psych::alpha(x = var(subset(dat, personX == "Man more likely\nn = 199")[,c("intelligent",
                                                                           "accurate", 
                                                                           "fair", 
                                                                           "just")]))

psych::alpha(x = var(subset(dat, personX == "Equally likely\nn = 201")[,c("intelligent",
                                                                          "accurate", 
                                                                          "fair", 
                                                                          "just")]))

dat$composite.avg <- with(dat, (intelligent + accurate + fair + just)/4)

## Man more likely
round(x = mean(dat$composite.avg[dat$personX == "Man more likely\nn = 199"]), digits = 2)
round(x = std.error(dat$composite.avg[dat$personX == "Man more likely\nn = 199"]), digits = 2)
t.test(dat$composite.avg[dat$personX == "Man more likely\nn = 199"], mu = 4)
cohensD(dat$composite.avg[dat$personX == "Man more likely\nn = 199"], mu = 4)

## 95% CI around Cohen's d
set.seed(123)
one.sample.t.cohensD <- function(data, indices) {
  d <- data[indices,]
  return(cohensD(x = d$composite.avg, mu = 4))
}

manMoreLikely.boot.cohensD.results <- boot(data = subset(dat, personX == "Man more likely\nn = 199"), 
                                           statistic = one.sample.t.cohensD, 
                                           R = 10000)

hist(manMoreLikely.boot.cohensD.results$t)

quantile(x = manMoreLikely.boot.cohensD.results$t, 
         probs = c(0.025, 0.975))

## Equally likely
round(x = mean(dat$composite.avg[dat$personX == "Equally likely\nn = 201"]), digits = 2)
round(x = std.error(dat$composite.avg[dat$personX == "Equally likely\nn = 201"]), digits = 2)
t.test(dat$composite.avg[dat$personX == "Equally likely\nn = 201"], mu = 4)
cohensD(dat$composite.avg[dat$personX == "Equally likely\nn = 201"], mu = 4)

eqLikely.boot.cohensD.results <- boot(data = subset(dat, personX == "Equally likely\nn = 201"), 
                                      statistic = one.sample.t.cohensD, 
                                      R = 10000)

hist(eqLikely.boot.cohensD.results$t)

quantile(x = eqLikely.boot.cohensD.results$t, 
         probs = c(0.025, 0.975))

## Items means and std. errors
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Unjust ... 7 = Just", 
                                            "1 = Inaccurate ... 7 = Accurate", 
                                            "1 = Unintelligent ... 7 = Intelligent"))

## Table S4
round(tapply(X = dat.composite$value, 
             INDEX = list(dat.composite$variable, dat.composite$personX), 
             FUN = mean), 
      digits = 2)

round(tapply(X = dat.composite$value, 
             INDEX = list(dat.composite$variable, dat.composite$personX), 
             FUN = std.error), 
      digits = 2)

