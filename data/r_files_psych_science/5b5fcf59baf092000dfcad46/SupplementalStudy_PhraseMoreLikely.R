## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Supplemental Study: Phrase 'more likely' and 'less likely'

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
if (!require("lsr")) {install.packages("lsr"); require("lsr")}
if (!require("boot")) {install.packages("boot"); require("boot")}

## Data preparation
## ----------------------------------------------------------------------------------------------------------- ##

## Load data
load("SupplementalStudy_PhraseMoreLikely.Rda")

## Get variables in proper format
dat <- droplevels(dat)
str(dat)

## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Turn ResponseId and workerID into character format
dat$ResponseId <- as.character(dat$ResponseId)
dat$workerID <-as.character(dat$workerID)

## Change ResponseID to responseID
colnames(dat)[3] <- "responseID"

## Label levels in cond
levels(dat$cond)

dat$cond <- factor(x = dat$cond, 
                   levels = c("1", "2"), 
                   labels = c("Man-Woman", "Woman-Man"))

## Turn gender into factor
dat$gender <- factor(x = dat$gender, 
                     levels = c("1", "2"), 
                     labels = c("Male", "Female"))

## Turn education into factor
levels(dat$educ)
dat$educ <- factor(x = dat$educ, 
                   levels = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "14"), 
                   labels = c("Some HS", "HS Grad", "Some College", "Associate's Degree", "Bachelor's Degree", 
                              "Some Grad School", "Master's Degree", "MBA", "JD", "MD", "Other Advanced Degree"))

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

## Rearrange columns
colnames(dat)

dat <- dat[,c("responseID", "workerID", "StartDate", "EndDate", "Date.of.Completion", 
              "birthday", "bmonth", "byear", "age", "gender", "educ", "politicalid",
              "cond",
              "c1.query", "c1.intelligent", "c1.accurate", "c1.fair", "c1.just", 
              "c2.query", "c2.intelligent", "c2.accurate", "c2.fair", "c2.just", 
              "c1.freeResponse", "c2.freeResponse")]

## Turn main DVs into numeric
dat$c1.query <- as.numeric(levels(dat$c1.query))[dat$c1.query]
dat$c2.query <- as.numeric(levels(dat$c2.query))[dat$c2.query]

dat$c1.intelligent <- as.numeric(levels(dat$c1.intelligent))[dat$c1.intelligent]
dat$c2.intelligent <- as.numeric(levels(dat$c2.intelligent))[dat$c2.intelligent]

dat$c1.accurate <- as.numeric(levels(dat$c1.accurate))[dat$c1.accurate]
dat$c2.accurate <- as.numeric(levels(dat$c2.accurate))[dat$c2.accurate]

dat$c1.fair <- as.numeric(levels(dat$c1.fair))[dat$c1.fair]
dat$c2.fair <- as.numeric(levels(dat$c2.fair))[dat$c2.fair]

dat$c1.just <- as.numeric(levels(dat$c1.just))[dat$c1.just]
dat$c2.just <- as.numeric(levels(dat$c2.just))[dat$c2.just]

## Collapse responses
levels(dat$cond)

dat$query <- ifelse(test = dat$cond == "Man-Woman", 
                    yes = dat$c1.query, 
                    no = dat$c2.query)

dat$intelligent <- ifelse(test = dat$cond == "Man-Woman", 
                          yes = dat$c1.intelligent, 
                          no = dat$c2.intelligent)

dat$accurate <- ifelse(test = dat$cond == "Man-Woman", 
                       yes = dat$c1.accurate, 
                       no = dat$c2.accurate)

dat$fair <- ifelse(test = dat$cond == "Man-Woman", 
                   yes = dat$c1.fair, 
                   no = dat$c2.fair)

dat$just <- ifelse(test = dat$cond == "Man-Woman", 
                   yes = dat$c1.just, 
                   no = dat$c2.just)

## Collapse comments
dat$c1.freeResponse <- as.character(dat$c1.freeResponse)
dat$c2.freeResponse <- as.character(dat$c2.freeResponse)

dat$comment <- ifelse(test = dat$cond == "Man-Woman", 
                      yes = dat$c1.freeResponse, 
                      no = dat$c2.freeResponse)

## Rename rows, 1-200
rownames(dat) <- c(1:nrow(dat))

## =============================================================================================================== ##
## Sample characteristics
## N = 200
count(dat$gender)
  ## 105 males, 95 females
round(x = mean(dat$age, na.rm = TRUE), digits = 2)
round(x = sd(dat$age, na.rm = TRUE), digits = 2)
  ## Mean age = 34.00 years, SD = 10.04 years

## =============================================================================================================== ##
## query: which statement do participants agree with?
count(dat$query)
1/20    ## <1% agree that man is less likely to be doctor
179/200 ## 90% agree that both man and woman are equally likely to be a doctor
20/200  ## 10% agree that man is more likely to be a doctor

## =============================================================================================================== ##
## Four main items of interest: intelligent, accurate, fair, and just

## Create dataset with just composite items
dat.composite <- dat[,c("intelligent", "accurate", "fair", "just")]

dat.composite <- melt(dat.composite, measure.vars = c("intelligent", "accurate", "fair", "just"))

levels(dat.composite$variable)
dat.composite$variable <- factor(x = dat.composite$variable, 
                                 levels = c("fair", "intelligent", "just", "accurate"), 
                                 labels = c("1 = Unfair ... 7 = Fair", 
                                            "1 = Unintelligent ... 7 = Intelligent", 
                                            "1 = Unjust ... 7 = Just",
                                            "1 = Inaccurate ... 7 = Accurate"))

dat.composite$study <- "eight.percentage.points.more.likely"

## Visualize
ggplot(data = dat.composite, 
       aes(x = value)) + 
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  facet_wrap(~variable, nrow = 2) + 
  coord_cartesian(xlim = c(0:8), ylim = seq(0, 0.3, 0.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 0.3, 0.1), labels = percent_format()) + 
  labs(y = "Proprotion of participants") + 
  theme_bw(base_size = 14) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.background = element_blank()) + 
  theme(panel.spacing = unit(2, "lines")) + 
  theme(strip.text.x = element_text(size = 14, face = "bold")) + 
  theme(axis.title.y = element_text(vjust = 2, size = 16)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title.x = element_blank())

## Cronbach's alpha
psych::alpha(x = var(dat[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.93

## Compute composite average
dat$composite.avg <- with(dat, (intelligent + accurate + fair + just) / 4)

## Distribution of composite average
ggplot(data = dat, 
       mapping = aes(x = composite.avg)) + 
  geom_histogram(binwidth = 0.25, breaks = seq(0.125, 7.125, 0.25), 
                 mapping = aes(y =..density..), 
                 color = "black", fill = "darkgrey") +
  coord_cartesian(xlim = c(1, 7), ylim = seq(0, 0.5, 0.1)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  scale_y_continuous(breaks = seq(0, 0.5, 0.1), labels = percent_format()) + 
  labs(x = "Composite average", y = "Proporotion of participants") + 
  theme_bw() + 
  theme(aspect.ratio = 1) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black"))

## Descriptives on composite average
round(x = mean(dat$composite.avg), digits = 2)
round(std.error(dat$composite.avg), digits = 2)

## Inferential statistics on composite average
t.test(x = dat$composite.avg, 
       mu = 4)

## Cohen's d
round(x = cohensD(x = dat$composite.avg, 
                  mu = 4), 
      digits = 2)

## 95% CI around Cohen's d
set.seed(123)
one.sample.t.cohensD <- function(data, indices) {
  d <- data[indices,]
  return(cohensD(x = d$composite.avg, mu = 4))
}

one.sample.t.cohensD.results <- boot(data = dat, 
                                     statistic = one.sample.t.cohensD, 
                                     R = 10000)

hist(one.sample.t.cohensD.results$t)

round(x = quantile(x = one.sample.t.cohensD.results$t, 
                   probs = c(0.025, 0.975)), 
      digits = 2)

## Means and standard errors
round(x = apply(X = dat[,c("fair", "just", "accurate", "intelligent")], MARGIN = 2, FUN = mean), 
      digits = 2)

round(x = apply(X = dat[,c("fair", "just", "accurate", "intelligent")], MARGIN = 2, FUN = std.error), 
      digits = 2)


