## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Supplemental Study - Pilot Probability Judgments: Analyses

## Set working directory
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
currentPath <- getActiveDocumentContext()$path
setwd(dir = dirname(path = currentPath))

## Packages needed
if (!require("car")) {install.packages("car"); require("car")}
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("grid")) {install.packages("grid"); require("grid")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggrepel")) {install.packages("ggrepel"); require("ggrepel")}
if (!require("nlme")) {install.packages("nlme"); require("nlme")}
if (!require("lme4")) {install.packages("lme4"); require("lme4")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("multcomp")) {install.packages("multcomp"); require("multcomp")}
if (!require("lsr")) {install.packages("lsr"); require("lsr")}
if (!require("lsmeans")) {install.packages("lsmeans"); require("lsmeans")}
if (!require("cowplot")) {install.packages("cowplot"); require("cowplot")}

## Load data
load("pilot_flightAttendant.Rda")

## Examine structure and summary
str(dat)

count(dat$able.to.update)
  ## 19 people not able to update
count(dat$Prior.Man.Pilot)
  ## These 19 people gave 100% probability to man being the pilot

## Exclude 19 participants for not being able to update
dat <- subset(dat, able.to.update == "yes")

## ======================================================================= ##
## Prior beliefs

## Percentage of participants whose priors were <50%, =50%, or >50%
dat$Prior.Man.Pilot.Discrete <- ifelse(test = dat$Prior.Man.Pilot < .50, 
                                       yes = "Favor woman as pilot", 
                                       no = ifelse(test = dat$Prior.Man.Pilot == .50, 
                                                   yes = "Favor both individuals equally", 
                                                   no = "Favor man as pilot"))

dat$Prior.Man.Pilot.Discrete <- factor(dat$Prior.Man.Pilot.Discrete, 
                                       levels = c("Favor woman as pilot", 
                                                  "Favor both individuals equally", 
                                                  "Favor man as pilot"))

count(dat$Prior.Man.Pilot.Discrete)

## Ensure that every participant's prior beliefs sum to 1
count(dat$Prior.Man.Pilot + dat$Prior.Woman.Pilot)

## Distribution of prior beliefs, binned by decile
ggplot(data = dat, 
       mapping = aes(x = Prior.Man.Pilot)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           binwidth = 0.05, breaks = seq(-0.025, 1.025, 0.05),
           col = "black" , fill = "#F8766D") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 1, .1), label = percent_format()) + 
  scale_y_continuous(breaks = seq(0, .2, .05), label = percent_format()) + 
  labs(x = "\nPerentage chance that Man = Pilot\n(Woman != Pilot)", 
       y = "Proportion of participants\n") + 
  theme_bw(base_size = 24) + 
  theme(aspect.ratio = 1) + theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black"))


## ================================================================================================== ##
## ======================================================== ##
## ======================================================== ##
## Log likelihood ratios

## Create wide version of dataset containing only:
## ResponseID, target, data, and likelihood responses
colnames(dat)

## Isolate variables that contain likelihood data
grep(pattern = "lk", x = colnames(dat), value = TRUE)

dat.lr <- dat[,c("ResponseID", "target", "data", "condition",
                 grep(pattern = "lk", x = colnames(dat), value = TRUE))]

## condition: interaction between target and data
dat.lr$condition.target.data <- interaction(dat.lr$target, dat.lr$data)

## Rearrange columns
dat.lr <- dat.lr[,c(1, 53, 2:4, 5:52)]

## ------------------------------------------------------------------------- ##
## percent.male.pilots.comm.ATC
dat.lr$percent.male.pilots.comm.ATC <- NA

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "1"] <- 
  dat.lr$cond1.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "1"]

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "2"] <- 
  dat.lr$cond2.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "2"]

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "13"] <- 
  dat.lr$cond13.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "13"]

dat.lr$percent.male.pilots.comm.ATC[dat.lr$condition == "14"] <- 
  dat.lr$cond14.lk_percent.male.pilots.comm.ATC[dat.lr$condition == "14"]

count(dat.lr$percent.male.pilots.comm.ATC)
  nrow(dat.lr)-791
  ## 154
count(dat.lr$condition.target.data)
  ## 154

hist(dat.lr$percent.male.pilots.comm.ATC)
  ## looks right; nearly all male pilots communicate with ATC

## ------------------------------------------------------------------------- ##
## percent.male.flightattendants.comm.ATC
dat.lr$percent.male.flightattendants.comm.ATC <- NA

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "1"] <- 
  dat.lr$cond1.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "1"]

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "2"] <- 
  dat.lr$cond2.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "2"]

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "13"] <- 
  dat.lr$cond13.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "13"]

dat.lr$percent.male.flightattendants.comm.ATC[dat.lr$condition == "14"] <- 
  dat.lr$cond14.lk_percent.male.flightAttendants.comm.ATC[dat.lr$condition == "14"]

count(dat.lr$percent.male.flightattendants.comm.ATC)
nrow(dat.lr)-791
  ## 154
count(dat.lr$condition.target.data)
  ## 154

hist(dat.lr$percent.male.flightattendants.comm.ATC)
  ## looks right; virtually no flight attendants communicate with ATC

## ------------------------------------------------------------------------- ##
## percent.male.pilots.special.line
dat.lr$percent.male.pilots.special.line <- NA

dat.lr$percent.male.pilots.special.line[dat.lr$condition == "3"] <- 
  dat.lr$cond3.lk_percent.male.pilots.special.line[dat.lr$condition == "3"]

dat.lr$percent.male.pilots.special.line[dat.lr$condition == "4"] <- 
  dat.lr$cond4.lk_percent.male.pilots.special.line[dat.lr$condition == "4"]

dat.lr$percent.male.pilots.special.line[dat.lr$condition == "15"] <- 
  dat.lr$cond15.lk_percent.male.pilots.special.line[dat.lr$condition == "15"]

dat.lr$percent.male.pilots.special.line[dat.lr$condition == "16"] <- 
  dat.lr$cond16.lk_percent.male.pilots.special.line[dat.lr$condition == "16"]

count(dat.lr$percent.male.pilots.special.line)
nrow(dat.lr)-784
  ## 161
count(dat.lr$condition.target.data)
  ## 161

hist(dat.lr$percent.male.pilots.special.line)
  ## looks right; nearlly all male pilots go through special line


## ------------------------------------------------------------------------- ##
## percent.male.flightattendants.special.line
dat.lr$percent.male.flightattendants.special.line <- NA

dat.lr$percent.male.flightattendants.special.line[dat.lr$condition == "3"] <- 
  dat.lr$cond3.lk_percent.male.flightAttendants.special.line[dat.lr$condition == "3"]

dat.lr$percent.male.flightattendants.special.line[dat.lr$condition == "4"] <- 
  dat.lr$cond4.lk_percent.male.flightAttendants.special.line[dat.lr$condition == "4"]

dat.lr$percent.male.flightattendants.special.line[dat.lr$condition == "15"] <- 
  dat.lr$cond15.lk_percent.male.flightAttendants.special.line[dat.lr$condition == "15"]

dat.lr$percent.male.flightattendants.special.line[dat.lr$condition == "16"] <- 
  dat.lr$cond16.lk_percent.male.flightAttendants.special.line[dat.lr$condition == "16"]

count(dat.lr$percent.male.flightattendants.special.line)
nrow(dat.lr)-784
  ## 161
count(dat.lr$condition.target.data)
  ## 161

hist(dat.lr$percent.male.flightattendants.special.line)
  ## looks right; nearlly all male flight attendants go through special line at airport security

## ------------------------------------------------------------------------- ##
## percent.male.pilots.serve.beverages
dat.lr$percent.male.pilots.serve.beverages <- NA

dat.lr$percent.male.pilots.serve.beverages[dat.lr$condition == "5"] <- 
  dat.lr$cond5.lk_percent.male.pilots.serve.beverages[dat.lr$condition == "5"]

dat.lr$percent.male.pilots.serve.beverages[dat.lr$condition == "6"] <- 
  dat.lr$cond6.lk_percent.male.pilots.serve.beverages[dat.lr$condition == "6"]

dat.lr$percent.male.pilots.serve.beverages[dat.lr$condition == "17"] <- 
  dat.lr$cond17.lk_percent.male.pilots.serve.beverages[dat.lr$condition == "17"]

dat.lr$percent.male.pilots.serve.beverages[dat.lr$condition == "18"] <- 
  dat.lr$cond18.lk_percent.male.pilots.serve.beverages[dat.lr$condition == "18"]

count(dat.lr$percent.male.pilots.serve.beverages)
nrow(dat.lr)-786
  ## 159
count(dat.lr$condition.target.data)
  ## 159

hist(dat.lr$percent.male.pilots.serve.beverages)
  ## looks right; virtually no male pilots serve beverages to passengers

## ------------------------------------------------------------------------- ##
## percent.male.flightattendants.serve.beverages
dat.lr$percent.male.flightattendants.serve.beverages <- NA

dat.lr$percent.male.flightattendants.serve.beverages[dat.lr$condition == "5"] <- 
  dat.lr$cond5.lk_percent.male.flightAttendants.serve.beverages[dat.lr$condition == "5"]

dat.lr$percent.male.flightattendants.serve.beverages[dat.lr$condition == "6"] <- 
  dat.lr$cond6.lk_percent.male.flightAttendants.serve.beverages[dat.lr$condition == "6"]

dat.lr$percent.male.flightattendants.serve.beverages[dat.lr$condition == "17"] <- 
  dat.lr$cond17.lk_percent.male.flightAttendants.serve.beverages[dat.lr$condition == "17"]

dat.lr$percent.male.flightattendants.serve.beverages[dat.lr$condition == "18"] <- 
  dat.lr$cond18.lk_percent.male.flightAttendants.serve.beverages[dat.lr$condition == "18"]

count(dat.lr$percent.male.flightattendants.serve.beverages)
nrow(dat.lr)-786
  ## 159
count(dat.lr$condition.target.data)
  ## 159

hist(dat.lr$percent.male.flightattendants.serve.beverages)
  ## looks right; virtually all male flight attendants serve beverages to passengers

## ======== ##
## ======== ##
## ======== ##

## ------------------------------------------------------------------------- ##
## percent.female.pilots.comm.ATC
dat.lr$percent.female.pilots.comm.ATC <- NA

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "7"] <- 
  dat.lr$cond7.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "7"]

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "8"] <- 
  dat.lr$cond8.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "8"]

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "19"] <- 
  dat.lr$cond19.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "19"]

dat.lr$percent.female.pilots.comm.ATC[dat.lr$condition == "20"] <- 
  dat.lr$cond20.lk_percent.female.pilots.comm.ATC[dat.lr$condition == "20"]

count(dat.lr$percent.female.pilots.comm.ATC)
nrow(dat.lr)-783
  ## 162
count(dat.lr$condition.target.data)
  ## 162

hist(dat.lr$percent.female.pilots.comm.ATC)
  ## looks right; virtuall all female pilots communicate with ATC

## ------------------------------------------------------------------------- ##
## percent.female.flightattendants.comm.ATC
dat.lr$percent.female.flightattendants.comm.ATC <- NA

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "7"] <- 
  dat.lr$cond7.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "7"]

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "8"] <- 
  dat.lr$cond8.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "8"]

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "19"] <- 
  dat.lr$cond19.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "19"]

dat.lr$percent.female.flightattendants.comm.ATC[dat.lr$condition == "20"] <- 
  dat.lr$cond20.lk_percent.female.flightAttendants.comm.ATC[dat.lr$condition == "20"]

count(dat.lr$percent.female.flightattendants.comm.ATC)
nrow(dat.lr)-783
  ## 162
count(dat.lr$condition.target.data)
  ## 162

hist(dat.lr$percent.female.flightattendants.comm.ATC)
  ## looks right; virtually no female flight attendants communicate with ATC during a flight

## ------------------------------------------------------------------------- ##
## percent.female.pilots.special.line
dat.lr$percent.female.pilots.special.line <- NA

dat.lr$percent.female.pilots.special.line[dat.lr$condition == "9"] <- 
  dat.lr$cond9.lk_percent.female.pilots.special.line[dat.lr$condition == "9"]

dat.lr$percent.female.pilots.special.line[dat.lr$condition == "10"] <- 
  dat.lr$cond10.lk_percent.female.pilots.special.line[dat.lr$condition == "10"]

dat.lr$percent.female.pilots.special.line[dat.lr$condition == "21"] <- 
  dat.lr$cond21.lk_percent.female.pilots.special.line[dat.lr$condition == "21"]

dat.lr$percent.female.pilots.special.line[dat.lr$condition == "22"] <- 
  dat.lr$cond22.lk_percent.female.pilots.special.line[dat.lr$condition == "22"]

count(dat.lr$percent.female.pilots.special.line)
nrow(dat.lr)-789
  ## 156
count(dat.lr$condition.target.data)
  ## 156

hist(dat.lr$percent.female.pilots.special.line)
  ## looks right; virtually all female pilots go through special line


## ------------------------------------------------------------------------- ##
## percent.female.flightattendants.special.line
dat.lr$percent.female.flightattendants.special.line <- NA

dat.lr$percent.female.flightattendants.special.line[dat.lr$condition == "9"] <- 
  dat.lr$cond9.lk_percent.female.flightAttendants.special.line[dat.lr$condition == "9"]

dat.lr$percent.female.flightattendants.special.line[dat.lr$condition == "10"] <- 
  dat.lr$cond10.lk_percent.female.flightAttendants.special.line[dat.lr$condition == "10"]

dat.lr$percent.female.flightattendants.special.line[dat.lr$condition == "21"] <- 
  dat.lr$cond21.lk_percent.female.flightAttendants.special.line[dat.lr$condition == "21"]

dat.lr$percent.female.flightattendants.special.line[dat.lr$condition == "22"] <- 
  dat.lr$cond22.lk_percent.female.flightAttendants.special.line[dat.lr$condition == "22"]

count(dat.lr$percent.female.flightattendants.special.line)
nrow(dat.lr)-789
  ## 156
count(dat.lr$condition.target.data)
  ## 156

hist(dat.lr$percent.female.flightattendants.special.line)
  ## looks right; virtually all female flight attendants go through special line

## ------------------------------------------------------------------------- ##
## percent.female.pilots.serve.beverages
dat.lr$percent.female.pilots.serve.beverages <- NA

dat.lr$percent.female.pilots.serve.beverages[dat.lr$condition == "11"] <- 
  dat.lr$cond11.lk_percent.female.pilots.serve.beverages[dat.lr$condition == "11"]

dat.lr$percent.female.pilots.serve.beverages[dat.lr$condition == "12"] <- 
  dat.lr$cond12.lk_percent.female.pilots.serve.beverages[dat.lr$condition == "12"]

dat.lr$percent.female.pilots.serve.beverages[dat.lr$condition == "23"] <- 
  dat.lr$cond23.lk_percent.female.pilots.serve.beverages[dat.lr$condition == "23"]

dat.lr$percent.female.pilots.serve.beverages[dat.lr$condition == "24"] <- 
  dat.lr$cond24.lk_percent.female.pilots.serve.beverages[dat.lr$condition == "24"]

count(dat.lr$percent.female.pilots.serve.beverages)
nrow(dat.lr)-792
  ## 153
count(dat.lr$condition.target.data)
  ## 153

hist(dat.lr$percent.female.pilots.serve.beverages)
  ## looks right; virtually no female pilots serve beverages

## ------------------------------------------------------------------------- ##
## percent.female.flightattendants.serve.beverages
dat.lr$percent.female.flightattendants.serve.beverages <- NA

dat.lr$percent.female.flightattendants.serve.beverages[dat.lr$condition == "11"] <- 
  dat.lr$cond11.lk_percent.female.flightAttendants.serve.beverages[dat.lr$condition == "11"]

dat.lr$percent.female.flightattendants.serve.beverages[dat.lr$condition == "12"] <- 
  dat.lr$cond12.lk_percent.female.flightAttendants.serve.beverages[dat.lr$condition == "12"]

dat.lr$percent.female.flightattendants.serve.beverages[dat.lr$condition == "23"] <- 
  dat.lr$cond23.lk_percent.female.flightAttendants.serve.beverages[dat.lr$condition == "23"]

dat.lr$percent.female.flightattendants.serve.beverages[dat.lr$condition == "24"] <- 
  dat.lr$cond24.lk_percent.female.flightAttendants.serve.beverages[dat.lr$condition == "24"]

count(dat.lr$percent.female.flightattendants.serve.beverages)
nrow(dat.lr)-792
  ## 153
count(dat.lr$condition.target.data)
  ## 153

hist(dat.lr$percent.female.flightattendants.serve.beverages)
  ## looks right; virtually all female flight attendants serve beverages to passengers

## ======== ##
## ======== ##
## ======== ##
## Reorganize dataset
colnames(dat.lr)
LR.dat <- dat.lr[,c(1:5, 54:65)]
colnames(LR.dat)

LR.dat$likelihood.pilot <- NA

LR.dat$likelihood.pilot[LR.dat$condition.target.data == "man.special_line"] <- 
  LR.dat$percent.male.pilots.special.line[LR.dat$condition.target.data == "man.special_line"]

LR.dat$likelihood.pilot[LR.dat$condition.target.data == "man.served_beverages"] <- 
  LR.dat$percent.male.pilots.serve.beverages[LR.dat$condition.target.data == "man.served_beverages"]

LR.dat$likelihood.pilot[LR.dat$condition.target.data == "man.comm_ATC"] <- 
  LR.dat$percent.male.pilots.comm.ATC[LR.dat$condition.target.data == "man.comm_ATC"]

LR.dat$likelihood.pilot[LR.dat$condition.target.data == "woman.special_line"] <- 
  LR.dat$percent.female.pilots.special.line[LR.dat$condition.target.data == "woman.special_line"]

LR.dat$likelihood.pilot[LR.dat$condition.target.data == "woman.served_beverages"] <- 
  LR.dat$percent.female.pilots.serve.beverages[LR.dat$condition.target.data == "woman.served_beverages"]

LR.dat$likelihood.pilot[LR.dat$condition.target.data == "woman.comm_ATC"] <- 
  LR.dat$percent.female.pilots.comm.ATC[LR.dat$condition.target.data == "woman.comm_ATC"]

## --------------- ##
## --------------- ##
## --------------- ##
LR.dat$likelihood.flightattendant <- NA

LR.dat$likelihood.flightattendant[LR.dat$condition.target.data == "man.special_line"] <- 
  LR.dat$percent.male.flightattendants.special.line[LR.dat$condition.target.data == "man.special_line"]

LR.dat$likelihood.flightattendant[LR.dat$condition.target.data == "man.served_beverages"] <- 
  LR.dat$percent.male.flightattendants.serve.beverages[LR.dat$condition.target.data == "man.served_beverages"]

LR.dat$likelihood.flightattendant[LR.dat$condition.target.data == "man.comm_ATC"] <- 
  LR.dat$percent.male.flightattendants.comm.ATC[LR.dat$condition.target.data == "man.comm_ATC"]

LR.dat$likelihood.flightattendant[LR.dat$condition.target.data == "woman.special_line"] <- 
  LR.dat$percent.female.flightattendants.special.line[LR.dat$condition.target.data == "woman.special_line"]

LR.dat$likelihood.flightattendant[LR.dat$condition.target.data == "woman.served_beverages"] <- 
  LR.dat$percent.female.flightattendants.serve.beverages[LR.dat$condition.target.data == "woman.served_beverages"]

LR.dat$likelihood.flightattendant[LR.dat$condition.target.data == "woman.comm_ATC"] <- 
  LR.dat$percent.female.flightattendants.comm.ATC[LR.dat$condition.target.data == "woman.comm_ATC"]

## ======== ##
## ======== ##
## ======== ##
LR.dat$LR <- with(LR.dat, likelihood.pilot / likelihood.flightattendant)
count(LR.dat$LR)
  ## 6 NaNs

## Examine 6 NaNs
LR.NaN <- subset(LR.dat, is.nan(LR))
  ## All in special line condition, control condition

## Makes sense to exclude these 6 participants; math doesn't work out
LR.dat <- subset(LR.dat, is.nan(LR) == FALSE)

## Do data overwhelm prior completely?
LR.dat$overwhelm.prior <- ifelse(test = LR.dat$LR == 0, 
                                 yes = "Data entirely diagnostic of flight attendant", 
                                 no = ifelse(test = is.infinite(LR.dat$LR) == TRUE, 
                                             yes = "Data entirely diagnostic of pilot", 
                                             no = "Data not entirely diagnostic"))

LR.dat$overwhelm.prior <- factor(LR.dat$overwhelm.prior, 
                                 levels = c("Data entirely diagnostic of flight attendant", 
                                            "Data not entirely diagnostic", 
                                            "Data entirely diagnostic of pilot"))


levels(LR.dat$overwhelm.prior)

## Create LR for plotting purposes
## Infinity = 10,000
## Zero = 1/10,000
LR.dat$value.plotting <- ifelse(test = is.infinite(LR.dat$LR) == TRUE, 
                                yes = 10000, 
                                no = ifelse(test = LR.dat$LR == 0, 
                                            yes = 1/10000, 
                                            no = LR.dat$LR))

## condition for plotting purposes
count(LR.dat$condition.target.data)

LR.dat$condition.plotting <- NA
LR.dat[grepl("^man.special_line$", LR.dat$condition.target.data), "condition.plotting"] <- 1
LR.dat[grepl("^woman.special_line$", LR.dat$condition.target.data), "condition.plotting"] <- 2
LR.dat[grepl("^man.served_beverages$", LR.dat$condition.target.data), "condition.plotting"] <- 3
LR.dat[grepl("^woman.served_beverages$", LR.dat$condition.target.data), "condition.plotting"] <- 4
LR.dat[grepl("^man.comm_ATC$", LR.dat$condition.target.data), "condition.plotting"] <- 5
LR.dat[grepl("^woman.comm_ATC", LR.dat$condition.target.data), "condition.plotting"] <- 6

LR.dat$condition.plotting <- factor(x = LR.dat$condition.plotting, 
                                    levels = c(1:6), 
                                    labels = c("Man went through special line\nn = 159", 
                                               "Woman went through special line\nn = 152", 
                                               "Man served beverages\nn = 159", 
                                               "Woman served beverages\nn = 153", 
                                               "Man communicated w/ATC\nn = 154", 
                                               "Woman communicated w/ATC\nn = 162"))

## Make sure n's sum up to 939
159+152+159+153+154+162

## Relevel for plotting purposes
LR.dat$condition.plotting <- factor(LR.dat$condition.plotting, 
                                    levels = c("Man went through special line\nn = 159",
                                               "Man served beverages\nn = 159",
                                               "Man communicated w/ATC\nn = 154",
                                               "Woman went through special line\nn = 152",
                                               "Woman served beverages\nn = 153", 
                                               "Woman communicated w/ATC\nn = 162"))


## Visualize
ggplot(data = LR.dat, 
       mapping = aes(x = log(value.plotting), fill = overwhelm.prior)) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(xlim = c(-9, 9), ylim = c(0, 0.80)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.80, 0.10), 
                     label = percent_format()) + 
  scale_fill_manual(values = c("white", "darkgrey", "black")) + 
  labs(x = "\nLog Likelihood Ratio", y = "Proportion of participants (in each condition)\n") + 
  theme_bw(base_size = 14) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(face = "bold", size = 14)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

count(LR.dat$condition.plotting)

## Proportion who said infinity
tapply(LR.dat$value.plotting, 
       LR.dat$condition.plotting, 
       count)

## Woman comm w/ATC
27/162 ## 17%

## Man comm w/ATC
36/154 ## 23%

## ==================================================================================== ##
## Inferential statistics on likelihood ratios
## Comparisons between genders

## Medians
round(x = tapply(log(LR.dat$LR), 
                 LR.dat$condition.target.data, 
                 FUN = median), 
      digits = 2)

## Man vs. Woman - comm w/ATC
wilcox.test(log(LR) ~ target, 
            data = droplevels(subset(LR.dat, condition.target.data == "man.comm_ATC" | condition.target.data == "woman.comm_ATC")))
  ## Wilcoxon P = 0.39
  ## Effect size, Pearson's r = 0.05
qnorm(wilcox.test(log(LR) ~ target, 
                  data = droplevels(subset(LR.dat, condition.target.data == "man.comm_ATC" | condition.target.data == "woman.comm_ATC")))$p.value/2) / sqrt(154+162)

  ## Man vs. Woman - served beverages
wilcox.test(log(LR) ~ target, 
            data = droplevels(subset(LR.dat, condition.target.data == "man.served_beverages" | condition.target.data == "woman.served_beverages")))
  ## Wilcoxon P = 0.67
  ## Effect size, Pearson's r = 0.02
qnorm(wilcox.test(log(LR) ~ target, 
                  data = droplevels(subset(LR.dat, condition.target.data == "man.served_beverages" | condition.target.data == "woman.served_beverages")))$p.value/2) / sqrt(159+153)


## Man vs. Woman - special line
wilcox.test(log(LR) ~ target, 
            data = droplevels(subset(LR.dat, condition.target.data == "man.special_line" | condition.target.data == "woman.special_line")))
  ## Wilcoxon P = 0.01
  ## Effect size, Pearson's r = 0.14
qnorm(wilcox.test(log(LR) ~ target, 
                  data = droplevels(subset(LR.dat, condition.target.data == "man.special_line" | condition.target.data == "woman.special_line")))$p.value/2) / sqrt(159+152)


## ================================================================================================== ##
## Calculate model posterior; compare to reported posterior

## Create new dataframe
prior.model.reported <- merge(x = dat[,c("ResponseID", "target", "data", 
                                         "gender", "age",
                                         "Prior.Man.Pilot", "Prior.Woman.Pilot", "Prior.Target.Pilot", 
                                         "Posterior.Man.Pilot", "Posterior.Woman.Pilot", "Posterior.Target.Pilot")], 
                              y = LR.dat[,c("ResponseID", "LR")], 
                              by = "ResponseID",
                              all.x = FALSE, 
                              all.y = TRUE)

## Create prior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$prior.odds.ratio <- ifelse(test = prior.model.reported$target == "man", 
                                                yes = prior.model.reported$Prior.Man.Pilot / prior.model.reported$Prior.Woman.Pilot, 
                                                no = prior.model.reported$Prior.Woman.Pilot / prior.model.reported$Prior.Man.Pilot)

## Create posterior.odds.ratio: divide differently for when target is man vs. woman
prior.model.reported$posterior.odds.ratio <- ifelse(test = prior.model.reported$target == "man", 
                                                    yes = prior.model.reported$Posterior.Man.Pilot / prior.model.reported$Posterior.Woman.Pilot, 
                                                    no = prior.model.reported$Posterior.Woman.Pilot / prior.model.reported$Posterior.Man.Pilot)


## Calculate model.posterior.odds.ratio
prior.model.reported$model.posterior.odds.ratio <- with(prior.model.reported, 
                                                        prior.odds.ratio * LR)

## Convert the following to percent format:
  ## prior.odds.ratio
  ## posterior.odds.ratio
  ## model.posterior.odds.ratio

## prior.odds.ratio --> percent format
prior.model.reported$prior_percent <- with(prior.model.reported, prior.odds.ratio / (1 + prior.odds.ratio) )

tapply(X = prior.model.reported$prior_percent, 
       INDEX = list(prior.model.reported$target, prior.model.reported$data), 
       FUN = mean)

## posterior.odds.ratio --> percent format
prior.model.reported$posterior_percent <- ifelse(test = is.infinite(prior.model.reported$posterior.odds.ratio) == TRUE, 
                                                 yes = 1, 
                                                 no = ( prior.model.reported$posterior.odds.ratio / (1 + prior.model.reported$posterior.odds.ratio) )
                                                )

tapply(X = prior.model.reported$posterior_percent, 
       INDEX = list(prior.model.reported$target, prior.model.reported$data), 
       FUN = mean)

## model.posterior.odds.ratio --> percent format
prior.model.reported$model.posterior_percent <- ifelse(test = is.infinite(prior.model.reported$model.posterior.odds.ratio) == TRUE, 
                                                       yes = 1, 
                                                       no = ( prior.model.reported$model.posterior.odds.ratio / (1 + prior.model.reported$model.posterior.odds.ratio) )
                                                      )

tapply(X = prior.model.reported$model.posterior_percent, 
       INDEX = list(prior.model.reported$target, prior.model.reported$data), 
       FUN = mean)

## Calculate difference
prior.model.reported$diff <- with(prior.model.reported, posterior_percent - model.posterior_percent)


## ================================================================================================== ##
## Create long form version for plotting purposes
prior.model.reported_long <- melt(prior.model.reported, measure.vars = c("prior_percent", "posterior_percent", "model.posterior_percent"))
939*3
  ## Each subject gives three responses: prior, model posterior, and reported posterior

prior.model.reported_long$variable <- factor(prior.model.reported_long$variable, 
                                             levels = c("prior_percent", "model.posterior_percent", "posterior_percent"), 
                                             labels = c("Prior", "Model Posterior", "Reported Posterior"))

prior.model.reported_long$data <- factor(prior.model.reported$data, 
                                         levels = c("special_line", "served_beverages", "comm_ATC"), 
                                         labels = c("Went through special line", "Served beverages", "Communicated w/ATC"))

prior.model.reported_long$target <- factor(prior.model.reported_long$target, 
                                           levels = c("man", "woman"), 
                                           labels = c("Man", "Woman"))

## Plot means
ggplot(data = prior.model.reported_long, 
       aes(x = target, y = value, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  facet_wrap(~data, nrow = 1) + 
  theme_bw() + 
  labs(y = "Probability that target is pilot\n") + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format()) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(2, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(axis.title.x = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(aspect.ratio = 1) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## ================================================================================================== ##
## Inferential statistics

## All subjects
fit.p_vs_mp <- lme(fixed = value ~ variable * target * data, 
                   random = ~1|ResponseID, 
                   data = prior.model.reported_long, 
                   method = "ML")

## Dataframe of fitted values
names(allEffects(fit.p_vs_mp))
p_vs_mp.plot <- as.data.frame(allEffects(fit.p_vs_mp)[["variable:target:data"]])
p_vs_mp.plot <- droplevels(p_vs_mp.plot)

## Relevel for plotting purposes
levels(p_vs_mp.plot$data)

p_vs_mp.plot$data <- factor(p_vs_mp.plot$data, 
                            levels = c("Went through special line", "Served beverages", "Communicated w/ATC"))


levels(p_vs_mp.plot$variable)

p_vs_mp.plot$variable <- factor(p_vs_mp.plot$variable, 
                                levels = c("Prior", "Model Posterior", "Reported Posterior"))

## Plot fitted values
ggplot(data = p_vs_mp.plot, 
       aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  facet_wrap(~data, nrow = 1) + 
  theme_bw(base_size = 14) + 
  labs(x = "Target",
       y = expression(paste(italic(P), "(Target = Pilot)"))) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold", size = 14)) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## Obtain contrasts
contrasts <- contrast(lsmeans(object = fit.p_vs_mp, specs = ~variable * target | data), 
                      method = "pairwise", 
                      adjust = "none")

## ============================================================================================= ##

## Demographics

plyr::count(prior.model.reported$gender)
round(mean(prior.model.reported$age, na.rm = TRUE), digits = 2)
round(sd(prior.model.reported$age, na.rm = TRUE), digits = 2)


## ============================================================================================= ##
## Pull out commATC
commATC.contrasts <- as.data.frame(subset(summary(contrasts), data == "Communicated w/ATC"))

## Subset out only relevant contrasts
commATC.contrasts <- subset(commATC.contrasts,
                            contrast == "Prior,Man - Prior,Woman" |
                            contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                              contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                              contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                              contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
commATC.contrasts$effect.size <- with(commATC.contrasts, 
                                      sqrt((t.ratio^2)/(t.ratio^2 + df)))

commATC.contrasts

## Round P values
round(commATC.contrasts$p.value, digits = 2)

## Means
subset(p_vs_mp.plot, data == "Communicated w/ATC")[,c("variable", "target", "data", "fit")]

## Percent saying >50% for priors
table(prior.model.reported$Prior.Man.Pilot[prior.model.reported$data == "comm_ATC"] > 0.50)
244/(244+72)

## ============================================================================================= ##
## Subset out relevant served beverage contrasts
beverage.contrasts <- as.data.frame(subset(summary(contrasts), data == "Served beverages"))

beverage.contrasts <- subset(beverage.contrasts,
                             contrast == "Prior,Man - Prior,Woman" |
                               contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                               contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                               contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                               contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
beverage.contrasts$effect.size <- with(beverage.contrasts, 
                                       sqrt((t.ratio^2)/(t.ratio^2 + df)))

beverage.contrasts

## Round P values
round(beverage.contrasts$p.value, digits = 4)

## Means
subset(p_vs_mp.plot, data == "Served beverages")[,c("variable", "target", "data", "fit")]

## ============================================================================================= ##
## Subset out relevant special line contrasts
specialLine.contrasts <- as.data.frame(subset(summary(contrasts), data == "Went through special line"))

specialLine.contrasts <- subset(specialLine.contrasts,
                                contrast == "Prior,Man - Prior,Woman" |
                                  contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                                  contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                                  contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                                  contrast == "Model Posterior,Woman - Reported Posterior,Woman")

## Calculate effect size
specialLine.contrasts$effect.size <- with(specialLine.contrasts, 
                                          sqrt((t.ratio^2)/(t.ratio^2 + df)))

specialLine.contrasts

## Round P values
round(specialLine.contrasts$p.value, digits = 4)

## Means
subset(p_vs_mp.plot, data == "Went through special line")[,c("variable", "target", "data", "fit")]

## ============================================================================================================== ##
## comm w/ATC

## commATC likelihoods
commATC.likelihoods <- ggplot(data = subset(LR.dat, data == "comm_ATC"), 
                              mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 14.4) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 14.4)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

## commATC plot
commATC.plot <- ggplot(data = subset(p_vs_mp.plot, data == "Communicated w/ATC"), 
                       mapping = aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14.4) + 
  labs(x = "\nTarget",
       ##y = "\n\n\n\n\nP(Target = Pilot)"
       y = expression(paste(italic(P), "(Target = Pilot)"))
      ) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man communicated/ATC\nn = 154", "Woman communicated w/ATC\nn = 162")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## Fig. S18
plot_grid(commATC.likelihoods, commATC.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ============================================================================================================== ##
## ============================================================================================================== ##
## ============================================================================================================== ##

## At the individual subject level

diff.histogram <- ggplot(data = subset(prior.model.reported, data == "comm_ATC"), 
                         mapping = aes(x = diff)) + 
  geom_histogram(mapping = aes(y = (..count..)/sum(..count..)), 
                 color = "black", fill = "lightgrey") + 
  coord_cartesian(ylim = c(0, 0.40)) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  theme(aspect.ratio = 1) + 
  labs(x = "\nReported Posterior - Model Posterior", 
       y = "Proportion of participants\n") + 
  theme(axis.title = element_text(size = 13)) + 
  theme(axis.text = element_text(size = 13))

## Sensitivity to likelihoods
summary(log(prior.model.reported$LR[prior.model.reported$data == "comm_ATC"]))

summary(lm(formula = posterior_percent ~ LR + prior_percent, 
           data = subset(prior.model.reported, data == "comm_ATC" & is.infinite(LR) == FALSE)))

sqrt( (5.771^2) / ( (5.771^2) + 250) )

lm.beta::lm.beta(lm(formula = posterior_percent ~ LR + prior_percent, 
                    data = subset(prior.model.reported, data == "comm_ATC" & is.infinite(LR) == FALSE)))


commATC.LR.reported <- subset(prior.model.reported, data == "comm_ATC")


commATC.LR.reported$LR.plotting <- commATC.LR.reported$LR

count(commATC.LR.reported$LR.plotting)

commATC.LR.reported$LR.plotting <- ifelse(test = is.infinite(commATC.LR.reported$LR.plotting) == TRUE, 
                                          yes = exp(10), 
                                          no = commATC.LR.reported$LR.plotting)


commATC.LR.reported$LR.type <- ifelse(test = is.infinite(commATC.LR.reported$LR) == TRUE, 
                                      yes = "Inf", 
                                      no = "Not Inf")


LR.reportedPosterior.scatter <- ggplot(data = commATC.LR.reported, 
                                       mapping = aes(x = log(LR.plotting), y = posterior_percent, color = LR.type)) + 
  geom_jitter() + 
  geom_smooth(method = "lm") + 
  coord_cartesian(xlim = c(-3, 10), ylim = c(0, 1)) + 
  scale_x_continuous(breaks = seq(-2, 10, 2), 
                     labels = c("-2", "0", "2", "4", "6", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     label = percent_format()) + 
  labs(x = "\nLog Likelihood Ratio", 
       y = "Reported Posterior\n") + 
  theme(legend.position = "none") + 
  theme(aspect.ratio = 10/6)

## Stitch two plots together
## Fig. S19
plot_grid(diff.histogram, LR.reportedPosterior.scatter, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 1),
          rel_heights = c(1, 1))

cor.test(x = commATC.LR.reported$LR[commATC.LR.reported$LR.type == "Not Inf"], 
         y = commATC.LR.reported$posterior_percent[commATC.LR.reported$LR.type == "Not Inf"])


## ============================================================================================================== ##
## ============================================================================================================== ##
## ============================================================================================================== ##
## beverages

## beverage likelihoods
beverage.likelihoods <- ggplot(data = subset(LR.dat, data == "served_beverages"), 
                               mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(ylim = c(0, 0.70)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.70, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 14.4) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 14.4)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

## beverages plot
beverages.plot <- ggplot(data = subset(p_vs_mp.plot, data == "Served beverages"), 
                         mapping = aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14.4) + 
  labs(x = "\nTarget",
       ##y = "\n\n\n\n\nP(Target = Pilot)"
       y = expression(paste(italic(P), "(Target = Pilot)"))
      ) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man served beverages\nn = 159", "Woman served beverages\nn = 153")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## Fig. S16
plot_grid(beverage.likelihoods, beverages.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ============================================================================================================== ##
## special line

## special line likelihoods
specialLine.likelihoods <- ggplot(data = subset(LR.dat, data == "special_line"), 
                                  mapping = aes(x = log(value.plotting))) + 
  geom_histogram(mapping = aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), 
                 binwidth = 0.50, 
                 breaks = seq(-10.25, 10.25, 0.50), 
                 color = "black", fill = "#00BFC4") + 
  facet_wrap(~condition.plotting, nrow = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  coord_cartesian(ylim = c(0, 0.90)) + 
  scale_x_continuous(breaks = seq(-9, 9, 1), 
                     labels = c("-Inf", " ", " ", " ",
                                " ", "-4", " ", "-2", " ",
                                "0", 
                                " ", "2", " ", "4", " ", 
                                " ", " ", " ", "+Inf")) + 
  scale_y_continuous(breaks = seq(0, 0.90, 0.10), 
                     label = percent_format(), expand = c(0, 0)) + 
  labs(x = "Log Likelihood Ratio", y = "Proportion of participants (in each condition)") + 
  theme_bw(base_size = 14.4) + 
  theme(aspect.ratio = 1) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(panel.grid.minor = element_blank()) + 
  theme(legend.title = element_blank()) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 13)) + 
  theme(panel.spacing = unit(3.5, "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical") + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black"))

## special line plot
specialLine.plot <- ggplot(data = subset(p_vs_mp.plot, data == "Went through special line"), 
                           mapping = aes(x = target, y = fit, fill = variable)) + 
  stat_summary(fun.y = mean, geom = "bar", 
               aes(width = 0.75), position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(width = 0.2, ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.75), lwd = 1.5) + 
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#C77CFF")) + 
  theme_bw(base_size = 14.4) + 
  labs(x = "\nTarget",
       ##y = "\n\n\n\n\nP(Target = Pilot)"
       y = expression(paste(italic(P), "(Target = Pilot)"))
       ) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(breaks = c("Man", "Woman"), 
                   labels = c("Man went through\nspecial line\nn = 159", 
                              "Woman went through\nspecial line\nn = 152")) + 
  scale_y_continuous(breaks = pretty_breaks(11), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(strip.background = element_rect(color = "white", fill = "white")) +
  theme(panel.spacing = unit(4, "lines")) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.text.x = element_text(color = "black", face = "bold")) + 
  theme(legend.key.size = unit(1, "lines")) + 
  theme(legend.title = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + theme(axis.line = element_line(color = "black")) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin = unit(c(0, 0, 0, 3.5), "lines")) + 
  theme(legend.position = "bottom") + 
  theme(legend.direction = "vertical")

## Fig. S17
plot_grid(specialLine.likelihoods, specialLine.plot, 
          labels=c("A", "B"), 
          ncol = 2, nrow = 1, 
          rel_widths = c(1, 2),
          rel_heights = c(0.75, 1))

## ================================================================================================================================== ##
## ================================================================================================================================== ##

## Robustness check with various adjustment factors
## Do this with same dataset but different name
pilot <- prior.model.reported_long
pilot$logit.transformed.value <- NULL

## Vector of adjustment factors
adjustment.factor <- seq(from = 0.005, to = 0.10, by = 0.005)

## Empty output list; to be filled with dataframe, one for each adjustment factor
output <- list()

## Loop through and obtain dataframe for each adjustment factor

for (i in 1:length(adjustment.factor)) {
  
  ## Print which adjustment factor is being processed; should go from 0.005 to 0.10 in increments of 0.005
  print(adjustment.factor[i])
  
  ## Create transformed DV
  pilot$logit.transformed.value <- logit(p = pilot$value, 
                                         adjust = adjustment.factor[i])
  
  ## Fit model
  fit.transformed <- lme(fixed = logit.transformed.value ~ variable * target * data,
                         random = ~1|ResponseID/variable, 
                         data = pilot, 
                         method = "ML")
  
  ## Obtain contrasts
  contrasts <- contrast(lsmeans(object = fit.transformed, specs = ~variable * target | data), 
                        method = "pairwise", 
                        adjust = "none")
  
  ## Pull out surgery condition
  commATC.contrasts <- as.data.frame(subset(summary(contrasts), data == "Communicated w/ATC"))
  
  ## Subset out only relevant contrasts
  commATC.contrasts <- subset(commATC.contrasts,
                              contrast == "Model Posterior,Man - Model Posterior,Woman" | 
                                contrast == "Reported Posterior,Man - Reported Posterior,Woman" | 
                                contrast == "Model Posterior,Man - Reported Posterior,Man" | 
                                contrast == "Model Posterior,Woman - Reported Posterior,Woman")
  
  ## Calculate effect size
  commATC.contrasts$effect.size <- with(commATC.contrasts, 
                                        sqrt((t.ratio^2)/(t.ratio^2 + df)))
  
  ## Add variable to indicate which adjustment factor
  commATC.contrasts$adjustment.factor <- adjustment.factor[i]
  
  ## Save dataframe in output
  output[[i]] <- commATC.contrasts
  
}

## Combine all dataframes in output into single dataframe
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}
adjust.df <- bind_rows(output)

## Clean up adjust.df
adjust.df <- droplevels(adjust.df)

## Relevel contrast
levels(adjust.df$contrast)

adjust.df$contrast <- factor(adjust.df$contrast, 
                             levels = c("Model Posterior,Man - Model Posterior,Woman", 
                                        "Reported Posterior,Man - Reported Posterior,Woman", 
                                        "Model Posterior,Man - Reported Posterior,Man", 
                                        "Model Posterior,Woman - Reported Posterior,Woman"))

## Plot P values
## Fig. S20
ggplot(data = adjust.df, 
       mapping = aes(x = adjustment.factor, y = p.value)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~contrast, nrow = 2, ncol = 2) + 
  geom_hline(yintercept = 0.05, color = "red") + 
  labs(x = "Adjustment factor", 
       y =  expression(paste(italic(P), " value"))) + 
  coord_cartesian(xlim = c(0, 0.10), ylim = c(0, 0.40)) + 
  scale_x_continuous(breaks = seq(0, 0.10, 0.01)) + 
  scale_y_continuous(breaks = seq(0, 0.40, 0.10)) + 
  theme_bw(base_size = 13) + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(strip.background = element_rect(fill = "white")) + 
  theme(strip.background =element_rect(color = "white")) + 
  theme(strip.text.x = element_text(face = "bold", size = 12)) + 
  theme(aspect.ratio = 4/10) + 
  theme(panel.spacing = unit(4, "lines"))



