## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Supplemental Study - Pilot Probability Judgments: Data prepration

## Packages needed
if (!require("plyr")) {install.packages("plyr"); require("plyr")}
if (!require("reshape2")) {install.packages("reshape2"); require("reshape2")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("scales")) {install.packages("scales"); require("scales")}
if (!require("nlme")) {install.packages("nlme"); require("nlme")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("multcomp")) {install.packages("multcomp"); require("multcomp")}
if (!require("lsmeans")) {install.packages("lsmeans"); require("lsmeans")}
if (!require("grid")) {install.packages("grid"); require("grid")}

## Load data
load("SupplementalStudy_PilotProbabilityJudgments.Rda")

## Rename to dat
dat <- study2b_rawDdat

## Rename "ResponseId" to "ResponseID"
colnames(dat)[3] <- "ResponseID"

## Get rid of unnecessary rows
dat <- dat[c(1, 48:1011),]

## Drop unused levels
dat <- droplevels(x = dat)

## Rename main data columns: 5-148
colnames(dat)

## cond1
colnames(dat)[5] <- "cond1.lk_percent.male.pilots.comm.ATC"
colnames(dat)[6] <- "cond1.lk_percent.male.flightAttendants.comm.ATC"
colnames(dat)[7] <- "cond1.prior_manPilot.womanFlightAttendant"
colnames(dat)[8] <- "cond1.prior_womanPilot.manFlightAttendant"
colnames(dat)[9] <- "cond1.post_manPilot.womanFlightAttendant"
colnames(dat)[10] <- "cond1.post_womanPilot.manFlightAttendant"

## cond2
colnames(dat)[11] <- "cond2.prior_manPilot.womanFlightAttendant"
colnames(dat)[12] <- "cond2.prior_womanPilot.manFlightAttendant"
colnames(dat)[13] <- "cond2.post_manPilot.womanFlightAttendant"
colnames(dat)[14] <- "cond2.post_womanPilot.manFlightAttendant"
colnames(dat)[15] <- "cond2.lk_percent.male.pilots.comm.ATC"
colnames(dat)[16] <- "cond2.lk_percent.male.flightAttendants.comm.ATC"

## cond3
colnames(dat)[17] <- "cond3.lk_percent.male.pilots.special.line"
colnames(dat)[18] <- "cond3.lk_percent.male.flightAttendants.special.line"
colnames(dat)[19] <- "cond3.prior_manPilot.womanFlightAttendant"
colnames(dat)[20] <- "cond3.prior_womanPilot.manFlightAttendant"
colnames(dat)[21] <- "cond3.post_manPilot.womanFlightAttendant"
colnames(dat)[22] <- "cond3.post_womanPilot.manFlightAttendant"

## cond4
colnames(dat)[23] <- "cond4.prior_manPilot.womanFlightAttendant"
colnames(dat)[24] <- "cond4.prior_womanPilot.manFlightAttendant"
colnames(dat)[25] <- "cond4.post_manPilot.womanFlightAttendant"
colnames(dat)[26] <- "cond4.post_womanPilot.manFlightAttendant"
colnames(dat)[27] <- "cond4.lk_percent.male.pilots.special.line"
colnames(dat)[28] <- "cond4.lk_percent.male.flightAttendants.special.line"

## cond5
colnames(dat)[29] <- "cond5.lk_percent.male.pilots.serve.beverages"
colnames(dat)[30] <- "cond5.lk_percent.male.flightAttendants.serve.beverages"
colnames(dat)[31] <- "cond5.prior_manPilot.womanFlightAttendant"
colnames(dat)[32] <- "cond5.prior_womanPilot.manFlightAttendant"
colnames(dat)[33] <- "cond5.post_manPilot.womanFlightAttendant"
colnames(dat)[34] <- "cond5.post_womanPilot.manFlightAttendant"

## cond6
colnames(dat)[35] <- "cond6.prior_manPilot.womanFlightAttendant"
colnames(dat)[36] <- "cond6.prior_womanPilot.manFlightAttendant"
colnames(dat)[37] <- "cond6.post_manPilot.womanFlightAttendant"
colnames(dat)[38] <- "cond6.post_womanPilot.manFlightAttendant"
colnames(dat)[39] <- "cond6.lk_percent.male.pilots.serve.beverages"
colnames(dat)[40] <- "cond6.lk_percent.male.flightAttendants.serve.beverages"

## cond7
colnames(dat)[41] <- "cond7.lk_percent.female.pilots.comm.ATC"
colnames(dat)[42] <- "cond7.lk_percent.female.flightAttendants.comm.ATC"
colnames(dat)[43] <- "cond7.prior_manPilot.womanFlightAttendant"
colnames(dat)[44] <- "cond7.prior_womanPilot.manFlightAttendant"
colnames(dat)[45] <- "cond7.post_manPilot.womanFlightAttendant"
colnames(dat)[46] <- "cond7.post_womanPilot.manFlightAttendant"

## cond8
colnames(dat)[47] <- "cond8.prior_manPilot.womanFlightAttendant"
colnames(dat)[48] <- "cond8.prior_womanPilot.manFlightAttendant"
colnames(dat)[49] <- "cond8.post_manPilot.womanFlightAttendant"
colnames(dat)[50] <- "cond8.post_womanPilot.manFlightAttendant"
colnames(dat)[51] <- "cond8.lk_percent.female.pilots.comm.ATC"
colnames(dat)[52] <- "cond8.lk_percent.female.flightAttendants.comm.ATC"

## cond9
colnames(dat)[53] <- "cond9.lk_percent.female.pilots.special.line"
colnames(dat)[54] <- "cond9.lk_percent.female.flightAttendants.special.line"
colnames(dat)[55] <- "cond9.prior_manPilot.womanFlightAttendant"
colnames(dat)[56] <- "cond9.prior_womanPilot.manFlightAttendant"
colnames(dat)[57] <- "cond9.post_manPilot.womanFlightAttendant"
colnames(dat)[58] <- "cond9.post_womanPilot.manFlightAttendant"

## cond10
colnames(dat)[59] <- "cond10.prior_manPilot.womanFlightAttendant"
colnames(dat)[60] <- "cond10.prior_womanPilot.manFlightAttendant"
colnames(dat)[61] <- "cond10.post_manPilot.womanFlightAttendant"
colnames(dat)[62] <- "cond10.post_womanPilot.manFlightAttendant"
colnames(dat)[63] <- "cond10.lk_percent.female.pilots.special.line"
colnames(dat)[64] <- "cond10.lk_percent.female.flightAttendants.special.line"

## cond11
colnames(dat)[65] <- "cond11.lk_percent.female.pilots.serve.beverages"
colnames(dat)[66] <- "cond11.lk_percent.female.flightAttendants.serve.beverages"
colnames(dat)[67] <- "cond11.prior_manPilot.womanFlightAttendant"
colnames(dat)[68] <- "cond11.prior_womanPilot.manFlightAttendant"
colnames(dat)[69] <- "cond11.post_manPilot.womanFlightAttendant"
colnames(dat)[70] <- "cond11.post_womanPilot.manFlightAttendant"

## cond12
colnames(dat)[71] <- "cond12.prior_manPilot.womanFlightAttendant"
colnames(dat)[72] <- "cond12.prior_womanPilot.manFlightAttendant"
colnames(dat)[73] <- "cond12.post_manPilot.womanFlightAttendant"
colnames(dat)[74] <- "cond12.post_womanPilot.manFlightAttendant"
colnames(dat)[75] <- "cond12.lk_percent.female.pilots.serve.beverages"
colnames(dat)[76] <- "cond12.lk_percent.female.flightAttendants.serve.beverages"

## =================================================================================== ##

## cond13
colnames(dat)[77] <- "cond13.lk_percent.male.pilots.comm.ATC"
colnames(dat)[78] <- "cond13.lk_percent.male.flightAttendants.comm.ATC"
colnames(dat)[79] <- "cond13.prior_womanPilot.manFlightAttendant"
colnames(dat)[80] <- "cond13.prior_manPilot.womanFlightAttendant"
colnames(dat)[81] <- "cond13.post_womanPilot.manFlightAttendant"
colnames(dat)[82] <- "cond13.post_manPilot.womanFlightAttendant"

## cond14
colnames(dat)[83] <- "cond14.prior_womanPilot.manFlightAttendant"
colnames(dat)[84] <- "cond14.prior_manPilot.womanFlightAttendant"
colnames(dat)[85] <- "cond14.post_womanPilot.manFlightAttendant"
colnames(dat)[86] <- "cond14.post_manPilot.womanFlightAttendant"
colnames(dat)[87] <- "cond14.lk_percent.male.pilots.comm.ATC"
colnames(dat)[88] <- "cond14.lk_percent.male.flightAttendants.comm.ATC"

## cond15
colnames(dat)[89] <- "cond15.lk_percent.male.pilots.special.line"
colnames(dat)[90] <- "cond15.lk_percent.male.flightAttendants.special.line"
colnames(dat)[91] <- "cond15.prior_womanPilot.manFlightAttendant"
colnames(dat)[92] <- "cond15.prior_manPilot.womanFlightAttendant"
colnames(dat)[93] <- "cond15.post_womanPilot.manFlightAttendant"
colnames(dat)[94] <- "cond15.post_manPilot.womanFlightAttendant"

## cond16
colnames(dat)[95] <- "cond16.prior_womanPilot.manFlightAttendant"
colnames(dat)[96] <- "cond16.prior_manPilot.womanFlightAttendant"
colnames(dat)[97] <- "cond16.post_womanPilot.manFlightAttendant"
colnames(dat)[98] <- "cond16.post_manPilot.womanFlightAttendant"
colnames(dat)[99] <- "cond16.lk_percent.male.pilots.special.line"
colnames(dat)[100] <- "cond16.lk_percent.male.flightAttendants.special.line"

## cond17
colnames(dat)[101] <- "cond17.lk_percent.male.pilots.serve.beverages"
colnames(dat)[102] <- "cond17.lk_percent.male.flightAttendants.serve.beverages"
colnames(dat)[103] <- "cond17.prior_womanPilot.manFlightAttendant"
colnames(dat)[104] <- "cond17.prior_manPilot.womanFlightAttendant"
colnames(dat)[105] <- "cond17.post_womanPilot.manFlightAttendant"
colnames(dat)[106] <- "cond17.post_manPilot.womanFlightAttendant"

## cond18
colnames(dat)[107] <- "cond18.prior_womanPilot.manFlightAttendant"
colnames(dat)[108] <- "cond18.prior_manPilot.womanFlightAttendant"
colnames(dat)[109] <- "cond18.post_womanPilot.manFlightAttendant"
colnames(dat)[110] <- "cond18.post_manPilot.womanFlightAttendant"
colnames(dat)[111] <- "cond18.lk_percent.male.pilots.serve.beverages"
colnames(dat)[112] <- "cond18.lk_percent.male.flightAttendants.serve.beverages"

## cond19
colnames(dat)[113] <- "cond19.lk_percent.female.pilots.comm.ATC"
colnames(dat)[114] <- "cond19.lk_percent.female.flightAttendants.comm.ATC"
colnames(dat)[115] <- "cond19.prior_womanPilot.manFlightAttendant"
colnames(dat)[116] <- "cond19.prior_manPilot.womanFlightAttendant"
colnames(dat)[117] <- "cond19.post_womanPilot.manFlightAttendant"
colnames(dat)[118] <- "cond19.post_manPilot.womanFlightAttendant"

## cond20
colnames(dat)[119] <- "cond20.prior_womanPilot.manFlightAttendant"
colnames(dat)[120] <- "cond20.prior_manPilot.womanFlightAttendant"
colnames(dat)[121] <- "cond20.post_womanPilot.manFlightAttendant"
colnames(dat)[122] <- "cond20.post_manPilot.womanFlightAttendant"
colnames(dat)[123] <- "cond20.lk_percent.female.pilots.comm.ATC"
colnames(dat)[124] <- "cond20.lk_percent.female.flightAttendants.comm.ATC"

## cond21
colnames(dat)[125] <- "cond21.lk_percent.female.pilots.special.line"
colnames(dat)[126] <- "cond21.lk_percent.female.flightAttendants.special.line"
colnames(dat)[127] <- "cond21.prior_womanPilot.manFlightAttendant"
colnames(dat)[128] <- "cond21.prior_manPilot.womanFlightAttendant"
colnames(dat)[129] <- "cond21.post_womanPilot.manFlightAttendant"
colnames(dat)[130] <- "cond21.post_manPilot.womanFlightAttendant"

## cond22
colnames(dat)[131] <- "cond22.prior_womanPilot.manFlightAttendant"
colnames(dat)[132] <- "cond22.prior_manPilot.womanFlightAttendant"
colnames(dat)[133] <- "cond22.post_womanPilot.manFlightAttendant"
colnames(dat)[134] <- "cond22.post_manPilot.womanFlightAttendant"
colnames(dat)[135] <- "cond22.lk_percent.female.pilots.special.line"
colnames(dat)[136] <- "cond22.lk_percent.female.flightAttendants.special.line"

## cond23
colnames(dat)[137] <- "cond23.lk_percent.female.pilots.serve.beverages"
colnames(dat)[138] <- "cond23.lk_percent.female.flightAttendants.serve.beverages"
colnames(dat)[139] <- "cond23.prior_womanPilot.manFlightAttendant"
colnames(dat)[140] <- "cond23.prior_manPilot.womanFlightAttendant"
colnames(dat)[141] <- "cond23.post_womanPilot.manFlightAttendant"
colnames(dat)[142] <- "cond23.post_manPilot.womanFlightAttendant"

## cond24
colnames(dat)[143] <- "cond24.prior_womanPilot.manFlightAttendant"
colnames(dat)[144] <- "cond24.prior_manPilot.womanFlightAttendant"
colnames(dat)[145] <- "cond24.post_womanPilot.manFlightAttendant"
colnames(dat)[146] <- "cond24.post_manPilot.womanFlightAttendant"
colnames(dat)[147] <- "cond24.lk_percent.female.pilots.serve.beverages"
colnames(dat)[148] <- "cond24.lk_percent.female.flightAttendants.serve.beverages"

## Confirm column names
confirm.column.names <- data.frame(col.names = c(colnames(dat)[5:148]),
                                   col.description = t(dat[1, c(5:148)])
                                  )

rownames(confirm.column.names) <- NULL

## Remove first row
dat <- dat[c(2:965),]
dat <- droplevels(dat)

## ========================================== ##

## Convert main data to from factor to numeric
str(dat)
colnames(dat)

dat[5:148] <- sapply(X = dat[5:148], 
                     FUN = as.character)

dat[5:148] <- sapply(X = dat[5:148], 
                     FUN = as.numeric)

## Convert to decimal out of 1
divide.by.100 <- function(x) {
  x/100
}

dat[5:148] <- sapply(X = dat[5:148], 
                     FUN = divide.by.100)

## ========================================== ##
## Clean up demographics

## gender
count(dat$gender)
  ## 4 unspecified
  ## 523 females
  ## 437 males

dat$gender <- factor(x = dat$gender, 
                     levels = c("Female", "Male"))

## education
count(dat$education)

dat$education <- factor(dat$education, 
                        levels = c("Junior High", 
                                   "Some High School", 
                                   "High School Graduate",
                                   "Some College", 
                                   "Associate's Degree", 
                                   "Bachelor's Degree",
                                   "Some Graduate School", 
                                   "Master's Degree", 
                                   "PhD", 
                                   "MBA", 
                                   "JD", 
                                   "MD",
                                   "Other Advanced Degree"))

## b.month
count(dat$b.month)
dat$b.month <- factor(x = dat$b.month, 
                      levels = c("January", "February", "March", "April", "May", "June", 
                                 "July", "August", "September", "October", "November", "December"))

dat$b.month.numeric <- NA
dat[grepl("^January$", dat$b.month), "b.month.numeric"] <- 1
dat[grepl("^February$", dat$b.month), "b.month.numeric"] <- 2
dat[grepl("^March$", dat$b.month), "b.month.numeric"] <- 3
dat[grepl("^April$", dat$b.month), "b.month.numeric"] <- 4
dat[grepl("^May$", dat$b.month), "b.month.numeric"] <- 5
dat[grepl("^June$", dat$b.month), "b.month.numeric"] <- 6
dat[grepl("^July$", dat$b.month), "b.month.numeric"] <- 7
dat[grepl("^August$", dat$b.month), "b.month.numeric"] <- 8
dat[grepl("^September$", dat$b.month), "b.month.numeric"] <- 9
dat[grepl("^October$", dat$b.month), "b.month.numeric"] <- 10
dat[grepl("^November$", dat$b.month), "b.month.numeric"] <- 11
dat[grepl("^December$", dat$b.month), "b.month.numeric"] <- 12

## b.year
dat$b.year <- as.numeric(levels(dat$b.year))[dat$b.year]

## Create birthday for each participant; assume everyone was born on 1st day of month
dat$birthday <- paste(dat$b.month.numeric, 
                      "1",          
                      dat$b.year, 
                      sep = "/")

dat$birthday <- as.POSIXct(dat$birthday, 
                           format = "%m/%d/%Y")


## politics
count(dat$politics)
dat$politics <- factor(x = dat$politics, 
                       levels = c("Strongly Liberal", "Moderately Liberal", "Slightly Liberal", 
                                  "Neutral/Moderate", 
                                  "Slightly Conservative", "Moderately Conservative", "Strongly Conservative"))

## lookup
count(dat$lookup)
dat$lookup <- factor(x = dat$lookup, 
                     levels = c("No", "Yes"))


colnames(dat)

## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Create new variable, how much time each respondent spent on survey, in minutes
dat$Total.Time.Spent <- with(dat, (EndDate - StartDate))
dat$Total.Time.Spent <- as.numeric(dat$Total.Time.Spent)

summary(dat$Total.Time.Spent)

## Calculate age for each participant
  ## Get each participant's StartDate

dat$Date.of.Completion <- sapply(X = strsplit(x = as.character(dat$StartDate), split = " "), 
                                 "[", 
                                 1)

dat$Date.of.Completion <- as.POSIXct(dat$Date.of.Completion, 
                                     format = "%Y-%m-%d")

dat$age <- with(dat, (Date.of.Completion - birthday)/365)

dat$age <- as.numeric(dat$age)

colnames(dat)

## =================================================================== ##
## Specify what was different about each condition

dat$order <- NA
dat[grepl("^1$", dat$condition), "order"] <- "LPP"
dat[grepl("^2$", dat$condition), "order"] <- "PPL"
dat[grepl("^3$", dat$condition), "order"] <- "LPP"
dat[grepl("^4$", dat$condition), "order"] <- "PPL"
dat[grepl("^5$", dat$condition), "order"] <- "LPP"
dat[grepl("^6$", dat$condition), "order"] <- "PPL"
dat[grepl("^7$", dat$condition), "order"] <- "LPP"
dat[grepl("^8$", dat$condition), "order"] <- "PPL"
dat[grepl("^9$", dat$condition), "order"] <- "LPP"
dat[grepl("^10$", dat$condition), "order"] <- "PPL"
dat[grepl("^11$", dat$condition), "order"] <- "LPP"
dat[grepl("^12$", dat$condition), "order"] <- "PPL"
dat[grepl("^13$", dat$condition), "order"] <- "LPP"
dat[grepl("^14$", dat$condition), "order"] <- "PPL"
dat[grepl("^15$", dat$condition), "order"] <- "LPP"
dat[grepl("^16$", dat$condition), "order"] <- "PPL"
dat[grepl("^17$", dat$condition), "order"] <- "LPP"
dat[grepl("^18$", dat$condition), "order"] <- "PPL"
dat[grepl("^19$", dat$condition), "order"] <- "LPP"
dat[grepl("^20$", dat$condition), "order"] <- "PPL"
dat[grepl("^21$", dat$condition), "order"] <- "LPP"
dat[grepl("^22$", dat$condition), "order"] <- "PPL"
dat[grepl("^23$", dat$condition), "order"] <- "LPP"
dat[grepl("^24$", dat$condition), "order"] <- "PPL"

dat$order <- factor(x = dat$order, 
                    levels = c("LPP", "PPL"))


dat$target <- NA
dat[grepl("^1$", dat$condition), "target"] <- "man"
dat[grepl("^2$", dat$condition), "target"] <- "man"
dat[grepl("^3$", dat$condition), "target"] <- "man"
dat[grepl("^4$", dat$condition), "target"] <- "man"
dat[grepl("^5$", dat$condition), "target"] <- "man"
dat[grepl("^6$", dat$condition), "target"] <- "man"
dat[grepl("^7$", dat$condition), "target"] <- "woman"
dat[grepl("^8$", dat$condition), "target"] <- "woman"
dat[grepl("^9$", dat$condition), "target"] <- "woman"
dat[grepl("^10$", dat$condition), "target"] <- "woman"
dat[grepl("^11$", dat$condition), "target"] <- "woman"
dat[grepl("^12$", dat$condition), "target"] <- "woman"
dat[grepl("^13$", dat$condition), "target"] <- "man"
dat[grepl("^14$", dat$condition), "target"] <- "man"
dat[grepl("^15$", dat$condition), "target"] <- "man"
dat[grepl("^16$", dat$condition), "target"] <- "man"
dat[grepl("^17$", dat$condition), "target"] <- "man"
dat[grepl("^18$", dat$condition), "target"] <- "man"
dat[grepl("^19$", dat$condition), "target"] <- "woman"
dat[grepl("^20$", dat$condition), "target"] <- "woman"
dat[grepl("^21$", dat$condition), "target"] <- "woman"
dat[grepl("^22$", dat$condition), "target"] <- "woman"
dat[grepl("^23$", dat$condition), "target"] <- "woman"
dat[grepl("^24$", dat$condition), "target"] <- "woman"

dat$target <- factor(x = dat$target, 
                     levels = c("man", "woman"))

dat$data <- NA
dat[grepl("^1$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^2$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^3$", dat$condition), "data"] <- "special_line"
dat[grepl("^4$", dat$condition), "data"] <- "special_line"
dat[grepl("^5$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^6$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^7$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^8$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^9$", dat$condition), "data"] <- "special_line"
dat[grepl("^10$", dat$condition), "data"] <- "special_line"
dat[grepl("^11$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^12$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^13$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^14$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^15$", dat$condition), "data"] <- "special_line"
dat[grepl("^16$", dat$condition), "data"] <- "special_line"
dat[grepl("^17$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^18$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^19$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^20$", dat$condition), "data"] <- "comm_ATC"
dat[grepl("^21$", dat$condition), "data"] <- "special_line"
dat[grepl("^22$", dat$condition), "data"] <- "special_line"
dat[grepl("^23$", dat$condition), "data"] <- "served_beverages"
dat[grepl("^24$", dat$condition), "data"] <- "served_beverages"

dat$data <- factor(x = dat$data, 
                   levels = c("special_line", "served_beverages", "comm_ATC"))

dat$question_order <- NA
dat[grepl("^1$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^2$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^3$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^4$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^5$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^6$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^7$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^8$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^9$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^10$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^11$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^12$", dat$condition), "question_order"] <- "man_pilot_1st"
dat[grepl("^13$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^14$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^15$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^16$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^17$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^18$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^19$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^20$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^21$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^22$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^23$", dat$condition), "question_order"] <- "woman_pilot_1st"
dat[grepl("^24$", dat$condition), "question_order"] <- "woman_pilot_1st"

dat$question_order <- factor(x = dat$question_order, 
                             levels = c("man_pilot_1st", "woman_pilot_1st"))

## =================================================================== ##

## Rearrange columns
colnames(dat)
dat <- dat[,c(3, 4,                       ## ResponseID, MTurkID
              1, 2, 160, 159,             ## StartDate, EndDate, Total.Time.Spent, Date.of.Completion, 
              156,                        ## condition
              162:165,                    ## order, target, data, question_order
              149, 150, 153, 154,         ## gender, education, politics, lookup
              151, 157, 152, 158, 161,    ## b.month, b.month.numeric, b.year, birthday, age
              5:148,                      ## Main Data
              155                         ## comments
              )]

## =================================================================== ##

## Relevel condition
dat$condition <- factor(x = dat$condition, 
                        levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
                                   "13", "14", "15", "16", "17", "18", 
                                   "19", "20", "21", "22", "23", "24"))

## =================================================================== ##

## Prior.Man.Pilot: prior probability that man is the Pilot
dat$Prior.Man.Pilot <- NA

dat$Prior.Man.Pilot[dat$condition == "1"] <- dat$cond1.prior_manPilot.womanFlightAttendant[dat$condition == "1"]
dat$Prior.Man.Pilot[dat$condition == "2"] <- dat$cond2.prior_manPilot.womanFlightAttendant[dat$condition == "2"]
dat$Prior.Man.Pilot[dat$condition == "3"] <- dat$cond3.prior_manPilot.womanFlightAttendant[dat$condition == "3"]
dat$Prior.Man.Pilot[dat$condition == "4"] <- dat$cond4.prior_manPilot.womanFlightAttendant[dat$condition == "4"]
dat$Prior.Man.Pilot[dat$condition == "5"] <- dat$cond5.prior_manPilot.womanFlightAttendant[dat$condition == "5"]
dat$Prior.Man.Pilot[dat$condition == "6"] <- dat$cond6.prior_manPilot.womanFlightAttendant[dat$condition == "6"]
dat$Prior.Man.Pilot[dat$condition == "7"] <- dat$cond7.prior_manPilot.womanFlightAttendant[dat$condition == "7"]
dat$Prior.Man.Pilot[dat$condition == "8"] <- dat$cond8.prior_manPilot.womanFlightAttendant[dat$condition == "8"]
dat$Prior.Man.Pilot[dat$condition == "9"] <- dat$cond9.prior_manPilot.womanFlightAttendant[dat$condition == "9"]
dat$Prior.Man.Pilot[dat$condition == "10"] <- dat$cond10.prior_manPilot.womanFlightAttendant[dat$condition == "10"]
dat$Prior.Man.Pilot[dat$condition == "11"] <- dat$cond11.prior_manPilot.womanFlightAttendant[dat$condition == "11"]
dat$Prior.Man.Pilot[dat$condition == "12"] <- dat$cond12.prior_manPilot.womanFlightAttendant[dat$condition == "12"]
dat$Prior.Man.Pilot[dat$condition == "13"] <- dat$cond13.prior_manPilot.womanFlightAttendant[dat$condition == "13"]
dat$Prior.Man.Pilot[dat$condition == "14"] <- dat$cond14.prior_manPilot.womanFlightAttendant[dat$condition == "14"]
dat$Prior.Man.Pilot[dat$condition == "15"] <- dat$cond15.prior_manPilot.womanFlightAttendant[dat$condition == "15"]
dat$Prior.Man.Pilot[dat$condition == "16"] <- dat$cond16.prior_manPilot.womanFlightAttendant[dat$condition == "16"]
dat$Prior.Man.Pilot[dat$condition == "17"] <- dat$cond17.prior_manPilot.womanFlightAttendant[dat$condition == "17"]
dat$Prior.Man.Pilot[dat$condition == "18"] <- dat$cond18.prior_manPilot.womanFlightAttendant[dat$condition == "18"]
dat$Prior.Man.Pilot[dat$condition == "19"] <- dat$cond19.prior_manPilot.womanFlightAttendant[dat$condition == "19"]
dat$Prior.Man.Pilot[dat$condition == "20"] <- dat$cond20.prior_manPilot.womanFlightAttendant[dat$condition == "20"]
dat$Prior.Man.Pilot[dat$condition == "21"] <- dat$cond21.prior_manPilot.womanFlightAttendant[dat$condition == "21"]
dat$Prior.Man.Pilot[dat$condition == "22"] <- dat$cond22.prior_manPilot.womanFlightAttendant[dat$condition == "22"]
dat$Prior.Man.Pilot[dat$condition == "23"] <- dat$cond23.prior_manPilot.womanFlightAttendant[dat$condition == "23"]
dat$Prior.Man.Pilot[dat$condition == "24"] <- dat$cond24.prior_manPilot.womanFlightAttendant[dat$condition == "24"]

## Prior.Woman.Pilot: prior probability that woman is the Pilot
dat$Prior.Woman.Pilot <- NA

dat$Prior.Woman.Pilot[dat$condition == "1"] <- dat$cond1.prior_womanPilot.manFlightAttendant[dat$condition == "1"]
dat$Prior.Woman.Pilot[dat$condition == "2"] <- dat$cond2.prior_womanPilot.manFlightAttendant[dat$condition == "2"]
dat$Prior.Woman.Pilot[dat$condition == "3"] <- dat$cond3.prior_womanPilot.manFlightAttendant[dat$condition == "3"]
dat$Prior.Woman.Pilot[dat$condition == "4"] <- dat$cond4.prior_womanPilot.manFlightAttendant[dat$condition == "4"]
dat$Prior.Woman.Pilot[dat$condition == "5"] <- dat$cond5.prior_womanPilot.manFlightAttendant[dat$condition == "5"]
dat$Prior.Woman.Pilot[dat$condition == "6"] <- dat$cond6.prior_womanPilot.manFlightAttendant[dat$condition == "6"]
dat$Prior.Woman.Pilot[dat$condition == "7"] <- dat$cond7.prior_womanPilot.manFlightAttendant[dat$condition == "7"]
dat$Prior.Woman.Pilot[dat$condition == "8"] <- dat$cond8.prior_womanPilot.manFlightAttendant[dat$condition == "8"]
dat$Prior.Woman.Pilot[dat$condition == "9"] <- dat$cond9.prior_womanPilot.manFlightAttendant[dat$condition == "9"]
dat$Prior.Woman.Pilot[dat$condition == "10"] <- dat$cond10.prior_womanPilot.manFlightAttendant[dat$condition == "10"]
dat$Prior.Woman.Pilot[dat$condition == "11"] <- dat$cond11.prior_womanPilot.manFlightAttendant[dat$condition == "11"]
dat$Prior.Woman.Pilot[dat$condition == "12"] <- dat$cond12.prior_womanPilot.manFlightAttendant[dat$condition == "12"]
dat$Prior.Woman.Pilot[dat$condition == "13"] <- dat$cond13.prior_womanPilot.manFlightAttendant[dat$condition == "13"]
dat$Prior.Woman.Pilot[dat$condition == "14"] <- dat$cond14.prior_womanPilot.manFlightAttendant[dat$condition == "14"]
dat$Prior.Woman.Pilot[dat$condition == "15"] <- dat$cond15.prior_womanPilot.manFlightAttendant[dat$condition == "15"]
dat$Prior.Woman.Pilot[dat$condition == "16"] <- dat$cond16.prior_womanPilot.manFlightAttendant[dat$condition == "16"]
dat$Prior.Woman.Pilot[dat$condition == "17"] <- dat$cond17.prior_womanPilot.manFlightAttendant[dat$condition == "17"]
dat$Prior.Woman.Pilot[dat$condition == "18"] <- dat$cond18.prior_womanPilot.manFlightAttendant[dat$condition == "18"]
dat$Prior.Woman.Pilot[dat$condition == "19"] <- dat$cond19.prior_womanPilot.manFlightAttendant[dat$condition == "19"]
dat$Prior.Woman.Pilot[dat$condition == "20"] <- dat$cond20.prior_womanPilot.manFlightAttendant[dat$condition == "20"]
dat$Prior.Woman.Pilot[dat$condition == "21"] <- dat$cond21.prior_womanPilot.manFlightAttendant[dat$condition == "21"]
dat$Prior.Woman.Pilot[dat$condition == "22"] <- dat$cond22.prior_womanPilot.manFlightAttendant[dat$condition == "22"]
dat$Prior.Woman.Pilot[dat$condition == "23"] <- dat$cond23.prior_womanPilot.manFlightAttendant[dat$condition == "23"]
dat$Prior.Woman.Pilot[dat$condition == "24"] <- dat$cond24.prior_womanPilot.manFlightAttendant[dat$condition == "24"]

## Confirm that priors sum to 100
count(with(dat, Prior.Man.Pilot + Prior.Woman.Pilot))

## Peak at distribution of priors overall
hist(dat$Prior.Man.Pilot)

## ================================================================================================= ##

## Prior.Target.Pilot
dat$Prior.Target.Pilot <- ifelse(test = dat$target == "man", 
                                 yes = dat$Prior.Man.Pilot, 
                                 no = dat$Prior.Woman.Pilot)

## ================================================================== ##
  ## Able to update
dat$able.to.update <- ifelse(test = dat$Prior.Man.Pilot == 1, 
                             yes = "no", 
                             no = "yes")

dat$able.to.update <- factor(x = dat$able.to.update, 
                             labels = c("no", "yes"))

count(dat$able.to.update)
  ## 19 participants can't update their priors

## =================================================================== ##

## Posterior.Man.Pilot: posterior probability that man is the Pilot
dat$Posterior.Man.Pilot <- NA

dat$Posterior.Man.Pilot[dat$condition == "1"] <- dat$cond1.post_manPilot.womanFlightAttendant[dat$condition == "1"]
dat$Posterior.Man.Pilot[dat$condition == "2"] <- dat$cond2.post_manPilot.womanFlightAttendant[dat$condition == "2"]
dat$Posterior.Man.Pilot[dat$condition == "3"] <- dat$cond3.post_manPilot.womanFlightAttendant[dat$condition == "3"]
dat$Posterior.Man.Pilot[dat$condition == "4"] <- dat$cond4.post_manPilot.womanFlightAttendant[dat$condition == "4"]
dat$Posterior.Man.Pilot[dat$condition == "5"] <- dat$cond5.post_manPilot.womanFlightAttendant[dat$condition == "5"]
dat$Posterior.Man.Pilot[dat$condition == "6"] <- dat$cond6.post_manPilot.womanFlightAttendant[dat$condition == "6"]
dat$Posterior.Man.Pilot[dat$condition == "7"] <- dat$cond7.post_manPilot.womanFlightAttendant[dat$condition == "7"]
dat$Posterior.Man.Pilot[dat$condition == "8"] <- dat$cond8.post_manPilot.womanFlightAttendant[dat$condition == "8"]
dat$Posterior.Man.Pilot[dat$condition == "9"] <- dat$cond9.post_manPilot.womanFlightAttendant[dat$condition == "9"]
dat$Posterior.Man.Pilot[dat$condition == "10"] <- dat$cond10.post_manPilot.womanFlightAttendant[dat$condition == "10"]
dat$Posterior.Man.Pilot[dat$condition == "11"] <- dat$cond11.post_manPilot.womanFlightAttendant[dat$condition == "11"]
dat$Posterior.Man.Pilot[dat$condition == "12"] <- dat$cond12.post_manPilot.womanFlightAttendant[dat$condition == "12"]
dat$Posterior.Man.Pilot[dat$condition == "13"] <- dat$cond13.post_manPilot.womanFlightAttendant[dat$condition == "13"]
dat$Posterior.Man.Pilot[dat$condition == "14"] <- dat$cond14.post_manPilot.womanFlightAttendant[dat$condition == "14"]
dat$Posterior.Man.Pilot[dat$condition == "15"] <- dat$cond15.post_manPilot.womanFlightAttendant[dat$condition == "15"]
dat$Posterior.Man.Pilot[dat$condition == "16"] <- dat$cond16.post_manPilot.womanFlightAttendant[dat$condition == "16"]
dat$Posterior.Man.Pilot[dat$condition == "17"] <- dat$cond17.post_manPilot.womanFlightAttendant[dat$condition == "17"]
dat$Posterior.Man.Pilot[dat$condition == "18"] <- dat$cond18.post_manPilot.womanFlightAttendant[dat$condition == "18"]
dat$Posterior.Man.Pilot[dat$condition == "19"] <- dat$cond19.post_manPilot.womanFlightAttendant[dat$condition == "19"]
dat$Posterior.Man.Pilot[dat$condition == "20"] <- dat$cond20.post_manPilot.womanFlightAttendant[dat$condition == "20"]
dat$Posterior.Man.Pilot[dat$condition == "21"] <- dat$cond21.post_manPilot.womanFlightAttendant[dat$condition == "21"]
dat$Posterior.Man.Pilot[dat$condition == "22"] <- dat$cond22.post_manPilot.womanFlightAttendant[dat$condition == "22"]
dat$Posterior.Man.Pilot[dat$condition == "23"] <- dat$cond23.post_manPilot.womanFlightAttendant[dat$condition == "23"]
dat$Posterior.Man.Pilot[dat$condition == "24"] <- dat$cond24.post_manPilot.womanFlightAttendant[dat$condition == "24"]

## Posterior.Woman.Pilot: prior probability that woman is the Pilot
dat$Posterior.Woman.Pilot <- NA

dat$Posterior.Woman.Pilot[dat$condition == "1"] <- dat$cond1.post_womanPilot.manFlightAttendant[dat$condition == "1"]
dat$Posterior.Woman.Pilot[dat$condition == "2"] <- dat$cond2.post_womanPilot.manFlightAttendant[dat$condition == "2"]
dat$Posterior.Woman.Pilot[dat$condition == "3"] <- dat$cond3.post_womanPilot.manFlightAttendant[dat$condition == "3"]
dat$Posterior.Woman.Pilot[dat$condition == "4"] <- dat$cond4.post_womanPilot.manFlightAttendant[dat$condition == "4"]
dat$Posterior.Woman.Pilot[dat$condition == "5"] <- dat$cond5.post_womanPilot.manFlightAttendant[dat$condition == "5"]
dat$Posterior.Woman.Pilot[dat$condition == "6"] <- dat$cond6.post_womanPilot.manFlightAttendant[dat$condition == "6"]
dat$Posterior.Woman.Pilot[dat$condition == "7"] <- dat$cond7.post_womanPilot.manFlightAttendant[dat$condition == "7"]
dat$Posterior.Woman.Pilot[dat$condition == "8"] <- dat$cond8.post_womanPilot.manFlightAttendant[dat$condition == "8"]
dat$Posterior.Woman.Pilot[dat$condition == "9"] <- dat$cond9.post_womanPilot.manFlightAttendant[dat$condition == "9"]
dat$Posterior.Woman.Pilot[dat$condition == "10"] <- dat$cond10.post_womanPilot.manFlightAttendant[dat$condition == "10"]
dat$Posterior.Woman.Pilot[dat$condition == "11"] <- dat$cond11.post_womanPilot.manFlightAttendant[dat$condition == "11"]
dat$Posterior.Woman.Pilot[dat$condition == "12"] <- dat$cond12.post_womanPilot.manFlightAttendant[dat$condition == "12"]
dat$Posterior.Woman.Pilot[dat$condition == "13"] <- dat$cond13.post_womanPilot.manFlightAttendant[dat$condition == "13"]
dat$Posterior.Woman.Pilot[dat$condition == "14"] <- dat$cond14.post_womanPilot.manFlightAttendant[dat$condition == "14"]
dat$Posterior.Woman.Pilot[dat$condition == "15"] <- dat$cond15.post_womanPilot.manFlightAttendant[dat$condition == "15"]
dat$Posterior.Woman.Pilot[dat$condition == "16"] <- dat$cond16.post_womanPilot.manFlightAttendant[dat$condition == "16"]
dat$Posterior.Woman.Pilot[dat$condition == "17"] <- dat$cond17.post_womanPilot.manFlightAttendant[dat$condition == "17"]
dat$Posterior.Woman.Pilot[dat$condition == "18"] <- dat$cond18.post_womanPilot.manFlightAttendant[dat$condition == "18"]
dat$Posterior.Woman.Pilot[dat$condition == "19"] <- dat$cond19.post_womanPilot.manFlightAttendant[dat$condition == "19"]
dat$Posterior.Woman.Pilot[dat$condition == "20"] <- dat$cond20.post_womanPilot.manFlightAttendant[dat$condition == "20"]
dat$Posterior.Woman.Pilot[dat$condition == "21"] <- dat$cond21.post_womanPilot.manFlightAttendant[dat$condition == "21"]
dat$Posterior.Woman.Pilot[dat$condition == "22"] <- dat$cond22.post_womanPilot.manFlightAttendant[dat$condition == "22"]
dat$Posterior.Woman.Pilot[dat$condition == "23"] <- dat$cond23.post_womanPilot.manFlightAttendant[dat$condition == "23"]
dat$Posterior.Woman.Pilot[dat$condition == "24"] <- dat$cond24.post_womanPilot.manFlightAttendant[dat$condition == "24"]

## Confirm that posteriors sum to 100
count(with(dat, Posterior.Man.Pilot + Posterior.Woman.Pilot))
  ## 1 NA

## Investigate NA further
which(is.na(dat$Posterior.Man.Pilot) == TRUE)
  ## row 850

  ## View this participant's responses
dat[850,c(117:122)]
  ## View this particiapnt's demogrpahics
dat[850,c(12:20)]

  ## Add a zero
dat[850, 122] <- 0

## Redo process

## Posterior.Man.Pilot: posterior probability that man is the Pilot
dat$Posterior.Man.Pilot <- NA

dat$Posterior.Man.Pilot[dat$condition == "1"] <- dat$cond1.post_manPilot.womanFlightAttendant[dat$condition == "1"]
dat$Posterior.Man.Pilot[dat$condition == "2"] <- dat$cond2.post_manPilot.womanFlightAttendant[dat$condition == "2"]
dat$Posterior.Man.Pilot[dat$condition == "3"] <- dat$cond3.post_manPilot.womanFlightAttendant[dat$condition == "3"]
dat$Posterior.Man.Pilot[dat$condition == "4"] <- dat$cond4.post_manPilot.womanFlightAttendant[dat$condition == "4"]
dat$Posterior.Man.Pilot[dat$condition == "5"] <- dat$cond5.post_manPilot.womanFlightAttendant[dat$condition == "5"]
dat$Posterior.Man.Pilot[dat$condition == "6"] <- dat$cond6.post_manPilot.womanFlightAttendant[dat$condition == "6"]
dat$Posterior.Man.Pilot[dat$condition == "7"] <- dat$cond7.post_manPilot.womanFlightAttendant[dat$condition == "7"]
dat$Posterior.Man.Pilot[dat$condition == "8"] <- dat$cond8.post_manPilot.womanFlightAttendant[dat$condition == "8"]
dat$Posterior.Man.Pilot[dat$condition == "9"] <- dat$cond9.post_manPilot.womanFlightAttendant[dat$condition == "9"]
dat$Posterior.Man.Pilot[dat$condition == "10"] <- dat$cond10.post_manPilot.womanFlightAttendant[dat$condition == "10"]
dat$Posterior.Man.Pilot[dat$condition == "11"] <- dat$cond11.post_manPilot.womanFlightAttendant[dat$condition == "11"]
dat$Posterior.Man.Pilot[dat$condition == "12"] <- dat$cond12.post_manPilot.womanFlightAttendant[dat$condition == "12"]
dat$Posterior.Man.Pilot[dat$condition == "13"] <- dat$cond13.post_manPilot.womanFlightAttendant[dat$condition == "13"]
dat$Posterior.Man.Pilot[dat$condition == "14"] <- dat$cond14.post_manPilot.womanFlightAttendant[dat$condition == "14"]
dat$Posterior.Man.Pilot[dat$condition == "15"] <- dat$cond15.post_manPilot.womanFlightAttendant[dat$condition == "15"]
dat$Posterior.Man.Pilot[dat$condition == "16"] <- dat$cond16.post_manPilot.womanFlightAttendant[dat$condition == "16"]
dat$Posterior.Man.Pilot[dat$condition == "17"] <- dat$cond17.post_manPilot.womanFlightAttendant[dat$condition == "17"]
dat$Posterior.Man.Pilot[dat$condition == "18"] <- dat$cond18.post_manPilot.womanFlightAttendant[dat$condition == "18"]
dat$Posterior.Man.Pilot[dat$condition == "19"] <- dat$cond19.post_manPilot.womanFlightAttendant[dat$condition == "19"]
dat$Posterior.Man.Pilot[dat$condition == "20"] <- dat$cond20.post_manPilot.womanFlightAttendant[dat$condition == "20"]
dat$Posterior.Man.Pilot[dat$condition == "21"] <- dat$cond21.post_manPilot.womanFlightAttendant[dat$condition == "21"]
dat$Posterior.Man.Pilot[dat$condition == "22"] <- dat$cond22.post_manPilot.womanFlightAttendant[dat$condition == "22"]
dat$Posterior.Man.Pilot[dat$condition == "23"] <- dat$cond23.post_manPilot.womanFlightAttendant[dat$condition == "23"]
dat$Posterior.Man.Pilot[dat$condition == "24"] <- dat$cond24.post_manPilot.womanFlightAttendant[dat$condition == "24"]

## Posterior.Woman.Pilot: prior probability that woman is the Pilot
dat$Posterior.Woman.Pilot <- NA

dat$Posterior.Woman.Pilot[dat$condition == "1"] <- dat$cond1.post_womanPilot.manFlightAttendant[dat$condition == "1"]
dat$Posterior.Woman.Pilot[dat$condition == "2"] <- dat$cond2.post_womanPilot.manFlightAttendant[dat$condition == "2"]
dat$Posterior.Woman.Pilot[dat$condition == "3"] <- dat$cond3.post_womanPilot.manFlightAttendant[dat$condition == "3"]
dat$Posterior.Woman.Pilot[dat$condition == "4"] <- dat$cond4.post_womanPilot.manFlightAttendant[dat$condition == "4"]
dat$Posterior.Woman.Pilot[dat$condition == "5"] <- dat$cond5.post_womanPilot.manFlightAttendant[dat$condition == "5"]
dat$Posterior.Woman.Pilot[dat$condition == "6"] <- dat$cond6.post_womanPilot.manFlightAttendant[dat$condition == "6"]
dat$Posterior.Woman.Pilot[dat$condition == "7"] <- dat$cond7.post_womanPilot.manFlightAttendant[dat$condition == "7"]
dat$Posterior.Woman.Pilot[dat$condition == "8"] <- dat$cond8.post_womanPilot.manFlightAttendant[dat$condition == "8"]
dat$Posterior.Woman.Pilot[dat$condition == "9"] <- dat$cond9.post_womanPilot.manFlightAttendant[dat$condition == "9"]
dat$Posterior.Woman.Pilot[dat$condition == "10"] <- dat$cond10.post_womanPilot.manFlightAttendant[dat$condition == "10"]
dat$Posterior.Woman.Pilot[dat$condition == "11"] <- dat$cond11.post_womanPilot.manFlightAttendant[dat$condition == "11"]
dat$Posterior.Woman.Pilot[dat$condition == "12"] <- dat$cond12.post_womanPilot.manFlightAttendant[dat$condition == "12"]
dat$Posterior.Woman.Pilot[dat$condition == "13"] <- dat$cond13.post_womanPilot.manFlightAttendant[dat$condition == "13"]
dat$Posterior.Woman.Pilot[dat$condition == "14"] <- dat$cond14.post_womanPilot.manFlightAttendant[dat$condition == "14"]
dat$Posterior.Woman.Pilot[dat$condition == "15"] <- dat$cond15.post_womanPilot.manFlightAttendant[dat$condition == "15"]
dat$Posterior.Woman.Pilot[dat$condition == "16"] <- dat$cond16.post_womanPilot.manFlightAttendant[dat$condition == "16"]
dat$Posterior.Woman.Pilot[dat$condition == "17"] <- dat$cond17.post_womanPilot.manFlightAttendant[dat$condition == "17"]
dat$Posterior.Woman.Pilot[dat$condition == "18"] <- dat$cond18.post_womanPilot.manFlightAttendant[dat$condition == "18"]
dat$Posterior.Woman.Pilot[dat$condition == "19"] <- dat$cond19.post_womanPilot.manFlightAttendant[dat$condition == "19"]
dat$Posterior.Woman.Pilot[dat$condition == "20"] <- dat$cond20.post_womanPilot.manFlightAttendant[dat$condition == "20"]
dat$Posterior.Woman.Pilot[dat$condition == "21"] <- dat$cond21.post_womanPilot.manFlightAttendant[dat$condition == "21"]
dat$Posterior.Woman.Pilot[dat$condition == "22"] <- dat$cond22.post_womanPilot.manFlightAttendant[dat$condition == "22"]
dat$Posterior.Woman.Pilot[dat$condition == "23"] <- dat$cond23.post_womanPilot.manFlightAttendant[dat$condition == "23"]
dat$Posterior.Woman.Pilot[dat$condition == "24"] <- dat$cond24.post_womanPilot.manFlightAttendant[dat$condition == "24"]

## Confirm that posteriors sum to 100
count(with(dat, Posterior.Man.Pilot + Posterior.Woman.Pilot))

## ================================================================================================= ##

## Posterior.Target.Pilot
dat$Posterior.Target.Pilot <- ifelse(test = dat$target == "man", 
                                     yes = dat$Posterior.Man.Pilot, 
                                     no = dat$Posterior.Woman.Pilot)

## ================================================================================================= ##
colnames(dat)
levels(dat$lookup)
levels(dat$able.to.update)
count(dat$lookup)
count(dat$able.to.update)

## ================================================================================================= ##

## Save dat as its own dataframe
save(dat, file = "pilot_flightAttendant.Rda")
