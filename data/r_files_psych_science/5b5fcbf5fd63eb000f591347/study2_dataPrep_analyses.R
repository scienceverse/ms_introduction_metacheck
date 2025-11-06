## Cao, Kleiman-Weiner, & Banaji
## Psych Sci paper
## Study 2: Data preparation and analyses

## Set working directory
if (!require("rstudioapi")) {install.packages("rstudioapi"); require("rstudioapi")}
currentPath <- getActiveDocumentContext()$path
setwd(dir = dirname(path = currentPath))

## Packages needed
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
if (!require("ggjoy")) {install.packages("ggjoy"); require("ggjoy")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("cowplot")) {install.packages("cowplot"); require("cowplot")}
if (!require("lsr")) {install.packages("lsr"); require("lsr")}


## Data preparation
## ----------------------------------------------------------------------------------------------------------- ##

## Load data
load("Other_Professions.Rda")

## Convert StartDate and EndDate to date format
dat$StartDate <- as.POSIXct(dat$StartDate, format = "%Y-%m-%d %H:%M:%S")
dat$EndDate <- as.POSIXct(dat$EndDate, format = "%Y-%m-%d %H:%M:%S")

## Turn ResponseId and workerID into character format
dat$ResponseId <- as.character(dat$ResponseId)
dat$workerID <-as.character(dat$workerID)

## Change ResponseID to responseID
colnames(dat)[3] <- "responseID"

## Specify profession (Construction Worker vs. Firefighter vs. Butcher)
class(dat$cond)
dat$profession <- ifelse(test = dat$cond == "1" | dat$cond == "2", 
                         yes = "Construction Worker", 
                         no = ifelse(test = dat$cond == "3" | dat$cond == "4", 
                                     yes = "Firefighter", 
                                     no = "Butcher"))

## Specify order (Man-Woman vs. Woman-Man)
dat$order <- ifelse(test = dat$cond == "1" | dat$cond == "3" | dat$cond == "5", 
                    yes = "Man-Woman", 
                    no = "Woman-Man")

## Clean up demographics
  ## gender
levels(dat$gender)
dat$gender <- factor(x = dat$gender, 
                     levels = c("1", "2"), 
                     labels = c("Male", "Female"))

  ## educ
levels(dat$educ)
dat$educ <- factor(x = dat$educ, 
                   levels = c("1", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" ,"13", "14"), 
                   labels = c("Elementary School", 
                              "Some HS", 
                              "HS Grad", 
                              "Some College", 
                              "Associate's", 
                              "Bachelor's", 
                              "Some Grad School", 
                              "Master's", 
                              "MBA", 
                              "JD", 
                              "MD", 
                              "PhD", 
                              "Other Advanced Degree"))


  ## politicalid
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
              "cond", "profession", "order", 
              "c1.query", "c1.intelligent", "c1.accurate", "c1.fair", "c1.just", "c1.freeResponse", 
              "c2.query", "c2.intelligent", "c2.accurate", "c2.fair", "c2.just", "c2.freeResponse", 
              "c3.query", "c3.intelligent", "c3.accurate", "c3.fair", "c3.just", "c3.freeResponse", 
              "c4.query", "c4.intelligent", "c4.accurate", "c4.fair", "c4.just", "c4.freeResponse", 
              "c5.query", "c5.intelligent", "c5.accurate", "c5.fair", "c5.just", "c5.freeResponse", 
              "c6.query", "c6.intelligent", "c6.accurate", "c6.fair", "c6.just", "c6.freeResponse")]    



dat$c1.query <- as.numeric(levels(dat$c1.query))[dat$c1.query]
dat$c2.query <- as.numeric(levels(dat$c2.query))[dat$c2.query]
dat$c3.query <- as.numeric(levels(dat$c3.query))[dat$c3.query]
dat$c4.query <- as.numeric(levels(dat$c4.query))[dat$c4.query]
dat$c5.query <- as.numeric(levels(dat$c5.query))[dat$c5.query]
dat$c6.query <- as.numeric(levels(dat$c6.query))[dat$c6.query]

dat$c1.intelligent <- as.numeric(levels(dat$c1.intelligent))[dat$c1.intelligent]
dat$c2.intelligent <- as.numeric(levels(dat$c2.intelligent))[dat$c2.intelligent]
dat$c3.intelligent <- as.numeric(levels(dat$c3.intelligent))[dat$c3.intelligent]
dat$c4.intelligent <- as.numeric(levels(dat$c4.intelligent))[dat$c4.intelligent]
dat$c5.intelligent <- as.numeric(levels(dat$c5.intelligent))[dat$c5.intelligent]
dat$c6.intelligent <- as.numeric(levels(dat$c6.intelligent))[dat$c6.intelligent]

dat$c1.accurate <- as.numeric(levels(dat$c1.accurate))[dat$c1.accurate]
dat$c2.accurate <- as.numeric(levels(dat$c2.accurate))[dat$c2.accurate]
dat$c3.accurate <- as.numeric(levels(dat$c3.accurate))[dat$c3.accurate]
dat$c4.accurate <- as.numeric(levels(dat$c4.accurate))[dat$c4.accurate]
dat$c5.accurate <- as.numeric(levels(dat$c5.accurate))[dat$c5.accurate]
dat$c6.accurate <- as.numeric(levels(dat$c6.accurate))[dat$c6.accurate]

dat$c1.fair <- as.numeric(levels(dat$c1.fair))[dat$c1.fair]
dat$c2.fair <- as.numeric(levels(dat$c2.fair))[dat$c2.fair]
dat$c3.fair <- as.numeric(levels(dat$c3.fair))[dat$c3.fair]
dat$c4.fair <- as.numeric(levels(dat$c4.fair))[dat$c4.fair]
dat$c5.fair <- as.numeric(levels(dat$c5.fair))[dat$c5.fair]
dat$c6.fair <- as.numeric(levels(dat$c6.fair))[dat$c6.fair]

dat$c1.just <- as.numeric(levels(dat$c1.just))[dat$c1.just]
dat$c2.just <- as.numeric(levels(dat$c2.just))[dat$c2.just]
dat$c3.just <- as.numeric(levels(dat$c3.just))[dat$c3.just]
dat$c4.just <- as.numeric(levels(dat$c4.just))[dat$c4.just]
dat$c5.just <- as.numeric(levels(dat$c5.just))[dat$c5.just]
dat$c6.just <- as.numeric(levels(dat$c6.just))[dat$c6.just]

## Query
  ## 1 = Woman more likely
  ## 2 = Equally likely
  ## 3 = Man more likely

dat$query <- ifelse(test = dat$cond == "1", 
                    yes = dat$c1.query, 
                    no = ifelse(test = dat$cond == "2", 
                                yes = dat$c2.query, 
                                no = ifelse(test = dat$cond == "3", 
                                            yes = dat$c3.query, 
                                            no = ifelse(test = dat$cond == "4", 
                                                        yes = dat$c4.query, 
                                                        no = ifelse(test = dat$cond == "5", 
                                                                    yes = dat$c5.query, 
                                                                    no = dat$c6.query)))))

dat$query <- factor(x = dat$query,
                    levels = c(1, 2, 3), 
                    labels = c("Woman more likely", "Equally likely", "Man more likely"))

## intelligent
dat$intelligent <- ifelse(test = dat$cond == "1", 
                          yes = dat$c1.intelligent, 
                          no = ifelse(test = dat$cond == "2", 
                                      yes = dat$c2.intelligent, 
                                      no = ifelse(test = dat$cond == "3", 
                                                  yes = dat$c3.intelligent, 
                                                  no = ifelse(test = dat$cond == "4", 
                                                              yes = dat$c4.intelligent, 
                                                              no = ifelse(test = dat$cond == "5", 
                                                                          yes = dat$c5.intelligent, 
                                                                          no = dat$c6.intelligent)))))

tapply(X = dat$intelligent, 
       INDEX = dat$profession, 
       FUN = mean)

## accurate
dat$accurate <- ifelse(test = dat$cond == "1", 
                       yes = dat$c1.accurate, 
                       no = ifelse(test = dat$cond == "2", 
                                   yes = dat$c2.accurate, 
                                   no = ifelse(test = dat$cond == "3", 
                                               yes = dat$c3.accurate, 
                                               no = ifelse(test = dat$cond == "4", 
                                                           yes = dat$c4.accurate, 
                                                           no = ifelse(test = dat$cond == "5", 
                                                                       yes = dat$c5.accurate, 
                                                                       no = dat$c6.accurate)))))


tapply(X = dat$accurate, 
       INDEX = dat$profession, 
       FUN = mean)

## fair
dat$fair <- ifelse(test = dat$cond == "1", 
                   yes = dat$c1.fair, 
                   no = ifelse(test = dat$cond == "2", 
                               yes = dat$c2.fair, 
                               no = ifelse(test = dat$cond == "3", 
                                           yes = dat$c3.fair, 
                                           no = ifelse(test = dat$cond == "4", 
                                                       yes = dat$c4.fair, 
                                                       no = ifelse(test = dat$cond == "5", 
                                                                   yes = dat$c5.fair, 
                                                                   no = dat$c6.fair)))))


tapply(X = dat$fair, 
       INDEX = dat$profession, 
       FUN = mean)

## just
dat$just <- ifelse(test = dat$cond == "1", 
                   yes = dat$c1.just, 
                   no = ifelse(test = dat$cond == "2", 
                               yes = dat$c2.just, 
                               no = ifelse(test = dat$cond == "3", 
                                           yes = dat$c3.just, 
                                           no = ifelse(test = dat$cond == "4", 
                                                       yes = dat$c4.just, 
                                                       no = ifelse(test = dat$cond == "5", 
                                                                   yes = dat$c5.just, 
                                                                   no = dat$c6.just)))))


tapply(X = dat$just, 
       INDEX = dat$profession, 
       FUN = mean)

## Combine comments
dat$c1.freeResponse <- as.character(dat$c1.freeResponse)
dat$c2.freeResponse <- as.character(dat$c2.freeResponse)
dat$c3.freeResponse <- as.character(dat$c3.freeResponse)
dat$c4.freeResponse <- as.character(dat$c4.freeResponse)
dat$c5.freeResponse <- as.character(dat$c5.freeResponse)
dat$c6.freeResponse <- as.character(dat$c6.freeResponse)

dat$comment <- ifelse(test = dat$cond == "1", 
                      yes = dat$c1.freeResponse, 
                      no = ifelse(test = dat$cond == "2", 
                                  yes = dat$c2.freeResponse, 
                                  no = ifelse(test = dat$cond == "3", 
                                              yes = dat$c3.freeResponse, 
                                              no = ifelse(test = dat$cond == "4", 
                                                          yes = dat$c4.freeResponse, 
                                                          no = ifelse(test = dat$cond == "5", 
                                                                      yes = dat$c5.freeResponse, 
                                                                      no = dat$c6.freeResponse)))))


## Demographics
table(dat$gender)
round(x = mean(dat$age, na.rm = TRUE), digits = 2)
round(x = sd(dat$age, na.rm = TRUE), digits = 2)
  ## 222 males, 382 females
  ## Mean age = 34.56
  ## SD = 10.68

## Relevant aspects of non-doctor data
colnames(dat)
non.doctor.dat <- dat[,c("responseID", "age", "gender", "educ", "politicalid",
                         "profession", "order", 
                         "query", 
                         "intelligent", "accurate", "fair", "just", 
                         "comment")]

## Load Doctor-surgery data from Study 1
load("doc.surgery.PersonX.Rda")

## Rename
doctor.dat <- study1_rawDat

## Get rid of previous
rm(study1_rawDat)

## Make columns of doctor.dat match those of non.doctor.dat
colnames(non.doctor.dat)
colnames(doctor.dat)

  ## Turn query into a factor with same levels
table(doctor.dat$query)
doctor.dat$query <- factor(x = doctor.dat$query, 
                           levels = c(2:3), 
                           labels = c("Equally likely", "Man more likely"))

  ## Rename cond in doctor.dat to order
table(doctor.dat$cond)
table(non.doctor.dat$order)
colnames(doctor.dat)[13] <- "order"

  ## Add profession variable to doctor.dat
doctor.dat$profession <- "Doctor"

  ## Subset out relevant variables
doctor.dat <- doctor.dat[,c("responseID", "age", "gender", "educ", "politicalid",
                            "profession", "order", 
                            "query", 
                            "intelligent", "accurate", "fair", "just", 
                            "comment")]

## Bind together, by row, non.doctor.dat and doctor.dat
eval.dat <- rbind(non.doctor.dat, doctor.dat)

## Check structure
str(eval.dat)
table(unique(eval.dat$responseID))
hist(eval.dat$age)
table(eval.dat$gender)
table(eval.dat$educ)
table(eval.dat$politicalid)
table(eval.dat$profession)
eval.dat$profession <- factor(eval.dat$profession, 
                              levels = c("Construction Worker", "Butcher", "Firefighter", "Doctor"))
table(eval.dat$order)
eval.dat$order <- factor(eval.dat$order, 
                         levels = c("Man-Woman", "Woman-Man"))
table(eval.dat$query)

## Add action.profession
eval.dat$action.profession <- ifelse(test = eval.dat$profession == "Doctor", 
                                     yes = "Performed surgery - Doctor", 
                                     no = ifelse(test = eval.dat$profession == "Firefighter", 
                                                 yes = "Extinguished a fire - Firefighter", 
                                                 no = ifelse(test = eval.dat$profession == "Butcher", 
                                                             yes = "Carved up a pig - Butcher", 
                                                             no = "Poured concrete - Construction Worker")))


eval.dat$action.profession <- factor(eval.dat$action.profession, 
                                     levels = c("Poured concrete - Construction Worker", 
                                                "Extinguished a fire - Firefighter",
                                                "Carved up a pig - Butcher", 
                                                "Performed surgery - Doctor"), 
                                     labels = c("Construction Worker\nPoured concrete", 
                                                "Firefighter\nExtinguished a fire",
                                                "Butcher\nCarved up a pig", 
                                                "Doctor\nPerformed surgery"))

## ============================================================================================= ##
## ============================================================================================= ##
## ============================================================================================= ##
## Now ready to examine data

eval.dat.composite.long <- melt(eval.dat, measure.vars = c("accurate", "intelligent", "fair", "just"))
levels(eval.dat.composite.long$action.profession)

levels(eval.dat.composite.long$variable)
eval.dat.composite.long$variable <- factor(x = eval.dat.composite.long$variable, 
                                           levels = c("fair", "accurate", "just", "intelligent"))

## Labels for facet panels
facet_list_composite <- list(
  'fair' = "1 = Unfair ... 7 = Fair", 
  'accurate' = "1 = Inaccurate ... 7 = Accurate", 
  'just' = "1 = Unjust ... 7 = Just",
  'intelligent' = "1 = Unintelligent ... 7 = Intelligent"
  )


facet_labeller_composite <- function(variable, value) {
  return(facet_list_composite[value])
}

ggplot(data = eval.dat.composite.long, 
       mapping = aes(x = value, y = action.profession)) + 
  geom_joy() + 
  facet_wrap(~variable, labeller = facet_labeller_composite) + 
  coord_cartesian(xlim = c(0, 8)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  theme(axis.title = element_blank()) + 
  theme(axis.ticks.y = element_blank()) + 
  theme(axis.text.y = element_text(vjust = -0.5, size = 15, face = "bold", color = "black")) + 
  theme(axis.text.x = element_text(color = "black", size = 15, face = "bold")) + 
  theme(panel.spacing = unit(2.5, "lines")) + 
  theme(strip.background = element_blank()) + 
  theme(strip.text.x = element_text(size = 15, face = "bold")) + 
  theme(aspect.ratio = 1)

## Relevel
eval.dat.composite.long$profession <- factor(eval.dat.composite.long$profession, 
                                             levels = c("Doctor", "Butcher", "Firefighter", "Construction Worker"))


eval.dat.composite.long$variable <- factor(eval.dat.composite.long$variable, 
                                           levels = c("fair", "just", "accurate", "intelligent"))

  ## Means
round(tapply(X = eval.dat.composite.long$value, 
             INDEX = list(eval.dat.composite.long$variable, eval.dat.composite.long$profession), 
             FUN = mean), 
      digits = 2)

  ## Standard errors
round(tapply(X = eval.dat.composite.long$value, 
             INDEX = list(eval.dat.composite.long$variable, eval.dat.composite.long$profession), 
             FUN = std.error), 
      digits = 2)

## Cronbach's alpha
  ## Doctor
psych::alpha(x = var(subset(eval.dat, profession == "Doctor")[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.92

  ## Firefighter
psych::alpha(x = var(subset(eval.dat, profession == "Firefighter")[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.90

  ## Butcher
psych::alpha(x = var(subset(eval.dat, profession == "Butcher")[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.91

  ## 0.94
psych::alpha(x = var(subset(eval.dat, profession == "Construction Worker")[,c("intelligent", "accurate", "fair", "just")]))
  ## alpha = 0.94

## Calculate composite.avg
eval.dat$composite.avg <- with(eval.dat, 
                               (accurate + intelligent + fair + just)/4)

## Means and SEs
library(dplyr)
eval.dat %>%
  group_by(profession) %>%
  summarise(mean = mean(composite.avg), 
            se = std.error(composite.avg))

## Relevel profession
eval.dat$profession <- factor(x = eval.dat$profession, 
                              levels = c("Doctor", "Butcher", "Firefighter", "Construction Worker"))

## Agreement proportion in each condition
query.summary <- eval.dat[,c("responseID", "profession", "query")] %>% 
  group_by(profession, query) %>%
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

agreement.prop <- ggplot(data = query.summary, 
                         mapping = aes(x = profession, y = perc, fill = query)) + 
  geom_bar(stat = "identity", width = 0.7) +
  labs(y = "Proportion of participants") +
  theme_bw(base_size = 11.5) + 
  scale_x_discrete(breaks = c("Doctor", "Butcher", "Firefighter", "Construction Worker"), 
                   labels = c("Doctor\nStudy 1\nn = 199", 
                              "Butcher\nStudy 2\nn = 199", 
                              "Firefighter\nStudy 2\nn = 200", 
                              "Construction Worker\nStudy 2\nn = 205")) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     label = percent_format(), 
                     expand = c(0, 0)) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.y = element_text(color = "black")) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "top") + 
  theme(legend.direction = "vertical") +
  theme(legend.key.size = unit(1.5, "line")) + 
  theme(aspect.ratio = 1)


## composite.avg in each condition
compositive.avg <- ggplot(data = eval.dat, 
                          mapping = aes(x = profession, y = composite.avg)) + 
  geom_violin(fill = "lightgrey") +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
               mapping = aes(width = 0.2), lwd = 1.5) + 
  coord_cartesian(ylim = c(1,7)) + 
  scale_x_discrete(breaks = c("Doctor", "Butcher", "Firefighter", "Construction Worker"), 
                   labels = c("Doctor\nStudy 1\nn = 199", 
                              "Butcher\nStudy 2\nn = 199", 
                              "Firefighter\nStudy 2\nn = 200", 
                              "Construction Worker\nStudy 2\nn = 205")) + 
  labs(y = "Evaluation of Person X") + 
  scale_y_continuous(breaks = seq(1, 7, 1)) + 
  theme_bw(base_size = 11.5) + 
  theme(panel.grid = element_blank()) + 
  theme(panel.border = element_blank()) + 
  theme(axis.line = element_line(color = "black")) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(color = "black")) +
  theme(axis.text.y = element_text(color = "black")) +
  theme(aspect.ratio = 1)


## Stitch two plots together
plot_grid(agreement.prop, compositive.avg, 
          labels=c("A", "B"), 
          ncol = 1, nrow = 2, 
          rel_widths = c(1.33, 1),
          rel_heights = c(1.33, 1))

## Dimensions:
## 500 x 1000

## Compare average evaluations
fit.composite.avg <- aov(composite.avg ~ action.profession, 
                         data = subset(eval.dat, profession != "Doctor"))

summary.lm(fit.composite.avg)
sqrt((-1.741)^2 / ((-1.741)^2 + 601))
TukeyHSD(x = fit.composite.avg)

## Analysis not in manuscript:
## Compare proportions
if (!require("fifer")) {install.packages("fifer"); require("fifer")}
chisq.test(x = table(eval.dat$profession, eval.dat$query)[2:4,])
chisq.post.hoc(tbl = table(eval.dat$profession, eval.dat$query)[2:4,], 
               control = "bonferroni")
