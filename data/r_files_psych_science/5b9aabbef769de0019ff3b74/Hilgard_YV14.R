# This script contains the analyses used in my comment on YV14 "Know thy avatar"
# Table of contents
# 1. use algebra to get cell sizes for YV14
# 2. make a table of summary statistics for YV14
# 3. compare the effect sizes from YV14 against meta-analyses of violent-game effects
# 4. analyze my Study 1 data and make summary stats
# 5. analyze my Study 2 data and fit a multilevel model
# 6. compare my data and YV14 data via bootstrapped Levine test

# load packages
library(tidyverse)
library(metafor)
library(compute.es)
library(psych)
library(readxl)
library(car)
library(moments)
library(lme4)

# function for calculating t value from m, sd, n
calc_t <- function(m1, m2, sd1, sd2, n1, n2) {
  numerator <- m1 - m2
  denom <- sqrt((sd1^2)/n1 + (sd2^2)/n2)
  t <- numerator/denom
  return(t)
}

# function for returning both SD and CI(sd) given data x
sd.ci <- function(x) {
  df <- length(x) - 1
  sd.hat <- sd(x)
  sd.ll <- sqrt((df*var(x)) / qchisq(.975, df))
  sd.ul <- sqrt((df*var(x)) / qchisq(.025, df))
  return(list(sd = sd.hat, sd.ll = sd.ll, sd.ul = sd.ul))
}

# function for returning both var and CI(var) given data x
var.ci <- function(x) {
  df <- length(x) - 1
  var.hat <- var(x)
  var.ll <- (df*var(x)) / qchisq(.975, df)
  var.ul <- (df*var(x)) / qchisq(.025, df)
  return(list(var = var.hat, var.ll = var.ll, var.ul = var.ul))
}

# function for generating CIs on SD given SD and cell N
sd.ci.summary <- function(sd, n) {
  df <- n - 1
  sd.hat <- sd
  sd.ll <- sqrt((df*sd^2) / qchisq(.975, df))
  sd.ul <- sqrt((df*sd^2) / qchisq(.025, df))
  return(list(sd = sd.hat, sd.ll = sd.ll, sd.ul = sd.ul))
}

# function for turning escalc "SMD" output into point and CI
smd.ci <- function(x) {
  se <- sqrt(x$vi)
  c(x$yi, x$yi - 1.96*se, x$yi + 1.96*se) %>% 
    round(3) %>% 
    return()
}


# 1. Figure out cell sizes based on reported df ----
# hero-choc vs villain-choc -> t(86) -> n = 88
# hero-choc vs neutral-choc -> t(54) -> n = 56
# neural-choc vs villain-chc-> t(53) -> n = 55
# algebra yields n_herochoc = 44.5, n_neutralchoc = 11.5, n_villainchoc = 43.5

# villain-chili vs hero-chili  -> t(84) -> n = 86
# villain-chili vs neutrl-chili-> t(52) -> n = 54
# neutral-chili vs hero-chili ->  t(53) -> n = 55
# algrebra yields n_herochili = 43.5, n_neutrlchili = 11.5, n_villainchili = 42.5

# Implies very lopsided cells, total n = 197
# Manuscript reports total n = 194
# I think they overstated dfs by 1 throughout
# Would mean to round down all cell sizes

# All groups have comparable SEs (see Figure 1)
#    which means neutral group has unusually small SD (see below)

# cell sizes in Exp 2 can be recovered from the raw data

# 2. Make table of summary stats ----
sumstats1 <- data.frame("Food" = rep(c("Choco", "Chili"), each = 3),
                       "Avatar" = rep(c("Hero", "Neutral", "Villain"), times = 2),
                       "M.g" = c(15.43, 12.1, 8.51, 8.44, 10.27, 16.12),
                       "SD" = c(8.26, 1.45, 3.65, 3.04, 1.68, 3.04),
                       "Var" = c(8.26, 1.45, 3.65, 3.04, 1.68, 3.04)^2,
                       "N" = c(44.5, 11.5, 43.5, 43.5, 11.5, 42.5)) %>% 
  # generate CIs
  mutate(sd.ll = round(sd.ci.summary(SD, floor(N))[["sd.ll"]], 2),
         sd.ul = round(sd.ci.summary(SD, floor(N))[["sd.ul"]], 2))
sumstats1
# Some of these variances seem strangely small
# people are *really* consistent at pouring 12g of choc sauce or 10g of sriracha
# e.g. a little under 1 tablespoon of choc, 3/4 tsp of sriracha?

# summary stats from 2nd study
sumstats2 <- data.frame("Participation" = rep(c("Active", "Passive"), each = 2),
                        "Avatar" = rep(c("Villain", "Hero"), times = 2),
                        "M" = c(13.10, 6.32, 9.97, 8.81),
                        "SD" = c(4.47, 2.60, 3.02, 2.21)) %>% 
  mutate("Var" = SD^2)
sumstats2

# 3. Comparing effect sizes to broader literature ----
# it is generally preferred to calculate directly from mean and SD
# however, calculation via t-value is also provided

# Exp 1
# villain-vs-control, hot sauce
# text reports d = 0.96
escalc("SMD", 
       m1i = 16.12, sd1i = 7.18, n1i = 42,
       m2i = 10.27, sd2i = 1.68, n2i = 11) %>% 
  smd.ci # 0.89 [0.20, 1.57]
tes(4.8, 42, 11) # d = 1.63 [0.88, 2.38] 

# villain-vs-control, chocolate
# text reports d = 1.11
escalc("SMD", 
       m1i = 8.51, sd1i = 3.65, n1i = 43,
       m2i = 12.1, sd2i = 1.45, n2i = 11) %>% 
  smd.ci # 1.06 [0.37, 1.76]
tes(5.07, 43, 11) # d = 1.71 [0.96, 2.47] 

# hero-vs-control, hot sauce
# text reports d = 0.66
escalc("SMD", 
       m1i = 8.44, sd1i = 3.04, n1i = 43,
       m2i = 10.27, sd2i = 1.68, n2i = 11) %>% 
  smd.ci # d = 0.64 [0.04, 1.31]
tes(1.91, 43, 11) # d = 0.65 [-0.04, 1.33] 

# hero-vs-control, chocolate
# text reports d = 0.48
escalc("SMD", 
       m1i = 15.43, sd1i = 8.26, n1i = 44,
       m2i = 12.1, sd2i = 1.45, n2i = 11) %>% 
  smd.ci # d = 0.44 [-0.23, 1.11]
tes(2.53, 44, 11) # d = 0.85 [0.16, 1.55] 

# Get CIs for YV14 exp 2, activity == "played", behavior == "harm", hero vs villain contrast
escalc("SMD", 
       m1i = 13.1, sd1i = 4.47, n1i = 31,
       m2i = 6.32, sd2i = 2.60, n2i = 31) %>% 
  smd.ci # d = 1.83 [1.24, 2.42]
tes(7.3, 31, 31) #1.83 [1.24, 2.42]
# their text reports d = 1.92

# Get CIs for YV14 exp 2, activity == "watched", behavior == "harm", hero vs villain contrast
escalc("SMD", 
       m1i = 8.81, sd1i = 2.21, n1i = 31,
       m2i = 9.97, sd2i = 3.02, n2i = 32) %>% 
  smd.ci #0.43 [-.07, 0.93]

# Transform other meta estimates into CIs on d
# Grab numbers from Anderson et al. (2010) on violent-game effects,
#    Greitemeyer & Mugge (2014) on prosocial-game effects
prev.metas <- data.frame("Game" = rep(c("violent", "prosocial"), each = 2),
                         "Behavior" = rep(c("harm", "help"), times = 2),
                         r = c(.21, -.182, -.16, .20),
                         r.ll = c(.172, -.257, -.30, .13),
                         r.ul = c(.248, -.106, -.02, .27))
prev.metas

# gather, mutate into d, and spread
prev.metas %>% 
  gather(key, value, r:r.ul) %>% 
  mutate(value = round(r2d(value), 3)) %>% 
  spread(key, value) %>% 
  rename(d = r, d.ll = r.ll, d.ul = r.ul)
  
# 4. My Study 1 data -----
classdat <- read_csv("mydata/saucemass.csv")
targets <- data.frame(sauce = c("Choco", "Chili"),
                      g = c(12.10, 10.27))

# Turn overloads into 21g
classdat[classdat == "Overload"] <- 21
# Coerce back to numeric
classdat <- mutate_at(classdat, .vars = vars(Choco:Chili), .funs = funs(as.numeric))
# Gather data into long
classdat <- gather(classdat, key = "sauce", value = "g", Choco:Chili) %>% 
  # subtract off mean mass of cup: 1.941,
  # append target amount,
  # Turn subject ID to factor
  mutate(g = g - 1.941,
         Target = case_when(sauce == "Choco" ~ 12.10,
                            sauce == "Chili" ~ 10.27),
         Cup = as.factor(Cup)) %>% 
  # Restrict to complete cases
  filter(complete.cases(.))

# Make histograms
ggplot(classdat, aes(x = g)) +
  geom_histogram() + 
  facet_grid(sauce~.) +
  geom_vline(data = targets, aes(xintercept = g))+
  scale_x_continuous(limits = c(0, 20))

# Make summary stats
classroom.summary <- classdat %>% 
  group_by(sauce) %>% 
  summarize(m = mean(g), sd = sd(g), skew = skewness(g), kurt = kurtosis(g),
            ME = mean(g - Target), RMSE = sqrt(mean((g - Target)^2)))
classroom.summary
# my SDs are larger, even with the ceiling
# What's the CIs on my sds?
classdat %>% 
  group_by(sauce) %>% 
  summarize(sd = sd.ci(g)[[1]], sd.ll = sd.ci(g)[[2]], sd.ul = sd.ci(g)[[3]])

# how big is the within-subjects effect size of instructed pour amount?
diffdat <- select(classdat, -Target) %>% 
  spread(key = sauce, value = g) %>% 
  mutate(diff = Choco - Chili)
hist(diffdat$diff)
mean(diffdat$diff)/sd(diffdat$diff) # d = 0.416

# and let's look at the variance components using MLM
mlm <- lmer(g ~ sauce + (1|Cup), data = classdat)
summary(mlm)
# error terms: SDs of 1.91 between subjects, 2.51 within
# both of these error components are larger than yoon's from S1 (1.45, 1.68)

# 5. My study 2 data ----

dat.within <- read_excel("mydata/saucemass_within.xlsx")
# gather to long format for lmer
dat.within <- select(dat.within, ID:Cup5) %>% 
  gather("trial", "g", Cup1:Cup5) %>% 
  # tare out weight of cup
  mutate(g = g - 1.941, 
         # convert ID to factor
         ID = as.factor(ID))

# variance and CI
var.ci(dat.within$g) #3.19 [1.94, 6.16]
# SD and CI
sd.ci(dat.within$g)

# fit mlm
mlm2 <- lmer(g ~ (1|ID), data = dat.within)
summary(mlm2)
# between-subjects variance: 2.36
# within-subjects variance: 1.22

# 6. Compare variances of my data and YV14 data via Levene test ----
# Function for bootstrapped Levene test
levene.boot <- function(g, m, sd, n, seed = 1) {
  set.seed(seed)
  p <- NULL
  for (i in 1:1000) {
    # simulate YV14S1 data
    x2 <- rnorm(n, m,	sd)
    x2 <- x2 / sd(x2) * sd # force SD
    x2 <- x2 - mean(x2) + m # force M
    # grab my data
    chili <- data.frame(g) %>% 
      mutate(sauce = "chili", group = "Hilgard")
    # bind them to same data.frame for levene test
    chili.temp <- data.frame(g = x2, sauce = "Chili", group = "Yoon")
    chili <- bind_rows(chili, chili.temp)
    # levene test
    lt <- leveneTest(g ~ group, data = chili)
    # store results
    p <- bind_rows(p, data.frame("F" = lt[1,2], "p" = lt[1,3]))
  }
  return(p)
}

# Chili sauce
p.chili <- filter(classdat, sauce == "Chili")$g %>% 
  levene.boot(g = ., m = 10.27, sd = 1.68, n = 11) # YV14, S1, neutral-chili group
summary(p.chili) # F(1, 81) = 7.16, p = .009

# Chocolate
# may not be a valid test given chips vs sauce
p.choco <- filter(classdat, sauce == "Choco")$g %>% 
  levene.boot(g = ., m = 12.1, sd = 1.45, n = 11) # YV14, S1, neutral-choco group
summary(p.choco) # F = 6.24, p = .015

# My study 2
# bring in the within-subjects data
dat.within <- read_excel("mydata/saucemass_within.xlsx")
# gather to long format for lmer
dat.within <- select(dat.within, ID:Cup5) %>% 
  gather("trial", "g", Cup1:Cup5) %>% 
  # tare out weight of cup
  mutate(g = g - 1.941, 
         # convert ID to factor
         ID = as.factor(ID))

p.chili2 <- levene.boot(dat.within$g, 10.27, 1.68, 11)
summary(p.chili2) # Median result, F(1, 34) = 0.93, p = 0.341

# are the ts accurate given lopsided cell sizes? ----
# Broadly, yes

# hero-choc vs villain-choc
calc_t(m1 = 15.43, sd1 = 8.26, n1 = 44.5, m2 = 8.51, sd2 = 3.65, n2 = 43.5)
# hero-choc vs neutral-choc
calc_t(m1 = 15.43, sd1 = 8.26, n1 = 44.5, m2 = 12.1, sd2 = 1.45, n2 = 11.5)
# villain-choc vs neutral-choc
calc_t(m1 = 8.51, sd1 = 3.65, n1 = 43.5, m2 = 12.1, sd2 = 1.45, n2 = 11.5)

# villain-chili vs hero-chili
calc_t(m1 = 16.12, sd1 = 7.18, n1 = 42.5, m2 = 8.44, sd2 = 3.04, n2 = 43.5)
# villain-chili vs neutral-chili
calc_t(m1 = 16.12, sd1 = 7.18, n1 = 42.5, m2 = 10.27, sd2 = 1.68, n2 = 11.5)
# neutral-chili vs hero-chili 
# This one mismatches? Text says t = 1.91, I get t = 2.7
calc_t(m1 = 8.44, sd1 = 3.04, n1 = 43.5, m2 = 10.27, sd2 = 1.68, n2 = 11.5)

# to make it match i have to drop a lot of n
# Perhaps there's a Welch adjustment for unequal variances?
calc_t(m1 = 8.44, sd1 = 3.04, n1 = 26, m2 = 10.27, sd2 = 1.68, n2 = 5)
# I don't know how to get d = 0.66 out of this
tes(1.91, 26, 5)
# Reported d = 0.66 seems approx consistent with my inferred cell sizes
tes(1.91, 43.5, 11.5)
tes(1.91, 44, 11)
tes(1.91, 45, 10)

# 8. Qualtrics questionnaire from study 1 ----
# Participants did a Qualtrics questionnaire of how nice/mean the sauce amount is
qualtrics <- read_csv("mydata/qualtrics.csv")
with(qualtrics, table(KnowSpicy)) # mostly yes
summarize_at(qualtrics, .funs = funs(mean), .vars = vars(KnowSpicy:ChocoMean))

# all subjects
gather(qualtrics, "key", "value", ChiliNice:ChocoMean) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~key)
# filter for those who indicate knowing how spicy the sauce is
filter(qualtrics, KnowSpicy == 1) %>% 
  gather("key", "value", ChiliNice:ChocoMean) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~key)

# Make a correlation table
select(qualtrics, ChiliNice:ChocoMean) %>% 
  cor() %>% 
  round(3)

# let's look at effect sizes of choco vs chili as nice/mean
select(qualtrics, ChiliNice:ChocoMean) %>% 
  summarize_all(.funs = funs(mean, sd)) %>% 
  gather("key", "value") %>% 
  separate(key, c("var", "stat"), sep = "_")
# 
(4.87 - 2.01) / 1.3 # about d = 2.2 for "chili meaner than choco"
(5.43 - 2.69) / 1.5 # about d = 1.8 for "choco nicer than chili"
