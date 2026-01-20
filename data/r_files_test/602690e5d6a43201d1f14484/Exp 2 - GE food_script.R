##--------------------------------------------##
# Analysis Exp 2 Boosting Consensus Reasoning
# Aart van Stekelenbrug
# a.vanstekelenburg@bsi.ru.nl
##--------------------------------------------##

# Note: The control condition referred to in some variables in the data/script corresponds to
# the consensus only condition in the accompanying paper. The value name is corrected in preparing
# the data.


# Sections in code -------------------------------------------------------------------------------------
# 1. Preparation
# 2. Prepare data
# 3. Sequential sampling analyses
# 4. Confirmatory analyses complete sample
# 5. Exploratory analyses


### 1. Preparation ---------------------------------------------------------------------------------------

# first install package pacman, to be able to install and load subsequent packages
install.packages("pacman")

# install and load required packages
pacman::p_load(
  ggplot2,
  #plotting
  reshape2,
  # melting
  Rmisc,
  # summarySE
  jtools,
  # theme_apa
  BayesFactor,
  # Bayesian ANCOVA etc
  psych,
  # cohen.d etc
  lsr,
  # etasquared etc
  car,
  # ANOVA etc
  multcomp,
  # glht
  here,
  # for here
  WRS2,
  # for robust ancova
  apaTables,
  # for ci partial eta squared
  skimr,
  # for skim
  tidyr,
  # for tidy data
  dplyr,
  # recode etc
  irr
  # for inter rater reliab
)


# disable exponential notation
options(scipen = 999)


### 2. Prepare data --------------------------------------------------------------------------------

# load anonymous data (make sure the data from the OSF are in the same folder as this script)
df_clean <- read.csv(here("BCR_Exp2_clean_anon.csv"))


# count participants who do hold misperception at the start of the experiment (prior belief measure)
length(which(df_clean$Prior > 0)) # 467


# remove participants who failed the attention check
df_clean <-
  subset(df_clean, AC == 2) # 489 > 479 (removed 10 failed ACs)


# remove participants with not US nationality
df_clean <-
  subset(df_clean, Nationality == "United States of America") # removed 4, leaves 475 participants


# again, count participants who do hold misperception at the start of the experiment (prior belief measure)
length(which(df_clean$Prior > 0)) # 453 now


# check responses for anything indicating problem with data (in Excel, makes for easier data viewing)

df_clean[c(56, 75, 214, 304, 325, 443), c(1, 15, 20)]  # for these rows (participants), print post and justification
# these participants should be excluded:
# 4 participants explicitly state that they made a mistake in the posterior belief measure
# 1 of those 4 (row 325) did not believe in misp at start of study so will be excluded at a later stage anyway
# 2 clearly chose opposite of what was intended (PPs 213, 439)

# remove participants who incorrectly answered post measure
df_clean <-
  df_clean[-c(56, 75, 214, 304, 443), ] # leaves 470 participants


# five participants clearly provided nonsense information, exclude
df_clean[c(227, 250, 263, 386, 439), ]

df_clean <-
  df_clean[-c(227, 250, 263, 386, 439), ] # leaves 465 participants


# one participant in the Consensus only condition almost literally gave correct answers to manipulation check
df_clean[176, ] # impossible without actually havng seen one of the boosts, remove

df_clean <- df_clean[-176, ] # leaves 464 participants


# calculate pre-post difference score per participant
df_clean$PP_DS <-
  df_clean$Post - df_clean$Prior


# compute difference score for consensus
df_clean$PPcons_DS <-
  df_clean$Post_cons - df_clean$Prior_cons


# compute centered version prior
df_clean$Prior_c <-
  scale(df_clean$Prior, center = T, scale = F) # do not standardize (scale = false)


# compute OMC score (min -3 to max +3)
df_clean$OMCscore <-
  (
    df_clean$OMC1 - df_clean$OMC2 + df_clean$OMC3 - df_clean$OMC4 + df_clean$OMC5 - df_clean$OMC6
  ) / 6 # questions 2, 4, and 6 reversed by subtracting


# compute boost evaluation score
df_clean$BEscore <-  (df_clean$BE_useful + df_clean$BE_apply) / 2


# compute message evaluation score (min -3 to max +3)
df_clean$MEscore <-
  (
    df_clean$ME_fair + df_clean$ME_biased + df_clean$ME_trust + df_clean$ME_accur + df_clean$ME_wholeStory +
      df_clean$ME_factual + df_clean$ME_factOpinion
  ) / 7 # remember: lower score is more positive evaluation


# compute political orientation variable

# first recode character values to numeric values
df_clean$Pol_affil_n <-
  recode(
    df_clean$Pol_affil,
    "Strong Democrat" = 1,
    "Democrat" = 2,
    "Independent Lean Democrat" = 3,
    "Independent" = 4,
    "Independent Lean Republican" = 5,
    "Republican" = 6,
    "Strong Republican" = 7
  )
df_clean$Pol_ideo_n <-
  recode(
    df_clean$Pol_ideo,
    "Very liberal" = 1,
    "Liberal" = 2,
    "Moderate" = 3,
    "Conservative" = 4,
    "Very conservative" = 5
  )

# then standardize and set center (with indepedent and moderate as middle scores) and create composite score
df_clean$Pol_affil_n <-
  scale(df_clean$Pol_affil_n, scale = T, center = 4)
df_clean$Pol_ideo_n <-
  scale(df_clean$Pol_ideo_n, scale = T, center = 3)

# combine centered and standardized variables into political orientation
df_clean$Pol_orient <-
  df_clean$Pol_affil_n + df_clean$Pol_ideo_n # negative scores = democrat/liberal, 0 = independent/moderate, positive scores = republican/conservative


# compute variable for reading time infographic (combine boost+/boost/consensus only columns)
df_clean$ReadTimeInf <-
  pmin(df_clean$ReadTime_BP,
       df_clean$ReadTime_B,
       df_clean$ReadTime_C,
       na.rm = T)

# can remove old variables from df for tidiness
df_clean <-
  subset(df_clean, select = -c(ReadTime_B, ReadTime_BP, ReadTime_C))


### compute variable for number of correct answers to practiceclaims combining all three conditions

# first combine variables

df_clean$PracticeClaim1 <-
  paste(df_clean$PracticeClaim1_boost,
        df_clean$PracticeClaim1_control,
        sep = "")

df_clean$PracticeClaim2 <-
  paste(df_clean$PracticeClaim2_boost,
        df_clean$PracticeClaim2_control,
        sep = "")

df_clean$PracticeClaim3 <-
  paste(df_clean$PracticeClaim3_boost,
        df_clean$PracticeClaim3_control,
        sep = "")

# then count correct answers and save in new variable

df_clean$countPT <-
  rep(0, times = nrow(df_clean)) # make count variable for practice trials and set at 0

for (row in 1:nrow(df_clean)) {
  if (df_clean$PracticeClaim1[row] == "No")  {
    df_clean$countPT[row] = df_clean$countPT[row] + 1
  }
  if (df_clean$PracticeClaim2[row] == "No")  {
    df_clean$countPT[row] = df_clean$countPT[row] + 1
  }
  if (df_clean$PracticeClaim3[row] == "Yes")  {
    df_clean$countPT[row] = df_clean$countPT[row] + 1
  }
} # # when correct (1 = no, 2 = no, 3 = yes) add point to score

# can remove old variables from df for tidiness
df_clean <-
  subset(
    df_clean,
    select = -c(
      PracticeClaim1_boost,
      PracticeClaim1_control,
      PracticeClaim2_boost,
      PracticeClaim2_control,
      PracticeClaim3_boost,
      PracticeClaim3_control
    )
  )


### compute correct answers for check of reading instruction in boost+

df_clean$countMCBP <-
  rep(0, times = nrow(df_clean)) # make count variable and set at 0

for (row in 1:nrow(df_clean)) {
  if (df_clean$ManCheckBP1[row] == "Vitamin supplements are good for your heart")  {
    df_clean$countMCBP[row] = df_clean$countMCBP[row] + 1
  }
  if (df_clean$ManCheckBP2[row] == "It starts with a question - Evidence is gathered - A consensus develops - Evaluate a claim")  {
    df_clean$countMCBP[row] = df_clean$countMCBP[row] + 1
  }
  if (df_clean$ManCheckBP3[row] == "The evidence is continuously discussed by scientists with the relevant expertise.")  {
    df_clean$countMCBP[row] = df_clean$countMCBP[row] + 1
  }
} # give points for correct answers

for (row in 1:nrow(df_clean)) {
  if (df_clean$Condition[row] != "BoostPlus") {
    df_clean$countMCBP[row] = NA
  }
} # only participants in boost+ answered these questions, set Consensus only and boost to NA


# use shorter name for dataframe
df <- df_clean

rm(df_clean) # keep tidy by removing old dfs

# save dataframe so can complete manipulation check manually in excel
write.csv(df[, c("PP", "ManCheck1", "ManCheck2", "ManCheck3")], file = "BCR_Exp2_MC.csv") # mancheck data


# complete manipulation check manually. 2 new variables: Consensus_mention (consensus mentioned in any
# of the steps (yes or no) and countMC (integer with number of steps from boost mentioned)

# load manipulation check data from coder and add to main df
MC <- read.csv("BCR_Exp2_MC_Coded.csv")

# remove variables that are already in main datafile (ManCheck1, ManCheck2, ManCheck3)
MC <- subset(MC, select = -c(ManCheck1, ManCheck2, ManCheck3))

# set PP to factor
df$PP <- as.factor(df$PP)
MC$PP <- as.factor(MC$PP)

# drop unused levels for df$PP before joining datasets
df$PP <- droplevels(df$PP)

# join to main df
df <- left_join(df, MC, by = "PP")

# and remove MC for tidiness
rm(MC)


### rename condition value 'control' to 'consensus only' to fit with manuscript

df$Condition <-
  recode(df$Condition, "Control" = "Consensus only")


### make subsets for contrasts

# boosts combined versus Consensus only (contrast 1)
dfC1 <- df # make new dataframe

dfC1$Condition <-
  recode(dfC1$Condition, BoostPlus = "Boost") # make conditions for contrast 1
contrasts(dfC1$Condition) # check, 0 / intercept = boost, 1 / coefficient = Consensus only, easier interpretation if reverse
dfC1$Condition <-
  relevel(dfC1$Condition, ref = "Consensus only") # set consensus only as reference level
contrasts(dfC1$Condition) # check

# boost+ vs boost (drop Consensus only; contrast 2)
dfC2 <-
  subset(df, Condition == "Boost" |
           Condition == "BoostPlus") # take subset for contrast 2

dfC2$Condition <-
  droplevels(dfC2$Condition) # drop unused level
levels(dfC2$Condition) # check
contrasts(dfC2$Condition) # check, 0 / intercept = boost, 1 / coefficient = boostplus, useful for interpretation

# exploraty contrast between boost+ and consensus only (contrast 3)
dfC3 <-
  subset(df, Condition == "Consensus only" |
           Condition == "BoostPlus") # take subset for contrast 2

dfC3$Condition <-
  droplevels(dfC3$Condition) # drop unused level
levels(dfC3$Condition) # check
contrasts(dfC3$Condition) # check, 0 / intercept = boost+, 1 / coefficient = Consensus only, change
dfC3$Condition <-
  relevel(dfC3$Condition, ref = "Consensus only") # relevel
contrasts(dfC3$Condition) # check


# take subset of participants who already believe GE food is safe, might need it later  (though cannot really use these data, participants have demonstrated to be inconsistent)
df_nonMisp <-
  subset(df, Prior < 0) # 18, 4 participants who scored 'don't know' (0) not included


# select participants who hold misperception at the start of the experiment (prior belief measure)
df_misp <-
  subset(df, Prior > 0) # 442 participants


# make df_misp the standard df from here on out, shorter name is easier
df <- df_misp
dfC1 <- subset(dfC1, Prior > 0) # also for contrast dfs
dfC2 <- subset(dfC2, Prior > 0)
dfC3 <- subset(dfC3, Prior > 0) # and for exploratory contrast


### 3. Sequential sampling analyses ----------------------------------------------------------------------

# The BF for the hypotheses will be calculated using two Bayesian ANCOVA (one for each hypothesis).
# If the BF for both hypothesis 1 is smaller than 1/6 or bigger than 6 AND the BF for hypothesis 2 is smaller than
# 1/3 or bigger than 3, data collection will be stopped. We will check the BF at every 75 new participants.

## ~~First check: 22-08-2019. total, total participants = 221 ----

# take subsets of only first 221 participants for hypothesis 1
dfC1_1 <- dfC1[1:221, ]

# and subset of first 149 for hypo 2
dfC2_1 <- dfC2[1:149, ]

# hypothesis 1

bf1aFull <-
  lmBF(Post ~ Condition + Prior, data = dfC1_1) # setup full model
bf1aNoCon <-
  lmBF(Post ~ Prior, data = dfC1_1) # setup model to compare it to
BF1a <- bf1aFull / bf1aNoCon
BF1a # BF for condition 0.25 for no effect comparing boosts to Consensus only

# hypothesis 2

bf1bFull <- lmBF(Post ~ Condition + Prior, data = dfC2_1)
bf1bNoCon <- lmBF(Post ~ Prior, data = dfC2_1)
BF1b <- bf1bFull / bf1bNoCon
BF1b # BF for condition 0.27 for no effect comparing boost to boost+


## ~~Second check: 27-08-2019. total participants = 293 ----

# take subsets of only first 293 participants for hypothesis 1
dfC1_2 <- dfC1[1:293, ]

# and subset of first 196 for hypo 2
dfC2_2 <- dfC2[1:196, ]

# hypothesis 1

bf2aFull <-
  lmBF(Post ~ Condition + Prior, data = dfC1_2) # setup full model
bf2aNoCon <-
  lmBF(Post ~ Prior, data = dfC1_2) # setup model to compare it to
BF2a <- bf2aFull / bf2aNoCon
BF2a # BF for condition now 0.50 for no effect comparing boosts to Consensus only

# hypothesis 2

bf2bFull <- lmBF(Post ~ Condition + Prior, data = dfC2_2)
bf2bNoCon <- lmBF(Post ~ Prior, data = dfC2_2)
BF2b <- bf2bFull / bf2bNoCon
BF2b # BF for condition now 0.24 for no effect comparing boost to boost+


## ~~Third check: 28-08-2019. total participants = 368 ----

# take subsets of only first 368 participants for hypothesis 1
dfC1_3 <- dfC1[1:368, ]

# and subset of first 248 for hypo 2
dfC2_3 <- dfC2[1:248, ]

# hypothesis 1

bf3aFull <-
  lmBF(Post ~ Condition + Prior, data = dfC1_3) # setup full model
bf3aNoCon <-
  lmBF(Post ~ Prior, data = dfC1_3) # setup model to compare it to
BF3a <- bf3aFull / bf3aNoCon
BF3a # BF for condition now 0.47 for no effect comparing boosts to Consensus only

# hypothesis 2

bf3bFull <- lmBF(Post ~ Condition + Prior, data = dfC2_3)
bf3bNoCon <- lmBF(Post ~ Prior, data = dfC2_3)
BF3b <- bf3bFull / bf3bNoCon
BF3b # BF for condition now 1.35 for no effect comparing boost to boost+

# Registered maximum N reached, data collection halted


### 4. Confirmatory analyses complete sample ----------------------------------------------------------------------------

## ~~Descriptives ----

# take quick look at data
skim(df)

# DVs
describeBy(df$Post, df$Condition) # small mean differences in expected direction
describeBy(df$Post_cons, df$Condition) # again, small differences in exp direction
describeBy(df$GE_Worry, df$Condition) # small differences in opposite direction
describeBy(df$GE_Intention, df$Condition) # here too
describeBy(df$GE_PolSupport, df$Condition) # boost condition bit higher than other 2


# check prior and difference score
describe(df$Prior)
describe(df$Post) # big difference prior to posterior
describeBy(df$Prior, df$Condition) # good randomization
describeBy(df$PP_DS, df$Condition) # again, small differences in exp direction

describe(df$Prior_cons)
describe(df$Post_cons) # also big difference prior to posterior
describeBy(df$Prior_cons, df$Condition)
describeBy(df$PPcons_DS, df$Condition) # quite some differences in exp direction


# and follow-up scores
describe(df$Post2) # still strong correction
describe(df$Post2_cons) # here too, still increase, but smaller than before
describeBy(df$Post2, df$Condition) # difference between boost+ and Consensus only gone
describeBy(df$Post2_cons, df$Condition) # small differences


# manipulation check
prop.table(table(df$Consensus_mention, df$Condition), margin = 2) # check consensus mentions
describeBy(df$countMC, df$Condition) # consensus reasoning total steps menioned if Consensus_mention

describeBy(dfC2$BEscore, dfC2$Condition) # eval slightly higher for short boost
describeBy(df$countPT, df$Condition) # also interesting: number of correct answers in practice trials
describe(df[df$Condition == "BoostPlus", ]$countMCBP) # see if in boost+ participants were paying attention to infographic


# note: age and gender were removed from OSF data to protect participants' privacy
# data are available upon request

# participant descriptives
table(df$Gender_MC) # 266 female, 174 male, 1 would rather not say, 1 other
df$Gender_text # other was: trans
describe(df$Age) # mean age is 38.54, SD = 13.06
table(df$Condition) # 147 boost+, 149 boost, 146 consensus only
table(df$Education)
table(df$Pol_affil)
table(df$Pol_ideo)


# time difference between experiment and follow-up
length(na.omit(df$TD)) # 365, with 370 completes, so 5 were dropped in data cleaning process

mean(df$TD, na.rm = T) # 14.02889 days = about 14 days, 42 minutes
sd(df$TD, na.rm = T) # 0.4206377 days = about 10 hours and 6 minutes
min(df$TD, na.rm = T) # 15.9375 days = about 15 days, 22 hours and 30 minutes
max(df$TD, na.rm = T) # 12.85694 days = about 12 days, 20 hours and 34 minutes


# visualize main DV
ggplot(df, aes(x = Post)) +
  geom_histogram() # as expected, seems a bit left-skewed

# visualize difference between prior and post by boost condition
ggplot(df, aes(x = PP_DS, color = Condition)) +
  geom_density() # looks like some change


## ~~Manipulation check ----

# mention consensus or not?

table(df$Consensus_mention, df$Condition) # manipulation quite succesful, seems

chisq.test(dfC1$Consensus_mention, dfC1$Condition) # boost seems successful
chisq.test(dfC2$Consensus_mention, dfC2$Condition) # explore boost vs boost+: no sign diff

prop.table(table(dfC1$Consensus_mention, dfC1$Condition), margin = 2) # substantial difference


# number of consensus reasoning steps mentioned

describeBy(dfC1$countMC, dfC1$Condition) # large mean difference

t.test(dfC1$countMC ~ dfC1$Condition, alternative = "less") # boost seems successful
cohen.d(dfC1[, c("countMC", "Condition")], "Condition") # get cohen's d and CIs

t.test(dfC2$countMC ~ dfC2$Condition) # explore contrast 2: not significant


### explore boost evaluation

describeBy(dfC2$BEscore, dfC2$Condition) # shorter boost slighlty better evaluated

t.test(dfC2$BEscore ~ dfC2$Condition) # difference sign
cohen.d(dfC2[, c("BEscore", "Condition")], "Condition") # get cohen's d and CIs


### explore reading time infographics

describeBy(df$ReadTimeInf, df$Condition) # as expected most time on boost+, then boost, least in Consensus only
which(abs(scale(df$ReadTimeInf)) > 3) # 5 univariate outliers at > 3SD
describeBy(df[-c(4, 76, 152, 312, 352),]$ReadTimeInf, df[-c(4, 76, 152, 312, 352),]$Condition) # descriptives without outliers


### explore if difference in number of correct answers in practice trials between conditions

t.test(dfC1$countPT ~ dfC1$Condition) # boost does not seem succesful for practice trials, even opposite

describeBy(dfC1$countPT, dfC1$Condition) # get descriptives. higher in Consensus only

t.test(dfC2$countPT ~ dfC2$Condition) # no sign difference for practicetrials between boost conditions

describeBy(dfC2$countPT, dfC2$Condition) # get descriptives


# check correlation with scientists, maybe anti-GE'ers do not accept boost when low trust in scientists
cor.test(dfC2$TrustScientists, dfC2$countMC) # no sign correlation with trust biomedical scientists
cor.test(dfC2$TrustScientists, as.numeric(dfC2$Consensus_mention)) # no sign correlation


## ~~Assumptions check ----

# check independence IV and cov (sort of randomization check)
Anova(lm(dfC1$Prior ~ dfC1$Condition), type = 3) # no problem
Anova(lm(dfC2$Prior ~ dfC2$Condition), type = 3) # no problem


# levenes test for homogeneity of variance between groups per DV with median
leveneTest(dfC1$Post, dfC1$Condition) # might be slight problem, probably due to differences in group size
leveneTest(dfC2$Post, dfC2$Condition) # no problem

# also test if variances differ between three conditions in general (if not, then report Glass's delta instead of cohen's d)
leveneTest(df$Post, df$Condition) # no significant difference, so can report cohen's d


# univariate outliers
which(abs(scale(df$Prior)) > 3)
which(abs(scale(df$Post)) > 3) # no univ outliers


# also check pre-post difference score as indicator of model outliers
which(abs(scale(df$PP_DS)) > 3) # no potential outliers


## ~~Setup models & model diagnostics ----

# setup models
model1 <-
  aov(Post ~ Condition + Prior, dfC1) # model for hypothesis 1
model2 <-
  aov(Post ~ Condition + Prior, dfC2) # model for hypothesis 2


# as registered check model outliers and remove > 3 sd

# check standarized residuals model1
dfC1$standRes <- rstandard(model1)
plot(dfC1$standRes)
which(abs(dfC1$standRes) > 3) # no model outliers, as expected

length(which(abs(dfC1$standRes) > 2)) / length(dfC1$standRes) # 4.98% stand res larger/smaller than ± 2, is fine
length(which(abs(dfC1$standRes) > 2.5)) / length(dfC1$standRes) # 1.81%, model doing good


# check standarized residuals model2
dfC2$standRes <- rstandard(model2)
plot(dfC2$standRes)
which(abs(dfC2$standRes) > 3) # no outliers

length(which(abs(dfC2$standRes) > 2)) / length(dfC2$standRes) # 3.72% stand res larger/smaller than ± 2
length(which(abs(dfC2$standRes) > 2.5)) / length(dfC2$standRes) # 2.03%, model doing good


# check model1 assumptions
plot(model1) # model looks acceptable
sqrt(vif(model1)) > 2 # no problem with multicoll
durbinWatsonTest(model1) # no problem with autocorrelation


# check model2 assumptions
plot(model2) # also look okay
sqrt(vif(model2)) > 2 # no problem with multicoll
durbinWatsonTest(model2) # slight problem with autocorrelation


## ~~Hypothesis testing ----

### hypothesis 1

Anova(model1, type = 3) # # nonsignficant difference between boosts and Consensus only
etaSquared(model1, type = 3) # get effect size anyway
get.ci.partial.eta.squared(2.9096, 1, 439, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

describeBy(dfC1$Post, dfC1$Condition) # get descriptives

# Bayesian equivalent
bf1Full <- lmBF(Post ~ Condition + Prior, data = dfC1)
bf1NoCon <- lmBF(Post ~ Prior, data = dfC1)
BFc1 <- bf1Full / bf1NoCon
BFc1 # BF 0.45 against hypo 1


### hypothesis 2

Anova(model2, type = 3) # nonsign difference between boost+ and boost
etaSquared(model2, type = 3) # get effect size anyway
get.ci.partial.eta.squared(3.0257, 1, 293, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

describeBy(dfC2$Post, dfC2$Condition) # descriptives

# Bayesian equivalent
bf2Full <- lmBF(Post ~ Condition + Prior, data = dfC2)
bf2NoCon <- lmBF(Post ~ Prior, data = dfC2)
BFc2 <- bf2Full / bf2NoCon
BFc2 # BF 0.53 against hypothesis 2


## ~~Plotting ----

# first plot results for hypo 1

# make summary of data

# make long format data
mdfC1 <-
  melt(
    dfC1,
    id.vars = c("PP", "Condition"),
    measure.vars = c("Prior", "Post"),
    variable.name = "Time",
    value.name = "Belief"
  )

head(mdfC1) # check

# quick plot of prior and posterior
ggplot(mdfC1, aes(x = Belief, color = Time)) +
  geom_density() # appears to be a substantial difference between prior and posterior

# make summarised dataframe for plotting
sumDataC1 <-
  summarySE(
    data = mdfC1,
    measurevar = "Belief",
    groupvars = c("Condition", "Time")
  )

head(sumDataC1) # check

# plot hypo 1 results
plot1 <-
  ggplot(sumDataC1,
         aes(
           x = Time,
           y = Belief,
           colour = Condition,
           group = Condition
         )) +
  geom_errorbar(
    aes(ymin = Belief - ci, ymax = Belief + ci),
    colour = "black",
    width = .1,
    position = position_dodge(0.15) # move points bit to the left and right so they don't overlap
  ) +
  geom_line(position = position_dodge(0.15)) +
  geom_point(position = position_dodge(0.15), size = 3) +
  theme_apa() +
  theme(
    text = element_text(
      size = 18,
      family = "serif",
      color = "black"
    ),
    legend.text = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18, color = "black")
  ) +
  xlab("Measurement time") +
  ylab("Belief in GE food worse for health")
plot1


# lets also plot all 3 conditions separately

# make summary of data

# make long format data
mdf <-
  melt(
    df,
    id.vars = c("PP", "Condition"),
    measure.vars = c("Prior", "Post"),
    variable.name = "Time",
    value.name = "Belief"
  )

head(mdf) # check

# make summarised dataframe for plotting
sumData <-
  summarySE(
    data = mdf,
    measurevar = "Belief",
    groupvars = c("Condition", "Time")
  )

head(sumData) # check

# plot all
plot_all <-
  ggplot(sumData,
         aes(
           x = Time,
           y = Belief,
           colour = Condition,
           group = Condition
         )) +
  geom_errorbar(
    aes(ymin = Belief - ci, ymax = Belief + ci),
    colour = "black",
    width = .1,
    position = position_dodge(0.15)
  ) +
  geom_line(position = position_dodge(0.15)) +
  geom_point(position = position_dodge(0.15), size = 3) +
  theme_apa() +
  theme(
    text = element_text(
      size = 18,
      family = "serif",
      color = "black"
    ),
    legend.text = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18, color = "black")
  ) +
  xlab("Measurement time") +
  ylab("Belief in GE food worse for health ") +
  ylim (-2, 64) + # set limits to be similar to plot from Exp 3
  geom_hline(yintercept = 0, linetype = "dashed") # add dashed line for 0 (dont know)
plot_all # remember, this is with outliers (for post hoc contrast was 1 outlier)


# plot including all datapoints
plot_all_allPoints <-
  ggplot(NULL,
         aes(
           x = Time,
           y = Belief,
           colour = Condition,
           group = Condition
         )) +
  geom_point(
    data = mdf,
    aes(x = Time, y = Belief, colour = Condition),
    position = position_dodge(0.15),
    size = 1
  ) +
  geom_errorbar(
    data = sumData,
    aes(ymin = Belief - ci, ymax = Belief + ci),
    colour = "black",
    width = .1,
    position = position_dodge(0.15)
  ) +
  geom_line(
    data = sumData,
    aes(
      x = Time,
      y = Belief,
      colour = Condition,
      group = Condition
    ),
    position = position_dodge(0.15)
  ) +
  geom_point(
    data = sumData,
    aes(
      x = Time,
      y = Belief,
      colour = Condition,
      group = Condition
    ),
    position = position_dodge(0.15),
    size = 3
  ) +
  theme_apa() +
  theme(
    text = element_text(
      size = 18,
      family = "serif",
      color = "black"
    ),
    legend.text = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18, color = "black")
  ) +
  xlab("Measurement time") +
  ylab("Belief in GE food worse for health") +
  geom_hline(yintercept = 0, linetype = "dashed") # add dashed line for 0 (dont know)
plot_all_allPoints # remember, includes outliers


## 5. Exploratory analyses ----

## ~~Post-hoc contrasts ----

PHmodel <-
  aov(Post ~ Condition + Prior, df) # set up model

# check outliers
df$standResPH <- rstandard(PHmodel) # save standardized residuals in variable
which(abs(df$standResPH) > 3) # one model outlier
df[c(308), ] # same as with contrast 2 model

PHmodel_NO <-
  aov(Post ~ Condition + Prior, df[-c(308), ]) # set up model without outlier

Anova(PHmodel, type = 3) # sign effect condition
etaSquared(PHmodel, type = 3) # effect size for omnibus effect
get.ci.partial.eta.squared(3.0393, 2, 438, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!
summary(glht(PHmodel, mcp(Condition = "Tukey"))) # significant difference between boost+ and Consensus only

Anova(PHmodel_NO, type = 3) # omnibus ANOVA for posthoc comparisons significant
etaSquared(PHmodel_NO, type = 3) # effect size for omnibus effect
get.ci.partial.eta.squared(3.2652, 2, 437, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!
summary(glht(PHmodel_NO, mcp(Condition = "Tukey"))) # same as with outliers

describeBy(df$Post, df$Condition) # boost+ seems effective in helping correct misperception


# Explore boost+ vs consensus only, specific contrast NHST and BF (for effect size and in light of results exp 3)

# NHST
model3 <-
  aov(Post ~ Condition + Prior, dfC3) # setup contrast model to determine effect size

dfC3$SR3 <- rstandard(model3)
which(abs(dfC3$SR3) > 3) # no model outliers

Anova(model3, type = 3)
etaSquared(model3, type = 3) # effect size for boost+ vs. Consensus only
get.ci.partial.eta.squared(5.285, 1, 290, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!
cohen.d(dfC3[, c("PP_DS", "Condition")], "Condition") # and cohen's d and CIs using change scores

# Bayesian equivalent
bfC3Full <- lmBF(Post ~ Condition + Prior, data = dfC3)
bfC3NoCon <- lmBF(Post ~ Prior, data = dfC3)
BFC3 <- bfC3Full / bfC3NoCon
BFC3 # BF 1.52 for difference between boost+ and Consensus only


## ~~Follow-up ----

# take subsets of participants that completed follow-up
df_fol <- subset(df,!is.na(Post2))

# check selective attrition (condition, trust, pol, age, education)
table(df$Condition)
table(df_fol$Condition) # no big differences in attrition between conditions; boost: 84.56%, boost+: 88.44%, consensus-only: 74.66% 

describe(df$TrustScientists)
describe(df_fol$TrustScientists) # also no substantial difference in trust in scientists
t.test(df$TrustScientists, df_fol$TrustScientists) # nonsign

describe(df$Pol_orient)
describe(df_fol$Pol_orient) # also no substantial difference in political orientation
t.test(df$Pol_orient, df_fol$Pol_orient) # nonsign

describe(df$Age)
describe(df_fol$Age) # also no substantial difference in age
t.test(df$Age, df_fol$Age) # nonsign

table(df$Education)
table(df_fol$Education) # also no substantial difference in education; col / A lev: 80.34%, grad degree: 84.21%, sec school / GCSE: 81.93%, undergrad: 83.33%

# compare conditions on belief at follow-up
PHmodel_fol <-
  aov(Post2 ~ Condition + Prior, df_fol) #

df_fol$SRfol <- rstandard(PHmodel_fol)
which(abs(df_fol$SRfol) > 3) # two model outliers
df_fol[c(130, 134), ] # just low misp at follow-up

PHmodel_fol_NO <-
  aov(Post2 ~ Condition + Prior, df_fol[-c(130, 134), ]) #

Anova(PHmodel_fol, type = 3) # omnibus ANOVA for posthoc comparisons nonsignificant
Anova(PHmodel_fol_NO, type = 3) # again, pretty similar

# check comparisons anyway
summary(glht(PHmodel_fol, mcp(Condition = "Tukey"))) # nonsign differences
summary(glht(PHmodel_fol_NO, mcp(Condition = "Tukey"))) # same without outliers

describeBy(df_fol$Post2, df_fol$Condition) #


## ~~Prior-post difference in belief across conditions ----

t.test(df$Prior,
       df$Post,
       paired = T,
       alternative = "greater") # sign for post greater than prior

PP_CD <-
  cohensD(df$Prior, df$Post, method = "paired") # get cohen's d
d.ci(PP_CD, n1 = nrow(df)) # get 95% CIs for this effect size and sample size

describe(df$Prior)
describe(df$Post)


## ~~Prior-follow-up difference in belief across conditions ----

t.test(df$Prior,
       df$Post2,
       paired = T,
       alternative = "greater") # sign for post greater than prior

PP_CD_fol <-
  cohensD(df$Prior, df$Post2, method = "paired") # get cohen's d
d.ci(PP_CD_fol, n1 = nrow(df_fol)) # get 95% CIs for this effect size and sample size

describe(df$Prior)
describe(df$Post2)

# but also sign different from post belief? (two-tailed)

t.test(df$Post,
       df$Post2,
       paired = T) # also sign

PP_CD_fol2 <-
  cohensD(df$Post, df$Post2, method = "paired") # get cohen's d
d.ci(PP_CD_fol2, n1 = nrow(df_fol)) # get 95% CIs for this effect size and sample size

describe(df$Post)
describe(df$Post2)


## ~~Perceived consensus ----

# posterior perceived consensus as function of boost

PHmodel_cons <-
  aov(Post_cons ~ Condition + Prior_cons, df) #

df$standResPH_cons <- rstandard(PHmodel_cons)
which(abs(df$standResPH_cons) > 3) # no model outliers

Anova(PHmodel_cons, type = 3) # omnibus ANOVA for posthoc comparisons without outlier, sign
etaSquared(PHmodel_cons, type = 3) # effect size 
get.ci.partial.eta.squared(3.5484, 2, 438, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

summary(glht(PHmodel_cons, mcp(Condition = "Tukey"))) # explore contrasts, cons only vs boost+ sign

describeBy(df$Post_cons, df$Condition) # in expected direction


# pre-post difference consensus

t.test(df$Post_cons,
       df$Prior_cons,
       paired = T,
       alternative = "greater") # sign for post greater than prior, across conditions

PP_CS_cons <-
  cohensD(df$Prior_cons, df$Post_cons, method = "paired") # get cohen's d
d.ci(PP_CS_cons, n1 = nrow(df)) # get 95% CIs for this effect size and sample size

describe(df$Prior_cons)
describe(df$Post_cons)


# post hoc contrasts for follow-up measure perceived consensus

PHmodel_cons_fol <-
  aov(Post2_cons ~ Condition + Prior_cons, df_fol) #

df_fol$SR_PH_CF <- rstandard(PHmodel_cons_fol)
which(abs(df_fol$SR_PH_CF) > 3) # one model outlier
df_fol[244, ] # at follow-up suddenly decreased perceived consensus enormously

PHmodel_cons_fol_NO <-
  aov(Post2_cons ~ Condition + Prior_cons, df_fol[-244, ]) #

Anova(PHmodel_cons_fol, type = 3) # nonsign
Anova(PHmodel_cons_fol_NO, type = 3) # same

summary(glht(PHmodel_cons_fol, mcp(Condition = "Tukey"))) # no sign diffences
summary(glht(PHmodel_cons_fol_NO, mcp(Condition = "Tukey"))) # nonsign diff boost+ vs boost

describeBy(df_fol[-244, ]$Post2_cons, df_fol[-244, ]$Condition) # in expected direction though


# pre-follow-up difference

t.test(df$Post2_cons,
       df$Prior_cons,
       paired = T,
       alternative = "greater") # sign for post2 greater than prior, across conditions

PP_CD_fol_cons <-
  cohensD(df$Prior_cons, df$Post2_cons, method = "paired") # get cohen's d
d.ci(PP_CD_fol_cons, n1 = nrow(df_fol)) # get 95% CIs for this effect size and sample size

describe(df$Prior_cons)
describe(df$Post2_cons)


# post-follow-up difference

t.test(df$Post_cons,
       df$Post2_cons,
       paired = T) # sign different post2 and post, across conditions

PP_CD_fol_cons2 <-
  cohensD(df$Post_cons, df$Post2_cons, method = "paired") # get cohen's d
d.ci(PP_CD_fol_cons2, n1 = nrow(df_fol)) # get 95% CIs for this effect size and sample size

describe(df$Post_cons)
describe(df$Post2_cons)


### explore if prepost difference in belief in misp correlated with prepost difference in perceived consensus

# first check correlation between difference scores
cor.test(df$PP_DS, df$PPcons_DS) # sign neg corr indicates might be mediation going on


## ~~Trust in science ----

describe(df$TrustScientists)
qplot(df$TrustScientists, binwidth = 0.5)

# effect boost on trust in biomedical scientists?

trust_model <-
  aov(TrustScientists ~ Condition, df) # setup model

# check outliers
df$SR_trust <- rstandard(trust_model)
which(abs(df$SR_trust) > 3) # no model outliers

Anova(trust_model, type = 3) # nothing goin on

describeBy(df$TrustScientists, df$Condition)


## ~~Worry, policy support, and intention ----

# setup models
worry_model <-
  aov(GE_Worry ~ Condition + Prior, df) #
polsup_model <-
  aov(GE_PolSupport ~ Condition + Prior, df) #
intention_model <-
  aov(GE_Intention ~ Condition + Prior, df) #

# check outliers
df$SR_worry <- rstandard(worry_model)
which(abs(df$SR_worry) > 3) # six model outliers
df[c(75, 154, 189, 191, 261, 301), ] # not sure if anything wrong with these scores

df$SR_polsup <- rstandard(polsup_model)
which(abs(df$SR_polsup) > 3) # two model outliers
df[c(306, 335), ] # 355 could be true outlier, not sure about 306

df$SR_intent <- rstandard(intention_model)
which(abs(df$SR_intent) > 3) # three model outliers
df[c(189, 306, 414), ] # 414 could be true outlier, not sure about other 2

# setup models without outliers
worry_model_NO <-
  aov(GE_Worry ~ Condition + Prior, df[-c(75, 154, 189, 191, 261, 301), ]) #
polsup_model_NO <-
  aov(GE_PolSupport ~ Condition + Prior, df[-c(306, 335), ]) #
intention_model_NO <-
  aov(GE_Intention ~ Condition + Prior, df[-c(189, 306, 414), ]) #

# results
Anova(worry_model, type = 3) # nonsign
Anova(polsup_model, type = 3) # nonsign
Anova(intention_model, type = 3) # nonsign

# without outliers
Anova(worry_model_NO, type = 3) # same
Anova(polsup_model_NO, type = 3) # now sign
Anova(intention_model_NO, type = 3) # same

# effect sizes and CIs
etaSquared(polsup_model_NO, type = 3) # effect size omn effect pol supp without outliers
get.ci.partial.eta.squared(4.0323, 2, 436, conf.level = 0.90) #

# post hoc comparisons with and without outliers
summary(glht(polsup_model, mcp(Condition = "Tukey"))) # nonsign 
summary(glht(polsup_model_NO, mcp(Condition = "Tukey"))) # boost+ vs boost sign without outliers

# descriptives
describeBy(df$GE_PolSupport, df$Condition) # as could be expected
describeBy(df[-c(306, 335), ]$GE_PolSupport, df[-c(306, 335), ]$Condition) # without outliers

# see if change in prior-post belief is correlated with secondary outcomes
cor.test(df$PP_DS, df$GE_Worry)
cor.test(df$PP_DS, df$GE_PolSupport)
cor.test(df$PP_DS, df$GE_Intention) # yes, strongly correlated with all secondary outcomes
