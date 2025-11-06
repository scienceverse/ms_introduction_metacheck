##--------------------------------------------##
# Analysis Exp 1 Boosting Consensus Reasoning
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
  compute.es,
  # for mes
  car,
  # ANOVA etc
  multcomp,
  # glht
  mediation,
  # for mediate
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
  irr,
  # for kappa and icc
  here
  # for here
)


# disable exponential notation
options(scipen = 999)


### 2. Prepare data --------------------------------------------------------------------------------

# load anonymous data (make sure the data from the OSF are in the same folder as this script)
df_clean <- read.csv(here("BCR_Exp1_clean_anon.csv"))


# count participants who do hold misperception at the start of the experiment (prior belief measure)
length(which(df_clean$Prior < 0)) # 222


# remove participants who failed the attention check
df_clean <-
  subset(df_clean, AC == 2) # 270 > 268 (removed 2 failed ACs, note that both were non misp holders)


# remove participants who report different nationality than US
str(df_clean$Nationality) # no participants report other nationality


# count participants who do hold misperception at the start of the experiment (prior belief measure)
length(which(df_clean$Prior < 0)) # still 222


#  calculate pre-post difference score per participant
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


# compute message evaluation score (min -3 to max +3)
df_clean$MEscore <-
  (
    df_clean$ME_fair + df_clean$ME_biased + df_clean$ME_trust + df_clean$ME_accur +
      df_clean$ME_wholeStory + df_clean$ME_factual + df_clean$ME_factOpinion
  ) / 7 # remember, lower score is more positive evaluation


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
} # when correct (1 = no, 2 = no, 3 = yes) add point to score

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
} # only participants in boost+ answered these questions, set consensus only and boost to NA


# use shorter name for dataframe
df <- df_clean

rm(df_clean) # keep tidy by removing old df


# save dataframe so can complete manipulation check manually in excel
write.csv(df[, c("PP", "ManCheck1", "ManCheck2", "ManCheck3")], file = "BCR_Exp1_MC.csv") # mancheck data


# complete manipulation check manually. 2 new variables: Consensus_mention (consensus mentioned in any
# of the steps (yes or no) and countMC (integer with number of steps from boost mentioned)

# load manipulation check data from coder and add to main df
MC <-
  read.csv(here("BCR_Exp1_MC_coded.csv"))

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

# boosts combined versus consensus only (contrast 1)
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


# take subset of participants who already believe in climate change (though cannot really use these data, participants have demonstrated to be inconsistent)
df_nonMisp <-
  subset(df, Prior > 0) # 1 'don't know' participant not included


# select participants who hold misperception at the start of the experiment (prior belief measure)
df_misp <-
  subset(df, Prior < 0) # 222 participants


# make df_misp the standard df from here on out, shorter name is easer
df <- df_misp
dfC1 <- subset(dfC1, Prior < 0) # also for contrast dfs
dfC2 <- subset(dfC2, Prior < 0)
dfC3 <- subset(dfC3, Prior < 0) # and for exploratory contrast


### 3. Sequential sampling analyses ----------------------------------------------------------------------

# The BF for the hypotheses will be calculated using two Bayesian ANCOVA (one for each hypothesis).
# If the BF for both hypotheses are smaller than 1/6 or bigger than 6, data collection will be stopped.
# We will check the BF at every 45 (10% of maximum sample) new participants.

## ~~First check: 18-05-2019. total participants = 222 ----

# hypothesis 1

bf1Full <-
  lmBF(Post ~ Condition + Prior, data = dfC1) # setup full model
bf1NoCon <-
  lmBF(Post ~ Prior, data = dfC1) # setup model to compare it to
BFc1 <- bf1Full / bf1NoCon
BFc1 # BF for condition 0.20 for no effect comparing boosts to Consensus only

# hypothesis 2

bf2Full <- lmBF(Post ~ Condition + Prior, data = dfC2)
bf2NoCon <- lmBF(Post ~ Prior, data = dfC2)
BFc2 <- bf2Full / bf2NoCon
BFc2 # BF for condition 0.26 for no effect comparing boost to boost+

# data collection was halted after the first check due to unexpectedly high costs


### 4. Confirmatory analyses complete sample ----------------------------------------------------------------------------

## ~~Descriptives ----

# take quick look at data
skim(df)

# DVs
describeBy(df$Post, df$Condition) # seem pretty equal
describeBy(df$Post_cons, df$Condition) # small differences
describeBy(df$CC_Worry, df$Condition)
describeBy(df$CC_Intention, df$Condition)
describeBy(df$CC_PolSupport, df$Condition) # Consensus only scores higher on all secondary outcomes, weird


# check prior and difference score
describe(df$Prior)
describe(df$Post) # small but substantial difference with prior
describeBy(df$Prior, df$Condition) # randomization not completely successful, check later
describeBy(df$PP_DS, df$Condition) # changes seem quite equal, biggest change in boost

describe(df$Prior_cons)
describe(df$Post_cons) # quite substantial increase in perceived consensus prior to post
describeBy(df$Prior_cons, df$Condition) # again, randomization not perfect
describeBy(df$PPcons_DS, df$Condition) # but changes seem equal


# manipulation check
prop.table(table(df$Consensus_mention, df$Condition), margin = 2) # check consensus mentions
describeBy(df$countMC, df$Condition) # consensus reasoning total steps menioned

describeBy(df$countPT, df$Condition) # also interesting: number of correct answers in practice trials
describe(df[df$Condition == "BoostPlus", ]$countMCBP) # see if in boost+ participants were paying attention to infographic


# note: age and gender were removed from OSF data to protect participants' privacy
# data are available upon request

# participant descriptives
table(df$Gender_MC) # 109 female, 111 male, 1 would rather not say, 1 other
df$Gender_text # other was: non-binary
describe(df$Age) # mean age is 44.17, sd = 14.24
table(df$Condition) # 72 boost+, 76 boost, 74 consensus only
table(df$Education)
table(df$Pol_affil)
table(df$Pol_ideo)


# visualize main DV
ggplot(df, aes(x = Post)) +
  geom_histogram() # as expected, seems pretty right-skewed

# visualize difference between prior and post by boost condition
ggplot(df, aes(x = PP_DS, color = Condition)) +
  geom_density() # small differences, do not expect (huge) effects


## ~~Manipulation check ----

# mention consensus or not?

table(df$Consensus_mention, df$Condition) # looks small success

chisq.test(dfC1$Consensus_mention, dfC1$Condition) # boost seems successful
chisq.test(dfC2$Consensus_mention, dfC2$Condition) # explore boost vs boost+: no sign diff

prop.table(table(dfC1$Consensus_mention, dfC1$Condition), margin = 2) # about 18% vs 3%


# number of consensus reasoning steps mentioned

describeBy(df$countMC, df$Condition) # small mean difference

t.test(dfC1$countMC ~ dfC1$Condition, alternative = "less") # here too, boost seems successful
cohen.d(dfC1[, c("countMC", "Condition")], "Condition") # get cohen's d and CIs

t.test(dfC2$countMC ~ dfC2$Condition) # explore contrast 2: significant. boost higher than boost+
cohen.d(dfC2[, c("countMC", "Condition")], "Condition") # get cohen's d and CIs


### explore reading time infographics

describeBy(df$ReadTimeInf, df$Condition) # as expected most time on boost+, then boost, least in Consensus only
which(abs(scale(df$ReadTimeInf)) > 3) # 2 univariate outliers at > 3SD
describeBy(df[-c(72, 133), ]$ReadTimeInf, df[-c(72, 133), ]$Condition) # descriptives without outliers


### explore if difference in number of correct answers in practice trials between conditions

t.test(dfC1$countPT ~ dfC1$Condition) # boost does not seem successful for practice trials

describeBy(dfC1$countPT, dfC1$Condition) # get descriptives

t.test(dfC2$countPT ~ dfC2$Condition) # no sign difference for practice trials between boost conditions

describeBy(dfC2$countPT, dfC2$Condition) # get descriptives


# check correlation with scientists, maybe climate change deniers do not accept boost when low trust in scientists
cor.test(dfC2$TrustScientists, dfC2$countMC) # sign correlation with trust in climate scientists
cor.test(dfC2$TrustScientists, as.numeric(dfC2$Consensus_mention)) # sign correlation as well


## ~~Assumptions check ----

# check independence IV and cov (sort of randomization check)
Anova(lm(dfC1$Prior ~ dfC1$Condition), type = 3) # no problem
Anova(lm(dfC2$Prior ~ dfC2$Condition), type = 3) # very slight problem. prior in boost+ little bit higher


# levenes test for homogeneity of variance between groups per DV with median
leveneTest(dfC1$Post, dfC1$Condition) # no problem
leveneTest(dfC2$Post, dfC2$Condition) # no problem

# also test if variances differ between three conditions in general (if not, then report Glass's delta instead of cohen's d)
leveneTest(df$Post, df$Condition) # no significant difference, so can report cohen's d


# univariate outliers
which(abs(scale(df$Prior)) > 3)
which(abs(scale(df$Post)) > 3) # no univ outliers


# also check pre-post difference score as indicator of model outliers
which(abs(scale(df$PP_DS)) > 3) # small number of potential outliers, check model outliers later!


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
which(abs(dfC1$standRes) > 3) # some model outliers, as expected
dfC1[c(27, 54, 59, 76, 128, 175, 207), ] # identified these cases before

length(which(abs(dfC1$standRes) > 2)) / length(dfC1$standRes) # 7.21% stand res larger/smaller than ± 2, quite high
length(which(abs(dfC1$standRes) > 2.5)) / length(dfC1$standRes) # 4.95% exp stand res larger/smaller than ± 2.5, also quite high


# setup new model1 without outliers
dfC1_NO <- dfC1[-c(27, 54, 59, 76, 128, 175, 207), ]

model1_NO <-
  aov(Post ~ Condition + Prior, dfC1_NO) # model for hypothesis 1, no outliers


# check standarized residuals model1 without outliers
dfC1_NO$standResNO <- rstandard(model1_NO)
plot(dfC1_NO$standResNO)
which(abs(dfC1_NO$standResNO) > 3) # new model outliers

length(which(abs(dfC1_NO$standResNO) > 2)) / length(dfC1_NO$standResNO) # 6.98% stand res larger/smaller than ± 2, not really beter
length(which(abs(dfC1_NO$standResNO) > 2.5)) / length(dfC1_NO$standResNO) # 5.58%, model not doing good


# check standarized residuals model2
dfC2$standRes <- rstandard(model2)
plot(dfC2$standRes)
which(abs(dfC2$standRes) > 3) # also some outliers
dfC2[c(54, 85, 118, 138), ] # check

length(which(abs(dfC2$standRes) > 2)) / length(dfC2$standRes) # 7.43% stand res larger/smaller than ± 2
length(which(abs(dfC2$standRes) > 2.5)) / length(dfC2$standRes) # 4.73% larger/smaller than ± 2.5, also not doing good


# make new model2 without outliers
dfC2_NO <- dfC2[-c(54, 85, 118, 138), ]

model2_NO <-
  aov(Post ~ Condition + Prior, dfC2_NO) # model for hypothesis 2, no outliers


# check standarized residuals model2 without outlier
dfC2_NO$standResNO <- rstandard(model2_NO)
plot(dfC2_NO$standResNO)
which(abs(dfC2_NO$standResNO) > 3) # new model outliers

length(which(abs(dfC2_NO$standResNO) > 2)) / length(dfC2_NO$standResNO) # 7.64% stand res larger/smaller than ± 2
length(which(abs(dfC2_NO$standResNO) > 2.5)) / length(dfC2_NO$standResNO) # 6.25%, model not doing better


# check model1 assumptions
plot(model1_NO) # indicates serious problems with model, consider doing robust ANCOVA
sqrt(vif(model1_NO)) > 2 # no problem with multicoll
durbinWatsonTest(model1_NO) # no problem with autocorrelation


# check model2 assumptions
plot(model2_NO) # again, serious problems, also here, consider doing robust ANCOVA
sqrt(vif(model2_NO)) > 2 # no problem with multicoll
durbinWatsonTest(model2_NO) # slight problem with autocorrelation


# both models do not look great, should conduct additional robust ANCOVAs


## ~~Hypothesis testing ----

### hypothesis 1

Anova(model1_NO, type = 3) # # no signficant difference between boosts and Consensus only
etaSquared(model1_NO, type = 3) # get effect size anyway
get.ci.partial.eta.squared(0.4629, 2, 212, conf.level = 0.90) #

Anova(model1, type = 3) # explore model with outliers: also no signficant difference

describeBy(dfC1_NO$Post, dfC1_NO$Condition) # get descriptives (without outliers)

# Bayesian equivalent no outliers
bf1Full_NO <- lmBF(Post ~ Condition + Prior, data = dfC1_NO)
bf1NoCon_NO <- lmBF(Post ~ Prior, data = dfC1_NO)
BFc1_NO <- bf1Full_NO / bf1NoCon_NO
BFc1_NO # BF 0.19 against hypo 1


### hypothesis 2

Anova(model2_NO, type = 3) # no sign difference between boost+ and boost
etaSquared(model2_NO, type = 3) # get effect size anyway
get.ci.partial.eta.squared(1.7286, 1, 141, conf.level = 0.90) #

Anova(model2, type = 3) # also no sign difference with outliers

describeBy(dfC2_NO$Post, dfC2_NO$Condition) # get descriptives again

# Bayesian equivalent without model-outliers
bf2Full_NO <- lmBF(Post ~ Condition + Prior, data = dfC2_NO)
bf2NoCon_NO <- lmBF(Post ~ Prior, data = dfC2_NO)
BFc2_NO <- bf2Full_NO / bf2NoCon_NO
BFc2_NO # BF 0.39 against hypothesis 2 (weaker than with outliers)


### explore with bootstrapped, robust ANCOVA using WRS package

# run robust ancovas
model1_rob <-
  ancboot(Post ~ Condition + Prior, data = dfC1, nboot = 10000) # remember: CIs are corrected for inflated Type I error, p values are not
model1_rob
model2_rob <-
  ancboot(Post ~ Condition + Prior, data = dfC2, nboot = 10000)
model2_rob # results are similar to normal ANCOVA: nonsignificant

### explore without outliers

# run robust ancovas
model1_NO_rob <-
  ancboot(Post ~ Condition + Prior, data = dfC1_NO, nboot = 10000) # remember: CIs are corrected for inflated Type I error, p values are not
model1_NO_rob
model2_NO_rob <-
  ancboot(Post ~ Condition + Prior, data = dfC2_NO, nboot = 10000)
model2_NO_rob # results are similar to with outliers


## ~~Plotting ----

# no significant difference between boost conditions, so plot results for hypo 1

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
  ylab("Belief in human-caused climate change")
plot1


# lets also plot all 3 conditions separately, for completeness

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
  ylab("Belief in human-caused climate change")
plot_all


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
  ylab("Belief in human-caused climate change") +
  geom_hline(yintercept = 0, linetype = "dashed") # add dashed line for 0 (dont know)
plot_all_allPoints # remember, includes outliers


## 5. Exploratory analyses -----------------------------------------------------------------------------

## ~~Post-hoc contrasts ----

# NHST omnibus ANCOVA

PHmodel <-
  aov(Post ~ Condition + Prior, df) # set up model

# check model outliers
df$standResPH <-
  rstandard(PHmodel) # save standardized residuals in new variable
which(abs(df$standResPH) > 3) # 6 model outliers
df[c(54, 59, 76, 128, 175, 207), ] # same as with contrast 2 model

PHmodel_NO <-
  aov(Post ~ Condition + Prior, df[-c(54, 59, 76, 128, 175, 207), ]) # model without outliers

Anova(PHmodel, type = 3) # no sign effect condition
etaSquared(PHmodel, type = 3) # get effect size anyway
get.ci.partial.eta.squared(0.7239, 2, 218, conf.level = 0.90) #
summary(glht(PHmodel, mcp(Condition = "Tukey"))) # check comparisons anyway

Anova(PHmodel_NO, type = 3) # omnibus ANOVA for posthoc comparisons without outliers, same concl
summary(glht(PHmodel_NO, mcp(Condition = "Tukey"))) # same as with outliers

describeBy(df$Post, df$Condition) # descriptives


# Explore boost+ vs consensus only, specific contrast NHST and BF (in light of results exp 2 and 3)

# NHST
model3 <-
  aov(Post ~ Condition + Prior, dfC3) # setup model

dfC3$SR_M3 <- rstandard(model3)
which(abs(dfC3$SR_M3) > 3) # 4 model outliers
dfC3[c(35, 38, 49, 137), ] # not sure what is wrong with these responses, just changed belief a lot

model3_NO <-
  aov(Post ~ Condition + Prior, dfC3[-c(35, 38, 49, 137), ])

Anova(model3, type = 3) # no sign difference between boost+ and consensus only on post
Anova(model3_NO, type = 3) # compare to included outliers, highly similar

describeBy(dfC3$Post, dfC3$Condition) # get descriptives
cohen.d(dfC3[, c("PP_DS", "Condition")], "Condition") # and cohen's d and CIs

# Bayesian equivalent (with outliers)
bf3Full <- lmBF(Post ~ Condition + Prior, data = dfC3)
bf3NoCon <- lmBF(Post ~ Prior, data = dfC3)
BFc3 <- bf3Full / bf3NoCon
BFc3 # BF 0.1 against difference


## ~~Perceived consensus ----

# posterior perceived consensus as function of boost

PHmodel_cons <-
  aov(Post_cons ~ Condition + Prior_cons, df) #

df$standResPH_cons <- rstandard(PHmodel_cons)
which(abs(df$standResPH_cons) > 3) # 1 model outlier
df[c(105), ] # made a weird turn in consensus estiamte prior to post

PHmodel_cons_NO <-
  aov(Post ~ Condition + Prior, df[-c(105), ]) # model without outlier

Anova(PHmodel_cons, type = 3) # no sign effect condition
summary(glht(PHmodel_cons, mcp(Condition = "Tukey"))) # explore anyway
etaSquared(PHmodel_cons, type = 3) #
get.ci.partial.eta.squared(0.4241, 2, 218, conf.level = 0.90) #

Anova(PHmodel_cons_NO, type = 3) # omnibus ANOVA for posthoc comparisons without outlier, not sign
summary(glht(PHmodel_cons_NO, mcp(Condition = "Tukey"))) # explore anyway

describeBy(df$Post_cons, df$Condition) # in expected direction, could be too low power for small effect


# pre-post difference

t.test(df$Post_cons,
       df$Prior_cons,
       paired = T,
       alternative = "greater") # sign for post greater than prior, across conditions

PP_CS_cons <-
  cohensD(df$Prior_cons, df$Post_cons, method = "paired") # get cohen's d
d.ci(PP_CS_cons, n1 = nrow(df)) # get 95% CIs for this effect size and sample size

describe(df$Prior_cons)
describe(df$Post_cons)


### explore if prepost difference in belief in misp correlated with prepost difference in perceived consensus

# test correlation between difference scores
cor.test(df$PP_DS, df$PPcons_DS) # sign


## ~~Prior-post difference in belief across conditions ----

t.test(df$Post,
       df$Prior,
       paired = T,
       alternative = "greater") # sign for post greater than prior

PP_CD <-
  cohensD(df$Prior, df$Post, method = "paired") # get cohen's d
d.ci(PP_CD, n1 = nrow(df)) # get 95% CIs for this effect size and sample size

describe(df$Prior)
describe(df$Post)


## ~~Worry, policy support, and intention ----

# setup models
worry_model <-
  aov(CC_Worry ~ Condition + Prior, df) #
polsup_model <-
  aov(CC_PolSupport ~ Condition + Prior, df) #
intention_model <-
  aov(CC_Intention ~ Condition + Prior, df) #

# check outliers
df$SR_worry <- rstandard(worry_model)
which(abs(df$SR_worry) > 3) # 3 model outliers
df[c(25, 106, 142), ] # score very high on worry but low on belief in climate change

df$SR_polsup <- rstandard(polsup_model)
which(abs(df$SR_polsup) > 3) # 3 model outliers
df[c(25, 142, 175), ] # same as worry + one that scored very low prior

df$SR_intent <- rstandard(intention_model)
which(abs(df$SR_intent) > 3) # no model outliers

# setup models without outliers
worry_model_NO <-
  aov(CC_Worry ~ Condition + Prior, df[-c(25, 106, 142), ]) #
polsup_model_NO <-
  aov(CC_PolSupport ~ Condition + Prior, df[-c(25, 142, 175), ]) #

# results
Anova(worry_model, type = 3) # omnibus ANOVA not sign
Anova(polsup_model, type = 3) # omnibus ANOVA sign, check posthoc comparisons
Anova(intention_model, type = 3) # nothing

# without outliers
Anova(worry_model_NO, type = 3) # nonsignificant without outliers
Anova(polsup_model_NO, type = 3) # no longer sign without outliers

# effect size and CIs
etaSquared(polsup_model, type = 3) #
get.ci.partial.eta.squared(3.4921, 2, 218, conf.level = 0.90) #

# post hoc comparisons with and without outliers
summary(glht(polsup_model, mcp(Condition = "Tukey"))) # sign Consensus only vs boost+, weird because in opposite direction
summary(glht(polsup_model_NO, mcp(Condition = "Tukey"))) # non sign

# descriptives
describeBy(df$CC_PolSupport, df$Condition) # is this some kind of reactance?

# see if change in prior-post belief is correlated with secondary outcomes
cor.test(df$PP_DS, df$CC_Worry)
cor.test(df$PP_DS, df$CC_PolSupport)
cor.test(df$PP_DS, df$CC_Intention) # yes it is, for all three secondary outcomes


## ~~Trust in science ----

describe(df$TrustScientists)
qplot(df$TrustScientists, binwidth = 0.5)

# effect boost on trust in climate scientists?

trust_model <-
  aov(TrustScientists ~ Condition, df) #

# check outliers
df$SR_trust <- rstandard(trust_model)
which(abs(df$SR_trust) > 3) # no model outliers

Anova(trust_model, type = 3) # nothing goin on

describeBy(df$TrustScientists, df$Condition)


## ~~Explore differences on man check with pilot ----

# load data from pilot
pdf <- read.csv(here("BCR_Pilot_final.csv")) # clean data from pilot
pdf$Condition <-
  recode(pdf$Condition, BoostPlus = "Boost") # combine boost conditions in pilot data

# differences between conditions on manipulation check exp 1
prop.table(table(dfC1$Consensus_mention, dfC1$Condition), margin = 2) # 18.24%
describeBy(dfC1$countMC, dfC1$Condition) # 0.74 steps on average

# scores from pilot (n = 45)
prop.table(table(pdf$Consensus_mention1, pdf$Condition), margin = 2) # 37.93%
describeBy(pdf$countMC1, pdf$Condition) # 1.52 steps on average


# test the differences (first make new dataframe)
pdf <- rename(pdf,
              Consensus_mention = 'Consensus_mention1',
              countMC = 'countMC1') # rename pdf variable names so same as experiment

ndf <-
  rbind(select(dfC1, c(Condition, Consensus_mention, countMC)),
        select(pdf, c(Condition, Consensus_mention, countMC))) # combine relevant variables in new df
ndf$DataSource <-
  c(rep("Exp1", times = nrow(dfC1)), rep("Pilot", times = nrow(pdf))) # add value to identify source data
ndf <-
  subset(ndf, Condition == "Boost") # only take relevant participants (from boost conditions)
ndf$DataSource <-
  as.factor(ndf$DataSource) # datasource to factor for cohen.d function

chisq.test(ndf$Consensus_mention, ndf$DataSource) # boost seems more successful in pilot
prop.table(table(ndf$Consensus_mention, ndf$DataSource), margin = 2)

t.test(ndf$countMC ~ ndf$DataSource) # also sign more steps mentioned in pilot than exp
cohen.d(ndf[, c("countMC", "DataSource")], "DataSource") # get cohen's d and CIs
