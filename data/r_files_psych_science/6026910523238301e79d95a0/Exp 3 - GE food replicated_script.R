##--------------------------------------------##
# Analysis Exp 3 Boosting Consensus Reasoning
# Aart van Stekelenbrug
# a.vanstekelenburg@bsi.ru.nl
##--------------------------------------------##


# Sections in code -------------------------------------------------------------------------------------
# 1. Preparation
# 2. Prepare data
# 3. Confirmatory analyses
# 4. Exploratory analyses


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
  # also for ci partial eta squared
  skimr,
  # for skim
  tidyr,
  # for tidy data
  dplyr,
  # recode etc
  irr
  # for inter rater reliability functions
)


# disable exponential notation
options(scipen = 999)


### 2. Prepare data --------------------------------------------------------------------------------

# load anonymous data (make sure the data from the OSF are in the same folder as this script)
df_clean <- read.csv(here("BCR_Exp3_clean_anon.csv"))


# count participants who do hold misperception at the start of the experiment (prior belief measure)
length(which(df_clean$Prior > 0)) # 903


# remove participants who failed the attention check
df_clean <-
  subset(df_clean, AC == 2) # 972 > 964 (removed 8 failed ACs)


# remove participants not US nationality
df_clean <-
  subset(df_clean, Nationality == "United States of America") # leaves 934 participants


# again, count participants who do hold misperception at the start of the experiment (prior belief measure)
length(which(df_clean$Prior > 0)) # 869, planned N


# check responses for anything indicating problem with data (in Excel, makes for easier data viewing)

# remove: 99, 345, 835, 836, 838, 852. obviously provided nonsense information

# remove participants who clearly provided nonsense information
df_clean <-
  df_clean[!(
    df_clean$PP == 99 |
      df_clean$PP == 345 |
      df_clean$PP == 835 |
      df_clean$PP == 836 |
      df_clean$PP == 838 |
      df_clean$PP == 852
  ),] # leaves 928 participants


# also some participants who clearly made mistake in completing postmeasure belief
# remove: 244, 269, 374, 404, 546, 731, 747, 781, 915, clearly made mistake

# remove participants who clearly made a mistake
df_clean <-
  df_clean[!(
    df_clean$PP == 244 |
      df_clean$PP == 269 |
      df_clean$PP == 374 |
      df_clean$PP == 404 |
      df_clean$PP == 546 |
      df_clean$PP == 731 |
      df_clean$PP == 747 |
      df_clean$PP == 781 |
      df_clean$PP == 915
  ),] # leaves 919 participants

# remember there are some more nonsense / mistaken responses in non misp data, should be careful with those data


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
       df_clean$ReadTime_C,
       na.rm = T)

# can remove old variables from df for tidiness
df_clean <- subset(df_clean, select = -c(ReadTime_BP, ReadTime_C))


### also for reading time science communication message (with and without cons)

df_clean$ReadTime_Mes <-
  pmin(df_clean$ReadTime_Fact,
       df_clean$ReadTime_Fact_cons,
       na.rm = T)

# remove old variables
df_clean <-
  subset(df_clean, select = -c(ReadTime_Fact, ReadTime_Fact_cons))


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
} # only participants in boost+ answered these questions, set control and consensus only to NA


### compute automatic score for manipulation check

# define correct answers:
# look for (plus or minus one letter):
# answer 1: 'consensus', 'agreement' or 'agree', 'percentage' or '%', 'how many'
# answer 2: 'source', 'organization', 'scientist', 'university', 'institution', 'who made', 'who is making', 'who is promoting', 'who is saying'
# answer 3: 'expertise' or 'experts' ('expert' is included in 'experts', not included separately because causes trouble with 'experience'), 'profession' or 'professional', 'credentials', 'field'


# join strings together
df_clean$MCComb <-
  paste(df_clean$ManCheck1,
        df_clean$ManCheck2,
        df_clean$ManCheck3,
        sep = " ")

# make consensus mention variable and set at n (no)
df_clean$Consensus_mention_auto <- rep("n", times = nrow(df_clean))

# make count variable for manipulation check and set at 0
df_clean$countMC_auto <- rep(0, times = nrow(df_clean))


# check for correct answers in combined string: answer 1
for (row in 1:nrow(df_clean)) {
  if (agrepl("consensus",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("agreement",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("agree",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("percentage",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("percent",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("how many",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T))
  {
    df_clean$Consensus_mention_auto[row] = "y"
  }
}

# also count in countMC variable
for (row in 1:nrow(df_clean)) {
  if (df_clean$Consensus_mention_auto[row] == "y")
  {
    df_clean$countMC_auto[row] = df_clean$countMC_auto[row] + 1
  }
}

# check for correct answers in combined string: answer 2
for (row in 1:nrow(df_clean)) {
  if (agrepl("source",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl(
        "organization",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ) |
      agrepl(
        "institution",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ) |
      agrepl("university",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("scientist",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("who made",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl(
        "who is making",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ) |
      agrepl(
        "who is promoting",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ) |
      agrepl("who said",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl(
        "who is saying",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ))
  {
    df_clean$countMC_auto[row] = df_clean$countMC_auto[row] + 1
  }
}

# check for correct answers in combined string: answer 3
for (row in 1:nrow(df_clean)) {
  if (agrepl("experts",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("expertise",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl("profession",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T) |
      agrepl(
        "professional",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ) |
      agrepl(
        "credentials",
        df_clean$MCComb[row],
        max.distance = 1,
        ignore.case = T
      ) |
      agrepl("field",
             df_clean$MCComb[row],
             max.distance = 1,
             ignore.case = T))
  {
    df_clean$countMC_auto[row] = df_clean$countMC_auto[row] + 1
  }
}

# set consensus_mention to factor
df_clean$Consensus_mention_auto <-
  as.factor(df_clean$Consensus_mention_auto)

# check variables
df_clean$Consensus_mention_auto
df_clean$countMC_auto


# use shorter name for dataframe and remove old ones
df <- df_clean

rm(df_clean) # keep tidy by removing old dfs


### save randoms sample of dataframe so can complete manipulation check manually in excel

# first set seed and take random sample of 100 participants
set.seed(12) # makes script reproducible
df_MCsample <- df[sample(nrow(df), 100), ]

# save data from sample so can work in excel
write.csv(df_MCsample[, c("PP", "ManCheck1", "ManCheck2", "ManCheck3")], file = "BCR_Exp3_MC.csv") # mancheck data from sample


# complete manipulation check manually. 2 new variables: Consensus_mention (consensus mentioned in any
# of the steps (yes or no) and countMC (integer with number of steps from boost mentioned)

# load manipulation check data from coder and add to df with MCsample
MC <- read.csv("BCR_Exp3_MC_Coded.csv")

# remove variables that are already in main datafile (ManCheck1, ManCheck2, ManCheck3)
MC <- subset(MC, select = -c(ManCheck1, ManCheck2, ManCheck3))

# set PP to factor
df$PP <- as.factor(df$PP)
df_MCsample$PP <- as.factor(df_MCsample$PP)
MC$PP <- as.factor(MC$PP)

# drop unused levels for df_MCsample$PP before joining datasets
df_MCsample$PP <- droplevels(df_MCsample$PP)

# join to sample df
df_MCsample <- left_join(df_MCsample, MC, by = "PP")

# and remove MC for tidiness
rm(MC)


### rename condition value 'consensus' to 'consensus only' to fit with manuscript

df$Condition <-
  recode(df$Condition, "Consensus" = "Consensus only")


### make subsets for contrasts

# boost versus consensus (hypothesis 1)
dfC1 <-
  subset(df, Condition == "BoostPlus" |
           Condition == "Consensus only") # take subset for hypothesis 1

dfC1$Condition <-
  droplevels(dfC1$Condition) # drop unused level
levels(dfC1$Condition) # check
contrasts(dfC1$Condition) # check, 0 / intercept = boost+ 1 / coefficient = consensus only, reverse for easy interpretation
dfC1$Condition <-
  relevel(dfC1$Condition, ref = "Consensus only") # set consensus only as reference level
contrasts(dfC1$Condition) # check

# Consensus only vs control (hypothesis 2)
dfC2 <-
  subset(df, Condition == "Consensus only" |
           Condition == "Control") # take subset for hypothesis 2

dfC2$Condition <-
  droplevels(dfC2$Condition) # drop unused level
levels(dfC2$Condition) # check
contrasts(dfC2$Condition) # check, 0 / intercept = consensus only, 1 / coefficient = control, reverse for easy interpretation
dfC2$Condition <-
  relevel(dfC2$Condition, ref = "Control") # set control as reference level
contrasts(dfC2$Condition) # check

# boost+ vs control (exploratory)
dfC3 <-
  subset(df, Condition == "BoostPlus" |
           Condition == "Control") # take subset for exploratory contrast

dfC3$Condition <-
  droplevels(dfC3$Condition) # drop unused level
levels(dfC3$Condition) # check
contrasts(dfC3$Condition) # check, 0 / intercept = boost+, 1 / coefficient = control, reverse for easy interpretation
dfC3$Condition <- relevel(dfC3$Condition, ref = "Control")
contrasts(dfC3$Condition) # check


# take subset of participants who already believe GE food is safe, might need it later (though careful, because provided inconsistent reponses, not most reliable participants)
df_nonMisp <-
  subset(df, Prior < 0) # 46, participants who scored 'don't know' (0) not included


# select participants who hold misperception at the start of the experiment (prior belief measure)
df_misp <-
  subset(df, Prior > 0) # 854 participants (= planned n (869) - 15 post hoc exclusions)


# make df_misp the standard df from here on out, shorter name is easier
df <- df_misp
dfC1 <- subset(dfC1, Prior > 0) # also for contrast dfs
dfC2 <- subset(dfC2, Prior > 0)
dfC3 <- subset(dfC3, Prior > 0)


### 3. Confirmatory analyses ----------------------------------------------------------------------------

## ~~Descriptives ----

# take quick look at data
skim(df)

# DVs
describeBy(df$Post, df$Condition) # substantial mean differences in expected direction
describeBy(df$Post_cons, df$Condition) # again, substantial differences in exp direction
describeBy(df$GE_Worry, df$Condition) # small differences in expected direction
describeBy(df$GE_Intention, df$Condition) # here slightly more substantial differences in exp dir
describeBy(df$GE_PolSupport, df$Condition) # small differences again


# check prior and difference score
describe(df$Prior) # similar to previous experiment
describe(df$Post) # big difference prior to posterior
describeBy(df$Prior, df$Condition) # good randomization
describeBy(df$PP_DS, df$Condition) # change in all conditions, substantial differences between conditions

describe(df$Prior_cons)
describe(df$Post_cons) # also big difference prior to posterior
describeBy(df$Prior_cons, df$Condition) # randomization might be little bit of an issue, keep in mind
describeBy(df$PPcons_DS, df$Condition) # quite some differences in exp direction, interesting that even control goes up a little


# manipulation check
prop.table(table(df$Consensus_mention_auto, df$Condition), margin = 2) # check consensus mentions
describeBy(df$countMC_auto, df$Condition) # consensus reasoning total steps menioned


# explore boost eval, practice trials and boost+ attention questions
describe(df[df$Condition == "BoostPlus",]$BEscore) # boost+ eval quite high
describeBy(df$countPT, df$Condition) # interesting: number of correct answers in practice trials again low in boost+
describe(df$countMCBP) # on average quite high score again on questions about boost+


# note: age and gender were removed from OSF data to protect participants' privacy
# data are available upon request

# participant descriptives
table(df$Gender_MC) # 562 female, 288 male, 1 would rather not say, 3 other
df$Gender_text # others were: agender, gender non-conforming, and nonbinary
describe(df$Age) # mean age = 35.9, sd = 19.92
table(df$Condition) # 275 boost+, 287 consensus only, 292 control
table(df$Pol_affil)
table(df$Pol_ideo) # more democrats/liberals in sample


# visualize main DV
ggplot(df, aes(x = Post)) +
  geom_density() # seems a bit right-skewed

# visualize difference between prior and post by condition
ggplot(df, aes(x = PP_DS, color = Condition)) +
  geom_density() # looks like some change


## ~~Manipulation check ----

# mention consensus or not?

# check inter rater reliab between coder and automatic coding for MCsample
kripp.alpha(
  rbind(
    df_MCsample$Consensus_mention_auto,
    df_MCsample$Consensus_mention
  ),
  "nominal"
) # irr is okay, not great. considering big differences between conditions this should be no problem

table(df_MCsample$Consensus_mention, df_MCsample$Condition)
table(df_MCsample$Consensus_mention_auto, df_MCsample$Condition) # in sample of 100 pps difference between autom and manual coding across conditions is small

# test
prop.table(table(df$Consensus_mention_auto, df$Condition), margin = 2) # quite some difference, similar to exp 2

chisq.test(dfC1$Consensus_mention, dfC1$Condition) # boost seems successful compared to Consensus only
chisq.test(dfC3$Consensus_mention, dfC3$Condition) # and compared to control


# number of consensus reasoning steps mentioned

# here too, check inter rater reliab between coder and automatic coding
icc(df_MCsample[, c("countMC_auto", "countMC")]) # intraclass correlation coefficient for countMC is good

describeBy(df_MCsample$countMC, df_MCsample$Condition)
describeBy(df_MCsample$countMC_auto, df_MCsample$Condition) # manual and automatic coding quite similar across conditions in sample

# and test
describeBy(df$countMC_auto, df$Condition) # substantial mean difference

t.test(dfC1$countMC_auto ~ dfC1$Condition, alternative = "less") # again, boost seems successful compared to Consensus only
cohen.d(dfC1[, c("countMC_auto", "Condition")], "Condition") # get cohen's d and CIs

t.test(dfC3$countMC_auto ~ dfC3$Condition, alternative = "less") # also compared to control
cohen.d(dfC3[, c("countMC_auto", "Condition")], "Condition") # get cohen's d and CIs


### explore boost evaluation

describe(df[df$Condition == "BoostPlus",]$BEscore) # evaulation quite high

describe(df[df$Condition == "BoostPlus",]$BE_apply)
describe(df[df$Condition == "BoostPlus",]$BE_useful) # per question


### explore reading time infographics

describeBy(df$ReadTimeInf, df$Condition) # as expected most time on boost+

# check univ outliers for boost+ and other 2 condition separately
which(abs(scale(df[df$Condition == "BoostPlus",]$ReadTimeInf)) >= 3) # 5 univariate outliers at >= 3SD

# get outlier values, calculated per condition (because infographic is different for boost+ compared to other two conditions)
df %>%
  group_by(Condition) %>% # group by condition
  mutate(outlier = scale(ReadTimeInf)) %>% # compute standardized score
  filter(outlier >= 3) %>% # filter for outliers >= 3 SD
  select(Condition, ReadTimeInf) %>% # select relevant variables to print
  ungroup() %>% # ungroup
  arrange(Condition, -ReadTimeInf) # sort by condition and by readtime descending

# get descriptives per condition without outliers
df %>%
  group_by(Condition) %>% # group by condition
  mutate(outlier = scale(ReadTimeInf)) %>% # compute standardized score
  filter(outlier < 3) %>% # filter for non-outliers
  select(Condition, ReadTimeInf) %>% # select relevant variables to print
  summarize(
    mean = mean(ReadTimeInf),
    sd = sd(ReadTimeInf),
    n = n()
  ) # summarize mean, sd and n


### explore if difference in number of correct answers in practice trials between conditions

t.test(dfC1$countPT ~ dfC1$Condition) # boost does not seem succesful for practice trials, even opposite
cohen.d(dfC1[, c("countPT", "Condition")], "Condition") # get cohen's d and CIs

t.test(dfC3$countPT ~ dfC3$Condition) # also sign difference for practicetrials between boost and control
cohen.d(dfC3[, c("countPT", "Condition")], "Condition") # get cohen's d and CIs

describeBy(df$countPT, df$Condition) # get descriptives


# check correlation with scientists, maybe anti-GE'ers do not accept boost when low trust in scientists
cor.test(df[df$Condition == "BoostPlus",]$TrustScientists, df[df$Condition == "BoostPlus",]$countMC_auto) # sign correlation with trust biomedical scientists
cor.test(df[df$Condition == "BoostPlus",]$TrustScientists, as.numeric(df[df$Condition == "BoostPlus",]$Consensus_mention)) # nonsign correlation


## ~~Assumptions check ----

# check independence IV and cov (sort of randomization check)
Anova(lm(dfC1$Prior ~ dfC1$Condition), type = 3) # no problem
Anova(lm(dfC2$Prior ~ dfC2$Condition), type = 3) # no problem


# levenes test for homogeneity of variance between groups per DV with median
leveneTest(dfC1$Post, dfC1$Condition) # problem
leveneTest(dfC2$Post, dfC2$Condition) # problem
# probably due to more updating in boost+ and Consensus only conditions, which makes for more variance
# groups sizes are quite equal though, so should not bias the ANCOVA


# univariate outliers
which(abs(scale(df$Prior)) >= 3)
which(abs(scale(df$Post)) >= 3) # no univ outliers


# also check pre-post difference score as indicator of model outliers
which(abs(scale(df$PP_DS)) >= 3) # 6 potential outliers, keep in mind for model-outliers


## ~~Setup models & model diagnostics ----

# setup models
model1 <-
  aov(Post ~ Condition + Prior, dfC1) # model for hypothesis 1
model2 <-
  aov(Post ~ Condition + Prior, dfC2) # model for hypothesis 2


# as registered check model outliers and remove >= 3 sd

# check standarized residuals model1
dfC1$standRes <- rstandard(model1)
plot(dfC1$standRes)
which(abs(dfC1$standRes) >= 3) # no model outliers

length(which(abs(dfC1$standRes) > 2)) / length(dfC1$standRes) # 4.80% stand res larger/smaller than ± 2, is fine
length(which(abs(dfC1$standRes) > 2.5)) / length(dfC1$standRes) # 1.25%, model doing good


# check standarized residuals model2
dfC2$standRes <- rstandard(model2)
plot(dfC2$standRes)
which(abs(dfC2$standRes) >= 3) # 5 outliers

dfC2[c(15, 94, 241, 308, 513),] # check, does not really seem to be anything weird going on except for substantial updating of beliefs

dfC2_NO <-
  dfC2[-c(15, 94, 241, 308, 513),] # new df without outliers

model2_NO <-
  aov(Post ~ Condition + Prior, dfC2_NO) #

dfC2_NO$standRes_NO <- rstandard(model2_NO)
plot(dfC2_NO$standRes_NO)
which(abs(dfC2_NO$standRes_NO) >= 3) # 1 new outlier

length(which(abs(dfC2_NO$standRes_NO) > 2)) / length(dfC2_NO$standRes_NO) # 5.75% stand res larger/smaller than ± 2
length(which(abs(dfC2_NO$standRes_NO) > 2.5)) / length(dfC2_NO$standRes_NO) # 2.26%, model doing good


# check model1 assumptions
plot(model1) # model looks acceptable
sqrt(vif(model1)) > 2 # no problem with multicoll
durbinWatsonTest(model1) # no problem with autocorrelation


# check model2 assumptions
plot(model2_NO) # also looks okay
sqrt(vif(model2_NO)) > 2 # no problem with multicoll
durbinWatsonTest(model2_NO) # no problem with autocorrelation


## ~~Hypothesis testing ----

### hypothesis 1

Anova(model1, type = 3) # # signficant difference between boost and Consensus only
etaSquared(model1, type = 3) # get effect size; small to medium
get.ci.partial.eta.squared(16.899, 1, 559, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

describeBy(dfC1$Post, dfC1$Condition) # get descriptives
cohen.d(dfC1[, c("PP_DS", "Condition")], "Condition") # and cohen's d and CIs

# Bayesian equivalent
bf1Full <- lmBF(Post ~ Condition + Prior, data = dfC1)
bf1NoCon <- lmBF(Post ~ Prior, data = dfC1)
BFc1 <- bf1Full / bf1NoCon
BFc1 # BF 309.98 for hypo 1, very strong evidence


### hypothesis 2

Anova(model2_NO, type = 3) # sign difference between Consensus only and control
etaSquared(model2_NO, type = 3) # get effect size, also small to medium, but smaller than h1
get.ci.partial.eta.squared(10.583, 1, 571, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

describeBy(dfC2_NO$Post, dfC2_NO$Condition) # descriptives
cohen.d(dfC2[, c("PP_DS", "Condition")], "Condition") # and cohen's d and CIs

# explore with outliers
Anova(model2, type = 3) # same
describeBy(dfC2$Post, dfC2$Condition) # slightly bigger difference with outliers

# Bayesian equivalent (without outliers)
bf2Full <- lmBF(Post ~ Condition + Prior, data = dfC2_NO)
bf2NoCon <- lmBF(Post ~ Prior, data = dfC2_NO)
BFc2 <- bf2Full / bf2NoCon
BFc2 # BF 15.45 for hypothesis 2, strong (with outliers = ~40)


## ~~Plotting ----

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


# first quick plot of prior and posterior
ggplot(mdf, aes(x = Belief, color = Time)) +
  geom_density() # appears to be a substantial difference between prior and posterior


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
  ylab("Belief in GE food worse for health") +
  geom_hline(yintercept = 0, linetype = "dashed") + # add dashed line for 0 (dont know)
  scale_color_manual(values = c("#00BA38", "#619CFF", "#FFC425")) # set colors to be similar to exps 1 and 2
plot_all # remember, this is with outliers for contrast 2


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
  )  +
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
  geom_hline(yintercept = 0, linetype = "dashed") + # add dashed line for 0 (dont know)
  scale_color_manual(values = c("#00BA38", "#619CFF", "#FFC425")) # set colors to be similar to exps 1 and 2
plot_all_allPoints # remember, includes outliers


### 4. Exploratory analyses ----

## ~~Explore remaining contrast specifically ('contrast 3'; boost+ vs. control) ----

model3 <-
  aov(Post ~ Condition + Prior, dfC3) # setup contrast model

dfC3$SR3 <- rstandard(model3)
which(abs(dfC3$SR3) >= 3) # 0 model outliers

Anova(model3, type = 3) # sign, as expected
etaSquared(model3, type = 3) # effect size for boost+ vs. control: medium to large
get.ci.partial.eta.squared(56.7213, 1, 564, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

bfC3Full <- lmBF(Post ~ Condition + Prior, data = dfC3)
bfC3NoCon <- lmBF(Post ~ Prior, data = dfC3)
BFC3 <- bfC3Full / bfC3NoCon
BFC3 # BF for difference between boost+ and control extremely high


## ~~Perceived consensus ----

# posterior perceived consensus as function of condition

PHmodel_cons <-
  aov(Post_cons ~ Condition + Prior_cons, df) #

df$standResPH_cons <- rstandard(PHmodel_cons)
which(abs(df$standResPH_cons) >= 3) # 2 model outliers
df[c(8, 165),]  # went in opposite direction with perc. cons., likely to have made a mistake

PHmodel_cons_NO <-
  aov(Post_cons ~ Condition + Prior_cons, df[-c(8, 165),])

Anova(PHmodel_cons, type = 3) # omnibus ANOVA for posthoc comparisons, highly sign
etaSquared(PHmodel_cons, type = 3) # effect size for omnibus test; large effect
get.ci.partial.eta.squared(75.079, 2, 850, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

Anova(PHmodel_cons_NO, type = 3) # same as with outliers

summary(glht(PHmodel_cons, mcp(Condition = "Tukey"))) # effect caused, as could be expected, by difference between boost+ and Consensus only vs control
summary(glht(PHmodel_cons_NO, mcp(Condition = "Tukey"))) # same

describeBy(df$Post_cons, df$Condition) # in expected direction

# get effect sizes for posthoc contrasts
model_cons_bo_cont <-
  aov(Post_cons ~ Condition + Prior_cons, dfC3) #
Anova(model_cons_bo_cont, type = 3)
etaSquared(model_cons_bo_cont)
get.ci.partial.eta.squared(139.96, 1, 564, conf.level = 0.90)

model_cons_cons_cont <-
  aov(Post_cons ~ Condition + Prior_cons, dfC2) #
Anova(model_cons_cons_cont, type = 3)
etaSquared(model_cons_cons_cont)
get.ci.partial.eta.squared(96.664, 1, 576, conf.level = 0.90)

model_cons_bo_cons <-
  aov(Post_cons ~ Condition + Prior_cons, dfC1) #
Anova(model_cons_bo_cons, type = 3)
etaSquared(model_cons_bo_cons)
get.ci.partial.eta.squared(2.9514, 1, 559, conf.level = 0.90)


### explore if prepost difference in belief in misp correlated with prepost difference in perceived consensus

# first check correlation between difference scores (and per condition)
cor.test(df[df$Condition == "BoostPlus",]$PP_DS, df[df$Condition == "BoostPlus",]$PPcons_DS) # sign neg corr 
cor.test(df[df$Condition == "Consensus only",]$PP_DS, df[df$Condition == "Consensus only",]$PPcons_DS) # sign neg corr 
cor.test(df[df$Condition == "Control",]$PP_DS, df[df$Condition == "Control",]$PPcons_DS) # also here, sign neg corr


## ~~Trust in science ----

describe(df$TrustScientists)
qplot(df$TrustScientists, binwidth = 0.5)

# effect boost on trust in biomedical scientists?

trust_model <-
  aov(TrustScientists ~ Condition, df) # setup model

# check outliers
df$SR_trust <- rstandard(trust_model)
which(abs(df$SR_trust) >= 3) # no model outliers

# results
Anova(trust_model, type = 3) # could be effect of condition on trust, check posthoc comps
etaSquared(trust_model, type = 3) # get effect size; small to medium
get.ci.partial.eta.squared(3.2165, 1, 851, conf.level = 0.90) # input: f value, df1, df2, for eta use 90% CIs!

summary(glht(trust_model, mcp(Condition = "Tukey"))) # significant difference between boost+ and control

describeBy(df$TrustScientists, df$Condition) # more trust in boost+ than other two conditions


## ~~Worry, policy support, and intention ----

# setup models
worry_model <-
  aov(GE_Worry ~ Condition + Prior, df) #
polsup_model <-
  aov(GE_PolSupport ~ Condition + Prior, df) #
intention_model <-
  aov(GE_Intention ~ Condition + Prior, df) # setup models

# check outliers
df$SR_worry <- rstandard(worry_model)
which(abs(df$SR_worry) >= 3) # 4 model outliers
df[c(130, 167, 296, 351),] # not sure if anything wrong with these scores

df$SR_polsup <- rstandard(polsup_model)
which(abs(df$SR_polsup) >= 3) # no model outliers

df$SR_intent <- rstandard(intention_model)
which(abs(df$SR_intent) >= 3) # 4 model outliers
df[c(22, 71, 90, 620),] # again, not sure

# setup models without outliers
worry_model_NO <-
  aov(GE_Worry ~ Condition + Prior, df[-c(130, 167, 296, 351),]) #
intention_model_NO <-
  aov(GE_Intention ~ Condition + Prior, df[-c(22, 71, 90, 620),]) #

# results
Anova(worry_model, type = 3) # nonsign
Anova(polsup_model, type = 3) # nonsign
Anova(intention_model, type = 3) # sign, check posthoc comps

# without outliers
Anova(worry_model_NO, type = 3) # same, nonsign
Anova(intention_model_NO, type = 3) # same, sign

# effect sizes and CIs
etaSquared(intention_model, type = 3) # effect size omn effect on intent
get.ci.partial.eta.squared(5.888, 2, 850, conf.level = 0.90) #

# post hoc comparisons
summary(glht(intention_model, mcp(Condition = "Tukey"))) # sign control vs boost+

# descriptives
describeBy(df$GE_Intention, df$Condition) # expected directions

# see if change in prior-post belief is correlated with secondary outcomes
cor.test(df$PP_DS, df$GE_Worry)
cor.test(df$PP_DS, df$GE_PolSupport)
cor.test(df$PP_DS, df$GE_Intention) # yes, strongly correlated with all secondary outcomes
