##------------------------------------------------------------##
# Analysis Pilot for Experiment 1 Boosting Consensus Reasoning
##------------------------------------------------------------##

# Note: The control condition referred to in some variables in the data/script corresponds to
# the consensus only condition in the accompanying paper. The value name is corrected in preparing
# the data.


# Sections in code -------------------------------------------------------------------------------------
# 1. Preparation
# 2. Prepare data
# 3. Descriptives and manipulation check 


### 1. Preparation ---------------------------------------------------------------------------------------

# first install package pacman, to be able to install and load subsequent packages
install.packages("pacman")

# install and load required packages
pacman::p_load(
  BayesFactor,
  # Bayesian ANCOVA etc
  psych,
  # cohen.d etc
  car,
  # ANOVA etc
  dplyr,
  # recode etc
  here,
  # for here
  irr,
  # for kripp.alpha, ICC
  here
  # for here
)

# disable exponential notation
options(scipen = 999)


### 2. Prepare data --------------------------------------------------------------------------------

# load anonymous data to be cleaned
pdf_clean <- read.csv(here("BCR_Pilot_clean_anon.csv"))


# remove participants who failed the attention check
pdf_clean <-
  subset(pdf_clean, AC == 2) # removed 0 failed ACs


# remove participants who report different nationality than US
pdf_clean <-
  subset(pdf_clean, Nationality == "United States of America") # removed 1, leaves 45 participants


# check participants who became more certain of misperception, might have misunderstood question (cutoff of 25, most influential)
pdf_clean$PP_DS <-
  pdf_clean$Post - pdf_clean$Prior # first calculate pre-post difference score per participant

which(pdf_clean$PP_DS < -25) # only one higher than 25
pdf_clean[c(14),] # check response. appears that this participant understood the question, simply changed belief


# compute difference score for consensus
pdf_clean$PPcons_DS <-
  pdf_clean$Post_cons - pdf_clean$Prior_cons 


# compute centered version prior
pdf_clean$Prior_c <- scale(pdf_clean$Prior, center = T, scale = F) # do not standardize (scale = false)


# compute OMC score (min -3 to max +3)
pdf_clean$OMCscore <-
  (
    pdf_clean$OMC1 - pdf_clean$OMC2 + pdf_clean$OMC3 - pdf_clean$OMC4 + pdf_clean$OMC5 - pdf_clean$OMC6
  ) / 6 # questions 2, 4, and 6 reversed by subtracting


# compute message evaluation score (min -3 to max +3)
pdf_clean$MEscore <-
  (
    pdf_clean$ME_fair + pdf_clean$ME_biased + pdf_clean$ME_trust + pdf_clean$ME_accur + pdf_clean$ME_wholeStory +
      pdf_clean$ME_factual + pdf_clean$ME_factOpinion
  ) / 7 # remember, lower score is more positive evaluation


# compute political orientation variable

# first recode character values to numeric values
pdf_clean$Pol_affil_n <-
  recode(
    pdf_clean$Pol_affil,
    "Strong Democrat" = 1,
    "Democrat" = 2,
    "Independent Lean Democrat" = 3,
    "Independent" = 4,
    "Independent Lean Republican" = 5,
    "Republican" = 6,
    "Strong Republican" = 7
  )
pdf_clean$Pol_ideo_n <-
  recode(
    pdf_clean$Pol_ideo,
    "Very liberal" = 1,
    "Liberal" = 2,
    "Moderate" = 3,
    "Conservative" = 4,
    "Very conservative" = 5
  )

# then standardize and set center (with indepedent and moderate as middle scores) and create composite score
pdf_clean$Pol_affil_n <-
  scale(pdf_clean$Pol_affil_n, scale = T, center = 4)
pdf_clean$Pol_ideo_n <-
  scale(pdf_clean$Pol_ideo_n, scale = T, center = 3)

# combine centered and standardized variables into political orientation
pdf_clean$Pol_orient <- pdf_clean$Pol_affil_n + pdf_clean$Pol_ideo_n # negative scores = democrat/liberal, 0 = independent/moderate, positive scores = republican/conservative


# compute variable for reading time infographic (combine boost+/boost/consensus only columns)
pdf_clean$ReadTimeInf <-
  pmin(pdf_clean$ReadTime_BP,
       pdf_clean$ReadTime_B,
       pdf_clean$ReadTime_C,
       na.rm = T)

# can remove old variables from df for tidiness
pdf_clean <-
  subset(pdf_clean, select = -c(ReadTime_B, ReadTime_BP, ReadTime_C))


### compute variables for practiceclaims combining all three conditions

# first combine variables

pdf_clean$PracticeClaim1 <-
  paste(pdf_clean$PracticeClaim1_boost,
        pdf_clean$PracticeClaim1_control,
        sep = "")

pdf_clean$PracticeClaim2 <-
  paste(pdf_clean$PracticeClaim2_boost,
        pdf_clean$PracticeClaim2_control,
        sep = "")

pdf_clean$PracticeClaim3 <-
  paste(pdf_clean$PracticeClaim3_boost,
        pdf_clean$PracticeClaim3_control,
        sep = "")

# count correct answers and save in new variable

pdf_clean$countPT <- 
  rep(0, times = nrow(pdf_clean)) # make count variable for practice trials and set at 0

for (row in 1:nrow(pdf_clean)) {
  if (pdf_clean$PracticeClaim1[row] == "No")  {
    pdf_clean$countPT[row] = pdf_clean$countPT[row] + 1
  }
  if (pdf_clean$PracticeClaim2[row] == "No")  {
    pdf_clean$countPT[row] = pdf_clean$countPT[row] + 1
  }
  if (pdf_clean$PracticeClaim3[row] == "Yes")  {
    pdf_clean$countPT[row] = pdf_clean$countPT[row] + 1
  }
} # when correct (1 = no, 2 = no, 3 = yes) add point to score

# can remove old variables from df for tidiness
pdf_clean <-
  subset(
    pdf_clean,
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

pdf_clean$countMCBP <-
  rep(0, times = nrow(pdf_clean)) # make count variable and set at 0

for (row in 1:nrow(pdf_clean)) {
  if (pdf_clean$ManCheckBP1[row] == "Vitamin supplements are good for your heart")  {
    pdf_clean$countMCBP[row] = pdf_clean$countMCBP[row] + 1
  }
  if (pdf_clean$ManCheckBP2[row] == "It starts with a question - Evidence is gathered - A consensus develops - Evaluate a claim")  {
    pdf_clean$countMCBP[row] = pdf_clean$countMCBP[row] + 1
  }
  if (pdf_clean$ManCheckBP3[row] == "The evidence is continuously discussed by scientists with the relevant expertise.")  {
    pdf_clean$countMCBP[row] = pdf_clean$countMCBP[row] + 1
  }
} # give points for correct answers

for (row in 1:nrow(pdf_clean)) {
  if (pdf_clean$Condition[row] != "BoostPlus") {
    pdf_clean$countMCBP[row] = NA
  }
} # only participants in boost+ answered these questions, set consensus only (control) and boost to NA


# use shorter name for dataframe
pdf <- pdf_clean

rm(pdf_clean) # keep tidy by removing old dfs


# save dataframe so can complete manipulation check manually in excel
write.csv(pdf[,c("PP", "ManCheck1", "ManCheck2", "ManCheck3")], file = "BCR_Pilot_MC.csv") # mancheck data


# complete manipulation check manually. 4 new variables: cons_mention (consensus mentioned in any
# of the steps (yes or no) and countMC (integer with number of steps mentioned), each from 2 coders

# load manipulation check data from coders
MC <-
  read.csv(here("BCR_Pilot_MCcombined.csv"), na.strings = c("NA", "")) # set empty values as NA

# remove variables that are already in main datafile (ManCheck1, ManCheck2, ManCheck3)
MC <- subset(MC, select = -c(ManCheck1, ManCheck2, ManCheck3))

# set PP to factor
pdf$PP <- as.factor(pdf$PP)
MC$PP <- as.factor(MC$PP)

# drop unused levels for pdf$PP before joining datasets
pdf$PP <- droplevels(pdf$PP)

# join with main dataframe
pdf <- left_join(pdf, MC, by = "PP")


### rename condition value 'control' to 'consensus only' to fit with manuscript

pdf$Condition <-
  recode(pdf$Condition, "Control" = "Consensus only")


### make subsets for contrasts ###

# boosts combined versus Consensus only (contrast 1)
pdfC1 <- pdf # make new dataframe

pdfC1$Condition <-
  recode(pdfC1$Condition, BoostPlus = "Boost") # make conditions for contrast 1
contrasts(pdfC1$Condition) # check, 0 / intercept = boost, 1 / coefficient = Consensus only, easier interpretation if reverse
pdfC1$Condition <- relevel(pdfC1$Condition, ref = "Consensus only") # set consensus only as reference level
contrasts(pdfC1$Condition) # check

# boost+ vs boost (drop Consensus only; contrast 2)
pdfC2 <-
  subset(pdf, Condition == "Boost" |
           Condition == "BoostPlus") # take subset for contrast 2

pdfC2$Condition <-
  droplevels(pdfC2$Condition) # drop unused level
levels(pdfC2$Condition) # check
contrasts(pdfC2$Condition) # check, 0 / intercept = boost, 1 / coefficient = boostplus, useful for interpretation


### 3. Descriptives and manipulation check ----------------------------------------------------------------------------

## ~~Descriptives ----

# DVs
describeBy(pdf$Post, pdf$Condition)
describeBy(pdf$Post_cons, pdf$Condition)
describeBy(pdf$CC_Worry, pdf$Condition)
describeBy(pdf$CC_Intention, pdf$Condition)
describeBy(pdf$CC_PolSupport, pdf$Condition)


# check prior and difference score
describeBy(pdf$Prior, pdf$Condition)
describeBy(pdf$PP_DS, pdf$Condition)

describeBy(pdf$Prior_cons, pdf$Condition)
describeBy(pdf$PPcons_DS, pdf$Condition)


# manipulation check
table(pdf$Consensus_mention1, pdf$Condition)
table(pdf$Consensus_mention2, pdf$Condition) # seems pretty similar (+ one empty value for a participant in boostplus condition)
describeBy(pdf$countMC1, pdf$Condition)
describeBy(pdf$countMC2, pdf$Condition)

describeBy(pdf$countPT, pdf$Condition)
describe(pdf$countMCBP)


# other variables
describeBy(pdf$ReadTimeInf, pdf$Condition)
which(abs(scale(pdf$ReadTimeInf)) > 3)
describeBy(pdf[-c(43),]$ReadTimeInf, pdf[-c(43),]$Condition)

describeBy(pdf$ReadTime_Fact, pdf$Condition)
which(abs(scale(pdf$ReadTime_Fact)) > 3)
describeBy(pdf[-c(36),]$ReadTime_Fact, pdf[-c(36),]$Condition)


# participant descriptives
table(pdf$Gender_MC) # 28 female, 15 male, 2 would rather not say
describe(pdf$Age) #  mean = 35.2, sd = 12.61
table(pdf$Condition) # 14 boost, 15 boost+, 16 consensus only


## ~~Manipulation check ----

# check inter-coder reliability
kripp.alpha(rbind(pdf$Consensus_mention1, pdf$Consensus_mention2), "nominal") # krippendorff's alpha for consensus mention = 0.94

icc(pdf[, c("countMC1", "countMC2")]) # Intraclass correlation coefficient for countMC = 0.92

# inter-coder reliability very high. will use coding from coder 1 for analyses


# mention consensus or not?

table(pdf$Consensus_mention1, pdf$Condition)

chisq.test(pdfC1$Consensus_mention1, pdfC1$Condition, simulate.p.value = T) # boost seems successful
chisq.test(pdfC2$Consensus_mention1, pdfC2$Condition, simulate.p.value = T) # explore boost vs boost+: no sign diff

prop.table(table(pdfC1$Consensus_mention1, pdfC1$Condition), margin = 2) # 0% in control vs 38% in boosts


# number of consensus reasoning steps mentioned, if consensus is mentioned

describeBy(pdf$countMC1, pdf$Condition) # quite large mean differences

t.test(pdfC1$countMC1 ~ pdfC1$Condition, alternative = "less") # boost seems successful
cohen.d(pdfC1[, c("countMC1", "Condition")], "Condition") # get cohen's d and CIs

t.test(pdfC2$countMC1 ~ pdfC2$Condition) # explore contrast 2: not significant


### explore reading time infographics

describeBy(pdf$ReadTimeInf, pdf$Condition) # weird, more time on boost than boost+
which(abs(scale(pdf$ReadTimeInf)) > 3) # 1 univariate outliers at > 3SD
pdf[43,] # spent 21 minutes on infographic, likely to have taken a break
describeBy(pdf[-43, ]$ReadTimeInf, pdf[-43, ]$Condition) # descriptives without outlier, make more sense


### explore if difference in number of correct answers in practice trials between conditions

t.test(pdfC1$countPT ~ pdfC1$Condition, alternative = "less") # boost does not seem to significantly increase accuracy for practice trials
t.test(pdfC2$countPT ~ pdfC2$Condition) # also no difference between boost conditions

describeBy(pdf$countPT, pdf$Condition) # get descriptives anyway
