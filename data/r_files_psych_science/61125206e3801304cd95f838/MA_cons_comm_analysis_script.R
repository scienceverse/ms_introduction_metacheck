###.###.###.###.###.###.###.###.###.###.###.###.###.###.##
###  Scientific consensus communication meta-analysis  ###
###.###.###.###.###.###.###.###.###.###.###.###.###.###.##

# Sections in code -------------------------------------------------------------------------------------
# 1. Preparation
# 2. Pool effect sizes
# 3. Compare topics
# 4. Sensitivity analyses
# 5. Reporting bias
# 6. Additional moderators


### 1. Preparation ---------------------------------------------------------------------------------------

# first install package pacman, to be able to install and load subsequent packages
install.packages("pacman")

# use pacman to install and load all required packages from CRAN
pacman::p_load("readxl",
               "here",
               "meta",
               "metafor",
               "MASS",
               "psych")

# install and load dmetar package
# is currently under development, the package is only available from GitHub

if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")

library(dmetar)


# print numeric values in fixed notation instead of exponential
options(scipen = 999)


# show here function where you are working
here::i_am("Analysis_script_OSF_20220127.R")

# load data
df <-
  read_excel(here("MA_data_OSF_20210810.xlsx"), na = "NA") # make sure the data is in the same folder as this script


# make two dfs with complete cases for each outcome
df_complete.PSC <- df[complete.cases(df$g_PSC), ] # PSC
df_complete.belief <- df[complete.cases(df$g_belief), ] # belief


### 2. Pool effect sizes ---------------------------------------------------------------------------------------

### ~PSC ----

# REML with Hartung-Knapp-Sidik-Jonkman method
pooled.effect_PSC <- metagen(
  g_PSC,
  gSE_PSC,
  df_complete.PSC,
  studlab = paste(Article, paste(": Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "REML",
  hakn = T,
  prediction = T,
  sm = "SMD"
)
pooled.effect_PSC

# check forest plot
forest(
  pooled.effect_PSC,
  sortvar = TE,
  smlab = "Consensus effect",
  leftcols = c("studlab"),
  leftlabs = c("Author(s), year, study"),
  rightlabs = c("Hedge's g", "95%-CI", "Weight"),
  col.inside = "black"
)

# redo meta-analysis with rma.uni (need this later)
pooled.effect.rma_PSC <- rma.uni(
  yi = g_PSC,
  sei = gSE_PSC,
  data = df_complete.PSC,
  slab = paste(Article, paste(": Study ", Study)),
  method = "REML",
  test = "knha"
)


### ~belief ----

# REML with Hartung-Knapp-Sidik-Jonkman method
pooled.effect_belief <- metagen(
  g_belief,
  gSE_belief,
  df_complete.belief,
  studlab = paste(Article, paste(": Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "REML",
  hakn = T,
  prediction = T,
  sm = "SMD"
)
pooled.effect_belief

# check forest plot
forest(
  pooled.effect_belief,
  sortvar = TE,
  smlab = "Consensus effect",
  leftcols = c("studlab"),
  leftlabs = c("Author(s), year, study"),
  rightlabs = c("Hedge's g", "95%-CI", "Weight"),
  col.inside = "black"
)

# redo meta-analysis with rma.uni (need this later)
pooled.effect.rma_belief <- rma.uni(
  yi = g_belief,
  sei = gSE_belief,
  data = df_complete.belief,
  slab = paste(Article, paste(": Study ", Study)),
  method = "REML",
  test = "knha"
)


### ~correlation PSC and belief ----

# take subset of complete cases
df_complete <-
  subset(df, complete.cases(df[, c("g_PSC", "g_belief")]))

# correlation
PSC.belief.cor <- cor.test(df$g_PSC, df$g_belief)
PSC.belief.cor # substantial correlation


### 3. Compare topics ---------------------------------------------------------------------------------------

# table for nr studies per topc
table(df$Topic)

# first create subsets with relevant experiments for comparison per outcome

# PSC
CC.GM_PSC <-
  subset(df_complete.PSC,
         Topic == "Climate change" |
           Topic == "GM food",
         drop = T)
CC.Vax_PSC <-
  subset(df_complete.PSC,
         Topic == "Climate change" |
           Topic == "Vaccination",
         drop = T)
GM.Vax_PSC <-
  subset(df_complete.PSC,
         Topic == "GM food" | Topic == "Vaccination",
         drop = T)

# belief
CC.GM_belief <-
  subset(df_complete.belief,
         Topic == "Climate change" |
           Topic == "GM food",
         drop = T)
CC.Vax_belief <-
  subset(df_complete.belief,
         Topic == "Climate change" |
           Topic == "Vaccination",
         drop = T)
GM.Vax_belief <-
  subset(df_complete.belief,
         Topic == "GM food" |
           Topic == "Vaccination",
         drop = T)


# PSC

# effect of intervention within topics
SA.topic_PSC <- update.meta(
  pooled.effect_PSC,
  byvar = Topic,
  comb.random = T,
  comb.fixed = F
)
SA.topic_PSC


# conduct meta-regression with only relevant subset: CC vs GM (not enough vax exps)
# have to do create a new meta-analysis object first
PE.CC_GM_PSC <- metagen(
  g_PSC,
  gSE_PSC,
  CC.GM_PSC,
  studlab = paste(Article, paste("Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "REML",
  hakn = T,
  prediction = T,
  sm = "SMD"
)
metareg(PE.CC_GM_PSC, ~ CC.GM_PSC$Topic)


# belief

# effect of intervention within topics
SA.topic_belief <- update.meta(
  pooled.effect_belief,
  byvar = Topic,
  comb.random = T,
  comb.fixed = F
)
SA.topic_belief

# here too, conduct meta-regression with only relevant subset: CC vs GM
PE.CC_GM_belief <- metagen(
  g_belief,
  gSE_belief,
  CC.GM_belief,
  studlab = paste(Article, paste("Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "REML",
  hakn = T,
  prediction = T,
  sm = "SMD"
)
metareg(PE.CC_GM_belief, ~ CC.GM_belief$Topic)


### 4. Sensitivity analyses ---------------------------------------------------------------------------------------

### ~DerSimonian-Laird estimator without Hartung-Knapp-Sidik-Jonkman method ----

# can do with same procedure as above but different estimator and setting HKSJ (hakn) to false

# PSC
SA.pooled.effect_PSC <- metagen(
  g_PSC,
  gSE_PSC,
  df_complete.PSC,
  studlab = paste(Article, paste("Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "DL",
  hakn = F,
  prediction = T,
  sm = "SMD"
)
SA.pooled.effect_PSC # check if results similar to above

# belief
SA.pooled.effect_belief <- metagen(
  g_belief,
  gSE_belief,
  df_complete.belief,
  studlab = paste(Article, paste("Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "DL",
  hakn = F,
  prediction = T,
  sm = "SMD"
)
SA.pooled.effect_belief # check if results similar to above


### ~outliers and influential cases ----

# PSC

# CI for random effect estimate, outliers identified if CI of specific effect do not overlap at all with pooled effect CI
pooled.effect_PSC$lower.random
pooled.effect_PSC$upper.random

find.outliers(pooled.effect_PSC) # 15 outliers identified, perhaps better look at extreme cases outside prediction interval

# check main analysis again to identify cases outside 95% prediction interval
pooled.effect_PSC # Cook & Lewandowsky (2016): Study 1 and Study 2, Kerr & Wilson: Study 2

# rerun main analysis without three effect sizes outside 95% prediction interval
update.meta(pooled.effect_PSC, exclude = c(5, 6, 17)) # smaller estimate


SA.inf.analysis_PSC <-
  InfluenceAnalysis(pooled.effect_PSC, random = T) # save in object

plot(SA.inf.analysis_PSC, "influence") # plots multiple influence indicators, highlights potentially influential cases in red

SA.inf.analysis_PSC # based on standardized residuals and Cook's distance, might be three influential cases: same three as identified with 95% pred int

# note that some studies used posttreatment attention check directed related to manipulation to exclude participants
# rerun main analysis without five effect sizes from Cook and Lewandowsky (2016), Kerr and Wilson (2018), and Lewandowsky et al. (2013) 
update.meta(pooled.effect_PSC, exclude = c(5, 6, 16, 17, 22)) # smaller estimate


# belief

# again, CI for random effect estimate, outliers identified if CI of specific effect do not overlap at all with pooled effect CI
pooled.effect_belief$lower.random
pooled.effect_belief$upper.random

find.outliers(pooled.effect_belief) # no outliers identified
SA.inf.analysis_belief <-
  InfluenceAnalysis(pooled.effect_belief, random = T) # save in object

plot(SA.inf.analysis_belief, "influence") # plots multiple influence indicators, highlights potentially influential cases in red

SA.inf.analysis_belief # might be influential cases: Dixon, Hmielowski, & Ma (2017): Study 1 and Lyons et al. (2021): Study 1

# here too, rerun main analysis without these cases
update.meta(pooled.effect_belief, exclude = c(14, 27))

# also rerun main analysis without five effect sizes from Cook and Lewandowsky (2016), Kerr and Wilson (2018), and Lewandowsky et al. (2013) 
update.meta(pooled.effect_belief, exclude = c(7, 8, 21, 22, 26)) # slightly smaller estimate


### ~GOSH plots ----

# PSC

# fit models
dat.gosh_PSC <-
  gosh(pooled.effect.rma_PSC) # may take a few hours depending on your processor
plot(dat.gosh_PSC, alpha = 0.1, col = "blue") # may take a minute


# belief

# fit models
dat.gosh_belief <-
  gosh(pooled.effect.rma_belief) # may take a few hours
plot(dat.gosh_belief, alpha = 0.1, col = "blue") # may take a minute


### 5. Reporting bias ---------------------------------------------------------------------------------------

# pool effect sizes again with only published or accepted articles (August 2021)

# PSC
pooled.publ_PSC <- metagen(
  g_PSC,
  gSE_PSC,
  subset(df_complete.PSC, Published.or.accepted == "y"),
  studlab = paste(Article, paste("Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "REML",
  hakn = T,
  prediction = T,
  sm = "SMD"
)
pooled.publ_PSC

# belief
pooled.publ_belief <- metagen(
  g_belief,
  gSE_belief,
  subset(df_complete.belief, Published.or.accepted == "y"),
  studlab = paste(Article, paste("Study ", Study)),
  comb.fixed = F,
  comb.random = T,
  method.tau = "REML",
  hakn = T,
  prediction = T,
  sm = "SMD"
)
pooled.publ_belief


### ~small-study bias ----

# PSC

# contour-enhanced funnel plot
funnel(
  pooled.publ_PSC,
  xlab = "Hedges' g",
  contour = c(.95, .975, .99),
  col.contour = c("gray50", "gray70", "gray90")
) +
  legend(
    1.2,
    0,
    c("p > .05", "p < .05", "p < .025", "p < .01"),
    bty = "n",
    fill = c("white", "gray50", "gray70", "gray90")
  )

# egger's test
eggers.test(pooled.publ_PSC) # test for asymmetry


# belief

# contour-enhanced funnel plot
funnel(
  pooled.publ_belief,
  xlab = "Hedges' g",
  contour = c(.95, .975, .99),
  col.contour = c("gray50", "gray70", "gray90")
) +
  legend(
    0.39,
    0,
    c("p > .05", "p < .05", "p < .025", "p < .01"),
    bty = "n",
    fill = c("white", "gray50", "gray70", "gray90")
  )

# egger's test
eggers.test(pooled.publ_belief) # test for asymmetry


### ~selection models ----

# redo pooling effect sizes with rma because need this for selection model test

# PSC
pooled.publ.rma_PSC <- rma.uni(
  yi = g_PSC,
  sei = gSE_PSC,
  data = subset(df_complete.PSC, Published.or.accepted == "y"),
  slab = paste(Article, paste("Study ", Study)),
  method = "REML",
  test = "knha"
)
summary(pooled.publ.rma_PSC)

# belief
pooled.publ.rma_belief <- rma.uni(
  yi = g_belief,
  sei = gSE_belief,
  data = subset(df_complete.belief, Published.or.accepted == "y"),
  slab = paste(Article, paste("Study ", Study)),
  method = "REML",
  test = "knha"
)
summary(pooled.publ.rma_belief)


# PSC
selmodel(pooled.publ.rma_PSC, type = "stepfun", steps = 0.025)

# belief
selmodel(pooled.publ.rma_belief, type = "stepfun", steps = 0.025)


### ~preregistration status ----

# PSC

# test effect of intervention per prereg status
SA.prereg_PSC <- update.meta(
  pooled.effect_PSC,
  byvar = Preregistered,
  comb.random = T,
  comb.fixed = F
)
SA.prereg_PSC # some difference, preregistered studies find smaller effect


# belief

# test effect of intervention per prereg status
SA.prereg_belief <- update.meta(
  pooled.effect_belief,
  byvar = Preregistered,
  comb.random = T,
  comb.fixed = F
)
SA.prereg_belief # here too, quite a difference, preregistered studies find smaller effect


### 6. Additional moderators ---------------------------------------------------------------------------------------

### ~conservatism ----

# descriptives
table(df$Location.US) # can include most experiments 
describe(df$Conservatism) # overall light skew to liberal side

# PSC

# main
metareg(pooled.effect_PSC, ~ df_complete.PSC$Conservatism)
permutest(metareg(pooled.effect_PSC, ~ df_complete.PSC$Conservatism)) # permutation test

# interaction with topic
cons.topic_PSC <- rma.uni(
  yi = g_PSC,
  sei = gSE_PSC,
  data = df_complete.PSC,
  slab = paste(Article, paste("Study ", Study)),
  method = "REML",
  mods = ~ df_complete.PSC$Topic * df_complete.PSC$Conservatism,
  test = "knha"
)
summary(cons.topic_PSC)
permutest(cons.topic_PSC)

# plot
regplot(
  metareg(pooled.effect_PSC, ~ df_complete.PSC$Conservatism),
  las = 1,
  bty = "l",
  xlab = "Conservatism",
  ylab = "Hedge's g for perceived scientific consensus"
)


# belief

# main
metareg(pooled.effect_belief, ~ df_complete.belief$Conservatism)
permutest(metareg(pooled.effect_belief, ~ df_complete.belief$Conservatism))

# interaction with topic
cons.topic_belief <- rma.uni(
  yi = g_belief,
  sei = gSE_belief,
  data = df_complete.belief,
  slab = paste(Article, paste("Study ", Study)),
  method = "REML",
  mods = ~ df_complete.belief$Topic * df_complete.belief$Conservatism,
  test = "knha"
)
summary(cons.topic_belief)
permutest(cons.topic_belief)


# plot
regplot(
  metareg(pooled.effect_belief, ~ df_complete.belief$Conservatism),
  las = 1,
  bty = "l",
  xlab = "Conservatism",
  ylab = "Hedge's g for belief in scientific facts"
)


### ~effect prior PSC on PSC ----

# descriptives
describe(df_complete.PSC$prior_PSC)
describeBy(df_complete.PSC$prior_PSC, df_complete.PSC$Topic) # some differences between topics, so note possibility of multi-collinearity

# main
metareg(pooled.effect_PSC, ~ df_complete.PSC$prior_PSC)
permutest(metareg(pooled.effect_PSC, ~ df_complete.PSC$prior_PSC))

# redo main without 5 potentially inflated effect sizes
priorPSC_mod_wo5 <- update.meta(pooled.effect_PSC, exclude = c(5, 6, 16, 17, 22)) # save object without five effects
metareg(priorPSC_mod_wo5, ~ df_complete.PSC[-c(5,6,16,17,22),]$prior_PSC) # slope largely gone

# interaction with topic
prior.topic_PSC <- rma.uni(
  yi = g_PSC,
  sei = gSE_PSC,
  data = df_complete.PSC,
  slab = paste(Article, paste("Study ", Study)),
  method = "REML",
  mods = ~ df_complete.PSC$Topic * df_complete.PSC$prior_PSC,
  test = "knha"
)
summary(prior.topic_PSC)
permutest(prior.topic_PSC)

# plot
regplot(
  metareg(pooled.effect_PSC, ~ df_complete.PSC$prior_PSC),
  las = 1,
  bty = "l",
  xlab = "Pre-exsting perception of consensus",
  ylab = "Hedge's g for perceived scientific consensus"
)


### ~effect prior belief on belief ----

# descriptives
describe(df_complete.belief$prior_belief)
describeBy(df_complete.belief$prior_belief, df_complete.belief$Topic) # here too, differences between topics, so note possibility of multi-collinearity

# main
metareg(pooled.effect_belief, ~ df_complete.belief$prior_belief)
permutest(metareg(pooled.effect_belief, ~ df_complete.belief$prior_belief))

# interaction with topic
prior.topic_belief <- rma.uni(
  yi = g_belief,
  sei = gSE_belief,
  data = df_complete.belief,
  slab = paste(Article, paste("Study ", Study)),
  method = "REML",
  mods = ~ df_complete.belief$Topic * df_complete.belief$prior_belief,
  test = "knha"
)
summary(prior.topic_belief)
permutest(prior.topic_belief)

# plot
regplot(
  metareg(pooled.effect_belief, ~ df_complete.belief$prior_belief),
  las = 1,
  bty = "l",
  xlab = "Pre-exsting belief in scientific facts",
  ylab = "Hedge's g for belief in scientific facts"
)
