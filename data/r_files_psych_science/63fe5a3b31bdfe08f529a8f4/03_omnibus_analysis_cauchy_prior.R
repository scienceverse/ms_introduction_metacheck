library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores())
library(here)
set.seed(1988)

#########################
## STEP 1: Read in data
#########################

dat <- read_csv(here("alldata.csv"))

## Only analyze data from `disagree` and `self` conditions
dat %>% filter(condition %in% c("Disagree", "Self")) -> dat

## Recode questions from both sets so that all are uniquely numbered
answer_set <- unique(dat$answer[dat$answer < 100])
dat %>%
  rowwise() %>%
  mutate(question_recoded = ifelse(answer > 100, question, which(answer_set %in% answer)+10)) -> dat

## Get common item numbers for experiment 4 and experiments 1b-3
dat %>% filter(experiment == "ex2") %>% group_by(question_recoded) %>% summarize(answer = answer[1]) -> key
dat %>% 
  rowwise() %>% 
  mutate(question_recoded = ifelse(answer %in% key$answer, 
                                   key$question_recoded[key$answer == answer], 
                                   question_recoded)) -> dat

## Remove extreme-value answers from ex4
dat %>% filter(question_recoded <= 16) -> dat

## Keep workspace clean
rm(key, answer_set)

#########################
## STEP 2: Reshape data
#########################

## Get data into long form
dat %>%
  select(experiment, id, condition, question_recoded, answer, guess_1, guess_2) %>%
  pivot_longer(cols = c(guess_1, guess_2),
               names_to = "guess",
               values_to = "estimate") %>%
  group_by(id, question_recoded) %>%
  mutate(pmse.avg = ((cummean(estimate)-answer)/answer)^2) %>%
  ungroup() -> dat

###################################
## STEP 3: Fit hurdle-gamma model
###################################

## Declare Cauchy priors on all population-level effects
prior_4 <- prior(cauchy(0, .5), class = "b")

## Writing model formula outside of `brm` function for readability
bf_4 <- bf(pmse.avg~condition*guess + 
             (condition*guess|question_recoded) +
             ## Estimate separate random effects for participants in different conditions
             (guess|gr(id, by = condition)) +
             (condition*guess|experiment),
           ## Note the log link
           family = hurdle_gamma(link = "log"))

## Fit the model
fit_all_4 <- brm(bf_4,
                 prior = prior_4,
                 ## Need to sample from prior to calculate Bayes factors
                 sample_prior = TRUE,
                 ## shuffle the data for parallel processing (see below)
                 data = dat[sample(nrow(dat)),],
                 ## set `adapt_delta` to .99 to offset divergent transitions
                 control = list(adapt_delta = .99),
                 ## running extra iterations to also help with divergent transitions
                 iter = 3000,
                 ## use parallel processing to speed up estimation time:
                 ## e.g., if you have 8 cores, then you can run 2 parallelized 
                 ## threads for each of the 4 MCMC chains
                 ## (if you don't have >= 8 cores, you can comment this line out)
                 threads = threading(2),
                 ## set seed to ensure reproducible estimates
                 seed = 1988)

###########################################################
## STEP 5: Get Bayes factors for population-level effects
###########################################################

## Bayes factors are indicated by `Evid.Ratio`

## prior: cauchy(0, .5)
hypothesis(fit_all_4, c("conditionSelf = 0",
                        "guessguess_2 = 0",
                        "guessguess_2 + conditionSelf:guessguess_2 = 0",
                        "conditionSelf:guessguess_2 = 0"))

