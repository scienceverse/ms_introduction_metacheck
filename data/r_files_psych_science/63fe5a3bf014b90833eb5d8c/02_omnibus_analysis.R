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

## Figure S3
## SD of average MSPE over participants is ~= .80

dat %>%
  group_by(id, guess, condition) %>%
  summarize(mean_pmse = mean(pmse.avg)) %>%
  filter(mean_pmse != 0) %>%
  mutate(log_mean_pmse = log(mean_pmse)) %>%
  group_by(condition, guess) %>%
  summarize(sd = sd(log_mean_pmse))

dat %>%
  group_by(id, guess, condition) %>%
  summarize(mean_pmse = mean(pmse.avg)) %>%
  filter(mean_pmse != 0) %>%
  mutate(log_mean_pmse = log(mean_pmse)) %>%
  ggplot(aes(x = log_mean_pmse)) + 
  geom_histogram(color = "black", fill = "gray") + 
  facet_wrap(~condition + guess, nrow = 2) +
  xlab("log(mean(MSPE))") +
  theme_classic()

ggsave("mspe_by_subject.png", height = 4, width = 4, dpi = 320)

###################################
## STEP 3: Fit hurdle-gamma model
###################################

## Declare Gaussian priors on all population-level effects
prior_1 <- prior(normal(0, 1), class = "b")

## Writing model formula outside of `brm` function for readability
bf_1 <- bf(pmse.avg~condition*guess + 
              (condition*guess|question_recoded) +
              ## Estimate separate random effects for participants in different conditions
              (guess|gr(id, by = condition)) +
              (condition*guess|experiment),
            ## Note the log link
            family = hurdle_gamma(link = "log"))

## Fit the model
fit_all_1 <- brm(bf_1,
              prior = prior_1,
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

save.image(file = "temp.RData")

################################################################
## STEP 5: Update original model with narrower and wider prior
################################################################

## Narrower prior
prior_2 <- prior(normal(0, .5), class = "b")
fit_all_2 <- update(fit_all_1, prior = prior_2)
save.image(file = "temp.RData")

## Wider prior
prior_3 <- prior(normal(0, 2), class = "b")
fit_all_3 <- update(fit_all_1, prior = prior_3)
save.image(file = "temp.RData")

###########################################################
## STEP 6: Get Bayes factors for population-level effects
###########################################################

## Bayes factors are indicated by `Evid.Ratio`

## prior: normal(0, .5)
hypothesis(fit_all_2, c("conditionSelf = 0",
                        "guessguess_2 = 0",
                        "guessguess_2 + conditionSelf:guessguess_2 = 0",
                        "conditionSelf:guessguess_2 = 0"))

## prior: normal(0, 1)
hypothesis(fit_all_1, c("conditionSelf = 0",
                        "guessguess_2 = 0",
                        "guessguess_2 + conditionSelf:guessguess_2 = 0",
                        "conditionSelf:guessguess_2 = 0"))

## prior: normal(0, 2)
hypothesis(fit_all_3, c("conditionSelf = 0",
                        "guessguess_2 = 0",
                        "guessguess_2 + conditionSelf:guessguess_2 = 0",
                        "conditionSelf:guessguess_2 = 0"))

###########################################
## STEP 7: Plot Bayes factors over priors
###########################################

sigma_arr <- c(2, 1, .5)

bf_arr <- c(hypothesis(fit_all_3, "conditionSelf:guessguess_2 = 0")$hypothesis$Evid.Ratio,
            hypothesis(fit_all_1, "conditionSelf:guessguess_2 = 0")$hypothesis$Evid.Ratio,
            hypothesis(fit_all_2, "conditionSelf:guessguess_2 = 0")$hypothesis$Evid.Ratio)

tibble(cbind(sigma_arr, bf_arr)) %>%
  ggplot(aes(x = sigma_arr, y = bf_arr)) +
  geom_point(color = "black", alpha = .5) +
  geom_line(color = "black", alpha = .5) +
  geom_hline(yintercept = 1, color = "red") +
  xlab(expression(sigma)) +
  ylab(bquote(BF["01"])) +
  theme_classic() +
  theme(panel.grid.major = element_line(linetype = "dotted", color = "light gray"),
        panel.border = element_rect(fill = NA, color = "black"))

ggsave("figure_s1.png", height = 2, width = 3, dpi = 320)
