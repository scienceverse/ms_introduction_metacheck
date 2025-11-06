library(tidyverse)
library(brms)
options(mc.cores = parallel::detectCores())
library(here)
set.seed(1988)

#########################
## STEP 1: Read in data
#########################

dat <- read_csv(here("alldata.csv"))

## Get differential MSE
dat %>% mutate(diff.error = mse.guess1-mse.mean) -> dat

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

######################################
## STEP 2: Estimate models in `brms`
######################################

## Exp. 1a
dat %>% filter(experiment %in% "ex1a") -> dat_1a

fit_1a_intercepts <- brm(diff.error~condition + 
                           (1|question_recoded) +
                           (1|id),
                         data = dat_1a,
                         ## increase iterations to facilitate convergence
                         iter = 3000,
                         ## set seed for reproducible estimates
                         seed = 1988,
                         ## need to save everything for LOO CV
                         save_pars = save_pars(all = TRUE))

fit_1a_maximal <- brm(diff.error~condition + 
                        (condition|question_recoded) +
                        (1|id),
                      data = dat_1a,
                      ## increase iterations to facilitate convergence
                      iter = 3000,
                      ## set seed for reproducible estimates
                      seed = 1988,
                      ## need to save everything for LOO CV
                      save_pars = save_pars(all = TRUE))

## Exp. 1b
dat %>% filter(experiment %in% "ex1b") -> dat_1b

fit_1b_intercepts <- brm(diff.error~condition + 
                           (1|question_recoded) +
                           (1|id),
                         data = dat_1b,
                         ## increase `adapt_delta` to offset divergent transitions
                         control = list(adapt_delta = .99),
                         ## set seed for reproducible estimates
                         seed = 1988,
                         ## need to save everything for LOO CV
                         save_pars = save_pars(all = TRUE))

fit_1b_maximal <- brm(diff.error~condition + 
                        (condition|question_recoded) +
                        (1|id),
                      data = dat_1b,
                      ## increase `adapt_delta` to offset divergent transitions
                      control = list(adapt_delta = .99),
                      ## set seed for reproducible estimates
                      seed = 1988,
                      ## need to save everything for LOO CV
                      save_pars = save_pars(all = TRUE))

## Exp. 2
dat %>% filter(experiment %in% "ex2") -> dat_2

fit_2_intercepts <- brm(diff.error~condition + 
                           (1|question_recoded) +
                           (1|id),
                         data = dat_2,
                         ## increase `adapt_delta` to offset divergent transitions
                         control = list(adapt_delta = .99),
                         ## set seed for reproducible estimates
                         seed = 1988,
                        ## need to save everything for LOO CV
                        save_pars = save_pars(all = TRUE))

fit_2_maximal <- brm(diff.error~condition + 
                        (condition|question_recoded) +
                        (1|id),
                      data = dat_2,
                      ## increase `adapt_delta` to offset divergent transitions
                      control = list(adapt_delta = .99),
                      ## set seed for reproducible estimates
                      seed = 1988,
                     ## need to save everything for LOO CV
                     save_pars = save_pars(all = TRUE))

## Exp. 3
dat %>% filter(experiment %in% "ex3") -> dat_3

fit_3_intercepts <- brm(diff.error~condition + 
                          (1|question_recoded) +
                          (1|id),
                        data = dat_3,
                        ## increase `adapt_delta` to offset divergent transitions
                        control = list(adapt_delta = .99),
                        ## set seed for reproducible estimates
                        seed = 1988,
                        ## need to save everything for LOO CV
                        save_pars = save_pars(all = TRUE))

fit_3_maximal <- brm(diff.error~condition + 
                       (condition|question_recoded) +
                       (1|id),
                     data = dat_3,
                     ## increase `adapt_delta` to offset divergent transitions
                     control = list(adapt_delta = .99),
                     ## set seed for reproducible estimates
                     seed = 1988,
                     ## need to save everything for LOO CV
                     save_pars = save_pars(all = TRUE))

## Exp. 4
dat %>% filter(experiment %in% "ex4") -> dat_4

fit_4_intercepts <- brm(diff.error~condition + 
                          (1|question_recoded) +
                          (1|id),
                        data = dat_4,
                        ## increase `adapt_delta` to offset divergent transitions
                        control = list(adapt_delta = .99),
                        ## set seed for reproducible estimates
                        seed = 1988,
                        ## need to save everything for LOO CV
                        save_pars = save_pars(all = TRUE))

fit_4_maximal <- brm(diff.error~condition + 
                       (condition|question_recoded) +
                       (1|id),
                     data = dat_4,
                     ## increase `adapt_delta` to offset divergent transitions
                     control = list(adapt_delta = .99),
                     ## set seed for reproducible estimates
                     seed = 1988,
                     ## need to save everything for LOO CV
                     save_pars = save_pars(all = TRUE))

## Exp. 2, 3, 4
dat %>% filter(experiment %in% c("ex2", "ex3", "ex4")) -> dat_234

fit_234_intercepts <- brm(diff.error~condition + 
                          (1|question_recoded) +
                          (1|id),
                        data = dat_234,
                        ## increase `adapt_delta` to offset divergent transitions
                        control = list(adapt_delta = .99),
                        ## set seed for reproducible estimates
                        seed = 1988,
                        ## need to save everything for LOO CV
                        save_pars = save_pars(all = TRUE))

fit_234_maximal <- brm(diff.error~condition + 
                       (condition|question_recoded) +
                       (1|id),
                     data = dat_234,
                     ## increase `adapt_delta` to offset divergent transitions
                     control = list(adapt_delta = .99),
                     ## set seed for reproducible estimates
                     seed = 1988,
                     ## need to save everything for LOO CV
                     save_pars = save_pars(all = TRUE))

###################################################
## STEP 3: Get estimates for Tables 1, S2, and S3
###################################################

## Exp. 1a
hypothesis(fit_1a_intercepts, 
           "conditionSelf = 0")

hypothesis(fit_1a_maximal, 
           "conditionSelf = 0")

## Exp. 1b
hypothesis(fit_1b_intercepts, 
           "conditionSelf = 0")

hypothesis(fit_1b_maximal, 
           "conditionSelf = 0")

## Exp. 2
hypothesis(fit_2_intercepts, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

hypothesis(fit_2_maximal, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

## Exp. 3
hypothesis(fit_3_intercepts, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

hypothesis(fit_3_maximal, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

## Exp. 4
hypothesis(fit_4_intercepts, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

hypothesis(fit_4_maximal, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

## Exp. 2, 3, 4
hypothesis(fit_234_intercepts, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

hypothesis(fit_234_maximal, 
           c("conditionSelf = conditionDisagree",
             "conditionDisagree = 0",
             "conditionSelf = 0"))

###############################
## STEP 4: Generate Figure S1
###############################

## Get column names for expanded data sets (i.e., sets with fits)
col_names_init <- c(colnames(dat_1a))

col_names_int <- c("re_question_int", "re_question_int_error", 
                   "re_question_int_lb", "re_question_int_ub")

col_names_max <- c("re_question_max", "re_question_max_error", 
                   "re_question_max_lb", "re_question_max_ub")

## Get item-level estimates incorporated into data sets
temp_int <- fitted(fit_1a_intercepts, re_formula = ~(1|question_recoded))
temp_max <- fitted(fit_1a_maximal, re_formula = ~(condition|question_recoded))
dat_1a <- cbind(dat_1a, temp_int, temp_max)
colnames(dat_1a) <- c(col_names_init, col_names_int, col_names_max)

temp_int <- fitted(fit_1b_intercepts, re_formula = ~(1|question_recoded))
temp_max <- fitted(fit_1b_maximal, re_formula = ~(condition|question_recoded))
dat_1b <- cbind(dat_1b, temp_int, temp_max)
colnames(dat_1b) <- c(col_names_init, col_names_int, col_names_max)

temp_int <- fitted(fit_2_intercepts, re_formula = ~(1|question_recoded))
temp_max <- fitted(fit_2_maximal, re_formula = ~(condition|question_recoded))
dat_2 <- cbind(dat_2, temp_int, temp_max)
colnames(dat_2) <- c(col_names_init, col_names_int, col_names_max)

temp_int <- fitted(fit_3_intercepts, re_formula = ~(1|question_recoded))
temp_max <- fitted(fit_3_maximal, re_formula = ~(condition|question_recoded))
dat_3 <- cbind(dat_3, temp_int, temp_max)
colnames(dat_3) <- c(col_names_init, col_names_int, col_names_max)

temp_int <- fitted(fit_4_intercepts, re_formula = ~(1|question_recoded))
temp_max <- fitted(fit_4_maximal, re_formula = ~(condition|question_recoded))
dat_4 <- cbind(dat_4, temp_int, temp_max)
colnames(dat_4) <- c(col_names_init, col_names_int, col_names_max)

dat_234$experiment <- "ex234"
temp_int <- fitted(fit_234_intercepts, re_formula = ~(1|question_recoded))
temp_max <- fitted(fit_234_maximal, re_formula = ~(condition|question_recoded))
dat_234 <- cbind(dat_234, temp_int, temp_max)
colnames(dat_234) <- c(col_names_init, col_names_int, col_names_max)

## Keep workspace clean
rm(temp_int, temp_max, col_names_init, col_names_int, col_names_max)

## Merge and aggregate data
dat_1a %>%
  bind_rows(dat_1b, dat_2, dat_3, dat_4, dat_234) %>%
  group_by(experiment, condition, question_recoded) %>%
  summarize(diff.error = mean(diff.error),
            re_question_int = re_question_int[1],
            re_question_int_lb = re_question_int_lb[1],
            re_question_int_ub = re_question_int_ub[1],
            re_question_max = re_question_max[1],
            re_question_max_lb = re_question_max_lb[1],
            re_question_max_ub = re_question_max_ub[1])-> dat_plot

## Get data into long format
dat_plot %>%
  pivot_longer(cols = c(re_question_int, re_question_max),
               names_to = "estimate",
               values_to = "value") -> temp_value

dat_plot %>%
  pivot_longer(cols = c(re_question_int_lb, re_question_max_lb),
               names_to = "model_lb",
               values_to = "lb") -> temp_lb

dat_plot %>%
  pivot_longer(cols = c(re_question_int_ub, re_question_max_ub),
               names_to = "model_ub",
               values_to = "ub") -> temp_ub

temp_value %>% 
  ungroup() %>% 
  mutate(temp_lb, temp_ub) %>%
  select(c(experiment:diff.error, estimate:value, model_lb:lb, model_ub:ub))-> dat_plot_long

rm(temp_value, temp_lb, temp_ub)

## Rename values for neater labels
dat_plot_long %>%
  mutate(experiment = case_when(experiment == "ex1b" ~ "Exp. 1b",
                                experiment == "ex2" ~ "Exp. 2",
                                experiment == "ex3" ~ "Exp. 3",
                                experiment == "ex4" ~ "Exp. 4",
                                experiment == "ex234" ~ "Exps. 2, 3, 4"),
         condition = case_when(condition == "Agree" ~ "Agr.",
                               condition == "Disagree" ~ "Dis.",
                               condition == "Self" ~ "Self"),
         question_recoded = question_recoded-10) -> dat_plot_long

## Plot data from Exps. 1b, 2, 3, 4, and {2, 3, 4}
dat_plot_long %>%
  filter(experiment != "ex1a") %>%
  ggplot(aes(x = condition, y = diff.error)) +
  geom_point(aes(color = condition,
                 fill = condition), 
             show.legend = FALSE) +
  geom_pointrange(aes(y = value, 
                      ymin = lb, 
                      ymax = ub, 
                      color = condition,
                      shape = model_lb),
                  position = position_dodge(.75), 
                  size = .25,
                  linewidth = .25,
                  alpha = .5,
                  show.legend = FALSE) +
  scale_shape_manual(values = c(3, 4)) +
  facet_wrap(~experiment + question_recoded, nrow = 5, scales = "free_y") +
  xlab("Condition") +
  ylab("Differential MSE") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = "dotted", color = "light gray"),
        panel.border = element_rect(fill = NA, color = "black"))

## Save the plot
ggsave("figure_s1.png", height = 6.5, width = 7.5, dpi = 320)

## Keep workspace clean
rm(dat_plot, dat_plot_long)

#####################################
## STEP 5: LOO-CV model comparisons
#####################################

## Table S1

## Exp. 1a
loo_1a <- loo(fit_1a_intercepts, fit_1a_maximal, 
              compare = TRUE, 
              reloo = TRUE)

## Exp. 1b
loo_1b <- loo(fit_1b_intercepts, fit_1b_maximal, 
              compare = TRUE, 
              moment_match = TRUE)

## Exp. 2
loo_2 <- loo(fit_2_intercepts, fit_2_maximal, 
              compare = TRUE, 
              reloo = TRUE)

## Exp. 3
loo_3 <- loo(fit_3_intercepts, fit_3_maximal, 
             compare = TRUE, 
             reloo = TRUE)

## Exp. 4
loo_4 <- loo(fit_4_intercepts, fit_4_maximal, 
             compare = TRUE, 
             moment_match = TRUE)

## Exp. 2, 3, 4
options(future.globals.maxSize= 891289600)
loo_234 <- loo(fit_234_intercepts, fit_234_maximal, 
             compare = TRUE, 
             reloo = TRUE)
