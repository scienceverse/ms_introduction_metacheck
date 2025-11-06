############################## Study 1 Pilot - Supplemental Analyses ###############################


# load libraries and custom function for HC4 adjustment

library(tidyverse)
library(lmtest)
library(sandwich)
vc <- function(x) vcovHC(x, type = "HC4")


# import data

pilot <- read_csv('Study 1 - Pilot.csv')

# experimental conditions as categorical variable
pilot$condition <- factor(pilot$condition, 
                          levels = c(0, 1 ,2),
                          labels = c("control","mindful-attention", 
                                     "mindful-gratitude"))


## Regression Models with effects coding

# effects coding of experimental conditions
V <- matrix(data = c(-1,-1,1,0,0,1),nrow = 3, ncol = 2, byrow = T)
contrasts(pilot$condition) <- V
head(pilot$condition) 

# base model with experimental conditions
mod1 <- lm(antiSe ~ condition, pilot)

# intermediary with collective narcissism and conditions no interactions 
# (baseline to compare for interaction effects)
mod1b <- lm(antiSe ~ CNSC + condition, pilot)

# full model with collective narcissism, conditions and interactions
mod2 <- lm(antiSe ~ (CNSC) * condition, pilot)

# omnibus tests and model comparison with HC4 adjustment
waldtest(mod1, vcov = vc) 
waldtest(mod2, vcov = vc) 
waldtest(mod1, mod2, vcov = vc) 

# F change and delta Rsquare
waldtest(mod2, mod1b, vcov = vc)

effectsize::r2_semipartial(mod2)
summary(mod2)$r.squared - summary(mod1b)$r.squared

# model summaries providing HC4 adjustment and bootstrapping
library(modelsummary)

modelsummary(mod1, vcov = c('HC4', 'bootstrap'),
             estimate  = "{estimate}",
             statistic = c("conf.int",
                           "s.e. = {std.error}", 
                           "t = {statistic}",
                           "p = {p.value}"))

modelsummary(mod2, vcov = c('HC4', 'bootstrap'),
             estimate  = "{estimate}",
             statistic = c("conf.int",
                           "s.e. = {std.error}", 
                           "t = {statistic}",
                           "p = {p.value}"))



## Regression tables

library(sjPlot)

# model 1 table including experimental conditions

labs = c("Mindfulness - Attention","Mindfulness - Gratitude")

tab_model(mod1, pred.labels = labs,
          string.est = "b(SE)",
          strings = c(ci = "95%CI LL,UL", stat = "t"),
          show.se = TRUE, collapse.se = T, 
          ci.hyphen = "&comma;",
          title = "",
          show.intercept = F,
          dv.labels = c("Model 1"),
          show.est = T,
          show.stat= F,
          col.order = c("est", "se", "ci", "stat", "p"),
          emph.p = F,
          vcov.fun = "HC4"
          
)

# full model 2 table with collective narcissism and interaction effects

labs1 = c("Collective Narcissism","Mindfulness - Attention","Mindfulness - Gratitude","Collective Narcissism X Attention", "Collective Narcissism X Gratitude")

tab_model(mod2, pred.labels = labs1,
                  string.est = "b(SE)",
                  strings = c(ci = "95%CI LL,UL", stat = "t"),
                  show.se = TRUE, collapse.se = T, 
                  ci.hyphen = "&comma;",
                  title = "",
                  show.intercept = F,
                  dv.labels = c("Model 1"),
                  show.est = T,
                  show.stat= F,
                  col.order = c("est", "se", "ci", "stat", "p"),
                  emph.p = F,
                  vcov.fun = "HC4"
                  
)

# Covariate models 

# Individual Narcissism

# intermediary model with individual narcissism and conditions no interactions 
# (baseline to compare for interaction effects)
mod1b.in <- lm(antiSe ~ INs + condition, pilot)

# full model with invididual narcissism, conditions and interactions
mod2.in <- lm(antiSe ~ INs * condition, pilot)

# F change and delta Rsquare
waldtest(mod2.in, mod1b, vcov = vc)

effectsize::r2_semipartial(mod2)
summary(mod2.in)$r.squared - summary(mod1b.in)$r.squared

# model summaries providing HC4 adjustment and bootstrapping
library(modelsummary)

modelsummary(mod2.in, vcov = c('HC4', 'bootstrap'),
             estimate  = "{estimate}",
             statistic = c("conf.int",
                           "s.e. = {std.error}", 
                           "t = {statistic}",
                           "p = {p.value}"))

# regression table

labs2 = c("Individual Narcissism","Mindfulness - Attention","Mindfulness - Gratitude","Individual Narcissism X Attention", "Individual Narcissism X Gratitude")

tab_model(mod2.in,  pred.labels = labs2,
                  string.est = "b(SE)",
                  strings = c(ci = "95%CI LL,UL", stat = "t"),
                  show.se = TRUE, collapse.se = T, 
                  ci.hyphen = "&comma;",
                  title = "",
                  show.intercept = F,
                  dv.labels = c("Model 1"),
                  show.est = T,
                  show.stat= F,
                  col.order = c("est", "se", "ci", "stat", "p"),
                  emph.p = F,
                  vcov.fun = "HC4"
                  
)

# Ingroup identification

# intermediary model with individual narcissism and conditions no interactions 
# (baseline to compare for interaction effects)
mod1b.id <- lm(antiSe ~ IGSAT + condition, pilot)

# full model with invididual narcissism, conditions and interactions
mod2.id <- lm(antiSe ~ IGSAT * condition, pilot)

# F change and delta Rsquare
waldtest(mod2.id, mod1b.id, vcov = vc)

effectsize::r2_semipartial(mod2)
summary(mod2.id)$r.squared - summary(mod1b.id)$r.squared

# model summaries providing HC4 adjustment and bootstrapping
library(modelsummary)

modelsummary(mod2.id, vcov = c('HC4', 'bootstrap'),
             estimate  = "{estimate}",
             statistic = c("conf.int",
                           "s.e. = {std.error}", 
                           "t = {statistic}",
                           "p = {p.value}"))


labs3 = c("Ingroup identification","Mindfulness - Attention","Mindfulness - Gratitude","Ingroup identification X Attention", "Ingroup identification X Gratitude")

tab_model(mod2.id,  pred.labels = labs3,
                  string.est = "b(SE)",
                  strings = c(ci = "95%CI LL,UL", stat = "t"),
                  show.se = TRUE, collapse.se = T, 
                  ci.hyphen = "&comma;",
                  title = "",
                  show.intercept = F,
                  dv.labels = c("Model 1"),
                  show.est = T,
                  show.stat= F,
                  col.order = c("est", "se", "ci", "stat", "p"),
                  emph.p = F,
                  vcov.fun = "HC4"
                  
)

################## (Pre-)Pilot Study - Non-Meditative Gratitude ##################

# load libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(rstatix)
library(sjPlot)

vc <- function(x) vcovHC(x, type = "HC4")

# import and prepare data

df <- read_csv('prepilot.csv')

df <- df %>% 
  rename(filt = `filter_$`, group = expCond2) %>% 
  filter(filt == 1) %>% mutate(id = row_number(), 
                               group = factor(group, levels = c(0,1), 
                                              labels = c("control", "gratitude")))



# regression - collective narcissism with prejudice towards mexicans
mod <- lm(MexPrej~CN*group, df)


# regression table
tab_model(mod, pred.labels = c("Collective Narcissism", "Gratitude", "Gratitude X Collective Narcissism"),
          string.est = "b(SE)",
          #string.std = "β",
          strings = c(ci = "95%CI LL,UL", stat = "t"),
          show.se = TRUE, collapse.se = T, 
          ci.hyphen = "&comma;",
          title = "",
          show.intercept = F,
          dv.labels = c("Model 1"),
          #show.std = F,
          show.est = T,
          show.stat= F,
          col.order = c("est", "se", "ci", "stat", "p"#, #"std.est", "std.se"
          ),
          emph.p = F
          ,vcov.fun = "HC4"
          
)

summary(mod)
waldtest(mod, vcov = vc)

## Simple slopes 

interactions::sim_slopes(mod, "CN", "group", robust = "HC4", digits = 3, confint = T)

sjPlot::plot_model(mod, type = "pred", terms = c("CN", "group"), vcov.fun = "HC4", title = "", colors = c("blue", "red")) + labs(y = "Prejudice towards Mexican Immigrants", x = "Collective Narcissism", color = "Gratitude Conditions") +  theme_classic() + theme(text = element_text(size = 12)) + scale_colour_manual(values = c("blue", "red"), labels=c('Control', 'Gratitude')) + scale_fill_manual(values = c("blue", "red"))


## Manipulation check of the effect of gratitude exercise on emotions
# Analysis originally conducted in SPSS


df_ch <- df %>% 
  pivot_longer(cols = c(emPride:emSorr), 
               names_to = "emotions", values_to = "score") %>% 
  mutate(id = as.factor(id), emotions = as.factor(emotions))


# mixed anova
res.aov <- anova_test(
  data = df_ch, dv = score, wid = id,
  between = group, within = emotions
)
get_anova_table(res.aov)


# effect of gratitude exercise per emotion
one.way <- df_ch %>%
  group_by(emotions) %>%
  anova_test(dv = score, wid = id, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way


# Effect of time at each level of exercises group

one.way2 <- df_ch %>%
  group_by(group) %>%
  anova_test(dv = score, wid = id, within = emotions) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2



