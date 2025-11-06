##################################### Study 1 Pilot ###################################


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


## Regression Models

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

sjPlot::tab_model(mod2, pred.labels = labs1,
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


## Simple slopes analysis of collective narcissism and conditions

trends.int <- interactions::sim_slopes(mod2, "CNSC", "condition", robust = "HC4", confint = T, digits = 3)
trends.int


# simple slopes plot


plot <- sjPlot::plot_model(mod2, type = "pred", terms = c("CNSC","condition"), colors = c("black","blue", "red"),title = "", vcov.fun = "HC4") + labs(y = "Anti-Semitism", x = "Collective Narcissism", color = "Manipulation") + theme_classic() + theme(text = element_text(size = 12)) + coord_cartesian(ylim = c(1, 6)) + scale_colour_manual(values = c("black","blue", "red"), labels=c('Control', 'Mindful-Attention Practice', 'Mindful-Gratitude Practice')) + scale_fill_manual(values = c("black","blue", "red"))
plot 
