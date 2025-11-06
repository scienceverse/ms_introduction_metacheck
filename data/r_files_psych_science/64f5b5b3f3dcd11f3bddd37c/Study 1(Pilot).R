####################################### Study 1 Pilot ###################################

library(tidyverse)
library(lmtest)
library(sandwich)
vc <- function(x) vcovHC(x, type = "HC4")


# Import Data
pilot <- rio::import("Study 1 - Pilot.sav")


pilot$condition <- factor(pilot$condition, levels = c(0, 1 ,2),
                          labels = c("control","mindful-attention", "mindful-gratitude"))


# Regressions 

# CN

mod1 <- lm(antiSe ~ condition, pilot)
mod2 <- lm(antiSe ~ (CNSC) * condition, pilot)

waldtest(mod2, vcov = vc) 
waldtest(mod1, mod2, vcov = vc) 

# IN
mod1a <- lm(antiSe ~ condition, pilot)
summary(mod1a)
mod2a <- lm(antiSe ~ (INs) * condition, pilot)
summary(mod2a)

waldtest(mod2a, vcov = vc) 
waldtest(mod1a, mod2a, vcov = vc) 

# ID

mod1b <- lm(antiSe ~ condition, pilot)
summary(mod1b)
mod2b <- lm(antiSe ~ (IGSAT) * condition, pilot)
summary(mod2b)

waldtest(mod2b, vcov = vc) 
waldtest(mod1a, mod2a, vcov = vc) 


# Regression tables

library(sjPlot)

labs1 = c("Collective Narcissism","Mindfulness - Attention","Mindfulness - Gratitude","Collective Narcissism X Attention", "Collective Narcissism X Gratitude")

# CN

sjPlot::tab_model(mod2,  pred.labels = c(labs1),
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

# IN

labs2 = c("Individual Narcissism","Mindfulness - Attention","Mindfulness - Gratitude","Individual Narcissism X Attention", "Individual Narcissism X Gratitude")

sjPlot::tab_model(mod2a,  pred.labels = c(labs2),
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

# ID

labs3 = c("Ingroup identification","Mindfulness - Attention","Mindfulness - Gratitude","Ingroup identification X Attention", "Ingroup identification X Gratitude")

sjPlot::tab_model(mod2b,  pred.labels = c(labs3),
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

c("Ingroup identification","Mindfulness - Attention","Mindfulness - Gratitude","Ingroup identification X Attention", "Ingroup identification X Gratitude")



trends.int <- interactions::sim_slopes(mod3, "CNSC", "condition", robust = "HC4", confint = T, digits = 3)
trends.int

trends.int <- interactions::sim_slopes(mod3, "IGSAT", "condition", robust = "HC4", confint = T, digits = 3)
trends.int


## simple slopes plot


plot <- sjPlot::plot_model(mod2, type = "pred", terms = c("CNSC","condition"), colors = c("black","blue", "red"),title = "", vcov.fun = "HC4") + labs(y = "Anti-Semitism", x = "Collective Narcissism", color = "Manipulation") + theme_classic() + theme(text = element_text(size = 12)) + coord_cartesian(ylim = c(1, 6)) + scale_colour_manual(values = c("black","blue", "red"), labels=c('Control', 'Mindful-Attention Practice', 'Mindful-Gratitude Practice')) + scale_fill_manual(values = c("black","blue", "red"))
plot 



