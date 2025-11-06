#———————————————————————————————————————————————————————————————————————————————
# Power analyses
#———————————————————————————————————————————————————————————————————————————————
library(tidyverse)
library(paramtest)

# This function simulates data with given parameters, runs a model, and saves
# coefficient estimates, p values, and significance (p < .05). It's modified 
# from an example given in the paramtest documentation:
# https://cran.r-project.org/web/packages/paramtest/vignettes/Simulating-Power.html

lm_test_interaction <- function(simNum, N, b1a, b1b, b2, b3a, b3b, b4) {
  
  cond <-  factor(rep(1:3, N)[1:N]) # Trichotomous condition variable
  peb <- ifelse(cond == 2, 1, 0) # PEB condition dummy code
  fun <- ifelse(cond == 3, 1, 0) # Fun condition dummy code
  mod <- rnorm(N, 0, 1) # Moderator
  swbT1 <- rnorm(N, 0, 1) # SWB at baseline
  yvar <- sqrt(1 - b1a^2 - b1b^2 - b2^2 - b3a^2 - b3b^2 - b4^2) 
  ym <- b1a*peb + b1b*fun + b2*mod + b3a*peb*mod + b3b*fun*mod + b4*swbT1
  y <- rnorm(N, ym, yvar)
  model <- lm(y ~ swbT1 + peb + fun + mod + peb:mod + fun:mod)
  res <- coef(summary(model))
  res2 <- data.frame(car::Anova(lm(y ~ swbT1 + cond*mod, 
                   contrasts = list(cond=contr.sum)), 
                type=3))
  
  est_fun <- res['fun', 'Estimate']
  p_fun <- res['fun', 'Pr(>|t|)']
  sig_fun <- p_fun < .05
  
  est_peb <- res['peb', 'Estimate']
  p_peb <- res['peb', 'Pr(>|t|)']
  sig_peb <- p_peb < .05
  
  est_mod <- res['mod', 'Estimate']
  p_mod <- res['mod', 'Pr(>|t|)']
  sig_mod <- p_mod < .05
  
  est_intFun <- res['fun:mod', 'Estimate']
  p_intFun <- res['fun:mod', 'Pr(>|t|)']
  sig_intFun <- p_intFun < .05
  
  est_intPEB <- res['peb:mod', 'Estimate']
  p_intPEB <- res['peb:mod', 'Pr(>|t|)']
  sig_intPEB <- p_intPEB < .05
  
  est_swbT1 <- res['swbT1', 'Estimate']
  p_swbT1 <- res['swbT1', 'Pr(>|t|)']
  sig_swbT1 <- p_swbT1 < .05
  
  p_anovaInt <- res2["cond:mod", "Pr..F."]
  sig_anovaInt <- p_anovaInt < .05
  
  return(c(est_peb=est_peb, p_peb=p_peb, sig_peb=sig_peb, 
           est_fun=est_fun, p_fun=p_fun, sig_fun=sig_fun, 
           est_mod=est_mod, p_mod=p_mod, sig_mod=sig_mod, 
           est_intPEB=est_intPEB, p_intPEB=p_intPEB, sig_intPEB=sig_intPEB, 
           est_intFun=est_intFun, p_intFun=p_intFun, sig_intFun=sig_intFun, 
           est_swbT1=est_swbT1, p_swbT1=p_swbT1, sig_swbT1=sig_swbT1,
           p_anovaInt=p_anovaInt, sig_anovaInt=sig_anovaInt))
}

sample_sizes <- seq(from=100, to=1500, by=100)
int_effects <- seq(from=.02, to=.2, by=.02)

power_lm_int <- grid_search(lm_test_interaction, 
                            params=list(N=sample_sizes, b3a = int_effects),
                            n.iter=5000, output='data.frame', 
                            # Coefficient estimates:
                            b1a = .18, # PEB 
                            b1b = .18, # Fun 
                            b2 = .03, # Moderator
                            b3b = 0, # Fun x Moderator
                            b4 = .83, # SWB at T1
                            parallel='snow', ncpus=10)

power_results <- results(power_lm_int) %>%
  group_by(N.test, b3a.test) %>%
  summarise(peb=mean(sig_peb),
            fun=mean(sig_fun),
            mod=mean(sig_mod),
            swbT1=mean(sig_swbT1),
            intPEB=mean(sig_intPEB),
            intFun=mean(sig_intFun),
            anovaInt=mean(sig_anovaInt))

# Results for power to detect the interaction in an ANCOVA
power_results %>% 
  ggplot(aes(N.test, anovaInt, color=factor(b3a.test))) +
  geom_point() +
  geom_line() +
  geom_text(data = power_results %>% filter(N.test==400 ),
                  aes(label= gsub("0\\.", "\\.", as.character(b3a.test))), 
            nudge_x=-20, nudge_y=.02) +
  geom_vline(xintercept = 545, linetype="dotted") + 
  scale_x_continuous(breaks=seq(100,1500, by=300)) +
  scale_y_continuous(breaks=seq(0,1, by=.1)) +
  labs(x="Sample Size", y="Power", 
       color="Interaction\nEffect Size") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(size=11, color="black"),
        axis.title=element_text(size=12, color="black"))

# Results for detecting the interaction in a regression model with dummy codes
power_results %>% 
  filter(N.test <= 1500) %>% 
  ggplot(aes(N.test, intPEB, color=factor(b3a.test))) +
  geom_point() + 
  geom_line() +
  geom_vline(xintercept = 545, linetype="dotted") + 
  scale_x_continuous(breaks=seq(100,1500, by=200)) +
  scale_y_continuous(breaks=seq(0,1, by=.2)) +
  labs(x="Sample Size", y="Power", 
       color="Interaction\nEffect Size") +
  theme_bw()

# Visualize interaction effect across various possible effect sizes
real_int <- lm(scale(swb.t2) ~ scale(swb.t1) + PEB + Fun + lv + 
                               PEB:lv + Fun:lv, s2)
fake_int1 <- real_int
fake_int1$coefficients["PEB:lv"] <- .14

fake_int2 <- real_int
fake_int2$coefficients["PEB:lv"] <- .1

fake_int3 <- real_int
fake_int3$coefficients["PEB:lv"] <- .02

sim_int1 <- interact_plot(fake_int1, pred=PEB, modx=lv,
                          x.label = "",
                          y.label="Subjejctive Well-Being") +
  theme(legend.position = "none") +
  coord_cartesian(ylim=c(-.25,.25))

sim_int2 <- interact_plot(fake_int2, pred=PEB, modx=lv,
                           x.label = "Condition",
                           y.label=element_blank()) +
  theme(axis.text.y = element_blank()) + 
  theme(legend.position = "none") +
  coord_cartesian(ylim=c(-.25,.25))

sim_int3 <- interact_plot(fake_int3, pred=PEB, modx=lv,
              x.label = "",
              y.label=element_blank(),
              legend.main="Moderator") +
  theme(axis.text.y = element_blank()) + 
  coord_cartesian(ylim=c(-.25,.25))

plot_grid(sim_int1, sim_int2, sim_int3, 
          rel_widths = c(1, .8, 1.55),
          nrow=1, labels = c("A: b = .14", "B: b = .10", "C: b = .02"))

