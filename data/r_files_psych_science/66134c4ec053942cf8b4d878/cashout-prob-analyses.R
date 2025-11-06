# libraries
require(car)
require(here)
require(lme4)
require(lmerTest)

## load in data (experiment 1)
behav_data_exp1 <- read.csv(here("data", "exp1", "behav_data.csv"))
meta_data_exp1  <- read.csv(here("data", "exp1", "meta_data.csv"))

## screen out participants who failed questionnaire attention checks (2 excluded total)
behav_data_exp1 <- subset(behav_data_exp1, behav_data_exp1$id %in% meta_data_exp1$id[meta_data_exp1$n_incorrect_pgsi == 0])
meta_data_exp1  <- subset(meta_data_exp1, meta_data_exp1$id %in% meta_data_exp1$id[meta_data_exp1$n_incorrect_pgsi == 0])

## exclude those failing more than one attention check (in the behavioural data) (6 excluded total)
behav_data_exp1 <- subset(behav_data_exp1, behav_data_exp1$id %in% meta_data_exp1$id[meta_data_exp1$prop_att_check_correct >= 0.875])
meta_data_exp1  <- subset(meta_data_exp1, meta_data_exp1$id %in% meta_data_exp1$id[meta_data_exp1$prop_att_check_correct >= 0.875])

## remove attention-check trials
behav_data_exp1 <- subset(behav_data_exp1, behav_data_exp1$ambiguity != "none")

## load in data (experiment 2)
behav_data_exp2 <- read.csv(here::here("data", "exp2", "behav_data.csv"))
meta_data_exp2 <- read.csv(here::here("data", "exp2", "meta_data.csv"))

## screen out participants who self-reported not taking the task seriously (3 excluded total)
behav_data_exp2 <- subset(behav_data_exp2, behav_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$took_seriously == "Yes"])
meta_data_exp2  <- subset(meta_data_exp2, meta_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$took_seriously == "Yes"])

## screen out participants who self-reported technical issues (3 excluded total)
behav_data_exp2 <- subset(behav_data_exp2, !(behav_data_exp2$id %in%  c("k0dk808o30u9dftj718prto4", "nkdik3bf5t6u40iaoua9yqer", "ydkeeirspitaeefwkk9oi4wc")))
meta_data_exp2  <- subset(meta_data_exp2, !(meta_data_exp2$id %in%  c("k0dk808o30u9dftj718prto4", "nkdik3bf5t6u40iaoua9yqer", "ydkeeirspitaeefwkk9oi4wc")))

## screen out participants who failed questionnaire attention checks (5 excluded total)
behav_data_exp2 <- subset(behav_data_exp2, behav_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$n_incorrect_pgsi == 0])
meta_data_exp2  <- subset(meta_data_exp2, meta_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$n_incorrect_pgsi == 0])

## screen out 1 participant who self-reported residing in Italy
behav_data_exp2 <- subset(behav_data_exp2, behav_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$country != "Italy"])
meta_data_exp2  <- subset(meta_data_exp2, meta_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$country != "Italy"])

## set up factor structure and contrasts for ambiguity variable
behav_data_exp1$ambiguity <- factor(behav_data_exp1$ambiguity)
behav_data_exp2$ambiguity <- factor(behav_data_exp2$ambiguity)
contrasts(behav_data_exp1$ambiguity) <- contr.sum(2)
contrasts(behav_data_exp2$ambiguity) <- contr.sum(2)

## rename pr(win) variable
behav_data_exp1$pr_win <- behav_data_exp1$prediction_appraisal
behav_data_exp2$pr_win <- behav_data_exp2$prediction_appraisal

## create quadratic pr(win) variable
behav_data_exp1$pr_win_quadratic <- (behav_data_exp1$pr_win - 50) ^ 2
behav_data_exp2$pr_win_quadratic <- (behav_data_exp2$pr_win - 50) ^ 2

## add PGSI scores to behavioural dataframe
behav_data_exp1$pgsi <- meta_data_exp1$pgsi[match(behav_data_exp1$id, meta_data_exp1$id)]
behav_data_exp2$pgsi <- meta_data_exp2$pgsi[match(behav_data_exp2$id, meta_data_exp2$id)]

#####################
# EXPERIMENT 1
#####################

## primary analysis of experiment 1 data
regression_model_exp1 <- glmer(
    data    = subset(behav_data_exp1, behav_data_exp1$cashout_availability == "available"),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(pr_win) +                                 # linear effect of pr(win) (z-scored)
        scale(pr_win_quadratic) +                       # quadratic effect of pr(win) (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(pr_win) +                       # interaction between ambiguity and pr(win)
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(pr_win) +             # interaction between cash-out offer value and pr(win)
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pr_win):scale(prompt_time) +              # interaction between pr(win) and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity    
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(pr_win) +                     # interaction between pgsi and pr(win)
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 + scale(cashout_cond) + scale(pr_win) + scale(pr_win_quadratic) | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

print(car::Anova(regression_model_exp1, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                             7.7951  1   0.005239 ** 
# ambiguity                               0.1815  1   0.670054    
# scale(cashout_cond)                    26.5401  1  2.581e-07 *** <- significant effect of cash-out offer value
# scale(pr_win)                           0.1642  1   0.685279    
# scale(pr_win_quadratic)                 5.5361  1   0.018628 *   <- significant quadratic effect of Pr(win)
# scale(prompt_time)                      0.7600  1   0.383329    
# scale(pgsi)                             2.3903  1   0.122087    
# ambiguity:scale(cashout_cond)           2.4636  1   0.116513    
# ambiguity:scale(pr_win)                24.9041  1  6.025e-07 *** <- significant interaction between ambiguity and Pr(win)
# ambiguity:scale(prompt_time)            0.6168  1   0.432256    
# scale(cashout_cond):scale(pr_win)       6.2182  1   0.012645 *   <- significant interaction between cashout-out offer value and Pr(win)
# scale(cashout_cond):scale(prompt_time)  2.1183  1   0.145545    
# scale(pr_win):scale(prompt_time)        0.3650  1   0.545769    
# ambiguity:scale(pgsi)                   1.4527  1   0.228094    
# scale(cashout_cond):scale(pgsi)         3.0934  1   0.078609 .
# scale(pr_win):scale(pgsi)               0.1844  1   0.667577    
# scale(prompt_time):scale(pgsi)          0.0559  1   0.813139    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp1))

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: binomial  ( logit )
# Formula: cashout_choice ~ ambiguity + scale(cashout_cond) + scale(pr_win) +      scale(pr_win_quadratic) + scale(prompt_time) + scale(pgsi) +  
#     ambiguity:scale(cashout_cond) + ambiguity:scale(pr_win) +      ambiguity:scale(prompt_time) + scale(cashout_cond):scale(pr_win) +  
#     scale(cashout_cond):scale(prompt_time) + scale(pr_win):scale(prompt_time) +      scale(pgsi):ambiguity + scale(pgsi):scale(cashout_cond) +  
#     scale(pgsi):scale(pr_win) + scale(pgsi):scale(prompt_time) +      (1 + scale(cashout_cond) + scale(pr_win) + scale(pr_win_quadratic) |          id)
#    Data: subset(behav_data_exp1, behav_data_exp1$cashout_availability ==      "available")
# Control: glmerControl(optimizer = "bobyqa")
# 
#      AIC      BIC   logLik deviance df.resid 
#   1893.9   2043.3   -919.9   1839.9     1845 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.9318 -0.4949 -0.1457  0.5043  4.0079 
# 
# Random effects:
#  Groups Name                    Variance Std.Dev. Corr             
#  id     (Intercept)             2.2217   1.4905                    
#         scale(cashout_cond)     0.1494   0.3865   -0.44            
#         scale(pr_win)           3.1783   1.7828   -0.04  0.09      
#         scale(pr_win_quadratic) 0.3311   0.5754   -0.26 -0.08 -0.55
# Number of obs: 1872, groups:  id, 52
# 
# Fixed effects:
#                                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.61781    0.22128  -2.792  0.00524 ** 
# ambiguity1                             -0.02692    0.06318  -0.426  0.67005    
# scale(cashout_cond)                     0.45163    0.08767   5.152 2.58e-07 ***
# scale(pr_win)                          -0.10543    0.26014  -0.405  0.68528    
# scale(pr_win_quadratic)                 0.25520    0.10846   2.353  0.01863 *  
# scale(prompt_time)                     -0.05483    0.06289  -0.872  0.38333    
# scale(pgsi)                            -0.35025    0.22654  -1.546  0.12209    
# ambiguity1:scale(cashout_cond)          0.09828    0.06261   1.570  0.11651    
# ambiguity1:scale(pr_win)                0.32731    0.06559   4.990 6.03e-07 ***
# ambiguity1:scale(prompt_time)           0.04896    0.06234   0.785  0.43226    
# scale(cashout_cond):scale(pr_win)       0.16808    0.06740   2.494  0.01264 *  
# scale(cashout_cond):scale(prompt_time) -0.09175    0.06304  -1.455  0.14555    
# scale(pr_win):scale(prompt_time)       -0.03941    0.06524  -0.604  0.54577    
# ambiguity1:scale(pgsi)                  0.08235    0.06832   1.205  0.22809    
# scale(cashout_cond):scale(pgsi)        -0.16089    0.09148  -1.759  0.07861 .  
# scale(pr_win):scale(pgsi)               0.10658    0.24817   0.429  0.66758    
# scale(prompt_time):scale(pgsi)          0.01643    0.06949   0.236  0.81314    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## post-hoc tests to probe interactions between ambiguity and Pr(win)
regression_model_exp1_lowambiguity <- glmer(
    data    = subset(behav_data_exp1, behav_data_exp1$cashout_availability == "available" & behav_data_exp1$ambiguity == "low"),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(pr_win) +                                 # linear effect of pr(win) (z-scored)
        scale(pr_win_quadratic) +                       # quadratic effect of pr(win) (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        scale(cashout_cond):scale(pr_win) +             # interaction between cash-out offer value and pr(win)
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pr_win):scale(prompt_time) +              # interaction between pr(win) and prompt time
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(pr_win) +                     # interaction between pgsi and pr(win)
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 + scale(cashout_cond) + scale(pr_win) + scale(pr_win_quadratic) || id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

regression_model_exp1_highambiguity <- glmer(
    data    = subset(behav_data_exp1, behav_data_exp1$cashout_availability == "available" & behav_data_exp1$ambiguity == "high"),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(pr_win) +                                 # linear effect of pr(win) (z-scored)
        scale(pr_win_quadratic) +                       # quadratic effect of pr(win) (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        scale(cashout_cond):scale(pr_win) +             # interaction between cash-out offer value and pr(win)
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pr_win):scale(prompt_time) +              # interaction between pr(win) and prompt time
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(pr_win) +                     # interaction between pgsi and pr(win)
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 + scale(cashout_cond) + scale(pr_win) + scale(pr_win_quadratic) || id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

print(summary(regression_model_exp1_lowambiguity))

# Fixed effects:
#                                          Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.5244294  0.2211990  -2.371 0.017747 *  
# scale(cashout_cond)                     0.3522679  0.1008587   3.493 0.000478 ***
# scale(pr_win)                          -0.5349500  0.3129883  -1.709 0.087420 .  
# scale(pr_win_quadratic)                 0.2154269  0.1063658   2.025 0.042832 *  
# scale(prompt_time)                     -0.0936245  0.0928991  -1.008 0.313546    
# scale(pgsi)                            -0.4704644  0.2326816  -2.022 0.043184 *  
# scale(cashout_cond):scale(pr_win)       0.0823545  0.1013116   0.813 0.416285    
# scale(cashout_cond):scale(prompt_time) -0.1145649  0.0934732  -1.226 0.220332    
# scale(pr_win):scale(prompt_time)       -0.0009222  0.1004759  -0.009 0.992677    
# scale(cashout_cond):scale(pgsi)        -0.1550322  0.1080963  -1.434 0.151514    
# scale(pr_win):scale(pgsi)               0.0124970  0.3135760   0.040 0.968210    
# scale(prompt_time):scale(pgsi)         -0.0140687  0.1074559  -0.131 0.895834    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp1_highambiguity))

# Fixed effects:
#                                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.62697    0.24425  -2.567  0.01026 *  
# scale(cashout_cond)                     0.54515    0.12073   4.515 6.32e-06 ***
# scale(pr_win)                           0.17355    0.26027   0.667  0.50491    
# scale(pr_win_quadratic)                 0.17846    0.12077   1.478  0.13948    
# scale(prompt_time)                     -0.04310    0.09344  -0.461  0.64459    
# scale(pgsi)                            -0.31633    0.24917  -1.270  0.20425    
# scale(cashout_cond):scale(pr_win)       0.28992    0.10250   2.828  0.00468 ** 
# scale(cashout_cond):scale(prompt_time) -0.06743    0.09417  -0.716  0.47400    
# scale(pr_win):scale(prompt_time)       -0.06961    0.09843  -0.707  0.47943    
# scale(cashout_cond):scale(pgsi)        -0.25577    0.12832  -1.993  0.04624 *  
# scale(pr_win):scale(pgsi)              -0.08176    0.26400  -0.310  0.75680    
# scale(prompt_time):scale(pgsi)          0.05147    0.09999   0.515  0.60674    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## post-hoc tests to probe interactions between Pr(win) and cash-out offer value
regression_model_exp1_prwin70 <- glmer(
    data    = subset(behav_data_exp1, behav_data_exp1$cashout_availability == "available" & behav_data_exp1$pr_win == 70),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

regression_model_exp1_prwin50 <- glmer(
    data    = subset(behav_data_exp1, behav_data_exp1$cashout_availability == "available" & behav_data_exp1$pr_win == 50),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

regression_model_exp1_prwin30 <- glmer(
    data    = subset(behav_data_exp1, behav_data_exp1$cashout_availability == "available" & behav_data_exp1$pr_win == 30),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

print(car::Anova(regression_model_exp1_prwin70, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                             1.9406  1   0.163603    
# ambiguity                               8.4532  1   0.003644 ** 
# scale(cashout_cond)                    22.3264  1    2.3e-06 ***
# scale(prompt_time)                      1.2533  1   0.262921    
# scale(pgsi)                             0.9045  1   0.341587    
# ambiguity:scale(cashout_cond)           5.8952  1   0.015182 *  
# ambiguity:scale(prompt_time)            0.1238  1   0.724902    
# scale(cashout_cond):scale(prompt_time)  2.4430  1   0.118052    
# ambiguity:scale(pgsi)                   0.4116  1   0.521171    
# scale(cashout_cond):scale(pgsi)         1.3980  1   0.237058    
# scale(prompt_time):scale(pgsi)          0.1331  1   0.715195    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp1_prwin70))

# Fixed effects:
#                                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.48918    0.35116  -1.393  0.16360    
# ambiguity1                              0.32374    0.11135   2.907  0.00364 ** 
# scale(cashout_cond)                     0.53980    0.11424   4.725  2.3e-06 ***
# scale(prompt_time)                     -0.12534    0.11196  -1.120  0.26292    
# scale(pgsi)                            -0.33454    0.35176  -0.951  0.34159    
# ambiguity1:scale(cashout_cond)          0.27073    0.11150   2.428  0.01518 *  
# ambiguity1:scale(prompt_time)          -0.03902    0.11088  -0.352  0.72490    
# scale(cashout_cond):scale(prompt_time) -0.17655    0.11296  -1.563  0.11805    
# ambiguity1:scale(pgsi)                 -0.07499    0.11689  -0.642  0.52117    
# scale(cashout_cond):scale(pgsi)        -0.13982    0.11826  -1.182  0.23706    
# scale(prompt_time):scale(pgsi)          0.04452    0.12202   0.365  0.71519    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(car::Anova(regression_model_exp1_prwin50, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                            10.7249  1   0.001057 ** 
# ambiguity                               0.6012  1   0.438131    
# scale(cashout_cond)                    26.7425  1  2.325e-07 ***
# scale(prompt_time)                      0.1081  1   0.742277    
# scale(pgsi)                             3.4135  1   0.064666 .  
# ambiguity:scale(cashout_cond)           0.0046  1   0.945786    
# ambiguity:scale(prompt_time)            0.9242  1   0.336374    
# scale(cashout_cond):scale(prompt_time)  0.4731  1   0.491555    
# ambiguity:scale(pgsi)                   2.7001  1   0.100344    
# scale(cashout_cond):scale(pgsi)         5.5122  1   0.018884 *  
# scale(prompt_time):scale(pgsi)          2.2250  1   0.135791    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp1_prwin50))

# Fixed effects:
#                                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.961973   0.293742  -3.275  0.00106 ** 
# ambiguity1                              0.083650   0.107886   0.775  0.43813    
# scale(cashout_cond)                     0.574142   0.111024   5.171 2.32e-07 ***
# scale(prompt_time)                     -0.035440   0.107772  -0.329  0.74228    
# scale(pgsi)                            -0.592404   0.320641  -1.848  0.06467 .  
# ambiguity1:scale(cashout_cond)         -0.007209   0.106014  -0.068  0.94579    
# ambiguity1:scale(prompt_time)           0.101919   0.106016   0.961  0.33637    
# scale(cashout_cond):scale(prompt_time) -0.073051   0.106203  -0.688  0.49155    
# ambiguity1:scale(pgsi)                  0.220122   0.133960   1.643  0.10034    
# scale(cashout_cond):scale(pgsi)        -0.307700   0.131058  -2.348  0.01888 *  
# scale(prompt_time):scale(pgsi)         -0.197415   0.132347  -1.492  0.13579    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(car::Anova(regression_model_exp1_prwin30, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                             0.5883  1     0.4431    
# ambiguity                              15.2216  1   9.56e-05 ***
# scale(cashout_cond)                     1.0106  1     0.3148    
# scale(prompt_time)                      0.1245  1     0.7242    
# scale(pgsi)                             0.7974  1     0.3719    
# ambiguity:scale(cashout_cond)           0.0489  1     0.8249    
# ambiguity:scale(prompt_time)            0.2869  1     0.5922    
# scale(cashout_cond):scale(prompt_time)  0.4656  1     0.4950    
# ambiguity:scale(pgsi)                   1.3687  1     0.2420    
# scale(cashout_cond):scale(pgsi)         0.3837  1     0.5357    
# scale(prompt_time):scale(pgsi)          2.9106  1     0.0880 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp1_prwin30))

# Fixed effects:
#                                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.29382    0.38308  -0.767    0.443    
# ambiguity1                             -0.43662    0.11191  -3.901 9.56e-05 ***
# scale(cashout_cond)                     0.10920    0.10862   1.005    0.315    
# scale(prompt_time)                     -0.03906    0.11071  -0.353    0.724    
# scale(pgsi)                            -0.35301    0.39530  -0.893    0.372    
# ambiguity1:scale(cashout_cond)          0.02407    0.10881   0.221    0.825    
# ambiguity1:scale(prompt_time)           0.05837    0.10898   0.536    0.592    
# scale(cashout_cond):scale(prompt_time) -0.07563    0.11083  -0.682    0.495    
# ambiguity1:scale(pgsi)                  0.13668    0.11683   1.170    0.242    
# scale(cashout_cond):scale(pgsi)        -0.07206    0.11634  -0.619    0.536    
# scale(prompt_time):scale(pgsi)          0.21553    0.12633   1.706    0.088 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## calculate mean cashout prob across participants and conditions
cashout_prob_30_exp1   <- as.vector(by(behav_data_exp1$cashout_choice[behav_data_exp1$prediction_appraisal==30], INDICES=behav_data_exp1$id[behav_data_exp1$prediction_appraisal==30], FUN=mean, na.rm=T))
cashout_prob_50_exp1   <- as.vector(by(behav_data_exp1$cashout_choice[behav_data_exp1$prediction_appraisal==50], INDICES=behav_data_exp1$id[behav_data_exp1$prediction_appraisal==50], FUN=mean, na.rm=T))
cashout_prob_70_exp1   <- as.vector(by(behav_data_exp1$cashout_choice[behav_data_exp1$prediction_appraisal==70], INDICES=behav_data_exp1$id[behav_data_exp1$prediction_appraisal==70], FUN=mean, na.rm=T))
cashout_prob_data_exp1 <- data.frame(cashout_prob_30_exp1, cashout_prob_50_exp1, cashout_prob_70_exp1)

## correlations between cashout conds
corr_mat <- psych::corr.test(x = cashout_prob_data_exp1, method="spearman")
print(corr_mat$r, 2)

#                      cashout_prob_30_exp1 cashout_prob_50_exp1 cashout_prob_70_exp1
# cashout_prob_30_exp1                 1.00                 0.16                -0.33
# cashout_prob_50_exp1                 0.16                 1.00                 0.52
# cashout_prob_70_exp1                -0.33                 0.52                 1.00

print(round(corr_mat$p, 4),2)

#                      cashout_prob_30_exp1 cashout_prob_50_exp1 cashout_prob_70_exp1
# cashout_prob_30_exp1                0.000               0.2681               0.0336
# cashout_prob_50_exp1                0.268               0.0000               0.0002
# cashout_prob_70_exp1                0.017               0.0001               0.0000

#####################
# EXPERIMENT 2
#####################

## primary analysis of experiment 1 data
regression_model_exp2 <- glmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available"),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(pr_win) +                                 # linear effect of pr(win) (z-scored)
        scale(pr_win_quadratic) +                       # quadratic effect of pr(win) (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(pr_win) +                       # interaction between ambiguity and pr(win)
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(pr_win) +             # interaction between cash-out offer value and pr(win)
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pr_win):scale(prompt_time) +              # interaction between pr(win) and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity    
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(pr_win) +                     # interaction between pgsi and pr(win)
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 + scale(cashout_cond) + scale(pr_win) + scale(pr_win_quadratic) | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

print(car::Anova(regression_model_exp2, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                           Chisq Df Pr(>Chisq)    
# (Intercept)                            114.9403  1  < 2.2e-16 ***
# ambiguity                                0.5077  1  0.4761434    
# scale(cashout_cond)                     14.1645  1  0.0001675 *** <- significant effect of cash-out offer value
# scale(pr_win)                            4.4209  1  0.0355014 *   <- significant linear effect of pr(win)
# scale(pr_win_quadratic)                 29.9652  1  4.399e-08 *** <- significant quadratic effect of pr(win)
# scale(prompt_time)                       2.1060  1  0.1467200    
# scale(pgsi)                              0.0295  1  0.8636738    
# ambiguity:scale(cashout_cond)            2.8265  1  0.0927231 .  
# ambiguity:scale(pr_win)                 34.2242  1  4.911e-09 *** <- significant interaction between ambiguity and Pr(win)
# ambiguity:scale(prompt_time)             2.0937  1  0.1479101    
# scale(cashout_cond):scale(pr_win)       11.7981  1  0.0005929 *** <- significant interaction between cashout-out offer value and Pr(win)
# scale(cashout_cond):scale(prompt_time)   0.0189  1  0.8906376    
# scale(pr_win):scale(prompt_time)         3.9589  1  0.0466243 *   <- significant interaction between Pr(win) and prompt time
# ambiguity:scale(pgsi)                    0.4507  1  0.5020161    
# scale(cashout_cond):scale(pgsi)          8.3872  1  0.0037787 **  <- significant interaction between cash-out offer value and PGSI
# scale(pr_win):scale(pgsi)                3.6409  1  0.0563759 .  
# scale(prompt_time):scale(pgsi)           2.8186  1  0.0931776 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp2))

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#  Family: binomial  ( logit )
# Formula: cashout_choice ~ ambiguity + scale(cashout_cond) + scale(pr_win) +      scale(pr_win_quadratic) + scale(prompt_time) + scale(pgsi) +  
#     ambiguity:scale(cashout_cond) + ambiguity:scale(pr_win) +      ambiguity:scale(prompt_time) + scale(cashout_cond):scale(pr_win) +  
#     scale(cashout_cond):scale(prompt_time) + scale(pr_win):scale(prompt_time) +      scale(pgsi):ambiguity + scale(pgsi):scale(cashout_cond) +  
#     scale(pgsi):scale(pr_win) + scale(pgsi):scale(prompt_time) +      (1 + scale(cashout_cond) + scale(pr_win) + scale(pr_win_quadratic) |          id)
#    Data: subset(behav_data_exp2, behav_data_exp2$cashout_availability ==      "available")
# Control: glmerControl(optimizer = "bobyqa")
# 
#      AIC      BIC   logLik deviance df.resid 
#   4819.4   4999.2  -2382.7   4765.4     5733 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.2008 -0.4224 -0.1867  0.3457  7.8230 
# 
# Random effects:
#  Groups Name                    Variance Std.Dev. Corr             
#  id     (Intercept)             1.668690 1.29178                   
#         scale(cashout_cond)     0.005026 0.07089   0.71            
#         scale(pr_win)           2.478500 1.57433  -0.06  0.62      
#         scale(pr_win_quadratic) 0.350042 0.59164  -0.63 -0.12  0.28
# Number of obs: 5760, groups:  id, 80
# 
# Fixed effects:
#                                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.675755   0.156306 -10.721  < 2e-16 ***
# ambiguity1                              0.027223   0.038206   0.713 0.476143    
# scale(cashout_cond)                     0.160292   0.042590   3.764 0.000168 ***
# scale(pr_win)                          -0.387865   0.184470  -2.103 0.035501 *  
# scale(pr_win_quadratic)                 0.502127   0.091729   5.474 4.40e-08 ***
# scale(prompt_time)                     -0.054990   0.037893  -1.451 0.146720    
# scale(pgsi)                            -0.023459   0.136630  -0.172 0.863674    
# ambiguity1:scale(cashout_cond)          0.063790   0.037943   1.681 0.092723 .  
# ambiguity1:scale(pr_win)                0.220059   0.037616   5.850 4.91e-09 ***
# ambiguity1:scale(prompt_time)           0.054613   0.037744   1.447 0.147910    
# scale(cashout_cond):scale(pr_win)       0.134725   0.039223   3.435 0.000593 ***
# scale(cashout_cond):scale(prompt_time) -0.005188   0.037734  -0.137 0.890638    
# scale(pr_win):scale(prompt_time)        0.074075   0.037229   1.990 0.046624 *  
# ambiguity1:scale(pgsi)                  0.025269   0.037641   0.671 0.502016    
# scale(cashout_cond):scale(pgsi)        -0.112896   0.038982  -2.896 0.003779 ** 
# scale(pr_win):scale(pgsi)              -0.340743   0.178576  -1.908 0.056376 .  
# scale(prompt_time):scale(pgsi)          0.062162   0.037026   1.679 0.093178 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


## post-hoc tests to probe interactions between ambiguity and Pr(win)
regression_model_exp2_lowambiguity <- glmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available" & behav_data_exp2$ambiguity == "low"),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(pr_win) +                                 # linear effect of pr(win) (z-scored)
        scale(pr_win_quadratic) +                       # quadratic effect of pr(win) (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        scale(cashout_cond):scale(pr_win) +             # interaction between cash-out offer value and pr(win)
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pr_win):scale(prompt_time) +              # interaction between pr(win) and prompt time
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(pr_win) +                     # interaction between pgsi and pr(win)
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 + scale(pr_win) + scale(pr_win_quadratic) || id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

regression_model_exp2_highambiguity <- glmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available" & behav_data_exp2$ambiguity == "high"),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(pr_win) +                                 # linear effect of pr(win) (z-scored)
        scale(pr_win_quadratic) +                       # quadratic effect of pr(win) (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        scale(cashout_cond):scale(pr_win) +             # interaction between cash-out offer value and pr(win)
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pr_win):scale(prompt_time) +              # interaction between pr(win) and prompt time
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(pr_win) +                     # interaction between pgsi and pr(win)
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 +  scale(pr_win) + scale(pr_win_quadratic) || id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

print(summary(regression_model_exp2_lowambiguity))

# Fixed effects:
#                                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.615535   0.162509  -9.941  < 2e-16 ***
# scale(cashout_cond)                     0.117107   0.054273   2.158 0.030949 *  
# scale(pr_win)                          -0.616243   0.196736  -3.132 0.001734 ** 
# scale(pr_win_quadratic)                 0.368876   0.076009   4.853 1.22e-06 ***
# scale(prompt_time)                     -0.117142   0.053562  -2.187 0.028740 *  
# scale(pgsi)                            -0.112323   0.163755  -0.686 0.492762    
# scale(cashout_cond):scale(pr_win)       0.186013   0.054044   3.442 0.000578 ***
# scale(cashout_cond):scale(prompt_time)  0.052717   0.053337   0.988 0.322960    
# scale(pr_win):scale(prompt_time)        0.001699   0.053183   0.032 0.974512    
# scale(cashout_cond):scale(pgsi)        -0.100672   0.055739  -1.806 0.070900 .  
# scale(pr_win):scale(pgsi)              -0.309218   0.200320  -1.544 0.122679    
# scale(prompt_time):scale(pgsi)          0.148687   0.054327   2.737 0.006202 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp2_highambiguity))

# Fixed effects:
#                                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.677313   0.166026 -10.103  < 2e-16 ***
# scale(cashout_cond)                     0.255656   0.056810   4.500 6.79e-06 ***
# scale(pr_win)                          -0.136912   0.210106  -0.652  0.51464    
# scale(pr_win_quadratic)                 0.312177   0.105310   2.964  0.00303 ** 
# scale(prompt_time)                      0.001281   0.056599   0.023  0.98194    
# scale(pgsi)                            -0.046828   0.158139  -0.296  0.76714    
# scale(cashout_cond):scale(pr_win)       0.126491   0.056792   2.227  0.02593 *  
# scale(cashout_cond):scale(prompt_time) -0.048786   0.056497  -0.864  0.38785    
# scale(pr_win):scale(prompt_time)        0.156368   0.056739   2.756  0.00585 ** 
# scale(cashout_cond):scale(pgsi)        -0.121877   0.054695  -2.228  0.02586 *  
# scale(pr_win):scale(pgsi)              -0.328940   0.209905  -1.567  0.11709    
# scale(prompt_time):scale(pgsi)         -0.016193   0.054974  -0.295  0.76834    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## post-hoc tests to probe interactions between Pr(win) and cash-out offer value and prompt time
regression_model_exp2_prwin70 <- glmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available" & behav_data_exp2$pr_win == 70),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

regression_model_exp2_prwin50 <- glmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available" & behav_data_exp2$pr_win == 50),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

regression_model_exp2_prwin30 <- glmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available" & behav_data_exp2$pr_win == 30),
    formula = 
        cashout_choice ~                                # dependent variable; 1 = cash-out offer accepted, 0 = cash-out offer rejected
        ## FIXED EFFECTS ##
        ambiguity +                                     # main effect of ambiguity    
        scale(cashout_cond) +                           # linear effect of cash-out offer value (z-scored)
        scale(prompt_time) +                            # linear effect of cash-out offer timing (z-scored)
        scale(pgsi) +                                   # participant's PGSI score
        ambiguity:scale(cashout_cond) +                 # interaction between ambiguity and cash-out offer value
        ambiguity:scale(prompt_time) +                  # interaction between ambiguity and prompt time
        scale(cashout_cond):scale(prompt_time) +        # interaction between cash-out offer value and prompt time
        scale(pgsi):ambiguity +                         # interaction between pgsi and ambiguity
        scale(pgsi):scale(cashout_cond) +               # interaction between pgsi and cash-out offer value
        scale(pgsi):scale(prompt_time) +                # interaction between pgsi and cash-out offer timing
        ## RANDOM EFFECTS ##
        (1 | id),
    family  = "binomial",
    control = glmerControl(optimizer = "bobyqa")
)

print(car::Anova(regression_model_exp2_prwin70, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                            44.1186  1  3.091e-11 ***
# ambiguity                              13.1828  1  0.0002825 ***
# scale(cashout_cond)                    25.7676  1  3.851e-07 ***
# scale(prompt_time)                      0.2284  1  0.6326813    
# scale(pgsi)                             1.6573  1  0.1979648    
# ambiguity:scale(cashout_cond)           0.0503  1  0.8224594    
# ambiguity:scale(prompt_time)            5.0568  1  0.0245288 *  
# scale(cashout_cond):scale(prompt_time)  0.2361  1  0.6270434    
# ambiguity:scale(pgsi)                   0.9878  1  0.3202923    
# scale(cashout_cond):scale(pgsi)         2.2064  1  0.1374430    
# scale(prompt_time):scale(pgsi)          5.7194  1  0.0167781 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp2_prwin70))

# Fixed effects:
#                                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -1.76997    0.26647  -6.642 3.09e-11 ***
# ambiguity1                              0.25058    0.06901   3.631 0.000283 ***
# scale(cashout_cond)                     0.35193    0.06933   5.076 3.85e-07 ***
# scale(prompt_time)                     -0.03269    0.06839  -0.478 0.632681    
# scale(pgsi)                            -0.34183    0.26552  -1.287 0.197965    
# ambiguity1:scale(cashout_cond)          0.01536    0.06843   0.224 0.822459    
# ambiguity1:scale(prompt_time)           0.15247    0.06780   2.249 0.024529 *  
# scale(cashout_cond):scale(prompt_time)  0.03288    0.06766   0.486 0.627043    
# ambiguity1:scale(pgsi)                 -0.08060    0.08110  -0.994 0.320292    
# scale(cashout_cond):scale(pgsi)        -0.12026    0.08096  -1.485 0.137443    
# scale(prompt_time):scale(pgsi)          0.20633    0.08627   2.392 0.016778 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(car::Anova(regression_model_exp2_prwin50, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                            87.4615  1  < 2.2e-16 ***
# ambiguity                               2.8919  1   0.089029 .  
# scale(cashout_cond)                    10.3453  1   0.001298 ** 
# scale(prompt_time)                      1.7952  1   0.180290    
# scale(pgsi)                             1.5036  1   0.220125    
# ambiguity:scale(cashout_cond)           0.9308  1   0.334646    
# ambiguity:scale(prompt_time)            0.8650  1   0.352349    
# scale(cashout_cond):scale(prompt_time)  2.0208  1   0.155159    
# ambiguity:scale(pgsi)                   0.6261  1   0.428806    
# scale(cashout_cond):scale(pgsi)         0.4339  1   0.510088    
# scale(prompt_time):scale(pgsi)          0.0564  1   0.812225    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp2_prwin50))

# Fixed effects:
#                                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -2.41318    0.25804  -9.352   <2e-16 ***
# ambiguity1                              0.12119    0.07127   1.701   0.0890 .  
# scale(cashout_cond)                     0.23009    0.07154   3.216   0.0013 ** 
# scale(prompt_time)                      0.09548    0.07126   1.340   0.1803    
# scale(pgsi)                            -0.31085    0.25351  -1.226   0.2201    
# ambiguity1:scale(cashout_cond)          0.06847    0.07096   0.965   0.3346    
# ambiguity1:scale(prompt_time)           0.06582    0.07077   0.930   0.3523    
# scale(cashout_cond):scale(prompt_time) -0.10096    0.07102  -1.422   0.1552    
# ambiguity1:scale(pgsi)                  0.05806    0.07338   0.791   0.4288    
# scale(cashout_cond):scale(pgsi)        -0.04788    0.07270  -0.659   0.5101    
# scale(prompt_time):scale(pgsi)         -0.01725    0.07263  -0.238   0.8122    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(car::Anova(regression_model_exp2_prwin30, type="III"))

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: cashout_choice
#                                          Chisq Df Pr(>Chisq)    
# (Intercept)                            10.4950  1  0.0011970 ** 
# ambiguity                              22.0172  1  2.702e-06 ***
# scale(cashout_cond)                     0.0068  1  0.9342689    
# scale(prompt_time)                     11.3658  1  0.0007481 ***
# scale(pgsi)                             1.9069  1  0.1673070    
# ambiguity:scale(cashout_cond)           2.2223  1  0.1360288    
# ambiguity:scale(prompt_time)            0.3914  1  0.5315880    
# scale(cashout_cond):scale(prompt_time)  0.4518  1  0.5014800    
# ambiguity:scale(pgsi)                   0.7938  1  0.3729528    
# scale(cashout_cond):scale(pgsi)         6.5932  1  0.0102371 *  
# scale(prompt_time):scale(pgsi)          1.2141  1  0.2705275    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

print(summary(regression_model_exp2_prwin30))

# Fixed effects:
#                                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                            -0.841721   0.259823  -3.240 0.001197 ** 
# ambiguity1                             -0.287979   0.061373  -4.692  2.7e-06 ***
# scale(cashout_cond)                    -0.005006   0.060700  -0.082 0.934269    
# scale(prompt_time)                     -0.206552   0.061267  -3.371 0.000748 ***
# scale(pgsi)                             0.356259   0.257989   1.381 0.167307    
# ambiguity1:scale(cashout_cond)          0.090915   0.060986   1.491 0.136029    
# ambiguity1:scale(prompt_time)          -0.038048   0.060820  -0.626 0.531588    
# scale(cashout_cond):scale(prompt_time)  0.040936   0.060902   0.672 0.501480    
# ambiguity1:scale(pgsi)                  0.049335   0.055373   0.891 0.372953    
# scale(cashout_cond):scale(pgsi)        -0.142217   0.055387  -2.568 0.010237 *  
# scale(prompt_time):scale(pgsi)          0.058864   0.053423   1.102 0.270528    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## calculate mean cashout prob across participants and conditions
cashout_prob_30_exp2   <- as.vector(by(behav_data_exp2$cashout_choice[behav_data_exp2$prediction_appraisal==30], INDICES=behav_data_exp2$id[behav_data_exp2$prediction_appraisal==30], FUN=mean, na.rm=T))
cashout_prob_50_exp2   <- as.vector(by(behav_data_exp2$cashout_choice[behav_data_exp2$prediction_appraisal==50], INDICES=behav_data_exp2$id[behav_data_exp2$prediction_appraisal==50], FUN=mean, na.rm=T))
cashout_prob_70_exp2   <- as.vector(by(behav_data_exp2$cashout_choice[behav_data_exp2$prediction_appraisal==70], INDICES=behav_data_exp2$id[behav_data_exp2$prediction_appraisal==70], FUN=mean, na.rm=T))
cashout_prob_data_exp2 <- data.frame(cashout_prob_30_exp2, cashout_prob_50_exp2, cashout_prob_70_exp2)

## correlations between cashout conds
corr_mat <- psych::corr.test(x = cashout_prob_data_exp2, method="spearman")
print(corr_mat$r, 2)

#                      cashout_prob_30_exp2 cashout_prob_50_exp2 cashout_prob_70_exp2
# cashout_prob_30_exp2                 1.00                 0.38                -0.46
# cashout_prob_50_exp2                 0.38                 1.00                 0.20
# cashout_prob_70_exp2                -0.46                 0.20                 1.00

print(round(corr_mat$p, 5),2)

#                      cashout_prob_30_exp2 cashout_prob_50_exp2 cashout_prob_70_exp2
# cashout_prob_30_exp2              0.00000               0.0011              0.00005
# cashout_prob_50_exp2              0.00054               0.0000              0.06886
# cashout_prob_70_exp2              0.00002               0.0689              0.00000