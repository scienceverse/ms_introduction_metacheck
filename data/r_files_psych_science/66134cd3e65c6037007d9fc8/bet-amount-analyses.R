# libraries
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

## load in data (experiment 2)
behav_data_exp2 <- read.csv(here::here("data", "exp2", "behav_data.csv"))
meta_data_exp2 <- read.csv(here::here("data", "exp2", "meta_data.csv"))

## screen out participants who self-reported not taking the task seriously (3 excluded total)
behav_data_exp2 <- subset(behav_data_exp2, behav_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$took_seriously == "Yes"])
meta_data_exp2  <- subset(meta_data_exp2, meta_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$took_seriously == "Yes"])

## screen reports of technical issues (3 excluded total)
behav_data_exp2 <- subset(behav_data_exp2, !(behav_data_exp2$id %in%  c("k0dk808o30u9dftj718prto4", "nkdik3bf5t6u40iaoua9yqer", "ydkeeirspitaeefwkk9oi4wc")))
meta_data_exp2  <- subset(meta_data_exp2, !(meta_data_exp2$id %in%  c("k0dk808o30u9dftj718prto4", "nkdik3bf5t6u40iaoua9yqer", "ydkeeirspitaeefwkk9oi4wc")))

## screen out participants who failed questionnaire attention checks (5 excluded total)
behav_data_exp2 <- subset(behav_data_exp2, behav_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$n_incorrect_pgsi == 0])
meta_data_exp2  <- subset(meta_data_exp2, meta_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$n_incorrect_pgsi == 0])

## screen out 1 participant who self-reported residing in Italy
behav_data_exp2 <- subset(behav_data_exp2, behav_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$country != "Italy"])
meta_data_exp2  <- subset(meta_data_exp2, meta_data_exp2$id %in% meta_data_exp2$id[meta_data_exp2$country != "Italy"])

## analyse Experiment 1 data using a Wilcoxon signed-rank test
bet_amount_exp1 <- as.data.frame.table(by(behav_data_exp1$bet_amount, INDICES=list(behav_data_exp1$cashout_availability, behav_data_exp1$id), FUN=mean))
colnames(bet_amount_exp1) <- c("cashout_availability", "id", "bet_amount")

print(t.test(x=bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "available"], y=bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "unavailable"], paired=TRUE))

# 	Paired t-test
# 
# data:  bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "available"] and bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "unavailable"]
# t = 5.3767, df = 51, p-value = 1.906e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   5.775061 12.657535
# sample estimates:
# mean difference 
#        9.216298 

## exclude participants who bet at ceiling
overall_bet_amount_exp1 <- as.data.frame.table(by(behav_data_exp1$bet_amount, INDICES=list(behav_data_exp1$id), FUN=mean))
ceiling_ids <- overall_bet_amount_exp1$Var1[overall_bet_amount_exp1$Freq == 50]
print(t.test(x=bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "available" & !(bet_amount_exp1$id %in% ceiling_ids)], y=bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "unavailable" & !(bet_amount_exp1$id %in% ceiling_ids)], paired=TRUE))

# 	Paired t-test
# 
# data:  bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "available" & !(bet_amount_exp1$id %in% ceiling_ids)] and bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "unavailable" & !(bet_amount_exp1$id %in% ceiling_ids)]
# t = 5.5407, df = 46, p-value = 1.404e-06
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   6.492378 13.901132
# sample estimates:
# mean difference 
#        10.19675 

## summary statistics
print(by(bet_amount_exp1$bet_amount, INDICES=bet_amount_exp1$cashout_availability, FUN=mean))

# bet_amount$cashout_availability: available
# [1] 35.95935
# --------------------------------------------------------------------------------------------------- 
#     bet_amount$cashout_availability: unavailable
# [1] 26.74306

## calculate effect size
print(lsr::cohensD(x=bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "available"], y=bet_amount_exp1$bet_amount[bet_amount_exp1$cashout_availability == "unavailable"], method="paired"))

# [1] 0.7456138

## analyse Experiment 2 data using a Wilcoxon rank-sum test
bet_amount_exp2 <- as.vector(by(behav_data_exp2$bet_amount, INDICES=behav_data_exp2$id, FUN=mean))

## analyse data using a Wilcoxon rank-sum test
print(wilcox.test(x=bet_amount_exp2[meta_data_exp2$cashout_group == "available"], y=bet_amount_exp2[meta_data_exp2$cashout_group == "unavailable"], paired=FALSE))

# Wilcoxon rank sum test with continuity correction
# 
# data:  bet_amount[meta_data$cashout_group == "available"] and bet_amount[meta_data$cashout_group == "unavailable"]
# W = 4375, p-value = 0.004149
# alternative hypothesis: true location shift is not equal to 0

## exclude participants who bet at ceiling
print(wilcox.test(x=bet_amount_exp2[meta_data_exp2$cashout_group == "available" & bet_amount_exp2 != 100], y=bet_amount_exp2[meta_data_exp2$cashout_group == "unavailable" & bet_amount_exp2 != 100], paired=FALSE))

# 	Wilcoxon rank sum test with continuity correction
# 
# data:  bet_amount_exp2[meta_data_exp2$cashout_group == "available" & bet_amount_exp2 != 100] and bet_amount_exp2[meta_data_exp2$cashout_group == "unavailable" & bet_amount_exp2 != 100]
# W = 3619, p-value = 0.006072
# alternative hypothesis: true location shift is not equal to 0

## summary statistics
print(by(bet_amount_exp2, INDICES=meta_data_exp2$cashout_group, FUN=mean))

# meta_data$cashout_group: available
# [1] 68.48889
# --------------------------------------------------------------------- 
#     meta_data$cashout_group: unavailable
# [1] 55.42848

## calculate effect size
print(lsr::cohensD(x=bet_amount_exp2[meta_data_exp2$cashout_group == "available"], y=bet_amount_exp2[meta_data_exp2$cashout_group == "unavailable"]))

# [1] 0.4562516

## regression analysis - experiment 1
behav_data_exp1$time_on_task <- NA
behav_data_exp1$time_on_task[behav_data_exp1$cashout_availability == "available"] <- 1:44
behav_data_exp1$time_on_task[behav_data_exp1$cashout_availability == "unavailable"] <- 1:36
behav_data_exp1$pgsi <- meta_data_exp1$pgsi[match(behav_data_exp1$id, meta_data_exp1$id)]

## set up contrasts and do regression
behav_data_exp1$cashout_availability <- factor(behav_data_exp1$cashout_availability)
contrasts(behav_data_exp1$cashout_availability) <- contr.sum(2)

# > print(contrasts(behav_data_exp1$cashout_availability))
# [,1]
# available      1
# unavailable   -1

exp1_analysis <- lmer(
    data    = behav_data_exp1,
    formula = scale(bet_amount) ~ scale(time_on_task)*cashout_availability + (1 + scale(time_on_task) * cashout_availability | id)
)
car::Anova(exp1_analysis, type="III")
summary(exp1_analysis)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: scale(bet_amount)
#                                            Chisq Df Pr(>Chisq)    
# (Intercept)                               0.0964  1   0.756181    
# scale(time_on_task)                       5.7953  1   0.016070 *  
# cashout_availability                     25.3096  1  4.883e-07 ***
# scale(time_on_task):cashout_availability  7.1418  1   0.007531 **
#
#
# Fixed effects:
#                                             Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                               -0.03313    0.10669 50.99640  -0.311   0.7574    
# scale(time_on_task)                        0.07949    0.03302 51.00905   2.407   0.0197 *  
# cashout_availability1                      0.25149    0.04999 51.01074   5.031 6.42e-06 ***
# scale(time_on_task):cashout_availability1  0.04789    0.01792 51.00482   2.672   0.0101 * 

exp1_analysis_pgsi <- lmer(
    data    = behav_data_exp1,
    formula = scale(bet_amount) ~ scale(time_on_task)*cashout_availability*pgsi + (1 + scale(time_on_task)*cashout_availability | id)
)
car::Anova(exp1_analysis_pgsi, type="III")
summary(exp1_analysis_pgsi)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: scale(bet_amount)
#                                                 Chisq Df Pr(>Chisq)    
# (Intercept)                                    0.1824  1    0.66931    
# scale(time_on_task)                            3.1759  1    0.07473 .  
# cashout_availability                          22.6754  1  1.918e-06 ***
# pgsi                                           2.2612  1    0.13265    
# scale(time_on_task):cashout_availability       3.4613  1    0.06282 .  
# scale(time_on_task):pgsi                       0.5083  1    0.47586    
# cashout_availability:pgsi                      0.5230  1    0.46958    
# scale(time_on_task):cashout_availability:pgsi  1.1554  1    0.28242    
#
#
#                                                 Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     0.050956   0.119308 50.002474   0.427   0.6711    
# scale(time_on_task)                             0.066940   0.037563 50.007264   1.782   0.0808 .  
# cashout_availability1                           0.270765   0.056861 50.000587   4.762 1.68e-05 ***
# pgsi                                           -0.065259   0.043398 50.002478  -1.504   0.1389    
# scale(time_on_task):cashout_availability1       0.037685   0.020256 49.999883   1.860   0.0687 .  
# scale(time_on_task):pgsi                        0.009742   0.013664 50.007264   0.713   0.4792    
# cashout_availability1:pgsi                     -0.014957   0.020683 50.000585  -0.723   0.4730    
# scale(time_on_task):cashout_availability1:pgsi  0.007920   0.007368 49.999883   1.075   0.2876  

## add task trial to behavioural dataframe
behav_data_exp2$time_on_task <- (behav_data_exp2$block - 1) * 18 + behav_data_exp2$trial
behav_data_exp2$pgsi <- meta_data_exp2$pgsi[match(behav_data_exp2$id, meta_data_exp2$id)]

## set up contrasts and do regression
behav_data_exp2$cashout_availability <- factor(behav_data_exp2$cashout_availability)
contrasts(behav_data_exp2$cashout_availability) <- contr.sum(2)

# > print(contrasts(behav_data_exp2$cashout_availability))
# [,1]
# available      1
# unavailable   -1

exp2_analysis <- lmer(
    data    = behav_data_exp2,
    formula = scale(bet_amount) ~ scale(time_on_task)*cashout_availability + (1 + scale(time_on_task) | id)
)
car::Anova(exp2_analysis, type="III")
summary(exp2_analysis)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: scale(bet_amount)
#                                            Chisq Df Pr(>Chisq)    
# (Intercept)                               4.5196  1  0.0335081 *  
# scale(time_on_task)                      13.3476  1  0.0002588 ***
# cashout_availability                      8.6756  1  0.0032250 ** 
# scale(time_on_task):cashout_availability  4.1989  1  0.0404494 *  
#
#
#                                                      Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                                 0.00775    0.06277 164.99904   0.123  0.90189   
# scale(time_on_task)                         0.05953    0.01846 165.00046   3.225  0.00152 **
# cashout_availability1                       0.18489    0.06277 164.99904   2.945  0.00369 **
# scale(time_on_task):cashout_availability1   0.03783    0.01846 165.00046   2.049  0.04203 * 

exp2_analysis_pgsi <- lmer(
    data    = behav_data_exp2,
    formula = scale(bet_amount) ~ scale(time_on_task)*cashout_availability*pgsi + (1 + scale(time_on_task) | id)
)
car::Anova(exp2_analysis_pgsi, type="III")
summary(exp2_analysis_pgsi)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: scale(bet_amount)
#                                                 Chisq Df Pr(>Chisq)    
# (Intercept)                                    2.8382  1  0.0920468 .  
# scale(time_on_task)                           11.2533  1  0.0007948 ***
# cashout_availability                          12.7362  1  0.0003586 ***
# pgsi                                           0.3490  1  0.5546700    
# scale(time_on_task):cashout_availability       2.2589  1  0.1328500    
# scale(time_on_task):pgsi                       0.0459  1  0.8302778    
# cashout_availability:pgsi                      4.6005  1  0.0319624 *  
# scale(time_on_task):cashout_availability:pgsi  0.5483  1  0.4590117  

# Fixed effects:
#                                                  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                                     -0.080107   0.069091 162.998270  -1.159 0.247968    
# scale(time_on_task)                              0.068829   0.020892 162.997356   3.295 0.001209 ** 
# cashout_availability1                            0.246570   0.069091 162.998270   3.569 0.000471 ***
# pgsi                                             0.055667   0.019311 162.998255   2.883 0.004475 ** 
# scale(time_on_task):cashout_availability1        0.031400   0.020892 162.997356   1.503 0.134785    
# scale(time_on_task):pgsi                        -0.005887   0.005839 162.997356  -1.008 0.314875    
# cashout_availability1:pgsi                      -0.041420   0.019311 162.998255  -2.145 0.033444 *  
# scale(time_on_task):cashout_availability1:pgsi   0.004324   0.005839 162.997356   0.740 0.460077  

exp2_analysis_pgsi_posthocavail <- lmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "available"),
    formula = scale(bet_amount) ~ scale(time_on_task)*pgsi + (1 + scale(time_on_task) | id)
)
car::Anova(exp2_analysis_pgsi_posthocavail, type="III")
summary(exp2_analysis_pgsi_posthocavail)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: scale(bet_amount)
#                            Chisq Df Pr(>Chisq)    
# (Intercept)               0.0780  1  0.7799969    
# scale(time_on_task)      13.7348  1  0.0002105 ***
# pgsi                      0.3879  1  0.5333934    
# scale(time_on_task):pgsi  0.0561  1  0.8128100    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# Fixed effects:
#                           Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)              -0.029080   0.104108 78.000215  -0.279 0.780736    
# scale(time_on_task)       0.111341   0.030043 78.000200   3.706 0.000392 ***
# pgsi                      0.015826   0.025409 78.000214   0.623 0.535210    
# scale(time_on_task):pgsi -0.001736   0.007332 78.000200  -0.237 0.813431      

exp2_analysis_pgsi_posthocunavail <- lmer(
    data    = subset(behav_data_exp2, behav_data_exp2$cashout_availability == "unavailable"),
    formula = scale(bet_amount) ~ scale(time_on_task)*pgsi + (1 + scale(time_on_task) | id)
)
car::Anova(exp2_analysis_pgsi_posthocunavail, type="III")
summary(exp2_analysis_pgsi_posthocunavail)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: scale(bet_amount)
#                           Chisq Df Pr(>Chisq)   
# (Intercept)              2.1945  1   0.138506   
# scale(time_on_task)      1.4086  1   0.235297   
# pgsi                     9.4843  1   0.002072 **
# scale(time_on_task):pgsi 1.0748  1   0.299874   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# Fixed effects:
#                           Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)              -0.142000   0.095856 84.999773  -1.481  0.14220   
# scale(time_on_task)       0.035545   0.029950 85.000326   1.187  0.23860   
# pgsi                      0.092194   0.029936 84.999774   3.080  0.00279 **
# scale(time_on_task):pgsi -0.009697   0.009353 85.000327  -1.037  0.30281 