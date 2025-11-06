Study 1 tests with covariates 

Call:
  lm(formula = antiSe ~ (CNSC + IN + IGSAT) * condition, data = pilot)

Residuals:
  Min       1Q   Median       3Q      Max 
-3.03707 -0.66244 -0.05175  0.63664  3.12610 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.726e+00  3.041e-01   5.674 2.25e-08 ***
  CNSC              4.468e-01  7.098e-02   6.294 6.26e-10 ***
  IN                5.384e-05  2.407e-02   0.002   0.9982    
IGSAT             8.929e-02  6.830e-02   1.307   0.1916    
condition1        6.322e-01  4.349e-01   1.454   0.1465    
condition2        1.161e+00  4.832e-01   2.402   0.0166 *  
  CNSC:condition1   8.920e-02  1.060e-01   0.842   0.4002    
CNSC:condition2  -2.719e-01  1.113e-01  -2.443   0.0149 *  
  IN:condition1    -4.095e-04  3.347e-02  -0.012   0.9902    
IN:condition2     1.135e-02  3.485e-02   0.326   0.7448    
IGSAT:condition1 -2.174e-01  9.949e-02  -2.185   0.0293 *  
  IGSAT:condition2 -6.257e-02  1.080e-01  -0.579   0.5626    
---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.065 on 557 degrees of freedom
Multiple R-squared:  0.1863,	Adjusted R-squared:  0.1702 
F-statistic: 11.59 on 11 and 557 DF,  p-value: < 2.2e-16

> trends.int <- interactions::sim_slopes(int, "CNSC", "condition", robust = T, confint = T)
Warning message:
  Johnson-Neyman intervals are not available for factor
moderators. 
> trends.int
SIMPLE SLOPES ANALYSIS 

Slope of CNSC when condition = 2: 
  
  Est.   S.E.    2.5%   97.5%   t val.      p
------ ------ ------- ------- -------- ------
  0.17   0.14   -0.10    0.45     1.25   0.21

Slope of CNSC when condition = 1: 
  
  Est.   S.E.   2.5%   97.5%   t val.      p
------ ------ ------ ------- -------- ------
  0.54   0.09   0.36    0.71     6.04   0.00

Slope of CNSC when condition = 0: 
  
  Est.   S.E.   2.5%   97.5%   t val.      p
------ ------ ------ ------- -------- ------
  0.45   0.08   0.29    0.60     5.78   0.00


coeftest(int, vcov = vcovHC(int, type = "HC4"))
t test of coefficients:
  
  Estimate  Std. Error t value  Pr(>|t|)
(Intercept)       1.7255e+00  2.8677e-01  6.0169 3.223e-09
CNSC              4.4678e-01  7.7913e-02  5.7344 1.606e-08
IN                5.3841e-05  2.3072e-02  0.0023   0.99814
IGSAT             8.9292e-02  8.1652e-02  1.0936   0.27462
condition1        6.3222e-01  4.7891e-01  1.3201   0.18734
condition2        1.1606e+00  5.6010e-01  2.0721   0.03871
CNSC:condition1   8.9205e-02  1.1876e-01  0.7511   0.45288
CNSC:condition2  -2.7192e-01  1.7001e-01 -1.5995   0.11029
IN:condition1    -4.0952e-04  3.6383e-02 -0.0113   0.99102
IN:condition2     1.1348e-02  3.7887e-02  0.2995   0.76466
IGSAT:condition1 -2.1742e-01  1.1526e-01 -1.8863   0.05977
IGSAT:condition2 -6.2574e-02  1.6976e-01 -0.3686   0.71256

(Intercept)      ***
  CNSC             ***
  IN                  
IGSAT               
condition1          
condition2       *  
  CNSC:condition1     
CNSC:condition2     
IN:condition1       
IN:condition2       
IGSAT:condition1 .  
IGSAT:condition2    
---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


