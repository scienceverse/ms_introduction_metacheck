### NZAVS - RI-CLPM (Conspiracy Beliefs and Psychological Need Satisfaction)
# Source 1: https://jeroendmulder.github.io/RI-CLPM/lavaan.html
# Source 2: https://johnflournoy.science/2017/10/20/riclpm-lavaan-demo/

#  Load packages
library(lavaan)
library(tidyr)
library(dplyr)

# Set Working directory
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Load datafile
data <- read.csv("Needs-Consp_data11_13_221110.csv", header = T)

# Describe sample
length(unique(data$Questionnaire.Num)) # N = 49,895
psych::describe(data$Age.T11)
psych::describe(data$Age.T12)
psych::describe(data$Age.T13)
data %>% dplyr::count(Gender.T11, sort = T) # 0 = female, 1 = male
data %>% dplyr::count(Gender.T12, sort = T)
data %>% dplyr::count(Gender.T13, sort = T)
data %>% dplyr::count(EthnicCats.T11, sort = T) # 1 = NZ European, 2 = Maori, 3 = Pacific, 4 = Asian
data %>% dplyr::count(EthnicCats.T12, sort = T)
data %>% dplyr::count(EthnicCats.T13, sort = T)

### Cronbach's alpha (3 items) / Correlations (2 items)
## Control
cor.test(data$PowerDependence01.T11_r, data$PowerDependence02.T11_r)
cor.test(data$PowerDependence01.T12_r, data$PowerDependence02.T12_r)
cor.test(data$PowerDependence01.T13_r, data$PowerDependence02.T13_r)

## Belonging
psych::alpha (data [, c( "Rel.Belong01.T11", "Rel.Belong02r.T11", "Rel.Belong03.T11")])
psych::alpha (data [, c( "Rel.Belong01.T12", "Rel.Belong02r.T12", "Rel.Belong03.T12")])
psych::alpha (data [, c( "Rel.Belong01.T13", "Rel.Belong02r.T13", "Rel.Belong03.T13")])

## Self-esteem
psych::alpha (data [, c( "SWB.SelfEsteem01.T11", "SWB.SelfEsteem02.T11", "SWB.SelfEsteem03r.T11")])
psych::alpha (data [, c( "SWB.SelfEsteem01.T12", "SWB.SelfEsteem02.T12", "SWB.SelfEsteem03r.T12")])
psych::alpha (data [, c( "SWB.SelfEsteem01.T13", "SWB.SelfEsteem02.T13", "SWB.SelfEsteem03r.T13")])

## Meaning
cor.test(data$LifeMeaning01.T11, data$LifeMeaning02.T11)
cor.test(data$LifeMeaning01.T12, data$LifeMeaning02.T12)
cor.test(data$LifeMeaning01.T13, data$LifeMeaning02.T13)


################ Overview ################
## Compare CLPM & RI-CLPM of needs on:
# 1. CLPM Conspiracy belief + 4 needs --> Reported
# 2. RI-CLPM Conspiracy belief + 4 needs
# 3. RI-CLPM Conspiracy belief + 4 needs (full stationarity) --> (X^2 = 490.23, p = 2.2e-16 ***)
# 4. RI-CLPM Conspiracy belief + 4 needs (partial stationarity)
# 4.1 RI-CLPM Conspiracy belief + 4 needs (a constrained) --> x (X^2 =  7.6008, p < 0.005834 **)
# 4.2 RI-CLPM Conspiracy belief + 4 needs (b constrained) --> x (X^2 = 6.509, p =  0.01073 *)
# 4.3 RI-CLPM Conspiracy belief + 4 needs (c constrained) --> constrain (X^2 = 0.7185, p = 0.3966)
# 4.4 RI-CLPM Conspiracy belief + 4 needs (d constrained) --> x (X^2 = 12.415, p= 0.000426 ***)
# 4.5 RI-CLPM Conspiracy belief + 4 needs (e constrained) --> x (X^2 = 31.385, p = 2.116e-08 ***)
# 4.6 RI-CLPM Conspiracy belief + 4 needs (f constrained) --> x (X^2 = 305.45, p < 2.2e-16 ***)
# 4.7 RI-CLPM Conspiracy belief + 4 needs (g constrained) --> x (X^2 = 3.8553, p = .04959)
# 4.8 RI-CLPM Conspiracy belief + 4 needs (h constrained) --> x (X^2 = 21.725, p < 3.146e-06 ***)
# 4.9 RI-CLPM Conspiracy belief + 4 needs (i constrained) --> constrain (X^2 = 0.10094, p = 0.7507)
# 4.10 RI-CLPM Conspiracy belief + 4 needs (j constrained) --> constrain (X^2 =  0.013511, p = 0.9075)
# 4.11 RI-CLPM Conspiracy belief + 4 needs (k constrained) --> x (X^2 = 11.393, p = 0.0007372 ***)
# 4.12 RI-CLPM Conspiracy belief + 4 needs (l constrained) --> constrain (X^2 = 0.85681, p = 0.3546)
# 4.13 RI-CLPM Conspiracy belief + 4 needs (m constrained) --> x (X^2 = 17.021, p < 3.697e-05 ***)
# 4.14 RI-CLPM Conspiracy belief + 4 needs (n constrained) --> constrain (X^2 = 0.77645, p = 0.3782)
# 4.15 RI-CLPM Conspiracy belief + 4 needs (o constrained) --> x (X^2 = 4.657, p = 0.03093 *)
# 4.16 RI-CLPM Conspiracy belief + 4 needs (p constrained) --> constrain (X^2 = 0.97883, p = 0.3225)
# 4.17 RI-CLPM Conspiracy belief + 4 needs (q constrained) --> constrain (X^2 = 2.9682, p = 0.08492 .)
# 4.18 RI-CLPM Conspiracy belief + 4 needs (r constrained) --> x (X^2 = 36.569, p = 1.474e-09 ***)
# 4.19 RI-CLPM Conspiracy belief + 4 needs (s constrained) --> constrain (X^2 = 3.0674, p = 0.07988 .)
# 4.20 RI-CLPM Conspiracy belief + 4 needs (t constrained) --> constrain (X^2 = 0.27716, p = 0.5986)
# 4.21 RI-CLPM Conspiracy belief + 4 needs (u constrained) --> x (X^2 = 7.9217, p = 0.004885 **)
# 4.22 RI-CLPM Conspiracy belief + 4 needs (v constrained) --> x (X^2 = 17.475 , p < 2.911e-05 ***)
# 4.23 RI-CLPM Conspiracy belief + 4 needs (w constrained) --> x (X^2 = 28.86, p = 7.779e-08 ***)
# 4.24 RI-CLPM Conspiracy belief + 4 needs (x constrained) --> constrain (X^2 = 3.0674, p = 0.07988 .)
# 4.25 RI-CLPM Conspiracy belief + 4 needs (y constrained) --> constrain (Final model with partial stationarity) (X^2 = 0.00074542, p = 0.9782) --> Reported
# 5 RI-CLPM Conspiracy belief + 4 needs (RI-constraints)
# 5.1 RI-CLPM Conspiracy belief + 4 needs (RI-constraints Belonging~~Consp vs. Control~~Consp) --> cov(Control, Consp) > cov(Belonging, Consp)  --> Reported
# 5.2 RI-CLPM Conspiracy belief + 4 needs (RI-constraints Belonging~~Consp vs. Control~~Consp) --> cov(Belonging, Consp) > cov(Self-esteem, Consp)  --> Reported
# 5.3 RI-CLPM Conspiracy belief + 4 needs (RI-constraints Belonging~~Consp vs. Control~~Consp) --> cov(Self-esteem, Consp) > cov(Meaning, Consp)  --> Reported

# 6. RI-CLPM Conspiracy belief + 4 needs (Cross-lagged path constraints)
# 6.1. RI-CLPM Conspiracy belief + 4 needs (g3 + h3 constrained)

################# 1 CLPM Conspiracy belief + 4 needs ###############

# CLPM
CLPM.CNeeds <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c3 * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c2 * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ 0*RI_Belong
RI_Belong ~~ 0*RI_Control
RI_Belong ~~ 0*RI_Meaning
RI_Belong ~~ 0*RI_Selfesteem
RI_Belong ~~ 0*RI_Consp

RI_Control ~~ 0*RI_Control
RI_Control ~~ 0*RI_Meaning
RI_Control ~~ 0*RI_Selfesteem
RI_Control ~~ 0*RI_Consp

RI_Meaning ~~ 0*RI_Meaning
RI_Meaning ~~ 0*RI_Selfesteem
RI_Meaning ~~ 0*RI_Consp

RI_Selfesteem ~~ 0*RI_Selfesteem
RI_Selfesteem ~~ 0*RI_Consp

RI_Consp ~~ 0*RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
CLPM.CNeeds.fit <- lavaan(CLPM.CNeeds, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(CLPM.CNeeds.fit, standardized = T, ci = T, fit.measures = TRUE, rsquare=T)


################# 2 RI-CLPM Conspiracy belief + 4 needs ###############

RICLPM.CNeeds <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c3 * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c2 * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.fit <- lavaan(RICLPM.CNeeds, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.fit, standardized = T)

anova(CLPM.CNeeds.fit, RICLPM.CNeeds.fit)  ## RI-CLPM fits significantly better

################# 3 RI-CLPM Conspiracy belief + 4 needs (fully stationarity) ###############

RICLPM.CNeeds.full <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered variables. (all autoregressive and cross-lagged effects are constrained to be equal)
w_Belong13 ~ a * w_Belong12 + b * w_Consp12 + c * w_Control12 + d * w_Meaning12 + e * w_Selfesteem12
w_Belong12 ~ a * w_Belong11 + b * w_Consp11 + c * w_Control11 + d * w_Meaning11 + e * w_Selfesteem11

w_Consp13 ~ f * w_Consp12 + g * w_Belong12 + h * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f * w_Consp11 + g * w_Belong11 + h * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k * w_Control12 + l * w_Belong12 + m * w_Consp12 + n * w_Meaning12 + o * w_Selfesteem12
w_Control12 ~ k * w_Control11 + l * w_Belong11 + m * w_Consp11 + n * w_Meaning11 + o * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u * w_Selfesteem12  + v * w_Control12  + w * w_Belong12  + x * w_Consp12  + y * w_Meaning12
w_Selfesteem12 ~ u * w_Selfesteem11  + v * w_Control11  + w * w_Belong11  + x * w_Consp11  + y * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.full.fit <- lavaan(RICLPM.CNeeds.full, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.full.fit, standardized = T)

anova(RICLPM.CNeeds.fit, RICLPM.CNeeds.full.fit)  ## Unconstrained RI-CLPM fits significantly better

################# 4. RI-CLPM Conspiracy belief + 4 needs (partial stationarity) ###############
################# 4.1 RI-CLPM Conspiracy belief + 4 needs (a constrained)       ###############

RICLPM.CNeeds.4.1 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a * w_Belong12 + b3 * w_Consp12 + c3 * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a * w_Belong11 + b2 * w_Consp11 + c2 * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.1.fit <- lavaan(RICLPM.CNeeds.4.1, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.1.fit, standardized = T)

anova(RICLPM.CNeeds.fit, RICLPM.CNeeds.4.1.fit)  ## Unconstrained model fits significantly better

################# 4.2 RI-CLPM Conspiracy belief + 4 needs (b constrained)       ###############

RICLPM.CNeeds.4.2 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b * w_Consp12 + c3 * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b * w_Consp11 + c2 * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.2.fit <- lavaan(RICLPM.CNeeds.4.2, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.2.fit, standardized = T)

anova(RICLPM.CNeeds.fit, RICLPM.CNeeds.4.2.fit)  ## Unconstrained model fits significantly better

################# 4.3 RI-CLPM Conspiracy belief + 4 needs (c constrained)       ###############

RICLPM.CNeeds.4.3 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.3.fit <- lavaan(RICLPM.CNeeds.4.3, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.3.fit, standardized = T)

anova(RICLPM.CNeeds.fit, RICLPM.CNeeds.4.3.fit)  ## Constrained model fits significantly better ( --> constrain c-path)

################# 4.4 RI-CLPM Conspiracy belief + 4 needs (c + d constrained)       ###############

RICLPM.CNeeds.4.4 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.4.fit <- lavaan(RICLPM.CNeeds.4.4, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.4.fit, standardized = T)

anova(RICLPM.CNeeds.4.3.fit, RICLPM.CNeeds.4.4.fit)  ## Less constrained model fits significantly better ( --> constrain only c-path)

################# 4.5 RI-CLPM Conspiracy belief + 4 needs (c + e constrained)       ###############

RICLPM.CNeeds.4.5 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.5.fit <- lavaan(RICLPM.CNeeds.4.5, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.5.fit, standardized = T)

anova(RICLPM.CNeeds.4.3.fit, RICLPM.CNeeds.4.5.fit)  ## Less constrained model fits significantly better ( --> constrain only c-path)

################# 4.6 RI-CLPM Conspiracy belief + 4 needs (c + f constrained)       ###############

RICLPM.CNeeds.4.6 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.6.fit <- lavaan(RICLPM.CNeeds.4.6, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.6.fit, standardized = T)

anova(RICLPM.CNeeds.4.3.fit, RICLPM.CNeeds.4.6.fit)  ## Less constrained model fits significantly better ( --> constrain only c-path)

################# 4.7 RI-CLPM Conspiracy belief + 4 needs (c + g constrained)       ###############

RICLPM.CNeeds.4.7 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g * w_Belong12 + h3 * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g * w_Belong11 + h2 * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.7.fit <- lavaan(RICLPM.CNeeds.4.7, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.7.fit, standardized = T)

anova(RICLPM.CNeeds.4.3.fit, RICLPM.CNeeds.4.7.fit)  ## Less constrained model fits significantly better ( --> constrain only c-path)

################# 4.8 RI-CLPM Conspiracy belief + 4 needs (c + h constrained)       ###############

RICLPM.CNeeds.4.8 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h * w_Control12 + i3 * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h * w_Control11 + i2 * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.8.fit <- lavaan(RICLPM.CNeeds.4.8, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.8.fit, standardized = T)

anova(RICLPM.CNeeds.4.3.fit, RICLPM.CNeeds.4.8.fit)  ## Less constrained model fits significantly better ( --> constrain only c-path)

################# 4.9 RI-CLPM Conspiracy belief + 4 needs (c + i constrained)       ###############

RICLPM.CNeeds.4.9 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j3 * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j2 * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.9.fit <- lavaan(RICLPM.CNeeds.4.9, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.9.fit, standardized = T)

anova(RICLPM.CNeeds.4.3.fit, RICLPM.CNeeds.4.9.fit)  ## More constrained model fits significantly better ( --> constrain c-path & i-path)

################# 4.10 RI-CLPM Conspiracy belief + 4 needs (c, i + j constrained)       ###############

RICLPM.CNeeds.4.10 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.10.fit <- lavaan(RICLPM.CNeeds.4.10, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.10.fit, standardized = T)

anova(RICLPM.CNeeds.4.9.fit, RICLPM.CNeeds.4.10.fit)  ## More constrained model fits significantly better ( --> constrain c-/i-path & j-path)

################# 4.11 RI-CLPM Conspiracy belief + 4 needs (c, i, j + k constrained)       ###############

RICLPM.CNeeds.4.11 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k * w_Control12 + l3 * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k * w_Control11 + l2 * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.11.fit <- lavaan(RICLPM.CNeeds.4.11, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.11.fit, standardized = T)

anova(RICLPM.CNeeds.4.10.fit, RICLPM.CNeeds.4.11.fit)  ## Less constrained model fits significantly better ( --> constrain only  c-/i-/j-path)

################# 4.12 RI-CLPM Conspiracy belief + 4 needs (c, i, j + l constrained)       ###############

RICLPM.CNeeds.4.12 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.12.fit <- lavaan(RICLPM.CNeeds.4.12, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.12.fit, standardized = T)

anova(RICLPM.CNeeds.4.10.fit, RICLPM.CNeeds.4.12.fit)  ## More constrained model fits significantly better ( --> constrain only  c-/i-/j-path + l-path)


################# 4.13 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l + m constrained)       ###############

RICLPM.CNeeds.4.13 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m * w_Consp12 + n3 * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m * w_Consp11 + n2 * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.13.fit <- lavaan(RICLPM.CNeeds.4.13, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.13.fit, standardized = T)

anova(RICLPM.CNeeds.4.12.fit, RICLPM.CNeeds.4.13.fit)  ## Less constrained model fits significantly better ( --> constrain only  c-/i-/j-/l-path)

################# 4.14 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l + n constrained)       ###############

RICLPM.CNeeds.4.14 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.14.fit <- lavaan(RICLPM.CNeeds.4.14, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.14.fit, standardized = T)

anova(RICLPM.CNeeds.4.12.fit, RICLPM.CNeeds.4.14.fit)  ## More constrained model fits better ( --> constrain c-/i-/j-/l-path + n-path)

################# 4.15 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n + o constrained)       ###############

RICLPM.CNeeds.4.15 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o * w_Selfesteem11

w_Meaning13 ~ p3 * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p2 * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.15.fit <- lavaan(RICLPM.CNeeds.4.15, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.15.fit, standardized = T)

anova(RICLPM.CNeeds.4.14.fit, RICLPM.CNeeds.4.15.fit)  ## Less constrained model fits significantly better ( --> constrain only c-/i-/j-/l-/n-path)

################# 4.16 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n + p constrained)       ###############

RICLPM.CNeeds.4.16 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q3 * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q2 * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.16.fit <- lavaan(RICLPM.CNeeds.4.16, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.16.fit, standardized = T)

anova(RICLPM.CNeeds.4.14.fit, RICLPM.CNeeds.4.16.fit)  ## More constrained model fits  better ( --> constrain only c-/i-/j-/l-/n-path + p-path)

################# 4.17 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p + q constrained)       ###############

RICLPM.CNeeds.4.17 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.17.fit <- lavaan(RICLPM.CNeeds.4.17, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.17.fit, standardized = T)

anova(RICLPM.CNeeds.4.16.fit, RICLPM.CNeeds.4.17.fit)  ## More constrained model fits  better ( --> constrain only c-/i-/j-/l-/n-/-p-path + q-path)

################# 4.18 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q + r constrained)       ###############

RICLPM.CNeeds.4.18 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r * w_Belong12  + s3 * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r * w_Belong11  + s2 * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.18.fit <- lavaan(RICLPM.CNeeds.4.18, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.18.fit, standardized = T)

anova(RICLPM.CNeeds.4.17.fit, RICLPM.CNeeds.4.18.fit)  ## Less constrained model fits  better ( --> constrain only c-/i-/j-/l-/n-/-p-/q-path)

################# 4.19 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q + s constrained)       ###############

RICLPM.CNeeds.4.19 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t3 * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t2 * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.19.fit <- lavaan(RICLPM.CNeeds.4.19, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.19.fit, standardized = T)

anova(RICLPM.CNeeds.4.17.fit, RICLPM.CNeeds.4.19.fit)  ## More constrained model fits  better ( --> constrain only c-/i-/j-/l-/n-/-p-/q-path + s-path)

################# 4.20 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s + t constrained)       ###############

RICLPM.CNeeds.4.20 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.20.fit <- lavaan(RICLPM.CNeeds.4.20, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.20.fit, standardized = T)

anova(RICLPM.CNeeds.4.19.fit, RICLPM.CNeeds.4.20.fit)  ## More constrained model fits  better ( --> constrain c-/i-/j-/l-/n-/-p-/q-/s-path + t-path)

################# 4.21 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t + u constrained)       ###############

RICLPM.CNeeds.4.21 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.21.fit <- lavaan(RICLPM.CNeeds.4.21, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.21.fit, standardized = T)

anova(RICLPM.CNeeds.4.20.fit, RICLPM.CNeeds.4.21.fit)  ## Less constrained model fits  better ( --> constrain c-/i-/j-/l-/n-/-p-/q-/s-/t-path)

################# 4.22 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t + v constrained)       ###############

RICLPM.CNeeds.4.22 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v * w_Control12  + w3 * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v * w_Control11  + w2 * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.22.fit <- lavaan(RICLPM.CNeeds.4.22, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.22.fit, standardized = T)

anova(RICLPM.CNeeds.4.20.fit, RICLPM.CNeeds.4.22.fit)  ## Less constrained model fits  better ( --> constrain c-/i-/j-/l-/n-/-p-/q-/s-/t-path)

################# 4.23 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t + w constrained)       ###############

RICLPM.CNeeds.4.23 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w * w_Belong12  + x3 * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w * w_Belong11  + x2 * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.23.fit <- lavaan(RICLPM.CNeeds.4.23, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.23.fit, standardized = T)

anova(RICLPM.CNeeds.4.20.fit, RICLPM.CNeeds.4.23.fit)  ## Less constrained model fits  better ( --> constrain c-/i-/j-/l-/n-/-p-/q-/s-/t-path)

################# 4.24 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t + x constrained)       ###############

RICLPM.CNeeds.4.24 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x * w_Consp12  + y3 * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x * w_Consp11  + y2 * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.24.fit <- lavaan(RICLPM.CNeeds.4.24, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.24.fit, standardized = T)

anova(RICLPM.CNeeds.4.20.fit, RICLPM.CNeeds.4.24.fit)  ## More constrained model fits  better ( --> constrain c-/i-/j-/l-/n-/-p-/q-/s-/t-path + x-path)

################# 4.25 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t, x + y constrained)       ###############

RICLPM.CNeeds.4.25 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x * w_Consp12  + y * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x * w_Consp11  + y * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.25.fit <- lavaan(RICLPM.CNeeds.4.25, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.25.fit, standardized = T, ci = T, fit.measures = TRUE)

anova(RICLPM.CNeeds.4.24.fit, RICLPM.CNeeds.4.25.fit)  ## More constrained model fits  better ( --> constrain c-/i-/j-/l-/n-/-p-/q-/s-/t-/x-path + y-path)

################# 5 RI-CLPM Conspiracy belief + 4 needs (RI-constraints)                                            ###############
################# 5.1 RI-CLPM Conspiracy belief + 4 needs (RI-constraints Belonging~~Consp vs. Control~~Consp)      ###############
################# Based on: 4.25 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t, x + y constrained) ###############

RICLPM.CNeeds.4.25.RI1 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x * w_Consp12  + y * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x * w_Consp11  + y * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ ab * RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ ab * RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.25.RI1.fit <- lavaan(RICLPM.CNeeds.4.25.RI1, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.25.RI1.fit, standardized = T, fit.measures = TRUE, rsquare=T)

anova(RICLPM.CNeeds.4.25.fit, RICLPM.CNeeds.4.25.RI1.fit)  ## Less constrained model fits better --> cov(Control, Consp) > cov(Belonging, Consp)


################# 5.2 RI-CLPM Conspiracy belief + 4 needs (RI-constraints Belonging~~Consp vs. Self-esteem~~Consp)      ###############
################# Based on: 4.25 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t, x + y constrained) ###############

RICLPM.CNeeds.4.25.RI2 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x * w_Consp12  + y * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x * w_Consp11  + y * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ ab * RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~  ab * RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.25.RI2.fit <- lavaan(RICLPM.CNeeds.4.25.RI2, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.25.RI2.fit, standardized = T)

anova(RICLPM.CNeeds.4.25.fit, RICLPM.CNeeds.4.25.RI2.fit)  ## Less constrained model fits better --> cov(Belonging, Consp) > cov(Self-esteem, Consp)



################# 5.3 RI-CLPM Conspiracy belief + 4 needs (RI-constraints Self-esteem~~Consp vs. Meaning~~Consp)      ###############
################# Based on: 4.25 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t, x + y constrained) ###############

RICLPM.CNeeds.4.25.RI3 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + g3 * w_Belong12 + h3 * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x * w_Consp12  + y * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x * w_Consp11  + y * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ ab * RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~  ab * RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.25.RI3.fit <- lavaan(RICLPM.CNeeds.4.25.RI3, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.25.RI3.fit, standardized = T)

anova(RICLPM.CNeeds.4.25.fit, RICLPM.CNeeds.4.25.RI3.fit)  ## Less constrained model fits better --> cov(Belonging, Consp) > cov(Self-esteem, Consp)


################# 6.1 RI-CLPM Conspiracy belief + 4 needs (g3 + h3 constrained)      ###############
################# Based on: 4.25 RI-CLPM Conspiracy belief + 4 needs (c, i, j, l, n, p, q, s, t, x + y constrained) ###############

RICLPM.CNeeds.4.25.CL1 <- '
  
# Create between components (random intercepts)
RI_Belong =~ 1*T11.BELONG + 1*T12.BELONG+ 1*T13.BELONG
RI_Control =~ 1*T11.CONTROL + 1*T12.CONTROL + 1*T13.CONTROL
RI_Meaning =~ 1*T11.LIFEMEANING + 1*T12.LIFEMEANING + 1*T13.LIFEMEANING
RI_Selfesteem =~ 1*T11.SELF.ESTEEM + 1*T12.SELF.ESTEEM + 1*T13.SELF.ESTEEM
RI_Consp =~ 1*ConspiracyBeliefs.T11 + 1*ConspiracyBeliefs.T12+ 1*ConspiracyBeliefs.T13

# Create within-person centered variables
w_Belong11 =~ 1*T11.BELONG
w_Belong12 =~ 1*T12.BELONG
w_Belong13 =~ 1*T13.BELONG
w_Control11 =~ 1*T11.CONTROL
w_Control12 =~ 1*T12.CONTROL
w_Control13 =~ 1*T13.CONTROL
w_Meaning11 =~ 1*T11.LIFEMEANING
w_Meaning12 =~ 1*T12.LIFEMEANING
w_Meaning13 =~ 1*T13.LIFEMEANING
w_Selfesteem11 =~ 1*T11.SELF.ESTEEM
w_Selfesteem12 =~ 1*T12.SELF.ESTEEM
w_Selfesteem13 =~ 1*T13.SELF.ESTEEM
w_Consp11 =~ 1*ConspiracyBeliefs.T11
w_Consp12 =~ 1*ConspiracyBeliefs.T12
w_Consp13 =~ 1*ConspiracyBeliefs.T13

# Estimate the lagged effects between the within-person centered vawables.
w_Belong13 ~ a3 * w_Belong12 + b3 * w_Consp12 + c * w_Control12 + d3 * w_Meaning12 + e3 * w_Selfesteem12
w_Belong12 ~ a2 * w_Belong11 + b2 * w_Consp11 + c * w_Control11 + d2 * w_Meaning11 + e2 * w_Selfesteem11

w_Consp13 ~ f3 * w_Consp12 + cl * w_Belong12 + cl * w_Control12 + i * w_Meaning12 + j * w_Selfesteem12
w_Consp12 ~ f2 * w_Consp11 + g2 * w_Belong11 + h2 * w_Control11 + i * w_Meaning11 + j * w_Selfesteem11

w_Control13 ~ k3 * w_Control12 + l * w_Belong12 + m3 * w_Consp12 + n * w_Meaning12 + o3 * w_Selfesteem12
w_Control12 ~ k2 * w_Control11 + l * w_Belong11 + m2 * w_Consp11 + n * w_Meaning11 + o2 * w_Selfesteem11

w_Meaning13 ~ p * w_Meaning12 + q * w_Control12  + r3 * w_Belong12  + s * w_Consp12  + t * w_Selfesteem12
w_Meaning12 ~ p * w_Meaning11 + q * w_Control11  + r2 * w_Belong11  + s * w_Consp11  + t * w_Selfesteem11

w_Selfesteem13 ~ u3 * w_Selfesteem12  + v3 * w_Control12  + w3 * w_Belong12  + x * w_Consp12  + y * w_Meaning12
w_Selfesteem12 ~ u2 * w_Selfesteem11  + v2 * w_Control11  + w2 * w_Belong11  + x * w_Consp11  + y * w_Meaning11

  
# Estimate the covariance between the within-person centered variables at the first wave.
# Estimate the covariances between the residuals of the within-person centered variables (the innovations).
w_Belong11 ~~ w_Control11 
w_Belong12 ~~ w_Control12
w_Belong13 ~~ w_Control13

w_Belong11 ~~ w_Meaning11 
w_Belong12 ~~ w_Meaning12
w_Belong13 ~~ w_Meaning13

w_Belong11 ~~ w_Selfesteem11 
w_Belong12 ~~ w_Selfesteem12
w_Belong13 ~~ w_Selfesteem13

w_Belong11 ~~ w_Consp11 
w_Belong12 ~~ w_Consp12
w_Belong13 ~~ w_Consp13

w_Control11 ~~ w_Meaning11 
w_Control12 ~~ w_Meaning12
w_Control13 ~~ w_Meaning13

w_Control11 ~~ w_Selfesteem11 
w_Control12 ~~ w_Selfesteem12
w_Control13 ~~ w_Selfesteem13

w_Control11 ~~ w_Consp11 
w_Control12 ~~ w_Consp12
w_Control13 ~~ w_Consp13

w_Meaning11 ~~ w_Selfesteem11 
w_Meaning12 ~~ w_Selfesteem12
w_Meaning13 ~~ w_Selfesteem13

w_Meaning11 ~~ w_Consp11 
w_Meaning12 ~~ w_Consp12
w_Meaning13 ~~ w_Consp13

w_Selfesteem11 ~~ w_Consp11 
w_Selfesteem12 ~~ w_Consp12
w_Selfesteem13 ~~ w_Consp13

# Estimate the variance and covariance of the random intercepts. 
RI_Belong ~~ RI_Belong
RI_Belong ~~ RI_Control
RI_Belong ~~ RI_Meaning
RI_Belong ~~ RI_Selfesteem
RI_Belong ~~ RI_Consp

RI_Control ~~ RI_Control
RI_Control ~~ RI_Meaning
RI_Control ~~ RI_Selfesteem
RI_Control ~~ RI_Consp

RI_Meaning ~~ RI_Meaning
RI_Meaning ~~ RI_Selfesteem
RI_Meaning ~~ RI_Consp

RI_Selfesteem ~~ RI_Selfesteem
RI_Selfesteem ~~ RI_Consp

RI_Consp ~~ RI_Consp

# Estimate the (residual) variance of the within-person centered variables.
w_Belong11 ~~ w_Belong11 # Variances
w_Belong12 ~~ w_Belong12 
w_Belong13 ~~ w_Belong13 
w_Control11 ~~ w_Control11 
w_Control12 ~~ w_Control12
w_Control13 ~~ w_Control13
w_Meaning11 ~~ w_Meaning11
w_Meaning12 ~~ w_Meaning12 
w_Meaning13 ~~ w_Meaning13 
w_Selfesteem11 ~~ w_Selfesteem11 
w_Selfesteem12 ~~ w_Selfesteem12
w_Selfesteem13 ~~ w_Selfesteem13
w_Consp11 ~~ w_Consp11 
w_Consp12 ~~ w_Consp12
w_Consp13 ~~ w_Consp13
'
RICLPM.CNeeds.4.25.CL1.fit <- lavaan(RICLPM.CNeeds.4.25.CL1, data = data, missing = 'ML', meanstructure = T, int.ov.free = T)
summary(RICLPM.CNeeds.4.25.CL1.fit, standardized = T)

anova(RICLPM.CNeeds.4.25.fit, RICLPM.CNeeds.4.25.CL1.fit)  ## ns. --> paths do not significantly differ


