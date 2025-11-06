#Pooled analyses (reduced data set - no missing values on core variables, N differs slightly 
#with respect to the study-specific sample due to missingness on the bigfive variables)

library(brms)
library(loo)
library(multilevel)


#read in data
data <- read.csv( "Data_merged_nomissings_mod.csv" )

newdata$samplef <- as.factor(newdata$sample)

var_SWLS <- aov(data$SWLS ~ as.factor(data$sample))
var_alone <- aov(data$alone ~ as.factor(data$sample))
var_smalltalk <- aov(data$smalltalk ~ as.factor(data$sample))
var_sm_talk <- aov(data$sm_talk ~ as.factor(data$sample))
var_dc <- aov(data$dc ~ as.factor(data$sample))
var_dc_talk <- aov(data$dc_talk ~ as.factor(data$sample))
var_talk <- aov(data$talk ~ as.factor(data$sample))
         
summary(var_SWLS)
ICC1(var_SWLS)
ICC1(var_alone)
ICC1(var_smalltalk)
ICC1(var_sm_talk)
ICC1(var_dc)
ICC1(var_dc_talk)
ICC1(var_talk)

#Note: all analyses are done only including sample 1,2a(=21),3,4; re-running the syntax will produce 
#slightly different results do to random estimation processes

newdata <- subset(data, sample < 22) 


########################## Model Alone #################################


#Frequentist Linear Regression.
b00lr <- lm(formula = ZSWLS ~ Zalone ,
            data =newdata)
summary(b00lr, waic=T)



#Bayesian linear regression with  zstand
b0lr <- brm(formula = ZSWLS ~ Zalone ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b0lr, waic=T) 

# controling for study membership
b1lr <- brm(formula = SWLS ~ alone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C  + alone*Sam12av34_C + alone*Sam13v2a4_C + alone*Sam14v2a3_C,
          data =newdata, family = gaussian(),
          control = list(adapt_delta = .8),
          warmup = 1000, iter = 10000, chains = 5)


summary(b1lr, waic=T) 

b2lr <- brm(formula = SWLS ~ alone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b2lr, waic=T)

LOO(b1lr, b2lr) 

#REsults
#LOOIC    SE
#b1lr        1444.28 24.90
#b2lr        1448.67 25.21
#b1lr - b2lr   -4.39  6.26
#diff CI includes 0 - take parsimonious model


# controling for (centered) Big5
b3lr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
             cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b3lr, waic=T)

# controling for Big5 + testing moderation: Extraversion
b4elr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*calone ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b4elr, waic=T)

LOO(b4elr, b3lr)

#results:
#             LOOIC    SE
#b4elr        1346.79 30.34
#b3lr         1344.82 30.36
#b4elr - b3lr    1.97  0.89
#Model 3 best model fit

# controling for Big5 + testing moderation: Neuroticism
b4nlr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*calone ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr, waic=T)

LOO( b4nlr, b3lr)

#LOOIC    SE
#b4nlr         LOOIC    SE
#b4nlr        1347.02 30.41
#b3lr         1344.82 30.36
#b4nlr - b3lr    2.20  0.40

# controling for Big5 + testing moderation: Openness
b4olr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*calone ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4olr, waic=T)

LOO( b4olr, b3lr)

#LOOIC    SE
#b4olr        1346.43 30.35
#b3lr         1344.82 30.36
#b4olr - b3lr    1.61  1.12

# controling for Big5 + testing moderation: Agreeableness
b4alr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*calone ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4alr, waic=T)

LOO( b4alr, b3lr)

#LOOIC    SE
#b4alr         1343.42 30.13
#b3lr         1344.82 30.36
#b4alr - b3lr   -1.40  3.82

# controling for Big5 + testing moderation: Contientiousness
b4clr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*calone ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4clr, waic=T)

LOO( b4clr, b3lr)

#LOOIC    SE
#b4clr        1345.44 30.34
#b3lr         1344.82 30.36
#b4clr - b3lr    0.62  1.98

###########Additional exploratory analyses, not reported in main manuscript

#Testing gender as moderator
b5lr <- brm(formula = SWLS ~ alone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + sex_final
            + sex_final*alone,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b5lr, waic=T)

LOO( b5lr, b2lr)
#Some Pareto k diagnostic values are too high - results not trustworthy

#results
## LOOIC    SE
#b2lr        1448.67 25.21
#b5lr        1452.77 25.34
#b5lr - b2lr   4.11  2.68


#Testing age as moderator
b6lr <- brm(formula = SWLS ~ calone + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + cage
            + cage*calone,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b6lr, waic=T)

LOO(b2lr, b6lr)

#LOOIC    SE
#b2lr        1448.67 25.21
#b6lr        1448.83 24.90
#b2lr - b6lr   -0.16  4.22


######################### Model Talk ##############################

b00lr_talk <- lm(formula = ZSWLS ~ Ztalk ,
            data =newdata)
summary(b00lr_talk, waic=T)


b0lr_talk <- brm(formula = ZSWLS ~ Ztalk ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_talk, waic=T) 

b1lr_talk <- brm(formula = SWLS ~ talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C  + talk*Sam12av34_C + talk*Sam13v2a4_C + talk*Sam14v2a3_C,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_talk, waic=T) 

b2lr_talk <- brm(formula = SWLS ~ talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_talk, waic=T)

LOO(b1lr_talk, b2lr_talk) 

#results
#LOO(b1lr_talk, b2lr_talk)
#LOOIC    SE
#b1lr_talk             1437.55 25.31
#b2lr_talk             1440.93 25.48
#b1lr_talk - b2lr_talk   -3.38  6.06


# controling for (centered) Big5
b3lr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_talk, waic=T)


b4lr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*ctalk ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b4lr_talk, waic=T)



LOO(b2lr_talk, b4lr_talk, b3lr_talk)

#results
#                        LOOIC    SE
#b2lr_talk             1440.93 25.48
#b4lr_talk             1344.14 30.82
#b3lr_talk             1341.93 30.79
#b2lr_talk - b4lr_talk   96.79 19.77
#b2lr_talk - b3lr_talk   99.00 19.75
#b4lr_talk - b3lr_talk    2.21  0.63

b4nlr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                   cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*ctalk ,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_talk, waic=T)

LOO( b4nlr_talk, b3lr_talk)

#LOOIC    SE
#b4nlr_talk             1343.97 30.77
#b3lr_talk              1341.93 30.79
#b4nlr_talk - b3lr_talk    2.04  0.72

# controling for Big5 + testing moderation: Openness
b4olr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*ctalk ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_talk, waic=T)

LOO( b4olr_talk, b3lr_talk)

#LOOIC    SE
#b4olr_talk             1340.31 30.30
#b3lr_talk              1341.93 30.79
#b4olr_talk - b3lr_talk   -1.62  4.23

# controling for Big5 + testing moderation: Agreeableness
b4alr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*ctalk ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_talk, waic=T)

LOO( b4alr_talk, b3lr_talk)

#LOOIC    SE
#b4alr_talk             1343.59 30.83
#b3lr_talk              1341.93 30.79
#b4alr_talk - b3lr_talk    1.66  1.55

# controling for Big5 + testing moderation: Contientiousness
b4clr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*ctalk ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_talk, waic=T)

LOO( b4clr_talk, b3lr_talk)

#LOOIC    SE
#b4clr_talk             1342.29 30.58
#b3lr_talk              1341.93 30.79
#b4clr_talk - b3lr_talk    0.36  1.99

###########Additional exploratory analyses, not reported in main manuscript

b5lr_talk <- brm(formula = SWLS ~ talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + sex_final
            + sex_final*talk,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_talk, waic=T)

LOO(b2lr_talk, b5lr_talk)
#Some Pareto k diagnostic values are too high - results not trustworthy

#                        LOOIC    SE
#b2lr_talk             1440.93 25.48
#b5lr_talk             1445.46 25.52
#b2lr_talk - b5lr_talk   -4.53  2.57

b6lr_talk <- brm(formula = SWLS ~ ctalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + cage
                 + cage*ctalk,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_talk, waic=T)

LOO(b2lr_talk, b6lr_talk)

# LOOIC    SE
#b2lr_talk             1440.93 25.48
#b6lr_talk             1444.89 25.48
#b2lr_talk - b6lr_talk   -3.96  0.85


############################## Model Smalltalk############################## 

b00lr_smalltalk <- lm(formula = ZSWLS ~ Zsmalltalk ,
                 data =newdata)
summary(b00lr_smalltalk, waic=T)


b0lr_smalltalk <- brm(formula = ZSWLS ~ Zsmalltalk ,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_smalltalk, waic=T) 


b1lr_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C  + smalltalk*Sam12av34_C + smalltalk*Sam13v2a4_C + smalltalk*Sam14v2a3_C,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_smalltalk, waic=T) 


b2lr_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_smalltalk, waic=T)

LOO(b1lr_smalltalk, b2lr_smalltalk) 

#Results
#                 LOOIC    SE
#b1lr_smalltalk                  1463.23 25.13
#b2lr_smalltalk                  1462.85 24.91
#b1lr_smalltalk - b2lr_smalltalk    0.38  4.64

b3lr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                   cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_smalltalk, waic=T)


b4lr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                   cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*csmalltalk ,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b4lr_smalltalk, waic=T)

LOO(b2lr_smalltalk, b4lr_smalltalk, b3lr_smalltalk)

#results
#                                LOOIC    SE
#b2lr_smalltalk                  1462.85 24.91
#b4lr_smalltalk                  1359.99 29.64
#b3lr_smalltalk                  1358.48 29.74
#b2lr_smalltalk - b4lr_smalltalk  102.87 19.89
#b2lr_smalltalk - b3lr_smalltalk  104.37 20.05
#b4lr_smalltalk - b3lr_smalltalk    1.51  1.08


b4nlr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                        cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*csmalltalk ,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_smalltalk, waic=T)
LOO(b3lr_smalltalk, b4nlr_smalltalk)

# REsults
#                                LOOIC    SE
#b3lr_smalltalk                   1358.48 29.74
#b4nlr_smalltalk                  1359.51 29.64
#b3lr_smalltalk - b4nlr_smalltalk   -1.03  1.75

# controling for Big5 + testing moderation: Openness
b4olr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                    cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*csmalltalk ,
                  data =newdata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_smalltalk, waic=T)

LOO( b4olr_smalltalk, b3lr_smalltalk)

#LOOIC    SE
#b4olr_smalltalk                  1360.00 29.72
#b3lr_smalltalk                   1358.48 29.74
#b4olr_smalltalk - b3lr_smalltalk    1.52  0.81

# controling for Big5 + testing moderation: Agreeableness
b4alr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                    cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*csmalltalk ,
                  data =newdata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_smalltalk, waic=T)

LOO( b4alr_smalltalk, b3lr_smalltalk)

#LOOIC    SE
#b4alr_smalltalk                  1360.27 29.65
#b3lr_smalltalk                   1358.48 29.74
#b4alr_smalltalk - b3lr_smalltalk    1.79  1.35

# controling for Big5 + testing moderation: Contientiousness
b4clr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                    cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*csmalltalk ,
                  data =newdata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_smalltalk, waic=T)

LOO( b4clr_smalltalk, b3lr_smalltalk)

#LOOIC    SE
#b4clr_smalltalk                  1360.12 29.68
#b3lr_smalltalk                   1358.48 29.74
#b4clr_smalltalk - b3lr_smalltalk    1.64  0.31

###########Additional exploratory analyses, not reported in main manuscript

b5lr_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + sex_final
                 + sex_final*smalltalk,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_smalltalk, waic=T)

LOO(b2lr_smalltalk, b5lr_smalltalk)
#Some Pareto k diagnostic values are too high - results not trustworthy

#   LOOIC    SE
#b2lr_smalltalk                  1462.85 24.91
#b5lr_smalltalk                  1467.08 24.98
#b2lr_smalltalk - b5lr_smalltalk   -4.22  2.74


b6lr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + cage
                      + cage*csmalltalk,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_smalltalk, waic=T)

LOO(b2lr_smalltalk, b6lr_smalltalk)

#LOOIC    SE
#b2lr_smalltalk                  1462.85 24.91
#b6lr_smalltalk                  1465.08 24.93
#b2lr_smalltalk - b6lr_smalltalk   -2.23  2.29

################### Model Smalltalk /TALK##############################

b00lr_smtalk <- lm(formula = ZSWLS ~ Zsm_talk ,
                      data =newdata)
summary(b00lr_smtalk, waic=T)


b0lr_smtalk <- brm(formula = ZSWLS ~ Zsm_talk ,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_smtalk, waic=T) 


b1lr_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C  + sm_talk*Sam12av34_C + sm_talk*Sam13v2a4_C + sm_talk*Sam14v2a3_C,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_smtalk, waic=T) 

b2lr_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_smtalk, waic=T)

LOO(b1lr_smtalk, b2lr_smtalk) 


#Results
#LOOIC    SE
#b1lr_smtalk               1462.06 25.45
#b2lr_smtalk               1460.91 25.36
#b1lr_smtalk - b2lr_smtalk    1.15  3.93

b3lr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                        cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_smtalk, waic=T)


b4lr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                        cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*csm_talk ,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b4lr_smtalk, waic=T)



LOO(b2lr_smtalk, b4lr_smtalk, b3lr_smtalk)

#results
# LOOIC    SE
#b2lr_smtalk               1460.91 25.36
#b4lr_smtalk               1364.40 29.87
#b3lr_smtalk               1362.23 29.85
#b2lr_smtalk - b4lr_smtalk   96.51 19.69
#b2lr_smtalk - b3lr_smtalk   98.68 19.67
#b4lr_smtalk - b3lr_smtalk    2.17  1.10

b4nlr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*csm_talk ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_smtalk, waic=T)
LOO(b4nlr_smtalk, b3lr_smtalk)

# LOOIC    SE
#b4nlr_smtalk               1363.33 29.64
#b3lr_smtalk                1362.23 29.85
#b4nlr_smtalk - b3lr_smtalk    1.10  1.91

# controling for Big5 + testing moderation: Openness
b4olr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                         cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*csm_talk ,
                       data =newdata, family = gaussian(),
                       control = list(adapt_delta = .8),
                       warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_smtalk, waic=T)

LOO( b4olr_smtalk, b3lr_smtalk)

#LOOIC    SE
#b4olr_smtalk                1360.64 29.72
#b3lr_smtalk                1362.23 29.85
#b4olr_smtalk - b3lr_smtalk   -1.59  3.41

# controling for Big5 + testing moderation: Agreeableness
b4alr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                         cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*csm_talk ,
                       data =newdata, family = gaussian(),
                       control = list(adapt_delta = .8),
                       warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_smtalk, waic=T)

LOO( b4alr_smtalk, b3lr_smtalk)

#LOOIC    SE
#b4alr_smtalk                1364.17 29.70
#b3lr_smtalk                 1362.23 29.85
#b4alr_smtalk - b3lr_smtalk    1.94  1.86

# controling for Big5 + testing moderation: Contientiousness
b4clr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                         cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*csm_talk ,
                       data =newdata, family = gaussian(),
                       control = list(adapt_delta = .8),
                       warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_smtalk, waic=T)

LOO( b4clr_smtalk, b3lr_smtalk)

#LOOIC    SE
#b4clr_smtalk                1364.15 29.83
#b3lr_smtalk                1362.23 29.85
#b4clr_smtalk - b3lr_smtalk    1.92  0.33

###########Additional exploratory analyses, not reported in main manuscript

b5lr_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + sex_final
                      + sex_final*sm_talk,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_smtalk, waic=T)

LOO(b2lr_smtalk, b5lr_smtalk)
#Some Pareto k diagnostic values are too high - results not trustworthy

#   LOOIC    SE
#b2lr_smtalk               1460.91 25.36
#b5lr_smtalk               1464.36 25.45
#b2lr_smtalk - b5lr_smtalk   -3.45  3.16

b6lr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + cage
                   + cage*csm_talk,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_smtalk, waic=T)

LOO(b2lr_smtalk, b6lr_smtalk)

# LOOIC    SE
#b2lr_smtalk                1460.91 25.36
#b6lr_smtalk               1463.86 25.36
#b2lr_smtalk - b6lr_smtalk   -2.95  2.17

################### DC  ##############################

b00lr_dc <- lm(formula = ZSWLS ~ Zdc ,
                   data =newdata)
summary(b00lr_dc, waic=T)


b0lr_dc <- brm(formula = ZSWLS ~ Zdc ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_dc, waic=T) 



b1lr_dc <- brm(formula = SWLS ~ dc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C  + dc*Sam12av34_C + dc*Sam13v2a4_C + dc*Sam14v2a3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_dc, waic=T) 

b2lr_dc <- brm(formula = SWLS ~ dc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_dc, waic=T)

LOO(b1lr_dc, b2lr_dc) 

#Results
# LOOIC    SE
#b1lr_dc           1441.14 25.94
#b2lr_dc           1437.64 25.85
#b1lr_dc - b2lr_dc    3.50  2.77


b3lr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_dc, waic=T)


b4lr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*cdc ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b4lr_dc, waic=T)



LOO(b2lr_dc, b4lr_dc, b3lr_dc)

#results
# LOOIC    SE
#b2lr_dc           1437.64 25.85
#b4lr_dc           1344.74 29.53
#b3lr_dc           1342.82 29.69
#b2lr_dc - b4lr_dc   92.90 19.20
#b2lr_dc - b3lr_dc   94.83 19.26
#b4lr_dc - b3lr_dc    1.93  1.04

b4nlr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                 cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*cdc ,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_dc, waic=T)



LOO(b3lr_dc, b4nlr_dc)

#     LOOIC    SE
#b3lr_dc            1342.82 29.69
#b4nlr_dc           1344.52 29.64
#b3lr_dc - b4nlr_dc   -1.70  0.72

# controling for Big5 + testing moderation: Openness
b4olr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                      cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*cdc ,
                    data =newdata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_dc, waic=T)

LOO( b4olr_dc, b3lr_dc)

#LOOIC    SE
#b4olr_dc           1344.31 29.60
#b3lr_dc            1342.82 29.69
#b4olr_dc - b3lr_dc    1.50  1.50

# controling for Big5 + testing moderation: Agreeableness
b4alr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                      cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*cdc ,
                    data =newdata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_dc, waic=T)

LOO( b4alr_dc, b3lr_dc)

#LOOIC    SE
#b4alr_dc           1341.54 29.64
#b3lr_dc            1342.82 29.69
#b4alr_dc - b3lr_dc   -1.28  3.10

# controling for Big5 + testing moderation: Contientiousness
b4clr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                      cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*cdc ,
                    data =newdata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_dc, waic=T)

LOO( b4clr_dc, b3lr_dc)

#LOOIC    SE
#b4clr_dc           1344.14 29.61
#b3lr_dc            1342.82 29.69
#b4clr_dc - b3lr_dc    1.32  1.18

###########Additional exploratory analyses, not reported in main manuscript

b5lr_dc <- brm(formula = SWLS ~ dc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + sex_final
                   + sex_final*dc,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_dc, waic=T)
LOO(b2lr_dc, b5lr_dc)

#Some Pareto k diagnostic values are too high - results not trustworthy

#   LOO(b2lr_dc, b5lr_dc)
#LOOIC    SE
#b2lr_dc           1437.64 25.85
#b5lr_dc           1435.55 26.02
#b2lr_dc - b5lr_dc    2.10  5.40



b6lr_dc <- brm(formula = SWLS ~ cdc + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + cage
               + cage*cdc,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_dc, waic=T)
LOO(b6lr_dc, b2lr_dc)

#LOOIC    SE
#b6lr_dc           1441.51 25.83
#b2lr_dc           1437.64 25.85
#b6lr_dc - b2lr_dc    3.87  1.10



################### DC /TALK##############################

b00lr_dctalk <- lm(formula = ZSWLS ~ Zdc_talk ,
                   data =newdata)
summary(b00lr_dctalk, waic=T)


b0lr_dctalk <- brm(formula = ZSWLS ~ Zdc_talk ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_dctalk, waic=T) 


b1lr_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C  + dc_talk*Sam12av34_C + dc_talk*Sam13v2a4_C + dc_talk*Sam14v2a3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_dctalk, waic=T) 

b2lr_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_dctalk, waic=T)

LOO(b1lr_dctalk, b2lr_dctalk) 

#LOOIC    SE
#b1lr_dctalk               1459.75 25.63
#b2lr_dctalk               1454.00 25.69
#b1lr_dctalk - b2lr_dctalk    5.76  0.66

b3lr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                 cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_dctalk, waic=T)


b4lr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                 cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*cdc_talk ,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b4lr_dctalk, waic=T)



LOO(b2lr_dctalk, b4lr_dctalk, b3lr_dctalk)

#results
#LOOIC    SE
#b2lr_dctalk                1454.00 25.69
#b4lr_dctalk               1357.95 29.45
#b3lr_dctalk               1355.99 29.62
#b2lr_dctalk - b4lr_dctalk   96.05 19.49
#b2lr_dctalk - b3lr_dctalk   98.01 19.58
#b4lr_dctalk - b3lr_dctalk    1.96  1.36

b4nlr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*cdc_talk ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_dctalk, waic=T)



LOO( b4nlr_dctalk, b3lr_dctalk)

# LOOIC    SE
#b4nlr_dctalk                1358.14 29.63
#b3lr_dctalk                1355.99 29.62
#b4nlr_dctalk - b3lr_dctalk    2.15  0.56

# controling for Big5 + testing moderation: Openness
b4olr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                  cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*cdc_talk ,
                data =newdata, family = gaussian(),
                control = list(adapt_delta = .8),
                warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_dctalk, waic=T)

LOO( b4olr_dctalk, b3lr_dctalk)

#LOOIC    SE
#b4olr_dctalk                1357.86 29.58
#b3lr_dctalk                1355.99 29.62
#b4olr_dctalk - b3lr_dctalk    1.87  0.24

# controling for Big5 + testing moderation: Agreeableness
b4alr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                  cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*cdc_talk ,
                data =newdata, family = gaussian(),
                control = list(adapt_delta = .8),
                warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_dctalk, waic=T)

LOO( b4alr_dctalk, b3lr_dctalk)

#LOOIC    SE
#b4alr_dctalk                1356.99 29.75
#b3lr_dctalk                1355.99 29.62
#b4alr_dctalk - b3lr_dctalk    1.01  2.18

# controling for Big5 + testing moderation: Contientiousness
b4clr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                  cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*cdc_talk ,
                data =newdata, family = gaussian(),
                control = list(adapt_delta = .8),
                warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_dctalk, waic=T)

LOO( b4clr_dctalk, b3lr_dctalk)

#LOOIC    SE
#b4clr_dctalk              1357.80 29.58
#b3lr_dctalk                1355.99 29.62
#b4clr_dctalk - b3lr_dctalk    1.81  0.37

###########Additional exploratory analyses, not reported in main manuscript

b5lr_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + sex_final
               + sex_final*dc_talk,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_dctalk, waic=T)
LOO(b2lr_dctalk, b5lr_dctalk)

#  LOOIC    SE
#b2lr_dctalk               1454.00 25.69
#b5lr_dctalk               1450.76 25.94
#b2lr_dctalk - b5lr_dctalk    3.23  5.80

#Some Pareto k diagnostic values are too high - results not trustworthy


b6lr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + cage
+ cage*cdc_talk,
data =newdata, family = gaussian(),
control = list(adapt_delta = .8),
warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_dctalk, waic=T)
LOO(b6lr_dctalk, b2lr_dctalk)

#LOOIC    SE
#b6lr_dctalk               1456.68 25.76
#b2lr_dctalk               1454.00 25.69
#b6lr_dctalk - b2lr_dctalk    2.68  2.38

################### Quantity and Quality entered at the same time ##############################



b2lr_qualt <- brm(formula = SWLS ~ dc + talk + smalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_qualt, waic=T)



b3lr_qualt <- brm(formula = SWLS ~ dc + talk + smalltalk + Sam12av34_C + Sam13v2a4_C + Sam14v2a3_C + 
                 BFIEv1_mean + BFIAv1_mean + BFICv1_mean + BFINv1_mean +BFIOv1_mean,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_qualt, waic=T)



