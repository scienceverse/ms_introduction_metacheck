#Pooled analyses (reduced data set - no missing values on core variables, N differs slightly 
#with respect to the study-specific sample due to missingness on the bigfive variables)

library(brms)
library(loo)
library(multilevel)


#read in data
data <- read.csv( "Data_merged_nomissings_mod2b.csv" )

samplef <- as.factor(data$sample)

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

#Note: all analyses are done only including sample 1,2b(=2b),3,4; re-running the syntax will produce 
#slightly different results do to random estimation processes

newdata <- subset(data, sample < 10 | sample >= 22, ) 

#read in data for reduced moderation analyses (no missings age and sex)
reddata <- read.csv( "Data_merged_nomissings_mod2b_agesex.csv" )

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
b1lr <- brm(formula = SWLS ~ alone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C  + alone*Sam12bv34_C + alone*Sam13v2b4_C + alone*Sam14v2b3_C,
          data =newdata, family = gaussian(),
          control = list(adapt_delta = .8),
          warmup = 1000, iter = 10000, chains = 5)


summary(b1lr, waic=T) 

b2lr <- brm(formula = SWLS ~ alone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b2lr, waic=T)

LOO(b1lr, b2lr) 

#LOOIC    SE
#b1lr        1450.03 26.41
#b2lr        1455.52 26.29
#b1lr - b2lr   -5.49  6.22


# controling for Big5
b3lr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
             cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b3lr, waic=T)

# controling for Big5 + testing moderation: Extraversion
b4elr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*calone ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b4elr, waic=T)

LOO(b4elr, b3lr)

#results:
#             LOOIC    SE
#b4elr        1363.24 30.19
#b3lr         1361.36 30.23
#b4elr - b3lr    1.88  0.78

# controling for Big5 + testing moderation: Neuroticism
b4nlr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*calone ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr, waic=T)

LOO( b4nlr, b3lr)

#LOOIC    SE
#b4nlr        1363.38 30.22
#b3lr         1361.36 30.23
#b4nlr - b3lr    2.02  0.56

# controling for Big5 + testing moderation: Openness
b4olr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*calone ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4olr, waic=T)

LOO( b4olr, b3lr)

#LOOIC    SE
#b4olr        1363.21 30.18
#b3lr         1361.36 30.23
#b4olr - b3lr    1.85  0.36

# controling for Big5 + testing moderation: Agreeableness
b4alr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*calone ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4alr, waic=T)

LOO( b4alr, b3lr)

#LOOIC    SE
#b4alr       1358.12 29.89
#b3lr         1361.36 30.23
#b4alr - b3lr   -3.24  4.78

# controling for Big5 + testing moderation: Contientiousness
b4clr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*calone ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4clr, waic=T)

LOO( b4clr, b3lr)

#LOOIC    SE
#b4clr       1363.24 30.22
#b3lr         1361.36 30.23
#b4clr - b3lr    1.88  0.56



###############Analyses not reported in Paper

#Testing gender as moderator with reduced dataset

b2lrred <- brm(formula = SWLS ~ alone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
            data =reddata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b2lrred, waic=T)

b5lr <- brm(formula = SWLS ~ alone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + sex_final
            + sex_final*alone,
            data =reddata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b5lr, waic=T)

LOO( b5lr, b2lrred)

#results
## LOOIC    SE
#b5lr         1440.09 25.66
#b2lrred        1442.17 25.99
#b5lr - b2lrred   -2.08  4.99


#Testing age as moderator
b6lr <- brm(formula = SWLS ~ calone + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + cage
            + cage*calone,
            data =reddata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b6lr, waic=T)

LOO(b2lrred, b6lr)

#LOOIC    SE
#b2lr           1442.17 25.99
#b6lr           1443.81 25.94
#b2lrred - b6lr   -1.64  3.34


######################### Model Talk ##############################

b00lr_talk <- lm(formula = ZSWLS ~ Ztalk ,
            data =newdata)
summary(b00lr_talk, waic=T)


b0lr_talk <- brm(formula = ZSWLS ~ Ztalk ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_talk, waic=T) 

b1lr_talk <- brm(formula = SWLS ~ talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C  + talk*Sam12bv34_C + talk*Sam13v2b4_C + talk*Sam14v2b3_C,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_talk, waic=T) 

b2lr_talk <- brm(formula = SWLS ~ talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_talk, waic=T)

LOO(b1lr_talk, b2lr_talk) 

# LOOIC    SE
#b1lr_talk             1444.99 26.85
#b2lr_talk             1452.05 26.40
#b1lr_talk - b2lr_talk   -7.06  7.44


b3lr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_talk, waic=T)


b4elr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
              cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*ctalk ,
            data =newdata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b4elr_talk, waic=T)



LOO(b4elr_talk, b3lr_talk)

#results
#                      LOOIC    SE
#b4elr_talk             1361.21 30.53
#b3lr_talk              1358.87 30.49
#b4elr_talk - b3lr_talk    2.34  0.32


b4nlr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                   cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*ctalk ,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_talk, waic=T)

LOO( b4nlr_talk, b3lr_talk)

#LOOIC    SE
#b4nlr_talk             1361.22 30.59
#b3lr_talk              1358.87 30.49
#b4nlr_talk - b3lr_talk    2.35  0.34



# controling for Big5 + testing moderation: Openness
b4olr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*ctalk ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_talk, waic=T)

LOO( b4olr_talk, b3lr_talk)

#LOOIC    SE
#b4olr_talk             1358.10 30.02
#b3lr_talk              1358.87 30.49
#b4olr_talk - b3lr_talk   -0.77  3.86

# controling for Big5 + testing moderation: Agreeableness
b4alr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*ctalk ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_talk, waic=T)

LOO( b4alr_talk, b3lr_talk)

#LOOIC    SE
#b4alr_talk             1359.46 30.76
#b3lr_talk              1358.87 30.49
#b4alr_talk - b3lr_talk    0.59  2.62

# controling for Big5 + testing moderation: Contientiousness
b4clr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
               cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*ctalk ,
             data =newdata, family = gaussian(),
             control = list(adapt_delta = .8),
             warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_talk, waic=T)

LOO( b4clr_talk, b3lr_talk)

#              LOOIC    SE
#b4clr_talk             1360.42 30.33
#b3lr_talk              1358.87 30.49
#b4clr_talk - b3lr_talk    1.56  1.12


###############Analyses not reported in Paper

b2lrred_talk <- brm(formula = SWLS ~ talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
               data =reddata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b2lrred_talk, waic=T)

b5lr_talk <- brm(formula = SWLS ~ talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + sex_final
            + sex_final*talk,
            data =reddata, family = gaussian(),
            control = list(adapt_delta = .8),
            warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_talk, waic=T)

LOO(b2lrred_talk, b5lr_talk)

#                       LOOIC    SE
#b2lrred_talk             1438.88 26.09
#b5lr_talk                1437.48 25.86
#b2lrred_talk - b5lr_talk    1.41  4.77

b6lr_talk <- brm(formula = SWLS ~ ctalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + cage
                 + cage*ctalk,
                 data =reddata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_talk, waic=T)

LOO(b2lrred_talk, b6lr_talk)

# LOOIC    SE
# b2lrred_talk             1438.88 26.09
#b6lr_talk                1443.19 26.12
#b2lrred_talk - b6lr_talk   -4.31  0.44


############################## Model Smalltalk############################## 

b00lr_smalltalk <- lm(formula = ZSWLS ~ Zsmalltalk ,
                 data =newdata)
summary(b00lr_smalltalk, waic=T)


b0lr_smalltalk <- brm(formula = ZSWLS ~ Zsmalltalk ,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_smalltalk, waic=T) 


b1lr_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C  + smalltalk*Sam12bv34_C + smalltalk*Sam13v2b4_C + smalltalk*Sam14v2b3_C,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_smalltalk, waic=T) 


b2lr_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_smalltalk, waic=T)

LOO(b1lr_smalltalk, b2lr_smalltalk) 

#Results
# LOOIC    SE
#b1lr_smalltalk                  1469.22 26.38
#b2lr_smalltalk                  1468.38 26.09
#b1lr_smalltalk - b2lr_smalltalk    0.84  4.19


b3lr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                   cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_smalltalk, waic=T)


b4elr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                   cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*csmalltalk ,
                 data =newdata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b4elr_smalltalk, waic=T)

LOO( b4elr_smalltalk, b3lr_smalltalk)

#results
#                                 LOOIC    SE
#b4elr_smalltalk                 1375.72 29.80
#b3lr_smalltalk                   1373.70 29.83
#b4elr_smalltalk - b3lr_smalltalk    2.02  0.48


b4nlr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                        cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*csmalltalk ,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_smalltalk, waic=T)
LOO(b3lr_smalltalk, b4nlr_smalltalk)

# REsults
#                                LOOIC    SE
#b3lr_smalltalk                   1373.70 29.83
#b4nlr_smalltalk                  1375.87 29.80
#b3lr_smalltalk - b4nlr_smalltalk   -2.17  0.90

# controling for Big5 + testing moderation: Openness
b4olr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                    cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*csmalltalk ,
                  data =newdata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_smalltalk, waic=T)

LOO( b4olr_smalltalk, b3lr_smalltalk)

#LOOIC    SE
#b4olr_smalltalk                   1375.65 29.83
#b3lr_smalltalk                   1373.70 29.83
#b4olr_smalltalk - b3lr_smalltalk    1.95  0.74                



# controling for Big5 + testing moderation: Agreeableness
b4alr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                    cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*csmalltalk ,
                  data =newdata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_smalltalk, waic=T)

LOO( b4alr_smalltalk, b3lr_smalltalk)

#LOOIC    SE
#b4alr_smalltalk                   1374.57 29.84
#b3lr_smalltalk                   1373.70 29.83
#b4alr_smalltalk - b3lr_smalltalk    0.87  2.53



# controling for Big5 + testing moderation: Contientiousness
b4clr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                    cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*csmalltalk ,
                  data =newdata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_smalltalk, waic=T)

LOO( b4clr_smalltalk, b3lr_smalltalk)

#LOOIC    SE
#b4clr_smalltalk                  1375.58 29.95
#b3lr_smalltalk                   1373.70 29.83
#b4clr_smalltalk - b3lr_smalltalk    1.88  0.96



###############Analyses not reported in Paper

b2lrred_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                    data =reddata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b2lrred_smalltalk, waic=T)

b5lr_smalltalk <- brm(formula = SWLS ~ smalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + sex_final
                 + sex_final*smalltalk,
                 data =reddata, family = gaussian(),
                 control = list(adapt_delta = .8),
                 warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_smalltalk, waic=T)

LOO(b2lrred_smalltalk, b5lr_smalltalk)

#   LOOIC    SE
#b2lrred_smalltalk                  1454.57 25.78
#b5lr_smalltalk                     1450.41 25.76
#b2lrred_smalltalk - b5lr_smalltalk    4.16  5.73


b6lr_smalltalk <- brm(formula = SWLS ~ csmalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + cage
                      + cage*csmalltalk,
                      data =reddata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_smalltalk, waic=T)

LOO(b2lrred_smalltalk, b6lr_smalltalk)

#LOOIC    SE
#b2lrred_smalltalk                   1454.57 25.78
#b6lr_smalltalk                     1457.26 25.79
#b2lrred_smalltalk - b6lr_smalltalk  -2.69  2.18

################### Model Smalltalk /TALK##############################

b00lr_smtalk <- lm(formula = ZSWLS ~ Zsm_talk ,
                      data =newdata)
summary(b00lr_smtalk, waic=T)


b0lr_smtalk <- brm(formula = ZSWLS ~ Zsm_talk ,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_smtalk, waic=T) 


b1lr_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C  + sm_talk*Sam12bv34_C + sm_talk*Sam13v2b4_C + sm_talk*Sam14v2b3_C,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_smtalk, waic=T) 

b2lr_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_smtalk, waic=T)

LOO(b1lr_smtalk, b2lr_smtalk) 


#Results
#LOOIC    SE
#b1lr_smtalk               1469.55 26.52
#b2lr_smtalk               1467.56 26.41
#b1lr_smtalk - b2lr_smtalk    2.00  3.81


b3lr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                        cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_smtalk, waic=T)


b4elr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                        cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*csm_talk ,
                      data =newdata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b4elr_smtalk, waic=T)



LOO(b4elr_smtalk, b3lr_smtalk)

#results
# LOOIC    SE
#b4elr_smtalk               1378.31 29.89
#b3lr_smtalk                1376.15 29.88
#b4elr_smtalk - b3lr_smtalk    2.16  1.17



b4nlr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*csm_talk ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_smtalk, waic=T)
LOO(b4nlr_smtalk, b3lr_smtalk)

# LOOIC    SE
#b4nlr_smtalk               1378.02 29.77
#b3lr_smtalk                1376.15 29.88
#b4nlr_smtalk - b3lr_smtalk    1.87  1.17

# controling for Big5 + testing moderation: Openness
b4olr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                         cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*csm_talk ,
                       data =newdata, family = gaussian(),
                       control = list(adapt_delta = .8),
                       warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_smtalk, waic=T)

LOO( b4olr_smtalk, b3lr_smtalk)

#LOOIC    SE
#b4olr_smtalk               1374.28 29.75
#b3lr_smtalk                1376.15 29.88
#b4olr_smtalk - b3lr_smtalk   -1.87  3.87

# controling for Big5 + testing moderation: Agreeableness
b4alr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                         cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*csm_talk ,
                       data =newdata, family = gaussian(),
                       control = list(adapt_delta = .8),
                       warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_smtalk, waic=T)

LOO( b4alr_smtalk, b3lr_smtalk)

#LOOIC    SE
#b4alr_smtalk               1377.13 29.73
#b3lr_smtalk                1376.15 29.88
#b4alr_smtalk - b3lr_smtalk    0.98  2.81

# controling for Big5 + testing moderation: Contientiousness
b4clr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                         cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*csm_talk ,
                       data =newdata, family = gaussian(),
                       control = list(adapt_delta = .8),
                       warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_smtalk, waic=T)

LOO( b4clr_smtalk, b3lr_smtalk)

#LOOIC    SE
#b4clr_smtalk               1378.44 29.96
#b3lr_smtalk                1376.15 29.88
#b4clr_smtalk - b3lr_smtalk    2.29  0.62

###############Analyses not reported in Paper

b2lrred_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                         data =reddata, family = gaussian(),
                         control = list(adapt_delta = .8),
                         warmup = 1000, iter = 10000, chains = 5)

summary(b2lrred_smtalk, waic=T)


b5lr_smtalk <- brm(formula = SWLS ~ sm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + sex_final
                      + sex_final*sm_talk,
                      data =reddata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_smtalk, waic=T)

LOO(b2lrred_smtalk, b5lr_smtalk)

#   LOOIC    SE
#b2lrred_smtalk               1453.89 26.09
#b5lr_smtalk                  1449.54 26.03
#b2lrred_smtalk - b5lr_smtalk    4.35  5.76


b6lr_smtalk <- brm(formula = SWLS ~ csm_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + cage
                   + cage*csm_talk,
                   data =reddata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_smtalk, waic=T)

LOO(b2lrred_smtalk, b6lr_smtalk)

# LOOIC    SE
#b2lrred_smtalk               1453.89 26.09
#b6lr_smtalk                  1457.84 26.19
#b2lrred_smtalk - b6lr_smtalk   -3.95  1.57


################### DC  ##############################

b00lr_dc <- lm(formula = ZSWLS ~ Zdc ,
                   data =newdata)
summary(b00lr_dc, waic=T)


b0lr_dc <- brm(formula = ZSWLS ~ Zdc ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_dc, waic=T) 



b1lr_dc <- brm(formula = SWLS ~ dc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C  + dc*Sam12bv34_C + dc*Sam13v2b4_C + dc*Sam14v2b3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_dc, waic=T) 

b2lr_dc <- brm(formula = SWLS ~ dc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_dc, waic=T)

LOO(b1lr_dc, b2lr_dc) 

#Results
# LOOIC    SE
# b1lr_dc           1449.68 27.45
#b2lr_dc           1449.73 26.97
#b1lr_dc - b2lr_dc   -0.05  5.55


b3lr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_dc, waic=T)


b4elr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*cdc ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b4elr_dc, waic=T)



LOO( b4elr_dc, b3lr_dc)

#results
# LOOIC    SE
#b4elr_dc           1358.1 29.59
#b3lr_dc            1356.4 29.73
#b4elr_dc - b3lr_dc    1.7  1.23
#

b4nlr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                 cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*cdc ,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_dc, waic=T)



LOO(b3lr_dc, b4nlr_dc)

#     LOOIC    SE
#b3lr_dc            1356.40 29.73
#b4nlr_dc           1358.07 29.69
#b3lr_dc - b4nlr_dc   -1.66  1.40

# controling for Big5 + testing moderation: Openness
b4olr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                      cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*cdc ,
                    data =newdata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_dc, waic=T)

LOO( b4olr_dc, b3lr_dc)

#LOOIC    SE
#b4olr_dc           1358.20 29.72
#b3lr_dc            1356.40 29.73
#b4olr_dc - b3lr_dc    1.79  1.07

# controling for Big5 + testing moderation: Agreeableness
b4alr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                      cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*cdc ,
                    data =newdata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_dc, waic=T)

LOO( b4alr_dc, b3lr_dc)

#LOOIC    SE
#b4alr_dc           1351.49 29.75
#b3lr_dc            1356.40 29.73
#b4alr_dc - b3lr_dc   -4.91  4.83

# controling for Big5 + testing moderation: Contientiousness
b4clr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                      cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*cdc ,
                    data =newdata, family = gaussian(),
                    control = list(adapt_delta = .8),
                    warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_dc, waic=T)

LOO( b4clr_dc, b3lr_dc)

#LOOIC    SE
#b4clr_dc           1357.85 29.62
#b3lr_dc            1356.40 29.73
#b4clr_dc - b3lr_dc    1.44  1.31

###############Analyses not reported in Paper

b2lrred_dc <- brm(formula = SWLS ~ dc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                      data =reddata, family = gaussian(),
                      control = list(adapt_delta = .8),
                      warmup = 1000, iter = 10000, chains = 5)

summary(b2lrred_dc, waic=T)



b5lr_dc <- brm(formula = SWLS ~ dc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + sex_final
                   + sex_final*dc,
                   data =reddata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_dc, waic=T)
LOO(b2lrred_dc, b5lr_dc)

#   LOO(b2lr_dc, b5lr_dc)
#LOOIC    SE
#b2lrred_dc           1436.38 26.72
#b5lr_dc              1434.15 26.66
#b2lrred_dc - b5lr_dc    2.23  5.03



b6lr_dc <- brm(formula = SWLS ~ cdc + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + cage
               + cage*cdc,
               data =reddata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_dc, waic=T)
LOO(b6lr_dc, b2lrred_dc)

#LOOIC    SE
#b6lr_dc              1439.68 26.64
#b2lrred_dc           1436.38 26.72
#b6lr_dc - b2lrred_dc    3.30  1.95



################### DC /TALK##############################

b00lr_dctalk <- lm(formula = ZSWLS ~ Zdc_talk ,
                   data =newdata)
summary(b00lr_dctalk, waic=T)


b0lr_dctalk <- brm(formula = ZSWLS ~ Zdc_talk ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b0lr_dctalk, waic=T) 


b1lr_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C  + dc_talk*Sam12bv34_C + dc_talk*Sam13v2b4_C + dc_talk*Sam14v2b3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)


summary(b1lr_dctalk, waic=T) 

b2lr_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_dctalk, waic=T)

LOO(b1lr_dctalk, b2lr_dctalk) 

#LOOIC    SE
#b1lr_dctalk               1466.87 26.85
#b2lr_dctalk               1461.14 26.77
#b1lr_dctalk - b2lr_dctalk    5.74  1.05


b3lr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                 cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_dctalk, waic=T)


b4elr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                 cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIEv1_mean*cdc_talk ,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b4elr_dctalk, waic=T)



LOO(b4elr_dctalk, b3lr_dctalk)

#results
#LOOIC    SE
#b4elr_dctalk               1370.01 29.47
#b3lr_dctalk                1368.03 29.61
#b4elr_dctalk - b3lr_dctalk    1.99  1.25

b4nlr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                     cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFINv1_mean*cdc_talk ,
                   data =newdata, family = gaussian(),
                   control = list(adapt_delta = .8),
                   warmup = 1000, iter = 10000, chains = 5)

summary(b4nlr_dctalk, waic=T)



LOO( b4nlr_dctalk, b3lr_dctalk)

# LOOIC    SE
#b4nlr_dctalk               1370.18 29.61
#b3lr_dctalk                1368.03 29.61
#b4nlr_dctalk - b3lr_dctalk    2.15  0.72

# controling for Big5 + testing moderation: Openness
b4olr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                  cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIOv1_mean*cdc_talk ,
                data =newdata, family = gaussian(),
                control = list(adapt_delta = .8),
                warmup = 1000, iter = 10000, chains = 5)

summary(b4olr_dctalk, waic=T)

LOO( b4olr_dctalk, b3lr_dctalk)

#LOOIC    SE
#b4olr_dctalk               1370.04 29.59
#b3lr_dctalk                1368.03 29.61
#b4olr_dctalk - b3lr_dctalk    2.01  0.31

# controling for Big5 + testing moderation: Agreeableness
b4alr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                  cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFIAv1_mean*cdc_talk ,
                data =newdata, family = gaussian(),
                control = list(adapt_delta = .8),
                warmup = 1000, iter = 10000, chains = 5)

summary(b4alr_dctalk, waic=T)

LOO( b4alr_dctalk, b3lr_dctalk)

#LOOIC    SE
#b4alr_dctalk               1368.46 29.80
#b3lr_dctalk                1368.03 29.61
#b4alr_dctalk - b3lr_dctalk    0.43  2.59

# controling for Big5 + testing moderation: Contientiousness
b4clr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                  cBFIEv1_mean + cBFIAv1_mean + cBFICv1_mean + cBFINv1_mean +cBFIOv1_mean + cBFICv1_mean*cdc_talk ,
                data =newdata, family = gaussian(),
                control = list(adapt_delta = .8),
                warmup = 1000, iter = 10000, chains = 5)

summary(b4clr_dctalk, waic=T)

LOO( b4clr_dctalk, b3lr_dctalk)

#LOOIC    SE
#b4clr_dctalk               1370.09 29.62
#b3lr_dctalk                1368.03 29.61
#4clr_dctalk - b3lr_dctalk    2.06  0.39


###############Analyses not reported in Paper

b2lrred_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
                  data =reddata, family = gaussian(),
                  control = list(adapt_delta = .8),
                  warmup = 1000, iter = 10000, chains = 5)

summary(b2lrred_dctalk, waic=T)

b5lr_dctalk <- brm(formula = SWLS ~ dc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + sex_final
               + sex_final*dc_talk,
               data =reddata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b5lr_dctalk, waic=T)
LOO(b2lrred_dctalk, b5lr_dctalk)

#  LOOIC    SE
#b2lrred_dctalk               1447.41 26.49
#b5lr_dctalk                  1442.70 26.48
#b2lrred_dctalk - b5lr_dctalk    4.71  5.87

b6lr_dctalk <- brm(formula = SWLS ~ cdc_talk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + cage
+ cage*cdc_talk,
data =reddata, family = gaussian(),
control = list(adapt_delta = .8),
warmup = 1000, iter = 10000, chains = 5)

summary(b6lr_dctalk, waic=T)
LOO(b6lr_dctalk, b2lrred_dctalk)

#LOOIC    SE
#b6lr_dctalk                  1450.68 26.48
#b2lrred_dctalk               1447.41 26.49
#b6lr_dctalk - b2lrred_dctalk    3.28  1.97

################### Quantity and Quality entered at the same time - not reported in manuscript ##############################



b2lr_qualt <- brm(formula = SWLS ~ dc + talk + smalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b2lr_qualt, waic=T)



b3lr_qualt <- brm(formula = SWLS ~ dc + talk + smalltalk + Sam12bv34_C + Sam13v2b4_C + Sam14v2b3_C + 
                 BFIEv1_mean + BFIAv1_mean + BFICv1_mean + BFINv1_mean +BFIOv1_mean,
               data =newdata, family = gaussian(),
               control = list(adapt_delta = .8),
               warmup = 1000, iter = 10000, chains = 5)

summary(b3lr_qualt, waic=T)



