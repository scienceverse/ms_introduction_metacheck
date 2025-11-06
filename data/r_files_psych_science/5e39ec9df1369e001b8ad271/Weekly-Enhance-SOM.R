
setwd("~/Documents/Scholarship/Manuscripts/Georgetown/Weekly Paper/ANALYSES/Weekly")

data<-read.csv("weekly-datalong-wMain.csv")
describe(data)
d<-data


d$time
d$time1<-d$time+9
d$time1
dd<-d

#model structure
VarCorr(m) # covaraince structure of random effects
vcov(m) # residuals covariance structure (of fixed effects)

#DIASGNOSTICS
require(car)
require(MASS)

SickF<-na.omit(d$SickF)
SickF<-as.vector(SickF)
sickcat<-as.vector(na.omit(d$sickcat))

#Categorizing
outlier_values <- boxplot.stats(d$SickF)$out  # outlier values.
outlier_values

d$SickSqrt<-sqrt(d$SickF)
outlier_values <- boxplot.stats(d$SickSqrt)$out  # outlier values.
outlier_values

d$SickLog10<-log10(d$SickF+1)
outlier_values <- boxplot.stats(d$SickLog10)$out  # outlier values.
outlier_values

d$sickcat <- d$SickF
d$sickcat[d$SickF > 5] <- 5

sickcat1<-d$sickcat+1

#selecting best distribution for outcome
qqp(dd$SickF, "norm") #against normal
qqp(dd$SickF, "lnorm") #against lognormal
poisson <- fitdistr(dd$SickF, "Poisson")
lambda<-poisson$estimate
qqp(dd$SickF, "pois", lambda = lambda)
poisson$estimate

qqp(sickcat, "norm") #against normal
qqp(sickcat, "lnorm") #against lognormal
nbinom <- fitdistr(sickcat, "negative binomial") #negative binomial
qqp(sickcat, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(dd$sickcat, "Poisson")
lambda<-poisson$estimate
qqp(dd$sickcat, "pois", lambda = lambda)
poisson$estimate

#diagnostics
plot(m) #residual plot
require(lattice)
qqmath(m) #qq plot of the residuals (assumption of normality)

install.packages("DHARMa")
require(DHARMa)
dd<-d[!is.na(d$SickF),] #create a dd with no missing SickF values
#resisuals
plotConventionalResiduals(m) #conventional residual plot
simulationOutput <- simulateResiduals(m, n = 250) #simulation
plotSimulatedResiduals(simulationOutput)
testUniformity(simulationOutput)
testOverdispersion(simulationOutput)
testZeroInflation(simulationOutput)
testTemporalAutocorrelation(simulationOutput, time = dd$time)

#missing predictors
dd$Condition<-as.integer(dd$Condition)
plotResiduals(dd$Condition,  simulationOutput$scaledResiduals)
plotResiduals(dd$time,  simulationOutput$scaledResiduals) #expect flat lines for no misspesification
dd$Inter<-dd$Condition*dd$time
plotResiduals(dd$Inter,  simulationOutput$scaledResiduals) 

#WITHIN_BETWEEN

library("psych")
names(d)
myvars <- c("ID", "time", "Condition", "SickF", "SickFt","PA","ff", "LS","Meaning", "SickFt.1",  "PA.1", "ff.1", "LS.1")
mydata<-d[myvars]
names(mydata)

#creating a data frame containing all correlaitons, p's, etc.)
correlations <- statsBy(mydata, "ID", cors = TRUE, method="pearson", na.rm=TRUE)

# printing the simple within and between correlations
print(correlations,short=FALSE)


#print and create data frame for r-between
rbg <- data.frame(correlations$rbg)
rbg
write.csv(rbg, "between cor.csv")
#print and create data frame for r-within
rwg <- data.frame(correlations$rwg)
rwg
write.csv(rwg, "within cor.csv")
#print and create data frame for pvalue-between
pbg <- data.frame(correlations$pbg)
round(pbg, 3)
#print and create data frame for pvalue-within
pwg <- data.frame(correlations$pwg)
round(pwg, 4)
m <-as.data.frame(correlations$mean)
m
write.csv(m, "means-by-subject.csv")
mc<-describeBy(m, m$Condition)
write.csv(mc$`1`, "means-by-subject_C.csv")
write.csv(mc$`2`, "means-by-subject_T.csv")


# #MOdel Selection
# 
# f_TTestPost <- sim_formula("y ~ treatment",
#                     test = "treatment",
#                     data_transform = transform_to_posttest)
# f_ANCOVA <- sim_formula("y ~ treatment + pretest",
#                         test = "treatment",
#                         data_transform = transform_to_posttest)
# f0 <- sim_formula("y ~ time * treatment + (1 | subject)")
# f1 <- sim_formula("y ~ time * treatment +  (1 | subject) + (0 + time | subject)")
# f2 <- sim_formula("y ~ time * treatment + (1 + time | subject)")
# f <- sim_formula_compare("subject-intercept" = f0, 
#                          "subject-slope diag" = f1, 
#                          "subject-slope full" = f2,
#                          "ttest" = f_TTestPost)
# 
# tests <-  list(
#                "subject-intercept" = "time:treatment",
#                "subject-slope diag" = "time:treatment",
#                "subject-slope full" = "time:treatment",
#                "ttest" = "treatment")
# 
# 
# res <- simulate(p, 
#                 formula = f, 
#                 nsim = 1000, 
#                 satterthwaite = TRUE, 
#                 cores = 16,
#                 CI = FALSE,
#                 family ="poisson")
# 
# 
# x <- summary(res, model_selection = "FW", LRT_alpha = 0.05)
# x
# x1 <- summary(res, para = tests)
# x1
# 
# p1 <- update(p, effect_size = c(0,cohend(0.2),cohend(0.35),cohend(0.5),cohend(0.65)), 
#              dropout = dropout_weibull(proportion = .20, rate = 1))
# get_power(p1)
# res_power <- simulate(p1, 
#                       formula = f, 
#                       nsim = 100, 
#                       cores = 16,
#                       satterthwaite = TRUE, 
#                       CI = FALSE)
# x <- summary(res_power, 
#              model_selection = "FW", 
#              LRT_alpha = 0.05,
#              para = tests
#              )
# x
# x1 <- summary(res_power,
#               para = tests
#               )
# x1




#Cohen D (classic)
(means <- with(d, tapply(Meaning, Condition, mean, na.rm=TRUE)))
md <- diff(means)
md
(cohend <- md / with(d, sqrt(mean(tapply(Meaning, Condition, var, na.rm=TRUE)))))

library(apa)
d10<-d[d$time == 0,]
cohens_d(d, dv=SickFt, iv = Condition, paired = FALSE, na.rm = T)
cohens_d(d10, dv=SickFt, iv = Condition, paired = FALSE, na.rm = T)


# m2 <-glmer(SickFt ~ time + (time| ID), d, na.action = na.omit,
#            family = 'poisson')
# tab_model(m2, 
#           show.aic = T, show.dev = T, show.obs=T)
# m3 <-glmer(SickFt ~ time * Condition + (time| ID), d, na.action = na.omit,
#            family = 'poisson')
# tab_model(m3, 
#           show.aic = T, show.dev = T, show.obs=T)
# summary(m3)
# anova(m2,m3)







#######################ADDITIONAL ANALYSES##################

#MEDIATION EFFECT Size
b_indeff<- -.0261
SD_b<- .0419
cohend <- b_indeff/SD_b
cohend

# Cohen d (t-test)
t <-  2.6508
df <- 78.6149
cohend <-2*t/sqrt(df)
cohend

#cohen d (z-test)
z = 2.41
cohendz<-2*z/sqrt(767)
cohendz

#pearson
ta <- 2.6508 #moderated
dfa <-78.6149
tbw <- -10.6651 #M to Y
dfbw <-668.3425


ta2 <- ta*ta
tbw2 <- tbw*tbw

ra <- sqrt(ta2/(ta2 + dfa))
ra
rb <- sqrt(tbw2/(tbw2 + dfa))
rb
ES <- ra*rb
ES

#Mediation POWER
install.packages("PowerUpR")
library(PowerUpR)

#Mediation
dd$M<-dd$PA #defined mediator
#Group-Mean
group.mean <- function(var, grp) {
  return(tapply(var,grp,mean,na.rm=F)[grp])
}
dd$Mgrm <- as.vector(group.mean(dd$M, dd$ID))
#Group-Mean Center
group.center <- function(var,grp) {
  return(var - tapply(var,grp,mean,na.rm=F)[grp])
}
dd$Mgrmc <- as.vector(group.center(dd$M, dd$ID))


m <-lmer(SickFt~ time + dd$Mgrmc+ dd$Mgrm + (time||ID), dd, na.action = na.omit)
summary(m)
tab_model(m, show.std = "std2", show.stat = T , show.df = T)




#TIME_LAGGED (TRANSFORM)
# require(DataCombine)
# #order by id and time
# d<-d[order(d$ID, d$time),]
# # Rename a column in R
# #lag
# d2<-slide(d, Var="SickFt", GroupVar="ID", slideBy=-1)
# d<-d2
# d2<-slide(d, Var="PA", GroupVar="ID", slideBy=-1)
# d<-d2
# d2<-slide(d, Var="ff", GroupVar="ID", slideBy=-1)
# d<-d2
# d2<-slide(d, Var="LS", GroupVar="ID", slideBy=-1)
# d<-d2
# d2<-slide(d, Var="Meaning", GroupVar="ID", slideBy=-1)
# d<-d2
#
# d$`SickFt.1b` <- ifelse(is.na(d$`SickFt-1`),
#                         d$`SickFt`, d$`SickFt-1`)
# d$SickFt.dif <- d$SickFt-d$SickFt.1b
# d$`PA.1b` <- ifelse(is.na(d$`PA-1`),
#                     d$`PA`, d$`PA-1`)
# d$PA.dif <- d$PA-d$PA.1b
# d$`LS.1b` <- ifelse(is.na(d$`LS-1`),
#                     d$`LS`, d$`LS-1`)
# d$LS.dif <- d$LS-d$LS.1b
#
# d$`ff.1b` <- ifelse(is.na(d$`ff-1`),
#                     d$`ff`, d$`ff-1`)
# d$ff.dif <- d$ff-d$ff.1b
#
# d$`Meaning.1b` <- ifelse(is.na(d$`Meaning-1`),
#                     d$`Meaning`, d$`Meaning-1`)
# d$Meaning.dif <- d$Meaning-d$Meaning.1b
#
# write.csv(d, "weekly-datalong-wMain.csv")


# J <- 100 # J participants
# d$time1 <- rep(0:9, J)
# d$time1
# d$time2 <- rep(c(0,0,0,0,0,1,2,3,4,5), J)
# d$time2


#time-Lagged
MLag0<-glmer(`SickFt` ~ SickFt.1b + (1| ID), d, na.action = na.omit, 
             family = 'poisson', 
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))
)
tab_model(MLag0,
          show.stat = T, show.df = T, show.se=T)

MLag<-glmer(`SickFt` ~ Meaning.dif + SickFt.1b + (1| ID), d, na.action = na.omit, 
            family = 'poisson', 
            glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))
)
tab_model(MLag,
          show.stat = T, show.df = T, show.std = "std2", show.se=T)

