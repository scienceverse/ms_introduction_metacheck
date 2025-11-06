##### Reanalysis of #####
##### Quoidbach, J., Taquet, M., Desseilles, M., de Montjoye, Y.-A., & Gross, J. J. (2019). 
##### Happiness and Social Behavior. Psychological Science. http://doi.org/10.1177/0956797619849666
##### osf.io/bxgn4 #####

rm(list = ls()) #clear the environment

# load relevant libraries ####
library(psych)
library(ggplot2)
library(lme4)
library(dplyr)
library(Rmisc)
library(doBy)
library(sjPlot)
library(EMAtools)

# load data ####
# downloaded from osf.io/bxgn4
dat <- read.csv("Happiness and Social Behavior OSF (beep level).csv") 

#source("dataprocessing.R") #or just load the processed data:
load("dat_2020-05-08.RData")

### compute some descriptives  ####

## number of observations per person ####
# compute the number of observations per person 
n.obs <- summaryBy(data = dat,
                             formula = int_t1 ~ user_id,
                             FUN = c(length))
colnames(n.obs)[2] <- "n.observations" 

mean(n.obs$n.observations) # mean number of observations for each individual
sd(n.obs$n.observations)
sum(n.obs$n.observations %in% 1)/nrow(n.obs) # percentage of individuals with 1 datapoint
table(n.obs$n.observations) # number of observations for each individual
qplot(n.obs$n.observations, bins = 100)

# number of observations per person and day  ####
n.obs.day <- doBy::summaryBy(data = dat,
                formula = int_t1 ~ user_id + date,
                FUN = c(length))
colnames(n.obs.day)[3] <- "n.observations.day" 

mean(n.obs.day$n.observations.day)
sd(n.obs.day$n.observations.day) 
range(n.obs.day$n.observations.day)
table(n.obs.day$n.observations.day) # number of observations for each individual and day

qplot(n.obs.day$n.observations.day, bins = 12) + scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  xlab("number of observations on a day")
table(n.obs.day$n.observations.day)/nrow(n.obs.day) *100 # percentages
# 66% of the daily averages are computed based on 1 measure 
sum(n.obs.day$n.observations.day %in% 1)/nrow(n.obs.day) 

# of which 97% are identical to happiness at Ht+1:
dat.oneObs <- dat[dat$n.obs.day %in% 1,]
dat.oneObs$H1.HAv <- dat.oneObs$Happiness_t1 == dat.oneObs$day_avg_happiness_no_current
sum((dat.oneObs$H1.HAv == 1))/nrow(dat.oneObs) # in 97% of the cases with only one observation 
#on a given day the Ht+1 happiness is identical to the dailyAverage

# View(dat[dat$n.obs.day %in% 1,c("user_id","date","time","Happiness_t0","Happiness_t1","day_avg_happiness_no_current",
#                                 "n.obs.day")]) # all cases with one obervation

# in how many datapoints is Happiness at t + 1 identical to the day average of happiness?
dat$H1.HAv <- dat$Happiness_t1 == dat$day_avg_happiness_no_current
sum((dat$H1.HAv == TRUE))/nrow(dat)  # 48.5 %


# number of observations per person and week ####
mean(dat$n.obs.week)
sd(dat$n.obs.week)

# number of observations per person and past day ####
mean(dat$n.obs.PastDay)
sd(dat$n.obs.PastDay)

##### Estimate models for Figure 1 in Commentary ####

# extract raw data means and confidence intervals
f1.dat <- summaryBy(data = dat,
                formula = int_t1 ~ Happiness_t0,
                FUN = c(CI))

# from the raw data compute the odds ratios (ORs) and their confidence intervals
f1.dat$int_t1.upper.OR <- f1.dat$int_t1.upper/(1-f1.dat$int_t1.upper)
f1.dat$int_t1.mean.OR <- f1.dat$int_t1.mean/(1-f1.dat$int_t1.mean)
f1.dat$int_t1.lower.OR <- f1.dat$int_t1.lower/(1-f1.dat$int_t1.lower)
f1.dat$Model <- "Raw Data"
f1.dat$var <- "raw.data"
f1.dat <- f1.dat[c(1,8,9,5:7)]

### Multilevel models ####

# specify reference categories
dat$hour.factor <- relevel(factor(dat$hour), ref = 7) # between 12-14h as the reference category
dat$day.factor <- relevel(dat$day, ref = "Sat") # saturday as the reference category

# Define estimation controls
ctrl = glmerControl(optCtrl = list(maxfun = 3e5)) # more iterations in the estimation than per default (1e5)


# estimate (see below) or load estimated models
#load("Reanalysis_models_2019-11-22.RData")

# estimate raw model
m1 <- glmer(int_t1 ~ int_t0 + # previous interaction status
                     Happiness_t0  + # happiness at time t
                     day.factor +  # factor of the day variable
                     hour.factor +  # factor of the hour variable 
                    (1 + Happiness_t0 | user_id), # random incercept and random happiness_t0 slope, data nested in user_id
                   data = dat,
                   control = ctrl, 
                   nAGQ=0, # nAGO > 1 did not converge, nAGO = 0 is less accurate but converged
                   family = binomial("logit"))
summary(m1)
save(m1, file =paste0("M1_",Sys.Date(),".RData"))

# with day average computed by Quoidbach et al., 2019
m2.dayAv <- glmer(int_t1 ~ int_t0 +
                          Happiness_t0  + 
                          day_avg_happiness_no_current +
                          day.factor + 
                          hour.factor + 
                          (1 + Happiness_t0 | user_id), 
                      data = dat,
                      control = ctrl, nAGQ=0,
                      family = binomial("logit"))
summary(m2.dayAv)
save(m2.dayAv, file =paste0("M2_dayAv_",Sys.Date(),".RData"))


# with week average
m3.weekAv <- glmer(int_t1 ~ 
                        int_t0 +
                        Happiness_t0  + 
                        PastWeekAverage_nocurrent +
                        day.factor + 
                        hour.factor + 
                        (1 + Happiness_t0 | user_id), 
                      data = dat,
                      control = ctrl, nAGQ=0,
                      family = binomial("logit"))
summary(m3.weekAv)
save(m3.weekAv, file =paste0("M3_weekAv_",Sys.Date(),".RData"))


# with past day average
m4.pastDayAv <- glmer(int_t1 ~ 
                     int_t0 +
                     Happiness_t0  + 
                    PastDayAverage_nocurrent +
                     day.factor + 
                     hour.factor + 
                     (1 + Happiness_t0 | user_id), 
                   data = dat,
                   control = ctrl, nAGQ=0,
                   family = binomial("logit"))
summary(m4.pastDayAv)
save(m4.pastDayAv, file =paste0("M3_pastDayAv_",Sys.Date(),".RData"))

# with total happiness average (not reported in manuscript)
m4.totalAv <- glmer(int_t1 ~ 
                        int_t0 +
                        Happiness_t0  + 
                        totalAverage_nocurrent +
                        day.factor + 
                        hour.factor +
                        (1 + Happiness_t0 | user_id), 
                      data = dat,
                      control = ctrl, nAGQ=0,
                      family = binomial("logit"))
summary(m4.totalAv)
save(m4.totalAv, file =paste0("M4_totalAv_",Sys.Date(),".RData"))

# with day average computed by Quoidbach et al., 2019, but cases removed where
# the average day variable was identical to the current happiness (not reported in manuscript)
dat$day_avg_happiness_no_current_clean <- dat$day_avg_happiness_no_current
dat$day_avg_happiness_no_current_clean[dat$day_avg_happiness_no_current_clean == dat$Happiness_t1] <- NA

m5.dayAv <- glmer(int_t1 ~ int_t0 +
                    Happiness_t0  + 
                    day_avg_happiness_no_current_clean +
                    day.factor + 
                    hour.factor + 
                    (1 + Happiness_t0 | user_id), 
                  data = dat,
                  control = ctrl, nAGQ=0,
                  family = binomial("logit"))
summary(m5.dayAv)
save(m5.dayAv, file =paste0("M5_dayAv_clean_",Sys.Date(),".RData"))


# with average of all previously reported happiness measures on a given day excluding t0 and t1 #
m6.dayAv_past_noT0 <- glmer(int_t1 ~ int_t0 +
                         Happiness_t0  + 
                         dayAverage_past_noT0 +
                         day.factor + 
                         hour.factor + 
                         (1 + Happiness_t0 | user_id), 
                       data = dat,
                       control = ctrl, nAGQ=0,
                       family = binomial("logit"))
summary(m6.dayAv_past_noT0)
save(m6.dayAv_past_noT0, file =paste0("M7_dayAv_past_noT0",Sys.Date(),".RData"))

# with Happiness t1 #
m7.Ht1 <- glmer(int_t1 ~ int_t0 +
                              Happiness_t0  + 
                              Happiness_t1 +
                              day.factor + 
                              hour.factor + 
                              (1 + Happiness_t0 | user_id), 
                            data = dat,
                            control = ctrl, nAGQ=0,
                            family = binomial("logit"))
summary(m7.Ht1)
save(m7.Ht1, file =paste0("M8_Happy_T1",Sys.Date(),".RData"))

# with delta Happiness #
m8.delta <- glmer(int_t1 ~ int_t0 +
                  Happiness_t0  +
                  Delta_happiness +
                  day.factor + 
                  hour.factor + 
                  (1 + Happiness_t0 | user_id), 
                data = dat,
                control = ctrl, nAGQ=0,
                family = binomial("logit"))
summary(m8.delta)
save(m8.delta, file =paste0("M8_Happy_delta",Sys.Date(),".RData"))

# # with person mean centered Happiness_t0#
m9.pmc <- glmer(int_t1 ~ int_t0 +
                    Happiness_t0_pmc +
                    day.factor + 
                    hour.factor + 
                    (1 + Happiness_t0 | user_id), 
                  data = dat,
                  control = ctrl, nAGQ=0,
                  family = binomial("logit"))
summary(m9.pmc)
save(m9.pmc, file =paste0("M9_pmc",Sys.Date(),".RData"))

# # with Ht-1 and Ht-2 happiness#
dat$happiness_Tminus1 <- dat$`Happiness_t-1`
dat$happiness_Tminus2 <- dat$`Happiness_t-2`
m10.Tminus2 <- glmer(int_t1 ~ int_t0 +
                  Happiness_t0 +
                  happiness_Tminus1 +
                  happiness_Tminus2 +
                  day.factor + 
                  hour.factor + 
                  (1 + Happiness_t0 | user_id), 
                data = dat,
                control = ctrl, nAGQ=0,
                family = binomial("logit"))
summary(m10.Tminus2)
save(m10.Tminus2, file =paste0("M10_Tminus2",Sys.Date(),".RData"))


save(m1, m2.dayAv, m3.weekAv, m4.totalAv, m5.dayAv, 
     m4.pastDayAv, m6.dayAv_past_noT0,m7.Ht1, m8.delta,
     m9.pmc,m10.Tminus2,
     file = paste0("Reanalysis_models_",Sys.Date(),".RData"))
#load("Reanalysis_models_2020-05-12.RData")
### Compute predicted values of the estimated models ####

# extract predicted values of the "no day average model"
coeff <- t(data.frame(CI.l = summary(m1)$coefficients[,1] - 1.96* (summary(m1)$coefficients[,2]), 
                      est = summary(m1)$coefficients[,1], 
                      CI.h =  summary(m1)$coefficients[,1] + 1.96* (summary(m1)$coefficients[,2])))

m1.OR <- data.frame(Happiness_t0 = 0:100, Model = "No Daily Average", var = "no.daily.average",
                    sapply(1:3, function(i) exp(coeff[i,colnames(coeff) == "(Intercept)"]+ 
                                                  (coeff[i,colnames(coeff) == "Happiness_t0"]*0:100)
                    ))) 
names(m1.OR)[4:6] <-c("int_t1.lower.OR","int_t1.mean.OR","int_t1.upper.OR") 

# extract predicted values of the "day average model" (model reported by Qouidbach et al. 2019)

get.predOR <- function(model,var.name = colnames(model@frame)[4], model.name){
  coeff <- t(data.frame(CI.l = summary(model)$coefficients[,1] - 1.96* (summary(model)$coefficients[,2] ), 
                        est = summary(model)$coefficients[,1], 
                        CI.h =  summary(model)$coefficients[,1] + 1.96* (summary(model)$coefficients[,2] )))
  
  
  out <- data.frame(Happiness_t0 = 0:100, Model = model.name, var = colnames(model@frame)[4],
                    sapply(1:3, function(i) exp(coeff[i,colnames(coeff) == "(Intercept)"] + 
                                                  (coeff[i,colnames(coeff) == "Happiness_t0"]*0:100) +
                                                  (coeff[i,colnames(coeff) == var.name]*50) 
                    ))) 
  names(out)[4:6] <-c("int_t1.lower.OR","int_t1.mean.OR","int_t1.upper.OR") 
  return(out)
}
m2.OR.dayA <- get.predOR(m2.dayAv, 
                         var.name = "day_avg_happiness_no_current", 
                         model.name = "Incl. Daily Average (Quoidbach et al., 2019)")

# compute predicted values of week average variable
m3.OR.weekA <- get.predOR(m3.weekAv, 
                         var.name = "PastWeekAverage_nocurrent", 
                         model.name = "Incl. Past Week Average")

# overall average data (not reportd in manuscript)
m4.OR.totalA <- get.predOR(m4.totalAv, 
                          var.name = "totalAverage_nocurrent", 
                          model.name = "Incl. Total Average")


# cleaned daily average model data (not reportd in manuscript)
m5.OR.dayA.clean <- get.predOR(m5.dayAv, 
                           var.name = "day_avg_happiness_no_current_clean", 
                           model.name = "Incl. Cleaned Daily Average")

# with an average of the passed 24h #
m6.OR.pastDayAv <- get.predOR(m4.pastDayAv, 
                               var.name = "PastDayAverage_nocurrent", 
                               model.name = "Incl. Past Day Average")

# with Happiness at t+1 
m7.OR.Ht1 <- get.predOR(m7.Ht1, 
                              var.name = "Happiness_t1", 
                              model.name = "Incl. Happiness t+1")

# with Delta happiness
m8.OR.delta <- get.predOR(m8.delta, 
                        var.name = "Delta_happiness", 
                        model.name = "Incl. Delta Happiness")

# combine all predicted values for figure 1 plot
f1.dat <- rbind(f1.dat,m1.OR, m2.OR.dayA, m3.OR.weekA, m4.OR.totalA, m5.OR.dayA.clean, m6.OR.pastDayAv,
                m7.OR.Ht1, m8.OR.delta#, m9.OR.pmc
                )
f1.dat$Model <- factor(f1.dat$Model, levels = unique(f1.dat$Model))

## plot the data 
ggplot(f1.dat[f1.dat$Model %in% levels(f1.dat$Model)[c(1,2,3,4,7)],], aes(x = Happiness_t0, y = int_t1.mean.OR, color = Model)) +
  geom_errorbar(aes(ymin = int_t1.lower.OR, ymax = int_t1.upper.OR), alpha = 0.3) +
 geom_point(aes(shape = Model), size = 2, alpha = 0.7) +
  #geom_line(aes(linetype = Model), size = 1.5, alpha = 0.7) +
  scale_shape_manual(values = c(16,15,17,8,9,21)) +
  theme_minimal() +
  labs(fill = "Model") +
  xlab("Happiness at Time t") +
  ylab("Odds Ratio of Being With Other People at Time t + 1") +
  scale_colour_manual(values = c("gray60","springgreen4","tomato1","dodgerblue2","skyblue","orangered2")) + 
  theme(legend.position = c(0.3,0.8),
        legend.background = element_rect(size=0.1, linetype="solid"))
ggsave(paste0("Figure1_commentary_",Sys.Date(),".pdf"))
ggsave(paste0("Figure1_commentary_",Sys.Date(),".jpg"))

#### Using different estimators ###
# the estimates with nAGQ = 0 are less precise as the estimates with more nAGO.
# However, only those with nAGQ = 0 converge. Below, I report additional models, 
# that have been estimated using more precise estimiators (nAGQ > 0), at the cost 
# having simpler model specification (day and time controls were removed), 
# by scaling the independent variables, and/or removing the random happiness_t0 slope


# without day average
m1.X <- glmer(int_t1 ~ int_t0 +
                      scale(Happiness_t0)  +  # variables are scaled for better estimation with nAGQ = 1
                      #scale(day_avg_happiness_no_current) +
                      (1 + scale(Happiness_t0) | user_id), 
                    data = dat,
                    nAGQ=1,
                    family = binomial("logit"))
summary(m1.X)

m1.X2 <- glmer(int_t1 ~ int_t0 +
                       Happiness_t0  + 
                       #day_avg_happiness_no_current +
                       (1  | user_id),  # no random slope, only random intercept
                     data = dat,
                     nAGQ=15,
                     control = glmerControl(optCtrl = list(maxfun = 3e5)), # more iterations
                     family = binomial("logit")) 
# almost converged
summary(m1.X2)

# with day average
m2.dayAv.X <- glmer(int_t1 ~ int_t0 +
                    scale(Happiness_t0)  +  # variables are scaled for better estimation with nAGQ = 1
                    scale(day_avg_happiness_no_current) +
                    (1 + scale(Happiness_t0) | user_id), 
                  data = dat,
                  nAGQ=1,
                  family = binomial("logit"))
summary(m2.dayAv.X)

m2.dayAv.X2 <- glmer(int_t1 ~ int_t0 +
                      Happiness_t0  + 
                      day_avg_happiness_no_current +
                      (1  | user_id),  # no random slope, only random intercept
                    data = dat,
                    nAGQ=15,
                    control = glmerControl(optCtrl = list(maxfun = 3e5)), # more iterations
                    family = binomial("logit")) 
# almost converged
summary(m2.dayAv.X2)

##### Predicted values for based on the including Ht+1 happiness model #####

m <- matrix(NA, 101, 101)
rownames(m) <- 0:100
colnames(m) <- 0:100

coeff <- t(data.frame(CI.l = summary(m7.Ht1)$coefficients[,1] - 1.96* (summary(m7.Ht1)$coefficients[,2] ), 
                       est = summary(m7.Ht1)$coefficients[,1], 
                       CI.h =  summary(m7.Ht1)$coefficients[,1] + 1.96* (summary(m7.Ht1)$coefficients[,2] )))

for(i in 101:1){
m[i,] <-# exp(coeff[2,colnames(coeff) == "(Intercept)"] + 
               #coeff[2,colnames(coeff) == "int_t0"] + 
      (coeff[2,colnames(coeff) == "Happiness_t0"]*0:100) +
      (coeff[2,colnames(coeff) == "Happiness_t1"]*(i-1))
     # )
}

m.m <- reshape2::melt(m) # get data in a format that is good for plotting
names(m.m)[1:2] <- c("Happiness_t1","Happiness_t0") 

ggplot(data = m.m,aes(x = Happiness_t0, y = Happiness_t1, color = value>=0)) +
  geom_point() +
  xlab("Happiness t") + ylab("Happiness t+1") +
  scale_color_manual(values =c("orange2","lightblue"), name ="joint contribution of happiness",
                     labels = c("negative interaction likelihood at t+1","positive interaction likelihood at t+1")) +
    geom_point(data = dat, aes(x = Happiness_t0, y = Happiness_t1, color= NULL), alpha = 0.05) +
  jtools::theme_apa(legend.pos = "topleft")
ggsave(paste0("SM_Figure_contribution_",Sys.Date(),".pdf"), width = 8, height = 8)

for(i in 1:nrow(dat)){
  dat[i,"effect_contribution"] <- (coeff[2,colnames(coeff) == "Happiness_t0"]*dat[i,"Happiness_t0"]) +
    (coeff[2,colnames(coeff) == "Happiness_t1"]*dat[i,"Happiness_t1"])
}
    

###### Reanalysis of the Figure 3a of the original article #####
###### Predicting interactions with specific interaction partners (e.g., best friend) 
###### based on previous happiness 

persons <- c("bestfriend","friends","family_other","siblings","kids","partner","acquaintances",
             "parents","workpeople","strangers")

###### Binary happiness predictior variable models #######
# create variable 1 SD below happiness mean
h.mean <- mean(dat$Happiness_t1) # happiness mean
h.sd <- sd(dat$Happiness_t1) # happiness standard deviation
h.sd <- 23.8 # reported on page 6 of the manuscript

dat$h_t0_belowSD <- dat$Happiness_t0 < (h.mean-h.sd) # compute binary variable H_unhappy
dat$h_t0_aboveSD <- dat$Happiness_t0 > (h.mean+h.sd)

for(i in 1:length(persons)){
  start.time <- Sys.time()
  # no day average
  m2 <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~ # dependent variable: interaction with person i
            h_t0_belowSD +  # predictor: unhappy at t
             day.factor +  # day factor control 
             hour.factor +", # hour factor control 
                    paste(persons[i],"_t0",sep = "", collapse=" + "), # previous interaction with person type i
                    "+ (1 + h_t0_belowSD  | user_id)"), # random intercept and random slope for predictor unhappy at time t, data nested in users
              data = dat,
              family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2, show.values = T,title = persons[i]))
  summary(m2) # show the model results
  assign(paste0("Fig3a_NoDayAv_",persons[i]), m2) # give the model a better name
  
  # day average 
  m2.dA <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            h_t0_belowSD + 
            day_avg_happiness_no_current +
             day.factor + 
             hour.factor +",
                       paste(persons[i],"_t0",sep = "", collapse=" + "), 
                       "+ (1 + h_t0_belowSD  | user_id)"),
                 data = dat,
                 family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2.dA, show.values = T,title = persons[i]))
  summary(m2.dA)
  assign(paste0("Fig3a_DayAv_",persons[i]), m2.dA)
  
  # past day average 
  m2.pdA <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            h_t0_belowSD + 
            PastDayAverage_nocurrent +
             day.factor + 
             hour.factor +",
                        paste(persons[i],"_t0",sep = "", collapse=" + "), 
                        "+ (1 + h_t0_belowSD  | user_id)"),
                  data = dat,
                  family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2.pdA, show.values = T,title = persons[i]))
  summary(m2.pdA)
  assign(paste0("Fig3a_PastDayAv_",persons[i]), m2.pdA)
  
  # past week average 
  m2.pwA <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            h_t0_belowSD + 
            PastWeekAverage_nocurrent +
             day.factor + 
             hour.factor +",
                        paste(persons[i],"_t0",sep = "", collapse=" + "), 
                        "+ (1 + h_t0_belowSD  | user_id)"),
                  data = dat,
                  family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2.pwA, show.values = T,title = persons[i]))
  summary(m2.pwA)
  assign(paste0("Fig3a_PastWeekAv_",persons[i]), m2.pwA)
  
  pdf(paste0("Model_comparison_",persons[i],".pdf"))
  print(plot_models(list(m2,m2.dA, m2.pdA, m2.pwA), show.values = T,title = persons[i]))
  dev.off()
  
  print(persons[i])
  print(Sys.time()- start.time)
  save(list = c(paste0("Fig3a_NoDayAv_",persons[i]),
                paste0("Fig3a_DayAv_",persons[i]),
                paste0("Fig3a_PastDayAv_",persons[i]),
                paste0("Fig3a_PastWeekAv_",persons[i])), file = paste0("Fig3a_",persons[i],"_",Sys.Date(),".RData"))
}

#save(list = ls()[grep(paste0("Fig3a_.*"), ls())], file = paste0("Fig3a_all_models_",Sys.Date(),".RData"))


# compare models 
F3.meta <- c()
for(i in persons){
  m <- ls()[grep(paste0("Fig3a.*",i), ls())]
  # day Av
  tmp1 <- as.data.frame(summary(eval(parse(text = m[1])))$coefficients)
  tmp1$person <- i
  tmp1$model <- "Incl. Daily Average (Quoidbach et al., 2019)"
  
  # no day Av
  tmp2 <- as.data.frame(summary(eval(parse(text = m[2])))$coefficients)
  tmp2$person <- i
  tmp2$model <- "No Daily Average"
  
  # past day Av
  tmp3 <- as.data.frame(summary(eval(parse(text = m[3])))$coefficients)
  tmp3$person <- i
  tmp3$model <- "Incl. Past Day Average"
  
  # past week Av
  tmp4 <- as.data.frame(summary(eval(parse(text = m[4])))$coefficients)
  tmp4$person <- i
  tmp4$model <- "Incl. Past Week Average"
  
  tmp <- rbind(tmp1,tmp2,tmp3,tmp4)
  F3.meta <- rbind(F3.meta, tmp)
  
  
}

F3.meta$CI95 <- F3.meta$`Std. Error`*1.96
F3.meta$parameter <- rownames(F3.meta)

F3.meta$model <- factor(F3.meta$model, levels = c("Raw Data","Incl. Daily Average (Quoidbach et al., 2019)",
                                                  "No Daily Average","Incl. Past Week Average",
                                                  "Incl. Past Day Average"))

#save(F3.meta, file = "F3a_meta.RData")
#load("F3a_meta.RData")

# extract results tables
for(i in persons){
  tmp <- F3.meta[F3.meta$person == i,] # data
  
  varlist <- c("(Intercept)", # list of independent variables
               "day.factorSun",
               "day.factorW",paste0("hour.factor",c(1:5,7:11)),
               paste0(i,"_t0"),
               "h_t0_belowSDTRUE",
               "day_avg_happiness_no_current","PastDayAverage_nocurrent",
               "PastWeekAverage_nocurrent") 
  
  for(var in 1:length(varlist)){
  tmp[grep(varlist[var], tmp[,"parameter"]),"var"] <- varlist[var]
  }
  out <- reshape(tmp, direction = "wide", timevar = "model", idvar = "var")
  out <- out[match(varlist, out$var),]
  
  write.csv(out, file = paste0("F3a_meta_table_",i,".csv"))
}
#write.csv(F3.meta, file = "F3a_model_results.csv")


F3.meta$person <- factor(F3.meta$person, 
                         levels = rev(c("bestfriend","friends","family_other","kids",
                                        "partner","siblings","acquaintances","workpeople",
                                        "parents","strangers")),
                         labels = rev(c("Best Friend","Friends","Other Family","Kids",
                                        "Romantic Partner","Siblings","Acquaintances","Coworkers",
                                        "Parents","Strangers")))# for odering in plot

ggplot(F3.meta[grep("h_t0_belowSDTRUE",F3.meta$parameter),], aes(x = person, y = exp(Estimate), color = model)) +
  geom_point(aes(shape = model),position=position_dodge(width=0.5), size = 2) +
  scale_shape_manual(values = c(15,17,8,9)) +
  geom_hline(yintercept = 1) + 
  scale_colour_manual(values = c("tomato1","springgreen4","dodgerblue2","skyblue")) + 
  geom_errorbar(aes(ymin = exp(Estimate-CI95), ymax = exp(Estimate+CI95)),
                width=0.2, position = position_dodge(width=0.5)) +
  xlab("") +
  ylab("Probability of Engaging in Different Social Interactions at Time t + 1
       When Feeling Unhappy at Time t (Odds Ratio)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(filename = paste0("Fig3a_robustness_",Sys.Date(),".pdf"), width = 8, height = 8)



###### Continuous happiness predictior variable models #######


for(i in 1:length(persons)){
  start.time <- Sys.time()
  # no day average
  m2 <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            Happiness_t0 + 
             day.factor + 
             hour.factor +",
                    paste(persons[i],"_t0",sep = "", collapse=" + "), 
                    "+ (1 + Happiness_t0  | user_id)"),
              data = dat,
              family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2, show.values = T,title = persons[i]))
  summary(m2)
  assign(paste0("Fig3aC_NoDayAv_c_",persons[i]), m2)
  
  # day average 
  m2.dA <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            Happiness_t0 + 
            day_avg_happiness_no_current +
             day.factor + 
             hour.factor +",
                       paste(persons[i],"_t0",sep = "", collapse=" + "), 
                       "+ (1 + Happiness_t0  | user_id)"),
                 data = dat,
                 family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2.dA, show.values = T,title = persons[i]))
  summary(m2.dA)
  assign(paste0("Fig3aC_DayAv_c_",persons[i]), m2.dA)
  
  # past day average 
  m2.pdA <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            Happiness_t0 + 
            PastDayAverage_nocurrent +
             day.factor + 
             hour.factor +",
                        paste(persons[i],"_t0",sep = "", collapse=" + "), 
                        "+ (1 + Happiness_t0  | user_id)"),
                  data = dat,
                  family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2.pdA, show.values = T,title = persons[i]))
  summary(m2.pdA)
  assign(paste0("Fig3aC_PastDayAv_c_",persons[i]), m2.pdA)
  
  # past week average 
  m2.pwA <- glmer(paste(paste(persons[i],"_t1",sep = "", collapse=" + ")," ~
            Happiness_t0 + 
            PastWeekAverage_nocurrent +
             day.factor + 
             hour.factor +",
                        paste(persons[i],"_t0",sep = "", collapse=" + "), 
                        "+ (1 + Happiness_t0  | user_id)"),
                  data = dat,
                  family = binomial("logit"), nAGQ=0, control = ctrl)
  
  print(plot_model(m2.pwA, show.values = T,title = persons[i]))
  summary(m2.pwA)
  assign(paste0("Fig3aC_PastWeekAv_c_",persons[i]), m2.pwA)
  
  pdf(paste0("Model_comparison_c_",persons[i],".pdf"))
  print(plot_models(list(m2,m2.dA, m2.pdA, m2.pwA), show.values = T,title = persons[i]))
  dev.off()
  
  print(persons[i])
  print(Sys.time()- start.time)
  save(list = c(paste0("Fig3aC_NoDayAv_c_",persons[i]),
                paste0("Fig3aC_DayAv_c_",persons[i]),
                paste0("Fig3aC_PastDayAv_c_",persons[i]),
                paste0("Fig3aC_PastWeekAv_c_",persons[i])), file = paste0("Fig3a_c_",persons[i],"_",Sys.Date(),".RData"))
}

#save(list = ls()[grep(paste0("Fig3a_.*"), ls())], file = paste0("Fig3a_all_models_",Sys.Date(),".RData"))


# compare models 

F3.meta.c <- c()
for(i in persons){
  m <- ls()[grep(paste0("Fig3aC.*",i), ls())]
  # day Av
  tmp1 <- as.data.frame(summary(eval(parse(text = m[1])))$coefficients)
  tmp1$person <- i
  tmp1$model <- "Incl. Daily Average (Quoidbach et al., 2019)"
  
  # no day Av
  tmp2 <- as.data.frame(summary(eval(parse(text = m[2])))$coefficients)
  tmp2$person <- i
  tmp2$model <- "No Daily Average"
  
  # past day Av
  tmp3 <- as.data.frame(summary(eval(parse(text = m[3])))$coefficients)
  tmp3$person <- i
  tmp3$model <- "Incl. Past Day Average"
  
  # past week Av
  tmp4 <- as.data.frame(summary(eval(parse(text = m[4])))$coefficients)
  tmp4$person <- i
  tmp4$model <- "Incl. Past Week Average"
  
  tmp <- rbind(tmp1,tmp2,tmp3,tmp4)
  F3.meta.c <- rbind(F3.meta.c, tmp)
  
  
}

F3.meta.c$CI95 <- F3.meta.c$`Std. Error`*1.96
F3.meta.c$parameter <- rownames(F3.meta.c)

F3.meta.c$model <- factor(F3.meta.c$model, levels = c("Raw Data","Incl. Daily Average (Quoidbach et al., 2019)",
                                                  "No Daily Average","Incl. Past Week Average",
                                                  "Incl. Past Day Average"))

#save(F3.meta.c, file = "F3a_meta_continuous.RData")
#load("F3a_meta_continuous.RData")

# extract results tables
for(i in persons){
  tmp <- F3.meta.c[F3.meta.c$person == i,] # data
  
  varlist <- c("(Intercept)", # list of independent variables
               "day.factorSun",
               "day.factorW",paste0("hour.factor",c(1:5,7:11)),
               paste0(i,"_t0"),
               "Happiness_t0",
               "day_avg_happiness_no_current","PastDayAverage_nocurrent",
               "PastWeekAverage_nocurrent") 
  
  for(var in 1:length(varlist)){
    tmp[grep(varlist[var], tmp[,"parameter"]),"var"] <- varlist[var]
  }
  out <- reshape(tmp, direction = "wide", timevar = "model", idvar = "var")
  out <- out[match(varlist, out$var),]
  
  write.csv(out, file = paste0("F3a_meta_table_continuous_",i,".csv"))
}


F3.meta.c$person <- factor(F3.meta.c$person, 
                         levels = rev(c("bestfriend","friends","family_other","kids",
                                        "partner","siblings","acquaintances","workpeople",
                                        "parents","strangers")),
                         labels = rev(c("Best Friend","Friends","Other Family","Kids",
                                        "Romantic Partner","Siblings","Acquaintances","Coworkers",
                                        "Parents","Strangers")))# for odering in plot

ggplot(F3.meta.c[grep("Happiness_t0",F3.meta.c$parameter),], aes(x = person, y = exp(Estimate), color = model)) +
  geom_point(aes(shape = model),position=position_dodge(width=0.5), size = 2) +
  scale_shape_manual(values = c(15,17,8,9)) +
  geom_hline(yintercept = 1) + 
  scale_colour_manual(values = c("tomato1","springgreen4","dodgerblue2","skyblue")) + 
  geom_errorbar(aes(ymin = exp(Estimate-CI95), ymax = exp(Estimate+CI95)),
                width=0.2, position = position_dodge(width=0.5)) +
  xlab("") +
  ylab("Probability of Engaging in Different Social Interactions at Time t + 1 per Happiness Point at Time t (Odds Ratio)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(filename = paste0("Fig3a_robustness_continuous",Sys.Date(),".pdf"), width = 8, height = 8)


