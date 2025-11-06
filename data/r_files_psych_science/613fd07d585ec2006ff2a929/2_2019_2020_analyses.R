# -----------------------Sep 13,2021-------------------------
# -----------------------Author: Rui Sun---------------------
# --------Data analyses for 2019 vs 2020-----------

# setwd("")

require(nlme)

dat <- read.csv("dat2019_2020_to_use.csv")

# -----------------------------------------------------------------
# -----------------Preliminary Analyses----------------------------
# ----------Table S5: Emotional experience in 2019 vs 2020--------- 
# -----------------------------------------------------------------
data2019=dat[which(dat$year=="2019"),]
data2020=dat[which(dat$year=="2020"),]

describe(data2019$Pos)
describe(data2019$Neg)
describe(data2020$Pos)
describe(data2020$Neg)

t.test(dat$Pos~dat$year,paired=TRUE)
t.test(dat$Neg~dat$year,paired=TRUE)

# -------------------------------------------------------
# This is for table S6: Age-Emotion Correlation by Year 
# -------------------------------------------------------
cor.test(data2019$Pos,data2019$scale_ini_age)
cor.test(data2019$Neg,data2019$scale_ini_age)

cor.test(data2020$Pos,data2020$scale_ini_age)
cor.test(data2020$Neg,data2020$scale_ini_age)


#######################################
# Primary Analysis (TABLE S7)
#######################################
model1_Pos <-  lme(Pos~ scale_ini_age,# Level 2: scale_ini_age;
                   random=~1|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id
summary(model1_Pos)
r.squaredGLMM(model1_Pos)

model2_Pos <-  lme(Pos~ scale_ini_age*covid,#Level 1: pos,covid condition; Level 2: scale_ini_age;
                   random=~1+covid|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(model2_Pos)
r.squaredGLMM(model2_Pos)

model3_Pos <-  lme(Pos~ scale_ini_age*covid +gender+ health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness,#Level 1: pos,covid condition,health,BIG5; Level 2: scale_ini_age;
                   random=~1+covid|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(model3_Pos)
r.squaredGLMM(model3_Pos)

model4_Pos <-  lme(Pos~ (scale_ini_age+ gender+health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness)*covid,#Level 1: pos,covid condition,health,BIG5; Level 2: scale_ini_age;
                   random=~1+covid|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(model4_Pos)
r.squaredGLMM(model4_Pos)


model1_Neg <-  lme(Neg~ scale_ini_age,# Level 2: scale_ini_age;
                   random=~1|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id
summary(model1_Neg)
r.squaredGLMM(model1_Neg)

model2_Neg <-  lme(Neg~ scale_ini_age*covid,#Level 1: Neg,covid condition; Level 2: scale_ini_age;
                   random=~1+covid|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(model2_Neg)
r.squaredGLMM(model2_Neg)

model3_Neg <-  lme(Neg~ scale_ini_age*covid + gender+health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness,#Level 1: Neg,covid condition,health,BIG5; Level 2: scale_ini_age;
                   random=~1+covid|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(model3_Neg)
r.squaredGLMM(model3_Neg)

model4_Neg <-  lme(Neg~ (scale_ini_age+ gender+health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness)*covid,#Level 1: Neg,covid condition,health,BIG5; Level 2: scale_ini_age;
                   random=~1+covid|id,data=dat,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(model4_Neg)
r.squaredGLMM(model4_Neg)

# GENERATE TABLE S7
stargazer::stargazer(model1_Neg,model2_Neg,model3_Neg,model4_Neg,model1_Pos,model2_Pos,model3_Pos,model4_Pos,
                     type = "text",star.char = c("*", "**", "***"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
                     notes.align = "c",
                     notes.append = F,
                     title = 'age advantages 19 20',
                     out ="age advantages 19_20.txt")

# ===============================================================
# =====Primary Analysis; only select 18-76 (Table S8)============
# ===============================================================

dat_short <- dat[which(dat$ini_age>17 & dat$ini_age<77),]
dat_short_2019 <- dat_short[which(dat_short$year==2019),]


m1_Pos <-  lme(Pos~ scale_ini_age,# Level 2: scale_ini_age;
               random=~1|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id
summary(m1_Pos)
r.squaredGLMM(m1_Pos)

m2_Pos <-  lme(Pos~ scale_ini_age*covid,#Level 1: pos,covid condition; Level 2: scale_ini_age;
               random=~1+covid|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(m2_Pos)
r.squaredGLMM(m2_Pos)

m3_Pos <-  lme(Pos~ scale_ini_age*covid +gender+ health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness,#Level 1: pos,covid condition,health,BIG5; Level 2: scale_ini_age;
               random=~1+covid|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(m3_Pos)
r.squaredGLMM(m3_Pos)

m4_Pos <-  lme(Pos~ (scale_ini_age+ gender+health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness)*covid,#Level 1: pos,covid condition,health,BIG5; Level 2: scale_ini_age;
               random=~1+covid|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(m4_Pos)
r.squaredGLMM(m4_Pos)

m1_Neg <-  lme(Neg~ scale_ini_age,# Level 2: scale_ini_age;
               random=~1|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id
summary(m1_Neg)
r.squaredGLMM(m1_Neg)

m2_Neg <-  lme(Neg~ scale_ini_age*covid,#Level 1: Neg,covid condition; Level 2: scale_ini_age;
               random=~1+covid|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(m2_Neg)
r.squaredGLMM(m2_Neg)

m3_Neg <-  lme(Neg~ scale_ini_age*covid + gender+health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness,#Level 1: Neg,covid condition,health,BIG5; Level 2: scale_ini_age;
               random=~1+covid|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(m3_Neg)
r.squaredGLMM(m3_Neg)

m4_Neg <-  lme(Neg~ (scale_ini_age+ gender+health+Extraversion+Agreeable+Conscientiousness+EmoStability+Openness)*covid,#Level 1: Neg,covid condition,health,BIG5; Level 2: scale_ini_age;
               random=~1+covid|id,data=dat_short,na.action = na.omit,control = lmeControl(opt = 'optim'))  #random intercept=id;random slope=covid
summary(m4_Neg)
r.squaredGLMM(m4_Neg)

# GENERATE TABLE S8
stargazer::stargazer(m1_Neg,m2_Neg,m3_Neg,m4_Neg,m1_Pos,m2_Pos,m3_Pos,m4_Pos,
                     type = "text",star.char = c("*", "**", "***"),
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
                     notes.align = "c",
                     notes.append = F,
                     title = 'age advantages_19_20_18_76',
                     out ="age advantages 19_20_18_76.txt")



