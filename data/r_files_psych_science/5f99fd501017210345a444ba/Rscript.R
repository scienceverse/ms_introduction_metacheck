rm(list = ls())

library(lattice)
library(ggplot2)
library(stats)
library(pastecs)
library(ggpubr)
library(aod)
library(popbio)
library(plotly)
library(lavaan)
library(reghelper)
library(lmerTest)
library(sjPlot)
library(MuMIn)
library(corrplot)
library(ggpubr)
library(Hmisc)
library(robustlmm)
library(boot)
require(foreign)
library(sfsmisc)
require(sjstats)
require(lme4)
require(merMod)
require(arm)
require(MASS)
require(Matrix)

#update for individual use
setwd("~/Downloads")

CDA_byroom_startle<-read.table(paste("Full_Data.txt", sep=""), header=TRUE)
CDA_byroom_startle$friend.stranger.ratio<-((CDA_byroom_startle$friends.group/CDA_byroom_startle$total.group) - (CDA_byroom_startle$others.group/CDA_byroom_startle$total.group))

set_theme(
  base = theme_blank(),
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 3
)


#~~~~~~~~~~~~~~~~~~~~~######## Empty Models ####~~~~~~~~~~~~~~~~~~~~~

empty.tonic<-lmer(CDA.Tonic~ 1 + (1|ID), data = CDA_byroom_startle, REML=FALSE)
summary(empty.tonic)
AIC(empty.tonic)
BIC(empty.tonic)
icc(empty.tonic)

empty.nSCR<-lmer(CDA.nSCR~ 1 + (1|ID), data = CDA_byroom_startle, REML=FALSE)
summary(empty.nSCR)
AIC(empty.nSCR)
BIC(empty.nSCR)
icc(empty.nSCR)

empty.AmpSum<-lmer(CDA.AmpSum ~ 1 + (1|ID), data = CDA_byroom_startle, REML=FALSE)
summary(empty.AmpSum)
AIC(empty.AmpSum)
BIC(empty.AmpSum)
icc(empty.AmpSum)

#~~~~~~~~~~~~~~~~~~~~~######## Models with Time ####~~~~~~~~~~~~~~~~~~~~~
empty.tonic.time<-lmer(CDA.Tonic~ 1 + Event.NID + (1|ID), data = CDA_byroom_startle, REML=FALSE)
summary(empty.tonic.time)
AIC(empty.tonic.time)
BIC(empty.tonic.time)
anova(empty.tonic, empty.tonic.time)

empty.nSCR.time<-lmer(CDA.nSCR~ 1 + Event.NID +  (1|ID), data = CDA_byroom_startle, REML=FALSE)
summary(empty.nSCR.time)
AIC(empty.nSCR.time)
BIC(empty.nSCR.time)
anova(empty.nSCR, empty.nSCR.time)

empty.AmpSum.time<-lmer(CDA.AmpSum ~ 1 + Event.NID + (1|ID), data = CDA_byroom_startle, REML=FALSE)
summary(empty.AmpSum.time)
AIC(empty.AmpSum.time)
BIC(empty.AmpSum.time)
anova(empty.AmpSum, empty.AmpSum.time)

#~~~~~~~~~~~~~~~~~~~~~######## Friend Ratio ####~~~~~~~~~~~~~~~~~~~~~

room.tonic.ratio<-lmer(CDA.Tonic~Event.NID +  friend.stranger.ratio+ (1|ID), data = CDA_byroom_startle)
summary(room.tonic.ratio)
AIC(room.tonic.ratio)
BIC(room.tonic.ratio)
rmse(room.tonic.ratio)
tab_model(room.tonic.ratio, digits=3, show.se=TRUE, string.se = "SE", string.ci="95%CI", show.stat=TRUE, string.stat = "t")
plot_model(room.tonic.ratio, type="pred", terms=c("Event.NID", "friend.stranger.ratio"))
plot_model(room.tonic.ratio, type="diag")
qqnorm(residuals(room.tonic.ratio))


  ##interaction
room.tonic.ratio.X<-lmer(CDA.Tonic~Event.NID *  friend.stranger.ratio+ (1|ID), data = CDA_byroom_startle)
summary(room.tonic.ratio.X)
plot_model(room.tonic.ratio.X, type="pred", terms=c("Event.NID", "friend.stranger.ratio [-.45, .65]"), colors = "bw")
simple_slopes(room.tonic.ratio.X, levels=list(friend.stranger.ratio=c(-.45, .65, 'sstest'), Event.NID=c(2, 17, 'sstest')))
interactions::sim_slopes(model=room.tonic.ratio.X, modx=Event.NID, pred=friend.stranger.ratio, digits=3)
plot_model(room.tonic.ratio.X, type="diag")
qqnorm(residuals(room.tonic.ratio.X))
plot(room.tonic.ratio.X)


#~~~~~~~~~~~~~~~~~~~~~######## Imminence ####~~~~~~~~~~~~~~~~~~~~~

  #linear
room.nSCR.imm.lin<-lmer(CDA.nSCR~Event.NID + Imminence + (1|ID), data = CDA_byroom_startle)
summary(room.nSCR.imm.lin)
tab_model(room.nSCR.imm.lin, digits=3, show.se=TRUE, string.se = "SE", string.ci="95%CI", show.stat=TRUE, string.stat = "t")
plot_model(room.nSCR.imm.lin, type="pred", terms=c("Imminence"))

  #linear
room.AmpSum.imm.lin<-lmer(CDA.AmpSum~Event.NID + Imminence + (1|ID), data = CDA_byroom_startle)
summary(room.AmpSum.imm.lin)
tab_model(room.AmpSum.imm.lin, digits=3, show.se=TRUE, string.se = "SE", string.ci="95%CI", show.stat=TRUE, string.stat = "t")
plot_model(room.AmpSum.imm.lin, type="pred", terms=c("Imminence"))
plot_model(room.AmpSum.imm.lin, type="diag")
qqnorm(residuals(room.AmpSum.imm.lin))
plot(room.AmpSum.imm.lin)
anova(room.AmpSum.imm.lin, room.AmpSum.imm)

  #quadratic
room.nSCR.imm<-lmer(CDA.nSCR~Event.NID + poly(Imminence,2) + (1|ID), data = CDA_byroom_startle)
summary(room.nSCR.imm)
tab_model(room.nSCR.imm, digits=3, show.se=TRUE, string.se = "SE", string.ci="95%CI", show.stat=TRUE, string.stat = "t")
plot_model(room.nSCR.imm, type="pred", terms=c("Imminence"))
plot_model(room.nSCR.imm, type="diag")
qqnorm(residuals(room.nSCR.imm))
plot(room.nSCR.imm)
anova(room.nSCR.imm.lin, room.nSCR.imm)

  #quadratic
room.AmpSum.imm<-lmer(CDA.AmpSum~Event.NID + poly(Imminence,2) + (1|ID), data = CDA_byroom_startle)
summary(room.AmpSum.imm)
tab_model(room.AmpSum.imm, digits=3, show.se=TRUE, string.se = "SE", string.ci="95%CI", show.stat=TRUE, string.stat = "t")
plot_model(room.AmpSum.imm, type="pred", terms=c("Imminence"))
plot_model(room.AmpSum.imm, type="diag")
qqnorm(residuals(room.AmpSum.imm))
plot(room.AmpSum.imm)


#~~~~~~~~~~~~~~~~~~~~~######## Subjective Fear ####~~~~~~~~~~~~~~~~~~~~~
room.nSCR.fear<-lmer(CDA.nSCR~Event.NID + before.fear + after.fear + (1|ID), data = CDA_byroom_startle)
summary(room.nSCR.fear)
tab_model(room.nSCR.fear, digits=3, show.se=TRUE, string.se = "SE", string.ci="95%CI", show.stat=TRUE, string.stat = "t")
plot_model(room.nSCR.fear, type="pred", terms=c("Event.NID", "after.fear"))
plot_model(room.nSCR.fear, type="pred",  terms=c("after.fear"))
plot_model(room.nSCR.fear, type="diag")
qqnorm(residuals(room.nSCR.fear))
plot(room.nSCR.fear)


#~~~~~~~~~~~~~~~~~~~~~######## Startle ####~~~~~~~~~~~~~~~~~~~~~

habit.nSCR<-lmer(CDA.nSCR ~ Event.NID + (Event.NID |ID), data =  CDA_byroom_startle)
summary(habit.nSCR)
coef(habit.nSCR)

habit.AmpSum<-lmer(CDA.AmpSum ~ Event.NID + (Event.NID |ID), data =  CDA_byroom_startle)
summary(habit.AmpSum)
coef(habit.AmpSum)

habit.nSCR.startle<-lm(Slope_nSCR ~ CDA.nSCR.Startle, data =  CDA_habituation)
summary(habit.nSCR.startle)
AIC(habit.nSCR.startle)
BIC(habit.nSCR.startle)
rmse(habit.nSCR.startle)
plot_model(habit.nSCR.startle, type="pred")
confint(habit.nSCR.startle, 'CDA.nSCR.Startle', level=0.95)
plot_model(habit.nSCR.startle, type="diag")
qqnorm(residuals(habit.nSCR.startle))
plot(habit.nSCR.startle)

habit.AmpSum.startle<-lm(Slope.AmpSum ~ CDA.AmpSum.Startle, data =  CDA_habituation)
summary(habit.AmpSum.startle)
AIC(habit.AmpSum.startle)
BIC(habit.AmpSum.startle)
rmse(habit.AmpSum.startle)
confint(habit.AmpSum.startle, 'CDA.AmpSum.Startle', level=0.95)
plot_model(habit.AmpSum.startle, type="pred")




