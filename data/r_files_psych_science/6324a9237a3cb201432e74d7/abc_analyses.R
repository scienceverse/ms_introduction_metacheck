#load required packages
library(dplyr)
library(lme4)
library(MuMIn)
library(rstatix)
library(afex)
library(psychometric)
library(ggplot2)

#load data
data <- read.csv("deidentified_data.csv") %>%
  dplyr::rename(id=deidentified.id)
data_recalled <- data %>%
  filter(recalled==1) 
data_s2<-data_recalled %>%
  filter(session==2)
data_s1<-data_recalled %>%
  filter(session==1)

#save function to convert unstandardized beta to standardized (see https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model)
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

#### RESULTS ####

#general descriptives
##age of events
data_recalled %>%
  group_by(session) %>%
  get_summary_stats(event_age,show=c("min","max","mean","sd","se"))
##retention interval
data_s2 %>%
  get_summary_stats(retention_interval,show=c("min","max","mean","sd","se"))
##vividness between sessions
m_vid <- lmer(vivid~session + (1|id) + (1|ev.no), data_recalled)
summary(m_vid)
stdCoef.merMod(m_vid)
r2_vid<-(r.squaredGLMM(m_vid))[1,2]
CI.Rsq(r2_vid,940,1,level=.95)
##episodic/internal details between sessions
m_epi <- lmer(int_tot~session + (1|id) + (1|ev.no), data_recalled)
summary(m_epi)
stdCoef.merMod(m_epi)
r2_epi<-(r.squaredGLMM(m_epi))[1,2]
CI.Rsq(r2_epi,940,1,level=.95)

#visual perspective descriptives
##own perspective between sessions
m_own <- lmer(own_pers~session + (1|id) + (1|ev.no), data_recalled)
summary(m_own)
stdCoef.merMod(m_own)
r2_own<-(r.squaredGLMM(m_own))[1,2]
CI.Rsq(r2_own,940,1,level=.95)
##obs perspective between sessions
m_obs <- lmer(obs_pers~session + (1|id) + (1|ev.no), data_recalled)
summary(m_obs)
stdCoef.merMod(m_obs)
r2_obs<-(r.squaredGLMM(m_obs))[1,2]
CI.Rsq(r2_obs,940,1,level=.95)
##relationship between own and observer
#session 1
m_pers_s1 <- lmer(obs_pers~own_pers + (1|id) + (1|ev.no), data_s1)
summary(m_pers_s1)
stdCoef.merMod(m_pers_s1)
r2_pers_s1<-(r.squaredGLMM(m_pers_s1))[1,2]
CI.Rsq(r2_pers_s1,470,1,level=.95)
#session 2
m_pers_s2 <- lmer(obs_pers~own_pers + (1|id) + (1|ev.no), data_s2)
summary(m_pers_s2)
stdCoef.merMod(m_pers_s2)
r2_pers_s2<-(r.squaredGLMM(m_pers_s2))[1,2]
CI.Rsq(r2_pers_s2,470,1,level=.95)

#consistency descriptives
data_s2 %>%
  get_summary_stats(cons_prop,show=c("min","max","mean","sd","se"))

#main analyses
##overall consistency 
###own perspective 
m1<-lmer(cons_prop~abs_own + (1|id) + (1|ev.no), data_s2)
summary(m1)
stdCoef.merMod(m1)
r2_m1<-(r.squaredGLMM(m1))[1,2]
CI.Rsq(r2_m1,470,1,level=.95)
###observer perspective
m2<-lmer(cons_prop~abs_obs + (1|id) + (1|ev.no), data_s2)
summary(m2)
stdCoef.merMod(m2)
r2_m2<-(r.squaredGLMM(m2))[1,2]
CI.Rsq(r2_m2,470,1,level=.95)
###own and observer perspective and overall consistency model
m3<-lmer(cons_prop~abs_own + abs_obs + (1|id) + (1|ev.no), data_s2)
summary(m3)
stdCoef.merMod(m3)
r2_m3<-(r.squaredGLMM(m3))[1,2]
CI.Rsq(r2_m3,470,2,level=.95)

##event detail consistency
###own perspective
ev_m1<-lmer(ev.cons_prop~abs_own + (1|id) + (1|ev.no), data_s2)
summary(ev_m1)
stdCoef.merMod(ev_m1)
r2_ev_m1<-(r.squaredGLMM(ev_m1))[1,2]
CI.Rsq(r2_ev_m1,470,1,level=.95)
###observer perspecitve
ev_m2<-lmer(ev.cons_prop~abs_obs + (1|id) + (1|ev.no), data_s2)
summary(ev_m2)
stdCoef.merMod(ev_m2)
r2_ev_m2<-(r.squaredGLMM(ev_m2))[1,2]
CI.Rsq(r2_ev_m2,470,1,level=.95)
### own and observer perspective 
ev_m3<-lmer(ev.cons_prop~abs_own + abs_obs + (1|id) + (1|ev.no), data_s2)
summary(ev_m3)
stdCoef.merMod(ev_m3)
r2_ev_m3<-(r.squaredGLMM(ev_m3))[1,2]
CI.Rsq(r2_ev_m3,470,2,level=.95)

##perceptual detail consistency
###own perspective
perc_m1<-lmer(perc.cons_prop~abs_own + (1|id) + (1|ev.no), data_s2)
summary(perc_m1)
stdCoef.merMod(perc_m1)
r2_perc_m1<-(r.squaredGLMM(perc_m1))[1,2]
CI.Rsq(r2_perc_m1,470,1,level=.95)
###observer perspective
perc_m2<-lmer(perc.cons_prop~abs_obs + (1|id) + (1|ev.no), data_s2)
summary(perc_m2)
stdCoef.merMod(perc_m2)
r2_perc_m2<-(r.squaredGLMM(perc_m2))[1,2]
CI.Rsq(r2_perc_m2,470,1,level=.95)
###own and observer perspective
perc_m3<-lmer(perc.cons_prop~abs_own + abs_obs + (1|id) + (1|ev.no), data_s2)
summary(perc_m3)
stdCoef.merMod(perc_m3)
r2_perc_m3<-(r.squaredGLMM(perc_m3))[1,2]
CI.Rsq(r2_perc_m3,470,2,level=.95)

##emotion detail consistency
###own perspecitve
emo_m1<-lmer(emo.cons_prop~abs_own + (1|id) + (1|ev.no), data_s2)
summary(emo_m1)
stdCoef.merMod(emo_m1)
r2_emo_m1<-(r.squaredGLMM(emo_m1))[1,2]
CI.Rsq(r2_emo_m1,470,1,level=.95)
###observer perspective
emo_m2<-lmer(emo.cons_prop~abs_obs + (1|id) + (1|ev.no), data_s2)
summary(emo_m2)
stdCoef.merMod(emo_m2)
r2_emo_m2<-(r.squaredGLMM(emo_m2))[1,2]
CI.Rsq(r2_emo_m2,470,1,level=.95)
###own and observer persepctive 
emo_m3<-lmer(emo.cons_prop~abs_own + abs_obs + (1|id) + (1|ev.no), data_s2)
summary(emo_m3)
stdCoef.merMod(emo_m3)
r2_emo_m3<-(r.squaredGLMM(emo_m3))[1,2]
CI.Rsq(r2_emo_m3,470,2,level=.95)

#### Supplemental Materials ####
#Table S4
data %>%
  group_by(recalled,session) %>%
  get_summary_stats(c(own_pers,obs_pers,event_age,retention_interval,valence,arousal,
                      imp_now,imp_then,unique,belief,vivid,rehearsal,coherence), 
                    show=c("mean","sd","se"))
#Table S5
##average phenomenological of ratings events within subjects
winSs<-data %>%
  filter(session==1) %>%
  filter(id!=18) %>% #remove subject with no not-recalled events
  group_by(id,recalled) %>%
  summarize(own_avg=mean(abs_own),obs_avg=mean(abs_obs),
            valence_avg=mean(abs_val),arousal_avg=mean(abs_ars),
            impnow_avg=mean(abs_impnow),impthen_avg=mean(abs_impthen),
            uni_avg=mean(abs_unique),belief_avg=mean(abs_belief),
            vivid_avg=mean(abs_vivid),rehrs_avg=mean(abs_rehearsal),
            coh_avg=mean(abs_coherence),ev.age_avg=mean(event_age)) %>%
  ungroup()
#decriptive statistics for recalled and not-recalled events
winSs %>%
  group_by(recalled) %>%
  get_summary_stats(c(own_avg,obs_avg,valence_avg,arousal_avg,impnow_avg,impthen_avg,uni_avg,
                      belief_avg,vivid_avg,rehrs_avg,coh_avg,ev.age_avg), show=c("mean","sd","se"))
#comparison of phenomenological ratings between recalled and not-recalled events
winSs %>%
  t_test(own_avg~recalled,paired=T)
winSs %>%
  t_test(obs_avg~recalled,paired=T)
winSs %>%
  t_test(valence_avg~recalled,paired=T)
winSs %>%
  t_test(arousal_avg~recalled,paired=T)
winSs %>%
  t_test(impnow_avg~recalled,paired=T)
winSs %>%
  t_test(impthen_avg~recalled,paired=T)
winSs %>%
  t_test(uni_avg~recalled,paired=T)
winSs %>%
  t_test(belief_avg~recalled,paired=T)
winSs %>%
  t_test(vivid_avg~recalled,paired=T)
winSs %>%
  t_test(rehrs_avg~recalled,paired=T)
winSs %>%
  t_test(coh_avg~recalled,paired=T)
winSs %>%
  t_test(ev.age_avg~recalled,paired=T)

#main analyses controlling for age of event at session 1
##overall consistency 
###own perspective 
sup.m1<-lmer(cons_prop~abs_own + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.m1)
stdCoef.merMod(sup.m1)
r2_sup.m1<-(r.squaredGLMM(sup.m1))[1,2]
CI.Rsq(r2_sup.m1,470,2,level=.95)
###observer perspective
sup.m2<-lmer(cons_prop~abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.m2)
stdCoef.merMod(sup.m2)
r2_sup.m2<-(r.squaredGLMM(sup.m2))[1,2]
CI.Rsq(r2_sup.m2,470,2,level=.95)
###own and observer perspective and overall consistency model
sup.m3<-lmer(cons_prop~abs_own + abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.m3)
stdCoef.merMod(sup.m3)
r2_sup.m3<-(r.squaredGLMM(sup.m3))[1,2]
CI.Rsq(r2_sup.m3,470,3,level=.95)

##event detail consistency 
###own perspective 
sup.ev_m1<-lmer(ev.cons_prop~abs_own + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.ev_m1)
stdCoef.merMod(sup.ev_m1)
r2_sup.ev_m1<-(r.squaredGLMM(sup.ev_m1))[1,2]
CI.Rsq(r2_sup.ev_m1,470,2,level=.95)
###observer perspective
sup.ev_m2<-lmer(ev.cons_prop~abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.ev_m2)
stdCoef.merMod(sup.ev_m2)
r2_sup.ev_m2<-(r.squaredGLMM(sup.ev_m2))[1,2]
CI.Rsq(r2_sup.ev_m2,470,2,level=.95)
###own and observer perspective and overall consistency model
sup.ev_m3<-lmer(ev.cons_prop~abs_own + abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.ev_m3)
stdCoef.merMod(sup.ev_m3)
r2_sup.ev_m3<-(r.squaredGLMM(sup.ev_m3))[1,2]
CI.Rsq(r2_sup.ev_m3,470,3,level=.95)

##perceptual detail consistency 
###own perspective 
sup.perc_m1<-lmer(perc.cons_prop~abs_own + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.perc_m1)
stdCoef.merMod(sup.perc_m1)
r2_sup.perc_m1<-(r.squaredGLMM(sup.perc_m1))[1,2]
CI.Rsq(r2_sup.perc_m1,470,2,level=.95)
###observer perspective
sup.perc_m2<-lmer(perc.cons_prop~abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.perc_m2)
stdCoef.merMod(sup.perc_m2)
r2_sup.perc_m2<-(r.squaredGLMM(sup.perc_m2))[1,2]
CI.Rsq(r2_sup.perc_m2,470,2,level=.95)
###own and observer perspective and overall consistency model
sup.perc_m3<-lmer(perc.cons_prop~abs_own + abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.perc_m3)
stdCoef.merMod(sup.perc_m3)
r2_sup.perc_m3<-(r.squaredGLMM(sup.perc_m3))[1,2]
CI.Rsq(r2_sup.perc_m3,470,3,level=.95)

##emotion detail consistency 
###own perspective 
sup.emo_m1<-lmer(emo.cons_prop~abs_own + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.emo_m1)
stdCoef.merMod(sup.emo_m1)
r2_sup.emo_m1<-(r.squaredGLMM(sup.emo_m1))[1,2]
CI.Rsq(r2_sup.emo_m1,470,2,level=.95)
###observer perspective
sup.emo_m2<-lmer(emo.cons_prop~abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.emo_m2)
stdCoef.merMod(sup.emo_m2)
r2_sup.emo_m2<-(r.squaredGLMM(sup.emo_m2))[1,2]
CI.Rsq(r2_sup.emo_m2,470,2,level=.95)
###own and observer perspective and overall consistency model
sup.emo_m3<-lmer(emo.cons_prop~abs_own + abs_obs + event_age + (1|id) + (1|ev.no), data_s1)
summary(sup.emo_m3)
stdCoef.merMod(sup.emo_m3)
r2_sup.emo_m3<-(r.squaredGLMM(sup.emo_m3))[1,2]
CI.Rsq(r2_sup.emo_m3,470,3,level=.95)
