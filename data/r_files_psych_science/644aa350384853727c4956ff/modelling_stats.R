#set up environment--------------------------------------------------------------
libs = c("dplyr","tidyverse","lme4","lmerTest","ez","lubridate", "ggsignif","karthik",
         "wesanderson","Cairo","DescTools","emmeans","broom","sjPlot", "patchwork",
         "ggeffects", "ggpubr", "pracma","imputeTS", "zoo", "Hmisc", "rmisc", "MuMIn")
lapply(libs, require, character.only = TRUE)

theme_set(theme_light(base_size = 40))
theme_update(text = element_text(size=20),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank())

#read in modelling and behavioural data-------------------
modExp1<-read_csv("~/Desktop/Research/Prediction&Consolidation/osf/modelOutput_exp1.csv")
modExp2<-read_csv("~/Desktop/Research/Prediction&Consolidation/osf/modelOutput_exp2.csv")

modExp1$exp<-"Exp 1"
modExp1$delay<--0.5
modExp2$exp<-"Exp 2"
modExp1$modDiffs<- modExp1$link_cue_diffNew
modExp1$sequence_e<- ifelse(modExp1$sequence == "A", -0.5, 0.5)
modExp2$modDiffs<- modExp2$link_cue_diffNew
modExp2$sequence_e<- ifelse(modExp2$sequence == "A", -0.5, 0.5)

#run tests---------------
#are the model fits > 0?
exp1_avg<-modExp1 %>% 
  dplyr::group_by(participant) %>% 
  dplyr::summarise(avgLink = mean(r2_diff, na.rm = T), 
                   avgCue = mean(r2_diffCueNew, na.rm = T))
t.test(exp1_avg$avgLink, mu = 0)
t.test(exp1_avg$avgCue, mu = 0)

exp2_avg<-modExp2 %>% 
  dplyr::group_by(participant, delay, delay_length) %>% 
  dplyr::summarise(avgLink = mean(r2_diff, na.rm = T), 
                   avgCue = mean(r2_diffCueNew, na.rm = T)) 
exp2_avgShort<-exp2_avg %>% 
  dplyr::filter(delay == -0.5)
exp2_avgLong<-exp2_avg %>% 
  dplyr::filter(delay == 0.5)

t.test(exp2_avgShort$avgLink, mu = 0)
t.test(exp2_avgLong$avgLink, mu = 0)
t.test(exp2_avgShort$avgCue, mu = 0)
t.test(exp2_avgLong$avgCue, mu = 0)

#is link r2 > cue r2?
summary(diff_exp1<-lmer(modDiffs~sequence_e+(1|participant), data = modExp1))
confint(diff_exp1, method = "Wald")
summary(diff_exp2<-lmer(modDiffs~delay*delay_length+sequence_e+(1+delay|participant), data = modExp2))
confint(diff_exp2, method = "Wald")

#relationship to accuracy
#exp 1
summary(m_exp1<-lmer(avg~modDiffs+sequence_e+(1|participant), data = modExp1))
confint(m_exp1, method = "Wald")

#exp 2 interaciton with delay
summary(m_exp2<-lmer(avg~modDiffs*delay*delay_length+sequence_e+(1|participant), data = modExp2))
confint(m_exp2, method = "Wald")

#immediate test
summary(m_exp2Short<-lmer(avg~modDiffs*delay_length+sequence_e+(1|participant), data = modExp2, subset = (delay == -0.5)))
confint(m_exp2Short, method = "Wald")

#delayed test
summary(m_exp2Long<-lmer(avg~modDiffs*delay_length+sequence_e+(1|participant), data = modExp2, subset = (delay == 0.5)))
confint(m_exp2Long, method = "Wald")


#graphs-------------
##model graphs
#make a nice model plot
effects_mod <- effects::effect(term= "modDiffs:delay", mod= m_exp2)
summary(effects_mod)
x_corDist <- as.data.frame(effects_mod)
x_corDist<-x_corDist %>% 
  filter(delay == 0.5 | delay == -0.5) %>% 
  mutate(delay = as.factor(delay),
         exp = "Exp 2")

effects_mod1 <- effects::effect(term= "modDiffs", mod= m_exp1)
summary(effects_mod1)
x_corDist1 <- as.data.frame(effects_mod1)
x_corDist1<-x_corDist1 %>% 
  mutate(delay = as.factor(-0.5),
         exp = "Exp 1")

x_corDist<-rbind(x_corDist1,x_corDist)

modExp1<-modExp1 %>% 
  dplyr::mutate(modDiffs = link_cue_diff) %>% 
  dplyr::select(c(participant, sequence, modDiffs, exp, delay, avg))

modExp2<-modExp2 %>% 
  dplyr::select(c(participant, sequence, modDiffs, exp, delay, avg))

modComb<-rbind(modExp1,modExp2)

ModDiffDelayPlot<-ggplot()+ #guts dprime_dist, aes(x = corrDist, y = dprime)
  geom_jitter(data =modComb, mapping=aes(x = modDiffs,y=avg, 
                                         color = as.factor(delay)),
              height = 0, width = 0.15, size = 1, alpha = 0.3)+
  geom_line(data=x_corDist, aes(x=modDiffs, y=fit, color = delay, group = delay), size = 1.5) +
  geom_ribbon(data= x_corDist, aes(x=modDiffs, ymin=lower, ymax=upper, group = delay, fill = delay), alpha= 0.3) +
  scale_color_manual(name = "Delay", labels = c("immediate", "delayed"),values=c("#168E7F","#af8dc3")) +
  scale_fill_manual(name = "Delay",labels = c("immediate", "delayed"),values=c("#168E7F","#af8dc3")) +
  facet_grid(~exp) +
  scale_x_continuous(breaks=c(-0.3, -0.1, 0.1, 0.3))+
  xlab("Link Model Fit-Cue Model Fit") +
  ylab("Accuracy")+
  #ylim(0,2.2)+
  theme_classic(base_size = 24)
ModDiffDelayPlot
ggsave("~/Desktop/Research/Prediction&Consolidation/exp3/graphs/figureReady_modDiffXdelayAcc.png", w = 10, h = 5)


#make figures link vs cue figure
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
ModDiffPlots<-ggplot(modComb, aes(x = as.factor(delay), y=modDiffs, color = as.factor(delay))) +   
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, alpha=.5, stroke=3, 
  #            fill = 'white', position = position_nudge(x = -.2, y = 0)) + 
  geom_point(size = 1, alpha=.5, position = position_jitterdodge(jitter.width = 0.2,
                                                                 dodge.width = 0.7)) + 
  #geom_line(aes(group = participant), color = "black")+
  xlab("Delay") + ylab("Link Model Fit-\nCue Model Fit") +
  geom_flat_violin(aes(fill = as.factor(delay)),
                   position = position_dodge(width = 0.1), alpha = .6)+
  stat_summary(fun.data=mean_cl_boot,  geom="pointrange",lwd=1, fatten=2.25,
               position = position_nudge(x = .15, y = 0)) +
  facet_grid(~exp, scales = "free_x", space = "free")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  scale_x_discrete(labels=c("-0.5" = "immediate", "0.5" = "delayed"))+
  scale_fill_manual(name = "Delay",values=c("#168E7F","#af8dc3"), labels = c("immediate", "delayed")) +
  scale_color_manual(name = "Delay",values=c("#168E7F","#af8dc3"), labels = c("immediate", "delayed")) +
  theme_classic(base_size = 24)
ModDiffPlots
ggsave("~/Desktop/Research/Prediction&Consolidation/exp3/graphs/linkVsCue_plot.png", h = 5, w = 9)


