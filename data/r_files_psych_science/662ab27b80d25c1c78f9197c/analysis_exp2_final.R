library(lme4)
library(lmerTest)
library(sjPlot)
library(patchwork)
library(tidyverse)
library(pwr)
library(lsr)
library(broom.mixed)

theme_set(theme_light(base_size = 40))
theme_update(text = element_text(size=20),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank())
#read in data------------------
pred<-read_csv("~/Desktop/Research/Prediction&Consolidation/osf/pred_exp2.csv")
enc<-read_csv("~/Desktop/Research/Prediction&Consolidation/osf/enc_exp2.csv")
recall_data<-read_csv("~/Desktop/Research/Prediction&Consolidation/osf/recallData_exp2.csv")

#exclusions-------------

#how much are people responding overall?
attemptNum<-pred %>%
  dplyr::group_by(participant, `Task Name`) %>% 
  dplyr::summarise(total = sum(attempt_clean, na.rm =T))
attemptNum

#overall acc
predAccExcl<-pred %>%
  dplyr::group_by(participant, `Task Name`) %>% 
  dplyr::filter(`Task Name` == "Prediction - Day 1") %>% 
  dplyr::summarise(avg = mean(corr))
predAccExcl

binom.test(91, 160, p = 0.5,
           alternative = c("greater"),
           conf.level = 0.95)

exclude<-predAccExcl %>%
  dplyr::filter(avg < 0.56875) #excluding numerically above chance-0.5 
exclude

pred<-pred %>% 
  dplyr::filter(!(participant %in% exclude$participant))

##encoding data analysis-----------------
predAcc<-pred %>%
  dplyr::group_by(participant, exp) %>% 
  dplyr::summarise(avg = mean(corr))
predAcc

enc<-enc %>% 
  #dplyr::mutate(participant = `Participant Public ID`) %>% 
  dplyr::filter(participant %in% predAcc$participant)

testTrial<-enc %>% 
  dplyr::filter(display == "TestTrial") %>%
  dplyr::mutate(trialNum = `Trial Number`)

testTrialResp<-testTrial %>% 
  dplyr::filter(Attempt == 1) %>% 
  dplyr::mutate(attempt_clean = Attempt) %>% 
  dplyr::select("attempt_clean", "Zone Type", "participant", "exp",
                "trialNum", "Correct", "Reaction Time")

testTrial<-left_join(testTrial,testTrialResp,  by = c("participant", "exp", "trialNum"))

testTrial<-testTrial %>% 
  filter(`Zone Type.x` == "response_keyboard_single")  %>% #only include timelimit zone bc it happens on every trial
  mutate(corr = if_else(!is.na(attempt_clean), Correct.y, 0), 
         trialNum = as.numeric(trialNum)) %>% 
  distinct()

#test trial analyses
encAcc<-testTrial %>%
  dplyr::group_by(participant, exp) %>% 
  dplyr::summarise(avg = mean(corr))
encAcc
t.test(encAcc$avg, mu = 0.5)

encAccSeq<-testTrial %>%
  dplyr::group_by(sequence) %>% 
  dplyr::summarise(avg = mean(corr))
encAccSeq
summary(m<-glmer(corr~sequence+(1|participant),family = "binomial", data = testTrial))
confint(m, method = "Wald")

#recall data
score<-recall_data %>% 
  dplyr::summarise(avg = mean(score, na.rm = T))
score
score<-recall_data %>% 
  dplyr::group_by(display) %>% 
  dplyr::summarise(avg = mean(score, na.rm = T))
score

score<-recall_data %>% 
  dplyr::group_by(participant) %>% 
  dplyr::filter(!is.na(score))
wilcox.test(score~display, data = score, paired = TRUE)


#anticipation task analsis------------------
pred <- pred %>% 
  dplyr::mutate(trialType_e = ifelse(trialType == "valid", -0.5, 0.5), 
                corrAnsDistance_e = ifelse(corrAnsDistance == 1, -1.5, 
                                           ifelse(corrAnsDistance == 2, -0.5, 
                                                  ifelse(corrAnsDistance == 3, 0.5,1.5))),
                incorrAnsDistance_e = ifelse(incorrAnsDistance == 1, -1.5, 
                                             ifelse(incorrAnsDistance == 2, -0.5, 
                                                    ifelse(incorrAnsDistance == 3, 0.5, 1.5))),
                corrFilter.e  = ifelse(corrFilter == "sponge.png", 0.5, -0.5), 
                sequence.e = ifelse(sequence == "A", -0.5, 0.5), 
                delay = ifelse(`Task Name` == "Prediction - Day 1", -0.5, 0.5),
                earlyLate = ifelse((trialNum < 80 & delay == -0.5), -0.5, ifelse((trialNum >= 80 & delay == -0.5), 0.5, ifelse((trialNum > 160 & delay == 0.5),-0.5,0.5))))

pred$RT<-as.numeric(pred$`Reaction Time.y`)

#overall task performance
predDelayAccShort<-pred %>% 
  dplyr::filter(delay == -0.5) %>% 
  dplyr::group_by(participant, exp) %>% 
  dplyr::summarise(avg = mean(corr, na.rm = T))
predDelayAccShort
t.test(predDelayAccShort$avg, mu = 0.5)

predDelayAccLong<-pred %>% 
  dplyr::filter(delay == 0.5) %>% 
  dplyr::group_by(participant, exp) %>% 
  dplyr::summarise(avg = mean(corr, na.rm = T))
predDelayAccLong
t.test(predDelayAccLong$avg, mu = 0.5)


predSeqAcc<-pred %>% 
  dplyr::group_by(sequence, exp, delay) %>% 
  dplyr::summarise(avg = mean(corr, na.rm = T))
predSeqAcc
summary(m<-glmer(corr~delay*exp*sequence.e+(delay*exp*sequence.e|participant), family = "binomial", data = pred))
confint(m, "sequence.e", level = 0.95, method = "Wald")

summary(m<-lmer(RT~delay*exp*sequence.e+(delay+sequence.e|participant), data = pred))
plot_model(m, type = "pred", terms = c("delay", "sequence.e"))
confint(m, "sequence.e", level = 0.95, method = "Wald")
confint(m, "delay:sequence.e", level = 0.95, method = "Wald")

summary(m<-glmer(corr~delay*exp*corrFilter.e+(delay*corrFilter.e|participant), family = "binomial", data = pred))
confint(m, "corrFilter.e", level = 0.95, method = "Wald")

summary(m<-lmer(RT~delay*exp*corrFilter.e+(delay*corrFilter.e|participant), data = pred))
confint(m, "corrFilter.e", level = 0.95, method = "Wald")

#primary analyses
predTrialTypeDistInvEff<-pred %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(participant, trialType_e, delay, corrAnsDistance_e, exp) %>% #, sequence.e, corrFilter.e
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) 
predTrialTypeDistInvEff
predTrialTypeDistInvEff$participant<-as.factor(predTrialTypeDistInvEff$participant)

#main model
summary(invEffmod<-lmer(invEff~trialType_e*corrAnsDistance_e*delay*exp+(1+trialType_e+corrAnsDistance_e+delay|participant), data = predTrialTypeDistInvEff))
confint(invEffmod, method = "Wald")

# forest plot 
coefPlotPrep<-tidy(invEffmod, conf.int = TRUE,effects="fixed") 
coefPlotPrep<-coefPlotPrep %>% 
  dplyr::filter(effect == "fixed") %>%
  dplyr::filter(term != "(Intercept)") %>% 
  dplyr::mutate(group = as.factor(ifelse(term =="corrAnsDistance_e", "Effects of Interest", ifelse(term =="delay","Effects of Interest",ifelse(term == "trialType_e","Effects of Interest",
                                                                                                                                               ifelse(term == "trialType_e:delay", "Effects of Interest",ifelse(term == "corrAnsDistance_e:delay","Effects of Interest",ifelse(term == "trialType_e:corrAnsDistance_e","Effects of Interest","Other Effects"))))))),
                term = factor(term, levels = c("corrFilter.e","sequence.e","trialType_e:corrAnsDistance_e:delay:exp","corrAnsDistance_e:delay:exp","trialType_e:delay:exp","trialType_e:corrAnsDistance_e:exp","trialType_e:corrAnsDistance_e:delay",
                                               "delay:exp","corrAnsDistance_e:exp","trialType_e:exp","trialType_e:corrAnsDistance_e","exp",
                                               "trialType_e:delay","corrAnsDistance_e:delay", "delay","trialType_e","corrAnsDistance_e")))

fp_invEff <- ggplot(data=coefPlotPrep, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = group)) +
  geom_pointrange(aes(group = group),size = 0.7) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  facet_grid(rows = vars(group), scales = "free_y", space = "free_y", switch = "y")+
  xlab("") + ylab("Coefficients") +
  scale_x_discrete(labels = c("corrAnsDistance_e" = "Steps", "delay"="Delay","trialType_e" = "Trial Type", "corrAnsDistance_e:delay"="Steps*Delay", 
                              "exp" = "Delay Length", "trialType_e:delay"="Trial Type*Delay", 
                              "corrAnsDistance_e:exp" = "Steps*Delay Length", "trialType_e:exp" = "Trial Type*Delay Length", "delay:exp" = "Delay*Delay Length", 
                              "corrAnsDistance_e:delay:exp" = "Steps*Delay*Delay Length","trialType_e:delay:exp" = "Trial Type*Delay*Delay Length", 
                              "trialType_e:corrAnsDistance_e:delay:exp" = "Trial Type*Steps*Delay*Delay Length", "trialType_e:corrAnsDistance_e" = "Trial Type*Steps", 
                              "trialType_e:corrAnsDistance_e:delay"="Trial Type*Steps*Delay", "trialType_e:corrAnsDistance_e:exp"="Trial Type*Steps*Delay Length", "sequence.e" = "Sequence", "corrFilter.e" = "Filter"))+ 
  scale_color_manual(values = c("black","#808080"))+ 
  guides(col = FALSE)+
  theme_classic(base_size = 22)  # use a white background
print(fp_invEff)
ggsave("~/Desktop/Research/Prediction&Consolidation/exp3/graphs/coefPlot_distXdelayInvEff.png", w = 7.5, h = 9)

#corr distance inv eff plot
predDistInvEff<-pred %>%
  dplyr::group_by(participant, delay, corrAnsDistance_e) %>%
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avgCorr = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avgCorr) 
predDistInvEff

effects_corDist <- effects::effect(term= "corrAnsDistance_e:delay", mod= invEffmod)
summary(effects_corDist)
x_corDist <- as.data.frame(effects_corDist)
x_corDist<-x_corDist %>% 
  filter(delay == 0.5 | delay == -0.5) %>% 
  mutate(delay = as.factor(delay))
DistDelayPlot<-ggplot()+ #guts dprime_dist, aes(x = corrDist, y = dprime)
  geom_point(data =predDistInvEff, mapping=aes(x = corrAnsDistance_e,y=invEff, 
                                               color = as.factor(delay)),
             height = 0, width = 0.15, size = 1, alpha = 0.3,position=position_jitterdodge(jitter.height = 0, dodge.width=0.5))+
  geom_line(data=x_corDist, aes(x=corrAnsDistance_e, y=fit, color = delay, group = delay), size = 1.5) +
  geom_ribbon(data= x_corDist, aes(x=corrAnsDistance_e, ymin=lower, ymax=upper, group = delay, fill = delay), alpha= 0.3) +
  scale_color_manual(name = "Delay", labels = c("immediate", "delayed"),values=c("#168E7F","#af8dc3")) +
  scale_fill_manual(name = "Delay",labels = c("immediate", "delayed"),values=c("#168E7F","#af8dc3")) +
  scale_x_continuous(breaks=c(-1.5, -0.5, 0.5, 1.5), labels = c(1,2,3,4))+
  xlab("Steps into the Future") +
  ylab("Inverse Efficiency")+
  theme_classic(base_size = 22)
DistDelayPlot
ggsave("~/Desktop/Research/Prediction&Consolidation/exp3/graphs/figureReady_distXdelayinvEff.png", w = 7, h = 5)

#trial type inv eff plot
predTrialTypeInvEff<-pred %>%
  dplyr::group_by(participant, delay, trialType_e) %>%
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avgCorr = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avgCorr) %>% 
  dplyr::select(-c(avgRT, avgCorr)) %>% 
  pivot_wider(names_from = trialType_e, names_prefix = "trialType_", values_from = invEff) %>% 
  mutate(invEffDiff = `trialType_0.5`-`trialType_-0.5`, 
         delay = as.factor(delay))
predTrialTypeInvEff

effects_trialType <- effects::effect(term= "trialType_e:delay", mod= invEffmod)
summary(effects_trialType)
x_trialType <- as.data.frame(effects_trialType)
x_trialType<-x_trialType %>% 
  filter(delay == 0.5 | delay == -0.5) %>%
  filter(trialType_e == 0.5 | trialType_e == -0.5) %>% 
  mutate(delay = as.factor(delay))

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
invEffValidDiffPlots<-ggplot(predTrialTypeInvEff, aes(x = delay, y=invEffDiff, color = delay)) +   
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, alpha=.5, stroke=3, 
  #            fill = 'white', position = position_nudge(x = -.2, y = 0)) + 
  geom_point(size = 1, alpha=.5, position = position_jitterdodge(jitter.width = 0.2,
                                                                 dodge.width = 0.7)) + 
  xlab("Delay") + ylab("Invalid - Valid \n Inverse Efficiency") +
  geom_flat_violin(aes(fill = delay),
                   position = position_dodge(width = 0.1), alpha = .6) + 
  #geom_point(data=x_trialType, aes(x=trialType_e, y=fit, color = delay, group = delay), size = 2)+
  stat_summary(fun.data=mean_cl_boot,  geom="pointrange",lwd=1, fatten=2.25,
               position = position_nudge(x = .15, y = 0)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  scale_color_manual(name = "Delay", labels = c("Immediate", "Delayed"),values=c("#168E7F","#af8dc3")) +
  scale_fill_manual(name = "Delay", labels = c("Immediate", "Delayed"),values=c("#168E7F","#af8dc3")) +
  scale_x_discrete(breaks=c(-0.5, 0.5), labels = c("Immediate", "Delayed"))+
  #scale_color_brewer(palette = 'Accent') +
  #scale_fill_brewer(palette = 'Accent')+
  theme(legend.position="none")+
  #coord_cartesian(ylim= c(-400,400))+
  theme_classic(base_size = 22)

invEffValidDiffPlots
ggsave("~/Desktop/Research/Prediction&Consolidation/exp3/graphs/figureReady_invEffDiff.png", w = 7, h = 5)


##granularity separately
predGran<-pred %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(participant, incorrAnsDistance_e) %>% 
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) 
predGran

##granularity model
summary(invEffmodGran<-lmer(invEff~incorrAnsDistance_e+(1+incorrAnsDistance_e|participant), data = predGran))
confint(invEffmodGran, method = "Wald")


#rt model for supplement
summary(lmer(RT~trialType_e*corrAnsDistance_e*delay*exp+(1+trialType_e+corrAnsDistance_e+delay||participant), data = pred, subset = (corr == 1)))

#acc model for supplement
summary(glmer(corr~trialType_e*corrAnsDistance_e*delay*exp+(1+trialType_e+corrAnsDistance_e+delay|participant),family = "binomial", data = pred))

#early late analysis
predTrialTypeDistInvEff<-pred %>%
  dplyr::ungroup() %>% 
  #dplyr::filter(earlyLate == -0.5) %>% 
  dplyr::group_by(participant,trialType_e, delay, corrAnsDistance_e, earlyLate, exp) %>% #, sequence.e, corrFilter.e
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) %>% 
  dplyr::filter(invEff != Inf)
predTrialTypeDistInvEff
predTrialTypeDistInvEff$participant<-as.factor(predTrialTypeDistInvEff$participant)
summary(invEffmod<-lmer(invEff~trialType_e*earlyLate*corrAnsDistance_e*exp+(1|participant), data = predTrialTypeDistInvEff, subset = (delay ==-0.5)))
confint(invEffmod, method ='Wald')

#day 1 late to day 2/7 early
predTrialTypeDistInvEff<-predTrialTypeDistInvEff %>% 
  dplyr::filter((delay == -0.5 & earlyLate == 0.5)|((delay == 0.5 & earlyLate == -0.5)))
summary(invEffmod<-lmer(invEff~trialType_e*delay*corrAnsDistance_e*exp+(1+delay+corrAnsDistance_e|participant), data = predTrialTypeDistInvEff))
confint(invEffmod, method = 'Wald')

#only 1 day
summary(invEffmod<-lmer(invEff~trialType_e*delay*corrAnsDistance_e+(1+delay+corrAnsDistance_e|participant), data = predTrialTypeDistInvEff, subset = (exp == -0.5)))
confint(invEffmod, method = 'Wald')

#only day 7
summary(invEffmod<-lmer(invEff~trialType_e*delay*corrAnsDistance_e+(1+delay+corrAnsDistance_e|participant), data = predTrialTypeDistInvEff, subset = (exp == 0.5)))
confint(invEffmod, method = 'Wald')


#time series plot
predSub<-pred %>% 
  dplyr::filter((delay == -0.5 & earlyLate == 0.5)|((delay == 0.5 & earlyLate == -0.5)))
timeseries_plot<-predSub %>% 
  dplyr::mutate(Trial = ifelse(delay == 0.5, trialNum-160, trialNum)) %>%
  dplyr::group_by(delay, Trial) %>% 
  dplyr::summarise(avg= mean(RT, na.rm =T))
p <- ggplot(timeseries_plot, aes(x=Trial, y=avg)) +
  geom_line()+
  facet_grid(~delay, scales = "free")+
  ylab('Average RT (ms)')
p


#try interacting sequence and gran with trial type and distance
#sequence
predInvEffCheck<-pred %>%
  dplyr::group_by(participant, sequence.e, corrAnsDistance_e) %>% 
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) %>% 
  dplyr::filter(invEff != Inf)
predInvEffCheck
summary(invEffGranMod<-lmer(invEff~sequence.e*corrAnsDistance_e+(1+sequence.e+corrAnsDistance_e|participant), data = predInvEffCheck))

predInvEffCheck<-pred %>%
  dplyr::group_by(participant, sequence.e, trialType_e) %>% 
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) %>% 
  dplyr::filter(invEff != Inf)
predInvEffCheck
summary(invEffGranMod<-lmer(invEff~sequence.e*trialType_e+(1+sequence.e+trialType_e|participant), data = predInvEffCheck))

#granularity
predInvEffCheck<-pred %>%
  dplyr::group_by(participant, incorrAnsDistance_e, corrAnsDistance_e) %>% 
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) %>% 
  dplyr::filter(invEff != Inf)
predInvEffCheck
summary(invEffGranMod<-lmer(invEff~incorrAnsDistance_e*corrAnsDistance_e+(1+incorrAnsDistance_e+corrAnsDistance_e||participant), data = predInvEffCheck))

predInvEffCheck<-pred %>%
  dplyr::group_by(participant, incorrAnsDistance_e, trialType_e) %>% 
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) %>% 
  dplyr::filter(invEff != Inf)
predInvEffCheck
summary(invEffGranMod<-lmer(invEff~incorrAnsDistance_e*trialType_e+(1+incorrAnsDistance_e+trialType_e|participant), data = predInvEffCheck))

