library(lme4)
library(lmerTest)
library(sjPlot)
library(patchwork)
library(tidyverse)
library(pwr)
library(lsr)

theme_set(theme_light(base_size = 40))
theme_update(text = element_text(size=20),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank())


pred<-read_csv("~/Desktop/Research/Prediction&Consolidation/osf/pred_controlExp.csv")


#exclusions------------------------------
#how much are people responding overall?
attemptNum<-pred %>%
  dplyr::group_by(participant) %>% 
  dplyr::summarise(total = sum(attempt_clean, na.rm =T))
attemptNum

#overall acc
predAcc<-pred %>%
  dplyr::group_by(participant) %>% 
  dplyr::summarise(avg = mean(corr))
predAcc

binom.test(91, 160, p = 0.5,
           alternative = c("greater"),
           conf.level = 0.95)

exclude<-predAcc %>%
  dplyr::filter(avg < 0.56875) #excluding numerically above chance-0.5 
exclude

pred<-pred %>% 
  dplyr::filter(!(participant %in% exclude$participant))

predAcc<-pred %>%
  dplyr::summarise(avg = mean(corr), 
                   std = sd(corr))
predAcc


##anticipation task analyses-------
pred <- pred %>% 
  dplyr::mutate(trialType_e = ifelse(trialType == "valid", -0.5, 0.5), 
                corrAnsDistance_e = ifelse(corrAnsDistance == 1, -1.5, 
                                   ifelse(corrAnsDistance == 2, -.5, 
                                          ifelse(corrAnsDistance == 3, 0.5, 1.5))),
                incorrAnsDistance_e = ifelse(incorrAnsDistance == 1, -1.5, 
                                           ifelse(incorrAnsDistance == 2, -0.5, 
                                                  ifelse(incorrAnsDistance == 3, 0.5, 1.5))),
                corrFilter.e  = ifelse(corrFilter == "sponge.png", 0.5, -0.5), 
                sequence.e = ifelse(sequence == "A", -0.5, 0.5))

#overall task performance
pred$RT<-as.numeric(pred$`Reaction Time.y`)
avg<-pred %>% 
  dplyr::group_by(participant) %>% 
  dplyr::summarise(avg = mean(corr, na.rm = T)) 
avg
t.test(avg$avg, mu = 0.5)

#primary analyses
#inverse efficiency
predInvEffValidDist<-pred %>%
  dplyr::group_by(participant, trialType_e, corrAnsDistance_e) %>% 
  dplyr::summarise(avgRT = mean(RT, na.rm = T), 
                   avg = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avg) %>% 
  dplyr::filter(invEff != Inf)
predInvEffValidDist

summary(invEffMod<-lmer(invEff~trialType_e*corrAnsDistance_e+(1+trialType_e+corrAnsDistance_e|participant), data = predInvEffValidDist))
confint(invEffMod, method = "Wald")

#plots
#forest plot for inv eff distance
coefPlotPrep<-broom.mixed::tidy(invEffMod, conf.int = TRUE,effects="fixed") # - new USE THIS
coefPlotPrep<-coefPlotPrep %>% 
  dplyr::filter(effect == "fixed") %>% 
  dplyr::filter(term != "(Intercept)")%>% 
  dplyr::mutate(group = as.factor(ifelse(term =="corrAnsDistance_e", "Effects of Interest", ifelse(term =="trialType_e", "Effects of Interest", ifelse(term =="trialType_e:corrAnsDistance_e", "Effects of Interest", "Other Effects")))),
                term = factor(term, levels = c("trialType_e:corrAnsDistance_e","trialType_e","corrAnsDistance_e")))

fp_inveff <- ggplot(data=coefPlotPrep, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, color = group)) +
  ##scale_color_manual(values= c("black","#808080"))+
  geom_pointrange(aes(group = group),size = 1) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  #facet_wrap(~group,ncol = 1, scales = "free")+
  coord_flip() +  # flip coordinates (puts labels on y axis)
  facet_grid(rows = vars(group), scales = "free_y", space = "free_y", switch = "y")+
  xlab("") + ylab("Coefficients") +
  scale_x_discrete(labels = c("corrAnsDistance_e" = "Steps", "trialType_e" = "Trial Type", "trialType_e:corrAnsDistance_e" = "Steps*Trial Type", "sequence.e" = "Sequence", "corrFilter.e" = "Filter"))+ 
  scale_color_manual(values = c("black","#808080"))+ ##values= c("black","black","#808080","#808080","black","#808080","#808080","#808080","#808080","#808080","#808080")
  guides(col = FALSE)+
  #annotate("rect", xmin = 8, xmax = 9, ymin = -200, ymax = 100, alpha = 0.2, fill = "white") +
  #geom_rect(xmin = 8, xmax = 9, ymin = -200, ymax = 100, alpha = 1, fill = "white") +
  theme_classic(base_size = 22)  # use a white background
print(fp_inveff)
ggsave("~/Desktop/Research/Prediction&Consolidation/indoorOutdoor/coefPlot_distXdelayInvEFf.png", w = 5.8, h = 6.8) #w = 7.5, h = 9

#corr distance inv eff plot
predDistInvEff<-pred %>%
  dplyr::group_by(participant, corrAnsDistance_e) %>%
  ##dplyr::mutate(logRT = log(`Reaction Time.y`)) %>% 
  #dplyr::filter(corr == 1) %>% 
  dplyr::summarise(avgRT = mean(`Reaction Time.y`, na.rm = T), 
                   avgCorr = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avgCorr) 
predDistInvEff

effects_corDist <- effects::effect(term= "corrAnsDistance_e", mod= invEffMod)
summary(effects_corDist)
x_corDist <- as.data.frame(effects_corDist)

DistDelayPlot<-ggplot()+ #guts dprime_dist, aes(x = corrDist, y = dprime)
  geom_jitter(data =predDistInvEff, mapping=aes(x = corrAnsDistance_e,y=invEff),
              height = 0, width = 0.15, size = 1, alpha = 0.3,color = "#168E7F")+
  geom_line(data=x_corDist, aes(x=corrAnsDistance_e, y=fit), size = 1.5, color = "#168E7F") +
  geom_ribbon(data= x_corDist, aes(x=corrAnsDistance_e, ymin=lower, ymax=upper), fill = "#168E7F", alpha= 0.3) +
  #scale_color_manual(name = "Delay", labels = c("short", "long"),values=c("#168E7F","#af8dc3")) +
  ##scale_fill_manual(name = "Delay",labels = c("short", "long"),values=c("#168E7F","#af8dc3")) +
  scale_x_continuous(breaks=c(-1.5, -0.5, 0.5, 1.5), labels = c(1,2,3,4))+
  xlab("Steps into the Future") +
  ylab("Inverse Efficiency")+
  #geom_hline(yintercept = 0.5, linetype = "dashed", color = "black")+
  #ylim(0,6300)+
  theme_classic(base_size = 22)
DistDelayPlot
ggsave("~/Desktop/Research/Prediction&Consolidation/indoorOutdoor/dist_indOut.png", w = 5, h = 5)

##trial type plot
predTrialTypeInvEff<-pred %>%
  dplyr::group_by(participant, trialType_e) %>%
  ##dplyr::mutate(logRT = log(`Reaction Time.y`)) %>% 
  #dplyr::filter(corr == 1) %>% 
  dplyr::summarise(avgRT = mean(`Reaction Time.y`, na.rm = T), 
                   avgCorr = mean(corr, na.rm = T)) %>% 
  dplyr::mutate(invEff = avgRT/avgCorr) %>% 
  select(-c(avgRT, avgCorr)) %>% 
  pivot_wider(names_from = trialType_e, names_prefix = "trialType_", values_from = invEff) %>% 
  mutate(invEffDiff = `trialType_0.5`-`trialType_-0.5`,
         x = 1)
predTrialTypeInvEff

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
invEffValidDiffPlots<-ggplot(predTrialTypeInvEff, aes(y=invEffDiff, x = x)) +   
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, alpha=.5, stroke=3, 
  #            fill = 'white', position = position_nudge(x = -.2, y = 0)) + 
  geom_jitter(width = 0.05, size = 1.5, alpha=.5,color = "#168E7F") + 
  xlab(" ") + ylab("Invalid - Valid \n Inverse Efficiency") +
  geom_flat_violin(fill = "#168E7F",
                   position = position_dodge(width = 0.1), alpha = .6) + 
  stat_summary(fun.data=mean_cl_boot,  geom="pointrange",lwd=1.5, fatten=2.25,
               position = position_nudge(x = .15, y = 0),color = "#168E7F") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  #3scale_color_manual(name = "Delay", labels = c("short", "long"),values=c("#168E7F","#af8dc3")) +
  ##scale_fill_manual(name = "Delay", labels = c("short", "long"),values=c("#168E7F","#af8dc3")) +
  scale_x_continuous(breaks=c(1, 1.2), labels = c(" ", " "))+
  #scale_color_brewer(palette = 'Accent') +
  #scale_fill_brewer(palette = 'Accent')+
  theme(legend.position="none")+
  #coord_cartesian(ylim= c(-400,400))+
  theme_classic(base_size = 22)
invEffValidDiffPlots
ggsave("~/Desktop/Research/Prediction&Consolidation/indoorOutdoor/figureReady_invEffDiffTT.png", w = 5, h = 5)

