library(lme4)
library(lmerTest)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sjPlot)
library(arm)
library(psych)

load("a.models.RData")
load("p.models.RData")

models=rbind(a.models, p.models)

models$lowci=models$Estimate-1.96*models$`Std. Error`
models$highci=models$Estimate+1.96*models$`Std. Error`

models$sig=ifelse((models$outcome=='life satisfaction' | models$outcome== 'relationship satisfaction') & models$`Pr(>|t|)`< .0125, 1, 
                  ifelse((models$outcome=='self-esteem' | models$outcome== 'intimacy'| models$outcome== 'admiration'
                         | models$outcome== 'dominance'| models$outcome== 'conflict') & models$`Pr(>|t|)`< .005, 1, 0))

models$outcome = factor(models$outcome, levels=c('conflict','dominance', 'admiration',
                                                 'intimacy', 'relationship satisfaction', 'self-esteem', 'life satisfaction'))

####plots
##selection
models.selection=subset(models, models$time=='selection')
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.position='none')

p=ggplot(models.selection, aes(y=outcome, x=Estimate, xmin=lowci, xmax=highci))+
  geom_point(color = 'black')+
  geom_point(data=subset(models.selection, sig==1), color='red', 
             shape=18, size=3)+
  geom_point(data=subset(models.selection, sig==0), color='black', 
             shape=18, size=3)+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(-0.7, 0.7), name='Effects')+
  ylab('Outcomes')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_grid(outcome.type~type, scales= 'free', space='free', labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  apatheme
p
ggsave(p, file='figure selection.jpeg', height=7, width=15, units = "cm", dpi=300)

##baseline change
models.selection=subset(models, models$time=='post-event change')
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.position='none')

p=ggplot(models.selection, aes(y=outcome, x=Estimate, xmin=lowci, xmax=highci))+
  geom_point(color = 'black')+
  geom_point(data=subset(models.selection, sig==1), color='red', 
             shape=18, size=3)+
  geom_point(data=subset(models.selection, sig==0), color='black', 
             shape=18, size=3)+
  geom_errorbarh(height=.1)+
  scale_x_continuous(limits=c(-0.7, 0.7), name='Effects')+
  ylab('Outcomes')+
  geom_vline(xintercept=0, color='black', linetype='dashed')+
  facet_grid(outcome.type~type, scales= 'free', space='free', labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  apatheme
p
ggsave(p, file='figure post event change.jpeg', height=7, width=15, units = "cm", dpi=300)


