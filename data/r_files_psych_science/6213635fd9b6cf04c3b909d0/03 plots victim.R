

library(dplyr)
library(reshape2)
library(psych)
library(haven)
library(ggplot2)
library(sjPlot)
library(lme4)
library(lmerTest)
library(stringr)
library(tidyr)


library(car)
recode.car <- car::recode

load("anchor.final.RData")

names(anchor.final)

######################create time indicators for controls
#assign controls the event date of the matched cheater
anchor.final.ind=as.data.frame(anchor.final)

table(anchor.final.ind$event.group, anchor.final.ind$pr.inf1.te)

#wave when cheating occured
anchor.final.ind=anchor.final.ind %>% group_by(subclass) %>% mutate(pr.inf1ev.wav.c=max(pr.inf1ev.wav, na.rm=T))
table(anchor.final.ind$pr.inf1ev.wav.c, anchor.final.ind$event.group) #use this one for controls
table(anchor.final.ind$pr.inf1ev.wav, anchor.final.ind$event.group)

anchor.final.ind$pr.inf1.te.c <- anchor.final.ind$wave - anchor.final.ind$pr.inf1ev.wav.c
table(anchor.final.ind$event.group, anchor.final.ind$pr.inf1.te)
table(anchor.final.ind$event.group, anchor.final.ind$pr.inf1.te.c)

anchor.final.ind$time=ifelse(anchor.final.ind$event.group=='control', anchor.final.ind$pr.inf1.te.c, 
                             anchor.final.ind$pr.inf1.te)
table(anchor.final.ind$time, exclude = T)

anchor.final.ind$pr.inf1.preD <- ifelse(anchor.final.ind$time<0, -1, 0)
anchor.final.ind$pr.inf1.postD <- ifelse((anchor.final.ind$time>0), 1, 0)
anchor.final.ind$pr.inf1.preLin <- ifelse(anchor.final.ind$time<0, (anchor.final.ind$time), 0)
anchor.final.ind$pr.inf1.postLin <- ifelse(anchor.final.ind$time>0, (anchor.final.ind$time), 0)


write_sav(anchor.final.ind, "anchor.final.ind.sav")
save(anchor.final.ind, file="anchor.final.ind.RData")

load("anchor.final.ind.RData")

####lsa
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.lsa)),
                                                                   n.p=sum(!is.na(z.pr.lsa.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.lsa=mean(z.an.lsa, na.rm=T),
                                                           z.pr.lsa.use=mean(z.pr.lsa.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.lsa, z.pr.lsa.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.lsa:z.pr.lsa.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)

means.long$outcome=ifelse(means.long$outcome=='z.an.lsa', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

lsa.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T, span=1) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Life satisfaction")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 39, 83, 146, 447),
             labels = c("3", "39", "83", "146", "447"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
lsa.plot
ggsave(lsa.plot, file='lsa.plot.raw.jpeg', height=6, width =15, units = "cm", dpi=300)


####sel
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.sel)),
                                                                   n.p=sum(!is.na(z.pr.sel.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.sel=mean(z.an.sel, na.rm=T),
                                                           z.pr.sel.use=mean(z.pr.sel.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.sel, z.pr.sel.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.sel:z.pr.sel.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)
table(means.long$n, exclude = T)

means.long$outcome=ifelse(means.long$outcome=='z.an.sel', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

sel.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Self-esteem")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 34, 77, 146, 435),
             labels = c("3", "34", "77", "146", "435"), guide="legend")+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
sel.plot
ggsave(sel.plot, file='sel.plot.raw.jpeg', height=6, width = 15, units = "cm", dpi=300)

####rsa
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.rsa.use)),
                                                                   n.p=sum(!is.na(z.pr.rsa.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.rsa.use=mean(z.an.rsa.use, na.rm=T),
                                                           z.pr.rsa.use=mean(z.pr.rsa.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.rsa.use, z.pr.rsa.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.rsa.use:z.pr.rsa.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)

means.long$outcome=ifelse(means.long$outcome=='z.an.rsa.use', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

rsa.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Relationship satisfaction")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 32, 68, 110, 287),
             labels = c("3", "32", "68", "110", "287"), guide="legend")+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
rsa.plot
ggsave(rsa.plot, file='rsa.plot.raw.jpeg', height=6, width = 15, units = "cm", dpi=300)

####rin
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.rin.use)),
                                                                   n.p=sum(!is.na(z.pr.rin.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.rin.use=mean(z.an.rin.use, na.rm=T),
                                                           z.pr.rin.use=mean(z.pr.rin.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.rin.use, z.pr.rin.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.rin.use:z.pr.rin.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)

means.long$outcome=ifelse(means.long$outcome=='z.an.rin.use', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

rin.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Intimacy")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 32, 67, 110, 277),
             labels = c("3", "32", "67", "110", "277"), guide="legend")+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
rin.plot
ggsave(rin.plot, file='rin.plot.raw.jpeg', height=6, width = 15, units = "cm", dpi=300)

####rad
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.rad.use)),
                                                                   n.p=sum(!is.na(z.pr.rad.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.rad.use=mean(z.an.rad.use, na.rm=T),
                                                           z.pr.rad.use=mean(z.pr.rad.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.rad.use, z.pr.rad.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.rad.use:z.pr.rad.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)

means.long$outcome=ifelse(means.long$outcome=='z.an.rad.use', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

rad.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Admiration")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 32, 67, 110, 277),
             labels = c("3", "32", "67", "110", "277"), guide="legend")+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
rad.plot
ggsave(rad.plot, file='rad.plot.raw.jpeg', height=6, width = 15, units = "cm", dpi=300)

####rdo
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.rdo.use)),
                                                                   n.p=sum(!is.na(z.pr.rdo.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.rdo.use=mean(z.an.rdo.use, na.rm=T),
                                                           z.pr.rdo.use=mean(z.pr.rdo.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.rdo.use, z.pr.rdo.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.rdo.use:z.pr.rdo.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)

means.long$outcome=ifelse(means.long$outcome=='z.an.rdo.use', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

rdo.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Dominance")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 32, 67, 110, 275),
             labels = c("3", "32", "67", "110", "275"), guide="legend")+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
rdo.plot
ggsave(rdo.plot, file='rdo.plot.raw.jpeg', height=6, width = 15, units = "cm", dpi=300)

####rcf
means=anchor.final.ind %>% group_by(event.group, time)  %>% mutate(n.a=sum(!is.na(z.an.rcf.use)),
                                                                   n.p=sum(!is.na(z.pr.rcf.use)))
means=means %>% group_by(event.group, time)  %>% summarise(z.an.rcf.use=mean(z.an.rcf.use, na.rm=T),
                                                           z.pr.rcf.use=mean(z.pr.rcf.use, na.rm=T),
                                                           n.a=mean(n.a, na.rm=T),
                                                           n.p=mean(n.p, na.rm=T))
means1=subset(means, select=c(event.group, time, z.an.rcf.use, z.pr.rcf.use))                                                                  
means.long1 <- means1 %>% gather(outcome, value, z.an.rcf.use:z.pr.rcf.use)
means2=subset(means, select=c(event.group, time, n.a, n.p))                                                                  
means.long2 <- means2 %>% gather(outcome, n, n.a:n.p)
means.long2=means.long2[, 4]
means.long=cbind(means.long1, means.long2)
summary(means.long$n)

means.long$outcome=ifelse(means.long$outcome=='z.an.rcf.use', 'Actor outcome', 'Partner outcome')
means.long$event.group=ifelse(means.long$event.group=='cheater', 'Actor victim', 'Control')

rcf.plot=ggplot(means.long, aes(x=time, y=value, color=event.group, linetype=event.group, weight=n))+
  geom_smooth(method="loess", aes(x=time, y=value, fill=event.group), se=T) +  ylim(-1,1)+ 
  theme_classic()+labs(x = "Years", y="Conflict")+geom_point(aes(x=time, y=value, size=n), alpha=.2)+
  scale_size(breaks = c(3, 32, 67, 110, 277),
             labels = c("3", "32", "67", "110", "277"), guide="legend")+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "right")+ geom_vline(xintercept = 0, linetype="dotted")+
  theme(legend.title=element_blank())+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash"))+
  guides(fill=F)+
  guides(color=guide_legend(override.aes=list(fill=NA)))
rcf.plot
ggsave(rcf.plot, file='rcf.plot.raw.jpeg', height=6, width = 15, units = "cm", dpi=300)

