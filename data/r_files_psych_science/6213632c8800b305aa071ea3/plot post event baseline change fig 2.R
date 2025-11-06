library(lme4)
library(lmerTest)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sjPlot)
library(arm)
library(psych)

load("lsa.perp.RData")
load("lsa.vict.RData")
load("sel.perp.RData")
load("sel.vict.RData")
load("rsa.perp.RData")
load("rsa.vict.RData")
load("rin.perp.RData")
load("rin.vict.RData")
load("rad.perp.RData")
load("rad.vict.RData")
load("rdo.perp.RData")
load("rdo.vict.RData")
load("rcf.perp.RData")
load("rcf.vict.RData")

lsa=rbind(lsa.perp, lsa.vict)
lsa$time=ifelse(lsa$x==0, 'before', 'after')
lsa$time=factor(lsa$time, c('before', 'after'))

sel=rbind(sel.perp, sel.vict)
sel$time=ifelse(sel$x==0, 'before', 'after')
sel$time=factor(sel$time, c('before', 'after'))

rsa=rbind(rsa.perp, rsa.vict)
rsa$time=ifelse(rsa$x==0, 'before', 'after')
rsa$time=factor(rsa$time, c('before', 'after'))

rin=rbind(rin.perp, rin.vict)
rin$time=ifelse(rin$x==0, 'before', 'after')
rin$time=factor(rin$time, c('before', 'after'))

rad=rbind(rad.perp, rad.vict)
rad$time=ifelse(rad$x==0, 'before', 'after')
rad$time=factor(rad$time, c('before', 'after'))

rdo=rbind(rdo.perp, rdo.vict)
rdo$time=ifelse(rdo$x==0, 'before', 'after')
rdo$time=factor(rdo$time, c('before', 'after'))

rcf=rbind(rcf.perp, rcf.vict)
rcf$time=ifelse(rcf$x==0, 'before', 'after')
rcf$time=factor(rcf$time, c('before', 'after'))


ggplot(lsa, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Life satisfaction") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("lsa.dich.jpeg", height=9, width = 8, unit="cm", dpi=300)

ggplot(sel, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Self-esteem") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("sel.dich.jpeg", height=9, width = 8, unit="cm",  dpi=300)

ggplot(rsa, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Relationship satisfaction") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("rsa.dich.jpeg", height=9, width = 8, unit="cm",  dpi=300)

ggplot(rin, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Intimacy") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("rin.dich.jpeg", height=9, width = 8, unit="cm",  dpi=300)

ggplot(rad, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Admiration") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("rad.dich.jpeg", height=9, width = 8, unit="cm",  dpi=300)

ggplot(rdo, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Dominance") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("rdo.dich.jpeg", height=9, width = 8, unit="cm", dpi=300)

ggplot(rcf, aes(x=type, y=predicted, fill=time)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9))+
  ylab("Conflict") +
  xlab("") +
  scale_x_discrete(labels=c("Actor perpetrator" = "Actor \n perpetrator", "Actor victim" = "Actor \n victim"))+
  facet_grid(.~outcome, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_bw(base_size = 12)+ coord_cartesian(ylim=c(-0.5, 0.5))+
  scale_fill_manual(name = "", labels = c("before", "after"), values=c("dodgerblue2", "coral2"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("rcf.dich.jpeg", height=9, width = 8, unit="cm",  dpi=300)

