library(lme4)
library(lmerTest)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sjPlot)
library(arm)
library(psych)

load("anchor.final.RData")

load("anchor.final.ind.RData")

anchor.final.ind$pr.inf1.postD=as.factor(anchor.final.ind$pr.inf1.postD)


names(anchor.final.ind)

####commitment 
anchor.final.ind$c.commitment.first.use=scale(anchor.final.ind$commitment.first.use, scale=F)
anchor.final.ind$c.p.commitment.first.use=scale(anchor.final.ind$p.commitment.first.use, scale=F)

anchor.final$c.commitment.first.use=scale(anchor.final$commitment.first.use, scale=F)
anchor.final$c.p.commitment.first.use=scale(anchor.final$p.commitment.first.use, scale=F)

cheater=subset(anchor.final.ind, anchor.final.ind$event.group=='cheater')
controls=subset(anchor.final.ind, anchor.final.ind$event.group=='control')

summary(cheater$commitment.first.use)
summary(controls$commitment.first.use)

cheater$c.commitment.first.use=scale(cheater$commitment.first.use, scale=F)
controls$c.commitment.first.use=scale(controls$commitment.first.use, scale=F)
sd(cheater$c.commitment.first.use, na.rm=T)
sd(controls$c.commitment.first.use, na.rm=T)

cheater$c.p.commitment.first.use=scale(cheater$p.commitment.first.use, scale=F)
controls$c.p.commitment.first.use=scale(controls$p.commitment.first.use, scale=F)
sd(cheater$c.p.commitment.first.use, na.rm=T)
sd(controls$c.p.commitment.first.use, na.rm=T)

##rsa
model.rsa <- lmer (z.an.rsa.use ~ testing + pr.inf1.preva + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1+ pr.inf1.postD + pr.inf1.postLin | id)+ (1 | subclass), 
                   data=anchor.final,  control = lmerControl(calc.derivs = FALSE))
summary(model.rsa)

model.rsa <- lmer (z.an.rsa.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1 + pr.inf1.postD + pr.inf1.postLin | id), 
                     data=cheater,  control = lmerControl(calc.derivs = FALSE))
summary(model.rsa)
rsa.prelin=as.data.frame(get_model_data(model.rsa, type="pred", terms=c("pr.inf1.preLin", 
                                                                        "c.commitment.first.use[-.82, .82]")))
rsa.postlin=as.data.frame(get_model_data(model.rsa, type="pred", terms=c("pr.inf1.postLin", 
                                                                         "c.commitment.first.use[-.82, .82]")))
rsa=rbind(rsa.prelin, rsa.postlin)
rsa$type='Actor victim'

model.rsa.c <- lmer (z.an.rsa.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                       pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                       livetogether +  (1+ pr.inf1.postD + pr.inf1.postLin | id) + (1 | subclass), data=controls, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.c)
rsa.c.prelin=as.data.frame(get_model_data(model.rsa.c, type="pred", terms=c("pr.inf1.preLin", 
                                                                            "c.commitment.first.use[-.74, .74]")))
rsa.c.postlin=as.data.frame(get_model_data(model.rsa.c, type="pred", terms=c("pr.inf1.postLin", 
                                                                             "c.commitment.first.use[-.74, .74]")))
rsa.c=rbind(rsa.c.prelin, rsa.c.postlin)
rsa.c$type='Control'

rsa=rbind(rsa, rsa.c)

rsa$groupnew=ifelse(rsa$group==-.82 | rsa$group==-.74, 'Actor commitment: low', 'Actor commitment: high')
table(rsa$groupnew, exclude = T)

rsa$groupnew<-factor(rsa$groupnew, levels=c('Actor commitment: low','Actor commitment: high'))

rsa.com.plot=ggplot(rsa, aes(x=x, y=predicted, color=type, linetype=type)) + geom_smooth(method="gam", formula = y~s(x), se=F) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=type), colour=NA, alpha=.1, show.legend = F) +ylim(-1,1.5)+ 
  theme_classic()+labs(x = "Years", y="Actor relationship satisfaction")+
  facet_grid(.~groupnew, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "bottom")+ geom_vline(xintercept = 0, linetype="dotted")+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash")) + theme(legend.title=element_blank())
rsa.com.plot
ggsave(rsa.com.plot, file='rsa.com.plot.jpeg', height = 9, width = 8, units = "cm", dpi=300)

##rin
model.rin <- lmer (z.an.rin.use ~ testing + pr.inf1.preva + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1 + pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), 
                   data=anchor.final,  control = lmerControl(calc.derivs = FALSE))
summary(model.rin)

model.rin <- lmer (z.an.rin.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1+ pr.inf1.postD + pr.inf1.postLin | id), 
                   data=cheater,  control = lmerControl(calc.derivs = FALSE))
summary(model.rin)
rin.prelin=as.data.frame(get_model_data(model.rin, type="pred", terms=c("pr.inf1.preLin", 
                                                                        "c.commitment.first.use[-.82, .82]")))
rin.postlin=as.data.frame(get_model_data(model.rin, type="pred", terms=c("pr.inf1.postLin", 
                                                                         "c.commitment.first.use[-.82, .82]")))
rin=rbind(rin.prelin, rin.postlin)
rin$type='Actor victim'

model.rin.c <- lmer (z.an.rin.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                       pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                       livetogether +  (1+ pr.inf1.postD + pr.inf1.postLin | id) + (1 | subclass), data=controls, control = lmerControl(calc.derivs = FALSE))
summary(model.rin.c)
rin.c.prelin=as.data.frame(get_model_data(model.rin.c, type="pred", terms=c("pr.inf1.preLin", 
                                                                            "c.commitment.first.use[-.74, .74]")))
rin.c.postlin=as.data.frame(get_model_data(model.rin.c, type="pred", terms=c("pr.inf1.postLin", 
                                                                             "c.commitment.first.use[-.74, .74]")))
rin.c=rbind(rin.c.prelin, rin.c.postlin)
rin.c$type='Control'

rin=rbind(rin, rin.c)

rin$groupnew=ifelse(rin$group==-.82 | rin$group==-.74, 'Actor commitment: low', 'Actor commitment: high')
table(rin$groupnew, exclude = T)
rin$groupnew<-factor(rin$groupnew, levels=c('Actor commitment: low','Actor commitment: high'))

rin.com.plot=ggplot(rin, aes(x=x, y=predicted, color=type, linetype=type)) + geom_smooth(method="gam", formula = y~s(x), se=F) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=type), colour=NA, alpha=.1, show.legend = F) +ylim(-1,1.5)+ 
  theme_classic()+labs(x = "Years", y="Actor intimacy")+
  facet_grid(.~groupnew, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "bottom")+ geom_vline(xintercept = 0, linetype="dotted")+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash")) + theme(legend.title=element_blank())
rin.com.plot
ggsave(rin.com.plot, file='rin.com.plot.jpeg', height = 9, width = 8, units = "cm", dpi=300)


##rad
model.rad <- lmer (z.an.rad.use ~ testing + pr.inf1.preva + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1 + pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), 
                   data=anchor.final,  control = lmerControl(calc.derivs = FALSE))
summary(model.rad)

model.rad <- lmer (z.an.rad.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1 + pr.inf1.postD + pr.inf1.postLin| id), 
                   data=cheater,  control = lmerControl(calc.derivs = FALSE))
summary(model.rad)
rad.prelin=as.data.frame(get_model_data(model.rad, type="pred", terms=c("pr.inf1.preLin", 
                                                                        "c.commitment.first.use[-.82, .82]")))
rad.postlin=as.data.frame(get_model_data(model.rad, type="pred", terms=c("pr.inf1.postLin", 
                                                                         "c.commitment.first.use[-.82, .82]")))
rad=rbind(rad.prelin, rad.postlin)
rad$type='Actor victim'

model.rad.c <- lmer (z.an.rad.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                       pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                       livetogether +  (1 + pr.inf1.postD + pr.inf1.postLin| id) + (1 | subclass), data=controls, control = lmerControl(calc.derivs = FALSE))
summary(model.rad.c)
rad.c.prelin=as.data.frame(get_model_data(model.rad.c, type="pred", terms=c("pr.inf1.preLin", 
                                                                            "c.commitment.first.use[-.74, .74]")))
rad.c.postlin=as.data.frame(get_model_data(model.rad.c, type="pred", terms=c("pr.inf1.postLin", 
                                                                             "c.commitment.first.use[-.74, .74]")))
rad.c=rbind(rad.c.prelin, rad.c.postlin)
rad.c$type='Control'

rad=rbind(rad, rad.c)

rad$groupnew=ifelse(rad$group==-.82 | rad$group==-.74, 'Actor commitment: low', 'Actor commitment: high')
table(rad$groupnew, exclude = T)
rad$groupnew<-factor(rad$groupnew, levels=c('Actor commitment: low','Actor commitment: high'))

rad.com.plot=ggplot(rad, aes(x=x, y=predicted, color=type, linetype=type)) + geom_smooth(method="gam", formula = y~s(x), se=F) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=type), colour=NA, alpha=.1, show.legend = F) +ylim(-1,1.5)+ 
  theme_classic()+labs(x = "Years", y="Actor admiration")+
  facet_grid(.~groupnew, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "bottom")+ geom_vline(xintercept = 0, linetype="dotted")+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash")) + theme(legend.title=element_blank())
rad.com.plot
ggsave(rad.com.plot, file='rad.com.plot.jpeg', height = 9, width = 8, units = "cm", dpi=300)


##rcf
model.rcf <- lmer (z.an.rcf.use ~ testing + pr.inf1.preva + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1 + pr.inf1.postD + pr.inf1.postLin| id)+ (1 | subclass), 
                   data=anchor.final,  control = lmerControl(calc.derivs = FALSE))
summary(model.rcf)

model.rcf <- lmer (z.an.rcf.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                     pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1+ pr.inf1.postD + pr.inf1.postLin | id), 
                   data=cheater,  control = lmerControl(calc.derivs = FALSE))
summary(model.rcf)
rcf.prelin=as.data.frame(get_model_data(model.rcf, type="pred", terms=c("pr.inf1.preLin", 
                                                                        "c.commitment.first.use[-.82, .82]")))
rcf.postlin=as.data.frame(get_model_data(model.rcf, type="pred", terms=c("pr.inf1.postLin", 
                                                                         "c.commitment.first.use[-.82, .82]")))
rcf=rbind(rcf.prelin, rcf.postlin)
rcf$type='Actor victim / Partner perpetrator'

model.rcf.c <- lmer (z.an.rcf.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.commitment.first.use+
                       pr.inf1.postLin*c.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                       livetogether +  (1+ pr.inf1.postD + pr.inf1.postLin | id) + (1 | subclass), data=controls, control = lmerControl(calc.derivs = FALSE))
summary(model.rcf.c)
rcf.c.prelin=as.data.frame(get_model_data(model.rcf.c, type="pred", terms=c("pr.inf1.preLin", 
                                                                            "c.commitment.first.use[-.74, .74]")))
rcf.c.postlin=as.data.frame(get_model_data(model.rcf.c, type="pred", terms=c("pr.inf1.postLin", 
                                                                             "c.commitment.first.use[-.74, .74]")))
rcf.c=rbind(rcf.c.prelin, rcf.c.postlin)
rcf.c$type='Control'

rcf=rbind(rcf, rcf.c)

rcf$groupnew=ifelse(rcf$group==-.82 | rcf$group==-.74, 'Actor commitment: low', 'Actor commitment: high')
table(rcf$groupnew, exclude = T)
rcf$groupnew<-factor(rcf$groupnew, levels=c('Actor commitment: low','Actor commitment: high'))

rcf.com.plot=ggplot(rcf, aes(x=x, y=predicted, color=type, linetype=type)) + geom_smooth(method="gam", formula = y~s(x), se=F) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=type), colour=NA, alpha=.1, show.legend = F) +ylim(-1,1.5)+ 
  theme_classic()+labs(x = "Years", y="Actor conflict")+
  facet_grid(.~groupnew, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "bottom")+ geom_vline(xintercept = 0, linetype="dotted")+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash")) + theme(legend.title=element_blank())
rcf.com.plot
ggsave(rcf.com.plot, file='rcf.com.plot.jpeg', height = 9, width = 8, units = "cm", dpi=300)


##rsa partner
model.rsa <- lmer (z.pr.rsa.use ~ testing + pr.inf1.preva + pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                     pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1+ pr.inf1.postD + pr.inf1.postLin | id)+ (1 | subclass), 
                   data=anchor.final,  control = lmerControl(calc.derivs = FALSE))
summary(model.rsa)

model.rsa <- lmer (z.pr.rsa.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                     pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                     livetogether + cheat.sum.max + (1 + pr.inf1.postD + pr.inf1.postLin| id), 
                   data=cheater,  control = lmerControl(calc.derivs = FALSE))
summary(model.rsa)
rsa.prelin=as.data.frame(get_model_data(model.rsa, type="pred", terms=c("pr.inf1.preLin", 
                                                                        "c.p.commitment.first.use[-.77, .77]")))
rsa.postlin=as.data.frame(get_model_data(model.rsa, type="pred", terms=c("pr.inf1.postLin", 
                                                                         "c.p.commitment.first.use[-.77, .77]")))
rsa=rbind(rsa.prelin, rsa.postlin)
rsa$type='Actor victim / Partner perpetrator'

model.rsa.c <- lmer (z.pr.rsa.use ~ testing + pr.inf1.preLin + pr.inf1.postD*c.p.commitment.first.use+
                       pr.inf1.postLin*c.p.commitment.first.use + male + homosex + c.age.first + sq.age.first + married +
                       livetogether +  (1 + pr.inf1.postD + pr.inf1.postLin| id) + (1 | subclass), data=controls, control = lmerControl(calc.derivs = FALSE))
summary(model.rsa.c)
rsa.c.prelin=as.data.frame(get_model_data(model.rsa.c, type="pred", terms=c("pr.inf1.preLin", 
                                                                            "c.p.commitment.first.use[-.63, .63]")))
rsa.c.postlin=as.data.frame(get_model_data(model.rsa.c, type="pred", terms=c("pr.inf1.postLin", 
                                                                             "c.p.commitment.first.use[-.63, .63]")))
rsa.c=rbind(rsa.c.prelin, rsa.c.postlin)
rsa.c$type='Control'

rsa=rbind(rsa, rsa.c)

rsa$groupnew=ifelse(rsa$group==-.77 | rsa$group==-.63, 'Partner commitment: low', 'Partner commitment: high')
table(rsa$groupnew, exclude = T)

rsa$groupnew<-factor(rsa$groupnew, levels=c('Partner commitment: low','Partner commitment: high'))
rsa.pr.com.plot=ggplot(rsa, aes(x=x, y=predicted, color=type, linetype=type)) + geom_smooth(method="gam", formula = y~s(x), se=F) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=type), colour=NA, alpha=.1, show.legend = F) +ylim(-1,1.5)+ 
  theme_classic()+labs(x = "Years", y="Partner relationship satisfaction")+
  facet_grid(.~groupnew, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  geom_vline(xintercept = 0, linetype="dotted")+theme(legend.position = "bottom")+
  scale_color_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim / \nPartner perpetrator", "Control"), values=c("solid", "twodash")) + theme(legend.title=element_blank())
rsa.pr.com.plot
ggsave(rsa.pr.com.plot, file='rsa.pr.com.plot.jpeg', height = 9, width = 8, units = "cm", dpi=300)


###separation
##plot
cheater=subset(anchor.final.ind, anchor.final.ind$event.group=='cheater')
controls=subset(anchor.final.ind, anchor.final.ind$event.group=='control')

##lsa
model.lsa <- lmer (z.an.lsa ~ row.n +pr.inf1.preLin + pr.inf1.postD+
                     pr.inf1.postLin*sep.cat.max + male + homosex + c.age + sq.age + married +
                     livetogether  + cheat.sum.max + (1 | id), 
                   control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=cheater)
summary(model.lsa)

lsa.prelin=as.data.frame(get_model_data(model.lsa, type="pred", terms=c("pr.inf1.preLin", 
                                                                        "sep.cat.max[0, 1, 2]")))
lsa.postlin=as.data.frame(get_model_data(model.lsa, type="pred", terms=c("pr.inf1.postLin", 
                                                                         "sep.cat.max[0, 1, 2]")))
lsa=rbind(lsa.prelin, lsa.postlin)
lsa$type='Actor victim'

model.lsa.c <- lmer (z.an.lsa ~ row.n  +pr.inf1.preLin + pr.inf1.postD+
                       pr.inf1.postLin*sep.cat.max  + male + homosex + c.age + sq.age + married +
                       livetogether   + (1 | id), 
                     control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE), data=controls)
summary(model.lsa.c)

lsa.c.prelin=as.data.frame(get_model_data(model.lsa.c, type="pred", terms=c("pr.inf1.preLin", 
                                                                            "sep.cat.max[0, 1, 2]")))
lsa.c.postlin=as.data.frame(get_model_data(model.lsa.c, type="pred", terms=c("pr.inf1.postLin",
                                                                             "sep.cat.max[0, 1, 2]")))
lsa.c=rbind(lsa.c.prelin, lsa.c.postlin)
lsa.c$type='Control'

lsa=rbind(lsa, lsa.c)

lsa$groupnew=ifelse(lsa$group==0, 'Did not separate', 
                    ifelse(lsa$group==1, 'Separated /no new relationship', 'Separated / new relationship'))
table(lsa$groupnew, exclude = T)

lsa$groupnew=factor(lsa$groupnew, level=c("Did not separate", "Separated /no new relationship", "Separated / new relationship"))

lsa.sep.plot=ggplot(lsa, aes(x=x, y=predicted, color=type, linetype=type)) + geom_smooth(method="gam", formula = y~s(x), se=F) +
  geom_ribbon(aes(ymin=conf.low,ymax=conf.high, fill=type), colour=NA, alpha=.1, show.legend = F) +ylim(-1.5,1)+ 
  theme_classic()+labs(x = "Years", y="Actor life satisfaction")+
  facet_grid(.~groupnew, labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(legend.position = "bottom")+ geom_vline(xintercept = 0, linetype="dotted")+
  scale_color_manual(labels = c("Actor victim", "Control"), values = c("red", "green4")) +                 
  scale_linetype_manual(labels = c("Actor victim", "Control"), values=c("solid", "twodash")) + theme(legend.title=element_blank())
lsa.sep.plot
ggsave(lsa.sep.plot, file='lsa.sep.plot.jpeg', height = 9, width = 8, units = "cm", dpi=300)

