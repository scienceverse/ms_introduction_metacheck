library(lme4)
library(lmerTest)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sjPlot)
library(arm)
library(psych)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(tidyr)
library(apaTables)

load("anchor.final.RData")
load("anchor.final.ind.RData")


names(anchor.final)


###all means for correlations

###perpetrator and control means
###relationship values at the beginning of the flagged relationship 
anchor.final.ind=anchor.final.ind[order(anchor.final.ind$subclass, anchor.final.ind$id, anchor.final.ind$wave),]
mean.rel = anchor.final.ind %>% group_by(id, an.use) %>% summarise(married.first=first(married[which(!is.na(married))[1]]),
                                                                livetogether.first=first(livetogether[which(!is.na(livetogether))[1]]),
                                                                nkidsp.first=first(nkidsp),
                                                                reldur.first=first(reldur),
                                                                nkidsp.last=last(nkidsp),
                                                                reldur.last=last(reldur),
                                                                page.first=first(page),
                                                                p.male.first=first(p.male),
                                                                assessment.first=first(wave),
                                                                assessment.last=last(wave),
                                                                an.rsa.use=mean(an.rsa.use, na.rm=T),
                                                                an.rin.use=mean(an.rin.use, na.rm=T),
                                                                an.rad.use=mean(an.rad.use, na.rm=T),
                                                                an.rdo.use=mean(an.rdo.use, na.rm=T),
                                                                an.rcf.use=mean(an.rcf.use, na.rm=T),
                                                                p.lsa.use=mean(pr.lsa.use, na.rm=T),
                                                                p.sel.use=mean(pr.sel.use, na.rm=T),
                                                                p.rsa.use=mean(pr.rsa.use, na.rm=T),
                                                                p.rin.use=mean(pr.rin.use, na.rm=T),
                                                                p.rad.use=mean(pr.rad.use, na.rm=T),
                                                                p.rdo.use=mean(pr.rdo.use, na.rm=T),
                                                                p.rcf.use=mean(pr.rcf.use, na.rm=T),
                                                                commitment.first.use=mean(commitment.first.use, na.rm=T),
                                                                p.commitment.first.use=mean(p.commitment.first.use, na.rm=T),
                                                                cheat.sum.max=max(cheat.sum.max, na.rm=T),
                                                                sep.max=max(sep.max, na.rm=T),
                                                                prelin=min(an.inf1.preLin),
                                                                postlin=max(an.inf1.postLin))

mean.rel$nwaves=mean.rel$assessment.last-mean.rel$assessment.first+1
table(mean.rel$nwaves)

mean.rel=subset(mean.rel, mean.rel$an.use==1)                                                                                       


##check
#var=subset(mean.rel, select=c(id, an.use, married.first,  livetogether.first,
#                              nkidsp.first, nkidsp.last, reldur.first, reldur.last, page.first, p.male.first,
#                              assessment.first, assessment.last, commitment.first.use, p.commitment.first.use, cheat.sum.max, sep.max))

###individual values (relationship independent)
anchor.final.ind=anchor.final.ind[order(anchor.final.ind$subclass, anchor.final.ind$id, anchor.final.ind$wave),]
mean.rel1=anchor.final.ind %>% group_by(id) %>% summarise(event.group=first(event.group),
                                                        age.first=mean(age.first, na.rm=T),
                                                        male=first(male),
                                                        homosex=first(homosex[which(!is.na(homosex))[1]]),
                                                        row.n=max(row.n, na.rm=T),
                                                        an.lsa=mean(an.lsa, na.rm=T),
                                                        an.sel=mean(an.sel, na.rm=T),
                                                        sep.cat.max=max(sep.cat.max, na.rm=T),
                                                        prelin.all=min(an.inf1.preLin),
                                                        postlin.all=max(an.inf1.postLin))

names(mean.rel)
names(mean.rel1)

mean.rel=mean.rel[, 2:31]
means=cbind(mean.rel, mean.rel1)

save(means, file="means.RData")

load("means.RData")

names(means)

table(means$sep.max, exclude = T)
means$sep.max=ifelse(means$sep.max=='-Inf', NA, means$sep.max)
table(means$sep.cat.max, exclude = T)
means$nosep=ifelse(means$sep.cat.max==0, 1, 0)
means$single=ifelse(means$sep.max==1 & means$sep.cat.max==1, 1, 
                    ifelse(means$sep.max==0, NA, 0))
means$newrel=ifelse(means$sep.max==1 & means$sep.cat.max==2, 1, 
                    ifelse(means$sep.max==0, NA, 0))
table(means$nosep, exclude = T)
table(means$single, exclude = T)
table(means$newrel, exclude = T)

###change prelin to reflect number of waves before the event
means$prelin.abs=abs(means$prelin)+1
table(means$prelin.abs, exclude = T)
summary(means$prelin.abs)
means$prelin.all.abs=abs(means$prelin.all)+1
table(means$prelin.all.abs, exclude = T)
summary(means$prelin.all.abs)

descriptives.groups=means %>% group_by(event.group) %>% summarise(
                                                  age.first.m=mean(age.first, na.rm=T),
                                                  age.first.sd=sd(age.first, na.rm=T),
                                                  page.first.m=mean(page.first, na.rm=T),
                                                  page.first.sd=sd(page.first, na.rm=T),
                                                  male=mean(male, na.rm=T),
                                                  homosex=mean(homosex, na.rm=T),
                                                  married.first=mean(married.first, na.rm=T),
                                                  livetogether.first=mean(livetogether.first, na.rm=T),
                                                  nkidsp.first=mean(nkidsp.first, na.rm=T),
                                                  nkidsp.last=mean(nkidsp.last, na.rm=T),
                                                  reldur.first=mean(reldur.first, na.rm=T),
                                                  reldur.last=mean(reldur.last, na.rm=T),
                                                  nwaves.m=mean(row.n, na.rm=T),
                                                  nwaves.sd=sd(row.n, na.rm=T),
                                                  #testing.m=mean(testing, na.rm=T),
                                                 # testing.sd=sd(testing, na.rm=T),
                                                  prelin.m=mean(prelin.abs, na.rm=T),
                                                  prelin.sd=sd(prelin.abs, na.rm=T),
                                                  postlin.m=mean(postlin, na.rm=T),
                                                  postlin.sd=sd(postlin, na.rm=T),
                                                  prelin.all.m=mean(prelin.all.abs, na.rm=T),
                                                  prelin.all.sd=sd(prelin.all.abs, na.rm=T),
                                                  postlin.all.m=mean(postlin.all, na.rm=T),
                                                  postlin.all.sd=sd(postlin.all, na.rm=T),
                                                  sep.max=mean(sep.max, na.rm=T),
                                                  nosep=mean(nosep, na.rm=T),
                                                  newrel=mean(newrel, na.rm=T),
                                                  single=mean(single, na.rm=T),
                                                  lsa.m=mean(an.lsa, na.rm=T),
                                                  lsa.sd=sd(an.lsa, na.rm=T),
                                                  sel.m=mean(an.sel, na.rm=T),
                                                  sel.sd=sd(an.sel, na.rm=T),
                                                  rsa.use.m=mean(an.rsa.use, na.rm=T),
                                                  rsa.use.sd=sd(an.rsa.use, na.rm=T),
                                                  rin.use.m=mean(an.rin.use, na.rm=T),
                                                  rin.use.sd=sd(an.rin.use, na.rm=T),
                                                  rad.use.m=mean(an.rad.use, na.rm=T),
                                                  rad.use.sd=sd(an.rad.use, na.rm=T),
                                                  rdo.use.m=mean(an.rdo.use, na.rm=T),
                                                  rdo.use.sd=sd(an.rdo.use, na.rm=T),
                                                  rcf.use.m=mean(an.rcf.use, na.rm=T),
                                                  rcf.use.sd=sd(an.rcf.use, na.rm=T),
                                                  p.lsa.use.m=mean(p.lsa.use, na.rm=T),
                                                  p.lsa.use.sd=sd(p.lsa.use, na.rm=T),
                                                  p.sel.use.m=mean(p.sel.use, na.rm=T),
                                                  p.sel.use.sd=sd(p.sel.use, na.rm=T),
                                                  p.rsa.use.m=mean(p.rsa.use, na.rm=T),
                                                  p.rsa.use.sd=sd(p.rsa.use, na.rm=T),
                                                  p.rin.use.m=mean(p.rin.use, na.rm=T),
                                                  p.rin.use.sd=sd(p.rin.use, na.rm=T),
                                                  p.rad.use.m=mean(p.rad.use, na.rm=T),
                                                  p.rad.use.sd=sd(p.rad.use, na.rm=T),
                                                  p.rdo.use.m=mean(p.rdo.use, na.rm=T),
                                                  p.rdo.use.sd=sd(p.rdo.use, na.rm=T),
                                                  p.rcf.use.m=mean(p.rcf.use, na.rm=T),
                                                  p.rcf.use.sd=sd(p.rcf.use, na.rm=T),
                                                  commitment.first.use.m=mean(commitment.first.use, na.rm=T),
                                                  commitment.first.use.sd=sd(commitment.first.use, na.rm=T),
                                                  p.commitment.first.use.m=mean(p.commitment.first.use, na.rm=T),
                                                  p.commitment.first.use.sa=sd(p.commitment.first.use, na.rm=T))

write.csv(descriptives.groups, "descriptives.csv")

means.ttest=subset(means, select=c(event.group, age.first, page.first, nkidsp.first, reldur.first, testing, nwaves, an.lsa, an.sel, an.rsa.use, an.rin.use, an.rad.use, 
                                   an.rdo.use, an.rcf.use, p.lsa.use, p.sel.use, p.rsa.use, p.rin.use, p.rad.use, p.rdo.use, p.rcf.use,
                                   commitment.first.use, p.commitment.first.use))

library(tidyr)
means.long <- means.ttest %>%
  pivot_longer(-event.group, names_to = "variables", values_to = "value")


stat.test <- means.long %>%
  group_by(variables) %>%
  t_test(value ~ event.group) %>%
  #adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test

chisq.test(means$event.group, means$married)
chisq.test(means$event.group, means$livetogether)
chisq.test(means$event.group, means$homosex)
chisq.test(means$event.group, means$sep.max)
chisq.test(means$event.group, means$newrel)
chisq.test(means$event.group, means$male)

names(means)
means$group=ifelse(means$event.group=='cheater', 1, 0)
table(means$group, exclude = T)
apa.cor.table(means[,c("group", "an.lsa", "an.sel", "an.rsa.use", "an.rin.use", "an.rad.use", "an.rdo.use", "an.rcf.use", "commitment.first.use",
                       "p.lsa.use", "p.sel.use", "p.rsa.use", "p.rin.use", "p.rad.use", "p.rdo.use", "p.rcf.use",
                       "p.commitment.first.use")],filename="correlations.doc",table.number=1)

rcorr(as.matrix(means[, c("group", "an.lsa", "an.sel", "an.rsa.use", "an.rin.use", "an.rad.use", "an.rdo.use", "an.rcf.use", "commitment.first.use",
                          "p.lsa.use", "p.sel.use", "p.rsa.use", "p.rin.use", "p.rad.use", "p.rdo.use", "p.rcf.use",
                          "p.commitment.first.use")]))



sd(means$nwaves, na.rm=T)
summary(means$nwaves)

sd(means$testing, na.rm=T)
summary(means$testing)






