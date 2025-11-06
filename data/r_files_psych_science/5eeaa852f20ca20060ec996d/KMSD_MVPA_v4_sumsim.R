#Loading in required packages
library(lmerTest)
library(lme4)
library(nlme)
library(ggplot2)
library(lattice)
library(tidyverse)
library(ggeffects)

#Loading in the RSA data

data = read.table('~/Dropbox/KMSD_Sumsim.txt',T)
data = read.table('C:/Users/timot/Google Drive/Metacognition - Neuro/Delayed JOL Project/Scripts/KMSD_SumSim.txt',T)


resp_mean=aggregate(resp~id,FUN='mean',data=data,subset=phase=='judge')

#Calculating neural similarity values of each type of cue (item seen on the screen during judgments)
data$cue_sim=ifelse(data$pair2=='sf'|data$pair2=='so',data$sim_scene,NA)
data$cue_sim=ifelse(data$pair2=='fo'|data$pair2=='fs',data$sim_face,data$cue_sim)
data$cue_sim=ifelse(data$pair2=='of'|data$pair2=='os',data$sim_obj,data$cue_sim)

#Calculating neural similarity values of each type of unseen target item
data$targ_sim=ifelse(data$pair2=='sf'|data$pair2=='of',data$sim_face,NA)
data$targ_sim=ifelse(data$pair2=='os'|data$pair2=='fs',data$sim_scene,data$targ_sim)
data$targ_sim=ifelse(data$pair2=='so'|data$pair2=='fo',data$sim_obj,data$targ_sim)

data$non_sim=ifelse(data$pair2=='os'|data$pair2=='so',data$sim_face,NA)
data$non_sim=ifelse(data$pair2=='fo'|data$pair2=='of',data$sim_scene,data$non_sim)
data$non_sim=ifelse(data$pair2=='sf'|data$pair2=='fs',data$sim_obj,data$non_sim)

#Conducting a check on RSA analysis. Determines whether our analysis was actually able to detect when a scene, face, or object was actually present

#Faces

data$relsim_faces=data$sim_face/(data$sim_face+data$sim_obj+data$sim_scene)
data$relsim_obj=data$sim_obj/(data$sim_face+data$sim_obj+data$sim_scene)
data$relsim_scene=data$sim_scene/(data$sim_face+data$sim_obj+data$sim_scene)

class_face=(data$relsim_face>data$relsim_obj)&(data$relsim_face>data$relsim_scene)
class_scene=(data$relsim_scene>data$relsim_obj)&(data$relsim_scene>data$relsim_face)
class_obj=(data$relsim_obj>data$relsim_scene)&(data$relsim_obj>data$relsim_face)

class_face_screen=tapply(class_face[data$pair2=='fo'|data$pair2=='fs'],data$id[data$pair2=='fo'|data$pair2=='fs'],mean,na.rm=T)
class_face_offscreen=tapply(class_face[data$pair2=='so'|data$pair2=='os'],data$id[data$pair2=='so'|data$pair2=='os'],mean,na.rm=T)

mean(class_face_screen)
t.test(class_face_screen-.33)

class_obj_screen=tapply(class_obj[data$pair2=='of'|data$pair2=='os'],data$id[data$pair2=='of'|data$pair2=='os'],mean,na.rm=T)

mean(class_obj_screen)
t.test(class_obj_screen-.33)

class_scene_screen=tapply(class_scene[data$pair2=='sf'|data$pair2=='so'],data$id[data$pair2=='sf'|data$pair2=='so'],mean,na.rm=T)

mean(class_scene_screen)
t.test(class_scene_screen-.33)


###############
#Target
################
rlP2=relevel(data$phase2,ref='immediate')
mod_targ=lmer(targ_sim~resp*rlP2+(resp*rlP2|id),data=data,su=phase=='judge'&jol_correct==1&resp!=4,REML=F)



mod_targ_delayed=lmer(targ_sim~resp+(resp|id),data=data,su=jol_correct==1&phase2=='delayed'&resp!=4,REML=F)

mod_targ_immediate=lmer(targ_sim~resp+(resp|id),data=data,su=jol_correct==1&phase2=='immediate'&resp!=4,REML=F)


data$rel_sim=data$targ_sim/(data$targ_sim+data$non_sim+data$cue_sim)

#Looking at how relative similarity is related to immediate and delayed judgments interacting with confidence
mod_rel=lmer(rel_sim~rlP2*resp+(resp*rlP2|id),data=data,su=phase=='judge'&jol_correct==1&resp!=4,REML=F)

mod_targ_delay_interact=lmer(targ_sim~scale(delay)*resp+(scale(delay)*resp|id),data=data,su=phase=='judge'&jol_correct==1&resp!=4&phase2=='delayed',REML=F)
anova(mod_targ_delay_interact)

mod_cue=lmer(cue_sim~resp*phase2+(resp*phase2|id),data=data,su=phase=='judge'&jol_correct==1&resp!=4,REML=F)

mod_non=lmer(non_sim~resp*phase2+(resp*phase2|id),data=data,su=phase=='judge'&jol_correct==1&resp!=4,REML=F)

#confidence intervals
ci_mod_targ_delay_alone = confint.merMod(mod_targ_delay_interact, method = "boot", boot.type = "perc", nsim = 500)

ci_mod_targ = confint.merMod(mod_targ, method = "boot", boot.type = "perc", nsim = 500)
ci_mod_targ_delayed = confint.merMod(mod_targ_delayed, method = "boot", boot.type = "perc", nsim = 500)
ci_mod_targ_immediate = confint.merMod(mod_targ_immediate, method = "boot", boot.type = "perc", nsim = 500)

ci_mod_rel = confint.merMod(mod_rel, method = "boot", boot.type = "perc", nsim = 500)

ci_mod_cue = confint.merMod(mod_cue, method = "boot", boot.type = "perc", nsim = 500)

ci_mod_non = confint.merMod(mod_non, method = "boot", boot.type = "perc", nsim = 500)

##########
#Plots
#########

targ_plot = ggpredict(mod_targ,c('resp','rlP2'))%>%plot(colors = "bw")
library(ggplot2)
targ_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(), plot.title = element_blank()) + labs(x = "Confidence Level", y = "Neural Similarity to Target Category")

cue_plot = ggpredict(mod_cue,c('resp','phase2'))%>%plot(colors = "bw")
library(ggplot2)
cue_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(), plot.title = element_blank()) + labs(x = "Confidence Level", y = "Neural Similarity to Cue Category")

non_plot = ggpredict(mod_non,c('resp','phase2'))%>%plot(colors = "bw")
library(ggplot2)
non_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(), plot.title = element_blank()) + labs(x = "Confidence Level", y = "Neural Similarity to Non-Present Category")

xyplot(targ_sim~resp|id,data=data,subset=data$phase2=='delayed',xlab='JOL Rating',ylab="Neural Similarity to Target Category")

xyplot(targ_sim~resp|id,data=data,subset=data$phase2=='immediate',xlab='JOL Rating',ylab="Neural Similarity to Target Category")


xyplot(cue_sim~resp|id,data=data,subset=data$phase2=='delayed',xlab='JOL Rating',ylab="Neural Similarity to Cue Category")

xyplot(cue_sim~resp|id,data=data,subset=data$phase2=='immediate',xlab='JOL Rating',ylab="Neural Similarity to Cue Category")

xyplot(non_sim~resp|id,data=data,subset=data$phase2=='delayed',xlab='JOL Rating',ylab="Neural Similarity to Non-Present Category")

xyplot(non_sim~resp|id,data=data,subset=data$phase2=='immediate',xlab='JOL Rating',ylab="Neural Similarity to Non-Present Category")




