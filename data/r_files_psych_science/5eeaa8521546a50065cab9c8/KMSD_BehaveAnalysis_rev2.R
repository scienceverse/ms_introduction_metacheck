library(lme4)

data = read.table("C:/Users/timot/Google Drive/Metacognition - Neuro/Delayed JOL Project/Scripts/KMSD_All_Behave_final.txt",T)

#Do immediate and delayed differ in pr(correct)?

mcorrect=glmer(jol_correct~ttype+(1|id/ttype),data=data,family='binomial',subset=data$phase=='recog')

#confidence intervals
ci_mcorrect = confint.merMod(mcorrect, method = "boot", boot.type = "perc", nsim = 500)

#Mean delayed performance per subject
mean_perf_del=tapply(data$jol_correct[data$ttype=='delayed'&data$phase=='recog'],data$id[data$ttype=='delayed'&data$phase=='recog'],mean)

#Mean and standard deviation of delayed performance
mean(mean_perf_del)
sd(mean_perf_del)

#Mean immediate performance per subject
mean_perf_imm=tapply(data$jol_correct[data$ttype=='immediate'&data$phase=='recog'],data$id[data$ttype=='immediate'&data$phase=='recog'],mean)

#Mean and standard deviation of immediate performance
mean(mean_perf_imm)
sd(mean_perf_imm)

#Test of whether length of delay matters for performance within delayed
mod_recog_delayed_lag=glmer(jol_correct~delay+(delay|id),data=data,family='binomial',subset=phase2=='delayed'&phase=='judge')

#confidence intervals
ci_mod_recog_delayed_lag = confint.merMod(mod_recog_delayed_lag, method = "boot", boot.type = "perc", nsim = 500)


#Mean JOL magnitude per subject for delayed judgments
mean_jol_del=tapply(data$resp[data$phase2=='delayed'&data$phase=='judge'&data$jol_correct == 1& data$resp != 4 ],data$id[data$phase2=='delayed'&data$phase=='judge'&data$jol_correct == 1 & data$resp != 4],mean)

#Mean and standard deviation of delayed JOL magnitude
mean(mean_jol_del)
sd(mean_jol_del)

#Mean JOL magnitude per subject for immediate judgments
mean_jol_imm=tapply(data$resp[data$phase2=='immediate'&data$phase=='judge'&data$jol_correct == 1& data$resp != 4 ],data$id[data$phase2=='immediate'&data$phase=='judge'&data$jol_correct == 1 & data$resp != 4],mean)

#Mean and standard deviation of immediate JOL magnitude
mean(mean_jol_imm)
sd(mean_jol_imm)

#Test of whether immediate and delayed JOL magnitude differ
t.test(mean_jol_del - mean_jol_imm)

#Test of whether length of delay matters for JOL magnitude within delayed
mod_resp_delayed_lag=lmer(resp~delay+(delay|id),data=data,su=jol_correct==1&phase2=='delayed'&resp!=4)

ci_mod_resp_delayed_lag = confint.merMod(mod_resp_delayed_lag, method = "boot", boot.type = "perc", nsim = 500)

