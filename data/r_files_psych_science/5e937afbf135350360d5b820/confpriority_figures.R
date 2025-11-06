##############FIGURES CONFIDENCE AS A PRIORITY SIGNAL###################

####FIGURE 1####
###FIGURE 1B: EXPERIMENT CONFIDENCE, EXPECTED VS OBSERVED PERFORMANCE###
resxplot <- resolution(exp1_perf_sav_rav$experf)
fig1b <- ggplot(exp1_perf_sav_rav,aes(x=experf, y=prob,fill=task)) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth) + exp_perf_xlab + papri_obs_perf_ylab + palfillpapri_perf + limexperf + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp1_perf_sin_rav,aes(experf-3,prob,fill=as.factor(task)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth)  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + theme(legend.position = c(0.5, 0.95), legend.direction  = "horizontal", axis.title.y = element_text(size=12))

###FIGURE 1C: EXPERIMENT CONFIDENCE, AVERAGE CONFIDENCE 1ST VS 2ND CHOICE###
fig1c <- ggplot(exp1_conffise_sin_rav,aes(conffirst,confsecond)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Average conf. 1st choice")  + ylab("Average conf. 2nd choice") + scale_x_continuous(breaks=seq(50,100,len=6),limits=c(50,100)) + scale_y_continuous(breaks=seq(50,100,len=6),limits=c(50,100))

###FIGURE 1D: EXPERIMENT CONFIDENCE, DENSITY PROP HIGHER CONFIDENCE FOR 1ST CHOICE, COMPARISON ALL DATASET VS BOTH CHOICES CORRECT IN EE AND HH POOLED TOGETHER VS BOTH CHOICES ERROR IN EE AND HH POOLED TOGETHER###
exp1_confhigher_all_correct_sav_rav <- exp1_confhigher_all_correct_sav_rav %>% mutate(datatype="All data")
exp1_confhigher_samediffcollapsed_correct_sav_rav <- exp1_confhigher_samediffcollapsed_correct_sav_rav %>% mutate(datatype="Same difficulty, both choices correct")
exp1_confhigher_samediffcollapsed_wrong_sav_rav <- exp1_confhigher_samediffcollapsed_wrong_sav_rav %>% mutate(datatype="Same difficulty, both choices wrong")
exp1_confhigher_all_correct_sin_rav <- exp1_confhigher_all_correct_sin_rav %>% mutate(datatype="All data")
exp1_confhigher_samediffcollapsed_correct_sin_rav <- exp1_confhigher_samediffcollapsed_correct_sin_rav %>% mutate(datatype="Same difficulty, both choices correct")
exp1_confhigher_samediffcollapsed_wrong_sin_rav <- exp1_confhigher_samediffcollapsed_wrong_sin_rav %>% mutate(datatype="Same difficulty, both choices wrong")

exp1_confhigher_multiple_sav_rav <- rbind(exp1_confhigher_all_correct_sav_rav,exp1_confhigher_samediffcollapsed_correct_sav_rav,exp1_confhigher_samediffcollapsed_wrong_sav_rav)
exp1_confhigher_multiple_sin_rav <- rbind(exp1_confhigher_all_correct_sin_rav,exp1_confhigher_samediffcollapsed_correct_sin_rav,exp1_confhigher_samediffcollapsed_wrong_sin_rav)

exp1_confhigher_multiple_sav_rav <- exp1_confhigher_multiple_sav_rav %>% ungroup() %>% mutate(yplot = c(4,5,6),tag=c("all","EE & HH, correct","EE & HH,error"))
exp1_confhigher_multiple_sin_rav <- exp1_confhigher_multiple_sin_rav %>% ungroup() %>% mutate(yplot = ifelse(datatype=="All data",0.5,ifelse(datatype=="Same difficulty, both choices correct",1,1.5)))

resyplot <- resolution(exp1_confhigher_multiple_sin_rav$prob)
fig1d <- ggplot(exp1_confhigher_multiple_sin_rav,aes(prob,col=datatype)) + geom_density(adjust = smoothvalue*2, size=genplotwidth)  + geom_errorbarh(data=exp1_confhigher_multiple_sav_rav,aes(x=prob,y=yplot,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp1_confhigher_multiple_sav_rav,aes(x=prob,y=yplot,fill=datatype),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + xlab("Prop. higher conf. for 1st choice") + ylab("Density")  + theme(axis.title.x = element_text(size=sizefont)) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_y_continuous(breaks=seq(0,6,len=3),labels=seq(0,6,len=3),limits=c(0,6.5),expand=c(0,0)) + scale_color_manual(values = c("black","#55a868","#c44e52")) + scale_fill_manual(values = c("black","#55a868","#c44e52")) + geom_point(data=exp1_confhigher_multiple_sin_rav, aes(prob,yplot,col=datatype),shape=1) # c("#de2d26","#fc9272","#67000d")

###FIGURE 1 TOGETHER###
fig1 <- plot_grid(NULL, NULL, NULL, fig1b, fig1c, fig1d, labels = c("A", "","", "B", "C","D"), ncol = 3, align="v")
save_plot("figures/confpri_figure1.pdf", fig1, ncol = 3, nrow =2, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE 2####
###FIGURE 2A: EXPERIMENT CHOICE-ONLY, PRIORITY EFFECT ACROSS TRIAL TYPES###
resxplot <- 1
fig2a <- ggplot(exp2_prio_sav_rav,aes(x=catperf2,y=prob)) + geom_line(data=exp2_prio_sin_rav ,aes(catperf2,prob, group = sub),col="grey") + geom_point(data=exp2_prio_sin_rav,aes(catperf2,prob),col="grey",shape=21,fill="white") + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_point(col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_hline(yintercept=.5,linetype="dashed",size=axeswidth) + xlab("Difficulty color-letter") + ylab("Prop. color first") + annotate("text", x = 2.5, y = 1, label = "easy = E, hard = H",size=sizeannot,col="black") + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5),limits = c(0,1)) 

####FIGURE 2B: EXPERIMENT CHOICE-ONLY, DENSITY PRIORITY EFFECT###
fig2b <- ggplot(exp2_priodif_sin_rav,aes(difacc)) + geom_density(adjust = smoothvalue, size=genplotwidth) + geom_errorbarh(data=exp2_priodif_sav_rav,aes(x=difacc,y=6,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=exp2_priodif_sav_rav,aes(x=difacc,y=6),col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + annotate("text", x = 0, y = 6.5, label = "hard 1st     easy 1st",size=sizeannot,col="black", fontface="italic") + scale_y_continuous(breaks=seq(0,6,len=3),labels=seq(0,6,len=3),limits = c(0,7), expand=c(0,0)) #+ geom_point(data=exp2_priodif_sin_rav, aes(difacc,6),shape=1,col="grey")

####FIGURE 2C: EXPERIMENT DELAYS, DENSITY PRIORITY EFFECT ACROSS DELAYS###
exp3_priodifdelay_sin_rav <- exp3_priodifdelay_sin_rav %>% mutate(delay2 = ifelse(delay==0,"0s",ifelse(delay==2,"2s","4s")))
resyplot <- resolution(exp3_priodifdelay_sin_rav$difacc)
exp3_priodifdelay_sav_rav <- exp3_priodifdelay_sav_rav %>% ungroup() %>% mutate(delay2 = c("0s","2s","4s"),yplot = c(5.5,6.5,7.5))
exp3_priodifdelay_sin_rav <- exp3_priodifdelay_sin_rav %>% ungroup() %>% mutate(yplot = ifelse(delay==0,5.5,ifelse(delay==2,6.5,7.5)))
fig2c <- ggplot(exp3_priodifdelay_sin_rav,aes(col=as.factor(delay2))) + geom_density(aes(difacc),adjust = smoothvalue*2, size=genplotwidth) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + geom_errorbarh(data=exp3_priodifdelay_sav_rav,aes(x=difacc,y=yplot,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=exp3_priodifdelay_sav_rav,aes(x=difacc,y=yplot,fill=as.factor(delay2)),col="black",size=3,shape=21,stroke=1.5) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + scale_color_manual(name = "Delay (s)",values = c("#bb9966","#996633","#200800")) + scale_fill_manual(name = "Delay (s)",values = c("#bb9966","#996633","#200800")) + guides(fill=FALSE, color=FALSE) + geom_text(data=exp3_priodifdelay_sav_rav,aes(x=0.3,y=yplot,label=delay2), fontface="bold") + annotate("text", x = 0.28, y = 8.3, label = "delay",size=sizeannot,col="black", fontface="bold") + annotate("text", x = 0, y = 8.5, label = "hard 1st     easy 1st",size=sizeannot,col="black", fontface="italic") + scale_y_continuous(breaks=seq(0,9,len=4),labels=seq(0,9,len=4),limits=c(0,9), expand=c(0,0)) #+ geom_point(data=exp3_priodifdelay_sin_rav, aes(difacc,yplot),shape=1,col="grey")

###FIGURE 2D: EXPERIMENT CONFIDENCE, REVERSAL
exp123_priosameaccdif_sav_rav <- exp123_priosameaccdif_sav_rav %>% mutate(height=ifelse(acccategory=="correct",3.8,4.3), catlabel = ifelse(acccategory=="correct","both choices correct","both choices errors")) 
fig2d <- ggplot(exp123_priosameaccdif_sin_rav,aes(difacc, color=as.factor(acccategory))) + geom_density(adjust = smoothvalue*2, size=genplotwidth) + geom_errorbarh(data=exp123_priosameaccdif_sav_rav,aes(x=difacc,y=height,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=exp123_priosameaccdif_sav_rav,aes(x=difacc,y=height,fill=as.factor(acccategory)),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + annotate("text", x = 0, y = 4.8, label = "hard 1st     easy 1st",size=sizeannot,col="black", fontface ="italic") + scale_y_continuous(breaks=seq(0,3,len=2),labels=seq(0,3,len=2),expand=c(0,0),limits = c(0,5)) + scale_color_manual(values = c("#55a868","#c44e52")) + scale_fill_manual(values = c("#55a868","#c44e52")) + theme(legend.position = "NONE") + geom_text(data=exp123_priosameaccdif_sav_rav,aes(x=0.65,y=height,label=catlabel), fontface="bold", size = 2.8)

####FIG 2 TOGETHER###
fig2 <- plot_grid(fig2a, fig2b, fig2c, fig2d, labels = c("A","B", "C", "D"), ncol = 2, align="v")
save_plot("figures/confpri_figure2.pdf", fig2, ncol = 2, nrow =2, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE 3####

####FIGURE 3B: EXPECTED VS OBSERVED PERFORMANCE FAMILIARIZATION AND TEST###
resxplot <- resolution(exp4_perf_sav_rav$experf)
fig3b <- ggplot(exp4_perf_sav_rav %>% filter(learn_active==1),aes(x=experf, y=prob,fill=as.factor(current_feat))) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth) + exp_perf_xlab + papri_obs_perf_ylab + palfillpapri_perf + limexperf + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp4_perf_sin_rav %>% filter(learn_active==1),aes(experf-3,prob,fill=as.factor(current_feat)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth)  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + theme(legend.position = c(0.5, 0.95), legend.direction  = "horizontal",axis.title.y = element_text(size=12)) #using only familiarization. Non-significantly different than when using only test


####FIGURE 3C: DENSITY PRIORITY EFFECT, SPLIT BY SAME VS DIFFERENT TASK BLOCKS###
exp4_chdif_typetask_sin_rav <- exp4_chdif_typetask_sin_rav %>% mutate(tasklabel = ifelse(task1_type==0,"same","different"),yplot = ifelse(task1_type==0,5,6))
exp4_chdif_typetask_sav_rav <- exp4_chdif_typetask_sav_rav %>% ungroup() %>% mutate(tasklabel = c("same","different"),yplot = c(5,6))

resyplot <- resolution(exp4_chdif_typetask_sin_rav$prob)
fig3c <- ggplot(exp4_chdif_typetask_sin_rav,aes(prob,col=as.factor(tasklabel))) + geom_density(adjust = smoothvalue*2, size=genplotwidth) + geom_errorbarh(data=exp4_chdif_typetask_sav_rav,aes(x=prob,y=yplot,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp4_chdif_typetask_sav_rav,aes(x=prob,y=yplot,fill=tasklabel),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + xlab("Prop. easy set first") + ylab("Density") + annotate("text", x = 0.5, y = 8, label = "hard 1st          easy 1st",size=sizeannot,col="black", fontface="italic") + scale_color_manual(values = c("#F1006B","#386cb0")) + scale_fill_manual(values = c("#F1006B","#386cb0")) + scale_x_continuous(breaks=seq(0.25,0.75,len=3),labels=seq(0.25,0.75,len=3)) + scale_y_continuous(breaks=seq(0,9,len=4),labels=seq(0,9,len=4),limits=c(0,9), expand=c(0,0)) + theme(legend.position = "none") + geom_text(data=exp4_chdif_typetask_sav_rav,aes(x=0.3,y=yplot,label=tasklabel),fontface="bold") + annotate("text", x = 0.33, y = 6.8, label = "tasks within block",size=sizeannot,col="black",fontface="bold") #+ geom_point(aes(prob,yplot),shape=1,col="grey") #values = c("#F1006B","#00B778")

####FIG 3 TOGETHER###
fig3 <- plot_grid(NULL, NULL, fig3b, fig3c, labels = c("A", "", "B", "C"), ncol = 2, align="v")
save_plot("figures/confpri_figure3.pdf", fig3, ncol = 2, nrow =2, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE 4####
###FIGURE 4B: RTs ACCORDING TO DIFFICULTY###
fig4b <-ggplot(exp5_rt_sav,aes(x=difficulty, y=rt)) + geom_line(data=exp5_rt_sin,aes(difficulty,rt,group=subject),col="grey") + geom_point(data=exp5_rt_sin,aes(difficulty,rt),col="grey",position=position_dodge(width = 0.9),shape=21, fill="white") + geom_bar(col="black",fill="white", alpha=0, position = "dodge",stat="identity",size=genplotwidth) + scale_y_continuous(breaks=seq(0,30,len=4),labels=seq(0,30,len=4),limits=c(0,32),expand=c(0,0)) + xlab("Problem difficulty") + scale_x_discrete (labels=c("Easy", "Hard")) + ylab("Response time (s)")

###FIGURE 4C: DENSITY MATH QUESTIONNAIRE###
fig4c <- ggplot(exp5_prio_sin) + geom_density(aes(prob),adjust = smoothvalue, size=genplotwidth) + geom_errorbarh(data=exp5_prio_sav,aes(x=prob,y=2.75,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp5_prio_sav,aes(x=prob,y=2.75),col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + annotate("text", x = 0.5, y = 3.2, label = "hard 1st       easy 1st",size=sizeannot,col="black", fontface="italic") + scale_x_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5)) + xlab("Prop. easy first") + ylab("Density") + scale_y_continuous(breaks=seq(0,3,len=4),labels=seq(0,3,len=4),limits=c(0,3.5),expand=c(0,0)) #+ geom_point(data=exp5_prio_sin, aes(prob,2.75),shape=1,col="grey") 

####FIG 4 TOGETHER###
fig4 <- plot_grid(NULL,fig4b, fig4c, labels = c("A", "B","C"), ncol = 3, align="v")
save_plot("figures/confpri_figure4.pdf", fig4, ncol = 3, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S1: SUPPLEMENTARY FIGURE EXPECTED AND OBSERVED PERFORMANCE####

#FIGURE S1A: EXPERIMENT 2
resxplot <- resolution(exp2_perf_sav_rav$experf)
figs1a <- ggplot(exp2_perf_sav_rav,aes(x=experf, y=prob,fill=task)) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth) + exp_perf_xlab + papri_obs_perf_ylab + palfillpapri_perf + limexperf + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp2_perf_sin_rav,aes(experf-3,prob,fill=as.factor(task)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth)  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + theme(legend.position = c(0.5, 0.95), legend.direction  = "horizontal", axis.title.y = element_text(size=12))
figs1a <- plot_grid(figs1a,labels = c("A"))
save_plot("figures/confpri_figures1a.pdf", figs1a, ncol = 1, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

#FIGURE S1B: EXPERIMENT 3
dfdelaysplot <- data.frame(label=c("delay = 0s", "delay = 2s", "delay = 4s"), delay= c(0,2,4))
resxplot <- resolution(exp3_perfdelay_sav_rav$experf)
figs1b <- ggplot(exp3_perfdelay_sav_rav,aes(x=experf, y=prob)) + geom_bar(aes(fill=task),col="black",position = "dodge",stat="identity",size=genplotwidth) + exp_perf_xlab + papri_obs_perf_ylab + palfillpapri_perf + limexperf + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob,fill=task),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp3_perfdelay_sin_rav,aes(experf-3.5,prob,fill=as.factor(task)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth)  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + theme(legend.position = c(0.5, 0.95), legend.direction  = "horizontal", axis.title.y = element_text(size=12)) + theme(strip.background = element_blank(),strip.text.x = element_blank()) + facet_wrap(~delay) + geom_text(data=dfdelaysplot,mapping=aes(x=75,y=1.1,label=label),color="black", size=sizeannot) + theme(legend.position = "none")
figs1b <- plot_grid(figs1b,labels = c("B"))
save_plot("figures/confpri_figures1b.pdf", figs1b, ncol = 1, nrow =1, base_height = 3,base_width = 8)

#FIGURE S1C: EXPERIMENT 4
dfphasesplot <- data.frame(phase= c("familiarization","test"))
exp4_perf_sav_rav <- exp4_perf_sav_rav %>% mutate(phase = ifelse(learn_active==1,"familiarization","test"))
exp4_perf_sin_rav <- exp4_perf_sin_rav %>% mutate(phase = ifelse(learn_active==1,"familiarization","test"))

resxplot <- resolution(exp4_perf_sav_rav$experf)
figs1c <- ggplot(exp4_perf_sav_rav,aes(x=experf, y=prob)) + geom_bar(aes(fill=as.factor(current_feat)),col="black",position = "dodge",stat="identity",size=genplotwidth) + exp_perf_xlab + papri_obs_perf_ylab + palfillpapri_perf + limexperf + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob,fill=as.factor(current_feat)),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp4_perf_sin_rav,aes(experf-3,prob,fill=as.factor(current_feat)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth)  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + theme(legend.position = "none") + theme(strip.background = element_blank(),strip.text.x = element_blank()) + facet_wrap(~phase) + geom_text(data=dfphasesplot,mapping=aes(x=75,y=1.1,label=phase),color="black", size=sizeannot)
figs1c <- plot_grid(figs1c,labels = c("C"))
save_plot("figures/confpri_figures1c.pdf", figs1c, ncol = 1, nrow =1, base_height = 3,base_width = 5.5)

####FIGURE S2: SUPPLEMENTARY FIGURE, MORE CONFIDENCE RATINGS ANALYSES FOR EXPERIMENT 1####
####FIGURE S2A: EXPERIMENT CONFIDENCE, DENSITY PROP HIGHER CONFIDENCE FOR 1ST CHOICE, ACROSS ALL 4 TRIAL TYPES, ALL DATA###
exp1_confhigher_alltypes_alldata_sin_rav <- exp1_confhigher_alltypes_alldata_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_confhigher_alltypes_alldata_sav_rav <- exp1_confhigher_alltypes_alldata_sav_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_confhigher_alltypes_alldata_sav_rav <- exp1_confhigher_alltypes_alldata_sav_rav %>% ungroup() %>% mutate(yplot = c(4,4.5,5,5.5))
exp1_confhigher_alltypes_alldata_sin_rav <- exp1_confhigher_alltypes_alldata_sin_rav %>% mutate(yplot=ifelse(catperf2=="EE",5.5,ifelse(catperf2=="EH",5,ifelse(catperf2=="HE",4.5,4))))

resyplot <- resolution(exp1_confhigher_alltypes_alldata_sin_rav$prob)
figs2a <- ggplot(exp1_confhigher_alltypes_alldata_sin_rav,aes(prob,col=as.factor(catperf2))) + geom_density(adjust = smoothvalue*2, size=genplotwidth)  + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + xlab("Prop. higher conf. for 1st choice") + ylab("Density") + theme(axis.title.x = element_text(size=sizefont)) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none")  + geom_errorbarh(data=exp1_confhigher_alltypes_alldata_sav_rav,aes(x=prob,y=yplot,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp1_confhigher_alltypes_alldata_sav_rav,aes(x=prob,y=yplot,fill=as.factor(catperf2)),col="black",size=3,shape=21,stroke=1.5) + geom_text(data=exp1_confhigher_alltypes_alldata_sav_rav,aes(x=0.8,y=yplot,label=catperf2),fontface="bold") + scale_color_manual(name="trial type",values = c(hue_EE,hue_EH,hue_HE,hue_HH)) + scale_fill_manual(name="trial type",values = c(hue_EE,hue_EH,hue_HE,hue_HH)) + scale_y_continuous(breaks=seq(0,6,len=3),labels=seq(0,6,len=3),limits=c(0,6),expand=c(0,0)) #+ geom_point(aes(prob,yplot),shape=1,col="grey")

####FIGURE S2B: EXPERIMENT CONFIDENCE, DENSITY PROP HIGHER CONFIDENCE FOR 1ST CHOICE, ACROSS ALL 4 TRIAL TYPES, ONLY WHEN BOTH CHOICES WERE CORRECT###
exp1_confhigher_alltypes_correct_sin_rav <- exp1_confhigher_alltypes_correct_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_confhigher_alltypes_correct_sav_rav <- exp1_confhigher_alltypes_correct_sav_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_confhigher_alltypes_correct_sav_rav <- exp1_confhigher_alltypes_correct_sav_rav %>% ungroup() %>% mutate(yplot = c(4,4.5,5,5.5))
exp1_confhigher_alltypes_correct_sin_rav <- exp1_confhigher_alltypes_correct_sin_rav %>% mutate(yplot=ifelse(catperf2=="EE",5.5,ifelse(catperf2=="EH",5,ifelse(catperf2=="HE",4.5,4))))

resyplot <- resolution(exp1_confhigher_alltypes_correct_sin_rav$prob)
figs2b <- ggplot(exp1_confhigher_alltypes_correct_sin_rav,aes(prob,col=as.factor(catperf2))) + geom_density(adjust = smoothvalue*2, size=genplotwidth)  + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + xlab("Prop. higher conf. for 1st choice") + ylab("Density") + theme(axis.title.x = element_text(size=sizefont)) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_y_continuous(breaks=seq(0,6,len=3),labels=seq(0,6,len=3),limits=c(0,6),expand=c(0,0)) + geom_errorbarh(data=exp1_confhigher_alltypes_correct_sav_rav,aes(x=prob,y=yplot,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp1_confhigher_alltypes_correct_sav_rav,aes(x=prob,y=yplot,fill=as.factor(catperf2)),col="black",size=3,shape=21,stroke=1.5) + geom_text(data=exp1_confhigher_alltypes_correct_sav_rav,aes(x=0.8,y=yplot,label=catperf2),fontface="bold") + scale_color_manual(name="trial type",values = c(hue_EE,hue_EH,hue_HE,hue_HH)) + scale_fill_manual(name="trial type",values = c(hue_EE,hue_EH,hue_HE,hue_HH)) #+ geom_point(aes(prob,yplot),shape=1,col="grey")

####FIGURE S2C: EXPERIMENT CONFIDENCE, DENSITY PROP HIGHER CONFIDENCE FOR 1ST CHOICE, ACROSS ALL 4 TRIAL TYPES, ONLY WHEN BOTH CHOICES WERE ERRORS###
exp1_confhigher_alltypes_wrong_sin_rav <- exp1_confhigher_alltypes_wrong_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_confhigher_alltypes_wrong_sav_rav <- exp1_confhigher_alltypes_wrong_sav_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))
exp1_confhigher_alltypes_wrong_sav_rav <- exp1_confhigher_alltypes_wrong_sav_rav %>% ungroup() %>% mutate(yplot = c(4,4.5,5,5.5))
exp1_confhigher_alltypes_wrong_sin_rav <- exp1_confhigher_alltypes_wrong_sin_rav %>% mutate(yplot=ifelse(catperf2=="EE",5.5,ifelse(catperf2=="EH",5,ifelse(catperf2=="HE",4.5,4))))

resyplot <- resolution(exp1_confhigher_alltypes_wrong_sin_rav$prob)
figs2c <- ggplot(exp1_confhigher_alltypes_wrong_sin_rav,aes(prob,col=as.factor(catperf2))) + geom_density(adjust = smoothvalue*2, size=genplotwidth)  + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + xlab("Prop. higher conf. for 1st choice") + ylab("Density") + theme(axis.title.x = element_text(size=sizefont)) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_y_continuous(breaks=seq(0,6,len=3),labels=seq(0,6,len=3),limits=c(0,6),expand=c(0,0)) + geom_errorbarh(data=exp1_confhigher_alltypes_wrong_sav_rav,aes(x=prob,y=yplot,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp1_confhigher_alltypes_wrong_sav_rav,aes(x=prob,y=yplot,fill=as.factor(catperf2)),col="black",size=3,shape=21,stroke=1.5) + geom_text(data=exp1_confhigher_alltypes_wrong_sav_rav,aes(x=0.15,y=yplot,label=catperf2),fontface="bold") + scale_color_manual(name="trial type",values = c(hue_EE,hue_EH,hue_HE,hue_HH)) + scale_fill_manual(name="trial type",values = c(hue_EE,hue_EH,hue_HE,hue_HH)) #+ geom_point(aes(prob,yplot),shape=1,col="grey")

####FIGURE TOGETHER###
figs2 <- plot_grid(figs2a, figs2b, figs2c, labels = c("A","B","C"), ncol = 3, align="v")
save_plot("figures/confpri_figures2.pdf", figs2, ncol = 3, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S3: SUPPLEMENTARY FIGURE PROXIES FOR CONFIDENCE ANALYSES FOR EXPERIMENT 1####
###FIGURE S3A: EXPERIMENT CONFIDENCE, CONFIDENCE CORRECT VS ERRORS IN BOTH TASKS###
exp1_confcorer_sin_rav <- exp1_confcorer_sin_rav %>% mutate(task2 = ifelse(task==0,"Color task","Letter task"),
                                                        dif2 = ifelse(dif==0 & correct == 0, 0.9, ifelse(dif==0 & correct == 1, 1.1, ifelse(dif==1 & correct==0,-0.1,0.1))))
exp1_confcorer_sav_rav <- exp1_confcorer_sav_rav %>% mutate(task2 = ifelse(task==0,"Color task","Letter task"),
                                                        dif2 = ifelse(dif==0 & correct == 0, 0.9, ifelse(dif==0 & correct == 1, 1.1, ifelse(dif==1 & correct==0,-0.1,0.1))))

resxplot <- 1
figs3a <- ggplot(exp1_confcorer_sav_rav,aes(dif2,conf,col=as.factor(correct))) + geom_line(aes(group=correct),size=genplotwidth) + geom_errorbar(aes(ymin=low_ci_conf, ymax=up_ci_conf),width = 0*resxplot,size=genplotwidth,col="black")  + facet_wrap(~task2) + geom_point(aes(fill=as.factor(correct)),size=3,col="black",shape=21,stroke=1.5) + xlab("Task difficulty")  + ylab("Average confidence") + scale_color_manual(name="Correctness",labels=c("error","correct"), values = c("#c44e52","#55a868"), guide = guide_legend(reverse = TRUE)) + scale_fill_manual(name="Correctness",labels=c("error","correct"), values = c("#c44e52","#55a868"), guide = guide_legend(reverse = TRUE)) + scale_x_continuous(breaks=c(0,1),labels=c("Hard","Easy"),limits=c(-0.4,1.4)) + scale_y_continuous(breaks=seq(50,100,len=6),limits=c(50,100)) + theme(legend.position = c(0.7, 0.85),legend.title=element_blank(),strip.background = element_blank()) #+ geom_point(data=exp1_confcorer_sin_rav, aes(dif,conf,col=as.factor(correct)), shape=21)

####FIGURE S3B: EXPERIMENT CONFIDENCE, PRIORITY EFFECT ACROSS TRIAL TYPES###
resxplot <- 1
figs3b <- ggplot(exp1_prio_sav_rav,aes(x=catperf2,y=prob)) + geom_line(data=exp1_prio_sin_rav ,aes(catperf2,prob, group = sub),col="grey") + geom_point(data=exp1_prio_sin_rav,aes(catperf2,prob),col="grey",shape=21,fill="white") + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_point(col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_hline(yintercept=.5,linetype="dashed",size=axeswidth) + xlab("Difficulty color-letter") + ylab("Prop. color first") + annotate("text", x = 2.5, y = 0.08, label = "easy = E, hard = H",size=sizeannot,col="black") + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5),limits = c(0,1))

####FIGURE S3C: EXPERIMENT CONFIDENCE, DENSITY PRIORITY EFFECT###
figs3c <- ggplot(exp1_priodif_sin_rav,aes(difacc)) + geom_density(adjust = smoothvalue, size=genplotwidth) + geom_errorbarh(data=exp1_priodif_sav_rav,aes(x=difacc,y=3.3,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=exp1_priodif_sav_rav,aes(x=difacc,y=3.3),col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + annotate("text", x = 0.1, y = 3.8, label = "easy 1st",size=sizeannot,col="black", fontface ="italic") + scale_y_continuous(breaks=seq(0,3,len=2),labels=seq(0,3,len=2),expand=c(0,0),limits = c(0,4)) #+ geom_point(data=exp1_priodif_sin_rav, aes(difacc,5),shape=1,col="grey")

####FIGURE TOGETHER###
figs3 <- plot_grid(figs3a, figs3b, figs3c, labels = c("A","B", "C"), ncol = 3, align="v")
save_plot("figures/confpri_figures3.pdf", figs3, ncol = 3, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S4: SUPPLEMENTARY FIGURE RESPONSE TIMES FOR EXPERIMENT 1####
exp1_rtgathered <- exp1_raw %>% group_by(sub,run,block,trial) %>% select(sub,run,block,trial,rtchoice1,rtchoice2) %>% gather(order,rt,rtchoice1:rtchoice2)
exp1_rtgathered_sin_rav <- exp1_rtgathered %>% group_by(sub,order) %>% summarise(rt = mean(rt))

figs4 <- ggplot(exp1_rtgathered) + geom_density(aes(rt,col=order),adjust = smoothvalue, size=genplotwidth) + xlab("Response time (s)") + ylab("Density") + scale_x_continuous(breaks=seq(0,3,len=4),labels=seq(0,3,len=4),limits=c(0,3),expand=c(0,0)) + scale_y_continuous(breaks=seq(0,3,len=2),labels=seq(0,3,len=2),limits=c(0,3.5),expand=c(0,0)) + theme(legend.position = c(0.7, 0.5)) + scale_color_manual(name="choice",labels=c("first","second"), values = c("black","grey")) + geom_point(data=exp1_rtgathered_sin_rav %>% filter(order=="rtchoice1"),aes(x=rt,y=3.2,col=order),shape=1, color="black") + geom_point(data=exp1_rtgathered_sin_rav %>% filter(order=="rtchoice2"),aes(x=rt,y=3.4,col=order),shape=1, color="grey")
save_plot("figures/confpri_figures4.pdf", figs4, ncol = 1, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S5: SUPPLEMENTARY FIGURE PREREGISTERED REPLICATION FOR EXPERIMENT 1####
###FIGURE S5A: REPLICATION EXPERIMENT CONFIDENCE, EXPECTED VS OBSERVED PERFORMANCE###
resxplot <- resolution(exprep1_perf_sav_rav$experf)
figs5a <- ggplot(exprep1_perf_sav_rav,aes(x=experf, y=prob,fill=task)) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth) + exp_perf_xlab + papri_obs_perf_ylab + palfillpapri_perf + limexperf + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exprep1_perf_sin_rav,aes(experf-3,prob,fill=as.factor(task)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth)  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + theme(legend.position = c(0.5, 0.95), legend.direction  = "horizontal", axis.title.y = element_text(size=12))

###FIGURE S5B: REPLICATION EXPERIMENT CONFIDENCE, AVERAGE CONFIDENCE 1ST VS 2ND CHOICE###
figs5b <- ggplot(exprep1_conffise_sin_rav,aes(conffirst,confsecond)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Average conf. 1st choice")  + ylab("Average conf. 2nd choice") + scale_x_continuous(breaks=seq(50,100,len=6),limits=c(50,100)) + scale_y_continuous(breaks=seq(50,100,len=6),limits=c(50,100))

###FIGURE S5C: REPLICATION EXPERIMENT CONFIDENCE, DENSITY PROP HIGHER CONFIDENCE FOR 1ST CHOICE, COMPARISON ALL DATASET VS BOTH CHOICES CORRECT IN EE AND HH POOLED TOGETHER VS BOTH CHOICES ERROR IN EE AND HH POOLED TOGETHER###
exprep1_confhigher_all_correct_sav_rav <- exprep1_confhigher_all_correct_sav_rav %>% mutate(datatype="All data")
exprep1_confhigher_samediffcollapsed_correct_sav_rav <- exprep1_confhigher_samediffcollapsed_correct_sav_rav %>% mutate(datatype="Same difficulty, both choices correct")
exprep1_confhigher_samediffcollapsed_wrong_sav_rav <- exprep1_confhigher_samediffcollapsed_wrong_sav_rav %>% mutate(datatype="Same difficulty, both choices wrong")
exprep1_confhigher_all_correct_sin_rav <- exprep1_confhigher_all_correct_sin_rav %>% mutate(datatype="All data")
exprep1_confhigher_samediffcollapsed_correct_sin_rav <- exprep1_confhigher_samediffcollapsed_correct_sin_rav %>% mutate(datatype="Same difficulty, both choices correct")
exprep1_confhigher_samediffcollapsed_wrong_sin_rav <- exprep1_confhigher_samediffcollapsed_wrong_sin_rav %>% mutate(datatype="Same difficulty, both choices wrong")

exprep1_confhigher_multiple_sav_rav <- rbind(exprep1_confhigher_all_correct_sav_rav,exprep1_confhigher_samediffcollapsed_correct_sav_rav,exprep1_confhigher_samediffcollapsed_wrong_sav_rav)
exprep1_confhigher_multiple_sin_rav <- rbind(exprep1_confhigher_all_correct_sin_rav,exprep1_confhigher_samediffcollapsed_correct_sin_rav,exprep1_confhigher_samediffcollapsed_wrong_sin_rav)

exprep1_confhigher_multiple_sav_rav <- exprep1_confhigher_multiple_sav_rav %>% ungroup() %>% mutate(yplot = c(4,5,6),tag=c("all","EE & HH, correct","EE & HH,error"))
exprep1_confhigher_multiple_sin_rav <- exprep1_confhigher_multiple_sin_rav %>% ungroup() %>% mutate(yplot = ifelse(datatype=="All data",0.5,ifelse(datatype=="Same difficulty, both choices correct",1,1.5)))

resyplot <- resolution(exprep1_confhigher_multiple_sin_rav$prob)
figs5c <- ggplot(exprep1_confhigher_multiple_sin_rav,aes(prob,col=datatype)) + geom_density(adjust = smoothvalue*2, size=genplotwidth)  + geom_errorbarh(data=exprep1_confhigher_multiple_sav_rav,aes(x=prob,y=yplot,xmin=low_ci_prob, xmax=up_ci_prob),height = 0,col="black", size=genplotwidth) + geom_point(data=exprep1_confhigher_multiple_sav_rav,aes(x=prob,y=yplot,fill=datatype),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0.5,linetype="dashed",size=axeswidth) + xlab("Prop. higher conf. for 1st choice") + ylab("Density")  + theme(axis.title.x = element_text(size=sizefont)) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_y_continuous(breaks=seq(0,6,len=3),labels=seq(0,6,len=3),limits=c(0,6.5),expand=c(0,0)) + scale_color_manual(values = c("black","#55a868","#c44e52")) + scale_fill_manual(values = c("black","#55a868","#c44e52")) + geom_point(data=exprep1_confhigher_multiple_sin_rav, aes(prob,yplot,col=datatype),shape=1) # c("#de2d26","#fc9272","#67000d")

###FIGURE S5D: EXPERIMENT CONFIDENCE, CONFIDENCE CORRECT VS ERRORS IN BOTH TASKS###
exprep1_confcorer_sin_rav <- exprep1_confcorer_sin_rav %>% mutate(task2 = ifelse(task==0,"Color task","Letter task"),
                                                                  dif2 = ifelse(dif==0 & correct == 0, 0.9, ifelse(dif==0 & correct == 1, 1.1, ifelse(dif==1 & correct==0,-0.1,0.1))))
exprep1_confcorer_sav_rav <- exprep1_confcorer_sav_rav %>% mutate(task2 = ifelse(task==0,"Color task","Letter task"),
                                                                  dif2 = ifelse(dif==0 & correct == 0, 0.9, ifelse(dif==0 & correct == 1, 1.1, ifelse(dif==1 & correct==0,-0.1,0.1))))

resxplot <- 1
figs5d <- ggplot(exprep1_confcorer_sav_rav,aes(dif2,conf,col=as.factor(correct))) + geom_line(aes(group=correct),size=genplotwidth) + geom_errorbar(aes(ymin=low_ci_conf, ymax=up_ci_conf),width = 0*resxplot,size=genplotwidth,col="black")  + facet_wrap(~task2) + geom_point(aes(fill=as.factor(correct)),size=3,col="black",shape=21,stroke=1.5) + xlab("Task difficulty")  + ylab("Average confidence") + scale_color_manual(name="Correctness",labels=c("error","correct"), values = c("#c44e52","#55a868"), guide = guide_legend(reverse = TRUE)) + scale_fill_manual(name="Correctness",labels=c("error","correct"), values = c("#c44e52","#55a868"), guide = guide_legend(reverse = TRUE)) + scale_x_continuous(breaks=c(0,1),labels=c("Hard","Easy"),limits=c(-0.4,1.4)) + scale_y_continuous(breaks=seq(50,100,len=6),limits=c(50,100)) + theme(legend.position = c(0.7, 0.85),legend.title=element_blank(),strip.background = element_blank()) #+ geom_point(data=exprep1_confcorer_sin_rav, aes(dif,conf,col=as.factor(correct)), shape=21)

####FIGURE S5E: EXPERIMENT CONFIDENCE, DENSITY PRIORITY EFFECT###
figs5e <- ggplot(exprep1_priodif_sin_rav,aes(difacc)) + geom_density(adjust = smoothvalue, size=genplotwidth) + geom_errorbarh(data=exprep1_priodif_sav_rav,aes(x=difacc,y=4.3,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=exprep1_priodif_sav_rav,aes(x=difacc,y=4.3),col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + annotate("text", x = 0.1, y = 4.8, label = "easy 1st",size=sizeannot,col="black", fontface ="italic") + scale_y_continuous(breaks=seq(0,3,len=2),labels=seq(0,3,len=2),expand=c(0,0),limits = c(0,5)) #+ geom_point(data=exprep1_priodif_sin_rav, aes(difacc,5),shape=1,col="grey")

###FIGURE S5F: REPLICATION EXPERIMENT CONFIDENCE, REVERSAL
exprep1_priosameaccdif_sav_rav <- exprep1_priosameaccdif_sav_rav %>% mutate(height=ifelse(acccategory=="correct",3.8,4.3), catlabel = ifelse(acccategory=="correct","both choices correct","both choices errors")) 
figs5f <- ggplot(exprep1_priosameaccdif_sin_rav,aes(difacc, color=as.factor(acccategory))) + geom_density(adjust = smoothvalue*2, size=genplotwidth) + geom_errorbarh(data=exprep1_priosameaccdif_sav_rav,aes(x=difacc,y=height,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=exprep1_priosameaccdif_sav_rav,aes(x=difacc,y=height,fill=as.factor(acccategory)),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + annotate("text", x = 0, y = 4.8, label = "hard 1st     easy 1st",size=sizeannot,col="black", fontface ="italic") + scale_y_continuous(breaks=seq(0,3,len=2),labels=seq(0,3,len=2),expand=c(0,0),limits = c(0,5)) + scale_color_manual(values = c("#55a868","#c44e52")) + scale_fill_manual(values = c("#55a868","#c44e52")) + theme(legend.position = "NONE") + geom_text(data=exprep1_priosameaccdif_sav_rav,aes(x=0.65,y=height,label=catlabel), fontface="bold", size = 2.8)

###FIGURE TOGETHER###
figs5 <- plot_grid(figs5a, figs5b, figs5c, figs5d, figs5e, figs5f, labels = c("A", "B", "C","D", "E", "F"), ncol = 3, align="v")
save_plot("figures/confpri_figures5.pdf", figs5, ncol = 3, nrow =2, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S6:  DIFFERENCE IN OBSERVED ACCURACY BETWEEN FIRST AND SECOND CHOICE####
####FIGURE s6a: EXPERIMENT CONFIDENCE, DENSITY ACCURACY 1ST-2ND CHOICE, FOR ALL TRIAL TYPES POOLED VS FOR EE AND HH POOLED###
exp1_acordtogetherall_sin_rav <- exp1_acordtogetherall_sin_rav %>% mutate(datatype="all data")
exp1_acordsplitcollapsed_sin_rav <- exp1_acordsplitcollapsed_sin_rav %>% mutate(datatype="EE & HH")
exp1_acordforplot_sin_rav <- rbind(exp1_acordtogetherall_sin_rav,exp1_acordsplitcollapsed_sin_rav)

exp1_acordforplot_sav_rav <- exp1_acordforplot_sin_rav %>% group_by(datatype) %>%
  summarise(sd_difprob = sd(difprob, na.rm = TRUE),
            difprob = mean(difprob, na.rm = TRUE),
            n_difprob = n()) %>%
  mutate(se_difprob = sd_difprob / sqrt(n_difprob),
         low_ci_difprob = difprob - qt(1 - (0.05 / 2), n_difprob - 1) * se_difprob,
         up_ci_difprob = difprob + qt(1 - (0.05 / 2), n_difprob - 1) * se_difprob)

exp1_acordforplot_sav_rav <- exp1_acordforplot_sav_rav %>% ungroup() %>% mutate(yplot = c(10.5,11.5))
exp1_acordforplot_sin_rav <- exp1_acordforplot_sin_rav %>% ungroup() %>% mutate(yplot = ifelse(datatype=="all data",10.5,11.5))

figs6a <- ggplot(exp1_acordforplot_sin_rav,aes(difprob,col=as.factor(datatype))) + geom_density(adjust = smoothvalue*2, size=genplotwidth) + geom_errorbarh(data=exp1_acordforplot_sav_rav,aes(x=difprob,y=yplot,xmin=low_ci_difprob, xmax=up_ci_difprob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp1_acordforplot_sav_rav,aes(x=difprob,y=yplot,fill=datatype),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Accuracy 1st - 2nd choice") + ylab("Density") + scale_color_manual(values = c("black","grey")) + scale_fill_manual(values = c("black","grey")) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_x_continuous(breaks=c(0,0.1),labels=c(0,0.1)) + scale_y_continuous(breaks=seq(0,12,len=5),labels=seq(0,12,len=5),limits=c(0,12),expand=c(0,0)) + geom_text(data=exp1_acordforplot_sav_rav,aes(x=0.15,y=yplot,label=datatype),fontface="bold") #+ geom_point(aes(difprob,yplot),shape=1,col="grey") 

####FIGURE s6b: EXPERIMENT CHOICE-ONLY, DENSITY ACCURACY 1ST-2ND CHOICE, FOR ALL TRIAL TYPES POOLED VS FOR EE AND HH POOLED###
exp2_acordtogetherall_sin_rav <- exp2_acordtogetherall_sin_rav %>% mutate(datatype="all data")
exp2_acordsplitcollapsed_sin_rav <- exp2_acordsplitcollapsed_sin_rav %>% mutate(datatype="EE & HH")
exp2_acordforplot_sin_rav <- rbind(exp2_acordtogetherall_sin_rav,exp2_acordsplitcollapsed_sin_rav)

exp2_acordforplot_sav_rav <- exp2_acordforplot_sin_rav %>% group_by(datatype) %>%
  summarise(sd_difprob = sd(difprob, na.rm = TRUE),
            difprob = mean(difprob, na.rm = TRUE),
            n_difprob = n()) %>%
  mutate(se_difprob = sd_difprob / sqrt(n_difprob),
         low_ci_difprob = difprob - qt(1 - (0.05 / 2), n_difprob - 1) * se_difprob,
         up_ci_difprob = difprob + qt(1 - (0.05 / 2), n_difprob - 1) * se_difprob)

exp2_acordforplot_sav_rav <- exp2_acordforplot_sav_rav %>% ungroup() %>% mutate(yplot = c(10.5,11.5))
exp2_acordforplot_sin_rav <- exp2_acordforplot_sin_rav %>% ungroup() %>% mutate(yplot = ifelse(datatype=="all data",10.5,11.5))

figs6b <- ggplot(exp2_acordforplot_sin_rav,aes(difprob,col=as.factor(datatype))) + geom_density(adjust = smoothvalue*2, size=genplotwidth)  + geom_errorbarh(data=exp2_acordforplot_sav_rav,aes(x=difprob,y=yplot,xmin=low_ci_difprob, xmax=up_ci_difprob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp2_acordforplot_sav_rav,aes(x=difprob,y=yplot,fill=datatype),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Accuracy 1st - 2nd choice") + ylab("Density") + scale_color_manual(values = c("black","grey")) + scale_fill_manual(values = c("black","grey")) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_y_continuous(breaks=seq(0,12,len=5),labels=seq(0,12,len=5),limits=c(0,12),expand=c(0,0)) + geom_text(data=exp2_acordforplot_sav_rav,aes(x=0.12,y=yplot,label=datatype),fontface="bold") #+ annotate("text", x = 0.12, y = 10, label = "dataset",size=sizeannot,col="black",fontface="bold") #+ geom_point(aes(difprob,yplot),shape=1,col="grey")  #values = c("#de2d26","#fc9272")

####FIGURE s6c: EXPERIMENT DELAYS, DENSITY ACCURACY 1ST-2ND CHOICE, FOR ALL TRIAL TYPES POOLED VS FOR EE AND HH POOLED###
exp3_acordtogetherall_sin_rav <- exp3_acordtogetherall_sin_rav %>% mutate(datatype="all data")
exp3_acordsplitcollapsed_sin_rav <- exp3_acordsplitcollapsed_sin_rav %>% mutate(datatype="EE & HH")
exp3_acordforplot_sin_rav <- rbind(exp3_acordtogetherall_sin_rav,exp3_acordsplitcollapsed_sin_rav)

exp3_acordforplot_sav_rav <- exp3_acordforplot_sin_rav %>% group_by(datatype) %>%
  summarise(sd_difprob = sd(difprob, na.rm = TRUE),
            difprob = mean(difprob, na.rm = TRUE),
            n_difprob = n()) %>%
  mutate(se_difprob = sd_difprob / sqrt(n_difprob),
         low_ci_difprob = difprob - qt(1 - (0.05 / 2), n_difprob - 1) * se_difprob,
         up_ci_difprob = difprob + qt(1 - (0.05 / 2), n_difprob - 1) * se_difprob)

exp3_acordforplot_sav_rav <- exp3_acordforplot_sav_rav %>% ungroup() %>% mutate(yplot = c(10.5,11.5))
exp3_acordforplot_sin_rav <- exp3_acordforplot_sin_rav %>% ungroup() %>% mutate(yplot = ifelse(datatype=="all data",10.5,11.5))

figs6c <- ggplot(exp3_acordforplot_sin_rav,aes(difprob,col=as.factor(datatype))) + geom_density(adjust = smoothvalue*2, size=genplotwidth) + geom_errorbarh(data=exp3_acordforplot_sav_rav,aes(x=difprob,y=yplot,xmin=low_ci_difprob, xmax=up_ci_difprob),height = 0,col="black", size=genplotwidth) + geom_point(data=exp3_acordforplot_sav_rav,aes(x=difprob,y=yplot,fill=datatype),col="black",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Accuracy 1st - 2nd choice") + ylab("Density") + scale_color_manual(values = c("black","grey")) + scale_fill_manual(values = c("black","grey")) + theme(axis.title.x = element_text(size=sizefont),legend.position = "none") + scale_x_continuous(breaks=c(0,0.1),labels=c(0,0.1)) + scale_y_continuous(breaks=seq(0,12,len=5),labels=seq(0,12,len=5),limits=c(0,12),expand=c(0,0)) + geom_text(data=exp3_acordforplot_sav_rav,aes(x=0.1,y=yplot,label=datatype),fontface="bold") #+ geom_point(aes(difprob,yplot),shape=1,col="grey") 


####FIGURE TOGETHER###
figs6 <- plot_grid(figs6a, figs6b, figs6c, labels = c("A","B", "C"), ncol = 3, align="v")
save_plot("figures/confpri_figures6.pdf", figsXXX, ncol = 3, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S7: SUPPLEMENTARY FIGURE RELATIONSHIP OVERALL BIAS AND DIFFERENCE BETWEEN UNEQUAL DIFFICULTY CONDITIONS####
figs7 <- ggplot(exp123_priodif_sin_rav,aes(distavecomb,difacc)) + geom_point(shape=21) + stat_smooth(method = "lm",color="black",se=FALSE) + xlab("Color/letter bias magnitude") + ylab("Prop. color first (EH  -  HE)") + annotate("text", x = 0.38, y = 0.25, label = "r=-0.370, p=0.001",size=sizeannot,col="black", fontface="italic")

save_plot("figures/confpri_figures7.pdf", figs7, ncol = 1, nrow =1, base_height = 3,base_aspect_ratio = 1.1)

####FIGURE S8: SUPPLEMENTARY FIGURE POSITION BIAS####
###FIGURE 8A: EXPERIMENTS 1, 2 AND 3###
resxplot <- resolution(exp123_loc_sav_rav$locresp1)
figs8a <- ggplot(exp123_loc_sav_rav,aes(locresp1,prop)) + geom_col(col="black",fill="white",position = "dodge",size=genplotwidth) + geom_hline(yintercept=.25,linetype="dashed",size=axeswidth) + xlab("Position of chosen square") + ylab("Prop. trials chosen") + scale_x_continuous(breaks=1:4,labels=c("top-left","top-right","bottom-left","bottom-right"))  + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_errorbar(aes(ymin=low_ci_prop, ymax=up_ci_prop),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp123_loc_sin_rav,aes(locresp1-0.25,prop),col="black",width=0.12,shape=1) + scale_y_continuous(breaks=seq(0,0.6,len=7),labels=seq(0,0.6,len=7), limits=c(0,0.6),expand=c(0,0)) + theme(legend.position = c(0.5, 0.935),strip.background = element_blank(), legend.direction  = "horizontal", axis.title.y = element_text(size=12)) + facet_wrap(~exptag)

figs8a <- plot_grid(figs8a, labels = c("A"))
save_plot("figures/confpri_figures8a.pdf", figs8a, ncol = 1, nrow = 1, base_height = 4,base_width = 8)

###FIGURE 8B: EXPERIMENT 4###
figs8b <- ggplot(exp4_chloc_pooled_sav_rav,aes(1,prob)) + geom_col(col="black",fill="white",position = "dodge",size=genplotwidth) + geom_hline(yintercept=.5,linetype="dashed",size=axeswidth) + xlab("Position of chosen square") + ylab("Prop. blocks left box chosen")  + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp4_chloc_pooled_sin_rav,aes(0.8,prob),width=0.1,shape=1) + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5), limits=c(0,1.025),expand=c(0,0)) + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
figs8b <- plot_grid(figs8b, labels = c("B"))
save_plot("figures/confpri_figures8b.pdf", figs8b, ncol = 1, nrow = 1, base_height = 4,base_width = 1.5)

###FIGURE 8C: EXPERIMENT 5###
figs8c <- ggplot(exp5_loc_sav,aes(1,prob)) + geom_col(col="black",fill="white",position = "dodge",size=genplotwidth) + geom_hline(yintercept=.5,linetype="dashed",size=axeswidth) + xlab("Position of chosen square") + ylab("Prop. problems left box chosen")  + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp5_loc_sin,aes(0.8,prob),width=0.1,shape=1) + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,1,len=5), limits=c(0,1.025),expand=c(0,0)) + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
figs8c <- plot_grid(figs8c, labels = c("C"))
save_plot("figures/confpri_figures8c.pdf", figs8c, ncol = 1, nrow =1, base_height = 4,base_width = 1.5)

####FIGURE S9: SUPPLEMENTARY FIGURE PERFORMANCE WHEN CHOOSING EASY FIRST VS HARD FIRST####
###FIGURE S9A: EXPERIMENTS 1 TO 3###
resxplot <- resolution(exp123_difchoice_sav_rav$task_dif)
figs9a <- ggplot(exp123_difchoice_sav_rav,aes(x=task_dif,y=prob,fill=as.factor(cheasy_first))) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth) + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth) + obs_perf_ylab + xlab("Task difficulty") + scale_x_continuous(breaks=c(0,1),labels=c("Hard","Easy"))  + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp123_difchoice_sin_rav,aes(task_dif-0.1,prob,fill=as.factor(cheasy_first)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=0.2),shape=1) + theme(legend.position = c(0.5, 0.935),strip.background = element_blank(), legend.direction  = "horizontal", axis.title.y = element_text(size=12)) + scale_fill_manual(name = expression(paste("Task first responded to")),values = c("grey","white"),labels = c("Hard", "Easy")) + papri_obs_perf_ylab  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0)) + facet_wrap(~exptag)
figs9a <- plot_grid(figs9a, labels = c("A"))
save_plot("figures/confpri_figures9a.pdf", figs9a, ncol = 1, nrow =1, base_height = 3,base_width = 8)

###FIGURE S9B: EXPERIMENT PROSPECTIVE CONFIDENCE###
resxplot <- resolution(exp4_difchoice_sav_rav$current_dif)
figs9b <- ggplot(exp4_difchoice_sav_rav,aes(x=current_dif,y=prob,fill=as.factor(chosen1_dif))) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth) + obs_perf_ylab + xlab("Task difficulty") + scale_x_continuous(breaks=c(60,90),labels=c("Hard","Easy")) + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=exp4_difchoice_sin_rav,aes(current_dif-3,prob,fill=as.factor(chosen1_dif)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=7),shape=1) + theme(legend.position = "none",strip.background = element_blank(), legend.direction  = "horizontal", axis.title.y = element_text(size=12)) + scale_fill_manual(name = expression(paste("Task first responded to")),values = c("grey","white"),labels = c("Hard", "Easy")) + papri_obs_perf_ylab  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0))
figs9b <- plot_grid(figs9b, labels = c("B"))
save_plot("figures/confpri_figures9b.pdf", figs9b, ncol = 1, nrow =1, base_height = 3,base_width = 3)

###FIGURE S9C: EXPERIMENT 5 (MENTAL CALCULATION)###
resxplot <- resolution(exp5_difrt_sav$dif2)
figs9c <- ggplot(exp5_difrt_sav,aes(x=dif2,y=rt,fill=as.factor(easyquestfirst))) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth) + xlab("Problem difficulty") + ylab("Response time (s)") + scale_x_continuous(breaks=c(0,1),labels=c("Hard","Easy"))  + geom_jitter(data=exp5_difrt_sin,aes(dif2,rt,fill=as.factor(easyquestfirst)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=0.4),shape=1) + theme(legend.position = "none",strip.background = element_blank(), legend.direction  = "horizontal", axis.title.y = element_text(size=12)) + scale_fill_manual(name = expression(paste("Problem first responded to")),values = c("grey","white"),labels = c("Hard", "Easy"))
figs9c <- plot_grid(figs9c, labels = c("C"))
save_plot("figures/confpri_figures9c.pdf", figs9c, ncol = 1, nrow =1, base_height = 3,base_width = 3)

####FIGURE S10: SUPPLEMENTARY FIGURE EXPERIMENT IMPOSED VS FREE ORDER####
####FIGURE S10A: DIFFERENCE IN PERFORMANCE BETWEEN SIMULTANEOUS AND SEQUENTIAL RESPONSE TYPE###
resxplot <- resolution(expsupp_difchoice_sav_rav$task_dif)
figs10a <- ggplot(expsupp_difchoice_sav_rav,aes(x=task_dif,y=prob,fill=as.factor(cheasy_first))) + geom_bar(col="black",position = "dodge",stat="identity",size=genplotwidth)  + geom_hline(yintercept=c(.6,.9),linetype="dashed",size=axeswidth) + obs_perf_ylab + xlab("Task difficulty") + scale_x_continuous(breaks=c(0,1),labels=c("Hard","Easy")) + geom_errorbar(aes(ymin=low_ci_prob, ymax=up_ci_prob),width = resxplot*0,position = position_dodge(width = 0.9*resxplot),col="black",size=genplotwidth) + geom_jitter(data=expsupp_difchoice_sin_rav,aes(task_dif-0.1,prob,fill=as.factor(cheasy_first)),col="black",position=position_jitterdodge(dodge.width = 0.9*resxplot,jitter.width=0.25),shape=1) + facet_wrap(~respordbin2) + theme(legend.position = c(0.5, 0.93),strip.background = element_blank(), legend.direction  = "horizontal", axis.title.y = element_text(size=12)) + scale_fill_manual(name = expression(paste("Task first responded to")),values = c("grey","white"),labels = c("Hard", "Easy")) + papri_obs_perf_ylab  + scale_y_continuous(breaks=seq(0,1,len=5),labels=seq(0,100,len=5),limits=c(0,1.2), expand=c(0,0))

###FIGURE S10B: DENSITY PRIORITY EFFECT###
figs10b <- ggplot(expsupp_priodif_sin_rav,aes(difacc)) + geom_density(adjust = smoothvalue*2, size=genplotwidth) + geom_errorbarh(data=expsupp_priodif_sav_rav,aes(x=difacc,y=8,xmin=low_ci_difacc, xmax=up_ci_difacc),height = 0,col="black", size=genplotwidth) + geom_point(data=expsupp_priodif_sav_rav,aes(x=difacc,y=8),col="black",fill="white",size=3,shape=21,stroke=1.5) + geom_vline(xintercept=0,linetype="dashed",size=axeswidth) + xlab("Prop. color first (EH  -  HE)") + ylab("Density") + annotate("text", x = 0.1, y = 8.75, label = "easy 1st",size=sizeannot,col="black", fontface ="italic") + scale_y_continuous(breaks=seq(0,9,len=4),labels=seq(0,9,len=4),limits = c(0,9), expand=c(0,0)) #+ geom_point(data=expsupp_priodif_sin_rav, aes(difacc,8),shape=1,col="grey")

####FIGURE TOGETHER###
figs10 <- plot_grid(figs10a, figs10b, labels = c("A","B"), ncol = 2, align="v",rel_widths = c(2/3, 1/3))
save_plot("figures/confpri_figures10.pdf", figs10, ncol = 2, nrow =1, base_height = 3.5,base_width = 5)

####FIGURE S11: RELIABILITY 1st VS 2nd HALF EXPERIMENT####
###FIGURE S11A: Exp 1, proportion higher confidence for 1st choice
figs11a <- ggplot(exp1_confhigher_all_correct_splitrun_sin_rav,aes(firsthalf,secondhalf)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Prop. higher conf. 1st choice (1st half)") + ylab("Prop. higher conf. 1st choice (2nd half)") + scale_x_continuous(breaks=seq(0.25,0.75,len=3),limits=c(0.15,0.85)) + scale_y_continuous(breaks=seq(0.25,0.75,len=3),limits=c(0.15,0.85)) + annotate("text", x = 0.325, y = 0.85, label = "r=0.672, p<0.001",size=sizeannot,col="black", fontface="italic") + theme(axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))

###FIGURE S11B: Exp 1, difference of proportions (EH - HE)
figs11b <- ggplot(exp1_dfdifdif_splitrun,aes(firsthalf,secondhalf)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Prop. color 1st (EH - HE) (1st half)") + ylab("Prop. color 1st (EH - HE) (2nd half)") + annotate("text", x = 0.035, y = 0.5, label = "r=0.628, p<0.001",size=sizeannot,col="black", fontface="italic") + theme(axis.title.x = element_text(size=9), axis.title.y = element_text(size=9)) + scale_x_continuous(breaks=c(0,0.25,0.5),labels=c(0,0.25,0.5),limits=c(-0.12,0.5)) + scale_y_continuous(breaks=c(0,0.25,0.5),labels=c(0,0.25,0.5),limits=c(-0.12,0.5))

###FIGURE S11C: Exp 2, difference of proportions (EH - HE)
figs11c <- ggplot(exp2_dfdifdif_splitrun,aes(firsthalf,secondhalf)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Prop. color 1st (EH - HE) (1st half)") + ylab("Prop. color 1st (EH - HE) (2nd half)") + annotate("text", x = -0.125, y = 0.3, label = "r=0.209, p=0.377",size=sizeannot,col="black", fontface="italic") + theme(axis.title.x = element_text(size=9), axis.title.y = element_text(size=9)) + scale_x_continuous(breaks=c(-0.25,0,0.25),labels=c(-0.25,0,0.25),limits=c(-0.25,0.3)) + scale_y_continuous(breaks=c(-0.25,0,0.25),labels=c(-0.25,0,0.25),limits=c(-0.25,0.3)) 

###FIGURE S11D: Exp 3, difference of proportions (EH - HE)
figs11d <- ggplot(exp3_dfdifdif_splitrun,aes(firsthalf,secondhalf)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Prop. color 1st (EH - HE) (1st half)") + ylab("Prop. color 1st (EH - HE) (2nd half)") + annotate("text", x = -0.125, y = 0.3, label = "r=0.418, p=0.022",size=sizeannot,col="black", fontface="italic") + theme(axis.title.x = element_text(size=9), axis.title.y = element_text(size=9)) + scale_x_continuous(breaks=c(-0.25,0,0.25),labels=c(-0.25,0,0.25),limits=c(-0.25,0.3)) + scale_y_continuous(breaks=c(-0.25,0,0.25),labels=c(-0.25,0,0.25),limits=c(-0.25,0.3))

###FIGURE S11E: Exp 4, proportion blocks easy chosen first
figs11e <- ggplot(exp4_chdif_collapsed_splitrun_sin_rav,aes(firsthalf,secondhalf)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Prop. easy set 1st (1st half)") + ylab("Prop. easy set 1st (2nd half)") + annotate("text", x = 0.35, y = 0.8, label = "r=-0.108, p=0.563",size=sizeannot,col="black", fontface="italic") + theme(axis.title.x = element_text(size=9), axis.title.y = element_text(size=9)) + scale_x_continuous(breaks=c(0.25,0.5,0.75),labels=c(0.25,0.5,0.75),limits=c(0.2,0.8)) + scale_y_continuous(breaks=c(0.25,0.5,0.75),labels=c(0.25,0.5,0.75),limits=c(0.2,0.8))  

###FIGURE S11F: Exp 5, proportion trials easy problem chosen first
figs11f <- ggplot(exp5_prio_splithalf_sin,aes(firsthalf,secondhalf)) + geom_point(shape=21) + stat_smooth(method = "lm", col = "black",se = FALSE) + geom_abline(slope=1,intercept=0,linetype="dashed",size=axeswidth) + theme(axis.title.y = element_text(size=sizefont)) + xlab("Prop. easy problem first (1st half)") + ylab("Prop. easy problem first (2nd half)") + scale_x_continuous(breaks=seq(0,1,len=5),limits=c(0,1)) + scale_y_continuous(breaks=seq(0,1,len=5),limits=c(0,1)) + annotate("text", x = 0.25, y = 1, label = "r=0.536, p<0.001",size=sizeannot,col="black", fontface="italic") + theme(axis.title.x = element_text(size=9), axis.title.y = element_text(size=9))

figs11 <- plot_grid(figs11a,figs11b,figs11c,figs11d,figs11e,figs11f, labels = c("A", "B", "C", "D", "E", "F"), ncol = 3, align="v")
save_plot("figures/confpri_figs11.pdf", figs11, ncol = 3, nrow =2, base_height = 3,base_aspect_ratio = 1.1)