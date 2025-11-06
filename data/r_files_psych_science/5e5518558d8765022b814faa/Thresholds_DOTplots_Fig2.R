# Main contributor is FG, CB was edited on 14.02.2020
# based on 20191024_Thresholds_dotplots.R script

# thres extracted from CN curves, with Quickpys in R

# needed libraries
library("ggplot2")
library("gridExtra")

# import data
threshold_path <-"/Users/your/local/directory/Quickpsy_fitting"

setwd(threshold_path)

plane.thre.out <- read.table("All_Thresholds_outliersmarked-500B.txt", sep=",", header=T)
names(plane.thre.out)[4]<-c('head')
plane.thre.out$arm <- factor(plane.thre.out$arm)
plane.thre.out$subj <- factor(plane.thre.out$subj)
plane.thre.out$head <- factor(plane.thre.out$head)
plane.thre.out$planes <- factor(plane.thre.out$planes)
plane.thre.out$group <- ifelse(plane.thre.out$group=='EB', c('CB'), c('SC'))

#omit all +-3SD outliers 
plane.thre.out<- subset(plane.thre.out, outlier==0)
plane.thre.out[which(plane.thre.out$thre >12),]

# mark 2 EB 
plane.thre.out$shapefact <- "o"
plane.thre.out <- within(plane.thre.out, shapefact[subj==1|subj==17]<-"w")

#divide back
horizontal.thre.out <- subset(plane.thre.out,planes=='hor')
vertical.thre.out <- subset(plane.thre.out,planes=='ver')

rownames(horizontal.thre.out) <-c(1:nrow(horizontal.thre.out)) #correct the row numbers
horizontal.thre.out[which(horizontal.thre.out$thre >10),] # we know from glmm there's one outlier
horizontal.thre.out <-horizontal.thre.out[-c(188),] 


rownames(vertical.thre.out) <-c(1:nrow(vertical.thre.out))
vertical.thre.out[which(vertical.thre.out$thre >12),] # we know from glmm there's one outlier
vertical.thre.out <-vertical.thre.out[-c(144),] 



# multiply threshold value by 4 to get degrees
horizontal.thre.out$thre <- horizontal.thre.out$thre*4
vertical.thre.out$thre <- vertical.thre.out$thre*4

# dividing by condition because  will need one barplot per condition (subsetting)
#horizontal
# 1.
thre.ctrh.front <- subset(horizontal.thre.out, arm=='ctrh'&head==1)
# aggregate means and standard errors for the plotting
thre.ctrh.front.av <- aggregate(thre.ctrh.front$thre, list(thre.ctrh.front$group), mean)
names(thre.ctrh.front.av) <- c("group", "thre")
thre.ctrh.front.sd <- aggregate(thre.ctrh.front$thre, list(thre.ctrh.front$group), sd)
names(thre.ctrh.front.sd) <- c("group", "sd")
thre.ctrh.front.av$sd <- thre.ctrh.front.sd$sd

###

# 2.
thre.left.front <- subset(horizontal.thre.out, arm=='left'&head==1)
# # aggregate to get means and standard errors for the plot
thre.left.front.av <- aggregate(thre.left.front$thre, list(thre.left.front$group), mean)
names(thre.left.front.av) <- c("group", "thre")
thre.left.front.sd <- aggregate(thre.left.front$thre, list(thre.left.front$group), sd)
names(thre.left.front.sd) <- c("group", "sd")
thre.left.front.av$sd <- thre.left.front.sd$sd

###

# 3.
thre.right.front <- subset(horizontal.thre.out, arm=='right'&head==1)
# # aggregate to get means and standard errors for the plot
thre.right.front.av <- aggregate(thre.right.front$thre, list(thre.right.front$group), mean)
names(thre.right.front.av) <- c("group", "thre")
thre.right.front.sd <- aggregate(thre.right.front$thre, list(thre.right.front$group), sd)
names(thre.right.front.sd) <- c("group", "sd")
thre.right.front.av$sd <- thre.right.front.sd$sd

##
# 4.
thre.ctrh.back <- subset(horizontal.thre.out, arm=='ctrh'&head==2)
# # aggregating for means and standard error
thre.ctrh.back.av <- aggregate(thre.ctrh.back$thre, list(thre.ctrh.back$group), mean)
names(thre.ctrh.back.av) <- c("group", "thre")
thre.ctrh.back.sd <- aggregate(thre.ctrh.back$thre, list(thre.ctrh.back$group), sd)
names(thre.ctrh.back.sd) <- c("group", "sd")
thre.ctrh.back.av$sd <- thre.ctrh.back.sd$sd
##
# 5. 
thre.left.back <- subset(horizontal.thre.out, arm=='left'&head==2)
# # aggregating for means and standard error
thre.left.back.av <- aggregate(thre.left.back$thre, list(thre.left.back$group), mean)
names(thre.left.back.av) <- c("group", "thre")
thre.left.back.sd <- aggregate(thre.left.back$thre, list(thre.left.back$group), sd)
names(thre.left.back.sd) <- c("group", "sd")
thre.left.back.av$sd <- thre.left.back.sd$sd


##
# 6.
thre.right.back <- subset(horizontal.thre.out, arm=='right'&head==2)
# #aggregating for means and standard error
thre.right.back.av <- aggregate(thre.right.back$thre, list(thre.right.back$group), mean)
names(thre.right.back.av) <- c("group", "thre")
thre.right.back.sd <- aggregate(thre.right.back$thre, list(thre.right.back$group), sd)
names(thre.right.back.sd) <- c("group", "sd")
thre.right.back.av$sd <- thre.right.back.sd$sd

#vertical

##
# 7.
thre.ctrv.front <- subset(vertical.thre.out, arm=='ctrv'&head==1)
# # aggregating for means and standard error
thre.ctrv.front.av <- aggregate(thre.ctrv.front$thre, list(thre.ctrv.front$group), mean)
names(thre.ctrv.front.av) <- c("group", "thre")
thre.ctrv.front.sd <- aggregate(thre.ctrv.front$thre, list(thre.ctrv.front$group), sd)
names(thre.ctrv.front.sd) <- c("group", "sd")
thre.ctrv.front.av$sd <- thre.ctrv.front.sd$sd

##
# 8.
thre.up.front <- subset(vertical.thre.out, arm=='up'&head==1)
# # aggregating for means and standard error
thre.up.front.av <- aggregate(thre.up.front$thre, list(thre.up.front$group), mean)
names(thre.up.front.av) <- c("group", "thre")
thre.up.front.sd <- aggregate(thre.up.front$thre, list(thre.up.front$group), sd)
names(thre.up.front.sd) <- c("group", "sd")
thre.up.front.av$sd <- thre.up.front.sd$sd

##
# 9.
thre.down.front <- subset(vertical.thre.out, arm=='down'&head==1)
# # aggregating for means and standard error
thre.down.front.av <- aggregate(thre.down.front$thre, list(thre.down.front$group), mean)
names(thre.down.front.av) <- c("group", "thre")
thre.down.front.sd <- aggregate(thre.down.front$thre, list(thre.down.front$group), sd)
names(thre.down.front.sd) <- c("group", "sd")
thre.down.front.av$sd <- thre.down.front.sd$sd

##
# 10.
thre.ctrv.back <- subset(vertical.thre.out, arm=='ctrv'&head==2)
thre.ctrv.back.av <- aggregate(thre.ctrv.back$thre, list(thre.ctrv.back$group), mean)
names(thre.ctrv.back.av) <- c("group", "thre")
thre.ctrv.back.sd <- aggregate(thre.ctrv.back$thre, list(thre.ctrv.back$group), sd)
names(thre.ctrv.back.sd) <- c("group", "sd")
thre.ctrv.back.av$sd <- thre.ctrv.back.sd$sd

##
# 11.
thre.up.back <- subset(vertical.thre.out, arm=='up'&head==2)
thre.up.back.av <- aggregate(thre.up.back$thre, list(thre.up.back$group), mean)
names(thre.up.back.av) <- c("group", "thre")
thre.up.back.sd <- aggregate(thre.up.back$thre, list(thre.up.back$group), sd)
names(thre.up.back.sd) <- c("group", "sd")
thre.up.back.av$sd <- thre.up.back.sd$sd

##
# 12. 
thre.down.back <- subset(vertical.thre.out, arm=='down'&head==2)
thre.down.back.av <- aggregate(thre.down.back$thre, list(thre.down.back$group), mean)
names(thre.down.back.av) <- c("group", "thre")
thre.down.back.sd <- aggregate(thre.down.back$thre, list(thre.down.back$group), sd)
names(thre.down.back.sd) <- c("group", "sd")
thre.down.back.av$sd <- thre.down.back.sd$sd

# dotplot folder
setwd("../Graphs/dotsbarplots")
library('svglite')

setlimit = c(0,20.6) 
setbreak = c(0,5,10,15,20)

shapesize = 4
transparent = 1 #0.6
jitter2EB = 0.35

# note: in this state, stat_summry function inherits the data as specified in call to ggplot

################# front  #################
# plotting the dot plot -- reorganise the data -- divide it into two dataframes to jitter differently


# ctrh front
Data2EB<- subset(thre.ctrh.front,subj=='1' | subj=='17')
Dataall<- subset(thre.ctrh.front,subj!='1' & subj!='17')


fig <- ggplot(data=thre.ctrh.front.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")
fig

ggsave("dotbarplot_ctrh_front_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrh_front_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrh_front_wsh.svg", fig, dpi=300, width=3, height=4.5)



# left front
Data2EB<- subset(thre.left.front,subj=='1' | subj=='17')
Dataall<- subset(thre.left.front,subj!='1' & subj!='17')

fig <- ggplot(data=thre.left.front.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")
fig


ggsave("dorbarplot_left_front_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_left_front_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_left_front_wsh.svg", fig, dpi=300, width=3, height=4.5)


# right front
Data2EB<- subset(thre.right.front,subj=='1' | subj=='17')
Dataall<- subset(thre.right.front,subj!='1' & subj!='17')

fig <- ggplot(data=thre.right.front.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")
fig

ggsave("dotbarplot_right_front_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_right_front_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_right_front_wsh.svg", fig, dpi=300, width=3, height=4.5)


################### back ###################

# ctrh back
Data2EB<- subset(thre.ctrh.back,subj=='1' | subj=='17')
Dataall<- subset(thre.ctrh.back,subj!='1' & subj!='17')


fig <- ggplot(data=thre.ctrh.back.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")
fig

ggsave("dotbarplot_ctrh_back_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrh_back_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrh_back_wsh.svg", fig, dpi=300, width=3, height=4.5)


# left back
#because in the back, left arm of LePoulpe is right space of participants
#let's make a correction here by inserting right instead of left
Data2EB<- subset(thre.right.back,subj=='1' | subj=='17')
Dataall<- subset(thre.right.back,subj!='1' & subj!='17')

fig <- ggplot(data=thre.right.back.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")
fig

# save picture
ggsave("dotbarplot_left_back_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_left_back_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_left_back_wsh.svg", fig, dpi=300, width=3, height=4.5)


# right back
#because in the back, left arm of LePoulpe is right space of participants
#let's make a correction here by inserting right instead of left
Data2EB<- subset(thre.left.back,subj=='1' | subj=='17')
Dataall<- subset(thre.left.back,subj!='1' & subj!='17')

fig <- ggplot(data=thre.left.back.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")  
fig


# save picture
ggsave("dotbarplot_right_back_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_right_back_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_right_back_wsh.svg", fig, dpi=300, width=3, height=4.5)




######################### VERTICAL #########################
############################################################
setlimit = c(0,50) 
setbreak = seq(0, 50, 10) 

################## front ###################################
# ctrv front
Data2EB<- subset(thre.ctrv.front,subj=='1' | subj=='17')
Dataall<- subset(thre.ctrv.front,subj!='1' & subj!='17')

fig <- ggplot(data=thre.ctrv.front.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")  
fig

# save picture
ggsave("dotbarplot_ctrv_front_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrv_front_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrv_front_wsh.svg", fig, dpi=300, width=3, height=4.5)


# up front
Data2EB<- subset(thre.up.front,subj=='1' | subj=='17')
Dataall<- subset(thre.up.front,subj!='1' & subj!='17')

fig <- ggplot(data=thre.up.front.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")  
fig

# save picture
ggsave("dotbarplot_up_front_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_up_front_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_up_front_wsh.svg", fig, dpi=300, width=3, height=4.5)


# down front
# setlimit = c(50,100) 
# setbreak = seq(50, 100, 10) 

Data2EB<- subset(thre.down.front,subj=='1' | subj=='17')
Dataall<- subset(thre.down.front,subj!='1' & subj!='17')

fig <- ggplot(data=thre.down.front.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none")   
fig

# save picture
ggsave("dotbarplot_down_front_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_down_front_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_down_front_wsh.svg", fig, dpi=300, width=3, height=4.5)

# ggsave("dotbarplot_down_front_wsh2.png", fig, dpi=300, width=3, height=4.5)
# ggsave("dotbarplot_down_front_wsh2.eps", fig, device="eps",dpi=300, width=3, height=4.5)
# ggsave("dotbarplot_down_front_wsh2.svg", fig, dpi=300, width=3, height=4.5)



################## back ###################################
# setlimit = c(50,100)
# setbreak = seq(50, 100, 10)

# ctrv back 
Data2EB<- subset(thre.ctrv.back,subj=='1' | subj=='17')
Dataall<- subset(thre.ctrv.back,subj!='1' & subj!='17')

fig <- ggplot(data=thre.ctrv.back.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none") 
fig

# save picture
ggsave("dotbarplot_ctrv_back_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrv_back_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_ctrv_back_wsh.svg", fig, dpi=300, width=3, height=4.5)

# ggsave("dotbarplot_ctrv_back_wsh2.png", fig, dpi=300, width=3, height=4.5)
# ggsave("dotbarplot_ctrv_back_wsh2.eps", fig, device="eps",dpi=300, width=3, height=4.5)
# ggsave("dotbarplot_ctrv_back_wsh2.svg", fig, dpi=300, width=3, height=4.5)




# up back 
# setlimit = c(50,100)
# setbreak = seq(50, 100, 10)

Data2EB<- subset(thre.up.back,subj=='1' | subj=='17')
Dataall<- subset(thre.up.back,subj!='1' & subj!='17')

fig <- ggplot(data=thre.up.back.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none") 
fig

# save picture
ggsave("dotbarplot_up_back_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_up_back_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_up_back_wsh.svg", fig, dpi=300, width=3, height=4.5)

# ggsave("dotbarplot_up_back_wsh2.png", fig, dpi=300, width=3, height=4.5)
# ggsave("dotbarplot_up_back_wsh2.eps", fig, device="eps",dpi=300, width=3, height=4.5)
# ggsave("dotbarplot_up_back_wsh2.svg", fig, dpi=300, width=3, height=4.5)

# down back 
Data2EB<- subset(thre.down.back,subj=='1' | subj=='17')
Dataall<- subset(thre.down.back,subj!='1' & subj!='17')

fig <- ggplot(data=thre.down.back.av, aes(x=group, y=thre, color=group)) +
  theme_classic() +
  geom_errorbar(aes(ymin = thre-sd/2, ymax = thre+sd/2, group=group), size=0.8, width=0.15, alpha = transparent) + 
  
  geom_point(data=Dataall,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=0.12,h=0)) +
  geom_point(data=Data2EB,aes(x=group, y=thre, color=group, shape=shapefact), size=shapesize, position=position_jitter(width=jitter2EB, h=0)) +
  
  stat_summary(aes(color=group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.4) +
  ggtitle("") +
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=18, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=18, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak, position="right") +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none") 
fig
# save picture
ggsave("dotbarplot_down_back_wsh.png", fig, dpi=300, width=3, height=4.5)
ggsave("dotbarplot_down_back_wsh.eps", fig, device="eps",dpi=300, width=3, height=4.5)
ggsave("dotbarplot_down_back_wsh.svg", fig, dpi=300, width=3, height=4.5)



