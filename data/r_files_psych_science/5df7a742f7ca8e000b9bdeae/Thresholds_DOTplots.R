# Thresholds extracted from CN curves, with Quickpys package in R

# import data from path
setwd("")

plane.thre.out <- read.table("All_Thresholds_outliersmarked-500B.txt", sep=",", header=T)

names(plane.thre.out)[4]<-c('head')
plane.thre.out$arm <- factor(plane.thre.out$arm)
plane.thre.out$subj <- factor(plane.thre.out$subj)
plane.thre.out$head <- factor(plane.thre.out$head)
plane.thre.out$planes <- factor(plane.thre.out$planes)
plane.thre.out$group <- ifelse(plane.thre.out$group=='EB', c('CB'), c('SC'))
plane.thre.out$group <- factor(plane.thre.out$group)

#omit all +-3SD outliers 
plane.thre.out<- subset(plane.thre.out, outlier==0)
plane.thre.out[which(plane.thre.out$thre >12),]

#divide back
horizontal.thre.out <- subset(plane.thre.out,planes=='hor')
vertical.thre.out <- subset(plane.thre.out,planes=='ver')

rownames(horizontal.thre.out) <-c(1:nrow(horizontal.thre.out)) #correct the row numbers
horizontal.thre.out[which(horizontal.thre.out$thre >10),] # we know from glmm there's one outlier
horizontal.thre.out <-horizontal.thre.out[-c(188),] 


rownames(vertical.thre.out) <-c(1:nrow(vertical.thre.out))
vertical.thre.out[which(vertical.thre.out$thre >12),] # we know from glmm there's one outlier
vertical.thre.out <-vertical.thre.out[-c(144),] 

# combine them back
plane.thre.out <- rbind(horizontal.thre.out,vertical.thre.out)

# convert the threshold values to visual angle 
plane.thre.out$thre<-plane.thre.out$thre*4
#horizontal.thre.out$thre<-horizontal.thre.out$thre*4
#vertical.thre.out$thre<-vertical.thre.out$thre*4


# needed libraries
library("ggplot2")
library("gridExtra")


###########################################################################
# I'd like to try to make 1 plot containing all the 6 plots
# from 3column factors let's drop to 2 column factors
plane.thre.out$armhead <-as.factor(paste(plane.thre.out$arm, sep = "",plane.thre.out$head))

#to label 2 EB subject differently, define them
plane.thre.out$blindness<-as.character(plane.thre.out$group)
plane.thre.out$subj <-as.numeric(plane.thre.out$subj )
plane.thre.out$blindness[plane.thre.out$subj == 1 | plane.thre.out$subj==17] <- 'EB'

# to label 2 EB differently only for making two shapes
plane.thre.out$shapefact <- '1'
plane.thre.out <- within(plane.thre.out, shapefact[subj==1 | subj==17]<-'5')

summary(plane.thre.out)

#now divide to 2 axes to plot separately because they need different scaling
horizontal.thre.out <- subset(plane.thre.out,planes=='hor')
vertical.thre.out <- subset(plane.thre.out,planes=='ver')

#reordering the plot order in blindness 
blindness_orderh <- factor(horizontal.thre.out$blindness,c('CB','SC','EB')) 
blindness_orderv <- factor(vertical.thre.out$blindness,c('CB','SC','EB')) 
# edited 05.11.2019 left2 replaced with right2 
#because in the back, left arm of LePoulpe is right space of participants
level_orderh <-factor(horizontal.thre.out$armhead,c('left1','ctrh1','right1','right2','ctrh2','left2'))
level_orderv <-factor(vertical.thre.out$armhead,c('up1','ctrv1','down1','up2','ctrv2','down2'))



#define the parameters
textsize <- 12 
setlimit = c(0,20.6)
setbreak = seq(0,20,4) 


# 
# ########################################
# ####### BOXPLOT ########################
# ########################################
# 
# 
# hbox <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=thre, fill = group, color = group)) +  # color= blindness_orderh, 
#   geom_boxplot(outlier.shape =NA, show.legend = F, alpha=0, aes(middle = mean(thre)) )+ #color = 'white'
#   
#   #geom_boxplot(outlier.shape =NA, show.legend = F, alpha=0.15, aes(middle = mean(thre)) )+ #color = 'white'
#   #geom_boxplot()
#   #facet_wrap( ~ group, scales = 'free') +
#   theme_classic() +
#   scale_fill_manual(values=c("dodgerblue3","firebrick2"))+
#   scale_y_continuous(limits=setlimit, breaks=setbreak) +
#   scale_x_discrete(breaks=c('left1','ctrh1','right1','left2','ctrh2','right2'),
#                    labels = c("Left\nFront","Center\nFront","Right\nFront","Left\nBack","Center\nBack","Right\nBack"))+
#   ylab("Threshold (degree angle)") +
#   xlab("") + #Area
#   #labs(colour = 'Groups') +
#   theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
#   theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
#   theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
#   theme(text=element_text(family="Microsoft Sans Serif")) +
#   geom_point(position = position_jitterdodge(),aes(color=blindness_orderh, group = group), show.legend = F)+
#   scale_color_manual(values=c('CB' = "dodgerblue3",
#                              'SC' = "firebrick2", 
#                              'EB'= 'black'))+
# #   'EB1'= '#5EB539', 
# # 'EB2' = '#e6b334'
# 
#   theme(legend.position = c(0.07, 0.9))
# hbox
# ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/Box_thre_hor_allpoints_jitter.png", hbox, dpi=300, width=6, height=7.5)
# 
# 
# #redefine the y axis scaling
# setlimit = c(0,100) #100 50
# setbreak = seq(0,100,10) #50
# vbox <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=thre, fill = group,color = group)) +  #fill is neccessary if you play with alpha # color= blindness_orderh, 
#   geom_boxplot(outlier.shape =NA, show.legend = F, alpha=0 )+ #color = 'white', #, aes(middle = mean(thre))
#   theme_classic() +
#   scale_fill_manual(values=c("dodgerblue3","firebrick2"))+
#   scale_y_continuous(limits=setlimit, breaks=setbreak) +
#   scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
#                    labels = c("Up\nFront","Center\nFront","Down\nFront","Up\nBack","Center\nBack","Down\nBack"))+
#   ylab("Threshold (degree angle)") +
#   xlab("") + #Area
#   theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
#   theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
#   theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
#   theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
#   theme(text=element_text(family="Microsoft Sans Serif")) +
#   geom_point(position = position_jitterdodge(),aes(color=blindness_orderv, group = group, shape = shapefact), show.legend = F)+
#   scale_color_manual(values=c('CB' = "dodgerblue3",
#                               'SC' = "firebrick2", 
#                               'EB' = 'black'))+
#   theme(legend.position="none") 
# vbox
# 
# ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/Box_thre_ver_allpoints_jitter.png", vbox, dpi=300, width=6, height=7.5)


########################################
####### DOTPLOT ########################
########################################
#plot horizontal
setlimit = c(0,20.6)
setbreak = seq(0,20,4) 



##########################################
# 
# 2 EB are in different SHAPE than CB and in SAME COLOR 
# - Circles (CB + SC), EB Triangle (filled  blue)

##########################################
setlimit = c(0,20.6)
setbreak = seq(0,20,4) 
h <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=thre, color= blindness_orderh, group = group)) + 
  geom_point(aes(shape=factor(shapefact)),size=2, position = position_jitterdodge(0.35))+
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.5,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('left1','ctrh1','right1','left2','ctrh2','right2'),
                   labels = NULL)+
  ylab("Threshold (degree angle)") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2",'dodgerblue3'), name = '')+ 
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position = 'none')
h
ggsave("/Dotplot_thre_hor_09122019_w8_h2dot4.png", h, dpi=300, width=8, height=2.4)

setlimit = c(0,100)
setbreak = seq(0,100,10)

v <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=thre, color= blindness_orderv, group = group)) + 
  geom_point(aes(shape=shapefact),size=2, position = position_jitterdodge(0.35))+
  scale_shape_identity() +
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.5,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
                   labels =NULL)+
  ylab("Threshold (degree angle)") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2",'dodgerblue3'))+
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none") 

v
ggsave("/Dotplot_thre_ver3_09122019_w8_h1dot0.png", v, dpi=300, width=8, height=1) #5pointomit







##########################################
###### 2 EB are in same color as CB but different SHAPE
##########################################

h <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=thre, color= group, group = group)) +
  geom_point(aes(shape=shapefact), size=3, position = position_jitterdodge(0.35))+
  scale_shape(solid = FALSE, guide = FALSE) +
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.7,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('left1','ctrh1','right1','left2','ctrh2','right2'),
                   labels = c("Left\nFront","Center\nFront","Right\nFront","Left\nBack","Center\nBack","Right\nBack"))+
  ylab("Threshold (degrees)") +
  xlab("") + #Area
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"), name = '')+ 
  theme(legend.position = c(0.07, 0.9),
        legend.box.background = element_blank(),
        legend.text=element_text(size=12))
h
ggsave("/Dotplot_thre_hor_allpoints_jitter35_wide_noFILL_tri_text16_legend.png", h, dpi=300, width=12, height=6) 

#plot vertical
#redefine the y axis scaling
setlimit = c(0,50) #100
setbreak = seq(0,50,10) #50


v <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=thre, color= group, group = group)) + 
   geom_point(aes(shape=shapefact), size=3, position = position_jitterdodge(0.35))+
  scale_shape(solid = FALSE) +
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.7,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
                   labels = c("Up\nFront","Center\nFront","Down\nFront","Up\nBack","Center\nBack","Down\nBack"))+
  ylab("Threshold (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+ 
  theme(legend.position="none") 
v

ggsave("/Dotplot_thre_ver_5points_jitter35_wide_noFILL_tritext16_part1.png", v, dpi=300, width=12, height=3.75)


##########################################
####### 2 EB are in same shape as CB and different COLOR 
##########################################
setlimit = c(0,20.6)
setbreak = seq(0,20,4) 
h <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=thre, color= blindness_orderh, group = group)) + 
  geom_point(aes(shape='circle open'),size=3, position = position_jitterdodge(0.08))+
  scale_shape(solid = FALSE) +
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.5,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('left1','ctrh1','right1','left2','ctrh2','right2'),
                   labels = c("Left\nFront","Center\nFront","Right\nFront","Left\nBack","Center\nBack","Right\nBack"))+
  ylab("Threshold (degree angle)") +
  xlab("") + 

  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2",'black'), name = '')+ 
  theme(legend.position = c(0.07, 0.9),
        legend.box.background = element_blank(),
        legend.text=element_text(size=12))
h
ggsave("/Dotplot_thre_hor_allpoints_jitter_wide_noFILL_Bcirc.png", h, dpi=300, width=12, height=3.75)

setlimit = c(0,50)
setbreak = seq(0,50,10) 

v <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=thre, color= blindness_orderv, group = group)) + 
  geom_point(aes(shape='circle open'),size=3, position = position_jitterdodge(0.08))+
  scale_shape(solid = FALSE) +
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.5,position = position_dodge(width=.75)) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
                   labels = c("Up\nFront","Center\nFront","Down\nFront","Up\nBack","Center\nBack","Down\nBack"))+
  ylab("Threshold (degree angle)") +
  xlab("") + #Area
  #labs(colour = 'Groups') +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2",'black'))+ 
  theme(legend.position="none") 

v
ggsave("/Dotplot_thre_ver_5points_jitter_wide_noFILL_Bcirc.png", v, dpi=300, width=12, height=3.75)




##############################################################################

# 2 EB are in different SHAPE than CB and different COLOR 
# - Circles (CB + SC), Triangle (EB black color)

##############################################################################
setlimit = c(0,20.6)
setbreak = seq(0,20,4) 
h <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=thre, color= blindness_orderh, group = group)) + 
  geom_point(aes(shape=factor(shapefact)),size=2, position = position_jitterdodge(0.35))+
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.5,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('left1','ctrh1','right1','left2','ctrh2','right2'),
                   labels = c("Left\nFront","Center\nFront","Right\nFront","Left\nBack","Center\nBack","Right\nBack"))+
  ylab("Threshold (degree angle)") +
  xlab("") + #Area
  #labs(colour = 'Groups') +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2",'black'), name = '')+ 
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position = c(0.07, 0.9),
        legend.box.background = element_blank(),
        legend.text=element_text(size=12))
h
ggsave("/Dotplot_thre_hor_allpoints_jitter_wide_noFILL_Btri.png", h, dpi=300, width=12, height=3.75)

setlimit = c(0,50)
setbreak = seq(0,50,10)

v <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=thre, color= blindness_orderv, group = group)) + 
  geom_point(aes(shape=shapefact),size=3, position = position_jitterdodge(0.35))+
  scale_shape_identity() +
  scale_shape(solid = FALSE) +
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.5,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
                   labels = c("Up\nFront","Center\nFront","Down\nFront","Up\nBack","Center\nBack","Down\nBack"))+
  ylab("Threshold (degree angle)") +
  xlab("") + 

  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2",'black'))+ 
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none") 

v
ggsave("/Dotplot_thre_ver_5points_jitter_wide_noFILL_Btri.png", v, dpi=300, width=12, height=3.75)



###########################

# 12 violin plots separately

###########################
##### parameters to change
bar_size = 0.5 
textsize = 12
setlimit = c(0,20.6) 
setbreak = seq(0,20,4)

# front
# ctrh front

# plotting the DOT plot
ctrh.front <- ggplot(data=thre.ctrh.front, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrh.front15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrh.front2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.3, color="grey33") +
  ggtitle("Center \n Front") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize-2, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none")
ctrh.front


# left front

left.front <- ggplot(data=thre.left.front, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.left.front15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.left.front2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Left\n Front") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("Threshold (deg in visual angle)") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
left.front


# right front

right.front <- ggplot(data=thre.right.front, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.right.front15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.right.front2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Right\n Front") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
right.front


hor.front.multipl <- grid.arrange(left.front, ctrh.front, right.front, nrow=1, ncol=3)
#ggsave("/violinplot_hor_front.png", hor.front.multipl, dpi=300, width=3, height=4.5)

############ MAKE 6 PLOTS IN THE SAME ROW ##################

# back
# ctrh back
ctrh.back <- ggplot(data=thre.ctrh.back, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrh.back15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrh.back2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Center\n Back") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
ctrh.back

#ggsave("/violinplot_ctrh_back.png", ctrh.back, dpi=300, width=3, height=4.5)


# left back
left.back <- ggplot(data=thre.left.back, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.left.back15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.left.back2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Left\n Back") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
left.back

# sav
#ggsave("/violinplot_left_back.png", left.back, dpi=300, width=3, height=4.5)


# right back 


right.back <- ggplot(data=thre.right.back, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.right.back15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.right.back2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Right\n Back") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
right.back


# save picture
#ggsave("/violinplot_right_back.png", right.back, dpi=300, width=3, height=4.5)


hor.back.multipl <- grid.arrange(left.back, ctrh.back, right.back, nrow=1, ncol=3)
horizontal.thres.multip <- grid.arrange(left.front, ctrh.front, right.front, left.back, ctrh.back, right.back, nrow=1, ncol=6)

ggsave("/Dotplot_thres_hor_v2.png", horizontal.thres.multip, width=12, height=7.5, dpi=300)




# vertical
setlimit = c(0,60) 
setbreak = seq(0,50,10) 

# front
# ctrv front

ctrv.front <- ggplot(data=thre.ctrv.front, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrv.front15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrv.front2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Center\n Front") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
ctrv.front

# save picture
#ggsave("violinplot_ctrv_front.png", ctrv.front, dpi=300, width=3, height=4.5)


# up front - 

up.front <- ggplot(data=thre.up.front, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.up.front15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.up.front2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Up\n Front") +
  theme(plot.title = element_text(size=18)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("Threshold (deg in visual angle)") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none")
up.front

# save
#ggsave("/violinplot_up_front.png", up.front, dpi=300, width=3, height=4.5)


# down front 

down.front <- ggplot(data=thre.down.front, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.down.front15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.down.front2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Down\n Front") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
down.front

# save picture
#ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/violinplot_down_front.png", down.front, dpi=300, width=3, height=4.5)


ver.front.multipl <- grid.arrange(up.front, ctrv.front, down.front, nrow=1, ncol=3)
#ggsave("/Dotplot_thres_ver_front.png", ver.front.multipl, dpi=300, width=3, height=4.5)



# back

# ctrv back


ctrv.back <- ggplot(data=thre.ctrv.back, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrv.back15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.ctrv.back2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Center\n Back") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
ctrv.back

# save picture
#ggsave("/violinplot_ctrv_back.png", ctrv.back, dpi=300, width=3, height=4.5)


# up back -


up.back <- ggplot(data=thre.up.back, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.up.back15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.up.back2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Up\n Back") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
up.back

# save picture
#ggsave("/violinplot_up_back.png", up.back, dpi=300, width=3, height=4.5)



# down back - 


down.back <- ggplot(data=thre.down.back, aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.down.back15EB,aes(x=group, y=thre, color=group)) +
  geom_point(data=thre.down.back2EB, aes(x=group, y=thre), colour=c('#5EB539', '#e6b334')) +
  theme_classic() +
  
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.3, width=0.3, color="grey33") +
  ggtitle("Down\n Back") +
  theme(plot.title = element_text(size=textsize)) +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab("") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  theme(legend.position="none") 
down.back

# save picture
#ggsave("/violinplot_down_back.png", down.back, dpi=300, width=3, height=4.5)


ver.back.multipl <- grid.arrange(up.back, ctrv.back, down.back, nrow=1, ncol=3)
#ggsave("/violinplot_vertical_back.png", ver.back.multipl, dpi=300, width=3, height=4.5)
vertical.thres.multip <- grid.arrange(up.front, ctrv.front, down.front,up.back, ctrv.back, down.back, nrow=1, ncol=6)
#ggsave("/violinplot_ver_AllSET32.png", vertical.thres.multip, width=10, height=10, dpi=300)

# save all in 1 fig
All.thres.multip <- grid.arrange(horizontal.thres.multip, vertical.thres.multip)
ggsave("/Dotplot_thres_v2.png", All.thres.multip, width=12, height=7.5, dpi=300)

