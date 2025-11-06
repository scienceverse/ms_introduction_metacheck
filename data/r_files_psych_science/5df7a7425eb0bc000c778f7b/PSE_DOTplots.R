# PSE values that are extracted from CN curves, with Quickpys sin R

# import data from path
setwd("")

plane.thre.out <- read.table("All_PSE_threoutliersmarked-500B.txt", sep=",", header=T)

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
# convert the pse values to degree angle 
plane.thre.out$pse<-plane.thre.out$pse*4



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
#reordering the plot order in x-axis
# edited 05.11.2019 left2 replaced with right2 
#because in the back, left arm of LePoulpe is right space of participants
level_orderh <-factor(horizontal.thre.out$armhead,c('left1','ctrh1','right1','right2','ctrh2','left2')) 
level_orderv <-factor(vertical.thre.out$armhead,c('up1','ctrv1','down1','up2','ctrv2','down2'))





########################################
####### DOTPLOT ########################
########################################

#define the parameters
textsize <- 12
setlimit = c(-12.5,12.2)
setbreak = c(seq(-12,12,4))

####################################################################################################



# CB + SC (circle, blue and red), EB (triangle filled)



####################################################################################################

h_pse <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=pse, color= group, group = group)) +
  geom_point(aes(shape=shapefact), size=2, position = position_jitterdodge(0.35))+
  guides(shape=FALSE)+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.7,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('left1','ctrh1','right1','right2','ctrh2','left2'),
                   labels = NULL)+
  geom_hline(yintercept=0,linetype='dotted', col = 'black')+  
  ylab("PSE values (degrees)") +
  xlab("") +
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"), name = '')+ 
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position ='none') 
h_pse


ggsave("/Dotplot_PSE_hor_09122019_w8_h2dot4.png", h_pse, dpi=300, width=8, height=2.4) # height=3.75 5pointomit

# theme(legend.position = c(0.06, 0.95),
#       legend.box.background = element_blank(),
#       legend.text=element_text(size=12)) 

#plot vertical
#redefine the y axis scaling
setlimit = c(-71,30) #insert the max and min here
setbreak = seq(-72,30,16) #50

v_pse <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=pse, color= group, group = group)) + 
  geom_point(aes(shape=shapefact), size=2, position = position_jitterdodge(0.35))+
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.7,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
                   labels = NULL)+
  geom_hline(yintercept=0,linetype='dotted', col = 'black')+  
  ylab("PSE values (degrees)") +
  xlab("") + #Area
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+ 
  scale_shape_manual(values=c(1,17), name = '')+
  theme(legend.position="none") 
v_pse

ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/Dotplot_PSE_ver_09122019_w8_h2dot4.png", v_pse, dpi=300, width=8, height=2.4) #height=3.75






##########################################

###### 2 EB are in same color as CB but different SHAPE

##########################################

h_pse <- ggplot(data = horizontal.thre.out, aes(x=level_orderh, y=pse, color= group, group = group)) +  #color= blindness_orderh
  geom_point(aes(shape=shapefact), size=2, position = position_jitterdodge(0.35))+
  scale_shape(solid = FALSE) +
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.7,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('left1','ctrh1','right1','right2','ctrh2','left2'),
                   labels = c("Left\nFront","Center\nFront","Right\nFront","Left\nBack","Center\nBack","Right\nBack"))+
  geom_hline(yintercept=0,linetype='dotted', col = 'black')+  
  ylab("PSE values (degrees)") +
  xlab("") + #Area
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"), name = '')+ 
  theme(legend.position="none") 
h_pse
ggsave("/Dotplot_PSE_hor_allpoints_jitter35_wide_noFILL_tri_text16_big.png", h_pse, dpi=300, width=12, height=6) 

#plot vertical
#redefine the y axis scaling
setlimit = c(-71,30) #insert the max and min here
setbreak = seq(-72,30,16) #50

v_pse <- ggplot(data = vertical.thre.out, aes(x=level_orderv, y=pse, color= group, group = group)) + 
   geom_point(aes(shape=shapefact), size=2, position = position_jitterdodge(0.35))+
  scale_shape(solid = FALSE) +
  theme_classic() +
  stat_summary(aes(group=group, color = group), fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=0.5, width=0.7,position = position_dodge(width=.75)) + 
  scale_y_continuous(limits=setlimit, breaks=setbreak) +
  scale_x_discrete(breaks=c('up1','ctrv1','down1','up2','ctrv2','down2'),
                   labels = c("Up\nFront","Center\nFront","Down\nFront","Up\nBack","Center\nBack","Down\nBack"))+
  geom_hline(yintercept=0,linetype='dotted', col = 'black')+  
  ylab("PSE values (degrees)") +
  xlab("") + #Area
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"))+ 
  theme(legend.position="none") 
v_pse

ggsave("/Dotplot_PSE_ver_allpoints_jitter35_wide_noFILL_tri_text16_big.png", v_pse, dpi=300, width=12, height=6)













