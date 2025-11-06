# graphs with proportion points per speaker
# start from reading the logfiles

setwd("/logfiles subjects")
data <- NA

dataNames = "*.txt"
temp = list.files(pattern=dataNames)
numSUBJ <- length(temp)     

SUBJ = list()
for (i in 1:numSUBJ) {
  x  = (read.csv(temp[i], skip = 1, sep = "", header=FALSE))
  x$id <- i
  SUBJ[i] = list(x)
}

# bind txt files using rbind comment
data = do.call(rbind, SUBJ)

# assign names to the columns
names(data) <- c("stim", "pos", "loopnum", 
                 "trial", "arm", "plane", 
                 "speaker", "response", "side", 
                 "corrside", "correct", "RT", "subj")

# adding the labels of groups: EB/SC
# be careful the order you read the all logfiles, otherwise you might mess up
#print "temp" for checking data order
data$group <- ifelse(data$subj<=17, c('CB'), c('SC'))

# get rid of stim 2 (pink noise) or stim 1 (voice)
#data.pink <- subset(data, stim==2)
data <- subset(data, stim==2)


# divide horizontal and vertical 
data.horizontal <- subset(data, data$plane==1)
data.vertical <- subset(data, data$plane==2)

# filter out plane and stim column  & loopnum
data.horizontal[,1] <- NULL
data.vertical[,1] <- NULL
# loopnum
data[,3] <- NULL
data[,3] <- NULL
data.horizontal[,2] <- NULL
data.vertical[,2] <- NULL


#HORIZONTAL

# filter out front/back response
data.hor.front <- subset(data.horizontal, pos==1)
data.hor.back <- subset(data.horizontal, pos==2)

# response buttons 5 == left, 6 == right
# change response => horizontal:6=1; 5=0; FOR FRONT 
data.hor.front <- within(data.hor.front, response[response==6] <- 1)
data.hor.front <- within(data.hor.front, response[response==5] <- 0)

# change response => horizontal:6=0; 5=1; FOR BACK
#cb edited 5 to 6, nd vice versa 16.01.2019
#cb triple/quatrople checked 04.11.2019 that response in the back should be like below
# the above comment was wrong, not the command below
data.hor.back <- within(data.hor.back, response[response==5] <- 0)
data.hor.back <- within(data.hor.back, response[response==6] <- 1)
# reverse correctness
data.hor.back <- within(data.hor.back, correct[correct==1] <- -1) #just to assign into a temp
data.hor.back <- within(data.hor.back, correct[correct==0] <- 1)
data.hor.back <- within(data.hor.back, correct[correct==-1] <- 0)

# let's also correct the corrside in the BACK
data.hor.back <- within(data.hor.back, corrside[corrside==5] <- -5) #just to assign into a temp
data.hor.back <- within(data.hor.back, corrside[corrside==6] <- 5)
data.hor.back <- within(data.hor.back, corrside[corrside==-5] <- 6)


# reversing "right" arm for error in coding
data.hor.front<- within(data.hor.front,side[arm=="right"]<- side[arm=="right"]*-1)

# correction of the script mistake
data.hor.back <- within(data.hor.back,side[arm=="left"]<- side[arm=="left"]*-1)
data.hor.back <- within(data.hor.back,side[arm=="ctrh"]<- side[arm=="ctrh"]*-1)


# in the horizontal back condition left arm is right area // right arm is left area of the subject
data.hor.back$arm <-as.character(data.hor.back$arm)
data.hor.back <- within(data.hor.back, arm[arm=="left"] <-("Alien"))#just to assign into a temp
data.hor.back <- within(data.hor.back, arm[arm=='right'] <- 'left')
data.hor.back <- within(data.hor.back, arm[arm=='Alien'] <- 'right')
#####

# recombine horizontal data
data.horizontal <- rbind(data.hor.front, data.hor.back)
# assign the number for the arms - stupid ifelse loop
data.horizontal$numcodedarm <-ifelse(data.horizontal$arm=='left', 1, ifelse(data.horizontal$arm=='ctrh',2, ifelse(data.horizontal$arm=='right',3,9)))


# filter out NaN response
data.horizontal <- subset(data.horizontal, response==1|response==0)

# VERTICAL
#response buttons 8 == down, 5 == up
# change response => vertical:8=1; 5=0
data.vertical<- within(data.vertical, response[response==8] <- 1)
data.vertical <- within(data.vertical, response[response==5] <- 0)

# filter out NaN response
data.vertical <- subset(data.vertical, response==1|response==0)

# filter out front/back response
data.ver.front <- subset(data.vertical, pos==1)
data.ver.back <- subset(data.vertical, pos==2)

#reversing "up" and "ctrv" part (for error in the code), front and back, stim 1 e 2
data.ver.front <- within(data.ver.front,side[arm=="up"]<- side[arm=="up"]*-1)
data.ver.back <- within(data.ver.back,side[arm=="up"]<- side[arm=="up"]*-1)

data.ver.front <- within(data.ver.front,side[arm=="ctrv"]<- side[arm=="ctrv"]*-1)
data.ver.back <- within(data.ver.back,side[arm=="ctrv"]<- side[arm=="ctrv"]*-1)


# COMBINE DATA
data.vertical <- rbind(data.ver.front, data.ver.back)
data.vertical$numcodedarm <-ifelse(data.vertical$arm=='up', 4, ifelse(data.vertical$arm=='ctrv',5, ifelse(data.vertical$arm=='down',6,9)))

# recombine for plane data
data.planes <- rbind(data.horizontal, data.vertical)
data.planes$axis <- ifelse(data.planes$numcodedarm<=3, c('hor'), c('ver'))


library("ggplot2")
library("gridExtra")


# data should be aggregate only across repetitions
# so only one point per speaker per subject

# divide the data into each condition
#vertical selection
up.front <- subset(data.vertical, numcodedarm==4&pos==1)
ctrv.front <- subset(data.vertical, numcodedarm==5&pos==1)
down.front <- subset(data.vertical, numcodedarm==6&pos==1)
up.back <- subset(data.vertical, numcodedarm==4&pos==2)
ctrv.back <- subset(data.vertical, numcodedarm==5&pos==2)
down.back <- subset(data.vertical, numcodedarm==6&pos==2)
#horizontal selection
left.front <- subset(data.horizontal, numcodedarm==1&pos==1)
ctrh.front <- subset(data.horizontal, numcodedarm==2&pos==1)
right.front <- subset(data.horizontal, numcodedarm==3&pos==1)
left.back <- subset(data.horizontal, numcodedarm==1&pos==2)
ctrh.back <- subset(data.horizontal, numcodedarm==2&pos==2)
right.back <- subset(data.horizontal, numcodedarm==3&pos==2)

#Calculating proportion of correct responses
#horizontal
pcorr.left.front <- aggregate(left.front$correct, list(left.front$side, left.front$subj, left.front$group), mean)
names(pcorr.left.front) <- c("speaker","subj","group","pcorr")

pcorr.right.front <- aggregate(right.front$correct, list(right.front$side, right.front$subj, right.front$group), mean)
names(pcorr.right.front) <- c("speaker","subj","group","pcorr")

pcorr.ctrh.front <- aggregate(ctrh.front$correct, list(ctrh.front$side, ctrh.front$subj, ctrh.front$group), mean)
names(pcorr.ctrh.front) <- c("speaker","subj","group","pcorr")

pcorr.left.back <- aggregate(left.back$correct, list(left.back$side, left.back$subj, left.back$group), mean)
names(pcorr.left.back) <- c("speaker","subj","group","pcorr")

pcorr.right.back <- aggregate(right.back$correct, list(right.back$side, right.back$subj, right.back$group), mean)
names(pcorr.right.back) <- c("speaker","subj","group","pcorr")

pcorr.ctrh.back <- aggregate(ctrh.back$correct, list(ctrh.back$side, ctrh.back$subj, ctrh.back$group), mean)
names(pcorr.ctrh.back) <- c("speaker","subj","group","pcorr")

#vertical
datav <-up.front
pcorr.up.front <- aggregate(datav$correct, list(datav$side, datav$subj, datav$group), mean)
names(pcorr.up.front) <- c("speaker","subj","group","pcorr")

datav <-ctrv.front
pcorr.ctrv.front <- aggregate(datav$correct, list(datav$side, datav$subj, datav$group), mean)
names(pcorr.ctrv.front) <- c("speaker","subj","group","pcorr")

datav <-down.front
pcorr.down.front <- aggregate(datav$correct, list(datav$side, datav$subj, datav$group), mean)
names(pcorr.down.front) <- c("speaker","subj","group","pcorr")

datav <-up.back
pcorr.up.back <- aggregate(datav$correct, list(datav$side, datav$subj, datav$group), mean)
names(pcorr.up.back) <- c("speaker","subj","group","pcorr")

datav <-ctrv.back
pcorr.ctrv.back <- aggregate(datav$correct, list(datav$side, datav$subj, datav$group), mean)
names(pcorr.ctrv.back) <- c("speaker","subj","group","pcorr")

datav <-down.back
pcorr.down.back <- aggregate(datav$correct, list(datav$side, datav$subj, datav$group), mean)
names(pcorr.down.back) <- c("speaker","subj","group","pcorr")



############################

# ballon plots

############################

##### parameters to change
stroke_size = 1.1
transparent = 0.3 
circle_max = 4.4 
bar_size = 0.3
bar_circle_size = 0.5 
textsize = 12
xlabels = c("D1","D2","D3","D4","D5")
xlimits = c(1:5) 

###### 
set_breaks = function(limits) {
  print(limits)
  seq(limits[1], limits[2], by = 3)
}

#LEFT FRONT
# Mirror Average  2sides (e.g. left/right)
DATA = pcorr.left.front
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")


plot.left.front <- ggplot(DATA2, aes(x=speaker, y=pcorr, color=group)) + 
geom_count(aes(group = group, size = ..n..),position = position_dodge(width=.8), fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +

 scale_size_area(max_size = circle_max,  breaks = set_breaks, guide = FALSE) + 
 # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("Proportion of Correct Responses") +
  xlab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks = seq(0, 1, .2),limits=c(0,1)) +   
  ggtitle("") +

  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2"), name = '') +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8))+
 theme(legend.box.background = element_blank()) +
  theme(legend.position = c(0.07, 0.95),
        legend.box.background = element_blank())
# theme(legend.position="none") # no legend at all
plot.left.front

###############

# RIGHT FRONT
# Mirror Average  2sides (e.g. left/right)
DATA = pcorr.right.front
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.right.front <- ggplot(DATA2, aes(x=speaker, y=pcorr, color=group)) +  
  geom_count(aes(group = group),position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max, name = '# of Occurances') +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8))+
  theme(legend.position="none")
plot.right.front



#CENTER FRONT
# Mirror Average  2sides (e.g. left/right)
DATA = pcorr.ctrh.front
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.ctrh.front <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max, name = '# of Occurances') + #m
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8))+
  theme(legend.position="none")
plot.ctrh.front

#LEFT BACK
# Mirror Average  2sides (e.g. left/right)
DATA = pcorr.left.back
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.left.back <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("Proportion of Correct Responses") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") +  
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.left.back

#RIGHT BACK
DATA = pcorr.right.back
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.right.back <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.right.back

#CENTER BACK
# Mirror Average  2sides (e.g. left/right)
DATA = pcorr.ctrh.back
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.ctrh.back <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.ctrh.back


#VERTICAL
#UP FRONT
# Mirror Average  2sides (e.g. up/down)
DATA = pcorr.up.front
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.up.front <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("Proportion of Correct Responses") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.up.front

###############

#DOWN FRONT
# Mirror Average  2sides (e.g. up/down)
DATA = pcorr.down.front
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.down.front <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.down.front

###############

#CENTERV FRONT
# Mirror Average  2sides (e.g. up/down)
DATA = pcorr.ctrv.front

abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.ctrv.front <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.ctrv.front

###############
#UP BACK
# Mirror Average  2sides (e.g. up/down)
DATA = pcorr.up.back

abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.up.back <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("Proportion of Correct Responses") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
plot.up.back

###############

#DOWN BACK
# Mirror Average  2sides (e.g. up/down)
DATA = pcorr.down.back
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.down.back <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.down.back

###############

#CENTERV BACK
# Mirror Average  2sides (e.g. up/down)
DATA = pcorr.ctrv.back
abs.speakers <- as.data.frame(abs(DATA$speaker))
names(abs.speakers) <- c('abs.speaker')
DATA<- cbind(DATA, abs.speakers)
DATA2 <- aggregate(DATA$pcorr, list(DATA$abs.speaker,DATA$subj, DATA$group), mean)
names(DATA2) <- c("speaker","subj","group","pcorr")

plot.ctrv.back <- ggplot(DATA2, aes(x=speaker, y=pcorr, group=group, color=group,fill = group)) + # 
  geom_count(position = position_dodge(width=.8),fill = "white", stroke = stroke_size, pch = 21, alpha=transparent) +
  scale_size_area(max_size = circle_max) +
  # geom_point(position = position_dodge(width=.8), alpha=transparent) +
  theme_classic() +
  ylab("") +
  scale_x_discrete(limits=xlimits,labels = xlabels) +
  scale_y_continuous(breaks= seq(0,1,.2), limits=c(0,1)) +  
  ggtitle("") +
  xlab("") + 
  theme(axis.text.x=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=textsize, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=textsize, angle=90, colour='black')) +
  theme(text=element_text(family="Microsoft Sans Serif")) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, geom="crossbar", size=bar_size, width=0.8, position = position_dodge(width=.8)) +
  stat_summary(fun.y=mean, fun.ymin = mean, fun.ymax = mean, shape = 16, size=bar_circle_size, position = position_dodge(width=.8)) +
  theme(legend.position="none")
#plot.ctrv.back

###############





##########################
# save
##########################
hor.front.multipl <- grid.arrange(plot.left.front, plot.ctrh.front, plot.right.front, nrow=1, ncol=3)
hor.back.multipl <- grid.arrange(plot.left.back, plot.ctrh.back, plot.right.back, nrow=1, ncol=3)
#ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/violinplot_vertical_back.png", ver.back.multipl, dpi=300, width=3, height=4.5)
horizontal.multip <- grid.arrange(hor.front.multipl, hor.back.multipl)

#ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/Balloon_trans_hor6.png", horizontal.multip, width=10, height=12, dpi=300)

ver.front.multipl <- grid.arrange(plot.up.front, plot.ctrv.front, plot.down.front, nrow=1, ncol=3)
ver.back.multipl <- grid.arrange(plot.up.back, plot.ctrv.back, plot.down.back, nrow=1, ncol=3)
vertical.multip <- grid.arrange(ver.front.multipl, ver.back.multipl)

#ggsave("/Users/cerenbattal/Cerens_files/fMRI/Projects/Ceren_manuscripts/Figures/Behavioral_figures/Balloon_trans_ver6.png", vertical.multip, width=10, height=12, dpi=300)

# save all in 1 fig
All.thres.multip <- grid.arrange(horizontal.multip, vertical.multip)
ggsave("/Balloon_trans_sizebar3circle5_trans5_new_Legend.png", All.thres.multip, width=7.5, height=12, dpi=300)




