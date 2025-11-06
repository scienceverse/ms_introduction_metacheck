# CURVE GRAPHS 
# start from reading the logfiles

# all nedded packages in this scrips
library("ggplot2")
library("gridExtra")
library(extrafont)
library("quickpsy")

# go to where the logfiles are
setwd("/Logfiles")
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

# change response => horizontal:6=1; 5=0; FOR BACK
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


# find outliers from fitting and glmm
gof.vertical.outl <- read.table("/Thresholds-vertical-500B.txt", sep=",", header=T)
outliers.ver.fit <- subset(gof.vertical.outl, marked_bad==1)
gof.horizontal.outl <- read.table("/Thresholds-horizontal-500B.txt", sep=",", header=T)
outliers.hor.fit <- subset(gof.horizontal.outl, marked_bad==1)
all.thre.outl <- read.table("/All_Thresholds_outliersmarked-500B.txt", sep=",", header=T)
outliers.3sd <- subset(all.thre.outl, outlier==1)
all.outliers <- rbind(outliers.ver.fit[,1:4],outliers.hor.fit[,1:4],outliers.3sd[,1:4])
# for glmm convergence
# need to exclude also : Subj29, head=2, right arm
# need to exclude also : subj25, head=2, up arm
all.outliers <- rbind(all.outliers, c(29, "right", "SC", 2), c(25, "up", "SC", 2))
# reorder by arm
all.outliers <- all.outliers[order(all.outliers$arm),]
all.outliers

# go to where curves folder is
setwd("/Graphs/curves")

##########################
####### HORIZONTAL #######
##########################

# subset arm horizontal
data.ctrh.front <- subset(data.horizontal, arm=="ctrh"&pos==1)
data.ctrh.back <- subset(data.horizontal, arm=="ctrh"&pos==2)
data.left.front <- subset(data.horizontal, arm=="left"&pos==1)
data.left.back <- subset(data.horizontal, arm=="left"&pos==2)
data.right.front <- subset(data.horizontal, arm=="right"&pos==1)
data.right.back <- subset(data.horizontal, arm=="right"&pos==2)



# Central horizontal arm - subjects facing the speakers
# delete outliers of this condition -> find them in "all.outliers" dataframe
data.ctrh.front <- subset(data.ctrh.front, subj!=4&subj!=14&subj!=34)

# aggregate to plot points
data.ctrh.front.prop <- aggregate(data.ctrh.front$response, list(data.ctrh.front$side, data.ctrh.front$group), mean)
names(data.ctrh.front.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.ctrh.front <- quickpsy(data.ctrh.front, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.ctrh.front$curves

cn.ctrh.front <- ggplot(data.ctrh.front.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.ctrh2.front.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=F) +
  #stat_function(fun=pnorm, args=list(mean=0, sd=1.2)) +
  scale_x_continuous(breaks = c(-5:5), labels = c("L5","L4","L3","L2","L1","C","R1","R2","R3","R4","R5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("Front\n Center") +
  xlab("")+
  theme(plot.title = element_text(hjust=0.5)) +
 # theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  #theme(axis.title.y=element_blank()) +
  #theme(axis.title.x=element_blank()) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.ctrh.front

# save picture
ggsave("CN_ctrh_front2.pdf", cn.ctrh.front, width=5, height=6, dpi=300)


# Left horizontal arm - subjects facing the speakers
# delete outliers of this condition
data.left.front <- subset(data.left.front, subj!=13)

# aggregate to plot points
data.left.front.prop <- aggregate(data.left.front$response, list(data.left.front$side, data.left.front$group), mean)
names(data.left.front.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.left.front <- quickpsy(data.left.front, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.left.front$curves

cn.left.front <- ggplot(data.left.front.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.left2.front.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = c(-5:5),labels = c("L5","L4","L3","L2","L1","C","R1","R2","R3","R4","R5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Left") +
  xlab("")+
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("proportion of 'right' responses") 
cn.left.front

ggsave("CN_left_front.pdf", cn.left.front, width=5, height=6, dpi=300)



# Right horizontal arm - subjects facing the speakers
# delete outliers of this condition
data.right.front <- subset(data.right.front, subj!=13)

# aggregate to plot points
data.right.front.prop <- aggregate(data.right.front$response, list(data.right.front$side, data.right.front$group), mean)
names(data.right.front.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.right.front <- quickpsy(data.right.front, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.right.front$curves

cn.right.front <- ggplot(data.right.front.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.right2.front.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = c(-5:5),labels = c("L5","L4","L3","L2","L1","C","R1","R2","R3","R4","R5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Right") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.right.front

ggsave("CN_right_front.pdf", cn.right.front, width=5, height=6, dpi=300)

# have a look at multiplot
multiplot.hor.front <- grid.arrange(cn.left.front, cn.ctrh.front, cn.right.front, nrow=1, ncol=3)
ggsave("horizontal_front.pdf", multiplot.hor.front, width=15, height=6, dpi=300)




# Central horizontal arm - subjects with their back to the speakers
# delete outliers of this condition
data.ctrh.back <- subset(data.ctrh.back, subj!=6&subj!=7&subj!=30)

# aggregate
data.ctrh.back.prop <- aggregate(data.ctrh.back$response, list(data.ctrh.back$side, data.ctrh.back$group), mean)
names(data.ctrh.back.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.ctrh.back <- quickpsy(data.ctrh.back, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=10)
data.for.cum.norm <- my.model.ctrh.back$curves

cn.ctrh.back <- ggplot(data.ctrh.back.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.ctrh.back.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = -5:5,labels = c("L5","L4","L3","L2","L1","C","R1","R2","R3","R4","R5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("Back\n Center") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.ctrh.back

ggsave("CN_ctrh_back.pdf", cn.ctrh.back, width=5, height=6, dpi=300)




#### NOTE FOR BACK HORIZONTAL CONDITION ####

# in the horizontal back condition left arm is right area // right arm is left area of the subject
# Corrections were made here because  the goodness-of-fit and outliers were calculated with this way.
# only plotting needs reversing left back to right back. 
#Similar reversing also applied before running any stats.

############################################
# Left horizontal arm - subjects with their back to the speakers
# delete outliers of this condition
data.left.back <- subset(data.left.back, subj!=8&subj!=16)

data.left.back.prop <- aggregate(data.left.back$response, list(data.left.back$side, data.left.back$group), mean)
names(data.left.back.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.left.back <- quickpsy(data.left.back, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.left.back$curves

# #reverse left back to right back
# by changing the name in saving file
cn.right.back <- ggplot(data.left.back.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.left2.back.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = -5:5,labels = c("L5","L4","L3","L2","L1","C","R1","R2","R3","R4","R5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Right") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("proportion of 'right' responses") 
cn.right.back


ggsave("CN_right_back.pdf", cn.right.back, width=5, height=6, dpi=300)



# Right horizontal arm - subjects with their back to the speakers
# delete outliers of this condition
data.right.back <- subset(data.right.back, subj!=23&subj!=29&subj!=30)

# aggregate
data.right.back.prop <- aggregate(data.right.back$response, list(data.right.back$side, data.right.back$group), mean)
names(data.right.back.prop) <- c("side", "group", "prop")



# fitting in quickpsy to get a cumulative normal curve
my.model.right.back <- quickpsy(data.right.back, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.right.back$curves

# #reverse left back to right back
# by changing the name in saving file
cn.left.back <- ggplot(data.right.back.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.right2.back.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = -5:5,labels = c("L5","L4","L3","L2","L1","C","R1","R2","R3","R4","R5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Left") +
  theme(plot.title = element_text(hjust=0.5)) +
 # theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.left.back

ggsave("CN_left_back.pdf", cn.left.back, width=5, height=6, dpi=300)

# have a look at the multiplot
multiplot.hor.back <- grid.arrange(cn.left.back, cn.ctrh.back, cn.right.back, nrow=1, ncol=3)
ggsave("horizontal_back.pdf", multiplot.hor.back, width=15, height=6, dpi=300)


#######################
###### VERTICAL #######
#######################

# subset arm vertical
data.ctrv.front <- subset(data.vertical, arm=="ctrv"&pos==1)
data.ctrv.back <- subset(data.vertical, arm=="ctrv"&pos==2)
data.up.front <- subset(data.vertical, arm=="up"&pos==1)
data.up.back <- subset(data.vertical, arm=="up"&pos==2)
data.down.front <- subset(data.vertical, arm=="down"&pos==1)
data.down.back <- subset(data.vertical, arm=="down"&pos==2)


# Central vertical arm - subjects facing the speakers
# delete outliers of this condition
data.ctrv.front <- subset(data.ctrv.front, subj!=10&subj!=31&subj!=32)

data.ctrv.front.prop <- aggregate(data.ctrv.front$response, list(data.ctrv.front$side, data.ctrv.front$group), mean)
names(data.ctrv.front.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.ctrv.front <- quickpsy(data.ctrv.front, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.ctrv.front$curves

cn.ctrv.front <- ggplot(data.ctrv.front.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.ctrv2.front.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=F) +
  #stat_function(fun=pnorm, args=list(mean=0, sd=1.2)) +
  scale_x_continuous(breaks = c(-5:5), labels = c("D5","D4","D3","D2","D1","C","U1","U2","U3","U4","U5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("Front\n Center") +
  xlab("")+
  theme(plot.title = element_text(hjust=0.5)) +
 # theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.ctrv.front

# save picture
ggsave("CN_ctrv_front.pdf", cn.ctrv.front, width=5, height=6, dpi=300)


# Upper vertical arm - subjects facing the speakers
# delete outliers of this condition
data.up.front <- subset(data.up.front, subj!=13&subj!=14)

data.up.front.prop <- aggregate(data.up.front$response, list(data.up.front$side, data.up.front$group), mean)
names(data.up.front.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.up.front <- quickpsy(data.up.front, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.up2.front$curves

cn.up.front <- ggplot(data.up.front.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.up2.front.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = c(-5:5),labels = c("D5","D4","D3","D2","D1","C","U1","U2","U3","U4","U5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Up") +
  xlab("")+
  theme(plot.title = element_text(hjust=0.5)) +
 # theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("proportion of 'up' responses") 
cn.up.front

# save picture
ggsave("CN_up_front.pdf", cn.up.front, width=5, height=6, dpi=300)



# Lower vertical arm - subjects facing the speakers
# delete outliers of this condition
data.down.front <- subset(data.down.front, subj!=26&subj!=32)

data.down.front.prop <- aggregate(data.down.front$response, list(data.down.front$side, data.down.front$group), mean)
names(data.down.front.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.down.front <- quickpsy(data.down.front, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.down.front$curves

cn.down.front <- ggplot(data.down.front.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.down2.front.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=F) +
  scale_x_continuous(breaks = c(-5:5),labels = c("D5","D4","D3","D2","D1","C","U1","U2","U3","U4","U5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Down") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.down.front

# save picture
ggsave("CN_down_front.pdf", cn.down.front, width=5, height=6, dpi=300)


# have a look at the multiplot
multiplot.ver.front <- grid.arrange(cn.up.front, cn.ctrv.front, cn.down.front, nrow=1, ncol=3)
ggsave("vertical_front.pdf", multiplot.ver.front, width=15, height=6, dpi=300)


# Central vertical arm - subjects with their back to the speakers
# delete outliers of this condition
data.ctrv.back <- subset(data.ctrv.back, subj!=9&subj!=11&subj!=13&subj!=27)

# aggregate
data.ctrv.back.prop <- aggregate(data.ctrv.back$response, list(data.ctrv.back$side, data.ctrv.back$group), mean)
names(data.ctrv.back.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.ctrv.front <- quickpsy(data.ctrv.back, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.ctrv.front$curves

cn.ctrv.back <- ggplot(data.ctrv.back.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.ctrv2.back.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = -5:5,labels = c("D5","D4","D3","D2","D1","C","U1","U2","U3","U4","U5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("Back\n Center") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.ctrv.back

# save
ggsave("CN_ctrv_back.pdf", cn.ctrv.back, width=5, height=6, dpi=300)



# Upper vertical arm - subjects with their back to the speakers
# delete outliers of this condition
data.up.back <- subset(data.up.back, subj!=3&subj!=8&subj!=10&subj!=15&subj!=16&subj!=22&subj!=25&subj!=26&subj!=27)

# aggregate
data.up.back.prop <- aggregate(data.up.back$response, list(data.up.back$side, data.up.back$group), mean)
names(data.up.back.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.up.front <- quickpsy(data.up.back, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.up.front$curves

cn.up.back <- ggplot(data.up.back.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.up2.back.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=T) +
  scale_x_continuous(breaks = -5:5,labels = c("D5","D4","D3","D2","D1","C","U1","U2","U3","U4","U5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Up") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
#  theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("proportion of 'up' responses") 
cn.up.back

# save
ggsave("CN_up_back.pdf", cn.up.back, width=5, height=6, dpi=300)



# Down vertical arm - subjects with their back to the speakers
# delete outliers of this condition
data.down.back <- subset(data.down.back, subj!=13&subj!=21&subj!=32)

#aggregate
data.down.back.prop <- aggregate(data.down.back$response, list(data.down.back$side, data.down.back$group), mean)
names(data.down.back.prop) <- c("side", "group", "prop")

# fitting in quickpsy to get a cumulative normal curve
my.model.down.front <- quickpsy(data.down.back, k=response, x=side, fun=cum_normal_fun, grouping=.(group), prob=0.5, B=1)
data.for.cum.norm <- my.model.down.front$curves

cn.down.back <- ggplot(data.down.back.prop, aes(x=side, y=prop, group=group, color=group)) + 
  theme_classic() +
  geom_point(size=2) +
  geom_line(data=data.for.cum.norm, aes(x=x, y=y, group=group), size=1.2) +
  #stat_smooth(data=data.down2.back.plot, aes(x=side, y=correct, group=group, color=group), method="glm", method.args=list(family="binomial"), size=1.2, se=F) +
  scale_x_continuous(breaks = -5:5,labels = c("D5","D4","D3","D2","D1","C","U1","U2","U3","U4","U5")) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  ggtitle("\n Down") +
  xlab("") +
  theme(plot.title = element_text(hjust=0.5)) +
 # theme(text=element_text(family="Microsoft Sans Serif")) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=16, angle=0, colour='black')) +
  theme(axis.text.y=element_text(size=16, angle=0, colour='black')) +
  theme(axis.title.y=element_text(size=15, angle=90, colour='black')) +
  theme(axis.title.x=element_text(size=15, angle=0, colour='black')) +
  scale_color_manual(values=c("dodgerblue3","firebrick2")) +
  ylab("") 
cn.down.back

# save
ggsave("CN_down_back.pdf", cn.down.back, width=5, height=6, dpi=300)

# multiplot
multiplot.ver.back <- grid.arrange(cn.up.back, cn.ctrv.back, cn.down.back, nrow=1, ncol=3)
ggsave("vertical_back.pdf", multiplot.ver.back, width=15, height=6, dpi=300)
