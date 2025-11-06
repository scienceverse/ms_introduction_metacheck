# fitting proportion of right responses with quickpsy
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
data$group <- ifelse(data$subj<=17, c('EB'), c('SC'))

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

# divide to fit (this will be a little quicker than fitting the whole data in one go, but completely equivalent)

data.horizontal.front <- subset(data.planes, pos==1&axis=="hor")
data.horizontal.back <- subset(data.planes, pos==2&axis=="hor")
data.vertical.front <- subset(data.planes, pos==1&axis=="ver")
data.vertical.back <- subset(data.planes, pos==2&axis=="ver")

library("quickpsy")

setwd("")

#run bootstaping & save goodness of fit & threshold values while marking the bad ones

#horfront
cn.fit.horfront <- quickpsy(data.horizontal.front, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.75, B=500)
write.table(cn.fit.horfront$deviance, file="Goodness_of_fit-horizontal_front-500B.txt", sep=",", row.names=F)

cn.fit.horfront$thresholds$marked_bad <- ifelse(cn.fit.horfront$deviance$p < 0.05,1,0)
write.table(cn.fit.horfront$thresholds, file="Thresholds-horizontal_front-500B.txt", sep=",", row.names=F)
bad.horfront <- which(cn.fit.horfront$deviance$p < 0.05)

#horback
cn.fit.horback <- quickpsy(data.horizontal.back, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.75, B=500)
write.table(cn.fit.horback$deviance, file="Goodness_of_fit-horizontal_back-500B.txt", sep=",", row.names=F)

cn.fit.horback$thresholds$marked_bad <- ifelse(cn.fit.horback$deviance$p < 0.05,1,0)
write.table(cn.fit.horback$thresholds, file="Thresholds-horizontal_back-500B.txt", sep=",", row.names=F)
bad.horback <-which(cn.fit.horback$deviance$p < 0.05)

#verfront
cn.fit.verfront <- quickpsy(data.vertical.front, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.75, B=500)
write.table(cn.fit.verfront$deviance, file="Goodness_of_fit-vertical_front-500B.txt", sep=",", row.names=F)

cn.fit.verfront$thresholds$marked_bad <- ifelse(cn.fit.verfront$deviance$p < 0.05,1,0)
write.table(cn.fit.verfront$thresholds, file="Thresholds-vertical_front-500B.txt", sep=",", row.names=F)
bad.verfront <-which(cn.fit.verfront$deviance$p < 0.05)

#verback
cn.fit.verback <- quickpsy(data.vertical.back, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.75, B=500)
write.table(cn.fit.verback$deviance, file="Goodness_of_fit-vertical_back-500B.txt", sep=",", row.names=F)

cn.fit.verback$thresholds$marked_bad <- ifelse(cn.fit.verback$deviance$p < 0.05,1,0)
write.table(cn.fit.verback$thresholds, file="Thresholds-vertical_back-500B.txt", sep=",", row.names=F)
bad.verback <-which(cn.fit.verback$deviance$p < 0.05)

# sum up all the bad curves
tot_outliers <-length(c(bad.horback,bad.horfront,bad.verback,bad.verfront))
tot_curves <- 408
proportion_badcurves <- (tot_outliers/tot_curves)*100

# to get the thresholds at 50% in order to make the subtraction
cn.fit.horfront50 <- quickpsy(data.horizontal.front, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.5, B=1)
write.table(cn.fit.horfront50$thresholds, file="Thresholds-horizontal_front50-500B.txt", sep=",", row.names=F)
cn.fit.horback50 <- quickpsy(data.horizontal.back, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.5, B=1)
write.table(cn.fit.horback50$thresholds, file="Thresholds-horizontal_back50-500B.txt", sep=",", row.names=F)
cn.fit.verfront50 <- quickpsy(data.vertical.front, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.5, B=1)
write.table(cn.fit.verfront50$thresholds, file="Thresholds-vertical_front50-500B.txt", sep=",", row.names=F)
cn.fit.verback50 <- quickpsy(data.vertical.back, k=response, x=side, fun=cum_normal_fun, grouping=.(subj,arm,group,pos), prob=0.5, B=1)
write.table(cn.fit.verback50$thresholds, file="Thresholds-vertical_back50-500B.txt", sep=",", row.names=F)

# substract 75% -50% to get threshold
#hor
a1<- read.table(file="Thresholds-horizontal_front50-500B.txt", sep=",", header = TRUE)
a2<- read.table(file="Thresholds-horizontal_back50-500B.txt", sep=",", header = TRUE)
a<- rbind(a1, a2)

thre1<- read.table(file="Thresholds-horizontal_front-500B.txt", sep=",", header = TRUE)
thre2<- read.table(file="Thresholds-horizontal_back-500B.txt", sep=",", header = TRUE)
thre.hor <-rbind(thre1,thre2)

thre50 <-a$thre
thre.hor <- cbind(thre.hor, thre50)
names(thre.hor)[5] <-c('thre75')
#thre <-abs(thre.hor$thre75 - thre.hor$thre50) # do we need abs here?
thre.hor <-cbind(thre.hor, thre)
write.table(thre.hor, file="Thresholds-horizontal-500B.txt", sep=",", row.names=F)

#ver
a1<- read.table(file="Thresholds-vertical_front50-500B.txt", sep=",", header = TRUE)
a2<- read.table(file="Thresholds-vertical_back50-500B.txt", sep=",", header = TRUE)
a<- rbind(a1, a2)

thre1<- read.table(file="Thresholds-vertical_front-500B.txt", sep=",", header = TRUE)
thre2<- read.table(file="Thresholds-vertical_back-500B.txt", sep=",", header = TRUE)
thre.ver <-rbind(thre1,thre2)

thre50 <-a$thre
thre.ver <- cbind(thre.ver, thre50)
names(thre.ver)[5] <-c('thre75')
#thre <-abs(thre.ver$thre75 - thre.ver$thre50) # do we need abs here?
thre.ver <-cbind(thre.ver, thre)
write.table(thre.ver, file="Thresholds-vertical-500B.txt", sep=",", row.names=F)


