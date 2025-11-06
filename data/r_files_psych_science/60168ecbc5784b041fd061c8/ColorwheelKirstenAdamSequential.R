# Pre-processing and plotting of Colorwheel Kirsten Adam Follow-Up with Sequential Presentation

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()
library(circular)
source("circfun.R")

preProcess <- T
plotoption <- 1        # 0 = plot window, 1 = new x11 window, 2 = tiff

if (preProcess) {
  
  Data <- read.table("CW.Adam.all.dat")
  names(Data) <- c("id", "session", "setsize", "trial", "wheelrotation", 
                   "inpos1", "origcolor1", "prescolor1", "response1", "error1", "guess1", "noguess1", "rt1.1", "rt1.2", "rt1.3",
                   "inpos2", "origcolor2", "prescolor2", "response2", "error2", "guess2", "noguess2", "rt2.1", "rt2.2", "rt2.3",
                   "inpos3", "origcolor3", "prescolor3", "response3", "error3", "guess3", "noguess3", "rt3.1", "rt3.2", "rt3.3",
                   "inpos4", "origcolor4", "prescolor4", "response4", "error4", "guess4", "noguess4", "rt4.1", "rt4.2", "rt4.3",
                   "inpos5", "origcolor5", "prescolor5", "response5", "error5", "guess5", "noguess5", "rt5.1", "rt5.2", "rt5.3",
                   "inpos6", "origcolor6", "prescolor6", "response6", "error6", "guess6", "noguess6", "rt6.1", "rt6.2", "rt6.3")
  
  # data are listed in order of output (1 to 6); inpos = serial position of presentation.
  # origcolor = original color on the color wheel (degrees), prescolor = angle of color in rotated color wheel (radians), 
  # response = angle of response in rotated color wheel (radians), error = signed deviation of response from target color (degrees)
  # rt1 = time to select location, rt2 = time to move mouse far enough to make first color visible, rt3 = time to final color selection
  # noguess: left mouse button was used; guess: right mouse button was used
  
  ####### Preprocess data, turn into long format #################
  
  # compute output order as a 6-digit number, such that the digits code the order in which items (identified by their input position) are reported
  Data$outorder <- 100000*Data$inpos1  # 1st digit = 1st output position
  for (ss in 2:6) {
    Data$outorder[Data$setsize>=ss] <- Data$outorder[Data$setsize>=ss] + 10^(6-ss)*Data[Data$setsize>=ss, paste0("inpos", ss)]
  }
  
  # translate into long format: one row for each response
  
  ntrials <- dim(Data)[1]
  Dlong <- as.data.frame(matrix(NA, ntrials*6, 29))
  names(Dlong) <- c("id", "session", "trial", "setsize", "inpos", "outpos",
                    "color1", "color2", "color3", "color4", "color5", "color6",
                    "loc1", "loc2", "loc3", "loc4", "loc5", "loc6",
                    "targetcolor", "targetloc", "response", "error", "guess",
                    "keepposition", "lagCRP", "firstRecall", "rt1", "rt2", "rt3")
  idx <- 1
  for (trial in 1:ntrials) {
    setsize <- Data[trial, "setsize"]
    locations <- pi*seq(0, 360, 360/setsize)/180   # locations of the stimuli: clock-wise on a virtual circle -> code in radians
    inpos <- Data[trial, c("inpos1", "inpos2", "inpos3", "inpos4", "inpos5", "inpos6")]
    keepposition <- mean(inpos[1:setsize] == 1:setsize)  # true when input position = output position
    lagCRP <- c(NA, diff(as.numeric(inpos))) # lag conditional response probability 
    for (response in 1:setsize) {
      Dlong[idx, "id"] <- Data[trial, "id"]
      Dlong[idx, "session"] <- Data[trial, "session"]
      Dlong[idx, "trial"] <- trial
      Dlong[idx, "setsize"] <- setsize
      Dlong[idx, "inpos"] <- as.numeric(inpos[response])
      Dlong[idx, "outpos"] <- response
      for (pos in 1:setsize) {
        Dlong[idx, paste0("color", pos)] <- Data[trial, paste0("prescolor", which(inpos==pos))]  
        # memory set in order of presentation
      }
      Dlong[idx, 13:(12+setsize)] <- locations[1:setsize]   # locations in order of presentation
      Dlong[idx, "targetcolor"] <- Data[trial, paste0("prescolor", response)]
      Dlong[idx, "targetloc"] <- locations[as.numeric(inpos[response])]
      Dlong[idx, "response"] <- Data[trial, paste0("response", response)]
      Dlong[idx, "error"] <- Data[trial, paste0("error", response)]
      Dlong[idx, "guess"] <- Data[trial, paste0("guess", response)]
      Dlong[idx, "keepposition"] <- keepposition
      Dlong[idx, "lagCRP"] <- lagCRP[response]
      Dlong[idx, "firstRecall"] <- inpos[1]
      for (rt in 1:3) Dlong[idx, paste0("rt", rt)] <- Data[trial, paste0("rt", response, ".", rt)]
      idx <- idx + 1
    }
  }
  
  Dlong <- Dlong[1:(idx-1),] # shed superfluous lines
  DlongNA <- Dlong[is.na(Dlong$error),]
  Dlong <- Dlong[!is.na(Dlong$error), ]
  Ntrials <- dim(Dlong)[1]
  
  save(Dlong, file="ColorwheelAdamReplicationLong.RData")
  
} else load ("ColorwheelAdamReplicationLong.RData")


### Proportion of trials (per set size) with strict forward-order recall; P(first recall)
Dlong$strictForward <- Dlong$keepposition == 1
strictForward <- aggregate(strictForward ~ setsize, data=Dlong, FUN=mean)
freqFirstRecall <- table(Dlong$setsize, Dlong$firstRecall)
probFirstRecall <- freqFirstRecall
for (j in 1:5) {
  probFirstRecall[j,] <- probFirstRecall[j,]/sum(probFirstRecall[j,])
}

############ Plot Stuff ##############################################################

## Summary data for plots

ObsMeans <- array(NA, dim=c(6, 6, 6)) # setsize, inpos, outpos
ObsPropGuess <- array(NA, dim=c(6, 6, 6)) # setsize, inpos, outpos

for (setsize in 1:6) {
  for (inpos in 1:setsize) {
    for (outpos in 1:setsize) {
      subdat <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$outpos==outpos])
      ObsMeans[setsize, inpos, outpos] <- mean(subdat)
      ObsPropGuess[setsize, inpos, outpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$outpos==outpos], na.rm=T)
    }
  }
}

# for trials with strict forward recall only vs. remaining trials
ObsSPCFwd <- matrix(NA, 6, 6) 
ObsSPCNotFwd <- matrix(NA, 6, 6)
ObsPGuessFwd <- matrix(NA, 6, 6)
ObsPGuessNotFwd <- matrix(NA, 6, 6)
ObsOutPCNotFwd <- matrix(NA, 6, 6)
ObsPGuessOutNotFwd <- matrix(NA, 6, 6)
for (setsize in 1:6) {
  for (inpos in 1:setsize) {
    errFwd <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$strictForward == 1])   
    ObsSPCFwd[setsize, inpos] <- mean(errFwd)
    errNotFwd <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$strictForward == 0])   
    ObsSPCNotFwd[setsize, inpos] <- mean(errNotFwd)
    ObsPGuessFwd[setsize, inpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$strictForward==1], na.rm=T)
    ObsPGuessNotFwd[setsize, inpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$strictForward==0], na.rm=T)
  }
  for (outpos in 1:setsize) {
    errNotFwd <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$outpos==outpos & Dlong$strictForward == 0])
    ObsOutPCNotFwd[setsize, outpos] <- mean(errNotFwd)
    ObsPGuessOutNotFwd[setsize, outpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$outpos==outpos & Dlong$strictForward==0], na.rm=T)
  }
}

# SPC and Guess rate for strictly forward recalled trials only 

if (plotoption == 1) x11(width=8, height=6)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Seq.ErrorGuess.Forward.tiff"), units="in", width=8, height=6, res=500)
layout(matrix(1:2,1,2))

legendtext <- NULL
for (setsize in c(1:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize, "; ", 100*round(strictForward[strictForward$setsize==setsize, 2], 2), "%"))
  plot(1:6, ObsSPCFwd[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,90), 
       xlab="Input=Output Position", ylab="Error (deg)")
  if (setsize < 6) par(new=T)
}
title("Trials with strict forward recall")
legend(6,0, legendtext, xjust=1, yjust=0, pch=19+c(1:4,6))

legendtext <- NULL
for (setsize in c(1:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize, "; ", 100*round(strictForward[strictForward$setsize==setsize, 2], 2), "%"))
  plot(1:6, ObsPGuessFwd[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,0.8), 
       xlab="Input=Output Position", ylab="P(Guess)")
  if (setsize < 6) par(new=T)
}
legend(1,0.8, legendtext, xjust=0, yjust=1, pch=19+c(1:4,6))
title("Trials with strict forward recall")
if (plotoption == 2) dev.off()


# SPC and Guess rate for trials with other recall order: 

if (plotoption == 1) x11(width=8, height=9)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Seq.ErrorGuess.NotForward.tiff"), units="in", width=8, height=9, res=500)
layout(matrix(1:4,2,2, byrow=T))

legendtext <- NULL
for (setsize in c(2:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize, "; ", 100-100*round(strictForward[strictForward$setsize==setsize, 2], 2), "%"))
  plot(1:6, ObsSPCNotFwd[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,90), 
       xlab="Input Position", ylab="Error (deg)")
  if (setsize < 6) par(new=T)
}
title("Trials with not strict forward recall")
legend(6,90, legendtext, xjust=1, yjust=1, pch=19+c(2:4,6))

legendtext <- NULL
for (setsize in c(2:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize, "; ", 100-100*round(strictForward[strictForward$setsize==setsize, 2], 2), "%"))
  plot(1:6, ObsPGuessNotFwd[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,0.8), 
       xlab="Input Position", ylab="P(Guess)")
  if (setsize < 6) par(new=T)
}
title("Trials with not strict forward recall")
legend(1,0.8, legendtext, xjust=0, yjust=1, pch=19+c(2:4,6))

legendtext <- NULL
for (setsize in c(2:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize, "; ", 100-100*round(strictForward[strictForward$setsize==setsize, 2], 2), "%"))
  plot(1:6, ObsOutPCNotFwd[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,90), 
       xlab="Output Position", ylab="Error (deg)")
  if (setsize < 6) par(new=T)
}
title("Trials with not strict forward recall")
legend(1,90, legendtext, xjust=0, yjust=1, pch=19+c(2:4,6))

legendtext <- NULL
for (setsize in c(2:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize, "; ", 100-100*round(strictForward[strictForward$setsize==setsize, 2], 2), "%"))
  plot(1:6, ObsPGuessOutNotFwd[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,0.8), 
       xlab="Output Position", ylab="P(Guess)")
  if (setsize < 6) par(new=T)
}
title("Trials with not strict forward recall")
legend(1,0.8, legendtext, xjust=0, yjust=1, pch=19+c(2:4,6))
if (plotoption == 2) dev.off()



# Histograms by set size and output position

nbins <- 45
Nsubsamples <- 20  # for sampling from posterior predictives of errors
breakpoints <- seq(-pi, pi, length.out=nbins+1)
x <- breakpoints[1:nbins]+(breakpoints[2]-breakpoints[1])/2
ymax <- 500  
if (plotoption == 1) x11(width=8, height=8)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Seq.ErrorHist.tiff"), units="in", width=8, height=8, res=500)
layout(matrix(c(1,0,0,0,0,0,2,3,0,0,0,0,4,5,6,0,0,0,7,8,9,10,0,0,11,12,13,14,15,16), 5, 6, byrow=T))
par(mar=c(2,2,2,2))
for (setsize in c(1:4,6)) {
  for (outpos in 1:setsize) {
    Eobs <- deg2rad(Dlong$error[Dlong$setsize==setsize & Dlong$outpos==outpos])
    H <- hist(Eobs, breaks=breakpoints, plot=T, xlim=range(x), ylim=c(0,ymax), #xlab="Error (Rad)", 
              xlab="", ylab="",
              main=paste0("SS/Out=[", setsize, ", ", outpos, "]"))
  }
}
if (plotoption == 2) dev.off()


# Histogram for guess and no-guess responses, set size 6 only
if (plotoption == 1) x11(width=8, height=5)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Seq.ErrorHistGuessNoGuess.tiff"), units="in", width=8, height=5, res=500)
layout(matrix(1:2,1,2))
for (g in 0:1) {
  Eobs <- deg2rad(Dlong$error[Dlong$setsize==6 & Dlong$guess==g])
  H <- hist(Eobs, breaks=breakpoints, plot=T, xlim=range(x), ylim=c(0,1200), #xlab="Error (Rad)", 
            xlab="", ylab="",
            main=paste0("Guessing=", g))
}
if (plotoption == 2) dev.off()



### Patterns of output order, sorted by their frequency

OutX <- as.data.frame(matrix(NA, 7, 8))
names(OutX) <- c("Orders-2", "Freq-2", "Orders-3", "Freq-3", "Orders-4", "Freq-4", "Orders-6", "Freq-6")
for (ss in c(2:4,6)) {
  subdat <- subset(Data, setsize==ss)
  TT <- table(subdat$outorder)
  Orders <- as.numeric(names(TT))
  Freq <- as.numeric(TT)
  sortByFreq <- order(Freq, decreasing=T)
  sortedOrders <- Orders[sortByFreq]
  sortedFrq <- Freq[sortByFreq]
  mx <- min(7, length(Orders))
  OutX[1:mx,paste0("Orders-", ss)] <- sortedOrders[1:mx]
  OutX[1:mx,paste0("Freq-", ss)] <- sortedFrq[1:mx]
}
print(OutX)

