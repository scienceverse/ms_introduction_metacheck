# Pre-processing and plotting  of Colorwheel Kirsten Adam Follow-Up with Simultaneous but slower presentation / equally fast presentation

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()
library(circular)
source("circfun.R")

# choose experiment
SlowFast <- "Slow"
preProcess <- T
plotoption <- 1  # 0 = plot window, 1 = new x11 window, 2 = tiff


if (preProcess) {

  Data <- read.table(paste0("CW.AdamSim.", SlowFast, ".all.dat"))
  names(Data) <- c("id", "session", "setsize", "trial", "wheelrotation", 
                   "inpos1", "origcolor1", "prescolor1", "response1", "error1", "guess1", "noguess1", "rt1.1", "rt1.2", "rt1.3",
                   "inpos2", "origcolor2", "prescolor2", "response2", "error2", "guess2", "noguess2", "rt2.1", "rt2.2", "rt2.3",
                   "inpos3", "origcolor3", "prescolor3", "response3", "error3", "guess3", "noguess3", "rt3.1", "rt3.2", "rt3.3",
                   "inpos4", "origcolor4", "prescolor4", "response4", "error4", "guess4", "noguess4", "rt4.1", "rt4.2", "rt4.3",
                   "inpos5", "origcolor5", "prescolor5", "response5", "error5", "guess5", "noguess5", "rt5.1", "rt5.2", "rt5.3",
                   "inpos6", "origcolor6", "prescolor6", "response6", "error6", "guess6", "noguess6", "rt6.1", "rt6.2", "rt6.3")
  
  # data are listed in order of output (1 to 6); inpos = screen location on a virtual circle, starting from top, going clockwise.
  # origcolor = original color on the color wheel (degrees), prescolor = angle of color in rotated color wheel (radians), 
  # response = angle of response in rotated color wheel (radians), error = signed deviation of response from target color (degrees)
  # rt1 = time to select location, rt2 = time to move mouse far enough to make first color visible, rt3 = time to final color selection
  # noguess: left mouse button was used; guess: right mouse button was used
  
  # compute output order as a 6-digit number, such that the digits code the order in which items (identified by their location, from the top) are reported
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
                    "keepposition", "lagCPT", "firstRecall", "rt1", "rt2", "rt3")
  idx <- 1
  for (trial in 1:ntrials) {
    setsize <- Data[trial, "setsize"]
    locations <- pi*seq(0, 360, 360/setsize)/180
    inpos <- Data[trial, c("inpos1", "inpos2", "inpos3", "inpos4", "inpos5", "inpos6")]
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
      Dlong[idx, "firstRecall"] <- inpos[1]
      for (rt in 1:3) Dlong[idx, paste0("rt", rt)] <- Data[trial, paste0("rt", response, ".", rt)]
      idx <- idx + 1
    }
  }
  
  Dlong <- Dlong[1:(idx-1),] # shed superfluous lines
  DlongNA <- Dlong[is.na(Dlong$error),]
  Dlong <- Dlong[!is.na(Dlong$error), ]
  Ntrials <- dim(Dlong)[1]
  
  if (SlowFast == "Fast") save(Dlong, file="ColorwheelAdamSimultaneousFastLong.RData")
  if (SlowFast == "Slow") save(Dlong, file="ColorwheelAdamSimultaneousSlowLong.RData")
    
} else {
  if (SlowFast == "Fast") load("ColorwheelAdamSimultaneousFastLong.RData")
  if (SlowFast == "Slow") load("ColorwheelAdamSimultaneousSlowLong.RData")
}

############ Plot Stuff ##############################################################

# Summary data for plots

ObsMeans <- array(NA, dim=c(6, 6, 6)) # setsize, inpos, outpos
ObsPropGuess <- array(NA, dim=c(6, 6, 6)) # setsize, inpos, outpos

# Errors as a function of set size, location ("input position"), and output position
for (setsize in 1:6) {
  for (inpos in 1:setsize) {
    for (outpos in 1:setsize) {
      subdat <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$outpos==outpos])
      ObsMeans[setsize, inpos, outpos] <- mean(subdat)
      ObsPropGuess[setsize, inpos, outpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$inpos==inpos & Dlong$outpos==outpos], na.rm=T)
    }
  }
}

ObsSPC <- matrix(NA, 6, 6)
ObsPGuess <- matrix(NA, 6, 6)
ObsOutPC <- matrix(NA, 6, 6)
ObsPGuessOut <- matrix(NA, 6, 6)
ObsRTOut <- matrix(NA, 6, 6)
for (setsize in 1:6) {
  for (inpos in 1:setsize) {
    err <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$inpos==inpos])
    ObsSPC[setsize, inpos] <- mean(err)
    ObsPGuess[setsize, inpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$inpos==inpos], na.rm=T)
  }
  for (outpos in 1:setsize) {
    err <- abs(Dlong$error[Dlong$setsize==setsize & Dlong$outpos==outpos])
    ObsOutPC[setsize, outpos] <- mean(err)
    ObsPGuessOut[setsize, outpos] <- mean(Dlong$guess[Dlong$setsize==setsize & Dlong$outpos==outpos], na.rm=T)
  }
}


## Plot Error and Guess rate as function of output position and set size

if (plotoption == 1) x11(width=8, height=6)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.", SlowFast, "OutputPosition.tiff"), units="in", width=8, height=6, res=500)
layout(matrix(1:2,1,2))
legendtext <- NULL
for (setsize in c(1:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize))
  plot(1:6, ObsOutPC[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,90), 
       xlab="Output Position", ylab="Error (deg)")
  if (setsize < 6) par(new=T)
}
title("Mean Error")
legend(1,90, legendtext, xjust=0, yjust=1, pch=19+c(1:4,6))

legendtext <- NULL
for (setsize in c(1:4, 6)) {
  legendtext <- c(legendtext, paste0("SS=", setsize))
  plot(1:6, ObsPGuessOut[setsize,], type="b", pch=19+setsize, xlim=c(1,6), ylim=c(0,0.8), 
       xlab="Output Position", ylab="P(Guess)")
  if (setsize < 6) par(new=T)
}
title("Guess Rate")
legend(1,0.6, legendtext, xjust=0, yjust=1, pch=19+c(1:4,6))
if (plotoption==2) dev.off()


# Histograms of errors by set size and output position
nbins <- 45
breakpoints <- seq(-pi, pi, length.out=nbins+1)
x <- breakpoints[1:nbins]+(breakpoints[2]-breakpoints[1])/2
ymax <- 500  
if (plotoption == 1) x11(width=8, height=8)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.", SlowFast, "ErrorHist.tiff"), units="in", width=8, height=8, res=500)
layout(matrix(c(1,0,0,0,0,0,2,3,0,0,0,0,4,5,6,0,0,0,7,8,9,10,0,0,11,12,13,14,15,16), 5, 6, byrow=T))
par(mar=c(2,2,2,2))
for (setsize in c(1:4,6)) {
  for (outpos in 1:setsize) {
    Eobs <- deg2rad(Dlong$error[Dlong$setsize==setsize & Dlong$outpos==outpos])
    H <- hist(Eobs, breaks=breakpoints, plot=T, xlim=range(x), ylim=c(0,ymax), #xlab="Error (Rad)", 
              xlab="", ylab="", main=paste0("SS/Out=[", setsize, ", ", outpos, "]"))
  }
}
if (plotoption==2) dev.off()


# Histogram for guess and no-guess responses, set size 6 only
if (plotoption == 1) x11(width=8, height=5)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.", SlowFast, "ErrorHistGuessNoGuess.tiff"), units="in", width=8, height=5, res=500)
layout(matrix(1:2,1,2))
for (g in 0:1) {
  Eobs <- deg2rad(Dlong$error[Dlong$setsize==6 & Dlong$guess==g])
  H <- hist(Eobs, breaks=breakpoints, plot=T, xlim=range(x), ylim=c(0,1200), #xlab="Error (Rad)", 
            xlab="", ylab="", main=paste0("Guessing=", g))
}
if (plotoption==2) dev.off()



