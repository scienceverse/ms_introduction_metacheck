# Pre-processing and plotting of Colorwheel Kirsten Adam Follow-Up with Simultaneous with fast presentation, set-size 6 only

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()
library(circular)
source("C:/Daten/R/toolbox/circfun.R")

# choose experiment
preProcess <- F
plotoption <- 3  # 0 = plot window, 1 = new x11 window, 2 = tiff, 3 = pdf


if (preProcess) {
  
  Data <- read.table("CW.AdamSimFast6.all.dat")
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
  for (ss in 6:6) {
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
  
  save(Dlong, file="ColorwheelAdamSimultaneousFast6Long.RData")
  
} else {
  load("ColorwheelAdamSimultaneousFast6Long.RData")
}

############ Plot Stuff ##############################################################

# Summary data for plots

ObsMeans <- array(NA, dim=c(6, 6)) # inpos, outpos
ObsPropGuess <- array(NA, dim=c(6, 6)) # inpos, outpos

# Errors as a function of set size, location ("input position"), and output position

for (inpos in 1:6) {
  for (outpos in 1:6) {
    subdat <- abs(Dlong$error[Dlong$inpos==inpos & Dlong$outpos==outpos])
    ObsMeans[inpos, outpos] <- mean(subdat)
    ObsPropGuess[inpos, outpos] <- mean(Dlong$guess[Dlong$inpos==inpos & Dlong$outpos==outpos], na.rm=T)
  }
}


ObsSPC <- matrix(NA, 1, 6)
ObsPGuess <- matrix(NA, 1, 6)
ObsOutPC <- matrix(NA, 1, 6)
ObsPGuessOut <- matrix(NA, 1, 6)
ObsRTOut <- matrix(NA, 1, 6)
for (inpos in 1:6) {
  err <- abs(Dlong$error[Dlong$inpos==inpos])
  ObsSPC[inpos] <- mean(err)
  ObsPGuess[inpos] <- mean(Dlong$guess[Dlong$inpos==inpos], na.rm=T)
}
for (outpos in 1:6) {
  err <- abs(Dlong$error[Dlong$outpos==outpos])
  ObsOutPC[outpos] <- mean(err)
  ObsPGuessOut[outpos] <- mean(Dlong$guess[Dlong$outpos==outpos], na.rm=T)
}


## Plot Error and Guess rate as function of output position 

if (plotoption == 1) x11(width=8, height=6)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Fast6.OutputPosition.tiff"), units="in", width=8, height=6, res=500)
if (plotoption == 3) pdf(paste0(getwd(), "/plots/Adam.Fast6.OutputPosition.pdf"), width=8, height=6)
layout(matrix(1:2,1,2))
plot(1:6, ObsOutPC, type="b", pch=19, xlim=c(1,6), ylim=c(0,90), 
       xlab="Output Position", ylab="Error (deg)")
title("Mean Error")
plot(1:6, ObsPGuessOut, type="b", pch=19, xlim=c(1,6), ylim=c(0,0.8), 
       xlab="Output Position", ylab="P(Guess)")
title("Guess Rate")
if (plotoption > 1) dev.off()


# Histograms of errors by output position
nbins <- 45
breakpoints <- seq(-pi, pi, length.out=nbins+1)
x <- breakpoints[1:nbins]+(breakpoints[2]-breakpoints[1])/2
ymax <- 2500  
if (plotoption == 1) x11(width=10, height=5)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Fast6.ErrorHist.tiff"), units="in", width=8, height=5, res=500)
if (plotoption == 3) pdf(paste0(getwd(), "/plots/Adam.Fast6.ErrorHist.pdf"), width=8, height=5)

layout(matrix(1:6, 1, 6, byrow=T))
par(mar=c(2,2,2,2))
for (outpos in 1:6) {
  Eobs <- deg2rad(Dlong$error[Dlong$outpos==outpos])
  H <- hist(Eobs, breaks=breakpoints, plot=T, xlim=range(x), ylim=c(0,ymax), #xlab="Error (Rad)", 
            xlab="", ylab="", main=paste0("Out=", outpos))
}
if (plotoption > 1) dev.off()

# Histogram for guess and no-guess responses
if (plotoption == 1) x11(width=8, height=5)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.Fast6.ErrorHistGuessNoGuess.tiff"), units="in", width=8, height=5, res=500)
if (plotoption == 3) pdf(paste0(getwd(), "/plots/Adam.Fast6.ErrorHistGuessNoGuess.pdf"), width=8, height=5)
layout(matrix(1:2,1,2))
for (g in 0:1) {
  Eobs <- deg2rad(Dlong$error[Dlong$guess==g])
  H <- hist(Eobs, breaks=breakpoints, plot=T, xlim=range(x), ylim=c(0,6000), #xlab="Error (Rad)", 
            xlab="", ylab="", main=paste0("Guessing=", g))
}
if (plotoption > 1) dev.off()



