# Plot results of Bayesian mixture model for all 3 experiments + the original color experiment of Adam et al. 2017

rm(list=ls(all=TRUE))
graphics.off()
library(Hmisc)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

Exp <- c(1,2,3,4,5)    # set of experiments to plot
ExpNames <- c("Exp. 1", "Exp. 2", "Exp. 3", "Exp. 4", "Adam et al (2017)")

plotoption <- 2        # 0 = plot window, 1 = new x11 window, 2 = tiff
centralT <- 3          # central tendency to be plotted: 1 = mean, 2 = median, 3 = mode

### Plot P(mem) for set size 6 as a function of output position

setsize <- 6
colors <- c("black", "red", "blue", "green", "orange")
Parnames <- c("P(mem)", "SD", "P(non-target)", "P(uniform)")

if (plotoption == 1) x11(width=8, height=8)
if (plotoption == 2) tiff(paste0(getwd(), "/plots/Adam.MixtureModelParms.JointPlot.tiff"), units="in", width=10, height=10, res=500)
layout(matrix(1:4,2,2, byrow=T))
par(mar=c(4,4,1,2))

for (par in 1:4) {
  
  idx <- 1
  for (exp in Exp) {
    filename <- paste0('Adam', exp, '.RData')
    load(filename)
    if (par == 1) post <- postPmem
    if (par == 2) post <- postSD
    if (par == 3) post <- postPNtarget
    if (par == 4) post <- 1-postPmem-postPNtarget
    if (par == 2) ylim <- c(0, 90) else ylim <- c(0,1)
    errbar(c(1:setsize)+((idx-2.5)/15), post[centralT,setsize,1:setsize], yplus=post[4,setsize,1:setsize], yminus=post[5,setsize,1:setsize], 
           xlim=c(0.5, 6.5), ylim=ylim, xlab="Output Position", ylab=Parnames[par], type="b", 
           cex=1.6, pch=21, errbar.col=colors[idx], bg=colors[idx])
    par(new=T)
    rm(postPmem)
    idx <- idx + 1
  }
  legend(0.5, (1-(par==1))*ylim[2], ExpNames, pch=21, pt.bg=colors, yjust=1-(par==1), cex=1.0) 
  
}
if (plotoption == 2) dev.off()

