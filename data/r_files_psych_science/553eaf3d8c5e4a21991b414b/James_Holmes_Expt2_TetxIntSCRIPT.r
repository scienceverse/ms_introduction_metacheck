rm(list=ls())
setwd("~/Desktop/")
tets=c(16413,3231,12505,20567,4816,24233,22672,44650,90077,21648,26590,29027,53526,107782,34285,1425,32853,26885)

intrs=c(1,2,3,0,2,3,2,1,7,0,3,2,2,1,0,1,0,4)

tdiff=c(6,5.4,4.7,3.1,4.8,1.8,1.8,1.8,3.6,2.7,2,3,0.6,1.2,1.6,2.2,1.2,5.7)

#pdf("Tetris_Score_Expt2.pdf")
par(mfrow=c(1,2))


plot(sqrt(tdiff[tdiff[]>-1]),intrs[tdiff[]>-1],pch=19,xlab="TetrisDifficulty-squarerootscale",ylab="TotalNumberofIntrusions"
,bty="l")

model2=(intrs[tdiff[]>-1]~sqrt(tdiff[tdiff[]>-1]))

res3=glm(model2,family="poisson")
summary(res3)

o=order(sqrt(tdiff[tdiff[]>-1]))
lines(sqrt(tdiff[tdiff[]>-1])[o],res3$fitted[o],lwd=2)

plot(sqrt(tets),intrs,pch=19,xlab="TetrisScore-squarerootscale",ylab="TotalNumberofIntrusions"
,bty="l")

model1=intrs~sqrt(tets)

res=glm(model1,family="poisson")
summary(res)

o=order(sqrt(tets))
lines(sqrt(tets)[o],res$fitted[o],lwd=2)

#res1=glm(intrs~sqrt(tets),family="quasi")
#summary(res1)

#o=order(sqrt(tets))
#lines(sqrt(tets)[o],res1$fitted[o],lwd=2,col="red")


#dev.off()
