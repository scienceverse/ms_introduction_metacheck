rm(list=ls())
setwd("~/Desktop/")
tets=c(38906, 54759,57775, 12699,6976,17927,46753,28389,1168,48798,
55906,85160,8316,30396,1597,29966,17605,4449,50792,25348,3133,12841,43898,12790,46194,47768)

intrs=c(1,1,1,3,6,0,5,1,3,3,0,2,6,1,6,1,2,0,5,8,3,6,2,2,1,1)

tdiff=c(5.6,0.7,4,4.7,2.4,1.8,0.7,2.9,5.8,0.9,0.9,1.5,10,-1,7,1.2,3.3,0.5,0.4,5.7,8,3.3,2.4,4.9,1.3,0)

#pdf("Tetris_Score_Expt1.pdf")
par(mfrow=c(1,2))


plot(sqrt(tdiff[tdiff[]>-1]),intrs[tdiff[]>-1],pch=19,xlab="Tetris Difficulty - square root scale", ylab="Total Number of Intrusions"
,bty="l")

model2=(intrs[tdiff[]>-1]~sqrt(tdiff[tdiff[]>-1]))

res3=glm(model2,family="poisson")
summary(res3)

o=order(sqrt(tdiff[tdiff[]>-1]))
lines(sqrt(tdiff[tdiff[]>-1])[o],res3$fitted[o],lwd=2)

plot(sqrt(tets),intrs,pch=19,xlab="Tetris Score - square root scale", ylab="Total Number of Intrusions"
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
