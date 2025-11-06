# This script includes the code we used to analyse the data for the paper "First Impressions of Personality Traits from Body Shapes".
# This script is composed of 4 parts: 1) CA; 2) Stereotypes; 3) Regression.

# How to use: download the "Data" folder and run the script 
rm(list=ls())
setwd("~/Desktop/Data")

#source the functions for later use
source("functions.R")

# Female Data
CA.f.data<-read.table("ratingcount_fem.csv",header = T,row.names=1,sep = ",")
# Male Data
CA.m.data<-read.table("ratingcount_male.csv",header = T,row.names=1,sep = ",")

#------------------------------   1) CA  ------------------------------------------------
install.packages("ExPosition")
library("ExPosition")

CA.f.res <- epCA(CA.f.data,graphs = F)
CA.m.res <- epCA(CA.m.data,graphs = F)

#-------------------Plot the CA result------------------------------------
# Plot the traits, with the same trait factors in the same color
#-----red: Extravertion
#-----brown:Conscientiousness
#-----green:Agreeableness
#-----Purple:Openness
#-----blue:Neuroticism

ex <- "red"
ag <- "springgreen3"
con <- "violetred"
op <- "steelblue3"
ne <- "peru"

dev.off()
fem.plot <- fem.biplot(CA.f.res) 

dev.off()
male.plot <- male.biplot(CA.m.res)




#------------------------------   2) Stereotypes  ---------------------------------------
#set the criterion

shape <- read.csv("betas_male.csv")
trait_r <- read.csv("ratingcount_male.csv")
shape <- as.matrix(shape[ ,-1])
trait_r <- scale(as.matrix(trait_r[ ,-1]))


body <- list()
for (i in 1:30){
  body[[i]] <- which(trait_r[ ,i]>2.13,arr.ind = T)
}
names(body) <- colnames(trait_r)

values <- matrix(NA, 30, 10)
for(j in 1:30){
  if(class(body[[j]])=="integer" & length(body[[j]])==1){
    values[j, ] <- shape[body[[j]], ]
  } else if(class(body[[j]])=="integer" & length(body[[j]]) > 1){
    values[j, ] <- colMeans(shape[body[[j]], ]) 
  }
}

rownames(values) <- colnames(trait_r)
values <- values[-which(is.na(values[ ,1]),arr.ind = T), ]
#unique.body <- length(unique(values[ ,1]))

#save the beta values
write.table(values, "~/male_bodytypes.txt", sep = ",")


#------------------------------   3) Regression  ----------------------------------------
library("MASS")


#load the data and cut the rownames
shape <- read.csv("betas_male.csv", row.names = 1)
trait_r <- read.csv("ratingcount_male.csv", row.names = 1)
trait_r <- scale(trait_r)

s2tr<- mmlr(trait_r, shape, 1:70)
summary(s2tr$similarity)

#permute the X table to get the null distribution
N <- 1000
mean_sim <- numeric(N)
mean_error <- matrix(0, nrow = N, ncol = ncol(s2tr$E))
sim.list <- list()
err.list <- list()
for(i in 1:N){
  predict <- mmlr(trait_r,apply(shape,2,sample), 1:70)
  mean_sim[i] <- mean(predict$similarity)
  mean_error[i, ]<- colMeans(abs(predict$E))
  sim.list[[i]] <- predict$similarity
  err.list[[i]] <- predict$E 
}

# t.test of the predicted sim distri and the real sim distri
t.test(s2tr$similarity,unlist(sim.list))

#plot the mean similarity
#plot the similarity null distribution
real_sim <- mean(s2tr$similarity)
p <- t.test(unlist(sim.list), s2tr$similarity)[3]
p.sim <- printP(round(p$p.value,2))
hist(mean_sim, col = "grey",xlab = "Mean Similarity", main = "",
     ylim = c(0,400), xlim = c(min(mean_sim),0.4))
#abline(v = real_sim, col = "red")
#mtext(printP(round(p$p.value,2)), side = 3)
#p.sim <- sum(mean_sim > real_sim)/N
#cutoff.p <- sort(mean_sim,decreasing = T)[N * 0.05]
quantile(mean_sim,probs = c(0.05, 0.95))


#plot the mean error magnitude for each predictor
op <- par(mfrow=c(5,6),
          oma = c(2,4,1.15,1),
          mar = c(1.3,3,1.2,0.5) + 0.5,#c(bottom, left, top, right)
          mgp = c(1.6, 0.6, 0))#axis label locations of the inner fig

p.err <- numeric(ncol(s2tr$E))

for (j in 1:ncol(s2tr$E)){
  
    PT <- quantile(mean_error[ ,j],0.002) #Pt:percentile
  hist(mean_error[ ,j],xlab = "", ylab = "",
       xlim = c(0.4,1.2), ylim = c(1,300), main = "",
       col = "grey45", cex.main = 1.2,lty = 0)
  
  p.err[j] <- sum(colMeans(abs(s2tr$E))[j] > mean_error[ ,j])/N
  
  segments(colMeans(abs(s2tr$E))[j], 0, colMeans(abs(s2tr$E))[j], 250, 
           col= "red")
  segments(PT, 0, PT, 250, 
           col= "blue",lty = 2)
  
  mtext(text = colnames(trait_r)[j], line = 0, outer = F,cex = 1.2)
}
title(xlab = "",
      ylab = "Openness Neuroticism Conscientiousness Agreeableness Extraversion",
      outer = TRUE, line = 2., cex.lab = 1.8)  
mtext(text = "Male", line = -0.1, outer = T,cex = 1.3)
par(op)


