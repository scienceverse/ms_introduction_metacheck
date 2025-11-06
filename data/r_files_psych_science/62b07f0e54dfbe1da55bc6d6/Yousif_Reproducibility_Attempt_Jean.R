#R version 3.6.3 (2020-02-29)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Sierra 10.12.6
sessionInfo()

### STUDY 1 YOUSIF - BROWN PROJECT

library(dplyr)
library(car)
library(psych)
DF<-yousif.study2

#Load data (change directory to match your own pathway to the file)
DF <- read.csv("~/Desktop/Study 1 Yousif.csv", sep=";")
describe(DF) #
str(DF) # l. 10 and 11 are for helping understand the data
table(DF$id) # Some real discrepancies between different participants (a lot of 84 trials, but take participant AAUXX : s/he only answered to 53 trials )
levels(DF$id2) #95 participants in total. Given what is written in the paper, I would have expecter 97 participants or 100 participants, but not 95.
levels(DF$id)#Note that id & id2 are discrepant for unknown reasons.

#First Ancova on accuracy
#I'll add some columns first
#make an accurate long format for the 2 way repeated measure ancova
DF$condition<- c()
  for (i in 1:nrow(DF)) {
          if( DF[i, 6] ==0 & DF[i, 7] == 0) { DF[i, 12] = "a0t0"}
       else if(DF[i, 6] ==0 & DF[i, 7] == 10) { DF[i, 12] = "a0t10"}
      else if(DF[i, 6] ==0 & DF[i, 7] == 20) { DF[i, 12] = "a0t20"}
      else if(DF[i, 6] ==0 & DF[i, 7] == 30) { DF[i, 12] = "a0t30"}
      else if(DF[i, 6] ==10 & DF[i, 7] == 0) { DF[i, 12] = "a10t0"}
      else if(DF[i, 6] ==20 & DF[i, 7] == 0) { DF[i, 12] = "a20t0"}
      else {DF[i, 12] = "a30t0"}
  }

interestModel<-lm(DF$RT~1)
analysis <- Anova(interestModel, idata = DF, idesign = ~aa * ta)    
analysis #  the output reports a df of 7918, hence I suppose I failed to run this within anova
   ####Trying other methods

#####this should be useful for doing ez anova
DF.mean <- aggregate(DF$RT,
                         by = list(DF$id, DF$aa,
                                   DF$ta, DF$V12),
                         FUN = 'mean')



colnames(DF.mean) <- c("id","aa","ta","cond", "RT")

DF.mean <- DF.mean[order(DF.mean$id), ]
head(DF.mean)

RT.aov <- with(DF.mean,
                   aov(RT ~ aa * ta +
                         Error(id / (aa * ta)))
)

####... There is a warning indicating missing value (singular model)
summary(RT.aov) #But it works. But I highly doubt that is what the authors wanted to do.
library(ez)

###Trying ezANOVA
ezANOVA(data = DF,wid = id, dv = RT, within = c(aa, ta))
#Same here... Missing data reported... For unknown reasons though :
temp = as.data.frame(table(DF.mean$id))

#Returns only the subjects that have less than number of levels (Missing data)
temp[temp$Freq<7,] ### No missing data
### After several hours I decide to stop trying to reproduce data from Study 1.
### To begin with: there are only 96 participants here (instead of the 97 expected participants, deduced from the reported degrees of freedom and read from the method section)
### The way the data is organized does not allow reproduction in R (nor in JASP / JAMOVI) of the results without sweating a bit.









