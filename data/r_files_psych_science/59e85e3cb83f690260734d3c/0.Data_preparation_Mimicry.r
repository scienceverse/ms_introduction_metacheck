
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##    Preparations for data analysis
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(foreign)
library(plyr)

RRSM <- read.spss("Raw_data_Ratings_Social_Mimicry.sav", to.data.frame=TRUE)

## 1. For the SRM we need a unique dyad number

Dyad.Builder <- function(data) {

  x <- sort(unique(data$subject))
  y <- sort(unique(data$target))

  no.dyad <- 1
  data$Dyad <- 99

  for (i in 1:(length(x)-1)) {
    for (j in (i+1):length(y)) {
       #print(i)
       #print(j)
       data[data$subject==x[i] & data$target==y[j],]$Dyad <- no.dyad
       data[data$subject==y[j] & data$target==x[i],]$Dyad <- no.dyad
       no.dyad <- no.dyad + 1
    }
  }

  data

}

RRSM <- ddply(RRSM,.var="group",.fun=Dyad.Builder)

## 2. Compute a dummy coded gender variable (0 = male, 1 = female)

RRSM$gender <- ifelse(RRSM$sex=="female",1,0)

## 3. Select the "raw" data for the main analyses

RRSM <- RRSM[c("group","subject","target","Dyad",
               "social_mimicry_global_rater1","social_mimicry_global_rater2","social_mimicry_global_rater3",
               "liking_likeable_1","liking_gettoknow_1",
               "meta_liking_1_likeable","meta_liking_1_gettoknow",
               "liking_likeable_2","liking_gettoknow_2","liking_friend",
               "meta_liking_2_likeable","meta_liking_2_gettoknow","meta_liking_friend","gender")]
               
RRSM$liking_empty_1 <- NA
RRSM$meta_liking_empty <- NA

## 4.  Reshape data

RRSM.long1 <- reshape(RRSM[,c(1:4,5:7)],idvar=c("group","subject","target","Dyad"),varying=list(5:7),v.names="Mimicry",direction="long")
RRSM.long2 <- reshape(RRSM[,c(1:4,8:9,19)],idvar=c("group","subject","target","Dyad"),varying=list(5:7),v.names="Liking_T1",direction="long")
RRSM.long3 <- reshape(RRSM[,c(1:4,10:11,20)],idvar=c("group","subject","target","Dyad"),varying=list(5:7),v.names="MetaLiking_T1",direction="long")
RRSM.long4 <- reshape(RRSM[,c(1:4,12:14)],idvar=c("group","subject","target","Dyad"),varying=list(5:7),v.names="Liking_T2",direction="long")
RRSM.long5 <- reshape(RRSM[,c(1:4,15:17)],idvar=c("group","subject","target","Dyad"),varying=list(5:7),v.names="MetaLiking_T2",direction="long")
RRSM.long <- join_all(list(RRSM.long1,RRSM.long2,RRSM.long3,RRSM.long4,RRSM.long5),by=c("group","subject","target","Dyad","time"))
RRSM.long <- RRSM.long[order(RRSM.long$group,RRSM.long$subject,RRSM.long$target,RRSM.long$Dyad,RRSM.long$time),]

## 5. Merge with gender

sex <- ddply(RRSM,.(subject),.fun=function(x) { gender <- x$gender[1] ; out <- data.frame(gender=gender); });
RRSM.long <- merge(RRSM.long,sex,by=c("subject"))
RRSM.long <- RRSM.long[,c(2,1,3,4:11)]
RRSM.long <- RRSM.long[order(RRSM.long$group,RRSM.long$subject,RRSM.long$target,RRSM.long$Dyad,RRSM.long$time),]

## 6. Rename the variables

names(RRSM.long)[1:5] <- c("Group","Actor","Partner","Dyad","Occasion")
#"Occasions" are Raters

## 7. Save several subfiles for later analyses

write.table(RRSM.long[,c(1,2,3,4,5,6)],"RawData_Mimicry.dat",row.names=F,col.names=T)

write.table(RRSM.long[,c(1,2,3,4,5,9,10,11)],"RawData_LikingMetaLikingT2.dat",row.names=F,col.names=T)

RRSM.long <- subset(RRSM.long,RRSM.long$Occasion < 3)
write.table(RRSM.long[,c(1,2,3,4,5,7,8)],"RawData_LikingMetaLikingT1.dat",row.names=F,col.names=T)

##### ## Please use the 1st R code saved in 1.Data_Analysis_Mimicry_and_Effects.r




