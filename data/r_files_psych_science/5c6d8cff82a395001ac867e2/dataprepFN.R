dataprepFN <- function(data, randfix) {
  library(gtools)
  
  
  #load("et2_data_raw.RData") #loads a "data" structure
  #data<-data[data$subject==10,] #use this for individual subject analysis
  
  #basic features of the data
  drois<-max(data$roi) #how many rois?
  temp<-data[data$fixnum==1,]
  dtrials<-nrow(temp) #how many trials total?
  dmaxfix<-max(data$fixnum)
  #dmaxfix<-max(temp$revfixnum) #maximum number of fixations (alternatively you can just set this manually)
  rm(temp)
  
  #create version of dataset with only middle fixations (we use this later for drawing random middle fixations)
  #edit 11.14.16: added data$fixdur >= 0 to avoid infinite fixation sequences
  if (length(data$midfix)>0) {
    temp <- data[data$midfix == 1 & data$fixdur >= 10,]
  }
  if (length(data$midfix) == 0) {
    temp<-data[(data$fixnum>1 & data$revfixnum>1 & data$fixdur >= 10),]
  }
  
  #prepare data matrix
  data_mat<-matrix(0,nrow=dtrials,ncol=(2+drois+dmaxfix*2)) #choice,rt,roi values,rois,fixation lengths
  
  #specify whether you want to add random fixations when the data runs out
  randfix<-1 
  
  #two item case
  if (drois==2){
    n=0
    for (i in 1:nrow(data)){
      if (data$fixnum[i]==1){
        n<-n+1
        data_mat[n,1]<-data$choice[i] #1=left choice, 0=right choice
        data_mat[n,2]<-data$rt[i] #in milliseconds
        data_mat[n,3]<-data$leftval[i] #left value
        data_mat[n,4]<-data$rightval[i] #right value
        data_mat[n,5]<-data$roi[i] #1=left, 2=right
        data_mat[n,(5+dmaxfix)]<-data$fixdur[i] #fixation duration in ms
      }
      if (data$fixnum[i]>1){
        data_mat[n,(4+data$fixnum[i])]<-data$roi[i]
        data_mat[n,(4+dmaxfix+data$fixnum[i])]<-data$fixdur[i]
      }
    }
  }
  
  #four item case
  if (drois==4){
    n=0
    for (i in 1:nrow(data)){
      if (data$fixnum[i]==1){
        n<-n+1
        data_mat[n,1]<-data$choice[i]
        data_mat[n,2]<-data$rt[i]
        data_mat[n,3]<-data$upleftval[i]
        data_mat[n,4]<-data$uprightval[i]
        data_mat[n,5]<-data$downleftval[i]
        data_mat[n,6]<-data$downrightval[i]
        data_mat[n,7]<-data$roi[i]
        data_mat[n,(7+dmaxfix)]<-data$fixdur[i]
      }
      if (data$fixnum[i]>1){
        data_mat[n,(6+data$fixnum[i])]<-data$roi[i]
        data_mat[n,(6+dmaxfix+data$fixnum[i])]<-data$fixdur[i]
      }
    }
  }
  
  #fill in missing fixation locations and durations for 2 items
  if (randfix==1 & drois==2){
    ####fill in missing fixation locations
    first_rois<-data_mat[,5]
    dim(first_rois)<-c(nrow(data_mat),1)
    second_rois<-3-data_mat[,5]
    dim(second_rois)<-c(nrow(data_mat),1)
    
    #cover the case of even nmaxfix
    if (even(dmaxfix)){
      roi_mat<-rep(c(first_rois,second_rois),floor(dmaxfix/2))
      dim(roi_mat)<-c(nrow(data_mat),(dmaxfix))
      data_mat[,5:(4+dmaxfix)]<-roi_mat
    }
    
    #cover the case of odd nmaxfix
    if (odd(dmaxfix)){
      roi_mat<-rep(c(second_rois,first_rois),floor(dmaxfix/2))
      dim(roi_mat)<-c(nrow(data_mat),(dmaxfix-1))
      data_mat[,6:(4+dmaxfix)]<-roi_mat
    }	
    
    ####fill in missing fixation durations
    pfixmat<-sample(temp$fixdur,dmaxfix*nrow(data_mat),replace=TRUE)
    dim(pfixmat)<-c(nrow(data_mat),dmaxfix)
    
    #matrix with 1s for missing fixations and 0s for non-missing fixations
    fix_mat<-ceiling((1-(data_mat[1:nrow(data_mat),(5+dmaxfix):ncol(data_mat)]))/1000000)
    data_mat[1:nrow(data_mat),(5+dmaxfix):ncol(data_mat)]<-data_mat[1:nrow(data_mat),(5+dmaxfix):ncol(data_mat)]+fix_mat*pfixmat
  }
  
  #fill in missing fixation locations and durations for 4 items
  if (randfix==1 & drois==4){
    ####fill in missing fixation locations
    prois<-c(1,2,3,4) #possible rois
    for (i in 1:nrow(data_mat)){
      for (j in 7:(6+dmaxfix)){
        if (data_mat[i,j]==0) { 
          if (length(data_mat[i,j-1])==0) {
            data_mat[i,j] <- sample(prois,1)
          }
          else if (length(data_mat[i,j-1])>0) {
            data_mat[i,j] <- sample(prois[prois!=data_mat[i,j-1]],1)
          }
          #need to check that this works
        }
      }
    }
    
    ####fill in missing fixation durations
    pfixmat<-sample(temp$fixdur,dmaxfix*nrow(data_mat),replace=TRUE)
    dim(pfixmat)<-c(nrow(data_mat),dmaxfix)
    
    #matrix with 1s for missing fixations and 0s for non-missing fixations
    fix_mat<-ceiling((1-(data_mat[1:nrow(data_mat),(7+dmaxfix):ncol(data_mat)]))/1000000)
    data_mat[1:nrow(data_mat),(7+dmaxfix):ncol(data_mat)]<-data_mat[1:nrow(data_mat),(7+dmaxfix):ncol(data_mat)]+fix_mat*pfixmat
  }
  return(data_mat)
}