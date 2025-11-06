rm(list=ls())


MainData <- read_excel("Path")

Correct=select(MainData,starts_with("Corr"))
Correct=as.vector(t(Correct[1,]))
MainData=select(MainData,-starts_with("Corr"))


#Meta Data

Runs=10000
Sizes=c(1,2,5,10,20,30)
DifferentSizes=6

ord=seq(1:41)
WorkData=MainData[,c(ord)]


#Create DataSet without empty rows

AnData=matrix(nrow = 50, ncol = 0)
AnData=as.data.frame(AnData)

for(i in 1:40) {
  
  Q=subset(WorkData,is.na(WorkData[i])=="FALSE" & WorkData$con=="An",select = c(i))
  Add=vector(mode="numeric", length=50-length(t(Q)))
  Add=as.data.frame((Add))
  names(Add)=names(Q)
  Q=rbind(Q,Add)
  
  AnData=cbind(AnData,Q)
  
}

InData=matrix(nrow = 50, ncol = 0)
InData=as.data.frame(InData)

for(i in 1:40) {
  
  Q=subset(WorkData,is.na(WorkData[i])=="FALSE" & WorkData$con=="In",select = c(i))
  Add=vector(mode="numeric", length=50-length(t(Q)))
  Add=as.data.frame((Add))
  names(Add)=names(Q)
  Q=rbind(Q,Add)
  
  InData=cbind(InData,Q)
  
}

CoData=matrix(nrow = 50, ncol = 0)
CoData=as.data.frame(CoData)

for(i in 1:40) {
  
  Q=subset(WorkData,is.na(WorkData[i])=="FALSE" & WorkData$con=="Co",select = c(i))
  Add=vector(mode="numeric", length=50-length(t(Q)))
  Add=as.data.frame((Add))
  names(Add)=names(Q)
  Q=rbind(Q,Add)
  
  CoData=cbind(CoData,Q)
  
}


#Start Simulation

AllCrowdsResults=vector(mode = "numeric", length = 41)

for (k in 1:DifferentSizes) {
  
ResultsAn=vector(mode = "numeric", length = 40)
ResultsIn=vector(mode = "numeric", length = 40)
ResultsCo=vector(mode = "numeric", length = 40)
ResultsMixed=vector(mode = "numeric", length = 40)
  
  
for (i in 1:Runs) {

CrowdSize=Sizes[k]

if((CrowdSize %% 2) == 0) {
  InSize=CrowdSize/2
  AnSize=CrowdSize/2
} else {
  if((i %% 2) == 0) {
    InSize=(CrowdSize+1)/2
    AnSize=(CrowdSize-1)/2
  } else {
    InSize=(CrowdSize-1)/2
    AnSize=(CrowdSize+1)/2
  }
}

AnCrowd=as.data.frame( matrix(0, nrow=CrowdSize,ncol=40))
InCrowd=as.data.frame( matrix(0, nrow=CrowdSize,ncol=40))
CoCrowd=as.data.frame( matrix(0, nrow=CrowdSize,ncol=40))
MixedCrowd=as.data.frame( matrix(0, nrow=CrowdSize,ncol=40))
MixedCrowdFH=as.data.frame( matrix(0, nrow=AnSize,ncol=40))
MixedCrowdSH=as.data.frame( matrix(0, nrow=InSize,ncol=40))


for (j in 1:40) {
AnCrowd[,j]=AnData[sample(nrow(subset(AnData,AnData[j]!="0")), CrowdSize, replace=F),j]
InCrowd[,j]=InData[sample(nrow(subset(InData,InData[j]!="0")), CrowdSize, replace=F),j]
CoCrowd[,j]=CoData[sample(nrow(subset(CoData,CoData[j]!="0")), CrowdSize, replace=F),j]

MixedCrowdFH[,j]=AnData[sample(nrow(subset(AnData,AnData[j]!="0")), AnSize, replace=F),j]
MixedCrowdSH[,j]=InData[sample(nrow(subset(InData,InData[j]!="0")), InSize, replace=F),j]
}

MixedCrowd=rbind(MixedCrowdFH,MixedCrowdSH)

AnCrowd=t(AnCrowd)
InCrowd=t(InCrowd)
CoCrowd=t(CoCrowd)
MixedCrowd=t(MixedCrowd)

ErrorAn=abs(rowMeans(AnCrowd,na.rm=TRUE)-Correct)
ErrorIn=abs(rowMeans(InCrowd,na.rm=TRUE)-Correct)
ErrorCo=abs(rowMeans(CoCrowd,na.rm=TRUE)-Correct)
ErrorMixed=abs(rowMeans(MixedCrowd,na.rm=TRUE)-Correct)


#Safe Results 
ResultsAn=cbind(ResultsAn,ErrorAn)
ResultsIn=cbind(ResultsIn,ErrorIn)
ResultsCo=cbind(ResultsCo,ErrorCo)
ResultsMixed=cbind(ResultsMixed,ErrorMixed)



}

#Create averages from simulations
ResultsAn=ResultsAn[,-c(1)]
ResultsIn=ResultsIn[,-c(1)]
ResultsCo=ResultsCo[,-c(1)]
ResultsMixed=ResultsMixed[,-c(1)]


FinalAn=rowMeans(ResultsAn,na.rm=TRUE)
FinalIn=rowMeans(ResultsIn,na.rm=TRUE)
FinalCo=rowMeans(ResultsCo,na.rm=TRUE)
FinalMixed=rowMeans(ResultsMixed,na.rm=TRUE)


JointFinal=rbind(FinalAn,FinalIn,FinalCo,FinalMixed)
JointFinal=cbind(CrowdSize,JointFinal)

AllCrowdsResults=rbind(AllCrowdsResults,JointFinal)

cat(paste(k, " "));
flush.console()


}


#Delete everything but final results
rm(list=setdiff(ls(), "AllCrowdsResults"))
AllCrowdsResults=as.data.frame( AllCrowdsResults[-c(1),])


#Create Table for computation of e-sizes and t-tests;
SizeSafe <- AllCrowdsResults[,c(1)]
Type4 <- c("FinalAn","FinalIn","FinalCon","FinalMixed")
Type8 <- append(Type4,Type4)
Type16 <- append(Type8,Type8)
Type24 <- append(Type16,Type8)

Names <- paste(Type24,SizeSafe,sep = "_")

Table1 <- AllCrowdsResults
Table1=Table1[,-c(1)]

rownames(Table1) <- Names  
Table1 <- as.data.frame(t(Table1))
rownames(Table1) <- NULL

write_xlsx(Table1,"Path")










