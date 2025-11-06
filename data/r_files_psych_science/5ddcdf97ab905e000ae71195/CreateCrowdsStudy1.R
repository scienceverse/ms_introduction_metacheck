
rm(list=ls())


Runs=10000
Sizes=c(1,2,5,10,20,48)
DifferentSizes=6

MainData <- read_excel("Path")

#Create vector with correct answers
Correct=select(MainData,starts_with("corr"))
Correct=as.vector(t(Correct[1,]))
MainData=select(MainData,-starts_with("corr"))


#Start Simulation

AllCrowdsResults=vector(mode = "numeric", length = 41)

for (k in 1:DifferentSizes) {
  
ResultsAn=vector(mode = "numeric", length = 40)
ResultsIn=vector(mode = "numeric", length = 40)
ResultsCon=vector(mode = "numeric", length = 40)
ResultsMixed=vector(mode = "numeric", length = 40)
  
  
for (i in 1:Runs) {

CrowdSize=Sizes[k]

AnCrowd=subset(MainData,con=="ANT")[sample(nrow(subset(MainData,con=="ANT")), CrowdSize, replace=F),]
AnCrowd=t(AnCrowd[,-c(1)])

InCrowd=subset(MainData,con=="INT")[sample(nrow(subset(MainData,con=="INT")), CrowdSize, replace=F),]
InCrowd=t(InCrowd[,-c(1)])

ConCrowd=subset(MainData,con=="CON")[sample(nrow(subset(MainData,con=="CON")), CrowdSize, replace=F),]
ConCrowd=t(ConCrowd[,-c(1)])

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


MixedCrowd=rbind(subset(MainData,con=="ANT")[sample(nrow(subset(MainData,con=="ANT")), AnSize, replace=F),],subset(MainData,con=="INT")[sample(nrow(subset(MainData,con=="INT")), InSize, replace=F),])
MixedCrowd=t(MixedCrowd[,-c(1)])

ErrorAn=abs(rowMeans(AnCrowd,na.rm=TRUE)-Correct)
ErrorIn=abs(rowMeans(InCrowd,na.rm=TRUE)-Correct)
ErrorCon=abs(rowMeans(ConCrowd,na.rm=TRUE)-Correct)
ErrorMixed=abs(rowMeans(MixedCrowd,na.rm=TRUE)-Correct)

#Safe Results 
ResultsAn=cbind(ResultsAn,ErrorAn)
ResultsIn=cbind(ResultsIn,ErrorIn)
ResultsCon=cbind(ResultsCon,ErrorCon)
ResultsMixed=cbind(ResultsMixed,ErrorMixed)

}
  

#Create averages from simulations
ResultsAn=ResultsAn[,-c(1)]
ResultsIn=ResultsIn[,-c(1)]
ResultsCon=ResultsCon[,-c(1)]
ResultsMixed=ResultsMixed[,-c(1)]


FinalAn=rowMeans(ResultsAn,na.rm=TRUE)
FinalIn=rowMeans(ResultsIn,na.rm=TRUE)
FinalCon=rowMeans(ResultsCon,na.rm=TRUE)
FinalMixed=rowMeans(ResultsMixed,na.rm=TRUE)


JointFinal=rbind(FinalAn,FinalIn,FinalCon,FinalMixed)
JointFinal=cbind(CrowdSize,JointFinal)

AllCrowdsResults=rbind(AllCrowdsResults,JointFinal)

}

#Delete everything but final results
rm(list=setdiff(ls(), "AllCrowdsResults"))

#Create Table with values across all questions; This can then be analyzed with t-tests or effect sizes can be computed)
Table1=AllCrowdsResults
Table1=Table1[-c(1),]

SizeSafe=Table1[,c(1)]
TypeSafe <- as.vector(rownames(Table1))
Names <- paste(TypeSafe,SizeSafe,sep = "_")

rownames(Table1) <- Names
Table1=Table1[,-c(1)]

Table1=as.data.frame(t(Table1))

write_xlsx(Table1,"Path")





