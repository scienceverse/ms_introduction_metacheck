rm(list=ls())

#Meta Data

Runs=10000
Sizes=c(1,2,5,10,20,31)
DifferentSizes=6

MainData <- read_excel("Path")

Correct=select(MainData,starts_with("Bet"))
Correct=as.vector(t(Correct[1,]))
MainData=select(MainData,-starts_with("Bet"))
MainData=select(MainData,-starts_with("outcome"))
MainData=select(MainData,-starts_with("pro"))
MainData=select(MainData,-starts_with("exp"))
MainData=select(MainData,-starts_with("age"))
MainData=select(MainData,-starts_with("gender"))

AllCrowdsResults=vector(mode = "numeric", length = 145)

for (k in 1:DifferentSizes) {
  
  ResultsAn=vector(mode = "numeric", length = 144)
  ResultsIn=vector(mode = "numeric", length = 144)
  ResultsCon=vector(mode = "numeric", length = 144)
  ResultsMixed=vector(mode = "numeric", length = 144)
  
  
  for (i in 1:Runs) {
    
    CrowdSize=Sizes[k]
    
    AnCrowd=subset(MainData,condition=="AN")[sample(nrow(subset(MainData,condition=="AN")), CrowdSize, replace=F),]
    AnCrowd=t(AnCrowd[,-c(1)])
    
    InCrowd=subset(MainData,condition=="IN")[sample(nrow(subset(MainData,condition=="IN")), CrowdSize, replace=F),]
    InCrowd=t(InCrowd[,-c(1)])
    
    ConCrowd=subset(MainData,condition=="CO")[sample(nrow(subset(MainData,condition=="CO")), CrowdSize, replace=F),]
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
    MixedCrowd=rbind(subset(MainData,condition=="AN")[sample(nrow(subset(MainData,condition=="AN")), AnSize, replace=F),],subset(MainData,condition=="IN")[sample(nrow(subset(MainData,condition=="IN")), InSize, replace=F),])
    MixedCrowd=t(MixedCrowd[,-c(1)])
    
    ErrorAn=abs(rowMeans(AnCrowd,na.rm=T)-Correct)
    ErrorIn=abs(rowMeans(InCrowd,na.rm=T)-Correct)
    ErrorCon=abs(rowMeans(ConCrowd,na.rm=T)-Correct)
    ErrorMixed=abs(rowMeans(MixedCrowd,na.rm=T)-Correct)
    
    ResultsAn=cbind(ResultsAn,ErrorAn)
    ResultsIn=cbind(ResultsIn,ErrorIn)
    ResultsCon=cbind(ResultsCon,ErrorCon)
    ResultsMixed=cbind(ResultsMixed,ErrorMixed)
    
    
  }
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
AllCrowdsResults=as.data.frame( AllCrowdsResults[-c(1),])



#Average error
T1Error=select(AllCrowdsResults,starts_with("Team1"))
DrawError=select(AllCrowdsResults,starts_with("draw"))
T2Error=select(AllCrowdsResults,starts_with("Team2"))
row.names(T1Error)=NULL
row.names(T2Error)=NULL
row.names(DrawError)=NULL

AvError=(T1Error+DrawError+T2Error)/3

#Create Table with values across all questions; This can then be analyzed with t-tests or effect sizes can be computed)

SizeSafe <- AllCrowdsResults[,c(1)]
Type4 <- c("FinalAn","FinalIn","FinalCon","FinalMixed")
Type8 <- append(Type4,Type4)
Type16 <- append(Type8,Type8)
Type24 <- append(Type16,Type8)

Names <- paste(Type24,SizeSafe,sep = "_")

Table1 <- AvError
rownames(Table1) <- Names  
Table1 <- as.data.frame(t(Table1))
rownames(Table1) <- NULL

write_xlsx(Table1,"Path")





  