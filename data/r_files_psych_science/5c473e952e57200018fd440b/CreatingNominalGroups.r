rm(list=ls(all=TRUE))

library(psych)
library(memisc)
library(nlme)

RIRP2 <- as.data.set(spss.system.file("RIRP2 Study 2.sav"))
RIRP2 <- as.data.frame(RIRP2)
set.seed(8675309)
val<-c()
p<-c()
val2<-c()
p2<-c()
for( i in 1:100 )
{
    #Randomizing IAT scores.
    iatvars <- c("iat.t0", "iat.t2")
    RIRP2Rand <- RIRP2[iatvars]

    RIRP2Rand <- RIRP2Rand[sample(nrow(RIRP2Rand), 5295), ]
    RIRP2Rand$ID<-1:5295
    RIRP2$ID<-1:5295
    RIRP2Randm <- merge(RIRP2, RIRP2Rand,by="ID")
    #Creating group means and Site-mean-centering baseline IAT.
    grpmeansR<-aggregate(RIRP2Randm$iat.t0.y,list(RIRP2Randm$site2),mean,na.rm=TRUE)
    names(grpmeansR)<-c("site2","iat.t0_grpm")
    RIRP2Randm<-merge(RIRP2Randm,grpmeansR,by="site2")
    RIRP2Randm$iat.t0_grpmc<-RIRP2Randm$iat.t0.y-RIRP2Randm$iat.t0_grpm
    #names(RIRP2Randm)
    #Random MLM model.
    rModel <-lme(iat.t2.y ~ iat.t0_grpmc + iat.t0_grpm, random = ~ 1 | site2, data=RIRP2Randm, na.action=na.exclude)
    val[[i]]<-summary(rModel)$tTable[,1]["iat.t0_grpm"]
    p[[i]]<-summary(rModel)$tTable[,5]["iat.t0_grpm"]
    val2[[i]]<-summary(rModel)$tTable[,1]["iat.t0_grpmc"]
    p2[[i]]<-summary(rModel)$tTable[,5]["iat.t0_grpmc"]
}
output = as.data.frame(list( 'campuslvl'= val, "campuslvl.pval"=p, 'indlvl'= val2, "indlvl.pval"=p2))
write.table(output, "NominalGroups_data.txt", row.names=FALSE, sep="\t")


