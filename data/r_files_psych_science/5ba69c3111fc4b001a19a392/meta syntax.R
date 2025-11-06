library(metafor)
library("forestplot", lib.loc="~/Library/R/3.4/library")

## meta-analysis with 8 studies
data <- read.csv("meta_8studies.csv")
es_R<-rma(yi=d,vi=variance_d,data=data,method="REML") 
es_R
forest(es_R,slab=paste(label=data$study))

## meta-analysis with 7 studies (excluding supplemental study 4)
data <- read.csv("meta_7studies.csv")
es_7<-rma(yi=d,vi=variance_d,data=data,method="REML") 
es_7