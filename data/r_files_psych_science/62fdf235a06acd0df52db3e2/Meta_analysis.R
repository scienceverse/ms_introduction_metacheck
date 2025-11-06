rm(list = ls()) #remove all variables

install.packages("metaSEM")
install.packages("dplyr")
install.packages("haven")
install.packages("metafor")


library(metaSEM)
library(metafor)
library(dplyr)
library(haven)

meta.data <- read_sav("Metadata.sav")

# first force variables into lower case. 
names(meta.data)<-tolower(names(meta.data))

-#--- calculate sampling variances: ---
  
set.seed(123)
vartot <- (1-meta.data$rtot^2)^2/(meta.data$ntot-1)
varag <- (1-meta.data$ragr^2)^2/(meta.data$nagr-1)
vardis <- (1-meta.data$rdis^2)^2/(meta.data$ndis-1)
vartot
varag
vardis

#put data in frame
minimeta <- data.frame(study=meta.data$study, rtotal=meta.data$rtot, vtot=vartot, ragr=meta.data$ragr,
                       vagr=varag, rdis=meta.data$rdis, vdis=vardis, dal=meta.data$deal)
minmetdef <- na.omit(minimeta)
set.seed(11)


#nested analysis for overall correlations:

ovcor <- meta3(y = rtotal, v = vtot, cluster=study, data = minmetdef)
summary(ovcor)

#nested analysis for correlations among participants believing the official reading:

ovag <- meta3(y = ragr, v = vagr, cluster=study, data = minmetdef)
summary(ovag)

#nested analysis for correlations among participants disbelieving the official reading:

ovdis <- meta3(y = rdis, v = vdis, cluster=study, data = minmetdef)
summary(ovdis)

#moderation of dead-or-alive cases among disbelievers of the official reading:

dalmod2 <- meta3(y = rdis, v = vdis, cluster=study, x=dal, data = minmetdef)
summary(dalmod2)

#create data frame NON-dead or alive cases only:

nondal <- data.frame(study=meta.data$study, rdisnd=meta.data$rnondea, vdis=vardis)
nondaldef <- na.omit(nondal)
set.seed(11)

#nested analysis for correlations among disbeliever for NON dead-or-alive cases only:
ovnonda <- meta3(y = rdisnd, v = vdis, cluster=study, data = nondaldef)
summary(ovnonda)

#create data frame dead or alive cases only:

daldat <- data.frame(study=meta.data$study, rdisdal=meta.data$rdea, vdis=vardis)
daldef <- na.omit(daldat)
set.seed(11)


#nested analysis for correlations among disbeliever for dead-or-alive cases only:
ovdal <- meta3(y = rdisdal, v = vdis, cluster=study, data = daldef)
summary(ovdal)




