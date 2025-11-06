#TD adapted from https://github.com/metacoglab/HMeta-d 
#See license file for re-use guidelines
## Packages 
library(tidyverse)
library(magrittr)
library(reshape2)
library(rjags)
library(coda)
library(lattice)
library(broom)
library(ggpubr)
library(ggmcmc)

## 
source('~/Dropbox/H-MetaD_KMSD/Function_metad_2group_repeated.R', chdir = TRUE)

subdata=read.table('~/Dropbox/subcounts_immdel.txt',F)

nR_S1_1=t(subdata[1:20,2:15])
row.names(nR_S1_1)=c()
colnames(nR_S1_1)=paste('p',1:20,sep='')
imm_nR_S1 =list(data.frame(nR_S1_1))

nR_S2_1=t(subdata[1:20,2:15+14])
row.names(nR_S2_1)=c()
colnames(nR_S2_1)=paste('p',1:20,sep='')
imm_nR_S2 =list(data.frame(nR_S2_1))


nR_S1_2=t(subdata[1:20,2:15+2*14])
row.names(nR_S1_2)=c()
colnames(nR_S1_2)=paste('p',1:20,sep='')
del_nR_S1 =list(data.frame(nR_S1_2))

nR_S2_2=t(subdata[1:20,2:15+3*14])
row.names(nR_S2_2)=c()
colnames(nR_S2_2)=paste('p',1:20,sep='')
del_nR_S2 =list(data.frame(nR_S2_2))


nR_S1 <- list(data.frame(nR_S1_1),
              data.frame(nR_S1_2))

nR_S2 <- list(data.frame(nR_S2_1),
              data.frame(nR_S2_2))


output <- metad_groupcorr(nR_S1 = nR_S1, nR_S2 = nR_S2)

plot(output, ask=T)

# Values 
Value <- summary(output)
stat <- data.frame(mean = Value$statistics[,"Mean"])
stat %<>%
  rownames_to_column(var = "name")

# Rhat 
Value <- gelman.diag(output, confidence = 0.95)
Rhat <- data.frame(conv = Value$psrf)


# HDI 
HDI <- data.frame(HPDinterval(output, prob = 0.905))
HDI %<>%
    rownames_to_column(var = "name")

# All values in the same dataframe
Fit <- stat %>%
  cbind(lower = HDI$lower,
        upper = HDI$upper,
        Rhat = Rhat[,1])





