source("[list_dir]/meta_misinformaiton_codebook_osf.v20180606.R")
dfs <- as.data.frame(dfs)
dfs$DebunkingDetails <- ifelse(dfs$DebunkingDetails=='detailed',2,
                               ifelse(dfs$DebunkingDetails=='not detailed',1,NA))

library(metafor) 
library(metaSEM)

### Get the datasets
###############################################
### All effects
###############################################
dfM <- subset(dfs, dfs$MisinformationEffectSizes != 'NA')
dfR <- subset(dfs, dfs$DebunkingEffectSizes != 'NA')
dfCI <- subset(dfs, dfs$MisinformationPersistenceEffectSizes != 'NA')
###############################################
### Effects without outliers
###############################################
dfMmini <- subset(dfM, dfM$MisinformationEffectSizes<8.90)
dfCImini <- subset(dfCI, dfCI$MisinformationPersistenceEffectSizes<6.40)

### Meta-regression analyses - All effects
###############################################
### Misinformation 
###############################################
### MEM 
mesM <- rma(yi=MisinformationEffectSizes,vi=MisinformationEffectSizesV1,
            mods=~ZGenerationExplanations,data=dfM, method="ML");summary(mesM)

### WMEM
summary(m <- lm(SampleSize ~ 
                  factor(PublicationStatus) + 
                  factor(OnlineLabDataCollection) + 
                  PublicationYear + 
                  ZGenerationExplanations, 
                data=dfM))
dfM[,"Nrow_stdres2"] <- rstandard(m) - min(rstandard(m)) + .0001
wmesM <- rma(yi=MisinformationEffectSizes,vi=MisinformationEffectSizesV1,weights=Nrow_stdres2, 
             mods=~ZGenerationExplanations, data=dfM, method="ML");summary(wmesM)

### Three-level MEM 
mes3M <- meta3(MisinformationEffectSizes, MisinformationEffectSizesV1, cluster=Authors, 
               x=cbind(scale(ZGenerationExplanations)), data=dfM, model.name="3 level model");summary(mes3M)

###############################################
### Debunking 
###############################################
dfR <- na.omit(dfR[c("Authors","SampleSize","DebunkingEffectSizes","DebunkingEffectSizesV1","PublicationYear","PublicationStatus","OnlineLabDataCollection",
                     "ZGenerationExplanations","ZGenerationCounterarguments","DebunkingDetails")])

### MEM
mesR <- rma(yi=DebunkingEffectSizes,vi=DebunkingEffectSizesV1,
            mods=~ZGenerationExplanations+ZGenerationCounterarguments+factor(DebunkingDetails),
            data=dfR, method="ML");summary(mesR)

### Three-level MEM 
mes3R <- meta3(DebunkingEffectSizes, DebunkingEffectSizesV1, cluster=Authors, 
               x=cbind(scale(ZGenerationExplanations),scale(ZGenerationCounterarguments),factor(DebunkingDetails)),
               data=dfR, model.name="3 level model");summary(mes3R)

###############################################
### Misinformation-persistence 
###############################################
### MEM
mesCI <- rma(yi=MisinformationPersistenceEffectSizes,vi=MisinformationPersistenceEffectSizesV1, 
             mods=~ZGenerationExplanations+ZGenerationCounterarguments+factor(DebunkingDetails),
             data= dfCI, method="ML");summary(mesCI)

### WMEM
dfCI <- na.omit(dfCI[c("Authors","SampleSize","MisinformationPersistenceEffectSizes","MisinformationPersistenceEffectSizesV1","PublicationYear","PublicationStatus","OnlineLabDataCollection",
                     "ZGenerationExplanations","ZGenerationCounterarguments","DebunkingDetails")])

summary(ci <- lm(SampleSize ~ 
                   factor(PublicationStatus) + 
                   factor(OnlineLabDataCollection) + 
                   PublicationYear +
                   ZGenerationExplanations + 
                   ZGenerationCounterarguments, 
                 data=dfCI))
dfCI[,"Nrow_stdres2"] <- rstandard(ci) - min(rstandard(ci)) + .0001
wmesCI <- rma(yi=MisinformationPersistenceEffectSizes,vi=MisinformationPersistenceEffectSizesV1, weights=Nrow_stdres2, 
              mods=~ZGenerationExplanations + ZGenerationCounterarguments + factor(DebunkingDetails),
              data= dfCI, method="ML");summary(wmesCI)

### Three-level MEM 
mes3CI <- meta3(MisinformationPersistenceEffectSizes, MisinformationPersistenceEffectSizesV1, cluster=Authors, 
                x=cbind(scale(ZGenerationExplanations),scale(ZGenerationCounterarguments),factor(DebunkingDetails)), 
                data=dfCI, model.name="3 level model");summary(mes3CI)



### Meta-regression analyses - Effects without outliers
###############################################
### Misinformation 
###############################################
### MEM 
mesMmini <- rma(yi=MisinformationEffectSizes,vi=MisinformationEffectSizesV1,
                mods=~ZGenerationExplanations, data=dfMmini, method="ML");summary(mesMmini)

### WMEM
summary(m <- lm(SampleSize ~ 
                  factor(PublicationStatus) + 
                  factor(OnlineLabDataCollection) + 
                  PublicationYear + 
                  ZGenerationExplanations, data=dfMmini)) 
dfMmini[,"Nrow_stdres2"] <- rstandard(m) - min(rstandard(m)) + .0001 
wmesMmini <- rma(yi=MisinformationEffectSizes,vi=MisinformationEffectSizesV1,weights=Nrow_stdres2, 
                 mods=~ZGenerationExplanations, data=dfMmini, method="ML");summary(wmesMmini)

###############################################
### Misinformation-persistence 
###############################################
### MEM
mesCImini <- rma(yi=MisinformationPersistenceEffectSizes,vi=MisinformationPersistenceEffectSizesV1, 
                 mods=~ZGenerationExplanations + ZGenerationCounterarguments + factor(DebunkingDetails),
                 data= dfCImini, method="ML");summary(mesCImini)

### WMEM
summary(ci <- lm(SampleSize ~ factor(PublicationStatus) + factor(OnlineLabDataCollection) + PublicationYear +
                   ZGenerationExplanations + ZGenerationCounterarguments, data=dfCImini))
dfCImini[,"Nrow_stdres2"] <- rstandard(ci) - min(rstandard(ci)) + .0001
wmesCImini <- rma(yi=MisinformationPersistenceEffectSizes,vi=MisinformationPersistenceEffectSizesV1, weights=Nrow_stdres2, 
                  mods=~ZGenerationExplanations + ZGenerationCounterarguments + factor(DebunkingDetails),
                  data= dfCImini, method="ML");summary(wmesCImini)
