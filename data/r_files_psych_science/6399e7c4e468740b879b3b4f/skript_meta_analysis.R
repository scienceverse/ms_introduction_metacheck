# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dat_meta <- read.csv2("Tabelle_Meta-Analyse.csv")					
View(dat_meta)

library(metafor)
citation("metafor")
sessionInfo()

metafor.rma <- rma(effect_size, 1/n, dat = dat_meta) # random effect
metafor.rma 
AIC(metafor.rma)
BIC(metafor.rma)

metafor.fe <- rma(effect_size, 1/n, dat = dat_meta, method = "FE") # fixed effect
metafor.fe  
AIC(metafor.fe)
BIC(metafor.fe)

n <- dat_meta$n

forest(metafor.fe, header = TRUE, 
       xlab = "effect size (Cohen´s d)", alim = c(-0.1, 0.40), 
       steps = 6, psize = sqrt(n*0.0006), # size of squares were adjusted
       cex = 1.20, annotate = TRUE)
