# 1st batch -----------------------------------------------

df_fac = data.table::fread("data/download 1a/Humor study 4a_January 28, 2021_09.09.csv", data.table = F)

df_fac = df_fac[df_fac$attention_check == 3,]
df_fac = df_fac[df_fac$techissues == '1',]
df_fac = df_fac[df_fac$vidaudCHECK == '1' | df_fac$vidaudCHECK == '2',]


colnames(df_fac)[colnames(df_fac) == 'imgsex1'] = 'imgself1'
colnames(df_fac)[colnames(df_fac) == 'imgsex2'] = 'imgsex1'
stimuli_in_batch = c("joaff5", "joagg5" , "joinc5" , "jonon4" , "jonon5" ,
                     "jorew4" ,   "jorew5" ,   "joself4",   "joself5", 'vidsex2', 'vidsex3',
                     "josex4"  ,  "josex5" ,   "imgaff1" ,   "imgaff2",
                     "imgagg1"  ,  "imgagg2" ,  "imginc1" ,  "imginc2" ,
                     "imgnon1" ,   "imgnon2"  ,  "imgrew1"  ,  "imgrew2" ,
                     "imgself1" ,  "imgsex1" ,   "vidaff5" ,   "vidaff4" ,
                     "vidagg3"  ,  "vidagg4" ,  "vidinc3" ,  "vidinc4" ,
                     "vidnon3"  , "vidnon4"  ,  "vidrew3"  ,  "vidself3" )

df_fac = sapply(df_fac[stimuli_in_batch], as.numeric)
sum(is.na(df_fac))
colnames(df_fac) = gsub("jo", "jo_", colnames(df_fac) )
colnames(df_fac) = gsub("img", "img_", colnames(df_fac) )
colnames(df_fac) = gsub("vid", "vid_", colnames(df_fac) )
colnames(df_fac) = gsub("rew", "enhance", colnames(df_fac) )
colnames(df_fac) = gsub("aff", "affiliative", colnames(df_fac) )
colnames(df_fac) = gsub("agg", "aggressive", colnames(df_fac) )
colnames(df_fac) = gsub("self", "self-def", colnames(df_fac) )
df_fac = df_fac[,!grepl("sex", colnames(df_fac))]
df_fac = df_fac[,!grepl("inc", colnames(df_fac))]
df_fac = df_fac[,!grepl("non", colnames(df_fac))]


correlations = cor(df_fac)
correlations = apply(correlations, 2, function(x){rank(x) > ncol(df_fac) - 5})
library(corrplot)
corrplot(correlations)


library(psych)
fact_an <- fa.parallel(df_fac)
fa = factanal(df_fac, fact_an$nfact)
library(GPArotation)
fa_rotat <- GPFoblq(loadings(fa), method="oblimin", normalize=TRUE)

fa_rotat$loadings

# 2nd batch ---------------------------------------------------------------

df_fac = data.table::fread("data/download 2a/Humor study 4b_February 7, 2021_12.33.csv", data.table = F)

df_fac = df_fac[df_fac$attention_check == 1,]
df_fac = df_fac[df_fac$techissues == '1',]
df_fac = df_fac[df_fac$vidaudCHECK == '1' | df_fac$vidaudCHECK == '2',]
stimuli_in_batch = c( "joaff1"        ,        "joaff2"       ,        
                      "joagg1"     ,           "joagg2"         ,       "joinc1"    ,           
                      "joinc2"     ,           "jonon1"         ,       "jonon2"    ,           
                      "jorew1"     ,           "joself1"        ,       "joself2"   ,           
                      "josex1"     ,           "imgaff3"        ,       "imgaff4"   ,           
                      "imgagg3"    ,           "imgagg4"        ,       "imginc3"   ,           
                      "imginc4"    ,           "imgnon3"        ,       "imgrew3"   ,           
                      "imgself2"   ,           "imgself3"       ,       "imgsex2"   ,           
                      "imgsex3"    ,           "vidaff3"        ,       "vidagg5"   ,           
                      "vidinc5"    ,           "vidnon5"        ,       "vidrew4"   ,           
                      "vidrew5"    ,           "vidself1"       ,       "vidself4"  ,           
                      "vidsel5"    ,           "vidsex4"        ,       "vidsex5")
df_fac = sapply(df_fac[stimuli_in_batch], as.numeric)
colnames(df_fac) = gsub("jo", "jo_", colnames(df_fac) )
colnames(df_fac) = gsub("img", "img_", colnames(df_fac) )
colnames(df_fac) = gsub("vid", "vid_", colnames(df_fac) )
colnames(df_fac) = gsub("rew", "enhance", colnames(df_fac) )
colnames(df_fac) = gsub("aff", "affiliative", colnames(df_fac) )
colnames(df_fac) = gsub("agg", "aggressive", colnames(df_fac) )
colnames(df_fac) = gsub("self", "self-def", colnames(df_fac) )
df_fac = df_fac[,!grepl("sex", colnames(df_fac))]
df_fac = df_fac[,!grepl("inc", colnames(df_fac))]
df_fac = df_fac[,!grepl("non", colnames(df_fac))]


correlations = cor(df_fac)
correlations = apply(correlations, 2, function(x){rank(x) > ncol(df_fac) - 5})
library(corrplot)
corrplot(correlations)

library(psych)
fact_an <- fa.parallel(df_fac)
fa = factanal(df_fac, fact_an$nfact)
library(GPArotation)
fa_rotat <- GPFoblq(loadings(fa), method="oblimin", normalize=TRUE)
fa_rotat$loadings

# 3rd batch ---------------------------------------------------------------


df_fac = data.table::fread("data/download 3a/Humor study 4c_February 1, 2021_11.59.csv", data.table = F)

df_fac = df_fac[df_fac$attention_check == 1,]
df_fac = df_fac[df_fac$techissues == '1',]
df_fac = df_fac[df_fac$vidaudCHECK == '1' | df_fac$vidaudCHECK == '2',]
stimuli_in_batch = c( "joaff3"  , "joaff4"  , "joagg3" ,  "joagg4"  , "joinc3"   ,"joinc4" ,
                     "jonon3"  , "jorew2"  , "jorew3" ,  "joself3" , "josex2"   ,"josex3" , 
                     "imgaff5" , "imgagg5" , "imginc5" , "imgnon4" , "imgnon5"  ,"imgrew4", 
                     "imgrew5" , "imgself4", "imgself5" ,"imgsex4" , "imgsex5"  ,"vidaff1", 
                     "vidaff2" , "vidagg1" , "vidagg2" , "vidinc1" , "vidinc2"  ,"vidnon1", 
                     "vidnon2" , "vidrew1" , "vidrew2" , "vidsex1" , "vidself2")
df_fac = sapply(df_fac[stimuli_in_batch], as.numeric)
colnames(df_fac) = gsub("jo", "jo_", colnames(df_fac) )
colnames(df_fac) = gsub("img", "img_", colnames(df_fac) )
colnames(df_fac) = gsub("vid", "vid_", colnames(df_fac) )
colnames(df_fac) = gsub("rew", "enhance", colnames(df_fac) )
colnames(df_fac) = gsub("aff", "affiliative", colnames(df_fac) )
colnames(df_fac) = gsub("agg", "aggressive", colnames(df_fac) )
colnames(df_fac) = gsub("self", "self-def", colnames(df_fac) )
df_fac = df_fac[,!grepl("sex", colnames(df_fac))]
df_fac = df_fac[,!grepl("inc", colnames(df_fac))]
df_fac = df_fac[,!grepl("non", colnames(df_fac))]


correlations = cor(df_fac)
correlations = apply(correlations, 2, function(x){rank(x) > ncol(df_fac) - 5})
library(corrplot)
corrplot(correlations)

library(psych)
fact_an <- fa.parallel(df_fac)
fa = factanal(df_fac, fact_an$nfact)
library(GPArotation)
fa_rotat <- GPFoblq(loadings(fa), method="oblimin", normalize=TRUE)
fa_rotat$loadings

