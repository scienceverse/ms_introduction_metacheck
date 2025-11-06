library(dplyr)
set.seed(42)
setwd("~/Projects/11 humor/scripts")
# load data ---------------------------------------------------------------
df = data.table::fread("~/Projects/11 humor/data/download 1a/Humor study 4a_January 28, 2021_09.09.csv", data.table = F)
dfb = data.table::fread("~/Projects/11 humor/data/download 1b/Humor study 4a - followup_February 20, 2021_17.07.csv", data.table = F)
df3 = data.table::fread('~/Projects/11 humor/data/download 3a/Humor study 4c_February 1, 2021_11.59.csv', data.table = F)
df3b= data.table::fread('~/Projects/11 humor/data/download 3b/Humor study 4c - followup_February 25, 2021_08.15.csv', data.table = F)
df2 = data.table::fread('~/Projects/11 humor/data/download 2total/Humor study 4b_March 2, 2021_06.47.csv', data.table = F)

#preregistered filters

df = df[df$attention_check == 3,]
df = df[df$techissues == '1'  | df$techissues == '2',]
df = df[df$vidaudCHECK == '1',]
dfb = dfb[dfb$vidaudCHECK == '1',]
dfb = dfb[dfb$attention_check == 3,]
dfb = dfb[dfb$techissues == '1' | dfb$techissues == '2',]
colnames(df)[colnames(df) == 'imgsex1'] = 'imgself1'
colnames(df)[colnames(df) == 'imgsex2'] = 'imgsex1'
colnames(dfb)[colnames(dfb) == 'imgsex1'] = 'imgself1'
colnames(dfb)[colnames(dfb) == 'imgsex2'] = 'imgsex1'
df3 = df3[df3$attention_check == 1,]
df3 = df3[df3$techissues == '1' | df3$techissues == '2',]
df3 = df3[df3$vidaudCHECK == '1',]

df3b = df3b[df3b$attention_check == 2,]
df3b = df3b[df3b$techissues == '1' | df3b$techissues == '2',]
df3b = df3b[df3b$vidaudCHECK == '1',]

df2 = df2[df2$attention_check == 1,]
df2 = df2[df2$techissues == '1' | df2$techissues == '2',]
df2 = df2[df2$vidaudCHECK == '1',]
df = plyr::rbind.fill(df, dfb, df3, df3b, df2)

columns_to_be_pivoted = c("joaff5", "joagg5" , "joinc5" , "jonon4" , "jonon5" ,
 "jorew4" ,   "jorew5" ,   "joself4",   "joself5", 'vidsex2', 'vidsex3',
 "josex4"  ,  "josex5" ,   "imgaff1" ,   "imgaff2",
 "imgagg1"  ,  "imgagg2" ,  "imginc1" ,  "imginc2" ,
 "imgnon1" ,   "imgnon2"  ,  "imgrew1"  ,  "imgrew2" ,
 "imgself1" ,  "imgsex1" ,   "vidaff5" ,   "vidaff4" ,
 "vidagg3"  ,  "vidagg4" ,  "vidinc3" ,  "vidinc4" ,
 "vidnon3"  , "vidnon4"  ,  "vidrew3"  ,  "vidself3" ,"joaff3"  , "joaff4"  , "joagg3" ,  "joagg4"  , "joinc3"   ,"joinc4" ,
"jonon3"  , "jorew2"  , "jorew3" ,  "joself3" , "josex2"   ,"josex3" , 
"imgaff5" , "imgagg5" , "imginc5" , "imgnon4" , "imgnon5"  ,"imgrew4", 
"imgrew5" , "imgself4", "imgself5" ,"imgsex4" , "imgsex5"  ,"vidaff1", 
"vidaff2" , "vidagg1" , "vidagg2" , "vidinc1" , "vidinc2"  ,"vidnon1", 
"vidnon2" , "vidrew1" , "vidrew2" , "vidsex1" , "vidself2",

"joaff1"        ,        "joaff2"       ,        
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
 "vidsel5"    ,           "vidsex4"        ,       "vidsex5" )

df[columns_to_be_pivoted] = sapply(df[columns_to_be_pivoted], as.numeric)
df$rater = df$prolificID
df = df[,c('rater', columns_to_be_pivoted)]
length(unique(df$rater))
sum(duplicated(df$rater))
df_long = tidyr::pivot_longer(df, cols = all_of(columns_to_be_pivoted), names_to = 'word')
df_long$raterjoke = paste0(df_long$rater, df_long$word)
length(unique(df_long$raterjoke))

# bootstrap CIs -----------------------------------------------------------

model = lme4::lmer(value~ (1|rater) + (1|word) + (1|rater:word), data=df_long)
sigm = (sigma(model))^2
summary(model)
calc.icc <- function(y) {
  sumy <- summary(y)
  r = sumy$varcor$rater[1] / (sumy$varcor$rater[1] + sumy$varcor$word[1]+  sumy$varcor$'rater:word'[1]+sumy$sigma^2)
  w = sumy$varcor$word[1] / (sumy$varcor$rater[1] + sumy$varcor$word[1]+  sumy$varcor$'rater:word'[1]+sumy$sigma^2)
  i = sumy$varcor$'rater:word'[1] / (sumy$varcor$rater[1] + sumy$varcor$word[1]+  sumy$varcor$'rater:word'[1]+sumy$sigma^2)
  c(r,w,i, r-w, i-r, i-w)
} 
##https://bit.ly/2NMf2wp
results = lme4::bootMer(model, calc.icc, nsim = 500)
beepr::beep()
# 
rater_iccs = results$t[,1]
joke_iccs = results$t[,2]
inter_iccs = results$t[,3]
diffraterjoke = results$t[,4]
diffinterrater = results$t[,5]
diffinterjoke = results$t[,6]

summary(joke_iccs)#
quantile(joke_iccs, 0.025)#
quantile(joke_iccs, 0.975)#
summary(rater_iccs)#
quantile(rater_iccs, 0.025)#
quantile(rater_iccs, 0.975)#
summary(diffraterjoke )#
quantile(diffraterjoke , 0.025)#
quantile(diffraterjoke , 0.975)#
summary(inter_iccs)
quantile(inter_iccs, 0.025)#
quantile(inter_iccs, 0.975)#
summary(diffinterrater)#
quantile(diffinterrater, 0.025)#
quantile(diffinterrater, 0.975)#
summary(diffinterjoke)#
quantile(diffinterjoke, 0.025)#
quantile(diffinterjoke, 0.975)#


# graphs ------------------------------------------------------------------

df_long$rater[duplicated(df_long$raterjoke)] = paste0(df_long$rater[duplicated(df_long$raterjoke)], 'repeat')
df_wide = tidyr::pivot_wider(df_long, id_cols = c('rater'), names_from = 'word', values_from = 'value')
raters = gsub(x = df_wide$rater, pattern = 'repeat', replacement = '')
df_wide$rater = NULL
cl <- parallel::makeCluster(3)
doParallel::registerDoParallel(cl)
library(foreach)
same_material_two_ratings = foreach(seed = 1:1000000, .combine = 'rbind') %do%{
  if(seed %% 10000 == 0){print(seed/10000)}
  set.seed(seed)
  samplejoke = sample(1:ncol(df_wide), 1)
  valid_ratings = df_wide[!is.na(df_wide[,samplejoke]), samplejoke]
  c(sample(unlist(valid_ratings), 2), samplejoke)
}
doParallel::stopImplicitCluster()
rm(cl)
beepr::beep()
save(same_material_two_ratings, file="C:/Users/rosenbus/surfdrive/pythanalytics/11 humor/data/same_material_two_ratings.Rdata")
load("C:/Users/rosenbus/surfdrive/pythanalytics/11 humor/data/same_material_two_ratings.Rdata")
cor.test(as.numeric(same_material_two_ratings[,1]), as.numeric(same_material_two_ratings[,2]), use = 'pairwise.complete')


#heattable
colnames(same_material_two_ratings) = row.names(same_material_two_ratings) = NULL
binsA = same_material_two_ratings[,1]
binsB = same_material_two_ratings[,2]
hmapdata = gmodels::CrossTable(binsA, binsB)$prop.col
hmapdata = as.matrix(as.data.frame.matrix(hmapdata))

sum(hmapdata[,2])
sum(hmapdata[2,])

h = data.table::melt(hmapdata)
library(ggplot2)
# ord=hclust(1-as.dist(corr_mat))$order
# co=melt(corr_mat[ord,ord])
ggplot(h, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(size = 6, aes(fill = h$value, label = round(h$value, 2))) + 
  scale_fill_gradient(low = "white", high = "gray70") + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.title.x = element_text(hjust = 0.5,vjust=4), 
        axis.title.y = element_text(hjust = 0.5, vjust = 2), 
        
        axis.text.x = element_text(hjust = 0,vjust=5,size = 12,face = "bold"), 
        axis.text.y = element_text(size = 12,face = "bold", hjust = 5,vjust=0)) + 
  coord_flip()+
  theme(legend.title=element_text(face="bold", size=14), text = element_text(size = 17)) + 
  ggtitle('Relationship between two ratings of same stimulus')+
  labs(fill="P(B|A)") +
  scale_x_continuous(name= 'Rating B',breaks = c(1:7), labels = c('1 ','2 ','3 ','4 ','5 ', '6 ', '7 '), limits = c(0.5,7.5))+
  scale_y_continuous(name= 'Rating A',breaks = c(1:7), labels = c('1 ','2 ','3 ','4 ','5 ', '6 ','7 '), limits = c(0.5,7.5))

ggsave(
  'C:/Users/rosenbus/surfdrive/pythanalytics/11 humor/scripts/new material stimuli.jpg',
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE)

#average correlation between two jokes rated by the same rater

# cl <- parallel::makeCluster(3)
# doParallel::registerDoParallel(cl)
# library(foreach)
# two_materials_same_rater = foreach(seed = 1:1000000, .combine = 'rbind') %do%{
#   if(seed %% 10000 == 0){print(seed/10000)}
#   set.seed(seed)
#   samplerater = sample(1:nrow(df_wide), 1)
#   valid_ratings = df_wide[samplerater, as.vector(!is.na(df_wide[samplerater, ]))]
#   if(ncol(valid_ratings)!=0){
#     c(sample(unlist(valid_ratings), 2), samplerater)}else{
#       c(NA,NA,NA)
#     }
# }
# doParallel::stopImplicitCluster()
# rm(cl)
# beepr::beep()
# save(two_materials_same_rater, file="C:/Users/rosenbus/surfdrive/pythanalytics/11 humor/data/two_materials_same_rater.Rdata")


load("C:/Users/rosenbus/surfdrive/pythanalytics/11 humor/data/two_materials_same_rater.Rdata")
cor.test(as.numeric(two_materials_same_rater[,1]), as.numeric(two_materials_same_rater[,2]), use = 'pairwise.complete')
df_wide$rater = raters

#heatmap
colnames(two_materials_same_rater) = row.names(two_materials_same_rater) = NULL
binsA2 = two_materials_same_rater[,1]
binsB2 = two_materials_same_rater[,2]
hmapdata2 = gmodels::CrossTable(binsA2, binsB2)$prop.col
hmapdata2 = as.matrix(as.data.frame.matrix(hmapdata2))

sum(hmapdata2[,2])
sum(hmapdata2[2,])

h2 = data.table::melt(hmapdata2)
library(ggplot2)
ggplot(h2, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value)) + 
  geom_text(size = 6,aes(fill = h2$value, label = round(h2$value, 2))) + 
  scale_fill_gradient(low = "white", high = "gray40") + 
  theme(panel.grid.major.x=element_blank(), 
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill="white"), 
        axis.title.x = element_text(hjust = 0.5,vjust=4), 
        axis.title.y = element_text(hjust = 0.5, vjust = 2), 
        
        axis.text.x = element_text(hjust = 0,vjust=5,size = 12,face = "bold"), 
        axis.text.y = element_text(size = 12,face = "bold", hjust = 5,vjust=0)) + 
  theme(legend.title=element_text(face="bold", size=14), text = element_text(size = 17)) + 
  ggtitle('Relationship between two ratings of same rater')+
  coord_flip() +
  labs(fill="P(B|A)") +
  scale_x_continuous(name= 'Rating B',breaks = c(1:7), labels = c('1 ','2 ','3 ','4 ','5 ','6 ','7 '), limits = c(0.5,7.5))+
  scale_y_continuous(name= 'Rating A',breaks = c(1:7), labels = c('1 ','2 ','3 ','4 ','5 ','6 ','7 '), limits = c(0.5,7.5))

ggsave(
  'C:/Users/rosenbus/surfdrive/pythanalytics/11 humor/scripts/new rater stimuli.jpg',
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm"),
  dpi = 300,
  limitsize = TRUE)


# OUTDATED confidence intervals ----------------------------------------------------

#iterations
rater_iccs = c()
joke_iccs = c()
inter_iccs = c()
correct_name = function(x){unlist(stringr::str_split(x, pattern = 'sep'))[1]}
for(iteration in 1:2){
  set.seed(iteration)
  print(iteration)
  #take bootstrap sample of participants and jokes
  participants = sample(1:nrow(df_wide), nrow(df_wide), replace = T)
  jokes = sample(1:(ncol(df_wide)-1), ncol(df_wide)-1, replace = T)
  bootdf = df_wide[participants, jokes]
  colnames(bootdf) = colnames(df_wide)[jokes]
  bootdf$rater = df_wide$rater[participants]
  for(i in 1:ncol(bootdf)){
    colnames(bootdf)[i] = paste0(colnames(bootdf)[i], 'sep',as.character(i))
  }
  colnames(bootdf)
  df_long_boot = tidyr::pivot_longer(bootdf, cols = colnames(bootdf)[1:ncol(bootdf)-1], names_to = 'word')
  df_long_boot$word = sapply(df_long_boot$word, correct_name)
  df_long_boot = rename(df_long_boot,  rater = ratersep106)
  #fit model
  model = lme4::lmer(value~ (1|rater) + (1|word) + (1|rater:word), data=df_long_boot)
  sigm = (sigma(model))^2
  
  r = summary(model)$varcor$rater[1]
  j = summary(model)$varcor$word[1]
  inter = summary(model)$varcor$'rater:word'[1]
  raicc = r / (r + j + sigm + inter)
  joicc = j / (r + j + sigm + inter)
  inicc = inter / (r + j + sigm + inter)
  rater_iccs = c(rater_iccs, raicc)
  joke_iccs = c(joke_iccs, joicc)
  inter_iccs = c(inter_iccs, inicc)
}
beepr::beep()
