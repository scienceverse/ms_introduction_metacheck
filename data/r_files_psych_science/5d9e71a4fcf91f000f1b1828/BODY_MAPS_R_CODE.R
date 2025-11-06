###R Code for Body Maps of Moral Concerns
###See https://osf.io/zbv6e/ for more info

message("loading/installing libraries--Study 1")
rm(list=ls())
if(!"pacman" %in% installed.packages()){
  install.packages("pacman")
  library(pacman)}

pacman::p_load(
  "tidyverse",
  "psych",
  "readr",
  "e1071",
  "data.table",
  "caret",
  "plyr",
  "EnvStats",
  "glmnet"
)


embody_data <- read_csv("aida_data_embody.csv")
describe(embody_data)
embody_data_2 <- embody_data%>%
  dplyr::select(-starts_with("q"), -userID,-timestamp, -age, -gender_fm,-gender_other,-conf_code)

emdata_new = embody_data%>%
  dplyr::select(-timestamp, -gender_other,-conf_code)%>%
  dplyr::filter(q2_16 == 0, q3_16 == 3)%>%
  dplyr::mutate(MFQ_H = (q2_0+q2_6+q2_11+q3_0+q3_6+q3_11)/6,
                MFQ_F = (q2_1+q2_7+q2_12+q3_1+q3_7+q3_12)/6,
                MFQ_I = (q2_2+q2_8+q2_13+q3_2+q3_8+q3_13)/6,
                MFQ_A = (q2_3+q2_9+q2_14+q3_3+q3_9+q3_14)/6,
                MFQ_P = (q2_4+q2_10+q2_15+q3_4+q3_10+q3_15)/6,
                MorallyWrong = q1_0,
                StrongResponse = q1_1,
                Political = (q4_0+q4_1)/2,
                Religiosity = q4_2
  )%>%
  dplyr::select(-starts_with("q"))%>%
  dplyr::mutate(Condition = ifelse (stimulus_index %in% c(0,1,2,3),"Care",
                                    ifelse(stimulus_index %in% c(4,5,6,7), "Fairness",
                                           ifelse(stimulus_index %in% c(8,9,10,11), "Loyalty",
                                                  ifelse(stimulus_index %in% c(12,13,14,15), "Authority", "Purity"))))
  )
psych::alpha(embody_data[,c('q2_0', 'q2_6', 'q2_11', 'q3_0', 'q3_6', 'q3_11')])
psych::alpha(embody_data[,c('q2_1','q2_7','q2_12','q3_1','q3_7','q3_12')])
psych::alpha(embody_data[,c('q2_2', 'q2_8', 'q2_13', 'q3_2', 'q3_8', 'q3_13')])
psych::alpha(embody_data[,c('q2_3', 'q2_9', 'q2_14', 'q3_3', 'q3_9', 'q3_14')])
psych::alpha(embody_data[,c('q2_4', 'q2_10', 'q2_15', 'q3_4', 'q3_10', 'q3_15')])
psych::alpha(embody_data[,c('q4_0', 'q4_1')])
psych::describe(embody_data$q1_0)
psych::describe(embody_data$q1_1)

emdata = emdata_new[,c(1,2,3,982:990,4:980)]  
embody_meta = dplyr::select(emdata,c(1:13))
embody_meta = dplyr::mutate(embody_meta, Condition = ifelse (stimulus_index %in% c(0,1,2,3),"Care",
                                                             ifelse(stimulus_index %in% c(4,5,6,7), "Fairness",
                                                                    ifelse(stimulus_index %in% c(8,9,10,11), "Loyalty",
                                                                           ifelse(stimulus_index %in% c(12,13,14,15), "Authority", "Purity"))))
)
embody_meta$moralresponse = (embody_meta$MorallyWrong+embody_meta$StrongResponse)/2
describe(embody_meta$moralresponse)
embody_meta = dplyr::mutate(embody_meta,
                            HardResponder = ifelse(embody_meta$moralresponse>4.11,"Strong",
                                                   ifelse(moralresponse<2.07,"Weak","Moderate")))
write_csv(embody_meta,"embody_meta.csv")
embody_meta = read_csv('embody_meta.csv')
#checking 
#based on strength of emotional response to stimuli 
Pixels <- read_csv("C:/Users/mohammad/Desktop/Pixels.csv")

embody_temp = left_join(Pixels,embody_meta,by = "userID")
embody_Care = dplyr::filter(embody_temp,Condition.x == "Care")


###FAIRNESS
embody_Fairness = left_join(Pixels,embody_meta,by = "userID")
embody_Fairness = dplyr::filter(embody_Fairness,Condition.x == "Fairness")


###Loyalty
embody_Loyalty = left_join(Pixels,embody_meta,by = "userID")
embody_Loyalty = dplyr::filter(embody_Loyalty,Condition.x == "Loyalty")

###Authority
embody_Authority = left_join(Pixels,embody_meta,by = "userID")
embody_Authority = dplyr::filter(embody_Authority,Condition.x == "Authority")

###Purity
embody_Purity = left_join(Pixels,embody_meta,by = "userID")
embody_Purity = dplyr::filter(embody_Purity,Condition.x == "Purity")

##writing different subsets 
embody_Care = subset(embody_temp,Condition.y =="Care")
embody_Care = subset(embody_Care,Political<=(3.83-1.86)|Political>=(3.83+1.86))
embody_Care$ideology = ifelse(embody_Care$Political<=(3.83-1.86),"Liberal","Conservative")
embody_Care$ideology
embody_Care = subset(embody_Care,select = c('0':'48953','ideology'))
dim(embody_Care)
write_csv(embody_Care,"embody_Care.csv")

embody_Fairness = subset(embody_temp,Condition.y =="Fairness")
embody_Fairness = subset(embody_Fairness,Political<=(3.83-1.86)|Political>=(3.83+1.86))
embody_Fairness$ideology = ifelse(embody_Fairness$Political<=(3.83-1.86),"Liberal","Conservative")
embody_Fairness$ideology
embody_Fairness = subset(embody_Fairness,select = c('0':'48953','ideology'))
write_csv(embody_Fairness,"embody_Fairness.csv")

embody_Loyalty = subset(embody_temp,Condition.y =="Loyalty")
embody_Loyalty = subset(embody_Loyalty,Political<=(3.83-1.86)|Political>=(3.83+1.86))
embody_Loyalty$ideology = ifelse(embody_Loyalty$Political<=(3.83-1.86),"Liberal","Conservative")
embody_Loyalty$ideology
embody_Loyalty = subset(embody_Loyalty,select = c('0':'48953','ideology'))
write_csv(embody_Loyalty,"embody_Loyalty.csv")

embody_Authority = subset(embody_temp,Condition.y =="Authority")
embody_Authority = subset(embody_Authority,Political<=(3.83-1.86)|Political>=(3.83+1.86))
embody_Authority$ideology = ifelse(embody_Authority$Political<=(3.83-1.86),"Liberal","Conservative")
embody_Authority$ideology
embody_Authority = subset(embody_Authority,select = c('0':'48953','ideology'))
write_csv(embody_Authority,"embody_Authority.csv")


embody_Purity = subset(embody_temp,Condition.y =="Purity")
embody_Purity = subset(embody_Purity,Political<=(3.83-1.86)|Political>=(3.83+1.86))
embody_Purity$ideology = ifelse(embody_Purity$Political<=(3.83-1.86),"Liberal","Conservative")
embody_Purity$ideology
embody_Purity = subset(embody_Purity,select = c('0':'48953','ideology'))
write_csv(embody_Purity,"embody_Purity.csv")


m.care = mean(embody_temp$MFQ_H)
m.fairness = mean(embody_temp$MFQ_F)
m.loyalty = mean(embody_temp$MFQ_I)
m.authority = mean(embody_temp$MFQ_A)
m.purity = mean(embody_temp$MFQ_P)
sd.care = sd(embody_temp$MFQ_H)
sd.fairness = sd(embody_temp$MFQ_F)
sd.loyalty = sd(embody_temp$MFQ_I)
sd.authority = sd(embody_temp$MFQ_I)
sd.purity = sd(embody_temp$MFQ_P)

highlow_Care = subset(embody_temp,Condition.y =="Care")
highlow_Care = subset(highlow_Care,MFQ_H>(m.care+sd.care)|MFQ_H<(m.care-sd.care))
highlow_Care$intensity = ifelse(highlow_Care$MFQ_H>(m.care+sd.care),"High","Low")
highlow_Care$intensity
highlow_Care = subset(highlow_Care,select = c('0':'48953','intensity'))
write_csv(highlow_Care,"highlow_Care.csv")

highlow_Fairness = subset(embody_temp,Condition.y =="Fairness")
highlow_Fairness = subset(highlow_Fairness,MFQ_F>(m.fairness+sd.fairness)|MFQ_F<(m.fairness-sd.fairness))
highlow_Fairness$intensity = ifelse(highlow_Fairness$MFQ_F>(m.fairness+sd.fairness),"High","Low")
highlow_Fairness$intensity
highlow_Fairness = subset(highlow_Fairness,select = c('0':'48953','intensity'))
write_csv(highlow_Fairness,"highlow_Fairness.csv")

highlow_Loyalty = subset(embody_temp,Condition.y =="Loyalty")
highlow_Loyalty = subset(highlow_Loyalty,MFQ_I>(m.loyalty+sd.loyalty)|MFQ_I<(m.loyalty-sd.loyalty))
highlow_Loyalty$intensity = ifelse(highlow_Loyalty$MFQ_I>(m.loyalty+sd.loyalty),"High","Low")
highlow_Loyalty$intensity
highlow_Loyalty = subset(highlow_Loyalty,select = c('0':'48953','intensity'))
write_csv(highlow_Loyalty,"highlow_Loyalty.csv")

highlow_Authority = subset(embody_temp,Condition.y =="Authority")
highlow_Authority = subset(highlow_Authority,MFQ_A>(m.authority+sd.authority)|MFQ_A<(m.authority-sd.authority))
highlow_Authority$intensity = ifelse(highlow_Authority$MFQ_A>(m.authority+sd.authority),"High","Low")
highlow_Authority$intensity
highlow_Authority = subset(highlow_Authority,select = c('0':'48953','intensity'))
write_csv(highlow_Authority,"highlow_Authority.csv")

highlow_Purity = subset(embody_temp,Condition.y =="Purity")
highlow_Purity = subset(highlow_Purity,MFQ_P>(m.purity+sd.purity)|MFQ_P<(m.purity-sd.purity))
highlow_Purity$intensity = ifelse(highlow_Purity$MFQ_P>(m.purity+sd.purity),"High","Low")
highlow_Purity$intensity
highlow_Purity = subset(highlow_Purity,select = c('0':'48953','intensity'))
write_csv(highlow_Purity,"highlow_Purity.csv")



#datafilename <- file.choose()
#Pixels <- fread(datafilename,sep=",")
Pixels <- fread("Pixels.csv",sep=",")

###CARE
Pixels[,userID:=NULL]
Y <- factor(Pixels$Condition)
Pixels[,Condition:=NULL]
nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]

YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Care","Other","Other","Other"))

Care <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Care), style = 3)
for(i in 1:dim(Care)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  CareSVM <- tune.out$best.model
  temp<-confusionMatrix(YTest, predict(CareSVM,testing))
  Care[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Care)
oneSamplePermutationTest(Care,alternative = "greater",mu = .50)


###FAIRNESS
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Other","Fairness","Other","Other"))

Fairness <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Fairness), style = 3)
for(i in 1:dim(Fairness)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  FairnessSVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(FairnessSVM,testing))
  Fairness[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Fairness)
sd(Fairness)
oneSamplePermutationTest(Fairness,alternative = "greater",mu = .50)

###LOYALTY
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Other","Other","Loyalty","Other"))

Loyalty <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Loyalty), style = 3)
for(i in 1:dim(Loyalty)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  LoyaltySVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(LoyaltySVM,testing))
  Loyalty[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Loyalty)
oneSamplePermutationTest(Loyalty,alternative = "greater",mu = .50)


###AUTHORITY
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Authority", "Other","Other","Other","Other"))

Authority <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Authority), style = 3)
for(i in 1:dim(Authority)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  AuthoritySVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(AuthoritySVM,testing))
  Authority[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Authority)
oneSamplePermutationTest(Authority,alternative = "greater",mu = .50)


###PURITY
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Other","Other","Other","Purity"))

Purity <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Purity), style = 3)
for(i in 1:dim(Purity)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  PuritySVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(PuritySVM,testing))
  Purity[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Purity)
sd(Purity)
var(Purity)
oneSamplePermutationTest(Purity,alternative = "greater",mu = .50)



# SVMs for political orientation 

Pixels <- fread("embody_Purity.csv",sep=",")
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
length(Y[Y=="Liberal"])
length(Y[Y=="Conservative"])
Pixels[,V48955:=NULL]
dim(Pixels)

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
pure <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  pure[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(pure)
sd(pure)
oneSamplePermutationTest(pure,alternative = "greater",mu = .50)





Pixels <- fread("embody_Care.csv",sep=",")
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
length(Y[Y=="Liberal"])
Pixels[,V48955:=NULL]
dim(Pixels)

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
care1 <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  care1[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(care1)
sd(care1)
oneSamplePermutationTest(care1,alternative = "greater",mu = .50)



Pixels <- fread("embody_Authority.csv",sep=",")
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
length(Y[Y=="Liberal"])
Pixels[,V48955:=NULL]
dim(Pixels)

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
author <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  author[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(author)
sd(author)
oneSamplePermutationTest(author,alternative = "greater",mu = .50)




Pixels <- fread("embody_Fairness.csv",sep=",")
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
length(Y[Y=="Liberal"])
Pixels[,V48955:=NULL]
dim(Pixels)

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
fair <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  fair[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(fair)
sd(fair)
oneSamplePermutationTest(fair,alternative = "greater",mu = .50)




Pixels <- fread("embody_Loyalty.csv",sep=",")
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
length(Y[Y=="Liberal"])
Pixels[,V48955:=NULL]
dim(Pixels)

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
loyal <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  loyal[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}


mean(loyal)
sd(loyal)
oneSamplePermutationTest(loyal,alternative = "greater",mu = .50)


# SVMs for MFQ 


#care
Pixels <- fread("highlow_Care.csv",sep=",")
dim(Pixels)
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
Pixels[,V48955:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highcare <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highcare[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highcare)
sd(highcare)
oneSamplePermutationTest(highcare,alternative = "greater",mu = .50)

#fairness
Pixels <- fread("highlow_Fairness.csv",sep=",")
dim(Pixels)
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
Pixels[,V48955:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highfairness <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highfairness[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highfairness)
sd(highfairness)
oneSamplePermutationTest(highfairness,alternative = "greater",mu = .50)

#loyalty
Pixels <- fread("highlow_Loyalty.csv",sep=",")
dim(Pixels)
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
Pixels[,V48955:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highloyalty <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highloyalty[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highloyalty)
sd(highloyalty)
oneSamplePermutationTest(highloyalty,alternative = "greater",mu = .50)


#authority
Pixels <- fread("highlow_Authority.csv",sep=",")
dim(Pixels)
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
Pixels[,V48955:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highauthority <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10),gamma = c(0.01,0.1,1)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highauthority[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highauthority)
sd(highauthority)
oneSamplePermutationTest(highauthority,alternative = "greater",mu = .50)


#purity
Pixels <- fread("highlow_Purity.csv",sep=",")
dim(Pixels)
Pixels <- Pixels[-1,]
Y <- factor(Pixels$V48955)
Pixels[,V48955:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highpurity <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.9, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, ranges =list(cost=c(0.1,1,10)))
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highpurity[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highpurity)
sd(highpurity)
oneSamplePermutationTest(highpurity,alternative = "greater",mu = .50)



###################################################
##################   STUDY 2  #####################
###################################################

message("analyzing Study 2...")
rm(list=ls())

setwd("C:/Users/mohammad/Desktop/EMBODY-STUDY2")
emb2_all_data <- read_csv("emb2_all_data.csv")

embody_data=emb2_all_data

embody_data[17,c("Validation","conf_code")]

embody_data_2 <- embody_data%>%
  dplyr::select(-starts_with("q"),
                -StartDate,
                -EndDate,
                -Status,
                -IPAddress,
                -Progress,
                -contains("Duration"),
                -Finished,
                -contains("Recipient"),
                -RecordedDate,
                -ResponseId,
                -contains("Location"),
                -DistributionChannel,
                -UserLanguage,
                -contains("age"),
                -starts_with("SECS"),
                -contains("ender"),
                -Validation,
                -start("Lang"),
                -starts_with("Humil"),
                -starts_with("Dise"),
                -SC0,
                -opp,
                -rid,
                -RISN,
                -V,
                -Q_TotalDuration,
                -gc,
                -term,
                -userID,
                -timestamp,
                -conf_code,
                -contains("race"),
                -ExternalReference,
                -political, 
                -Lang_var)

names(embody_data_2[,1:120])
names(embody_data_2[,48940:48956])

emdata = embody_data%>%
  dplyr::select(-userID,-timestamp, -gender_other,-conf_code)%>%
  #filter(q2_16 == 0, q3_16 == 3)%>%
  dplyr::mutate(MFQ_H = (q2_0+q2_6+q2_11+q3_0+q3_6+q3_11)/6,
                MFQ_F = (q2_1+q2_7+q2_12+q3_1+q3_7+q3_12)/6,
                MFQ_I = (q2_2+q2_8+q2_13+q3_2+q3_8+q3_13)/6,
                MFQ_A = (q2_3+q2_9+q2_14+q3_3+q3_9+q3_14)/6,
                MFQ_P = (q2_4+q2_10+q2_15+q3_4+q3_10+q3_15)/6,
                MorallyWrong = q1_0,
                StrongResponse = q1_1,
                Political = (q4_0+q4_1)/2,
                Religiosity = q4_2
  )%>%
  dplyr::select(-starts_with("q"))%>%
  dplyr::mutate(Condition = ifelse (stimulus_index %in% c(0,1,2,3),"Care",
                                    ifelse(stimulus_index %in% c(4,5,6,7), "Fairness",
                                           ifelse(stimulus_index %in% c(8,9,10,11), "Loyalty",
                                                  ifelse(stimulus_index %in% c(12,13,14,15), "Authority", "Purity"))))
  )

embdata2 = emdata[,c(58:49011,49021)] 
Pixels_2 = emdata[,c(25,58:49011,49021)]

table(embdata2$Condition)
embdata2$Condition = as.factor(embdata2$Condition)
#Care
embody_Care_2 = embdata2%>%
  dplyr::filter(Condition=="Care")
nrow(embody_Care_2)
#Fairness
embody_Fairness_2 = embdata2%>%
  dplyr::filter(Condition=="Fairness")
nrow(embody_Fairness_2)
#Loyalty
embody_Loyalty_2 = embdata2%>%
  dplyr::filter(Condition=="Loyalty")
nrow(embody_Loyalty_2)
#Authority
embody_Authority_2 = embdata2%>%
  dplyr::filter(Condition=="Authority")
nrow(embody_Authority_2)
#Purity
embody_Purity_2 = embdata2%>%
  dplyr::filter(Condition=="Purity")
nrow(embody_Purity_2)

#meta file 
embody_data$SECS_1_RE = 100-embody_data$SECS_1
embody_data$SECS_5_RE = 100-embody_data$SECS_5

embody_meta_2 <- embody_data[,c(1:101,49056,49057)]%>%
  dplyr::select(starts_with("q"),
                -StartDate,
                -EndDate,
                -Status,
                -IPAddress,
                -Progress,
                -contains("Duration"),
                -Finished,
                -contains("Recipient"),
                -RecordedDate,
                -ResponseId,
                -contains("Location"),
                -DistributionChannel,
                -UserLanguage,
                -contains("age"),
                starts_with("SECS"),
                -contains("ender"),
                Validation,
                -start("Lang"),
                -starts_with("Humil"),
                -starts_with("Dise"),
                -SC0,
                -opp,
                -rid,
                -RISN,
                -V,
                -Q_TotalDuration,
                -gc,
                -term,
                -userID,
                -timestamp,
                -conf_code,
                -contains("race"),
                -ExternalReference,
                -political, 
                -Lang_var,
                stimulus_index)%>%
  dplyr::mutate(MFQ_H = (q2_0+q2_6+q2_11+q3_0+q3_6+q3_11)/6,
                MFQ_F = (q2_1+q2_7+q2_12+q3_1+q3_7+q3_12)/6,
                MFQ_I = (q2_2+q2_8+q2_13+q3_2+q3_8+q3_13)/6,
                MFQ_A = (q2_3+q2_9+q2_14+q3_3+q3_9+q3_14)/6,
                MFQ_P = (q2_4+q2_10+q2_15+q3_4+q3_10+q3_15)/6,
                MorallyWrong = q1_0,
                StrongResponse = q1_1,
                #Political = (q4_0+q4_1)/2,
                Religiosity = q4_2,
                Conserv_SECS = rowMeans(subset(embody_data, select=c(SECS_1_RE,
                                                                     SECS_2,
                                                                     SECS_3,
                                                                     SECS_4,
                                                                     SECS_5_RE,
                                                                     SECS_6,
                                                                     SECS_7,
                                                                     SECS_8,
                                                                     SECS_9,
                                                                     SECS_10,
                                                                     SECS_11,
                                                                     SECS_12)),na.rm = T))%>%
  dplyr::select(-starts_with("q"))%>%
  dplyr::select(-starts_with("SECS_"))%>%
  dplyr::mutate(Condition = ifelse (stimulus_index %in% c(0,1,2,3),"Care",
                                    ifelse(stimulus_index %in% c(4,5,6,7), "Fairness",
                                           ifelse(stimulus_index %in% c(8,9,10,11), "Loyalty",
                                                  ifelse(stimulus_index %in% c(12,13,14,15), "Authority", "Purity")
                                           )
                                    )
  )
  )

M_SECS = mean(embody_meta_2$Conserv_SECS, na.rm=T)
SD_SECS = sd(embody_meta_2$Conserv_SECS, na.rm = T)
write_csv(embody_meta_2,"embody_meta_2.csv")

embody_temp = left_join(Pixels_2,embody_meta_2,by = "Validation")
names(embody_temp[,c(1:120,48950:48967)])

embody_temp$PoliticalAff = ifelse(embody_temp$Conserv_SECS>=(M_SECS+SD_SECS),"Conservative",
                                  ifelse(embody_temp$Conserv_SECS<=(M_SECS-SD_SECS),"Liberal","Moderate"))
table(embody_temp$PoliticalAff)
embody_care = dplyr::filter(embody_temp,Condition.x == "Care")
embody_fairness = dplyr::filter(embody_temp, Condition.x == "Fairness")
embody_loyalty = dplyr::filter(embody_temp, Condition.x == "Loyalty")
embody_authority = dplyr::filter(embody_temp, Condition.x == "Authority")
embody_purity = dplyr::filter(embody_temp, Condition.x == "Purity")

##figures for liberals and conservatives across foundations
#Care

embody_care_liberal = dplyr::filter(embody_care,PoliticalAff=="Liberal")
embody_care_conservative = dplyr::filter(embody_care, PoliticalAff=="Conservative")

#fairness

embody_fairness_liberal = dplyr::filter(embody_fairness,PoliticalAff=="Liberal")
embody_fairness_conservative = dplyr::filter(embody_fairness, PoliticalAff=="Conservative")

#loyalty

embody_loyalty_liberal = dplyr::filter(embody_loyalty,PoliticalAff=="Liberal")
embody_loyalty_conservative = dplyr::filter(embody_loyalty, PoliticalAff=="Conservative")

#authority

embody_authority_liberal = dplyr::filter(embody_authority,PoliticalAff=="Liberal")
embody_authority_conservative = dplyr::filter(embody_authority, PoliticalAff=="Conservative")

#purity

embody_purity_liberal = dplyr::filter(embody_purity,PoliticalAff=="Liberal")
embody_purity_conservative = dplyr::filter(embody_purity, PoliticalAff=="Conservative")

# zeros datasets for visualization 
names(embody_care_liberal[,c(1:30,48950:48968)])
#care
care_lib=embody_care_liberal%>%dplyr::select(-Validation,
                                             -starts_with("MFQ"),
                                             -starts_with("Condition"),
                                             -stimulus_index,
                                             -MorallyWrong,
                                             -StrongResponse,
                                             -Religiosity,
                                             -Conserv_SECS,
                                             -PoliticalAff)

care_lib = t(scale(t(care_lib),center = T,scale = T)*100)%>%as.data.frame()

zero_care_liberal_tvalues = sapply(care_lib[,c(1:ncol(care_lib))], 
                                   function(i) t.test(i,mu=0,data = care_lib)$statistic[[1]])
zero_care_liberal_tvalues = as.data.frame(zero_care_liberal_tvalues)
psych::describe(zero_care_liberal_tvalues)
zero_care_liberal_effsize = zero_care_liberal_tvalues/sqrt(nrow(care_lib))
write_csv(zero_care_liberal_effsize,"zero_care_liberal_effsize.csv")


care_con = embody_care_conservative%>%dplyr::select(-Validation,
                                                    -starts_with("MFQ"),
                                                    -starts_with("Condition"),
                                                    -stimulus_index,
                                                    -MorallyWrong,
                                                    -StrongResponse,
                                                    -Religiosity,
                                                    -Conserv_SECS,
                                                    -PoliticalAff)
care_con = t(scale(t(care_con),center = T,scale = T)*100)%>%as.data.frame()
zero_care_conservative_tvalues = sapply(care_con[,c(1:ncol(care_con))], 
                                        function(i) t.test(i,mu=0,data = care_con)$statistic[[1]])
zero_care_conservative_tvalues = as.data.frame(zero_care_conservative_tvalues)
psych::describe(zero_care_conservative_tvalues)
zero_care_conservative_effsize = zero_care_conservative_tvalues/sqrt(nrow(care_con))
write_csv(zero_care_conservative_effsize,"zero_care_conservative_effsize.csv")
#fairness
fairness_lib=embody_fairness_liberal%>%dplyr::select(-Validation,
                                                     -starts_with("MFQ"),
                                                     -starts_with("Condition"),
                                                     -stimulus_index,
                                                     -MorallyWrong,
                                                     -StrongResponse,
                                                     -Religiosity,
                                                     -Conserv_SECS,
                                                     -PoliticalAff)

fairness_lib = t(scale(t(fairness_lib),center = T,scale = T)*100)%>%as.data.frame()

zero_fairness_liberal_tvalues = sapply(fairness_lib[,c(1:ncol(fairness_lib))], 
                                       function(i) t.test(i,mu=0,data = fairness_lib)$statistic[[1]])
zero_fairness_liberal_tvalues = as.data.frame(zero_fairness_liberal_tvalues)
psych::describe(zero_fairness_liberal_tvalues)
zero_fairness_liberal_effsize = zero_fairness_liberal_tvalues/sqrt(nrow(fairness_lib))
write_csv(zero_fairness_liberal_effsize,"zero_fairness_liberal_effsize.csv")


fairness_con = embody_fairness_conservative%>%dplyr::select(-Validation,
                                                            -starts_with("MFQ"),
                                                            -starts_with("Condition"),
                                                            -stimulus_index,
                                                            -MorallyWrong,
                                                            -StrongResponse,
                                                            -Religiosity,
                                                            -Conserv_SECS,
                                                            -PoliticalAff)
fairness_con = t(scale(t(fairness_con),center = T,scale = T)*100)%>%as.data.frame()
zero_fairness_conservative_tvalues = sapply(fairness_con[,c(1:ncol(fairness_con))], 
                                            function(i) t.test(i,mu=0,data = fairness_con)$statistic[[1]])
zero_fairness_conservative_tvalues = as.data.frame(zero_fairness_conservative_tvalues)
psych::describe(zero_fairness_conservative_tvalues)
zero_fairness_conservative_effsize = zero_fairness_conservative_tvalues/sqrt(nrow(fairness_con))
write_csv(zero_fairness_conservative_effsize,"zero_fairness_conservative_effsize.csv")
#loyalty
loyalty_lib=embody_loyalty_liberal%>%dplyr::select(-Validation,
                                                   -starts_with("MFQ"),
                                                   -starts_with("Condition"),
                                                   -stimulus_index,
                                                   -MorallyWrong,
                                                   -StrongResponse,
                                                   -Religiosity,
                                                   -Conserv_SECS,
                                                   -PoliticalAff)

loyalty_lib = t(scale(t(loyalty_lib),center = T,scale = T)*100)%>%as.data.frame()

zero_loyalty_liberal_tvalues = sapply(loyalty_lib[,c(1:ncol(loyalty_lib))], 
                                      function(i) t.test(i,mu=0,data = loyalty_lib)$statistic[[1]])
zero_loyalty_liberal_tvalues = as.data.frame(zero_loyalty_liberal_tvalues)
psych::describe(zero_loyalty_liberal_tvalues)
zero_loyalty_liberal_effsize = zero_loyalty_liberal_tvalues/sqrt(nrow(loyalty_lib))
write_csv(zero_loyalty_liberal_effsize,"zero_loyalty_liberal_effsize.csv")


loyalty_con = embody_loyalty_conservative%>%dplyr::select(-Validation,
                                                          -starts_with("MFQ"),
                                                          -starts_with("Condition"),
                                                          -stimulus_index,
                                                          -MorallyWrong,
                                                          -StrongResponse,
                                                          -Religiosity,
                                                          -Conserv_SECS,
                                                          -PoliticalAff)
loyalty_con = t(scale(t(loyalty_con),center = T,scale = T)*100)%>%as.data.frame()
zero_loyalty_conservative_tvalues = sapply(loyalty_con[,c(1:ncol(loyalty_con))], 
                                           function(i) t.test(i,mu=0,data = loyalty_con)$statistic[[1]])
zero_loyalty_conservative_tvalues = as.data.frame(zero_loyalty_conservative_tvalues)
psych::describe(zero_loyalty_conservative_tvalues)
zero_loyalty_conservative_effsize = zero_loyalty_conservative_tvalues/sqrt(nrow(loyalty_con))
write_csv(zero_loyalty_conservative_effsize,"zero_loyalty_conservative_effsize.csv")
#authority
authority_lib=embody_authority_liberal%>%dplyr::select(-Validation,
                                                       -starts_with("MFQ"),
                                                       -starts_with("Condition"),
                                                       -stimulus_index,
                                                       -MorallyWrong,
                                                       -StrongResponse,
                                                       -Religiosity,
                                                       -Conserv_SECS,
                                                       -PoliticalAff)

authority_lib = t(scale(t(authority_lib),center = T,scale = T)*100)%>%as.data.frame()

zero_authority_liberal_tvalues = sapply(authority_lib[,c(1:ncol(authority_lib))], 
                                        function(i) t.test(i,mu=0,data = authority_lib)$statistic[[1]])
zero_authority_liberal_tvalues = as.data.frame(zero_authority_liberal_tvalues)
psych::describe(zero_authority_liberal_tvalues)
zero_authority_liberal_effsize = zero_authority_liberal_tvalues/sqrt(nrow(authority_lib))
write_csv(zero_authority_liberal_effsize,"zero_authority_liberal_effsize.csv")


authority_con = embody_authority_conservative%>%dplyr::select(-Validation,
                                                              -starts_with("MFQ"),
                                                              -starts_with("Condition"),
                                                              -stimulus_index,
                                                              -MorallyWrong,
                                                              -StrongResponse,
                                                              -Religiosity,
                                                              -Conserv_SECS,
                                                              -PoliticalAff)
authority_con = t(scale(t(authority_con),center = T,scale = T)*100)%>%as.data.frame()
zero_authority_conservative_tvalues = sapply(authority_con[,c(1:ncol(authority_con))], 
                                             function(i) t.test(i,mu=0,data = authority_con)$statistic[[1]])
zero_authority_conservative_tvalues = as.data.frame(zero_authority_conservative_tvalues)
psych::describe(zero_authority_conservative_tvalues)
zero_authority_conservative_effsize = zero_authority_conservative_tvalues/sqrt(nrow(authority_con))
write_csv(zero_authority_conservative_effsize,"zero_authority_conservative_effsize.csv")
#purity
purity_lib=embody_purity_liberal%>%dplyr::select(-Validation,
                                                 -starts_with("MFQ"),
                                                 -starts_with("Condition"),
                                                 -stimulus_index,
                                                 -MorallyWrong,
                                                 -StrongResponse,
                                                 -Religiosity,
                                                 -Conserv_SECS,
                                                 -PoliticalAff)

purity_lib = t(scale(t(purity_lib),center = T,scale = T)*100)%>%as.data.frame()

zero_purity_liberal_tvalues = sapply(purity_lib[,c(1:ncol(purity_lib))], 
                                     function(i) t.test(i,mu=0,data = purity_lib)$statistic[[1]])
zero_purity_liberal_tvalues = as.data.frame(zero_purity_liberal_tvalues)
psych::describe(zero_purity_liberal_tvalues)
zero_purity_liberal_effsize = zero_purity_liberal_tvalues/sqrt(nrow(purity_lib))
write_csv(zero_purity_liberal_effsize,"zero_purity_liberal_effsize.csv")

purity_con = embody_purity_conservative%>%dplyr::select(-Validation,
                                                        -starts_with("MFQ"),
                                                        -starts_with("Condition"),
                                                        -stimulus_index,
                                                        -MorallyWrong,
                                                        -StrongResponse,
                                                        -Religiosity,
                                                        -Conserv_SECS,
                                                        -PoliticalAff)
purity_con = t(scale(t(purity_con),center = T,scale = T)*100)%>%as.data.frame()
zero_purity_conservative_tvalues = sapply(purity_con[,c(1:ncol(purity_con))], 
                                          function(i) t.test(i,mu=0,data = purity_con)$statistic[[1]])
zero_purity_conservative_tvalues = as.data.frame(zero_purity_conservative_tvalues)
psych::describe(zero_purity_conservative_tvalues)
zero_purity_conservative_effsize = zero_purity_conservative_tvalues/sqrt(nrow(purity_con))
write_csv(zero_purity_conservative_effsize,"zero_purity_conservative_effsize.csv")

##SVM##

names(Pixels_2[,c(1:10,48953:48956)])
table(Pixels_2$Condition)
write.csv(Pixels_2,"Pixels_2.csv")
Pixels <-data.table::fread("Pixels_2.csv",sep=",",header = T)
Pixels[,Validation:=NULL]
Y <- factor(Pixels$Condition)
Pixels[,Condition:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]
##CARE vs OTHER
set.seed(666)
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Care","Other","Other","Other"))

Care <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Care), style = 3)
for(i in 1:dim(Care)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  CareSVM <- svm (x=training, y=YTrain,cost=1)
  temp<-confusionMatrix(YTest, predict(CareSVM,testing))
  Care[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}
mean(Care)
sd(Care)
oneSamplePermutationTest(Care, alternative = "greater", mu = .50)


##FAIRNESS vS OTHER
set.seed(666)
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Other","Fairness","Other","Other"))

Fairness <- array(dim=200)
pb <- txtProgressBar(min = 0, max = dim(Fairness), style = 3)
for(i in 1:dim(Fairness)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  FairnessSVM <- svm (x=training, y=YTrain, cost=1)
  temp<-confusionMatrix(YTest, predict(FairnessSVM,testing))
  Fairness[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Fairness)
sd(Fairness)
oneSamplePermutationTest(Fairness,alternative = "greater",mu = .50)

##LOYALTY VS OTHER
set.seed(666)
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Other","Other","Loyalty","Other"))

Loyalty <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Loyalty), style = 3)
for(i in 1:dim(Loyalty)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  LoyaltySVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(LoyaltySVM,testing))
  Loyalty[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Loyalty)
sd(Loyalty)
oneSamplePermutationTest(Loyalty,alternative = "greater",mu = .50)


##AUTHORITY VS OTHER
set.seed(666)
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Authority", "Other","Other","Other","Other"))

Authority <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Authority), style = 3)
for(i in 1:dim(Authority)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  AuthoritySVM <- svm (x=training, y=YTrain,cost=1)
  temp<-confusionMatrix(YTest, predict(AuthoritySVM,testing))
  Authority[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Authority)
sd(Authority)
oneSamplePermutationTest(Authority,alternative = "greater",mu = .50)


##PURITY VS OTHER
set.seed(666)
YTemp <- mapvalues(Y, from = c("Authority", "Care","Fairness","Loyalty","Purity"), 
                   to = c("Other", "Other","Other","Other","Purity"))

Purity <- array(dim=100)
pb <- txtProgressBar(min = 0, max = dim(Purity), style = 3)
for(i in 1:dim(Purity)){
  t<- downSample(Pixels,YTemp)
  downSampledY <- t$Class
  t <- subset(t, select = -c(Class))
  intrain <- createDataPartition(y =downSampledY, p= 0.9, list = FALSE)
  training <- t[intrain,]
  testing <- t[-intrain,]
  YTrain <- downSampledY[intrain]
  YTest <- downSampledY[-intrain]
  PuritySVM <- svm (x=training, y=YTrain, cost=1)
  temp<-confusionMatrix(YTest, predict(PuritySVM,testing))
  Purity[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(Purity)
sd(Purity)
oneSamplePermutationTest(Purity,alternative = "greater",mu = .50)


##Liberal vs. Conservative##
#CARE (Liberal vs. Conservative)
care_politics=embody_care%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity)%>%
  dplyr::filter(PoliticalAff %in%c("Liberal","Conservative"))
write_csv(care_politics,"care_politics.csv")
Pixels <- fread("care_politics.csv",sep=",", header = T)
Y <- factor(Pixels$PoliticalAff)
length(Y[Y=="Liberal"])
Pixels[,PoliticalAff:=NULL]
nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]

l<-100
care1 <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.8, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  care1[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(care1)
sd(care1)
oneSamplePermutationTest(care1,alternative = "greater",mu = .50)


#fairness
fairness_politics=embody_fairness%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity)%>%
  dplyr::filter(PoliticalAff %in%c("Liberal","Conservative"))
write_csv(fairness_politics,"fairness_politics.csv")
Pixels <- fread("fairness_politics.csv",sep=",", header = T)
Y <- factor(Pixels$PoliticalAff)
length(Y[Y=="Liberal"])
Pixels[,PoliticalAff:=NULL]
nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]

l<-100
fairness1 <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.8, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  fairness1[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(fairness1)
sd(fairness1)
oneSamplePermutationTest(fairness1,alternative = "greater",mu = .50)

#loyalty
loyalty_politics=embody_loyalty%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity)%>%
  dplyr::filter(PoliticalAff %in%c("Liberal","Conservative"))
write_csv(loyalty_politics,"loyalty_politics.csv")
Pixels <- fread("loyalty_politics.csv",sep=",", header = T)
Y <- factor(Pixels$PoliticalAff)
length(Y[Y=="Liberal"])
Pixels[,PoliticalAff:=NULL]
nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]

l<-100
loyalty1 <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.8, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  loyalty1[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(loyalty1)
sd(loyalty1)
oneSamplePermutationTest(loyalty1,alternative = "greater",mu = .50)

#authority
authority_politics=embody_authority%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity)%>%
  dplyr::filter(PoliticalAff %in%c("Liberal","Conservative"))
write_csv(authority_politics,"authority_politics.csv")
Pixels <- fread("authority_politics.csv",sep=",", header = T)
Y <- factor(Pixels$PoliticalAff)
length(Y[Y=="Liberal"])
Pixels[,PoliticalAff:=NULL]
nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]

l<-100
authority1 <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.8, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  authority1[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(authority1)
sd(authority1)
oneSamplePermutationTest(authority1,alternative = "greater",mu = .50)

#purity
purity_politics=embody_purity%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity)%>%
  dplyr::filter(PoliticalAff %in%c("Liberal","Conservative"))
write_csv(purity_politics,"purity_politics.csv")
Pixels <- fread("purity_politics.csv",sep=",", header = T)
Y <- factor(Pixels$PoliticalAff)
length(Y[Y=="Liberal"])
Pixels[,PoliticalAff:=NULL]
nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]

l<-100
purity1 <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.8, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain,]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  purity1[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(purity1)
sd(purity1)
oneSamplePermutationTest(purity1,alternative = "greater",mu = .50)


##MFQ individual differences 
mean_mfq_care = mean(emdata$MFQ_H,na.rm = T)
sd_mfq_care = sd(emdata$MFQ_H,na.rm = T)

mean_mfq_fairness = mean(emdata$MFQ_F,na.rm = T)
sd_mfq_fairness = sd(emdata$MFQ_F,na.rm = T)

mean_mfq_loyalty = mean(emdata$MFQ_I, na.rm = T)
sd_mfq_loyalty = sd(emdata$MFQ_I, na.rm = T)

mean_mfq_authority = mean(emdata$MFQ_A,na.rm = T) 
sd_mfq_authority = sd(emdata$MFQ_A,na.rm = T)

mean_mfq_purity = mean(emdata$MFQ_P, na.rm = T)
sd_mfq_purity = sd(emdata$MFQ_P, na.rm = T)

#care

highlow_care=embody_care%>%
  dplyr::mutate(highlow_response = ifelse(MFQ_H > mean_mfq_care+sd_mfq_care,"high",
                                          ifelse(MFQ_H < mean_mfq_care-sd_mfq_care,"low",
                                                 "moderate")))%>%
  dplyr::filter(highlow_response %in% c("high", "low"))%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity,
                -PoliticalAff)

write_csv(highlow_care,"highlow_care.csv")
Pixels <- fread("highlow_care.csv",sep=",", header = T)
dim(Pixels)

Y <- factor(Pixels$highlow_response)
Pixels[,highlow_response:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highcare <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.7, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highcare[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highcare)
sd(highcare)
oneSamplePermutationTest(highcare,alternative = "greater",mu = .50)

#fairness

highlow_fairness=embody_fairness%>%
  dplyr::mutate(highlow_response = ifelse(MFQ_H > mean_mfq_fairness+sd_mfq_fairness,"high",
                                          ifelse(MFQ_H < mean_mfq_fairness-sd_mfq_fairness,"low",
                                                 "moderate")))%>%
  dplyr::filter(highlow_response %in% c("high", "low"))%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity,
                -PoliticalAff)

write_csv(highlow_fairness,"highlow_fairness.csv")
Pixels <- fread("highlow_fairness.csv",sep=",", header = T)
dim(Pixels)

Y <- factor(Pixels$highlow_response)
Pixels[,highlow_response:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highfairness <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.7, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highfairness[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highfairness)
sd(highfairness)
oneSamplePermutationTest(highfairness,alternative = "greater",mu = .50)

#loyalty

highlow_loyalty=embody_loyalty%>%
  dplyr::mutate(highlow_response = ifelse(MFQ_H > mean_mfq_loyalty+sd_mfq_loyalty,"high",
                                          ifelse(MFQ_H < mean_mfq_loyalty-sd_mfq_loyalty,"low",
                                                 "moderate")))%>%
  dplyr::filter(highlow_response %in% c("high", "low"))%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity,
                -PoliticalAff)

write_csv(highlow_loyalty,"highlow_loyalty.csv")
Pixels <- fread("highlow_loyalty.csv",sep=",", header = T)
dim(Pixels)

Y <- factor(Pixels$highlow_response)
Pixels[,highlow_response:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highloyalty <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.7, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highloyalty[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highloyalty)
sd(highloyalty)
oneSamplePermutationTest(highloyalty,alternative = "greater",mu = .50)

#authority

highlow_authority=embody_authority%>%
  dplyr::mutate(highlow_response = ifelse(MFQ_H > mean_mfq_authority+sd_mfq_authority,"high",
                                          ifelse(MFQ_H < mean_mfq_authority-sd_mfq_authority,"low",
                                                 "moderate")))%>%
  dplyr::filter(highlow_response %in% c("high", "low"))%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity,
                -PoliticalAff)

write_csv(highlow_authority,"highlow_authority.csv")
Pixels <- fread("highlow_authority.csv",sep=",", header = T)
dim(Pixels)

Y <- factor(Pixels$highlow_response)
Pixels[,highlow_response:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highauthority <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.7, list = FALSE)
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highauthority[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highauthority)
sd(highauthority)
oneSamplePermutationTest(highauthority,alternative = "greater",mu = .50)

#purity

highlow_purity=embody_purity%>%
  dplyr::mutate(highlow_response = ifelse(MFQ_H > mean_mfq_purity+(sd_mfq_purity/2),"high", #there was only "high" with 1SD, so we used 0.5SD for purity
                                          ifelse(MFQ_H < mean_mfq_purity-(sd_mfq_purity/2),"low",
                                                 "moderate")))%>%
  dplyr::filter(highlow_response %in% c("high", "low"))%>%
  dplyr::select(-Validation,
                -Condition.x,
                -Condition.y,
                -stimulus_index,
                -Conserv_SECS,
                -starts_with("MFQ"),
                -MorallyWrong,
                -StrongResponse,
                -Religiosity,
                -PoliticalAff)

write_csv(highlow_purity,"highlow_purity.csv")
Pixels <- fread("highlow_purity.csv",sep=",", header = T)
dim(Pixels)

Y <- factor(Pixels$highlow_response)
Pixels[,highlow_response:=NULL]

nzv <- nearZeroVar(Pixels)
Pixels[,c(nzv):=NULL]


l<-100
highpurity <- rep(NA,l)

pb <- txtProgressBar(min = 0, max = l, style = 3)
for (i in 1:l){
  intrain <- createDataPartition(y =Y, p= 0.6, list = FALSE)#due to very low occurance of "low" responses we used 0.6
  training <- Pixels[intrain,]
  testing <- Pixels[-intrain]
  YTrain <- Y[intrain]
  YTest <- Y[-intrain]
  tune.out <- tune (svm,train.x=training, train.y=YTrain, cost=1)
  SVM <- tune.out$best.model
  temp <- confusionMatrix(YTest, predict(SVM,testing))
  highpurity[i] <- temp$overall[1]
  setTxtProgressBar(pb, i)
}

mean(highpurity)
sd(highpurity)
oneSamplePermutationTest(highpurity,alternative = "greater",mu = .50)


################ RIDGE REGRESSION ####################
#read the inferset representation of 20 vignettes used 

infersent <- read_csv("vignettes.embeddings4096.csv")
infersent$stimulus_index = c(0:19)
infersent <- infersent[,c(4097,1:4096)]
infersent[,1]
head(infersent)

#R2map 
Pixels_2 <- read_csv("Pixels_2.csv")
names(Pixels_2[,48950:48956])
Pixels_2$Condition <- NULL

colnames(infersent) <- c("stimulus_index", paste0("m",1:4096))

infersent_and_pixels_2 <- embody_meta_2%>%
  dplyr::select(Validation,stimulus_index)%>%
  left_join(Pixels_2,by= "Validation")%>%
  left_join(infersent, by = "stimulus_index")%>%
  dplyr::select(-Validation,
                -stimulus_index,
                -X1)

#new ridge loop
library(glmnet)
y <- infersent_and_pixels_2[,c(1:48954)]
x <- infersent_and_pixels_2[,c(48955:53050)]


y <- as.matrix(y)
x <- as.matrix(x)
rsquared <- c(rep(NA,ncol(y)))

pb <- txtProgressBar(min = 0, max = ncol(y), style = 3)
for (i in 1:ncol(y)) {
  lambdas <- 10^seq(2, -2, by = -1)
  set.seed(14)
  y_temp <- y[,i]
  cv_fit <- cv.glmnet(x, y_temp, alpha = 0, lambda = lambdas)
  opt_lambda <- cv_fit$lambda.min
  fit <- cv_fit$glmnet.fit
  y_predicted <- predict(fit, s = opt_lambda, newx = x)
  sst <- sum((y_temp - mean(y_temp))^2)
  sse <- sum((y_predicted - y_temp)^2)
  rsq <- 1 - (sse / sst)
  rsq <- rsq*100
  rsquared[i] <- rsq
  setTxtProgressBar(pb, i)
}

rsquared
psych::describe(rsquared)
rsquared_2 = as.data.frame(rsquared)
write_csv(rsquared_2,"rsquared_2.csv")

#####
#Cronbach's alpha 
psych::alpha(embody_data[,c('q2_0', 'q2_6', 'q2_11', 'q3_0', 'q3_6', 'q3_11')])
psych::alpha(embody_data[,c('q2_1','q2_7','q2_12','q3_1','q3_7','q3_12')])
psych::alpha(embody_data[,c('q2_2', 'q2_8', 'q2_13', 'q3_2', 'q3_8', 'q3_13')])
psych::alpha(embody_data[,c('q2_3', 'q2_9', 'q2_14', 'q3_3', 'q3_9', 'q3_14')])
psych::alpha(embody_data[,c('q2_4', 'q2_10', 'q2_15', 'q3_4', 'q3_10', 'q3_15')])
