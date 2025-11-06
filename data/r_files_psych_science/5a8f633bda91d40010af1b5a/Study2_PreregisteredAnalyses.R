####################################### Analysis 1: ICC(2,k) #######################################
install.packages("psych")
library(psych)
# please adjust the directory to the file location on your computer
# ICC were calcuated based on complete cases as recommended by the package
# ICC for Corruptibility Ratings
data1 <- read.csv(file = "/Study2_Corruptible.csv")
ICC(na.omit(data1[,2:79]), alpha=.05)
# ICC for Dishonesty Ratings
data2 <- read.csv(file = "/Study2_Dishonest.csv")
ICC(na.omit(data2[,2:79]), alpha=.05)
# ICC for Selfishness Ratings
data3 <- read.csv(file = "/Study2_Selfish.csv")
ICC(na.omit(data3[,2:79]), alpha=.05)
# ICC for Trustworthiness Ratings
data4 <- read.csv(file = "/Study2_Trustworthy.csv")
ICC(na.omit(data4[,2:79]), alpha=.05)
# ICC for Generosity Ratings
data5 <- read.csv(file = "/Study2_Generous.csv")
ICC(na.omit(data5[,2:79]), alpha=.05)


####################################### Analysis 2: Consistency #######################################
# --------- Individual level ---------
ind_C <- c()
for(j in c(2:ncol(data1))) {
  ind_C <- c(ind_C, data1[,j])
}
ind_D <- c()
for(j in c(2:ncol(data2))) {
  ind_D <- c(ind_D, data2[,j])
}
ind_S <- c()
for(j in c(2:ncol(data3))) {
  ind_S <- c(ind_S, data3[,j])
}
ind_T <- c()
for(j in c(2:ncol(data4))) {
  ind_T <- c(ind_T, data4[,j])
}
ind_G <- c()
for(j in c(2:ncol(data5))) {
  ind_G <- c(ind_G, data5[,j])
}
imageID <- rep(c(1:80),78)
subFULL <- c(1:100)
subExclude <- c(1,6,11,14,22,27,35,40,58,62,63,70,74,75,76,79,82,84,86,90,91,98)
subID <- c()
for (i in (setdiff(subFULL,subExclude))) {subID <- c(subID, rep(i,80))}
ind_data <- data.frame(cbind(subID, imageID, ind_C, ind_D, ind_S, ind_T, ind_G))
ind_data_noMissing <- ind_data[(!(is.na(ind_data$ind_C))) & (!(is.na(ind_data$ind_D))) & (!(is.na(ind_data$ind_S))) & (!(is.na(ind_data$ind_T))) & (!(is.na(ind_data$ind_G))),]

# --------- Aggregate level ---------
# Corruptibility, calculate for each offcial his rating averaged over participants
agg_C <- c()
for (i in c(1:nrow(data1))) {
  temp <- mean(as.numeric(data1[i,2:79]), na.rm = TRUE)
  agg_C <- c(agg_C, temp)}
# Dishonesty, calculate for each offcial his rating averaged over participants
agg_D <- c()
for (i in c(1:nrow(data2))) {
  temp <- mean(as.numeric(data2[i,2:79]), na.rm = TRUE)
  agg_D <- c(agg_D, temp)}
# Selfishness, calculate for each offcial his rating averaged over participants
agg_S <- c()
for (i in c(1:nrow(data3))) {
  temp <- mean(as.numeric(data3[i,2:79]), na.rm = TRUE)
  agg_S <- c(agg_S, temp)}
# Trustworthiness, calculate for each offcial his rating averaged over participants
agg_T <- c()
for (i in c(1:nrow(data4))) {
  temp <- mean(as.numeric(data4[i,2:79]), na.rm = TRUE)
  agg_T <- c(agg_T, temp)}
# Generosity, calculate for each offcial his rating averaged over participants
agg_G <- c()
for (i in c(1:nrow(data5))) {
  temp <- mean(as.numeric(data5[i,2:79]), na.rm = TRUE)
  agg_G <- c(agg_G, temp)}

# Aggregate data
install.packages("mada")
library(mada)
agg_data <- data.frame(cbind(agg_C, agg_D, agg_S, agg_T, agg_G))
cor.test(agg_data$agg_C, agg_data$agg_D, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(0.88368 , 80, level = 0.95)
cor.test(agg_data$agg_C, agg_data$agg_S, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(0.8464167, 80, level = 0.95)
cor.test(agg_data$agg_C, agg_data$agg_T, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(-0.8908918, 80, level = 0.95)
cor.test(agg_data$agg_C, agg_data$agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(-0.7688231, 80, level = 0.95)
cor.test(agg_data$agg_D, agg_data$agg_S, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(0.9114369, 80, level = 0.95)
cor.test(agg_data$agg_D, agg_data$agg_T, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(-0.9030467, 80, level = 0.95)
cor.test(agg_data$agg_D, agg_data$agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(-0.8449719, 80, level = 0.95)
cor.test(agg_data$agg_S, agg_data$agg_T, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(-0.9086918, 80, level = 0.95)
cor.test(agg_data$agg_S, agg_data$agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(-0.8888003, 80, level = 0.95)
cor.test(agg_data$agg_T, agg_data$agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(0.8848717, 80, level = 0.95)


######### Analysis 3 & 4: Association between Trait judgments and Actual Records of Conviction #########
# --------- Aggregate level ---------
install.packages("powerAnalysis")
library(powerAnalysis)
agg_data$officialID <- cbind(c(1:80))
agg_data$convicted <- cbind(c(rep(1,36), rep(0,40), rep(1,4)))
acc_C <- 0
for(i in c(1:36,77:80)) {if(agg_data$agg_C[i]>3){acc_C<-acc_C+1}}
for(i in c(37:76)) {if(agg_data$agg_C[i]<=3){acc_C<-acc_C+1}}
prop.test(x = acc_C, n=80, p = 0.5,alternative ="greater",conf.level = 0.95)
acc_D <- 0
for(i in c(1:36,77:80)) {if(agg_data$agg_D[i]>3){acc_D<-acc_D+1}}
for(i in c(37:76)) {if(agg_data$agg_D[i]<=3){acc_D<-acc_D+1}}
prop.test(x = acc_D, n=80, p = 0.5,alternative ="greater",conf.level = 0.95)
acc_S <- 0
for(i in c(1:36,77:80)) {if(agg_data$agg_S[i]>3){acc_S<-acc_S+1}}
for(i in c(37:76)) {if(agg_data$agg_S[i]<=3){acc_S<-acc_S+1}}
prop.test(x = acc_S, n=80, p = 0.5,alternative ="greater",conf.level = 0.95)
acc_T <- 0
for(i in c(1:36,77:80)) {if(agg_data$agg_T[i]<3){acc_T<-acc_T+1}}
for(i in c(37:76)) {if(agg_data$agg_T[i]>=3){acc_T<-acc_T+1}}
prop.test(x = acc_T, n=80, p = 0.5,alternative ="greater",conf.level = 0.95)
acc_G <- 0
for(i in c(1:36,77:80)) {if(agg_data$agg_G[i]<3){acc_G<-acc_G+1}}
for(i in c(37:76)) {if(agg_data$agg_G[i]>=3){acc_G<-acc_G+1}}
prop.test(x = acc_G, n=80, p = 0.5,alternative ="greater",conf.level = 0.95)

# --------- Individual level ---------
# Corruptibility Judgments
ind_data$b_C <- ifelse(is.na(ind_data$ind_C), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_C %in% c(4,5))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_C %in% c(1,2,3))), 1, 0))
ind_a_C <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_C[ind_data$subID == i], na.rm = TRUE)
  ind_a_C <- c(ind_a_C, temp)
}
t.test(ind_a_C, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_C)
install.packages("lsr")
library(lsr)
cohensD( x = ind_a_C, mu = 0.5)
# Dishonesty Judgments
ind_data$b_D <- ifelse(is.na(ind_data$ind_D), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_D %in% c(4,5))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_D %in% c(1,2,3))), 1, 0))
ind_a_D <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_D[ind_data$subID == i], na.rm = TRUE)
  ind_a_D <- c(ind_a_D, temp)
}
t.test(ind_a_D, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_D)
cohensD( x = ind_a_D, mu = 0.5)
# Selfishness Judgments
ind_data$b_S <- ifelse(is.na(ind_data$ind_S), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_S %in% c(4,5))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_S %in% c(1,2,3))), 1, 0))
ind_a_S <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_S[ind_data$subID == i], na.rm = TRUE)
  ind_a_S <- c(ind_a_S, temp)
}
t.test(ind_a_S, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_S)
cohensD( x = ind_a_S, mu = 0.5)
# Trustworthiness Judgments
ind_data$b_T <- ifelse(is.na(ind_data$ind_T), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_T %in% c(1,2))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_T %in% c(3,4,5))), 1, 0))
ind_a_T <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_T[ind_data$subID == i], na.rm = TRUE)
  ind_a_T <- c(ind_a_T, temp)
}
t.test(ind_a_T, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_T)
cohensD( x = ind_a_T, mu = 0.5)
# Generosity Judgments
ind_data$b_G <- ifelse(is.na(ind_data$ind_G), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_G %in% c(1,2))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_G %in% c(3,4,5))), 1, 0))
ind_a_G <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_G[ind_data$subID == i], na.rm = TRUE)
  ind_a_G <- c(ind_a_G, temp)
}
t.test(ind_a_G, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_G)
cohensD( x = ind_a_G, mu = 0.5)

# --------- Individual level: categorizing midpoint 3 in the opposite way ---------
# Corruptibility Judgments
ind_data$b_C <- ifelse(is.na(ind_data$ind_C), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_C %in% c(3, 4,5))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_C %in% c(1,2))), 1, 0))
ind_a_C <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_C[ind_data$subID == i], na.rm = TRUE)
  ind_a_C <- c(ind_a_C, temp)
}
t.test(ind_a_C, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_C)
install.packages("lsr")
library(lsr)
cohensD( x = ind_a_C, mu = 0.5)
# Dishonesty Judgments
ind_data$b_D <- ifelse(is.na(ind_data$ind_D), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_D %in% c(3, 4,5))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_D %in% c(1,2))), 1, 0))
ind_a_D <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_D[ind_data$subID == i], na.rm = TRUE)
  ind_a_D <- c(ind_a_D, temp)
}
t.test(ind_a_D, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_D)
cohensD( x = ind_a_D, mu = 0.5)
# Selfishness Judgments
ind_data$b_S <- ifelse(is.na(ind_data$ind_S), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_S %in% c(3, 4,5))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_S %in% c(1,2))), 1, 0))
ind_a_S <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_S[ind_data$subID == i], na.rm = TRUE)
  ind_a_S <- c(ind_a_S, temp)
}
t.test(ind_a_S, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_S)
cohensD( x = ind_a_S, mu = 0.5)
# Trustworthiness Judgments
ind_data$b_T <- ifelse(is.na(ind_data$ind_T), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_T %in% c(1,2, 3))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_T %in% c(4,5))), 1, 0))
ind_a_T <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_T[ind_data$subID == i], na.rm = TRUE)
  ind_a_T <- c(ind_a_T, temp)
}
t.test(ind_a_T, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_T)
cohensD( x = ind_a_T, mu = 0.5)
# Generosity Judgments
ind_data$b_G <- ifelse(is.na(ind_data$ind_G), NA, ifelse(((ind_data$imageID %in% c(1:36,77:80)) & (ind_data$ind_G %in% c(1,2, 3))) | 
                                                           ((ind_data$imageID %in% c(37:76)) & (ind_data$ind_G %in% c(4,5))), 1, 0))
ind_a_G <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_G[ind_data$subID == i], na.rm = TRUE)
  ind_a_G <- c(ind_a_G, temp)
}
t.test(ind_a_G, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_G)
cohensD( x = ind_a_G, mu = 0.5)


########################### Other analyses planned in our preregistration ###########################

# --------- Logistic Regression ---------
# (1) Based on Full data
# model1
imagedata <- read.csv(file="/Study2_PhotoCharacteristic.csv",head=TRUE)
colnames(imagedata)[1] <- "imageID"
# variable normalization to [0,1]
imagedata$AGE <- (imagedata$age - mean(imagedata$age))/(sd(imagedata$age))
imagedata$PIXEL <- (imagedata$Megapixels - mean(imagedata$Megapixels))/(sd(imagedata$Megapixels))
predict1 <- glm(convicted ~ AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource, family = binomial(link = "logit"), data = imagedata)
summary(predict1)
# model2
# variable normalization to [0,1]
ind_data_noMissing$std_C <- (ind_data_noMissing$ind_C - mean(ind_data_noMissing$ind_C))/sd(ind_data_noMissing$ind_C)
ind_data_noMissing$std_D <- (ind_data_noMissing$ind_D - mean(ind_data_noMissing$ind_D))/sd(ind_data_noMissing$ind_D)
ind_data_noMissing$std_S <- (ind_data_noMissing$ind_S - mean(ind_data_noMissing$ind_S))/sd(ind_data_noMissing$ind_S)
ind_data_noMissing$std_T <- (ind_data_noMissing$ind_T - mean(ind_data_noMissing$ind_T))/sd(ind_data_noMissing$ind_T)
ind_data_noMissing$std_G <- (ind_data_noMissing$ind_G - mean(ind_data_noMissing$ind_G))/sd(ind_data_noMissing$ind_G)
fulldata <- merge(ind_data_noMissing, imagedata, by="imageID")
# Handle non-independence of repeated measures by adjusting standard errors, clustering at individual and image levels
install.packages("multiwayvcov")
library(multiwayvcov)
install.packages("mfx")
library(mfx)
library(MASS)
library(betareg)
predict2C <- glm(convicted ~ std_C +
                   AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource
                 , family = binomial(link = "logit"), data = fulldata)
vcov2C <- cluster.vcov(predict2C, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict2C, vcov2C)
predict2D <- glm(convicted ~ std_D +
                   AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource
                 , family = binomial(link = "logit"), data = fulldata)
vcov2D <- cluster.vcov(predict2D, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict2D, vcov2D)
predict2S <- glm(convicted ~ std_S +
                   AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource
                 , family = binomial(link = "logit"), data = fulldata)
vcov2S <- cluster.vcov(predict2S, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict2S, vcov2S)
predict2T <- glm(convicted ~ std_T +
                   AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource
                 , family = binomial(link = "logit"), data = fulldata)
vcov2T <- cluster.vcov(predict2T, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict2T, vcov2T)
predict2G <- glm(convicted ~ std_G +
                   AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource
                 , family = binomial(link = "logit"), data = fulldata)
vcov2G <- cluster.vcov(predict2G, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict2G, vcov2G)

# --------- Correlation between Response Times and Accuracies ---------
timedata <- read.csv(file ="/Study2_Time.csv",head=TRUE)
surveydata <- read.csv(file ="/Study2_Survey.csv",head=TRUE)
mydata <- merge(timedata, surveydata, by="ID")
hist(mydata$RT, breaks=seq(0.1,5,0.05)) # log transform is needed for responses times
mydata$acc <- NA
mydata$acc[((mydata$image %in% c(1:36,77:80)) & (mydata$trait %in% c(1,2,3)) & (mydata$reponse %in% c(4,5))) | 
             ((mydata$image %in% c(1:36,77:80)) & (mydata$trait %in% c(4,5)) & (mydata$reponse %in% c(1,2))) |
             ((mydata$image %in% c(37:76)) & (mydata$trait %in% c(1,2,3)) & (mydata$reponse %in% c(1,2,3))) |
             ((mydata$image %in% c(37:76)) & (mydata$trait %in% c(4,5)) & (mydata$reponse %in% c(3,4,5)))] <-1
mydata$acc[((mydata$image %in% c(1:36,77:80)) & (mydata$trait %in% c(1,2,3)) & (mydata$reponse %in% c(1,2,3))) | 
             ((mydata$image %in% c(1:36,77:80)) & (mydata$trait %in% c(4,5)) & (mydata$reponse %in% c(3,4,5))) |
             ((mydata$image %in% c(37:76)) & (mydata$trait %in% c(1,2,3)) & (mydata$reponse %in% c(4,5))) |
             ((mydata$image %in% c(37:76)) & (mydata$trait %in% c(4,5)) & (mydata$reponse %in% c(1,2)))] <-0
time1 <- glm(acc ~ log(RT), family = binomial(link = "logit"), data = mydata)
vcovT1 <- cluster.vcov(time1, cbind(mydata$ID,mydata$image))
coeftest(time1, vcovT1)
# survey responses
colnames(mydata)[7:29] <- c("Q1", "Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23")
data <- mydata[(!(is.na(mydata$acc))) &
                 (!(is.na(mydata$Q1))) & (!(is.na(mydata$Q2))) & (!(is.na(mydata$Q3))) & 
                 (!(is.na(mydata$Q4))) & (!(is.na(mydata$Q5))) & (!(is.na(mydata$Q6))) & 
                 (!(is.na(mydata$Q7))) & (!(is.na(mydata$Q8))) & (!(is.na(mydata$Q9))) & 
                 (!(is.na(mydata$Q10))) & (!(is.na(mydata$Q11))) & (!(is.na(mydata$Q12))) & 
                 (!(is.na(mydata$Q13))) & (!(is.na(mydata$Q14))) & (!(is.na(mydata$Q15))) & 
                 (!(is.na(mydata$Q16))) & (!(is.na(mydata$Q17))) & (!(is.na(mydata$Q18))) & 
                 (!(is.na(mydata$Q19))) & (!(is.na(mydata$Q20))) & (!(is.na(mydata$Q23))),]
# standardize continous predictors
data$Extraversion <- ((data$Q1 + (8-data$Q6)) - mean((data$Q1 + (8-data$Q6))))/sd((data$Q1 + (8-data$Q6)))
data$Agreeableness <- ((data$Q7 + (8-data$Q2)) - mean((data$Q7 + (8-data$Q2))))/sd((data$Q7 + (8-data$Q2)))
data$Conscientiousness <- ((data$Q3 + (8-data$Q8)) - mean((data$Q3 + (8-data$Q8))))/sd((data$Q3 + (8-data$Q8)))
data$EmotionalStability <- ((data$Q9 + (8-data$Q4)) - mean((data$Q9 + (8-data$Q4))))/sd((data$Q9 + (8-data$Q4)))
data$Openness <- ((data$Q5 + (8-data$Q10)) - mean((data$Q5 + (8-data$Q10))))/sd((data$Q5 + (8-data$Q10)))
data$Vote <- data$Q11
data$PoliticalParticipation <- (data$Q12 - mean(data$Q12))/sd(data$Q12)
data$TrustGov <- (data$Q13-mean(data$Q13))/sd(data$Q13)
data$GovWaste <- (data$Q14-mean(data$Q14))/sd(data$Q14)
data$GovCorrupt <- (data$Q15-mean(data$Q15))/sd(data$Q15)
data$Gay <- (data$Q16-mean(data$Q16))/sd(data$Q16)
data$Ideology <- (data$Q17-mean(data$Q17))/sd(data$Q17)
data$Age <- (as.numeric(data$Q18)-mean(as.numeric(data$Q18)))/sd(as.numeric(data$Q18))
data$female <- ifelse(data$Q19 == 1, 1,0)
data$Education <- (data$Q20-mean(data$Q20))/sd(data$Q20)
data$White <- ifelse(data$Q23 == 1, 1,0)
data$Black <- ifelse(data$Q23 == 2, 1,0)
data$Asian <- ifelse(data$Q23 == 4, 1, 0)
data$rt <- (log(data$RT) - mean(log(data$RT)))/sd(log(data$RT))
time2 <- glm(acc ~ rt + Extraversion + Agreeableness + Conscientiousness + EmotionalStability + Openness + 
               Vote + PoliticalParticipation + TrustGov + GovWaste + GovCorrupt + Gay + Ideology + Age + female + Education + White,
             family = binomial(link = "logit"), data = data)
vcovT2 <- cluster.vcov(time2, cbind(data$ID,data$image))
coeftest(time2, vcovT2)







