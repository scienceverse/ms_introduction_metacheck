####################################### Analysis 1: ICC(2,k) #######################################
install.packages("psych")
library(psych)
# please adjust the directory to the file location on your computer
# ICC were calcuated based on complete cases as recommended by the package
# ICC for Corruptibility Ratings
data1 <- read.csv(file = "/Study3_Corruptible.csv")
ICC(na.omit(data1[,2:86]), alpha=.05)
# ICC for Aggressiveness Ratings
data2 <- read.csv(file = "/Study3_Aggressive.csv")
ICC(na.omit(data2[,2:86]), alpha=.05)
# ICC for Masculinity Ratings
data3 <- read.csv(file = "/Study3_Masculine.csv")
ICC(na.omit(data3[,2:86]), alpha=.05)
# ICC for Competence Ratings
data4 <- read.csv(file = "/Study3_Competent.csv")
ICC(na.omit(data4[,2:86]), alpha=.05)
# ICC for Ambitiousness Ratings
data5 <- read.csv(file = "/Study3_Ambitious.csv")
ICC(na.omit(data5[,2:86]), alpha=.05)


######### Analysis 2: Association between Trait judgments and Actual Records of Conviction #########
# --------- Aggregate level ---------
agg_C <- c()
for (i in c(1:nrow(data1))) {
  temp <- mean(as.numeric(data1[i,2:86]), na.rm = TRUE)
  agg_C <- c(agg_C, temp)}
agg_A <- c()
for (i in c(1:nrow(data2))) {
  temp <- mean(as.numeric(data2[i,2:86]), na.rm = TRUE)
  agg_A <- c(agg_A, temp)}
agg_M <- c()
for (i in c(1:nrow(data3))) {
  temp <- mean(as.numeric(data3[i,2:86]), na.rm = TRUE)
  agg_M <- c(agg_M, temp)}
agg_CM <- c()
for (i in c(1:nrow(data4))) {
  temp <- mean(as.numeric(data4[i,2:86]), na.rm = TRUE)
  agg_CM <- c(agg_CM, temp)}
agg_AM <- c()
for (i in c(1:nrow(data5))) {
  temp <- mean(as.numeric(data5[i,2:86]), na.rm = TRUE)
  agg_AM <- c(agg_AM, temp)}
agg_data <- data.frame(cbind(agg_C, agg_A, agg_M, agg_CM, agg_AM))
install.packages("powerAnalysis")
library(powerAnalysis)
agg_data$officialID <- cbind(c(1:72))
agg_data$convicted <- cbind(c(rep(1,36), rep(0,36)))
acc_C <- 0
for(i in c(1:36)) {if(agg_data$agg_C[i]>3){acc_C<-acc_C+1}}
for(i in c(37:72)) {if(agg_data$agg_C[i]<=3){acc_C<-acc_C+1}}
prop.test(x = acc_C, n=72, p = 0.5,alternative ="greater",conf.level = 0.95)
# compare corruptibility judgments from participants in Study 1 and Study 3
data1_1 <- read.csv(file = "/Study1_Corruptible.csv")
agg_C_1 <- c()
for (i in c(1:nrow(data1_1))) {
  temp <- mean(as.numeric(data1_1[i,2:83]), na.rm = TRUE)
  agg_C_1 <- c(agg_C_1, temp)}
cor.test(agg_C, agg_C_1, alternative = "two.sided", method = "spearman", exact = FALSE)
CIrho(0.876167, 72, level = 0.95)
acc_A <- 0
for(i in c(1:36)) {if(agg_data$agg_A[i]>3){acc_A<-acc_A+1}}
for(i in c(37:72)) {if(agg_data$agg_A[i]<=3){acc_A<-acc_A+1}}
prop.test(x = acc_A, n=72, p = 0.5,alternative ="greater",conf.level = 0.95) 
acc_M <- 0
for(i in c(1:36)) {if(agg_data$agg_M[i]>3){acc_M<-acc_M+1}}
for(i in c(37:72)) {if(agg_data$agg_M[i]<=3){acc_M<-acc_M+1}}
prop.test(x = acc_M, n=72, p = 0.5,alternative ="greater",conf.level = 0.95)
acc_CM <- 0
for(i in c(1:36)) {if(agg_data$agg_CM[i]<3){acc_CM<-acc_CM+1}}
for(i in c(37:72)) {if(agg_data$agg_CM[i]>=3){acc_CM<-acc_CM+1}}
prop.test(x = acc_CM, n=72, p = 0.5,alternative ="greater",conf.level = 0.95)
acc_AM <- 0
for(i in c(1:36)) {if(agg_data$agg_AM[i]<3){acc_AM<-acc_AM+1}}
for(i in c(37:72)) {if(agg_data$agg_AM[i]>=3){acc_AM<-acc_AM+1}}
prop.test(x = acc_AM, n=72, p = 0.5,alternative ="greater",conf.level = 0.95)

# --------- Individual level ---------
ind_C <- c()
for(j in c(2:ncol(data1))) {
  ind_C <- c(ind_C, data1[,j])
}
ind_A <- c()
for(j in c(2:ncol(data2))) {
  ind_A <- c(ind_A, data2[,j])
}
ind_M <- c()
for(j in c(2:ncol(data3))) {
  ind_M <- c(ind_M, data3[,j])
}
ind_CM <- c()
for(j in c(2:ncol(data4))) {
  ind_CM <- c(ind_CM, data4[,j])
}
ind_AM <- c()
for(j in c(2:ncol(data5))) {
  ind_AM <- c(ind_AM, data5[,j])
}
imageID <- rep(c(1:72),85)
subFULL <- c(1:100)
subExclude <- c(25, 86, 3, 43, 2, 12, 32, 35, 37, 49, 51, 52, 57, 58, 98)
subID <- c()
for (i in (setdiff(subFULL,subExclude))) {subID <- c(subID, rep(i,72))}
ind_data <- data.frame(cbind(subID, imageID, ind_C, ind_A, ind_M, ind_CM, ind_AM))
ind_data_noMissing <- ind_data[(!(is.na(ind_data$ind_C))) & (!(is.na(ind_data$ind_A))) & (!(is.na(ind_data$ind_M))) & (!(is.na(ind_data$ind_CM))) & (!(is.na(ind_data$ind_AM))),]
# Corruptibility Judgments
ind_data$b_C <- ifelse(is.na(ind_data$ind_C), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_C %in% c(4,5))) | 
                                                           ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_C %in% c(1,2,3))), 1, 0))
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
# Aggressiveness Judgments
ind_data$b_A <- ifelse(is.na(ind_data$ind_A), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_A %in% c(4,5))) | 
                                                           ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_A %in% c(1,2,3))), 1, 0))
ind_a_A <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_A[ind_data$subID == i], na.rm = TRUE)
  ind_a_A <- c(ind_a_A, temp)
}
t.test(ind_a_A, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_A)
cohensD( x = ind_a_A, mu = 0.5)
# Masculinity Judgments
ind_data$b_M <- ifelse(is.na(ind_data$ind_M), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_M %in% c(4,5))) | 
                                                           ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_M %in% c(1,2,3))), 1, 0))
ind_a_M <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_M[ind_data$subID == i], na.rm = TRUE)
  ind_a_M <- c(ind_a_M, temp)
}
t.test(ind_a_M, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_A)
cohensD( x = ind_a_M, mu = 0.5)
# Competence Judgments
ind_data$b_CM <- ifelse(is.na(ind_data$ind_CM), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_CM %in% c(1,2))) | 
                                                             ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_CM %in% c(3,4,5))), 1, 0))
ind_a_CM <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_CM[ind_data$subID == i], na.rm = TRUE)
  ind_a_CM <- c(ind_a_CM, temp)
}
t.test(ind_a_CM, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_CM)
cohensD( x = ind_a_CM, mu = 0.5)
# Ambitiousness Judgments
ind_data$b_AM <- ifelse(is.na(ind_data$ind_AM), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_AM %in% c(1,2))) | 
                                                             ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_AM %in% c(3,4,5))), 1, 0))
ind_a_AM <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_AM[ind_data$subID == i], na.rm = TRUE)
  ind_a_AM <- c(ind_a_AM, temp)
}
t.test(ind_a_AM, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_AM)
cohensD( x = ind_a_AM, mu = 0.5)

# --------- Individual level: categorizing midpoint 3 in the opposite way ---------
# Corruptibility Judgments
ind_data$b_C <- ifelse(is.na(ind_data$ind_C), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_C %in% c(3,4,5))) | 
                                                           ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_C %in% c(1,2))), 1, 0))
ind_a_C <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_C[ind_data$subID == i], na.rm = TRUE)
  ind_a_C <- c(ind_a_C, temp)
}
t.test(ind_a_C, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_a_C)
cohensD( x = ind_a_C, mu = 0.5)
# Aggressiveness Judgments
ind_data$b_A <- ifelse(is.na(ind_data$ind_A), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_A %in% c(3,4,5))) | 
                                                           ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_A %in% c(1,2))), 1, 0))
ind_a_A <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_A[ind_data$subID == i], na.rm = TRUE)
  ind_a_A <- c(ind_a_A, temp)
}
t.test(ind_a_A, alternative = 'two.sided', mu=0.5,  conf.level = 0.95)
sd(ind_a_A)
cohensD( x = ind_a_A, mu = 0.5)
# Masculinity Judgments
ind_data$b_M <- ifelse(is.na(ind_data$ind_M), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_M %in% c(3,4,5))) | 
                                                           ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_M %in% c(1,2))), 1, 0))
ind_a_M <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_M[ind_data$subID == i], na.rm = TRUE)
  ind_a_M <- c(ind_a_M, temp)
}
t.test(ind_a_M, alternative = 'two.sided', mu=0.5,  conf.level = 0.95)
sd(ind_a_A)
cohensD( x = ind_a_M, mu = 0.5)
# Competence Judgments
ind_data$b_CM <- ifelse(is.na(ind_data$ind_CM), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_CM %in% c(1,2,3))) | 
                                                             ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_CM %in% c(4,5))), 1, 0))
ind_a_CM <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_CM[ind_data$subID == i], na.rm = TRUE)
  ind_a_CM <- c(ind_a_CM, temp)
}
t.test(ind_a_CM, alternative = 'two.sided', mu=0.5,  conf.level = 0.95)
sd(ind_a_CM)
cohensD( x = ind_a_CM, mu = 0.5)
# Ambitiousness Judgments
ind_data$b_AM <- ifelse(is.na(ind_data$ind_AM), NA, ifelse(((ind_data$imageID %in% c(1:36)) & (ind_data$ind_AM %in% c(1,2,3))) | 
                                                             ((ind_data$imageID %in% c(37:72)) & (ind_data$ind_AM %in% c(4,5))), 1, 0))
ind_a_AM <- c()
for (i in (setdiff(subFULL,subExclude))) {
  temp <- mean(ind_data$b_AM[ind_data$subID == i], na.rm = TRUE)
  ind_a_AM <- c(ind_a_AM, temp)
}
t.test(ind_a_AM, alternative = 'two.sided', mu=0.5,  conf.level = 0.95)
sd(ind_a_AM)
cohensD( x = ind_a_AM, mu = 0.5)

# --------- Two-sided t-tests on the average individual-level accuracy across traits ---------
test <- data.frame(cbind(ID = c(setdiff(subFULL,subExclude)),ind_a_C,ind_a_A,ind_a_M,ind_a_CM,ind_a_AM))
t.test(test$ind_a_C,test$ind_a_A,paired = TRUE)
t.test(test$ind_a_C,test$ind_a_M,paired = TRUE)
t.test(test$ind_a_C,test$ind_a_CM,paired = TRUE)
t.test(test$ind_a_C,test$ind_a_AM,paired = TRUE)
t.test(test$ind_a_A,test$ind_a_M,paired = TRUE)
t.test(test$ind_a_A,test$ind_a_CM,paired = TRUE)
t.test(test$ind_a_A,test$ind_a_AM,paired = TRUE)
t.test(test$ind_a_M,test$ind_a_CM,paired = TRUE)
t.test(test$ind_a_M,test$ind_a_AM,paired = TRUE)
t.test(test$ind_a_CM,test$ind_a_AM,paired = TRUE)

# --------- Logistic Regression ---------
# (1) Based on Full data
imagedata <- read.csv(file="/Study1_PhotoCharacteristics.csv",head=TRUE)
colnames(imagedata)[1] <- "imageID"
# standardizing continuous predictors
imagedata$AGE <- (imagedata$age - mean(imagedata$age))/(sd(imagedata$age))
imagedata$PIXEL <- (imagedata$Megapixels - mean(imagedata$Megapixels))/(sd(imagedata$Megapixels))
# model2
# standardizing continuous predictors
ind_data_noMissing$std_C <- (ind_data_noMissing$ind_C - mean(ind_data_noMissing$ind_C))/sd(ind_data_noMissing$ind_C)
ind_data_noMissing$std_A <- (ind_data_noMissing$ind_A - mean(ind_data_noMissing$ind_A))/sd(ind_data_noMissing$ind_A)
ind_data_noMissing$std_M <- (ind_data_noMissing$ind_M - mean(ind_data_noMissing$ind_M))/sd(ind_data_noMissing$ind_M)
ind_data_noMissing$std_CM <- (ind_data_noMissing$ind_CM - mean(ind_data_noMissing$ind_CM))/sd(ind_data_noMissing$ind_CM)
ind_data_noMissing$std_AM <- (ind_data_noMissing$ind_AM - mean(ind_data_noMissing$ind_AM))/sd(ind_data_noMissing$ind_AM)
fulldata <- merge(ind_data_noMissing, imagedata, by="imageID")
# Handle non-independence of repeated measures by adjusting standard errors, clustering at individual and image levels
install.packages("multiwayvcov")
library(multiwayvcov)
install.packages("mfx")
library(mfx)
library(MASS)
library(betareg)
# model1
predict1 <- glm(convicted ~ std_C, family = binomial(link = "logit"), data = fulldata)
vcov1 <- cluster.vcov(predict1, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict1, vcov1)
# model2
predict2 <- glm(convicted ~ std_C + std_A + std_M + std_CM + std_AM
                , family = binomial(link = "logit"), data = fulldata)
vcov2 <- cluster.vcov(predict2, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict2, vcov2)
# model3
predict3 <- glm(convicted ~ std_C + std_A + std_M + std_CM + std_AM + 
                  AGE + glasses + bald + beard + mustache + smile + PIXEL + wiki + news
                , family = binomial(link = "logit"), data = fulldata)
vcov3 <- cluster.vcov(predict3, cbind(fulldata$subID,fulldata$imageID))
coeftest(predict3, vcov3)


################## Analysis 3: Correlation structure of trait inferences ##################
data2_1 <- read.csv(file = "/Study1_Dishonest.csv")
data3_1 <- read.csv(file = "/Study1_Selfish.csv")
data4_1 <- read.csv(file = "/Study1_Trustworthy.csv")
data5_1 <- read.csv(file = "/Study1_Generous.csv")
# Dishonesty, calculate for each offcial his rating averaged over participants
agg_D <- c()
for (i in c(1:nrow(data2_1))) {
  temp <- mean(as.numeric(data2_1[i,2:83]), na.rm = TRUE)
  agg_D <- c(agg_D, temp)}
# Selfishness, calculate for each offcial his rating averaged over participants
agg_S <- c()
for (i in c(1:nrow(data3_1))) {
  temp <- mean(as.numeric(data3_1[i,2:83]), na.rm = TRUE)
  agg_S <- c(agg_S, temp)}
# Trustworthiness, calculate for each offcial his rating averaged over participants
agg_T <- c()
for (i in c(1:nrow(data4_1))) {
  temp <- mean(as.numeric(data4_1[i,2:83]), na.rm = TRUE)
  agg_T <- c(agg_T, temp)}
# Generosity, calculate for each offcial his rating averaged over participants
agg_G <- c()
for (i in c(1:nrow(data5_1))) {
  temp <- mean(as.numeric(data5_1[i,2:83]), na.rm = TRUE)
  agg_G <- c(agg_G, temp)}
agg_C_combined <- (agg_C + agg_C_1)/2
merge_data <- data.frame(cbind(imageID = c(1:72),agg_C_combined, agg_D, agg_S, agg_T, agg_G, agg_A, agg_M, agg_CM, agg_AM))
# Spearman Correlations
cor.test(agg_C_combined, agg_D, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_S, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_T, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_A, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_M, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_C_combined, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_D, agg_S, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_D, agg_T, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_D, agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_D, agg_A, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_D, agg_M, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_D, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_D, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_S, agg_T, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_S, agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_S, agg_A, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_S, agg_M, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_S, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_S, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_T, agg_G, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_T, agg_A, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_T, agg_M, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_T, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_T, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_G, agg_A, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_G, agg_M, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_G, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_G, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_A, agg_M, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_A, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_A, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_M, agg_CM, alternative = "two.sided", method = "spearman", exact = FALSE)
cor.test(agg_M, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

cor.test(agg_CM, agg_AM, alternative = "two.sided", method = "spearman", exact = FALSE)

# visualize correlation 
install.packages("corrplot")
library(corrplot)
M <- cor(merge_data[,2:10])
col2 <- colorRampPalette(c("#053061","#2166AC","#4393C3","#92C5DE","#D1E5F0","#FFFFFF","#FDDBC7","#F4A582","#D6604D", "#B2182B","#67001F"))
#col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
#                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
corrplot(M, method = "color", col=col2(200))

################## Analysis 4: Logistic regression on factors ##################
# Varimax Rotated Principal Components Analysis on the aggregated judgments for each trait for each politician
# retaining 3 components 
install.packages("psych")
library(psych)
pca <- prcomp(merge_data[,2:10], center = TRUE, scale. = TRUE)
print(pca)
summary(pca) # the first three factors counts for over 90% of the variances. 
fit <- principal(merge_data[,2:10], nfactors=3, rotate="varimax")
fit # print results
mydata <- merge(merge_data, imagedata, by="imageID")
# normalize predictors to [0,1]
mydata$AGE <- (mydata$age - min(mydata$age))/(max(mydata$age)-min(mydata$age))
mydata$SMILE <- (mydata$smile - min(mydata$smile))/(max(mydata$smile)-min(mydata$smile))
mydata$PIXEL <- (mydata$Megapixels - min(mydata$Megapixels))/(max(mydata$Megapixels)-min(mydata$Megapixels))
mydata$Corruptibility <- (mydata$agg_C_combined - min(mydata$agg_C_combined))/(max(mydata$agg_C_combined)-min(mydata$agg_C_combined))
mydata$Dishonesty <- (mydata$agg_D - min(mydata$agg_D))/(max(mydata$agg_D)-min(mydata$agg_D))
mydata$Selfishness <- (mydata$agg_S - min(mydata$agg_S))/(max(mydata$agg_S)-min(mydata$agg_S))
mydata$Trustworthiness <- (mydata$agg_T - min(mydata$agg_T))/(max(mydata$agg_T)-min(mydata$agg_T))
mydata$Generosity <- (mydata$agg_G - min(mydata$agg_G))/(max(mydata$agg_G)-min(mydata$agg_G))
mydata$Aggressiveness <- (mydata$agg_A - min(mydata$agg_A))/(max(mydata$agg_A)-min(mydata$agg_A))
mydata$Masculinity <- (mydata$agg_M - min(mydata$agg_M))/(max(mydata$agg_M)-min(mydata$agg_M))
mydata$Competence <- (mydata$agg_CM - min(mydata$agg_CM))/(max(mydata$agg_CM)-min(mydata$agg_CM))
mydata$Ambitiousness <- (mydata$agg_AM - min(mydata$agg_AM))/(max(mydata$agg_AM)-min(mydata$agg_AM))
mydata$do1 <- (mydata$Corruptibility + mydata$Dishonesty + mydata$Selfishness -  mydata$Trustworthiness -  mydata$Generosity +  mydata$Aggressiveness)
mydata$domain1 <- (mydata$do1 - min(mydata$do1))/(max(mydata$do1)-min(mydata$do1))
mydata$do2 <- (mydata$Ambitiousness + mydata$Competence)
mydata$domain2 <- (mydata$do2 - min(mydata$do2))/(max(mydata$do2)-min(mydata$do2))
mydata$domain3 <- mydata$Masculinity
library(pscl)
d1 <- glm(convicted ~ domain1 + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + wiki + news, data = mydata, family = binomial(link = "logit"))
summary(d1)
pR2(d1)
d2 <- glm(convicted ~ domain2 + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + wiki + news, data = mydata,family = binomial(link = "logit"))
summary(d2)
pR2(d2)
d3 <- glm(convicted ~ domain3 + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + wiki + news, data = mydata,family = binomial(link = "logit"))
summary(d3)
pR2(d3)
# visualize coefficients - method 
doInstall <- TRUE
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)
model111Frame <- data.frame(Variable = rownames(summary(d1)$coef)[2:11],
                            Coefficient = summary(d1)$coef[2:11,1],
                            SE = summary(d1)$coef[2:11,2],
                            modelName = "Corruptibility-Factor")
model11Frame <- data.frame(Variable = rownames(summary(d1)$coef)[2:11],
                           Coefficient = summary(d2)$coef[2:11,1],
                           SE = summary(d2)$coef[2:11,2],
                           modelName = "Competence-Factor")
model1Frame <- data.frame(Variable = rownames(summary(d1)$coef)[2:11],
                          Coefficient = summary(d3)$coef[2:11,1],
                          SE = summary(d3)$coef[2:11,2],
                          modelName = "Masculinity-Factor")
allModelFrame <- data.frame(rbind(model1Frame,model11Frame,model111Frame))
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier
chooseColor <- c("#808080","#0000FF","#FF0000") # Grey, Red, Blue, Yellow, friendly for colorblind
zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + scale_colour_manual(values=chooseColor) # manually choose color, remove this line back to default colors
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + scale_linetype_manual(values=chooseLine) # manually set line types
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1.5, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + theme(legend.text = element_text(size = 14,face="bold"), 
                   axis.text=element_text(size=14),
                   axis.title=element_text(size=16,face="bold"),
                   legend.position="top")
zp1 <- zp1 + ggtitle("Comparing several models")
print(zp1)  



