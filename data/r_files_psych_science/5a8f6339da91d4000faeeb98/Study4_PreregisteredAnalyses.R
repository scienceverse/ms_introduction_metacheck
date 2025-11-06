####################################### Study 4a Analyses #######################################
# Study 4a was not preregister. For ease of reference, we also attached Study 4a analysis codes here together with Study 4b.
# --------- Study4a_Face1 ---------
# facial metrics data
facedata <- read.csv(file = "/Study4a_Face1.csv", header = TRUE)
names(facedata)[1] <- "imageID"
# standardize continous variables
facedata$AGE <- (facedata$age - mean(facedata$age))/(sd(facedata$age))
facedata$SMILE <- (facedata$teeth - mean(facedata$teeth))/(sd(facedata$teeth))
facedata$PIXEL <- (facedata$Megapixels - mean(facedata$Megapixels))/(sd(facedata$Megapixels))
# rating data
data1 <- read.csv(file = "/Study1_Corruptible.csv")
data2 <- read.csv(file = "/Study1_Dishonest.csv")
data3 <- read.csv(file = "/Study1_Selfish.csv")
data4 <- read.csv(file = "/Study1_Trustworthy.csv")
data5 <- read.csv(file = "/Study1_Generous.csv")
# long form data
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
imageID <- rep(c(1:72),82)
subFULL <- c(1:100)
subExclude <- c(3,4,9,10,15,16,18,19,47,50,53,56,67,68,78,94,97,98)
subID <- c()
for (i in (setdiff(subFULL,subExclude))) {subID <- c(subID, rep(i,72))}
ind_data <- data.frame(cbind(subID, imageID, ind_C, ind_D, ind_S, ind_T, ind_G))
mydata1 <- merge(ind_data, facedata, by="imageID")
# --------- Study4a_Face2 ---------
facedata <- read.csv(file = "/Study4a_Face2.csv", header = TRUE)
names(facedata)[1] <- "imageID"
names(facedata)[25] <- "gov"
names(facedata)[2] <- "convicted"
# standardize variables
facedata$AGE <- (facedata$age - mean(facedata$age))/(sd(facedata$age))
facedata$SMILE <- facedata$teeth 
facedata$PIXEL <- (facedata$Megapixels - mean(facedata$Megapixels))/(sd(facedata$Megapixels))
# rating data
data1 <- read.csv(file = "/Study2_Corruptible.csv")
data2 <- read.csv(file = "/Study2_Dishonest.csv")
data3 <- read.csv(file = "/Study2_Selfish.csv")
data4 <- read.csv(file = "/Study2_Trustworthy.csv")
data5 <- read.csv(file = "/Study2_Generous.csv")
# long form data
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
mydata2 <- merge(ind_data, facedata, by="imageID")
mydata2$imageID <- 72+mydata2$imageID 
mydata2$subID <- 100+mydata2$subID
# Face1 and Face2
mydata <- rbind(cbind(mydata1[,1:21], mydata1[,26:38]), mydata2)
# composite corruptibility inferences
mydata$do1 <- (mydata$ind_C + mydata$ind_D + mydata$ind_S - mydata$ind_T -  mydata$ind_G)
# standardize variable
mydata$domain1 <- (mydata$do1 - mean(mydata$do1, na.rm=TRUE))/(sd(mydata$do1, na.rm=TRUE))
# mediation analysis
install.packages("lme4")
library(lme4)
library(car)
install.packages("RMediation")
library(RMediation)
# fWHR
fit1 <- lmer(domain1 ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
fit2 <- lm(fWHR ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ fWHR + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
summary(fit2)
summary(fit3)
# FaceWidthLowerFaceHeight
fit2 <- lm(FaceWidthLowerFaceHeight ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ FaceWidthLowerFaceHeight + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
# LowerFaceHeight
fit2 <- lm(LowerFaceHeight ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ LowerFaceHeight + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
# ChecckboneProminence
fit2 <- lm(ChecckboneProminence ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ ChecckboneProminence + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
# IntereyeFaceWidth
fit2 <- lm(IntereyeFaceWidth ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ IntereyeFaceWidth + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
# NoseMouthWidth
fit2 <- lm(NoseMouthWidth ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ NoseMouthWidth + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
# NoseLowerFaceHeight
fit2 <- lm(NoseLowerFaceHeight ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ NoseLowerFaceHeight + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)
# MouthLowerFaceWidth
fit2 <- lm(MouthLowerFaceWidth ~ convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov, data = facedata)
fit3 <- lmer(domain1 ~ MouthLowerFaceWidth + convicted + AGE + glasses + bald + beard + mustache + SMILE + PIXEL + gov + 
               (1|subID) + (1|imageID) + (1|subID:convicted), data = mydata)
a = summary(fit2)$coefficients[2,1]
se.a = summary(fit2)$coefficients[2,2]
b = summary(fit3)$coefficients[2,1]
se.b = summary(fit3)$coefficients[2,2]
medci(mu.x=a, mu.y=b, se.x=se.a, se.y=se.b, rho=0, alpha=.05,type="prodclin", plot=TRUE, plotCI=TRUE)

####################################### Study 4b Preregistered Analyses #######################################
mydata <- read.csv(file = "/Study4b.csv", header = TRUE)
# slim
slimID <- c()
for (i in c(1:150)) {
  temp <- 1 + 3*(i-1)
  slimID <- c(slimID, temp)
}
slimSet <- paste("a",slimID,sep="")
slim <- mydata[names(mydata) %in% slimSet]
# origin
originID <- c()
for (i in c(1:150)) {
  temp <- 2 + 3*(i-1)
  originID <- c(originID, temp)
}
originSet <- paste("a",originID,sep="")
origin <- mydata[names(mydata) %in% originSet]
# fat
fatID <- c()
for (i in c(1:150)) {
  temp <- 3*i
  fatID <- c(fatID, temp)
}
fatSet <- paste("a",fatID,sep="")
fat <- mydata[names(mydata) %in% fatSet]
# difference Fat-Slim
diff_fs <- c()
for (i in c(1:150)) {
  temp <- fat[,i] - slim[,i]
  diff_fs <- c(diff_fs, temp)
}
# fat-slim: one-sample one-sided t-test on the differences
t.test(diff_fs, mu = 0, alternative = "greater")
sd(diff_fs, na.rm = TRUE)
# original-slim and fat-original: one-sample one-sided t-test with Bonferroni correction 
diff_fo <- c()
for (i in c(1:150)) {
  temp <- fat[,i] - origin[,i]
  diff_fo <- c(diff_fo, temp)
}
t.test(diff_fo, mu = 0, alternative = "greater")
diff_os <- c()
for (i in c(1:150)) {
  temp <- origin[,i] - slim[,i]
  diff_os <- c(diff_os, temp)
}
t.test(diff_os, mu = 0, alternative = "greater")
# associations between face width manipulation on perceived corruptibility
aggregate_origin <- c()
for (i in (1:150)) {
  aggregate_origin <- c(aggregate_origin, mean(origin[,i], na.rm=TRUE))
}
aggregate_diff <- c()
for (i in (1:150)) {
  temp <- mean(diff_fs[(nrow(origin)*(i-1)+1):(nrow(origin)*i)], na.rm=TRUE)
  aggregate_diff <- c(aggregate_diff, temp)
}
# read in photo characteristics data
imagedata1 <- read.csv(file="/Study1_PhotoCharacteristics.csv",head=TRUE)
image1 <- imagedata1[imagedata1$image != 64,]
image1$imageID <- c(1:71)
image1$ImgSource <- ifelse(image1$gov==1, 1, 0)
image1_match <- data.frame(cbind(image1[,1:11]), ImgSource=image1[,18], image1[,16:17])
imagedata2 <- read.csv(file="/Study2_PhotoCharacteristic.csv",head=TRUE)
image2 <- imagedata2[imagedata2$image != 67,]
image2$imageID <- c(72: 150)
test <- data.frame(rbind(image1_match, image2))
test$origin <- aggregate_origin 
test$diff <- aggregate_diff
cor.test(aggregate_diff,aggregate_origin)
fit <- lm(diff ~ origin + age + glasses + bald + beard + mustache + smile + Megapixels + ImgSource, data = test)
summary(fit)
# graphical representation
compare <- data.frame(cbind(c(1:150),aggregate_origin, aggregate_diff))
compare$group <- NA
compare$group[compare$V1 %in% c(1:36)] <- "Corrupt Officials of Face Set 1"
compare$group[compare$V1 %in% c(72:111)] <- "Corrupt Officials of Face Set 2"
compare$group[compare$V1 %in% c(37:71)] <- "Non-corrupt Officials of Face Set 1"
compare$group[compare$V1 %in% c(112:150)] <- "Non-corrupt Officials of Face Set 2"
install.packages("ggplot2")
library(ggplot2)
scatter <- ggplot(compare, aes(x = aggregate_origin, y = aggregate_diff, colour = group, shape=group)) + geom_point(size = 4) 
scatter <- scatter + scale_color_manual(values=c("#FF4c52","#FFD600","#4CACFF","#C1CCD4")) # "#FF7F83" "#7FC3FF"
scatter <- scatter + scale_shape_manual(values=c(8, 16, 8, 16))
scatter <- scatter + geom_vline(xintercept = 5, colour = gray(1/2), lty = 2)
scatter <- scatter + geom_hline(yintercept = 0,colour = gray(1/2))
scatter <- scatter + ylim(-0.50, 0.70)
scatter <- scatter + theme(legend.text = element_text(size = 14), 
                           axis.text=element_text(size=14),
                           axis.title=element_text(size=16),
                           legend.key.height=unit(2,"line"))
scatter <- scatter + labs(x = "\nAggregate-level perceived corruptibility inferred from original version photos", 
                          y = "Difference of aggregate-level perceived corruptibility (fat - slim)\n")
print(scatter)








