####################################### Analysis 1: Base Rate #######################################
data1 <- read.csv(file = "/Study2_Corruptible.csv")
data2 <- read.csv(file = "/Study2_Dishonest.csv")
data3 <- read.csv(file = "/Study2_Selfish.csv")
data4 <- read.csv(file = "/Study2_Trustworthy.csv")
data5 <- read.csv(file = "/Study2_Generous.csv")

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
# Repeated measure correlation coefficients
install.packages("rmcorr")
library(rmcorr)
rmcorr(subID, ind_C, ind_D, data=ind_data_noMissing)
rmcorr(subID, ind_C, ind_S, data=ind_data_noMissing)
rmcorr(subID, ind_C, ind_T, data=ind_data_noMissing)
rmcorr(subID, ind_C, ind_G, data=ind_data_noMissing)
rmcorr(subID, ind_D, ind_S, data=ind_data_noMissing)
rmcorr(subID, ind_D, ind_T, data=ind_data_noMissing)
rmcorr(subID, ind_D, ind_G, data=ind_data_noMissing)
rmcorr(subID, ind_S, ind_T, data=ind_data_noMissing)
rmcorr(subID, ind_S, ind_G, data=ind_data_noMissing)
rmcorr(subID, ind_T, ind_G, data=ind_data_noMissing)

# --------- individual mean rating across faces -----------
# Corruptibility Ratings
mean_C <- c()
for (i in c(2:ncol(data1))) {
  temp <- mean(as.numeric(data1[,i]), na.rm = TRUE)
  mean_C <- c(mean_C, temp)}
sd_C <- c()
for (i in c(2:ncol(data1))) {
  temp <- sd(as.numeric(data1[,i]), na.rm = TRUE)
  sd_C <- c(sd_C, temp)}
subFULL <- c(1:100)
subExclude <- c(1,6,11,14,22,27,35,40,58,62,63,70,74,75,76,79,82,84,86,90,91,98)
ID <- setdiff(subFULL,subExclude)
plot_C <- data.frame(cbind(ID, mean_C,sd_C))
sort_plot_C <- plot_C[order(plot_C$mean_C),]
sort_plot_C$rank <- c(1:78)
plot(sort_plot_C$rank, sort_plot_C$mean_C,
     ylim=range(c(sort_plot_C$mean_C-sort_plot_C$sd_C, sort_plot_C$mean_C+sort_plot_C$sd_C)),
     pch=19, xlab="Counts of Participants", ylab="Mean +/- SD",
     main="Means and Standard Deviations of Corruptibility Ratings for Each Participant"
)
arrows(sort_plot_C$rank, sort_plot_C$mean_C-sort_plot_C$sd_C, sort_plot_C$rank, sort_plot_C$mean_C+sort_plot_C$sd_C, length=0.05, angle=90, code=3)
abline(h = 3.0)
# Dishonesty Ratings
mean_D <- c()
for (i in c(2:ncol(data2))) {
  temp <- mean(as.numeric(data2[,i]), na.rm = TRUE)
  mean_D <- c(mean_D, temp)}
sd_D <- c()
for (i in c(2:ncol(data2))) {
  temp <- sd(as.numeric(data2[,i]), na.rm = TRUE)
  sd_D <- c(sd_D, temp)}
plot_D <- data.frame(cbind(ID, mean_D,sd_D))
sort_plot_D <- plot_D[order(plot_D$mean_D),]
sort_plot_D$rank <- c(1:78)
plot(sort_plot_D$rank, sort_plot_D$mean_D,
     ylim=range(c(sort_plot_D$mean_D-sort_plot_D$sd_D, sort_plot_D$mean_D+sort_plot_D$sd_D)),
     pch=19, xlab="Counts of Participants", ylab="Mean +/- SD",
     main="Means and Standard Deviations of Dishonesty Ratings for Each Participant"
)
arrows(sort_plot_D$rank, sort_plot_D$mean_D-sort_plot_D$sd_D, sort_plot_D$rank, sort_plot_D$mean_D+sort_plot_D$sd_D, length=0.05, angle=90, code=3)
abline(h = 3.0)
# Selfishness Ratings
mean_S <- c()
for (i in c(2:ncol(data3))) {
  temp <- mean(as.numeric(data3[,i]), na.rm = TRUE)
  mean_S <- c(mean_S, temp)}
sd_S <- c()
for (i in c(2:ncol(data3))) {
  temp <- sd(as.numeric(data3[,i]), na.rm = TRUE)
  sd_S <- c(sd_S, temp)}
plot_S <- data.frame(cbind(ID, mean_S,sd_S))
sort_plot_S <- plot_S[order(plot_S$mean_S),]
sort_plot_S$rank <- c(1:78)
plot(sort_plot_S$rank, sort_plot_S$mean_S,
     ylim=range(c(sort_plot_S$mean_S-sort_plot_S$sd_S, sort_plot_S$mean_S+sort_plot_S$sd_S)),
     pch=19, xlab="Counts of Participants", ylab="Mean +/- SD",
     main="Means and Standard Deviations of Selfishness Ratings for Each Participant"
)
arrows(sort_plot_S$rank, sort_plot_S$mean_S-sort_plot_S$sd_S, sort_plot_S$rank, sort_plot_S$mean_S+sort_plot_S$sd_S, length=0.05, angle=90, code=3)
abline(h = 3.0)
# Trustworthiness Ratings
mean_T <- c()
for (i in c(2:ncol(data4))) {
  temp <- mean(as.numeric(data4[,i]), na.rm = TRUE)
  mean_T <- c(mean_T, temp)}
sd_T <- c()
for (i in c(2:ncol(data4))) {
  temp <- sd(as.numeric(data4[,i]), na.rm = TRUE)
  sd_T <- c(sd_T, temp)}
plot_T <- data.frame(cbind(ID, mean_T,sd_T))
sort_plot_T <- plot_T[order(plot_T$mean_T),]
sort_plot_T$rank <- c(1:78)
plot(sort_plot_T$rank, sort_plot_T$mean_T,
     ylim=range(c(sort_plot_T$mean_T-sort_plot_T$sd_T, sort_plot_T$mean_T+sort_plot_T$sd_T)),
     pch=19, xlab="Counts of Participants", ylab="Mean +/- SD",
     main="Means and Standard Deviations of Trustworthiness Ratings for Each Participant"
)
arrows(sort_plot_T$rank, sort_plot_T$mean_T-sort_plot_T$sd_T, sort_plot_T$rank, sort_plot_T$mean_T+sort_plot_T$sd_T, length=0.05, angle=90, code=3)
abline(h = 3.0)
# Generosity Ratings
mean_G <- c()
for (i in c(2:ncol(data5))) {
  temp <- mean(as.numeric(data5[,i]), na.rm = TRUE)
  mean_G <- c(mean_G, temp)}
sd_G <- c()
for (i in c(2:ncol(data5))) {
  temp <- sd(as.numeric(data5[,i]), na.rm = TRUE)
  sd_G <- c(sd_G, temp)}
plot_G <- data.frame(cbind(ID, mean_G,sd_G))
sort_plot_G <- plot_G[order(plot_G$mean_G),]
sort_plot_G$rank <- c(1:78)
plot(sort_plot_G$rank, sort_plot_G$mean_G,
     ylim=range(c(sort_plot_G$mean_G-sort_plot_G$sd_G, sort_plot_G$mean_G+sort_plot_G$sd_G)),
     pch=19, xlab="Counts of Participants", ylab="Mean +/- SD",
     main="Means and Standard Deviations of Generosity Ratings for Each Participant"
)
arrows(sort_plot_G$rank, sort_plot_G$mean_G-sort_plot_G$sd_G, sort_plot_G$rank, sort_plot_G$mean_G+sort_plot_G$sd_G, length=0.05, angle=90, code=3)
abline(h = 3.0)

# --------- single trial level ---------
data <- data.frame(cbind(subID, imageID, ind_C, ind_D, ind_S, ind_T, ind_G))
mean_data <- data.frame(cbind(subID=setdiff(subFULL,subExclude), mean_C, mean_D, mean_S, mean_T, mean_G))
center_data <- merge(data, mean_data, by="subID")
center_data$acc_C <- NA
center_data$acc_C[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_C>center_data$mean_C) & (!(is.na(center_data$ind_C))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_C<center_data$mean_C) & (!(is.na(center_data$ind_C))))] <- 1
center_data$acc_C[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_C<center_data$mean_C) & (!(is.na(center_data$ind_C))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_C>center_data$mean_C) & (!(is.na(center_data$ind_C))))] <- 0
center_data$acc_D <- NA
center_data$acc_D[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_D>center_data$mean_D) & (!(is.na(center_data$ind_D))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_D<center_data$mean_D) & (!(is.na(center_data$ind_D))))] <- 1
center_data$acc_D[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_D<center_data$mean_D) & (!(is.na(center_data$ind_D))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_D>center_data$mean_D) & (!(is.na(center_data$ind_D))))] <- 0
center_data$acc_S <- NA
center_data$acc_S[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_S>center_data$mean_S) & (!(is.na(center_data$ind_S))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_S<center_data$mean_S) & (!(is.na(center_data$ind_S))))] <- 1
center_data$acc_S[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_S<center_data$mean_S) & (!(is.na(center_data$ind_S))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_S>center_data$mean_S) & (!(is.na(center_data$ind_S))))] <- 0
center_data$acc_T <- NA
center_data$acc_T[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_T>center_data$mean_T) & (!(is.na(center_data$ind_T))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_T<center_data$mean_T) & (!(is.na(center_data$ind_T))))] <- 0
center_data$acc_T[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_T<center_data$mean_T) & (!(is.na(center_data$ind_T))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_T>center_data$mean_T) & (!(is.na(center_data$ind_T))))] <- 1
center_data$acc_G <- NA
center_data$acc_G[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_G>center_data$mean_G) & (!(is.na(center_data$ind_G))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_G<center_data$mean_G) & (!(is.na(center_data$ind_G))))] <- 0
center_data$acc_G[((center_data$imageID %in% c(1:36,77:80)) & (center_data$ind_G<center_data$mean_G) & (!(is.na(center_data$ind_G))))|
                    ((center_data$imageID %in% c(37:76)) & (center_data$ind_G>center_data$mean_G) & (!(is.na(center_data$ind_G))))] <- 1
# --------- individual-level accuracy t-test ---------
# corruptibility
ind_acc_C <- c()
for(i in (setdiff(subFULL,subExclude))) {
  temp <- mean(center_data$acc_C[center_data$subID==i], na.rm=TRUE)
  ind_acc_C <- c(ind_acc_C, temp)
}
t.test(ind_acc_C, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_acc_C)
install.packages("lsr")
library(lsr)
cohensD( x = ind_acc_C, mu = 0.5)
# dishonesty
ind_acc_D <- c()
for(i in (setdiff(subFULL,subExclude))) {
  temp <- mean(center_data$acc_D[center_data$subID==i], na.rm=TRUE)
  ind_acc_D <- c(ind_acc_D, temp)
}
t.test(ind_acc_D, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_acc_D)
cohensD( x = ind_acc_D, mu = 0.5)
# selfishness
ind_acc_S <- c()
for(i in (setdiff(subFULL,subExclude))) {
  temp <- mean(center_data$acc_S[center_data$subID==i], na.rm=TRUE)
  ind_acc_S <- c(ind_acc_S, temp)
}
t.test(ind_acc_S, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_acc_S)
cohensD(x = ind_acc_S, mu = 0.5)
# trustworthiness
ind_acc_T <- c()
for(i in (setdiff(subFULL,subExclude))) {
  temp <- mean(center_data$acc_T[center_data$subID==i], na.rm=TRUE)
  ind_acc_T <- c(ind_acc_T, temp)
}
t.test(ind_acc_T, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_acc_T)
cohensD(x = ind_acc_T, mu = 0.5)
# generosity
ind_acc_G <- c()
for(i in (setdiff(subFULL,subExclude))) {
  temp <- mean(center_data$acc_G[center_data$subID==i], na.rm=TRUE)
  ind_acc_G <- c(ind_acc_G, temp)
}
t.test(ind_acc_G, alternative = 'greater', mu=0.5,  conf.level = 0.95)
sd(ind_acc_G)
cohensD(x = ind_acc_G, mu = 0.5)


####################################### Analysis 2: Mixed Effect Model  ############################################
################# Associations between individual-level trait ratings and corruption records  ######################
imagedata <- read.csv(file="/Study2_PhotoCharacteristic.csv",head=TRUE)
colnames(imagedata)[1] <- "imageID"
# standardize continous variables
imagedata$AGE <- (imagedata$age - mean(imagedata$age))/(sd(imagedata$age))
imagedata$PIXEL <- (imagedata$Megapixels - mean(imagedata$Megapixels))/(sd(imagedata$Megapixels))
# merge data
mydata <- merge(ind_data_noMissing, imagedata, by="imageID")
# standardize continous variables
mydata$std_C <- (mydata$ind_C - mean(mydata$ind_C))/sd(mydata$ind_C)
mydata$std_D <- (mydata$ind_D - mean(mydata$ind_D))/sd(mydata$ind_D)
mydata$std_S <- (mydata$ind_S - mean(mydata$ind_S))/sd(mydata$ind_S)
mydata$std_T <- (mydata$ind_T - mean(mydata$ind_T))/sd(mydata$ind_T)
mydata$std_G <- (mydata$ind_G - mean(mydata$ind_G))/sd(mydata$ind_G)
# mixed model: within-subjects and between-stimuli design 
# three random effects: subject, image nested within record type, and the interaction between subject and record type
install.packages("lme4")
library(lme4)
library(car)

# ---- (1) full data set ----
# Corruptibility Ratings
modelC <- glmer(convicted ~ std_C +  (1|subID), family = binomial, data = mydata)
modelCPH <- glmer(convicted ~ std_C + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID) , family = binomial, data=mydata)
summary(modelC)
summary(modelCPH)
anova(modelC,ddf="Satterthwaite")
anova(modelCPH,ddf="Satterthwaite")
confint(modelC,  method="Wald")
confint(modelCPH,  method="Wald")
# Dishonesty Ratings
modelD <- glmer(convicted ~ std_D +  (1|subID), family = binomial, data = mydata)
modelDPH <- glmer(convicted ~ std_D + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID),family = binomial, data=mydata)
summary(modelD)
summary(modelDPH)
anova(modelD,ddf="Satterthwaite")
anova(modelDPH,ddf="Satterthwaite")
confint(modelD,  method="Wald")
confint(modelDPH,  method="Wald")
# Selfishness Ratings
modelS <- glmer(convicted ~ std_S +  (1|subID), family = binomial, data = mydata)
modelSPH <- glmer(convicted ~ std_S + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID), family = binomial,data=mydata)
summary(modelS)
summary(modelSPH)
anova(modelS,ddf="Satterthwaite")
anova(modelSPH,ddf="Satterthwaite")
confint(modelS,  method="Wald")
confint(modelSPH,  method="Wald")
# Trustworthiness Ratings
modelT <- glmer(convicted ~ std_T +  (1|subID), family = binomial, data = mydata)
modelTPH <- glmer(convicted ~ std_T + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID), family = binomial, data=mydata)
summary(modelT)
summary(modelTPH)
anova(modelT,ddf="Satterthwaite")
anova(modelTPH,ddf="Satterthwaite")
confint(modelT,  method="Wald")
confint(modelTPH,  method="Wald")
# Generosity Ratings
modelG <- glmer(convicted ~ std_G +  (1|subID), family = binomial, data = mydata)
modelGPH <- glmer(convicted ~ std_G + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID) , family = binomial, data=mydata)
summary(modelG)
summary(modelGPH)
anova(modelG,ddf="Satterthwaite")
anova(modelGPH,ddf="Satterthwaite")
confint(modelG,  method="Wald")
confint(modelGPH,  method="Wald")

# ---- (2) Subset of data that contains officials whose photos were from government and personal websites ----
# Corruptibility Ratings
sub_gov <- mydata[mydata$ImgSource==1,] 
# Officials N = 69 (33 convicted, 36 clean)
# bald:5, beard:8, mustche:12
modelCPH <- glmer(convicted ~ std_C + AGE + glasses + bald + beard + mustache + smile + PIXEL + 
                    (1|subID) , family = binomial, data=sub_gov)
summary(modelCPH)
anova(modelCPH,ddf="Satterthwaite")
confint(modelCPH,  method="Wald")
# Dishonesty Ratings
modelDPH <- glmer(convicted ~ std_D + AGE + glasses + bald + beard + mustache + smile + PIXEL +
                    (1|subID),family = binomial, data=sub_gov)
summary(modelDPH)
anova(modelDPH,ddf="Satterthwaite")
confint(modelDPH,  method="Wald")
# Selfishness Ratings
modelSPH <- glmer(convicted ~ std_S + AGE + glasses + bald + beard + mustache + smile + PIXEL + 
                    (1|subID), family = binomial,data=sub_gov)
summary(modelSPH)
anova(modelSPH,ddf="Satterthwaite")
confint(modelSPH,  method="Wald")
# Trustworthiness Ratings
modelTPH <- glmer(convicted ~ std_T + AGE + glasses + bald + beard + mustache + smile + PIXEL + 
                    (1|subID), family = binomial, data=sub_gov)
summary(modelTPH)
anova(modelTPH,ddf="Satterthwaite")
confint(modelTPH,  method="Wald")
# Generosity Ratings
modelGPH <- glmer(convicted ~ std_G + AGE + glasses + bald + beard + mustache + smile + PIXEL + 
                    (1|subID) , family = binomial, data=sub_gov)
summary(modelGPH)
anova(modelGPH,ddf="Satterthwaite")
confint(modelGPH,  method="Wald")

# ---- (3) Subset of data that contains officials whose photo taken date was known ----
sub_date <- mydata[mydata$uncertain.date==0,] 
# Officials N = 75 (35 convicted, 40 clean)
# bald: 5, beard: 9, mustache: 15
# Corruptibility Ratings
modelCPH <- glmer(convicted ~ std_C + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID) , family = binomial, data=sub_date)
summary(modelCPH)
anova(modelCPH,ddf="Satterthwaite")
confint(modelCPH,  method="Wald")
# Dishonesty Ratings
modelDPH <- glmer(convicted ~ std_D + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID),family = binomial, data=sub_date)
summary(modelDPH)
anova(modelDPH,ddf="Satterthwaite")
confint(modelDPH,  method="Wald")
# Selfishness Ratings
modelSPH <- glmer(convicted ~ std_S + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID), family = binomial,data=sub_date)
summary(modelSPH)
anova(modelSPH,ddf="Satterthwaite")
confint(modelSPH,  method="Wald")
# Trustworthiness Ratings
modelTPH <- glmer(convicted ~ std_T + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID), family = binomial, data=sub_date)
summary(modelTPH)
anova(modelTPH,ddf="Satterthwaite")
confint(modelTPH,  method="Wald")
# Generosity Ratings
modelGPH <- glmer(convicted ~ std_G + AGE + glasses + bald + beard + mustache + smile + PIXEL + ImgSource +
                    (1|subID) , family = binomial, data=sub_date)
summary(modelGPH)
anova(modelGPH,ddf="Satterthwaite")
confint(modelGPH,  method="Wald")





