####################################### Study 4b Analyses: mixed model #######################################
# ---- Perception differences between versions ----
mydata <- read.csv(file = "/Study4b.csv", header = TRUE)
# slim
slimID <- c()
for (i in c(1:150)) {
  temp <- 1 + 3*(i-1)
  slimID <- c(slimID, temp)
}
slimSet <- paste("a",slimID,sep="")
slim <- data.frame(t(mydata[names(mydata) %in% slimSet]))
# origin
originID <- c()
for (i in c(1:150)) {
  temp <- 2 + 3*(i-1)
  originID <- c(originID, temp)
}
originSet <- paste("a",originID,sep="")
origin <- data.frame(t(mydata[names(mydata) %in% originSet]))
# fat
fatID <- c()
for (i in c(1:150)) {
  temp <- 3*i
  fatID <- c(fatID, temp)
}
fatSet <- paste("a",fatID,sep="")
fat <- data.frame(t(mydata[names(mydata) %in% fatSet]))
# long-form data
imageID <- rep(c(1:150),80)
subFULL <- c(1:100)
subExclude <- c(1,7,8,9,16,21,30,31,39,40,42,44,55,56,64,65,78,80,81,82)
subID <- c()
for (i in (setdiff(subFULL,subExclude))) {subID <- c(subID, rep(i,150))}
ind_slim <- c()
for(j in c(1:ncol(slim))) {
  ind_slim <- c(ind_slim, slim[,j])
}
ind_fat <- c()
for(j in c(1:ncol(fat))) {
  ind_fat <- c(ind_fat, fat[,j])
}
ind_origin <- c()
for(j in c(1:ncol(origin))) {
  ind_origin <- c(ind_origin, origin[,j])
}
data_slim <- data.frame(cbind(subID, condition=rep(1,150), imageID, rating=ind_slim))
data_origin <- data.frame(cbind(subID, condition=rep(2,150), imageID, rating=ind_origin))
data_fat <- data.frame(cbind(subID, condition=rep(3,150), imageID, rating=ind_fat))
# mixed model
# fat-slim
model_fs <- lmer(rating ~ condition + (1|subID) + (1|imageID) + (1|subID:condition), data=rbind(data_slim,data_fat))
summary(model_fs)
anova(model_fs,ddf="Satterthwaite")
confint(model_fs,  method="Wald")
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}
r2.corr.mer(model_fs)
# fat-origin
model_fo <- lmer(rating ~ condition + (1|subID) + (1|imageID) + (1|subID:condition), data=rbind(data_fat,data_origin))
summary(model_fo)
anova(model_fo,ddf="Satterthwaite")
confint(model_fo,  method="Wald")
r2.corr.mer(model_fo)
# origin-slim
model_os <- lmer(rating ~ condition + (1|subID) + (1|imageID) + (1|subID:condition), data=rbind(data_origin,data_slim))
summary(model_os)
anova(model_os,ddf="Satterthwaite")
confint(model_os,  method="Wald")
r2.corr.mer(model_os)

