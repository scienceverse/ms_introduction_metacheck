rm(list=ls())

# setworking directory
#setwd("~/Documents/2012_fall_psy254/kimLee/")

# read in original qualtrics data
original.d <- read.csv("data_original.csv", header = T, stringsAsFactors=FALSE)
head(original.d)
names(original.d)

original.dd = original.d[-1,]

########### trim original data (drop unnecessary variables)
d <- subset (original.dd, select = c("V6", "mturkcode", "condition", "a_enjoy_1", "i_enjoy_1", "w1", "w2", "w3"
                                     ,"w4", "w5" , "w6", "w7", "w8", "w9", "w10", "w11", "w12", "w13", "w14", "w15"
                                     , "m1", "m2", "m3", "m4", "m5", "m6", "gender", "age", "edu", "major"))

names(d)[1] <- "IPAddress"


# combine enjoy columns
d$enjoy <- ifelse (d$condition == "action", d$a_enjoy_1, d$i_enjoy_1)
d$enjoy <- as.numeric(d$enjoy)
names(d)

# covert to numeric and factor 
d$age <- as.numeric(d$age)
d$gender <- as.factor(d$gender)
d$condition <- as.factor(d$condition)


########### grading verbal and math questions 
grade = function(data, cols, correctAnswer) {
  i = 0
  dTemp = data
  for(col in cols){
    i = i + 1
    ccol = paste("c", col, sep ="")
    dTemp[[ccol]] <- as.numeric(data[[col]] == correctAnswer[i])
  }
  dTemp
}

## grading answers
gra.d <- grade(d, c("w1", "w2", "w3","w4","w5","w6","w7","w8","w9","w10","w11","w12","w13","w14","w15", 
           "m1", "m2","m3", "m4", "m5", "m6"), c(1, 2, 5, 3, 1, 4, 3, 3, 4, 4, 3, 2, 1, 2, 2, 4, 4, 4, 2, 4, 3))


## calculating TOTAL scores

gra.d$vTotal = with(cw1 + cw2 + cw3 + cw4 + cw5 + cw6 + cw7 + cw8 + cw9 + cw10 + cw11 + cw12 + cw13
+cw14 + cw15, data=gra.d)

gra.d$mTotal = with(cm1 + cm2 + cm3 + cm4 + cm5 +cm6, data=gra.d)

gra.d$totalScore = with( mTotal + vTotal,data=gra.d)
summary(gra.d$totalScore)


###### adding duration variable to the dataset

qual.duration <- read.csv("qualtrics.duration.csv")
qual.duration <- qual.duration[,1:3] # first three columns only

gra.d <- merge (gra.d, qual.duration, by.x = "IPAddress", by.y = "IPAddress", all.x = F, all.y = F) 
names(gra.d)

## qualtrics duration in seconds calucations

s <- gsub ("m", "", gra.d$Duration)
s <- gsub ("s", "", s)
s <- strsplit (s, " ")
s[[1]][1]

min <- c(1:length(s))
sec <- c(1:length(s))

for (i in 1:length(s)) {
  s[[i]][1] -> min[i]
  s[[i]][2] -> sec[i]
}


gra.d$qual.duration <- as.numeric(min) * 60 + as.numeric(sec)

head(gra.d)


## check duration outliers +/- 3 sd away dropped (Repeat the process as many as necessary)

means <- mean(gra.d$qual.duration, na.rm = T)
sds <- sd(gra.d$qual.duration, na.rm = T)

table(ifelse (gra.d$qual.duration < (as.numeric(means) - 3*as.numeric(sds)), "T", "F"))
table(ifelse (gra.d$qual.duration > (as.numeric(means) + 3*as.numeric(sds)), "T", "F"))

gra.d <- subset (gra.d, (means-3*sds < gra.d$qual.duration & gra.d$qual.duration < means+3*sds))



## check Accuracy and outliers +/- 3 sd away dropped (repeat this as many as necessary)

means <- mean(gra.d$totalScore, na.rm = T)
sds <- sd(gra.d$totalScore, na.rm = T)

table(ifelse (gra.d$totalScore < (as.numeric(means) - 3*as.numeric(sds)), "T", "F"))
table(ifelse (gra.d$totalScore > (as.numeric(means) + 3*as.numeric(sds)), "T", "F"))

gra.d <- subset (gra.d, (means-3*sds < gra.d$totalScore & gra.d$totalScore < means+3*sds))



## final dataset with 81 participants

final <- gra.d
head(final)
dim(final)


## adjusting row numbers 
row.names(final) <- 1:nrow(final)

## create subid variable

final$subid = 1:nrow(final)

head(final)
dim(final)
names(final)
final

write.csv(final, "data_prepped.csv")

## save R data
save(final, d, gra.d,  file="Final.Rdata")






