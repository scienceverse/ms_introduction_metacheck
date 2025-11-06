data4 <- read.table("data_mTurk.txt", sep = "\t", head = T)

data4 <- data4[, c(2:41, 72)]

temp <- matrix(0, nrow(data4[apply(data4, 1, function(x) sum(!is.na(x)) > 2),]), 11)

count <- 1
for(i in 1:nrow(data4)) {
  if(sum(!is.na(data4[i,])) > 2) {
    temp[count,] <- data4[i,][!is.na(data4[i,])]
    count <- count + 1
  }
}

exclude <- function (x) {any(matrix(table(x)) > 8)}

data4 <- temp[!apply(temp, 1, exclude),]

fluent <- c(F, F, T, T, F, F, T, F, T, T)

data4 <- data.frame(data4)

data4 <- cbind(data4, 
               easy = apply(data4, 1, function(x) mean(x[c(fluent, F)])),
               hard = apply(data4, 1, function(x) mean(x[c(!fluent, F)])),
               syria = data4[,11] %in% c(840189, 840198),
               careful = data4[,11] %in% c(840189, 840201),
               )
data4 <- cbind(data4,
               dif = data4$easy - data4$hard)

unpairedTest(data4$dif[data4$syria], data4$dif[!data4$syria])
singleTest(data4$dif[data4$syria])
singleTest(data4$dif[!data4$syria])
singleTest(data4$dif)

summary(aov(dif ~ syria * careful, data = data4))



