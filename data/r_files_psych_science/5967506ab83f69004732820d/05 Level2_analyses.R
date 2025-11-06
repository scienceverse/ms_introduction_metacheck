#Run this on valid.sub

library(Hmisc)
library(foreign)

### LOAD PACKAGES AND DATA 

#input.1 = "C:/Users/mluhmann/Dropbox/SOEP_fit"
input.1 = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data"

setwd(input.1)

valid <- read.delim("valid.csv")

jobs <- aggregate(valid[, c(4, 28, 7, 18:27)], by=list(valid$ISCO), FUN=mean, na.rm=TRUE)
rcorr(as.matrix(jobs[,2:14]))

hist(jobs$ba2_c)

##Check conscientiousness jobs
ratings <- read.delim("~/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/ba2_ratings.csv")

names(jobs)[1] <- "ISCO"
jobs <- merge(jobs, ratings[,1:2], by="ISCO")

jobs$ba2_c.c <- scale(jobs$ba2_c)
jobs$ba2_c_grp <- cut(jobs$ba2_c.c, 
                         breaks = c(-Inf, -1, +1, Inf), 
                         labels = c("L", "M", "H"), 
                         right = FALSE)

jobs_c <- jobs[order(jobs$ba2_c.c),]

a <- 1:176
jobs_c <- jobs_c[(a[seq(1, length(a), 4)]),]

write.table(valid2, "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/check_ba2_c_jobs.csv", sep="\t") 
