library(foreign)
library(psych)
library(lme4)

input.1 = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats"

### IMPORT DATA FILES ###

#note: the 2013 file contains around 10.000 more with very little information (are these other household members who did not take part in the core survey?)

setwd(input.1)

bdp13 <- read.dta("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/stata/bdp.dta", convert.factors = FALSE)
bdp13[bdp13<0]<-NA

bdpcal13 <- read.dta("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/stata/bdpkal.dta", convert.factors = FALSE)
bdpcal13[bdpcal13<0]<-NA

bdpgen13 <- read.dta("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/stata/bdpgen.dta", convert.factors = FALSE)
bdpgen13[bdpgen13<0]<-NA

bdhbrutto13 <- read.dta("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/stata/bdhbrutto.dta", convert.factors = FALSE)
bdhbrutto13[bdhbrutto13<0]<-NA

bdh13 <- read.dta("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/stata/bdh.dta", convert.factors = FALSE)
bdh13[bdh13<0]<-NA


ba2_ratings <- read.delim("/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats/Data/ba2_ratings.csv")

# EXTRACT PERSONALITY VARIABLES AND RECODE IF NECESSARY ### 

bdp13$ex_1 <- bdp13$bdp15102
bdp13$ex_2 <- bdp13$bdp15108
bdp13$ex_3 <- 8-bdp13$bdp15112

bdp13$ag_1 <- 8-bdp13$bdp15103
bdp13$ag_2 <- bdp13$bdp15106
bdp13$ag_3 <- bdp13$bdp15113

bdp13$co_1 <- bdp13$bdp15101
bdp13$co_2 <- 8-bdp13$bdp15107
bdp13$co_3 <- bdp13$bdp15111

bdp13$ne_1 <- bdp13$bdp15105
bdp13$ne_2 <- bdp13$bdp15110
bdp13$ne_3 <- 8-bdp13$bdp15115

bdp13$op_1 <- bdp13$bdp15104
bdp13$op_2 <- bdp13$bdp15109
bdp13$op_3 <- bdp13$bdp15114



# Internal consistencies and item analysis for personality items

alpha(bdp13[,c("ex_1", "ex_2", "ex_3")])
alpha(bdp13[,c("ag_1", "ag_2", "ag_3")])
alpha(bdp13[,c("co_1", "co_2", "co_3")])
alpha(bdp13[,c("ne_1", "ne_2", "ne_3")])
alpha(bdp13[,c("op_1", "op_2", "op_3")])


# Create summary scores for personality scales

bdp13$per_e <- rowMeans(bdp13[,c("ex_1", "ex_2", "ex_3")])
bdp13$per_a <- rowMeans(bdp13[,c("ag_1", "ag_2", "ag_3")])
bdp13$per_c <- rowMeans(bdp13[,c("co_1", "co_2", "co_3")])
bdp13$per_n <- rowMeans(bdp13[,c("ne_1", "ne_2", "ne_3")])
bdp13$per_o <- rowMeans(bdp13[,c("op_1", "op_2", "op_3")])


### CREATE AND RECODE COVARIATES

#vh datafile (ultimately not used)
bdh13$children <- bdh13$bch5003

#vp datafile
bdp13$female <- bdp13$bdp13401 - 1
bdp13$age <- 2013 - bdp13$bdp13403
bdp13$foreign <- bdp13$bdp143 - 1
bdp13$marstat <- factor(bdp13$bdp135, levels=1:5, labels=c("married", "separated", "never married", "divorced", "widowed"))

bdp13$jobsat <- bdp13$bdp0103
bdp13$incsat <- bdp13$bdp0106
bdp13$lifsat <- bdp13$bdp15801
bdp13$heasat <- bdp13$bdp0101

bdp13$wrkstatus <- factor(bdp13$bdp18, levels=1:9, labels=c("fulltime", "parttime", "training", "geringfueg", "alterteilzeit", "wehrdienst", "zivildienst", "behindertenwerkstatt", "nicht erwerbstaetig"))
bdp13$tempwrk <- 2 - bdp13$bdp48

bdp13$pockmon <- bdp13$bdp85

#datafile: gen

bdpgen13$income <- bdpgen13$labgro13

#occupational prestige (SIOPS)
bdpgen13$prestige <- bdpgen13$siops13

# Education 
bdpgen13$educat <- bdpgen13$bdbilzeit

#years in fulltime
bdpgen13$experience <- bdpgen13$expft13

#avg work hours pro week (incl. self-employed)
bdpgen13$wrkhrs <- bdpgen13$bdtatzeit

#datafile: cal

### xxxx which variable?

# Worktime (how many months did people work in the past year)
bdpcal13$work.full <- bdpcal13$bdp1a02

#dataset: brutto

# Current sample region 
# 1 = Westdeutschland, alte Bundeslaender, 2 = Ostdeutschland, neue Bundeslaender

bdhbrutto13$east = factor(bdhbrutto13$bdsampreg, levels = c(1,2), labels = c("west", "east"))

# select key variables from various datasets

names(bdp13)
bdp13.sub <- cbind(bdp13[c("persnr","bdhhnr", "sample1")], bdp13[863:874])
bdpgen13.sub <- bdpgen13[,c("bdhhnr", "persnr", "is8813", "prestige", "income", "educat", "experience", "wrkhrs")]
bdpcal13.sub <- bdpcal13[,c("bdhhnr", "persnr", "work.full")]
east05.sub <- bdhbrutto13[,c("bdhhnr", "east")]

# Merge data (begin with individual data, keep all individuals)

a <- merge(bdp13.sub, bdpgen13.sub, by=c("persnr", "bdhhnr"), all.x = TRUE)
b <- merge(a, bdpcal13.sub, by=c("persnr", "bdhhnr"), all.x = TRUE)
c <- merge(b, east05.sub, by=c("bdhhnr"), all.x = TRUE)
names(c)[which(names(c)=="is8813")] <- "ISCO"

nrow(bdp13.sub); nrow(a); nrow(b); nrow(c)

# Merge with job ratings, keep all individuals 
merged <- merge(c, ba2_ratings, by="ISCO", all.x = TRUE)
nrow(c); nrow(merged)

#remove samples from #24 onwards (see below)
merged <- merged[which(merged$sample1<24),]

# Export as csv file
setwd(input.1)
write.table(merged, "soep_fit_2013.csv", sep="\t") 