###################################
### PERSON-JOB-FIT AND INCOME #####
###################################

##########################
#### DATA PREPARATION ####
##########################

input.1 = "/Users/jaapdenissen/Dropbox/perdev/09_Amsterdam/SOEP_fit_stats"
input.2 = "/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/"

#input.1 = "C:/Users/Maike Luhmann/Dropbox/SOEP_fit"
#input.2 = "G:/Koeln/Data/cs-transfer/spss_de_v30beta"

library(foreign)
library(psych)
library(lme4)

zp09 <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/zp.sav", to.data.frame=T, use.value.labels = F)
names(zp09) <- tolower(names(zp09))
zp09[zp09<0]<-NA

zpcal09 <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/zpkal.sav", to.data.frame=T, use.value.labels = F)
names(zpcal09) <- tolower(names(zpcal09))
zpcal09[zpcal09<0]<-NA

zpgen09 <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/zpgen.sav", to.data.frame=T, use.value.labels = F)
names(zpgen09) <- tolower(names(zpgen09))
zpgen09[zpgen09<0]<-NA

zhbrutto09 <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/zhbrutto.sav", to.data.frame=T, use.value.labels = F)
names(zhbrutto09) <- tolower(names(zhbrutto09))
zhbrutto09[zhbrutto09<0]<-NA

zh <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/zh.sav", to.data.frame=T, use.value.labels = F)
names(zh) <- tolower(names(zh))
zh[zh<0]<-NA

ba2_ratings <- read.delim("~/Documents/Work/Projects/RSA/ba2_ratings.csv")

#vp12502, vp12508, vp12512, = zp12002, zp12008, zp12012,

zp09$ex_1 <- zp09$zp12002
zp09$ex_2 <- zp09$zp12008
zp09$ex_3 <- 8-zp09$zp12012

#vp12503, vp12506, vp12513, = zp12003, zp12006, zp12013,

zp09$ag_1 <- 8-zp09$zp12003
zp09$ag_2 <- zp09$zp12006
zp09$ag_3 <- zp09$zp12013

#vp12501, vp12507, vp12511, = zp12001, zp12007, zp12011,

zp09$co_1 <- zp09$zp12001
zp09$co_2 <- 8-zp09$zp12007
zp09$co_3 <- zp09$zp12011

#  vp12505, vp12510, vp12515, = zp12005, zp12010, zp12015,

zp09$ne_1 <- zp09$zp12005
zp09$ne_2 <- zp09$zp12010
zp09$ne_3 <- 8-zp09$zp12015

#vp12504, vp12509, vp12514, = zp12004, zp12009, zp12014,

zp09$op_1 <- zp09$zp12004
zp09$op_2 <- zp09$zp12009
zp09$op_3 <- zp09$zp12014

alpha(zp09[,c("ex_1", "ex_2", "ex_3")])
alpha(zp09[,c("ag_1", "ag_2", "ag_3")])
alpha(zp09[,c("co_1", "co_2", "co_3")])
alpha(zp09[,c("ne_1", "ne_2", "ne_3")])
alpha(zp09[,c("op_1", "op_2", "op_3")])

zp09$per_e <- rowMeans(zp09[,c("ex_1", "ex_2", "ex_3")])
zp09$per_a <- rowMeans(zp09[,c("ag_1", "ag_2", "ag_3")])
zp09$per_c <- rowMeans(zp09[,c("co_1", "co_2", "co_3")])
zp09$per_n <- rowMeans(zp09[,c("ne_1", "ne_2", "ne_3")])
zp09$per_o <- rowMeans(zp09[,c("op_1", "op_2", "op_3")])

### CREATE AND RECODE COVARIATES

#zh datafile
zh$children <- zh$zh5103

#vp datafile
zp09$female <- zp09$zp12901 - 1
zp09$age <- 2009 - zp09$zp12902
zp09$foreign <- zp09$zp137 - 1
zp09$marstat <- factor(zp09$zp130, levels=1:5, labels=c("married", "separated", "never married", "divorced", "widowed"))

zp09$jobsat <- zp09$zp0103
zp09$incsat <- zp09$zp0106
zp09$lifsat <- zp09$zp15701
zp09$heasat <- zp09$zp0101

zp09$wrkstatus <- factor(zp09$zp09, levels=1:9, labels=c("fulltime", "parttime", "training", "geringfueg", "alterteilzeit", "wehrdienst", "zivildienst", "behindertenwerkstatt", "nicht erwerbstaetig"))
zp09$tempwrk <- 2 - zp09$zp39

zp09$pockmon <- zp09$zp78

zpgen09$income <- zpgen09$labgro09

#occupational prestige (SIOPS)
zpgen09$prestige <- zpgen09$siops09

zpgen09$educat <- zpgen09$zbilzeit

#years in fulltime
zpgen09$experience <- zpgen09$expft09

#avg work hours pro week (incl. self-employed)
zpgen09$wrkhrs <- zpgen09$ztatzeit

# Worktime (how many months did people work in the past year)
zpcal09$work.full <- zpcal09$zp1a02

#vsampreg: 1 = Westdeutschland, alte Bundeslaender, 2 = Ostdeutschland, neue Bundeslaender
zhbrutto09$east = factor(zhbrutto09$zsampreg, levels = c(1,2), labels = c("west", "east"))

#select key variables from various datasets
names(zp09)
zp09.sub <- cbind(zp09[c("persnr","zhhnr")], zp09[569:584])
zpgen09.sub <- zpgen09[,c("zhhnr", "persnr", "is8809", "prestige", "income", "educat", "experience", "wrkhrs")]
zpcal09.sub <- zpcal09[,c("zhhnr", "persnr", "work.full")]
east09.sub <- zhbrutto09[,c("zhhnr", "east")]

a <- merge(zp09.sub, zpgen09.sub, by=c("persnr", "zhhnr"), all.x = TRUE)
b <- merge(a, zpcal09.sub, by=c("persnr", "zhhnr"), all.x = TRUE)
c <- merge(b, east09.sub, by=c("zhhnr"), all.x = TRUE)
names(c)[which(names(c)=="is8809")] <- "ISCO"

nrow(zp09.sub); nrow(a); nrow(b); nrow(c)

# Merge with job ratings, keep all individuals 
merged <- merge(c, ba2_ratings, by="ISCO", all.x = TRUE)
nrow(c); nrow(merged)

# Export as csv file
setwd(input.1)
write.table(merged, "soep_fit_2009.csv", sep="\t") 
