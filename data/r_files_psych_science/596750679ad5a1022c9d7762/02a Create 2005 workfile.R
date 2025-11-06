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


### IMPORT DATA FILES ###

#note: vp.sav is somehow deficient...
setwd(input.1)
vp05 <- read.table("vp.csv", header=TRUE, sep=",", strip.white=TRUE)
#vp05[vp05<0]<-NA

#extract labels, to check whether the correct variables were used
#variables05 <- read.spss("/Users/jaapdenissen/Documents/Work/Scientific resources/Datasets/soep/SOEP CD1/data/soep/spss_en/vp.sav")
#variables05 <- as.data.frame(attr(variables05, "variable.labels") )
#names(variables05)[1] <- "var"
#write.table(variables05, "/Users/jaapdenissen/Documents/Work/Projects/RSA/variables05.csv", sep="\t")

setwd(input.2)

vpcal05 <- read.spss("vpkal.sav", to.data.frame=T, use.value.labels = F)
names(vpcal05) <- tolower(names(vpcal05))
vpcal05[vpcal05<0]<-NA

vpgen05 <- read.spss("vpgen.sav", to.data.frame=T, use.value.labels = F)
names(vpgen05) <- tolower(names(vpgen05))
vpgen05[vpgen05<0]<-NA

vhbrutto05 <- read.spss("vhbrutto.sav", to.data.frame=T, use.value.labels = F)
names(vhbrutto05) <- tolower(names(vhbrutto05))
vhbrutto05[vhbrutto05<0]<-NA

vh <- read.spss("vh.sav", to.data.frame=T, use.value.labels = F)
names(vh) <- tolower(names(vh))
vh[vh<0]<-NA

setwd(input.1)
ba2_ratings <- read.delim("ba2_ratings.csv")


# EXTRACT PERSONALITY VARIABLES AND RECODE IF NECESSARY ### 

vp05$ex_1 <- vp05$vp12502
vp05$ex_2 <- vp05$vp12508
vp05$ex_3 <- 8-vp05$vp12512

vp05$ag_1 <- 8-vp05$vp12503
vp05$ag_2 <- vp05$vp12506
vp05$ag_3 <- vp05$vp12513

vp05$co_1 <- vp05$vp12501
vp05$co_2 <- 8-vp05$vp12507
vp05$co_3 <- vp05$vp12511

vp05$ne_1 <- vp05$vp12505
vp05$ne_2 <- vp05$vp12510
vp05$ne_3 <- 8-vp05$vp12515

vp05$op_1 <- vp05$vp12504
vp05$op_2 <- vp05$vp12509
vp05$op_3 <- vp05$vp12514


# Internal consistencies and item analysis for personality items

alpha(vp05[,c("ex_1", "ex_2", "ex_3")])
alpha(vp05[,c("ag_1", "ag_2", "ag_3")])
alpha(vp05[,c("co_1", "co_2", "co_3")])
alpha(vp05[,c("ne_1", "ne_2", "ne_3")])
alpha(vp05[,c("op_1", "op_2", "op_3")])


# Create summary scores for personality scales

vp05$per_e <- rowMeans(vp05[,c("ex_1", "ex_2", "ex_3")])
vp05$per_a <- rowMeans(vp05[,c("ag_1", "ag_2", "ag_3")])
vp05$per_c <- rowMeans(vp05[,c("co_1", "co_2", "co_3")])
vp05$per_n <- rowMeans(vp05[,c("ne_1", "ne_2", "ne_3")])
vp05$per_o <- rowMeans(vp05[,c("op_1", "op_2", "op_3")])


### CREATE AND RECODE COVARIATES

#vh datafile
vh$children <- vh$vh5003

#vp datafile
vp05$female <- vp05$vp14701 - 1
vp05$age <- 2005 - vp05$vp14702 
vp05$foreign <- vp05$vp135 - 1
vp05$marstat <- factor(vp05$vp148, levels=1:5, labels=c("married", "separated", "never married", "divorced", "widowed"))

vp05$jobsat <- vp05$vp0102
vp05$incsat <- vp05$vp0105
vp05$lifsat <- vp05$vp154
vp05$heasat <- vp05$vp0101
vp05$wrkstatus <- factor(vp05$vp10, levels=1:9, labels=c("fulltime", "parttime", "training", "geringfueg", "alterteilzeit", "wehrdienst", "zivildienst", "behindertenwerkstatt", "nicht erwerbstaetig"))
vp05$tempwrk <- 2 - vp05$vp40

# add net amount of wage income and self-employed income
#vp05$wages <- vp05$vp100a03
#vp05$fees <- vp05$vp100b03

vp05$pockmon <- vp05$vp79

vpgen05$income <- vpgen05$labgro05

#occupational prestige (SIOPS)
vpgen05$prestige <- vpgen05$siops05

# Education 
vpgen05$educat <- vpgen05$vbilzeit
#vpgen05[which(vpgen05$educat<0),] <- NA

#years in fulltime
vpgen05$experience <- vpgen05$expft05

#avg work hours pro week (incl. self-employed)
vpgen05$wrkhrs <- vpgen05$vtatzeit

# Worktime (how many months did people work in the past year)
vpcal05$work.full <- vpcal05$vp1a02

# Current sample region (dataset: vhbrutto05)
# 1 = Westdeutschland, alte Bundeslaender, 2 = Ostdeutschland, neue Bundeslaender

vhbrutto05$east = factor(vhbrutto05$vsampreg, levels = c(1,2), labels = c("west", "east"))

# select key variables from various datasets

names(vp05)
vp05.sub <- cbind(vp05[c("persnr","vhhnr")], vp05[543:558])
vpgen05.sub <- vpgen05[,c("vhhnr", "persnr", "is8805", "prestige", "income", "educat", "experience", "wrkhrs")]
vpcal05.sub <- vpcal05[,c("vhhnr", "persnr", "work.full")]
east05.sub <- vhbrutto05[,c("vhhnr", "east")]

# Merge data (begin with individual data, keep all individuals)

a <- merge(vp05.sub, vpgen05.sub, by=c("persnr", "vhhnr"), all.x = TRUE)
b <- merge(a, vpcal05.sub, by=c("persnr", "vhhnr"), all.x = TRUE)
c <- merge(b, east05.sub, by=c("vhhnr"), all.x = TRUE)
names(c)[which(names(c)=="is8805")] <- "ISCO"

nrow(vp05.sub); nrow(a); nrow(b); nrow(c)

# Merge with job ratings, keep all individuals 
merged <- merge(c, ba2_ratings, by="ISCO", all.x = TRUE)
nrow(c); nrow(merged)

# Export as csv file
setwd(input.1)
write.table(merged, "soep_fit_2005.csv", sep="\t") 

