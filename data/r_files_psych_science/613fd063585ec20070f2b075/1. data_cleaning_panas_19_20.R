# -----------------------Sep 13,2021-------------------------
# -----------------------Author: Rui Sun---------------------
# --------Data preparation for 2019 vs 2020 comparison-----------

# setwd("")

library(foreign)
library(nlme)
library(data.table)
library(dplyr)
library(psych)
library(ggplot2)
library(MuMIn)

# data resource: 
# personality panel: https://www.dataarchive.lissdata.nl/study_units/view/14
# researchers can apply to access the data through the liss panel

liss19 <- read.spss("cp19k_EN_1.0p.sav",to.data.frame=TRUE)
liss20 <- read.spss("cp20l_EN_1.0p.sav",to.data.frame=TRUE)
liss19_1 <- liss19[,c(1,2,120:139)]
liss20_1 <- liss20[,c(1,2,120:139)]

colname_panas <- c("id","year_month","interested","distressed","excited","upset","strong","guilty","scared","hostile","enthusiastic","proud","irritable","alert","ashamed","inspired","nervous","determined","attentive","jittery","active","afraid")

ll <- list(liss19_1,liss20_1)

#------------ change all variables in all dataframes to character
temp <- lapply(ll,function(x) lapply(x, as.character))
names(temp) <- c("liss19_1","liss20_1")
list2env(temp, envir = .GlobalEnv)
names(liss20_1)

# -------------------rename variables in all dataframes
ChangeNames <- function(x) {
  names(x) <- colname_panas
  return(x)
}
ll <- lapply(ll, ChangeNames)

data <- bind_rows(ll, .id = "column_label")

data[,4:23] <- sapply(data[,4:23],as.numeric)

data$Pos <- rowMeans(data[,c("interested","excited","strong","enthusiastic","proud","alert","inspired","determined","attentive","active")],na.rm = TRUE)
data$Neg <- rowMeans(data[,c("distressed","upset","guilty","scared","hostile","irritable","ashamed","nervous","jittery","afraid")],na.rm = TRUE)

# --- create year and covid condition

data$year[data$year_month==201905|data$year_month==201906] <- 2019
data$year[data$year_month==202005|data$year_month==202006] <- 2020

data$covid[data$year<2020] <- 0
data$covid[data$year==2020] <- 1

data <- data[complete.cases(data),] 
# there're some participants who didn't have panas data, we remove those
#10998 to 10886

# choose those who answered 2 times
n_occur <- data.frame(table(data$id))
n_occur2 <- n_occur[which(n_occur$Freq==2),]
n_occur2$Var1 <- as.character(n_occur2$Var1)
data$id <- as.character(data$id)
df <- setDT(data)[id %chin% n_occur2$Var1] 
#8740; therefore, 4370ppts finished the survey in both years



# ---------------
# demographic information of the participants
# ---------------
# 4370 participants took part in the study in both 2019 and 2020  

# -------------------
# data resource: 
# background data: https://www.dataarchive.lissdata.nl/study_units/view/322
# researchers can apply to access the data through the liss panel

# now load background info
demo <- read.spss("avars_202001_EN_1.0p_backgroundinfo.sav",to.data.frame=TRUE)
# 0 Male
# 1 Female

demo$gender[demo$geslacht=="Male"] <- 0
demo$gender[demo$geslacht=="Female"] <- 1
demo$id <- demo$nomem_encr
demo1 <- demo[,c("id","gebjaar","gender")]
# ------------------------------------------------------

data1 <- merge.data.frame(df,demo1,by="id",all.x=TRUE)
data1$age <- data1$year-data1$gebjaar
data <- data1
rm(data1,demo,demo1)

# get initial age
ini_age <- data[which(data$year==2019),][,c("id","age")]
colnames(ini_age)[2] <- "ini_age"
ini_age$scale_ini_age <- scale(ini_age$ini_age)

data <- merge.data.frame(data,ini_age,by="id",all.x = TRUE)

data$covid <- as.factor(data$covid)
data$gender <- as.factor(data$gender)


# ---------------
# subjective health information of the participants
# ---------------
# data resource: 
# https://www.dataarchive.lissdata.nl/study_units/view/12
# researchers can apply to access the data through the liss panel

# data loaded from the LISS Health survey
liss19_health <- read.spss("ch19l_EN_1.0p.sav",to.data.frame=TRUE)
liss20_health <- read.spss("ch20m_EN_1.0p.sav",to.data.frame=TRUE)
liss19_1_health <- liss19_health[,c(1:6)]
liss20_1_health <- liss20_health[,c(1:6)]
names(liss19_1_health) <- c("id","year_month","gender","age","job","sub_health")
names(liss20_1_health) <- c("id","year_month","gender","age","job","sub_health")

liss19_1_health$covid <- 0
liss20_1_health$covid <- 1

health <- rbind.data.frame(liss19_1_health,liss20_1_health)

health$year[health$year_month==201911|health$year_month==201912] <- 2019
health$year[health$year_month==202011|health$year_month==202012] <- 2020

health1 <- health[,c("id","job","sub_health","year","covid")]

data <- merge.data.frame(data,health1,by=c("id","year","covid"),all.x=TRUE)

data$health[data$sub_health=="poor"] <- 1
data$health[data$sub_health=="moderate"] <- 2
data$health[data$sub_health=="good"] <- 3
data$health[data$sub_health=="very good"] <- 4
data$health[data$sub_health=="excellent"] <- 5

# ---------------
# Personality information of the participants
# ---------------
# =================personality data=======================
liss19_2 <- liss19[,c(1,2,7:11,13:62)]
liss20_2 <- liss20[,c(1,2,7:11,13:62)]
ll2 <- list(liss19_2,liss20_2)
#------------ change all variables in all dataframes to character
temp <- lapply(ll2,function(x) lapply(x, as.character))
names(temp) <- c("liss19_2","liss20_2")
list2env(temp, envir = .GlobalEnv)
names(liss20_2)

# -------------------rename variables in all dataframes
colname_swl_ipip <- c("id","year_month","SWL1","SWL2","SWL3","SWL4","SWL5",paste0("IPIP",1:50))
ChangeNames <- function(x) {
  names(x) <- colname_swl_ipip
  return(x)
}
ll2 <- lapply(ll2, ChangeNames)

# ----------------------create one dataframe from the list
data2 <- bind_rows(ll2, .id = "column_label")
write.csv(data2,"liss_swl_ipip_19_20.csv")

data2 <- read.csv("liss_swl_ipip_19_20_numeric.csv")
data2$IPIP2_r <- 6-data2$IPIP2
data2$IPIP4_r <- 6-data2$IPIP4
data2$IPIP6_r <- 6-data2$IPIP6
data2$IPIP8_r <- 6-data2$IPIP8
data2$IPIP10_r <- 6-data2$IPIP10
data2$IPIP12_r <- 6-data2$IPIP12
data2$IPIP14_r <- 6-data2$IPIP14
data2$IPIP16_r <- 6-data2$IPIP16
data2$IPIP18_r <- 6-data2$IPIP18
data2$IPIP20_r <- 6-data2$IPIP20
data2$IPIP22_r <- 6-data2$IPIP22
data2$IPIP24_r <- 6-data2$IPIP24
data2$IPIP26_r <- 6-data2$IPIP26
data2$IPIP28_r <- 6-data2$IPIP28
data2$IPIP29_r <- 6-data2$IPIP29
data2$IPIP30_r <- 6-data2$IPIP30
data2$IPIP32_r <- 6-data2$IPIP32
data2$IPIP34_r <- 6-data2$IPIP34
data2$IPIP36_r <- 6-data2$IPIP36
data2$IPIP38_r <- 6-data2$IPIP38
data2$IPIP39_r <- 6-data2$IPIP39
data2$IPIP44_r <- 6-data2$IPIP44
data2$IPIP46_r <- 6-data2$IPIP46
data2$IPIP49_r <- 6-data2$IPIP49

# Extraversion items: 1,6,11,16,21,26,31,36,41,46
# Agreeableness items: 2,7,12,17,22,27,32,37,42,47
# Conscientiousness items: 3,8,13,18,23,28,33,38,43,48
# EmoStability items: 4,9,14,19,24,29,34,39,44,49
# Intellect/Imagination items: 5,10,15,20,25,30,35,40,45,50

data2$Extraversion <- rowMeans(data2[,c("IPIP1","IPIP6_r","IPIP11","IPIP16_r","IPIP21","IPIP26_r","IPIP31","IPIP36_r","IPIP41","IPIP46_r")])
data2$Agreeable <- rowMeans(data2[,c("IPIP2_r","IPIP7","IPIP12_r","IPIP17","IPIP22_r","IPIP27","IPIP32_r","IPIP37","IPIP42","IPIP47")])
data2$Conscientiousness <- rowMeans(data2[,c("IPIP3","IPIP8_r","IPIP13","IPIP18_r","IPIP23","IPIP28_r","IPIP33","IPIP38_r","IPIP43","IPIP48")])
data2$EmoStability <- rowMeans(data2[,c("IPIP4_r","IPIP9","IPIP14_r","IPIP19","IPIP24_r","IPIP29_r","IPIP34_r","IPIP39_r","IPIP44_r","IPIP49_r")])
data2$Openness <- rowMeans(data2[,c("IPIP5","IPIP10_r","IPIP15","IPIP20_r","IPIP25","IPIP30_r","IPIP35","IPIP40","IPIP45","IPIP50")])

data2$covid[data2$year<202000] <- 0
data2$covid[data2$year>202000] <- 1

data2$SWL <- rowMeans(data2[,c("SWL1","SWL2","SWL3","SWL4","SWL5")],na.rm = TRUE)

data_swl_ipip <- data2[,c("id","SWL","Extraversion","Agreeable","Conscientiousness","EmoStability","Openness","covid" )]

# =====adding personality and swl into 2019/2020 data=======
dat <- merge.data.frame(data,data_swl_ipip,by=c("id","covid"),all.x=TRUE)

write.csv(dat,"dat2019_2020_to_use.csv")


