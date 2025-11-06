# -----------------------Sep 13,2021-------------------------
# -----------------------Author: Rui Sun---------------------
# ----------------Data cleaning for Study 2 LISS-------------
# -------------------Data resource: LISS panel---------------

# https://www.lissdata.nl/research/liss-core-study
# LISS personality panel: to get PANAS and big 5
# LISS background info: to get birth year and gender
# LISS health survey: to get health data (control variable in this study)


rm(list = ls())
setwd("")

library(foreign)
library(nlme)
library(data.table)
library(dplyr)
library(stringr)
library(psych)
library(ggplot2)

# LISS Personality Panel
liss08 <- read.spss("cp08a_1p_EN.sav",to.data.frame=TRUE)
liss09 <- read.spss("cp09b_1.0p_EN.sav",to.data.frame=TRUE)
liss10 <- read.spss("cp10c_1.0p_EN.sav",to.data.frame=TRUE)
liss11 <- read.spss("cp11d_1.0p_EN.sav",to.data.frame=TRUE)
liss12 <- read.spss("cp12e_1.0p_EN.sav",to.data.frame=TRUE)
liss13 <- read.spss("cp13f_EN_1.0p.sav",to.data.frame=TRUE)
liss14 <- read.spss("cp14g_EN_1.0p.sav",to.data.frame=TRUE)
liss15 <- read.spss("cp15h_EN_1.0p.sav",to.data.frame=TRUE)
liss17 <- read.spss("cp17i_EN_1.0p.sav",to.data.frame=TRUE)
liss18 <- read.spss("cp18j_EN_1.0p.sav",to.data.frame=TRUE)
liss19 <- read.spss("cp19k_EN_1.0p.sav",to.data.frame=TRUE)
liss20 <- read.spss("cp20l_EN_1.0p.sav",to.data.frame=TRUE)

# =============================
# =============================
# -------Get PANAS-------------
# =============================
# =============================
liss08_1 <- liss08[,c(1,3,149:168)]
liss09_1 <- liss09[,c(1,3,150:169)]
liss10_1 <- liss10[,c(1,3,151:170)]
liss11_1 <- liss11[,c(1,3,151:170)]
liss12_1 <- liss12[,c(1,2,150:169)]
liss13_1 <- liss13[,c(1,2,154:173)]
liss14_1 <- liss14[,c(1,2,149:168)]
liss15_1 <- liss15[,c(1,2,150:169)]
liss17_1 <- liss17[,c(1,2,149:168)]
liss18_1 <- liss18[,c(1,2,150:169)]
liss19_1 <- liss19[,c(1,2,120:139)]
liss20_1 <- liss20[,c(1,2,120:139)]

colname_panas <- c("id","year_month","interested","distressed","excited","upset","strong","guilty","scared","hostile","enthusiastic","proud","irritable","alert","ashamed","inspired","nervous","determined","attentive","jittery","active","afraid")

ll <- list(liss08_1,liss09_1,liss10_1,liss11_1,liss12_1,liss13_1,liss14_1,liss15_1,liss17_1,liss18_1,liss19_1,liss20_1)

#------------ change all variables in all dataframes to character
temp <- lapply(ll,function(x) lapply(x, as.character))
names(temp) <- c("liss08_1","liss09_1","liss10_1","liss11_1","liss12_1","liss13_1","liss14_1","liss15_1","liss17_1","liss18_1","liss19_1","liss20_1")
list2env(temp, envir = .GlobalEnv)
names(liss20_1)

# -------------------rename variables in all dataframes
ChangeNames <- function(x) {
  names(x) <- colname_panas
  return(x)
}
ll <- lapply(ll, ChangeNames)

# ----------------------create one dataframe from the list
data <- bind_rows(ll, .id = "column_label")

data[,4:23] <- sapply(data[,4:23],as.numeric)

data$Pos <- rowMeans(data[,c("interested","excited","strong","enthusiastic","proud","alert","inspired","determined","attentive","active")],na.rm = TRUE)
data$Neg <- rowMeans(data[,c("distressed","upset","guilty","scared","hostile","irritable","ashamed","nervous","jittery","afraid")],na.rm = TRUE)

# --- create year and covid condition
data$year[data$year_month==200805|data$year_month==200808 ] <- 2008
data$year[data$year_month==200905|data$year_month==200906 ] <- 2009
data$year[data$year_month==201005|data$year_month==201006 ] <- 2010
data$year[data$year_month==201105|data$year_month==201106 ] <- 2011
data$year[data$year_month==201205|data$year_month==201206 ] <- 2012
data$year[data$year_month==201305|data$year_month==201306 ] <- 2013
data$year[data$year_month==201411|data$year_month==201412 ] <- 2014
data$year[data$year_month==201511|data$year_month==201512] <- 2015
data$year[data$year_month==201705|data$year_month==201706] <- 2017
data$year[data$year_month==201805|data$year_month==201806] <- 2018
data$year[data$year_month==201905|data$year_month==201906] <- 2019
data$year[data$year_month==202005|data$year_month==202006] <- 2020

data$covid[data$year<2020] <- 0
data$covid[data$year==2020] <- 1

# ================
# NOTE:A few years there's not enough PANAS data: 2010,2012,2015,2018
# remove these four years from the data in data to use
data1 <- data[which(data$year == 2008|data$year == 2009|data$year == 2011|data$year == 2013|data$year == 2014|data$year == 2017|data$year == 2019|data$year == 2020),]

# choose those who answered 8 times
n_occur <- data.frame(table(data1$id))
n_occur8 <- n_occur[which(n_occur$Freq==8),]
n_occur8$Var1 <- as.character(n_occur8$Var1)
data1$id <- as.character(data1$id)
df <- setDT(data1)[id %chin% n_occur8$Var1] #13448

# ==========
# there're some participants who didn't have panas data, we remove those
# ==========
df <- df[complete.cases(df)] #13393
n_occur <- data.frame(table(df$id))
n_occur8 <- n_occur[which(n_occur$Freq==8),]
n_occur8$Var1 <- as.character(n_occur8$Var1)
df$id <- as.character(df$id)
df <- setDT(df)[id %chin% n_occur8$Var1] #13032

# 13032/8=1629
# 1629 ppts took part in the study in these 8 years with PANAS data

# ==========================================================
# ==========================================================
# -------get demographic information of the participants----
# ==========================================================
# ==========================================================

# 1629 participants had PANAS in 8 years

# -------------------
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

# ----getting demographics
y2008 <- data[which(data$year==2008),] #1629 ppts
table(y2008$gender,useNA = "always") #816 male, 813 female
table(y2008$age,useNA = "always") 
# initial age: 16-85, mean = 50.79 (sd = 13.19)


# ==========================================================
# ==========================================================
# ----------------get personality data---------------
# ==========================================================
# ==========================================================

liss08_2 <- liss08[,c(1,3,23:72)]
liss09_2 <- liss09[,c(1,3,23:72)]
liss10_2 <- liss10[,c(1,3,24:73)]
liss11_2 <- liss11[,c(1,3,24:73)]
liss12_2 <- liss12[,c(1,2,23:72)]
liss13_2 <- liss13[,c(1,2,25:74)]
liss14_2 <- liss14[,c(1,2,22:71)]
liss15_2 <- liss15[,c(1,2,23:72)]
liss17_2 <- liss17[,c(1,2,22:71)]
liss18_2 <- liss18[,c(1,2,23:72)]
liss19_2 <- liss19[,c(1,2,13:62)]
liss20_2 <- liss20[,c(1,2,13:62)]

colname_swl_ipip <- c("id","year_month",paste0("IPIP",1:50))

ll2 <- list(liss08_2,liss09_2,liss10_2,liss11_2,liss12_2,liss13_2,liss14_2,liss15_2,liss17_2,liss18_2,liss19_2,liss20_2)

#------------ change all variables in all dataframes to character
temp <- lapply(ll2,function(x) lapply(x, as.character))
names(temp) <- c("liss08_2","liss09_2","liss10_2","liss11_2","liss12_2","liss13_2","liss14_2","liss15_2","liss17_2","liss18_2","liss19_2","liss20_2")
list2env(temp, envir = .GlobalEnv)
names(liss20_2)

# -------------------rename variables in all dataframes
ChangeNames <- function(x) {
  names(x) <- colname_swl_ipip
  return(x)
}
ll2 <- lapply(ll2, ChangeNames)

# ----------------------create one dataframe from the list
data2 <- bind_rows(ll2, .id = "column_label")

# recode the character answers to number
require(dplyr)
data3 <- data2 %>% 
                  mutate_at(vars(IPIP1:IPIP50), funs(recode(.,'very inaccurate'= 1, 
                                                            'moderately inaccurate'= 2, 
                                                            "neither inaccurate nor accurate"= 3,
                                                            "moderately accurate"= 4,
                                                            "very accurate" = 5)))

# 1 very inaccurate
# 2 moderately inaccurate
# 3 neither inaccurate nor accurate
# 4 moderately accurate
# 5 very accurate

# ---------------------------------------
# ------------code BIG 5-----------------
# ---------------------------------------

# ipip50 coding
# https://ipip.ori.org/new_ipip-50-item-scale.htm
# (1) Extraversion, (2) Agreeableness, (3) Conscientiousness, (4) Emotional Stability, or (5) Intellect/Imagination) and its direction of scoring (+ or -).

data3$IPIP2_r <- 6-data3$IPIP2
data3$IPIP4_r <- 6-data3$IPIP4
data3$IPIP6_r <- 6-data3$IPIP6
data3$IPIP8_r <- 6-data3$IPIP8
data3$IPIP10_r <- 6-data3$IPIP10
data3$IPIP12_r <- 6-data3$IPIP12
data3$IPIP14_r <- 6-data3$IPIP14
data3$IPIP16_r <- 6-data3$IPIP16
data3$IPIP18_r <- 6-data3$IPIP18
data3$IPIP20_r <- 6-data3$IPIP20
data3$IPIP22_r <- 6-data3$IPIP22
data3$IPIP24_r <- 6-data3$IPIP24
data3$IPIP26_r <- 6-data3$IPIP26
data3$IPIP28_r <- 6-data3$IPIP28
data3$IPIP29_r <- 6-data3$IPIP29
data3$IPIP30_r <- 6-data3$IPIP30
data3$IPIP32_r <- 6-data3$IPIP32
data3$IPIP34_r <- 6-data3$IPIP34
data3$IPIP36_r <- 6-data3$IPIP36
data3$IPIP38_r <- 6-data3$IPIP38
data3$IPIP39_r <- 6-data3$IPIP39
data3$IPIP44_r <- 6-data3$IPIP44
data3$IPIP46_r <- 6-data3$IPIP46
data3$IPIP49_r <- 6-data3$IPIP49

# Extraversion items: 1,6,11,16,21,26,31,36,41,46
# Agreeableness items: 2,7,12,17,22,27,32,37,42,47
# Conscientiousness items: 3,8,13,18,23,28,33,38,43,48
# EmoStability items: 4,9,14,19,24,29,34,39,44,49
# Intellect/Imagination items: 5,10,15,20,25,30,35,40,45,50

data3$Extraversion <- rowMeans(data3[,c("IPIP1","IPIP6_r","IPIP11","IPIP16_r","IPIP21","IPIP26_r","IPIP31","IPIP36_r","IPIP41","IPIP46_r")])
data3$Agreeable <- rowMeans(data3[,c("IPIP2_r","IPIP7","IPIP12_r","IPIP17","IPIP22_r","IPIP27","IPIP32_r","IPIP37","IPIP42","IPIP47")])
data3$Conscientiousness <- rowMeans(data3[,c("IPIP3","IPIP8_r","IPIP13","IPIP18_r","IPIP23","IPIP28_r","IPIP33","IPIP38_r","IPIP43","IPIP48")])
data3$EmoStability <- rowMeans(data3[,c("IPIP4_r","IPIP9","IPIP14_r","IPIP19","IPIP24_r","IPIP29_r","IPIP34_r","IPIP39_r","IPIP44_r","IPIP49_r")])
data3$Openness <- rowMeans(data3[,c("IPIP5","IPIP10_r","IPIP15","IPIP20_r","IPIP25","IPIP30_r","IPIP35","IPIP40","IPIP45","IPIP50")])

data3$year[data3$year_month==200805|data3$year_month==200808 ] <- 2008
data3$year[data3$year_month==200905|data3$year_month==200906 ] <- 2009
data3$year[data3$year_month==201005|data3$year_month==201006 ] <- 2010
data3$year[data3$year_month==201105|data3$year_month==201106 ] <- 2011
data3$year[data3$year_month==201205|data3$year_month==201206 ] <- 2012
data3$year[data3$year_month==201305|data3$year_month==201306 ] <- 2013
data3$year[data3$year_month==201411|data3$year_month==201412 ] <- 2014
data3$year[data3$year_month==201511|data3$year_month==201512] <- 2015
data3$year[data3$year_month==201705|data3$year_month==201706] <- 2017
data3$year[data3$year_month==201805|data3$year_month==201806] <- 2018
data3$year[data3$year_month==201905|data3$year_month==201906] <- 2019
data3$year[data3$year_month==202005|data3$year_month==202006] <- 2020

data3$covid[data3$year<2020] <- 0
data3$covid[data3$year==2020] <- 1

data_ipip <- data3[,c(1,2,78:84)]

# ======= merging PANAS and personality======

data_all <- merge.data.frame(data,data_ipip,by=c( "id", "column_label","covid","year"))
data_all$column_label <- NULL
# data_all is the file that has age, gender, year,covid,big five personality, PANAS

data_all$covid <- as.factor(data_all$covid)
data_all$gender <- as.factor(data_all$gender)


# ==========================================================
# ==========================================================
# ----------------get subjective health data---------------
# ================LISS Health panel========================
# ==========================================================

h_liss08 <- read.spss("ch08b_EN_1.3p.sav",to.data.frame=TRUE)
h_liss09 <- read.spss("ch09c_EN_1.1p.sav",to.data.frame=TRUE)
h_liss10 <- read.spss("ch10d_EN_1.0p.sav",to.data.frame=TRUE)
h_liss11 <- read.spss("ch11e_EN_1.0p.sav",to.data.frame=TRUE)
h_liss12 <- read.spss("ch12f_EN_1.0p.sav",to.data.frame=TRUE)
h_liss13 <- read.spss("ch13g_EN_1.0p.sav",to.data.frame=TRUE)
h_liss15 <- read.spss("ch15h_EN_1.2p.sav",to.data.frame=TRUE)
h_liss16 <- read.spss("ch16i_EN_1.0p.sav",to.data.frame=TRUE)
h_liss17 <- read.spss("ch17j_EN_1.0p.sav",to.data.frame=TRUE)
h_liss18 <- read.spss("ch18k_EN_1.0p.sav",to.data.frame=TRUE)
h_liss19 <- read.spss("ch19l_EN_1.0p.sav",to.data.frame=TRUE)
h_liss20 <- read.spss("ch20m_EN_1.0p.sav",to.data.frame=TRUE)

h_liss08_1 <- h_liss08[,c(1,3,7)]
h_liss09_1 <- h_liss09[,c(1,3,7)]
h_liss10_1 <- h_liss10[,c(1,3,7)]
h_liss11_1 <- h_liss11[,c(1,3,7)]
h_liss12_1 <- h_liss12[,c(1,2,6)]
h_liss13_1 <- h_liss13[,c(1,2,6)]
h_liss15_1 <- h_liss15[,c(1,2,6)]
h_liss16_1 <- h_liss16[,c(1,2,6)]
h_liss17_1 <- h_liss17[,c(1,2,6)]
h_liss18_1 <- h_liss18[,c(1,2,6)]
h_liss19_1 <- h_liss19[,c(1,2,6)]
h_liss20_1 <- h_liss20[,c(1,2,6)]

ll_health <- list(h_liss08_1,h_liss09_1,h_liss10_1,h_liss11_1,h_liss12_1,h_liss13_1,h_liss15_1,h_liss16_1,h_liss17_1,h_liss18_1,h_liss19_1,h_liss20_1)

#------------ change all variables in all dataframes to character
temp <- lapply(ll_health,function(x) lapply(x, as.character))
names(temp) <- c("h_liss08_1","h_liss09_1","h_liss10_1","h_liss11_1","h_liss12_1","h_liss13_1","h_liss15_1","h_liss16_1","h_liss17_1","h_liss18_1","h_liss19_1","h_liss20_1")
list2env(temp, envir = .GlobalEnv)

# -------------------rename variables in all dataframes
ChangeNames <- function(x) {
  names(x) <-  c("nomem_encr","year_month","sub_health")
  return(x)
}
ll_health <- lapply(ll_health, ChangeNames)

# ----------------------create one dataframe from the list
data_health <- bind_rows(ll_health, .id = "column_label")

data_health$year[data_health$year_month==200811|data_health$year_month==200812 ] <- 2008
data_health$year[data_health$year_month==200911|data_health$year_month==200912 ] <- 2009
data_health$year[data_health$year_month==201011|data_health$year_month==201012 ] <- 2010
data_health$year[data_health$year_month==201111|data_health$year_month==201112 ] <- 2011
data_health$year[data_health$year_month==201211|data_health$year_month==201212 ] <- 2012
data_health$year[data_health$year_month==201311|data_health$year_month==201312 ] <- 2013
data_health$year[data_health$year_month==201507|data_health$year_month==201508 ] <- 2015
data_health$year[data_health$year_month==201611|data_health$year_month==201612] <- 2016
data_health$year[data_health$year_month==201711|data_health$year_month==201712] <- 2017
data_health$year[data_health$year_month==201811|data_health$year_month==201812] <- 2018
data_health$year[data_health$year_month==201911|data_health$year_month==201912] <- 2019
data_health$year[data_health$year_month==202011|data_health$year_month==202012] <- 2020

data_health$column_label <- NULL


# need to recode #sub_health, but there's white space; remove those
data_health$sub_health <- as.character(data_health$sub_health)

data_health <- data_health %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))


data_health1 <- data_health %>% 
  mutate(sub_health=recode(sub_health, 
                                    'poor'= 1,
                                    'moderate'= 2,
                                    "good"= 3,
                                    "verygood"= 4,
                                    "excellent" = 5))
names(data_health1)[1] <- "id"
data_health2 <- data_health1[,c(1,3,4)]

# ==========================================================================
# merge health data with data_all (panas and personality and background data)
# ==========================================================================

lissdata <- merge.data.frame(data_all,data_health2,by=c("id","year"),all.x = TRUE)
lissdata$year_month <- NULL
write.csv(lissdata,"Study2_liss.csv",row.names = FALSE)
