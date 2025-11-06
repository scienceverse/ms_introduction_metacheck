#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Read libraries and set options
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read libraries
library(dplyr)
library(tidyr)
library (readxl)
library(psych)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Read in and clean data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data_screen<- read_excel("Self-verification_Screen_January_19.xlsx", col_names = TRUE, sheet="Arkusz2") %>%
  filter(Progress==100) # Selects only those who completed the survey s 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Recoding variables
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_screen$TSBI_1R <- recode(data_screen$TSBI_1, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L,`5`= 0L)#reverse scored
data_screen$TSBI_2R <- recode(data_screen$TSBI_2, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_3R <- recode(data_screen$TSBI_3, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_4R <- recode(data_screen$TSBI_4, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_5R <- recode(data_screen$TSBI_5, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L,`5`= 0L)#reverse scored
data_screen$TSBI_6R <- recode(data_screen$TSBI_6, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L,`5`= 0L)#reverse scored
data_screen$TSBI_7R <- recode(data_screen$TSBI_7, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_8R <- recode(data_screen$TSBI_8, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_9R <- recode(data_screen$TSBI_9, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_10R <- recode(data_screen$TSBI_10, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_11R <- recode(data_screen$TSBI_11, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_12R <- recode(data_screen$TSBI_12, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L,`5`= 0L)#reverse scored
data_screen$TSBI_13R <- recode(data_screen$TSBI_13, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L,`5`= 0L)#reverse scored
data_screen$TSBI_14R <- recode(data_screen$TSBI_14, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)
data_screen$TSBI_15R <- recode(data_screen$TSBI_15, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L,`5`= 0L)#reverse scored
data_screen$TSBI_16R <- recode(data_screen$TSBI_16, `1`=0L, `2`= 1L, `3`= 2L, `4`= 3L,`5`= 4L)

data_screen$SES_3R <- recode(data_screen$SES_3, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L)#reverse scored
data_screen$SES_5R <- recode(data_screen$SES_5, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L)#reverse scored
data_screen$SES_8R <- recode(data_screen$SES_8, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L)#reverse scored
data_screen$SES_9R <- recode(data_screen$SES_9, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L)#reverse scored
data_screen$SES_10R <- recode(data_screen$SES_10, `1`=4L, `2`= 3L, `3`= 2L, `4`= 1L)#reverse scored

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregating data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
TSBI<-data_screen %>%
  select(TSBI_1R:TSBI_16R)
SES<-data_screen %>%
  select(SES_1,SES_2,SES_3R,SES_4,SES_5R,SES_6,SES_7,SES_8R,SES_9R,SES_10R)

data_screen <- data_screen %>%
  mutate(TSBI=rowMeans(TSBI,na.rm=TRUE),
         SES=rowMeans(SES,na.rm=TRUE))

# checking reliabilities
psych::alpha(TSBI)
psych::alpha(SES)

library(Hmisc)
correlation<-rcorr(as.matrix(select(data_screen,TSBI,SES)),type="pearson")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Binning data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(OneR)

TSBI_binned <- bin(data_screen$TSBI,nbins=4,method="content", labels=c("low","medium low","medium high","high"))
data_screen<-cbind(data_screen,TSBI_binned)

data_screen_ordered<- data_screen %>%
  arrange(TSBI)

SES_binned<- bin(data_screen_ordered$SES,nbins=4,labels=c("low","medium low","medium high","high"),
                 method="content")
TSBIvsSES<-sum(data_screen_ordered$TSBI_binned==SES_binned)

data_screen_ordered<- cbind(data_screen_ordered,SES_binned)
write.csv(data_screen_ordered, file="Study 1_Screen_January.csv")
