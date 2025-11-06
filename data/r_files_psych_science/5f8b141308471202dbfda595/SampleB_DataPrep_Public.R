##############PREPARE R ENVIRONMENT##############
rm(list=ls()) #clear environment
my_packages <- c("nlme","psych")
lapply(my_packages, library, character.only = TRUE) #load packages
setwd("")
data<-read.csv("sampleb_raw.csv") #read in data

##############CALCULATE STUDY VARIABLES##############
T2_Reap_Success<-transform(cbind(data$T2_ER_Reappraise_Success_1,
                                 data$T2_ER_Reappraise_Success_2,
                                 data$T2_ER_Reappraise_Success_3,
                                 data$T2_ER_Reappraise_Success_4),
                           MEAN=apply(cbind(data$T2_ER_Reappraise_Success_1,
                                            data$T2_ER_Reappraise_Success_2,
                                            data$T2_ER_Reappraise_Success_3,
                                            data$T2_ER_Reappraise_Success_4),1, mean, na.rm = TRUE))
data$T2_Reap_Success<-T2_Reap_Success$MEAN

T2_Reap_Try<-transform(cbind(data$T2_ER_Reappraise_Try_1,
                                 data$T2_ER_Reappraise_Try_2,
                                 data$T2_ER_Reappraise_Try_3,
                                 data$T2_ER_Reappraise_Try_4),
                           MEAN=apply(cbind(data$T2_ER_Reappraise_Try_1,
                                            data$T2_ER_Reappraise_Try_2,
                                            data$T2_ER_Reappraise_Try_3,
                                            data$T2_ER_Reappraise_Try_4),1, mean, na.rm = TRUE))
data$T2_Reap_Try<-T2_Reap_Try$MEAN

gpos_t2a<-transform(cbind(data$T2a_Emo_Resp_Amused,
                             data$T2a_Emo_Resp_Glad),
                       MEAN=apply(cbind(data$T2a_Emo_Resp_Amused,
                                        data$T2a_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t2a<-gpos_t2a$MEAN

spos_t2a<-transform(cbind(data$T2a_Emo_Resp_Grateful,
                                data$T2a_Emo_Resp_Inspired,
                                data$T2a_Emo_Resp_Love),
                          MEAN=apply(cbind(data$T2a_Emo_Resp_Grateful,
                                           data$T2a_Emo_Resp_Inspired,
                                           data$T2a_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t2a<-spos_t2a$MEAN

fear_t2a<-transform(cbind(data$T2a_Emo_Resp_Worry,
                          data$T2a_Emo_Resp_Panicked),
                    MEAN=apply(cbind(data$T2a_Emo_Resp_Worry,
                                     data$T2a_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t2a<-fear_t2a$MEAN

sneg_t2a<-transform(cbind(data$T2a_Emo_Resp_Guilty,
                                data$T2a_Emo_Resp_Ashamed),
                          MEAN=apply(cbind(data$T2a_Emo_Resp_Guilty,
                                           data$T2a_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t2a<-sneg_t2a$MEAN

gpos_t2b<-transform(cbind(data$T2b_Emo_Resp_Amused,
                             data$T2b_Emo_Resp_Glad),
                       MEAN=apply(cbind(data$T2b_Emo_Resp_Amused,
                                        data$T2b_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t2b<-gpos_t2b$MEAN

spos_t2b<-transform(cbind(data$T2b_Emo_Resp_Grateful,
                                data$T2b_Emo_Resp_Inspired,
                                data$T2b_Emo_Resp_Love),
                          MEAN=apply(cbind(data$T2b_Emo_Resp_Grateful,
                                           data$T2b_Emo_Resp_Inspired,
                                           data$T2b_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t2b<-spos_t2b$MEAN

fear_t2b<-transform(cbind(data$T2b_Emo_Resp_Worry,
                          data$T2b_Emo_Resp_Panicked),
                    MEAN=apply(cbind(data$T2b_Emo_Resp_Worry,
                                     data$T2b_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t2b<-fear_t2b$MEAN

sneg_t2b<-transform(cbind(data$T2b_Emo_Resp_Guilty,
                                data$T2b_Emo_Resp_Ashamed),
                          MEAN=apply(cbind(data$T2b_Emo_Resp_Guilty,
                                           data$T2b_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t2b<-sneg_t2b$MEAN

gpos_t2c<-transform(cbind(data$T2c_Emo_Resp_Amused,
                             data$T2c_Emo_Resp_Glad),
                       MEAN=apply(cbind(data$T2c_Emo_Resp_Amused,
                                        data$T2c_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t2c<-gpos_t2c$MEAN

spos_t2c<-transform(cbind(data$T2c_Emo_Resp_Grateful,
                                data$T2c_Emo_Resp_Inspired,
                                data$T2c_Emo_Resp_Love),
                          MEAN=apply(cbind(data$T2c_Emo_Resp_Grateful,
                                           data$T2c_Emo_Resp_Inspired,
                                           data$T2c_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t2c<-spos_t2c$MEAN

fear_t2c<-transform(cbind(data$T2c_Emo_Resp_Worry,
                          data$T2c_Emo_Resp_Panicked),
                    MEAN=apply(cbind(data$T2c_Emo_Resp_Worry,
                                     data$T2c_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t2c<-fear_t2c$MEAN

sneg_t2c<-transform(cbind(data$T2c_Emo_Resp_Guilty,
                                data$T2c_Emo_Resp_Ashamed),
                          MEAN=apply(cbind(data$T2c_Emo_Resp_Guilty,
                                           data$T2c_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t2c<-sneg_t2c$MEAN

gpos_t3<-transform(cbind(data$T3_Emo_Resp_Amused,
                            data$T3_Emo_Resp_Glad),
                      MEAN=apply(cbind(data$T3_Emo_Resp_Amused,
                                       data$T3_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t3<-gpos_t3$MEAN

spos_t3<-transform(cbind(data$T3_Emo_Resp_Grateful,
                               data$T3_Emo_Resp_Inspired,
                               data$T3_Emo_Resp_Love),
                         MEAN=apply(cbind(data$T3_Emo_Resp_Grateful,
                                          data$T3_Emo_Resp_Inspired,
                                          data$T3_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t3<-spos_t3$MEAN

fear_t3<-transform(cbind(data$T3_Emo_Resp_Worry,
                         data$T3_Emo_Resp_Panicked),
                   MEAN=apply(cbind(data$T3_Emo_Resp_Worry,
                                    data$T3_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t3<-fear_t3$MEAN

sneg_t3<-transform(cbind(data$T3_Emo_Resp_Guilty,
                               data$T3_Emo_Resp_Ashamed),
                         MEAN=apply(cbind(data$T3_Emo_Resp_Guilty,
                                          data$T3_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t3<-sneg_t3$MEAN

T4_Past_Behaviors<-transform(cbind(data$T4_ActPast_Prevent_Self,
                                   data$T4_ActPast_Prevent_Mask,
                                   data$T4_ActPast_Prevent_Sanitize,
                                   data$T4_ActPast_SocialDistance,
                                   data$T4_ActPast_SelfIsolate),
                             MEAN=apply(cbind(data$T4_ActPast_Prevent_Self,
                                              data$T4_ActPast_Prevent_Mask,
                                              data$T4_ActPast_Prevent_Sanitize,
                                              data$T4_ActPast_SocialDistance,
                                              data$T4_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
data$T4_Past_Behaviors<-T4_Past_Behaviors$MEAN

T4_PWB_Mean<-transform(cbind(data$T4_PWB_Meaningful,
                             data$T4_PWB_ClearPurpose,
                             (6-data$T4_PWB_NoPurpose)),
                       MEAN=apply(cbind(data$T4_PWB_Meaningful,
                                        data$T4_PWB_ClearPurpose,
                                        (6-data$T4_PWB_NoPurpose)),1,mean,na.rm=TRUE))
data$T4_PWB_Mean<-T4_PWB_Mean$MEAN

T4_SWL_Mean<-transform(cbind(data$T4_SWL_1,
                             data$T4_SWL_2,
                             data$T4_SWL_3,
                             data$T4_SWL_4,
                             data$T4_SWL_5),
                       MEAN=apply(cbind(data$T4_SWL_1,
                                        data$T4_SWL_2,
                                        data$T4_SWL_3,
                                        data$T4_SWL_4,
                                        data$T4_SWL_5),1,mean,na.rm=TRUE))
data$T4_SWL_Mean<-T4_SWL_Mean$MEAN

T4_Depression_Sum<-transform(cbind(data$T4_CES_D_1,
                                   data$T4_CES_D_2,
                                   data$T4_CES_D_3,
                                   data$T4_CES_D_4,
                                   (3-data$T4_CES_D_5),
                                   data$T4_CES_D_6,
                                   data$T4_CES_D_7,
                                   (3-data$T4_CES_D_8),
                                   data$T4_CES_D_9,
                                   data$T4_CES_D_10),
                             MEAN=apply(cbind(data$T4_CES_D_1,
                                             data$T4_CES_D_2,
                                             data$T4_CES_D_3,
                                             data$T4_CES_D_4,
                                             (3-data$T4_CES_D_5),
                                             data$T4_CES_D_6,
                                             data$T4_CES_D_7,
                                             (3-data$T4_CES_D_8),
                                             data$T4_CES_D_9,
                                             data$T4_CES_D_10),1,mean,na.rm=TRUE))
data$T4_Depression_Sum<-T4_Depression_Sum$MEAN*10

T4_Anxiety_Sum<-transform(cbind(data$T4_HADS_1,
                                data$T4_HADS_2,
                                data$T4_HADS_3,
                                data$T4_HADS_4,
                                data$T4_HADS_5,
                                data$T4_HADS_6,
                                data$T4_HADS_7),
                          MEAN=apply(cbind(data$T4_HADS_1,
                                           data$T4_HADS_2,
                                           data$T4_HADS_3,
                                           data$T4_HADS_4,
                                           data$T4_HADS_5,
                                           data$T4_HADS_6,
                                           data$T4_HADS_7),1,mean,na.rm=TRUE))
data$T4_Anxiety_Sum<-T4_Anxiety_Sum$MEAN*7

T4_MentalHealth<-transform(cbind(scale(data$T4_Anxiety_Sum),
                             scale(data$T4_Depression_Sum)),
                       MEAN=apply(cbind(scale(data$T4_Anxiety_Sum),
                                        scale(data$T4_Depression_Sum)),1, mean, na.rm = TRUE))
data$T4_MentalHealth<-T4_MentalHealth$MEAN

T4_Wellbeing<-transform(cbind(scale(data$T4_SWL_Mean),
                              scale(data$T4_PWB_Mean)),
                        MEAN=apply(cbind(scale(data$T4_SWL_Mean),
                                         scale(data$T4_PWB_Mean)),1, mean, na.rm = TRUE))
data$T4_Wellbeing<-T4_Wellbeing$MEAN

T3_Future_Behaviors<-transform(cbind(data$T3_ActPresent_Prevent_Self,
                                     data$T3_ActPresent_Prevent_Mask,
                                     data$T3_ActPresent_Prevent_Sanitize,
                                     data$T3_ActPresent_SocialDistance,
                                     data$T3_ActPresent_SelfIsolate),
                               MEAN=apply(cbind(data$T3_ActPresent_Prevent_Self,
                                                data$T3_ActPresent_Prevent_Mask,
                                                data$T3_ActPresent_Prevent_Sanitize,
                                                data$T3_ActPresent_SocialDistance,
                                                data$T3_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
data$T3_Future_Behaviors<-T3_Future_Behaviors$MEAN

##############RESTRUCTURE DATA##############

#restructure data for sequential mediation models
data_wide<-cbind.data.frame(data$ID, #pare down dataset 
                            data$T2_Reap_Success,
                            data$gpos_t2a,
                            data$spos_t2a,
                            data$fear_t2a,
                            data$sneg_t2a,
                            data$gpos_t2b,
                            data$spos_t2b,
                            data$fear_t2b,
                            data$sneg_t2b,
                            data$gpos_t2c,
                            data$spos_t2c,
                            data$fear_t2c,
                            data$sneg_t2c,
                            data$gpos_t3,
                            data$spos_t3,
                            data$fear_t3,
                            data$sneg_t3,
                            data$T3_Future_Behaviors,
                            data$T4_Past_Behaviors,
                            data$T4_MentalHealth,
                            data$T4_Wellbeing)

names(data_wide)<-c("ID", #name variables
                    "T2_Reap_Success",
                    "gpos1",
                    "spos1",
                    "fear1",
                    "sneg1",
                    "gpos2",
                    "spos2",
                    "fear2",
                    "sneg2",
                    "gpos3",
                    "spos3",
                    "fear3",
                    "sneg3",
                    "gpos4",
                    "spos4",
                    "fear4",
                    "sneg4",
                    "T3_Future_Behaviors",
                    "T4_Past_Behaviors",
                    "T4_MentalHealth",
                    "T4_Wellbeing")

long <- reshape(data_wide,varying = c("gpos1",  #shape in long format 
                                      "gpos2",
                                      "gpos3",
                                      "gpos4",
                                      "spos1", 
                                      "spos2",
                                      "spos3",
                                      "spos4",
                                      "sneg1", 
                                      "sneg2", 
                                      "sneg3", 
                                      "sneg4", 
                                      "fear1", 
                                      "fear2", 
                                      "fear3",
                                      "fear4"),
                direction = 'long', idvar = 'ID', sep = '')

#calculate association between fear and social positive emotions before introducing -99s
summary(lme(scale(spos) ~ 1 + scale(fear), random =~1|ID, data=long, na.action="na.exclude"))

#replace NAs with -99 for mplus
long[is.na(long)] <- -99

#NOTE: export csv files for MPlus, you will need to open the resulting files and delete the first row (the variable names)
write.csv(long,"sampleb_sequential.csv",row.names=FALSE)

##############CALCULATE RELIABILITY STATISTICS##############

psych::alpha(cbind.data.frame(data$T2_ER_Reappraise_Success_1,
                              data$T2_ER_Reappraise_Success_2,
                              data$T2_ER_Reappraise_Success_3,
                              data$T2_ER_Reappraise_Success_4)) #.92

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Grateful,
                              data$T2a_Emo_Resp_Inspired,
                              data$T2a_Emo_Resp_Love)) #.86

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Grateful,
                              data$T2b_Emo_Resp_Inspired,
                              data$T2b_Emo_Resp_Love)) #.84

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Grateful,
                              data$T2c_Emo_Resp_Inspired,
                              data$T2c_Emo_Resp_Love)) #.85

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Grateful,
                              data$T3_Emo_Resp_Inspired,
                              data$T3_Emo_Resp_Love)) #.86

#note, a warning message will be shown because some statistis in the alpha output do not make sense for two-item measures, the standarrdized alpha can still be interpreted

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Worry,
                              data$T2a_Emo_Resp_Panicked)) #.82

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Worry,
                              data$T2b_Emo_Resp_Panicked)) #.83

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Worry,
                              data$T2c_Emo_Resp_Panicked)) #.82

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Worry,
                              data$T3_Emo_Resp_Panicked)) #.86

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Amused,
                              data$T2a_Emo_Resp_Glad)) #.78

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Amused,
                              data$T2b_Emo_Resp_Glad)) #.76

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Amused,
                              data$T2c_Emo_Resp_Glad)) #.80

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Amused,
                              data$T3_Emo_Resp_Glad)) #.80

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Guilty,
                              data$T2a_Emo_Resp_Ashamed)) #.78

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Guilty,
                              data$T2b_Emo_Resp_Ashamed)) #.72

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Guilty,
                              data$T2c_Emo_Resp_Ashamed)) #.75

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Guilty,
                              data$T3_Emo_Resp_Ashamed)) #.78

psych::alpha(cbind.data.frame(data$T4_ActPast_Prevent_Self,
                              data$T4_ActPast_Prevent_Mask,
                              data$T4_ActPast_Prevent_Sanitize,
                              data$T4_ActPast_SocialDistance,
                              data$T4_ActPast_SelfIsolate)) #.80

psych::alpha(cbind.data.frame(data$T3_ActPresent_Prevent_Self,
                              data$T3_ActPresent_Prevent_Mask,
                              data$T3_ActPresent_Prevent_Sanitize,
                              data$T3_ActPresent_SocialDistance,
                              data$T3_ActPresent_SelfIsolate)) #.80

#note, a warning message will be shown because some statistis in the alpha output do not make sense for two-item measures, the standarrdized alpha can still be interpreted

psych::alpha(cbind.data.frame(scale(data$T4_Anxiety_Sum),
                              scale(data$T4_Depression_Sum))) #.91

psych::alpha(cbind.data.frame(scale(data$T4_SWL_Mean),
                              scale(data$T4_PWB_Mean))) #.82


##############EXCLUDE PARTICIPANTS WHO WERE NOT INCLUDED IN ANY ANALYSES BEFORE CAlCULATING DESCRIPTIVE STATISTICS##############
data_pared<-subset(long,long$T2_Reap_Success!=-99|long$T4_MentalHealth!=-99|long$T4_Past_Behaviors!=-99|long$spos!=-99)
nrow(data_pared)
sequential_ps<-unique(data_pared$ID)
length(sequential_ps)
data_pared<-subset(data,data$ID%in%sequential_ps)

##############CALCULATE DESCRIPTIVE STATISTICS##############

#Demographics
summary(data_pared$T1_Age)
sd(data_pared$T1_Age,na.rm=T)
summary(as.factor(data_pared$T1_Gender))/nrow(data_pared)
summary(as.factor(data_pared$T1_Ethnicity))/nrow(data_pared)
summary(as.factor(data_pared$T1_PolParty))/nrow(data_pared)

#Reappraisal Descriptives for Results Section
mean(data_pared$T2_Reap_Try,na.rm=T)
sd(data_pared$T2_Reap_Try,na.rm=T)
sum(data_pared$T2_Reap_Try>0,na.rm=T)/sum(!is.na(data_pared$T2_Reap_Try))
mean(data_pared$T2_Reap_Success,na.rm=T)
sd(data_pared$T2_Reap_Success,na.rm=T)

#Emotions Descriptives for Results Section
sum(data_pared$fear_t2a>0,na.rm=T)/sum(!is.na(data_pared$fear_t2a))
sum(data_pared$fear_t2b>0,na.rm=T)/sum(!is.na(data_pared$fear_t2b))
sum(data_pared$fear_t2c>0,na.rm=T)/sum(!is.na(data_pared$fear_t2c))
sum(data_pared$fear_t3>0,na.rm=T)/sum(!is.na(data_pared$fear_t3))
#80%-90%

sum(data_pared$spos_t2a>0,na.rm=T)/sum(!is.na(data_pared$spos_t2a))
sum(data_pared$spos_t2b>0,na.rm=T)/sum(!is.na(data_pared$spos_t2b))
sum(data_pared$spos_t2c>0,na.rm=T)/sum(!is.na(data_pared$spos_t2c))
sum(data_pared$spos_t3>0,na.rm=T)/sum(!is.na(data_pared$spos_t3))
#94%-97%


#Supplementary Table 1

mean(data_pared$T2_Reap_Success, na.rm=T)
sd(data_pared$T2_Reap_Success, na.rm=T)

#

mean(data_pared$fear_t2a, na.rm=T)
sd(data_pared$fear_t2a, na.rm=T)

mean(data_pared$fear_t2b, na.rm=T)
sd(data_pared$fear_t2b, na.rm=T)

mean(data_pared$fear_t2c, na.rm=T)
sd(data_pared$fear_t2c, na.rm=T)

mean(data_pared$fear_t3, na.rm=T)
sd(data_pared$fear_t3, na.rm=T)

#

mean(data_pared$spos_t2a, na.rm=T)
sd(data_pared$spos_t2a, na.rm=T)

mean(data_pared$spos_t2b, na.rm=T)
sd(data_pared$spos_t2b, na.rm=T)

mean(data_pared$spos_t2c, na.rm=T)
sd(data_pared$spos_t2c, na.rm=T)

mean(data_pared$spos_t3, na.rm=T)
sd(data_pared$spos_t3, na.rm=T)

#

mean(data_pared$sneg_t2a, na.rm=T)
sd(data_pared$sneg_t2a, na.rm=T)

mean(data_pared$sneg_t2b, na.rm=T)
sd(data_pared$sneg_t2b, na.rm=T)

mean(data_pared$sneg_t2c, na.rm=T)
sd(data_pared$sneg_t2c, na.rm=T)

mean(data_pared$sneg_t3, na.rm=T)
sd(data_pared$sneg_t3, na.rm=T)

#

mean(data_pared$gpos_t2a, na.rm=T)
sd(data_pared$gpos_t2a, na.rm=T)

mean(data_pared$gpos_t2b, na.rm=T)
sd(data_pared$gpos_t2b, na.rm=T)

mean(data_pared$gpos_t2c, na.rm=T)
sd(data_pared$gpos_t2c, na.rm=T)

mean(data_pared$gpos_t3, na.rm=T)
sd(data_pared$gpos_t3, na.rm=T)

#

mean(data_pared$T4_Past_Behaviors, na.rm=T)
sd(data_pared$T4_Past_Behaviors, na.rm=T)

mean(data_pared$T3_Future_Behaviors, na.rm=T)
sd(data_pared$T3_Future_Behaviors, na.rm=T)

mean(data_pared$T4_MentalHealth, na.rm=T)
sd(data_pared$T4_MentalHealth, na.rm=T)

mean(data_pared$T4_Wellbeing, na.rm=T)
sd(data_pared$T4_Wellbeing, na.rm=T)


