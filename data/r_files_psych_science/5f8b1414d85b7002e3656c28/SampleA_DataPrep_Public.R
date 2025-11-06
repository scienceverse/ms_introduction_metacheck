##############PREPARE R ENVIRONMENT##############
rm(list=ls()) #clear environment
my_packages <- c("nlme","psych")
lapply(my_packages, library, character.only = TRUE) #load packages
setwd("")
data<-read.csv("samplea_raw.csv") #read in data

##############CALCULATE STUDY VARIABLES##############

T1_Reap_Success<-transform(cbind(data$T1_ER_Reappraise_Success_1,
                                  data$T1_ER_Reappraise_Success_2,
                                  data$T1_ER_Reappraise_Success_3,
                                  data$T1_ER_Reappraise_Success_4),
                            MEAN=apply(cbind(data$T1_ER_Reappraise_Success_1,
                                             data$T1_ER_Reappraise_Success_2,
                                             data$T1_ER_Reappraise_Success_3,
                                             data$T1_ER_Reappraise_Success_4),1, mean, na.rm = TRUE))
data$T1_Reap_Success<-T1_Reap_Success$MEAN

T1b_Reap_Success<-transform(cbind(data$T1b_ER_Reappraise_Success_1,
                                  data$T1b_ER_Reappraise_Success_2,
                                  data$T1b_ER_Reappraise_Success_3,
                                  data$T1b_ER_Reappraise_Success_4),
                            MEAN=apply(cbind(data$T1b_ER_Reappraise_Success_1,
                                             data$T1b_ER_Reappraise_Success_2,
                                             data$T1b_ER_Reappraise_Success_3,
                                             data$T1b_ER_Reappraise_Success_4),1, mean, na.rm = TRUE))
data$T1b_Reap_Success<-T1b_Reap_Success$MEAN

T1b_Reap_Try<-transform(cbind(data$T1b_ER_Reappraise_Try_1,
                                  data$T1b_ER_Reappraise_Try_2,
                                  data$T1b_ER_Reappraise_Try_3,
                                  data$T1b_ER_Reappraise_Try_4),
                            MEAN=apply(cbind(data$T1b_ER_Reappraise_Try_1,
                                             data$T1b_ER_Reappraise_Try_2,
                                             data$T1b_ER_Reappraise_Try_3,
                                             data$T1b_ER_Reappraise_Try_4),1, mean, na.rm = TRUE))
data$T1b_Reap_Try<-T1b_Reap_Try$MEAN

T2_Reap_Success<-transform(cbind(data$T2_ER_Reappraise_Success_1,
                                  data$T2_ER_Reappraise_Success_2,
                                  data$T2_ER_Reappraise_Success_3,
                                  data$T2_ER_Reappraise_Success_4),
                            MEAN=apply(cbind(data$T2_ER_Reappraise_Success_1,
                                             data$T2_ER_Reappraise_Success_2,
                                             data$T2_ER_Reappraise_Success_3,
                                             data$T2_ER_Reappraise_Success_4),1, mean, na.rm = TRUE))
data$T2_Reap_Success<-T2_Reap_Success$MEAN

T2b_Reap_Success<-transform(cbind(data$T2b_ER_Reappraise_Success_1,
                                  data$T2b_ER_Reappraise_Success_2,
                                  data$T2b_ER_Reappraise_Success_3,
                                  data$T2b_ER_Reappraise_Success_4),
                            MEAN=apply(cbind(data$T2b_ER_Reappraise_Success_1,
                                             data$T2b_ER_Reappraise_Success_2,
                                             data$T2b_ER_Reappraise_Success_3,
                                             data$T2b_ER_Reappraise_Success_4),1, mean, na.rm = TRUE))
data$T2b_Reap_Success<-T2b_Reap_Success$MEAN

T3_Reap_Success<-transform(cbind(data$T3_ER_Reappraise_Success_1,
                                 data$T3_ER_Reappraise_Success_2,
                                 data$T3_ER_Reappraise_Success_3,
                                 data$T3_ER_Reappraise_Success_4),
                           MEAN=apply(cbind(data$T3_ER_Reappraise_Success_1,
                                            data$T3_ER_Reappraise_Success_2,
                                            data$T3_ER_Reappraise_Success_3,
                                            data$T3_ER_Reappraise_Success_4),1, mean, na.rm = TRUE))
data$T3_Reap_Success<-T3_Reap_Success$MEAN

gpos_t1b<-transform(cbind(data$T1b_Emo_Resp_Amused,
                             data$T1b_Emo_Resp_Glad),
                       MEAN=apply(cbind(data$T1b_Emo_Resp_Amused,
                                        data$T1b_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t1b<-gpos_t1b$MEAN

spos_t1b<-transform(cbind(data$T1b_Emo_Resp_Grateful,
                              data$T1b_Emo_Resp_Inspired,
                              data$T1b_Emo_Resp_Love),
                        MEAN=apply(cbind(data$T1b_Emo_Resp_Grateful,
                                         data$T1b_Emo_Resp_Inspired,
                                         data$T1b_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t1b<-spos_t1b$MEAN

fear_t1b<-transform(cbind(data$T1b_Emo_Resp_Worry,
                          data$T1b_Emo_Resp_Panicked),
                    MEAN=apply(cbind(data$T1b_Emo_Resp_Worry,
                                     data$T1b_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t1b<-fear_t1b$MEAN

sneg_t1b<-transform(cbind(data$T1b_Emo_Resp_Guilty,
                              data$T1b_Emo_Resp_Ashamed),
                        MEAN=apply(cbind(data$T1b_Emo_Resp_Guilty,
                                         data$T1b_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t1b<-sneg_t1b$MEAN

gpos_t1c<-transform(cbind(data$T1c_Emo_Resp_Amused,
                             data$T1c_Emo_Resp_Glad),
                       MEAN=apply(cbind(data$T1c_Emo_Resp_Amused,
                                        data$T1c_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t1c<-gpos_t1c$MEAN

spos_t1c<-transform(cbind(data$T1c_Emo_Resp_Grateful,
                              data$T1c_Emo_Resp_Inspired,
                              data$T1c_Emo_Resp_Love),
                        MEAN=apply(cbind(data$T1c_Emo_Resp_Grateful,
                                         data$T1c_Emo_Resp_Inspired,
                                         data$T1c_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t1c<-spos_t1c$MEAN

fear_t1c<-transform(cbind(data$T1c_Emo_Resp_Worry,
                          data$T1c_Emo_Resp_Panicked),
                    MEAN=apply(cbind(data$T1c_Emo_Resp_Worry,
                                     data$T1c_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t1c<-fear_t1c$MEAN

sneg_t1c<-transform(cbind(data$T1c_Emo_Resp_Guilty,
                              data$T1c_Emo_Resp_Ashamed),
                        MEAN=apply(cbind(data$T1c_Emo_Resp_Guilty,
                                         data$T1c_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t1c<-sneg_t1c$MEAN

gpos_t2<-transform(cbind(data$T2_Emo_Resp_Amused,
                            data$T2_Emo_Resp_Glad),
                      MEAN=apply(cbind(data$T2_Emo_Resp_Amused,
                                       data$T2_Emo_Resp_Glad),1, mean, na.rm = TRUE))
data$gpos_t2<-gpos_t2$MEAN

spos_t2<-transform(cbind(data$T2_Emo_Resp_Grateful,
                             data$T2_Emo_Resp_Inspired,
                             data$T2_Emo_Resp_Love),
                       MEAN=apply(cbind(data$T2_Emo_Resp_Grateful,
                                        data$T2_Emo_Resp_Inspired,
                                        data$T2_Emo_Resp_Love),1, mean, na.rm = TRUE))
data$spos_t2<-spos_t2$MEAN

fear_t2<-transform(cbind(data$T2_Emo_Resp_Worry,
                         data$T2_Emo_Resp_Panicked),
                   MEAN=apply(cbind(data$T2_Emo_Resp_Worry,
                                    data$T2_Emo_Resp_Panicked),1, mean, na.rm = TRUE))
data$fear_t2<-fear_t2$MEAN

sneg_t2<-transform(cbind(data$T2_Emo_Resp_Guilty,
                             data$T2_Emo_Resp_Ashamed),
                       MEAN=apply(cbind(data$T2_Emo_Resp_Guilty,
                                        data$T2_Emo_Resp_Ashamed),1, mean, na.rm = TRUE))
data$sneg_t2<-sneg_t2$MEAN

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

T2_Past_Behaviors<-transform(cbind(data$T2_ActPast_Prevent_Self,
                                   data$T2_ActPast_Prevent_Mask,
                                   data$T2_ActPast_Prevent_Sanitize,
                                   data$T2_ActPast_SocialDistance,
                                   data$T2_ActPast_SelfIsolate),
                             MEAN=apply(cbind(data$T2_ActPast_Prevent_Self,
                                              data$T2_ActPast_Prevent_Mask,
                                              data$T2_ActPast_Prevent_Sanitize,
                                              data$T2_ActPast_SocialDistance,
                                              data$T2_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
data$T2_Past_Behaviors<-T2_Past_Behaviors$MEAN

T2a_Past_Behaviors<-transform(cbind(data$T2a_ActPast_Prevent_StrictHygiene,
                                    data$T2a_ActPast_SelfIsolate,
                                    data$T2a_ActPast_Prevent_Mask,
                                    data$T2a_ActPast_SocialDistance),
                              MEAN=apply(cbind(data$T2a_ActPast_Prevent_StrictHygiene,
                                               data$T2a_ActPast_SelfIsolate,
                                               data$T2a_ActPast_Prevent_Mask,
                                               data$T2a_ActPast_SocialDistance),1, mean, na.rm = TRUE))
data$T2a_Past_Behaviors<-T2a_Past_Behaviors$MEAN

T2b_Past_Behaviors<-transform(cbind(data$T2b_ActPast_Prevent_StrictHygiene,
                                    data$T2b_ActPast_SelfIsolate,
                                    data$T2b_ActPast_Prevent_Mask,
                                    data$T2b_ActPast_SocialDistance),
                              MEAN=apply(cbind(data$T2b_ActPast_Prevent_StrictHygiene,
                                               data$T2b_ActPast_SelfIsolate,
                                               data$T2b_ActPast_Prevent_Mask,
                                               data$T2b_ActPast_SocialDistance),1, mean, na.rm = TRUE))
data$T2b_Past_Behaviors<-T2b_Past_Behaviors$MEAN

T2c_Past_Behaviors<-transform(cbind(data$T2c_ActPast_Prevent_StrictHygiene,
                                    data$T2c_ActPast_SelfIsolate,
                                    data$T2c_ActPast_Prevent_Mask,
                                    data$T2c_ActPast_SocialDistance),
                              MEAN=apply(cbind(data$T2c_ActPast_Prevent_StrictHygiene,
                                               data$T2c_ActPast_SelfIsolate,
                                               data$T2c_ActPast_Prevent_Mask,
                                               data$T2c_ActPast_SocialDistance),1, mean, na.rm = TRUE))
data$T2c_Past_Behaviors<-T2c_Past_Behaviors$MEAN

T3_Past_Behaviors<-transform(cbind(data$T3_ActPast_Prevent_Self,
                                   data$T3_ActPast_Prevent_Mask,
                                   data$T3_ActPast_Prevent_Sanitize,
                                   data$T3_ActPast_SocialDistance,
                                   data$T3_ActPast_SelfIsolate),
                             MEAN=apply(cbind(data$T3_ActPast_Prevent_Self,
                                              data$T3_ActPast_Prevent_Mask,
                                              data$T3_ActPast_Prevent_Sanitize,
                                              data$T3_ActPast_SocialDistance,
                                              data$T3_ActPast_SelfIsolate),1, mean, na.rm = TRUE))
data$T3_Past_Behaviors<-T3_Past_Behaviors$MEAN

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

T2_Future_Behaviors<-transform(cbind(data$T2_ActPresent_Prevent_Self,
                                     data$T2_ActPresent_Prevent_Mask,
                                     data$T2_ActPresent_Prevent_Sanitize,
                                     data$T2_ActPresent_SocialDistance,
                                     data$T2_ActPresent_SelfIsolate),
                               MEAN=apply(cbind(data$T2_ActPresent_Prevent_Self,
                                                data$T2_ActPresent_Prevent_Mask,
                                                data$T2_ActPresent_Prevent_Sanitize,
                                                data$T2_ActPresent_SocialDistance,
                                                data$T2_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
data$T2_Future_Behaviors<-T2_Future_Behaviors$MEAN

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

T4_Future_Behaviors<-transform(cbind(data$T4_ActPresent_Prevent_Self,
                                     data$T4_ActPresent_Prevent_Mask,
                                     data$T4_ActPresent_Prevent_Sanitize,
                                     data$T4_ActPresent_SocialDistance,
                                     data$T4_ActPresent_SelfIsolate),
                               MEAN=apply(cbind(data$T4_ActPresent_Prevent_Self,
                                                data$T4_ActPresent_Prevent_Mask,
                                                data$T4_ActPresent_Prevent_Sanitize,
                                                data$T4_ActPresent_SocialDistance,
                                                data$T4_ActPresent_SelfIsolate),1, mean, na.rm = TRUE))
data$T4_Future_Behaviors<-T4_Future_Behaviors$MEAN

T2_Depression_Sum<-transform(cbind(data$T2_CES_D_1,
                             data$T2_CES_D_2,
                             data$T2_CES_D_3,
                             data$T2_CES_D_4,
                             (3-data$T2_CES_D_5),
                             data$T2_CES_D_6,
                             data$T2_CES_D_7,
                             (3-data$T2_CES_D_8),
                             data$T2_CES_D_9,
                             data$T2_CES_D_10),
                       MEAN=apply(cbind(data$T2_CES_D_1,
                                        data$T2_CES_D_2,
                                        data$T2_CES_D_3,
                                        data$T2_CES_D_4,
                                        (3-data$T2_CES_D_5),
                                        data$T2_CES_D_6,
                                        data$T2_CES_D_7,
                                        (3-data$T2_CES_D_8),
                                        data$T2_CES_D_9,
                                        data$T2_CES_D_10),1,mean,na.rm=TRUE))
data$T2_Depression_Sum<-T2_Depression_Sum$MEAN*10

T3_Depression_Sum<-transform(cbind(data$T3_CES_D_1,
                                   data$T3_CES_D_2,
                                   data$T3_CES_D_3,
                                   data$T3_CES_D_4,
                                   (3-data$T3_CES_D_5),
                                   data$T3_CES_D_6,
                                   data$T3_CES_D_7,
                                   (3-data$T3_CES_D_8),
                                   data$T3_CES_D_9,
                                   data$T3_CES_D_10),
                             MEAN=apply(cbind(data$T3_CES_D_1,
                                             data$T3_CES_D_2,
                                             data$T3_CES_D_3,
                                             data$T3_CES_D_4,
                                             (3-data$T3_CES_D_5),
                                             data$T3_CES_D_6,
                                             data$T3_CES_D_7,
                                             (3-data$T3_CES_D_8),
                                             data$T3_CES_D_9,
                                             data$T3_CES_D_10),1,mean,na.rm=TRUE))
data$T3_Depression_Sum<-T3_Depression_Sum$MEAN*10

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

T2_Anxiety_Sum<-transform(cbind(data$T2_HADS_1,
                                data$T2_HADS_2,
                                data$T2_HADS_3,
                                data$T2_HADS_4,
                                data$T2_HADS_5,
                                data$T2_HADS_6,
                                data$T2_HADS_7),
                             MEAN=apply(cbind(data$T2_HADS_1,
                                             data$T2_HADS_2,
                                             data$T2_HADS_3,
                                             data$T2_HADS_4,
                                             data$T2_HADS_5,
                                             data$T2_HADS_6,
                                             data$T2_HADS_7),1,mean,na.rm=TRUE))
data$T2_Anxiety_Sum<-T2_Anxiety_Sum$MEAN*7

T3_Anxiety_Sum<-transform(cbind(data$T3_HADS_1,
                                data$T3_HADS_2,
                                data$T3_HADS_3,
                                data$T3_HADS_4,
                                data$T3_HADS_5,
                                data$T3_HADS_6,
                                data$T3_HADS_7),
                          MEAN=apply(cbind(data$T3_HADS_1,
                                           data$T3_HADS_2,
                                           data$T3_HADS_3,
                                           data$T3_HADS_4,
                                           data$T3_HADS_5,
                                           data$T3_HADS_6,
                                           data$T3_HADS_7),1,mean,na.rm=TRUE))
data$T3_Anxiety_Sum<-T3_Anxiety_Sum$MEAN*7

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

T2_PWB_Mean<-transform(cbind(data$T2_PWB_Meaningful,
                             data$T2_PWB_ClearPurpose,
                             (6-data$T2_PWB_NoPurpose)),
                       MEAN=apply(cbind(data$T2_PWB_Meaningful,
                                        data$T2_PWB_ClearPurpose,
                                        (6-data$T2_PWB_NoPurpose)),1,mean,na.rm=TRUE))
data$T2_PWB_Mean<-T2_PWB_Mean$MEAN

T3_PWB_Mean<-transform(cbind(data$T3_PWB_Meaningful,
                             data$T3_PWB_ClearPurpose,
                             (6-data$T3_PWB_NoPurpose)),
                       MEAN=apply(cbind(data$T3_PWB_Meaningful,
                                        data$T3_PWB_ClearPurpose,
                                        (6-data$T3_PWB_NoPurpose)),1,mean,na.rm=TRUE))
data$T3_PWB_Mean<-T3_PWB_Mean$MEAN

T4_PWB_Mean<-transform(cbind(data$T4_PWB_Meaningful,
                             data$T4_PWB_ClearPurpose,
                             (6-data$T4_PWB_NoPurpose)),
                       MEAN=apply(cbind(data$T4_PWB_Meaningful,
                                        data$T4_PWB_ClearPurpose,
                                        (6-data$T4_PWB_NoPurpose)),1,mean,na.rm=TRUE))
data$T4_PWB_Mean<-T4_PWB_Mean$MEAN

T2_SWL_Mean<-transform(cbind(data$T2_SWL_1,
                             data$T2_SWL_2,
                             data$T2_SWL_3,
                             data$T2_SWL_4,
                             data$T2_SWL_5),
                       MEAN=apply(cbind(data$T2_SWL_1,
                                        data$T2_SWL_2,
                                        data$T2_SWL_3,
                                        data$T2_SWL_4,
                                        data$T2_SWL_5),1,mean,na.rm=TRUE))
data$T2_SWL_Mean<-T2_SWL_Mean$MEAN

T3_SWL_Mean<-transform(cbind(data$T3_SWL_1,
                             data$T3_SWL_2,
                             data$T3_SWL_3,
                             data$T3_SWL_4,
                             data$T3_SWL_5),
                       MEAN=apply(cbind(data$T3_SWL_1,
                                        data$T3_SWL_2,
                                        data$T3_SWL_3,
                                        data$T3_SWL_4,
                                        data$T3_SWL_5),1,mean,na.rm=TRUE))
data$T3_SWL_Mean<-T3_SWL_Mean$MEAN

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

T2_MentalHealth<-transform(cbind(scale(data$T2_Anxiety_Sum),
                             scale(data$T2_Depression_Sum)),
                       MEAN=apply(cbind(scale(data$T2_Anxiety_Sum),
                                        scale(data$T2_Depression_Sum)),1, mean, na.rm = TRUE))
data$T2_MentalHealth<-T2_MentalHealth$MEAN

T2_Wellbeing<-transform(cbind(scale(data$T2_SWL_Mean),
                              scale(data$T2_PWB_Mean)),
                        MEAN=apply(cbind(scale(data$T2_SWL_Mean),
                                         scale(data$T2_PWB_Mean)),1, mean, na.rm = TRUE))
data$T2_Wellbeing<-T2_Wellbeing$MEAN


T2a_MentalHealth<-transform(cbind(data$T2a_Anxious,
                                  data$T2a_CES_D_Depressed),
                            MEAN=apply(cbind(data$T2a_Anxious,
                                             data$T2a_CES_D_Depressed),1, mean, na.rm = TRUE))
data$T2a_MentalHealth<-T2a_MentalHealth$MEAN

T2a_Wellbeing<-transform(cbind(data$T2a_Satisfied,
                               data$T2a_PWB_ClearPurpose),
                         MEAN=apply(cbind(data$T2a_Satisfied,
                                          data$T2a_PWB_ClearPurpose),1, mean, na.rm = TRUE))
data$T2a_Wellbeing<-T2a_Wellbeing$MEAN

T2b_MentalHealth<-transform(cbind(data$T2b_Anxious,
                                  data$T2b_CES_D_Depressed),
                            MEAN=apply(cbind(data$T2b_Anxious,
                                             data$T2b_CES_D_Depressed),1, mean, na.rm = TRUE))
data$T2b_MentalHealth<-T2b_MentalHealth$MEAN

T2b_Wellbeing<-transform(cbind(data$T2b_Satisfied,
                               data$T2b_PWB_ClearPurpose),
                         MEAN=apply(cbind(data$T2b_Satisfied,
                                          data$T2b_PWB_ClearPurpose),1, mean, na.rm = TRUE))
data$T2b_Wellbeing<-T2b_Wellbeing$MEAN

T2c_MentalHealth<-transform(cbind(data$T2c_Anxious,
                                  data$T2c_CES_D_Depressed),
                            MEAN=apply(cbind(data$T2c_Anxious,
                                             data$T2c_CES_D_Depressed),1, mean, na.rm = TRUE))
data$T2c_MentalHealth<-T2c_MentalHealth$MEAN

T2c_Wellbeing<-transform(cbind(data$T2c_Satisfied,
                               data$T2c_PWB_ClearPurpose),
                         MEAN=apply(cbind(data$T2c_Satisfied,
                                          data$T2c_PWB_ClearPurpose),1, mean, na.rm = TRUE))
data$T2c_Wellbeing<-T2c_Wellbeing$MEAN

T3_MentalHealth<-transform(cbind(scale(data$T3_Anxiety_Sum),
                             scale(data$T3_Depression_Sum)),
                       MEAN=apply(cbind(scale(data$T3_Anxiety_Sum),
                                        scale(data$T3_Depression_Sum)),1, mean, na.rm = TRUE))
data$T3_MentalHealth<-T3_MentalHealth$MEAN

T3_Wellbeing<-transform(cbind(scale(data$T3_SWL_Mean),
                              scale(data$T3_PWB_Mean)),
                        MEAN=apply(cbind(scale(data$T3_SWL_Mean),
                                         scale(data$T3_PWB_Mean)),1, mean, na.rm = TRUE))
data$T3_Wellbeing<-T3_Wellbeing$MEAN

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

##############RESTRUCTURE DATA##############

#restructure data for sequential mediation models
data_wide<-cbind.data.frame(data$ID, #pare down dataset 
                            data$T1b_Reap_Success,
                            data$gpos_t1c,
                            data$spos_t1c,
                            data$fear_t1c,
                            data$sneg_t1c,
                            data$gpos_t2,
                            data$spos_t2,
                            data$fear_t2,
                            data$sneg_t2,
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
                   "T1b_Reap_Success",
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
                   "gpos5",
                   "spos5",
                   "fear5",
                   "sneg5",
                   "gpos6",
                   "spos6",
                   "fear6",
                   "sneg6",
                   "T3_Future_Behaviors",
                   "T4_Past_Behaviors",
                   "T4_MentalHealth",
                   "T4_Wellbeing")

long <- reshape(data_wide,varying = c("gpos1", #shape in long format 
                                      "gpos2",
                                      "gpos3",
                                      "gpos4",
                                      "gpos5",
                                      "gpos6",
                                 "spos1", 
                                 "spos2",
                                 "spos3",
                                 "spos4",
                                 "spos5",
                                 "spos6",
                                 "sneg1", 
                                 "sneg2", 
                                 "sneg3", 
                                 "sneg4", 
                                 "sneg5",
                                 "sneg6",
                                 "fear1", 
                                 "fear2", 
                                 "fear3", 
                                 "fear4", 
                                 "fear5",
                                 "fear6"),
                direction = 'long', idvar = 'ID', sep = '')

#calculate association between fear and social positive emotions before introducing -99s
summary(lme(scale(spos) ~ 1 + scale(fear), random =~1|ID, data=long, na.action="na.exclude"))

#replace NAs with -99 for mplus
long[is.na(long)] <- -99 

#rehape data for cross-lag panel mediation models
#prepare dataset for weekly CLPM
clpm1<-cbind.data.frame(data$T1b_Reap_Success, 
                            data$T2_Reap_Success,
                            data$T2b_Reap_Success,
                            data$fear_t1b,
                            data$fear_t2,
                            data$fear_t2b,
                            data$spos_t1b,
                            data$spos_t2,
                            data$spos_t2b,
                            data$sneg_t1b,
                            data$sneg_t2,
                            data$sneg_t2b,
                            data$gpos_t1b,
                            data$gpos_t2,
                            data$gpos_t2b,
                            data$T2a_MentalHealth,
                            data$T2b_MentalHealth,
                            data$T2c_MentalHealth,
                            data$T2a_Wellbeing,
                            data$T2b_Wellbeing,
                            data$T2c_Wellbeing,
                            data$T2a_Past_Behaviors,
                            data$T2b_Past_Behaviors,
                            data$T2c_Past_Behaviors)

#prepare dataset for monthly CLPM
clpm2<-cbind.data.frame(data$T1_Reap_Success, 
                        data$T2_Reap_Success,
                        data$T3_Reap_Success,
                        data$fear_t1b,
                        data$fear_t2,
                        data$fear_t3,
                        data$spos_t1b,
                        data$spos_t2,
                        data$spos_t3,
                        data$sneg_t1b,
                        data$sneg_t2,
                        data$sneg_t3,
                        data$gpos_t1b,
                        data$gpos_t2,
                        data$gpos_t3,
                        data$T2_MentalHealth,
                        data$T3_MentalHealth,
                        data$T4_MentalHealth,
                        data$T2_Wellbeing,
                        data$T3_Wellbeing,
                        data$T4_Wellbeing,
                        data$T2_Past_Behaviors,
                        data$T3_Past_Behaviors,
                        data$T4_Past_Behaviors,
                        data$T2_Future_Behaviors,
                        data$T3_Future_Behaviors,
                        data$T4_Future_Behaviors)

clpm1[is.na(clpm1)] <- -99 #replace missing values with -99
clpm2[is.na(clpm2)] <- -99 #replace missing values with -99

#NOTE: export csv files for MPlus, you will need to open the resulting files and delete the first row (the variable names)
write.csv(long,"samplea_sequential.csv", row.names=F)
write.csv(clpm1,"samplea_clpm_weekly.csv", row.names=F)
write.csv(clpm2,"samplea_clpm_monthly.csv", row.names=F)

##############CALCULATE RELIABILITY STATISTICS##############
psych::alpha(cbind.data.frame(data$T1_ER_Reappraise_Success_1,
                              data$T1_ER_Reappraise_Success_2,
                              data$T1_ER_Reappraise_Success_3,
                              data$T1_ER_Reappraise_Success_4)) #.88

psych::alpha(cbind.data.frame(data$T1b_ER_Reappraise_Success_1,
                              data$T1b_ER_Reappraise_Success_2,
                              data$T1b_ER_Reappraise_Success_3,
                              data$T1b_ER_Reappraise_Success_4)) #.92

psych::alpha(cbind.data.frame(data$T2_ER_Reappraise_Success_1,
                              data$T2_ER_Reappraise_Success_2,
                              data$T2_ER_Reappraise_Success_3,
                              data$T2_ER_Reappraise_Success_4)) #.92

psych::alpha(cbind.data.frame(data$T3_ER_Reappraise_Success_1,
                              data$T3_ER_Reappraise_Success_2,
                              data$T3_ER_Reappraise_Success_3,
                              data$T3_ER_Reappraise_Success_4)) #.93

psych::alpha(cbind.data.frame(data$T1b_Emo_Resp_Grateful,
                              data$T1b_Emo_Resp_Inspired,
                              data$T1b_Emo_Resp_Love)) #.83

psych::alpha(cbind.data.frame(data$T1c_Emo_Resp_Grateful,
                              data$T1c_Emo_Resp_Inspired,
                              data$T1c_Emo_Resp_Love)) #.80

psych::alpha(cbind.data.frame(data$T2_Emo_Resp_Grateful,
                              data$T2_Emo_Resp_Inspired,
                              data$T2_Emo_Resp_Love)) #.85

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Grateful,
                              data$T2a_Emo_Resp_Inspired,
                              data$T2a_Emo_Resp_Love)) #.83

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Grateful,
                              data$T2b_Emo_Resp_Inspired,
                              data$T2b_Emo_Resp_Love)) #.85

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Grateful,
                              data$T2c_Emo_Resp_Inspired,
                              data$T2c_Emo_Resp_Love)) #.84

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Grateful,
                              data$T3_Emo_Resp_Inspired,
                              data$T3_Emo_Resp_Love)) #.83

#NOTE: a warning message will be shown because some statistis in the alpha output do not make sense for two-item measures, the standarrdized alpha can still be interpreted
psych::alpha(cbind.data.frame(data$T1b_Emo_Resp_Worry,
                              data$T1b_Emo_Resp_Panicked)) #.85

psych::alpha(cbind.data.frame(data$T1c_Emo_Resp_Worry,
                              data$T1c_Emo_Resp_Panicked)) #.84

psych::alpha(cbind.data.frame(data$T2_Emo_Resp_Worry,
                              data$T2_Emo_Resp_Panicked)) #.86

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Worry,
                              data$T2a_Emo_Resp_Panicked)) #.81

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Worry,
                              data$T2b_Emo_Resp_Panicked)) #.83

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Worry,
                              data$T2c_Emo_Resp_Panicked)) #.84

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Worry,
                              data$T3_Emo_Resp_Panicked)) #.88

#
psych::alpha(cbind.data.frame(data$T1b_Emo_Resp_Guilty,
                              data$T1b_Emo_Resp_Ashamed)) #.75

psych::alpha(cbind.data.frame(data$T1c_Emo_Resp_Guilty,
                              data$T1c_Emo_Resp_Ashamed)) #.72

psych::alpha(cbind.data.frame(data$T2_Emo_Resp_Guilty,
                              data$T2_Emo_Resp_Ashamed)) #.76

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Guilty,
                              data$T2a_Emo_Resp_Ashamed)) #.78

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Guilty,
                              data$T2b_Emo_Resp_Ashamed)) #.74

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Guilty,
                              data$T2c_Emo_Resp_Ashamed)) #.73

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Guilty,
                              data$T3_Emo_Resp_Ashamed)) #.83
#
psych::alpha(cbind.data.frame(data$T1b_Emo_Resp_Amused,
                              data$T1b_Emo_Resp_Glad)) #.74

psych::alpha(cbind.data.frame(data$T1c_Emo_Resp_Amused,
                              data$T1c_Emo_Resp_Glad)) #.74

psych::alpha(cbind.data.frame(data$T2_Emo_Resp_Amused,
                              data$T2_Emo_Resp_Glad)) #.71

psych::alpha(cbind.data.frame(data$T2a_Emo_Resp_Amused,
                              data$T2a_Emo_Resp_Glad)) #.78

psych::alpha(cbind.data.frame(data$T2b_Emo_Resp_Amused,
                              data$T2b_Emo_Resp_Glad)) #.77

psych::alpha(cbind.data.frame(data$T2c_Emo_Resp_Amused,
                              data$T2c_Emo_Resp_Glad)) #.80

psych::alpha(cbind.data.frame(data$T3_Emo_Resp_Amused,
                              data$T3_Emo_Resp_Glad)) #.77
#
psych::alpha(cbind.data.frame(data$T2_ActPast_Prevent_Self,
                              data$T2_ActPast_Prevent_Mask,
                              data$T2_ActPast_Prevent_Sanitize,
                              data$T2_ActPast_SocialDistance,
                              data$T2_ActPast_SelfIsolate)) #.69

psych::alpha(cbind.data.frame(data$T3_ActPast_Prevent_Self,
                              data$T3_ActPast_Prevent_Mask,
                              data$T3_ActPast_Prevent_Sanitize,
                              data$T3_ActPast_SocialDistance,
                              data$T3_ActPast_SelfIsolate)) #.77

psych::alpha(cbind.data.frame(data$T4_ActPast_Prevent_Self,
                              data$T4_ActPast_Prevent_Mask,
                              data$T4_ActPast_Prevent_Sanitize,
                              data$T4_ActPast_SocialDistance,
                              data$T4_ActPast_SelfIsolate)) #.78

psych::alpha(cbind.data.frame(data$T2a_ActPast_Prevent_StrictHygiene,
                              data$T2a_ActPast_SelfIsolate,
                              data$T2a_ActPast_Prevent_Mask,
                              data$T2a_ActPast_SocialDistance)) #.61

psych::alpha(cbind.data.frame(data$T2b_ActPast_Prevent_StrictHygiene,
                              data$T2b_ActPast_SelfIsolate,
                              data$T2b_ActPast_Prevent_Mask,
                              data$T2b_ActPast_SocialDistance)) #.60

psych::alpha(cbind.data.frame(data$T2c_ActPast_Prevent_StrictHygiene,
                              data$T2c_ActPast_SelfIsolate,
                              data$T2c_ActPast_Prevent_Mask,
                              data$T2c_ActPast_SocialDistance)) #.68

#
psych::alpha(cbind.data.frame(data$T2_ActPresent_Prevent_Self,
                              data$T2_ActPresent_Prevent_Mask,
                              data$T2_ActPresent_Prevent_Sanitize,
                              data$T2_ActPresent_SocialDistance,
                              data$T2_ActPresent_SelfIsolate)) #.75

psych::alpha(cbind.data.frame(data$T3_ActPresent_Prevent_Self,
                              data$T3_ActPresent_Prevent_Mask,
                              data$T3_ActPresent_Prevent_Sanitize,
                              data$T3_ActPresent_SocialDistance,
                              data$T3_ActPresent_SelfIsolate)) #.81

psych::alpha(cbind.data.frame(data$T4_ActPresent_Prevent_Self,
                              data$T4_ActPresent_Prevent_Mask,
                              data$T4_ActPresent_Prevent_Sanitize,
                              data$T4_ActPresent_SocialDistance,
                              data$T4_ActPresent_SelfIsolate)) #.82

#NOTE, a warning message will be shown because some statistis in the alpha output do not make sense for two-item measures, the standarrdized alpha can still be interpreted
psych::alpha(cbind.data.frame(scale(data$T2_Anxiety_Sum),
                              scale(data$T2_Depression_Sum))) #.91

psych::alpha(cbind.data.frame(scale(data$T3_Anxiety_Sum),
                              scale(data$T3_Depression_Sum))) #.91

psych::alpha(cbind.data.frame(scale(data$T4_Anxiety_Sum),
                              scale(data$T4_Depression_Sum))) #.91

psych::alpha(cbind.data.frame(data$T2a_Anxious,
                              data$T2a_CES_D_Depressed)) #.79

psych::alpha(cbind.data.frame(data$T2b_Anxious,
                              data$T2b_CES_D_Depressed)) #.82

psych::alpha(cbind.data.frame(data$T2c_Anxious,
                              data$T2c_CES_D_Depressed)) #.81

#
psych::alpha(cbind.data.frame(scale(data$T2_SWL_Mean),
                              scale(data$T2_PWB_Mean))) #.77

psych::alpha(cbind.data.frame(scale(data$T3_SWL_Mean),
                              scale(data$T3_PWB_Mean))) #.80

psych::alpha(cbind.data.frame(scale(data$T4_SWL_Mean),
                              scale(data$T4_PWB_Mean))) #.81

psych::alpha(cbind.data.frame(data$T2a_Satisfied,
                              data$T2a_PWB_ClearPurpose)) #.79

psych::alpha(cbind.data.frame(data$T2b_Satisfied,
                              data$T2b_PWB_ClearPurpose)) #.82

psych::alpha(cbind.data.frame(data$T2c_Satisfied,
                              data$T2c_PWB_ClearPurpose)) #.80

##############EXCLUDE PARTICIPANTS WHO WERE NOT INCLUDED IN ANY ANALYSES BEFORE CAlCULATING DESCRIPTIVE STATISTICS##############

#sequential model
data_pared<-subset(long,long$T1b_Reap_Success!=-99|long$T4_MentalHealth!=-99|long$T4_Past_Behaviors!=-99|long$spos!=-99)
nrow(data_pared)
sequential_ps<-unique(data_pared$ID)
length(sequential_ps)

#weekly clpm model
data_pared2<-subset(data,!is.na(data$T1b_Reap_Success)|!is.na(data$fear_t1b)|!is.na(data$T2a_Past_Behaviors)
                      |!is.na(data$T2_Reap_Success)|!is.na(data$fear_t2)|!is.na(data$T2b_Past_Behaviors)
|!is.na(data$T2b_Reap_Success)|!is.na(data$fear_t2b)|!is.na(data$T2c_Past_Behaviors))
clpm_weekly_ps<-unique(data_pared2$ID)
length(clpm_weekly_ps) #53 Ps excluded for missing on all variables 

data_pared2<-subset(data_pared2,!is.na(data_pared2$T1b_Reap_Success)&!is.na(data_pared2$fear_t1b)&!is.na(data_pared2$T2a_Past_Behaviors))
clpm_weekly_ps<-unique(data_pared2$ID)
length(clpm_weekly_ps) #153 Ps excluded for missing on x variables

data_pared2<-subset(data_pared2,!is.na(data_pared2$T2_Reap_Success)|!is.na(data_pared2$fear_t2)|!is.na(data_pared2$T2b_Past_Behaviors)
                    |!is.na(data_pared2$T2b_Reap_Success)|!is.na(data_pared2$fear_t2b)|!is.na(data_pared2$T2c_Past_Behaviors))
clpm_weekly_ps<-unique(data_pared2$ID)
length(clpm_weekly_ps) #4 Ps excluded for missing on all variables except x variables  

#monthly clpm model
data_pared3<-subset(data,!is.na(data$T1_Reap_Success)|!is.na(data$fear_t1b)|!is.na(data$T2_Past_Behaviors)
                    |!is.na(data$T2_Reap_Success)|!is.na(data$fear_t2)|!is.na(data$T3_Past_Behaviors)
                    |!is.na(data$T3_Reap_Success)|!is.na(data$fear_t3)|!is.na(data$T4_Past_Behaviors))
clpm_monthly_ps<-unique(data_pared3$ID)
length(clpm_monthly_ps) #3 Ps excluded for missing on all variables 

data_pared3<-subset(data_pared3,!is.na(data_pared3$T1_Reap_Success)&!is.na(data_pared3$fear_t1b)&!is.na(data_pared3$T2_Past_Behaviors))
clpm_monthly_ps<-unique(data_pared3$ID)
length(clpm_monthly_ps) #219 Ps excluded for missing on x variables

data_pared3<-subset(data_pared3,!is.na(data_pared3$T2_Reap_Success)|!is.na(data_pared3$fear_t2)|!is.na(data_pared3$T3_Past_Behaviors)
                    |!is.na(data_pared3$T3_Reap_Success)|!is.na(data_pared3$fear_t3)|!is.na(data_pared3$T4_Past_Behaviors))
clpm_monthly_ps<-unique(data_pared3$ID)
length(clpm_monthly_ps) #0 Ps excluded for missing on all variables except x variables  

total_ps<-c(sequential_ps,
clpm_weekly_ps,
clpm_monthly_ps)
total_ps<-unique(total_ps)
data_pared<-subset(data,data$ID%in%total_ps)
nrow(data_pared)

##############CALCULATE DESCRIPTIVE STATISTICS##############

#Demographics
summary(data_pared$T1_Age)
sd(data_pared$T1_Age,na.rm=T)
summary(as.factor(data_pared$T1_Gender))/nrow(data_pared)
summary(as.factor(data_pared$T1_Ethnicity))/nrow(data_pared)
summary(as.factor(data_pared$T1_PolParty))/nrow(data_pared)

#Reappraisal Descriptives for Results Section
mean(data_pared$T1b_Reap_Try,na.rm=T)
sd(data_pared$T1b_Reap_Try,na.rm=T)
sum(data_pared$T1b_Reap_Try>0,na.rm=T)/sum(!is.na(data_pared$T1b_Reap_Try))

mean(data_pared$T1b_Reap_Success,na.rm=T)
sd(data_pared$T1b_Reap_Success,na.rm=T)
sum(data_pared$T1b_Reap_Success>0,na.rm=T)/sum(!is.na(data_pared$T1b_Reap_Success))

#Emotions Descriptives for Results Section
sum(data_pared$fear_t1c>0,na.rm=T)/sum(!is.na(data_pared$fear_t1c))
sum(data_pared$fear_t2>0,na.rm=T)/sum(!is.na(data_pared$fear_t2))
sum(data_pared$fear_t2a>0,na.rm=T)/sum(!is.na(data_pared$fear_t2a))
sum(data_pared$fear_t2b>0,na.rm=T)/sum(!is.na(data_pared$fear_t2b))
sum(data_pared$fear_t2c>0,na.rm=T)/sum(!is.na(data_pared$fear_t2c))
sum(data_pared$fear_t3>0,na.rm=T)/sum(!is.na(data_pared$fear_t3))
#82%-94%

sum(data_pared$spos_t1c>0,na.rm=T)/sum(!is.na(data_pared$spos_t1c))
sum(data_pared$spos_t2>0,na.rm=T)/sum(!is.na(data_pared$spos_t2))
sum(data_pared$spos_t2a>0,na.rm=T)/sum(!is.na(data_pared$spos_t2a))
sum(data_pared$spos_t2b>0,na.rm=T)/sum(!is.na(data_pared$spos_t2b))
sum(data_pared$spos_t2c>0,na.rm=T)/sum(!is.na(data_pared$spos_t2c))
sum(data_pared$spos_t3>0,na.rm=T)/sum(!is.na(data_pared$spos_t3))
#89%-92%

#Supplementary Table 1
mean(data_pared$T1_Reap_Success, na.rm=T)
sd(data_pared$T1_Reap_Success, na.rm=T)

mean(data_pared$T1b_Reap_Success, na.rm=T)
sd(data_pared$T1b_Reap_Success, na.rm=T)

mean(data_pared$T2_Reap_Success, na.rm=T)
sd(data_pared$T2_Reap_Success, na.rm=T)

mean(data_pared$T2b_Reap_Success, na.rm=T)
sd(data_pared$T2b_Reap_Success, na.rm=T)

mean(data_pared$T3_Reap_Success, na.rm=T)
sd(data_pared$T3_Reap_Success, na.rm=T)

#

mean(data_pared$fear_t1b, na.rm=T)
sd(data_pared$fear_t1b, na.rm=T)

mean(data_pared$fear_t1c, na.rm=T)
sd(data_pared$fear_t1c, na.rm=T)

mean(data_pared$fear_t2, na.rm=T)
sd(data_pared$fear_t2, na.rm=T)

mean(data_pared$fear_t2a, na.rm=T)
sd(data_pared$fear_t2a, na.rm=T)

mean(data_pared$fear_t2b, na.rm=T)
sd(data_pared$fear_t2b, na.rm=T)

mean(data_pared$fear_t2c, na.rm=T)
sd(data_pared$fear_t2c, na.rm=T)

mean(data_pared$fear_t3, na.rm=T)
sd(data_pared$fear_t3, na.rm=T)

#

mean(data_pared$spos_t1b, na.rm=T)
sd(data_pared$spos_t1b, na.rm=T)

mean(data_pared$spos_t1c, na.rm=T)
sd(data_pared$spos_t1c, na.rm=T)

mean(data_pared$spos_t2, na.rm=T)
sd(data_pared$spos_t2, na.rm=T)

mean(data_pared$spos_t2a, na.rm=T)
sd(data_pared$spos_t2a, na.rm=T)

mean(data_pared$spos_t2b, na.rm=T)
sd(data_pared$spos_t2b, na.rm=T)

mean(data_pared$spos_t2c, na.rm=T)
sd(data_pared$spos_t2c, na.rm=T)

mean(data_pared$spos_t3, na.rm=T)
sd(data_pared$spos_t3, na.rm=T)

#

mean(data_pared$sneg_t1b, na.rm=T)
sd(data_pared$sneg_t1b, na.rm=T)

mean(data_pared$sneg_t1c, na.rm=T)
sd(data_pared$sneg_t1c, na.rm=T)

mean(data_pared$sneg_t2, na.rm=T)
sd(data_pared$sneg_t2, na.rm=T)

mean(data_pared$sneg_t2a, na.rm=T)
sd(data_pared$sneg_t2a, na.rm=T)

mean(data_pared$sneg_t2b, na.rm=T)
sd(data_pared$sneg_t2b, na.rm=T)

mean(data_pared$sneg_t2c, na.rm=T)
sd(data_pared$sneg_t2c, na.rm=T)

mean(data_pared$sneg_t3, na.rm=T)
sd(data_pared$sneg_t3, na.rm=T)

#

mean(data_pared$gpos_t1b, na.rm=T)
sd(data_pared$gpos_t1b, na.rm=T)

mean(data_pared$gpos_t1c, na.rm=T)
sd(data_pared$gpos_t1c, na.rm=T)

mean(data_pared$gpos_t2, na.rm=T)
sd(data_pared$gpos_t2, na.rm=T)

mean(data_pared$gpos_t2a, na.rm=T)
sd(data_pared$gpos_t2a, na.rm=T)

mean(data_pared$gpos_t2b, na.rm=T)
sd(data_pared$gpos_t2b, na.rm=T)

mean(data_pared$gpos_t2c, na.rm=T)
sd(data_pared$gpos_t2c, na.rm=T)

mean(data_pared$gpos_t3, na.rm=T)
sd(data_pared$gpos_t3, na.rm=T)


#
mean(data_pared$T2_Past_Behaviors, na.rm=T)
sd(data_pared$T2_Past_Behaviors, na.rm=T)

mean(data_pared$T2a_Past_Behaviors, na.rm=T)
sd(data_pared$T2a_Past_Behaviors, na.rm=T)

mean(data_pared$T2b_Past_Behaviors, na.rm=T)
sd(data_pared$T2b_Past_Behaviors, na.rm=T)

mean(data_pared$T2c_Past_Behaviors, na.rm=T)
sd(data_pared$T2c_Past_Behaviors, na.rm=T)

mean(data_pared$T3_Past_Behaviors, na.rm=T)
sd(data_pared$T3_Past_Behaviors, na.rm=T)

mean(data_pared$T4_Past_Behaviors, na.rm=T)
sd(data_pared$T4_Past_Behaviors, na.rm=T)

#
mean(data_pared$T2_Future_Behaviors, na.rm=T)
sd(data_pared$T2_Future_Behaviors, na.rm=T)

mean(data_pared$T3_Future_Behaviors, na.rm=T)
sd(data_pared$T3_Future_Behaviors, na.rm=T)

mean(data_pared$T4_Future_Behaviors, na.rm=T)
sd(data_pared$T4_Future_Behaviors, na.rm=T)

#
mean(data_pared$T2_MentalHealth, na.rm=T)
sd(data_pared$T2_MentalHealth, na.rm=T)

mean(data_pared$T2a_MentalHealth, na.rm=T)
sd(data_pared$T2a_MentalHealth, na.rm=T)

mean(data_pared$T2b_MentalHealth, na.rm=T)
sd(data_pared$T2b_MentalHealth, na.rm=T)

mean(data_pared$T2c_MentalHealth, na.rm=T)
sd(data_pared$T2c_MentalHealth, na.rm=T)

mean(data_pared$T3_MentalHealth, na.rm=T)
sd(data_pared$T3_MentalHealth, na.rm=T)

mean(data_pared$T4_MentalHealth, na.rm=T)
sd(data_pared$T4_MentalHealth, na.rm=T)

#
mean(data_pared$T2_Wellbeing, na.rm=T)
sd(data_pared$T2_Wellbeing, na.rm=T)

mean(data_pared$T2a_Wellbeing, na.rm=T)
sd(data_pared$T2a_Wellbeing, na.rm=T)

mean(data_pared$T2b_Wellbeing, na.rm=T)
sd(data_pared$T2b_Wellbeing, na.rm=T)

mean(data_pared$T2c_Wellbeing, na.rm=T)
sd(data_pared$T2c_Wellbeing, na.rm=T)

mean(data_pared$T3_Wellbeing, na.rm=T)
sd(data_pared$T3_Wellbeing, na.rm=T)

mean(data_pared$T4_Wellbeing, na.rm=T)
sd(data_pared$T4_Wellbeing, na.rm=T)

