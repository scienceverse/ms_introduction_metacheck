#################################################################
##                        Preprocess NO                        ##
#################################################################



##Cleaning NA in file
df_cipa <- df_cipa_raw |> 
  left_join(distance |> dplyr::select(postcode, dis_pop_adj, dis50, dis20, dis200, longitude, latitude,),
            by = c("PostalCode_T1" = "postcode")) |> 
  drop_na()

#sum(is.na(df_cipa$PostalCode_T1))

hist(df_cipa$Age_T1)
hist(df_cipa$Sex_T1)
#Many > 60
hist(df_cipa$PHQTot_T1)
hist(df_cipa$GADTot_T1)
hist(df_cipa$PsychDiagCatCurrent_T1_DepressionDisorder)
hist(df_cipa$PsychDiagCatCurrent_T1_AnxietyDisorder)

analysis_df_cipa <- df_cipa %>% 
  mutate(depression = PHQTot_T1 %>% as.character() %>% as.numeric(),
         anxiety = GADTot_T1 %>% as.character() %>% as.numeric(),
         sex = Sex_T1,
         age = Age_T1, 
         postcode = PostalCode_T1,
         scaled_depression = scale(depression),
         scaled_anxiety = scale(anxiety),
         self_reported_depression = PsychDiagCatCurrent_T1_DepressionDisorder,
         self_reported_anxiety = PsychDiagCatCurrent_T1_AnxietyDisorder,
         study = "CIPA") |> 
  dplyr::select(scaled_depression, scaled_anxiety, depression, anxiety, study, dis_pop_adj, dis50, dis20, dis200,
                age, sex, postcode,longitude, latitude, self_reported_depression, self_reported_anxiety)


NO_total_data <- rbind(analysis_df, analysis_df_cipa)



