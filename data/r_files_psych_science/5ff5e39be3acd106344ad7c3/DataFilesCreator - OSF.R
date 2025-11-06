# packages
library(ggplot2)
library(dplyr)
library(knitr)
library(reshape2)

# load raw data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Israeli = T # T to run on Israeli files

if(Israeli){
  
  # files
  SR_after_cross = read.delim("Sample_report_Israel.txt",
                              sep = "", header = TRUE, row.names=NULL)
  df_fixation = read.delim("Fixation_report_Israel.txt", 
                           sep = "", header = TRUE, row.names=NULL)
  trial_reveal = read.delim("trial_reveal_Israel.csv", 
                            sep = ",", header = TRUE, row.names=NULL)
  trial_contingent = read.delim("trial_contingent_Israel.csv", 
                            sep = ",", header = TRUE, row.names=NULL)
  bad_participants = c("shto329") # not a native Israeli
  output_file = "Israeli"
  
} else {
  
  #files
  SR_after_cross = read.delim("sample_report_Germany.txt", 
                              sep = "", header = TRUE, row.names=NULL)
  df_fixation = read.delim("Fixation_report_Germany.txt", 
                           sep = "", header = TRUE, row.names=NULL)
  trial_reveal = read.delim("trial_reveal_Germany.csv", 
                            sep = ",", header = TRUE, row.names=NULL)
  trial_contingent = read.delim("trial_contingent_Germany.csv", 
                                sep = ",", header = TRUE, row.names=NULL)
  bad_participants = c("vp01") # pilot participant (research assistant)
  output_file = "German"
}

# parameters
sub_vec = unique(SR_after_cross$RECORDING_SESSION_LABEL)

# remove participants
if(length(bad_participants) >0){
  df_fixation = df_fixation[!df_fixation$RECORDING_SESSION_LABEL==bad_participants,]
  sub_vec = sub_vec[!(sub_vec==bad_participants)]
}

# seperate the fixation report to the data of color, revealed and gaze-contingent blocks
df_fix_color = df_fixation 
df_fix_reveal = df_fixation
df_fix_contingent = df_fixation

for (sub in sub_vec){
  max_trial = max(df_fixation$TRIAL_INDEX[df_fixation$RECORDING_SESSION_LABEL==sub])
  df_fix_color = df_fix_color[!((df_fix_color$RECORDING_SESSION_LABEL==sub) & 
                                  df_fix_color$TRIAL_INDEX>(max_trial-114)),]
  df_fix_reveal = df_fix_reveal[!((df_fix_reveal$RECORDING_SESSION_LABEL==sub) & 
                                    (df_fix_reveal$TRIAL_INDEX>(max_trial-10)|
                                       df_fix_reveal$TRIAL_INDEX<(max_trial-109))),]
  df_fix_contingent = df_fix_contingent[!((df_fix_contingent$RECORDING_SESSION_LABEL==sub) & 
                                            df_fix_contingent$TRIAL_INDEX<(max_trial-9)),]
}

## Data cleaning

# remove fixaions detected outside of an interest area
df_fix_color = df_fix_color[!df_fix_color$CURRENT_FIX_INTEREST_AREA_ID==".",] 
df_fix_reveal = df_fix_reveal[!df_fix_reveal$CURRENT_FIX_INTEREST_AREA_ID==".",] 
df_fix_contingent = df_fix_contingent[!df_fix_contingent$CURRENT_FIX_INTEREST_AREA_ID==".",] 

# save data for the analysis without exclusion of trials (Supplemental Materials)
write.csv(df_fix_reveal, paste0(output_file,"_df_fix_reveal_original.csv"))
write.csv(df_fix_contingent, paste0(output_file,"_df_fix_contingent_original.csv"))


## Remove trials - Revealed block

# create dataframe for the proportion of missing data samples in each trial (i.e., bad recording)
bad_trials = data.frame("RECORDING_SESSION_LABEL" = NA,
                        "TRIAL_INDEX" = NA,
                        "NA_prop" = NA)
row = 1
for (sub in sub_vec){
  cur_sub = SR_after_cross[SR_after_cross$RECORDING_SESSION_LABEL==sub,]
  start_trial = max(cur_sub$TRIAL_INDEX) - 109 # first trial in the revealed block (after the color block and training)
  for (t in seq(start_trial,start_trial+99)){
    cur_trial = cur_sub[cur_sub$TRIAL_INDEX==t,]
    NA_prop = sum(cur_trial$RIGHT_IN_BLINK==1 | cur_trial$LEFT_IN_BLINK==1)/nrow(cur_trial)
    bad_trials[row,] = c(sub,t,NA_prop) 
    row = row + 1
  }
}

bad_trials$NA_prop = as.numeric(as.character(bad_trials$NA_prop))
to_remove = bad_trials[bad_trials$NA_prop>0.2,] # the index of trials with bad recordings 

#  extract trials in which participants did not look at the cetral cross
col1 = grep("sub",colnames(trial_reveal))
col2 = grep("trial_reveal",colnames(trial_reveal))
no_looking_trials = trial_reveal[trial_reveal$error_reveal==1,c(col1,col2)] # the number of trials withput lookig at the cross

# extract trials in which the last fixation was not on the familiar face (i.e., the target)
reveal_last_fix = df_fix_reveal %>% group_by(RECORDING_SESSION_LABEL, TRIAL_INDEX) %>%
  summarise(last_fix = max (CURRENT_FIX_INDEX))
reveal_last_df = merge(df_fix_reveal, reveal_last_fix,
                       by.x = c("RECORDING_SESSION_LABEL", "TRIAL_INDEX"),
                       by.y = c("RECORDING_SESSION_LABEL", "TRIAL_INDEX"))
reveal_check = reveal_last_df[reveal_last_df$CURRENT_FIX_INDEX==reveal_last_df$last_fix,
                              c("RECORDING_SESSION_LABEL","TRIAL_INDEX","trial_reveal",
                                "CURRENT_FIX_INTEREST_AREA_ID","german_loc", "israeli_loc")]
if(Israeli){
  reveal_check$remove_trial = ifelse(reveal_check$CURRENT_FIX_INTEREST_AREA_ID==
                                       reveal_check$israeli_loc, 0, 1)
}else{
  reveal_check$remove_trial = ifelse(reveal_check$CURRENT_FIX_INTEREST_AREA_ID==
                                       reveal_check$german_loc, 0, 1)
}

incorect_trials = 
  data.frame("sub" = reveal_check$RECORDING_SESSION_LABEL[reveal_check$remove_trial==1], 
             "trial_reveal" = reveal_check$trial_reveal[reveal_check$remove_trial==1]) # the number of trials in which participant press the space bar without looking at the target

# exclude trials + analyze proportion of excluded trials per participant
data_clean = df_fix_reveal

# dataframe for the proportion of trials excluded for each crieteria 
prop_trials = data.frame (sub = NA,
                          bad_recording = NA,
                          incorrect_trial = NA,
                          look_out_of_center = NA,
                          total = NA)

# dataframe for the proportion of the target's interest area in the excluded trials
prop_IA = data.frame(sub = NA,
                     prop1 = NA,
                     prop2 = NA,
                     prop3 = NA,
                     prop4 = NA,
                     prop5 = NA)

# dataframe for the excluded trial's number and the cause for exclusion
bad_trial_all_sub = data.frame(sub = c(),
                               bad_trials = c(),
                               type_trials = c())
row = 1
for (sub in sub_vec){
  cur_sub = data_clean[data_clean$RECORDING_SESSION_LABEL==sub,]
  
  # extract trial number for each exclusion criteria
  bad_trial_index = as.numeric(to_remove$TRIAL_INDEX[to_remove$RECORDING_SESSION_LABEL==sub]) 
  bad_trial_recording = unique(cur_sub$trial_reveal[cur_sub$TRIAL_INDEX %in% bad_trial_index])
  bad_trial_incorrect = unique(incorect_trials$trial_reveal[incorect_trials$sub==sub])
  bad_trial_no_look = unique(no_looking_trials$trial_reveal[no_looking_trials$sub==as.character(sub)])
  
  bad_trial_all = unique(c(bad_trial_recording,bad_trial_incorrect,bad_trial_no_look))
  bad_trial_inclusive = c(bad_trial_recording,bad_trial_incorrect,bad_trial_no_look)
  type_trials = c(rep("bad recording", length(bad_trial_recording)),
                  rep("incorrect", length(bad_trial_incorrect)),
                  rep("cross", length(bad_trial_no_look)))
  
  prop_trials[row,] = c(as.character(sub),
                        ifelse (length(bad_trial_recording)>0,
                          length(bad_trial_recording)/100,length(bad_trial_index)/100), # in case there are no such trials
                        length(bad_trial_incorrect)/100,
                        length(bad_trial_no_look)/100,
                        length(bad_trial_all)/100)
  
  # analyze excluded data
  data_bad = cur_sub[cur_sub$trial_reveal%in% bad_trial_all,]
  if(Israeli){
    IA_bad = data_bad %>% group_by(trial_reveal) %>% summarise(IA = mean(israeli_loc))
  } else{
    IA_bad = data_bad %>% group_by(trial_reveal) %>% summarise(IA = mean(german_loc))
  }
  
  prop_IA[row,] = c(as.character(sub),
                    sum(IA_bad$IA==1),
                    sum(IA_bad$IA==2),
                    sum(IA_bad$IA==3),
                    sum(IA_bad$IA==4),
                    sum(IA_bad$IA==5))
  
  df_bad_trial_sub = data.frame("sub" = rep(sub, length(bad_trial_inclusive)),
                            "bad_trials" = bad_trial_inclusive,
                            "type_trials" = type_trials)
  bad_trial_all_sub = rbind(bad_trial_all_sub, df_bad_trial_sub)
  
  # exclude trials
  data_clean = data_clean[!(data_clean$RECORDING_SESSION_LABEL==sub &
                              (data_clean$trial_reveal %in% bad_trial_all)),]
  row = row+1
}

# remove participants with more than 50% of excluded trials
remove_above_50 = TRUE
above_50_sub = prop_trials$sub[prop_trials$total>0.5]
if(remove_above_50){
  data_clean = data_clean[!data_clean$RECORDING_SESSION_LABEL %in% above_50_sub,]
  df_fix_contingent = df_fix_contingent[!df_fix_contingent$RECORDING_SESSION_LABEL %in% above_50_sub,]
  df_fix_color = df_fix_color[!df_fix_color$RECORDING_SESSION_LABEL %in% above_50_sub,]
  prop_trials = prop_trials[!prop_trials$sub %in% above_50_sub,]
  prop_IA = prop_IA[!prop_IA$sub %in% above_50_sub,]
}

# add column indicating whether the fixation is in the familiar face's IA
if(Israeli){
  data_clean$IA_familiar = ifelse(data_clean$israeli_loc==data_clean$CURRENT_FIX_INTEREST_AREA_ID,1,0)  
} else{
  data_clean$IA_familiar = ifelse(data_clean$german_loc==data_clean$CURRENT_FIX_INTEREST_AREA_ID,1,0)  
}

## Remove trials - gaze contingent block

# create dataframe for the proportion of missing data samples in each trial (i.e., bad recording)
bad_trials_con = data.frame("RECORDING_SESSION_LABEL" = NA,
                        "TRIAL_INDEX" = NA,
                        "NA_prop" = NA)
row = 1
for (sub in sub_vec){
  cur_sub = SR_after_cross[SR_after_cross$RECORDING_SESSION_LABEL==sub,]
  start_trial = max(cur_sub$TRIAL_INDEX) - 9
  for (t in seq(start_trial,start_trial+9)){
    cur_trial = cur_sub[cur_sub$TRIAL_INDEX==t,]
    NA_prop = sum(cur_trial$RIGHT_IN_BLINK==1 | cur_trial$LEFT_IN_BLINK==1)/nrow(cur_trial)
    bad_trials_con[row,] = c(sub,t,NA_prop) 
    row = row + 1
  }
}

bad_trials_con$NA_prop = as.numeric(as.character(bad_trials_con$NA_prop))
to_remove_con = bad_trials_con[bad_trials_con$NA_prop>0.2,] # according to trial index

#  extract trials in which participants did not look at the cetral cross
col1 = grep("sub",colnames(trial_contingent))
col2 = grep("trial_contingent",colnames(trial_contingent))
no_looking_trials_con = trial_contingent[trial_contingent$error_contingent==1,c(col1,col2)]

# extract trials in which the last fixation was not on the familiar face (i.e., the target)
contingent_last_fix = df_fix_contingent %>% group_by(RECORDING_SESSION_LABEL, trial_contingent) %>%
  summarise(last_fix = max (CURRENT_FIX_INDEX))
contingent_last_df = merge(df_fix_contingent, contingent_last_fix,
                           by.x = c("RECORDING_SESSION_LABEL", "trial_contingent"),
                           by.y = c("RECORDING_SESSION_LABEL", "trial_contingent"))
contingent_check = contingent_last_df[contingent_last_df$CURRENT_FIX_INDEX==contingent_last_df$last_fix,
                                      c("RECORDING_SESSION_LABEL","trial_contingent",
                                        "CURRENT_FIX_INTEREST_AREA_ID","german_loc_con", "israeli_loc_con")]
if(Israeli){
  contingent_check$remove_trial = ifelse(contingent_check$CURRENT_FIX_INTEREST_AREA_ID==
                                           contingent_check$israeli_loc, 0, 1)
}else{
  contingent_check$remove_trial = ifelse(contingent_check$CURRENT_FIX_INTEREST_AREA_ID==
                                           contingent_check$german_loc, 0, 1)
}

incorect_trials_con = 
  data.frame("sub" = contingent_check$RECORDING_SESSION_LABEL[contingent_check$remove_trial==1], 
             "trial_contingent" = contingent_check$trial_contingent[contingent_check$remove_trial==1])

# exclude trials + analyze proportion of excluded trials per participant
data_clean_con = df_fix_contingent

# dataframe for the proportion of trials excluded for each crieteria 
prop_trials_con = data.frame (sub = NA,
                          bad_recording = NA,
                          incorrect_trial = NA,
                          look_out_of_center = NA,
                          total = NA)

# dataframe for the proportion of the target's interest area in the excluded trials
prop_IA_con = data.frame(sub = NA,
                     prop1 = NA,
                     prop2 = NA,
                     prop3 = NA,
                     prop4 = NA,
                     prop5 = NA)

row = 1
for (sub in sub_vec){
  cur_sub = data_clean_con[data_clean_con$RECORDING_SESSION_LABEL==sub,]
  
  # bad trials seperatly for each error type
  bad_trial_index = as.numeric(to_remove_con$TRIAL_INDEX[to_remove_con$RECORDING_SESSION_LABEL==sub]) 
  bad_trial_recording = unique(cur_sub$trial_contingent[cur_sub$TRIAL_INDEX %in% bad_trial_index])
  
  bad_trial_incorrect = unique(incorect_trials_con$trial_contingent[incorect_trials_con$sub==sub])
  bad_trial_no_look = unique(no_looking_trials_con$trial_contingent[no_looking_trials_con$sub==as.character(sub)])
  
  bad_trial_all = unique(c(bad_trial_recording,bad_trial_incorrect,bad_trial_no_look))
  
  prop_trials_con[row,] = c(as.character(sub),
                            ifelse (length(bad_trial_recording)>0,
                                    length(bad_trial_recording)/10,length(bad_trial_index)/10),                        length(bad_trial_incorrect)/10,
                        length(bad_trial_no_look)/10,
                        length(bad_trial_all)/10)
  
  # analyze excluded data
  data_bad = cur_sub[cur_sub$trial_contingent%in% bad_trial_all,]
  if(Israeli){
    IA_bad = data_bad %>% group_by(trial_contingent) %>% summarise(IA = mean(israeli_loc_con))
  } else{
    IA_bad = data_bad %>% group_by(trial_contingent) %>% summarise(IA = mean(german_loc_con))
  }
  
  prop_IA_con[row,] = c(as.character(sub),
                    sum(IA_bad$IA==1),
                    sum(IA_bad$IA==2),
                    sum(IA_bad$IA==3),
                    sum(IA_bad$IA==4),
                    sum(IA_bad$IA==5))
  
  # exclude trials
  data_clean_con = data_clean_con[!(data_clean_con$RECORDING_SESSION_LABEL==sub &
                              (data_clean_con$trial_contingent %in% bad_trial_all)),]
  row = row+1
}

# add column indicating whether the fixation is in the familiar face's IA
if(Israeli){
  data_clean_con$IA_familiar = ifelse(data_clean_con$israeli_loc_con==
                                        data_clean_con$CURRENT_FIX_INTEREST_AREA_ID,1,0)
} else{
  data_clean_con$IA_familiar = ifelse(data_clean_con$german_loc_con==
                                        data_clean_con$CURRENT_FIX_INTEREST_AREA_ID,1,0)
} 

# write files

write.csv(data_clean, paste0(output_file,"_df_reveal.csv"))
write.csv(prop_IA, paste0(output_file,"_prop_IA.csv"))
write.csv(prop_trials, paste0(output_file,"_prop_trials.csv"))
write.csv(bad_trial_all_sub, paste0(output_file,"_bad_trials.csv"))
write.csv(data_clean_con, paste0(output_file,"_df_contingent.csv"))
write.csv(prop_trials_con, paste0(output_file,"_prop_trials_con.csv"))
write.csv(prop_IA_con, paste0(output_file,"_prop_IA_con.csv"))
write.csv(df_fix_color, paste0(output_file,"_df_color.csv"))


