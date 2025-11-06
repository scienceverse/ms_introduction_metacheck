##############CONFIDENCE AS A PRIORITY SIGNAL - DATA MANIPULATIONS FOR EXPERIMENT 4 (PROSPECTIVE CONFIDENCE)###################

####ARRANGE DATA AND CREATE USEFUL VARIABLES FOR MAIN DATA FRAME####
total_elements <- 80
exp4_raw <- exp4_raw %>% filter(sub!=0)
exp4_raw <- exp4_raw %>% arrange(sub,run,block,learn_active,trial)
exp4_raw <- exp4_raw %>% mutate(current_feat = ifelse(is.nan(correctcol)==TRUE, 1, 0),
                                task=ifelse(current_color==1,"color","letter"))
exp4_raw <- exp4_raw %>% group_by(sub,current_feat) %>% mutate(current_dif_old = (current_dif),current_dif = ifelse(number_bo==min(number_bo),90,ifelse(total_elements==number_bo+min(number_bo),90,60)))

exp4_rawcol <- exp4_raw %>% filter(correctcol == 0 | correctcol == 1)
exp4_rawlet <- exp4_raw %>% filter(correctlet == 0 | correctlet == 1)

####EXPECTED AND OBSERVED PERFORMANCE####

#individual subjects, averaged runs, split by familiarization/test
exp4_perf_sin_rav <- exp4_raw %>% group_by(sub,learn_active,current_feat,current_dif) %>% do(calculate_proportions(.,correct1))

#averaged subjects, averaged runs, split by familiarization/test
exp4_perf_sav_rav <- exp4_perf_sin_rav %>% group_by(learn_active,current_feat,current_dif) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exp4_perf_sin_rav <- exp4_perf_sin_rav %>% mutate(experf=ifelse(current_dif==90,90,60))
exp4_perf_sav_rav <- exp4_perf_sav_rav %>% mutate(experf=ifelse(current_dif==90,90,60))

####CHOICES SETS####
exp4_tcraw <- exp4_tcraw %>% mutate(choiceorder=ifelse(chosen1_order==1,1,0),#1 if first presented task was chosen
                                choicefeat=ifelse(chosen1_feat==1,0,1),#1 if color task was chosen
                                choicedif=ifelse(chosen1_dif==1,0,1),#1 if easy task was chosen
                                choiceloc=ifelse(locboxresp==0,1,0))#1 if left box was chosen 

#individual subjects, averaged runs
exp4_chord_sin_rav <- exp4_tcraw %>% group_by(sub) %>% do(calculate_proportions(.,choiceorder)) #choice order as tasks were presented
exp4_chfeat_sin_rav <- exp4_tcraw %>% group_by(sub) %>% do(calculate_proportions(.,choicefeat)) #choice feat
exp4_chdif_sin_rav <- exp4_tcraw %>% group_by(sub) %>% do(calculate_proportions(.,choicedif)) #choice difficulty
exp4_chloc_sin_rav <- exp4_tcraw %>% group_by(sub) %>% do(calculate_proportions(.,choiceloc)) #choice box location

#same but split by type of block (same / different task)
exp4_chord_typetask_sin_rav <- exp4_tcraw %>% group_by(sub,task1_type) %>% do(calculate_proportions(.,choiceorder)) #choice order as tasks were presented
exp4_chfeat_typetask_sin_rav <- exp4_tcraw %>% group_by(sub,task1_type) %>% do(calculate_proportions(.,choicefeat)) #choice feat
exp4_chdif_typetask_sin_rav <- exp4_tcraw %>% group_by(sub,task1_type) %>% do(calculate_proportions(.,choicedif)) #choice difficulty
exp4_chdif_typetask_sav_rav <- exp4_chdif_typetask_sin_rav %>% group_by(task1_type) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exp4_chloc_typetask_sin_rav <- exp4_tcraw %>% group_by(sub,task1_type) %>% do(calculate_proportions(.,choiceloc)) #choice box location
exp4_chloc_typetask_sav_rav <- exp4_chloc_typetask_sin_rav %>% group_by(task1_type) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

#location but disregarding type of task
exp4_chloc_pooled_sin_rav <- exp4_tcraw %>% group_by(sub) %>% do(calculate_proportions(.,choiceloc)) #choice box location
exp4_chloc_pooled_sav_rav <- exp4_chloc_pooled_sin_rav %>% ungroup() %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####PERFORMANCE WHEN CHOOSING EASY FIRST VS HARD FIRST####
#individual subjects, averaged runs
exp4_difchoice_sin_rav <- exp4_raw %>% filter(learn_active==0) %>% select(sub,block,trial,chosen1_dif,current_dif,task,correct1) %>% mutate(set = ifelse(trial<5,1,2))#chosen1_dif = 1 if easy set was chosen
exp4_difchoice_sin_rav <- exp4_difchoice_sin_rav %>% group_by(sub,chosen1_dif,current_dif) %>% do(calculate_proportions(.,correct1))

#averaged subjects, averaged runs
exp4_difchoice_sav_rav <- exp4_difchoice_sin_rav %>% group_by(chosen1_dif,current_dif) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

