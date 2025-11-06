##############CONFIDENCE AS A PRIORITY SIGNAL - DATA MANIPULATIONS FOR EXPERIMENT 2 (CHOICE-ONLY WITHOUT CONFIDENCE RATINGS)###################

####ARRANGE DATA AND CREATE USEFUL VARIABLES FOR MAIN DATA FRAME####
total_elements <- 80
exp2_raw <- exp2_raw %>% filter(sub!=0)
exp2_raw <- exp2_raw %>% arrange(sub,run,block,trial)
exp2_raw <- exp2_raw %>% group_by(sub) %>% mutate(
  experf_col = ifelse(number_blues==min(number_blues),90,ifelse(total_elements==number_blues+min(number_blues),90,60)),
  experf_let=ifelse(number_os==min(number_os),90,ifelse(total_elements==number_os+min(number_os),90,60)),
  choice1bin = ifelse(choice1<3,1,0),
  catperf = ifelse(experf_col==60 & experf_let==60,"60-60",ifelse(experf_col==90 & experf_let==60,"90-60",ifelse(experf_col==60 & experf_let==90,"60-90","90-90"))),
  expac = ifelse(catperf=="60-90",-1,ifelse(catperf=="90-60",1,0)),
  expac_lag = lag(expac))

####COMPARE EXPECTED AND OBSERVED PERFORMANCE####

#individual subjects, averaged runs
exp2_perfcol_sin_rav <- exp2_raw %>% group_by(sub,experf_col) %>% do(calculate_proportions(.,correctcol)) %>% mutate(task="color") %>% ungroup()
names(exp2_perfcol_sin_rav)[2] <- "experf"
exp2_perflet_sin_rav <- exp2_raw %>% group_by(sub,experf_let) %>% do(calculate_proportions(.,correctlet)) %>% mutate(task="letter") %>% ungroup()
names(exp2_perflet_sin_rav)[2] <- "experf"
exp2_perf_sin_rav <- rbind(exp2_perfcol_sin_rav,exp2_perflet_sin_rav)

#averaged subjects, averaged runs
exp2_perf_sav_rav <- exp2_perf_sin_rav %>% group_by(task,experf) %>% summarize (prob = mean(prob))

exp2_perf_sav_rav <- exp2_perf_sin_rav %>% group_by(task,experf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####COLOR OR LETTER BIASES####

##individual subjects, individual runs
exp2_biascol_sin_rin <- exp2_raw %>% group_by(sub,run,experf_col) %>% summarise(ratioblue=sum(choiceblue==1)/sum((number_blues>total_elements/2)==TRUE)) %>% mutate(task="color")
names(exp2_biascol_sin_rin) <- c("sub", "run","experf","ratio","task")
exp2_biaslet_sin_rin <- exp2_raw %>% group_by(sub,run,experf_let) %>% summarise(ratioo=sum(choiceo==1)/sum((number_os>total_elements/2)==TRUE)) %>% mutate(task="letter")
names(exp2_biaslet_sin_rin) <- c("sub", "run","experf","ratio","task")
exp2_bias_sin_rin <- rbind(exp2_biascol_sin_rin,exp2_biaslet_sin_rin)

##individual subjects, averaged runs
exp2_biascol_sin_rav <- exp2_raw %>% group_by(sub,experf_col) %>% summarise(ratioblue=sum(choiceblue==1)/sum((number_blues>total_elements/2)==TRUE)) %>% mutate(task="color")
names(exp2_biascol_sin_rav) <- c("sub", "experf","ratio","task")
exp2_biaslet_sin_rav <- exp2_raw %>% group_by(sub,experf_let) %>% summarise(ratioo=sum(choiceo==1)/sum((number_os>total_elements/2)==TRUE)) %>% mutate(task="letter")
names(exp2_biaslet_sin_rav) <- c("sub", "experf","ratio","task")
exp2_bias_sin_rav <- rbind(exp2_biascol_sin_rav,exp2_biaslet_sin_rav)

##averaged subjects, averaged runs
exp2_bias_sav_rav <- exp2_bias_sin_rav %>% group_by(experf,task) %>% summarise(ratio = mean(ratio))

####LOCALIZATION BIASES####

#individual subjects, individual runs
exp2_loc_sin_rin <- exp2_raw %>% group_by(sub,run) %>% mutate(ntrials=length(trial))  %>% group_by(sub,run,locresp1) %>% summarise(prop=length(trial)/mean(ntrials))

#individual subjects, averaged runs
exp2_loc_sin_rav <- exp2_loc_sin_rin %>% group_by(sub,locresp1) %>% summarise(prop=mean(prop)) #this is good only if each run has the same number of trials 

#averaged subjects, averaged runs
exp2_loc_sav_rav <- exp2_loc_sin_rav %>% group_by(locresp1) %>%
  summarise(sd_prop = sd(prop, na.rm = TRUE),
            prop = mean(prop, na.rm = TRUE),
            n_prop = n()) %>%
  mutate(se_prop = sd_prop / sqrt(n_prop),
         low_ci_prop = prop - qt(1 - (0.05 / 2), n_prop - 1) * se_prop,
         up_ci_prop = prop + qt(1 - (0.05 / 2), n_prop - 1) * se_prop)

###PRIORITY

##individual subjects, individual runs
exp2_prio_sin_rin <- exp2_raw %>% group_by(sub,run,catperf) %>% do(calculate_proportions(.,choice1bin))

##averaged subjects, individual runs
exp2_prio_sav_rin <- exp2_prio_sin_rin %>% group_by(run,catperf) %>% summarise(prob =mean(prob))

#individual subjects, averaged runs
exp2_prio_sin_rav <- exp2_raw %>% group_by(sub,catperf) %>% do(calculate_proportions(.,choice1bin))

exp2_priodif_sin_rav <- dcast(data = exp2_prio_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exp2_priodif_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exp2_priodif_sin_rav <- exp2_priodif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

exp2_priodif_sav_rav <- exp2_priodif_sin_rav %>% group_by() %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)

#same but only for correct trials
exp2_priocorrect_sin_rav <- exp2_raw %>% group_by(sub,catperf) %>% filter(correct1 == 1 & correct2 == 1) %>% do(calculate_proportions(.,choice1bin))
exp2_priodifcorrect_sin_rav <- dcast(data = exp2_priocorrect_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exp2_priodifcorrect_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exp2_priodifcorrect_sin_rav <- exp2_priodifcorrect_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

#same but only for error trials
exp2_prioerror_sin_rav <- exp2_raw %>% group_by(sub,catperf) %>% filter(correct1 == 0 & correct2 == 0) %>% do(calculate_proportions(.,choice1bin))
exp2_priodiferror_sin_rav <- dcast(data = exp2_prioerror_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exp2_priodiferror_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exp2_priodiferror_sin_rav <- exp2_priodiferror_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4,expnumber=1)

#averaged subjects, averaged runs
exp2_prio_sav_rav <- exp2_prio_sin_rav %>% group_by(catperf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####ACCURACY CHOICE 1 VS CHOICE 2####

#individual subjects, averaged runs
exp2_acord1_sin_rav <- exp2_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct1))
exp2_acord1_sin_rav <- exp2_acord1_sin_rav %>% mutate(order="first")
exp2_acord2_sin_rav <- exp2_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exp2_acord2_sin_rav <- exp2_acord2_sin_rav %>% mutate(order="second")
exp2_acord_sin_rav <- rbind(exp2_acord1_sin_rav,exp2_acord2_sin_rav)

#same, but split by EE and HH
exp2_acord1split_sin_rav <- exp2_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct1))
exp2_acord1split_sin_rav <- exp2_acord1split_sin_rav %>% mutate(order="first")
exp2_acord2split_sin_rav <- exp2_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct2))
exp2_acord2split_sin_rav <- exp2_acord2split_sin_rav %>% mutate(order="second")
exp2_acordsplit_sin_rav <- rbind(exp2_acord1split_sin_rav,exp2_acord2split_sin_rav)
exp2_acordsplit_sin_rav <- exp2_acordsplit_sin_rav %>% select(sub,catperf,prob,order) %>% group_by(sub,catperf) %>% spread(order,prob)
exp2_acordsplit_sin_rav <- exp2_acordsplit_sin_rav %>% mutate(difprob = first - second)

#same, only by EE and HH, but this time collapsed
exp2_acord1splitcollapsed_sin_rav <- exp2_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct1))
exp2_acord1splitcollapsed_sin_rav <- exp2_acord1splitcollapsed_sin_rav %>% mutate(order="first")
exp2_acord2splitcollapsed_sin_rav <- exp2_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exp2_acord2splitcollapsed_sin_rav <- exp2_acord2splitcollapsed_sin_rav %>% mutate(order="second")
exp2_acordsplitcollapsed_sin_rav <- rbind(exp2_acord1splitcollapsed_sin_rav,exp2_acord2splitcollapsed_sin_rav)
exp2_acordsplitcollapsed_sin_rav <- exp2_acordsplitcollapsed_sin_rav %>% select(sub,prob,order) %>% group_by(sub) %>% spread(order,prob)
exp2_acordsplitcollapsed_sin_rav <- exp2_acordsplitcollapsed_sin_rav %>% mutate(difprob = first - second)

#same, but split by each trial type: EE, EH, HE, HH
exp2_acord1splitall_sin_rav <- exp2_raw %>%  group_by(sub,catperf) %>% do(calculate_proportions(.,correct1))
exp2_acord1splitall_sin_rav <- exp2_acord1splitall_sin_rav %>% mutate(order="first")
exp2_acord2splitall_sin_rav <- exp2_raw %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct2))
exp2_acord2splitall_sin_rav <- exp2_acord2splitall_sin_rav %>% mutate(order="second")
exp2_acordsplitall_sin_rav <- rbind(exp2_acord1splitall_sin_rav,exp2_acord2splitall_sin_rav)
exp2_acordsplitall_sin_rav <- exp2_acordsplitall_sin_rav %>% select(sub,catperf,prob,order) %>% group_by(sub,catperf) %>% spread(order,prob)
exp2_acordsplitall_sin_rav <- exp2_acordsplitall_sin_rav %>% mutate(difprob = first - second)

#collapsed across all trial types: EE, EH, HE, HH
exp2_acord1togetherall_sin_rav <- exp2_raw %>%  group_by(sub) %>% do(calculate_proportions(.,correct1))
exp2_acord1togetherall_sin_rav <- exp2_acord1togetherall_sin_rav %>% mutate(order="first")
exp2_acord2togetherall_sin_rav <- exp2_raw %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exp2_acord2togetherall_sin_rav <- exp2_acord2togetherall_sin_rav %>% mutate(order="second")
exp2_acordtogetherall_sin_rav <- rbind(exp2_acord1togetherall_sin_rav,exp2_acord2togetherall_sin_rav)
exp2_acordtogetherall_sin_rav <- exp2_acordtogetherall_sin_rav %>% select(sub,prob,order) %>% group_by(sub) %>% spread(order,prob)
exp2_acordtogetherall_sin_rav <- exp2_acordtogetherall_sin_rav %>% mutate(difprob = first - second)

####PERFORMANCE WHEN CHOOSING EASY FIRST VS HARD FIRST####

#individual subjects, averaged runs
exp2_difchoice_sin_rav <- exp2_raw %>% filter(catperf=="60-90" | catperf=="90-60") %>% mutate(cheasy_first = ifelse(catperf=="90-60" & choice1bin == 1, 1, ifelse(catperf=="90-60" & choice1bin == 0, 0, ifelse(catperf=="60-90" & choice1bin == 0, 1, 0)))) #if first chosen task is easy, cheasy_first = 1, otherwise 0

exp2_difchoice1_sin_rav <- exp2_difchoice_sin_rav %>% group_by(sub,cheasy_first) %>% do(calculate_proportions(.,correct1))
exp2_difchoice2_sin_rav <- exp2_difchoice_sin_rav %>% group_by(sub,cheasy_first) %>% do(calculate_proportions(.,correct2))

exp2_difchoice1_sin_rav <- exp2_difchoice1_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 1,0)) #if task_dif = 1, easy task
exp2_difchoice2_sin_rav <- exp2_difchoice2_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 0,1))

exp2_difchoice_sin_rav <- rbind(exp2_difchoice1_sin_rav,exp2_difchoice2_sin_rav)

#averaged subjects, averaged runs
exp2_difchoice_sav_rav <- exp2_difchoice_sin_rav %>% group_by(task_dif,cheasy_first) %>% summarize (prob = mean(prob))

exp2_difchoice_sav_rav <- exp2_difchoice_sin_rav %>% group_by(task_dif,cheasy_first) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####REVERSAL PRIORITY EFFECT FROM DIFFICULTY FOR ERRORS####
exp2_priosameacc_sin_rav <- exp2_raw %>% mutate(acccategory=ifelse(correct1==1 & correct2 == 1,"correct",ifelse(correct1==0 & correct2 == 0,"error","none"))) %>% filter(acccategory != "none") %>% group_by(sub,catperf,acccategory) %>% do(calculate_proportions(.,choice1bin))
exp2_priosameacc_sin_rav <- exp2_priosameacc_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))

exp2_priosameaccdif_sin_rav <- exp2_priosameacc_sin_rav %>% group_by(sub,acccategory) %>% select(sub,acccategory,catperf,prob) %>% spread(catperf,prob)
names(exp2_priosameaccdif_sin_rav) <- c("sub", "acccategory", "hard_hard","hard_easy","easy_hard","easy_easy")
exp2_priosameaccdif_sin_rav <- exp2_priosameaccdif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

exp2_priosameaccdif_sav_rav <- exp2_priosameaccdif_sin_rav %>% group_by(acccategory) %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)