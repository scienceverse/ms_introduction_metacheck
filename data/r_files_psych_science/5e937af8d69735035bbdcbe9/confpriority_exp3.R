##############CONFIDENCE AS A PRIORITY SIGNAL - DATA MANIPULATIONS FOR EXPERIMENT 3 (DELAYS)###################

####ARRANGE DATA AND CREATE USEFUL VARIABLES FOR MAIN DATA FRAME####
total_elements <- 80
exp3_raw <- exp3_raw %>% filter(sub!=0)
exp3_raw <- exp3_raw %>% arrange(sub,run,block,trial)
exp3_raw <- exp3_raw %>% group_by(sub) %>% mutate(
  experf_col = ifelse(number_blues==min(number_blues),90,ifelse(total_elements==number_blues+min(number_blues),90,60)),
  experf_let=ifelse(number_os==min(number_os),90,ifelse(total_elements==number_os+min(number_os),90,60)),
  choice1bin = ifelse(choice1<3,1,0),
  catperf = ifelse(experf_col==60 & experf_let==60,"60-60",ifelse(experf_col==90 & experf_let==60,"90-60",ifelse(experf_col==60 & experf_let==90,"60-90","90-90"))),
  expac = ifelse(catperf=="60-90",-1,ifelse(catperf=="90-60",1,0)),
  expac_lag = lag(expac))

####COMPARE EXPECTED AND OBSERVED PERFORMANCE####

#individual subjects, averaged runs
exp3_perfcol_sin_rav <- exp3_raw %>% group_by(sub,experf_col) %>% do(calculate_proportions(.,correctcol)) %>% mutate(task="color") %>% ungroup()
names(exp3_perfcol_sin_rav)[2] <- "experf"
exp3_perflet_sin_rav <- exp3_raw %>% group_by(sub,experf_let) %>% do(calculate_proportions(.,correctlet)) %>% mutate(task="letter") %>% ungroup()
names(exp3_perflet_sin_rav)[2] <- "experf"
exp3_perf_sin_rav <- rbind(exp3_perfcol_sin_rav,exp3_perflet_sin_rav)

#averaged subjects, averaged runs
exp3_perf_sav_rav <- exp3_perf_sin_rav %>% group_by(task,experf) %>% summarize (prob = mean(prob))

exp3_perf_sav_rav <- exp3_perf_sin_rav %>% group_by(task,experf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

#same, but split per delay
exp3_perfcoldelay_sin_rav <- exp3_raw %>% group_by(sub,delay,experf_col) %>% do(calculate_proportions(.,correctcol)) %>% mutate(task="color") %>% ungroup()
names(exp3_perfcoldelay_sin_rav)[3] <- "experf"
exp3_perfletdelay_sin_rav <- exp3_raw %>% group_by(sub,delay,experf_let) %>% do(calculate_proportions(.,correctlet)) %>% mutate(task="letter") %>% ungroup()
names(exp3_perfletdelay_sin_rav)[3] <- "experf"
exp3_perfdelay_sin_rav <- rbind(exp3_perfcoldelay_sin_rav,exp3_perfletdelay_sin_rav)

exp3_perfdelay_sav_rav <- exp3_perfdelay_sin_rav %>% group_by(task,delay,experf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)


####COLOR OR LETTER BIASES####

##individual subjects, individual runs
exp3_biascol_sin_rin <- exp3_raw %>% group_by(sub,run,experf_col) %>% summarise(ratioblue=sum(choiceblue==1)/sum((number_blues>total_elements/2)==TRUE)) %>% mutate(task="color")
names(exp3_biascol_sin_rin) <- c("sub", "run","experf","ratio","task")
exp3_biaslet_sin_rin <- exp3_raw %>% group_by(sub,run,experf_let) %>% summarise(ratioo=sum(choiceo==1)/sum((number_os>total_elements/2)==TRUE)) %>% mutate(task="letter")
names(exp3_biaslet_sin_rin) <- c("sub", "run","experf","ratio","task")
exp3_bias_sin_rin <- rbind(exp3_biascol_sin_rin,exp3_biaslet_sin_rin)

##individual subjects, averaged runs
exp3_biascol_sin_rav <- exp3_raw %>% group_by(sub,experf_col) %>% summarise(ratioblue=sum(choiceblue==1)/sum((number_blues>total_elements/2)==TRUE)) %>% mutate(task="color")
names(exp3_biascol_sin_rav) <- c("sub", "experf","ratio","task")
exp3_biaslet_sin_rav <- exp3_raw %>% group_by(sub,experf_let) %>% summarise(ratioo=sum(choiceo==1)/sum((number_os>total_elements/2)==TRUE)) %>% mutate(task="letter")
names(exp3_biaslet_sin_rav) <- c("sub", "experf","ratio","task")
exp3_bias_sin_rav <- rbind(exp3_biascol_sin_rav,exp3_biaslet_sin_rav)

##averaged subjects, averaged runs
exp3_bias_sav_rav <- exp3_bias_sin_rav %>% group_by(experf,task) %>% summarise(ratio = mean(ratio))

####LOCALIZATION BIASES####

#individual subjects, individual runs
exp3_loc_sin_rin <- exp3_raw %>% group_by(sub,run) %>% mutate(ntrials=length(trial))  %>% group_by(sub,run,locresp1) %>% summarise(prop=length(trial)/mean(ntrials))

#individual subjects, averaged runs
exp3_loc_sin_rav <- exp3_loc_sin_rin %>% group_by(sub,locresp1) %>% summarise(prop=mean(prop)) #this is good only if each run has the same number of trials 

#averaged subjects, averaged runs
exp3_loc_sav_rav <- exp3_loc_sin_rav %>% group_by(locresp1) %>%
  summarise(sd_prop = sd(prop, na.rm = TRUE),
            prop = mean(prop, na.rm = TRUE),
            n_prop = n()) %>%
  mutate(se_prop = sd_prop / sqrt(n_prop),
         low_ci_prop = prop - qt(1 - (0.05 / 2), n_prop - 1) * se_prop,
         up_ci_prop = prop + qt(1 - (0.05 / 2), n_prop - 1) * se_prop)

###PRIORITY

##individual subjects, individual runs
exp3_prio_sin_rin <- exp3_raw %>% group_by(sub,run,catperf) %>% do(calculate_proportions(.,choice1bin))

##averaged subjects, individual runs
exp3_prio_sav_rin <- exp3_prio_sin_rin %>% group_by(run,catperf) %>% summarise(prob =mean(prob))

#individual subjects, averaged runs
exp3_prio_sin_rav <- exp3_raw %>% group_by(sub,catperf) %>% do(calculate_proportions(.,choice1bin))

exp3_priodif_sin_rav <- dcast(data = exp3_prio_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exp3_priodif_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exp3_priodif_sin_rav <- exp3_priodif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

#same but only for correct trials
exp3_priocorrect_sin_rav <- exp3_raw %>% group_by(sub,catperf) %>% filter(correct1 == 1 & correct2 == 1) %>% do(calculate_proportions(.,choice1bin))
exp3_priodifcorrect_sin_rav <- dcast(data = exp3_priocorrect_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exp3_priodifcorrect_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exp3_priodifcorrect_sin_rav <- exp3_priodifcorrect_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

#same but only for error trials
exp3_prioerror_sin_rav <- exp3_raw %>% group_by(sub,catperf) %>% filter(correct1 == 0 & correct2 == 0) %>% do(calculate_proportions(.,choice1bin))
exp3_priodiferror_sin_rav <- dcast(data = exp3_prioerror_sin_rav,formula = sub~catperf,fun.aggregate = sum,value.var = "prob")
names(exp3_priodiferror_sin_rav) <- c("sub", "hard_hard","hard_easy","easy_hard","easy_easy")
exp3_priodiferror_sin_rav <- exp3_priodiferror_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4,expnumber=3)

#same, but split by delay
exp3_priodelay_sin_rav <- exp3_raw %>% group_by(sub, catperf, delay) %>% do(calculate_proportions(.,choice1bin))

exp3_priodifdelay_sin_rav <- dcast(data = exp3_priodelay_sin_rav, formula = sub*delay~catperf, fun.aggregate = sum, value.var = "prob")
names(exp3_priodifdelay_sin_rav) <- c("sub", "delay", "hard_hard","hard_easy","easy_hard","easy_easy")
exp3_priodifdelay_sin_rav <- exp3_priodifdelay_sin_rav %>% group_by (sub, delay) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

#averaged subjects, averaged runs
exp3_prio_sav_rav <- exp3_prio_sin_rav %>% group_by(catperf) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

exp3_priodifdelay_sav_rav <- exp3_priodifdelay_sin_rav %>% group_by(delay) %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)

####ACCURACY CHOICE 1 VS CHOICE 2####

#individual subjects, averaged runs
exp3_acord1_sin_rav <- exp3_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct1))
exp3_acord1_sin_rav <- exp3_acord1_sin_rav %>% mutate(order="first")
exp3_acord2_sin_rav <- exp3_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exp3_acord2_sin_rav <- exp3_acord2_sin_rav %>% mutate(order="second")
exp3_acord_sin_rav <- rbind(exp3_acord1_sin_rav,exp3_acord2_sin_rav)

#same, but split by EE and HH
exp3_acord1split_sin_rav <- exp3_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct1))
exp3_acord1split_sin_rav <- exp3_acord1split_sin_rav %>% mutate(order="first")
exp3_acord2split_sin_rav <- exp3_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct2))
exp3_acord2split_sin_rav <- exp3_acord2split_sin_rav %>% mutate(order="second")
exp3_acordsplit_sin_rav <- rbind(exp3_acord1split_sin_rav,exp3_acord2split_sin_rav)
exp3_acordsplit_sin_rav <- exp3_acordsplit_sin_rav %>% select(sub,catperf,prob,order) %>% group_by(sub,catperf) %>% spread(order,prob)
exp3_acordsplit_sin_rav <- exp3_acordsplit_sin_rav %>% mutate(difprob = first - second)

#same, only by EE and HH, but this time collapsed
exp3_acord1splitcollapsed_sin_rav <- exp3_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct1))
exp3_acord1splitcollapsed_sin_rav <- exp3_acord1splitcollapsed_sin_rav %>% mutate(order="first")
exp3_acord2splitcollapsed_sin_rav <- exp3_raw %>% filter(catperf=="60-60" | catperf=="90-90") %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exp3_acord2splitcollapsed_sin_rav <- exp3_acord2splitcollapsed_sin_rav %>% mutate(order="second")
exp3_acordsplitcollapsed_sin_rav <- rbind(exp3_acord1splitcollapsed_sin_rav,exp3_acord2splitcollapsed_sin_rav)
exp3_acordsplitcollapsed_sin_rav <- exp3_acordsplitcollapsed_sin_rav %>% select(sub,prob,order) %>% group_by(sub) %>% spread(order,prob)
exp3_acordsplitcollapsed_sin_rav <- exp3_acordsplitcollapsed_sin_rav %>% mutate(difprob = first - second)

#same, but split by each trial type: EE, EH, HE, HH
exp3_acord1splitall_sin_rav <- exp3_raw %>%  group_by(sub,catperf) %>% do(calculate_proportions(.,correct1))
exp3_acord1splitall_sin_rav <- exp3_acord1splitall_sin_rav %>% mutate(order="first")
exp3_acord2splitall_sin_rav <- exp3_raw %>% group_by(sub,catperf) %>% do(calculate_proportions(.,correct2))
exp3_acord2splitall_sin_rav <- exp3_acord2splitall_sin_rav %>% mutate(order="second")
exp3_acordsplitall_sin_rav <- rbind(exp3_acord1splitall_sin_rav,exp3_acord2splitall_sin_rav)
exp3_acordsplitall_sin_rav <- exp3_acordsplitall_sin_rav %>% select(sub,catperf,prob,order) %>% group_by(sub,catperf) %>% spread(order,prob)
exp3_acordsplitall_sin_rav <- exp3_acordsplitall_sin_rav %>% mutate(difprob = first - second)

#collapsed across all trial types: EE, EH, HE, HH
exp3_acord1togetherall_sin_rav <- exp3_raw %>%  group_by(sub) %>% do(calculate_proportions(.,correct1))
exp3_acord1togetherall_sin_rav <- exp3_acord1togetherall_sin_rav %>% mutate(order="first")
exp3_acord2togetherall_sin_rav <- exp3_raw %>% group_by(sub) %>% do(calculate_proportions(.,correct2))
exp3_acord2togetherall_sin_rav <- exp3_acord2togetherall_sin_rav %>% mutate(order="second")
exp3_acordtogetherall_sin_rav <- rbind(exp3_acord1togetherall_sin_rav,exp3_acord2togetherall_sin_rav)
exp3_acordtogetherall_sin_rav <- exp3_acordtogetherall_sin_rav %>% select(sub,prob,order) %>% group_by(sub) %>% spread(order,prob)
exp3_acordtogetherall_sin_rav <- exp3_acordtogetherall_sin_rav %>% mutate(difprob = first - second)

####PERFORMANCE WHEN CHOOSING EASY FIRST VS HARD FIRST####

#individual subjects, averaged runs
exp3_difchoice_sin_rav <- exp3_raw %>% filter(catperf=="60-90" | catperf=="90-60") %>% mutate(cheasy_first = ifelse(catperf=="90-60" & choice1bin == 1, 1, ifelse(catperf=="90-60" & choice1bin == 0, 0, ifelse(catperf=="60-90" & choice1bin == 0, 1, 0)))) #if first chosen task is easy, cheasy_first = 1, otherwise 0

exp3_difchoice1_sin_rav <- exp3_difchoice_sin_rav %>% group_by(sub,cheasy_first) %>% do(calculate_proportions(.,correct1))
exp3_difchoice2_sin_rav <- exp3_difchoice_sin_rav %>% group_by(sub,cheasy_first) %>% do(calculate_proportions(.,correct2))

exp3_difchoice1_sin_rav <- exp3_difchoice1_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 1,0)) #if task_dif = 1, easy task
exp3_difchoice2_sin_rav <- exp3_difchoice2_sin_rav %>% mutate(task_dif = ifelse(cheasy_first == 1, 0,1))

exp3_difchoice_sin_rav <- rbind(exp3_difchoice1_sin_rav,exp3_difchoice2_sin_rav)

#averaged subjects, averaged runs
exp3_difchoice_sav_rav <- exp3_difchoice_sin_rav %>% group_by(task_dif,cheasy_first) %>% summarize (prob = mean(prob))

exp3_difchoice_sav_rav <- exp3_difchoice_sin_rav %>% group_by(task_dif,cheasy_first) %>%
  summarise(sd_prob = sd(prob, na.rm = TRUE),
            prob = mean(prob, na.rm = TRUE),
            n_prob = n()) %>%
  mutate(se_prob = sd_prob / sqrt(n_prob),
         low_ci_prob = prob - qt(1 - (0.05 / 2), n_prob - 1) * se_prob,
         up_ci_prob = prob + qt(1 - (0.05 / 2), n_prob - 1) * se_prob)

####REVERSAL PRIORITY EFFECT FROM DIFFICULTY FOR ERRORS####
exp3_priosameacc_sin_rav <- exp3_raw %>% mutate(acccategory=ifelse(correct1==1 & correct2 == 1,"correct",ifelse(correct1==0 & correct2 == 0,"error","none"))) %>% filter(acccategory != "none") %>% group_by(sub,catperf,acccategory) %>% do(calculate_proportions(.,choice1bin))
exp3_priosameacc_sin_rav <- exp3_priosameacc_sin_rav %>% mutate(catperf2 = ifelse(catperf=="60-60","HH",ifelse(catperf=="90-60","EH",ifelse(catperf=="60-90","HE","EE"))))

exp3_priosameaccdif_sin_rav <- exp3_priosameacc_sin_rav %>% group_by(sub,acccategory) %>% select(sub,acccategory,catperf,prob) %>% spread(catperf,prob)
names(exp3_priosameaccdif_sin_rav) <- c("sub", "acccategory", "hard_hard","hard_easy","easy_hard","easy_easy")
exp3_priosameaccdif_sin_rav <- exp3_priosameaccdif_sin_rav %>% group_by (sub) %>% mutate (difacc = easy_hard - hard_easy,avecomb=sum(easy_easy,easy_hard,hard_easy,hard_hard)/4)

exp3_priosameaccdif_sav_rav <- exp3_priosameaccdif_sin_rav %>% group_by(acccategory) %>%
  summarise(sd_difacc = sd(difacc, na.rm = TRUE),
            difacc = mean(difacc, na.rm = TRUE),
            n_difacc = n()) %>%
  mutate(se_difacc = sd_difacc / sqrt(n_difacc),
         low_ci_difacc = difacc - qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc,
         up_ci_difacc = difacc + qt(1 - (0.05 / 2), n_difacc - 1) * se_difacc)